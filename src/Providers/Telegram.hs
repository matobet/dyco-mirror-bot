
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeApplications #-}
module Providers.Telegram where

import Config
import Core
import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Monad
import Control.Monad.Except
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (toStrict)
import Data.Text (Text, unpack, pack)
import Logging
import Providers.API
import Servant.Client hiding (Response)
import Telegram.Bot.API hiding (Message)
import qualified Telegram.Bot.API as TBA
import Telegram.Bot.Simple.BotApp.Internal (startPolling)
import Optics.Operators
import Data.Maybe
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import Optics
import TextShow
import Data.Coerce (coerce)
import Data.Int (Int32)

log' :: Logger
log' = mkLog "Telegram"

type TelegramM = ClientM

bot :: Endpoint -> Text -> TelegramM ()
bot endpoint token = do
  void . async $ startPolling onUpdate
  publishLoop
  where
    onUpdate :: Update -> TelegramM ()
    onUpdate Update {..} = maybe ignoreMsg handleMsg msg
      where
        msg = do
          message <- updateMessage
          user    <- messageFrom message
          let userName = fromMaybe (userFirstName user) $ userUsername user
          let channel   = messageChat message
          let channelId = chatId channel
          channelName <- chatTitle channel
          let content =
                TBA.messageText message
                  <|> TBA.messageCaption message -- in case of images, the messageCaption is set instead of messageText
                  <|> (TBA.messageSticker message >>= TBA.stickerEmoji) -- in case of sticker, we want to extract the underlying emoji
          let photoSizes = TBA.messagePhoto message
          let replyTo    = TBA.messageReplyToMessage message
          return
            ( message
            , UserRef userName
            , Channel (ChannelId . toStrict . encodeToLazyText $ channelId) channelName
            , content
            , photoSizes
            , replyTo
            )

        ignoreMsg = log' Info "Skipping incoming message"

        handleMsg
          :: (TBA.Message, UserRef, Channel, Maybe Text, Maybe [PhotoSize], Maybe TBA.Message) -> TelegramM ()
        handleMsg (message, userRef, chan, content, photoSizes, replyTo) = do
          log' Info $ "Handling incoming message: " <> pack (show updateMessage)
          image <- case photoSizes of
            Just sizes -> do
              log' Info $ "Resolving incoming photo: " <> pack (show sizes)
              let bestSize = maximumBy (comparing photoSizeFileSize) sizes
              log' Info $ "Biggest size is: " <> pack (show bestSize)
              fmap ImageBytes <$> downloadFile (photoSizeFileId bestSize)
            Nothing -> return Nothing
          onMessageReceived endpoint
            $ Message (messageIdFromTelegramMessage message) userRef chan content image replyToMsg
          where replyToMsg = messageIdFromTelegramMessage <$> replyTo

        downloadFile :: FileId -> ClientM (Maybe BS.ByteString)
        downloadFile fileId = do
          res <- getFile fileId
          case res of
            Response { responseOk = True, responseResult = File { fileFilePath = Just path } } -> do
              log' Info $ "Downloading file, path = " <> path
              fileContent <-
                httpBS <=< parseRequest . unpack $ "https://api.telegram.org/file/bot" <> token <> "/" <> path
              return . Just $ getResponseBody fileContent
            _ -> do
              log' Error $ "Error getting Telegram File: " <> pack (show fileId)
              return Nothing

    publishLoop :: ClientM ()
    publishLoop = forever $ do
      log' Info "Awaiting message dispatch..."
      (message, targetChannelId) <- liftIO $ awaitMessageDispatched endpoint
      let body = formatMessage message
      log' Info $ mconcat ["Dispatching ", showt message]
      publishedMsg <-
        sendTo (ChatId . read . unpack . unChannelId $ targetChannelId)
               (TBA.MessageId . read . unpack <$> (message ^? #replyTo % _Just % #id))
               body
        $  message
        ^. #image
      onMessagePublished endpoint $ messageIdFromTelegramMessage publishedMsg

    messageIdFromTelegramMessage = MessageID provider . showt @Int32 . coerce . TBA.messageMessageId
    provider                     = endpointProvider endpoint

sendTo :: ChatId -> Maybe TBA.MessageId -> Text -> Maybe Image -> ClientM TBA.Message
sendTo chatId replyToId msgText image = do
  res <- case image of
    Just (ImageUrl url) -> sendPhoto SendPhotoRequest { sendPhotoChatId              = SomeChatId chatId
                                                      , sendPhotoPhoto               = PhotoUrl url
                                                      , sendPhotoThumb               = Nothing
                                                      , sendPhotoCaption             = Just msgText
                                                      , sendPhotoParseMode           = Nothing
                                                      , sendPhotoDisableNotification = Nothing
                                                      , sendPhotoReplyToMessageId    = replyToId
                                                      , sendPhotoReplyMarkup         = Nothing
                                                      }
    _ -> sendMessage SendMessageRequest { sendMessageChatId                = SomeChatId chatId
                                        , sendMessageText                  = msgText
                                        , sendMessageParseMode             = Nothing
                                        , sendMessageDisableWebPagePreview = Nothing
                                        , sendMessageDisableNotification   = Nothing
                                        , sendMessageReplyToMessageId      = replyToId
                                        , sendMessageReplyMarkup           = Nothing
                                        }
  unless (responseOk res) . log' Error $ "Telegram publish failed with: " <> pack (show res) -- TODO: error handling to prevent DeadLock
  return . TBA.responseResult $ res

instance ProviderEndpoint Telegram where
  spawnProviderEndpoint (Telegram ProviderConfig {..}) =
    withNewEndpoint TelegramPT $ void . async . runTelegramWithErrorHandling
    where
      runTelegramWithErrorHandling endpoint = do
        res <- defaultRunBot (Token token) (bot endpoint token)
        case res of
          Left  err -> log' Error $ "Telegram endpoint failed with: " <> pack (show err)
          Right ()  -> return ()
