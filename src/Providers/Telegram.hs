
{-# OPTIONS_GHC -Wno-orphans #-}
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

log' :: Logger
log' = mkLog "Telegram"

bot :: Endpoint -> Text -> ClientM ()
bot endpoint token = do
  void . async $ startPolling onUpdate
  publishLoop
  where
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
          return
            ( UserRef userName
            , Channel (ChannelId . toStrict . encodeToLazyText $ channelId) channelName
            , content
            , photoSizes
            )

        ignoreMsg = log' Info "Skipping incoming message"

        handleMsg (userRef, chan, content, photoSizes) = do
          log' Info $ "Handling incoming message: " <> pack (show updateMessage)
          image <- case photoSizes of
            Just sizes -> do
              log' Info $ "Resolving incoming photo: " <> pack (show sizes)
              let bestSize = maximumBy (comparing photoSizeFileSize) sizes
              log' Info $ "Biggest size is: " <> pack (show bestSize)
              fmap ImageBytes <$> downloadFile (photoSizeFileId bestSize)
            Nothing -> return Nothing
          onMessageReceived endpoint $ Message userRef chan content image

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

    publishLoop = forever $ do
      log' Info "Awaiting message dispatch..."
      (message, targetChannelId) <- liftIO $ awaitMessageDispatched endpoint
      let body = formatMessage message
      -- log' Info $ mconcat ["Dispatching ", body, " to ", showt targetChannelId]
      sendTo (ChatId . read . unpack . unChannelId $ targetChannelId) body $ message ^. #image

sendTo :: ChatId -> Text -> Maybe Image -> ClientM ()
sendTo chatId msgText image = void . async $ do
  res <- case image of
    Just (ImageUrl url) -> sendPhoto SendPhotoRequest { sendPhotoChatId              = SomeChatId chatId
                                                      , sendPhotoPhoto               = PhotoUrl url
                                                      , sendPhotoThumb               = Nothing
                                                      , sendPhotoCaption             = Just msgText
                                                      , sendPhotoParseMode           = Nothing
                                                      , sendPhotoDisableNotification = Nothing
                                                      , sendPhotoReplyToMessageId    = Nothing
                                                      , sendPhotoReplyMarkup         = Nothing
                                                      }
    _ -> sendMessage SendMessageRequest { sendMessageChatId                = SomeChatId chatId
                                        , sendMessageText                  = msgText
                                        , sendMessageParseMode             = Nothing
                                        , sendMessageDisableWebPagePreview = Nothing
                                        , sendMessageDisableNotification   = Nothing
                                        , sendMessageReplyToMessageId      = Nothing
                                        , sendMessageReplyMarkup           = Nothing
                                        }
  unless (responseOk res) . log' Error $ "Telegram publish failed with: " <> pack (show res)

instance ProviderEndpoint Telegram where
  spawnProviderEndpoint (Telegram ProviderConfig {..}) =
    withNewEndpoint TelegramPT $ void . async . runTelegramWithErrorHandling
    where
      runTelegramWithErrorHandling endpoint = do
        res <- defaultRunBot (Token token) (bot endpoint token)
        case res of
          Left  err -> log' Error $ "Telegram endpoint failed with: " <> pack (show err)
          Right ()  -> return ()
