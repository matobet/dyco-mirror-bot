
{-# LANGUAGE NoMonomorphismRestriction #-}
module Providers.Telegram where

import Config
import Core
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
import TextShow

(logInfo, logError) = mkLog "Telegram"

bot :: Endpoint -> ClientM ()
bot endpoint = do
  async $ startPolling onUpdate
  publishLoop
  where
    onUpdate Update {..} = maybe ignoreMsg handleMsg msg
      where
        msg = do
          message  <- updateMessage
          userName <- userUsername =<< messageFrom message
          let channel   = messageChat message
          let channelId = chatId channel
          channelName <- chatTitle channel
          content     <- TBA.messageText message
          return $ Message (UserRef userName)
                            (Channel (ChannelId . toStrict . encodeToLazyText $ channelId) channelName)
                            content

        ignoreMsg = logInfo $ "Skipping incoming message: " <> showt msg

        handleMsg msg = do
          logInfo "Handling incoming message"
          onMessageReceived endpoint msg


    publishLoop = forever $ do
      logInfo "Awaiting message dispatch..."
      (message, targetChannelId) <- liftIO $ awaitMessageDispatched endpoint
      let body = formatMessage message
      logInfo $ mconcat ["Dispatching ", body, " to ", showt targetChannelId]
      sendTo (ChatId . read . unpack . unChannelId $ targetChannelId) body

sendTo :: ChatId -> Text -> ClientM (Response TBA.Message)
sendTo chatId msgText = sendMessage SendMessageRequest { sendMessageChatId                = SomeChatId chatId
                                                       , sendMessageText                  = msgText
                                                       , sendMessageParseMode             = Nothing
                                                       , sendMessageDisableWebPagePreview = Nothing
                                                       , sendMessageDisableNotification   = Nothing
                                                       , sendMessageReplyToMessageId      = Nothing
                                                       , sendMessageReplyMarkup           = Nothing
                                                       }

instance ProviderEndpoint TelegramConfig where
  spawnProviderEndpoint TelegramConfig {..} =
    withNewEndpoint $ void . async . runTelegramWithErrorHandling
    where
      runTelegramWithErrorHandling endpoint = do
        res <- defaultRunBot (Token token) (bot endpoint)
        case res of
          Left err -> logError $ "Telegram endpoint failed with: " <> pack (show err)
          Right () -> return ()
