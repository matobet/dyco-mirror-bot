
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
import Data.Maybe

log' :: Logger
log' = mkLog "Telegram"

bot :: Endpoint -> ClientM ()
bot endpoint = do
  void . async $ startPolling onUpdate
  publishLoop
  where
    onUpdate Update {..} = maybe ignoreMsg handleMsg msg
      where
        msg = do
          message  <- updateMessage
          user <- messageFrom message
          let userName  = fromMaybe (userFirstName user) $ userUsername user
          let channel   = messageChat message
          let channelId = chatId channel
          channelName <- chatTitle channel
          content     <- TBA.messageText message
          return $ Message (UserRef userName)
                            (Channel (ChannelId . toStrict . encodeToLazyText $ channelId) channelName)
                            content

        ignoreMsg = log' Info $ "Skipping incoming message: " <> showt msg

        handleMsg m = do
          log' Info "Handling incoming message"
          onMessageReceived endpoint m


    publishLoop = forever $ do
      log' Info "Awaiting message dispatch..."
      (message, targetChannelId) <- liftIO $ awaitMessageDispatched endpoint
      let body = formatMessage message
      log' Info $ mconcat ["Dispatching ", body, " to ", showt targetChannelId]
      sendTo (ChatId . read . unpack . unChannelId $ targetChannelId) body

sendTo :: ChatId -> Text -> ClientM ()
sendTo chatId msgText = void . async $ do
  res <- sendMessage SendMessageRequest { sendMessageChatId                = SomeChatId chatId
                                        , sendMessageText                  = msgText
                                        , sendMessageParseMode             = Nothing
                                        , sendMessageDisableWebPagePreview = Nothing
                                        , sendMessageDisableNotification   = Nothing
                                        , sendMessageReplyToMessageId      = Nothing
                                        , sendMessageReplyMarkup           = Nothing
                                        }
  unless (responseOk res) . log' Error $ "Telegram publish failed with: " <> pack (show res)

instance ProviderEndpoint Telegram where
  spawnProviderEndpoint (Telegram ProviderConfig{..}) =
    withNewEndpoint TelegramPT $ void . async . runTelegramWithErrorHandling
    where
      runTelegramWithErrorHandling endpoint = do
        res <- defaultRunBot (Token token) (bot endpoint)
        case res of
          Left err -> log' Error $ "Telegram endpoint failed with: " <> pack (show err)
          Right () -> return ()
