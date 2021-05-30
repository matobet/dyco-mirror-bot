module Providers.Discord where

import Core
import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Data.Function
import Data.Text as T
import Discord
import qualified Discord.Types as DT
import qualified Discord.Requests as DR
import Logging
import Providers.API
import TextShow

log' :: Logger
log' = mkLog "Discord"

instance ProviderEndpoint Discord where
  spawnProviderEndpoint (Discord ProviderConfig {..}) = withNewEndpoint DiscordPT $ \endpoint -> void . async $ log' Error =<< runDiscord def
    { discordToken   = token
    , discordOnStart = spawnSender endpoint
    , discordOnEvent = eventHandler endpoint
    }
    where
      eventHandler :: Endpoint -> DT.Event -> DiscordHandler ()
      eventHandler endpoint (DT.MessageCreate m) = do
        log' Info $ "Handling incoming message from: " <> (m & DT.messageAuthor & DT.userName)
        Right channelName <- fmap DT.channelName <$> getChannel cid
        let channel = Channel channelId channelName
        when (content /= "") $ onMessageReceived endpoint Message { .. } -- ignore images for now
        where
          user       = UserRef . DT.userName . DT.messageAuthor $ m
          cid        = DT.messageChannel m
          channelId  = ChannelId $ T.pack . show $ cid
          content    = DT.messageText m
          getChannel = restCall . DR.GetChannel
      eventHandler _ _ = return ()

      spawnSender :: Endpoint -> DiscordHandler ()
      spawnSender endpoint = void . async . forever $ do
        log' Info "Awaiting message dispatch..."
        (message, targetChannelId) <- awaitMessageDispatched endpoint
        let body      = formatMessage message
            channelId = read . T.unpack . unChannelId $ targetChannelId
        log' Info $ mconcat ["Dispatching ", body, " to ", showt targetChannelId]
        async $ do
          res <- restCall $ DR.CreateMessage channelId body
          case res of
            Left err -> log' Error $ "Discord publish failed with: " <> pack (show err)
            Right _ -> return ()

