{-# OPTIONS_GHC -Wno-orphans #-}
module Providers.Discord where

import Core
import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Data.Default
import Data.Function
import qualified Data.Text as T
import Discord
import qualified Discord.Types as DT
import qualified Discord.Requests as DR
import Optics.Operators
import Logging
import Providers.API
import Data.Maybe

log' :: Logger
log' = mkLog "Discord"

instance ProviderEndpoint Discord where
  spawnProviderEndpoint (Discord ProviderConfig {..}) = withNewEndpoint DiscordPT $ \endpoint ->
    void . async $ log' Error =<< runDiscord def { discordToken   = token
                                                 , discordOnStart = spawnSender endpoint
                                                 , discordOnEvent = eventHandler endpoint
                                                 }
    where
      eventHandler :: Endpoint -> DT.Event -> DiscordHandler ()
      eventHandler endpoint (DT.MessageCreate m) = do
        log' Info $ "Handling incoming message from: " <> (m & DT.messageAuthor & DT.userName)
        Right channelName <- fmap DT.channelName <$> getChannel cid
        let channel = Channel channelId channelName
        onMessageReceived endpoint Message { .. }
        where
          user       = UserRef . DT.userName . DT.messageAuthor $ m
          cid        = DT.messageChannel m
          channelId  = ChannelId $ T.pack . show $ cid
          content    = Just $ DT.messageText m
          getChannel = restCall . DR.GetChannel
          image      = listToMaybe $ ImageUrl . DT.attachmentUrl <$> DT.messageAttachments m
      eventHandler _ _ = return ()

      spawnSender :: Endpoint -> DiscordHandler ()
      spawnSender endpoint = void . async . forever $ do
        log' Info "Awaiting message dispatch..."
        (message, targetChannelId) <- awaitMessageDispatched endpoint
        let body      = formatMessage message
            channelId = read . T.unpack . unChannelId $ targetChannelId
        -- log' Info $ mconcat ["Dispatching ", body, " to ", showt targetChannelId]
        async $ do
          let createMessage = case message ^. #image of
                Just image -> do
                  let imageToUpload (ImageUrl   url  ) = DT.CreateEmbedImageUrl url
                      imageToUpload (ImageBytes bytes) = DT.CreateEmbedImageUpload bytes
                  DR.CreateMessageEmbed channelId body $ def { DT.createEmbedImage = Just $ imageToUpload image }
                Nothing -> DR.CreateMessage channelId body
          res <- restCall createMessage
          case res of
            Left  err -> log' Error $ "Discord publish failed with: " <> T.pack (show err)
            Right _   -> return ()

