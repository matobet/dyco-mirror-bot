{-# OPTIONS_GHC -Wno-orphans #-}
module Providers.Discord where

import Core
import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Data.Default
import Data.Function ((&))
import qualified Data.Text as T
import Discord
import qualified Discord.Types as DT
import qualified Discord.Requests as DR
import Optics.Operators
import Logging
import Providers.API
import Data.Maybe
import qualified Discord.Internal.Rest.Channel as DT
import Optics
import TextShow

log' :: Logger
log' = mkLog "Discord"

instance ProviderEndpoint Discord where
  spawnProviderEndpoint (Discord ProviderConfig {..}) = withNewEndpoint DiscordPT discordMain
    where
      discordMain endpoint = void . async $ log' Error =<< runDiscord def { discordToken   = token
                                                                          , discordOnStart = spawnSender
                                                                          , discordOnEvent = eventHandler
                                                                          }
        where
          eventHandler :: DT.Event -> DiscordHandler ()
          eventHandler (DT.MessageCreate m) = do
            log' Info $ "Handling incoming message from: " <> (m & DT.messageAuthor & DT.userName)
            Right channelName <- fmap DT.channelName <$> getChannel cid
            let channel = Channel channelId channelName
                msgType = DT.messageType m
            when (msgType == DT.MessageTypeDefault || msgType == DT.MessageTypeReply) $ do
              onMessageReceived endpoint Message { .. }
            where
              id         = messageIdFromDiscordMessage m
              user       = UserRef . DT.userName . DT.messageAuthor $ m
              cid        = DT.messageChannelId m
              channelId  = ChannelId $ T.pack . show $ cid
              content    = Just $ DT.messageContent m
              getChannel = restCall . DR.GetChannel
              image      = listToMaybe $ ImageUrl . DT.attachmentUrl <$> DT.messageAttachments m
              replyTo    = messageIdFromDiscordMessage <$> DT.messageReferencedMessage m
          eventHandler _ = return ()

          messageIdFromDiscordMessage = MessageID (endpointProvider endpoint) . T.pack . show . DT.messageId

          imageToUpload (ImageUrl   url  ) = DT.CreateEmbedImageUrl url
          imageToUpload (ImageBytes bytes) = DT.CreateEmbedImageUpload bytes

          spawnSender :: DiscordHandler ()
          spawnSender = void . async . forever $ do
            log' Info "Awaiting message dispatch..."
            (message, targetChannelId) <- awaitMessageDispatched endpoint
            log' Info $ mconcat ["Dispatching: ", showt message]
            let body      = formatMessage message
                channelId = read . T.unpack . unChannelId $ targetChannelId
            res <- restCall $ DR.CreateMessageDetailed
              channelId
              def
                { DT.messageDetailedContent   = body
                , DT.messageDetailedEmbed     = (message ^. #image) <&> imageToUpload <&> \image ->
                                                  def { DT.createEmbedImage = Just image }
                , DT.messageDetailedReference =
                  (message ^? #replyTo % _Just % #id)
                  <&> (read . T.unpack)
                  <&> (\referenceId -> def { DT.referenceMessageId = Just referenceId })
                }
            case res of
              Left  err -> log' Error $ "Discord publish failed with: " <> T.pack (show err) -- TODO: error handling to prevent DeadLock
              Right msg -> onMessagePublished endpoint $ messageIdFromDiscordMessage msg
