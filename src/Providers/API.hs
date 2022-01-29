module Providers.API
( Endpoint(endpointProvider)
, ProviderEndpoint(..)
, withNewEndpoint
, publishMessage
, onMessagePublished
, onMessageReceived
, awaitMessageReceived
, awaitMessageDispatched
) where

import Core
import Config
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO, MonadIO)

data Endpoint = Endpoint
  { endpointProvider :: ProviderType
  , sender           :: TQueue (Message, ChannelId)
  , receiver         :: TQueue Message
  , publishResult    :: TMVar MessageID
  }

newEndpoint :: MonadIO m => ProviderType -> m Endpoint
newEndpoint providerType =
  liftIO . atomically $ Endpoint providerType <$> newTQueue <*> newTQueue <*> newEmptyTMVar

publishMessage :: MonadIO m => Endpoint -> Message -> ChannelId -> m MessageID
publishMessage endpoint message targetChannelId = do
  liftIO . atomically $ writeTQueue (sender endpoint) (message, targetChannelId)
  liftIO . atomically $ takeTMVar (publishResult endpoint)

onMessagePublished :: MonadIO m => Endpoint -> MessageID -> m ()
onMessagePublished endpoint = liftIO . atomically . putTMVar (publishResult endpoint)

onMessageReceived :: MonadIO m => Endpoint -> Message -> m ()
onMessageReceived endpoint = liftIO . atomically . writeTQueue (receiver endpoint)

awaitMessageReceived :: MonadIO m => Endpoint -> m Message
awaitMessageReceived = liftIO . atomically . readTQueue . receiver

awaitMessageDispatched :: MonadIO m => Endpoint -> m (Message, ChannelId)
awaitMessageDispatched = liftIO . atomically . readTQueue . sender

withNewEndpoint :: MonadIO m => ProviderType -> (Endpoint -> m ()) -> m Endpoint
withNewEndpoint providerType f = do
  endpoint <- newEndpoint providerType
  f endpoint
  return endpoint

class ProviderEndpoint config where
  spawnProviderEndpoint :: config -> IO Endpoint
