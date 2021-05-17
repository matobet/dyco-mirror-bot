module Main where

import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Core
import Data.Maybe
import Data.Text (Text)
import GHC.Exts
import Logging
import Main.Utf8 (withUtf8)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import Optics
import qualified Prometheus
import qualified Prometheus.Metric.GHC as Prometheus
import Providers.API
import Providers.Telegram ()
import Providers.Discord ()
import System.Environment
import TextShow
import Text.Read (readMaybe)

main :: IO ()
main = withUtf8 $ do
  setupLogging

  prometheusPort <- fromMaybe 9100 . (readMaybe =<<) <$> lookupEnv "DYCO_MIRROR_PROMETHEUS_PORT"
  void $ Prometheus.register Prometheus.ghcMetrics

  log' Info $ "Running Prometheus metrics endpoint on port: " <> showt prometheusPort
  void . async $ Warp.run prometheusPort Prometheus.metricsApp

  configFile <- fromMaybe "dyco-mirror.yml" <$> lookupEnv "DYCO_MIRROR_CONF"
  config@Config { name = botName, telegram = telegramConfig, discord = discordConfig, mirrors } <- readConfig configFile

  log' Info $ "Loaded config: " <> showt config

  telegram <- spawnProviderEndpoint telegramConfig
  discord  <- spawnProviderEndpoint discordConfig

  runMirrorMap botName mirrors
    [ (Telegram, (telegram, telegramConfig ^. #channels))
    , (Discord, (discord, discordConfig ^. #channels))
    ]

log' :: Logger
log' = mkLog "Mirror"

runMirrorMap :: Text -> [MirrorConfig] -> [(Provider, (Endpoint, [ChannelConfig]))] -> IO ()
runMirrorMap botName mirrors endpoints = do
  forConcurrently_ mirrorsBySource $ \(sourceProvider, mirrors) -> do
    case lookup sourceProvider endpoints of
      Nothing ->
        log' Error $ "Source provider " <> showt sourceProvider <> " not configured"

      Just (sourceEndpoint, _) -> forever $ do
        log' Info $ "Listening on " <> showt sourceProvider <> " events..."
        message <- awaitMessageReceived sourceEndpoint

        unless (message ^. #user % #name == botName) $ do -- skip messages from the bot itself
          let matchingMirrors = filter (\MirrorConfig{source = ChannelRef{channelName}} -> channelName == message ^. #channel % #name) mirrors
          log' Info $ "Matched mirrors: " <> showt matchingMirrors

          forM_ matchingMirrors $ \MirrorConfig{target = ChannelRef{provider = targetProvider, channelName = targetChannelName}} -> do
            case lookup targetProvider endpoints of
              Nothing ->
                log' Error $ mconcat [ "Target provider ", showt targetProvider, " not configured"]

              Just (targetEndpoint, channels) ->
                case channelIdByName targetChannelName channels of
                  Nothing -> log' Error $ mconcat ["Channel: ", showt targetChannelName, " not found in provider", showt targetProvider]
                  Just targetChannelId -> do
                    log' Info $ "Delegaing to provider " <> showt targetProvider <> " for dispatch"
                    publishMessage targetEndpoint message (ChannelId targetChannelId)
  where
    mirrorsBySource =
      [ (the sourceProvider, mirror)
      | mirror@MirrorConfig { source = ChannelRef { provider = sourceProvider } } <- mirrors
      , then group by sourceProvider using groupWith
      ]
