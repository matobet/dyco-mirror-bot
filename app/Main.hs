module Main where

import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Core
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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
import Data.Foldable (find)
import System.Exit
import Control.Concurrent (threadDelay)

data Command
  = Quit
  | Rollback
  deriving Show

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
    [ (telegram, unTelegram telegramConfig)
    , (discord, unDiscord discordConfig)
    ]

log' :: Logger
log' = mkLog "Mirror"

runMirrorMap :: Text -> [MirrorConfig] -> [(Endpoint, ProviderConfig)] -> IO ()
runMirrorMap botName mirrors endpoints = do
  forConcurrently_ mirrorsBySource $ \(sourceProvider, sourceMirrors) -> do
    case lookupEndpoint sourceProvider endpoints of
      Nothing ->
        log' Error $ "Source provider " <> showt sourceProvider <> " not configured"

      Just (sourceEndpoint, sourceConfig) -> forever $ do
        log' Info $ "Listening on " <> showt sourceProvider <> " events..."
        message <- awaitMessageReceived sourceEndpoint

        let senderName = message ^. #user % #name
        let isAdmin = senderName `elem` sourceConfig ^. #admins
        let isBot = senderName == botName
        let mCommand = parseCommand =<< message ^. #content

        case (isAdmin, mCommand) of
          (True, Just Quit) -> do
            log' Info "Received QUIT command, exiting."
            later exitSuccess

          (True, Just Rollback) -> do
            log' Info "Received ROLLBACK command, exiting with magic exit code 42."
            later . exitWith $ ExitFailure 42 -- magic error code to signal rollback

          _ -> unless isBot $ do -- skip messages from the bot itself
            let matchingMirrors = filter (\MirrorConfig{source = ChannelRef{channelName}} -> channelName == message ^. #channel % #name) sourceMirrors
            log' Info $ "Matched mirrors: " <> showt matchingMirrors

            forM_ matchingMirrors $ \MirrorConfig{target = ChannelRef{provider = targetProvider, channelName = targetChannelName}} -> do
              case lookupEndpoint targetProvider endpoints of
                Nothing ->
                  log' Error $ mconcat ["Target provider ", showt targetProvider, " not configured"]

                Just (targetEndpoint, targetConfig) ->
                  case channelIdByName targetChannelName (targetConfig ^. #channels) of
                    Nothing -> log' Error $ mconcat ["Channel: ", showt targetChannelName, " not found in provider", showt targetProvider]
                    Just targetChannelId -> do
                      log' Info $ mconcat ["Delegaing to provider ", showt targetProvider, " for dispatch"]
                      publishMessage targetEndpoint message (ChannelId targetChannelId)
  where
    lookupEndpoint providerType = find $ (== providerType) . endpointProvider . fst

    parseCommand :: Text -> Maybe Command
    parseCommand text = do
      guard $ mempty /= text
      guard $ T.head text `elem` ['/', '!']
      case T.toLower $ T.tail text of
        "quit" -> pure Quit
        "rollback" -> pure Rollback
        _ -> Nothing

    mirrorsBySource =
      [ (the sourceProvider, mirror)
      | mirror@MirrorConfig { source = ChannelRef { provider = sourceProvider } } <- mirrors
      , then group by sourceProvider using groupWith
      ]

    -- give Telegram client chance to sync message offset
    -- (otherwise we would keep getting the /quit command unpon each bot restart)
    later m = threadDelay 1000001 >> m
