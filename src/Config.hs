module Config where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Yaml as Y
import GHC.Generics
import Optics.TH
import TextShow
import TextShow.Generic

data Config = Config
  { name :: Text,
    telegram :: TelegramConfig,
    discord :: DiscordConfig,
    mirrors :: [MirrorConfig]
  }
  deriving (Generic, FromJSON)
  deriving TextShow via FromGeneric Config

data TelegramConfig = TelegramConfig
  { token :: Text
  , channels :: [ChannelConfig]
  }
  deriving (Generic, FromJSON)
  deriving TextShow via FromGeneric TelegramConfig

data DiscordConfig = DiscordConfig
  { token :: Text
  , channels :: [ChannelConfig]
  }
  deriving (Generic, FromJSON)
  deriving TextShow via FromGeneric DiscordConfig

data ChannelConfig = ChannelConfig {name :: Text, id :: Text}
  deriving Generic
  deriving TextShow via FromGeneric ChannelConfig

channelIdByName :: Text -> [ChannelConfig] -> Maybe Text
channelIdByName name channels = lookup name $ (\ChannelConfig {..} -> (name, id)) <$> channels

instance {-# OVERLAPPING #-} FromJSON [ChannelConfig] where
  parseJSON (Object v) = mapM parseChannel $ HM.toList v
    where
      parseChannel (channel, String id) = return $ ChannelConfig channel id
      parseChannel _                    = fail "Expected a key-value String pair"
  parseJSON _ = fail "Expected an object"

data MirrorConfig = MirrorConfig {source :: ChannelRef, target :: ChannelRef}
  deriving (Generic, FromJSON, Eq)
  deriving TextShow via FromGeneric MirrorConfig

data Provider = Telegram | Discord
  deriving (Generic, FromJSON, Ord, Eq)
  deriving TextShow via FromGeneric Provider

data ChannelRef = ChannelRef {provider :: Provider, channelName :: Text}
  deriving (Generic, Eq)
  deriving TextShow via FromGeneric ChannelRef

instance FromJSON ChannelRef where
  parseJSON (String (splitOn "/" -> [provider, channelName])) =
    ChannelRef <$> parseProvider (toLower provider) <*> return channelName
    where
      parseProvider "telegram" = return Telegram
      parseProvider "discord"  = return Discord
      parseProvider _          = fail "Expected either <telegram> or <discord> chat provider"
  parseJSON _ = fail "Expected <provider/channel>"

readConfig :: String -> IO Config
readConfig = BS.readFile >=> Y.decodeThrow

makeFieldLabelsWith noPrefixFieldLabels ''Config
makeFieldLabelsWith noPrefixFieldLabels ''TelegramConfig
makeFieldLabelsWith noPrefixFieldLabels ''DiscordConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelConfig
makeFieldLabelsWith noPrefixFieldLabels ''MirrorConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelRef
