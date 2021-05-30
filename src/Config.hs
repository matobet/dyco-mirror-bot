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
    telegram :: Telegram,
    discord :: Discord,
    mirrors :: [MirrorConfig]
  }
  deriving (Generic, FromJSON)
  deriving TextShow via FromGeneric Config

data ProviderConfig = ProviderConfig
  { token :: Text
  , admins :: [Text]
  , channels :: [ChannelConfig]
  }
  deriving (Generic, FromJSON)
  deriving TextShow via FromGeneric ProviderConfig

newtype Telegram = Telegram { unTelegram :: ProviderConfig }
  deriving newtype (FromJSON)
  deriving stock Generic
  deriving TextShow via FromGeneric Telegram

newtype Discord = Discord { unDiscord :: ProviderConfig }
  deriving newtype (FromJSON)
  deriving stock Generic
  deriving TextShow via FromGeneric Discord

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

data ProviderType = TelegramPT | DiscordPT
  deriving (Generic, FromJSON, Ord, Eq)
  deriving TextShow via FromGeneric ProviderType

data ChannelRef = ChannelRef {provider :: ProviderType, channelName :: Text}
  deriving (Generic, Eq)
  deriving TextShow via FromGeneric ChannelRef

instance FromJSON ChannelRef where
  parseJSON (String (splitOn "/" -> [provider, channelName])) =
    ChannelRef <$> parseProvider (toLower provider) <*> return channelName
    where
      parseProvider "telegram" = return TelegramPT
      parseProvider "discord"  = return DiscordPT
      parseProvider _          = fail "Expected either <telegram> or <discord> chat provider"
  parseJSON _ = fail "Expected <provider/channel>"

readConfig :: String -> IO Config
readConfig = BS.readFile >=> Y.decodeThrow

makeFieldLabelsWith noPrefixFieldLabels ''Config
makeFieldLabelsWith noPrefixFieldLabels ''ProviderConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelConfig
makeFieldLabelsWith noPrefixFieldLabels ''MirrorConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelRef
