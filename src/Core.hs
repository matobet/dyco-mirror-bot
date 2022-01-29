module Core where

import Data.Maybe
import Data.Text
import Optics.TH
import Optics.Operators

import TextShow
import TextShow.Generic
import GHC.Generics

import qualified Data.ByteString as BS
import Config (ProviderType)

newtype UserRef = UserRef { name :: Text }
  deriving newtype TextShow

data Image = ImageUrl Text | ImageBytes BS.ByteString
  deriving Generic
  deriving TextShow via FromGeneric Image

data MessageID = MessageID
  { provider :: ProviderType
  , id       :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving TextShow via FromGeneric MessageID

data Message = Message
  { id      :: MessageID
  , user    :: UserRef
  , channel :: Channel
  , content :: Maybe Text
  , image   :: Maybe Image
  , replyTo :: Maybe MessageID
  }
  deriving Generic
  deriving TextShow via FromGeneric Message

formatMessage :: Message -> Text
formatMessage Message {..} =
  mconcat ["[#", channel ^. #name, "]: ", user ^. #name, ": ", fromMaybe "" content]

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving newtype TextShow

-- newtype ChannelName = ChannelName { unChannelName :: Text }
--   deriving TextShow

data Channel = Channel
  { id   :: ChannelId
  , name :: Text
  }
  deriving Generic
  deriving TextShow via FromGeneric Channel

makeFieldLabelsWith noPrefixFieldLabels ''UserRef
makeFieldLabelsWith noPrefixFieldLabels ''Message
makeFieldLabelsWith noPrefixFieldLabels ''MessageID
makeFieldLabelsWith noPrefixFieldLabels ''Channel
