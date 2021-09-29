module Core where

import Data.Maybe
import Data.Text
import Optics.TH
import Optics.Operators

import TextShow
import TextShow.Generic
import GHC.Generics

import qualified Data.ByteString as BS

newtype UserRef = UserRef { name :: Text }
  deriving newtype TextShow

data Image = ImageUrl Text | ImageBytes BS.ByteString
  deriving Generic
  deriving TextShow via FromGeneric Image

data Message = Message
  { user    :: UserRef
  , channel :: Channel
  , content :: Maybe Text
  , image   :: Maybe Image
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
makeFieldLabelsWith noPrefixFieldLabels ''Channel
