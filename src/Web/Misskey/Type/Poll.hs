{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Web.Misskey.Type.Poll (
  PollChoice
, pollChoice_text
, pollChoice_votes
, pollChoice_isVoted

, Poll
, poll_multiple
, poll_expiresAt
, poll_choices
) where
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import Control.Lens (makeLenses)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:))
import Web.Misskey.Type.Common

-- | A choice for 'Poll'
--
-- This is used inside 'Poll' datatype
data PollChoice = PollChoice { _pollChoice_text     :: String
                             , _pollChoice_votes    :: Int
                             , _pollChoice_isVoted  :: Bool
                             } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''PollChoice)
makeLenses ''PollChoice


-- | A poll along with 'Note'
--
-- This is generated from raw API output so that might contain some mistakes
data Poll = Poll { _poll_multiple   :: Bool           -- ^ True if multiple voting is allowed
                 , _poll_expiresAt  :: Maybe UTCTime
                 , _poll_choices    :: [PollChoice]
                 } deriving (Show)

makeLenses ''Poll

instance FromJSON Poll where
    parseJSON (Object v) = Poll <$> v .: "multiple"
                                <*> v `parseDate` "expiresAt"
                                <*> v .: "choices"
    parseJSON _          = fail "Failed to parse Poll Object"

instance ToJSON Poll where
    toJSON _ = String "poll"
