{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Type.Emoji (
  Emoji(..)
, 
) where

import RIO
import Web.Misskey.Type.Common
import Data.Text (Text)
import Control.Lens (makeLenses, makePrisms)

-- | Represents Emojis.
data Emoji = UnicodeEmoji Text -- ^ TODO: Should we decode it to Text?
           | CustomEmoji {
                  _customEmoji_id       :: Id       -- ^ The unique identifier for this Emoji.
                , _customEmoji_aliases  :: [Text]   -- ^ List to make it easier to be displayed as a candidate when entering emoji.
                , _customEmoji_name     :: Text     -- ^ Official name of custom emoji.
                , _customEmoji_category :: Text     -- ^ Names categorized in the emoji list.
                , _customEmoji_host     :: Text     -- ^ If it is another server, the FQDN will be returned here.
                , _customEmoji_url      :: Url      -- ^ Image URL of Emoji
                } -- ^ Emojis that is defined in each instance.
           | PopulatedCustomEmoji {
                  _populatedCustomEmoji_name  :: Text -- ^ Official name of custom emoji.
                , _populatedCustomEmoji_url   :: Url  -- ^ Image URL of Emoji
                } -- ^ Custom Emojis that is populated to Notes
    deriving (Show)

makeLenses ''Emoji
makePrisms ''Emoji
