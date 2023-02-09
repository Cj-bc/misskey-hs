{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Web.Misskey.Type.Note (
  Note
  -- ** Lenses for Note
, note_id, note_createdAt, note_text, note_cw, note_userId
, note_user, note_replyId, note_renoteId, note_reply, note_renote
, note_viaMobile, note_isHidden, note_visibility, note_mentions
, note_visibleUserIds, note_fileIds, note_files, note_tags
, note_poll

) where
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON(..), Object(..), Value(..), (.:), (.:?), fieldLabelModifier)
import Data.Aeson.KeyMap (toHashMapText)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text as T
import Data.Time (UTCTime)
import Data.HashMap.Strict (HashMap)
import Data.Scientific (floatingOrInteger)
import Web.Misskey.Type.Common
import Web.Misskey.Type.File
import Web.Misskey.Type.Poll

data NoteVisibilities = Public | Home | Followers | Specified
    deriving (Show)

instance FromJSON NoteVisibilities where
    parseJSON (String s)     = case T.toLower s of
                                        "public"    -> return Public
                                        "home"      -> return Home
                                        "followers" -> return Followers
                                        "specified" -> return Specified
    parseJSON _              = fail "Unknown Visibility"



-- | Represents Reactions for Note
--
-- Key specify Emoji, and Value holds how many it has reacted.
--
-- I didn't use 'Emoji' as key because
--
-- -  It'll require "emojis" field of Note JSON object,
--    which prevent us from making standalone FromJSON instance for NoteReactions
--
-- -  It is inefficient as we have 'Emojis' field in 'Note' type and it'll be duplicated
data NoteReactions = NoteReactions (HashMap Text Integer)
    deriving (Show)

instance FromJSON NoteReactions where
    parseJSON (Object o) = NoteReactions <$> mapM f (toHashMapText o)
        where
            f :: Value -> Parser Integer
            f (Number i) = either (const $ fail "NoteReactions' number should be Integer, but got float") return $ floatingOrInteger i
            f e          = fail $ "Expected Number, but encounted " ++ show e
    parseJSON e = fail $ "Expected Object, but encounted " ++ show e

-- | A Note object
--
-- Docs: https://misskey.io/api-doc#operation/drive/files/show
data Note = Note { _note_id                 :: NoteId      -- ^ Original is 'id'
                 , _note_createdAt          :: UTCTime
                 , _note_text               :: Maybe String
                 , _note_cw                 :: Maybe String
                 , _note_userId             :: UserId
                 , _note_user               :: Maybe BaseUser
                 , _note_replyId            :: Maybe Id
                 , _note_renoteId           :: Maybe Id
                 , _note_reply              :: Maybe Note
                 , _note_renote             :: Maybe Note
                 , _note_viaMobile          :: Maybe Bool
                 , _note_isHidden           :: Maybe Bool
                 , _note_visibility         :: NoteVisibilities
                 , _note_mentions           :: Maybe [Id]
                 , _note_visibleUserIds     :: Maybe [Id]
                 , _note_fileIds            :: Maybe [Id]
                 , _note_files              :: Maybe [File]
                 , _note_tags               :: Maybe [String]
                 , _note_poll               :: Maybe Poll
                 , _note_reactions          :: Maybe NoteReactions -- TODO: Implement Reaction
                 } deriving (Show)

makeLenses ''Note

instance FromJSON Note where
    parseJSON (Object v) = Note <$> v .: "id"
                                <*> v `parseDateUnsafe` "createdAt"
                                <*> v .:? "text"
                                <*> v .:? "cw"
                                <*> v .:  "userId"
                                <*> v .:? "user"
                                <*> v .:? "replyId"
                                <*> v .:? "renoteId"
                                <*> v .:? "reply"
                                <*> v .:? "renote"
                                <*> v .:? "viaMobile"
                                <*> v .:? "isHidden"
                                <*> v .:  "visibility"
                                <*> v .:? "mentions"
                                <*> v .:? "visibleUserIds"
                                <*> v .:? "fileIds"
                                <*> v .:? "files"
                                <*> v .:? "tags"
                                <*> v .:? "poll"
                                <*> v .:? "reactions"
    parseJSON _          = fail "Failed to parse Note Object"
