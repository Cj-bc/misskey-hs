{-# LANGUAGE OverloadedStrings #-}

module Network.Misskey.Type where

import Data.ByteString
import Data.Aeson
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust, isNothing)


type Url = String
type UserId  = String
type NoteId = String
type Id = String

-- | I can't find any documents as Geo wasn't on any response
--
-- I'll fix this later, just leave this as placeholder
data Geo = Geo

instance FromJSON Geo where
    parseJSON (Object _) = return Geo
    parseJSON _          = mempty


-- | A choice for Poll
data PollChoice = PollChoice { _pollChoice_text     :: String
                             , _pollChoice_votes    :: Int
                             , _isVoted             :: Bool
                             }

instance FromJSON PollChoice where
    parseJSON (Object v) = PollChoice <$> v .: "text"
                                      <*> v .: "votes"
                                      <*> v .: "isVoted"
    parseJSON _          = mempty

-- | A poll along with Note
--
-- This is generated from raw API output so that might contain some mistakes
data Poll = Poll { _poll_multiple :: Bool
                 , _poll_expiresAt  :: Maybe UTCTime
                 , _choices         :: [PollChoice]
                 }

instance FromJSON Poll where
    parseJSON (Object v) = Poll <$> v .: "multiple"
                                <*> v `parseData` "expiresAt"
                                <*> v .: "choices"
    parseJSON _          = mempty


-- | A File
--
-- Docs: https://misskey.io/api-doc#operation/drive/files/show
data File = File { _file_id         :: Id           -- ^ Unique identifier for this drive file.
                 , _file_createdAt  :: UTCTime      -- ^ The date that the Drive file was created on Misskey.
                 , _file_name       :: String       -- ^ The file name with extension.
                 , _file_type       :: String       -- ^ The MIME type of this Drive file.
                 , _file_md5        :: String       -- ^ The MD5 hash of this Drive file.
                 , _file_size       :: Int          -- ^ The size of this Drive file. (bytes)
                 , _file_url        :: Url          -- ^ The URL of this Drive file.
                 , _file_folderId   :: Id           -- ^ The parent folder ID of this Drive file
                 , _isSensitive     :: Bool         -- ^ Whether this Drive file is sensitive.
                 }

instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "id"
                                <*> (fromJust <$> parseISO8601 <$> v .: "createdAt")
                                <*> v .: "name"
                                <*> v .: "type"
                                <*> v .: "md5"
                                <*> v .: "size"
                                <*> v .: "url"
                                <*> v .: "folderId"
                                <*> v .: "isSensitive"
    parseJSON _          = mempty

-- | A Note object
--
-- Docs: https://misskey.io/api-doc#operation/drive/files/show
data Note = Note { _note_id                 :: NoteId      -- ^ Original is 'id'
                 , _note_createdAt          :: UTCTime
                 , _note_text               :: String
                 , _note_cw                 :: Maybe String
                 , _note_userId             :: UserId
                 , _note_user               :: User
                 , _note_replyId            :: Maybe Id
                 , _note_renoteId           :: Maybe Id
                 , _note_reply              :: Maybe Note
                 , _note_renote             :: Maybe Note
                 , _note_viaMobile          :: Maybe Bool
                 , _note_isHidden           :: Maybe Bool
                 , _note_visibility         :: String
                 , _note_mentions           :: Maybe [Id]
                 , _note_visibleUserIds     :: Maybe [Id]
                 , _note_fileIds            :: Maybe [Id]
                 , _note_files              :: Maybe [File]
                 , _note_tags               :: Maybe [String]
                 , _note_poll               :: Maybe Poll
                 , _note_geo                :: Maybe Geo
                 }

instance FromJSON Note where
    parseJSON (Object v) = Note <$> v .: "id"
                                <*> (fromJust <$> parseISO8601 <$> v.: "createdAt")
                                <*> v .:  "text"
                                <*> v .:? "cw"
                                <*> v .:  "userId"
                                <*> v .:  "user"
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
                                <*> v .:? "geo"
    parseJSON _          = mempty



-- | User object
--
-- Docs: https://misskey.io/api-doc#operation/users/show
data User = User { _id                      :: UserId -- ^ Original is 'id'
                 , _username                :: String
                 , _name                    :: String
                 , _url                     :: Maybe Url
                 , _avatarUrl               :: Url
                 , _avatarColor             :: String -- ^ This is documented as 'any' in doc
                 , _bannerUrl               :: Maybe Url
                 , _bannerColor             :: Maybe String -- ^ This is documented as 'any' in doc
                 , _emojis                  :: [String]     -- ^ This is documented as 'any'
                 , _host                    :: String
                 , _description             :: Maybe String
                 , _birthday                :: Maybe UTCTime
                 , _createdAt               :: Maybe UTCTime
                 , _updatedAt               :: Maybe UTCTime
                 , _location                :: Maybe String
                 , _followersCount          :: Maybe Int
                 , _followingCount          :: Maybe Int
                 , _notesCount              :: Maybe Int
                 , _isBot                   :: Maybe Bool
                 , _pinnedNoteIds           :: Maybe [NoteId]
                 , _pinnedNotes             :: Maybe [Note]
                 , _isCat                   :: Maybe Bool
                 , _isAdmin                 :: Maybe Bool
                 , _isModerator             :: Maybe Bool
                 , _isLocked                :: Maybe Bool
                 , _hasUnreadSpecifiedNotes :: Maybe Bool
                 , _hasUnreadMentions       :: Maybe Bool
                 }

instance FromJSON User where
    parseJSON (Object v) = User <$> v .:  "id"
                                <*> v .:  "username"
                                <*> v .:  "name"
                                <*> v .:? "url"
                                <*> v .:  "avatarUrl"
                                <*> v .:  "avatarColor"
                                <*> v .:? "bannerUrl"
                                <*> v .:? "bannerColor"
                                <*> v .:  "emojis"
                                <*> v .:  "host"
                                <*> v .:? "description"
                                <*> v `parseData` "birthday"
                                <*> v `parseData` "createdAt"
                                <*> v `parseData` "updatedAt"
                                <*> v .:? "location"
                                <*> v .:? "followersCount"
                                <*> v .:? "followingCount"
                                <*> v .:? "notesCount"
                                <*> v .:? "isBot"
                                <*> v .:? "pinnedNoteIds"
                                <*> v .:? "pinnedNotes"
                                <*> v .:? "isCat"
                                <*> v .:? "isAdmin"
                                <*> v .:? "isModerator"
                                <*> v .:? "isLocked"
                                <*> v .:? "hasUnreadSpecifiedNotes"
                                <*> v .:? "hasUnreadMentions"
    parseJSON _          = mempty


parseData v s = do
    b <- v .:? s
    if isNothing b
    then return Nothing
    else return $ parseISO8601 $ fromJust b
