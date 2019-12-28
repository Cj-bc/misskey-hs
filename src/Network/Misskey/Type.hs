{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Misskey.Type where

import Lens.Simple
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust, isNothing)
import GHC.Generics


type Url = String
type UserId  = String
type NoteId = String
type Id = String


-- | Environment to execute misskey API
--
--TODO: Should I validate if URL is valid?
data MisskeyEnv = MisskeyEnv { _token :: String
                             , _url   :: Url
                             }
makeLenses ''MisskeyEnv


data APIErrorInfo = APIErrorInfo { param :: String
                                 , reason :: String
                                 } deriving (Show)

$(deriveJSON defaultOptions ''APIErrorInfo)

-- | Error response of all API
data APIError = APIError { code     :: String
                         , message  :: String
                         , id       :: String
                         , kind     :: Maybe String -- Undocumented
                         , info     :: Maybe APIErrorInfo -- Undocumented
                         } deriving (Show)

instance FromJSON APIError where
    parseJSON (Object v) = v .: "error" >>= parseError
        where
            parseError (Object v) = APIError <$> v .:  "code"
                                    <*> v .:  "message"
                                    <*> v .:  "id"
                                    <*> v .:? "kind"
                                    <*> v .:? "info"


-- | I can't find any documents. Also, Geo wasn't on any response
--
-- I'll fix this later, just leave this as placeholder
data Geo = Geo deriving (Show)

instance FromJSON Geo where
    parseJSON (Object _) = return Geo
    parseJSON _          = mempty


-- | A choice for Poll
--
-- This is used inside Poll datatype
data PollChoice = PollChoice { pollChoice_text     :: String
                             , pollChoice_votes    :: Int
                             , pollChoice_isVoted  :: Bool
                             } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 11 } ''PollChoice)


-- | A poll along with Note
--
-- This is generated from raw API output so that might contain some mistakes
data Poll = Poll { _poll_multiple   :: Bool           -- ^ True if multiple voting is allowed
                 , _poll_expiresAt  :: Maybe UTCTime
                 , _choices         :: [PollChoice]
                 } deriving (Show)

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
                 , _file_url        :: Maybe Url    -- ^ The URL of this Drive file.
                 , _file_folderId   :: Maybe Id     -- ^ The parent folder ID of this Drive file
                 , _isSensitive     :: Bool         -- ^ Whether this Drive file is sensitive.
                 } deriving (Show)

instance FromJSON File where
    parseJSON (Object v) = File <$> v .:  "id"
                                <*> (fromJust <$> parseISO8601 <$> v .: "createdAt")
                                <*> v .:  "name"
                                <*> v .:  "type"
                                <*> v .:  "md5"
                                <*> v .:  "size"
                                <*> v .:? "url"
                                <*> v .:? "folderId"
                                <*> v .:  "isSensitive"
    parseJSON _          = mempty


data PageContent = PageContent { pageContent_id         :: Id
                               , pageContent_var        :: Maybe n -- I don't know what is n
                               , pageContent_text       :: String
                               , pageContent_type       :: String
                               , pageContent_event      :: Maybe n
                               , pageContent_action     :: String
                               , pageContent_content    :: Maybe n
                               , pageContent_message    :: Maybe n
                               , pageContent_primary    :: Bool
                               }

data PageVariableArg = PageVariableArg { pageVArg_id    :: Id
                                       , pageVArg_type  :: String
                                       , pageVArg_value :: String
                                       }

data PageVariable = PageVariable { pageV_id   :: Id
                                 , pageV_args :: [PageVariableArg]
                                 , pageV_name :: String
                                 , pageV_type :: String
                                 , pageV_value :: Maybe n
                                 }

-- | Page
--
-- Docs: https://misskey.io/api-doc#operation/pages/show
data Page = Page { page_id  :: Id
                 , page_createdAt :: UTCTime
                 , page_updatedAt :: UTCTime
                 , page_title        :: String
                 , page_name         :: String
                 , page_summary      :: Maybe String
                 , page_content      :: [PageContent]
                 , page_variables    :: [PageVariable]
                 , page_userId       :: Id
                 , page_user         :: User
                 -- Those fields below are undocumented
                 , page_hideTitleWhenPinned :: Bool
                 , page_alignCenter         :: Bool
                 , page_font                :: String
                 , page_eyeCatchingImageId  :: Maybe String -- ^ TODO: Check whether this type correct
                 , page_eyeCatchingImage    :: Maybe String -- ^ TODO: Check whether this type correct
                 , page_attachedFiles       :: [File] -- ^ TODO: Check whether this type correct
                 , page_likedCount          :: Int
                 }

-- | A Note object
--
-- Docs: https://misskey.io/api-doc#operation/drive/files/show
data Note = Note { _note_id                 :: NoteId      -- ^ Original is 'id'
                 , _note_createdAt          :: UTCTime
                 , _note_text               :: Maybe String
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
                 } deriving (Show)

instance FromJSON Note where
    parseJSON (Object v) = Note <$> v .: "id"
                                <*> (fromJust <$> parseISO8601 <$> v.: "createdAt")
                                <*> v .:? "text"
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


data UserTwitterInfo = UserTwitterInfo { userTwitterInfo_id :: String
                                       , userTwitterInfo_screenName :: String}
                       deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''UserTwitterInfo)

-- | User object
--
-- Docs: https://misskey.io/api-doc#operation/users/show
data User = User { _user_id                      :: UserId -- ^ Original is 'id'
                 , _user_username                :: String
                 , _user_name                    :: Maybe String
                 , _user_url                     :: Maybe Url
                 , _user_avatarUrl               :: Maybe Url
                 , _user_avatarColor             :: Maybe String -- ^ This is documented as 'any' in doc
                 , _user_bannerUrl               :: Maybe Url
                 , _user_bannerColor             :: Maybe String -- ^ This is documented as 'any' in doc
                 , _user_emojis                  :: Maybe [String]     -- ^ This is documented as 'any'
                 , _user_host                    :: Maybe String
                 , _user_description             :: Maybe String
                 , _user_birthday                :: Maybe UTCTime
                 , _user_createdAt               :: Maybe UTCTime
                 , _user_updatedAt               :: Maybe UTCTime
                 , _user_location                :: Maybe String
                 , _user_followersCount          :: Maybe Int
                 , _user_followingCount          :: Maybe Int
                 , _user_notesCount              :: Maybe Int
                 , _user_isBot                   :: Maybe Bool
                 , _user_pinnedNoteIds           :: Maybe [NoteId]
                 , _user_pinnedNotes             :: Maybe [Note]
                 , _user_isCat                   :: Maybe Bool
                 , _user_isAdmin                 :: Maybe Bool
                 , _user_isModerator             :: Maybe Bool
                 , _user_isLocked                :: Maybe Bool
                 , _user_hasUnreadSpecifiedNotes :: Maybe Bool
                 , _user_hasUnreadMentions       :: Maybe Bool
                 -- Those fields below are undocumented
                 , _user_github                  :: Maybe a
                 , _user_twitter                 :: Maybe UserTwitterInfo
                 , _user_discord                 :: Maybe a
                 , _user_fields                  :: [a] -- I don't know what values are
                 , _user_twoFactorEnabled        :: Bool
                 , _user_usePasswordLessLogin    :: Bool
                 , _user_securityKeys            :: Bool
                 , _user_isSilenced              :: Bool
                 , _user_isSuspended             :: Bool
                 , _user_pinnedPage              :: Page
                 , _user_pinnedPageId            :: String
                 } deriving (Show)

instance FromJSON User where
    parseJSON (Object v) = User <$> v .:  "id"
                                <*> v .:  "username"
                                <*> v .:? "name"
                                <*> v .:? "url"
                                <*> v .:? "avatarUrl"
                                <*> v .:? "avatarColor"
                                <*> v .:? "bannerUrl"
                                <*> v .:? "bannerColor"
                                <*> v .:? "emojis"
                                <*> v .:? "host"
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