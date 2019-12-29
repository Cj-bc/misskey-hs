{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Network.Misskey.Type
Description : Type definitions for misskey-hs
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Common Data types for misske-hs
-}
module Network.Misskey.Type where

import Lens.Simple
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson
import Data.Aeson.Types
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


-- APIError {{{
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
-- }}}

-- Misskey
type Misskey res = ReaderT MisskeyEnv IO (Either APIError res)
runMisskey = runReaderT


-- Poll {{{

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
                 -- , _poll_expiresAt  :: Maybe UTCTime
                 , _poll_expiresAt  :: String -- This is temporary set to String
                 , _choices         :: [PollChoice]
                 } deriving (Show)

instance FromJSON Poll where
    parseJSON (Object v) = Poll <$> v .: "multiple"
                                -- <*> v `parseData` "expiresAt"
                                <*> v .: "expiresAt"
                                <*> v .: "choices"
    parseJSON _          = mempty
-- }}}

-- File {{{

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
                                <*> (fromJust . parseISO8601 <$> (v .: "createdAt"))
                                <*> v .:  "name"
                                <*> v .:  "type"
                                <*> v .:  "md5"
                                <*> v .:  "size"
                                <*> v .:? "url"
                                <*> v .:? "folderId"
                                <*> v .:  "isSensitive"
    parseJSON _          = mempty
-- }}}

-- Page {{{


-- TODO: Implement this
type PageContentType = String

data PageContent = PageContentText { pageContent_id   :: Id
                                   , pageContent_text :: String
                                   }
                 | PageContentSection { pageContent_id       :: Id
                                      , pageContent_title    :: String
                                      , pageContent_children :: [PageContent]
                                      }
                 | PageContentImage { pageContent_id     :: Id
                                    , pageContent_fileId :: Id
                                    }
                 | PageContentTextArea { pageContent_id   :: Id
                                       , pageContent_text :: String
                                       }
                 | PageContentButton { pageContent_id      :: Id
                                     , pageContent_var     :: Maybe String
                                     , pageContent_text    :: String
                                     , pageContent_event   :: Maybe String  -- TODO: Check those type
                                     , pageContent_action  :: Maybe String  -- TODO: Check those type
                                     , pageContent_content :: Maybe String  -- TODO: Check those type
                                     , pageContent_message :: Maybe String  -- TODO: Check those type
                                     , pageContent_primary :: Bool
                                     }
                 | PageContentRadioButton { pageContent_id       :: Id
                                          , pageContent_name     :: String
                                          , pageContent_title    :: String
                                          , pageContent_values   :: String
                                          , pageContent_defuault :: Maybe String
                                          }
                 | PageContentTextInput { pageContent_id       :: Id
                                        , pageContent_name     :: String
                                        , pageContent_text     :: String
                                        , pageContent_defuault :: Maybe String
                                        }
                 | PageContentTextAreaInput { pageContent_id       :: Id
                                            , pageContent_name     :: String
                                            , pageContent_text     :: String
                                            , pageContent_defuault :: Maybe String
                                            }
                 | PageContentSwitch { pageContent_id      :: Id
                                     , pageContent_name    :: String
                                     , pageContent_text    :: String
                                     , pageContent_default :: Maybe String
                                     }
                 | PageContentCounter { pageContent_id   :: Id
                                      , pageContent_name :: String
                                      , pageContent_text :: String
                                      , pageContent_inc  :: Maybe String
                                      }
                 | PageContentIf { pageContent_id       :: Id
                                 , pageContent_var      :: Maybe String
                                 , pageContent_children :: [PageContent]
                                 }
                 | PageContentPost { pageContent_id   :: Id
                                   , pageContent_text :: String
                                   }
                    deriving (Show)

instance FromJSON PageContent where
    parseJSON (Object v) = do
        (ctype :: String) <- v .: "type"
        littleParser ctype
        where
            littleParser "text"          = PageContentText          <$> v .: "id"       <*> v .: "text"
            littleParser "section"       = PageContentSection       <$> v .: "id"       <*> v .: "title"
                                                                    <*> v .: "children"
            littleParser "image"         = PageContentImage         <$> v .: "id"       <*> v .: "fileId"
            littleParser "textarea"      = PageContentTextArea      <$> v .: "id"       <*> v .: "text"
            littleParser "button"        = PageContentButton        <$> v .: "id"       <*> v .:? "var"
                                                                    <*> v .: "text"     <*> v .:? "event"
                                                                    <*> v .:? "action"  <*> v .:? "content"
                                                                    <*> v .:? "message" <*> v .: "primary"
            littleParser "radioButton"   = PageContentRadioButton   <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "title"    <*> v .: "values"
                                                                    <*> v .:? "default"
            littleParser "textInput"     = PageContentTextInput     <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "textareaInput" = PageContentTextAreaInput <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "switch"        = PageContentSwitch        <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "counter"       = PageContentCounter       <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "inc"
            littleParser "if"            = PageContentIf            <$> v .: "id"       <*> v .:? "var"
                                                                    <*> v .: "children"
            littleParser "post"          = PageContentPost          <$> v .: "id"       <*> v .: "text"
    -- We do not expect a non-Object value here.
    -- We could use empty to fail, but typeMismatch
    -- gives a much more informative error message.
    parseJSON invalid    =
        prependFailure "parsing PageContent failed, "
            (typeMismatch "Object" invalid)


data PageVariableArg = PageVariableArg { pageVArg_id    :: Id
                                       , pageVArg_type  :: String
                                       , pageVArg_value :: String
                                       } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''PageVariableArg)

-- TODO: Implement this
--
-- This is temporary set to 'String', though it should be enum
-- To Implement this, I should know all sort of variable type
type PageVariableType = String

-- | Page variable that can be declared for each page
data PageVariable = PageVariable { pageV_id   :: Id
                                 , pageV_args :: [PageVariableArg]
                                 , pageV_name :: String
                                 , pageV_type :: PageVariableType
                                 , pageV_value :: Maybe String -- TODO: what type is this?
                                 } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''PageVariable)

-- | Page
--
-- Docs:
--
--   * https://misskey.io/api-doc#operation/pages/show
--
--   * https://join.misskey.page/ja/wiki/usage/pages
data Page = Page { page_id  :: Id
                 , page_createdAt :: UTCTime
                 , page_updatedAt :: UTCTime
                 , page_title        :: String
                 , page_name         :: String
                 , page_summary      :: Maybe String
                 , page_content      :: [PageContent]
                 , page_variables    :: [PageVariable]
                 , page_userId       :: Id
                 , page_user         :: BaseUser
                 -- Those fields below are undocumented
                 , page_hideTitleWhenPinned :: Bool
                 , page_alignCenter         :: Bool
                 , page_font                :: String
                 , page_eyeCatchingImageId  :: Maybe String -- ^ TODO: Check whether this type correct
                 , page_eyeCatchingImage    :: Maybe String -- ^ TODO: Check whether this type correct
                 , page_attachedFiles       :: [File] -- ^ TODO: Check whether this type correct
                 , page_likedCount          :: Int
                 } deriving (Show)


instance FromJSON Page where
    parseJSON (Object v) = Page <$> v .:  "id"
                                <*> (fromJust . parseISO8601 <$> (v .: "createdAt"))
                                <*> (fromJust . parseISO8601 <$> (v .: "updatedAt"))
                                <*> v .:  "title"
                                <*> v .:  "name"
                                <*> v .:? "summary"
                                <*> v .:  "content"
                                <*> v .:  "variables"
                                <*> v .:  "userId"
                                <*> v .:  "user"
                                <*> v .:  "hideTitleWhenPinned"
                                <*> v .:  "alignCenter"
                                <*> v .:  "font"
                                <*> v .:? "eyeCatchingImageId"
                                <*> v .:? "eyeCatchingImage"
                                <*> v .:  "attachedFiles"
                                <*> v .:  "likedCount"
--- }}}

-- Note {{{

-- | I can't find any documents. Also, Geo wasn't on any response
--
-- I'll fix this later, just leave this as placeholder
data Geo = Geo deriving (Show)

instance FromJSON Geo where
    parseJSON (Object _) = return Geo
    parseJSON _          = mempty


-- | Subset of 'User' that isn't depend on 'Note'/'Page'/Poll' etc
--
-- This is used instead of 'User' in 'Note'/'Page'/Poll'
--
-- *Caution* This definition is made from API result so might contain some mistakes
data BaseUser = BaseUser { _baseUser_id          :: String
                         , _baseUser_name        :: String
                         , _baseUser_username    :: String
                         , _baseUser_host        :: Maybe String
                         , _baseUser_avatarUrl   :: String
                         , _baseUser_avatarColor :: String
                         , _baseUser_isCat       :: Bool
                         , _baseUser_emojis      :: [String]
                         } deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''BaseUser)

-- | A Note object
--
-- Docs: https://misskey.io/api-doc#operation/drive/files/show
data Note = Note { _note_id                 :: NoteId      -- ^ Original is 'id'
                 , _note_createdAt          :: UTCTime
                 , _note_text               :: Maybe String
                 , _note_cw                 :: Maybe String
                 , _note_userId             :: UserId
                 , _note_user               :: BaseUser
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
                                <*> (fromJust . parseISO8601 <$> (v.: "createdAt"))
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

--- }}}

-- User {{{

data UserTwitterInfo = UserTwitterInfo { userTwitterInfo_id :: String
                                       , userTwitterInfo_screenName :: String}
                       deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''UserTwitterInfo)

-- TODO: Implement this
--
-- This seems to be `rgb(xxx,yyy,zzz)` which can be parsed
type Color = String

-- | User object
--
-- Docs: https://misskey.io/api-doc#operation/users/show
data User = User { _user_id                      :: UserId
                 , _user_username                :: String
                 , _user_name                    :: Maybe String
                 , _user_url                     :: Maybe Url
                 , _user_avatarUrl               :: Maybe Url
                 , _user_avatarColor             :: Maybe Color -- ^ This is documented as 'any' in doc
                 , _user_bannerUrl               :: Maybe Url
                 , _user_bannerColor             :: Maybe Color -- ^ This is documented as 'any' in doc
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
                 , _user_hasUnreadSpecifiedNotes :: Maybe Bool -- This doesn't exist
                 , _user_hasUnreadMentions       :: Maybe Bool -- This doesn't exist
                 -- Those fields below are undocumented
                 -- I don't know exact type(especially if it's 'Maybe' or not)
                 , _user_github                  :: Maybe String   -- This is temporary set to String
                 , _user_twitter                 :: Maybe UserTwitterInfo
                 , _user_discord                 :: Maybe String   -- This is temporary set to String
                 , _user_fields                  :: Maybe [String] -- This is temporary set to String
                 , _user_twoFactorEnabled        :: Maybe Bool
                 , _user_usePasswordLessLogin    :: Maybe Bool
                 , _user_securityKeys            :: Maybe Bool
                 , _user_isSilenced              :: Maybe Bool
                 , _user_isSuspended             :: Maybe Bool
                 , _user_pinnedPage              :: Maybe Page
                 , _user_pinnedPageId            :: Maybe String
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
                                <*> v .:? "github"
                                <*> v .:? "twitter"
                                <*> v .:? "discord"
                                <*> v .:? "fields"
                                <*> v .:? "twoFactorEnabled"
                                <*> v .:? "usePasswordLessLogin"
                                <*> v .:? "securityKeys"
                                <*> v .:? "isSilenced"
                                <*> v .:? "isSuspended"
                                <*> v .:? "pinnedPage"
                                <*> v .:? "pinnedPageId"
    parseJSON _          = mempty

--- }}}

parseData v s = do
    b <- v .:? s
    if isNothing b
    then return Nothing
    else return $ parseISO8601 $ fromJust b
