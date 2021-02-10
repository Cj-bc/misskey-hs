{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Web.Misskey.Type
Description : Type definitions for misskey-hs
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Common Data types for misske-hs
-}
module Web.Misskey.Type (
  -- * Common type synonym
  Id
, Url
  -- * Library basic types
, MisskeyEnv(..)
, Misskey
, runMisskey
, APIError
  -- * Types corresponds to each Misskey data
, Poll
, File
, Page
, Note
, User
, Geo(..)

  -- * Lenses
  -- ** Lenses for BaseUser
, baseUser_id, baseUser_name, baseUser_username, baseUser_host
, baseUser_avatarUrl, baseUser_isCat
, baseUser_emojis

  -- ** Lenses for Note
, note_id, note_createdAt, note_text, note_cw, note_userId
, note_user, note_replyId, note_renoteId, note_reply, note_renote
, note_viaMobile, note_isHidden, note_visibility, note_mentions
, note_visibleUserIds, note_fileIds, note_files, note_tags
, note_poll, note_geo

  -- ** Lenses for Page
, page_id, page_createdAt, page_updatedAt, page_title, page_name
, page_summary, page_content, page_variables, page_userId, page_user
, page_hideTitleWhenPinned, page_alignCenter, page_font
, page_eyeCatchingImageId, page_eyeCatchingImage, page_attachedFiles
, page_likedCount

  -- ** Lenses for PageVArg
, pageVArg_id, pageVArg_type, pageVArg_value

  -- ** Lenses for PageV
, pageV_id, pageV_args, pageV_name, pageV_type, pageV_value

  -- ** Lenses for User
, user_id, user_username, user_name, user_url, user_avatarUrl
, user_bannerUrl, user_emojis
, user_host, user_description, user_birthday, user_createdAt
, user_updatedAt, user_location, user_followersCount, user_followingCount
, user_notesCount, user_isBot, user_pinnedNoteIds, user_pinnedNotes
, user_isCat, user_isAdmin, user_isModerator, user_isLocked
, user_hasUnreadSpecifiedNotes, user_hasUnreadMentions, user_github
, user_twitter, user_discord, user_fields, user_twoFactorEnabled
, user_usePasswordLessLogin, user_securityKeys, user_isSilenced
, user_isSuspended, user_pinnedPage, user_pinnedPageId
) where

import Control.Lens
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad ((=<<))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust, isNothing)
import GHC.Generics
import Data.Text (Text)


type Url = String
type UserId  = String
type NoteId = String
type Id = String

parseData :: Object -> Text -> Parser (Maybe UTCTime)
parseData v s = do
    b <- v .:? s :: Parser (Maybe String)
    return $ parseISO8601 =<< b


-- | Environment to execute misskey API
--
-- TODO: Should I validate if URL is valid?
data MisskeyEnv = MisskeyEnv { _misskeyEnvToken :: String
                             , _misskeyEnvUrl   :: Url
                             }

makeLenses ''MisskeyEnv

-- APIError {{{
data APIErrorInfo = APIErrorInfo { _APIErrorInfoParam :: String
                                 , _APIErrorInfoReason :: String
                                 } deriving (Show)

$(deriveJSON defaultOptions ''APIErrorInfo)
makeLenses ''APIErrorInfo

-- | Error response of all API
-- 
-- This value will be returned from each API caller if API returns error
data APIError = APIError { _APIErrorCode     :: String
                         , _APIErrorMessage  :: String
                         , _APIErrorId       :: String
                         , _APIErrorKind     :: Maybe String -- Undocumented
                         , _APIErrorInfo     :: Maybe APIErrorInfo -- Undocumented
                         } deriving (Show)
makeLenses ''APIError

instance FromJSON APIError where
    parseJSON (Object v) = v .: "error" >>= parseError
        where
            parseError (Object v) = APIError <$> v .:  "code"
                                    <*> v .:  "message"
                                    <*> v .:  "id"
                                    <*> v .:? "kind"
                                    <*> v .:? "info"
-- }}}

-- | Misskey monad
--
-- It's just Reader monad with MisskeyEnv
type Misskey res = ReaderT MisskeyEnv IO (Either APIError res)
runMisskey = runReaderT


-- | Subset of 'User' that isn't depend on 'Note' \/ 'Page' \/ 'Poll' etc
--
-- This is used instead of 'User' in 'Note' \/ 'Page' \/ 'Poll'
--
-- __Caution__: This definition is made from API result so might contain some mistakes
data BaseUser = BaseUser { _baseUser_id          :: String
                         , _baseUser_name        :: Maybe String
                         , _baseUser_username    :: String
                         , _baseUser_host        :: Maybe String
                         , _baseUser_avatarUrl   :: Maybe String
                         , _baseUser_isCat       :: Maybe Bool
                         , _baseUser_emojis      :: [String]
                         } deriving (Show)


$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''BaseUser)
makeLenses ''BaseUser


-- Poll {{{

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
                 , _choices         :: [PollChoice]
                 } deriving (Show)

makeLenses ''Poll

instance FromJSON Poll where
    parseJSON (Object v) = Poll <$> v .: "multiple"
                                <*> v `parseData` "expiresAt"
                                <*> v .: "choices"
    parseJSON _          = mempty

instance ToJSON Poll where
    toJSON _ = String "poll"
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

makeLenses ''File

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

-- | A Enum of page content
--
-- This is mainly used in 'Page' to represent page content
--
-- Those values are manually picked up from the Page making webpage
-- (https://<your-domain>/i/pages/new) by me.
--
-- So It might have some mistakes.
--
-- TODO: Generate Prism for this data type
data PageContent = PageContentText { _pageContent_id   :: Id
                                   , _pageContent_text :: String
                                   }
                 | PageContentSection { _pageContent_id       :: Id
                                      , _pageContent_title    :: String
                                      , _pageContent_children :: [PageContent]
                                      }
                 | PageContentImage { _pageContent_id     :: Id
                                    , _pageContent_fileId :: Id
                                    }
                 | PageContentTextArea { _pageContent_id   :: Id
                                       , _pageContent_text :: String
                                       }
                 | PageContentButton { _pageContent_id      :: Id
                                     , _pageContent_var     :: Maybe String
                                     , _pageContent_text    :: String
                                     , _pageContent_event   :: Maybe String  -- TODO: Check those type
                                     , _pageContent_action  :: Maybe String  -- TODO: Check those type
                                     , _pageContent_content :: Maybe String  -- TODO: Check those type
                                     , _pageContent_message :: Maybe String  -- TODO: Check those type
                                     , _pageContent_primary :: Bool
                                     }
                 | PageContentRadioButton { _pageContent_id       :: Id
                                          , _pageContent_name     :: String
                                          , _pageContent_title    :: String
                                          , _pageContent_values   :: String
                                          , _pageContent_defuault :: Maybe String
                                          }
                 | PageContentTextInput { _pageContent_id       :: Id
                                        , _pageContent_name     :: String
                                        , _pageContent_text     :: String
                                        , _pageContent_defuault :: Maybe String
                                        }
                 | PageContentTextAreaInput { _pageContent_id       :: Id
                                            , _pageContent_name     :: String
                                            , _pageContent_text     :: String
                                            , _pageContent_defuault :: Maybe String
                                            }
                 | PageContentSwitch { _pageContent_id      :: Id
                                     , _pageContent_name    :: String
                                     , _pageContent_text    :: String
                                     , _pageContent_default :: Maybe String
                                     }
                 | PageContentCounter { _pageContent_id   :: Id
                                      , _pageContent_name :: String
                                      , _pageContent_text :: String
                                      , _pageContent_inc  :: Maybe String
                                      }
                 | PageContentIf { _pageContent_id       :: Id
                                 , _pageContent_var      :: Maybe String
                                 , _pageContent_children :: [PageContent]
                                 }
                 | PageContentPost { _pageContent_id   :: Id
                                   , _pageContent_text :: String
                                   }
                    deriving (Show)


-- instance FromJSON PageContent where {{{
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
            littleParser x               = prependFailure ("parsing PageContent's type failed, " ++ x ++ " is not valid")
                                            (typeMismatch "Object" (Object v))
    -- We do not expect a non-Object value here.
    -- We could use empty to fail, but typeMismatch
    -- gives a much more informative error message.
    parseJSON invalid    =
        prependFailure "parsing PageContent failed, "
            (typeMismatch "Object" invalid)
-- }}}
makePrisms ''PageContent
makeLenses ''PageContent

data PageVariableArg = PageVariableArg { _pageVArg_id    :: Id
                                       , _pageVArg_type  :: String
                                       , _pageVArg_value :: String
                                       } deriving (Show)

makeLenses ''PageVariableArg
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''PageVariableArg)

-- TODO: Implement this
--
-- This is temporary set to 'String', though it should be enum
-- To Implement this, I should know all sort of variable type
type PageVariableType = String

-- | Page variable that can be declared for each page
data PageVariable = PageVariable { _pageV_id   :: Id
                                 , _pageV_args :: [PageVariableArg]
                                 , _pageV_name :: String
                                 , _pageV_type :: PageVariableType
                                 , _pageV_value :: Maybe String -- ^ TODO: what type is this?
                                 } deriving (Show)

makeLenses ''PageVariable
$(deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''PageVariable)

-- | Page
--
-- Docs:
--
--   * https://misskey.io/api-doc#operation/pages/show
--
--   * https://join.misskey.page/ja/wiki/usage/pages
data Page = Page { _page_id  :: Id
                 , _page_createdAt :: UTCTime
                 , _page_updatedAt :: UTCTime
                 , _page_title        :: String
                 , _page_name         :: String
                 , _page_summary      :: Maybe String
                 , _page_content      :: [PageContent]
                 , _page_variables    :: [PageVariable]
                 , _page_userId       :: Id
                 , _page_user         :: BaseUser
                 -- Those fields below are undocumented
                 , _page_hideTitleWhenPinned :: Bool
                 , _page_alignCenter         :: Bool
                 , _page_font                :: String
                 , _page_eyeCatchingImageId  :: Maybe String -- ^ TODO: Check whether this type correct
                 , _page_eyeCatchingImage    :: Maybe String -- ^ TODO: Check whether this type correct
                 , _page_attachedFiles       :: [File] -- ^ TODO: Check whether this type correct
                 , _page_likedCount          :: Int
                 } deriving (Show)

makeLenses ''Page

-- instance FromJSON Page where {{{
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
-- }}}
-- }}}

-- Note {{{

-- | I can't find any documents. Also, 'Geo' wasn't on any response
--
-- I'll fix this later, just leave this as placeholder
data Geo = Geo deriving (Show)

instance FromJSON Geo where
    parseJSON (Object _) = return Geo
    parseJSON _          = mempty

instance ToJSON Geo where
    toJSON _ = String "geo"

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
                 , _note_visibility         :: String
                 , _note_mentions           :: Maybe [Id]
                 , _note_visibleUserIds     :: Maybe [Id]
                 , _note_fileIds            :: Maybe [Id]
                 , _note_files              :: Maybe [File]
                 , _note_tags               :: Maybe [String]
                 , _note_poll               :: Maybe Poll
                 , _note_geo                :: Maybe Geo
                 } deriving (Show)

makeLenses ''Note

instance FromJSON Note where
    parseJSON (Object v) = Note <$> v .: "id"
                                <*> (fromJust . parseISO8601 <$> (v.: "createdAt"))
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
                                <*> v .:? "geo"
    parseJSON _          = mempty

--- }}}

-- User {{{

data UserTwitterInfo = UserTwitterInfo { _userTwitterInfo_id :: String
                                       , _userTwitterInfo_screenName :: String}
                       deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''UserTwitterInfo)
makeLenses ''UserTwitterInfo

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
                 , _user_bannerUrl               :: Maybe Url
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

makeLenses ''User

instance FromJSON User where
    parseJSON (Object v) = User <$> v .:  "id"
                                <*> v .:  "username"
                                <*> v .:? "name"
                                <*> v .:? "url"
                                <*> v .:? "avatarUrl"
                                <*> v .:? "bannerUrl"
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

