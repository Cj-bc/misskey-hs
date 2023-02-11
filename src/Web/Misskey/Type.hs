{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
, note_poll

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

import RIO
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Lens (makeLenses)
import Control.Monad ((=<<))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import Data.Maybe (fromJust, isNothing)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import Web.Misskey.Type.Common
import Web.Misskey.Type.Emoji
import Web.Misskey.Type.File
import Web.Misskey.Type.Poll
import Web.Misskey.Type.Note
import Web.Misskey.Type.Page
import Web.Misskey.Type.User

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
