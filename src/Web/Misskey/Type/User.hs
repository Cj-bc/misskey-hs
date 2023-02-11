{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Type.User (
  User

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
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Web.Misskey.Type.Common
import Web.Misskey.Type.Note
import Web.Misskey.Type.Page

data UserTwitterInfo = UserTwitterInfo { _userTwitterInfo_id :: String
                                       , _userTwitterInfo_screenName :: String}
                       deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''UserTwitterInfo)
makeLenses ''UserTwitterInfo

-- | Key-value pair that can be set via: Settings>Profile>Edit additional Information
data UserField = UserField Text Url
    deriving (Show)

instance FromJSON UserField where
    parseJSON (Object v) = UserField <$> v .: "name"
                                     <*> v .: "value"
    parseJSON e          = fail $ "when parsing the constructor UserField of type Web.Misskey.Type.UserField expected Object but got " ++ show e

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
                 , _user_fields                  :: Maybe [UserField]
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
                                <*> v `parseDate` "birthday"
                                <*> v `parseDate` "createdAt"
                                <*> v `parseDate` "updatedAt"
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
    parseJSON _          = fail "Failed to parse User Object"
