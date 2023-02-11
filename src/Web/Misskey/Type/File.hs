{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Type.File (
  File(File)

, file_id, file_createdAt, _file_name
, file_type, file_md5, file_size, file_url
, file_folderId, isSensitive
) where
import RIO
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import Control.Lens (makeLenses)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.:?))
import Web.Misskey.Type.Common

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
                                <*> v `parseDateUnsafe` "createdAt"
                                <*> v .:  "name"
                                <*> v .:  "type"
                                <*> v .:  "md5"
                                <*> v .:  "size"
                                <*> v .:? "url"
                                <*> v .:? "folderId"
                                <*> v .:  "isSensitive"
    parseJSON _          = fail "Failed to parse File Object"
