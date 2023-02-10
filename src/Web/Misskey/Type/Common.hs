{-# LANGUAGE TemplateHaskell, OverloadedStrings, LambdaCase #-}
module Web.Misskey.Type.Common (
  -- * Some type synonyms
  Url, UserId, NoteId, Id
  -- * BaseUser
, BaseUser(BaseUser)
, baseUser_id, baseUser_name, baseUser_username
, baseUser_host, baseUser_avatarUrl, baseUser_isCat
, baseUser_emojis

, parseDate, parseDateUnsafe

) where
import Control.Lens (makeLenses)
import Data.Aeson (Object, (.:?), fieldLabelModifier)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Aeson.Key (fromText)


type Url = String
type UserId  = String
type NoteId = String
type Id = String

-- TODO: Implement this
--
-- This seems to be `rgb(xxx,yyy,zzz)` which can be parsed
type Color = String

parseDate :: Object -> Text -> Parser (Maybe UTCTime)
parseDate v s = do
    b <- v .:? fromText s :: Parser (Maybe String)
    return $ parseISO8601 =<< b

-- | Parse Data and return raw value of it
--
-- This is allowed because it's bug if 'parseISO8601' fails
parseDateUnsafe :: Object -> Text -> Parser UTCTime
parseDateUnsafe v s = parseDate v s >>= \case
  Just t -> return t
  Nothing -> error $ "Data of '" ++ show s ++ "' wasn't parsable"


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
