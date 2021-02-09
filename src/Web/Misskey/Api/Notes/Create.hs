{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-|
Module      : Web.Misskey.Api.Notes.Create
Description : Misskey API Endpoint and Request for notes/create
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `notes/create` Misskey API

API document is: https://virtual-kaf.fun/api-doc#operation/notes/create
-}
module Web.Misskey.Api.Notes.Create
( notesCreate
, APIRequest(..)
, Visibility(..)
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (deriveJSON, defaultOptions, constructorTagModifier)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Char (toLower)
import Control.Lens ((^.), makeLenses)
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data Visibility = Public | Home | Followers | Specified deriving (Read)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Visibility)


data APIRequest = APIRequest { _visibility        :: Visibility
                             , _visibleUserIds    :: [Id]
                             , _text              :: Maybe String
                             , _cw                :: Maybe String
                             , _viaMobile         :: Bool
                             , _localOnly         :: Bool
                             , _noExtractMentions :: Bool
                             , _noExtractHashtags :: Bool
                             , _noExtractEmojis   :: Bool
                             , _geo               :: Maybe Geo
                             , _fileIds           :: [String]
                             , _replyId           :: Id
                             , _renoteId          :: Id
                             , _poll              :: Maybe Poll
                             }
makeLenses ''APIRequest

-- | Object that wraps newly created 'Note' in response
data CreatedNote = CreatedNote {createdNote :: Note}

instance FromJSON CreatedNote where
    parseJSON (Object v) = CreatedNote <$> v .: "createdNote"

-- | Call 'notes/create' API and return result
notesCreate :: APIRequest -> Misskey Note
notesCreate req = do
    responce <- postRequest "/api/notes/create" body :: Misskey CreatedNote
    case responce of
        Left  e               -> return $ Left e
        Right (CreatedNote n) -> return $ Right n
    where
        nothingIfEmpty x      = if x == [] then Nothing else (Just x)
        visibilityBody        = createObj      "visibility"        (req^.visibility)
        visibleUserIdsBody    = createObj      "visibleUserIds"    (req^.visibleUserIds)
        textBody              = createMaybeObj "text"              (req^.text)
        cwBody                = createMaybeObj "cw"                (req^.cw)
        viaMobileBody         = createObj      "viaMobile"         (req^.viaMobile)
        localOnlyBody         = createObj      "localOnly"         (req^.localOnly)
        noExtractMentionsBody = createObj      "noExtractMentions" (req^.noExtractMentions)
        noExtractHashtagsBody = createObj      "noExtractHashtags" (req^.noExtractHashtags)
        noExtractEmojisBody   = createObj      "noExtractEmojis"   (req^.noExtractEmojis)
        geoBody               = createMaybeObj "geo"               (req^.geo)
        -- Those 3 fields should not be '[]' in JSON schema. Otherwise error response will be returned
        fileIdsBody           = createMaybeObj "fileIds"           $ nothingIfEmpty (req^.fileIds)
        replyIdBody           = createMaybeObj "replyId"           $ nothingIfEmpty (req^.replyId)
        renoteIdBody          = createMaybeObj "renoteId"          $ nothingIfEmpty (req^.renoteId)
        pollBody              = createMaybeObj "poll"              (req^.poll)
        body                  = mconcat [visibilityBody, visibleUserIdsBody, textBody
                                        , cwBody, viaMobileBody, localOnlyBody
                                        , noExtractMentionsBody, noExtractHashtagsBody, noExtractEmojisBody
                                        , geoBody, fileIdsBody, replyIdBody
                                        , renoteIdBody, pollBody]
