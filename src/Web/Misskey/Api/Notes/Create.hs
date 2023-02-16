{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Web.Misskey.Api.Notes.Create
Description : Misskey API Endpoint and Request for notes/create
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `notes/create` Misskey API

API document is: https://virtual-kaf.fun/api-doc#operation/notes/create
-}
module Web.Misskey.Api.Notes.Create
( NotesCreate(..)
, Visibility(..)
) where

import RIO hiding (poll)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (deriveJSON, defaultOptions, constructorTagModifier)
import Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(toJSON), fromJSON, Result(Error, Success))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Control.Lens (makeLenses)
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data Visibility = Public | Home | Followers | Specified deriving (Read)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Visibility)


data NotesCreate = NotesCreate { _visibility        :: Visibility
                             , _visibleUserIds    :: [Id]
                             , _text              :: Maybe String
                             , _cw                :: Maybe String
                             , _viaMobile         :: Bool
                             , _localOnly         :: Bool
                             , _noExtractMentions :: Bool
                             , _noExtractHashtags :: Bool
                             , _noExtractEmojis   :: Bool
                             , _fileIds           :: [String]
                             , _replyId           :: Id
                             , _renoteId          :: Id
                             , _poll              :: Maybe Poll
                             }
makeLenses ''NotesCreate

instance ToJSON NotesCreate where
  toJSON req = Object $ KM.fromList body
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
        -- Those 3 fields should not be '[]' in JSON schema. Otherwise error response will be returned
        fileIdsBody           = createMaybeObj "fileIds"           $ nothingIfEmpty (req^.fileIds)
        replyIdBody           = createMaybeObj "replyId"           $ nothingIfEmpty (req^.replyId)
        renoteIdBody          = createMaybeObj "renoteId"          $ nothingIfEmpty (req^.renoteId)
        pollBody              = createMaybeObj "poll"              (req^.poll)
        body                  = mconcat [visibilityBody, visibleUserIdsBody, textBody
                                        , cwBody, viaMobileBody, localOnlyBody
                                        , noExtractMentionsBody, noExtractHashtagsBody, noExtractEmojisBody
                                        , fileIdsBody, replyIdBody
                                        , renoteIdBody, pollBody]


-- | Object that wraps newly created 'Note' in response
data CreatedNote = CreatedNote {createdNote :: Note}

instance FromJSON CreatedNote where
    parseJSON (Object v) = CreatedNote <$> v .: "createdNote"

instance APIRequest NotesCreate where
  type APIResponse NotesCreate = Note
  apiPath _ = "/api/notes/create"
  parseResponse _ value = case createdNote <$> fromJSON value of
                            Error e -> throwM (ResponseParseFailed e)
                            Success v -> return v
