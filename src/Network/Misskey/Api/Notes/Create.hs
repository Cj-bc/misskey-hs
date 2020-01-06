{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-|
Module      : Network.Misskey.Api.Notes.Create
Description : Misskey API Endpoint and Request for notes/create
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `notes/create` Misskey API

API document is: https://virtual-kaf.fun/api-doc#operation/notes/create
-}
module Network.Misskey.Api.Notes.Create
( notesCreate
, APIRequest(..)
, Visibility(..)
) where

import Data.Aeson.TH (deriveJSON, defaultOptions, constructorTagModifier)
import Data.Char (toLower)
import Lens.Simple ((^.), makeLenses)
import Network.Misskey.Type
import Network.Misskey.Api.Internal

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


-- | Call 'notes/create' API and return result
notesCreate :: APIRequest -> Misskey Note
notesCreate req = postRequest "/api/notes/create" body
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
        fileIdsBody           = createMaybeObj "fileIds"           $ nothingIfEmpty (req^.fileIds)
        replyIdBody           = createMaybeObj "replyId"           $ nothingIfEmpty (req^.replyId)
        renoteIdBody          = createMaybeObj "renoteId"          $ nothingIfEmpty (req^.renoteId)
        pollBody              = createMaybeObj "poll"              (req^.poll)
        body                  = mconcat [visibilityBody, visibleUserIdsBody, textBody
                                        , cwBody, viaMobileBody, localOnlyBody
                                        , noExtractMentionsBody, noExtractHashtagsBody, noExtractEmojisBody
                                        , geoBody, fileIdsBody, replyIdBody
                                        , renoteIdBody, pollBody]
