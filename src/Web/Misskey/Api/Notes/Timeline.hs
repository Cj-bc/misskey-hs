{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-|
Module      : Web.Misskey.Api.Notes.Timeline
Description : Misskey API Endpoint and Request for notes/timeline
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `notes/timeline` Misskey API
-}
module Web.Misskey.Api.Notes.Timeline
( APIRequest(APIRequest)
, notesTimeline
) where

import Data.Time (UTCTime)
import Lens.Simple (makeLenses, (^.))
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = APIRequest { _limit                 :: Maybe Int -- [1..100]
                             , _sinceId               :: Maybe String
                             , _untilId               :: Maybe String
                             , _sinceDate             :: Maybe UTCTime
                             , _untilDate             :: Maybe UTCTime
                             , _includeMyRenotes      :: Bool
                             , _includeRenotedMyNotes :: Bool
                             , _includeLocalRenotes   :: Bool
                             , _withFiles             :: Bool
                             }
makeLenses ''APIRequest


notesTimeline :: APIRequest -> Misskey [Note]
notesTimeline req = postRequest "/api/notes/timeline" body
    where
        limitBody                 = createMaybeObj   "limit"                 (req^.limit)
        sinceIdBody               = createMaybeObj   "sinceId"               (req^.sinceId)
        untilIdBody               = createMaybeObj   "untilId"               (req^.untilId)
        sinceDateBody             = createUTCTimeObj "sinceDate"             (req^.sinceDate)
        untilDateBody             = createUTCTimeObj "untilDate"             (req^.untilDate)
        includeMyRenotesBody      = createObj        "includeMyRenotes"      (req^.includeMyRenotes)
        includeRenotedMyNotesBody = createObj        "includeRenotedMyNotes" (req^.includeRenotedMyNotes)
        includeLocalRenotesBody   = createObj        "includeLocalRenotes"   (req^.includeLocalRenotes)
        withFilesBody             = createObj        "withFiles"             (req^.withFiles)
        body                      = mconcat [ limitBody, sinceIdBody, untilIdBody
                                            , sinceDateBody, untilDateBody, includeMyRenotesBody
                                            , includeRenotedMyNotesBody, includeLocalRenotesBody, withFilesBody
                                            ]
