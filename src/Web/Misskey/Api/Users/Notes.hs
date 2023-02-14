{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Web.Misskey.Api.Users.Notes
Description : Misskey API Endpoint and Request for users/notes
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/notes` Misskey API
API document is: https://misskey.io/api-doc#operation/users/notes
-}

module Web.Misskey.Api.Users.Notes
( usersNotes
, APIRequest(..)
) where
import RIO
import Control.Lens (makeLenses)
import Data.Time (UTCTime)
import Data.Aeson ((.=), object, ToJSON (toJSON))


import Web.Misskey.Type
import Web.Misskey.Api.Internal (postRequest, createObj, createMaybeObj, createUTCTimeObj)

data APIRequest = APIRequest { _userId           :: Id
                             , _includeReplies   :: Bool
                             , _limit            :: Maybe Int -- [1..100]
                             , _sinceId          :: Maybe String
                             , _untilId          :: Maybe String
                             , _sinceDate        :: Maybe UTCTime
                             , _untilDate        :: Maybe UTCTime
                             , _includeMyRenotes :: Bool
                             , _withFiles        :: Bool
                             , _fileType         :: Maybe [String]
                             , _excludeNsfw      :: Bool
                             }
makeLenses ''APIRequest

instance ToJSON APIRequest where
  toJSON req = object body
    where
      userIdObj           = createObj        "userId"           (req^.userId)
      includeRepObj       = createObj        "includeReplies"   (req^.includeReplies)
      limitObj            = createMaybeObj   "limit"            (req^.limit)
      sinceIdObj          = createMaybeObj   "sinceId"          (req^.sinceId)
      untilIdObj          = createMaybeObj   "untilId"          (req^.untilId)
      sinceDateObj        = createUTCTimeObj "sinceDate"        (req^.sinceDate)
      untilDateObj        = createUTCTimeObj "untilDate"        (req^.untilDate)
      includeMyRenotesObj = createObj        "includeMyRenotes" (req^.includeMyRenotes)
      withFilesObj        = createObj        "withFiles"        (req^.withFiles)
      fileTypeObj         = createMaybeObj   "fileType"         (req^.fileType)
      excludeNsfwObj      = createObj        "excludeNsfw"      (req^.excludeNsfw)
      body                = mconcat [userIdObj, includeRepObj, limitObj, sinceIdObj
                                    , untilIdObj, sinceDateObj, untilDateObj, includeMyRenotesObj
                                    , withFilesObj, fileTypeObj, excludeNsfwObj]

usersNotes :: (HasMisskeyEnv env) => APIRequest -> RIO env [Note]
usersNotes = postRequest "/api/users/notes" . toJSON



