{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Misskey.Api.Users.Notes
( usersNotes
, APIRequest(..)
) where
import Lens.Simple (makeLenses, (^.))
import Data.Time (UTCTime)
import Data.Aeson ((.=), object)
import System.Posix.Types (EpochTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))

import Network.Misskey.Type
import Network.Misskey.Api.Internal (postRequest)

data APIRequest = APIRequest { _userId           :: Id
                             , _includeReplies   :: Maybe Bool
                             , _limit            :: Maybe Int -- [1..100]
                             , _sinceId          :: Maybe String
                             , _untilId          :: Maybe String
                             , _sinceDate        :: Maybe UTCTime
                             , _untilDate        :: Maybe UTCTime
                             , _includeMyRenotes :: Maybe Bool
                             , _withFiles        :: Maybe Bool
                             , _fileType         :: Maybe [String]
                             , _excludeNsfw      :: Maybe Bool
                             }
makeLenses ''APIRequest

usersNotes :: APIRequest -> Misskey [Note]
usersNotes req = postRequest "/api/users/notes" obj
    where
        createObj t         = maybe [] (\x -> [t .= x])
        userIdObj           = ["userId" .= (req^.userId)]
        includeRepObj       = createObj "includeReplies" (req^.includeReplies)
        limitObj            = createObj "limit"       (req^.limit)
        sinceIdObj          = createObj "sinceId"     (req^.sinceId)
        untilIdObj          = createObj "untilId"     (req^.untilId)
        sinceDateObj        = maybe [] (\x -> ["sinceDate" .= uToE x]) (req^.sinceDate)
        untilDateObj        = maybe [] (\x -> ["untilDate" .= uToE x]) (req^.untilDate)
        includeMyRenotesObj = createObj "includeMyRenotes"   (req^.includeMyRenotes)
        withFilesObj        = createObj "withFiles"   (req^.withFiles)
        fileTypeObj         = createObj "fileType"    (req^.fileType)
        excludeNsfwObj      = createObj "excludeNsfw" (req^.excludeNsfw)
        obj                 = object $ mconcat [userIdObj, includeRepObj, limitObj, sinceIdObj
                                               , untilIdObj, sinceDateObj, untilDateObj, includeMyRenotesObj
                                               , withFilesObj, fileTypeObj, excludeNsfwObj]


-- | Convert UTCTime to UNIX time
-- 
-- This code is from: https://kazu-yamamoto.hatenablog.jp/entry/20130329/1364525770
uToE :: UTCTime -> EpochTime
uToE = CTime . truncate . utcTimeToPOSIXSeconds
