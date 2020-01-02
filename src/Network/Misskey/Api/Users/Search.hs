{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Misskey.Api.Users.Search
Description : Misskey API Endpoint and Request for users/search
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/search` Misskey API
API document is: https://misskey.io/api-doc#operation/users/search
-}
module Network.Misskey.Api.Users.Search
( usersSearch
, APIRequest(..)
) where

import Control.Monad.Trans.Reader (ask)
import Data.Aeson ((.=), object)
import Data.Maybe (isNothing)
import Network.Misskey.Type
import Network.Misskey.Api.Internal (postRequest, createMaybeObj)

data APIRequest = APIRequest { query     :: String
                             , offset    :: Maybe Int
                             , limit     :: Maybe Int
                             , localOnly :: Maybe Bool
                             , detail    :: Maybe Bool
                             }



usersSearch :: APIRequest -> Misskey [User]
usersSearch (APIRequest q o l local d) = postRequest "/api/users/search" obj
    where
        offsetObj     = createMaybeObj "offset"    o
        limitObj      = createMaybeObj "limit"     l
        localOnlyObj  = createMaybeObj "localOnly" local
        detailObj     = createMaybeObj "detail"    d
        obj = object $ ["query" .= q] ++ offsetObj ++ limitObj ++ localOnlyObj ++ detailObj
