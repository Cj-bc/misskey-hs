{-# LANGUAGE OverloadedStrings #-}
module Network.Misskey.Api.Users.Search
( usersSearch
, APIRequest(..)
) where

import Control.Monad.Trans.Reader (ask)
import Data.Aeson ((.=), object)
import Data.Maybe (isNothing)
import Network.Misskey.Type
import Network.Misskey.Api.Internal (postRequest)

data APIRequest = APIRequest { query     :: String
                             , offset    :: Maybe Int
                             , limit     :: Maybe Int
                             , localOnly :: Maybe Bool
                             , detail    :: Maybe Bool
                             }



usersSearch :: APIRequest -> Misskey [User]
usersSearch (APIRequest q o l local d) = postRequest "/api/users/search" obj
    where
        offsetObj     = if isNothing o     then [] else ["offset" .= o]
        limitObj      = if isNothing l     then [] else ["limit" .= l]
        localOnlyObj  = if isNothing local then [] else ["localOnly" .= local]
        detailObj     = if isNothing d     then [] else ["detail" .= d]
        obj = object $ ["query" .= q] ++ offsetObj ++ limitObj ++ localOnlyObj ++ detailObj
