{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Web.Misskey.Api.Users.Search
Description : Misskey API Endpoint and Request for users/search
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/search` Misskey API
API document is: https://misskey.io/api-doc#operation/users/search
-}
module Web.Misskey.Api.Users.Search
( usersSearch
, APIRequest(APIRequest)

-- ** Lenses for APIRequest
, query, offset, limit, localOnly, detail
) where

import RIO
import Control.Monad.Trans.Reader (ask)
import Control.Lens (makeLenses)
import Data.Aeson ((.=), object, ToJSON (toJSON))
import Data.Maybe (isNothing)
import Web.Misskey.Type
import Web.Misskey.Api.Internal (postRequest, createMaybeObj)

data APIRequest = APIRequest { _query     :: String
                             , _offset    :: Maybe Int
                             , _limit     :: Maybe Int
                             , _localOnly :: Maybe Bool
                             , _detail    :: Maybe Bool
                             }
makeLenses ''APIRequest

instance ToJSON APIRequest where
  toJSON (APIRequest q o l local d) = object body
    where
      offsetObj     = createMaybeObj "offset"    o
      limitObj      = createMaybeObj "limit"     l
      localOnlyObj  = createMaybeObj "localOnly" local
      detailObj     = createMaybeObj "detail"    d
      body          = ["query" .= q] ++ offsetObj ++ limitObj ++ localOnlyObj ++ detailObj


usersSearch :: (HasMisskeyEnv env) => APIRequest -> RIO env [User]
usersSearch = postRequest "/api/users/search" . toJSON
