{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-|
Module      : Network.Misskey.Api.Users.Following
Description : Misskey API Endpoint and Request for users/following
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/following` Misskey API
API document is: https://misskey.io/api-doc#operation/users/following
-}
module Network.Misskey.Api.Users.Following (
  APIRequest(APIRequest)
, usersFollowing
) where

import Data.Aeson (object)
import Lens.Simple ((^.), makeLenses)
import Network.Misskey.Type
import Network.Misskey.Api.Internal


data APIRequest = APIRequest { _userId   :: Maybe String
                             , _username :: Maybe String
                             , _host     :: Maybe String
                             , _sinceId  :: Maybe String
                             , _untilId  :: Maybe String
                             , _limit    :: Maybe Int
                             }
makeLenses ''APIRequest

-- | Call 'users/following' API and return result
usersFollowing :: APIRequest -> Misskey [User]
usersFollowing req = postRequest "/api/users/Following" obj
        where
            userIdObj   = createMaybeObj "userId"   (req^.userId)
            usernameObj = createMaybeObj "username" (req^.username)
            hostObj     = createMaybeObj "host"     (req^.host)
            sinceIdObj  = createMaybeObj "sinceId"  (req^.sinceId)
            untilIdObj  = createMaybeObj "untilId"  (req^.untilId)
            limitObj    = createMaybeObj "limit"    (req^.limit)
            obj = object $ mconcat [userIdObj, usernameObj, hostObj, sinceIdObj, untilIdObj, limitObj]
