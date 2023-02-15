{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Web.Misskey.Api.Users.Following
Description : Misskey API Endpoint and Request for users/following
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/following` Misskey API
API document is: https://misskey.io/api-doc#operation/users/following
-}
module Web.Misskey.Api.Users.Following (
  UsersFollowing(UsersFollowing)
) where

import RIO
import Data.Aeson (object, ToJSON (toJSON))
import Control.Lens (makeLenses)
import Web.Misskey.Type
import Web.Misskey.Api.Internal


data UsersFollowing = UsersFollowing { _userId   :: Maybe String
                                     , _username :: Maybe String
                                     , _host     :: Maybe String
                                     , _sinceId  :: Maybe String
                                     , _untilId  :: Maybe String
                                     , _limit    :: Maybe Int
                                     }
makeLenses ''UsersFollowing

instance ToJSON UsersFollowing where
  toJSON req = object body
    where
      userIdObj   = createMaybeObj "userId"   (req^.userId)
      usernameObj = createMaybeObj "username" (req^.username)
      hostObj     = createMaybeObj "host"     (req^.host)
      sinceIdObj  = createMaybeObj "sinceId"  (req^.sinceId)
      untilIdObj  = createMaybeObj "untilId"  (req^.untilId)
      limitObj    = createMaybeObj "limit"    (req^.limit)
      body        = mconcat [userIdObj, usernameObj, hostObj, sinceIdObj, untilIdObj, limitObj]

instance APIRequest UsersFollowing where
  type APIResponse UsersFollowing = [User]
  apiPath _ = "/api/users/Following"
