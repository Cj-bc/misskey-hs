{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Web.Misskey.Api.Users.Users
Description : Misskey API Endpoint and Request for users
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users` Misskey API
API document is: https://misskey.io/api-doc#operation/users
-}
module Web.Misskey.Api.Users.Users
( UsersUsers(..)
, UsersSortParam(..)
, UsersStateParam(..)
, UsersOriginParam(..)
) where

import RIO
import Data.Aeson (object, ToJSON (toJSON))
import Control.Lens (makeLenses)
import Web.Misskey.Type
import Web.Misskey.Api.Internal (postRequest, createObj, createMaybeObj, APIRequest(APIResponse, apiPath))

data UsersSortParam = FollowerInc   | FollowerDec
                    | CreatedAtInc  | CreatedAtDec
                    | UpdatedAtInc  | UpdatedAtDec
                    deriving (Read)

-- instance Show UsersSortParam {{{
instance Show UsersSortParam where
    show FollowerInc  = "+follow"
    show FollowerDec  = "-follow"
    show CreatedAtInc = "+createdAt"
    show CreatedAtDec = "-createdAt"
    show UpdatedAtInc = "+updatedAt"
    show UpdatedAtDec = "-updatedAt"
-- }}}

data UsersStateParam = All | Admin | Moderator | AdminOrModerator | Alive deriving (Read)

-- instance Show UsersStateParam {{{
instance Show UsersStateParam where
    show All              = "all"
    show Admin            = "admin"
    show Moderator        = "moderator"
    show AdminOrModerator = "adminOrModerator"
    show Alive            = "alive"
-- }}}

data UsersOriginParam = Combined | Local | Remote
                        deriving (Read)

-- instance Show UsersOriginParam {{{
instance Show UsersOriginParam where
    show Combined = "combined"
    show Local    = "local"
    show Remote   = "remote"
-- }}}

data UsersUsers = UsersUsers { _limit  :: Maybe Int -- [1..100]
                             , _offset :: Maybe Int
                             , _sort   :: Maybe UsersSortParam
                             , _state  :: Maybe UsersStateParam
                             , _origin :: Maybe UsersOriginParam
                             }
makeLenses ''UsersUsers

instance ToJSON UsersUsers where
  toJSON req = object body
    where
      limitObj  = createMaybeObj "limit"  (req^.limit)
      offsetObj = createMaybeObj "offset" (req^.offset)
      sortObj   = createMaybeObj "sort"   (fmap show (req^.sort))
      stateObj  = createMaybeObj "state"  (fmap show (req^.state))
      originObj = createMaybeObj "origin" (fmap show (req^.origin))
      body      = mconcat [limitObj, offsetObj, sortObj, stateObj, originObj]

instance APIRequest UsersUsers where
  type APIResponse UsersUsers = [User]
  apiPath _ = "/api/users"
