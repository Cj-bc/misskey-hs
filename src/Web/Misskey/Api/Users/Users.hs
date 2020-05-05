{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.Misskey.Api.Users.Users
Description : Misskey API Endpoint and Request for users
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users` Misskey API
API document is: https://misskey.io/api-doc#operation/users
-}
module Network.Misskey.Api.Users.Users
( APIRequest(..)
, users
, UsersSortParam(..)
, UsersStateParam(..)
, UsersOriginParam(..)
) where

import Data.Aeson (object)
import Lens.Simple (makeLenses, (^.))
import Network.Misskey.Type
import Network.Misskey.Api.Internal (postRequest, createObj, createMaybeObj)

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

data APIRequest = APIRequest { _limit  :: Maybe Int -- [1..100]
                             , _offset :: Maybe Int
                             , _sort   :: Maybe UsersSortParam
                             , _state  :: Maybe UsersStateParam
                             , _origin :: Maybe UsersOriginParam
                             }
makeLenses ''APIRequest

users :: APIRequest -> Misskey [User]
users req = postRequest "/api/users" body
    where
        limitObj  = createMaybeObj "limit"  (req^.limit)
        offsetObj = createMaybeObj "offset" (req^.offset)
        sortObj   = createMaybeObj "sort"   (fmap show (req^.sort))
        stateObj  = createMaybeObj "state"  (fmap show (req^.state))
        originObj = createMaybeObj "origin" (fmap show (req^.origin))
        body      = mconcat [limitObj, offsetObj, sortObj, stateObj, originObj]
