{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Web.Misskey.Api.Users.Show
Description : Misskey API Endpoint and Request for users/show
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/show` Misskey API
API document is: https://misskey.io/api-doc#operation/users/show
-}
module Web.Misskey.Api.Users.Show (
usersShow
, APIRequest
, _UserId, _UserIds, _UserName
)

where

import RIO
import Data.Aeson (encode, object, (.=), Value, decode', fromJSON, Result(..), ToJSON(toJSON))
import Data.List (singleton)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.), makeLenses, makePrisms, _Right)
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, httpJSON, getResponseBody, getResponseStatusCode)

import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = UserId   String
                | UserIds  [String]
                | UserName String (Maybe String)
makePrisms ''APIRequest

instance ToJSON APIRequest where
  toJSON (UserIds is) = object ["userIds" .= is]
  toJSON (UserId i) = object ["userId" .= i]
  toJSON (UserName n h) = object ["username" .= n , "host" .= h]

-- | Call API `users/show` and return result
--
-- This supports to post *only one of userId/userIds/username/host property*
--
-- Doc: https://misskey.io/api-doc#operation/users/show
usersShow :: (HasMisskeyEnv env) => APIRequest -> RIO env [User]
usersShow req@(UserIds is) = postRequest "/api/users/show" $ toJSON req
usersShow req              = singleton <$> postRequest "/api/users/show" (toJSON req)
