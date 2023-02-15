{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Web.Misskey.Api.Users.Show
Description : Misskey API Endpoint and Request for users/show
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019-2023
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/show` Misskey API
API document is: https://misskey.io/api-doc#operation/users/show
-}
module Web.Misskey.Api.Users.Show (
  UsersShow
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

-- | Call API `users/show` and return result
--
-- This supports to post *only one of userId/userIds/username/host property*
--
-- Doc: https://misskey.io/api-doc#operation/users/show
data UsersShow = UserId   String
                | UserIds  [String]
                | UserName String (Maybe String)
makePrisms ''UsersShow

instance ToJSON UsersShow where
  toJSON (UserIds is) = object ["userIds" .= is]
  toJSON (UserId i) = object ["userId" .= i]
  toJSON (UserName n h) = object ["username" .= n , "host" .= h]

instance APIRequest UsersShow where
  type APIResponse UsersShow = [User] 
  apiPath _ = "/api/users/show"
  parseResponse req response = case responseParser req response of
                                 Error e -> throwM (ResponseParseFailed e)
                                 Success v -> return v
    where
      responseParser (UserIds _) = fromJSON :: Value -> Result [User]
      responseParser _           = fmap singleton . (fromJSON :: Value -> Result User)
