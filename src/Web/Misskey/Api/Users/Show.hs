{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

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

import Data.Aeson (encode, object, (.=), Value, decode', fromJSON, Result(..))
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.), makeLenses, makePrisms)
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, httpJSON, getResponseBody, getResponseStatusCode)

import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = UserId   String
                | UserIds  [String]
                | UserName String (Maybe String)
makePrisms ''APIRequest

-- | Call API `users/show` and return result
--
-- This supports to post *only one of userId/userIds/username/host property*
--
-- Doc: https://misskey.io/api-doc#operation/users/show
usersShow :: APIRequest -> Misskey [User]
usersShow (UserIds is) = postRequest "/api/users/show" $ ["userIds"  .= is]
usersShow req          = do
    env <- ask
    result <- liftIO $ runMisskey (postRequest "/api/users/show" body :: Misskey User) env
    case result of
        Right u -> return $ Right [u]
        Left  e -> return $ Left e

    where
        body = case req of
                UserId i     -> ["userId"   .= i]
                UserName n h -> ["username" .= n , "host" .= h]
