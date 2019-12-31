{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Misskey.Api.Users.Show
Description : Misskey API Endpoint and Request for users/show
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2019
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Call `users/show` Misskey API
API document is: https://misskey.io/api-doc#operation/users/show
-}
module Network.Misskey.Api.Users.Show (
usersShow
, APIRequest(..)
)

where

import Data.Aeson (encode, object, (.=), Value, decode', fromJSON, Result(..))
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.ByteString.Lazy (ByteString)
import Lens.Simple ((^.), makeLenses)
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, httpJSON, getResponseBody, getResponseStatusCode)

import Network.Misskey.Type
import Network.Misskey.Api.Internal

data APIRequest = UserId   String
                | UserIds  [String]
                | UserName String (Maybe String)

-- | Call API `users/show` and return result
--
-- This supports to post *only one of userId/userIds/username/host property*
--
-- Doc: https://misskey.io/api-doc#operation/users/show
usersShow :: APIRequest -> Misskey [User]
usersShow (UserIds is) = postRequest "/api/users/show" $ object ["userIds"  .= is]
usersShow req          = do
    env <- ask
    result <- liftIO $ runMisskey (postRequest "/api/users/show" obj :: Misskey User) env
    case result of
        Right u -> return $ Right [u]
        Left  e -> return $ Left e

    where
        obj = case req of
               UserId i     -> object ["userId"   .= i]
               UserName n h -> object ["username" .= n , "host" .= h]
