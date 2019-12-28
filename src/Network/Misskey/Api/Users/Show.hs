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
import Network.Misskey.Type
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, httpJSON, getResponseBody, getResponseStatusCode)


data APIRequest = UserId   String
                | UserIds  [String]
                | UserName String
                | Host     String

-- | Call API `users/show` and return result
--
-- This supports to post *only one of userId/userIds/username/host property*
--
-- Doc: https://misskey.io/api-doc#operation/users/show
usersShow :: APIRequest -> ReaderT MisskeyEnv IO (Either APIError [User])
usersShow req = let obj = case req of
                            UserId i   -> object ["userId"   .= i ]
                            UserIds is -> object ["userIds"  .= is]
                            UserName n -> object ["username" .= n ]
                            Host h     -> object ["host"     .= h ]
                in usersShowBase obj


-- | Basement of usersShow
usersShowBase :: Value -> ReaderT MisskeyEnv IO (Either APIError [User])
usersShowBase obj = do
    env <- ask
    initReq <- parseRequest $ (env^.url) ++ "/api/users/show"
    let request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode obj
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }

    response <- httpLbs request

    let responseBody = getResponseBody response
    case getResponseStatusCode response of
        200 -> case (decode' responseBody :: Maybe User) of
                Just a ->  return $ Right [a]
                Nothing -> error $ unlines ["userShowUsername: error while decoding User"
                                           , show responseBody]
        _   -> case (decode' responseBody :: Maybe APIError) of
                Just a -> return $ Left a
                Nothing -> error $ unlines ["userShowUsername: error while decoding APIError"
                                           , show responseBody]

