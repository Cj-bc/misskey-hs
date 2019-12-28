{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
                            UserId i   -> object ["userId" .= i]
                            UserIds is -> object ["userIds" .= is]
                            UserName n -> object ["username" .= n]
                            Host h     -> object ["host" .= h]
                in usersShowBase obj

usersShowBase = undefined
