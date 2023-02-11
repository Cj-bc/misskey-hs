{-# Language OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Api.Internal
( -- * Creating objects
  createMaybeObj
, createObj
, createUTCTimeObj
-- * IO related
, postRequest
) where

import RIO
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson.Types (Pair, KeyValue, ToJSON)
import Data.Aeson (Value, encode, FromJSON, (.=), fromJSON, Result(..), object)
import Data.Time (UTCTime)
import Data.Text (Text)
import System.Posix.Types (EpochTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))
import Control.Lens ((^.))
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpJSON, getResponseBody, getResponseStatusCode)

import Web.Misskey.Type
import Data.Aeson.Key (fromText)

-- | Create 'Data.Aeson.KeyValue' a Object
createMaybeObj :: (KeyValue kv, ToJSON v) => Text -> Maybe v -> [kv]
createMaybeObj t = maybe [] (\x -> [fromText t .= x])

-- | Create 'Data.Aeson.KeyValue' a Object
createObj :: (KeyValue kv, ToJSON v) => Text -> v -> [kv]
createObj t x = [fromText t .= x]

-- | Create 'Data.Aeson.KeyValue' a Object
createUTCTimeObj :: KeyValue kv => Text -> Maybe UTCTime -> [kv]
createUTCTimeObj t = maybe [] (\x -> [fromText t .= uToE x])

-- | Convert UTCTime to UNIX time
-- 
-- This code is from: https://kazu-yamamoto.hatenablog.jp/entry/20130329/1364525770
uToE :: UTCTime -> EpochTime
uToE = CTime . truncate . utcTimeToPOSIXSeconds


-- | Post API request and returns API result
--
-- Currently I don't return HTTP status code
--
-- __This function will throw error__ if parsing response failed.
-- As /Parsing error/ is fatal and should be fixed by Library author,
-- not by user, this error should be reported as issue
postRequest :: (FromJSON a, HasMisskeyEnv env, HasLogFunc env) => String -> [Pair] -> RIO env a
postRequest apiPath body =  do
    (MisskeyEnv token url) <- view misskeyEnvL
    initReq <- parseRequest $ url ++ apiPath
    let bodyWithToken = object $ ("i" .= token) : body
        request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode bodyWithToken
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }

    response <- liftIO $ httpJSON request :: RIO env (Response Value)

    let responseBody = getResponseBody response
    case getResponseStatusCode response of
        200 -> case (fromJSON responseBody :: FromJSON a => Result a) of
                    Success a' -> return a'
                    Error   e  -> throwM $ ResponseParseFailed e
        code -> case fromJSON responseBody of
                    Success a' -> throwM $ InvalidStatusCodeReturned code a'
                    Error   e  -> throwM $ ResponseParseFailed e
