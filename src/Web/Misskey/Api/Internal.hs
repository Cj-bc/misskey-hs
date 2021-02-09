{-# Language OverloadedStrings #-}
module Web.Misskey.Api.Internal
( -- * Creating objects
  createMaybeObj
, createObj
, createUTCTimeObj
-- * IO related
, postRequest
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ask, ReaderT)
import Data.Aeson.Types (Pair)
import Data.Aeson (Value, encode, FromJSON, (.=), fromJSON, Result(..), object)
import Data.Time (UTCTime)
import System.Posix.Types (EpochTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))
import Control.Lens ((^.))
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpJSON, getResponseBody, getResponseStatusCode)

import Web.Misskey.Type

-- | Create 'Data.Aeson.KeyValue' a Object
createMaybeObj t = maybe [] (\x -> [t .= x])

-- | Create 'Data.Aeson.KeyValue' a Object
createObj t x = [t .= x]

-- | Create 'Data.Aeson.KeyValue' a Object
createUTCTimeObj t = maybe [] (\x -> [t .= uToE x])

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
postRequest :: FromJSON a => String -> [Pair] -> Misskey a
postRequest apiPath body =  do
    (MisskeyEnv token url) <- ask
    initReq <- parseRequest $ url ++ apiPath
    let bodyWithToken = object $ ("i" .= token) : body
        request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode bodyWithToken
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }

    response <- httpJSON request :: MonadIO m => m (Response Value)

    let responseBody = getResponseBody response
    case getResponseStatusCode response of
        200 -> case fromJSON responseBody of
                    Success a' -> return $ Right a'
                    Error   e  -> error  $ unlines [apiPath ++ ": error while decoding Result"
                                                   , "FATAL: Please file those outputs to author(or PR is welcome)."
                                                   , "========== raw ByteString =========="
                                                   , show responseBody
                                                   , "========== Error message =========="
                                                   , show e
                                                   ]
        _   -> case fromJSON responseBody of
                    Success a' -> return $ Left a'
                    Error   e  -> error  $ unlines [apiPath ++ ": error while decoding APIError"
                                                   , "FATAL: Please file those outputs to author(or PR is welcome)."
                                                   , "========== raw ByteString =========="
                                                   , show responseBody
                                                   , "========== Error message =========="
                                                   , show e
                                                   ]

