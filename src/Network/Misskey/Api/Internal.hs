{-# Language OverloadedStrings #-}
module Network.Misskey.Api.Internal where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ask, ReaderT)
import Data.Aeson (Value, encode, FromJSON, (.=), fromJSON, Result(..))
import Lens.Simple ((^.))
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpJSON, getResponseBody, getResponseStatusCode)

import Network.Misskey.Type

-- | Create 'Data.Aeson.KeyValue' a Object
createMaybeObj t = maybe [] (\x -> [t .= x])

-- | Create 'Data.Aeson.KeyValue' a Object
createObj t x = [t .= x]


-- | Post API request and returns API result
--
-- Currently I don't return HTTP status code
--
-- __This function will throw error__ if parsing response failed.
-- As /Parsing error/ is fatal and should be fixed by Library author,
-- not by user, this error should be reported as issue
postRequest :: FromJSON a => String -> Value -> Misskey a
postRequest apiPath body =  do
    (MisskeyEnv token url) <- ask
    initReq <- parseRequest $ url ++ apiPath
    let request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode body
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

