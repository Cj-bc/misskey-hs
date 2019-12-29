{-# Language OverloadedStrings #-}
module Network.Misskey.Api.Internal where

import Control.Monad.Trans.Reader (ask)
import Data.Aeson (Value, encode, decode', FromJSON)
import Lens.Simple ((^.))
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, httpJSON, getResponseBody, getResponseStatusCode)

import Network.Misskey.Type

postRequest :: FromJSON a => String -> Value -> Misskey a
postRequest apiPath body =  do
    env <- ask
    initReq <- parseRequest $ (env^.url) ++ apiPath
    let request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode body
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }

    response <- httpLbs request

    let responseBody = getResponseBody response
    case getResponseStatusCode response of
        200 -> case (decode' responseBody) of
                Just a ->  return $ Right a
                Nothing -> error $ unlines [apiPath ++ ": error while decoding Result"
                                           , show responseBody]
        _   -> case (decode' responseBody :: Maybe APIError) of
                Just a -> return $ Left a
                Nothing -> error $ unlines [apiPath ++ ": error while decoding APIError"
                                           , show responseBody]
