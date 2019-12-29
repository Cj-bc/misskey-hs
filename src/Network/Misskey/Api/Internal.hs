{-# Language OverloadedStrings #-}
module Network.Misskey.Api.Internal where

import Control.Monad.Trans.Reader (ask)
import Data.Aeson (Value, encode, FromJSON, eitherDecode')
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
        200 -> case (eitherDecode' responseBody) of
                Right a ->  return $ Right a
                Left e -> error $ unlines [apiPath ++ ": error while decoding Result"
                                          , "========== raw ByteString =========="
                                          , show responseBody
                                          , "========== Error message =========="
                                          , show e
                                          ]
        _   -> case (eitherDecode' responseBody :: Either String APIError) of
                Right a -> return $ Left a
                Left e  -> error $ unlines [apiPath ++ ": error while decoding APIError"
                                           , "========== raw ByteString =========="
                                           , show responseBody
                                           , "========== Error message =========="
                                           , show e
                                           ]
