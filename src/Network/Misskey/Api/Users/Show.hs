{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Misskey.Api.Users.Show (
usersShow
, APIRequest(..)
)

where

import Data.Aeson (encode, object, (.=), Value, decode')
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.ByteString.Lazy (ByteString)
import Lens.Simple ((^.), makeLenses)
import Network.Misskey.Type
import Network.HTTP.Client (method, requestBody, RequestBody(RequestBodyLBS), requestHeaders
                           , Response, parseRequest)
import Network.HTTP.Simple (httpLbs, getResponseBody, getResponseStatusCode)


data APIRequest = UserId   String
                | UserIds  [String]
                | UserName String
                | Host     String

usersShow :: APIRequest -> ReaderT MisskeyEnv IO (Either APIError [User])
usersShow (UserId i)   = userShowId i
usersShow (UserIds is) = userShowIds is
usersShow (UserName n) = userShowUsername n


-- userShowUsername :: String -> ReaderT MisskeyEnv IO (Either APIError User)
userShowUsername :: String -> ReaderT MisskeyEnv IO (Either APIError [User])
userShowUsername name = do
    env <- ask
    initReq <- parseRequest $ (env^.url) ++ "/api/users/show"
    let requestObject = object [ "username" .= name ]
        request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode requestObject
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }

    response <- httpLbs request
    case getResponseStatusCode response of
        200 -> return $ Right $ fromJust $ decode' $ getResponseBody response
        _   -> return $ Left  $ fromJust $ decode' $ getResponseBody response



-- userShowIds :: [String] -> ReaderT MisskeyEnv IO (Either APIError [User])
userShowIds :: [String] -> ReaderT MisskeyEnv IO (Either APIError [User])
userShowIds ids = do
    env <- ask
    initReq <- parseRequest $ (env^.url) ++ "/api/users/show"
    let requestObject = object [ "userIds"  .= ids ]
        request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode requestObject
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }
    liftIO $ do
        putStrLn "======= DEBUG ======"
        putStrLn $ show request
        putStrLn "===================="

    response <- httpLbs request
    case getResponseStatusCode response of
        200 -> return $ Right $ fromJust $ decode' $ getResponseBody response
        _   -> return $ Left  $ fromJust $ decode' $ getResponseBody response


userShowId :: String -> ReaderT MisskeyEnv IO (Either APIError [User])
userShowId id = do
    env <- ask
    initReq <- parseRequest $ (env^.url) ++ "/api/users/show"
    let requestObject = object [ "userId"  .= id ]
        request       = initReq { method = "POST"
                                , requestBody = RequestBodyLBS $ encode requestObject
                                , requestHeaders =
                                      [("Content-Type", "application/json; charset=utf-8")]
                                }
    response <- httpLbs request
    case getResponseStatusCode response of
        200 -> return $ Right $ fromJust $ decode' $ getResponseBody response
        _   -> return $ Left  $ fromJust $ decode' $ getResponseBody response
