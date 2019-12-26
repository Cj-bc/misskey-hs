{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson (encode, object, (.=), Value)
import Text.Show.Unicode (ushow)
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.Misskey.Type (User)


main :: IO ()
main = do
    let requestObject = object ["username" .= ("cj_bc_sd" :: String)]
    initReq <- parseRequest "https://virtual-kaf.fun/api/users/show"
    let request = initReq { method = "POST"
                          , requestBody = RequestBodyLBS $ encode requestObject
                          , requestHeaders =
                                [("Content-Type", "application/json; charset=utf-8")]
                          }

    response <- httpJSON request
    putStrLn $ ushow (getResponseBody response :: User)
