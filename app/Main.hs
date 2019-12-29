{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans.Reader (runReaderT)
import Text.Show.Unicode (ushow)
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.Misskey.Type
import Network.Misskey.Api.Users.Show


main :: IO ()
main = do
    let env = MisskeyEnv "" "https://virtual-kaf.fun"
        request = UserName "cj_bc_sd"

    response <- runReaderT (usersShow request) env
    case response of
        Left er -> putStrLn $ "Error occured: " ++ show er
        Right u -> print u
