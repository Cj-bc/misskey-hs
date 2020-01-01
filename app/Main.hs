{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Show.Unicode (ushow)
import System.Environment (getArgs)
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.Misskey.Type
import qualified Network.Misskey.Api.Users.Notes as UN
import qualified Network.Misskey.Api.Users.Show as USh
import Lens.Simple ((^.))
import Options.Applicative
import Options.Applicative.Types (readerAsk)



wrapWithJustReader :: ReadM (Maybe String)
wrapWithJustReader = readerAsk >>= (\x -> return $ Just x)

usersShowParser :: Parser USh.APIRequest
usersShowParser = USh.UserId    <$> strOption       (long "id"       <> metavar "USER-ID"    <> help "Specify target with user id")
              <|> USh.UserIds   <$> many (strOption (long "ids"      <> metavar "USER-IDs"   <> help "Specify list of target user ids"))
              <|> USh.UserName  <$> strOption       (long "username" <> metavar "USER-NAME"  <> help "Specify target with user name")
                                    <*> option wrapWithJustReader
                                              (long "host" <> metavar "HOST" <> value Nothing <> help "Specify host instance that target user is on")


main :: IO ()
main = do
    (domain:name:_) <- getArgs
    let env = MisskeyEnv "" $ "https://" ++ domain
    print "========== users/show =========="
    response <- runMisskey (USh.usersShow (USh.UserName name Nothing)) env
    case response of
        Left er   -> print $ "Error occured while users/show: " ++ ushow er
        Right usr -> do
            let request = UN.APIRequest (head usr ^.user_id) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            notes <- runMisskey (UN.usersNotes request) env
            case notes of
                Left er -> print $ "Error occured while users/notes: " ++ ushow er
                Right notes -> (putStrLn . ushow) notes

