{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Text.Show.Unicode (ushow)
import System.Environment (getArgs)
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.Misskey.Type
import qualified Network.Misskey.Api.Users.Notes as UN
import qualified Network.Misskey.Api.Users.Show as USh
import Lens.Simple ((^.))
import Options.Applicative
import Options.Applicative.Types (readerAsk, Parser(NilP))

data SubCmds = UsersShow USh.APIRequest | UsersNotes UN.APIRequest | UsersSearch USe.APIRequest


wrapWithJustReader :: ReadM (Maybe String)
wrapWithJustReader = readerAsk >>= (\x -> return $ Just (read x))

maybeUTCTimeReader :: ReadM (Maybe UTCTime)
maybeUTCTimeReader = readerAsk >>= return . parseISO8601


-- usersShowParser {{{
usersShowParser :: Parser SubCmds
usersShowParser = UsersShow <$> (USh.UserId        <$> strOption       (long "id"       <> metavar "USER-ID"    <> help "Specify target with user id")
                                 <|> USh.UserIds   <$> some (strOption (long "ids"      <> metavar "USER-IDs"   <> help "Specify list of target user ids"))
                                 <|> USh.UserName  <$> strOption       (long "username" <> metavar "USER-NAME"  <> help "Specify target with user name")
                                                   <*> option wrapWithJustReader
                                                             (long "host" <> metavar "HOST" <> value Nothing <> help "Specify host instance that target user is on"))

usersShowInfo :: ParserInfo SubCmds
usersShowInfo = Options.Applicative.info (usersShowParser <**> helper) (fullDesc <> progDesc "call users/show API")
-- }}}


-- usersNotesParser {{{
usersNotesParser :: Parser SubCmds
usersNotesParser = UsersNotes <$> (UN.APIRequest <$> strOption (long "id" <> metavar "USER-ID" <> help "Uesr id of the target user")
                                                 <*> switch (long "includeReplies" <> help "whether include replies or not")
                                                 <*> option (readerAsk >>= (\x -> return $ Just $ read x))
                                                                  (long "limit" <> value (Just 10) <> metavar "LIMIT" <> help "Maxmum amount")
                                                 <*> option wrapWithJustReader
                                                                         (long "since" <> value Nothing <> metavar "SINCE" <> help "Grab notes since given id")
                                                 <*> option wrapWithJustReader
                                                                  (long "until" <> value Nothing <> metavar "UNTIL" <> help "Grab notes until given id")
                                                 <*> option maybeUTCTimeReader
                                                                  (long "until" <> value Nothing <> metavar "SINCE-DATE" <> help "Grab notes since given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)")
                                                 <*> option maybeUTCTimeReader
                                                                  (long "until" <> value Nothing <> metavar "UNTIL" <> help "Grab notes until given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)")
                                                 <*> flag False True (long "no-includeMyRenotes" <> help "whether include own renotes or not")
                                                 <*> switch (long "withFiles" <> help "True to grab notes with files")
                                                 <*> (many (option wrapWithJustReader (long "fileType" <> metavar "FILETYPE" <> help "Grab notes with file which is specified filetype")) <**> (NilP $ Just sequence))
                                                 <*> switch (long "excludeNsfw" <> help "True to exclude NSFW contents (use with 'fileType' opt to perform this)"))

usersNotesInfo :: ParserInfo SubCmds
usersNotesInfo = Options.Applicative.info (usersNotesParser <**> helper) (fullDesc <> progDesc "call users/notes API")
-- }}}


commandParser = subparser (command "users/show" (usersShowInfo) <> command "users/notes" (usersNotesInfo))

main :: IO ()
main = do
    apiRequest <- execParser (Options.Applicative.info (commandParser <**> helper) (fullDesc <> progDesc "call Misskey API"))
    let env = MisskeyEnv "" $ "https://" ++ "virtual-kaf.fun"

    case apiRequest of
        UsersShow req  -> runMisskey (USh.usersShow req) env >>= evalResult
        UsersNotes req -> runMisskey (UN.usersNotes req) env >>= evalResult
    where
        evalResult resp = case resp of
                            Left er   -> print $ "Error occured while users/show: " ++ ushow er
                            Right usr -> (putStrLn . ushow) usr

