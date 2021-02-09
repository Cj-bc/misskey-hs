{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (when)
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Either (isLeft)
import Data.Yaml (decodeFileEither, ParseException)
import Text.Show.Unicode (ushow)
import System.Environment (getArgs, getEnv)
import System.Exit (die)
import Network.HTTP.Client
import Network.HTTP.Simple
import Web.Misskey.Type
import Web.Misskey.Api.Users.Search (usersSearch)
import qualified Web.Misskey.Api.Users.Search as USe
import qualified Web.Misskey.Api.Users.Notes  as UN
import qualified Web.Misskey.Api.Users.Show   as USh
import qualified Web.Misskey.Api.Users.Users  as US
import qualified Web.Misskey.Api.Users.Followers as UFr
import qualified Web.Misskey.Api.Users.Following as UFi
import qualified Web.Misskey.Api.Notes.Create as NC
import qualified Web.Misskey.Api.Notes.Timeline as NT
import Control.Lens ((^.))
import Options.Applicative
import Options.Applicative.Types (readerAsk, Parser(NilP))

data SubCmds = UsersShow      GeneralOption USh.APIRequest
             | UsersNotes     GeneralOption UN.APIRequest
             | UsersSearch    GeneralOption USe.APIRequest
             | Users          GeneralOption US.APIRequest
             | UsersFollowers GeneralOption UFr.APIRequest
             | UsersFollowing GeneralOption UFi.APIRequest
             | NotesCreate    GeneralOption NC.APIRequest
             | NotesTimeline  GeneralOption NT.APIRequest


-- | Apply GeneralOption to SubCmds if it doesn't have GeneralOption
applyGeneralOption :: SubCmds -> GeneralOption -> SubCmds
applyGeneralOption (UsersShow      NoOption req) opt = UsersShow      opt req
applyGeneralOption (UsersNotes     NoOption req) opt = UsersNotes     opt req
applyGeneralOption (UsersSearch    NoOption req) opt = UsersSearch    opt req
applyGeneralOption (Users          NoOption req) opt = Users          opt req
applyGeneralOption (UsersFollowers NoOption req) opt = UsersFollowers opt req
applyGeneralOption (UsersFollowing NoOption req) opt = UsersFollowing opt req
applyGeneralOption (NotesCreate    NoOption req) opt = NotesCreate    opt req
applyGeneralOption other _ = other


-- Custom readers for optparse {{{
maybeStr :: ReadM (Maybe String)
maybeStr = Just <$> str

maybeAuto :: Read a => ReadM (Maybe a)
maybeAuto = Just <$> auto

maybeUTCTimeReader :: ReadM (Maybe UTCTime)
maybeUTCTimeReader = parseISO8601 <$> readerAsk
-- }}}

-- Custom builder for optparse {{{
reversedSwitch = flag False True
-- }}}


-- usersShowParser {{{

usersShowParser :: Parser SubCmds
usersShowParser = UsersShow NoOption <$> (USh.UserId        <$> strOption       (long "id"       <> metavar "USER-ID"    <> help "Specify target with user id")
                                          <|> USh.UserIds   <$> some (strOption (long "ids"      <> metavar "USER-IDs"   <> help "Specify list of target user ids"))
                                          <|> USh.UserName  <$> strOption       (long "username" <> metavar "USER-NAME"  <> help "Specify target with user name")
                                                            <*> option maybeStr
                                                                      (long "host" <> metavar "HOST" <> value Nothing <> help "Specify host instance that target user is on"))

usersShowInfo :: ParserInfo SubCmds
usersShowInfo = Options.Applicative.info (usersShowParser <**> helper) (fullDesc <> progDesc "call users/show API")
-- }}}

-- usersSearchParser {{{
usersSearchParser :: Parser SubCmds
usersSearchParser = UsersSearch NoOption <$> (USe.APIRequest <$> strOption (long "query" <> metavar "QUERY-STRING" <> help "Query string")
                                                             <*> option maybeAuto  (long "offset" <> value Nothing <> help "Offset")
                                                             <*> option maybeAuto (long "limit"  <> value (Just 10) <> help "Number to grab")
                                                             <*> option maybeAuto (long "localOnly" <> value Nothing <> help "True to search for only local users")
                                                             <*> option maybeAuto (long "detail" <> value Nothing <> help "True to contains detailed user info")
                                             )

usersSearchInfo :: ParserInfo SubCmds
usersSearchInfo = Options.Applicative.info (usersSearchParser <**> helper) (fullDesc <> progDesc "call users/search API")
--}}}

-- usersNotesParser {{{
usersNotesParser :: Parser SubCmds
usersNotesParser = UsersNotes NoOption <$> (UN.APIRequest <$> strOption (long "id" <> metavar "USER-ID" <> help "Uesr id of the target user")
                                                          <*> switch (long "includeReplies" <> help "whether include replies or not")
                                                          <*> option maybeAuto
                                                                           (long "limit" <> value (Just 10) <> metavar "LIMIT" <> help "Maxmum amount")
                                                          <*> option maybeStr
                                                                         (long "sinceId" <> value Nothing <> metavar "SINCE" <> help "Grab notes since given id")
                                                          <*> option maybeStr
                                                                           (long "untilId" <> value Nothing <> metavar "UNTIL" <> help "Grab notes until given id")
                                                          <*> option maybeUTCTimeReader
                                                                           (long "sinceDate" <> value Nothing <> metavar "SINCE-DATE" <> help "Grab notes since given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)")
                                                          <*> option maybeUTCTimeReader
                                                                           (long "untilDate" <> value Nothing <> metavar "UNTIL" <> help "Grab notes until given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)")
                                                          <*> flag False True (long "no-includeMyRenotes" <> help "whether include own renotes or not")
                                                          <*> switch (long "withFiles" <> help "True to grab notes with files")
                                                          <*> fmap sequence (many (option maybeStr (long "fileType" <> metavar "FILETYPE" <> help "Grab notes with file which is specified filetype")))
                                                          <*> switch (long "excludeNsfw" <> help "True to exclude NSFW contents (use with 'fileType' opt to perform this)")
                                           )

usersNotesInfo :: ParserInfo SubCmds
usersNotesInfo = Options.Applicative.info (usersNotesParser <**> helper) (fullDesc <> progDesc "call users/notes API")
-- }}}

-- usersParser {{{
usersParser :: Parser SubCmds
usersParser = Users NoOption <$> (US.APIRequest <$> option maybeAuto (long "limit"  <> value Nothing <> metavar "LIMIT"  <> help "Maxmum amount")
                                                <*> option maybeAuto (long "offset" <> value Nothing <> metavar "OFFSET" <> help "Offset")
                                                <*> option maybeAuto (long "sort"   <> value Nothing <> metavar "SORT"   <> help "Specify sorting. [+follow|-follow|+createdAt|-createdAt|+updatedAt|-updatedAt]")
                                                <*> option maybeAuto (long "state"  <> value Nothing <> metavar "STATE"  <> help "Filter for role. [all|admin|moderator|adminOrModerator|alive]")
                                                <*> option maybeAuto (long "origin" <> value Nothing <> metavar "origin" <> help "Filter for origin. [combined|local|remote]")
                                 )

usersInfo :: ParserInfo SubCmds
usersInfo = Options.Applicative.info (usersParser <**> helper) (fullDesc <> progDesc "call users API")
-- }}}

-- usersFollowersParser {{{
usersFollowersParser :: Parser SubCmds
usersFollowersParser = UsersFollowers NoOption <$> (UFr.APIRequest <$> option maybeStr (long "userId"   <> value Nothing <> metavar "USER-ID"  <> help "Target user id")
                                                                   <*> option maybeStr (long "username" <> value Nothing <> metavar "USERNAME" <> help "Target user name")
                                                                   <*> option maybeStr (long "host"     <> value Nothing <> metavar "HOST"     <> help "Host")
                                                                   <*> option maybeStr (long "sinceId"  <> value Nothing <> metavar "SINCE-ID")
                                                                   <*> option maybeStr (long "untilId"  <> value Nothing <> metavar "UNTIL-ID")
                                                                   <*> option maybeAuto (long "limit"    <> value Nothing <> metavar "LIMIT"    <> help "Limit amount of users to fetch(default: 10)")
                                                                   )

usersFollowersInfo :: ParserInfo SubCmds
usersFollowersInfo = Options.Applicative.info (usersFollowersParser <**> helper) (fullDesc <> progDesc "call users/followers API")
-- }}}

-- usersFollowingParser {{{
usersFollowingParser :: Parser SubCmds
usersFollowingParser = UsersFollowing NoOption <$> (UFi.APIRequest <$> option maybeStr (long "userId"   <> value Nothing <> metavar "USER-ID"  <> help "Target user id")
                                                                   <*> option maybeStr (long "username" <> value Nothing <> metavar "USERNAME" <> help "Target user name")
                                                                   <*> option maybeStr (long "host"     <> value Nothing <> metavar "HOST"     <> help "Host")
                                                                   <*> option maybeStr (long "sinceId"  <> value Nothing <> metavar "SINCE-ID")
                                                                   <*> option maybeStr (long "untilId"  <> value Nothing <> metavar "UNTIL-ID")
                                                                   <*> option maybeAuto (long "limit"   <> value Nothing <> metavar "LIMIT"    <> help "Limit amount of users to fetch(default: 10)")
                                                   )

usersFollowingInfo :: ParserInfo SubCmds
usersFollowingInfo = Options.Applicative.info (usersFollowingParser <**> helper) (fullDesc <> progDesc "call users/following API")
-- }}}

-- notesCreateParser {{{
-- | Parse args for notes/create API
--
-- __'Geo' and 'Poll' is currently disabled__
notesCreateParser :: Parser SubCmds
notesCreateParser = NotesCreate NoOption <$> (NC.APIRequest <$> option auto (long "visibility" <> value NC.Public <> metavar "VISIBILITY" <> help "Visibility range [Public|Home|Followers|Specified]")
                                                            <*> many (strOption (long "visibleUserId" <> metavar "VISIBLE-USER-ID" <> help "Users who can read the note(if visibility is 'Specified')"))
                                                            <*> option maybeStr (long "text" <> value Nothing <> metavar "TEXT" <> help "Text to post")
                                                            <*> option maybeStr (long "cw"   <> value Nothing <> metavar "CW"   <> help "warning of content. This will hide note content")
                                                            <*> switch (long "viaMobile" <> help "via mobile or not")
                                                            <*> switch (long "localOnly" <> help "True to only post to local")
                                                            <*> switch (long "noExtractMentions" <> help "True to suspend extracting mentions from text")
                                                            <*> switch (long "noExtractHashtags" <> help "True to suspend extracting hashtags from text")
                                                            <*> switch (long "noExtractEmojis" <> help "True to suspend extracting emojis from text")
                                                            <*> pure Nothing
                                                            <*> many (strOption (long "fileId" <> metavar "FILEID" <> help "file IDs to add"))
                                                            <*> strOption (long "replyId" <> value "" <> help "target to reply")
                                                            <*> strOption (long "renoteId" <> value "" <> help "target to renote")
                                                            <*> pure Nothing
                                             )

notesCreateParserInfo :: ParserInfo SubCmds
notesCreateParserInfo = Options.Applicative.info (notesCreateParser <**> helper) (fullDesc <> progDesc "call notes/create API")
-- }}}

-- notesTimelineParser {{{
notesTimelineParser :: Parser SubCmds
notesTimelineParser = NotesTimeline NoOption <$> (NT.APIRequest
        <$> option maybeAuto          (long "limit"                    <> help "Limit amount of Notes to fetch(default: 10)"               <> value Nothing <> metavar "LIMIT" )
        <*> option maybeStr           (long "sinceId"                  <> help "Grab notes since given id"                                 <> value Nothing <> metavar "SINCEID")
        <*> option maybeStr           (long "untilId"                  <> help "Grab notes until given id"                                 <> value Nothing <> metavar "UNTILID")
        <*> option maybeUTCTimeReader (long "sinceDate"                <> help "Grab notes since given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)" <> value Nothing <> metavar "SINCEDATE")
        <*> option maybeUTCTimeReader (long "untilDate"                <> help "Grab notes until given time(YYYY-MM-DDTHH:mm:SS+TIMEZONE)" <> value Nothing <> metavar "UNTILDATE")
        <*> reversedSwitch            (long "no-includeMyRenotes"      <> help "whether include notes renoted by yourself")
        <*> reversedSwitch            (long "no-includeRenotedMyNotes" <> help "whether include your renoted notes")
        <*> reversedSwitch            (long "no-includeLocalRenotes"   <> help "whether include renoted local notes")
        <*> switch                    (long "withFiles"                <> help "True to only grab notes with files")
                                                 )

notesTimelineParserInfo :: ParserInfo SubCmds
notesTimelineParserInfo = Options.Applicative.info (notesTimelineParser <**> helper) (fullDesc <> progDesc "call notes/timeline API")
-- }}}


commandParser = subparser $ command    "users/show"     usersShowInfo
                            <> command "users/notes"    usersNotesInfo
                            <> command "users/search"   usersSearchInfo
                            <> command "users"          usersInfo
                            <> command "users/followers" usersFollowersInfo
                            <> command "users/following" usersFollowingInfo
                            <> command "notes/create"   notesCreateParserInfo
                            <> command "notes/timeline" notesTimelineParserInfo

-- GeneralOption {{{
data GeneralOption = NoOption
                   | GeneralOption { quiet :: Bool
                                   }
generalOptionParser = GeneralOption <$> switch (long "quiet" <> short 'q' <> help "Quiet output")

-- }}}

-- Config File related {{{
data ConfigFile = ConfigFile {token         :: String
                             , instance_url :: String
                             } deriving (Show)
$(deriveJSON defaultOptions ''ConfigFile)


-- }}}

main :: IO ()
main = do
    -- Prepare env
    apiRequest <- execParser (Options.Applicative.info ((applyGeneralOption <$> commandParser <*> generalOptionParser) <**> helper) (fullDesc <> progDesc "call Misskey API"))

    home <- getEnv "HOME"
    cfgEither <- decodeFileEither $ home ++ "/.config/misskey-hs/config.yaml" :: IO (Either ParseException ConfigFile)

    when (isLeft cfgEither) $ die $ show cfgEither

    let (Right cfg) = cfgEither
        env         = MisskeyEnv (token cfg) $ "https://" ++ (instance_url cfg)


    case apiRequest of
        UsersShow opt req      -> runMisskey (USh.usersShow req) env      >>= evalResult opt
        UsersNotes opt req     -> runMisskey (UN.usersNotes req) env      >>= evalResult opt
        UsersSearch opt req    -> runMisskey (USe.usersSearch req) env    >>= evalResult opt
        Users opt req          -> runMisskey (US.users req) env           >>= evalResult opt
        UsersFollowers opt req -> runMisskey (UFr.usersFollowers req) env >>= evalResult opt
        UsersFollowing opt req -> runMisskey (UFi.usersFollowing req) env >>= evalResult opt
        NotesCreate opt req    -> runMisskey (NC.notesCreate req) env     >>= evalResult opt
        NotesTimeline opt req  -> runMisskey (NT.notesTimeline req) env   >>= evalResult opt
    where
        evalResult NoOption resp = case resp of
                                Left er   -> print $ "Error occured while users/show: " ++ ushow er
                                Right usr -> (putStrLn . ushow) usr
        evalResult opt resp = case resp of
                                Left er   -> print $ "Error occured while users/show: " ++ ushow er
                                Right usr -> when (not $ quiet opt) $ (putStrLn . ushow) usr

