{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import RIO
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
import System.IO (print, putStrLn)
import Network.HTTP.Client
import Network.HTTP.Simple
import Web.Misskey.Type
import Web.Misskey.Api.Internal (call)
import Web.Misskey.Api.Users.Search (UsersSearch(UsersSearch))
import Web.Misskey.Api.Users.Notes (UsersNotes(UsersNotes))
import Web.Misskey.Api.Users.Show (UsersShow, _UserId, _UserIds, _UserName)
import Web.Misskey.Api.Users.Users (UsersUsers(UsersUsers))
import Web.Misskey.Api.Users.Followers (UsersFollowers(UsersFollowers))
import Web.Misskey.Api.Users.Following (UsersFollowing(UsersFollowing))
import Web.Misskey.Api.Notes.Create (NotesCreate(NotesCreate), Visibility(Public))
import Web.Misskey.Api.Notes.Timeline (NotesTimeline(NotesTimeline))
import Web.Misskey.Api.Notes.Show (NotesShow(NoteId))
import Control.Lens (review)
import Options.Applicative
import Options.Applicative.Types (readerAsk, Parser(NilP))

-- | Sum type of all Subcommands.
-- I need this because all 'Command's should have same type.
--
-- Defining sum-type is also recommended in optparse-applicative's README:
-- <https://hackage.haskell.org/package/optparse-applicative-0.17.0.0#commands>
data SubCmds = CmdUsersShow      GeneralOption UsersShow
             | CmdUsersNotes     GeneralOption UsersNotes
             | CmdUsersSearch    GeneralOption UsersSearch
             | CmdUsers          GeneralOption UsersUsers
             | CmdUsersFollowers GeneralOption UsersFollowers
             | CmdUsersFollowing GeneralOption UsersFollowing
             | CmdNotesCreate    GeneralOption NotesCreate
             | CmdNotesTimeline  GeneralOption NotesTimeline
             | CmdNotesShow      GeneralOption NotesShow

-- | 'Lens' for Subcommand 'GeneralOption'
cmdOpt :: Lens' SubCmds GeneralOption
cmdOpt = lens getter setter
  where
    getter (CmdUsersShow      opt _) = opt
    getter (CmdUsersNotes     opt _) = opt
    getter (CmdUsersSearch    opt _) = opt
    getter (CmdUsers          opt _) = opt
    getter (CmdUsersFollowers opt _) = opt
    getter (CmdUsersFollowing opt _) = opt
    getter (CmdNotesCreate    opt _) = opt
    getter (CmdNotesTimeline  opt _) = opt
    getter (CmdNotesShow      opt _) = opt

    setter (CmdUsersShow      _ req) opt = CmdUsersShow      opt req
    setter (CmdUsersNotes     _ req) opt = CmdUsersNotes     opt req
    setter (CmdUsersSearch    _ req) opt = CmdUsersSearch    opt req
    setter (CmdUsers          _ req) opt = CmdUsers          opt req
    setter (CmdUsersFollowers _ req) opt = CmdUsersFollowers opt req
    setter (CmdUsersFollowing _ req) opt = CmdUsersFollowing opt req
    setter (CmdNotesCreate    _ req) opt = CmdNotesCreate    opt req
    setter (CmdNotesTimeline  _ req) opt = CmdNotesTimeline  opt req
    setter (CmdNotesShow      _ req) opt = CmdNotesShow      opt req

-- | Apply GeneralOption to SubCmds if it doesn't have GeneralOption
applyGeneralOption :: SubCmds -> GeneralOption -> SubCmds
applyGeneralOption subcmd opt = subcmd&cmdOpt%~(<> opt)


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
usersShowParser = CmdUsersShow NoOption <$> ((review _UserId)        <$> strOption       (long "id"       <> metavar "USER-ID"    <> help "Specify target with user id")
                                          <|> (review _UserIds)   <$> some (strOption (long "ids"      <> metavar "USER-IDs"   <> help "Specify list of target user ids"))
                                          <|> (review _UserName)  <$> ((,) <$> strOption       (long "username" <> metavar "USER-NAME"  <> help "Specify target with user name")
                                                                              <*> option maybeStr
                                                                                        (long "host" <> metavar "HOST" <> value Nothing <> help "Specify host instance that target user is on")))

usersShowInfo :: ParserInfo SubCmds
usersShowInfo = Options.Applicative.info (usersShowParser <**> helper) (fullDesc <> progDesc "call users/show API")
-- }}}

-- usersSearchParser {{{
usersSearchParser :: Parser SubCmds
usersSearchParser = CmdUsersSearch NoOption <$> (UsersSearch <$> strOption (long "query" <> metavar "QUERY-STRING" <> help "Query string")
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
usersNotesParser = CmdUsersNotes NoOption <$> (UsersNotes <$> strOption (long "id" <> metavar "USER-ID" <> help "Uesr id of the target user")
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
usersParser = CmdUsers NoOption <$> (UsersUsers <$> option maybeAuto (long "limit"  <> value Nothing <> metavar "LIMIT"  <> help "Maxmum amount")
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
usersFollowersParser = CmdUsersFollowers NoOption <$> (UsersFollowers <$> option maybeStr (long "userId"   <> value Nothing <> metavar "USER-ID"  <> help "Target user id")
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
usersFollowingParser = CmdUsersFollowing NoOption <$> (UsersFollowing <$> option maybeStr (long "userId"   <> value Nothing <> metavar "USER-ID"  <> help "Target user id")
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
-- __'Poll' is currently disabled__
notesCreateParser :: Parser SubCmds
notesCreateParser = CmdNotesCreate NoOption <$> (NotesCreate <$> option auto (long "visibility" <> value Public <> metavar "VISIBILITY" <> help "Visibility range [Public|Home|Followers|Specified]")
                                                            <*> many (strOption (long "visibleUserId" <> metavar "VISIBLE-USER-ID" <> help "Users who can read the note(if visibility is 'Specified')"))
                                                            <*> option maybeStr (long "text" <> value Nothing <> metavar "TEXT" <> help "Text to post")
                                                            <*> option maybeStr (long "cw"   <> value Nothing <> metavar "CW"   <> help "warning of content. This will hide note content")
                                                            <*> switch (long "viaMobile" <> help "via mobile or not")
                                                            <*> switch (long "localOnly" <> help "True to only post to local")
                                                            <*> switch (long "noExtractMentions" <> help "True to suspend extracting mentions from text")
                                                            <*> switch (long "noExtractHashtags" <> help "True to suspend extracting hashtags from text")
                                                            <*> switch (long "noExtractEmojis" <> help "True to suspend extracting emojis from text")
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
notesTimelineParser = CmdNotesTimeline NoOption <$> (NotesTimeline
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

-- {{{ notesShowParser 
notesShowParser :: Parser SubCmds
notesShowParser = CmdNotesShow NoOption <$> (NoteId <$> strArgument (metavar "NOTE_ID"))

notesShowParserInfo :: ParserInfo SubCmds
notesShowParserInfo = Options.Applicative.info (notesShowParser <**> helper) (fullDesc <> progDesc "call notes/show API")
-- }}}

commandParser = subparser $ command    "users/show"     usersShowInfo
                            <> command "users/notes"    usersNotesInfo
                            <> command "users/search"   usersSearchInfo
                            <> command "users"          usersInfo
                            <> command "users/followers" usersFollowersInfo
                            <> command "users/following" usersFollowingInfo
                            <> command "notes/create"   notesCreateParserInfo
                            <> command "notes/timeline" notesTimelineParserInfo
                            <> command "notes/show"     notesShowParserInfo

-- GeneralOption {{{
data GeneralOption = NoOption
                   | GeneralOption { quiet :: Bool
                                   }
instance Semigroup GeneralOption where
  NoOption <> opt = opt
  opt <> NoOption = opt
  opt <> _        = opt
  
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

    runRIO env $ do 
      case apiRequest of
        CmdUsersShow opt req      -> call req >>= evalResult opt
        CmdUsersNotes opt req     -> call req >>= evalResult opt
        CmdUsersSearch opt req    -> call req >>= evalResult opt
        CmdUsers opt req          -> call req >>= evalResult opt
        CmdUsersFollowers opt req -> call req >>= evalResult opt
        CmdUsersFollowing opt req -> call req >>= evalResult opt
        CmdNotesCreate opt req    -> call req >>= evalResult opt
        CmdNotesTimeline opt req  -> call req >>= evalResult opt
        CmdNotesShow opt req      -> call req >>= evalResult opt
        where
          evalResult NoOption = liftIO . putStrLn . ushow
          evalResult opt      = when (not $ quiet opt) . liftIO . putStrLn . ushow
