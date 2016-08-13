-- | Get todos from git
--
-- A bare `git todo` invocation will do different things based on your git
-- checkout state:
--
-- If you have local, unstaged changes, it will show all todos in the unstaged
-- code.
--
-- If you have no local changes, it will show all of the todos between your branch and
-- `master`.
--
-- You can also see the todos present in particular files by running
--
--     $ git todo <path> <path> ...
--
-- These paths will recurse, using git ls-files.

module Main (main) where

import Protolude

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Data.Text.IO (hPutStrLn)
import GHC.IO (FilePath)
import Options.Applicative
  ( ParserInfo
  , argument
  , execParser
  , fullDesc
  , header
  , helper
  , info
  , metavar
  , progDesc
  , str
  )
import System.IO (stderr)
import System.Process (readProcess)

import qualified Fixme
import Fixme.Comment (Comment)


data Config = Config { paths :: [FilePath]
                     } deriving (Eq, Show)


options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser = Config <$> many (argument str (metavar "FILES..."))

    description = mconcat
      [ fullDesc
      , progDesc "Get todos from source code in git"
      , header "git-todo - Get todos from source code in git"
      ]

commentsFromDiff :: IO [Comment]
commentsFromDiff =
  -- TODO: Take git diff flags as option
  either abort pure . Fixme.newCommentsFromDiff =<< loadDiff

  where
    abort e = do
      hPutStrLn stderr $ "ERROR: " <> e
      exitWith (ExitFailure 1)

    -- TODO: Read this as a bytestring from the start
    loadDiff = do
      d <- gitDiff Nothing
      if ByteString.null d
        then gitDiff (Just "master...")
        else pure d

commentsFromFiles :: [FilePath] -> IO [Comment]
commentsFromFiles paths =
  concatMapM commentsFrom =<< gitListFiles paths
  where
    -- If we can't figure out the language, then just assume it has no
    -- comments of interest.
    commentsFrom path = fromMaybe (pure []) (Fixme.readComments path)

-- TODO: Use gitlib

-- | Run `git diff <diffspec>` in the current working directory.
gitDiff :: Maybe Text -> IO ByteString
gitDiff diffSpec =
  toS <$> readProcess "git" ("diff":arg) ""
  where
    arg = maybeToList (toS <$> diffSpec)

-- TODO: Factor out todo reporting

-- TODO: Kind of slow. About 3.7s on a git repo with 232 files.

-- | Run `git ls-files` with the given files in the current working directory.
gitListFiles :: [FilePath] -> IO [FilePath]
gitListFiles files =
  -- TODO: Don't assume git repo location is CWD
  toLines <$> readProcess "git" ("ls-files":files) ""
  where
    toLines = map toS . Text.lines . toS


main :: IO ()
main = do
  config <- execParser options
  comments <- case paths config of
                [] -> commentsFromDiff
                filenames -> commentsFromFiles filenames
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
