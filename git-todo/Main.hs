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
-- These paths will recurse.

module Main (main) where

import Protolude

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
commentsFromDiff = do
  -- TODO: Read this as a bytestring from the start
  -- TODO: Take git diff flags as options
  diff <- gitDiff "master..."
  case Fixme.newCommentsFromDiff diff of
    Left e -> do
      hPutStrLn stderr $ "ERROR: " <> e
      exitWith (ExitFailure 1)
    Right comments -> pure comments

commentsFromFiles :: [FilePath] -> IO [Comment]
commentsFromFiles paths = concatMapM (map (maybe [] identity) . Fixme.readComments) =<< gitListFiles paths

-- TODO: Use gitlib

-- | Run `git diff <diffspec>` in the current working directory.
gitDiff :: Text -> IO ByteString
gitDiff diffSpec = toS <$> readProcess "git" ["diff", toS diffSpec] ""


-- | Run `git ls-files` with the given files in the current working directory.
gitListFiles :: [FilePath] -> IO [FilePath]
gitListFiles files = do
  -- TODO: Don't assume git repo location is CWD
  output <- readProcess "git" ("ls-files":files) ""
  pure (map toS (Text.lines (toS output)))


main :: IO ()
main = do
  config <- execParser options
  comments <- case paths config of
                [] -> commentsFromDiff
                filenames -> commentsFromFiles filenames
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
