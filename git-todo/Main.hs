-- | Get todos from git
--
-- Use git-todo to find all of the TODOs in a git repository.
--
-- e.g.
--
-- To see all the todos added between your working tree and HEAD:
--     $ git todo
--
-- To see all the todos in your branch:
--     $ git todo origin/master
--
-- To see all the todos in your code base:
--     $ git todo --files
--
-- git-todo arguments behave like git-diff arguments, unless --files is
-- specified, in which case they act like git-ls-files arguments.

-- TODO: `git todo --help` doesn't work because it needs a manpage. Try to
-- make it work somehow.

module Main (main) where

import Protolude

import qualified Data.Text as Text
import Data.Text.IO (hPutStrLn)
import GHC.IO (FilePath)
import Options.Applicative
  ( ParserInfo
  , argument
  , execParser
  , flag
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , switch
  , str
  )
import System.IO (stderr)
import System.Process (readProcess)

import qualified Fixme
import Fixme.Comment (Comment)


data Config = Diff Bool [FilePath] | Files [FilePath] deriving (Eq, Show)

options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser = modeFlag <*> cachedFlag <*> many (argument str (metavar "FILES..."))

    modeFlag = flag Diff (const Files) (mconcat [ long "files"
                                                , short 'f'
                                                , help "Show all todos in source files instead of examining diffs"
                                                ])
    cachedFlag = switch (mconcat [ long "cached"
                                 , help "Get todos from changes staged for next commit, optionally relative to another commit. Ignored if --files is set."
                                 ])
    description = mconcat
      [ fullDesc
      , progDesc "Get todos from source code in git"
      , header "git-todo - Get todos from source code in git"
      ]

commentsFromDiff :: [FilePath] -> IO [Comment]
commentsFromDiff args =
  either abort pure . Fixme.newCommentsFromDiff =<< gitDiff args
  where
    abort e = do
      hPutStrLn stderr $ "ERROR: " <> e
      exitWith (ExitFailure 1)

commentsFromFiles :: [FilePath] -> IO [Comment]
commentsFromFiles paths =
  concatMapM commentsFrom =<< gitListFiles paths
  where
    -- If we can't figure out the language, then just assume it has no
    -- comments of interest.
    commentsFrom path = fromMaybe (pure []) (Fixme.readComments path)

-- TODO: Use gitlib

-- | Run `git diff <diffspec>` in the current working directory.
gitDiff :: [FilePath] -> IO ByteString
gitDiff args = toS <$> readProcess "git" ("diff":args) ""

-- TODO: Factor out todo reporting

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
  comments <- case config of
                Diff False args -> commentsFromDiff args
                Diff True args  -> commentsFromDiff ("--cached":args)
                Files args      -> commentsFromFiles args
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
