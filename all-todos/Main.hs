-- | Get todos from source code
--
-- e.g.
--   all-todos foo.py

module Main (main) where

import Protolude

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

import qualified Fixme


options :: ParserInfo [FilePath]
options =
  info (helper <*> parser) description
  where
    parser = many (argument str (metavar "FILES..."))

    description = mconcat
      [ fullDesc
      , progDesc "Find all todos from source code"
      , header "all-todos - Get all todos in source code"
      ]


main :: IO ()
main = do
  files <- execParser options
  comments <- concatMapM commentsFrom files
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
  where
    -- If we can't figure out the language, then just assume it has no
    -- comments of interest.
    commentsFrom path = fromMaybe (pure []) (Fixme.readComments path)
