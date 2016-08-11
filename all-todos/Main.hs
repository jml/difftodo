-- | Get todos from source code
--
-- e.g.
--   all-todos foo.py

module Main (main) where

import Protolude

import Data.Text.IO (readFile)
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
import Fixme.Comment (Comment, languageForFile)


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


readComments :: FilePath -> IO (Maybe [Comment])
readComments filename =
  case languageForFile (toS filename) of
    Nothing -> pure Nothing
    Just language -> do
      contents <- readFile filename
      pure (Just $ Fixme.parseComments (Just (toS filename)) language contents)


main :: IO ()
main = do
  files <- execParser options
  forM_ files $ \filename -> do
    comments' <- readComments filename
    case comments' of
      Nothing -> pure ()
      Just comments -> do
        mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
