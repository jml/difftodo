-- | Get todos from diffs
--
-- e.g.
--   git diff master... | diff-todo

module Main (main) where

import Protolude

import Data.ByteString (hGetContents)
import Data.Text.IO (hPutStrLn)
import System.IO (stdin, stderr)

import qualified Fixme


main :: IO ()
main = do
  diff <- hGetContents stdin
  case Fixme.newCommentsFromDiff diff of
    Left e -> do
      hPutStrLn stderr $ "ERROR: " <> e
      exitWith (ExitFailure 1)
    Right comments ->
      mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
