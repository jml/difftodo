module Main (main) where

import Protolude

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Comment
import qualified Diff
import qualified Todo


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Fixme"
  [ Comment.tests
  , Diff.tests
  , Todo.tests
  ]
