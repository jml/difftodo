module Main (main) where

import Protolude

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Comment
import qualified Diff


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Fixme"
  [ Diff.tests
  , Comment.tests
  ]
