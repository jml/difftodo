module Todo (tests) where

import Protolude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Fixme.Comment (newComment)
import Fixme.Todo (getTodos)

tests :: TestTree
tests =
  testGroup "Fixme.Todo"
  [ testCase "No todos" $ do
      let comment = newComment (Just "somefile") 1 "# wolverhampton"
      let expected = []
      expected @=? getTodos comment
  , testCase "One todo" $ do
      let comment = newComment (Just "somefile") 1 "# TODO: wolverhampton"
      let expected = [ toS <$> comment ]
      let observed = getTodos comment
      expected @=? observed
  ]
