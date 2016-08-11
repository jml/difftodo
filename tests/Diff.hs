module Diff (tests) where

import Protolude

import qualified Data.ByteString.Char8
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Fixme.Comment (newComment)
import Fixme.Diff (newCommentsFromDiff)


tests :: TestTree
tests =
  testGroup "Fixme.Diff"
  [ testCase "New file" $ do
      let input = Data.ByteString.Char8.unlines $
            [ "diff --git a/src/Fixme/Diff.hs b/src/Fixme/Diff.hs"
            , "new file mode 100644"
            , "index 0000000..c926970"
            , "--- /dev/null"
            , "+++ b/src/Fixme/Diff.hs"
            , "@@ -0,0 +1,8 @@"
            , "+-- | Extract comments from diffs"
            , "+"
            , "+module Fixme.Diff"
            , "+  ( parseComments"
            , "+  , newCommentsFromDiff"
            , "+  , newComment"
            , "+  , comment"
            , "+  ) where"
            ]
      let expected = [ newComment 0 "-- | Extract comments from diffs"]
      Right expected @=? newCommentsFromDiff input
  , testCase "Multi-line comment" $ do
      let input = Data.ByteString.Char8.unlines $
            [ "diff --git a/src/Fixme/Diff.hs b/src/Fixme/Diff.hs"
            , "new file mode 100644"
            , "index 0000000..c926970"
            , "--- /dev/null"
            , "+++ b/src/Fixme/Diff.hs"
            , "@@ -0,0 +1,10 @@"
            , "+-- | Extract comments from diffs"
            , "+--"
            , "+-- Yep."
            , "+"
            , "+module Fixme.Diff"
            , "+  ( parseComments"
            , "+  , newCommentsFromDiff"
            , "+  , newComment"
            , "+  , comment"
            , "+  ) where"
            ]
      let expected = [ newComment 0 "-- | Extract comments from diffs\n--\n-- Yep."]
      Right expected @=? newCommentsFromDiff input
  ]
