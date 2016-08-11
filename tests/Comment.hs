module Comment (tests) where

import Protolude

import Data.Text (stripEnd, unlines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Text.Highlighting.Kate (TokenType(..))

import Fixme.Comment
  ( parseComments'
  , parseComments
  , newComment
  , highlightCode
  )


tests :: TestTree
tests =
  testGroup "Fixme.Comment"
  [ testGroup "Text.Highlighting.Kate"
    [ testCase "parse python" $ do
        let lexed = highlightCode "python" (toS pythonExample)
        let expected = [ [(CommentTok,"# first comment")]
                       , []
                       , [(CommentTok,"# second comment")]
                       , [(CommentTok,"# is multi-line")]
                       , []
                       , [(DecValTok,"1"),(NormalTok," "),(OperatorTok,"+"),(NormalTok," "),(DecValTok,"2"),(NormalTok,"  "),(CommentTok,"# trailing comment")]
                       ]
        expected @=? lexed
    , testCase "parse haskell" $ do
        let lexed = highlightCode "haskell" (toS haskellExample)
        let expected = [ [(CommentTok,"-- first comment")]
                       , []
                       , [(CommentTok,"-- second comment")]
                       , [(CommentTok,"-- is multi-line")]
                       , []
                       , [(CommentTok,"{- comment block start")]
                       , [(CommentTok,"continues")]
                       , [(CommentTok,"ends -}")]
                       , []
                       , [ (NormalTok,"f "),(FunctionTok,"="),(NormalTok," "),(CommentTok,"{- foo -}"),(NormalTok," ")
                         , (StringTok,"\"bar\""),(NormalTok," "),(CommentTok,"{- baz -}"),(NormalTok," ")
                         , (CommentTok,"-- qux")
                         ]
                       ]
        expected @=? lexed
    , testCase "parse indented haskell" $ do
        let lexed = highlightCode "haskell" (toS indentedHaskellExample)
        let expected = [ [(CommentTok,"-- first comment")]
                       , []
                       , [(NormalTok," "),(NormalTok," "),(CommentTok,"-- second comment")]
                       , [(NormalTok," "),(NormalTok," "),(CommentTok,"-- is multi-line")]
                       , []
                       , [(CommentTok,"{- comment block start")]
                       , [(CommentTok,"   continues")]
                       , [(CommentTok,"   ends -}")]
                       ]
        expected @=? lexed
    , testCase "parse comment with newline" $ do
        let example = unlines $ [ "-- first line"
                                , "--"
                                , "-- second line"
                                ]
        let expected = [ [(CommentTok,"-- first line")]
                       , [(CommentTok,"--")]
                       , [(CommentTok,"-- second line")]
                       ]
        expected @=? highlightCode "haskell" (toS example)
    , testCase "parse comment with newline (indented)" $ do
        let example = unlines $ [ "  -- first line"
                                , "  --"
                                , "  -- second line"
                                ]
        let expected = [ [(NormalTok," "),(NormalTok," "),(CommentTok,"-- first line")]
                       , [(NormalTok," "),(NormalTok," "),(CommentTok,"--")]
                       , [(NormalTok," "),(NormalTok," "),(CommentTok,"-- second line")]
                       ]
        expected @=? highlightCode "haskell" (toS example)
    , testCase "parse comment with newline (python)" $ do
        let example = unlines $ [ "# first line"
                                , "#"
                                , "# second line"
                                ]
        let expected = [ [(CommentTok,"# first line")]
                       , [(CommentTok,"#")]
                       , [(CommentTok,"# second line")]
                       ]
        expected @=? highlightCode "python" (toS example)
    ]
  , testGroup "Parsing comments from tokens"
    [ testCase "Parse Python example" $ do
        let input = [ [(CommentTok,"# first comment")]
                    , []
                    , [(CommentTok,"# second comment")]
                    , [(CommentTok,"# is multi-line")]
                    , []
                    , [(DecValTok,"1"),(NormalTok," "),(OperatorTok,"+"),(NormalTok," "),(DecValTok,"2"),(NormalTok,"  "),(CommentTok,"# trailing comment")]
                    ]
        let expected = [ newComment Nothing 0 "# first comment"
                       , newComment Nothing 2 "# second comment\n# is multi-line"
                       , newComment Nothing 5 "# trailing comment"
                       ]
        expected @=? parseComments' Nothing input
    , testCase "Parse indented example" $ do
        let input = [ [(NormalTok," "),(NormalTok," "),(CommentTok,"-- second comment")]
                    , [(NormalTok," "),(NormalTok," "),(CommentTok,"-- is multi-line")]
                    ]
        let expected = [ newComment Nothing 0 "-- second comment\n-- is multi-line" ]
        expected @=? parseComments' Nothing input
    , testCase "Commented blank line continues comment" $ do
        let input = [ [(CommentTok,"# comment")]
                    , [(CommentTok,"#")]
                    , [(CommentTok,"# is multi-line")]
                    ]
        let expected = [ newComment Nothing 0 "# comment\n#\n# is multi-line" ]
        expected @=? parseComments' Nothing input
    ]
  , testGroup "Parsing comments from source"
    [ testCase "Multi-line Haskell comment" $ do
        let example = unlines $
              [ "-- | First line."
              , "--"
              , "-- Explanatory"
              , "-- paragraph."
              , "--"
              , "-- Conclusion"
              ]
        let expected = [ newComment Nothing 0 (stripEnd example) ]
        expected @=? parseComments Nothing "haskell" example
    ]
  ]

pythonExample :: Text
pythonExample = unlines $
  [ "# first comment"
  , ""
  , "# second comment"
  , "# is multi-line"
  , ""
  , "1 + 2  # trailing comment"
  ]

haskellExample :: Text
haskellExample = unlines $
  [ "-- first comment"
  , ""
  , "-- second comment"
  , "-- is multi-line"
  , ""
  , "{- comment block start"
  , "continues"
  , "ends -}"
  , ""
  , "f = {- foo -} \"bar\" {- baz -} -- qux"
  ]

indentedHaskellExample :: Text
indentedHaskellExample = unlines $
  [ "-- first comment"
  , ""
  , "  -- second comment"
  , "  -- is multi-line"
  , ""
  , "{- comment block start"
  , "   continues"
  , "   ends -}"
  ]
