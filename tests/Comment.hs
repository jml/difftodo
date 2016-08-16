module Comment (tests) where

import Protolude

import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (unlines)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.Highlighter
  ( lexerFromFilename
  , shortName
  , Token(..)
  , TokenType(..)
  )
import Text.Show.Pretty (ppShow)

import Fixme.Comment
  ( Language
  , parseComments
  , newComment
  , highlightCode
  )


tests :: TestTree
tests =
  testGroup "Fixme.Comment"
  [ -- | Exploratory tests that show how our underlying lexer actually works.
    testGroup "Text.Highlighter"
    [ testCase "parse python" $ do
        let lexed = highlightCode python pythonExample
        let expected = [ comment "# first comment", eol
                       , eol
                       , comment "# second comment", eol
                       , comment "# is multi-line", eol
                       , eol
                       , Token (Literal :. Number :. Integer) "1", text " ", Token Operator "+", text " ", Token (Literal :. Number :. Integer) "2", text "  ", comment "# trailing comment", eol
                       ]
        expected `tokensEqual` lexed
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
        let expected = [ newComment Nothing 0 (ByteString.init example) ]
        expected @=? parseComments Nothing haskell example
    ]
  ]


haskell :: Language
haskell = fromJust (lexerFromFilename "foo.hs")

python :: Language
python =  fromJust (lexerFromFilename "foo.py")

pythonExample :: ByteString
pythonExample = unlines $
  [ "# first comment"
  , ""
  , "# second comment"
  , "# is multi-line"
  , ""
  , "1 + 2  # trailing comment"
  ]


equalsBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
equalsBy _ [] []         = True
equalsBy _ _  []         = False
equalsBy _ [] _          = False
equalsBy p (x:xs) (y:ys) = p x y && equalsBy p xs ys

tokensEqual :: [Token] -> [Token] -> Assertion
tokensEqual xs ys =
  assertBool (ppShow xs <> " /= " <> ppShow ys) (equalsBy tokenEquals xs ys)
  where
    tokenEquals (Token xType xText) (Token yType yText) =
      (and [ shortName xType == shortName yType, xText == yText ])


comment :: ByteString -> Token
comment = Token Comment

text :: ByteString -> Token
text = Token Text

eol :: Token
eol = Token Text "\n"

