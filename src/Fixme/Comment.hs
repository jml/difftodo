module Fixme.Comment
  ( -- * Understand comments
    Comment
  , parseComments
  , readComments
  , commentText
  , filename
  , startLine
  , endLine
    -- ** Generic support for things located in files
  , Located
  , locatedValue
    -- ** Exposed for testing
  , newComment

    -- * Understand programming languages
  , Language
  , languageForFile
    -- ** Exposed for testing
  , highlightCode
  ) where

import Protolude

import qualified Data.ByteString as ByteString
import GHC.IO (FilePath)
import Text.Highlighter
  ( lexerFromFilename
  , runLexer
  , Lexer
  , Token(..)
  , TokenType(..)
  )


-- | Given some source code, return a list of comments.
parseComments :: Filename -> Language -> ByteString -> [Comment]
parseComments filename language = parseComments' filename . highlightCode language

-- | Given a consecutive sequence of lexed lines of source, return a list of
-- all the comments found, along with the line number on which the comment
-- starts.
parseComments' :: Filename -> [Token] -> [Comment]
parseComments' filename =
  coalesce appendComment . mapMaybe getComment . locateTokens filename

  where
    coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
    coalesce maybeAppend (x:y:ys) =
      case x `maybeAppend` y of
        Nothing -> x:(coalesce maybeAppend (y:ys))
        Just z -> (coalesce maybeAppend (z:ys))
    coalesce _ xs = xs


-- | Read the given file, and parse out any comments.
--
-- Return Nothing if we cannot determine what language the file is in. Raises
-- exceptions on bad IO, and also if the file cannot be decoded to Text.
readComments :: FilePath -> Maybe (IO [Comment])
readComments filename =
  case languageForFile (toS filename) of
    Nothing -> Nothing
    Just language -> Just $ do
      contents <- ByteString.readFile filename
      pure (parseComments (Just (toS filename)) language contents)


-- | A thing that is located somewhere in a text file.
data Located a = Located { filename :: Filename
                         , startLine :: Int
                         , locatedValue :: a
                         } deriving (Eq, Show)

instance Functor Located where
  fmap f (Located fn start x) = Located fn start (f x)

-- | How we identify which blob of text a thing is located in.
type Filename = Maybe Text

locateTokens :: Filename -> [Token] -> [Located Token]
locateTokens fn tokens =
  [ Located fn i t | (i, t) <- zip startLines tokens ]
  where
    startLines = scanl (+) 0 [ numLines value | Token _ value <- tokens ]

numLines :: ByteString -> Int
numLines = ByteString.count newline
  where newline = fromIntegral (ord '\n')

type Comment = Located ByteString

-- XXX: Rename commentText to be less misleading about type
commentText :: Comment -> ByteString
commentText = locatedValue

newComment :: Filename -> Int -> ByteString -> Comment
newComment fn i text = Located fn i text

endLine :: Comment -> Int
endLine (Located _ startLine c) = startLine + (numLines c)

appendComment :: Comment -> Comment -> Maybe Comment
appendComment x y
  | filename x /= filename y = Nothing
  | startLine y - endLine x > 1 = Nothing
  | startLine y - endLine x == 1 = Just (newComment (filename x) (startLine x) (commentText x <> "\n" <> commentText y))
  | otherwise = Just (newComment (filename x) (startLine x) (commentText x <> commentText y))

getComment :: Located Token -> Maybe Comment
getComment (Located filename lineNum token) =
  case getComment' token of
    Nothing -> Nothing
    Just comment -> Just $ Located filename lineNum comment

  where
    getComment' :: Token -> Maybe ByteString
    getComment' (Token tType value) = bool Nothing (Just value) (isComment tType)

    isComment Comment = True
    isComment (Arbitrary "Comment") = True
    isComment (x :. y) = isComment x || isComment y
    isComment _ = False


-- | Wrappers around syntax highlighting code.

-- TODO: Move these to a separate module, maybe.

type Language = Lexer

languageForFile :: FilePath -> Maybe Language
languageForFile = lexerFromFilename

highlightCode :: Language -> ByteString -> [Token]
highlightCode language code =
  case runLexer language code of
    Left _ -> []
    Right tokens -> tokens
