module Fixme.Comment
  ( -- * Understand comments
    Comment
  , Located
  , parseComments
  , readComments
  , commentText
  , filename
  , startLine
  , endLine
    -- ** Exposed for testing
  , newComment
  , parseComments'

    -- * Understand programming languages
  , Language
  , languageForFile
    -- ** Exposed for testing
  , highlightCode
  ) where

import Protolude

import qualified Data.Text as Text
import Data.Text.IO (readFile)
import GHC.IO (FilePath)
import Text.Highlighting.Kate
  ( SourceLine
  , Token
  , TokenType(..)
  , highlightAs
  , languagesByFilename
  )


-- | Given some source code, return a list of comments.
parseComments :: Filename -> Language -> Text -> [Comment]
parseComments filename language = parseComments' filename . highlightCode language

-- | Given a consecutive sequence of lexed lines of source, return a list of
-- all the comments found, along with the line number on which the comment
-- starts.
parseComments' :: Filename -> [SourceLine] -> [Comment]
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
      contents <- readFile filename
      pure (parseComments (Just (toS filename)) language contents)


-- | A thing that is located somewhere in a text file.
data Located a = Located { filename :: Filename
                         , startLine :: Int
                         , value :: a
                         } deriving (Eq, Show)

-- | How we identify which blob of text a thing is located in.
type Filename = Maybe Text


type LocatedToken = Located Token

locateTokens :: Filename -> [SourceLine] -> [LocatedToken]
locateTokens fn lines = [Located fn i x | (i, xs) <- zip [0..] lines, x <- xs]


type Comment = Located Text

commentText :: Comment -> Text
commentText = value

newComment :: Filename -> Int -> Text -> Comment
newComment fn i text = Located fn i text

endLine :: Comment -> Int
endLine (Located _ startLine c) = startLine + (Text.count "\n" c)

appendComment :: Comment -> Comment -> Maybe Comment
appendComment x y
  | filename x /= filename y = Nothing
  | startLine y - endLine x > 1 = Nothing
  | startLine y - endLine x == 1 = Just (newComment (filename x) (startLine x) (commentText x <> "\n" <> commentText y))
  | otherwise = Just (newComment (filename x) (startLine x) (commentText x <> commentText y))

getComment :: LocatedToken -> Maybe Comment
getComment (Located filename lineNum token) =
  case getComment' token of
    Nothing -> Nothing
    Just comment -> Just $ Located filename lineNum comment

getComment' :: Token -> Maybe Text
getComment' (AlertTok, value) = Just (toS value)
getComment' (CommentTok, value) = Just (toS value)
getComment' _ = Nothing


-- | Wrappers around syntax highlighting code.

-- TODO: Move these to a separate module, maybe.

type Language = Text

languageForFile :: Text -> Maybe Language
languageForFile = fmap toS . head . languagesByFilename . toS

highlightCode :: Language -> Text -> [SourceLine]
highlightCode language code =
  let highlighted = highlightAs (toS language) (toS code)
  in case Text.toLower language of
       "haskell" -> map fixupHaskellComments highlighted
       _ -> highlighted

  where
    -- | For some reason, Kate highlights a Haskell comment with no text (i.e.
    -- "--") as a function. Fix this up.
    fixupHaskellComments = map (\t -> if t == (FunctionTok, "--") then (CommentTok, "--") else t)
