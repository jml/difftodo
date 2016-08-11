module Fixme.Comment
  ( -- * Understand comments
    Comment
  , Located
  , parseComments
  , commentText
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
import Text.Highlighting.Kate
  ( SourceLine
  , Token
  , TokenType(..)
  , highlightAs
  , languagesByFilename
  )


-- | Given some source code, return a list of comments.
parseComments :: Language -> Text -> [Comment]
parseComments language = parseComments' . highlightCode language

-- | Given a consecutive sequence of lexed lines of source, return a list of
-- all the comments found, along with the line number on which the comment
-- starts.
parseComments' :: [SourceLine] -> [Comment]
parseComments' =
  coalesce appendComment . mapMaybe getComment . locateTokens

  where
    coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
    coalesce maybeAppend (x:y:ys) =
      case x `maybeAppend` y of
        Nothing -> x:(coalesce maybeAppend (y:ys))
        Just z -> (coalesce maybeAppend (z:ys))
    coalesce _ xs = xs


-- | A thing that is located somewhere in a text file.
data Located a = Located { startLine :: Int
                         , value :: a
                         } deriving (Eq, Show)

type LocatedToken = Located Token

locateTokens :: [SourceLine] -> [LocatedToken]
locateTokens lines = [Located i x | (i, xs) <- zip [0..] lines, x <- xs]


type Comment = Located Text

commentText :: Comment -> Text
commentText = value

newComment :: Int -> Text -> Comment
newComment i text = Located i text

endLine :: Comment -> Int
endLine (Located startLine c) = startLine + (Text.count "\n" c)

appendComment :: Comment -> Comment -> Maybe Comment
appendComment x y
  | startLine y - endLine x > 1 = Nothing
  | startLine y - endLine x == 1 = Just (newComment (startLine x) (commentText x <> "\n" <> commentText y))
  | otherwise = Just (newComment (startLine x) (commentText x <> commentText y))

getComment :: LocatedToken -> Maybe Comment
getComment (Located lineNum token) =
  case getComment' token of
    Nothing -> Nothing
    Just comment -> Just $ Located lineNum comment

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
