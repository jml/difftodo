-- | Get todos from comments

module Fixme.Todo
  ( getTodos
  , formatTodo
  ) where

import Protolude

import qualified Data.Text as Text
import Fixme.Comment (Comment, Located, commentText, filename, startLine, locatedValue)

type Todo = Located Text

todoText :: Todo -> Text
todoText = locatedValue


getTodos :: Comment -> [Todo]
getTodos comment =
  if any (`Text.isInfixOf` (toS (commentText comment))) defaultTags then
    [toS <$> comment] else []

formatTodo :: Todo -> Text
formatTodo todo =
  fn <> ":" <> lineNum <> ":\n" <> indentedComment
  where
    fn = maybe "<unknown>" identity (filename todo)
    lineNum = show (startLine todo)
    indentedComment = Text.unlines (map ("  " <>) (Text.lines (todoText todo)))

defaultTags :: [Text]
defaultTags = [ "XXX"
              , "TODO"
              , "FIXME"
              ]
