-- | Get todos from comments

module Fixme.Todo
  ( getTodos
  , formatTodo
  ) where

import Protolude

import qualified Data.Text as Text
import Fixme.Comment (Comment, Located, commentText, filename, startLine)


type Todo = Located Text

getTodos :: Comment -> [Todo]
getTodos comment =
  if any (`Text.isInfixOf` commentText comment) defaultTags then
    [comment] else []

formatTodo :: Todo -> Text
formatTodo todo =
  fn <> ":" <> lineNum <> ":\n" <> indentedComment
  where
    fn = maybe "<unknown>" identity (filename todo)
    lineNum = show (startLine todo)
    indentedComment = Text.unlines (map ("  " <>) (Text.lines (commentText todo)))

defaultTags :: [Text]
defaultTags = [ "XXX"
              , "TODO"
              , "FIXME"
              ]
