-- | Get todos from comments

module Fixme.Todo
  ( getTodos
  , formatTodo
  ) where

import Protolude

import qualified Data.Text as Text
import Fixme.Comment (Comment, Located, commentText)


type Todo = Located Text

getTodos :: Comment -> [Todo]
getTodos comment =
  if any (`Text.isInfixOf` commentText comment) defaultTags then
    [comment] else []

formatTodo :: Todo -> Text
formatTodo = ((<>) "\n") . commentText

defaultTags :: [Text]
defaultTags = [ "XXX"
              , "TODO"
              , "FIXME"
              ]
