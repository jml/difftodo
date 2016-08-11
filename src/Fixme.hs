-- | Routines for getting todos from source code
--
-- No backwards compatibility is promised for any code in this library.

module Fixme
  ( -- | Get comments from code or from diffs
    Comment.parseComments
  , Diff.newCommentsFromDiff
    -- | Turn comments into todos
  , Todo.getTodos
    -- | Format todos
  , Todo.formatTodo
  ) where

import qualified Fixme.Comment as Comment
import qualified Fixme.Diff as Diff
import qualified Fixme.Todo as Todo
