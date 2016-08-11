-- | Extract comments from diffs

module Fixme.Diff
  ( newCommentsFromDiff
  ) where

import Protolude

import qualified Data.Text as Text
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types
  ( Annotation(..)
  , Content(..)
  , FileDelta(..)
  , FileStatus(..)
  , Hunk(..)
  , Line(..)
  )
import Fixme.Comment
  ( Comment
  , Language
  , languageForFile
  , parseComments
  , startLine
  , endLine
  )

-- TODO: Try to use lenses for this.

-- | Get the comments that were added or modified in a diff.
--
-- Silently ignores files if we can't figure out what their programming
-- language is.
newCommentsFromDiff :: ByteString -> Either Text [Comment]
newCommentsFromDiff =
  bimap toS (join . catMaybes . map getNewCommentsForFile) . parseDiff .toS
  where
    getNewCommentsForFile (FileDelta Deleted _ _ _) = Just []
    getNewCommentsForFile (FileDelta _ _ _ Binary) = Just []
    getNewCommentsForFile (FileDelta _ _ filename (Hunks hunks)) =
      case languageForFile filename of
        Nothing -> Nothing
        Just language -> Just $ concatMap (getNewCommentsForHunk filename language) hunks


-- | Retrieve all the comments that were either added or modified in 'hunk'.
--
-- What we actually want to do is to find all of the TODOs that were added in
-- a branch by means of examining the diff between that branch and 'master',
-- so that we have a way of figuring out what's left to do while working on a
-- branch.
--
-- This means we can use a simplified approach to analyze the code:
--
--  * if a hunk has no insertions, then it can't be a pending todo
--  * if a hunk has no comments, then it can't be a pending todo
--  * if a comment appears in a hunk, but nothing was inserted, then it can't be a
--    pending todo
--  * if a comment appears in a hunk, and there is a line of insertion in that
--    comment, then it *might* be a pending todo
--
-- Here, "comment" means a contiguous sequence of comment tokens.
getNewCommentsForHunk :: Text -> Language -> Hunk -> [Comment]
getNewCommentsForHunk filename language hunk =
  let comments = parseComments (Just filename) language afterText
  in filterInsertions addedLineNumbers comments
  where
    -- | Reconstitute the "after" text of the diff hunk.
    -- That is, the text that you would get after applying the hunk as a patch.
    afterText :: Text
    afterText = Text.unlines $ map lineContent rightSide

    -- | Return only the comments that are touched by the given line numbers.
    --
    -- Assumes that:
    --   * comments are in order
    --   * comments are not overlapping
    --   * line numbers are sorted
    filterInsertions :: [Int] -> [Comment] -> [Comment]
    filterInsertions [] _ = []
    filterInsertions _ [] = []
    filterInsertions lineNums@(i:lineNums') comments@(c:comments') =
      case lineInComment i c of
        LT -> filterInsertions lineNums' comments
        EQ -> c:(filterInsertions lineNums comments')
        GT -> filterInsertions lineNums comments'

    -- | Is this line number before (LT), within (EQ), or after (GT) this
    -- comment?
    lineInComment :: Int -> Comment -> Ordering
    lineInComment lineNum comment
      | lineNum > endLine comment = GT
      | lineNum < startLine comment = LT
      | otherwise = EQ

    -- | Return the line numbers (of the right side of the diff) that are
    -- insertions.
    addedLineNumbers :: [Int]
    addedLineNumbers =
      [ i | (i, line) <- (zip [0..] rightSide)
          , lineAnnotation line == Added ]

    -- | Get the right-hand side of the diff. That is, only the insertions and
    -- context lines. We don't care about the "before", only the "after".
    rightSide :: [Line]
    rightSide = [ line | line <- hunkLines hunk, lineAnnotation line /= Removed ]
