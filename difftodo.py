# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>

"""A library for extracting TODOs from comments in Python source code."""

from bzrlib import patches


def get_comments_from_diff(patch):
    return []


def get_todos_from_diff(patch):
    for comment in get_comment_blocks_from_diff(patch):
        if contains_todo(comment):
            yield make_todo(comment)


def get_comment_blocks_from_diff(patch):
    for chunk in get_chunks(diff):
        for line in get_lines(chunk):
            if is_comment(line):
                yield get_comment_block


