# Copyright (c) 2009-2014 Jonathan M. Lange <jml@mumak.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""A library for extracting TODOs from comments in Python source code."""

__all__ = [
]

import re
import StringIO

import pygments
from pygments import lexers
from pygments.token import Token


# bytestring -> [(Token, str)]
def lex_diff(text):
    lexer = lexers.get_lexer_by_name('diff')
    lexer.add_filter('tokenmerge')
    return pygments.lex(text, lexer)


# [(Token, str)] -> [(filename, [(line_number, [(Token, [line])])])]
def _parse_diff(tokens):
    # XXX: This is actually a parser. Maybe swap this out for someone else's
    # better mainatained parser (unidiff?).

    # XXX: Heavily biased toward new content.
    stack = []
    for (token, content) in tokens:
        if token == Token.Generic.Heading:
            filename = _get_filename(content)
            if filename:
                if stack:
                    yield stack.pop()
                stack.append((filename, []))
        elif stack:
            chunk = stack[-1][1]
            if token == Token.Generic.Subheading:
                chunk.append((_get_line_no(content), []))
            elif chunk:
                chunk[-1][1].append((token, content))
            else:
                # We ignore any tokens that don't appear after a subheading.
                pass
        else:
            pass
    while stack:
        yield stack.pop()


# [(Token, str)] -> [(filename, line_number, [(Token, str)])]
def parse_diff(tokens):
    # XXX: Maybe we can kill off _parse_diff later.
    for filename, chunks in _parse_diff(tokens):
        for line_number, content in chunks:
            yield filename, line_number, content


def _get_line_no(subheading, _matcher=re.compile('@@ -\d+(?:,\d+)? [+](\d+)(?:,\d+)? @@')):
    return int(_matcher.search(subheading).group(1))


_filename_res = map(re.compile, [
    r'b/(.+)\n',
    r'=== modified file \'(.+)\'',
])
def _get_filename(header):
    for matcher in _filename_res:
        match = matcher.search(header)
        if match:
            return match.group(1)
    return None


def get_new_content(parsed_diff):
    for filename, line_no, content in parsed_diff:
        if _contains_insert(content):
            yield filename, line_no, ''.join(_get_new_content(content))


def _contains_insert(chunk_content):
    return Token.Generic.Inserted in (t[0] for t in chunk_content)


def _get_new_content(chunk_content):
    for token, content in chunk_content:
        if token != Token.Generic.Deleted:
            for line in content.splitlines(True):
                yield line[1:]


def get_new_comments(filename, line_no, diff):
    comments = get_comments(filename, line_no, ''.join(_get_new_content(diff)))
    comment_end = -1
    for insert_start, insert_end in _get_inserted_lines(diff, line_no):
        while not comment_end > insert_start:
            comment_start, col, comment = comments.next()
            comment_end = comment_start + len(comment.splitlines())

        while comment_start < insert_end:
            yield comment_start, col, comment
            comment_start, col, comment = comments.next()
            comment_end = comment_start + len(comment.splitlines())


def _get_inserted_lines(diff, starting_line):
    for line, col, token, content in annotate(diff, starting_line):
        if token == Token.Generic.Inserted:
            yield line, line + len(content.splitlines())


# filename, line_number, str -> [(line_number, col, str)]
def get_comments(filename, line_no, code):
    buffered_comments = []
    for comment in _iter_comments(filename, line_no, code):
        if buffered_comments:
            last = buffered_comments[-1]
            if _is_continuation(last, comment):
                buffered_comments.append(comment)
            else:
                yield _combine_buffered_comments(buffered_comments)
                buffered_comments = [comment]
        else:
            # If it's multi-line, we can trust that the lexer has correctly
            # combined things.
            if comment[2] == Token.Comment.Multiline:
                yield comment[0], comment[1], comment[3]
            else:
                buffered_comments.append(comment)

    if buffered_comments:
        yield _combine_buffered_comments(buffered_comments)


def _is_continuation(last, comment):
    return comment[0] == last[0] + 1 and comment[1] == last[1] and comment[2] == last[2]


def _combine_buffered_comments(comments):
    if not comments:
        raise ValueError("Expected multiple comments, got empty list")
    content = '\n'.join(c[3] for c in comments)
    first = comments[0]
    return first[0], first[1], content


def _iter_comments(filename, line_no, code):
    for line, col, token, content in annotate(_lex_code(filename, code), line_no):
        if token in Token.Comment:
            yield line, col, token, content


def _lex_code(filename, code):
    try:
        lexer = lexers.guess_lexer_for_filename(filename, code)
    except pygments.util.ClassNotFound:
        return iter([])
    return pygments.lex(code, lexer)


# XXX: Untested
# [(Token, str)] -> [(line_number, column_number, Token, str)]
def annotate(tokens, starting_line=1, starting_column=0):
    line = starting_line
    col = starting_column
    for (token, content) in tokens:
        yield line, col, token, content
        lines = content.split('\n')
        new_lines = len(lines) - 1
        if new_lines:
            line += new_lines
            col = len(lines[-1])
        else:
            col += len(content)
