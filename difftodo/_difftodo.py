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

import pygments
from pygments import lexers
from pygments.token import Token


# bytestring -> [(Token, str)]
def lex_diff(text):
    lexer = lexers.get_lexer_by_name('diff')
    lexer.add_filter('tokenmerge')
    return pygments.lex(text, lexer)


# [(Token, str)] -> [(filename, [(line_number, [(Token, [line])])])]
def parse_diff(tokens):
    # XXX: This is actually a parser. Maybe write a proper parse and separate
    # out the new content bit.
    stack = []
    for (token, content) in tokens:
        if token == Token.Generic.Heading:
            if stack:
                yield stack.pop()
            stack.append((_get_filename(content), []))
        elif stack:
            chunk = stack[-1][1]
            if token == Token.Generic.Subheading:
                chunk.append((_get_line_no(content), []))
            elif chunk:
                chunk[-1][1].append((token, _get_lines(content)))
            else:
                pass
        else:
            pass
    while stack:
        yield stack.pop()


def _get_line_no(subheading, _matcher=re.compile('@@ -\d+,\d+ [+](\d+),\d+ @@')):
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
    raise ValueError("Couldn't extract filename from heading: {}".format(header))


def _get_lines(content):
    # Get rid of the '+', '-', ' ' prefix that diff lines have
    return [line[1:] for line in content.splitlines()]


def get_new_content(parsed_diff):
    for filename, chunks in parsed_diff:
        new_chunks = []
        for line_no, content in chunks:
            if Token.Generic.Inserted in (t[0] for t in content):
                new_content = []
                for t, c in content:
                    if t != Token.Generic.Deleted:
                        new_content.extend(c)
                new_chunks.append((line_no, new_content))
        if new_chunks:
            yield filename, new_chunks


# XXX: We've got one major problem:
#
# Comments that appear in a chunk that has insertion are treated as todos,
# when we really only want comments that themselves have been modified.


# filename, line_number, str -> [(filename, line_number, str)]
def get_comments(filename, line_no, code):
    buffered_comments = []
    for comment in _get_comments(filename, line_no, code):
        if buffered_comments:
            last = buffered_comments[-1]
            if _is_continuation(last, comment):
                buffered_comments.append(comment)
            else:
                yield _combine_buffered_comments(buffered_comments)
                buffered_comments = []
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


def _get_comments(filename, line_no, code):
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
