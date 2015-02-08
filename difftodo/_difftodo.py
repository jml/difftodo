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


def lex_diff(text):
    lexer = lexers.get_lexer_by_name('diff')
    lexer.add_filter('tokenmerge')
    return pygments.lex(text, lexer)


def parse_diff(tokens):
    # XXX: This is actually a parser. Maybe write a proper parse and separate
    # out the new content bit.
    stack = []
    for (token, content) in tokens:
        if token == Token.Generic.Heading:
            if stack:
                yield stack.pop()
            stack.append((_get_filename(content), []))
        else:
            if not stack:
                continue
            else:
                chunk = stack[-1][1]
                if token == Token.Generic.Subheading:
                    chunk.append((_get_line_no(content), []))
                else:
                    if chunk:
                        chunk[-1][1].append((token, _get_lines(content)))
                    else:
                        continue
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


def get_comments(filename, code):
    try:
        lexer = lexers.guess_lexer_for_filename(filename, code)
    except pygments.util.ClassNotFound:
        return
    for token, content in pygments.lex(code, lexer):
        if token in Token.Comment:
            yield content


def get_todos_from_diff(content):
    # XXX: Called from todos_from_diff script. Currently a quick-and-dirty
    # hack used for rough-and-ready integration testing.
    i = 0
    for filename, chunks in get_new_content(parse_diff(lex_diff(content))):
        for line_no, chunk in chunks:
            for comment in get_comments(filename, '\n'.join(chunk)):
                i += 1
                print 'Comment #{}'.format(i)
                print comment
