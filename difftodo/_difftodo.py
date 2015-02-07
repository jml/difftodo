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
    'Comment',
    'Todo',
    'todo_from_comment',
    'todos_from_comments',
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
    lexer = lexers.guess_lexer_for_filename(filename, code)
    for token, content in pygments.lex(code, lexer):
        if token in Token.Comment:
            yield content


class Comment(object):
    """A comment block in a Python source file."""

    def __init__(self, filename, start_line, raw_lines):
        self.filename = filename.split('\t')[0]
        self.start_line = start_line
        self.raw_lines = raw_lines

    @classmethod
    def is_comment(self, line):
        return line.lstrip().startswith('#')

    def __eq__(self, other):
        return all([
            self.filename == other.filename,
            self.start_line == other.start_line,
            self.raw_lines == other.raw_lines])

    def __ne__(self, other):
        return not (self == other)

    def __repr__(self):
        return '%s(%r, %s, %s)' % (
            self.__class__.__name__,
            self.filename,
            self.start_line,
            self.raw_lines)

    def __str__(self):
        lines = ["%s:%s:" % (self.filename, self.start_line)]
        lines.extend(["  " + line for line in self.lines])
        lines.append('')
        return '\n'.join(lines)

    def __contains__(self, text):
        return text in self.text

    def append(self, new_line):
        self.raw_lines.append(new_line)

    @property
    def lines(self):
        return (
            line.lstrip()[2:].rstrip()
            for line in self.raw_lines)

    @property
    def text(self):
        return '\n'.join(self.lines)


class Todo(object):

    def __init__(self, filename, start_line, lines):
        self.filename = filename
        self.start_line = start_line
        self.lines = lines

    def __repr__(self):
        return '%s(%r, %r, %r)' % (
            self.__class__.__name__,
            self.filename, self.start_line, self.lines)

    def __str__(self):
        lines = ["%s:%s:" % (self.filename, self.start_line)]
        lines.extend(["  " + line for line in self.lines])
        lines.append('')
        return '\n'.join(lines)

    def __eq__(self, other):
        return all([
            self.filename == other.filename,
            self.start_line == other.start_line,
            self.lines == other.lines])

    def __ne__(self, other):
        return not (self == other)


def todo_from_comment(comment, tags):
    """Yield all todos hiding in 'comment'."""
    lines = list(comment.lines)
    current_todo_start = None

    for offset, line in enumerate(lines):
        if any(tag in line for tag in tags):
            if current_todo_start is not None:
                yield Todo(
                    comment.filename,
                    comment.start_line + current_todo_start,
                    lines[current_todo_start:offset])
            current_todo_start = offset
    if current_todo_start is not None:
        yield Todo(
            comment.filename,
            comment.start_line + current_todo_start,
            lines[current_todo_start:])


def todos_from_comments(comments, tags):
    for comment in comments:
        for todo in todo_from_comment(comment, tags):
            yield todo
