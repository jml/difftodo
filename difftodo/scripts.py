#!/usr/bin/env python
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

import sys

import argparse

from difftodo._difftodo import (
    get_comments,
    get_new_comments,
    lex_diff,
    parse_diff,
)


DEFAULT_TAGS = ('XXX', 'FIXME', 'TODO')


def human_format(todos, output):
    counter = 0
    for todo in todos:
        human_format_todo(todo, output)
        counter += 1
        output.write('\n')
    output.write("Things to do: {}\n".format(counter))


def human_format_todo((filename, line_no, col, comment), output):
    output.write('{}:{}:{}:\n'.format(filename, line_no, col))
    output.write(comment)
    output.write('\n')


def iter_diff_todos(content, tags):
    # XXX: Called from todos_from_diff script. Currently a quick-and-dirty
    # hack used for rough-and-ready integration testing.
    for filename, line_no, chunk in parse_diff(lex_diff(content)):
        for line, col, comment in get_new_comments(filename, line_no, chunk):
            if not tags or any(tag in comment for tag in tags):
                yield filename, line, col, comment


def diff_todo(input_stream, output_stream, formatter, tags):
    # XXX: Loads everything into memory
    formatter(iter_diff_todos(input_stream.read(), tags), output_stream)


def todos_from_diff():
    parser = argparse.ArgumentParser(
        description="Show code todos from a diff, read from STDIN",
    )
    parser.add_argument(
        '--tag', action='append',
        help="Words in comments that indicate a thing to do")
    args = parser.parse_args()
    tags = args.tag if args.tag else DEFAULT_TAGS
    diff_todo(sys.stdin, sys.stdout, human_format, tags)


def iter_all_todos(filenames, tags):
    for filename in filenames:
        # XXX: Everything in memory again.
        data = open(filename, 'r').read()
        for line_no, col_no, comment in get_comments(filename, 1, data):
            if not tags or any(tag in comment for tag in tags):
                yield filename, line_no, col_no, comment


def cmd_all_todos():
    parser = argparse.ArgumentParser(
        description="Show todos in code",
    )
    parser.add_argument(
        '--tag', action='append',
        help="Words in comments that indicate a thing to do"
    )
    parser.add_argument(
        'files', type=str, nargs='+',
        help='Where to look for todos')
    args = parser.parse_args()
    tags = args.tag if args.tag else DEFAULT_TAGS
    human_format(iter_all_todos(args.files, tags), sys.stdout)
