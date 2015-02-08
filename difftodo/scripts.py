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

from difftodo._difftodo import (
    get_new_comments,
    lex_diff,
    parse_diff,
)

# XXX: Allow customization of TODO tags.


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


def comments_from_diff(content):
    # XXX: Called from todos_from_diff script. Currently a quick-and-dirty
    # hack used for rough-and-ready integration testing.
    for filename, line_no, chunk in parse_diff(lex_diff(content)):
        for line, col, comment in get_new_comments(filename, line_no, chunk):
            yield filename, line, col, comment


def todos_from_diff():
    data = sys.stdin.read()
    human_format(comments_from_diff(data), sys.stdout)
