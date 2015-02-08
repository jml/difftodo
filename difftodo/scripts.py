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
    get_comments,
    get_new_content,
    lex_diff,
    parse_diff,
)

# XXX: Allow customization of TODO tags.

def comments_from_diff(content):
    # XXX: Called from todos_from_diff script. Currently a quick-and-dirty
    # hack used for rough-and-ready integration testing.
    for filename, chunks in get_new_content(parse_diff(lex_diff(content))):
        for line_no, chunk in chunks:
            for comment in get_comments(filename, '\n'.join(chunk)):
                yield filename, line_no, comment


def todos_from_diff():
    for filename, line_no, comment in comments_from_diff(sys.stdin.read()):
        print '{}:{}:'.format(filename, line_no)
        print comment
        print

