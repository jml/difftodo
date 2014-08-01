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

from StringIO import StringIO

from bzrlib.commands import Command, register_command
from bzrlib.diff import get_trees_and_branches_to_diff_locked, show_diff_trees
from bzrlib.option import (
    _parse_revision_str,
    )
from bzrlib.patches import parse_patches

from difftodo import get_comments_from_diff, todos_from_comments


class cmd_todo(Command):
    """Find the TODO items added to a branch.
    """

    takes_args = ['location?']

    def run(self):
        revision = _parse_revision_str('submit:')
        diff_data = get_trees_and_branches_to_diff_locked(
            None, revision, None, None, self.add_cleanup, apply_view=True)
        (old_tree, new_tree, old_branch, new_branch, specific_files,
         extra_trees) = diff_data
        stream = StringIO()
        show_diff_trees(
            old_tree, new_tree, stream, old_label='', new_label='',
            extra_trees=extra_trees)
        stream.seek(0)
        patches = parse_patches(stream)
        comments = get_comments_from_diff(patches)
        tags = ('XXX', 'TODO')
        number = -1
        for number, todo in enumerate(todos_from_comments(comments, tags)):
            print todo
        print "Things to do: %s" % (number + 1)


register_command(cmd_todo)
