from StringIO import StringIO

from bzrlib.commands import Command, register_command
from bzrlib.diff import get_trees_and_branches_to_diff, show_diff_trees
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
        diff_data = get_trees_and_branches_to_diff(
            None, revision, None, None, apply_view=True)
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
