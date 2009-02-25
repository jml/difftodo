from StringIO import StringIO

from bzrlib.commands import Command, register_command
from bzrlib.diff import _get_trees_to_diff, show_diff_trees
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
        old_tree, new_tree, specific_files, extra_trees = (
            _get_trees_to_diff(None, revision, None, None, apply_view=True))
        stream = StringIO()
        diff = show_diff_trees(old_tree, new_tree, stream,
                               specific_files=specific_files,
                               external_diff_options=None,
                               old_label='', new_label='',
                               extra_trees=extra_trees, using=None)
        stream.seek(0)
        patches = parse_patches(stream)
        comments = get_comments_from_diff(patches)
        tags = ('XXX', 'TODO')
        number = -1
        for number, todo in enumerate(todos_from_comments(comments, tags)):
            print todo
        print "Things to do: %s" % (number + 1)


register_command(cmd_todo)
