# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>

"""A library for extracting TODOs from comments in Python source code."""


from bzrlib import patches


class PatchParser(object):
    """Parser for a single patch."""

    def __init__(self, patch):
        self.patch = patch
        self._hunks = patch.hunks

    def parse(self):
        self.patch_started()
        for hunk in self._hunks:
            self.hunk_started(hunk)
            pos = hunk.mod_pos - 1
            for line in hunk.lines:
                if isinstance(line, patches.ContextLine):
                    self.context_line_received(hunk, pos, line.contents)
                elif isinstance(line, patches.InsertLine):
                    self.insert_line_received(hunk, pos, line.contents)
                elif isinstance(line, patches.RemoveLine):
                    self.insert_line_received(hunk, pos, line.contents)
                pos += 1
            self.hunk_finished(hunk)
        self.patch_finished()

    def patch_started(self):
        """Called when patch parsing is started."""

    def patch_finished(self):
        """Called when patch parsing is finished."""

    def hunk_started(self, hunk):
        """Called when 'hunk' is started."""

    def hunk_finished(self, hunk):
        """Called when 'hunk' is finished."""

    def insert_line_received(self, hunk, line_number, line_contents):
        """Called when a insert line of diff is received."""

    def context_line_received(self, hunk, line_number, line_contents):
        """Called when a context line of diff is received."""

    def remove_line_received(self, hunk, line_number, line_contents):
        """Called when a remove line of diff is received."""


class CommentParser(PatchParser):

    def __init__(self, patch):
        super(CommentParser, self).__init__(patch)
        self._current_comment = []

    def is_comment(self, contents):
        return contents.lstrip().startswith('#')

    def _end_comment(self):
        if len(self._current_comment) == 0:
            return
        self.comment_received(
            ''.join((line.lstrip() for line in self._current_comment)))
        self._current_comment = []

    def insert_line_received(self, hunk, line_number, contents):
        if self.is_comment(contents):
            self._current_comment.append(contents)
        else:
            self._end_comment()

    def context_line_received(self, hunk, line_number, contents):
        self._end_comment()

    def remove_line_received(self, hunk, line_number, contents):
        self._end_comment()

    def patch_finished(self):
        self._end_comment()

    def comment_received(self, comment):
        pass


def get_comments_from_diff(patches):
    comments = []
    for patch in patches:
        parser = CommentParser(patch)
        parser.comment_received = comments.append
        parser.parse()
    return comments
