# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>

"""A library for extracting TODOs from comments in Python source code."""


from bzrlib import patches


class PatchParser(object):

    def __init__(self, patch):
        self.patch = patch
        self._hunks = patch.hunks

    def _iter_patch_lines(self):
        for hunk in self._hunks:
            pos = hunk.mod_pos - 1
            for line in hunk.lines:
                yield (pos, line)
                pos += 1

    def _get_handler_for_line(self, line):
        if isinstance(line, patches.ContextLine):
            return self.context_line_received
        elif isinstance(line, patches.InsertLine):
            return self.insert_line_received
        elif isinstance(line, patches.RemoveLine):
            return self.remove_line_received
        else:
            raise AssertionError("Cannot handle %r" % (line,))

    def parse(self):
        for pos, line in self._iter_patch_lines():
            handler = self._get_handler_for_line(line)
            result = handler(pos, line.contents)
            if result is not None:
                yield result
        result = self.patch_finished()
        if result is not None:
            yield result

    def patch_finished(self):
        """Called when patch parsing is finished."""

    def insert_line_received(self, line_number, line_contents):
        """Called when a insert line of diff is received."""

    def context_line_received(self, line_number, line_contents):
        """Called when a context line of diff is received."""

    def remove_line_received(self, line_number, line_contents):
        """Called when a remove line of diff is received."""


class CommentParser(PatchParser):

    def __init__(self, patch):
        super(CommentParser, self).__init__(patch)
        self._insert_in_comment = False
        self._current_comment = []

    def is_comment(self, contents):
        return contents.lstrip().startswith('#')

    def _end_comment(self):
        if len(self._current_comment) == 0:
            return
        current_comment, self._current_comment = self._current_comment, []
        if not self._insert_in_comment:
            return
        comment = ''.join((line.lstrip() for line in current_comment))
        self._insert_in_comment = False
        return comment

    def _add_to_comment(self, contents):
        self._current_comment.append(contents)

    def insert_line_received(self, line_number, contents):
        if self.is_comment(contents):
            self._insert_in_comment = True
            self._add_to_comment(contents)
        else:
            return self._end_comment()

    def context_line_received(self, line_number, contents):
        if self.is_comment(contents):
            self._add_to_comment(contents)
        else:
            return self._end_comment()

    def remove_line_received(self, line_number, contents):
        if not self.is_comment(contents):
            return self._end_comment()

    def patch_finished(self):
        return self._end_comment()


def get_comments_from_diff(patches):
    for patch in patches:
        for comment in CommentParser(patch).parse():
            yield comment
