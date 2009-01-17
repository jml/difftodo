# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>

"""A library for extracting TODOs from comments in Python source code."""


def get_comments_from_diff(patches):
    for patch in patches:
        current_comment = []
        last_line_no = 0
        for (line_no, line) in patch.iter_inserted():
            if line_no != last_line_no + 1:
                if current_comment:
                    yield ''.join(current_comment)
                    current_comment = []
            last_line_no = line_no
            stripped_line = line.contents.lstrip()
            if stripped_line.startswith('#'):
                current_comment.append(stripped_line)
            elif len(current_comment) == 0:
                continue
            else:
                yield ''.join(current_comment)
                current_comment = []
        if len(current_comment) != 0:
            yield ''.join(current_comment)
