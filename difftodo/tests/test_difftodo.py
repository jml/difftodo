# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>
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


"""Tests for extracting TODOs from comments in Python source code."""

from testtools import TestCase

from pygments.token import Token

from difftodo._difftodo import (
    get_comments,
    get_new_content,
    get_new_comments,
    lex_diff,
    parse_diff,
)


class TestLexDiffs(TestCase):
    """Test our ability to lex diffs."""

    def lex_diff(self, text):
        return list(lex_diff(text))

    def test_empty(self):
        self.assertEqual([(Token.Text, u'\n')], self.lex_diff(''))

    def test_simple_bzr_diff(self):
        diff = ("""\
=== modified file 'a'
--- a	2009-01-17 05:07:59 +0000
+++ a	2009-01-17 05:08:20 +0000
@@ -9,4 +9,5 @@
         # Line 1
         # Line 2
         # Line 3
+        # Line 4
         pass
""")
        expected = [
            (Token.Generic.Heading, u"=== modified file 'a'\n"),
            (Token.Generic.Deleted, u'--- a\t2009-01-17 05:07:59 +0000\n'),
            (Token.Generic.Inserted, u'+++ a\t2009-01-17 05:08:20 +0000\n'),
            (Token.Generic.Subheading, u'@@ -9,4 +9,5 @@\n'),
            (Token.Text, u'         # Line 1\n         # Line 2\n         # Line 3\n'),
            (Token.Generic.Inserted, u'+        # Line 4\n'),
            (Token.Text, u'         pass\n')]
        self.assertEqual(expected, self.lex_diff(diff))

    def test_simple_git_diff(self):
        diff = ('''\
diff --git a/.gitignore b/.gitignore
index 241973c..9bbeb13 100644
--- a/.gitignore
+++ b/.gitignore
@@ -1,3 +1,4 @@
+.env/
 *.pyc
 /_trial_temp/
 /difftodo.egg-info/
diff --git a/difftodo/_difftodo.py b/difftodo/_difftodo.py
index 1618681..cda4adb 100644
--- a/difftodo/_difftodo.py
+++ b/difftodo/_difftodo.py
@@ -26,6 +26,10 @@ from bzrlib import patches
 
 from extensions import filter_none
 
+# A comment
+
+# XXX: Use Pygments to do all of our lexing for us.
+
 
 class Comment(object):
     """A comment block in a Python source file."""
''')
        expected = [
            (Token.Generic.Heading,
             u'diff --git a/.gitignore b/.gitignore\nindex 241973c..9bbeb13 100644\n'),
            (Token.Generic.Deleted, u'--- a/.gitignore\n'),
            (Token.Generic.Inserted, u'+++ b/.gitignore\n'),
            (Token.Generic.Subheading, u'@@ -1,3 +1,4 @@\n'),
            (Token.Generic.Inserted, u'+.env/\n'),
            (Token.Text, u' *.pyc\n /_trial_temp/\n /difftodo.egg-info/\n'),
            (Token.Generic.Heading,
             u'diff --git a/difftodo/_difftodo.py b/difftodo/_difftodo.py\nindex '
             '1618681..cda4adb 100644\n'),
            (Token.Generic.Deleted, u'--- a/difftodo/_difftodo.py\n'),
            (Token.Generic.Inserted, u'+++ b/difftodo/_difftodo.py\n'),
            (Token.Generic.Subheading,
             u'@@ -26,6 +26,10 @@ from bzrlib import patches\n'),
            (Token.Text, u' \n from extensions import filter_none\n \n'),
            (Token.Generic.Inserted,
             u'+# A comment\n+\n+# XXX: Use Pygments to do all of our lexing for us.\n+\n'),
            (Token.Text,
             u' \n class Comment(object):\n     """A comment block in a Python source file."""\n')
        ]
        self.assertEqual(expected, self.lex_diff(diff))


class TestParseDiff(TestCase):
    """Given a lexed diff, parse it."""

    def test_git_diff(self):
        tokens = [
            (Token.Generic.Heading,
             u'diff --git a/.gitignore b/.gitignore\nindex 241973c..9bbeb13 100644\n'),
            (Token.Generic.Deleted, u'--- a/.gitignore\n'),
            (Token.Generic.Inserted, u'+++ b/.gitignore\n'),
            (Token.Generic.Subheading, u'@@ -1,3 +1,4 @@\n'),
            (Token.Generic.Inserted, u'+.env/\n'),
            (Token.Text, u' *.pyc\n /_trial_temp/\n /difftodo.egg-info/\n'),
            (Token.Generic.Heading,
             u'diff --git a/difftodo/_difftodo.py b/difftodo/_difftodo.py\nindex '
             '1618681..cda4adb 100644\n'),
            (Token.Generic.Deleted, u'--- a/difftodo/_difftodo.py\n'),
            (Token.Generic.Inserted, u'+++ b/difftodo/_difftodo.py\n'),
            (Token.Generic.Subheading,
             u'@@ -26,6 +26,10 @@ from bzrlib import patches\n'),
            (Token.Text, u' \n from extensions import filter_none\n \n'),
            (Token.Generic.Inserted,
             u'+# A comment\n+\n+# XXX: Use Pygments to do all of our lexing for us.\n+\n'),
            (Token.Text,
             u' \n class Comment(object):\n     """A comment block in a Python source file."""\n')
        ]
        # Got two choices:
        # - infer the contents of the new file from the diff
        # - given the new content, load it from the file on disk
        expected = [
            ('.gitignore', 1, [
                (Token.Generic.Inserted, '+.env/\n'),
                (Token.Text,
                 ' *.pyc\n'
                 ' /_trial_temp/\n'
                 ' /difftodo.egg-info/\n'
                ),
            ]),
            # XXX: Throws away extra diff chunk header info
            ('difftodo/_difftodo.py', 26, [
                (Token.Text,
                 ' \n'
                 ' from extensions import filter_none\n'
                 ' \n'
                ),
                (Token.Generic.Inserted,
                 '+# A comment\n'
                 '+\n'
                 '+# XXX: Use Pygments to do all of our lexing for us.\n'
                 '+\n'
                ),
                (Token.Text,
                 ' \n'
                 ' class Comment(object):\n'
                 '     """A comment block in a Python source file."""\n'
                ),
            ]),
        ]
        self.assertEqual(expected, list(parse_diff(tokens)))

    def test_git_add(self):
        tokens = [
            (Token.Generic.Heading, u'diff --git a/example.py b/example.py\n'),
            (Token.Text, u'new file mode 100644\n'),
            (Token.Generic.Heading, u'index 0000000..3edf921\n'),
            (Token.Generic.Deleted, u'--- /dev/null\n'),
            (Token.Generic.Inserted, u'+++ b/example.py\n'),
            (Token.Generic.Subheading, u'@@ -0,0 +1,3 @@\n'),
            (Token.Generic.Inserted, u'+\n+# XXX: this is a todo\n+\n'),
        ]
        expected = [
            ('example.py', 1, [
                (Token.Generic.Inserted, u'+\n+# XXX: this is a todo\n+\n'),
            ]),
        ]
        self.assertEqual(expected, list(parse_diff(tokens)))

    def test_bzr_diff(self):
        tokens = [
            (Token.Generic.Heading, u"=== modified file 'a'\n"),
            (Token.Generic.Deleted, u'--- a\t2009-01-17 05:07:59 +0000\n'),
            (Token.Generic.Inserted, u'+++ a\t2009-01-17 05:08:20 +0000\n'),
            (Token.Generic.Subheading, u'@@ -9,4 +9,5 @@\n'),
            (Token.Text, u'         # Line 1\n         # Line 2\n         # Line 3\n'),
            (Token.Generic.Inserted, u'+        # Line 4\n'),
            (Token.Text, u'         pass\n')]
        expected = [
            ('a', 9, [
                (Token.Text,
                    '         # Line 1\n'
                    '         # Line 2\n'
                    '         # Line 3\n'
                ),
                (Token.Generic.Inserted, '+        # Line 4\n'),
                (Token.Text, '         pass\n'),
            ]),
        ]
        self.assertEqual(expected, list(parse_diff(tokens)))

    def test_subheading_no_chunk_length(self):
        # Sometimes subheadings don't have a chunk length, e.g.
        #   @@ -1 +1 @@
        #
        # As opposed to:
        #   @@ -1,3 +1,4 @@
        tokens = [
            (Token.Generic.Heading,
             u'diff --git a/version.txt b/version.txt\nindex 6207c74..d78c785 100644\n'),
            (Token.Generic.Deleted, u'--- a/version.txt\n'),
            (Token.Generic.Inserted, u'+++ b/version.txt\n'),
            (Token.Generic.Subheading, u'@@ -1 +1 @@\n'),
            (Token.Generic.Deleted, u'-v0.13.0-0-gf4d2787\n'),
            (Token.Text, u'\\ No newline at end of file\n'),
            (Token.Generic.Inserted, u'+v0.13.0-2-g676934f\n'),
            (Token.Text, u'\\ No newline at end of file\n        \n'),
        ]
        expected = [
            ('version.txt', 1,
             [
                 (Token.Generic.Deleted, u'-v0.13.0-0-gf4d2787\n'),
                 (Token.Text, u'\\ No newline at end of file\n'),
                 (Token.Generic.Inserted, u'+v0.13.0-2-g676934f\n'),
                 (Token.Text, u'\\ No newline at end of file\n        \n'),
             ]),
        ]
        self.assertEqual(expected, list(parse_diff(tokens)))


class TestNewContent(TestCase):

    def test_strip_files_with_only_deletes(self):
        parsed = [
            (u'a', 6, [
                (Token.Text,
                    u' class TestBar(unittest.TestCase):\n'
                    u' \n'
                    u'     def test_bar(self):\n'),
                (Token.Generic.Deleted,
                    u'-        # This test is going to be awesome.\n'),
                (Token.Text, u' pass\n'),
            ]),
        ]
        expected = []
        self.assertEqual(expected, list(get_new_content(parsed)))

    def test_strip_chunks_with_only_deletes(self):
        parsed = [
            (u'a', 6, [
                (Token.Text,
                    u' class TestBar(unittest.TestCase):\n'
                    u' \n'
                    u'     def test_bar(self):\n'),
                (Token.Generic.Deleted,
                    u'-        # This test is going to be awesome.\n'),
                (Token.Text, u'pass\n')]),
            (u'a', 20, [
                (Token.Text,
                    u' class TestFoo(unittest.TestCase):\n'
                    u' \n'
                    u'     def test_foo(self):\n'),
                (Token.Generic.Inserted,
                    u'+        # This is the real awesome.\n'),
                (Token.Text, u' pass\n')]),
        ]
        expected = [
            (u'a', 20,
             u'class TestFoo(unittest.TestCase):\n'
             u'\n'
             u'    def test_foo(self):\n'
             u'        # This is the real awesome.\n'
             u'pass\n'),
        ]
        self.assertEqual(expected, list(get_new_content(parsed)))

    def test_strip_deletes_within_chunk(self):
        parsed = [
            (u'a', 6, [
                (Token.Text,
                 u' class TestBar(unittest.TestCase):\n'
                 u' \n'
                 u'     def test_bar(self):\n'),
                (Token.Generic.Deleted,
                 u'-        # This test is going to be awesome.\n'),
                (Token.Generic.Inserted,
                 u'+        # This test is awesome.\n'),
                (Token.Text,
                 u' pass\n')]),
        ]
        expected = [
            (u'a', 6,
             u'class TestBar(unittest.TestCase):\n'
             u'\n'
             u'    def test_bar(self):\n'
             u'        # This test is awesome.\n'
             u'pass\n'),
        ]
        self.assertEqual(expected, list(get_new_content(parsed)))


class TestGetComments(TestCase):

    def test_empty_code(self):
        code = ''
        self.assertEqual([], list(get_comments('foo.py', 1, code)))

    def test_only_comments(self):
        code = """
        # This is a comment.
        """
        self.assertEqual(
            [(1, 8, '# This is a comment.')], list(get_comments('foo.py', 1, code)))

    def test_all_comments(self):
        code = """
        # This is a comment.
        x = 1
        # This is another.
        x = 2
        # Final comment:
        # spans lines
        x = 3
        """
        self.assertEqual(
            [(1, 8, u'# This is a comment.'),
             (3, 8, u'# This is another.'),
             (5, 8, u'# Final comment:\n# spans lines')],
            list(get_comments('foo.py', 1, code)))

    def test_non_python_comments(self):
        code = "/* This is also a comment */"
        self.assertEqual(
            [(1, 0, '/* This is also a comment */')], list(get_comments('foo.c', 1, code)))

    def test_multi_line_comments(self):
        code = """\
        /* This is a comment.
         *
         * It spans multiple lines.
         */
        """
        self.assertEqual([(1, 8, code[8:].strip())], list(get_comments('foo.c', 1, code)))

    def test_multi_line_python_comments(self):
        code = """\
# This is a single comment
# that spans two
# or three lines.
        """
        self.assertEqual(
            [(1, 0, '# This is a single comment\n# that spans two\n# or three lines.')],
            list(get_comments('foo.py', 1, code)))

    def test_multiple_c_style_comments(self):
        code = """\
/* This is one comment */
/* This is another */
        """
        self.assertEqual(
            [(1, 0, '/* This is one comment */'),
             (2, 0, '/* This is another */')],
            list(get_comments('foo.c', 1, code)))


class TestGetNewComments(TestCase):

    def test_only_deletions(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'),
            (Token.Generic.Deleted,
             u'-        # This test is going to be awesome.\n'),
            (Token.Text, u' pass\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 1, diff)
        self.assertEqual([], list(new_comments))

    def test_only_insertion(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'),
            (Token.Generic.Inserted,
             u'+        # This test is going to be awesome.\n'),
            (Token.Text, u' pass\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 1, diff)
        self.assertEqual(
            [(4, 8, '# This test is going to be awesome.')], list(new_comments))

    def test_later_insertion(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'),
            (Token.Generic.Inserted,
             u'+        # This test is going to be awesome.\n'),
            (Token.Text, u' pass\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 10, diff)
        self.assertEqual(
            [(13, 8, '# This test is going to be awesome.')], list(new_comments))

    def test_exclude_unchanged_comments(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'
             u'         # This comment will not appear in output.\n'),
            (Token.Generic.Inserted,
             u'+        x = 1 + 2\n'),
            (Token.Text,
             u'         self.assertEqual(4, x)\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 1, diff)
        self.assertEqual([], list(new_comments))

    def test_include_appended_comments(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'
             u'         # This comment will appear in output.\n'),
            (Token.Generic.Inserted,
             u'+        # Because this line was added.\n'),
            (Token.Text,
             u'         self.assertEqual(4, x)\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 1, diff)
        self.assertEqual(
            [(4, 8,
              '# This comment will appear in output.\n'
              '# Because this line was added.')],
            list(new_comments))

    def test_include_prepended_comments(self):
        diff = [
            (Token.Text,
             u' class TestBar(unittest.TestCase):\n'
             u' \n'
             u'     def test_bar(self):\n'),
            (Token.Generic.Inserted,
             u'+        # This comment will appear in output.\n'
             ),
            (Token.Text,
             u'         # Because this line was added.\n'
             u'         self.assertEqual(4, x)\n'),
        ]
        new_comments = get_new_comments('test_bar.py', 1, diff)
        self.assertEqual(
            [(4, 8,
              '# This comment will appear in output.\n'
              '# Because this line was added.')],
            list(new_comments))
