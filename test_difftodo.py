# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>

"""Tests for extracting TODOs from comments in Python source code."""


from bzrlib import patches
from bzrlib.tests import TestCase

from difftodo import get_comments_from_diff


class TestTodoFinder(TestCase):

    def parse_diff(self, diff_text):
        diff_lines = (line + '\n' for line in diff_text.splitlines())
        return patches.parse_patches(diff_lines)

    def test_get_comments_from_diff_no_comments(self):
        # If the diff doesn't change any comments, then get_comments won't
        # yield anything.
        diff = """\
=== modified file 'a'
--- a	2009-01-17 02:28:16 +0000
+++ a	2009-01-17 02:28:21 +0000
@@ -3,7 +3,7 @@
 import unittest
 
 
-class TestFoo(unittest.TestCase):
+class TestBar(unittest.TestCase):
 
-    def test_foo(self):
+    def test_bar(self):
         pass
"""
        patch = self.parse_diff(diff)
        self.assertEqual([], list(get_comments_from_diff(patch)))
