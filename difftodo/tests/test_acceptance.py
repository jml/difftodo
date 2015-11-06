# Copyright (c) 2015 Jonathan M. Lange <jml@mumak.net>
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

"""Acceptance tests for difftodo.

Given raw diffs, assert that we generate the correct TODOs.
"""

from StringIO import StringIO

from testtools import TestCase

from difftodo.scripts import diff_todo, human_format, DEFAULT_TAGS


def get_output_for_diff(diff, tags=DEFAULT_TAGS):
    """Given a diff, return the expected output from difftodo.

    :param text diff: A text diff that we'd expect to receive as input.
    :return: difftodo output
    """
    diff_file = StringIO(diff)
    output = StringIO()
    # XXX: Something other than human_format might be better for testing.
    diff_todo(diff_file, output, human_format, tags)
    return output.getvalue()


# Diff that failed in https://github.com/jml/difftodo/issues/17
ISSUE_17_DIFF = ('''\
diff --git a/flocker/node/agents/blockdevice.py b/flocker/node/agents/blockdevice.py
index 540fcac..a97d000 100644
--- a/flocker/node/agents/blockdevice.py
+++ b/flocker/node/agents/blockdevice.py
@@ -1116,6 +1184,7 @@ class BlockDeviceDeployerLocalState(PClass):
         These are the only parts of the state that need to be sent to the
         control service.
         """
+        # XXX above untested
         return (self.node_state, self.nonmanifest_datasets)
''')


class TestDiffs(TestCase):

    def test_17(self):
        expected = ('''\
flocker/node/agents/blockdevice.py:1187:8:
# XXX above untested

Things to do: 1
''')
        self.assertEqual(expected, get_output_for_diff(ISSUE_17_DIFF))
