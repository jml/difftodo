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

"""Sample data to be used in tests."""


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
+        # XXX: above untested
         return (self.node_state, self.nonmanifest_datasets)
''')
