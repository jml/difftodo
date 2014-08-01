# Copyright (c) 2009 Jonathan M. Lange <jml@mumak.net>, except where otherwise
# noted.
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

__all__ = [
    'merge_function_metadata',
    'singleton',
    ]

import inspect
from itertools import ifilter
import new


# Copied from Twisted. Copyright Twisted Matrix Labs.
def merge_function_metadata(f, g):
    """
    Overwrite C{g}'s name and docstring with values from C{f}.  Update
    C{g}'s instance dictionary with C{f}'s.

    To use this function safely you must use the return value. In Python 2.3,
    L{mergeFunctionMetadata} will create a new function. In later versions of
    Python, C{g} will be mutated and returned.

    @return: A function that has C{g}'s behavior and metadata merged from
        C{f}.
    """
    try:
        g.__name__ = f.__name__
    except TypeError:
        try:
            merged = new.function(
                g.func_code, g.func_globals,
                f.__name__, inspect.getargspec(g)[-1],
                g.func_closure)
        except TypeError:
            pass
    else:
        merged = g
    try:
        merged.__doc__ = f.__doc__
    except (TypeError, AttributeError):
        pass
    try:
        merged.__dict__.update(g.__dict__)
        merged.__dict__.update(f.__dict__)
    except (TypeError, AttributeError):
        pass
    merged.__module__ = f.__module__
    return merged


def filter_none(generator):
    """Decorate 'generator' so that all None's are filtered out."""

    def decorated(*args, **kwargs):
        i = generator(*args, **kwargs)
        return ifilter(None, i)

    return merge_function_metadata(generator, decorated)
