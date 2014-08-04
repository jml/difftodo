#!/usr/bin/env python
# Copyright (c) 2014 Jonathan M. Lange <jml@mumak.net>
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


from setuptools import setup, find_packages
import os.path

description = file(os.path.join(os.path.dirname(__file__), 'README.md'), 'rb').read()


setup(
    name="difftodo",
    version="0.0.2",
    description="Create todo list based on code diffs.",
    long_description=description,
    author="Jonathan M. Lange",
    author_email="jml@mumak.net",
    install_requires=[
        "bzr",
        ],
    zip_safe=True,
    packages=find_packages('.'),
    classifiers = [
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'License :: OSI Approved :: Apache Software License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        ],
    entry_points = {
        'console_scripts': [
            'diffcomments = difftodo.scripts:comments_from_diff',
            'difftodos = difftodo.scripts:todos_from_diff',
        ]
    }
)
