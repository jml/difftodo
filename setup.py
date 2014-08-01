#!/usr/bin/env python

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
)
