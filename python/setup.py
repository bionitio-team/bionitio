#!/usr/bin/env python

from distutils.core import setup

setup(
    name='biotool-py',
    version='0.1.0.0',
    author='Bernie Pope',
    author_email='bjpope@unimelb.edu.au',
    packages=['biotool'],
    package_dir={'biotool': 'biotool'},
    entry_points={
        'console_scripts': ['biotool-py = biotool.biotool:main']
    },
    url='https://github.com/bjpop/biotool',
    license='LICENSE',
    description=('XXX FIXME'),
    long_description=('FIXME'),
    install_requires=["biopython"],
)
