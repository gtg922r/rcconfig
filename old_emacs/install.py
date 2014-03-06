#!/usr/bin/env python
# encoding: utf-8

import sys
import os

home = os.path.expanduser('~')

if os.path.exists(os.path.join(home, '.emacs')):
    print '.emacs already exists'
else:
    os.symlink(os.path.join(os.getcwd(), 'init.el'), os.path.join(home, '.emacs.d/init.el'))
    os.symlink(os.getcwd(), os.path.join(home, '.emacs.d'))
