#!/usr/bin/env python
# encoding: utf-8

import sys
import os

home = os.path.expanduser('~')

if os.path.exists(os.path.join(home, '.emacs/init.el')):
    print 'init.el already exists'
elif os.path.exists(os.path.join(home, '.emacs.d')):
    print '.emacs.d already exists'
else:
    os.symlink(os.path.join(os.getcwd(), '.emacs.d'), os.path.join(home, '.emacs.d'))
    os.system('curl -fsSkL https://raw.github.com/cask/cask/master/go | python')
    print 'Now export the path as instructed, then enter .emacs.d and `cask install`'

