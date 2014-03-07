#!/usr/bin/env python
# encoding: utf-8

import sys
import os
import string

home = os.path.expanduser('~')

if os.path.exists(os.path.join(home, '.emacs/init.el')):
    print 'init.el already exists'
elif os.path.exists(os.path.join(home, '.emacs.d')):
    print '.emacs.d already exists'
else:
    print 'Linking .emacs.d'
    os.symlink(os.path.join(os.getcwd(), '.emacs.d'), os.path.join(home, '.emacs.d'))
    print 'Installing cask...'
    os.system('curl -fsSkL https://raw.github.com/cask/cask/master/go | python')
    os.chdir(os.path.join(os.getcwd(), '.emacs.d'))
    print 'Installing packages from cask file...'
    os.system(os.path.join(home, '.cask/bin/cask') + ' install')
    print 'Emacs ready for use!'

