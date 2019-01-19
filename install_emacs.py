#!/usr/bin/env python
# encoding: utf-8

import sys
import os
import string

home = os.path.expanduser('~')

if os.path.exists(os.path.join(home, '.emacs/init.el')):
    print('init.el already exists. Aborting')
elif os.path.exists(os.path.join(home, '.emacs.d')):
    print('.emacs.d already exists. Aborting')
else:
    print('Linking .emacs.d')
    os.symlink(os.path.join(os.getcwd(), '.emacs.d'),
               os.path.join(home, '.emacs.d'))
    print('Installing cask...')
    # should be brew install cask --ignore-dependencies if using emacs-plus
    os.system('brew install cask')
    os.chdir(os.path.join(os.getcwd(), '.emacs.d'))
    print('Installing packages from cask file...'
    os.system('cask install')
    print('If problems with availability, look in to issues with TLS')
    print('Try:')
    print('$> emacs -q')
    print('```')
    print('(require \'package)')
    print('(package-initialize)')
    print('(add-to-list \'package-archives ' +
          '(cons "gnu" "https://elpa.gnu.org/packages/"))')
    print('(add-to-list \'package-archives ' +
          '(cons "melpa" "https://melpa.org/packages/"))')
    print('(package-refresh-contents)')
    print('```')
    print('Emacs ready for use!')

