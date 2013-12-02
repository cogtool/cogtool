#!/bin/sh
# Usage: utils/fix-perms.sh
# fixes the permissions of the files in the build tree
#
# To verify:   find . -type f -perm +111 -print

find . -type f -follow -perm +111 '(' \
     -name '*.in' \
  -o -name '*.xml' \
  -o -name '*.html' \
  -o -name '*.png' \
  -o -name '*.xsl' \
  -o -name '*.m4' \
  -o -name '*.h' \
  -o -name '*.c' \
  -o -name '*.lisp' \
  -o -name '*.tst' \
  -o -name '*.bat' \
  -o -name '.cvsignore' \
  -o -name 'Makefile' \
  -o -name 'README' \
  ')' \
  -exec chmod a-x '{}' ';'

find . -name configure -exec chmod +x '{}' ';'
