#!/bin/sh
#
# Regenerate configuration files
# libtoolize --force --copy
aclocal
automake -afc
# autoheader
autoconf

# Run configure for this platform
#./configure $*
echo "Now you are ready to run ./configure"
