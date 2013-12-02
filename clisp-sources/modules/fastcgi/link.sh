# link.sh for FastCGI library
# $Id: link.sh,v 1.2 2004/02/25 16:22:58 haible Exp $
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="fastcgi.o fastcgi_wrappers.o"
NEW_LIBS="fastcgi.o fastcgi_wrappers.o -lfcgi"
NEW_MODULES="fastcgi"
TO_LOAD="fastcgi"
