file_list=''
mod_list=''
if test -f netica.c; then
  file_list="$file_list"' netica.o'
  mod_list="$mod_list"' netica'
fi
netica=/usr/local/netica/
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
NEW_LIBS="$file_list ${netica}netica.lib"
NEW_MODULES="$mod_list"
TO_LOAD='netica wrap'
CLFLAGS="${CLFLAGS} -L${netica}"
