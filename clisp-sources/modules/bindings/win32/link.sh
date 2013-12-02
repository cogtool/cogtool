file_list=''
mod_list=''
if test -f win32.c; then
  file_list="$file_list"' win32.o'
  mod_list="$mod_list"' win32'
fi
make clisp-module CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
NEW_LIBS="$file_list -lm"
NEW_MODULES="$mod_list"
TO_LOAD='win32'
