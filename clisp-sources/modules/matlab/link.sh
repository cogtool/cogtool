file_list=''
mod_list=''
if test -f matlab.c; then
  file_list="$file_list"' matlab.o'
  mod_list="$mod_list"' matlab'
fi
# matlab=/usr/local/matlab/extern/lib/
matlab=d:/MATLAB7/extern/lib/win32/microsoft/msvc60

make clisp-module \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
  INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
NEW_LIBS="$file_list";
for lib in eng mat mex mx; do
  NEW_LIBS=${NEW_LIBS}" ${matlab}/lib${lib}.lib"
done
NEW_MODULES="$mod_list"
TO_LOAD='matlab wrap'
CLFLAGS="${CLFLAGS}" # -L${matlab}
PATH="${PATH}:/cygdrive/d/MATLAB7/bin/win32/"
