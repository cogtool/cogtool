# -*-Makefile-*- for the GNU C preprocessor

CFLAGS = -I. -Wall -O2 -DOUTPUT_LINE_COMMANDS
# -DOUTPUT_EXTENDED_LINE_COMMANDS is good for GCC but not for
# the lousy compilers we are working with here...
OS=$(shell uname);
ifneq (,$(filter CYGWIN%,$(OS)))
CC = gcc -mno-cygwin
EXE = cccp.exe
else
CC = cc
EXE = cccp
endif

# Choose which alloca() function to use.
# The builtin alloca():
ALLOCA =
# The portable alloca() from alloca.c:
# ALLOCA = alloca.o

$(EXE) : cccp.o cexp.o version.o obstack.o $(ALLOCA)
	$(CC) cccp.o cexp.o version.o obstack.o $(ALLOCA) -o $(EXE)

cccp.o : cccp.c config.h
	$(CC) $(CFLAGS) -c cccp.c

cexp.o : cexp.c config.h
	$(CC) $(CFLAGS) -c cexp.c

version.o : version.c
	$(CC) $(CFLAGS) -c version.c

obstack.o : obstack.c obstack.h
	$(CC) $(CFLAGS) -c obstack.c

alloca.o : alloca.c
	$(CC) $(CFLAGS) -c alloca.c

clean:
	rm -f *.o $(EXE)
