# Makefile for the character set conversion utilities
# Bruno Haible 8.7.1994


# Directories:
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin


# Programs:
CC = cc
CFLAGS = -O
#CC = gcc
#CFLAGS = -O2 -fomit-frame-pointer

# Targets:

unix : $(bindir)/touchr

dos : $(bindir)/crlf-to-nl $(bindir)/nl-to-crlf $(bindir)/touchr


# CR/LF <--> NL conversion:

$(bindir)/crlf-to-nl : crlf-to-nl.c
	$(CC) $(CFLAGS) crlf-to-nl.c -o $(bindir)/crlf-to-nl

$(bindir)/nl-to-crlf : nl-to-crlf.c
	$(CC) $(CFLAGS) nl-to-crlf.c -o $(bindir)/nl-to-crlf

# functionally the same as crlf-to-nl, except that it converts "in place".
$(bindir)/fromdos : fromdos.c
	$(CC) $(CFLAGS) fromdos.c -o $(bindir)/fromdos


# Preserving the date:

$(bindir)/touchr : touchr.c
	$(CC) $(CFLAGS) touchr.c -o $(bindir)/touchr || (echo '#!/bin/sh' > $(bindir)/touchr ; chmod a+x $(bindir)/touchr)


clean : force
	rm -f *.s *.o core
	rm -f crlf-to-nl nl-to-crlf fromdos
	rm -f touchr

force :

