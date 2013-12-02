# Makefile for CLISP module set netica
# <http://norsys.com/netica_c_api.htm>

CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
INCLUDES = ..
NETICA = /usr/local/netica
CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = netica.o link.sh Makefile netica.lisp wrap.lisp
distribdir =

all : netica.o wrap.fas

netica.c netica.fas : netica.lisp
	$(CLISP) -c netica.lisp

netica.o : netica.c $(INCLUDES)/clisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -I$(NETICA) -c netica.c

%.fas: %.lisp
	$(CLISP) -c $<

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

