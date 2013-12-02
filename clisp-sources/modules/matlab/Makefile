# Makefile for CLISP module set matlab
# <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>

CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
INCLUDES = ..
# MATLAB = /usr/local/matlab/extern/include
MATLAB = d:/MATLAB7/extern/include
CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = matlab.o link.sh Makefile matlab.lisp wrap.lisp test.tst
distribdir =

all : matlab.fas matlab.o wrap.fas

matlab.c matlab.fas : matlab.lisp
	$(CLISP) -c matlab.lisp

matlab.o : matlab.c $(INCLUDES)/clisp.h
	$(CC) $(CFLAGS) -I$(INCLUDES) -I$(MATLAB) -c matlab.c

%.fas: %.lisp
	$(CLISP) -c $<

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

