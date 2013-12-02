# Makefile for CLISP module set win32

CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
INCLUDES= ../..

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = link.sh Makefile win32.lisp # win32.o
distribdir =

# add win32.o if a C file is generated
all : win32.fas # win32.o

win32.c win32.fas : win32.lisp
	$(CLISP) -c win32.lisp

win32.o : win32.c $(INCLUDES)/clisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c win32.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

