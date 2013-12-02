# Makefile for the sample CLISP module QUEENS

# Programs used by "make":
CC = gcc -O
CPPFLAGS =
CFLAGS = -Wall -fomit-frame-pointer -O2
INCLUDES=..
CPP = gcc -O -E -w
CLFLAGS =

MODPREP = ../modprep.fas

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = *
distribdir =

all : queens.o callqueens.o

queens.o : queens.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c queens.c

callqueens.m.c : callqueens.c $(MODPREP)
	$(CLISP) $(MODPREP) callqueens.c

callqueens.o : callqueens.m.c $(INCLUDES)/clisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c callqueens.m.c -o callqueens.o

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

