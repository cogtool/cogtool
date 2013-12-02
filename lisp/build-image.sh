#!/bin/bash

# This script builds the CLisp memory image for CogTool. 

# On Mac OS X this should be run in Leopard (10.5) or later, as in earlier versions 
# of OS X $MACHTYPE was not being set correctly.

case $MACHTYPE in
    i?86-apple*)
	    platform=mac-intel ;;
    x86_64-apple*)
	    platform=mac-intel ;;
    i?86-pc*)
	    platform=win ;;
    *)
	    echo "Unknown platform" ; exit 1 ;;
esac

case $platform in
    win)
	    exefile=lisp.exe ;;
    *)
	    exefile=lisp.run ;;
esac

# We do the compile and load twice, to ensure that the second time, which is the one from 
# which we save the memory image, contains no compilation spoor.
cmd0="./clisp/$platform/base/$exefile -M clisp/$platform/base/lispinit.mem -i actr6.lisp"
cmd="$cmd0 -x '(ext:quit)'"
echo $cmd 
eval $cmd
cmd="$cmd0 -x '(ext:saveinitmem \"../clisp-$platform/actr6.mem\")'"
echo $cmd 
eval $cmd

cp -pv ./clisp/$platform/base/$exefile ../clisp-$platform/$exefile


