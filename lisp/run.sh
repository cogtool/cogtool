#! /bin/bash

# Runs CLisp standalone for testing. Not part of the product, just a maintenance aid.

case $MACHTYPE in
    *86*apple*)
        platform=mac-intel ;;
    i?86-pc*)
        platform=win ;;
    *)
        echo "Unknown platform" ; exit 1 ;;
esac


if [[ $# -lt 1 ]] ; then
   memimage="../clisp-$platform/actr6.mem" ;
else
   memimage="clisp/$platform/base/lispinit.mem" ;
fi

case $platform in
    win)
	    exefile=lisp.exe ;;
    *)
	    exefile=lisp.run ;;
esac

../clisp-$platform/$exefile -M $memimage

