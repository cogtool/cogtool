#!/bin/bash

cp -p $1/actr6/core-modules/*.lisp actr6/core-modules/
cp -p $1/actr6/framework/*.lisp actr6/framework/
cp -p $1/actr6/other-files/*.lisp actr6/other-files/
cp -p $1/actr6/tools/*.lisp actr6/tools/
cp -p $1/actr6/devices/virtual/*.lisp actr6/devices/virtual/
cp -p $1/actr6/commands/*.lisp actr6/commands/
cp -p $1/actr6/extras/emma/*.lisp actr6/extras/emma/
cp -p $1/actr6/modules/*.lisp actr6/modules/
cp -p $1/actr6/support/*.lisp actr6/support/

rm actr6/tools/environment-loader.lisp

