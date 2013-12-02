@echo off

echo +----------------------------------------------------------+
echo I this will install CLISP on your system,                  I
echo I associating file types FAS, MEM and LISP with CLISP.     I
echo I it will also create a shortcut to CLISP on your desktop. I
echo I               press C-c to abort                         I
echo +----------------------------------------------------------+
pause

if exist src\install.lisp goto installsrc
if exist install.lisp goto install
goto notfound
:installsrc
clisp.exe -K full -norc -C src\install.lisp
goto exit
:install
clisp.exe -K full -norc -C install.lisp
goto exit
:notfound
echo Sorry, install.lisp not found, cannot install
:exit
pause
