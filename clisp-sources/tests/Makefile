# Makefile for CLISP regression tests
# ifneq/else/endif requires GNU Make, so we cannot use them

# build directory (with [back]slash appended)
BD=../
# executable extension (.exe on win32, .run everywhere else)
LEXE=.run

LISP=$(BD)lisp$(LEXE) -E utf-8 -norc -B $(BD) -N $(BD)locale -M $(BD)lispinit.mem -m 30000KW
# LISP=../clisp -norc
# LISP=clisp -norc

RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests.fas: tests.lisp
	$(LISP) -c tests

tests : force tests.fas
	$(LISP) -i tests -x "(time (run-all-tests))"

complete : force tests.fas
	$(LISP) -i tests -x "(time (run-all-tests nil))"

compare : force
	(echo *.erg | grep '*' >/dev/null) || (echo "Test failed:" ; ls -l *erg; echo "To see which tests failed, type" ; echo "    cat "`pwd`"/*.erg" ; exit 1)
	echo "Test passed."

%.erg : %.tst
	$(LISP) -i tests -x '(run-test "$<")'

force :

