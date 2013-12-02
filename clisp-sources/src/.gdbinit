# CLISP .gdbinit
set language c

define boot
  file lisp.run
  set args -B . -N locale -E 1:1 -q -norc -M lispinit.mem
end
document boot
         debug the boot linking set
end

define base
  file base/lisp.run
  set args -B . -N locale -E 1:1 -q -norc -M base/lispinit.mem
end
document base
         debug the base linking set
end

define full
  file full/lisp.run
  set args -B . -N locale -E 1:1 -q -norc -M full/lispinit.mem
  # -i ../tests/tests -x '(run-test "***/test")'
  # -i clx/new-clx/demos/clx-demos.lisp -x '(clx-demos:qix)' -x '(clx-demos:koch)' -x '(clx-demos:sokoban)'
  break my_type_error
  break closed_display_error
  break error_bdb
  break pcre_error
end
document full
         debug the full linking set
end

define interpreted
  file lisp.run
  set args -B . -N locale -E 1:1 -q -norc -M interpreted.mem
end
document interpreted
         debug the boot linking set with the interpreted memory image
end

define zout
  output object_out($arg0)
  echo \n
end
document zout
         print the specified object with PRIN1
end

define xout
  output nobject_out(0,$arg0)
  echo \n
end
document xout
         print the specified object with nobject_out()
end

define run_test
  run -B . -N locale -E 1:1 -q -norc -M lispinit.mem -i tests/tests -x "(run-test \"tests/$arg0\")"
end
document run_test
         run the specified test in the test suite
end

define run_all_tests
  run -B . -N locale -E 1:1 -q -norc -M lispinit.mem -i tests/tests -x "(cd \"tests/\") (run-all-tests)"
end
document run_all_tests
         run the whole test suite
end

define ansi_tests
  run -B . -N locale -E 1:1 -q -norc -M lispinit.mem -ansi -x "(cd \"ansi-tests/\") (load \"clispload.lsp\") (in-package \"CL-TEST\") (time (regression-test:do-tests))"
end
document ansi_tests
         run the gcl/ansi-test suite
end

define ansi_tests_compiled
  run -B . -N locale -E 1:1 -q -norc -M lispinit.mem -ansi -x "(cd \"ansi-tests/\") (load \"clispload.lsp\") (in-package \"CL-TEST\") (setq regression-test::*compile-tests* t) (time (regression-test:do-tests))"
end
document ansi_tests
         run the gcl/ansi-test suite - compiled
end

define stack
  set $idx = $arg1
  while $idx >= $arg0
    echo ***** STACK_
    output $idx
    echo \ *****\n
    output object_out(STACK[-1-$idx])
    echo \n
    set $idx = $idx-1
  end
end
document stack
         print the section of STACK
end

define zbacktrace
  p back_trace_out(0,0)
end
document zbacktrace
         print the CLISP backtrace
end

break funcall
commands
        xout fun
end

break apply
commands
        xout fun
end

break eval
commands
        xout form
end

break interpret_bytecode_
commands
        xout closure
end

break gar_col

watch back_trace
commands
        zbacktrace
        continue
end

# disable all the above breaks
disable 1 2 3 4 5 6

break fehler_notreached
break SP_ueber
break STACK_ueber

break fehler
break prepare_error
break OS_error
break OS_file_error
break OS_filestream_error
break errno_out_low

info break

#ifdef GENERATIONAL_GC
# This command fails in a build without GENERATIONAL_GC; this is normal.
break sigsegv_handler_failed
# You need to execute these two commands by hand, but *only* in a build
# with GENERATIONAL_GC. Also you need to undo them when the breakpoint
# at sigsegv_handler_failed has been triggered.
#handle SIGSEGV noprint nostop
#handle SIGBUS noprint nostop
#endif

# cut and paste when you stop in interpret_bytecode_()
watch byteptr
commands
        output byteptr-byteptr_in
        echo \n
end
