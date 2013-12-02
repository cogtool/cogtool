
; Examples to test the Oracle CLISP library This is VERY incomplete,
; but should at least tell you if you built it right.

; $Id: examples.lisp,v 1.2 2002/09/20 13:51:54 hin Exp $

; Replace the connect info with your user and server

(oracle:connect "myuser" "mypasswd" "myserver")

; Create a table and insert some data
(oracle:run-sql "CREATE TABLE foo (i number(7), j number(7))")
(oracle:insert-row "foo" '((i 10) (j 20)))
(oracle:insert-row "foo" '((i 15) (j 30)))
(oracle:insert-row "foo" '((i 99) (j 88)))

; SELECT the data
(oracle:run-sql "SELECT i, j FROM foo")
(oracle:do-rows (i j)
         (print "You should see two numeric values")
         (print i)
         (print j))

; Remove the table
(oracle:run-sql "DROP TABLE foo")
