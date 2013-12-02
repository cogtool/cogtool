;; -*- Lisp -*-
;; tests for PostGreSQL
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "postgresql/test")'

;;; Based on the examples distributed with PostgreSQL (man libpq)

;;; if you get "FATAL: database \"postgres\" does not exist":
;;; $ createdb -U postgres postgres

(sql:with-sql-connection (conn :name "template1" :log *standard-output*)
  (sql:sql-transaction conn "BEGIN" sql:PGRES_COMMAND_OK)
  ;; fetch instances from the pg_database, the system catalog of databases
  (sql:sql-transaction
   conn "DECLARE mycursor CURSOR FOR select * from pg_database"
   sql:PGRES_COMMAND_OK)
  ;; FETCH ALL
  (sql:with-sql-transaction
      (res conn "FETCH ALL in mycursor" sql:PGRES_TUPLES_OK)
    (let* ((nfields (sql:PQnfields res)) (ntuples (sql:PQntuples res))
           (names (make-array nfields)))
      (format t " + ~D field~:P; ~D ntuple~:P~%" nfields ntuples)
      ;; first, print out the attribute names
      (dotimes (ii nfields)
        (format t "~3:D: ~S~%" ii
                (setf (aref names ii) (sql:PQfname res ii))))
      ;; next, print out the instances
      (dotimes (ii ntuples)
        (format t "~%<<~D>>~%" ii)
        (dotimes (jj nfields (terpri))
          (format t "~3:D ~15@S = ~S~%"
                  jj (aref names jj) (sql:PQgetvalue res ii jj))))))
  ;; close the cursor
  (sql:sql-transaction conn "CLOSE mycursor" sql:PGRES_COMMAND_OK)
  ;; commit the transaction
  (sql:sql-transaction conn "COMMIT" sql:PGRES_COMMAND_OK)
  NIL)
NIL

;;;
;;; asynchronous notification interface
;;;
;;; populate a database with the following:
;;; CREATE TABLE TBL1 (i int4);
;;; CREATE TABLE TBL2 (i int4);
;;; CREATE RULE r1 AS ON INSERT TO TBL1 DO INSERT INTO TBL2 values (new.i); NOTIFY TBL2;
;;;
;;;  Then start up this program
;;;  After the program has begun, do
;;; INSERT INTO TBL1 values (10);

#+(or)
(sql:with-sql-connection (conn :log *standard-output*)
  (sql:sql-transaction conn "LISTEN TBL2" sql:PGRES_COMMAND_OK)
  (loop (sql:PQconsumeInput conn)
    (loop :for notify = (sql:PQnotifies conn)
      :while notify :do (format t "ASYNC NOTIFY: ~a~%" notify)
      (break))
    (sleep 1)))

;;;
;;; test the binary cursor interface
;;;
;;; *** this is not supported by CLISP at the moment:
;;; *** need to include geo_decls.h
;;;
;;; populate a database by doing the following:
;;;
;;;       CREATE TABLE test1 (i int4, d float4, p polygon);
;;;
;;;       INSERT INTO test1 values (1, 3.567, '(3.0, 4.0, 1.0, 2.0)'::polygon);
;;;
;;;       INSERT INTO test1 values (2, 89.05, '(4.0, 3.0, 2.0, 1.0)'::polygon);
;;;
;;;        the expected output is:
;;;
;;;       tuple 0: got
;;;        i = (4 bytes) 1,
;;;        d = (4 bytes) 3.567000,
;;;        p = (4 bytes) 2 points         boundbox = (hi=3.000000/4.000000, lo = 1.000000,2.000000)
;;;       tuple 1: got
;;;        i = (4 bytes) 2,
;;;        d = (4 bytes) 89.050003,
;;;        p = (4 bytes) 2 points         boundbox = (hi=4.000000/3.000000, lo = 2.000000,1.000000)
;;;

#+(or)
(sql:with-sql-connection (conn :log *standard-output*)
  (sql:sql-transaction conn "BEGIN" sql:PGRES_COMMAND_OK)
  (sql:sql-transaction
   conn "DECLARE mycursor BINARY CURSOR FOR select * from test1"
   sql:PGRES_COMMAND_OK)
  (sql:with-sql-transaction
      (res conn "FETCH ALL in mycursor" sql:PGRES_TUPLES_OK)
    (let ((i-fnum (sql:PQfnumber res "i"))
          (d-fnum (sql:PQfnumber res "d"))
          (p-fnum (sql:PQfnumber res "p"))
          (nfields (sql:PQnfields res))
          (ntuples (sql:PQntuples res)))
      (format t " + ~d fields; ~d ntuples; i: ~d; d: ~d; p: ~d~%"
              nfields ntuples i-fnum d-fnum p-fnum)
      (dotimes (ii 3)
        (format t "type[~d] = ~d, size[~d] = ~d~%"
                ii (sql:PQftype res ii) ii (sql:PQfsize res ii)))
      (dotimes (ii ntuples)
        (let ((plen (sql:PQgetlength res ii p-fnum))
              (ival (sql:PQgetvalue res ii i-fnum))
              (dval (sql:PQgetvalue res ii d-fnum)))
          (format t " ++ plen: ~d; ival: ~d; dval: ~f~%" plen ival dval)))))
  (sql:sql-transaction conn "CLOSE mycursor" sql:PGRES_COMMAND_OK)
  (sql:sql-transaction conn "COMMIT" sql:PGRES_COMMAND_OK))
