;; -*- Lisp -*-

#+LISPWORKS
(progn
  (defun gc () (mark-and-sweep 3))
  (defun hash-table-weak-p (ht) (system::hash-table-weak-kind ht))
  t)
#+LISPWORKS
T

(hash-table-weak-p
  (progn
    (setq tab (make-hash-table #+LISPWORKS :weak-kind #-LISPWORKS :weak :key
                               :test 'equal
                               #+CLISP :initial-contents #+CLISP '((1 . 2) ("foo" . "bar"))))
    #-CLISP (setf (gethash 1 tab) 2)
    #-CLISP (setf (gethash "foo" tab) "bar")
    tab))
:key

(gethash 1 tab)
2

(gethash "foo" tab)
"bar"

(gethash "zot" tab)
nil

(gethash "bar" tab)
nil

(progn (gc) t)
t

(gethash 1 tab)
2

(gethash "foo" tab)
nil

(gethash "zot" tab)
nil

(gethash "bar" tab)
nil

#+LISPWORKS (set-hash-table-weak tab nil)
#-LISPWORKS (setf (hash-table-weak-p tab) nil)
nil

(gethash 1 tab)
2

(gethash "foo" tab)
nil

(setf (gethash "foo" tab) "bar")
"bar"

(gethash "foo" tab)
"bar"

(progn (gc) t)
t

(gethash "foo" tab)
"bar"

#+LISPWORKS (set-hash-table-weak tab :key)
#-LISPWORKS (setf (hash-table-weak-p tab) :key)
:key

(progn (gc) t)
t

(gethash "foo" tab)
nil

#+LISPWORKS (set-hash-table-weak tab :value)
#-LISPWORKS (setf (hash-table-weak-p tab) :value)
:value
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) 1
(gethash 1 tab) nil
(gethash "zoo" tab) nil

#+LISPWORKS (set-hash-table-weak tab :both) #+LISPWORKS :both
#-LISPWORKS (setf (hash-table-weak-p tab) :key-and-value) #-LISPWORKS :key-and-value
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) nil
(gethash 1 tab) nil
(gethash "zoo" tab) nil

#+LISPWORKS (set-hash-table-weak tab :either) #+LISPWORKS :either
#-LISPWORKS (setf (hash-table-weak-p tab) :key-or-value) #-LISPWORKS :key-or-value
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) 1
(gethash 1 tab) "bar"
(gethash "zoo" tab) nil

(let ((htv (make-hash-table :test 'eql
                            #+LISPWORKS :weak-kind #-LISPWORKS :weak :value))
      (htk (make-hash-table :test 'eql
                            #+LISPWORKS :weak-kind #-LISPWORKS :weak :key))
      (li nil))
  (loop :for i :from 0 :to 1000
    :for string = (format nil "~r" i)
    :do (push string li)
    (setf (gethash i htv) string
          (gethash string htk) i))
  (list (length li)
        (cons (hash-table-count htv) (hash-table-count htk))
        (progn (gc) (cons (hash-table-count htv) (hash-table-count htk)))
        (progn (setq li nil) (gc)
               (cons (hash-table-count htv) (hash-table-count htk)))))
(1001 (1001 . 1001) (1001 . 1001) (0 . 0))

; This was a bug that - strangely - led to crashes _only_ in the
; SPVW_PAGES LINUX_NOEXEC_HEAPCODES NO_GENERATIONAL_GC configuration.
#+CLISP
(flet ((ht_kvtable (ht)
         (if (integerp (sys::%record-ref ht 1)) ; GENERATIONAL_GC build?
           (sys::%record-ref ht 2)
           (sys::%record-ref ht 1)))
       (whal_itable (kvt) (sys::%record-ref kvt 1)))
  (let* ((ht (make-hash-table :test 'ext::stablehash-eq :weak :key))
         (kvt (ht_kvtable ht)))
    (assert (simple-vector-p (whal_itable kvt)))
    (gc) ; first GC removed kvt from the all_weakpointers list
    (gc) ; second GC dropped the itable
    (and (eq (ht_kvtable ht) kvt)
         (simple-vector-p (whal_itable kvt)))))
#+CLISP
T
