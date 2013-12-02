;; -*- Lisp -*-
;; Test :warn-if-needs-rehash-after-gc.

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((x1 (make-instance 'ext:standard-stablehash))
          (x2 (make-instance 'ext:standard-stablehash))
          (ht (make-hash-table :test 'ext:stablehash-eq)))
      (setf (gethash x1 ht) 11)
      (setf (gethash x2 ht) 22)
      (setf (gethash '1000 ht) 11999)
      (gc)
      (gethash x1 ht)
      (setf (gethash '10000000000000000000 ht) 11999999999999)
      (gc)
      (gethash x1 ht))))
11

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((x1 (make-instance 'ext:standard-stablehash))
          (x2 (make-instance 'ext:standard-stablehash))
          (ht (make-hash-table :test 'ext:stablehash-eq
                               :warn-if-needs-rehash-after-gc t)))
      (setf (gethash x1 ht) 11)
      (setf (gethash x2 ht) 22)
      (setf (gethash '1000 ht) 11999)
      (gc)
      (gethash x1 ht)
      (setf (gethash '10000000000000000000 ht) 11999999999999))))
WARNING

;; Test *warn-on-hashtable-needing-rehash-after-gc*.

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((custom:*warn-on-hashtable-needing-rehash-after-gc* t))
      (let ((x1 (make-instance 'ext:standard-stablehash))
            (x2 (make-instance 'ext:standard-stablehash))
            (ht (make-hash-table :test 'ext:stablehash-eq)))
        (setf (gethash x1 ht) 11)
        (setf (gethash x2 ht) 22)
        (setf (gethash '1000 ht) 11999)
        (gc)
        (gethash x1 ht)
        (setf (gethash '10000000000000000000 ht) 11999999999999)
        (gc)
        (gethash x1 ht)))))
WARNING

;; read/write consistency
(let ((ht (make-hash-table :test 'eq)))
  (setf (gethash ht ht) ht)
  (setq ht (read-from-string (with-standard-io-syntax (write-to-string ht))))
  (eq (gethash ht ht) ht))
T

(let ((ht (make-hash-table :test 'eq)) x)
  (defstruct ht-test-struct a b c)
  (setq x (make-ht-test-struct :a 1 :b 2 :c ht))
  (setf (gethash ht ht) ht
        (gethash x ht) 12)
  (let ((l (read-from-string (with-standard-io-syntax
                               (write-to-string (list x ht))))))
    (setq ht (second l) x (first l)))
  (list (eq (gethash ht ht) ht)
        (gethash x ht)))
(T 12)

(let ((ht (make-hash-table :test 'ext:fasthash-eq)))
  (defstruct ht-test-struct a b c)
  (setq x (make-ht-test-struct :a 1 :b 2 :c ht))
  (setf (gethash ht ht) ht
        (gethash x ht) 12)
  (setq x (read-from-string (with-standard-io-syntax (write-to-string x)))
        ht (ht-test-struct-c x))
  (setf (ht-test-struct-a x) (ext:! 123)
        (gethash (ext:! 20) ht) (ext:! 21)
        (gethash (ext:! 21) ht) (ext:! 22)
        (gethash (ext:! 22) ht) (ext:! 23))
  (ext:gc)
  (setf (ht-test-struct-b x) (ext:! 124)
        (gethash (ext:! 30) ht) (ext:! 61)
        (gethash (ext:! 41) ht) (ext:! 72)
        (gethash (ext:! 52) ht) (ext:! 83))
  (ext:gc)
  (list (eq (gethash ht ht) ht)
        (gethash x ht)))
(T 12)

;; clean-up
(setf (find-class 'ht-test-struct) nil) nil
