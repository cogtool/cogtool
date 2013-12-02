;; -*- Lisp -*-

#+LISPWORKS
(progn
  (defun gc () (mark-and-sweep 3))
  t)
#+LISPWORKS
T

(defun weak-ht-fill-initially (tab)
  (setf (gethash (copy-seq "foo") tab) 1)
  (setf (gethash 1 tab) (copy-seq "bar"))
  (setf (gethash (copy-seq "zoo") tab) (copy-seq "zot")))
weak-ht-fill-initially


;; Test that weak hash-tables of kind :KEY work.

(let ((tab (make-hash-table :test #+OpenMCL 'eq #-OpenMCL 'equal
                            #+ALLEGRO :weak-keys #+ALLEGRO t
                            #+CMU19 :weak-p #+CMU19 t
                            #+LISPWORKS :weak-kind #-(or ALLEGRO CMU19 LISPWORKS) :weak
                              #-(or ALLEGRO CMU19) :key)))
  (weak-ht-fill-initially tab)
  (gc)
  (list (gethash "foo" tab) (gethash 1 tab) (gethash "zoo" tab)))
(NIL "bar" NIL)


;; Test that weak hash-tables of kind :KEY don't suffer from the
;; "key in value" problem.

(let ((tab (make-hash-table :test 'eq
                            #+ALLEGRO :weak-keys #+ALLEGRO t
                            #+CMU19 :weak-p #+CMU19 t
                            #+LISPWORKS :weak-kind #-(or ALLEGRO CMU19 LISPWORKS) :weak
                              #-(or ALLEGRO CMU19) :key)))
  (let ((a (list 'x)))
    (let ((b (list 'y)))
      (setf (gethash a tab) 'xxx)
      (setf (gethash b tab) (cons 'yyy b)))
    (gc)
    (list (hash-table-count tab)
          (gethash a tab)
          (let ((l nil)) (maphash #'(lambda (k v) (push k l)) tab) l))))
(1 XXX ((X)))


;; Perform all the WEAK-MAPPING tests, emulating WEAK-MAPPING with
;; weak hash-tables.

(progn
  (defun make-freak-mapping (a b)
    (let ((tab (make-hash-table :test 'eq
                                #+ALLEGRO :weak-keys #+ALLEGRO t
                                #+CMU19 :weak-p #+CMU19 t
                                #+LISPWORKS :weak-kind #-(or ALLEGRO CMU19 LISPWORKS) :weak
                                  #-(or ALLEGRO CMU19) :key)))
      (setf (gethash a tab) b)
      tab))
  (defun freak-mapping-pair (tab)
    (let (a b c)
      (maphash #'(lambda (k v) (setq a k b v c t)) tab)
      (values a b c)))
  (defun freak-mapping-value (tab)
    (block nil
      (maphash #'(lambda (k v) (return-from nil v)) tab)
      nil))
  (defun (setf freak-mapping-value) (new-value tab)
    (block nil
      (maphash #'(lambda (k v) (setf (gethash k tab) new-value) (return-from nil))
               tab))
    new-value)
  t)
T

(let ((a (list 'x))
      (b (list 'y)))
  (let ((w (make-freak-mapping a b)))
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
(((x) (y) t) ((y)))

(let ((a (list 'x))
      (b (list 'y)))
  (let ((w (make-freak-mapping a b)))
    (setq b nil)
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
(((x) (y) t) ((y)))

(let ((a (list 'x))
      (b (list 'y)))
  (let ((w (make-freak-mapping a b)))
    (setq a nil)
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
((nil nil nil) (nil))

(let ((a (list 'x))
      (b (list 'y)))
  (let ((w (make-freak-mapping a b)))
    (setq a nil b nil)
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
((nil nil nil) (nil))

(let ((a1 (list 'x1))
      (a2 (list 'x2))
      (a3 (list 'x3))
      (a4 (list 'x4))
      (a5 (list 'x5)))
  (let ((w1 (make-freak-mapping a3 a4))
        (w2 (make-freak-mapping a1 a2))
        (w3 (make-freak-mapping a4 a5))
        (w4 (make-freak-mapping a2 a3)))
    (setq a2 nil a3 nil a4 nil a5 nil)
    (gc)
    (list (freak-mapping-value w2)
          (freak-mapping-value w4)
          (freak-mapping-value w1)
          (freak-mapping-value w3))))
((x2) (x3) (x4) (x5))

(let ((a1 (list 'x1))
      (a2 (list 'x2))
      (a3 (list 'x3))
      (a4 (list 'x4))
      (a5 (list 'x5)))
  (let ((w1 (make-freak-mapping a3 a4))
        (w2 (make-freak-mapping a1 a2))
        (w3 (make-freak-mapping a4 a5))
        (w4 (make-freak-mapping a2 a3)))
    (setq a1 nil a3 nil a4 nil a5 nil)
    (gc)
    (list (freak-mapping-value w2)
          (freak-mapping-value w4)
          (freak-mapping-value w1)
          (freak-mapping-value w3))))
(nil (x3) (x4) (x5))

(let ((a1 (list 'x1))
      (a2 (list 'x2))
      (a3 (list 'x3))
      (a4 (list 'x4))
      (a5 (list 'x5)))
  (let ((w1 (make-freak-mapping a3 a4))
        (w2 (make-freak-mapping a1 a2))
        (w3 (make-freak-mapping a4 a5))
        (w4 (make-freak-mapping a2 a3)))
    (setq a1 nil a2 nil a4 nil a5 nil)
    (gc)
    (list (freak-mapping-value w2)
          (freak-mapping-value w4)
          (freak-mapping-value w1)
          (freak-mapping-value w3))))
(nil nil (x4) (x5))

(let ((a1 (list 'x1))
      (a2 (list 'x2))
      (a3 (list 'x3))
      (a4 (list 'x4))
      (a5 (list 'x5)))
  (let ((w1 (make-freak-mapping a3 a4))
        (w2 (make-freak-mapping a1 a2))
        (w3 (make-freak-mapping a4 a5))
        (w4 (make-freak-mapping a2 a3)))
    (setq a1 nil a2 nil a3 nil a5 nil)
    (gc)
    (list (freak-mapping-value w2)
          (freak-mapping-value w4)
          (freak-mapping-value w1)
          (freak-mapping-value w3))))
(nil nil nil (x5))

(let ((a1 (list 'x1))
      (a2 (list 'x2))
      (a3 (list 'x3))
      (a4 (list 'x4))
      (a5 (list 'x5)))
  (let ((w1 (make-freak-mapping a3 a4))
        (w2 (make-freak-mapping a1 a2))
        (w3 (make-freak-mapping a4 a5))
        (w4 (make-freak-mapping a2 a3)))
    (setq a1 nil a2 nil a3 nil a4 nil)
    (gc)
    (list (freak-mapping-value w2)
          (freak-mapping-value w4)
          (freak-mapping-value w1)
          (freak-mapping-value w3))))
(nil nil nil nil)

(let ((a (list 'x))
      (b (list 'y))
      (c (list 'z)))
  (let ((w (make-freak-mapping a b)))
    (setf (freak-mapping-value w) c)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
(((x) (z) t) ((z)))

(let ((a (list 'x))
      (b (list 'y))
      (c (list 'z)))
  (let ((w (make-freak-mapping a b)))
    (setf (freak-mapping-value w) c)
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
(((x) (z) t) ((z)))

(let ((a (list 'x))
      (b (list 'y))
      (c (list 'z)))
  (let ((w (make-freak-mapping a b)))
    (gc)
    (setf (freak-mapping-value w) c)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
(((x) (z) t) ((z)))

(let ((a (list 'x))
      (b (list 'y))
      (c (list 'z)))
  (let ((w (make-freak-mapping a b)))
    (setq a nil)
    (setf (freak-mapping-value w) c)
    (gc)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
((nil nil nil) (nil))

(let ((a (list 'x))
      (b (list 'y))
      (c (list 'z)))
  (let ((w (make-freak-mapping a b)))
    (setq a nil)
    (gc)
    (setf (freak-mapping-value w) c)
    (list (multiple-value-list (freak-mapping-pair w))
          (multiple-value-list (freak-mapping-value w)))))
((nil nil nil) (nil))


;; Check that the GC can propagate through long chains of WEAK-MAPPINGs,
;; emulating WEAK-MAPPING with weak hash-tables.

(progn
  (defun test-weak-mapping-chain (n)
    (let (wm0)
      (let ((sym (make-array n)))
        (dotimes (i n) (setf (aref sym i) (make-symbol (prin1-to-string i))))
        ;; Build a chain
        ;;   (gethash sym0 wm0) = wm1
        ;;   (gethash sym1 wm1) = wm2
        ;;   ...
        (let ((wm (make-array n)))
          (dotimes (i n) (setf (aref wm i) (make-freak-mapping 'a 'b)))
          (setq wm0 (aref wm 0))
          (do ((i 1 (1+ i)))
              ((>= i n))
            (setf (gethash (aref sym (- i 1)) (aref wm (- i 1))) (aref wm i))))
        (time (gc))
        ;; Verify that the chain is still intact.
        (do ((i 0 (1+ i))
             (w wm0 (gethash (aref sym i) w)))
            ((>= i n)))
        (setq sym nil)
        (time (gc))
        (gethash 'a wm0))))
  (test-weak-mapping-chain 10000))
B

; Likewise with reverse order of allocation of the weak hash tables.
; This test exhibits O(n^2) behaviour in LispWorks 4.3.
(progn
  (defun test-weak-mapping-chain-reverse (n)
    (let (wm0)
      (let ((sym (make-array n)))
        (dotimes (i n) (setf (aref sym i) (make-symbol (prin1-to-string i))))
        ;; Build a chain
        ;;   (gethash sym0 wm0) = wm1
        ;;   (gethash sym1 wm1) = wm2
        ;;   ...
        (let ((wm (make-array n)))
          (dotimes (i n) (setf (aref wm (- n 1 i)) (make-freak-mapping 'a 'b)))
          (setq wm0 (aref wm 0))
          (do ((i 1 (1+ i)))
              ((>= i n))
            (setf (gethash (aref sym (- i 1)) (aref wm (- i 1))) (aref wm i))))
        (time (gc))
        ;; Verify that the chain is still intact.
        (do ((i 0 (1+ i))
             (w wm0 (gethash (aref sym i) w)))
            ((>= i n)))
        (setq sym nil)
        (time (gc))
        (gethash 'a wm0))))
  (test-weak-mapping-chain-reverse 10000))
B


;; Test that weak hash-tables of kind :VALUE work.

#+(or CLISP OpenMCL LISPWORKS)
(let ((tab (make-hash-table :test 'eq
                            #+LISPWORKS :weak-kind #-LISPWORKS :weak :value)))
  (setf (gethash 'foo tab) 1)
  (setf (gethash 1 tab) (copy-seq "bar"))
  (setf (gethash 'zoo tab) (copy-seq "zot"))
  (gc)
  (list (gethash 'foo tab) (gethash 1 tab) (gethash 'zoo tab)))
#+(or CLISP OpenMCL LISPWORKS)
(1 NIL NIL)

#+(or CLISP LISPWORKS)
(let ((tab (make-hash-table :test 'equal
                            #+LISPWORKS :weak-kind #-LISPWORKS :weak :value)))
  (weak-ht-fill-initially tab)
  (gc)
  (list (gethash "foo" tab) (gethash 1 tab) (gethash "zoo" tab)))
#+(or CLISP LISPWORKS)
(1 NIL NIL)

;; Test that weak hash-tables of kind :VALUE don't suffer from the
;; "value in key" problem.

#+(or CLISP OpenMCL LISPWORKS)
(let ((tab (make-hash-table :test 'eq
                            #+LISPWORKS :weak-kind #-LISPWORKS :weak :value)))
  (let ((a (list 'x)))
    (let ((b (list 'y)))
      (setf (gethash 'xxx tab) a)
      (setf (gethash (cons 'yyy b) tab) b))
    (gc)
    (list (hash-table-count tab)
          (eq (gethash 'xxx tab) a)
          (let ((l nil)) (maphash #'(lambda (k v) (push v l)) tab) l))))
#+(or CLISP OpenMCL LISPWORKS)
(1 T ((X)))


;; Test that weak hash-tables of kind :KEY-AND-VALUE work.

#+(or CLISP LISPWORKS)
(let ((tab (make-hash-table :test #+OpenMCL 'eq #-OpenMCL 'equal
                            #+LISPWORKS :weak-kind #+LISPWORKS :both
                            #-LISPWORKS :weak #-LISPWORKS :key-and-value)))
  (weak-ht-fill-initially tab)
  (gc)
  (list (gethash "foo" tab) (gethash 1 tab) (gethash "zoo" tab)))
#+(or CLISP LISPWORKS)
(NIL NIL NIL)


;; Test that weak hash-tables of kind :KEY-OR-VALUE work.

#+(or CLISP LISPWORKS)
(let ((tab (make-hash-table :test #+OpenMCL 'eq #-OpenMCL 'equal
                            #+LISPWORKS :weak-kind #+LISPWORKS :either
                            #-LISPWORKS :weak #-LISPWORKS :key-or-value)))
  #+LISPWORKS (set-hash-table-weak tab :either)
  (weak-ht-fill-initially tab)
  (gc)
  (list (gethash "foo" tab) (gethash 1 tab) (gethash "zoo" tab)))
#+(or CLISP LISPWORKS)
(1 "bar" NIL)
