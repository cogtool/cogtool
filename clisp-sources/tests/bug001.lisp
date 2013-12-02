(declaim (special *s1* *s2* *s3* *s4* *s5* *s6* *s7* *s8* *s9*))

(let ((vars '(a b c d e f g h))
      (vals '(17078019 -6016542674289 13315228697694 367843 13517278917
              3640256 -105424 880231916317))
      (form
       '(if
         (or
          (if
           (if (eql b g)
               (< -45 25457)
               (if (<= -61807)
                   (if (logbitp 14 c)
                       (if (<= 0)
                           (and (< a)
                                (= -407933 h))
                           (and (< e) nil))
                       (if (>= b) (or t) (= -8)))
                   (equal c -1)))
           t
           (and
            (not
             (if
              (= 10
                 (logeqv
                  (if nil 0 (let* ((*s6* (catch 'ct4 -16))) (+ *s6*)))
                  (dpb (+ (let ((*s3* -103315560)) -1871))
                       (byte 9 18) 1)))
              (typep d '(integer * 2114980))
              (logbitp 25 (- (+ (min -24 a))))))
            (typep (let ((*s8* (dotimes (iv2 2 a) (dotimes (iv3 3 d)))))
                     228063)
                   '(integer -35821785 41031417))))
          (or
           (if (<= -4)
               nil
               (not (and (= c) nil)))
           (and
            (not
             (typep
              (- (+
                  (progn
                    (tagbody
                      (dotimes (iv1 3))
                      (when t (flet ((%f13 nil (go 1))) 0))
                     1)
                    f)
                  (if (logbitp 1 291656) -1408278
                      (if (eql e -174947) -58564
                          (if (<= d -85934401) 62750 1575271)))))
              '(integer * -62)))
            (and
             (if
              (or (not (if (= h) (<= a) (<= 106812))))
              (if (eql c 11879)
                  (>= -23)
                  (and (logbitp 6 b) nil))
              (not
               (and (or (<= -146)
                        (and (/=
                              (integer-length 10000000)
                              (let ((*s2* h))
                                (block b6 -593)))
                             (<= d g)))
                    (not
                     (> b
                        (logandc2
                         (if (or (< a e) (if t (<= b) (< a)))
                             (if (not (= (progv '(*s6*) (list 988797392) a) b))
                                 b
                                 (catch 'ct5
                                   (if (eql 23059 1552528360)
                                       (if (/= 8 -54681) 40 -5) a)))
                             (setf h 689085499960))
                         1))))))
             (ldb-test (byte 25 32)
                       (1+
                        (restart-case
                            (+
                             (catch 'ct5
                               (if (/= 15
                                       (if (eql c h) a
                                           (- (* (logior -114854 c)))))
                                   19
                                   0))))))))))
         (floor
          (case b
            ((76) 21)
            ((59) 22)
            ((8) c)
            ((15) 23)
            ((113) 10284)
            ((72) 1)
            ((121) 17)
            ((38) e)
            ((10) 19)
            ((118) f)
            (t
             (if
              (logbitp 19
                       (if
                        (not
                         (or
                          (logbitp 16 (+ (+ a f d)))
                          (if (>= d 457162)
                              (and (or (/= 1989 c)
                           (= 146002 0)))
                              (< 66142 (+ (+ 100 d) (+ (+ a -2) h) -2)))))
                        (+ (catch 'ct8 (* 2 c)))
                        (lognor
                         (lognor (min 6278190 (min 4251 271551184))
                                 (logior (if (>=  (catch 'ct8 d) 6)
                                             (mask-field (byte 16 16) d)
                                             b)))
                         (lcm 1001 -26))))
              (let*
                  ((*s7*
                    (if
                     (zerop b)
                     f
                     (flet ((%f3 (&optional (f3-1 (progv '(*s3*) (list a) h)))
                              (declare (ignore f3-1))
                              1))
                       (%f3)))))
                (+ (flet ((%f7 () (if (equal 0 d) -129 b))) (%f7))))
              (+
               (dotimes (iv1 3 0)
                 (case (* f 4)
                   ((13 72 -335) 0)
           (t (dotimes (iv4 1) (dotimes (iv2 1)))
              (restart-case -5))))
               (let ((*s4* (ignore-errors g)))
                 (let ((*s2* 3683)) g)))))))
         1)))
  (let* ((fn (coerce `(lambda ,vars ,form) 'function))
         (cfn (compile nil fn))
         (e (apply fn vals))
         (c (apply cfn vals)))
    (format t "~&EVAL: ~S~%COMPILE: ~S~%" e c)
    (assert (equal e c))))
