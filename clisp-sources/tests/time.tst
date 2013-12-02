;; -*- Lisp -*-
;; check universal time functions
;; <http://www.lisp.org/HyperSpec/Body/sec_25-1-4.html>
;; <http://www.lisp.org/HyperSpec/Body/sec_25-1-4-1.html>
;; <http://www.lisp.org/HyperSpec/Body/sec_25-1-4-2.html>

;; from <http://www.lisp.org/HyperSpec/Body/fun_encode-universal-time.html>
(encode-universal-time 0 0 0 1 1 1900 0) 0
(encode-universal-time 0 0 1 4 7 1976 5) 2414296800

(defun check-universal-time (time &optional tz)
  "check that DECODE-UNIVERSAL-TIME is the inverse of ENCODE-UNIVERSAL-TIME"
  (multiple-value-bind (se mi ho da mo ye wk ds-p zone)
      (decode-universal-time time tz)
    ;; we must give encode-universal-time some DST information because
    ;; otherwize it has no way to tell if 1995-10-27 01:30:00 is DST or not
    (let ((ut (encode-universal-time se mi ho da mo ye
                                     (or tz (if ds-p (1- zone) zone)))))
      (unless (= ut time)
        (list time (list se mi ho da mo ye wk ds-p zone tz) ut (- ut time))))))
CHECK-UNIVERSAL-TIME

;; compile for speed & check documentation perseverance
(stringp (documentation 'check-universal-time 'function)) T
(compile 'check-universal-time) CHECK-UNIVERSAL-TIME
(stringp (documentation #'check-universal-time t)) T

(check-universal-time 2879996399) NIL
(check-universal-time 2879996400) NIL

(check-universal-time 3)   NIL
(check-universal-time 3 7) NIL

(defun time-loop (start end step &optional tz)
  "return the periods of badness"
  (time
   (loop :with state = nil :with ret = nil
     :for tm :from start :below end :by step
     :for check = (check-universal-time tm tz) :do
     (case state
       ((:good) (when check (setq state :bad) (show check) (push check ret)))
       ((:bad) (unless check
                 (push (show (list tm (multiple-value-list
                                       (decode-universal-time tm tz))))
                       (car ret))
                 (setq state :good)))
       (t (setq state (if check :bad :good)) (show (list state tm check))))
     :finally (return (nreverse ret)))))
TIME-LOOP

;; compile for speed & check documentation perseverance
(stringp (documentation #'time-loop t)) T
(compile 'time-loop) TIME-LOOP
(stringp (documentation 'time-loop 'function)) T

;; default timezone
(time-loop 100000 5000000000 5000)  NIL
(time-loop 4300066700 4300181201 10) NIL

;; specific timezone
(time-loop 100000 5000000000 5000 0)  NIL
(time-loop 4300066700 4300081201 10 0) NIL

;; random times, default timezone
(let ((total 10000))
  (loop :with bad = 0 :repeat total :for time = (random 10000000000)
    :for check = (check-universal-time time)
    :when check :collect (progn (incf bad) check)
    :finally (format t "~&~:D out of ~:D bad: ~5F%~%"
                     bad total (/ bad total 1d-2))))
NIL

;; random times, specific timezone
(let ((total 10000))
  (loop :with bad = 0 :repeat total :for time = (random 10000000000)
    :for check = (check-universal-time time 0)
    :when check :collect (progn (incf bad) check)
    :finally (format t "~&~:D out of ~:D bad: ~5F%~%"
                     bad total (/ bad total 1d-2))))
NIL

;; crash on woe32 in localtime
(- (encode-universal-time 12 34 21 23 12 2208)
   (encode-universal-time 12 34 20 23 12 2208))
3600

;; clean up
(unintern 'time-loop) T
(unintern 'check-universal-time) T
