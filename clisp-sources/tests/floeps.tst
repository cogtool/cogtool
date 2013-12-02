;; -*- Lisp -*-
;; test floating point epsilons
;; (c) 2002 Sam Steingold <sds@gnu.org>
;; http://www.lisp.org/HyperSpec/Body/convar_short-_tive-epsilon.html
;; adapted from code by Stefan Kain <smk@users.sourceforge.net>

(defun test-pos-epsilon (<EPSILON>)
  (/= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>)))
test-pos-epsilon

(test-pos-epsilon short-float-epsilon)  t
(test-pos-epsilon single-float-epsilon) t
(test-pos-epsilon double-float-epsilon) t
(test-pos-epsilon long-float-epsilon)   t

(defun test-neg-epsilon (<EPSILON>)
  (/= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>)))
test-neg-epsilon

(test-neg-epsilon short-float-negative-epsilon)  t
(test-neg-epsilon single-float-negative-epsilon) t
(test-neg-epsilon double-float-negative-epsilon) t
(test-neg-epsilon long-float-negative-epsilon)   t

(defun binary-search (lower-bound upper-bound old-value precision test-fun)
  (let* ((new-value  (float (/ (+ lower-bound upper-bound) 2) precision)))
    (if (= old-value new-value)
        (float upper-bound precision)
        (if (funcall test-fun new-value)
            (binary-search lower-bound new-value new-value
                           precision test-fun)
            (binary-search new-value upper-bound new-value
                           precision test-fun)))))
binary-search

(defun check-eps (eps test) (= eps (binary-search 0 1 0 eps test)))
check-eps

(check-eps short-float-epsilon #'test-pos-epsilon)           t
(check-eps short-float-negative-epsilon #'test-neg-epsilon)  t
(check-eps single-float-epsilon #'test-pos-epsilon)          t
(check-eps single-float-negative-epsilon #'test-neg-epsilon) t
(check-eps double-float-epsilon #'test-pos-epsilon)          t
(check-eps double-float-negative-epsilon #'test-neg-epsilon) t
(check-eps long-float-epsilon #'test-pos-epsilon)            t
(check-eps long-float-negative-epsilon #'test-neg-epsilon)   t
