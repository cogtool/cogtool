;;;; Common Lisp Object System for CLISP
;;;; Objects with stable hash code
;;;; Part 2: Final class definition, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod shared-initialize ((object standard-stablehash) situation &rest args
                              &key)
  (apply #'shared-initialize-<standard-stablehash> object situation args))

;;; ===========================================================================

;; Definition of <structure-stablehash>.
;; Used for (make-hash-table :test 'stablehash-eq).
(defstruct (structure-stablehash (:predicate nil) (:copier nil))
  (hashcode (sys::random-posfixnum))) ; GC invariant hash code

;;; ===========================================================================
