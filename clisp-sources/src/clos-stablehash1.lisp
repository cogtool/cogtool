;;;; Common Lisp Object System for CLISP
;;;; Objects with stable hash code
;;;; Part 1: Class definition.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;;; The class <standard-stablehash> allows CLOS instances to have a
;;; GC-invariant EQ hash code.
;;; Used for (make-hash-table :test 'stablehash-eq).

(defvar *<standard-stablehash>-defclass*
  '(defclass standard-stablehash ()
     (($hashcode :initform (sys::random-posfixnum))) ; GC invariant hash code
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<standard-stablehash>-hashcode-location* 1)

;; No need for accessors. The hashcode is used by hashtabl.d.

;; Initialization of a <standard-stablehash> instance.
(defun shared-initialize-<standard-stablehash> (object situation &rest args
                                                &key &allow-other-keys)
  (if *classes-finished*
    (apply #'%shared-initialize object situation args) ; == (call-next-method)
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (standard-instance-access object *<standard-stablehash>-hashcode-location*)
            (sys::random-posfixnum))))
  object)

;;; ===========================================================================
