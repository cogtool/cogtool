;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")

;; A vector that looks like a class. Needed to make instance_of_stablehash_p
;; work already during bootstrapping.
(defvar *dummy-class*
        (vector nil ; inst_class_version
                nil ; $hashcode
                nil ; $direct-generic-functions
                nil ; $direct-methods
                nil ; $classname
                nil ; $direct-subclasses
                nil ; $direct-superclasses
                nil ; $all-superclasses
                nil ; $precedence-list
                nil ; $direct-slots
                nil ; $slots
                nil ; $slot-location-table
                nil ; $direct-default-initargs
                nil ; $default-initargs
                nil ; $documentation
                nil ; $initialized
                t   ; $subclass-of-stablehash-p
                nil ; $generic-accessors
                nil ; $direct-accessors
                nil ; $valid-initargs
                nil ; $instance-size
)       )

;; A new class-version is created each time a class is redefined.
;; Used to keep the instances in sync through lazy updates.
;; Note: Why are the shared-slots an element of the class-version, not of the
;;   class? Answer: When a class is redefined in such a way that a shared slot
;;   becomes local, the update of the instances of its subclasses needs to
;;   access the value of the shared slot before the redefinition. This is
;;   prepared by class-version-compute-slotlists for each subclass; but when
;;   this is run, the pair (class . index) is not sufficient any more to
;;   retrieve the value. Hence we use a pair (class-version . index) instead.
;;   Then, storing the shared-slots vector in the class-version avoids an
;;   indirection:    class-version -> shared-slots
;;   instead of      class-version -> class -> shared-slots.
#|
(defstruct (class-version (:type vector) (:predicate nil) (:copier nil) (:conc-name "CV-"))
  newest-class             ; the CLASS object describing the newest available version
  class                    ; the CLASS object describing the slots
  shared-slots             ; simple-vector with the values of all shared slots, or nil
  serial                   ; serial number of this class version
  (next nil)               ; next class-version, or nil
  (slotlists-valid-p nil)  ; true if the following fields are already computed
  kept-slot-locations      ; plist of old and new slot locations of those slots
                           ; that remain local or were shared and become local
  added-slots              ; list of local slots that are added in the next version
  discarded-slots          ; list of local slots that are removed or become
                           ; shared in the next version
  discarded-slot-locations ; plist of local slots and their old slot locations
                           ; that are removed or become shared in the next version
)
|#
(defun make-class-version (&key (newest-class *dummy-class*)
                                (class *dummy-class*)
                                shared-slots serial next
                                slotlists-valid-p kept-slot-locations
                                added-slots discarded-slots discarded-slot-locations)
  (vector newest-class class shared-slots serial next
          slotlists-valid-p kept-slot-locations
          added-slots discarded-slots discarded-slot-locations))
(proclaim '(inline cv-newest-class))
(defun cv-newest-class (object) (svref object 0))
(defsetf cv-newest-class (object) (value) `(setf (svref ,object 0) ,value))
(proclaim '(inline cv-class))
(defun cv-class (object) (svref object 1))
(defsetf cv-class (object) (value) `(setf (svref ,object 1) ,value))
(proclaim '(inline cv-shared-slots))
(defun cv-shared-slots (object) (svref object 2))
(defsetf cv-shared-slots (object) (value) `(setf (svref ,object 2) ,value))
(proclaim '(inline cv-serial))
(defun cv-serial (object) (svref object 3))
(defsetf cv-serial (object) (value) `(setf (svref ,object 3) ,value))
(proclaim '(inline cv-next))
(defun cv-next (object) (svref object 4))
(defsetf cv-next (object) (value) `(setf (svref ,object 4) ,value))
(proclaim '(inline cv-slotlists-valid-p))
(defun cv-slotlists-valid-p (object) (svref object 5))
(defsetf cv-slotlists-valid-p (object) (value) `(setf (svref ,object 5) ,value))
(proclaim '(inline cv-kept-slot-locations))
(defun cv-kept-slot-locations (object) (svref object 6))
(defsetf cv-kept-slot-locations (object) (value) `(setf (svref ,object 6) ,value))
(proclaim '(inline cv-added-slots))
(defun cv-added-slots (object) (svref object 7))
(defsetf cv-added-slots (object) (value) `(setf (svref ,object 7) ,value))
(proclaim '(inline cv-discarded-slots))
(defun cv-discarded-slots (object) (svref object 8))
(defsetf cv-discarded-slots (object) (value) `(setf (svref ,object 8) ,value))
(proclaim '(inline cv-discarded-slot-locations))
(defun cv-discarded-slot-locations (object) (svref object 9))
(defsetf cv-discarded-slot-locations (object) (value) `(setf (svref ,object 9) ,value))
(defun class-version-p (object) (and (simple-vector-p object) (eql (length object) 10)))

;; Indicates whether all class-version instances are filled, so that CLASS-OF
;; works.
(defvar *classes-finished* nil)

;; Preliminary.
;; This is needed so that <standard-object> and <structure-object> instances
;; can be printed as long as 1. some class-versions are not yet filled (delayed
;; defclass) and 2. PRINT-OBJECT is not yet defined.
(predefun print-object (object stream)
  (declare (ignore object))
  (write-string "#<UNKNOWN>" stream))
