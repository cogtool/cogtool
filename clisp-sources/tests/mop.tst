;; -*- Lisp -*-
;; Test some MOP-like CLOS features

; Make the MOP symbols accessible from package CLOS.
#-(or CLISP GCL ALLEGRO LISPWORKS)
(let ((packname
         #+SBCL "SB-PCL" ; or "SB-MOP"?
         #+CMU "PCL" ; or "MOP"?
         #+OpenMCL "OPENMCL-MOP" ; or "CCL" ?
         ))
  #+SBCL (unlock-package packname)
  (rename-package packname packname (cons "CLOS" (package-nicknames packname)))
  t)
#-(or CLISP GCL ALLEGRO LISPWORKS)
T

#+ALLEGRO
(without-package-locks
  (import 'excl::compute-effective-slot-definition-initargs "CLOS")
  (export 'excl::compute-effective-slot-definition-initargs "CLOS"))
#+ALLEGRO
T

#+CMU
(export 'pcl::compute-effective-slot-definition-initargs "PCL")
#+CMU
T

#+SBCL
(export 'sb-pcl::compute-effective-slot-definition-initargs "SB-PCL")
#+SBCL
T

#+OpenMCL
(progn
  (import 'ccl::funcallable-standard-object "OPENMCL-MOP")
  (export 'ccl::funcallable-standard-object "OPENMCL-MOP")
  (import 'ccl::eql-specializer "OPENMCL-MOP")
  (export 'ccl::eql-specializer "OPENMCL-MOP")
  (import 'ccl::slot-definition "OPENMCL-MOP")
  (export 'ccl::slot-definition "OPENMCL-MOP")
  (import 'ccl::direct-slot-definition "OPENMCL-MOP")
  (export 'ccl::direct-slot-definition "OPENMCL-MOP")
  (import 'ccl::effective-slot-definition "OPENMCL-MOP")
  (export 'ccl::effective-slot-definition "OPENMCL-MOP"))
#+OpenMCL
T

#+LISPWORKS
(progn
  (export 'clos::compute-default-initargs "CLOS")
  (export 'clos::compute-discriminating-function "CLOS")
  (export 'clos::compute-effective-slot-definition-initargs "CLOS"))
#+LISPWORKS
T

#+LISPWORKS
(progn
  (defun gc () (mark-and-sweep 3))
  t)
#+LISPWORKS
T

#-(or ALLEGRO CMU18 OpenMCL LISPWORKS)
(progn
  (defstruct rectangle1 (x 0.0) (y 0.0))
  (defclass counted1-class (structure-class)
    ((counter :initform 0)))
  (defclass counted1-rectangle (rectangle1) () (:metaclass counted1-class))
  (defmethod make-instance :after ((c counted1-class) &rest args)
    (incf (slot-value c 'counter)))
  (slot-value (find-class 'counted1-rectangle) 'counter)
  (make-instance 'counted1-rectangle)
  (slot-value (find-class 'counted1-rectangle) 'counter))
#-(or ALLEGRO CMU18 OpenMCL LISPWORKS)
1

#-CMU18
(progn
  (defclass rectangle2 ()
    ((x :initform 0.0 :initarg x) (y :initform 0.0 :initarg y)))
  (defclass counted2-class (standard-class)
    ((counter :initform 0)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 counted2-class) (c2 standard-class))
    t)
  (defclass counted2-rectangle (rectangle2) () (:metaclass counted2-class))
  (defmethod make-instance :after ((c counted2-class) &rest args)
    (incf (slot-value c 'counter)))
  (slot-value (find-class 'counted2-rectangle) 'counter)
  (make-instance 'counted2-rectangle)
  (slot-value (find-class 'counted2-rectangle) 'counter))
#-CMU18
1

(progn
  (defclass counter ()
    ((count :allocation :class :initform 0 :reader how-many)))
  (defclass counted-object (counter) ((name :initarg :name)))
  (defmethod initialize-instance :after ((obj counter) &rest args)
    (incf (slot-value obj 'count)))
  (unless (clos:class-finalized-p (find-class 'counter))
    (clos:finalize-inheritance (find-class 'counter)))
  (list (how-many (make-instance 'counted-object :name 'foo))
        (how-many (clos:class-prototype (find-class 'counter)))
        (how-many (make-instance 'counted-object :name 'bar))
        (how-many (clos:class-prototype (find-class 'counter)))))
(1 1 2 2)

;; Check that the slot :accessor option works also on structure-class.
#-(or ALLEGRO OpenMCL LISPWORKS)
(progn
  (defclass structure01 () ((x :initarg :x :accessor structure01-x))
    (:metaclass structure-class))
  (let ((object (make-instance 'structure01 :x 17)))
    (list (typep #'structure01-x 'generic-function)
          (structure01-x object)
          (progn (incf (structure01-x object)) (structure01-x object)))))
#-(or ALLEGRO OpenMCL LISPWORKS)
(t 17 18)

;; Check that defstruct and defclass interoperate with each other.
#-(or ALLEGRO LISPWORKS)
(progn
  (defstruct structure02a
    slot1
    (slot2 t)
    (slot3 (floor pi))
    #-(or CMU SBCL) (slot4 44))
  (defclass structure02b (structure02a)
    ((slot4 :initform -44)
     (slot5)
     (slot6 :initform t)
     (slot7 :initform (floor (* pi pi)))
     (slot8 :initform 88))
    (:metaclass structure-class))
  (defstruct (structure02c (:include structure02b (slot8 -88)))
    slot9
    (slot10 t)
    (slot11 (floor (exp 3))))
  (let ((a (make-structure02c))
        (b (make-instance 'structure02c)))
    (list (structure02c-slot1 a)
          (structure02c-slot2 a)
          (structure02c-slot3 a)
          (structure02c-slot4 a)
          (structure02c-slot5 a)
          (structure02c-slot6 a)
          (structure02c-slot7 a)
          (structure02c-slot8 a)
          (structure02c-slot9 a)
          (structure02c-slot10 a)
          (structure02c-slot11 a)
          ;(structure02c-slot1 b) ; may be #<UNBOUND>
          (structure02c-slot2 b)
          (structure02c-slot3 b)
          (structure02c-slot4 b)
          ;(structure02c-slot5 b) ; #<UNBOUND>
          (structure02c-slot6 b)
          (structure02c-slot7 b)
          (structure02c-slot8 b)
          ;(structure02c-slot9 b) ; may be #<UNBOUND>
          (structure02c-slot10 b)
          (structure02c-slot11 b)
          (equalp a (copy-structure a))
          (equalp b (copy-structure b))
          (equalp a b))))
#-(or ALLEGRO LISPWORKS)
(nil t 3 -44 nil t 9 -88 nil t 20
     t 3 -44     t 9 -88     t 20
 t t nil)

;; Check that defstruct and defclass interoperate with each other.
#-(or ALLEGRO LISPWORKS)
(progn
  (defclass structure03a ()
    ((slot1)
     (slot2 :initform t)
     (slot3 :initform (floor pi))
     (slot4 :initform 44))
    (:metaclass structure-class))
  (defstruct (structure03b (:include structure03a (slot4 -44)))
    slot5
    (slot6 t)
    (slot7 (floor (* pi pi)))
    #-(or CMU SBCL) (slot8 88))
  (defclass structure03c (structure03b)
    ((slot8 :initform -88)
     (slot9)
     (slot10 :initform t)
     (slot11 :initform (floor (exp 3))))
    (:metaclass structure-class))
  (let ((b (make-instance 'structure03c)))
    (list ;(slot-value b 'slot1) ; #<UNBOUND>
          (slot-value b 'slot2)
          (slot-value b 'slot3)
          (slot-value b 'slot4)
          ;(slot-value b 'slot5) ; may be #<UNBOUND>
          (slot-value b 'slot6)
          (slot-value b 'slot7)
          (slot-value b 'slot8)
          ;(slot-value b 'slot9) ; #<UNBOUND>
          (slot-value b 'slot10)
          (slot-value b 'slot11)
          (equalp b (copy-structure b)))))
#-(or ALLEGRO LISPWORKS)
(    t 3 -44     t 9 -88     t 20
 t)

;; Check that print-object can print all kinds of uninitialized metaobjects.
(defun as-string (obj)
  (let ((string (write-to-string obj :escape t :pretty nil)))
    ;; For CLISP: Remove pattern #x[0-9A-F]* from it:
    (let ((i (search "#x" string)))
      (when i
        (let ((j (or (position-if-not #'(lambda (c) (digit-char-p c 16)) string
                                      :start (+ i 2))
                     (length string))))
          (setq string (concatenate 'string (subseq string 0 i) (subseq string j))))))
    ;; For CMUCL, SBCL: Substitute {} for pattern {[0-9A-F]*} :
    (do ((pos 0))
        (nil)
      (let ((i (search "{" string :start2 pos)))
        (unless i (return))
        (let ((j (position-if-not #'(lambda (c) (digit-char-p c 16)) string
                                  :start (+ i 1))))
          (if (and j (eql (char string j) #\}))
            (progn
              (setq string (concatenate 'string (subseq string 0 (+ i 1)) (subseq string j)))
              (setq pos (+ i 2)))
            (setq pos (+ i 1))))))
    ;; For LISPWORKS: Substitute > for pattern [0-9A-F]{8}> :
    (do ((pos 0))
        (nil)
      (let ((i (search ">" string :start2 pos)))
        (unless i (return))
        (if (and (>= (- i pos) 8)
                 (eql (position-if-not #'(lambda (c) (digit-char-p c 16)) string
                                       :start (- i 8))
                      i))
          (progn
            (setq string (concatenate 'string (subseq string 0 (- i 8)) (subseq string i)))
            (setq pos (+ (- i 8) 1)))
          (setq pos (+ i 1)))))
    string))
AS-STRING

#-LISPWORKS
(as-string (allocate-instance (find-class 'clos:specializer)))
#+CLISP "#<SPECIALIZER >"
#+ALLEGRO "#<ACLMOP:SPECIALIZER @ >"
#+CMU "#<PCL:SPECIALIZER {}>"
#+SBCL "#<SB-MOP:SPECIALIZER {}>"
#+OpenMCL "#<SPECIALIZER >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'class)))
#+CLISP "#<CLASS #<UNBOUND>>"
#+ALLEGRO "#<CLASS \"Unnamed\" @ >"
#+CMU "#<CLASS \"unbound\" {}>"
#+SBCL "#<CLASS \"unbound\">"
#+LISPWORKS "#<CLASS  >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'standard-class)))
#+CLISP "#<STANDARD-CLASS #<UNBOUND> :UNINITIALIZED>"
#+ALLEGRO "#<STANDARD-CLASS \"Unnamed\" @ >"
#+CMU "#<STANDARD-CLASS \"unbound\" {}>"
#+SBCL "#<STANDARD-CLASS \"unbound\">"
#+LISPWORKS "#<STANDARD-CLASS  >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'structure-class)))
#+CLISP "#<STRUCTURE-CLASS #<UNBOUND>>"
#+ALLEGRO "#<STRUCTURE-CLASS \"Unnamed\" @ >"
#+CMU "#<STRUCTURE-CLASS \"unbound\" {}>"
#+SBCL "#<STRUCTURE-CLASS \"unbound\">"
#+LISPWORKS "#<STRUCTURE-CLASS  >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-LISPWORKS
(as-string (allocate-instance (find-class 'clos:eql-specializer)))
#+CLISP "#<EQL-SPECIALIZER #<UNBOUND>>"
#+ALLEGRO "#<ACLMOP:EQL-SPECIALIZER #<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>>"
#+CMU "#<PCL:EQL-SPECIALIZER {}>"
#+SBCL "#<SB-MOP:EQL-SPECIALIZER {}>"
#+OpenMCL "#<EQL-SPECIALIZER \"<unbound>\" >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:slot-definition)))
#+CLISP "#<SLOT-DEFINITION #<UNBOUND> >"
#+ALLEGRO "#<ACLMOP:SLOT-DEFINITION @ >"
#+CMU "#<SLOT-DEFINITION \"unbound\" {}>"
#+SBCL "#<SB-MOP:SLOT-DEFINITION \"unbound\">"
#+LISPWORKS "#<SLOT-DEFINITION >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:direct-slot-definition)))
#+CLISP "#<DIRECT-SLOT-DEFINITION #<UNBOUND> >"
#+ALLEGRO "#<ACLMOP:DIRECT-SLOT-DEFINITION @ >"
#+CMU "#<DIRECT-SLOT-DEFINITION \"unbound\" {}>"
#+SBCL "#<SB-MOP:DIRECT-SLOT-DEFINITION \"unbound\">"
#+LISPWORKS "#<DIRECT-SLOT-DEFINITION >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:effective-slot-definition)))
#+CLISP "#<EFFECTIVE-SLOT-DEFINITION #<UNBOUND> >"
#+ALLEGRO "#<ACLMOP:EFFECTIVE-SLOT-DEFINITION @ >"
#+CMU "#<EFFECTIVE-SLOT-DEFINITION \"unbound\" {}>"
#+SBCL "#<SB-MOP:EFFECTIVE-SLOT-DEFINITION \"unbound\">"
#+LISPWORKS "#<EFFECTIVE-SLOT-DEFINITION >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:standard-direct-slot-definition)))
#+CLISP "#<STANDARD-DIRECT-SLOT-DEFINITION #<UNBOUND> >"
#+ALLEGRO "#<ACLMOP:STANDARD-DIRECT-SLOT-DEFINITION #<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>>"
#+CMU "#<STANDARD-DIRECT-SLOT-DEFINITION \"unbound\" {}>"
#+SBCL "#<SB-MOP:STANDARD-DIRECT-SLOT-DEFINITION \"unbound\">"
#+LISPWORKS "#<STANDARD-DIRECT-SLOT-DEFINITION \"#< Unbound Slot >\" >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:standard-effective-slot-definition)))
#+CLISP "#<STANDARD-EFFECTIVE-SLOT-DEFINITION #<UNBOUND> >"
#+ALLEGRO "#<ACLMOP:STANDARD-EFFECTIVE-SLOT-DEFINITION #<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>>"
#+CMU "#<STANDARD-EFFECTIVE-SLOT-DEFINITION \"unbound\" {}>"
#+SBCL "#<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION \"unbound\">"
#+LISPWORKS "#<STANDARD-EFFECTIVE-SLOT-DEFINITION \"#< Unbound Slot >\" >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'method-combination)))
#+CLISP "#<METHOD-COMBINATION #<UNBOUND> >"
#+ALLEGRO "#<METHOD-COMBINATION @ >"
#+(or CMU SBCL) "#<METHOD-COMBINATION {}>"
#+LISPWORKS "#<METHOD-COMBINATION NIL >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(as-string (allocate-instance (find-class 'method)))
#+CLISP "#<METHOD >"
#+ALLEGRO "#<METHOD @ >"
#+(or CMU SBCL) "#<METHOD {}>"
#+OpenMCL "#<METHOD >"
#+LISPWORKS "#<METHOD >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'standard-method)))
#+CLISP "#<STANDARD-METHOD :UNINITIALIZED>"
#+ALLEGRO "#<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>"
#+CMU "#<#<STANDARD-METHOD {}> {}>"
#+SBCL "#<STANDARD-METHOD #<STANDARD-METHOD {}> {}>"
#+LISPWORKS "#<STANDARD-METHOD >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:standard-reader-method)))
#+CLISP "#<STANDARD-READER-METHOD :UNINITIALIZED>"
#+ALLEGRO "#<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>"
#+CMU "#<#<#<PCL:STANDARD-READER-METHOD {}> {}> {}>"
#+SBCL "#<SB-MOP:STANDARD-READER-METHOD #<SB-MOP:STANDARD-READER-METHOD #<SB-MOP:STANDARD-READER-METHOD {}> {}> {}>"
#+LISPWORKS "#<STANDARD-READER-METHOD >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#-OpenMCL
(as-string (allocate-instance (find-class 'clos:standard-writer-method)))
#+CLISP "#<STANDARD-WRITER-METHOD :UNINITIALIZED>"
#+ALLEGRO "#<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>"
#+CMU "#<#<#<PCL:STANDARD-WRITER-METHOD {}> {}> {}>"
#+SBCL "#<SB-MOP:STANDARD-WRITER-METHOD #<SB-MOP:STANDARD-WRITER-METHOD #<SB-MOP:STANDARD-WRITER-METHOD {}> {}> {}>"
#+LISPWORKS "#<STANDARD-WRITER-METHOD >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(as-string (allocate-instance (find-class 'clos:funcallable-standard-object)))
#+CLISP "#<FUNCALLABLE-STANDARD-OBJECT #<UNBOUND>>"
#+ALLEGRO "#<ACLMOP:FUNCALLABLE-STANDARD-OBJECT @ >"
#+CMU "#<PCL:FUNCALLABLE-STANDARD-OBJECT {}>"
#+SBCL "#<SB-MOP:FUNCALLABLE-STANDARD-OBJECT {}>"
#+OpenMCL "#<CCL::FUNCALLABLE-STANDARD-OBJECT >"
#+LISPWORKS "#<CLOS:FUNCALLABLE-STANDARD-OBJECT >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(as-string (allocate-instance (find-class 'generic-function)))
#+CLISP "#<GENERIC-FUNCTION #<UNBOUND>>"
#+ALLEGRO "#<GENERIC-FUNCTION #<Printer Error, obj=: #<PROGRAM-ERROR @ #x>>>"
#+(or CMU SBCL) "#<GENERIC-FUNCTION {}>"
#+OpenMCL "#<GENERIC-FUNCTION >"
#+LISPWORKS "#<GENERIC-FUNCTION >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(as-string (allocate-instance (find-class 'standard-generic-function)))
#+CLISP "#<STANDARD-GENERIC-FUNCTION #<UNBOUND>>"
#+ALLEGRO "#<STANDARD-GENERIC-FUNCTION #<Printer Error, obj=: #<UNBOUND-SLOT @ #x>>>"
#+CMU "#<STANDARD-GENERIC-FUNCTION \"unbound\" \"?\" {}>"
#+SBCL "#<STANDARD-GENERIC-FUNCTION \"unbound\" \"?\">"
#+OpenMCL "#<Anonymous STANDARD-GENERIC-FUNCTION >"
#+LISPWORKS "#<STANDARD-GENERIC-FUNCTION  >"
#-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN


;; It is possible to redefine a class in a way that makes it non-finalized,
;; if it was not yet instantiated. Fetching the class-prototype doesn't count
;; as an instantiation.
(progn
  (defclass foo135b () ((s :initarg :s :accessor foo135b-s)))
  (unless (clos:class-finalized-p (find-class 'foo135b))
    (clos:finalize-inheritance (find-class 'foo135b)))
  (clos:class-prototype (find-class 'foo135b))
  (defclass foo135b (foo135a) ((s :accessor foo135b-s)))
  t)
T


;; Check that the prototype of every non-abstract built-in class is correct.
(let ((wrong '()))
  (labels ((check-tree (c)
             (unless (member (class-name c)
                             '(stream sequence list number real rational))
               (let ((p (clos:class-prototype c)))
                 (unless (eq (class-of p) c) (push (list c p) wrong))))
             (unless (or (member (find-class 'standard-object) (clos:class-precedence-list c))
                         (member (find-class 'structure-object) (clos:class-precedence-list c)))
               (mapc #'check-tree (clos:class-direct-subclasses c)))))
    (check-tree (find-class 't))
    wrong))
NIL


;; Check that undefined classes are treated as undefined, even though they
;; are represented by a FORWARD-REFERENCED-CLASS.
(progn
  #+CLISP (setq custom:*forward-referenced-class-misdesign* t)
  (defclass foo133 (forwardclass03) ())
  (defparameter *forwardclass* (first (clos:class-direct-superclasses (find-class 'foo133))))
  t)
T
(typep 1 *forwardclass*)
ERROR
(locally (declare (compile)) (typep 1 *forwardclass*))
ERROR
(type-expand *forwardclass*)
ERROR
(subtypep *forwardclass* 't)
ERROR
(subtypep 'nil *forwardclass*)
ERROR
#+CLISP (sys::subtype-integer *forwardclass*)
#+CLISP ERROR
#+CLISP (sys::subtype-sequence *forwardclass*)
#+CLISP NIL ; should also be ERROR
(write-to-string *forwardclass* :readably t)
ERROR
(setf (find-class 'foo133a) *forwardclass*)
ERROR
(class-name *forwardclass*)
FORWARDCLASS03
(setf (class-name *forwardclass*) 'forwardclass03changed)
ERROR
(class-name *forwardclass*)
FORWARDCLASS03
(clos:class-direct-superclasses *forwardclass*)
NIL
(clos:class-direct-slots *forwardclass*)
NIL
(clos:class-direct-default-initargs *forwardclass*)
NIL
(clos:class-precedence-list *forwardclass*)
ERROR
(clos:class-slots *forwardclass*)
ERROR
(clos:class-default-initargs *forwardclass*)
ERROR
(clos:class-finalized-p *forwardclass*)
NIL
(clos:class-prototype *forwardclass*)
ERROR
(clos:finalize-inheritance *forwardclass*)
ERROR
(clos:class-finalized-p *forwardclass*)
NIL
(eval `(defmethod foo133a ((x ,*forwardclass*))))
ERROR
(progn
  (defgeneric foo133b (x)
    (:method ((x integer)) x))
  (add-method #'foo133b
    (make-instance 'standard-method
      :qualifiers '()
      :lambda-list '(x)
      :specializers (list *forwardclass*)
      :function #'(lambda (args next-methods) (first args))))
  #-CLISP (foo133b 7))
ERROR
(not (not (typep *forwardclass* 'class)))
T ; misdesign!
#-LISPWORKS (not (not (typep *forwardclass* 'clos:specializer)))
#-LISPWORKS T ; misdesign!
(subtypep 'clos:forward-referenced-class 'class)
T ; misdesign!
#-LISPWORKS (subtypep 'clos:forward-referenced-class 'clos:specializer)
#-LISPWORKS T ; misdesign!
;; Same thing with opposite setting of *forward-referenced-class-misdesign*.
(progn
  #+CLISP (setq custom:*forward-referenced-class-misdesign* nil)
  (defclass foo134 (forwardclass04) ())
  (defparameter *forwardclass* (first (clos:class-direct-superclasses (find-class 'foo134))))
  t)
T
(typep 1 *forwardclass*)
ERROR
(locally (declare (compile)) (typep 1 *forwardclass*))
ERROR
(type-expand *forwardclass*)
ERROR
(subtypep *forwardclass* 't)
ERROR
(subtypep 'nil *forwardclass*)
ERROR
#+CLISP (sys::subtype-integer *forwardclass*)
#+CLISP ERROR
#+CLISP (sys::subtype-sequence *forwardclass*)
#+CLISP NIL ; should also be ERROR
(write-to-string *forwardclass* :readably t)
ERROR
(setf (find-class 'foo134a) *forwardclass*)
ERROR
(class-name *forwardclass*)
FORWARDCLASS04
(setf (class-name *forwardclass*) 'forwardclass04changed)
ERROR
(class-name *forwardclass*)
FORWARDCLASS04
(clos:class-direct-superclasses *forwardclass*)
NIL
(clos:class-direct-slots *forwardclass*)
NIL
(clos:class-direct-default-initargs *forwardclass*)
NIL
(clos:class-precedence-list *forwardclass*)
ERROR
(clos:class-slots *forwardclass*)
ERROR
(clos:class-default-initargs *forwardclass*)
ERROR
(clos:class-finalized-p *forwardclass*)
NIL
(clos:class-prototype *forwardclass*)
ERROR
(clos:finalize-inheritance *forwardclass*)
ERROR
(clos:class-finalized-p *forwardclass*)
NIL
(eval `(defmethod foo134a ((x ,*forwardclass*))))
ERROR
(progn
  (defgeneric foo134b (x)
    (:method ((x integer)) x))
  (add-method #'foo134b
    (make-instance 'standard-method
      :qualifiers '()
      :lambda-list '(x)
      :specializers (list *forwardclass*)
      :function #'(lambda (args next-methods) (first args))))
  #-CLISP (foo134b 7))
ERROR
(not (not (typep *forwardclass* 'class)))
#+CLISP NIL
#-CLISP T ; misdesign!
#-LISPWORKS (not (not (typep *forwardclass* 'clos:specializer)))
#+CLISP NIL
#-(or CLISP LISPWORKS) T ; misdesign!
(subtypep 'clos:forward-referenced-class 'class)
#+CLISP NIL
#-CLISP T ; misdesign!
#-LISPWORKS (subtypep 'clos:forward-referenced-class 'clos:specializer)
#+CLISP NIL
#-(or CLISP LISPWORKS) T ; misdesign!


;; Funcallable instances

; Check set-funcallable-instance-function with a SUBR.
#-LISPWORKS
(let ((f (make-instance 'clos:funcallable-standard-object)))
  (clos:set-funcallable-instance-function f #'cons)
  (funcall f 'a 'b))
#-LISPWORKS
(A . B)

; Check set-funcallable-instance-function with a small compiled-function.
#-LISPWORKS
(let ((f (make-instance 'clos:funcallable-standard-object)))
  (clos:set-funcallable-instance-function f #'(lambda (x y) (declare (compile)) (cons x y)))
  (funcall f 'a 'b))
#-LISPWORKS
(A . B)

; Check set-funcallable-instance-function with a large compiled-function.
#-LISPWORKS
(let ((f (make-instance 'clos:funcallable-standard-object)))
  (clos:set-funcallable-instance-function f #'(lambda (x y) (declare (compile)) (list 'start x y 'end)))
  (funcall f 'a 'b))
#-LISPWORKS
(START A B END)

; Check set-funcallable-instance-function with an interpreted function.
#-LISPWORKS
(let ((f (make-instance 'clos:funcallable-standard-object)))
  (clos:set-funcallable-instance-function f #'(lambda (x y) (cons x y)))
  (funcall f 'a 'b))
#-LISPWORKS
(A . B)

; Check set-funcallable-instance-function with a generic.
#-LISPWORKS
(let ((f (make-instance 'clos:funcallable-standard-object)))
  (defgeneric test-funcallable-01 (x y)
    (:method (x y) (cons x y)))
  (clos:set-funcallable-instance-function f #'test-funcallable-01)
  (funcall f 'a 'b))
#-LISPWORKS
(A . B)


;; Check that changing the class of a generic function works.
;; MOP p. 61 doesn't allow this, but CLISP supports it as an extension.

(progn
  (defclass my-gf-class (standard-generic-function)
    ((myslot :initform 17 :accessor my-myslot))
    (:metaclass clos:funcallable-standard-class))
  t)
T

#-OpenMCL
(progn
  (defgeneric foo110 (x))
  (defmethod foo110 ((x integer)) (* x x))
  (defgeneric foo110 (x) (:generic-function-class my-gf-class))
  (defmethod foo110 ((x float)) (* x x x))
  (list (foo110 10) (foo110 3.0) (my-myslot #'foo110)))
#-OpenMCL
(100 27.0 17)

; Also check that the GC cleans up forward pointers.

#-OpenMCL
(progn
  (defgeneric foo111 (x))
  (defmethod foo111 ((x integer)) (* x x))
  (defgeneric foo111 (x) (:generic-function-class my-gf-class))
  (gc)
  (defmethod foo111 ((x float)) (* x x x))
  (list (foo111 10) (foo111 3.0) (my-myslot #'foo111)
        #+CLISP (eq (sys::%record-ref #'foo111 0) (clos::class-current-version (find-class 'my-gf-class)))))
#-OpenMCL
(100 27.0 17 #+CLISP T)

#-OpenMCL
(progn
  (defgeneric foo112 (x))
  (defmethod foo112 ((x integer)) (* x x))
  (defgeneric foo112 (x) (:generic-function-class my-gf-class))
  (defmethod foo112 ((x float)) (* x x x))
  (gc)
  (list (foo112 10) (foo112 3.0) (my-myslot #'foo112)
        #+CLISP (eq (sys::%record-ref #'foo112 0) (clos::class-current-version (find-class 'my-gf-class)))))
#-OpenMCL
(100 27.0 17 #+CLISP T)


;; Check that ensure-generic-function supports both :DECLARE (ANSI CL)
;; and :DECLARATIONS (MOP).

(progn
  (ensure-generic-function 'foo113 :declare '((optimize (speed 3))))
  (clos:generic-function-declarations #'foo113))
((OPTIMIZE (SPEED 3)))

(progn
  (ensure-generic-function 'foo114 :declarations '((optimize (speed 3))))
  (clos:generic-function-declarations #'foo114))
((OPTIMIZE (SPEED 3)))


;; Check that ensure-generic-function without :lambda-list argument works.
(progn
  (ensure-generic-function 'foo115)
  (defmethod foo115 (x y) (list x y))
  (foo115 3 4))
(3 4)


;; Check that defclass supports user-defined options.
(progn
  (defclass option-class (standard-class)
    ((option :accessor cl-option :initarg :my-option)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 option-class) (c2 standard-class))
    t)
  (macrolet ((eval-succeeds (form)
               `(not (nth-value 1 (ignore-errors (eval ',form))))))
    (list (eval-succeeds
            (defclass testclass02a ()
              ()
              (:my-option foo)
              (:metaclass option-class)))
          (cl-option (find-class 'testclass02a))
          (eval-succeeds
            (defclass testclass02b ()
              ()
              (:my-option bar)
              (:my-option baz)
              (:metaclass option-class)))
          (eval-succeeds
            (defclass testclass02c ()
              ()
              (:other-option foo)
              (:metaclass option-class))))))
(T (FOO) NIL NIL)


;; Check that defclass supports user-defined slot options.
(progn
  (defclass option-slot-definition (clos:standard-direct-slot-definition)
    ((option :accessor sl-option :initarg :my-option)))
  (defclass option-slot-class (standard-class)
    ())
  (defmethod clos:direct-slot-definition-class ((c option-slot-class) &rest args)
    (declare (ignore args))
    (find-class 'option-slot-definition))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 option-slot-class) (c2 standard-class))
    t)
  (macrolet ((eval-succeeds (form)
               `(not (nth-value 1 (ignore-errors (eval ',form))))))
    (list (eval-succeeds
            (defclass testclass03a ()
              ((x :my-option foo))
              (:metaclass option-slot-class)))
          (sl-option (first (clos:class-direct-slots (find-class 'testclass03a))))
          (eval-succeeds
            (defclass testclass03b ()
              ((x :my-option bar :my-option baz))
              (:metaclass option-slot-class)))
          (sl-option (first (clos:class-direct-slots (find-class 'testclass03b))))
          (eval-succeeds
            (defclass testclass03c ()
              ((x :other-option foo))
              (:metaclass option-slot-class)))
          (eval-succeeds
            (defclass testclass03d ()
              ((x :my-option foo))
              (:my-option bar)
              (:metaclass option-slot-class))))))
(T FOO T (BAR BAZ) NIL NIL)

;; Check that after a class redefinition, new user-defined direct slots
;; have replaced the old direct slots.
(progn
  (defclass extended-slot-definition (clos:standard-direct-slot-definition)
    ((option1 :initarg :option1)
     (option2 :initarg :option2)))
  (defclass extended-slot-class (standard-class)
    ())
  (defmethod clos:direct-slot-definition-class ((c extended-slot-class) &rest args)
    (declare (ignore args))
    (find-class 'extended-slot-definition))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 extended-slot-class) (c2 standard-class))
    t)
  (defclass testclass03e () ((x :option1 bar)) (:metaclass extended-slot-class))
  (defclass testclass03e () ((x :option2 baz)) (:metaclass extended-slot-class))
  (let ((cl (find-class 'testclass03e)))
    (list (length (clos:class-direct-slots cl))
          (slot-boundp (first (clos:class-direct-slots cl)) 'option1)
          (slot-boundp (first (clos:class-direct-slots cl)) 'option2))))
(1 NIL T)


;; Check that in defclass, the default-initargs of the metaclass have
;; precedence over the usual defaults.
(progn
  (defclass testclass51 (standard-class)
    ()
    (:default-initargs
      :documentation "some doc"))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 testclass51) (c2 standard-class))
    t)
  (mapcar #'(lambda (x) (documentation x 'type))
    (list
      (defclass testclass51a () ())
      (defclass testclass51b () ()
        (:metaclass testclass51))
      (defclass testclass51c () ()
        (:documentation "some other doc")
        (:metaclass testclass51)))))
(NIL "some doc" "some other doc")


;; Check that defgeneric supports user-defined options.
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS)
(progn
  (defclass option-generic-function (standard-generic-function)
    ((option :accessor gf-option :initarg :my-option))
    (:metaclass clos:funcallable-standard-class))
  (macrolet ((eval-succeeds (form)
               `(not (nth-value 1 (ignore-errors (eval ',form))))))
    (list (eval-succeeds
            (defgeneric testgf04a (x y)
              (:my-option foo)
              (:generic-function-class option-generic-function)))
          (gf-option #'testgf04a)
          (eval-succeeds
            (defgeneric testgf04b (x y)
              (:my-option bar)
              (:my-option baz)
              (:generic-function-class option-generic-function)))
          (eval-succeeds
            (defgeneric testgf04c (x y)
              (:my-option bar)
              (:other-option baz)
              (:generic-function-class option-generic-function))))))
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS)
(T (FOO) NIL NIL)


;; Check that in defgeneric, the default-initargs of the generic-function-class
;; have precedence over the usual defaults.
(progn
  (defclass testmethod50 (standard-method)
    ())
  (defclass testgenericfunction50 (standard-generic-function)
    ()
    (:default-initargs
      :method-class (find-class 'testmethod50))
    (:metaclass clos:funcallable-standard-class))
  (mapcar #'class-name
    (mapcar #'clos:generic-function-method-class
      (list
        (defgeneric testgf50a (x))
        (defgeneric testgf50b (x)
          (:generic-function-class testgenericfunction50))
        (defgeneric testgf50c (x)
          (:method-class standard-method)
          (:generic-function-class testgenericfunction50))
        (defgeneric testgf50d (x)
          (:method-class testmethod50)
          (:generic-function-class testgenericfunction50))))))
(STANDARD-METHOD TESTMETHOD50 STANDARD-METHOD TESTMETHOD50)
#|
; Same thing with generic-flet.
(progn
  (defclass testmethod51 (standard-method)
    ())
  (defclass testgenericfunction51 (standard-generic-function)
    ()
    (:default-initargs
      :method-class (find-class 'testmethod51))
    (:metaclass clos:funcallable-standard-class))
  (mapcar #'class-name
    (mapcar #'clos:generic-function-method-class
      (list
        (generic-flet ((testgf (x)))
          #'testgf)
        (generic-flet ((testgf (x)
                         (:generic-function-class testgenericfunction51)))
          #'testgf)
        (generic-flet ((testgf (x)
                         (:method-class standard-method)
                         (:generic-function-class testgenericfunction51)))
          #'testgf)
        (generic-flet ((testgf (x)
                         (:method-class testmethod51)
                         (:generic-function-class testgenericfunction51)))
          #'testgf)))))
(STANDARD-METHOD TESTMETHOD50 STANDARD-METHOD TESTMETHOD50)
|#


;; Check dependents notification on classes.
(progn
  (defclass dependent05 () ((counter :initform 0)))
  (defclass testclass05 () ())
  (defmethod clos:update-dependent ((c class) (d dependent05) &rest args)
    (incf (slot-value d 'counter)))
  (let ((testclass (find-class 'testclass05))
        (dep1 (make-instance 'dependent05))
        (dep2 (make-instance 'dependent05))
        (dep3 (make-instance 'dependent05)))
    (clos:add-dependent testclass dep1)
    (clos:add-dependent testclass dep2)
    (clos:add-dependent testclass dep3)
    (clos:add-dependent testclass dep1)
    (reinitialize-instance testclass :name 'testclass05-renamed)
    (clos:remove-dependent testclass dep2)
    (reinitialize-instance testclass :name 'testclass05-rerenamed)
    (list (slot-value dep1 'counter)
          (slot-value dep2 'counter)
          (slot-value dep3 'counter))))
(2 1 2)

;; Check dependents notification on generic functions.
(progn
  (defclass dependent06 () ((history :initform '())))
  (defgeneric testgf06 (x))
  (defmethod clos:update-dependent ((gf generic-function) (d dependent06) &rest args)
    (push args (slot-value d 'history)))
  (let ((testgf #'testgf06)
        (dep1 (make-instance 'dependent06))
        (dep2 (make-instance 'dependent06))
        (dep3 (make-instance 'dependent06)))
    (clos:add-dependent testgf dep1)
    (clos:add-dependent testgf dep2)
    (clos:add-dependent testgf dep3)
    (clos:add-dependent testgf dep1)
    (reinitialize-instance testgf :name 'testgf06-renamed)
    (defmethod testgf06 ((x integer)))
    (clos:remove-dependent testgf dep2)
    (defmethod testgf06 ((x real)))
    (remove-method testgf (find-method testgf '() (list (find-class 'integer))))
    (mapcar #'(lambda (history)
                (mapcar #'(lambda (event)
                            (mapcar #'(lambda (x)
                                        (if (typep x 'method)
                                          (list 'method (mapcar #'class-name (method-specializers x)))
                                          x))
                                    event))
                        history))
            (list (reverse (slot-value dep1 'history))
                  (reverse (slot-value dep2 'history))
                  (reverse (slot-value dep3 'history))))))
(((:name testgf06-renamed) (add-method (method (integer))) (add-method (method (real))) (remove-method (method (integer))))
 ((:name testgf06-renamed) (add-method (method (integer))))
 ((:name testgf06-renamed) (add-method (method (integer))) (add-method (method (real))) (remove-method (method (integer)))))


;;; Check the dependent protocol
;;;   add-dependent remove-dependent map-dependents

(progn
  (defparameter *timestamp* 0)
  (defclass prioritized-dependent ()
    ((priority :type real :initform 0 :initarg :priority :reader dependent-priority)))
  (defclass prioritized-dispatcher ()
    ((dependents :type list :initform nil)))
  (defmethod clos:add-dependent ((metaobject prioritized-dispatcher) (dependent prioritized-dependent))
    (unless (member dependent (slot-value metaobject 'dependents))
      (setf (slot-value metaobject 'dependents)
            (sort (cons dependent (slot-value metaobject 'dependents)) #'>
                  :key #'dependent-priority))))
  (defmethod clos:remove-dependent ((metaobject prioritized-dispatcher) (dependent prioritized-dependent))
    (setf (slot-value metaobject 'dependents)
          (delete dependent (slot-value metaobject 'dependents))))
  (defmethod clos:map-dependents ((metaobject prioritized-dispatcher) function)
    ; Process the dependents list in decreasing priority order.
    (dolist (dependent (slot-value metaobject 'dependents))
      (funcall function dependent)
      (incf *timestamp*)))
  t)
T

;; Check that notification on classes can proceed by priorities.
(progn
  (setq *timestamp* 0)
  (defclass prioritized-class (prioritized-dispatcher standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 prioritized-class) (c2 standard-class))
    t)
  (defclass testclass07 () () (:metaclass prioritized-class))
  (defclass dependent07 (prioritized-dependent) ((history :initform nil)))
  (defmethod clos:update-dependent ((c class) (d dependent07) &rest args)
    (push (cons *timestamp* args) (slot-value d 'history)))
  (let ((testclass (find-class 'testclass07))
        (dep1 (make-instance 'dependent07 :priority 5))
        (dep2 (make-instance 'dependent07 :priority 10))
        (dep3 (make-instance 'dependent07 :priority 1)))
    (clos:add-dependent testclass dep1)
    (clos:add-dependent testclass dep2)
    (clos:add-dependent testclass dep3)
    (clos:add-dependent testclass dep1)
    (reinitialize-instance testclass :name 'testclass07-renamed)
    (clos:remove-dependent testclass dep2)
    (reinitialize-instance testclass :name 'testclass07-rerenamed)
    (list (reverse (slot-value dep1 'history))
          (reverse (slot-value dep2 'history))
          (reverse (slot-value dep3 'history)))))
(((1 :name testclass07-renamed) (3 :name testclass07-rerenamed))
 ((0 :name testclass07-renamed))
 ((2 :name testclass07-renamed) (4 :name testclass07-rerenamed)))

;; Check that notification on generic-functions can proceed by priorities.
(progn
  (setq *timestamp* 0)
  (defclass prioritized-generic-function (prioritized-dispatcher standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defgeneric testgf08 (x) (:generic-function-class prioritized-generic-function))
  (defclass dependent08 (prioritized-dependent) ((history :initform '())))
  (defmethod clos:update-dependent ((gf generic-function) (d dependent08) &rest args)
    (push (cons *timestamp* args) (slot-value d 'history)))
  (let ((testgf #'testgf08)
        (dep1 (make-instance 'dependent08 :priority 1))
        (dep2 (make-instance 'dependent08 :priority 10))
        (dep3 (make-instance 'dependent08 :priority 5)))
    (clos:add-dependent testgf dep1)
    (clos:add-dependent testgf dep2)
    (clos:add-dependent testgf dep3)
    (clos:add-dependent testgf dep1)
    (reinitialize-instance testgf :name 'testgf08-renamed)
    (defmethod testgf08 ((x integer)))
    (clos:remove-dependent testgf dep2)
    (defmethod testgf08 ((x real)))
    (remove-method testgf (find-method testgf '() (list (find-class 'integer))))
    (mapcar #'(lambda (history)
                (mapcar #'(lambda (event)
                            (mapcar #'(lambda (x)
                                        (if (typep x 'method)
                                          (list 'method (mapcar #'class-name (method-specializers x)))
                                          x))
                                    event))
                        history))
            (list (reverse (slot-value dep1 'history))
                  (reverse (slot-value dep2 'history))
                  (reverse (slot-value dep3 'history))))))
(((2 :name testgf08-renamed) (5 add-method (method (integer))) (7 add-method (method (real))) (9 remove-method (method (integer))))
 ((0 :name testgf08-renamed) (3 add-method (method (integer))))
 ((1 :name testgf08-renamed) (4 add-method (method (integer))) (6 add-method (method (real))) (8 remove-method (method (integer)))))


;;; Check the direct-methods protocol
;;;   add-direct-method remove-direct-method
;;;   specializer-direct-generic-functions specializer-direct-methods

;; Check that it's possible to avoid storing all trivially specialized methods.
;; We can do this since the class <t> will never change.
(let ((<t> (find-class 't))
      (operation-counter 0))
  (defmethod clos:add-direct-method ((specializer (eql <t>)) (method method))
    (incf operation-counter))
  (defmethod clos:remove-direct-method ((specializer (eql <t>)) (method method))
    (incf operation-counter))
  (defmethod clos:specializer-direct-generic-functions ((class (eql <t>)))
    '())
  (defmethod clos:specializer-direct-methods ((class (eql <t>)))
    '())
  (setq operation-counter 0)
  ;; Note that add-direct-method is called once for each specializer of the
  ;; new method; since it has three times the specializer <t>, add-direct-method
  ;; is called three times.
  (fmakunbound 'testgf09)
  (defmethod testgf09 (x y z) (+ x y z))
  (list (null (clos:specializer-direct-generic-functions (find-class 't)))
        (null (clos:specializer-direct-methods (find-class 't)))
        operation-counter))
(t t 3)

;; Check that redefinition of a generic function correctly updates the lists
;; of generic functions belonging to each specializer.
(progn
  (defgeneric foo142 (x) (:method ((x t))))
  (defgeneric foo142 (x))
  (null (member #'foo142
                (clos:specializer-direct-generic-functions (find-class 't)))))
T


;;; Check the direct-subclasses protocol
;;;   add-direct-subclass remove-direct-subclass class-direct-subclasses

;; Check that it's possible to count only instantiated direct subclasses.
;; (Subclasses that have no instances yet can be treated like garbage-collected
;; subclasses and be ignored.)
(progn
  (defclass volatile-class (standard-class)
    ((instantiated :type boolean :initform nil)))
  (defparameter *volatile-class-hack* nil)
  (defmethod clos:add-direct-subclass :around ((superclass volatile-class) (subclass volatile-class))
    (when *volatile-class-hack* (call-next-method)))
  (defmethod clos:remove-direct-subclass :around ((superclass volatile-class) (subclass volatile-class))
    nil)
  (defun note-volatile-class-instantiated (class)
    (unless (slot-value class 'instantiated)
      (setf (slot-value class 'instantiated) t)
      (dolist (superclass (clos:class-direct-superclasses class))
        (when (typep superclass 'volatile-class)
          (unless (member class (clos:class-direct-subclasses superclass))
            (let ((*volatile-class-hack* t))
              (clos:add-direct-subclass superclass class))
            (note-volatile-class-instantiated superclass))))))
  (defmethod allocate-instance :after ((class volatile-class) &rest initargs)
    (note-volatile-class-instantiated class))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 volatile-class) (c2 standard-class))
    t)
  (defclass testclass10 () () (:metaclass volatile-class))
  (defclass testclass10a (testclass10) () (:metaclass volatile-class))
  (defclass testclass10b (testclass10) () (:metaclass volatile-class))
  (defclass testclass10c (testclass10) () (:metaclass volatile-class))
  (defclass testclass10d (testclass10b) () (:metaclass volatile-class))
  (let ((results '()))
    (push (clos:class-direct-subclasses (find-class 'testclass10)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10a)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10b)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10c)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10d)) results)
    (make-instance 'testclass10d)
    (push (clos:class-direct-subclasses (find-class 'testclass10)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10a)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10b)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10c)) results)
    (push (clos:class-direct-subclasses (find-class 'testclass10d)) results)
    (mapcar #'(lambda (l) (mapcar #'class-name l)) (nreverse results))))
(() () () () ()
 (testclass10b) () (testclass10d) () ())


;;; Check the compute-applicable-methods protocol
;;;   compute-applicable-methods compute-applicable-methods-using-classes

;; Check that it's possible to change the order of applicable methods from
;; most-specific-first to most-specific-last.
(progn
  (defclass msl-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defun reverse-method-list (methods)
    (let ((result '()))
      (dolist (method methods)
        (if (and (consp result)
                 (equal (method-qualifiers method) (method-qualifiers (caar result))))
          (push method (car result))
          (push (list method) result)))
      (reduce #'append result)))
  (defmethod compute-applicable-methods ((gf msl-generic-function) arguments)
    (reverse-method-list (call-next-method)))
  #-LISPWORKS
  (defmethod clos:compute-applicable-methods-using-classes ((gf msl-generic-function) classes)
    (reverse-method-list (call-next-method)))
  (defgeneric testgf11 (x) (:generic-function-class msl-generic-function)
    (:method ((x integer)) (cons 'integer (if (next-method-p) (call-next-method))))
    (:method ((x real)) (cons 'real (if (next-method-p) (call-next-method))))
    (:method ((x number)) (cons 'number (if (next-method-p) (call-next-method))))
    (:method :around ((x integer)) (coerce (call-next-method) 'vector)))
  (list (testgf11 5.0) (testgf11 17)))
((number real) #(number real integer))

;; Check that it's possible to filter-out applicable methods.
(progn
  (defclass nonumber-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defun nonumber-method-list (methods)
    (remove-if #'(lambda (method)
                   (member (find-class 'number) (clos:method-specializers method)))
               methods))
  (defmethod compute-applicable-methods ((gf nonumber-generic-function) arguments)
    (nonumber-method-list (call-next-method)))
  #-LISPWORKS
  (defmethod clos:compute-applicable-methods-using-classes ((gf nonumber-generic-function) classes)
    (nonumber-method-list (call-next-method)))
  (defgeneric testgf12 (x) (:generic-function-class nonumber-generic-function)
    (:method ((x integer)) (cons 'integer (if (next-method-p) (call-next-method))))
    (:method ((x real)) (cons 'real (if (next-method-p) (call-next-method))))
    (:method ((x number)) (cons 'number (if (next-method-p) (call-next-method))))
    (:method :around ((x integer)) (coerce (call-next-method) 'vector)))
  (list (testgf12 5.0) (testgf12 17)))
((real) #(integer real))


;;; Check the compute-class-precedence-list protocol
;;;   compute-class-precedence-list

;; Check that it's possible to compute the precedence list using a
;; breadth-first search instead of a depth-first search.
(progn
  (defclass bfs-class (standard-class)
    ())
  (defmethod clos:compute-class-precedence-list ((class bfs-class))
    (let ((queue (list class))
          (next-queue '())
          (cpl '()))
      (loop
        (when (null queue)
          (setq queue (reverse next-queue) next-queue '())
          (when (null queue)
            (return)))
        (let ((c (pop queue)))
          (unless (member c cpl)
            (push c cpl)
            (setq next-queue (revappend (clos:class-direct-superclasses c) next-queue)))))
      (nreverse cpl)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 bfs-class) (c2 standard-class))
    t)
  ;          a
  ;        /   \
  ;      b       d
  ;      |       |
  ;      c       e
  ;        \   /
  ;          f
  (defclass testclass13a () () (:metaclass bfs-class))
  (defclass testclass13b (testclass13a) () (:metaclass bfs-class))
  (defclass testclass13c (testclass13b) () (:metaclass bfs-class))
  (defclass testclass13d (testclass13a) () (:metaclass bfs-class))
  (defclass testclass13e (testclass13d) () (:metaclass bfs-class))
  (defclass testclass13f (testclass13c testclass13e) () (:metaclass bfs-class))
  (unless (clos:class-finalized-p (find-class 'testclass13f))
    (clos:finalize-inheritance (find-class 'testclass13f)))
  (mapcar #'class-name (subseq (clos:class-precedence-list (find-class 'testclass13f)) 0 6)))
;; With the default depth-first / topological-sort search algorithm:
;; (testclass13f testclass13c testclass13b testclass13e testclass13d testclass13a)
(testclass13f testclass13c testclass13e testclass13b testclass13d testclass13a)


;;; Check the compute-default-initargs protocol
;;;   compute-default-initargs

;; Check that it's possible to add additional initargs.
(progn
  (defparameter *extra-value* 'extra)
  (defclass custom-default-initargs-class (standard-class)
    ())
  (defmethod clos:compute-default-initargs ((class custom-default-initargs-class))
    (let ((original-default-initargs
            (remove-duplicates
              (reduce #'append
                      (mapcar #'clos:class-direct-default-initargs
                              (clos:class-precedence-list class)))
              :key #'car
              :from-end t)))
      (cons (list ':extra '*extra-value* #'(lambda () *extra-value*))
            (remove ':extra original-default-initargs :key #'car))))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 custom-default-initargs-class) (c2 standard-class))
    t)
  (defclass testclass14 () ((slot :initarg :extra)) (:metaclass custom-default-initargs-class))
  (slot-value (make-instance 'testclass14) 'slot))
EXTRA


;;; Check the compute-direct-slot-definition-initargs protocol
;;;   compute-direct-slot-definition-initargs

;; Check that it's possible to generate accessors automatically.
#+CLISP
(progn
  (defclass auto-accessors-2-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 auto-accessors-2-class) (c2 standard-class))
    t)
  (defmethod clos::compute-direct-slot-definition-initargs ((class auto-accessors-2-class) &rest slot-spec)
    (if (and (null (getf slot-spec ':readers)) (null (getf slot-spec ':writers)))
      (let* ((containing-class-name (class-name class))
             (accessor-name
               (intern (concatenate 'string
                                    (symbol-name containing-class-name)
                                    "-"
                                    (symbol-name (getf slot-spec ':name)))
                       (symbol-package containing-class-name))))
        (list* ;; Here are the additional reader/writer lists.
               :readers (list accessor-name)
               :writers (list (list 'setf accessor-name))
               (call-next-method)))
      (call-next-method)))
  (defclass testclass15 ()
    ((x :initarg :x) (y))
    (:metaclass auto-accessors-2-class))
  (let ((inst (make-instance 'testclass15 :x 12)))
    (list (testclass15-x inst) (setf (testclass15-y inst) 13))))
#+CLISP
(12 13)


;;; Check the compute-discriminating-function protocol
;;;   compute-discriminating-function

;; Check that it's possible to add tracing to a generic function.
(progn
  (defclass traced-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defvar *last-traced-arguments* nil)
  (defvar *last-traced-values* nil)
  (defmethod clos:compute-discriminating-function ((gf traced-generic-function))
    (let ((orig-df (call-next-method))
          (name (clos:generic-function-name gf)))
      #'(lambda (&rest arguments)
          (declare (compile))
          (format *trace-output* "~%=> ~S arguments: ~:S" name arguments)
          (setq *last-traced-arguments* arguments)
          (let ((values (multiple-value-list (apply orig-df arguments))))
            (format *trace-output* "~%<= ~S values: ~:S" name values)
            (setq *last-traced-values* values)
            (values-list values)))))
  (defgeneric testgf15 (x) (:generic-function-class traced-generic-function)
     (:method ((x number)) (values x (- x) (* x x) (/ x))))
  (testgf15 5)
  (list *last-traced-arguments* *last-traced-values*))
((5) (5 -5 25 1/5))


;;; Check the compute-effective-method protocol
;;;   compute-effective-method

;; Check that it is possible to modify the effective-method in a way that is
;; orthogonal to the method-combination. In particular, check that it's
;; possible to provide 'redo' and 'return' restarts for each method invocation.
(progn
  (defun prompt-for-new-values ()
    (format *debug-io* "~&New values: ")
    (list (read *debug-io*)))
  (defun add-method-restarts (form method)
    (let ((block (gensym))
          (tag (gensym)))
      `(BLOCK ,block
         (TAGBODY
           ,tag
           (RETURN-FROM ,block
             (RESTART-CASE ,form
               (METHOD-REDO ()
                 :REPORT (LAMBDA (STREAM) (FORMAT STREAM "Try calling ~S again." ,method))
                 (GO ,tag))
               (METHOD-RETURN (L)
                 :REPORT (LAMBDA (STREAM) (FORMAT STREAM "Specify return values for ~S call." ,method))
                 :INTERACTIVE (LAMBDA () (PROMPT-FOR-NEW-VALUES))
                 (RETURN-FROM ,block (VALUES-LIST L)))))))))
  (defun convert-effective-method (efm)
    (if (consp efm)
      (if (eq (car efm) 'CALL-METHOD)
        (let ((method-list (third efm)))
          (if (or (typep (first method-list) 'method) (rest method-list))
            ; Reduce the case of multiple methods to a single one.
            ; Make the call to the next-method explicit.
            (convert-effective-method
              `(CALL-METHOD ,(second efm)
                 ((MAKE-METHOD
                    (CALL-METHOD ,(first method-list) ,(rest method-list))))))
            ; Now the case of at most one method.
            (if (typep (second efm) 'method)
              ; Wrap the method call in a RESTART-CASE.
              (add-method-restarts
                (cons (convert-effective-method (car efm))
                      (convert-effective-method (cdr efm)))
                (second efm))
              ; Normal recursive processing.
              (cons (convert-effective-method (car efm))
                    (convert-effective-method (cdr efm))))))
        (cons (convert-effective-method (car efm))
              (convert-effective-method (cdr efm))))
      efm))
  (defclass debuggable-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defmethod clos:compute-effective-method ((gf debuggable-generic-function) method-combination methods)
    (convert-effective-method (call-next-method)))
  (defgeneric testgf16 (x) (:generic-function-class debuggable-generic-function))
  (defclass testclass16a () ())
  (defclass testclass16b (testclass16a) ())
  (defclass testclass16c (testclass16a) ())
  (defclass testclass16d (testclass16b testclass16c) ())
  (defmethod testgf16 ((x testclass16a))
    (list 'a
          (not (null (find-restart 'method-redo)))
          (not (null (find-restart 'method-return)))))
  (defmethod testgf16 ((x testclass16b))
    (cons 'b (call-next-method)))
  (defmethod testgf16 ((x testclass16c))
    (cons 'c (call-next-method)))
  (defmethod testgf16 ((x testclass16d))
    (cons 'd (call-next-method)))
  (testgf16 (make-instance 'testclass16d)))
(D B C A T T)


;;; Check the compute-effective-slot-definition protocol
;;;   compute-effective-slot-definition

;; Check that it's possible to generate initargs automatically and have a
;; default initform of 42.
#-(or ALLEGRO OpenMCL LISPWORKS)
(progn
  (defclass auto-initargs-class (standard-class)
    ())
  (defmethod clos:compute-effective-slot-definition ((class auto-initargs-class) name direct-slot-definitions)
    (let ((eff-slot (call-next-method)))
      ;; NB: The MOP doesn't specify setters for slot-definition objects, but
      ;; most implementations have it. Without these setters, it is not possible
      ;; to make use of compute-effective-slot-definition, since the MOP p. 43
      ;; says "the value returned by the extending method must be the value
      ;; returned by [the predefined] method".
      (unless (clos:slot-definition-initargs eff-slot)
        (setf (clos:slot-definition-initargs eff-slot)
              (list (intern (symbol-name (clos:slot-definition-name eff-slot))
                            (find-package "KEYWORD")))))
      (unless (clos:slot-definition-initfunction eff-slot)
        (setf (clos:slot-definition-initfunction eff-slot) #'(lambda () 42)
              (clos:slot-definition-initform eff-slot) '42))
      eff-slot))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 auto-initargs-class) (c2 standard-class))
    t)
  (defclass testclass17 () ((x) (y)) (:metaclass auto-initargs-class))
  (let ((inst (make-instance 'testclass17 :x 17)))
    (list (slot-value inst 'x) (slot-value inst 'y))))
#-(or ALLEGRO OpenMCL LISPWORKS)
(17 42)


;;; Check the compute-effective-slot-definition-initargs protocol
;;;   compute-effective-slot-definition-initargs

;; Check that it's possible to generate initargs automatically and have a
;; default initform of 42.
#+(or CLISP ALLEGRO CMU SBCL LISPWORKS)
(progn
  (defclass auto-initargs-2-class (standard-class)
    ())
  (defmethod clos:compute-effective-slot-definition-initargs ((class auto-initargs-2-class) #+LISPWORKS name direct-slot-definitions)
    (let ((initargs (call-next-method)))
      (unless (getf initargs ':initargs)
        (setq initargs
              (list* ':initargs
                     (list (intern (symbol-name (getf initargs ':name))
                                   (find-package "KEYWORD")))
                     initargs)))
      (unless (getf initargs ':initfunction)
        (setq initargs
              (list* ':initfunction #'(lambda () 42)
                     ':initform '42
                     initargs)))
      initargs))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 auto-initargs-2-class) (c2 standard-class))
    t)
  (defclass testclass17-2 () ((x) (y)) (:metaclass auto-initargs-2-class))
  (let ((inst (make-instance 'testclass17-2 :x 17)))
    (list (slot-value inst 'x) (slot-value inst 'y))))
#+(or CLISP ALLEGRO CMU SBCL LISPWORKS)
(17 42)


;;; Check the compute-slots protocol
;;;   compute-slots

;; Check that it's possible to add additional local slots.
(progn
  (defclass testclass18b (testclass18a) ())
  (defmethod clos:compute-slots ((class (eql (find-class 'testclass18b))))
    (append (call-next-method)
            (list (make-instance 'clos:standard-effective-slot-definition
                    :name 'y
                    :allocation :instance))))
  (defclass testclass18a ()
    ((x :allocation :class)))
  (clos:finalize-inheritance (find-class 'testclass18b))
  ;; testclass18b should now have a shared slot, X, and a local slot, Y.
  (append
    (mapcar #'(lambda (slot)
                (list (clos:slot-definition-name slot)
                      (integerp (clos:slot-definition-location slot))))
            (clos:class-slots (find-class 'testclass18b)))
    (let ((inst1 (make-instance 'testclass18b))
          (inst2 (make-instance 'testclass18b)))
      (setf (slot-value inst1 'y) 'abc)
      (setf (slot-value inst2 'y) 'def)
      (list (slot-value inst1 'y) (slot-value inst2 'y)))))
((X NIL) (Y T) ABC DEF)

;; Check that it's possible to add additional shared slots.
(progn
  (defclass testclass19b (testclass19a) ())
  (defmethod clos:compute-slots ((class (eql (find-class 'testclass19b))))
    (append (call-next-method)
            (list (make-instance 'clos:standard-effective-slot-definition
                    :name 'y
                    :allocation :class))))
  (defclass testclass19a ()
    ((x :allocation :class)))
  (clos:finalize-inheritance (find-class 'testclass19b))
  ;; testclass19b should now have two shared slots, X and Y.
  (append
    (mapcar #'(lambda (slot)
                (list (clos:slot-definition-name slot)
                      (integerp (clos:slot-definition-location slot))))
            (clos:class-slots (find-class 'testclass19b)))
    (let ((inst1 (make-instance 'testclass19b))
          (inst2 (make-instance 'testclass19b)))
      (setf (slot-value inst1 'y) 'abc)
      (setf (slot-value inst2 'y) 'def)
      (list (slot-value inst1 'y) (slot-value inst2 'y)))))
((X NIL) (Y NIL) DEF DEF)


;;; Check the direct-slot-definition-class protocol
;;;   direct-slot-definition-class

;; Check that it's possible to generate accessors automatically.
(progn
  (defclass auto-accessors-direct-slot-definition-class (standard-class)
    ((containing-class-name :initarg :containing-class-name)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 auto-accessors-direct-slot-definition-class) (c2 standard-class))
    t)
  (defclass auto-accessors-class (standard-class)
    ())
  (defmethod clos:direct-slot-definition-class ((class auto-accessors-class) &rest initargs)
    (let ((dsd-class-name (gensym)))
      (clos:ensure-class dsd-class-name
        :metaclass (find-class 'auto-accessors-direct-slot-definition-class)
        :direct-superclasses (list (find-class 'clos:standard-direct-slot-definition))
        :containing-class-name (class-name class))
      (eval `(defmethod initialize-instance :around ((dsd ,dsd-class-name) &rest args)
               (if (and (null (getf args ':readers)) (null (getf args ':writers)))
                 (let* ((containing-class-name (slot-value (class-of dsd) 'containing-class-name))
                        (accessor-name
                          (intern (concatenate 'string
                                               (symbol-name containing-class-name)
                                               "-"
                                               (symbol-name (getf args ':name)))
                                  (symbol-package containing-class-name))))
                   (apply #'call-next-method dsd
                          :readers (list accessor-name)
                          :writers (list (list 'setf accessor-name))
                          args))
                 (call-next-method))))
      (find-class dsd-class-name)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 auto-accessors-class) (c2 standard-class))
    t)
  (defclass testclass20 ()
    ((x :initarg :x) (y))
    (:metaclass auto-accessors-class))
  (let ((inst (make-instance 'testclass20 :x 12)))
    (list (testclass20-x inst) (setf (testclass20-y inst) 13))))
(12 13)


;;; Check the effective-slot-definition-class protocol
;;;   effective-slot-definition-class

;; See below, with the slot-value-using-class protocol.


;;; Check the slot-value-using-class protocol
;;;   slot-value-using-class (setf slot-value-using-class)
;;;   slot-boundp-using-class slot-makunbound-using-class

;; Check that it's possible to store all slot values in property lists.
(progn
  (defparameter *external-slot-values* '())
  (defclass external-slot-definition (clos:standard-effective-slot-definition)
    ())
  (let ((unbound (gensym "UNBOUND")))
    (defmethod clos:slot-value-using-class ((class standard-class) instance (slot external-slot-definition))
      (let ((value (getf (getf *external-slot-values* instance) (clos:slot-definition-name slot) unbound)))
        (if (eq value unbound)
          (slot-unbound class instance (clos:slot-definition-name slot))
          value)))
    (defmethod (setf clos:slot-value-using-class) (new-value (class standard-class) instance (slot external-slot-definition))
      (setf (getf (getf *external-slot-values* instance) (clos:slot-definition-name slot)) new-value))
    (defmethod clos:slot-boundp-using-class ((class standard-class) instance (slot external-slot-definition))
      (let ((value (getf (getf *external-slot-values* instance) (clos:slot-definition-name slot) unbound)))
        (not (eq value unbound))))
    (defmethod clos:slot-makunbound-using-class ((class standard-class) instance (slot external-slot-definition))
      (remf (getf *external-slot-values* instance) (clos:slot-definition-name slot))
      instance))
  (defclass external-slot-definition-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 external-slot-definition-class) (c2 standard-class))
    t)
  (defmethod clos:effective-slot-definition-class ((class external-slot-definition-class) &rest args)
    (find-class 'external-slot-definition))
  (defclass testclass22 ()
    ((x :initarg :x) (y :initarg :y))
    (:metaclass external-slot-definition-class))
  (let ((inst1 (make-instance 'testclass22 :x 3 :y 4))
        (inst2 (make-instance 'testclass22 :x 5 :y 12))
        (results '()))
    (push (slot-value inst1 'x) results)
    (push (slot-value inst2 'x) results)
    (push (slot-value inst1 'y) results)
    (push (slot-value inst2 'y) results)
    (push (or (equal *external-slot-values*
                     (list inst2 (list 'x 5 'y 12) inst1 (list 'x 3 'y 4)))
              (equal *external-slot-values*
                     (list inst2 (list 'y 12 'x 5) inst1 (list 'y 4 'x 3))))
          results)
    (setf (slot-value inst2 'x) -5)
    (push (slot-value inst2 'x) results)
    (slot-makunbound inst1 'y)
    (push (list (slot-boundp inst1 'x) (slot-boundp inst1 'y)) results)
    (slot-makunbound inst1 'x)
    (push (or (equal *external-slot-values*
                     (list inst2 (list 'x -5 'y 12) inst1 nil))
              (equal *external-slot-values*
                     (list inst2 (list 'y 12 'x -5) inst1 nil)))
          results)
    (nreverse results)))
(3 5 4 12 T -5 (T NIL) T)


;;; Check the ensure-class-using-class protocol
;;;   ensure-class-using-class

;; Check that it's possible to take the documentation from elsewhere.
(progn
  (defparameter *doc-database*
    '((testclass23 . "This is a dumb class for testing.")
      (testgf24 . "This is a dumb generic function for testing.")))
  (defclass externally-documented-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 externally-documented-class) (c2 standard-class))
    t)
  (dolist (given-name (mapcar #'car *doc-database*))
    (defmethod clos:ensure-class-using-class ((class class) (name (eql given-name)) &rest args &key documentation &allow-other-keys)
      (if (and (null documentation)
               (setq documentation (cdr (assoc name *doc-database*))))
        (apply #'call-next-method class name (list* ':documentation documentation args))
        (call-next-method)))
    (defmethod clos:ensure-class-using-class ((class null) (name (eql given-name)) &rest args &key documentation &allow-other-keys)
      (if (and (null documentation)
               (setq documentation (cdr (assoc name *doc-database*))))
        (apply #'call-next-method class name (list* ':documentation documentation args))
        (call-next-method))))
  (defclass testclass23 ()
    ()
    (:metaclass externally-documented-class))
  (documentation 'testclass23 'type))
"This is a dumb class for testing."


;;; Check the ensure-generic-function-using-class protocol
;;;   ensure-generic-function-using-class

;; Check that it's possible to take the documentation from elsewhere.
(progn
  (defparameter *doc-database*
    '((testclass23 . "This is a dumb class for testing.")
      (testgf24 . "This is a dumb generic function for testing.")))
  (defclass externally-documented-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (dolist (given-name (mapcar #'car *doc-database*))
    (defmethod clos:ensure-generic-function-using-class ((gf generic-function) (name (eql given-name)) &rest args &key documentation &allow-other-keys)
      (if (and (null documentation)
               (setq documentation (cdr (assoc name *doc-database* :test #'equal))))
        (apply #'call-next-method gf name (list* ':documentation documentation args))
        (call-next-method)))
    (defmethod clos:ensure-generic-function-using-class ((gf null) (name (eql given-name)) &rest args &key documentation &allow-other-keys)
      (if (and (null documentation)
               (setq documentation (cdr (assoc name *doc-database* :test #'equal))))
        (apply #'call-next-method gf name (list* ':documentation documentation args))
        (call-next-method))))
  (defgeneric testgf24 (x)
    (:generic-function-class externally-documented-generic-function))
  (documentation 'testgf24 'function))
"This is a dumb generic function for testing."


;;; Check the reader-method-class protocol
;;;   reader-method-class

;; Check that it's possible to define reader methods that do typechecking.
(progn
  (defclass typechecking-reader-method (clos:standard-reader-method)
    ())
  (defmethod initialize-instance ((method typechecking-reader-method) &rest initargs
                                  &key slot-definition)
    (let ((name (clos:slot-definition-name slot-definition))
          (type (clos:slot-definition-type slot-definition)))
      (apply #'call-next-method method
             :function #'(lambda (args next-methods)
                           (declare (ignore next-methods))
                           #+CLISP (declare (compile))
                           (apply #'(lambda (instance)
                                      (let ((value (slot-value instance name)))
                                        (unless (typep value type)
                                          (error "Slot ~S of ~S is not of type ~S: ~S"
                                                 name instance type value))
                                        value))
                                  args))
             initargs)))
  (defclass typechecking-reader-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 typechecking-reader-class) (c2 standard-class))
    t)
  (defmethod reader-method-class ((class typechecking-reader-class) direct-slot &rest args)
    (find-class 'typechecking-reader-method))
  (defclass testclass25 ()
    ((pair :type (cons symbol (cons symbol null)) :initarg :pair :accessor testclass25-pair))
    (:metaclass typechecking-reader-class))
  (macrolet ((succeeds (form)
               `(not (nth-value 1 (ignore-errors ,form)))))
    (let ((p (list 'abc 'def))
          (x (make-instance 'testclass25)))
      (list (succeeds (make-instance 'testclass25 :pair '(seventeen 17)))
            (succeeds (setf (testclass25-pair x) p))
            (succeeds (setf (second p) 456))
            (succeeds (testclass25-pair x))
            (succeeds (slot-value x 'pair))))))
(t t t nil t)


;;; Check the writer-method-class protocol
;;;   writer-method-class

;; Check that it's possible to define writer methods that do typechecking.
(progn
  (defclass typechecking-writer-method (clos:standard-writer-method)
    ())
  (defmethod initialize-instance ((method typechecking-writer-method) &rest initargs
                                  &key slot-definition)
    (let ((name (clos:slot-definition-name slot-definition))
          (type (clos:slot-definition-type slot-definition)))
      (apply #'call-next-method method
             :function #'(lambda (args next-methods)
                           (declare (ignore next-methods))
                           #+CLISP (declare (compile))
                           (apply #'(lambda (new-value instance)
                                      (unless (typep new-value type)
                                        (error "Slot ~S of ~S: new value is not of type ~S: ~S"
                                               name instance type new-value))
                                      (setf (slot-value instance name) new-value))
                                  args))
             initargs)))
  (defclass typechecking-writer-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 typechecking-writer-class) (c2 standard-class))
    t)
  (defmethod writer-method-class ((class typechecking-writer-class) direct-slot &rest args)
    (find-class 'typechecking-writer-method))
  (defclass testclass26 ()
    ((pair :type (cons symbol (cons symbol null)) :initarg :pair :accessor testclass26-pair))
    (:metaclass typechecking-writer-class))
  (macrolet ((succeeds (form)
               `(not (nth-value 1 (ignore-errors ,form)))))
    (let ((p (list 'abc 'def))
          (x (make-instance 'testclass26)))
      (list (succeeds (make-instance 'testclass26 :pair '(seventeen 17)))
            (succeeds (setf (testclass26-pair x) p))
            (succeeds (setf (second p) 456))
            (succeeds (testclass26-pair x))
            (succeeds (setf (testclass26-pair x) p))
            (succeeds (setf (slot-value x 'pair) p))))))
(t t t t nil t)


;;; Check the validate-superclass protocol
;;;   validate-superclass

;; Check that it's possible to create subclasses of generic-function
;; that are not instances of funcallable-standard-class.
(progn
  (defmethod clos:validate-superclass ((c1 standard-class) (c2 clos:funcallable-standard-class))
    t)
  (defclass uncallable-generic-function (standard-generic-function)
    ()
    (:metaclass standard-class))
  (let ((inst (make-instance 'uncallable-generic-function
                :name 'testgf27
                :lambda-list '(x y)
                :method-class (find-class 'standard-method)
                :method-combination (clos:find-method-combination #'print-object 'standard nil))))
    (list (typep inst 'standard-object)
          (typep inst 'clos:funcallable-standard-object)
          (typep (class-of inst) 'standard-class)
          (typep (class-of inst) 'clos:funcallable-standard-class))))
#+(or CLISP ALLEGRO) ERROR
#-(or CLISP ALLEGRO) (T T T NIL)

;; Check that it's possible to create uncounted subclasses of counted classes.
(progn
  (defparameter *counter27* 0)
  (defclass counted27-class (standard-class)
    ())
  (defmethod make-instance :after ((c counted27-class) &rest args)
    (incf *counter27*))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 counted27-class) (c2 standard-class))
    t)
  (defclass testclass27a () () (:metaclass counted27-class))
  (make-instance 'testclass27a)
  (defmethod clos:validate-superclass ((c1 standard-class) (c2 counted27-class))
    t)
  (defclass testclass27b (testclass27a) () (:metaclass standard-class))
  (make-instance 'testclass27b)
  (make-instance 'testclass27b)
  *counter27*)
1


;;; Check that finalize-inheritance is called when it should be.
(let ((finalize-inheritance-history '()))
  (ext:without-package-lock ("CLOS")
    (defmethod clos:finalize-inheritance :after ((class class))
      (push (class-name class) finalize-inheritance-history)))
  (defclass testclass52a () ())
  (defclass testclass52c (testclass52a testclass52b) ())
  (defclass testclass52d (testclass52c) ())
  (defclass testclass52b () ())
  (make-instance 'testclass52d)
  (prog1
    finalize-inheritance-history
    (remove-method #'clos:finalize-inheritance
      (find-method #'clos:finalize-inheritance '(:after) (list (find-class 'class))))))
(TESTCLASS52D TESTCLASS52C TESTCLASS52B TESTCLASS52A)


;;; Check that extending many MOP generic functions is possible, however
;;; overriding methods of these MOP generic functions is forbidden.

;; Check class-default-initargs.
(let ((*sampclass* (defclass sampclass01 () ())))
  (defmethod clos:class-default-initargs ((c (eql *sampclass*)))
    (call-next-method))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (clos:class-default-initargs *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass02 () ())))
  (let ((badmethod
          (defmethod clos:class-default-initargs ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unless (clos:class-finalized-p *sampclass*)
      (clos:finalize-inheritance *sampclass*))
    (unwind-protect
      (nth-value 1 (clos:class-default-initargs *sampclass*))
      (remove-method #'clos:class-default-initargs badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-direct-default-initargs.
(let ((*sampclass* (defclass sampclass03 () ())))
  (defmethod clos:class-direct-default-initargs ((c (eql *sampclass*)))
    (call-next-method))
  (clos:class-direct-default-initargs *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass04 () ())))
  (let ((badmethod
          (defmethod clos:class-direct-default-initargs ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:class-direct-default-initargs *sampclass*))
      (remove-method #'clos:class-direct-default-initargs badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-direct-slots.
(let ((*sampclass* (defclass sampclass05 () ())))
  (defmethod clos:class-direct-slots ((c (eql *sampclass*)))
    (call-next-method))
  (clos:class-direct-slots *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass06 () ())))
  (let ((badmethod
          (defmethod clos:class-direct-slots ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:class-direct-slots *sampclass*))
      (remove-method #'clos:class-direct-slots badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-direct-superclasses.
(let ((*sampclass* (defclass sampclass07 () ())))
  (defmethod clos:class-direct-superclasses ((c (eql *sampclass*)))
    (call-next-method))
  (clos:class-direct-superclasses *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass08 () ())))
  (let ((badmethod
          (defmethod clos:class-direct-superclasses ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:class-direct-superclasses *sampclass*))
      (remove-method #'clos:class-direct-superclasses badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-finalized-p.
(let ((*sampclass* (defclass sampclass09 () ())))
  (defmethod clos:class-finalized-p ((c (eql *sampclass*)))
    (call-next-method))
  (clos:class-finalized-p *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass10 () ())))
  (let ((badmethod
          (defmethod clos:class-finalized-p ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:class-finalized-p *sampclass*))
      (remove-method #'clos:class-finalized-p badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-precedence-list.
(let ((*sampclass* (defclass sampclass11 () ())))
  (defmethod clos:class-precedence-list ((c (eql *sampclass*)))
    (call-next-method))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (clos:class-precedence-list *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass12 () ())))
  (let ((badmethod
          (defmethod clos:class-precedence-list ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unless (clos:class-finalized-p *sampclass*)
      (clos:finalize-inheritance *sampclass*))
    (unwind-protect
      (nth-value 1 (clos:class-precedence-list *sampclass*))
      (remove-method #'clos:class-precedence-list badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-prototype.
(let ((*sampclass* (defclass sampclass13 () ())))
  (defmethod clos:class-prototype ((c (eql *sampclass*)))
    (call-next-method))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (clos:class-prototype *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass14 () ())))
  (let ((badmethod
          (defmethod clos:class-prototype ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unless (clos:class-finalized-p *sampclass*)
      (clos:finalize-inheritance *sampclass*))
    (unwind-protect
      (nth-value 1 (clos:class-prototype *sampclass*))
      (remove-method #'clos:class-prototype badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check class-slots.
(let ((*sampclass* (defclass sampclass15 () ())))
  (defmethod clos:class-slots ((c (eql *sampclass*)))
    (call-next-method))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (clos:class-slots *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass16 () ())))
  (let ((badmethod
          (defmethod clos:class-slots ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unless (clos:class-finalized-p *sampclass*)
      (clos:finalize-inheritance *sampclass*))
    (unwind-protect
      (nth-value 1 (clos:class-slots *sampclass*))
      (remove-method #'clos:class-slots badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check (setf class-name).
(let ((*sampclass* (defclass sampclass17 () ())))
  (defmethod (setf class-name) (new-value (c (eql *sampclass*)))
    (call-next-method))
  (setf (class-name *sampclass*) 'sampclass17renamed)
  t)
T
(let ((*sampclass* (defclass sampclass18 () ())))
  (let ((badmethod
          (defmethod (setf class-name) (new-value (c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (setf (class-name *sampclass*) 'sampclass18renamed))
      (remove-method #'(setf class-name) badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check finalize-inheritance.
(let ((*sampclass* (defclass sampclass19 () ())))
  (defmethod clos:finalize-inheritance ((c (eql *sampclass*)))
    (call-next-method))
  (clos:finalize-inheritance *sampclass*)
  t)
T
(let ((*sampclass* (defclass sampclass20 () ())))
  (let ((badmethod
          (defmethod clos:finalize-inheritance ((c (eql *sampclass*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:finalize-inheritance *sampclass*))
      (remove-method #'clos:finalize-inheritance badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check find-method-combination.
(let ((*sampgf* (defgeneric sampgf01 (x y))))
  (defmethod clos:find-method-combination ((gf (eql *sampgf*)) name options)
    (call-next-method))
  (clos:find-method-combination *sampgf* 'standard nil)
  t)
T
(let ((*sampgf* (defgeneric sampgf02 (x y))))
  (let ((badmethod
          (defmethod clos:find-method-combination ((gf (eql *sampgf*)) name options)
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:find-method-combination *sampgf* 'standard nil))
      (remove-method #'clos:find-method-combination badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-argument-precedence-order.
(let ((*sampgf* (defgeneric sampgf03 (x y))))
  (defmethod clos:generic-function-argument-precedence-order ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-argument-precedence-order *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf04 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-argument-precedence-order ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-argument-precedence-order *sampgf*))
      (remove-method #'clos:generic-function-argument-precedence-order badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-declarations.
(let ((*sampgf* (defgeneric sampgf05 (x y))))
  (defmethod clos:generic-function-declarations ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-declarations *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf06 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-declarations ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-declarations *sampgf*))
      (remove-method #'clos:generic-function-declarations badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-lambda-list.
(let ((*sampgf* (defgeneric sampgf07 (x y))))
  (defmethod clos:generic-function-lambda-list ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-lambda-list *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf08 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-lambda-list ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-lambda-list *sampgf*))
      (remove-method #'clos:generic-function-lambda-list badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-method-class.
(let ((*sampgf* (defgeneric sampgf09 (x y))))
  (defmethod clos:generic-function-method-class ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-method-class *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf10 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-method-class ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-method-class *sampgf*))
      (remove-method #'clos:generic-function-method-class badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-method-combination.
#-LISPWORKS
(let ((*sampgf* (defgeneric sampgf11 (x y))))
  (defmethod clos:generic-function-method-combination ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-method-combination *sampgf*)
  t)
#-LISPWORKS
T
#-LISPWORKS
(let ((*sampgf* (defgeneric sampgf12 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-method-combination ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-method-combination *sampgf*))
      (remove-method #'clos:generic-function-method-combination badmethod))))
#+CLISP ERROR
#-(or CLISP LISPWORKS) T

;; Check generic-function-methods.
(let ((*sampgf* (defgeneric sampgf13 (x y))))
  (defmethod clos:generic-function-methods ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-methods *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf14 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-methods ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-methods *sampgf*))
      (remove-method #'clos:generic-function-methods badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check generic-function-name.
(let ((*sampgf* (defgeneric sampgf15 (x y))))
  (defmethod clos:generic-function-name ((gf (eql *sampgf*)))
    (call-next-method))
  (clos:generic-function-name *sampgf*)
  t)
T
(let ((*sampgf* (defgeneric sampgf16 (x y))))
  (let ((badmethod
          (defmethod clos:generic-function-name ((gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:generic-function-name *sampgf*))
      (remove-method #'clos:generic-function-name badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check (setf generic-function-name).
(let ((*sampgf* (defgeneric sampgf17 (x y))))
  (defmethod (setf clos:generic-function-name) (new-value (gf (eql *sampgf*)))
    (call-next-method))
  (setf (clos:generic-function-name *sampgf*) 'sampgf17renamed)
  t)
T
(let ((*sampgf* (defgeneric sampgf18 (x y))))
  (let ((badmethod
          (defmethod (setf clos:generic-function-name) (new-value (gf (eql *sampgf*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (setf (clos:generic-function-name *sampgf*) 'sampgf18renamed))
      (remove-method #'(setf clos:generic-function-name) badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check method-function.
(let ((*sampmethod* (defmethod sampgf19 () 'bar)))
  (defmethod clos:method-function ((method (eql *sampmethod*)))
    (call-next-method))
  (clos:method-function *sampmethod*)
  t)
T
(let ((*sampmethod* (defmethod sampgf20 () 'bar)))
  (let ((badmethod
          (defmethod clos:method-function ((method (eql *sampmethod*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:method-function *sampmethod*))
      (remove-method #'clos:method-function badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check method-generic-function.
#-LISPWORKS
(let ((*sampmethod* (defmethod sampgf21 () 'bar)))
  (defmethod clos:method-generic-function ((method (eql *sampmethod*)))
    (call-next-method))
  (clos:method-generic-function *sampmethod*)
  t)
#-LISPWORKS
T
#-LISPWORKS
(let ((*sampmethod* (defmethod sampgf22 () 'bar)))
  (let ((badmethod
          (defmethod clos:method-generic-function ((method (eql *sampmethod*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:method-generic-function *sampmethod*))
      (remove-method #'clos:method-generic-function badmethod))))
#+CLISP ERROR
#-(or CLISP LISPWORKS) T

;; Check method-lambda-list.
(let ((*sampmethod* (defmethod sampgf23 () 'bar)))
  (defmethod clos:method-lambda-list ((method (eql *sampmethod*)))
    (call-next-method))
  (clos:method-lambda-list *sampmethod*)
  t)
T
(let ((*sampmethod* (defmethod sampgf24 () 'bar)))
  (let ((badmethod
          (defmethod clos:method-lambda-list ((method (eql *sampmethod*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:method-lambda-list *sampmethod*))
      (remove-method #'clos:method-lambda-list badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check method-specializers.
#-LISPWORKS
(let ((*sampmethod* (defmethod sampgf25 () 'bar)))
  (defmethod clos:method-specializers ((method (eql *sampmethod*)))
    (call-next-method))
  (clos:method-specializers *sampmethod*)
  t)
#-LISPWORKS
T
#-LISPWORKS
(let ((*sampmethod* (defmethod sampgf26 () 'bar)))
  (let ((badmethod
          (defmethod clos:method-specializers ((method (eql *sampmethod*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:method-specializers *sampmethod*))
      (remove-method #'clos:method-specializers badmethod))))
#+CLISP ERROR
#-(or CLISP LISPWORKS) T

;; Check accessor-method-slot-definition.
#-LISPWORKS
(let ((*sampmethod*
        (progn (defclass sampclass21 () ((x :reader sampclass21x)))
               (first (clos:generic-function-methods #'sampclass21x)))))
  (defmethod clos:accessor-method-slot-definition ((method (eql *sampmethod*)))
    (call-next-method))
  (clos:accessor-method-slot-definition *sampmethod*)
  t)
#-LISPWORKS
T
#-LISPWORKS
(let ((*sampmethod*
        (progn (defclass sampclass22 () ((x :reader sampclass22x)))
               (first (clos:generic-function-methods #'sampclass22x)))))
  (let ((badmethod
          (defmethod clos:accessor-method-slot-definition ((slotdef (eql *sampmethod*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:accessor-method-slot-definition *sampmethod*))
      (remove-method #'clos:accessor-method-slot-definition badmethod))))
#+CLISP ERROR
#-(or CLISP LISPWORKS) T

;; Check slot-definition-allocation.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass23 () ((x)))))))
  (defmethod clos:slot-definition-allocation ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-allocation *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass24 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-allocation ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-allocation *sampslot*))
      (remove-method #'clos:slot-definition-allocation badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-initargs.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass25 () ((x)))))))
  (defmethod clos:slot-definition-initargs ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-initargs *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass26 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-initargs ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-initargs *sampslot*))
      (remove-method #'clos:slot-definition-initargs badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-initform.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass27 () ((x)))))))
  (defmethod clos:slot-definition-initform ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-initform *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass28 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-initform ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-initform *sampslot*))
      (remove-method #'clos:slot-definition-initform badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-initfunction.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass29 () ((x)))))))
  (defmethod clos:slot-definition-initfunction ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-initfunction *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass30 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-initfunction ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-initfunction *sampslot*))
      (remove-method #'clos:slot-definition-initfunction badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-name.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass31 () ((x)))))))
  (defmethod clos:slot-definition-name ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-name *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass32 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-name ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-name *sampslot*))
      (remove-method #'clos:slot-definition-name badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-type.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass33 () ((x)))))))
  (defmethod clos:slot-definition-type ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-type *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass34 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-type ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-type *sampslot*))
      (remove-method #'clos:slot-definition-type badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-readers.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass35 () ((x)))))))
  (defmethod clos:slot-definition-readers ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-readers *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass36 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-readers ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-readers *sampslot*))
      (remove-method #'clos:slot-definition-readers badmethod))))
#+CLISP ERROR
#-CLISP T

#+CLISP
(let ((struct (defstruct struct04 slot1)))
  (nconc (mapcar #'clos:slot-definition-readers
                 (clos:class-direct-slots (find-class struct)))
         (mapcar #'clos:slot-definition-writers
                 (clos:class-direct-slots (find-class struct)))))
#+CLISP ((STRUCT04-SLOT1) ((SETF STRUCT04-SLOT1)))

#+CLISP
(let ((struct (defstruct struct04ro (slot1 t :read-only t))))
  (nconc (mapcar #'clos:slot-definition-readers
                 (clos:class-direct-slots (find-class struct)))
         (mapcar #'clos:slot-definition-writers
                 (clos:class-direct-slots (find-class struct)))))
#+CLISP ((STRUCT04RO-SLOT1) NIL)

#+CLISP
(let ((struct (defstruct (struct04v (:type vector)) slot1)))
  (nconc (mapcar #'clos:slot-definition-readers
                 (sys::structure-direct-slots struct))
         (mapcar #'clos:slot-definition-writers
                 (sys::structure-direct-slots struct))))
#+CLISP ((STRUCT04V-SLOT1) ((SETF STRUCT04V-SLOT1)))

#+CLISP
(let ((struct (defstruct (struct04rov (:type vector)) (slot1 t :read-only t))))
  (nconc (mapcar #'clos:slot-definition-readers
                 (sys::structure-direct-slots struct))
         (mapcar #'clos:slot-definition-writers
                 (sys::structure-direct-slots struct))))
#+CLISP ((STRUCT04ROV-SLOT1) NIL)

;; check that there are no redefinition warnings
(let* ((f "defstruct-test.lisp")
       (c (compile-file-pathname f))
       #+CLISP (custom:*suppress-check-redefinition* nil)
       #+CLISP (l (make-pathname :type "lib" :defaults c))
       (*break-on-signals* t))
  (with-open-file (s f :direction :output :if-exists :supersede)
    (write '(defstruct struct05 slot) :stream s) (terpri s)
    (write '(defstruct (struct05v (:type vector)) slotv) :stream s) (terpri s))
  (unwind-protect (progn (compile-file f) nil)
    (delete-file f)
    (delete-file c)
    #+clisp (delete-file l)))
NIL

;; Check slot-definition-writers.
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass37 () ((x)))))))
  (defmethod clos:slot-definition-writers ((slotdef (eql *sampslot*)))
    (call-next-method))
  (clos:slot-definition-writers *sampslot*)
  t)
T
(let ((*sampslot*
        (first (clos:class-direct-slots (defclass sampclass38 () ((x)))))))
  (let ((badmethod
          (defmethod clos:slot-definition-writers ((slotdef (eql *sampslot*)))
            (values (call-next-method) t))))
    (unwind-protect
      (nth-value 1 (clos:slot-definition-writers *sampslot*))
      (remove-method #'clos:slot-definition-writers badmethod))))
#+CLISP ERROR
#-CLISP T

;; Check slot-definition-location.
(let ((*sampclass* (defclass sampclass39 () ((x)))))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (let ((*sampslot* (first (clos:class-slots *sampclass*))))
    (defmethod clos:slot-definition-location ((slotdef (eql *sampslot*)))
      (call-next-method))
    (clos:slot-definition-location *sampslot*)
    t))
T
(let ((*sampclass* (defclass sampclass39 () ((x)))))
  (unless (clos:class-finalized-p *sampclass*)
    (clos:finalize-inheritance *sampclass*))
  (let ((*sampslot* (first (clos:class-slots *sampclass*))))
    (let ((badmethod
            (defmethod clos:slot-definition-location ((slotdef (eql *sampslot*)))
              (values (call-next-method) t))))
      (unwind-protect
        (nth-value 1 (clos:slot-definition-location *sampslot*))
        (remove-method #'clos:slot-definition-location badmethod)))))
#+CLISP ERROR
#-CLISP T


;; Check that DEFMETHOD calls ADD-METHOD.
(let ((add-method-called nil))
  (defclass testgenericfunction142 (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defmethod add-method :before ((gf testgenericfunction142) (method standard-method))
    (setq add-method-called t))
  (defgeneric testgf142 (x)
    (:generic-function-class testgenericfunction142))
  (defmethod testgf142 (x)
    (declare (ignore x)))
  add-method-called)
T


;; Check that DEFMETHOD calls REMOVE-METHOD.
(let ((remove-method-called nil))
  (defclass testgenericfunction143 (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defmethod remove-method :before ((gf testgenericfunction143) (method standard-method))
    (setq remove-method-called t))
  (defgeneric testgf143 (x)
    (:generic-function-class testgenericfunction143))
  (defmethod testgf143 (x)
    (declare (ignore x))
    17)
  (defmethod testgf143 (x)
    (declare (ignore x))
    19)
  remove-method-called)
T


;; Check that it's possible to call methods individually.
(progn
  (defgeneric foo141 (x)
    (:method ((x integer)) (isqrt x))
    (:method ((x real)) (- x)))
  (let ((my-method (find-method #'foo141 nil (list (find-class 'real))))
        (my-arglist (list 43)))
    (funcall (clos:method-function my-method) my-arglist '())))
-43


;; Check that it's possible to create custom method classes.
(progn
  (defclass custom-method (method)
    ((qualifiers       :reader method-qualifiers
                       :writer (setf custom-method-qualifiers))
     (lambda-list      :reader method-lambda-list
                       :writer (setf custom-method-lambda-list))
     (specializers     :reader method-specializers
                       :writer (setf custom-method-specializers))
     (function         :reader method-function
                       :writer (setf custom-method-function))
     (documentation    :accessor custom-method-documentation)
     (generic-function :reader method-generic-function
                       :writer (setf custom-method-generic-function))))
  (defmethod shared-initialize ((method custom-method) situation &rest args
                                  &key (qualifiers nil qualifiers-p)
                                       (lambda-list nil lambda-list-p)
                                       (specializers nil specializers-p)
                                       (function nil function-p)
                                       (documentation nil documentation-p))
    (call-next-method)
    (when (or (eq situation 't) qualifiers-p)
      (setf (custom-method-qualifiers method) qualifiers))
    (when (or (eq situation 't) lambda-list-p)
      (setf (custom-method-lambda-list method) lambda-list))
    (when (or (eq situation 't) specializers-p)
      (setf (custom-method-specializers method) specializers))
    (when (or (eq situation 't) function-p)
      (setf (custom-method-function method) function))
    (when (or (eq situation 't) documentation-p)
      (setf (custom-method-documentation method) documentation))
    (when (eq situation 't)
      (setf (custom-method-generic-function method) nil))
    method)
  (defmethod documentation ((x custom-method) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (custom-method-documentation x))
  (defmethod (setf documentation) (new-value (x custom-method) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (setf (custom-method-documentation x) new-value))
  ;; (setf method-generic-function) is a CLISP extension.
  (defmethod (setf method-generic-function) (new-gf (method custom-method))
    (setf (custom-method-generic-function method) new-gf))
  #| ; Instead of overriding add-method and remove-method:
  (defmethod add-method ((gf standard-generic-function) (m custom-method))
    (setf (custom-method-generic-function m) gf)
    (call-next-method))
  (defmethod remove-method ((gf standard-generic-function) (m custom-method))
    (setf (custom-method-generic-function m) nil)
    (call-next-method))
  |#
  (let ((result '()))
    (defgeneric testgf30 (a b)
      (:method ((a integer) (b integer)) (- (call-next-method) (floor a b)))
      (:method ((a real) (b real)) (/ (float a) (float b)))
      (:method-class custom-method))
    (push (not (find-method #'testgf30 nil (list (find-class 'integer) (find-class 'integer)) nil))
          result)
    (push (testgf30 17 2) result)
    (defgeneric testgf30 (a b)
      (:method ((a real) (b real)) (/ (float a) (float b)))
      (:method-class custom-method))
    (push (not (find-method #'testgf30 nil (list (find-class 'integer) (find-class 'integer)) nil))
          result)
    (push (testgf30 17 2) result)
    (nreverse result)))
(NIL 0.5 T 8.5)


;; Check that changing a method's class clears the generic function's
;; effective-methods or discriminating-function cache.
(progn
  (defgeneric testgf34 (x))
  (defmethod testgf34 ((x integer))
    'old-integer)
  (defmethod testgf34 ((x real))
    'real)
  (list*
    (testgf34 3) ; OLD-INTEGER
    (testgf34 22/7) ; REAL
    (progn
      (let ((method (find-method #'testgf34 '() (list (find-class 'integer)))))
        (change-class method (find-class 'custom-method)
          :qualifiers '()
          :lambda-list '(x)
          :specializers (list (find-class 'rational))
          :function #'(lambda (arguments next-methods) 'new-rational)
          :documentation nil))
      (list
        (testgf34 3) ; NEW-RATIONAL
        (testgf34 22/7) ; NEW-RATIONAL
      ))))
(OLD-INTEGER REAL NEW-RATIONAL NEW-RATIONAL)


;; Check that changing a generic function's class clears its
;; effective-methods and discriminating-function cache.
; The effective-methods cache:
#-OpenMCL
(progn
  (defgeneric testgf35 (x))
  (defmethod testgf35 ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (defmethod testgf35 ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (defclass customized5-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defmethod clos:compute-effective-method ((gf customized5-generic-function) method-combination methods)
    `(REVERSE ,(call-next-method)))
  (list
    (testgf35 3)
    (progn
      (change-class #'testgf35 'customized5-generic-function)
      (testgf35 3))))
#-OpenMCL
((INTEGER REAL) (REAL INTEGER))
; The discriminating-function cache:
#-OpenMCL
(progn
  (defgeneric testgf36 (x))
  (defmethod testgf36 ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (defmethod testgf36 ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (defclass customized6-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defmethod clos:compute-discriminating-function ((gf customized6-generic-function))
    (let ((orig-df (call-next-method)))
      #'(lambda (&rest arguments)
          (reverse (apply orig-df arguments)))))
  (list
    (testgf36 3)
    (progn
      (change-class #'testgf36 'customized6-generic-function)
      (testgf36 3))))
#-OpenMCL
((INTEGER REAL) (REAL INTEGER))


#| ;; Not implemented, because the MOP's description of
   ;; compute-discriminating-function doesn't say that we need to invalidate
   ;; the effective method cache in this case.

;; Check that defining a method on compute-applicable-methods[-using-classes]
;; invalidates the cache of all affected generic functions.
(progn
  (defclass customized1-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defgeneric testgf31 (x)
    (:generic-function-class customized1-generic-function))
  (defmethod testgf31 ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (defmethod testgf31 ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (list
    (testgf31 3)
    (progn
      (defmethod compute-applicable-methods ((gf customized1-generic-function) args)
        (let ((all-applicable (call-next-method)))
          (if all-applicable (list (first all-applicable)) '())))
      #-LISPWORKS
      (defmethod clos:compute-applicable-methods-using-classes ((gf customized1-generic-function) classes)
        (let ((all-applicable (call-next-method)))
          (if all-applicable (list (first all-applicable)) '())))
      (testgf31 3))))
((INTEGER REAL) (INTEGER))

;; Check that defining a method on compute-effective-method
;; invalidates the cache of all affected generic functions.
(progn
  (defclass customized2-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defgeneric testgf32 (x)
    (:generic-function-class customized2-generic-function))
  (defmethod testgf32 ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (defmethod testgf32 ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (list
    (testgf32 3)
    (progn
      (defmethod clos:compute-effective-method ((gf customized2-generic-function) method-combination methods)
        `(REVERSE ,(call-next-method)))
      (testgf32 3))))
((INTEGER REAL) (REAL INTEGER))

;; Check that defining a method on compute-discriminating-function
;; invalidates the cache of all affected generic functions.
(progn
  (defclass customized3-generic-function (standard-generic-function)
    ()
    (:metaclass clos:funcallable-standard-class))
  (defgeneric testgf33 (x)
    (:generic-function-class customized3-generic-function))
  (defmethod testgf33 ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (defmethod testgf33 ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (list
    (testgf33 3)
    (progn
      (defmethod clos:compute-discriminating-function ((gf customized3-generic-function))
        (let ((orig-df (call-next-method)))
          #'(lambda (&rest arguments)
              (reverse (apply orig-df arguments)))))
      (testgf33 3))))
((INTEGER REAL) (REAL INTEGER))

|#


;;; Application example: Typechecked slots

(progn
  (defclass typechecked-slot-definition (clos:standard-effective-slot-definition)
    ())
  (defmethod clos:slot-value-using-class ((class standard-class) instance (slot typechecked-slot-definition))
    (let ((value (call-next-method)))
      (unless (typep value (clos:slot-definition-type slot))
        (error "Slot ~S of ~S has changed, no longer of type ~S"
               (clos:slot-definition-name slot) instance (clos:slot-definition-type slot)))
      value))
  (defmethod (setf clos:slot-value-using-class) (new-value (class standard-class) instance (slot typechecked-slot-definition))
    (unless (typep new-value (clos:slot-definition-type slot))
      (error "Slot ~S of ~S: new value is not of type ~S: ~S"
             (clos:slot-definition-name slot) instance (clos:slot-definition-type slot) new-value))
    (call-next-method))
  (defclass typechecked-slot-definition-class (standard-class)
    ())
  #-CLISP
  (defmethod clos:validate-superclass ((c1 typechecked-slot-definition-class) (c2 standard-class))
    t)
  (defmethod clos:effective-slot-definition-class ((class typechecked-slot-definition-class) &rest args)
    (find-class 'typechecked-slot-definition))
  (defclass testclass28 ()
    ((pair :type (cons symbol (cons symbol null)) :initarg :pair :accessor testclass28-pair))
    (:metaclass typechecked-slot-definition-class))
  (macrolet ((succeeds (form)
               `(not (nth-value 1 (ignore-errors ,form)))))
    (let ((p (list 'abc 'def))
          (x (make-instance 'testclass28)))
      (list (succeeds (make-instance 'testclass28 :pair '(seventeen 17)))
            (succeeds (setf (testclass28-pair x) p))
            (succeeds (setf (second p) 456))
            (succeeds (testclass28-pair x))
            (succeeds (slot-value x 'pair))))))
(nil t t nil nil)


;;; Application example: Slot which has one value cell per subclass.

#+(or CLISP CMU SBCL LISPWORKS)
(progn

  ;; We must limit the support for per-subclass slots to those that inherit
  ;; from this class, because we need to specialize
  ;; clos:direct-slot-definition-class, clos:compute-slots and a few other
  ;; generic functions and must not override the method responsible for
  ;; standard-class.
  (defclass class-supporting-classof-slots (standard-class)
    ((slotname-to-dummyslotname :type list :initform nil)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 class-supporting-classof-slots) (c2 standard-class))
    t)

  ;; Define subclasses of direct-slot-definition that support a :per-subclass
  ;; option. (It's not portable to use :allocation :classof, so we use
  ;; :per-subclass t instead.)
  (defclass classof-direct-slot-definition-mixin ()
    ())
  (let ((add-mixin-table (make-hash-table :test #+clisp 'ext:stablehash-eq #-clisp 'eq)))
    ;; For a given direct slot definition class, returns a subclass that also
    ;; inherits from classof-direct-slot-definition-mixin.
    (defun add-classof-direct-mixin (slot-class)
      (if (subtypep slot-class (find-class 'classof-direct-slot-definition-mixin))
        slot-class
        (or (gethash slot-class add-mixin-table)
            (setf (gethash slot-class add-mixin-table)
                  (clos:ensure-class (make-symbol (concatenate 'string (symbol-name (class-name slot-class)) "-WITH-CLASSOF-SUPPORT"))
                    :metaclass (class-of slot-class)
                    :direct-superclasses (list slot-class (find-class 'classof-direct-slot-definition-mixin))))))))
  (defmethod clos:direct-slot-definition-class ((class class-supporting-classof-slots) &rest initargs)
    (if (getf initargs ':per-subclass)
      (add-classof-direct-mixin (call-next-method))
      (call-next-method)))
  (defmethod initialize-instance :after ((slot classof-direct-slot-definition-mixin) &rest initargs &key per-subclass)
    (declare (ignore per-subclass)))

  ;; If the direct slot has :per-subclass t, let the effective slot have
  ;; :per-subclass t as well.
  (defmethod clos:compute-effective-slot-definition-initargs ((class class-supporting-classof-slots) #+LISPWORKS name direct-slot-definitions)
    (if (typep (first direct-slot-definitions) 'classof-direct-slot-definition-mixin)
      (append (call-next-method) (list ':per-subclass t))
      (call-next-method)))

  ;; Define subclasses of effective-slot-definition that support a :per-subclass
  ;; option.
  (defclass classof-effective-slot-definition-mixin ()
    ((value-slot-name :type symbol)))
  (let ((add-mixin-table (make-hash-table :test #+clisp 'ext:stablehash-eq #-clisp 'eq)))
    ;; For a given effective slot definition class, returns a subclass that also
    ;; inherits from classof-effective-slot-definition-mixin.
    (defun add-classof-effective-mixin (slot-class)
      (if (subtypep slot-class (find-class 'classof-effective-slot-definition-mixin))
        slot-class
        (or (gethash slot-class add-mixin-table)
            (setf (gethash slot-class add-mixin-table)
                  (clos:ensure-class (make-symbol (concatenate 'string (symbol-name (class-name slot-class)) "-WITH-CLASSOF-SUPPORT"))
                    :metaclass (class-of slot-class)
                    :direct-superclasses (list slot-class (find-class 'classof-effective-slot-definition-mixin))))))))
  (defmethod clos:effective-slot-definition-class ((class class-supporting-classof-slots) &rest initargs)
    (if (getf initargs ':per-subclass)
      (add-classof-effective-mixin (call-next-method))
      (call-next-method)))
  (defmethod initialize-instance :after ((slot classof-effective-slot-definition-mixin) &rest initargs &key per-subclass)
    (declare (ignore per-subclass)))

  ;; Add dummy effective slots, used to store the per-subclass value.
  ;; (Using a dummy slot here, instead of just storing the value in the
  ;; classof-effective-slot-definition-mixin, provides for smooth behaviour
  ;; when a class is redefined: the values of slots are kept, but
  ;; effective-slot-definitions and their contents are thrown away.)
  (defmethod clos:compute-slots ((class class-supporting-classof-slots))
    (let* ((slots (call-next-method))
           (dummy-slots
             (let ((old-dummyslotnames (slot-value class 'slotname-to-dummyslotname))
                   (new-dummyslotnames '()))
               (prog1
                 (mapcan #'(lambda (slot)
                             (if (typep slot 'classof-effective-slot-definition-mixin)
                               (let* ((value-slot-name
                                        ;; Try to keep the same dummyslotname as in the previous
                                        ;; definition, so that the slot's value is preserved if possible.
                                        (or (getf old-dummyslotnames (clos:slot-definition-name slot))
                                            (make-symbol (concatenate 'string
                                                           "VALUE-OF-"
                                                           (symbol-name (clos:slot-definition-name slot))
                                                           "-IN-"
                                                           (symbol-name (class-name class))))))
                                      (value-slot
                                        (make-instance 'clos:standard-effective-slot-definition
                                          :name value-slot-name
                                          :allocation :class
                                          :initform (clos:slot-definition-initform slot)
                                          :initfunction (clos:slot-definition-initfunction slot)
                                          :type (clos:slot-definition-type slot))))
                                 (setf (slot-value slot 'value-slot-name) value-slot-name)
                                 (setf (getf new-dummyslotnames (clos:slot-definition-name slot)) value-slot-name)
                                 (list value-slot))
                               '()))
                         slots)
                 (setf (slot-value class 'slotname-to-dummyslotname) new-dummyslotnames)))))
      (append slots dummy-slots)))

  ;; Redirect slot-value et al. from the slot with :per-subclass t to the dummy
  ;; slot.
  (defmethod clos:slot-value-using-class ((class standard-class) object (slot classof-effective-slot-definition-mixin))
    (slot-value object (slot-value slot 'value-slot-name)))
  (defmethod (setf clos:slot-value-using-class) (new-value (class standard-class) object (slot classof-effective-slot-definition-mixin))
    (setf (slot-value object (slot-value slot 'value-slot-name)) new-value))
  (defmethod clos:slot-boundp-using-class ((class standard-class) object (slot classof-effective-slot-definition-mixin))
    (slot-boundp object (slot-value slot 'value-slot-name)))
  (defmethod clos:slot-makunbound-using-class ((class standard-class) object (slot classof-effective-slot-definition-mixin))
    (slot-makunbound object (slot-value slot 'value-slot-name)))

  ;; Provide a general initialization hook, where the initform may depend on the
  ;; class in which it is located.
  (defgeneric initialize-classof-slot (class slot)
    (:method ((class class-supporting-classof-slots) (slot classof-effective-slot-definition-mixin))))
  (defmethod initialize-instance :after ((class class-supporting-classof-slots) &rest initargs)
    (dolist (slot (clos:class-slots class))
      (when (and (typep slot 'classof-effective-slot-definition-mixin)
                 (not (slot-boundp (clos:class-prototype class) (clos:slot-definition-name slot))))
        (initialize-classof-slot class slot))))

  ;; Test it.
  (defclass testclass29a ()
    ((x :allocation :instance)
     (y :allocation :class :per-subclass t)
     (z :allocation :class))
    (:metaclass class-supporting-classof-slots))
  (defclass testclass29b (testclass29a)
    ()
    (:metaclass class-supporting-classof-slots))
  (let ((insta1 (make-instance 'testclass29a))
        (insta2 (make-instance 'testclass29a))
        (instb1 (make-instance 'testclass29b))
        (instb2 (make-instance 'testclass29b)))
    (setf (slot-value insta1 'x) 'x1)
    (setf (slot-value insta1 'y) 'y1)
    (setf (slot-value insta1 'z) 'z1)
    (setf (slot-value instb1 'x) 'x2)
    (setf (slot-value instb1 'y) 'y2)
    (setf (slot-value instb1 'z) 'z2)
    (setf (slot-value instb2 'x) 'x3)
    (setf (slot-value instb2 'y) 'y3)
    (setf (slot-value instb2 'z) 'z3)
    (setf (slot-value insta2 'x) 'x4)
    (setf (slot-value insta2 'y) 'y4)
    (setf (slot-value insta2 'z) 'z4)
    (list (slot-value insta1 'x) (slot-value insta1 'y) (slot-value insta1 'z)
          (slot-value insta2 'x) (slot-value insta2 'y) (slot-value insta2 'z)
          (slot-value instb1 'x) (slot-value instb1 'y) (slot-value instb1 'z)
          (slot-value instb2 'x) (slot-value instb2 'y) (slot-value instb2 'z))))
#+(or CLISP CMU SBCL LISPWORKS)
(x1 y4 z4
 x4 y4 z4
 x2 y3 z4
 x3 y3 z4)


(progn
  (load (merge-pathnames "mop-aux.lisp" *run-test-truename*))
  (load (merge-pathnames "hash-classes.lisp" *run-test-truename*))
  t)
t


;;; Application example: Virtual-dispatch generic functions

;; There are two variants:
;; In C++, each instance contains a virtual function table at a fixed location.
;; In Java, the virtual function table is a member of the class.
;; Here we represent the virtual function table as a per-subclass shared slot.
;; TODO: Needs a little more work to deal with non-finalized classes.

#+(or CLISP CMU SBCL)
(progn

  ;; Every virtual generic function belongs to a particular "base class";
  ;; it is only applicable to instances of this base class. Such a base class
  ;; must be of metaclass virtual-base-class. All subclasses of a class with
  ;; metaclass virtual-base-class must be of metaclass virtual-class (or
  ;; a subclass of it, such as virtual-base-class).

  ;; The metaclass of all objects that can be subject to virtual dispatch.
  (defclass virtual-class (class-supporting-classof-slots standard-class)
    ())
  ;; The metaclass of all classes that can be tied to a virtual generic
  ;; function.
  (defclass virtual-base-class (virtual-class)
    ((vt-functions              ; vector of all virtual generic functions
       :type vector             ; with this base class
       :accessor vtbase-vt-functions)
     (vt-slot-name              ; name of virtual table slot in all subclasses
       :type symbol
       :accessor vtbase-vt-slot-name)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 virtual-base-class) (c2 standard-class))
    t)
  (defmethod clos:validate-superclass ((c1 virtual-class) (c2 virtual-base-class))
    t)

  ;; Ensure every subclass is equipped with a virtual table.
  (defmethod initialize-instance ((class virtual-base-class) &rest initargs
                                  &key (direct-slots '()))
    (setf (vtbase-vt-functions class) (make-array 10 :adjustable t :fill-pointer 0))
    (setf (vtbase-vt-slot-name class) (gensym "VTABLE"))
    (apply #'call-next-method class
           :direct-slots (cons (list ':name (vtbase-vt-slot-name class)
                                     ':allocation ':class ':per-subclass t
                                     ':base-class class)
                               direct-slots)
           initargs))

  ;; The virtual table slot in all subclasses needs to have a pointer to the
  ;; base class where it comes from (for its initialization). Therefore we
  ;; need to pass the base-class pointer from the (inheritable) direct vt slot
  ;; to the (not inherited) effective vt slot.
  (defclass virtual-table-direct-slot-definition (clos:standard-direct-slot-definition classof-direct-slot-definition-mixin)
    ((base-class :initarg :base-class)))
  (defclass virtual-table-effective-slot-definition (clos:standard-effective-slot-definition classof-effective-slot-definition-mixin)
    ((base-class :initarg :base-class)))
  (defmethod clos:direct-slot-definition-class ((class virtual-base-class) &rest initargs)
    (if (getf initargs ':base-class)
      (find-class 'virtual-table-direct-slot-definition)
      (call-next-method)))
  (defmethod clos:compute-effective-slot-definition-initargs ((class virtual-class) #+LISPWORKS name direct-slot-definitions)
    (if (typep (first direct-slot-definitions) 'virtual-table-direct-slot-definition)
      (append (call-next-method)
              (list ':base-class (slot-value (first direct-slot-definitions) 'base-class)))
      (call-next-method)))
  (defmethod clos:effective-slot-definition-class ((class virtual-class) &rest initargs)
    (if (getf initargs ':base-class)
      (find-class 'virtual-table-effective-slot-definition)
      (call-next-method)))

  ;; Computes the effective method (as a function) for executing gf (which
  ;; must be a virtual generic function) for _direct_ instances of the given
  ;; class.
  (defun compute-virtual-generic-function-effective-method (gf class)
    ;; This relies on the known method specializer format, verified by
    ;; add-method below.
    (multiple-value-bind (methods certain)
        (clos:compute-applicable-methods-using-classes gf
          (cons class
                (make-list (1- (length (clos:generic-function-argument-precedence-order gf)))
                           :initial-element (find-class 't))))
      (unless certain
        (error "Problem determining the applicable methods of ~S on ~S" gf class))
      (clos::compute-effective-method-as-function gf methods
        (cons (clos:class-prototype class)
              (make-list (1- (length (clos:generic-function-argument-precedence-order gf)))
                         :initial-element nil)))))

  ;; Initialize the virtual table slot.
  (defmethod initialize-classof-slot ((class virtual-class) (slot virtual-table-effective-slot-definition))
    (setf (slot-value (clos:class-prototype class) (clos:slot-definition-name slot))
          (let* ((base-class (slot-value slot 'base-class))
                 (current-length (length (vtbase-vt-functions base-class)))
                 (vtable (make-array current-length :adjustable t :fill-pointer current-length)))
            (dotimes (i current-length)
              (setf (aref vtable i)
                    (compute-virtual-generic-function-effective-method
                      (aref (vtbase-vt-functions base-class) i)
                      class)))
            vtable)))

  ;; Auxiliary function: Return a list of all subclasses of class, including
  ;; class itself, in an arbitrary order.
  (defun collect-all-subclasses (class)
    (let ((result '()) (todo (list class)))
      (loop
        (unless todo (return))
        (let ((last-todo todo))
          (setq todo '())
          (dolist (c last-todo)
            (unless (member c result)
              (setq todo (revappend (clos:class-direct-subclasses c) todo))
              (push c result)))))
      (nreverse result)))

  ;; A virtual generic function is tied to a base-class.
  (defclass virtual-generic-function (standard-generic-function)
    ((base-class
       :type class
       :accessor vtgf-base-class)
     (vt-index                  ; index in (vtbase-vt-functions base-class)
       :type fixnum
       :accessor vtgf-vt-index))
    (:metaclass clos:funcallable-standard-class))

  ;; When a new virtual generic function is created, it needs to be registered
  ;; in its base class.
  (defmethod shared-initialize ((gf virtual-generic-function) situation &rest args
                                &key (base-class nil base-class-p))
    (call-next-method)
    (when base-class-p
      (when (consp base-class)
        (setq base-class (car base-class)))
      (unless (typep base-class 'class)
        (setq base-class (find-class base-class)))
      ; base-class is now a class.
      (setf (vtgf-base-class gf) base-class)
      (setf (vtgf-vt-index gf)
            (or (position gf (vtbase-vt-functions base-class))
                ; Add gf to the functions in the base-class.
                (let ((index
                        (vector-push-extend gf (vtbase-vt-functions base-class)))
                      (vt-slot-name (vtbase-vt-slot-name base-class)))
                  (dolist (cl (collect-all-subclasses base-class))
                    (let ((cl-proto (clos:class-prototype cl)))
                      #|
                      (unless (slot-boundp cl-proto vt-slot-name)
                        (setf (slot-value cl-proto vt-slot-name) (make-array 10 :adjustable t :fill-pointer 0)))
                      |#
                      (assert (= (fill-pointer (slot-value cl-proto vt-slot-name))
                              index))
                      ;; Preliminary initialization.
                      (vector-push-extend '#:not-yet-updated (slot-value cl-proto vt-slot-name))))
                  index))))
    gf)

  ;; Updates the computed effective methods for the given virtual generic
  ;; functions, in the vtables of all subclasses of class (including class
  ;; itself). class may be the gf's base-class or a subclass of it.
  (defun update-virtual-generic-function (gf &optional (class (vtgf-base-class gf)))
    (let ((vt-slot-name (vtbase-vt-slot-name (vtgf-base-class gf)))
          (vt-index (vtgf-vt-index gf)))
      (dolist (cl (collect-all-subclasses class))
        (setf (aref (slot-value (clos:class-prototype cl) vt-slot-name) vt-index)
              (compute-virtual-generic-function-effective-method gf cl)))))

  ;; Notification: When methods are added or removed to a generic function,
  ;; the computed effective methods in the vtables must be updated. (But the
  ;; dispatch function remains the same.)
  (defclass virtual-generic-function-updater ()
    ())
  (defparameter *virtual-generic-function-updater*
    (make-instance 'virtual-generic-function-updater))
  (defmethod clos:update-dependent ((gf virtual-generic-function) (dependent virtual-generic-function-updater) &rest details)
    (declare (ignore details))
    ;; TODO: Exploit the details, to minimize the updates.
    (update-virtual-generic-function gf))

  ;; When a new virtual generic function is created, it needs to be call
  ;; update-virtual-generic-function now, and later when the method set changes.
  (defmethod initialize-instance :after ((gf virtual-generic-function) &rest args)
    (update-virtual-generic-function gf)
    (clos:add-dependent gf *virtual-generic-function-updater*))

  ;; Verify that only methods dispatching on the first argument are added.
  (defmethod add-method ((gf virtual-generic-function) (method method))
    (let ((<t> (find-class 't)))
      (unless (every #'(lambda (specializer) (eq specializer <t>))
                     (rest (clos:method-specializers method)))
        (error "invalid method for ~S: ~S. May only dispatch on the first argument."
               gf method)))
    (unless (typep (first (clos:method-specializers method)) 'class)
      (error "invalid method for ~S: ~S. The specializer on the first argument must be a class."
             gf method))
    (call-next-method))

  ;; Computes the dispatch for a virtual generic function.
  ;; This is the heart of the example.
  (defmethod clos:compute-discriminating-function ((gf virtual-generic-function))
    (let ((vt-slot-name (vtbase-vt-slot-name (vtgf-base-class gf)))
          (vt-index (vtgf-vt-index gf)))
      (assert (eq (aref (vtbase-vt-functions (vtgf-base-class gf)) vt-index) gf))
      #'(lambda (first-arg &rest other-args)
          (apply (aref (slot-value first-arg vt-slot-name) vt-index)
                 first-arg other-args))))

  ;; Now an example.
  ;;
  ;;   f,g - A     C - h
  ;;         |    /
  ;;         B   /
  ;;          \ /
  ;;           D
  ;;
  (defclass testclass30a ()
    ()
    (:metaclass virtual-base-class))
  (defclass testclass30b (testclass30a)
    ()
    (:metaclass virtual-class))
  (defclass testclass30c ()
    ()
    (:metaclass virtual-base-class))
  (defgeneric testgf30f (x)
    (:method ((x testclass30a))
      "f on A")
    (:generic-function-class virtual-generic-function)
    (:base-class testclass30a))
  (defgeneric testgf30g (x y)
    (:method ((x testclass30a) y)
      (list "g on A" y))
    (:method ((x testclass30b) y)
      (list "g on B" y))
    (:generic-function-class virtual-generic-function)
    (:base-class testclass30a))
  (defgeneric testgf30h (x y)
    (:method ((x testclass30c) y)
      (list "h on C" y))
    (:generic-function-class virtual-generic-function)
    (:base-class testclass30c))
  (defclass testclass30d (testclass30b testclass30c)
    ()
    (:metaclass virtual-class))
  (defmethod testgf30g ((x testclass30d) y)
    (list "g on D" y))
  (defmethod testgf30h ((x testclass30d) y)
    (list "h on D" y))
  (let ((insta (make-instance 'testclass30a))
        (instc (make-instance 'testclass30c))
        (instd (make-instance 'testclass30d)))
    (list (testgf30f insta)
          (testgf30f instd)
          (testgf30g insta 10)
          (testgf30g instd 20)
          (testgf30h instc 30)
          (testgf30h instd 40))))
#+(or CLISP CMU SBCL)
("f on A" "f on A" ("g on A" 10) ("g on D" 20) ("h on C" 30) ("h on D" 40))

;;; user-defined :allocation :hash
;; http://sourceforge.net/tracker/index.php?func=detail&aid=1359066&group_id=1355&atid=101355
(progn
  (defclass person ()
    ((name :initarg :name :allocation :hash :accessor person-name)
     (address :initarg :address :allocation :hash :accessor person-address))
    (:metaclass hash-classes:hash-class))
  (let ((dilbert (make-instance 'person :name "Dilbert")))
    (list (string= (person-name dilbert) "Dilbert")
          (slot-boundp dilbert 'name)
          (slot-boundp dilbert 'address)
          (slot-exists-p dilbert 'foo)
          (string= (gethash 'name (slot-value dilbert
                                              'hash-classes::hash-slots))
                   "Dilbert")
          (progn
            (remhash 'name (slot-value dilbert 'hash-classes::hash-slots))
            (slot-boundp dilbert 'name)))))
(T T NIL NIL T NIL)

;; http://sourceforge.net/tracker/index.php?func=detail&aid=1369668&group_id=1355&atid=101355
;; but the allocation must be defined!
(progn (defclass class-bad-slot () ((bad-slot :allocation :bad-allocation)))
       (make-instance 'class-bad-slot))
ERROR

;; mop.xml#mop-sa-funcallable
(let (constructor)
  (defclass constructor ()
    ((name :initarg :name :accessor constructor-name)
     (fields :initarg :fields :accessor constructor-fields))
    (:metaclass funcallable-standard-class))
  (defmethod initialize-instance :after ((c constructor) &key)
    (with-slots (name fields) c
      (set-funcallable-instance-function
       c
       #'(lambda ()
           (let ((new (make-array (1+ (length fields)))))
             (setf (aref new 0) name)
             new)))))
  (setq constructor (make-instance 'constructor :name 'position :fields '(x y)))
  (list (stringp (with-output-to-string (*standard-output*)
                   (describe constructor)))
        (funcall constructor)))
(T #(POSITION NIL NIL))

;; cleanup
(setf (find-class 'class-bad-slot) nil
      (find-class 'constructor) nil
      (find-class 'person) nil
      (find-class 'counted1-class) nil
      (find-class 'counted1-rectangle) nil
      (find-class 'rectangle2) nil
      (find-class 'counted2-class) nil
      (find-class 'counted2-rectangle) nil
      (find-class 'counter) nil
      (find-class 'counted-object) nil
      (find-class 'structure01) nil
      (find-class 'structure02b) nil
      (find-class 'structure03a) nil
      (find-class 'structure03c) nil
      (find-class 'foo135b) nil
      (find-class 'foo135b) nil
      (find-class 'foo133) nil
      (find-class 'foo134) nil
      (find-class 'my-gf-class) nil
      (find-class 'option-class) nil
      (find-class 'testclass02a) nil
      (find-class 'testclass02b) nil
      (find-class 'testclass02c) nil
      (find-class 'option-slot-definition) nil
      (find-class 'option-slot-class) nil
      (find-class 'testclass03a) nil
      (find-class 'testclass03b) nil
      (find-class 'testclass03c) nil
      (find-class 'testclass03d) nil
      (find-class 'extended-slot-definition) nil
      (find-class 'extended-slot-class) nil
      (find-class 'testclass03e) nil
      (find-class 'testclass03e) nil
      (find-class 'testclass51) nil
      (find-class 'testclass51a) nil
      (find-class 'testclass51b) nil
      (find-class 'testclass51c) nil
      (find-class 'option-generic-function) nil
      (find-class 'testmethod50) nil
      (find-class 'testgenericfunction50) nil
      (find-class 'testmethod51) nil
      (find-class 'testgenericfunction51) nil
      (find-class 'dependent05) nil
      (find-class 'testclass05) nil
      (find-class 'dependent06) nil
      (find-class 'prioritized-dependent) nil
      (find-class 'prioritized-dispatcher) nil
      (find-class 'prioritized-class) nil
      (find-class 'testclass07) nil
      (find-class 'dependent07) nil
      (find-class 'prioritized-generic-function) nil
      (find-class 'dependent08) nil
      (find-class 'volatile-class) nil
      (find-class 'testclass10) nil
      (find-class 'testclass10a) nil
      (find-class 'testclass10b) nil
      (find-class 'testclass10c) nil
      (find-class 'testclass10d) nil
      (find-class 'msl-generic-function) nil
      (find-class 'nonumber-generic-function) nil
      (find-class 'bfs-class) nil
      (find-class 'testclass13a) nil
      (find-class 'testclass13b) nil
      (find-class 'testclass13c) nil
      (find-class 'testclass13d) nil
      (find-class 'testclass13e) nil
      (find-class 'testclass13f) nil
      (find-class 'custom-default-initargs-class) nil
      (find-class 'testclass14) nil
      (find-class 'auto-accessors-2-class) nil
      (find-class 'testclass15) nil
      (find-class 'traced-generic-function) nil
      (find-class 'debuggable-generic-function) nil
      (find-class 'testclass16a) nil
      (find-class 'testclass16b) nil
      (find-class 'testclass16c) nil
      (find-class 'testclass16d) nil
      (find-class 'auto-initargs-class) nil
      (find-class 'testclass17) nil
      (find-class 'auto-initargs-2-class) nil
      (find-class 'testclass17-2) nil
      (find-class 'testclass18b) nil
      (find-class 'testclass18a) nil
      (find-class 'testclass19b) nil
      (find-class 'testclass19a) nil
      (find-class 'auto-accessors-direct-slot-definition-class) nil
      (find-class 'auto-accessors-class) nil
      (find-class 'testclass20) nil
      (find-class 'external-slot-definition) nil
      (find-class 'external-slot-definition-class) nil
      (find-class 'testclass22) nil
      (find-class 'externally-documented-class) nil
      (find-class 'testclass23) nil
      (find-class 'externally-documented-generic-function) nil
      (find-class 'typechecking-reader-method) nil
      (find-class 'typechecking-reader-class) nil
      (find-class 'testclass25) nil
      (find-class 'typechecking-writer-method) nil
      (find-class 'typechecking-writer-class) nil
      (find-class 'testclass26) nil
      (find-class 'uncallable-generic-function) nil
      (find-class 'counted27-class) nil
      (find-class 'testclass27a) nil
      (find-class 'testclass27b) nil
      (find-class 'testclass52a) nil
      (find-class 'testclass52c) nil
      (find-class 'testclass52d) nil
      (find-class 'testclass52b) nil
      (find-class 'sampclass01) nil
      (find-class 'sampclass02) nil
      (find-class 'sampclass03) nil
      (find-class 'sampclass04) nil
      (find-class 'sampclass05) nil
      (find-class 'sampclass06) nil
      (find-class 'sampclass07) nil
      (find-class 'sampclass08) nil
      (find-class 'sampclass09) nil
      (find-class 'sampclass10) nil
      (find-class 'sampclass11) nil
      (find-class 'sampclass12) nil
      (find-class 'sampclass13) nil
      (find-class 'sampclass14) nil
      (find-class 'sampclass15) nil
      (find-class 'sampclass16) nil
      (find-class 'sampclass17) nil
      (find-class 'sampclass18) nil
      (find-class 'sampclass19) nil
      (find-class 'sampclass20) nil
      (find-class 'sampclass21) nil
      (find-class 'sampclass22) nil
      (find-class 'sampclass23) nil
      (find-class 'sampclass24) nil
      (find-class 'sampclass25) nil
      (find-class 'sampclass26) nil
      (find-class 'sampclass27) nil
      (find-class 'sampclass28) nil
      (find-class 'sampclass29) nil
      (find-class 'sampclass30) nil
      (find-class 'sampclass31) nil
      (find-class 'sampclass32) nil
      (find-class 'sampclass33) nil
      (find-class 'sampclass34) nil
      (find-class 'sampclass35) nil
      (find-class 'sampclass36) nil
      (find-class 'sampclass37) nil
      (find-class 'sampclass38) nil
      (find-class 'sampclass39) nil
      (find-class 'sampclass39) nil
      (find-class 'testgenericfunction142) nil
      (find-class 'testgenericfunction143) nil
      (find-class 'custom-method) nil
      (find-class 'customized5-generic-function) nil
      (find-class 'customized6-generic-function) nil
      (find-class 'customized1-generic-function) nil
      (find-class 'customized2-generic-function) nil
      (find-class 'customized3-generic-function) nil
      (find-class 'typechecked-slot-definition) nil
      (find-class 'typechecked-slot-definition-class) nil
      (find-class 'testclass28) nil
      (find-class 'class-supporting-classof-slots) nil
      (find-class 'classof-direct-slot-definition-mixin) nil
      (find-class 'classof-effective-slot-definition-mixin) nil
      (find-class 'testclass29a) nil
      (find-class 'testclass29b) nil
      (find-class 'virtual-class) nil
      (find-class 'virtual-base-class) nil
      (find-class 'virtual-table-direct-slot-definition) nil
      (find-class 'virtual-table-effective-slot-definition) nil
      (find-class 'virtual-generic-function) nil
      (find-class 'virtual-generic-function-updater) nil
      (find-class 'testclass30a) nil
      (find-class 'testclass30b) nil
      (find-class 'testclass30c) nil
      (find-class 'testclass30d) nil)
NIL
