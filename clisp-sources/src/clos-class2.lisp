;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part 2: The class namespace.
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;;; Predefined classes (see ANSI CL 4.3.7.):

;; Metaclasses:
(defvar <potential-class>)             ; <standard-class>
(defvar <defined-class>)               ; <standard-class>
(defvar <standard-class>)              ; <standard-class>
(defvar <funcallable-standard-class>)  ; <standard-class>
(defvar <structure-class>)             ; <standard-class>
(defvar <built-in-class>)              ; <standard-class>
;; Not really metaclasses:
(defvar <forward-reference-to-class>)  ; <standard-class>
(defvar <misdesigned-forward-referenced-class>) ; <standard-class>
;; Classes:
(defvar <standard-object>)             ; <standard-class>
(defvar <funcallable-standard-object>) ; <funcallable-standard-class>
(defvar <structure-object>)            ; <structure-class>
(defvar <generic-function>)            ; <funcallable-standard-class>
(defvar <standard-generic-function>)   ; <funcallable-standard-class> ; ABI
;(defvar <method>)                     ; <standard-class>
;(defvar <standard-method>)            ; <standard-class>
(defvar <standard-reader-method>)      ; <standard-class>
(defvar <standard-writer-method>)      ; <standard-class>
;(defvar <method-combination>)         ; <standard-class>
(defvar <array>)                       ; <built-in-class>
(defvar <bit-vector>)                  ; <built-in-class>
(defvar <character>)                   ; <built-in-class>
(defvar <complex>)                     ; <built-in-class>
(defvar <cons>)                        ; <built-in-class>
(defvar <float>)                       ; <built-in-class>
(defvar <function>)                    ; <built-in-class>
(defvar <hash-table>)                  ; <built-in-class>
(defvar <integer>)                     ; <built-in-class>
(defvar <list>)                        ; <built-in-class>
(defvar <null>)                        ; <built-in-class>
(defvar <number>)                      ; <built-in-class>
(defvar <package>)                     ; <built-in-class>
(defvar <pathname>)                    ; <built-in-class>
#+LOGICAL-PATHNAMES
(defvar <logical-pathname>)            ; <built-in-class>
(defvar <random-state>)                ; <built-in-class>
(defvar <ratio>)                       ; <built-in-class>
(defvar <rational>)                    ; <built-in-class>
(defvar <readtable>)                   ; <built-in-class>
(defvar <real>)                        ; <built-in-class>
(defvar <sequence>)                    ; <built-in-class>
(defvar <stream>)                      ; <built-in-class>
(defvar <file-stream>)                 ; <built-in-class>
(defvar <synonym-stream>)              ; <built-in-class>
(defvar <broadcast-stream>)            ; <built-in-class>
(defvar <concatenated-stream>)         ; <built-in-class>
(defvar <two-way-stream>)              ; <built-in-class>
(defvar <echo-stream>)                 ; <built-in-class>
(defvar <string-stream>)               ; <built-in-class>
(defvar <string>)                      ; <built-in-class>
(defvar <symbol>)                      ; <built-in-class>
(defvar <t>)                           ; <built-in-class>
(defvar <vector>)                      ; <built-in-class>
;; Condition classes and RESTART are defined later, in condition.lisp.


;;; Global management of classes and their names:

#|| ; see PREDTYPE.D
 (defun find-class (symbol &optional (errorp t) environment)
   (declare (ignore environment)) ; ignore distinction between
                                  ; compile-time and run-time environment
   (unless (symbolp symbol)
     (error-of-type 'type-error
       :datum symbol :expected-type 'symbol
       (TEXT "~S: argument ~S is not a symbol")
       'find-class symbol))
   (let ((class (get symbol 'CLOSCLASS)))
     (if (not (defined-class-p class))
       (if errorp
         (error-of-type 'error
           (TEXT "~S: ~S does not name a class")
           'find-class symbol)
         nil)
       class)))
||#

(defun (setf find-class) (new-value symbol &optional errorp environment)
  (declare (ignore errorp environment)) ; ignore distinction between
                                        ; compile-time and run-time environment
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      '(setf find-class) symbol))
  (unless (or (null new-value) (defined-class-p new-value))
    (error-of-type 'type-error
      :datum new-value :expected-type 'class
      (TEXT "~S: ~S is not a class")
      '(setf find-class) new-value))
  (let ((h (get symbol 'CLOSCLASS)))
    (when (defined-class-p h)
      (when (and (built-in-class-p h) (eq (class-name h) symbol)) ; protect structure classes, too??
        (error-of-type 'error
          (TEXT "~S: cannot redefine built-in class ~S")
          '(setf find-class) h)))
    ;; Should we do (setf (class-name h) nil) ? No, because CLHS of FIND-CLASS
    ;; says that "the class object itself is not affected".
    (sys::check-redefinition symbol '(setf find-class)
                             (and (defined-class-p h) "class"))
    (when (and h (forward-reference-to-class-p h) new-value)
      ;; Move the list of subclasses from the old class object to the new one.
      (dolist (subclass (class-direct-subclasses h))
        (add-direct-subclass new-value subclass))))
  (if new-value
    (setf (get symbol 'CLOSCLASS) new-value)
    (progn (remprop symbol 'CLOSCLASS) nil)))

;; Converts a class to a pretty printing type.
(defun class-pretty (class)
  (if (or (forward-reference-to-class-p class)
          (eq (find-class (class-name class) nil) class))
    (class-name class)
    class))
