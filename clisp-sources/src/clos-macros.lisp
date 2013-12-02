;;;; Common Lisp Object System for CLISP
;;;; Internal Macros
;;;; Bruno Haible 2004

(in-package "CLOS")

;;; ===========================================================================

;;; Support for weak sets that are stored as either
;;; - NIL or a weak-list (for saving memory when there are few subclasses), or
;;; - a weak-hash-table (for speed when there are many subclasses).

;;; (def-weak-set-accessors ACCESSOR ELEMENT-TYPE
;;;   ADDER REMOVER LISTER)
;;; defines three functions
;;;   (defun ADDER (holder element) ...) ; adds element to the set
;;;   (defun REMOVER (holder element) ...) ; removes element from the set
;;;   (defun LISTER (holder) ...) ; returns the set as a freshly consed list

(defmacro def-weak-set-accessors (accessor element-type adder remover lister)
  `(PROGN
     (DEFUN ,adder (HOLDER ELEMENT)
       (ADD-TO-WEAK-SET HOLDER (,accessor HOLDER) ELEMENT
                        #'(SETF ,accessor) ',element-type))
     (DEFUN ,remover (HOLDER ELEMENT)
       (REMOVE-FROM-WEAK-SET HOLDER (,accessor HOLDER) ELEMENT))
     (DEFUN ,lister (HOLDER)
       (LIST-WEAK-SET (,accessor HOLDER)))))

;; Auxiliary functions for def-weak-set-accessors.

(defun add-to-weak-set (holder set element setter element-type)
  (cond ((null set)
         (let ((new-set (ext:make-weak-list (list element))))
           (funcall setter new-set holder)))
        ((ext:weak-list-p set)
         (let ((list (ext:weak-list-list set)))
           (unless (member element list :test #'eq)
             (push element list)
             (if (<= (length list) 10)
               (setf (ext:weak-list-list set) list)
               (let ((new-set
                       (let ((ht (make-hash-table :key-type element-type :value-type '(eql t)
                                                  :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t
                                                  :weak :key)))
                         (dolist (x list) (setf (gethash x ht) t))
                         ht)))
                 (funcall setter new-set holder))))))
        (t (setf (gethash element set) t))))

(defun remove-from-weak-set (holder set element)
  (declare (ignore holder))
  (cond ((null set))
        ((ext:weak-list-p set)
         (let ((list (ext:weak-list-list set)))
           (when (member element list :test #'eq)
             (setf (ext:weak-list-list set) (remove element list :test #'eq)))))
        (t (remhash element set))))

(defun list-weak-set (set)
  (cond ((null set) '())
        ((ext:weak-list-p set)
         (ext:weak-list-list set))
        (t (let ((l '()))
             (maphash #'(lambda (x y) (declare (ignore y)) (push x l)) set)
             l))))

;;; ===========================================================================

;; Typecheck in accessor functions that are not generic functions.
;; (accessor-typecheck object class caller)
;; > object: a form producing any object.
;; > class: a form producing a class or a class name.
;; > caller: a form producing the accessor's name, usually a quoted symbol.
(defmacro accessor-typecheck (object class caller)
  `(UNLESS (TYPEP ,object ,class)
     (ERROR-ACCESSOR-TYPECHECK ,caller ,object ,class)))

;; Auxiliary function for non-generic accessors.
(defun error-accessor-typecheck (caller object class) ; ABI
  (error-of-type 'type-error
    :datum object :expected-type class
    "~S: The argument is not of type ~S: ~S"
    caller
    (if (and (defined-class-p class) (eq (find-class (class-name class) nil) class))
      (class-name class)
      class)
    object))
