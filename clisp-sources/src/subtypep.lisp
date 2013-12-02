;;;; SUBTYPEP
;;;; Bruno Haible 2004-03-28, 2004-04-03

;; SUBTYPEP is very powerful. It can tell:
;; - whether a type is empty:
;;     (subtypep a 'NIL)
;; - whether two types are disjoint:
;;     (subtypep `(AND ,a ,b) 'NIL)
;; - whether two types are equal:
;;     (and (subtypep a b) (subtypep b a))
;; - whether an object is an element of a given type:
;;     (subtypep `(eql ,x) a)
;; - whether two objects are identical:
;;     (subtypep `(eql ,x) `(eql ,y))
;; - whether a number is positive:
;;     (subtypep `(eql ,x) '(real (0) *))
;; and much more.

(in-package "SYSTEM")

;;; SUBTYPEP
;;; After many hacked attempts that always failed to recognize complicated
;;; cases like the equivalence of
;;;   (INTEGER 0 3)                       (MEMBER 0 3 1 2)
;;;   (AND (INTEGER 0 4) (NOT (EQL 2)))   (MEMBER 0 3 1 4)
;;;   (AND (RATIONAL -1 1) (NOT INTEGER)) (AND (OR (RATIONAL -1 (0)) (RATIONAL (0) 1)) (NOT INTEGER))
;;; here finally comes an implementation of the approach presented in
;;;   Henry G. Baker: A Decision Procedure for Common Lisp's SUBTYPEP Predicate
;;;   Lisp and Symbolic Computation 5,3 (Sept. 1992), 157-190.
;;; The main idea is to use AND/OR logic to reduce the problem to 17 disjoint
;;; categories of types, and implement this logic correctly for each of the 17
;;; categories.
;;; The general outline of the decision procedure:
;;; 1. Do some superficial simplification and error checking:
;;;    CANONICALIZE-TYPE.
;;; 2. Flatten the ANDs and ORs of the outermost level and reduce the
;;;    problem to  (SUBTYPEP `(AND ,a1 ... ,am) `(OR ,b1 ... ,bn)).
;;; 3. Remove MEMBER and EQL specifiers from a1, ..., am, b1, ..., bn.
;;; 4. Classify a1, ..., am, b1, ..., bn into one of the 17 categories.
;;; 5. In each of the 17 categories, simplify the AND part and the OR part
;;;    to an appropriate canonical representation (for example, for REAL,
;;;    an ordered list of intervals).
;;; 6. Decide the SUBTYPEP relationship in the particular category.

;;; SUBTYPEP and (function ...) types
;;;
;;; How is SUBTYPEP on specific function types meant to work?
;;;
;;; HyperSpec/Body/syscla_function.html says:
;;;
;;;   "The list form of the function type-specifier can be used only for
;;;    declaration and not for discrimination."
;;;
;;; So this means, the function call
;;;
;;;   (typep #'cons '(function (t t) cons))
;;;
;;; is invalid. But HyperSpec/Body/fun_subtypep.html talks about what
;;; may happen when (function ...) is passed to SUBTYPEP. So the result of
;;;
;;;   (subtypep '(eql #.#'cons) '(function (t t) cons))
;;;
;;; must be either nil;nil (means unknown) or t;t (means true).
;;;
;;; Now, what about
;;;
;;;   (defun covariant ()
;;;     (subtypep '(function (integer integer) cons)
;;;               '(function (t t) cons)))
;;;
;;;   (defun contravariant ()
;;;     (subtypep '(function (t t) cons)
;;;               '(function (integer integer) cons)))
;;;
;;; Can either of these functions ever return true? In other words, are the
;;; argument types in function types covariant or contravariant?
;;;
;;; Since a function that accepts any objects as arguments also accepts,
;;; integers but not versa, the interpretation of a type as a "set of objects"
;;; suggests that
;;;   (covariant)     => nil;t or nil;nil
;;;   (contravariant) => t;t or nil;nil
;;;
;;; But type relationships can also be defined through declarations: According
;;; to HyperSpec/Body/dec_type.html, (subtypep A B) is true if and only if
;;;
;;;   (locally (declare (type A x)) (declare (type B x))
;;;     (form ...))
;;;
;;; is equivalent to
;;;
;;;   (locally (declare (type A x))
;;;     (form ...))
;;;
;;; When we transpose this to function types, (subtypep A B) should be true if
;;; and only if
;;;
;;;   (locally (declare (ftype A x)) (declare (ftype B x))
;;;     (form ...))
;;;
;;; is equivalent to
;;;
;;;   (locally (declare (ftype A x))
;;;     (form ...))
;;;
;;; Using the definition of the semantics of FTYPE declarations in
;;; HyperSpec/Body/syscla_function.html we arrive to the conclusion that
;;;
;;;   (locally (declare (ftype (function (t t) cons) x))
;;;            (declare (ftype (function (integer integer) cons)))
;;;     (form ...))
;;;
;;; is equivalent to
;;;
;;;   (locally (declare (ftype (function (integer integer) cons)))
;;;     (form ...))
;;;
;;; hence
;;;
;;;   (covariant)     => t;t or nil;nil
;;;   (contravariant) => nil;t or nil;nil
;;;
;;; In summary, the view of a type as a "set of objects" leads to a view of
;;; subtypes that is opposite to the view of a function type as describing
;;; function calls. Since "the function type-specifier can be used only for
;;; declaration and not for discrimination" the first view is not the correct
;;; one. Rather the second view should be used.
;;;
;;; However, I believe the contradiction persists because the view of a type
;;; as a "set of objects" is so fundamental.
;;;
;;; Implementations like SBCL, CMUCL use the second view:
;;;
;;;   (covariant)     => t;t
;;;   (contravariant) => nil;t
;;;
;;; Implementations like ACL 6.2 and LispWorks 4.3 punt and return nil;nil.
;;; We do the same.

;;; Defines a type category.
;;; (DEF-TYPE-CATEGORY name atomic-type-specifiers list-type-specifiers
;;;                    and-simplifier or-simplifier subtype-decider)
;;; The name denotes the type category.
;;; The atomic-type-specifiers and list-type-specifiers are lists of symbols.
;;; The and-simplifier, or-simplifier, subtype-decider are lambda expressions.
;;; and-simplifier: Simplifies `(AND ,@parts). Takes a nonempty list of
;;;                 elementary type specifiers, returns a type specifier.
;;;                 Can, but does not need to, return NIL if `(AND ,@parts)
;;;                 is the empty type.
;;; or-simplifier: Simplifies `(OR ,@parts). Takes a list of elementary type
;;;                specifiers, returns a list of elementary type specifiers.
;;; subtype-decider: Decides whether type1 is a subtype of `(OR ,@type2parts).
;;;                  Takes a result of and-simplifier (but not NIL), a result
;;;                  of or-simplifier, and a boolean telling whether an attempt
;;;                  should be made to prove (no) when a (yes) cannot be proven.
;;;                  Returns two values, with the same meaning as in SUBTYPEP.
;;;                  The macros (yes), (no), (unknown) can be used here. (no)
;;;                  may only be returned if the third argument try-prove-no is
;;;                  true.
(defmacro def-type-category (name atomic-type-specifiers list-type-specifiers
                             and-simplifier or-simplifier subtype-decider)
  (let ((and-simplifier-name
          (intern (string-concat "SIMPLIFY-AND-" (symbol-name name)) "SYSTEM"))
        (or-simplifier-name
          (intern (string-concat "SIMPLIFY-OR-" (symbol-name name)) "SYSTEM"))
        (subtype-decider-name
          (intern (string-concat "SUBTYPEP-" (symbol-name name)) "SYSTEM")))
    `(PROGN
       ,@(if (symbol-package name)
           `((EVAL-WHEN (COMPILE LOAD EVAL)
               (SETF (GET ',name 'SUBTYPEP-ATOM) ',atomic-type-specifiers)
               (SETF (GET ',name 'SUBTYPEP-LIST) ',list-type-specifiers)
               (SETQ *type-categories* (UNION *type-categories* '(,name)))
         )  ))
       (DEFUN ,and-simplifier-name ,(cadr and-simplifier)
         ,@(cddr and-simplifier))
       (DEFUN ,or-simplifier-name ,(cadr or-simplifier)
         ,@(cddr or-simplifier))
       ,(multiple-value-bind (body-rest declarations)
            (sys::parse-body (cddr subtype-decider))
          `(DEFUN ,subtype-decider-name ,(cadr subtype-decider)
             ,@(if declarations `((DECLARE ,@declarations)))
             (MACROLET ((YES () '(RETURN-FROM ,subtype-decider-name (VALUES T T)))
                        (NO () '(RETURN-FROM ,subtype-decider-name (VALUES NIL T)))
                        (UNKNOWN () '(RETURN-FROM ,subtype-decider-name (VALUES NIL NIL))))
               ,@body-rest)))
       ,@(if (symbol-package name)
           `((SETF (GET ',name 'SUBTYPEP-SIMPLIFY-AND) ',and-simplifier-name)
             (SETF (GET ',name 'SUBTYPEP-SIMPLIFY-OR) ',or-simplifier-name)
             (SETF (GET ',name 'SUBTYPEP-DECIDE) ',subtype-decider-name)
         )  )
       ',name
     )
) )
;; Defines a type category for a type which has no special subtypes except
;; NIL and itself (ignoring SATISFIES types).
(defmacro def-misc-type-category (name)
  `(PROGN
     (EVAL-WHEN (COMPILE LOAD EVAL)
       (SETF (GET ',name 'SUBTYPEP-ATOM) '(,name))
       (SETF (GET ',name 'SUBTYPEP-LIST) '())
       (SETQ *type-categories* (UNION *type-categories* '(,name)))
     )
     (SETF (GET ',name 'SUBTYPEP-SIMPLIFY-AND) 'SIMPLIFY-AND-MISC)
     (SETF (GET ',name 'SUBTYPEP-SIMPLIFY-OR) 'SIMPLIFY-OR-MISC)
     (SETF (GET ',name 'SUBTYPEP-DECIDE) 'SUBTYPEP-MISC)
     ',name
   )
)
(eval-when (compile load eval)
  (defvar *type-categories* '())
)

(def-type-category ARRAY (ARRAY SIMPLE-ARRAY) (ARRAY SIMPLE-ARRAY)
  ;; Each part is of the form ([SIMPLE-]ARRAY &optional el-type dims).
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (let ((final-kind 'ARRAY) (final-eltype '*) (final-dims '*))
      (dolist (type parts `(,final-kind ,final-eltype ,final-dims))
        (when (or (eq type 'SIMPLE-ARRAY)
                  (and (consp type) (eq (first type) 'SIMPLE-ARRAY)))
          (setq final-kind 'SIMPLE-ARRAY))
        (when (and (consp type) (rest type))
          (let ((eltype (second type)))
            (unless (eq eltype '*)
              (setq eltype (upgraded-array-element-type eltype))
              ; Constraint: All upgraded-eltypes must be the same.
              ; Here we use the fact that the upgraded-array-element-type
              ; return values are so simple that they can be compared with
              ; EQUAL.
              (if (eq final-eltype '*)
                (setq final-eltype eltype)
                (unless (equal eltype final-eltype)
                  ; Contradictory element-types.
                  (return 'NIL))))))
        (when (and (consp type) (cddr type))
          (let ((dims (third type)))
            (unless (eq dims '*)
              (if (eq final-dims '*)
                (setq final-dims dims)
                (if (listp dims)
                  (if (listp final-dims)
                    (if (eql (length dims) (length final-dims))
                      (setq final-dims
                            (mapcar #'(lambda (d1 d2)
                                        (if (integerp d2)
                                          (if (integerp d1)
                                            (if (eql d1 d2)
                                              d1
                                              ; Contradictory dimension constraint.
                                              (return 'NIL))
                                            d2)
                                          d1))
                                    final-dims dims))
                      ; Contradictory dimensions constraint.
                      (return 'NIL))
                    (if (eql (length dims) final-dims)
                      (setq final-dims dims)
                      ; Contradictory dimensions constraint.
                      (return 'NIL)))
                  (unless (eql dims (if (listp final-dims) (length final-dims) final-dims))
                    ; Contradictory dimensions constraint.
                    (return 'NIL))))))))))
  (lambda (parts)
    parts)
  (lambda (type1 type2parts try-prove-no)
    (let (kind1 eltype1 dims1)
      (if (atom type1)
        (setq kind1 type1
              eltype1 '*
              dims1 '*)
        (setq kind1 (first type1)
              eltype1 (if (rest type1) (second type1) '*)
              dims1 (if (cddr type1) (third type1) '*)))
      (assert (member kind1 '(ARRAY SIMPLE-ARRAY)))
      (assert (or (eq eltype1 '*)
                  (equal eltype1 (upgraded-array-element-type eltype1))))
      (let ((uncovered-eltypes
              ; The possible results of upgraded-array-element-type.
              '(NIL CHARACTER BIT (UNSIGNED-BYTE 2) (UNSIGNED-BYTE 4)
                (UNSIGNED-BYTE 8) (UNSIGNED-BYTE 16) (UNSIGNED-BYTE 32) T)))
        (dolist (type2 type2parts)
          (let (kind2 eltype2 dims2)
            (if (atom type2)
              (setq kind2 type2
                    eltype2 '*
                    dims2 '*)
              (setq kind2 (first type2)
                    eltype2 (if (rest type2) (second type2) '*)
                    dims2 (if (cddr type2) (third type2) '*)))
            (assert (member kind2 '(ARRAY SIMPLE-ARRAY)))
            (when
              (and (or (eq kind2 'ARRAY) (eq kind2 kind1))
                   (or (eq dims2 '*)
                       (if (listp dims1)
                         (if (listp dims2)
                           (and (eql (length dims1) (length dims2))
                                (every #'(lambda (a b) (or (eq b '*) (eql a b)))
                                       dims1 dims2))
                           (eql (length dims1) dims2))
                         (if (listp dims2)
                           (and (eql dims1 (length dims2))
                                (every #'(lambda (b) (eq b '*)) dims2))
                           (eql dims1 dims2)))))
              (if (eq eltype2 '*)
                (yes)
                (let ((eltype2 (upgraded-array-element-type eltype2)))
                  (if (eq eltype1 '*)
                    ;; type2 does not cover all of type1 but only a significant
                    ;; portion of it. Keep track which portions are covered.
                    (setq uncovered-eltypes
                      (remove eltype2 uncovered-eltypes :test #'equal))
                    (when (equal eltype1 eltype2)
                      (yes))))))))
        (when (null uncovered-eltypes)
          ;; eltype1 was *, and the type2parts cover all possible array element
          ;; types.
          (yes)))
      (when try-prove-no
        ;; We could construct a testimony using MAKE-ARRAY.
        ;; Don't test eltype1 against NIL, since we have arrays with element
        ;; type NIL.
        (when (or (eq dims1 '*)
                  (if (listp dims1)
                    (and (< (length dims1) #,array-rank-limit)
                         (every #'(lambda (a) (or (eq a '*) (< a #,array-dimension-limit)))
                                dims1)
                         (or (member '* dims1)
                             (< (reduce #'* dims1) #,array-total-size-limit)))
                    (< dims1 #,array-rank-limit)))
          (no)))
      (unknown)))
)

(def-type-category COMPLEX (COMPLEX) (COMPLEX)
  ;; Each part is of the form (COMPLEX &optional rtype itype).
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (let ((final-rtype '*) (final-itype '*))
      (dolist (type parts)
        (when (and (consp type) (rest type))
          (let* ((rtype (second type))
                 (itype (if (cddr type) (third type) rtype)))
            (unless (eq rtype '*)
              (setq rtype (upgraded-complex-part-type rtype))
              (when (eq final-rtype '*) (setq final-rtype 'REAL))
              (setq final-rtype `(AND ,final-rtype ,rtype)))
            (unless (eq itype '*)
              (setq itype (upgraded-complex-part-type itype))
              (when (eq final-itype '*) (setq final-itype 'REAL))
              (setq final-itype `(AND ,final-itype ,itype))))))
      (if (or (and (not (eq final-rtype '*)) (subtypep final-rtype 'NIL))
              (and (not (eq final-itype '*)) (subtypep final-itype 'NIL)))
        ; Contradictory type constraint.
        'NIL
        (if (and (eq final-rtype '*) (eq final-itype '*))
          'COMPLEX
          `(COMPLEX ,final-rtype ,final-itype)))))
  (lambda (parts)
    parts)
  (lambda (type1 type2parts try-prove-no)
    (let (rtype1 itype1)
      (if (atom type1)
        (setq rtype1 '*
              itype1 '*)
        (setq rtype1 (if (rest type1) (second type1) '*)
              itype1 (if (cddr type1) (third type1) '*)))
      (dolist (type2 type2parts)
        (let (rtype2 itype2)
          (if (atom type2)
            (setq rtype2 '*
                  itype2 '*)
            (setq rtype2 (if (rest type2) (second type2) '*)
                  itype2 (if (cddr type2) (third type2) '*)))
          (when
            (and (or (eq rtype2 '*)
                     (subtypep (if (eq rtype1 '*) 'REAL `(AND REAL ,rtype1))
                               (upgraded-complex-part-type `(AND REAL ,rtype2))))
                 (or ; shortcut to avoid calling subtypep a second time
                     (and (eq itype1 rtype1) (eq itype2 rtype2))
                     (eq itype2 '*)
                     (subtypep (if (eq itype1 '*) 'REAL `(AND REAL ,itype1))
                               (upgraded-complex-part-type `(AND REAL ,itype2)))))
            (yes))))
      (when try-prove-no
        ;; It's too hard to prove a (no) if there is more than one type2.
        (when (<= (length type2parts) 1)
          (multiple-value-bind (remptyis remptyknown)
              (if (eq rtype1 '*)
                (values nil t)
                (subtypep `(AND REAL ,rtype1) 'NIL))
            (multiple-value-bind (iemptyis iemptyknown)
                (if (eq itype1 '*)
                  (values nil t)
                  (subtypep `(AND REAL ,itype1) 'NIL))
              (case (length type2parts)
                (0 (when (and remptyknown (not remptyis)
                              iemptyknown (not iemptyis))
                     (no)))
                (1
                  (let ((type2 (first type2parts)) rtype2 itype2)
                    (if (atom type2)
                      (setq rtype2 '*
                            itype2 '*)
                      (setq rtype2 (if (rest type2) (second type2) '*)
                            itype2 (if (cddr type2) (third type2) '*)))
                    (multiple-value-bind (rsubis rsubknown)
                        (if (eq rtype2 '*)
                          (values t t)
                          (subtypep (if (eq rtype1 '*) 'REAL `(AND REAL ,rtype1))
                                    (upgraded-complex-part-type `(AND REAL ,rtype2))))
                      (multiple-value-bind (isubis isubknown)
                          (if (eq itype2 '*)
                            (values t t)
                            (subtypep (if (eq itype1 '*) 'REAL `(AND REAL ,itype1))
                                      (upgraded-complex-part-type `(AND REAL ,itype2))))
                        (when (or (and rsubknown (not rsubis)
                                       iemptyknown (not iemptyis))
                                  (and isubknown (not isubis)
                                       remptyknown (not remptyis)))
                          (no)))))))))))
        (unknown)))
)

(def-type-category CONS (CONS) (CONS)
  ;; Each part is of the form (CONS &optional cartype cdrtype).
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (let ((final-cartype '*) (final-cdrtype '*))
      (dolist (type parts)
        (when (and (consp type) (rest type))
          (let ((cartype (second type)))
            (unless (eq cartype '*)
              (setq final-cartype
                    (if (eq final-cartype '*)
                      cartype
                      `(AND ,final-cartype ,cartype)))))
          (when (cddr type)
            (let ((cdrtype (third type)))
              (unless (eq cdrtype '*)
                (setq final-cdrtype
                      (if (eq final-cdrtype '*)
                        cdrtype
                        `(AND ,final-cdrtype ,cdrtype))))))))
      (if (or (and (not (eq final-cartype '*)) (subtypep final-cartype 'NIL))
              (and (not (eq final-cdrtype '*)) (subtypep final-cdrtype 'NIL)))
        ; Contradictory type constraint.
        'NIL
        (if (and (eq final-cartype '*) (eq final-cdrtype '*))
          'CONS
          `(CONS ,final-cartype ,final-cdrtype)))))
  (lambda (parts)
    parts)
  (lambda (type1 type2parts try-prove-no)
    (let (cartype1 cdrtype1)
      (if (atom type1)
        (setq cartype1 '*
              cdrtype1 '*)
        (setq cartype1 (if (rest type1) (second type1) '*)
              cdrtype1 (if (cddr type1) (third type1) '*)))
      (dolist (type2 type2parts)
        (let (cartype2 cdrtype2)
          (if (atom type2)
            (setq cartype2 '*
                  cdrtype2 '*)
            (setq cartype2 (if (rest type2) (second type2) '*)
                  cdrtype2 (if (cddr type2) (third type2) '*)))
          (when
            (and (or (eq cartype2 '*)
                     (subtypep (if (eq cartype1 '*) 'T cartype1) cartype2))
                 (or (eq cdrtype2 '*)
                     (subtypep (if (eq cdrtype1 '*) 'T cdrtype1) cdrtype2)))
            (yes))))
      (when try-prove-no
        ;; It's too hard to prove a (no) if there is more than one type2.
        (when (<= (length type2parts) 1)
          (multiple-value-bind (caremptyis caremptyknown)
              (if (eq cartype1 '*) (values nil t) (subtypep cartype1 'NIL))
            (multiple-value-bind (cdremptyis cdremptyknown)
                (if (eq cdrtype1 '*) (values nil t) (subtypep cdrtype1 'NIL))
              (case (length type2parts)
                (0 (when (and caremptyknown (not caremptyis)
                              cdremptyknown (not cdremptyis))
                     (no)))
                (1
                  (let ((type2 (first type2parts)) cartype2 cdrtype2)
                    (if (atom type2)
                      (setq cartype2 '*
                            cdrtype2 '*)
                      (setq cartype2 (if (rest type2) (second type2) '*)
                            cdrtype2 (if (cddr type2) (third type2) '*)))
                    (multiple-value-bind (carsubis carsubknown)
                        (if (eq cartype2 '*)
                          (values t t)
                          (subtypep (if (eq cartype1 '*) 'T cartype1) cartype2))
                      (multiple-value-bind (cdrsubis cdrsubknown)
                          (if (eq cdrtype2 '*)
                            (values t t)
                            (subtypep (if (eq cdrtype1 '*) 'T cdrtype1) cdrtype2))
                        (when (or (and carsubknown (not carsubis)
                                       cdremptyknown (not cdremptyis))
                                  (and cdrsubknown (not cdrsubis)
                                       caremptyknown (not caremptyis)))
                          (no)))))))))))
      (unknown)))
)

(def-type-category REAL
  (REAL RATIONAL INTEGER FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
  (INTERVALS)
  ;; Each part is of the form (kind &optional lo-bound hi-bound).
  ;; The canonical representation is (INTERVALS kind [lo-bound hi-bound]+), a
  ;; non-empty list of disjoint intervals, in ascending order. This allows
  ;; grouping nonadjacent intervals of the same kind together, and also allows
  ;; removing single elements from an interval without too much list surgery.
  ;; If the kind is FLOAT or LONG-FLOAT, the bounds can be rational numbers
  ;; instead of floating-point numbers; this is needed because we don't have
  ;; a floating-point type of maximal precision.
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (block simplify
      (let ((final-kind 'REAL))
        (dolist (type parts)
          (let ((kind (if (atom type) type (second type))))
            (if (or ; Exact and inexact types are disjoint.
                    (and (member final-kind '(RATIONAL INTEGER))
                         (member kind '(FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)))
                    (and (member final-kind '(FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
                         (member kind '(RATIONAL INTEGER)))
                    ; The four different floating-point types are disjoint.
                    (and (member final-kind '(SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
                         (member kind '(SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
                         (not (eq kind final-kind))))
              (return-from simplify 'NIL)
              (if (real-types-subtypep kind final-kind)
                (setq final-kind kind)
                (assert (real-types-subtypep final-kind kind))))))
        ;; Map the parts all to final-kind, make all INTEGER bounds inclusive,
        ;; and return NIL if any of the parts is empty.
        (setq parts
              (mapcar #'(lambda (type)
                          (let ((intervals (if (atom type) '(* *) (cddr type))))
                            (setq intervals (intervals-mapto intervals final-kind))
                            (when (null intervals)
                              (return-from simplify 'NIL))
                            `(INTERVALS ,final-kind ,@intervals)))
                      parts))
        ;; Intersect the interval ranges.
        (let ((intervals (reduce #'intervals-intersection parts :key #'cddr)))
          (if intervals
            `(INTERVALS ,final-kind ,@intervals)
            'NIL)))))
  (lambda (parts)
    ;; Make all INTEGER bounds inclusive, and remove empty parts.
    (setq parts
          (mapcan #'(lambda (type)
                      (let ((kind (if (atom type) type (second type)))
                            (intervals (if (atom type) '(* *) (cddr type))))
                        (setq intervals (intervals-mapto intervals kind))
                        (if intervals
                          (list `(INTERVALS ,kind ,@intervals))
                          '())))
                  parts))
    ;; Merge those parts together that belong to the same kind. The parts
    ;; list thus becomes a kind of alist, indexed by the number kind.
    (let ((new-parts '()))
      (dolist (type parts)
        (let* ((kind (second type))
               (other (find kind new-parts :key #'second)))
          (if other
            (setf (cddr other) (intervals-union (cddr other) (cddr type) kind))
            (push (list* 'INTERVALS kind (cddr type)) new-parts))))
      (nreverse new-parts)))
  (lambda (type1 type2parts try-prove-no)
    (let ((kind1 (second type1))
          (intervals1 (cddr type1)))
      (dolist (type2 type2parts)
        (let ((kind2 (second type2))
              (intervals2 (cddr type2)))
          (when (real-types-subtypep kind1 kind2)
            (when (intervals-subtypep intervals1 (intervals-mapto intervals2 kind1))
              (yes)))))
      ;; Handle disjoint unions: REAL == RATIONAL u FLOAT,
      ;; FLOAT == SHORT-FLOAT u SINGLE-FLOAT u DOUBLE-FLOAT u LONG-FLOAT,
      ;; through recursion.
      ;; (Think of (FLOAT 0.0 1.0) and
      ;;    (OR (SHORT-FLOAT 0.0s0 1.0s0) (SINGLE-FLOAT 0.0f0 1.0f0)
      ;;        (DOUBLE-FLOAT 0.0d0 1.0d0) (LONG-FLOAT 0.0L0 1.0L0)) ...)
      (when (and (eq kind1 'REAL)
                 ; No possible win if the type2parts are only REAL.
                 (intersection (mapcar #'second type2parts)
                               '(RATIONAL INTEGER FLOAT SHORT-FLOAT
                                 SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)))
        (when (and (let ((type1a (simplify-and-REAL (list type1 'RATIONAL))))
                     (or (eq type1a 'NIL)
                         (subtypep-REAL type1a type2parts nil)))
                   (let ((type1b (simplify-and-REAL (list type1 'FLOAT))))
                     (or (eq type1b 'NIL)
                         (subtypep-REAL type1b type2parts nil))))
          (yes)))
      (when (and (eq kind1 'FLOAT)
                 ; No possible win if the type2parts are only FLOAT and REAL.
                 (intersection (mapcar #'second type2parts)
                               '(SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT
                                 LONG-FLOAT)))
        (when (and (let ((type1a (simplify-and-REAL (list type1 'SHORT-FLOAT))))
                     (or (eq type1a 'NIL)
                         (subtypep-REAL type1a type2parts nil)))
                   (let ((type1b (simplify-and-REAL (list type1 'SINGLE-FLOAT))))
                     (or (eq type1b 'NIL)
                         (subtypep-REAL type1b type2parts nil)))
                   (let ((type1c (simplify-and-REAL (list type1 'DOUBLE-FLOAT))))
                     (or (eq type1c 'NIL)
                         (subtypep-REAL type1c type2parts nil)))
                   (let ((type1d (simplify-and-REAL (list type1 'LONG-FLOAT))))
                     (or (eq type1d 'NIL)
                         (subtypep-REAL type1d type2parts nil))))
          (yes)))
      ;; Similarly, RATIONAL == RATIO u INTEGER.
      ;; (Think of (RATIONAL -1 1) and
      ;;    (OR (INTEGER * *) (RATIONAL -1 (0)) (RATIONAL (0) 1)) ...)
      (when (and (eq kind1 'RATIONAL)
                 ; No possible win if the type2parts are only RATIONAL and REAL.
                 (intersection (mapcar #'second type2parts) '(INTEGER)))
        (when (and ; Look at the INTEGER part only:
                   (let ((type1a (simplify-and-REAL (list type1 'INTEGER))))
                     (or (eq type1a 'NIL)
                         (subtypep-REAL type1a type2parts nil)))
                   ; Look at the RATIO part only:
                   (let ((type2b (find 'RATIONAL type2parts :key #'second)))
                     (and type2b
                          (subtypep-REAL type1
                                         `((INTERVALS RATIONAL ,@(intervals-integer-closure (cddr type2b))))
                                         nil))))
          (yes)))
      (when try-prove-no
        ;; All checks have already been done above.
        (no))
      (unknown)))
)
;; Subtypep for elements of the list
;; (REAL RATIONAL INTEGER FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT):
(defun real-types-subtypep (kind1 kind2)
  (or (eq kind1 kind2)
      (eq kind2 'REAL)
      (and (eq kind2 'RATIONAL) (eq kind1 'INTEGER))
      (and (eq kind2 'FLOAT)
           (or (eq kind1 'SHORT-FLOAT) (eq kind1 'SINGLE-FLOAT)
               (eq kind1 'DOUBLE-FLOAT) (eq kind1 'LONG-FLOAT)))))
;; Auxiliary functions working on interval lists: ([lo-bound hi-bound]*).
;; Each interval gives rise to 4 variables:
;;   lo-incl lo-excl  -  inclusive and exclusive lower bound,
;;   hi-incl hi-excl  -  inclusive and exclusive upper bound.
;; Missing bounds indicate -infty (for lo) or infty (for hi).
(defun interval-lolo-<= (lo1-incl lo1-excl lo2-incl lo2-excl)
  (if lo1-incl
    (if lo2-incl (<= lo1-incl lo2-incl) (if lo2-excl (<= lo1-incl lo2-excl) nil))
    (if lo1-excl
      (if lo2-incl (< lo1-excl lo2-incl) (if lo2-excl (<= lo1-excl lo2-excl) nil))
      t)))
(defun interval-lolo-< (lo1-incl lo1-excl lo2-incl lo2-excl)
  (if lo1-incl
    (if lo2-incl (< lo1-incl lo2-incl) (if lo2-excl (< lo1-incl lo2-excl) nil))
    (if lo1-excl
      (if lo2-incl (< lo1-excl lo2-incl) (if lo2-excl (< lo1-excl lo2-excl) nil))
      (if (or lo2-incl lo2-excl) t nil))))
(defun interval-hihi-<= (hi1-incl hi1-excl hi2-incl hi2-excl)
  (if hi1-incl
    (if hi2-incl (<= hi1-incl hi2-incl) (if hi2-excl (< hi1-incl hi2-excl) t))
    (if hi1-excl
      (if hi2-incl (<= hi1-excl hi2-incl) (if hi2-excl (<= hi1-excl hi2-excl) t))
      (if (or hi2-incl hi2-excl) nil t))))
(defun interval-hihi-< (hi1-incl hi1-excl hi2-incl hi2-excl)
  (if hi1-incl
    (if hi2-incl (< hi1-incl hi2-incl) (if hi2-excl (< hi1-incl hi2-excl) t))
    (if hi1-excl
      (if hi2-incl (< hi1-excl hi2-incl) (if hi2-excl (< hi1-excl hi2-excl) t))
      nil)))
; Tests whether the interval [lo,hi] is nonempty.
(defun interval-lohi-<= (lo-incl lo-excl hi-incl hi-excl)
  (if lo-incl
    (if hi-incl (<= lo-incl hi-incl) (if hi-excl (< lo-incl hi-excl) t))
    (if lo-excl
      (if hi-incl (< lo-excl hi-incl) (if hi-excl (< lo-excl hi-excl) t))
      t)))
; Tests whether there are numbers not covered by (-infty,hi] u [lo,infty).
(defun interval-hilo-< (hi-incl hi-excl lo-incl lo-excl)
  (if hi-incl
    (if lo-incl (< hi-incl lo-incl) (if lo-excl (< hi-incl lo-excl) nil))
    (if hi-excl
      (if lo-incl (< hi-excl lo-incl) (if lo-excl (<= hi-excl lo-excl) nil))
      nil)))
; Maps an interval list to a given number type.
(defun intervals-mapto (intervals kind)
  (let ((new-intervals '()))
    (do ((l intervals (cddr l)))
        ((endp l))
      (let* ((lo (car l))
             (lo-incl (and (realp lo) lo))
             (lo-excl (and (consp lo) (car lo)))
             (hi (cadr l))
             (hi-incl (and (realp hi) hi))
             (hi-excl (and (consp hi) (car hi))))
        (flet ((mapto (value kind)
                 (case kind
                   (RATIONAL (values (rational value) nil))
                   (INTEGER (values (ceiling value) nil))
                   ((FLOAT LONG-FLOAT)
                    (if (rationalp value)
                      (let ((x (coerce value kind)))
                        (if (= x value)
                          (values x nil)
                          ;; Keep the rational number, because there is
                          ;; no smallest FLOAT > value.
                          (values value t)))
                      (values value nil)))
                   (t (values (coerce value kind) nil)))))
          (cond (lo-incl
                 (multiple-value-bind (x force-excl) (mapto lo-incl kind)
                   (if (or (< x lo-incl) force-excl)
                     (setq lo-excl x lo-incl nil)
                     (setq lo-incl x))))
                (lo-excl
                 (let ((x (mapto lo-excl kind)))
                   (if (<= x lo-excl)
                     (setq lo-excl x)
                     (setq lo-incl x lo-excl nil))))))
        (flet ((mapto (value kind)
                 (case kind
                   (RATIONAL (values (rational value) nil))
                   (INTEGER (values (floor value) nil))
                   ((FLOAT LONG-FLOAT)
                    (if (rationalp value)
                      (let ((x (coerce value kind)))
                        (if (= x value)
                          (values x nil)
                          ;; Keep the rational number, because there is
                          ;; no smallest FLOAT < value.
                          (values value t)))
                      (values value nil)))
                   (t (values (coerce value kind) nil)))))
          (cond (hi-incl
                 (multiple-value-bind (x force-excl) (mapto hi-incl kind)
                   (if (or (> x hi-incl) force-excl)
                     (setq hi-excl x hi-incl nil)
                     (setq hi-incl x))))
                (hi-excl
                 (let ((x (mapto hi-excl kind)))
                   (if (>= x hi-excl)
                     (setq hi-excl x)
                     (setq hi-incl x hi-excl nil))))))
        (when (eq kind 'INTEGER)
          (when lo-excl (setq lo-incl (+ lo-excl 1) lo-excl nil))
          (when hi-excl (setq hi-incl (- hi-excl 1) hi-excl nil)))
        (when (interval-lohi-<= lo-incl lo-excl hi-incl hi-excl)
          (push (if lo-incl lo-incl (if lo-excl `(,lo-excl) '*)) new-intervals)
          (push (if hi-incl hi-incl (if hi-excl `(,hi-excl) '*)) new-intervals))))
    (nreverse new-intervals)))
; Returns the intersection of two interval lists, assuming the same number type.
(defun intervals-intersection (intervals1 intervals2)
  (if (and intervals1 intervals2)
    (let ((new-intervals '())
          lo1-incl lo1-excl hi1-incl hi1-excl
          lo2-incl lo2-excl hi2-incl hi2-excl)
      (macrolet ((advance1 ()
                   '(let ((lo (car intervals1)) (hi (cadr intervals1)))
                      (setq lo1-incl (and (realp lo) lo))
                      (setq lo1-excl (and (consp lo) (car lo)))
                      (setq hi1-incl (and (realp hi) hi))
                      (setq hi1-excl (and (consp hi) (car hi)))
                      (setq intervals1 (cddr intervals1))))
                 (advance2 ()
                   '(let ((lo (car intervals2)) (hi (cadr intervals2)))
                      (setq lo2-incl (and (realp lo) lo))
                      (setq lo2-excl (and (consp lo) (car lo)))
                      (setq hi2-incl (and (realp hi) hi))
                      (setq hi2-excl (and (consp hi) (car hi)))
                      (setq intervals2 (cddr intervals2)))))
        (advance1)
        (advance2)
        (block outer-loop
          (loop
            ;; Search the start of a common interval.
            (loop
              (cond ((not (interval-lohi-<= lo2-incl lo2-excl hi1-incl hi1-excl))
                     (when (null intervals1) (return-from outer-loop))
                     (advance1))
                    ((not (interval-lohi-<= lo1-incl lo1-excl hi2-incl hi2-excl))
                     (when (null intervals2) (return-from outer-loop))
                     (advance2))
                    (t (return))))
            (multiple-value-bind (lo-incl lo-excl)
                (if (interval-lolo-<= lo1-incl lo1-excl lo2-incl lo2-excl)
                  (values lo2-incl lo2-excl)
                  (values lo1-incl lo1-excl))
              (push (if lo-incl lo-incl (if lo-excl `(,lo-excl) '*))
                    new-intervals)
              ;; Seach the end of the common interval.
              (if (interval-hihi-<= hi1-incl hi1-excl hi2-incl hi2-excl)
                (progn
                  (push (if hi1-incl hi1-incl (if hi1-excl `(,hi1-excl) '*))
                        new-intervals)
                  (when (null intervals1) (return-from outer-loop))
                  (advance1))
                (progn
                  (push (if hi2-incl hi2-incl (if hi2-excl `(,hi2-excl) '*))
                        new-intervals)
                  (when (null intervals2) (return-from outer-loop))
                  (advance2)))))))
      (nreverse new-intervals))
    '()))
; Returns the union of two interval lists, assuming the same number type.
(defun intervals-union (intervals1 intervals2 kind)
  (if (and intervals1 intervals2)
    (let ((new-intervals '())
          lo1-incl lo1-excl (hi1-incl 'uninitialized) hi1-excl
          lo2-incl lo2-excl (hi2-incl 'uninitialized) hi2-excl)
      (macrolet ((halfadvance1 ()
                   '(let ((lo (car intervals1)))
                      (setq lo1-incl (and (realp lo) lo))
                      (setq lo1-excl (and (consp lo) (car lo)))))
                 (halfadvance2 ()
                   '(let ((lo (car intervals2)))
                      (setq lo2-incl (and (realp lo) lo))
                      (setq lo2-excl (and (consp lo) (car lo)))))
                 (advance1 ()
                   '(let ((hi (cadr intervals1)) (lo (caddr intervals1)))
                      (setq hi1-incl (and (realp hi) hi))
                      (setq hi1-excl (and (consp hi) (car hi)))
                      (setq lo1-incl (and (realp lo) lo))
                      (setq lo1-excl (and (consp lo) (car lo)))
                      (setq intervals1 (cddr intervals1))))
                 (advance2 ()
                   '(let ((hi (cadr intervals2)) (lo (caddr intervals2)))
                      (setq hi2-incl (and (realp hi) hi))
                      (setq hi2-excl (and (consp hi) (car hi)))
                      (setq lo2-incl (and (realp lo) lo))
                      (setq lo2-excl (and (consp lo) (car lo)))
                      (setq intervals2 (cddr intervals2)))))
        (halfadvance1)
        (halfadvance2)
        (block outer-loop
          (tagbody start
            ;; Search the start of a union interval.
            (if (interval-lolo-<= lo1-incl lo1-excl lo2-incl lo2-excl)
              (progn
                (push (if lo1-incl lo1-incl (if lo1-excl `(,lo1-excl) '*))
                      new-intervals)
                (go label1))
              (progn
                (push (if lo2-incl lo2-incl (if lo2-excl `(,lo2-excl) '*))
                      new-intervals)
                (go label2)))
            ;; Seach the end of the union interval.
           label1
            (advance1)
            (when (null intervals1)
              (loop
                (cond ((and intervals2
                            (not (interval-hilo-< hi1-incl hi1-excl
                                                  lo2-incl lo2-excl)))
                       (advance2))
                      (t (return))))
              (return-from outer-loop))
            (when (not (interval-hilo-< hi1-incl hi1-excl lo2-incl lo2-excl))
              (go label2))
            (push (if hi1-incl hi1-incl (if hi1-excl `(,hi1-excl) '*))
                  new-intervals)
            (go start)
           label2
            (advance2)
            (when (null intervals2)
              (loop
                (cond ((and intervals1
                            (not (interval-hilo-< hi2-incl hi2-excl
                                                  lo1-incl lo1-excl)))
                       (advance1))
                      (t (return))))
              (return-from outer-loop))
            (when (not (interval-hilo-< hi2-incl hi2-excl lo1-incl lo1-excl))
              (go label1))
            (push (if hi2-incl hi2-incl (if hi2-excl `(,hi2-excl) '*))
                  new-intervals)
            (go start)))
        (if (cond ((eq hi1-incl 'uninitialized) t)
                  ((eq hi2-incl 'uninitialized) nil)
                  (t (interval-hihi-<= hi1-incl hi1-excl hi2-incl hi2-excl)))
          (push (if hi2-incl hi2-incl (if hi2-excl `(,hi2-excl) '*))
                new-intervals)
          (push (if hi1-incl hi1-incl (if hi1-excl `(,hi1-excl) '*))
                new-intervals)))
      (setq new-intervals (nreconc new-intervals (or intervals1 intervals2)))
      ;; Simplify [x,y] u (y,z] and [x,y) u [y,z] to [x,z].
      (do ((l new-intervals))
          ((null (cddr l)))
        (let* ((hi (cadr l))
               (hi-incl (and (realp hi) hi))
               (hi-excl (and (consp hi) (car hi)))
               (lo (caddr l))
               (lo-incl (and (realp lo) lo))
               (lo-excl (and (consp lo) (car lo))))
          (if (or (and hi-incl lo-excl (= lo-excl hi-incl))
                  (and hi-excl lo-incl (= lo-incl hi-excl))
                  (and (eq kind 'INTEGER)
                       hi-incl lo-incl (eql lo-incl (1+ hi-incl))))
            (setf (cdr l) (cdddr l))
            (setq l (cddr l)))))
      new-intervals)
    (or intervals1 intervals2)))
; Tests whether intervals1 is contained in intervals2, assuming the same number
; type.
(defun intervals-subtypep (intervals1 intervals2)
  (if intervals1
    (if intervals2
      (let (lo1-incl lo1-excl hi1-incl hi1-excl
            lo2-incl lo2-excl hi2-incl hi2-excl)
        (macrolet ((advance1 ()
                     '(let ((lo (car intervals1)) (hi (cadr intervals1)))
                        (setq lo1-incl (and (realp lo) lo))
                        (setq lo1-excl (and (consp lo) (car lo)))
                        (setq hi1-incl (and (realp hi) hi))
                        (setq hi1-excl (and (consp hi) (car hi)))
                        (setq intervals1 (cddr intervals1))))
                   (advance2 ()
                     '(let ((lo (car intervals2)) (hi (cadr intervals2)))
                        (setq lo2-incl (and (realp lo) lo))
                        (setq lo2-excl (and (consp lo) (car lo)))
                        (setq hi2-incl (and (realp hi) hi))
                        (setq hi2-excl (and (consp hi) (car hi)))
                        (setq intervals2 (cddr intervals2)))))
          (advance1)
          (advance2)
          (loop
            (when (not (interval-lolo-<= lo2-incl lo2-excl lo1-incl lo1-excl))
              (return-from intervals-subtypep nil))
            (if (interval-hilo-< hi2-incl hi2-excl lo1-incl lo1-excl)
              (progn
                (when (null intervals2)
                  (return-from intervals-subtypep nil))
                (advance2))
              (progn
                (when (not (interval-hihi-<= hi1-incl hi1-excl hi2-incl hi2-excl))
                  (return-from intervals-subtypep nil))
                (when (null intervals1)
                  (return-from intervals-subtypep t))
                (advance1))))))
      nil)
    t))
; Tests whether a given object is covered by the interval.
(defun intervals-typep (intervals obj)
  (do ((l intervals (cddr l)))
      ((endp l) nil)
    (let* ((lo (car l))
           (lo-incl (and (realp lo) lo))
           (lo-excl (and (consp lo) (car lo)))
           (hi (cadr l))
           (hi-incl (and (realp hi) hi))
           (hi-excl (and (consp hi) (car hi))))
      (when (and (if lo-incl (<= lo-incl obj) (if lo-excl (< lo-excl obj) t))
                 (if hi-incl (<= obj hi-incl) (if hi-excl (< obj hi-excl) t)))
        (return t)))))
; Returns the intervals, with exclusive integer bounds replaced with inclusive
; integer bounds. Implicitly, kind = RATIONAL.
(defun intervals-integer-closure (intervals)
  (let ((new-intervals '()))
    (do ((l intervals (cddr l)))
        ((endp l))
      (let* ((lo (car l))
             (lo-incl (and (realp lo) lo))
             (lo-excl (and (consp lo) (car lo)))
             (hi (cadr l))
             (hi-incl (and (realp hi) hi))
             (hi-excl (and (consp hi) (car hi))))
        (when (integerp lo-excl) (setq lo-incl lo-excl lo-excl nil))
        (when (integerp hi-excl) (setq hi-incl hi-excl hi-excl nil))
        (setq new-intervals
              (list* (if hi-incl hi-incl (if hi-excl `(,hi-excl) '*))
                     (if lo-incl lo-incl (if lo-excl `(,lo-excl) '*))
                     new-intervals))))
    (setq new-intervals (nreverse new-intervals))
    ;; Simplify [x,y] u [y,z] and [x,y] u (y,z] and [x,y) u [y,z] to [x,z].
    (do ((l new-intervals))
        ((null (cddr l)))
      (let* ((hi (cadr l))
             (hi-incl (and (realp hi) hi))
             (hi-excl (and (consp hi) (car hi)))
             (lo (caddr l))
             (lo-incl (and (realp lo) lo))
             (lo-excl (and (consp lo) (car lo))))
        (if (or (and hi-incl lo-incl (= lo-incl hi-incl))
                (and hi-incl lo-excl (= lo-excl hi-incl))
                (and hi-excl lo-incl (= lo-incl hi-excl)))
          (setf (cdr l) (cdddr l))
          (setq l (cddr l)))))
    new-intervals))
; Returns the intervals minus a given singleton, assuming it's of the right
; number type.
(defun intervals-remove-one (intervals kind obj)
  (let ((new-intervals '()))
    (flet ((cons-new-interval (lo-incl lo-excl hi-incl hi-excl kind new-intervals)
             (when (eq kind 'INTEGER)
               (when lo-excl (setq lo-incl (+ lo-excl 1) lo-excl nil))
               (when hi-excl (setq hi-incl (- hi-excl 1) hi-excl nil)))
             (if (interval-lohi-<= lo-incl lo-excl hi-incl hi-excl)
               (list* (if hi-incl hi-incl (if hi-excl `(,hi-excl) '*))
                      (if lo-incl lo-incl (if lo-excl `(,lo-excl) '*))
                      new-intervals)
               new-intervals)))
      (macrolet ((add-new-interval (lo-incl lo-excl hi-incl hi-excl)
                   `(setq new-intervals
                          (cons-new-interval ,lo-incl ,lo-excl ,hi-incl ,hi-excl kind
                                             new-intervals))))
        (do ((l intervals (cddr l)))
            ((endp l))
          (let* ((lo (car l))
                 (lo-incl (and (realp lo) lo))
                 (lo-excl (and (consp lo) (car lo)))
                 (hi (cadr l))
                 (hi-incl (and (realp hi) hi))
                 (hi-excl (and (consp hi) (car hi))))
            (if (and (if lo-incl (<= lo-incl obj) (if lo-excl (< lo-excl obj) t))
                     (if hi-incl (<= obj hi-incl) (if hi-excl (< obj hi-excl) t)))
              (progn
                (add-new-interval lo-incl lo-excl nil obj)
                (add-new-interval nil obj hi-incl hi-excl))
              (add-new-interval lo-incl lo-excl hi-incl hi-excl))))))
    (nreverse new-intervals)))
;; Remove a single object from the type. The type is of the form
;; (kind [lo-bound hi-bound]+). The result is a type in the same form, or
;; an `(OR ...) of types in the same form, or NIL.
(defun subtypep-REAL-remove-singleton (type obj)
  (let* ((kind (if (atom type) type (second type)))
         (intervals (if (atom type) '(* *) (cddr type)))
         (test (get kind 'TYPE-SYMBOL)))
    (if (and (funcall test obj) (intervals-typep intervals obj))
      (let ((splittypes
              (case kind
                ;; Have to split the type before removing obj. For example,
                ;; (AND REAL (NOT (EQL 0))) is equivalent to
                ;; (OR (RATIONAL * (0) (0) *) FLOAT), not (REAL * (0) (0) *).
                (REAL
                  (remove 'NIL
                    (list (subtypep-REAL-remove-singleton
                            `(INTERVALS RATIONAL ,@(intervals-mapto intervals 'RATIONAL)) obj)
                          (subtypep-REAL-remove-singleton
                            `(INTERVALS FLOAT ,@(intervals-mapto intervals 'FLOAT)) obj))))
                (FLOAT
                  (remove 'NIL
                    (list (subtypep-REAL-remove-singleton
                            `(INTERVALS SHORT-FLOAT ,@(intervals-mapto intervals 'SHORT-FLOAT)) obj)
                          (subtypep-REAL-remove-singleton
                            `(INTERVALS SINGLE-FLOAT ,@(intervals-mapto intervals 'SINGLE-FLOAT)) obj)
                          (subtypep-REAL-remove-singleton
                            `(INTERVALS DOUBLE-FLOAT ,@(intervals-mapto intervals 'DOUBLE-FLOAT)) obj)
                          (subtypep-REAL-remove-singleton
                            `(INTERVALS LONG-FLOAT ,@(intervals-mapto intervals 'LONG-FLOAT)) obj))))
                (t
                  (setq intervals (intervals-remove-one intervals kind obj))
                  (if intervals (list `(INTERVALS ,kind ,@intervals)) '())))))
        (case (length splittypes)
          (0 'NIL)
          (1 (first splittypes))
          (t `(OR ,@splittypes))))
      type)))

(def-type-category CHARACTER (CHARACTER) (CHARACTER-INTERVALS)
  ;; Each part is of the form (CHARACTER-INTERVALS [lo-bound hi-bound]+), a
  ;; non-empty list of disjoint intervals, in ascending order, or an encoding.
  ;; This allows manipulating encoding as true sets of characters.
  ;; TODO: This needs an update when we use upgraded-complex-element-type.
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    ;; First, simplify encodings that cover the whole character range.
    (setq parts (mapcar #'subtypep-CHARACTER-pre-simplify parts))
    (setq parts (remove 'CHARACTER parts))
    (case (length parts)
      (0 'CHARACTER)
      (1 (first parts))
      (t
        ;; Intersect the interval ranges.
        (let ((intervals
                (reduce #'intervals-intersection parts
                        :key #'(lambda (type)
                                 (if (encodingp type)
                                   (map 'list #'char-code (get-charset-range (encoding-charset type)))
                                   (rest type))))))
          (if intervals
            `(CHARACTER-INTERVALS ,@intervals)
            'NIL)))))
  (lambda (parts)
    ;; First, simplify encodings that cover the whole character range.
    (setq parts (mapcar #'subtypep-CHARACTER-pre-simplify parts))
    parts)
  (lambda (type1 type2parts try-prove-no)
    ;; Try to work out things without expanding encodings to intervals unless
    ;; really needed.
    (when (member 'CHARACTER type2parts)
      (yes))
    (cond ((encodingp type1)
           (when (member type1 type2parts)
             (yes))
           #| ;; Shortcut that avoids consing but makes testing harder.
           (when (and (eql (length type2parts) 1) (encodingp (first type2parts)))
             (when (charset-subtypep type1 (first type2parts))
               (yes))
             (when try-prove-no (no))
             (unknown))
           |#
           (let ((intervals
                   (map 'list #'char-code (get-charset-range (encoding-charset type1)))))
             (setq type1 `(CHARACTER-INTERVALS ,@intervals))))
          ((eq type1 'CHARACTER)
           (setq type1 '(CHARACTER-INTERVALS 0 #,char-code-limit))))
    ;; Now type1 is in `(CHARACTER-INTERVALS ,@intervals) form.
    (assert (eq (first type1) 'CHARACTER-INTERVALS))
    (let ((intervals1 (rest type1)))
      ;; First pass, ignoring encodings on the right-hand side.
      (dolist (type2 type2parts)
        (unless (encodingp type2)
          (assert (eq (first type2) 'CHARACTER-INTERVALS))
          (let ((intervals2 (rest type2)))
            (when (intervals-subtypep intervals1 intervals2)
              (yes)))))
      ;; Now we need to expand the encodings on the right-hand side.
      (setq type2parts
            (mapcar #'(lambda (type2)
                        (if (encodingp type2)
                          (let ((intervals
                                  (map 'list #'char-code (get-charset-range (encoding-charset type2)))))
                            (when (intervals-subtypep intervals1 intervals)
                              (yes))
                            `(CHARACTER-INTERVALS ,@intervals))
                          type2))
                    type2parts))
      ;; Now all the type2parts are in `(CHARACTER-INTERVALS ,@intervals) form.
      (when (> (length type2parts) 1)
        (let ((intervals2 (reduce #'intervals-union type2parts :key #'rest)))
          (when (intervals-subtypep intervals1 intervals2)
            (yes))))
      (when try-prove-no
        ;; All checks have already been done above.
        (no))
      (unknown)))
)
;; Simplify an encoding that covers the whole character range.
(defun subtypep-CHARACTER-pre-simplify (type)
  (if (encodingp type)
    (let ((charset (encoding-charset type)))
      (case charset
        #+UNICODE
        ((charset:unicode-16-big-endian charset:unicode-16-little-endian
          charset:unicode-32-big-endian charset:unicode-32-little-endian
          charset:utf-8 charset:java)
          'CHARACTER)
        (t
          (if (and (stringp charset)
                   (or (string= charset "UTF-16") (string= charset "UTF-7")))
            'CHARACTER
            type))))
    type))
;; Remove a single object from the type. The type is of the form
;; (CHARACTER [lo-bound hi-bound]+). The result is a type in the same form,
;; or NIL.
(defun subtypep-CHARACTER-remove-singleton (type obj)
  (let ((intervals (if (atom type) '(* *) (rest type))))
    (if (and (characterp obj) (intervals-typep intervals (char-code obj)))
      (progn
        (setq intervals (intervals-remove-one intervals 'INTEGER (char-code obj)))
        (if intervals `(CHARACTER-INTERVALS ,@intervals) 'NIL))
      type)))
;; Conversion of an encoding to a list of intervals.
#+UNICODE
(defun get-charset-range (charset &optional maxintervals)
  (let ((table #,(make-hash-table :key-type '(or string symbol)
                                  :value-type 'simple-string
                                  :test 'stablehash-equal
                                  :warn-if-needs-rehash-after-gc t)))
    ;; cache: charset name -> list of intervals #(start1 end1 ... startm endm)
    #| ; Now in C and much more efficient.
  (defun charset-range (encoding start end)
    (setq start (char-code start))
    (setq end (char-code end))
    (let ((intervals '())   ; finished intervals
          (i1 nil) (i2 nil) ; interval being built
          (i start))
      (loop
        (if (charset-typep (code-char i) encoding)
          (if i2 (setq i2 i) (setq i1 i i2 i))
          (if i2 (setq intervals (list* i2 i1 intervals) i1 nil i2 nil)))
        (when (eql i end) (return))
        (incf i))
      (when i2 (setq intervals (list* i2 i1 intervals)))
      (map 'simple-string #'code-char (nreverse intervals))))
    |#
    ;; Return the definition range of a character set. If necessary, compute it
    ;; and store it in the cache.
    (or (gethash charset table)
        (setf (gethash charset table)
              (charset-range (make-encoding :charset charset)
                             (code-char 0) (code-char (1- char-code-limit))
                             maxintervals)))))
#| ;; Older code for a special case.
;; Test whether all characters encodable in encoding1 are also encodable in
;; encoding2.
 (defun charset-subtypep (encoding1 encoding2)
  #-UNICODE (declare (ignore encoding1 encoding2)) #-UNICODE t
  #+UNICODE
  (let* ((intervals1 (get-charset-range (encoding-charset encoding1)))
         (intervals2 (get-charset-range (encoding-charset encoding2)))
         (n1 (length intervals1))
         (n2 (length intervals2))
         (jj1 0)  ; grows by 2 from 0 to n1
         (jj2 0)) ; grows by 2 from 0 to n2
    (loop
      ;; Get next interval from intervals1.
      (when (eql jj1 n1) (return-from charset-subtypep t))
      (let ((i1 (schar intervals1 jj1)) (i2 (schar intervals1 (+ jj1 1))))
        ;; Test whether [i1,i2] is contained in intervals2.
        (let (i3 i4)
          (loop
            (when (eql jj2 n2) ; [i1,i2] not contained in intervals2.
              (return-from charset-subtypep nil))
            (setq i3 (schar intervals2 jj2))
            (setq i4 (schar intervals2 (+ jj2 1)))
            ;; If i3 <= i4 < i1 <= i2, skip the interval [i3,i4].
            (when (char>= i4 i1) (return))
            (incf jj2 2))
          (when (char< i1 i3) ; i1 not contained in intervals2.
            (return-from charset-subtypep nil))
          (when (char< i4 i2) ; i4+1 (in [i1,i2]) not contained in intervals2.
            (return-from charset-subtypep nil))
          ;; Now (<= i3 i1) and (<= i2 i4),
          ;; hence [i1,i2] is contained in intervals2.
          (incf jj1 2))))))
|#

(def-type-category PATHNAME (PATHNAME LOGICAL-PATHNAME) ()
  ;; Each part is PATHNAME or LOGICAL-PATHNAME.
  (lambda (parts)
    (cond #+LOGICAL-PATHNAMES ((member 'LOGICAL-PATHNAME parts) 'LOGICAL-PATHNAME)
          (t 'PATHNAME)))
  (lambda (parts)
    (cond ((member 'PATHNAME parts) (list 'PATHNAME))
          #+LOGICAL-PATHNAMES ((member 'LOGICAL-PATHNAME parts) (list 'LOGICAL-PATHNAME))
          (t '())))
  (lambda (type1 type2parts try-prove-no)
    (when (and type2parts
               #+LOGICAL-PATHNAMES (or (eq type1 'LOGICAL-PATHNAME)
                                       (eq (first type2parts) 'PATHNAME)))
      (yes))
    (when try-prove-no
      ;; All checks have already been done above.
      (no))
    (unknown))
)

;; Miscellaneous types that each form their own category.
(def-type-category #:misc () ()
  (lambda (parts)
    ; All parts are the same.
    (first parts))
  (lambda (parts)
    parts)
  (lambda (type1 type2parts try-prove-no)
    (declare (ignore type1))
    (when type2parts (yes))
    (when try-prove-no (no))
    (unknown))
)
(def-misc-type-category ENCODING)
(def-misc-type-category HASH-TABLE)
(def-misc-type-category PACKAGE)
(def-misc-type-category RANDOM-STATE)
(def-misc-type-category READTABLE)
(def-misc-type-category SYMBOL)
(def-misc-type-category BYTE)
(def-misc-type-category SPECIAL-OPERATOR)
(def-misc-type-category LOAD-TIME-EVAL)
(def-misc-type-category SYMBOL-MACRO)
#+(or UNIX FFI WIN32)
(def-misc-type-category FOREIGN-POINTER)
#+FFI
(def-misc-type-category FFI::FOREIGN-ADDRESS)
#+FFI
(def-misc-type-category FFI::FOREIGN-VARIABLE)
(def-misc-type-category WEAK-POINTER)
(def-misc-type-category READ-LABEL)
(def-misc-type-category FRAME-POINTER)
(def-misc-type-category SYSTEM-INTERNAL)

(def-type-category STRUCTURE-OBJECT () ()
  ;; Each part is a CLOS class of metaclass <structure-class>.
  ;; Exploit the fact that this metaclass supports no multiple inheritance.
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (if (rest parts)
      (let ((minimal-class (first parts)))
        (dolist (other-class (rest parts) minimal-class)
          (cond ((clos::subclassp minimal-class other-class))
                ((clos::subclassp other-class minimal-class)
                 (setq minimal-class other-class))
                (t
                  ;; No multiple inheritance -> minimal-class and other-class
                  ;; can have no common subclass.
                  (return 'NIL)))))
      (first parts)))
  (lambda (parts)
    ;; It's not worth simplifying.
    parts)
  (lambda (type1 type2parts try-prove-no)
    ;; We actually need to test (clos::subclassp type1 class2) only for
    ;; those class2 that are maximal among type2parts. But it's not worth
    ;; determining the maximal classes. Just process them all.
    (dolist (class2 type2parts)
      (when (clos::subclassp type1 class2)
        (yes)))
    (when try-prove-no
      ;; All checks already done. Any instance of class1 would be a testimony.
      (no))
    (unknown))
)

(def-type-category STANDARD-OBJECT () (FUNCTION)
  ;; Each part is
  ;; a CLOS class of metaclass <standard-class> or <funcallable-standard-class>,
  ;; or <function>, of metaclass <built-in-class>,
  ;; or of the form (FUNCTION ...).
  (lambda (parts)
    ;; Simplify `(AND ,@parts):
    (cond #+FFI
          ((member 'FFI::FOREIGN-FUNCTION parts)
           (let ((other-parts
                   (set-difference parts
                     (list (find-class 'FUNCTION) #+FFI 'FFI::FOREIGN-FUNCTION))))
             (if (some #'atom other-parts)
               'NIL
               (if (null other-parts)
                 'FFI::FOREIGN-FUNCTION
                 `(AND ,@parts)))))
          (t (when (some #'consp parts)
               ; If some part is (FUNCTION ...), #<class FUNCTION> is redundant.
               (setq parts (remove (find-class 'FUNCTION) parts)))
             (if (rest parts) `(AND ,@parts) (first parts)))))
  (lambda (parts)
    ;; It's not worth simplifying the intersection of parts with
    ;; '(FUNCTION #+FFI FFI::FOREIGN-FUNCTION).
    parts)
  (lambda (type1 type2parts try-prove-no)
    ;; Under <standard-object>, any set of classes can have a common subclass
    ;; (you only need to be careful about the order of the superclasses when
    ;; you define the subclass, to avoid an error when computing the CPL)
    ;; and therefore also a common instance.
    (cond #+FFI
          ((eq type1 'FFI::FOREIGN-FUNCTION)
           (when (intersection type2parts
                   (list (find-class 'FUNCTION) #+FFI 'FFI::FOREIGN-FUNCTION))
             (yes)))
          ((and (consp type1) (eq (car type1) 'FUNCTION)) ; (FUNCTION ...)
           (when (member (find-class 'FUNCTION) type2parts)
             (yes))
           (when (member type1 type2parts :test #'canonicalized-types-equal-p)
             (yes)))
          (t (let ((type1parts
                     (if (consp type1)
                       (progn (assert (eq (first type1) 'AND)) (rest type1))
                       (list type1))))
               (dolist (class1 type1parts)
                 (if (consp class1) ; (FUNCTION ...)
                   (when (member class1 type2parts :test #'canonicalized-types-equal-p)
                     (yes))
                   (dolist (class2 type2parts)
                     (when (clos::subclassp class1 class2)
                       (yes))))))))
    (when try-prove-no
      ;; For classes, all checks already done. Any common instance of type1
      ;; would be a testimony.
      (unless (or (consp type1) (some #'consp type2parts))
        (no)))
    (unknown))
)

;; Determine the category of the given type.
;; Returns NIL for types about which nothing is known, such as SATISFIES types.
(defun type-category (type)
  (macrolet ((case-atomic (expr &rest clauses)
               `(CASE ,expr
                  ,@(append (mapcan #'(lambda (category)
                                        (let ((specifiers (get category 'SUBTYPEP-ATOM)))
                                          (if specifiers
                                            (list `(,specifiers ',category))
                                            '())))
                                    *type-categories*)
                            clauses)))
             (case-list (expr &rest clauses)
               `(CASE ,expr
                  ,@(append (mapcan #'(lambda (category)
                                        (let ((specifiers (get category 'SUBTYPEP-LIST)))
                                          (if specifiers
                                            (list `(,specifiers ',category))
                                            '())))
                                    *type-categories*)
                            clauses))))
    (if (consp type)
      (case-list (first type)
        (t 'NIL)) ; SATISFIES and VALUES types cannot be classified.
      (case-atomic type
        ((FUNCTION #+FFI FFI::FOREIGN-FUNCTION)
         ;; FUNCTION is not a category of its own, because of GENERIC-FUNCTION.
         'STANDARD-OBJECT)
        (t (cond ((clos::defined-class-p type)
                  (if (clos::structure-class-p type)
                    'STRUCTURE-OBJECT
                    'STANDARD-OBJECT))
                 ((encodingp type) 'CHARACTER)
                 ((symbolp type)
                  ;; Symbols that name CLOS classes have already been mapped
                  ;; the class object by canonicalize-type. No need to handle
                  ;; them here.
                  'NIL)
                 (t 'NIL)))))))

;; To efficiently handle ranges of real numbers and characters, the REAL and
;; CHARACTER type categories use the type specifiers
;;   (INTERVALS kind [lo-bound hi-bound]+)
;;   (CHARACTER-INTERVALS [lo-bound hi-bound]+)
;; We use this type specifier, instead of just (kind [lo-bound hi-bound]+),
;; so that canonicalize-type can do proper error checking and reject malformed
;; type specifiers like (INTEGER 0 1 4 5) or (CHARACTER 0 255). The reason is
;; that canonicalized types can be fed to canonicalize-type again; indeed,
;; the handling of MEMBER types in subtypep-logic, through
;; subtypep-REAL-remove-singleton and subtypep-CHARACTER-remove-singleton,
;; passes a mix of INTERVALS types and canonicalized types to subtypep.
(setf (get 'INTERVALS 'TYPE-LIST)
      (function type-list-intervals
        (lambda (x kind &rest intervals)
          (let ((test (get kind 'TYPE-SYMBOL)))
            (and (funcall test x)
                 (intervals-typep intervals x))))))
(setf (get 'CHARACTER-INTERVALS 'TYPE-LIST)
      (function type-list-character-intervals
        (lambda (x &rest intervals)
          (and (characterp x) (intervals-typep intervals (char-code x))))))

;; (CANONICALIZE-TYPE type) returns a superficially simplified type.
;; The purpose of this function is not to be a complicated simplifier,
;; but rather it only brings the type into a recognizable shape so that
;; SUBTYPEP can work in the right direction. SUBTYPEP is recursive,
;; therefore CANONICALIZE-TYPE does not need to be recursive. But
;; CANONICALIZE-TYPE must prepare for the classification; that's why
;; for example NUMBER becomes '(OR REAL COMPLEX).
;; CANONICALIZE-TYPE also does non-recursive error checking.
(defun canonicalize-type (type
                          &aux head)
  ;; Expand DEFTYPE types at the outermost level only.
  (setq type (expand-deftype type))
  (cond ((symbolp type)
         (case type
           (ATOM '(NOT CONS))
           (BASE-CHAR #+BASE-CHAR=CHARACTER 'CHARACTER
                      #-BASE-CHAR=CHARACTER '(CHARACTER-INTERVALS 0 #,base-char-code-limit))
           (BIGNUM  '(AND INTEGER (NOT FIXNUM)))
           (BIT '(INTERVALS INTEGER 0 1))
           (BOOLEAN '(MEMBER NIL T))
           (COMPILED-FUNCTION '(AND FUNCTION (SATISFIES COMPILED-FUNCTION-P)))
           (EXTENDED-CHAR #+BASE-CHAR=CHARACTER '(OR) ; NIL
                          #-BASE-CHAR=CHARACTER '(CHARACTER-INTERVALS #,(1+ base-char-code-limit) #,char-code-limit))
           (FIXNUM '(INTERVALS INTEGER #,most-negative-fixnum #,most-positive-fixnum))
           (KEYWORD '(AND SYMBOL (SATISFIES KEYWORDP)))
           (LIST '(OR CONS (MEMBER NIL)))
           ((NIL) '(OR))
           (NULL '(MEMBER NIL))
           (NUMBER '(OR REAL COMPLEX))
           (RATIO '(AND RATIONAL (NOT INTEGER)))
           (SEQUENCE '(OR LIST VECTOR)) ; user-defined sequences??
           (SIGNED-BYTE '(INTERVALS INTEGER * *))
           (STANDARD-CHAR '(CHARACTER-INTERVALS #,(char-code #\Newline) #,(char-code #\Newline) #,(char-code #\Space) #,(char-code #\~)))
           (STRING-CHAR 'CHARACTER)
           ((T) '(AND))
           (UNSIGNED-BYTE '(INTERVALS INTEGER 0 *))
           (ARRAY
            ; (canonicalize-type (list type)) =>
            '(ARRAY))
           (SIMPLE-ARRAY
            ; (canonicalize-type (list type)) =>
            '(SIMPLE-ARRAY))
           (BIT-VECTOR
            ; (canonicalize-type (list type)) =>
            '(ARRAY BIT (*)))
           (SIMPLE-BIT-VECTOR
            ; (canonicalize-type (list type)) =>
            '(SIMPLE-ARRAY BIT (*)))
           ((STRING cs-cl:string)
            ; (canonicalize-type (list type)) =>
            '(OR (ARRAY CHARACTER (*))
                 #-BASE-CHAR=CHARACTER (ARRAY BASE-CHAR (*))
                 (ARRAY NIL (*))))
           (SIMPLE-STRING
            ; (canonicalize-type (list type)) =>
            '(OR (SIMPLE-ARRAY CHARACTER (*))
                 #-BASE-CHAR=CHARACTER (SIMPLE-ARRAY BASE-CHAR (*))
                 (SIMPLE-ARRAY NIL (*))))
           (BASE-STRING
            ; (canonicalize-type (list type)) =>
            '(ARRAY BASE-CHAR (*)))
           (SIMPLE-BASE-STRING
            ; (canonicalize-type (list type)) =>
            '(SIMPLE-ARRAY BASE-CHAR (*)))
           (VECTOR
            ; (canonicalize-type (list type)) =>
            '(ARRAY * (*)))
           (SIMPLE-VECTOR
            ; (canonicalize-type (list type)) =>
            '(SIMPLE-ARRAY T (*)))
           (COMPLEX
            ; (canonicalize-type (list type)) =>
            '(COMPLEX))
           (REAL
            ; (canonicalize-type (list type)) =>
            '(INTERVALS REAL * *))
           (INTEGER
            ; (canonicalize-type (list type)) =>
            '(INTERVALS INTEGER * *))
           (RATIONAL
            ; (canonicalize-type (list type)) =>
            '(INTERVALS RATIONAL * *))
           (FLOAT
            ; (canonicalize-type (list type)) =>
            '(INTERVALS FLOAT * *))
           (SHORT-FLOAT
            ; (canonicalize-type (list type)) =>
            '(INTERVALS SHORT-FLOAT * *))
           (SINGLE-FLOAT
            ; (canonicalize-type (list type)) =>
            '(INTERVALS SINGLE-FLOAT * *))
           (DOUBLE-FLOAT
            ; (canonicalize-type (list type)) =>
            '(INTERVALS DOUBLE-FLOAT * *))
           (LONG-FLOAT
            ; (canonicalize-type (list type)) =>
            '(INTERVALS LONG-FLOAT * *))
           ((FUNCTION STREAM FILE-STREAM SYNONYM-STREAM BROADCAST-STREAM
             CONCATENATED-STREAM TWO-WAY-STREAM ECHO-STREAM STRING-STREAM)
            ;; We treat FUNCTION like a CLOS class, so that
            ;; (subtypep 'GENERIC-FUNCTION FUNCTION) can return T.
            ;; We treat STREAM and subclasses like CLOS classes, so that
            ;; (subtypep 'FUNDAMENTAL-STREAM 'STREAM) can return T.
            (or (clos-class type) type))
           ;; Misc type categories, excluding those that are CLOS classes.
           ((ENCODING BYTE SPECIAL-OPERATOR LOAD-TIME-EVAL SYMBOL-MACRO
             #+(or UNIX FFI WIN32) FOREIGN-POINTER
             #+FFI FFI::FOREIGN-ADDRESS
             #+FFI FFI::FOREIGN-VARIABLE
             #+FFI FFI::FOREIGN-FUNCTION
             WEAK-POINTER READ-LABEL FRAME-POINTER SYSTEM-INTERNAL)
            type)
           (t
            (let ((f (clos-class type)))
              (if f
                (if (clos::built-in-class-p f) type f)
                (or (ds-canonicalize-type type)
                    (typespec-error 'subtypep type)))))))
        ((and (consp type) (symbolp (setq head (first type))))
         (unless (and (list-length type) (null (cdr (last type))))
           (typespec-error 'subtypep type))
         (case head
           (MEMBER ; (MEMBER &rest objects)
            (if (null (rest type))
              '(OR)
              type))
           (EQL ; (EQL object)
            (unless (eql (length type) 2)
              (typespec-error 'subtypep type))
            `(MEMBER ,(second type)))
           ((AND OR) ; (AND type*), (OR type*)
            type)
           (NOT ; (NOT type)
            (unless (eql (length type) 2)
              (typespec-error 'subtypep type))
            type)
           (MOD ; (MOD n)
            (unless (eql (length type) 2)
              (typespec-error 'subtypep type))
            (let ((n (second type)))
              (unless (and (integerp n) (>= n 0))
                (typespec-error 'subtypep type))
              (if (eql n 0) '(OR) `(INTERVALS INTEGER 0 ,(1- n)))))
           (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((s (if (cdr type) (second type) '*)))
              (if (eq s '*)
                '(INTERVALS INTEGER * *)
                (progn
                  (unless (and (integerp s) (plusp s))
                    (typespec-error 'subtypep type))
                  (let ((n (ash 1 (1- s)))) ; (ash 1 n) == (expt 2 n)
                    `(INTERVALS INTEGER ,(- n) ,(1- n)))))))
           (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((s (if (cdr type) (second type) '*)))
              (if (eq s '*)
                '(INTERVALS INTEGER 0 *)
                (progn
                  (unless (and (integerp s) (>= s 0))
                    (typespec-error 'subtypep type))
                  `(INTERVALS INTEGER 0 ,(1- (ash 1 s))))))) ; (ash 1 n) == (expt 2 n)
           (SIMPLE-BIT-VECTOR ; (SIMPLE-BIT-VECTOR &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(SIMPLE-ARRAY BIT (,size))))
           (SIMPLE-STRING ; (SIMPLE-STRING &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(OR (SIMPLE-ARRAY CHARACTER (,size))
                   #-BASE-CHAR=CHARACTER (SIMPLE-ARRAY BASE-CHAR (,size))
                   (SIMPLE-ARRAY NIL (,size)))))
           (SIMPLE-BASE-STRING ; (SIMPLE-BASE-STRING &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(SIMPLE-ARRAY BASE-CHAR (,size))))
           (SIMPLE-VECTOR ; (SIMPLE-VECTOR &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(SIMPLE-ARRAY T (,size))))
           (BIT-VECTOR ; (BIT-VECTOR &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(ARRAY BIT (,size))))
           ((STRING cs-cl:string) ; (STRING &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(OR (ARRAY CHARACTER (,size))
                   #-BASE-CHAR=CHARACTER (ARRAY BASE-CHAR (,size))
                   (ARRAY NIL (,size)))))
           (BASE-STRING ; (BASE-STRING &optional size)
            (when (cddr type)
              (typespec-error 'subtypep type))
            (let ((size (if (cdr type) (second type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(ARRAY BASE-CHAR (,size))))
           (VECTOR ; (VECTOR &optional el-type size)
            (when (cdddr type)
              (typespec-error 'subtypep type))
            (let ((el-type (if (cdr type) (second type) '*))
                  (size (if (cddr type) (third type) '*)))
              (unless (or (eq size '*) (and (integerp size) (>= size 0)))
                (typespec-error 'subtypep type))
              `(ARRAY ,el-type (,size))))
           ((ARRAY SIMPLE-ARRAY) ; ([SIMPLE-]ARRAY &optional el-type dims)
            (when (cdddr type)
              (typespec-error 'subtypep type))
            (let ((dims (if (cddr type) (third type) '*)))
              (unless (or (eq dims '*)
                          (and (integerp dims) (>= dims 0))
                          (and (listp dims)
                               (every #'(lambda (d)
                                          (or (eq d '*)
                                              (and (integerp d) (>= d 0))))
                                      dims)))
                (typespec-error 'subtypep type))
              type))
           ((COMPLEX) ; (COMPLEX &optional rtype itype)
            (when (cdddr type)
              (typespec-error 'subtypep type))
            type)
           ((REAL RATIONAL INTEGER FLOAT
             SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
            (when (cdddr type)
              (typespec-error 'subtypep type))
            (let ((lo (if (cdr type) (second type) '*))
                  (hi (if (cddr type) (third type) '*))
                  (test (get head 'SYS::TYPE-SYMBOL)))
              (flet ((valid-interval-designator-p (bound test)
                       ;; Tests whether the bound is valid interval designator
                       ;; for the given type.
                       (or (eq bound '*)
                           (funcall test bound)
                           (and (consp bound) (null (cdr bound))
                                (funcall test (first bound))))))
                (unless (and (valid-interval-designator-p lo test)
                             (valid-interval-designator-p hi test))
                  (typespec-error 'subtypep type))
                ; Recons the type, to bring into the multiple-intervals
                ; representation that is used later.
                `(INTERVALS ,head ,lo ,hi))))
           (INTERVALS
            (unless (and (member (second type)
                                 '(REAL RATIONAL INTEGER FLOAT
                                   SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
                         (oddp (length (rest type))))
              (typespec-error 'subtypep type))
            ;; We don't perform a full check of the intervals list here,
            ;; for speed.
            type)
           (CHARACTER-INTERVALS
            (unless (oddp (length type))
              (typespec-error 'subtypep type))
            ;; We don't perform a full check of the intervals list here,
            ;; for speed.
            type)
           ((CONS) ; (CONS &optional cartype cdrtype)
            (when (cdddr type)
              (typespec-error 'subtypep type))
            type)
           (SATISFIES ; (SATISFIES predicate)
            (unless (and (null (cddr type)) (symbolp (second type)))
              (typespec-error 'subtypep type))
            type)
           (VALUES ; (VALUES &rest types)
            type)
           (FUNCTION ; (FUNCTION &optional argtypes valuetype)
            (unless (and (listp (cdr type)) (listp (cadr type))
                         (listp (cddr type)) (null (cdddr type)))
              (typespec-error 'subtypep type))
            (if (null (cdr type))
              ;; (FUNCTION) = FUNCTION. Return the CLOS class, as above.
              (clos-class 'FUNCTION)
              type))
           (t (typespec-error 'subtypep type))))
        ((clos::defined-class-p type)
         (if (and (clos::built-in-class-p type)
                  (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type))
           (canonicalize-type (clos:class-name type))
           type))
        ((clos::eql-specializer-p type)
         `(MEMBER ,(clos::eql-specializer-singleton type)))
        ((encodingp type)
         #+UNICODE type
         #-UNICODE 'CHARACTER)
        (t (typespec-error 'subtypep type))))

;; Compares two canonicalized types for superficial equality.
;; Note that EQUAL is forbidden on types, because of (EQL #1=(x . #1#)).
(defun canonicalized-types-equal-p (type1 type2)
  (or (eq type1 type2)
      (and (consp type1) (consp type2)
           (eq (first type1) (first type2))
           (case (first type1)
             (MEMBER ; (MEMBER &rest objects)
              (let ((objects1 (remove-duplicates (rest type1) :test #'eql))
                    (objects2 (remove-duplicates (rest type2) :test #'eql)))
                (and (eql (length objects1) (length objects2))
                     (or (every #'eql objects1 objects2)
                         (and (null (set-difference objects1 objects2 :test #'eql))
                              (null (set-difference objects2 objects1 :test #'eql)))))))
             (EQL ; (EQL object)
              (eql (second type1) (second type2)))
             ((AND OR COMPLEX CONS VALUES)
              ; (AND type*), (OR type*),
              ; (COMPLEX &optional rtype itype), (CONS &optional cartype cdrtype),
              ; (VALUES &rest types)
              (let ((types1 (rest type1))
                    (types2 (rest type2)))
                (and (eql (length types1) (length types2))
                     (every #'canonicalized-types-equal-p types1 types2))))
             (NOT ; (NOT type)
              (canonicalized-types-equal-p (second type1) (second type2)))
             ((ARRAY SIMPLE-ARRAY) ; ([SIMPLE-]ARRAY &optional el-type dims)
              (and (canonicalized-types-equal-p (if (cdr type1) (second type1))
                                                (if (cdr type2) (second type2)))
                   (equal (cddr type1) (cddr type2))))
             (FUNCTION ; (FUNCTION &optional argtypes valuetype)
              (and (= (length type1) (length type2))
                   (= (length (second type1)) (length (second type2)))
                   (every #'canonicalized-types-equal-p (second type1) (second type2))
                   (canonicalized-types-equal-p (third type1) (third type2))))
             (t ; Other canonicalized types only contain numbers and symbols.
              (equal (rest type1) (rest type2)))))))

(defun subtypep (type1 type2 &optional env
                 &aux head1)
  (macrolet ((yes () '(return-from subtypep (values t t)))
             (no () '(return-from subtypep (values nil t)))
             (unknown () '(return-from subtypep (values nil nil))))
    ;; Rules:
    ;; - Say (yes) or (no) only when the result is mathematically provable.
    ;; - Don't say (unknown) too early, use (unknown) only as a last resort.
    ;; - Don't compare type specificier using EQUAL.
    ;;
    ;; Quick superficial simplification.
    ;; Note: it maps NIL to (OR), T to (AND).
    (setq type1 (canonicalize-type type1))
    (setq type2 (canonicalize-type type2))
    ;; Trivial cases.
    (when (equal '(OR) type1) ; type1 the empty type?
      (yes))
    (when (equal '(AND) type2) ; type2 is T?
      (yes))
    (when (canonicalized-types-equal-p type1 type2)
      ; (subtypep type type) always true
      (yes))
    (when (consp type1)
      (setq head1 (first type1))
      (cond ;; Nothing is known about SATISFIES types.
            ;; But don't signal (unknown) too early.
            ;((and (eq head1 'SATISFIES) (eql (length type1) 2))
            ; (unknown)
            ;)
            ;; EQL: Element must belong to type2.
            ((eq head1 'EQL)
             (return-from subtypep (safe-typep (second type1) type2 env)))
            ;; MEMBER: All elements must belong to type2.
            ((eq head1 'MEMBER)
             (let ((all-yes t))
               (dolist (x (rest type1))
                 (multiple-value-bind (is known) (safe-typep x type2 env)
                   (unless is
                     (if known (no) (setq all-yes nil)))))
               (if all-yes (yes) (unknown))))
            ;; OR: Each type must be a subtype of type2.
            ((eq head1 'OR)
             (let ((all-yes t))
               (dolist (type (rest type1))
                 (multiple-value-bind (is known) (subtypep type type2)
                   (unless is
                     (if known (no) (setq all-yes nil)))))
               (if all-yes (yes) (unknown))))))
    (when (consp type2)
      (cond ;; Nothing is known about SATISFIES types.
            ;; But don't signal (unknown) too early.
            ;((and (eq (first type2) 'SATISFIES) (eql (length type2) 2))
            ; (unknown)
            ;)
            ;; AND: type1 must be a subtype of each type.
            ((eq (first type2) 'AND)
             (let ((all-yes t))
               (dolist (type (rest type2))
                 (multiple-value-bind (is known) (subtypep type1 type)
                   (unless is
                     (if known (no) (setq all-yes nil)))))
               (if all-yes (yes) (unknown))))))
    (subtypep-logic (list type1) (list type2))))
;; Flattening the ANDs and ORs uses two mutually recursive functions.
;; Since our LABELS implementation allocates closures at runtime, use global
;; functions instead.
(defvar *subtypep-type1parts*)
(defvar *subtypep-type2parts*)
(defvar *subtypep-type2eqlparts*)
;; Add a canonicalized type to the left-hand side: type1parts.
(defun subtypep-flatten-AND (type)
  (if (consp type)
    (case (first type)
      (AND
       (mapc #'(lambda (x) (subtypep-flatten-AND (canonicalize-type x)))
             (rest type)))
      (NOT
       (subtypep-flatten-OR (canonicalize-type (second type))))
      (t
       (push type *subtypep-type1parts*)))
    (push type *subtypep-type1parts*)))
;; Add a canonicalized type to the right-hand side: type2parts, type2eqlparts.
(defun subtypep-flatten-OR (type)
  (if (consp type)
    (case (first type)
      (OR
       (mapc #'(lambda (x) (subtypep-flatten-OR (canonicalize-type x)))
             (rest type)))
      (NOT
       (subtypep-flatten-AND (canonicalize-type (second type))))
      (MEMBER
       (setq *subtypep-type2eqlparts*
             (revappend (rest type) *subtypep-type2eqlparts*)))
      (EQL
       (push (second type) *subtypep-type2eqlparts*))
      (t
       (push type *subtypep-type2parts*)))
    (push type *subtypep-type2parts*)))
;; Entry point taking lists of canonicalized type specs.
(defun subtypep-logic (types1 types2)
  (macrolet ((yes () '(return-from subtypep-logic (values t t)))
             (no () '(return-from subtypep-logic (values nil t)))
             (unknown () '(return-from subtypep-logic (values nil nil))))
    ;; Logic simplification: (subtypep type1 type2) is equivalent to
    ;; (subtypep (and type1 (not type2)) nil). Therefore the interesting
    ;; irreducible case is when type1 is an (AND ...) and type2 is an (OR ...).
    ;; Write type1 as `(AND ,@type1parts)
    ;; and type2 as `(OR ,@type2parts (MEMBER ,@type2eqlparts)),
    ;; shuffling around the NOTs according to the rules
    ;;   (subtypep `(AND ,@a (NOT ,c)) `(OR ,@b)) <==> (subtypep `(AND ,@a) `(OR ,@b ,c))
    ;;   (subtypep `(AND ,@a) `(OR ,@b (NOT ,c))) <==> (subtypep `(AND ,@a ,c) `(OR ,@b))
    (let (type1parts type2parts type2eqlparts)
      (let ((*subtypep-type1parts* '())
            (*subtypep-type2parts* '())
            (*subtypep-type2eqlparts* '()))
        (mapc #'subtypep-flatten-AND types1)
        (mapc #'subtypep-flatten-OR types2)
        (setq type1parts (nreverse *subtypep-type1parts*))
        (setq type2parts (nreverse *subtypep-type2parts*))
        (setq type2eqlparts (nreverse *subtypep-type2eqlparts*)))
      ;; type1parts and type2parts are now lists of canonicalized types.
      ;; Now: None of the type1parts is an AND.
      ;;      None of the type2parts is an OR or MEMBER/EQL.
      ;; Remove trivialities:
      (when (member '(OR) type1parts :test #'equal)
        ; The left-hand side is equivalent to the type NIL.
        (yes))
      (when (member '(AND) type2parts :test #'equal)
        ; The right-hand side is equivalent to the type T.
        (yes))
      ;; Remove duplicates:
      (setq type1parts (remove-duplicates type1parts :test #'canonicalized-types-equal-p))
      (setq type2parts (remove-duplicates type2parts :test #'canonicalized-types-equal-p))
      (setq type2eqlparts (remove-duplicates type2eqlparts))
      (setq type2eqlparts
        (remove-if #'(lambda (x)
                       (dolist (tp type2parts nil)
                         (when (safe-typep x tp) (return t))))
                   (the list type2eqlparts)))
      ;; Enumeratable results:
      ;; Does type1parts contain only a finite set?
      (let ((set 't))
        (dolist (part1 type1parts)
          (when (consp part1)
            (cond ((eq (first part1) 'MEMBER)
                   (let ((l (rest part1)))
                     (setq set (if (eq set 't) l (intersection set l)))))
                  ((eq (first part1) 'EQL)
                   (let ((l (list (second part1))))
                     (setq set (if (eq set 't) l (intersection set l))))))))
        (unless (eq set 't) ; Is type1parts entirely enumerated?
          ; Yes, so we can decide the result by a finite number of TYPEP calls.
          (let ((all-yes t))
            (dolist (x set)
              (multiple-value-bind (is1 known1)
                  ; Is x eliminated by `(AND ,@type1parts) ?
                  (let ((all-yes t))
                    (dolist (tp type1parts (values nil all-yes))
                      (multiple-value-bind (is known) (safe-typep x tp)
                        (unless is
                          (if known (return (values t t)) (setq all-yes nil))))))
                (multiple-value-bind (is2 known2)
                    ; Is x contained in `(OR ,@type2parts (MEMBER ,@type2eqlparts)) ?
                    (let ((all-known t))
                      (dolist (tp type2parts
                               (if (member x type2eqlparts) (values t t) (values nil all-known)))
                        (multiple-value-bind (is known) (safe-typep x tp)
                          (when is (return (values t t)))
                          (unless known (setq all-known nil)))))
                  (cond ((and known1 known2)
                         (unless (or is1 is2) (no)))
                        (known1 (unless is1 (setq all-yes nil)))
                        (known2 (unless is2 (setq all-yes nil)))
                        (t (setq all-yes nil))))))
            (if all-yes (yes) (unknown)))))
      ;; Now: None of the type1parts is an AND or MEMBER/EQL.
      ;;      None of the type2parts is an OR or MEMBER/EQL.
      ;; Handle `(OR ...) in type1parts:
      (let ((reducible-part1
              (member-if #'(lambda (part1)
                             (and (consp part1) (eq (first part1) 'OR)))
                         type1parts)))
        (when reducible-part1
          (let ((type1parts-before (ldiff type1parts reducible-part1))
                (type1parts-after (rest reducible-part1))
                (all-yes t))
            (dolist (sub (rest (first reducible-part1)))
              (setq sub (canonicalize-type sub))
              (multiple-value-bind (is known)
                  (subtypep-logic
                    (append type1parts-before (list sub) type1parts-after)
                    (append type2parts (if type2eqlparts `((MEMBER ,@type2eqlparts)) '())))
                (unless is
                  (if known (no) (setq all-yes nil)))))
            (if all-yes (yes) (unknown)))))
      ;; Handle `(AND ...) in type2parts:
      (let ((reducible-part2
              (member-if #'(lambda (part2)
                             (and (consp part2) (eq (first part2) 'AND)))
                         type2parts)))
        (when reducible-part2
          (let ((type2parts-before (ldiff type2parts reducible-part2))
                (type2parts-after (rest reducible-part2))
                (all-yes t))
            (dolist (sub (rest (first reducible-part2)))
              (setq sub (canonicalize-type sub))
              (multiple-value-bind (is known)
                  (subtypep-logic
                    type1parts
                    (append type2parts-before (list sub) type2parts-after
                            (if type2eqlparts `((MEMBER ,@type2eqlparts)) '())))
                (unless is
                  (if known (no) (setq all-yes nil)))))
            (if all-yes (yes) (unknown)))))
      ;; Now: None of the type1parts is an AND, OR or MEMBER/EQL.
      ;;      None of the type2parts is an AND, OR or MEMBER/EQL.
      ;; Simple results:
      ;; Is some part of type1parts the same as a part of type2parts?
      (dolist (part1 type1parts)
        (dolist (part2 type2parts)
          (when (canonicalized-types-equal-p part1 part2)
            (yes))))
      #|
      ;; Is some part of type1parts a subtype of a part of type2parts?
      ;; FIXME: Is this test good or bad for performance?
      (when (or (> (length type1parts) 1) ; avoid infinite recursion
                (> (length type2parts) 1))
        (dolist (part1 type1parts)
          (dolist (part2 type2parts)
            (when (subtypep-logic (list part1) (list part2))
              (yes)))))
      |#
      (when type2eqlparts
        ;; Handle the type2eqlparts.
        ;; The type1parts are either
        ;;   - built-in types that have an arbitrary number of instances, or
        ;;   - numeric or character types, or
        ;;   - SATISFIES types, which are undecidable anyway.
        ;; From each of them we remove each of the type2eqlparts. (It would
        ;; be necessary to remove each of the type2eqlparts only from _one_ of
        ;; the type1parts, but we don't know in advance which one is the best.)
        (let ((modified nil))
          (do ((type2eqlpartsr type2eqlparts))
              ((null type2eqlpartsr))
            (let ((obj (pop type2eqlpartsr)))
              (when (or (realp obj) (characterp obj))
                (let ((got-OR nil))
                  (mapl #'(lambda (l)
                            (let ((type1part (car l)))
                              (cond ((and (realp obj)
                                          (if (atom type1part)
                                            (member type1part
                                                    '(REAL INTEGER RATIONAL FLOAT SHORT-FLOAT
                                                      SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
                                            (eq (first type1part) 'INTERVALS)))
                                     (let ((type1part-without-obj
                                             (subtypep-REAL-remove-singleton
                                               type1part obj)))
                                       (when (eq type1part-without-obj 'NIL)
                                         (yes))
                                       (unless (equal type1part-without-obj type1part)
                                         (setq modified t)
                                         (when (and (consp type1part-without-obj)
                                                    (eq (first type1part-without-obj) 'OR))
                                           (setq got-OR t)))
                                       (setf (car l) type1part-without-obj)))
                                    ((and (characterp obj)
                                          (if (atom type1part)
                                            (eq type1part 'CHARACTER)
                                            (eq (first type1part) 'CHARACTER-INTERVALS)))
                                     (let ((type1part-without-obj
                                             (subtypep-CHARACTER-remove-singleton
                                               type1part obj)))
                                       (when (eq type1part-without-obj 'NIL)
                                         (yes))
                                       (unless (equal type1part-without-obj type1part)
                                         (setq modified t)
                                         ; `(OR ...) types cannot occur here.
                                         (when (consp type1part-without-obj)
                                           (assert (eq (first type1part-without-obj) 'CHARACTER-INTERVALS))))
                                       (setf (car l) type1part-without-obj))))))
                        type1parts)
                  (when got-OR
                    ; Eliminate the OR, then continue processing.
                    (return-from subtypep-logic
                      (subtypep-logic
                        type1parts
                        (append type2parts
                                (if type2eqlpartsr `((MEMBER ,@type2eqlpartsr)) '())))))))))
          (when modified
            ;; Do the simple tests once again.
            ;; Is some part of type1parts the same as a part of type2parts?
            (dolist (part1 type1parts)
              (dolist (part2 type2parts)
                (when (canonicalized-types-equal-p part1 part2)
                  (yes)))))))
      ;; Now we can forget about type2eqlparts.
      ;; Now it's time to group the type1parts and type2parts into categories.
      (let ((type1category nil))
        (dolist (type1part type1parts)
          (let ((category (type-category type1part)))
            (if category
              (cond ((null type1category)
                     (setq type1category category))
                    ((eq type1category category))
                    (t ; Encountered two different categories.
                      ; `(AND ,@type1parts) therefore is the empty type.
                      (yes))))))
        (unless type1category
          ;; Could not categorize the type1parts.
          (when (null type1parts)
            ;; No wonder: the left-hand side is equivalent to the type T.
            (let ((try-prove-no t))
              (dolist (type2part type2parts)
                (unless (type-category type2part) (setq try-prove-no nil)))
              (when try-prove-no
                ;; No right-hand side can possibly cover all possible objects
                ;; (think of implementation dependent extra misc type
                ;; categories), therefore we can say
                (no))))
          (unknown))
        ;; Keep only the parts belonging to the same category.
        ;; Set try-prove-no to nil if we remove some uncategorized parts
        ;; from type1parts.
        (let ((try-prove-no t))
          (setq type1parts
                (remove-if-not #'(lambda (type1part)
                                   (let ((category (type-category type1part)))
                                     (if (eq category type1category)
                                       t
                                       (progn
                                         (setq try-prove-no nil)
                                         nil))))
                               (the list type1parts)))
          (setq type2parts
                (remove-if-not #'(lambda (type2part)
                                   (let ((category (type-category type2part)))
                                     (if (eq category type1category)
                                       t
                                       (progn
                                         (unless category (setq try-prove-no nil))
                                         nil))))
                               (the list type2parts)))
          ;; Operate using the functions specific to the category.
          (let ((and-simplifier (get type1category 'SUBTYPEP-SIMPLIFY-AND))
                (or-simplifier (get type1category 'SUBTYPEP-SIMPLIFY-OR))
                (decider (get type1category 'SUBTYPEP-DECIDE)))
            ;; Simplify the type1parts. (It is still a non-empty list.)
            ;; Also see if it evidently represents the empty type.
            (let ((type1 (funcall and-simplifier type1parts)))
              (when (eq type1 'NIL)
                ;; The type1parts represent the empty type.
                (yes))
              ;; Simplify the type2parts.
              (setq type2parts (funcall or-simplifier type2parts))
              ;; Now perform the decision.
              (funcall decider type1 type2parts try-prove-no))))))))

#|
;; Debugging hints:
 (in-package "SYSTEM")
 (setf (package-lock *system-package-list*) nil)
 (trace sys::simplify-and-array sys::simplify-or-array sys::subtypep-array
        sys::simplify-and-complex sys::simplify-or-complex sys::subtypep-complex
        sys::simplify-and-cons sys::simplify-or-cons sys::subtypep-cons
        sys::simplify-and-real sys::simplify-or-real sys::subtypep-real
        sys::intervals-mapto sys::intervals-intersection sys::intervals-union sys::intervals-subtypep
        sys::intervals-typep sys::intervals-integer-closure
        sys::intervals-remove-one sys::subtypep-REAL-remove-singleton
        sys::simplify-and-character sys::simplify-or-character sys::subtypep-character
        sys::subtypep-CHARACTER-pre-simplify sys::subtypep-CHARACTER-remove-singleton
        sys::subtypep-pathname
        sys::subtypep-misc
        sys::simplify-and-structure-object sys::subtypep-structure-object
        sys::simplify-and-standard-object sys::subtypep-standard-object
        sys::type-category
        sys::canonicalize-type
        subtypep sys::subtypep-logic)
|#
