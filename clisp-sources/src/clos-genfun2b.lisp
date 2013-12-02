;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part 2: Generic function dispatch and execution
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ================ Combination of Applicable Methods, Part 3 ================

;;; Calculate the effective method for the given arguments.
;;; It is actually the effective method for all arguments,
;;; for the same EQL and class restrictions as the given arguments,
;;; therefore compute dispatch is already taken care of.
(defun compute-applicable-methods-effective-method (gf &rest args)
  (when (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    (return-from compute-applicable-methods-effective-method
      (no-method-caller 'no-applicable-method gf)))
  (let ((req-num (sig-req-num (safe-gf-signature gf))))
    (if (>= (length args) req-num)
      (let ((req-args (subseq args 0 req-num)))
        (multiple-value-bind (methods originator)
            ;; compute-applicable-methods would be sufficient, but the MOP p. 40
            ;; specifies a two-step process.
            (multiple-value-bind (methods certain)
                (funcall (cond ((eq gf |#'compute-applicable-methods-using-classes|)
                                #'compute-applicable-methods-using-classes-<generic-function>)
                               (t #'compute-applicable-methods-using-classes))
                         gf (mapcar #'class-of req-args))
              (if certain
                (values methods 'compute-applicable-methods-using-classes)
                (values
                  (funcall (cond ((or (eq gf |#'compute-applicable-methods|)
                                      (eq gf |#'compute-effective-method|))
                                  #'compute-applicable-methods-<generic-function>)
                                 (t #'compute-applicable-methods))
                           gf args)
                  'compute-applicable-methods)))
          ; Some checks, to guarantee that user-defined methods on
          ; compute-applicable-methods or compute-applicable-methods-using-classes
          ; don't break our CLOS.
          (unless (proper-list-p methods)
            (error (TEXT "Wrong ~S result for generic function ~S: not a proper list: ~S")
                   originator gf methods))
          (dolist (m methods)
            (unless (typep-class m <method>)
              (error (TEXT "Wrong ~S result for generic function ~S: list element is not a method: ~S")
                     originator gf m)))
          ;; Combine the methods to an effective method:
          (or (cdr (assoc methods (std-gf-effective-method-cache gf) :test #'equal))
              (let ((effective-method
                      (compute-effective-method-as-function gf methods args)))
                (push (cons methods effective-method) (std-gf-effective-method-cache gf))
                effective-method))))
      (error (TEXT "~S: ~S has ~S required argument~:P, but only ~S arguments were passed to ~S: ~S")
             'compute-applicable-methods-effective-method gf req-num (length args)
             'compute-applicable-methods-effective-method args))))

;; compute-applicable-methods-effective-method-for-set
;; is a generalization of compute-applicable-methods-effective-method.
;; For each argument position you can specify a set of possible required
;; arguments through one of:
;;   (TYPEP class)          [covers direct and indirect instances of class],
;;   (INSTANCE-OF-P class)  [covers only direct instances of class],
;;   (EQL object)           [covers the given object only].
;; Returns either the best possible effective-method, the gf itself otherwise.
(defun compute-applicable-methods-effective-method-for-set (gf req-arg-specs tentative-args)
  (when (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    (return-from compute-applicable-methods-effective-method-for-set
      (no-method-caller 'no-applicable-method gf)))
  (let ((req-num (sig-req-num (safe-gf-signature gf))))
    (if (and (= (length req-arg-specs) req-num)
             (>= (length tentative-args) req-num))
      (multiple-value-bind (methods certain)
          (compute-applicable-methods-for-set gf req-arg-specs)
        (unless certain
          (return-from compute-applicable-methods-effective-method-for-set gf))
        ;; Combine the methods to an effective method:
        (or (cdr (assoc methods (std-gf-effective-method-cache gf) :test #'equal))
            (let ((effective-method
                    (compute-effective-method-as-function gf methods tentative-args)))
              (push (cons methods effective-method) (std-gf-effective-method-cache gf))
              effective-method)))
      (error (TEXT "~S: ~S has ~S required argument~:P")
             'compute-applicable-methods-effective-method-for-set gf req-num))))

(defun compute-effective-method-as-function (gf methods args)
  (when (null methods)
    (return-from compute-effective-method-as-function
      (no-method-caller 'no-applicable-method gf)))
  ;; Apply method combination:
  (let ((ef-fun (compute-effective-method-as-function-form gf (safe-gf-method-combination gf) methods args)))
    ;; Evaluate or compile the resulting form:
    (if (constantp ef-fun) ; constant or self-evaluating form?
      ;; No need to invoke the compiler for a constant form.
      ef-fun
      ;; For a general form:
      ;; (eval ef-fun)                                 ; interpreted
      ;; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiled
      (eval `(LET () (DECLARE (COMPILE) (INLINE FUNCALL APPLY))
               ,ef-fun)))))

(defun no-method-caller (no-method-name gf)
  #'(lambda (&rest args) (apply no-method-name gf args)))


;; ======================= Computing the Dispatch Code =======================

;; The dispatch-code for generic functions is formed with
;; `(%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
;; - similar to `(FUNCTION (LAMBDA ,@lambdabody)) -.
;; The following must not occur therein:
;; - access to dynamic variables, binding of dynamic variables,
;; - nontrivial BLOCK, RETURN-FROM, TAGBODY, GO constructions,
;; - invocation of global functions, that are not inline,
;; - formation of non-autonomous functions (closures).
;; So the following is necessary:
;;   (declare (inline case eql eq typep
;;                    arrayp bit-vector-p characterp complexp consp floatp
;;                    functionp hash-table-p integerp listp null numberp
;;                    packagep pathnamep sys::logical-pathname-p random-state-p
;;                    rationalp readtablep realp sys::sequencep
;;                    clos::std-instance-p streamp sys::file-stream-p
;;                    sys::synonym-stream-p sys::broadcast-stream-p
;;                    sys::concatenated-stream-p sys::two-way-stream-p
;;                    sys::echo-stream-p sys::string-stream-p stringp
;;                    clos::structure-object-p symbolp vectorp
;;                    class-of cons gethash funcall apply ...
;;   )        )

;; Preliminary.
(defun compute-discriminating-function (gf)
  (compute-discriminating-function-<generic-function> gf))

(defun compute-discriminating-function-<generic-function> (gf)
  (multiple-value-bind (bindings lambdabody) (compute-dispatch gf)
    (let ((preliminary
            (eval `(LET ,bindings
                     (DECLARE ,@(safe-gf-declspecs gf) (COMPILE))
                     (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
      (assert (<= (sys::%record-length preliminary) 3))
      preliminary)))

;; Calculates the dispatch-code of a generic function.
;; It looks as follows:
;; (LAMBDA (variables) ; the required vars separately, everything else with &rest
;;   (DECLARE (INLINE ...)) ; everything inline because of %GENERIC-FUNCTION-LAMBDA
;;   If-cascades, where EQL-parameter-specializers and most of the
;;   builtin-classes are queried online via TYPEP.
;;   CLASS-OF is called for the other required-parameters, the results
;;   are gathered and inserted into a hash table as index. There, the effective
;;   method is located:
;;   (LET ((EM (GETHASH (CONS (CLASS-OF ...) ...) ht1)))
;;     (WHEN EM (RETURN-FROM block (APPLY EM Arguments))))
;;   If that failed:
;;   (APPLY 'COMPUTE-AND-ADD-EFFECTIVE-METHOD gf Arguments)
;; )
;; One does not need to write (APPLY ... Arguments),
;; it is done by %GENERIC-FUNCTION-LAMBDA automatically.
(defun compute-dispatch (gf)
  (when (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    (return-from compute-dispatch
      (values
        '()
        `((&REST ,(gensym))
          (DECLARE (INLINE FUNCALL))
          (FUNCALL 'NO-METHOD-CALLER 'NO-APPLICABLE-METHOD ',gf)))))
  (let* ((signature (safe-gf-signature gf))
         (req-anz (sig-req-num signature))
         (req-vars (gensym-list req-anz))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (arg-order (safe-gf-argorder gf))
         (methods (safe-gf-methods gf))
         (block-name (gensym))
         (maybe-no-applicable nil)
         (ht-vars '())) ; list of hashtable variables and their inits
    ;; We do a recursion over the arguments.
    (labels
       ((recursion (remaining-args ; an nthcdr of arg-order
                    remaining-methods ; sublist of methods
                    class-of-exprs) ; list of CLASS-OF expressions
          (if (null remaining-methods)
            (progn
              (setq maybe-no-applicable t)
              'NIL) ; nothing to do, call NO-APPLICABLE-METHOD later
            (if (null remaining-args)
              ;; All arguments processed.
              #|| ; use GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    ht-key-binding ; binding of a variable to an n-tuple
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        ht-key-binding '()
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (let ((tuple-var (gensym)))
                    (setq ht-init
                          `(MAKE-HASH-TABLE
                             ; :KEY-TYPE '(CONS ... CLASS ...) :VALUE-TYPE 'FUNCTION
                             :TEST ',(if (eql n 1) 'EXT:STABLEHASH-EQ 'EXT:STABLEHASH-EQUAL)
                             :WARN-IF-NEEDS-REHASH-AFTER-GC 'T)
                          ht-key-binding
                          `((,tuple-var
                             ,(let ((tuple-fun (hash-tuple-function n)))
                                (if (member '&rest (second tuple-fun))
                                  `(,tuple-fun ,@(reverse class-of-exprs))
                                  ;; no &rest -> can optimize
                                  ;; (the compiler is not yet too good at that)
                                  (sublis (mapcar #'cons (second tuple-fun) (reverse class-of-exprs))
                                          (third tuple-fun))))))
                          em-expr
                          `(GETHASH ,tuple-var ,ht-var)
                          setf-em-expr
                          ;; `(SETF (GETHASH ,tuple-var ,ht-var)) would also work;
                          ;; but the following spares two temporary variables:
                          `(SYSTEM::PUTHASH ,tuple-var ,ht-var))))
                (push (list ht-var ht-init) ht-vars)
                `(LET ,ht-key-binding
                   (RETURN-FROM ,block-name
                     (OR ,em-expr
                         (,@setf-em-expr
                               (,apply-fun 'COMPUTE-APPLICABLE-METHODS-EFFECTIVE-METHOD
                                           ',gf ,@apply-args))))))
              |# ; use CLASS-GETHASH and CLASS-TUPLE-GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (setq class-of-exprs
                        (reverse class-of-exprs)
                        ht-init
                        `(MAKE-HASH-TABLE
                           ; :KEY-TYPE '(CONS ... CLASS ...) :VALUE-TYPE 'FUNCTION
                           :TEST ',(if (eql n 1) 'EXT:STABLEHASH-EQ 'EXT:STABLEHASH-EQUAL)
                           :WARN-IF-NEEDS-REHASH-AFTER-GC 'T)
                        em-expr
                        (if (eql n 1) ; whatever is faster
                          ;; `(GETHASH ,@class-of-exprs ,ht-var) ==
                          `(CLASS-GETHASH ,ht-var ,(second (first class-of-exprs)))
                          `(CLASS-TUPLE-GETHASH ,ht-var ,@(mapcar #'second class-of-exprs)))
                        setf-em-expr
                        `(SYSTEM::PUTHASH
                          ,(let ((tuple-fun (hash-tuple-function n)))
                             (if (memq '&rest (second tuple-fun))
                               `(,tuple-fun ,@class-of-exprs)
                               ;; no &rest -> can optimize
                               ;; (the compiler is not yet too good at that)
                               (sublis (mapcar #'cons (second tuple-fun) class-of-exprs)
                                       (third tuple-fun))))
                          ,ht-var)))
                (push (list ht-var ht-init) ht-vars)
                `(RETURN-FROM ,block-name
                   (OR ,em-expr
                       (,@setf-em-expr
                             (,apply-fun 'COMPUTE-APPLICABLE-METHODS-EFFECTIVE-METHOD
                                         ',gf ,@apply-args)))))
              ;; Process next argument:
              (let* ((arg-index (first remaining-args))
                     (arg-var (nth arg-index req-vars))
                     (eql-cases ; all EQL-specializers for this argument
                       (remove-duplicates
                         (mapcar #'eql-specializer-object
                           (remove-if-not #'eql-specializer-p
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (safe-method-specializers m gf)))
                               remaining-methods)))
                         :test #'eql))
                     (eql-caselist ; case-list for CASE
                       (mapcar
                         #'(lambda (object)
                             `((,object)
                               ,(recursion
                                  (cdr remaining-args)
                                  (remove-if-not
                                    #'(lambda (m)
                                        (typep object
                                          (nth arg-index
                                            (safe-method-specializers m gf))))
                                    (the list remaining-methods))
                                  class-of-exprs)))
                         eql-cases)))
                ;; Until further notice we do not need to consider the
                ;; EQL-specialized methods anymore.
                (setq remaining-methods
                      (remove-if
                        #'(lambda (m)
                            (eql-specializer-p
                              (nth arg-index (safe-method-specializers m gf))))
                        (the list remaining-methods)))
                ((lambda (other-cases)
                   (if eql-caselist
                     `(CASE ,arg-var ,@eql-caselist (T ,other-cases))
                     other-cases))
                 (let ((classes
                         (delete <t>
                           (delete-duplicates
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (safe-method-specializers m gf)))
                                     remaining-methods)))))
                   ;; If all classes that are to be tested for are
                   ;; built-in-classes, then we will inline the type-dispatch,
                   ;; because in the hierarchy of the built-in-classes
                   ;; (that does not know multiple inheritance except for NULL
                   ;; and VECTOR) all CPLs are consistent.
                   ;; Hence, we can work with
                   ;; (subclassp (class-of obj) class) == (typep obj class)
                   ;; In the other case a hash-table-access is necessary
                   ;; anyway. Then we spare the test for the built-in-
                   ;; classes and include it into the hash-table.
                   (if (and (every #'bc-p classes)
                            (<= (length classes) 5)) ; too many cases -> hash
                     (labels
                        ((built-in-subtree (class remaining-classes remaining-methods)
                           ;; treats the cases, when the argument belongs to
                           ;; the Class class and affiliation to one of the
                           ;; remaining-classes has to be tested.
                           ;; (One can presume that (bc-and class x) /= nil
                           ;; for all x from remaining-classes.)
                           (if (null remaining-classes)
                             ;; case differentiation is no longer necessary
                             (recursion
                               (cdr remaining-args)
                               (remove-if-not
                                 #'(lambda (m)
                                     (bc-and class
                                       (nth arg-index
                                         (safe-method-specializers m gf))))
                                 (the list remaining-methods))
                               class-of-exprs)
                             ;; case differentiation via TYPEP
                             (let ((test-class (first remaining-classes)))
                               ;; better choose test-class maximal:
                               (loop
                                 (let ((other-class
                                         (find-if
                                           #'(lambda (x)
                                               (and (subclassp test-class x)
                                                    (not (eq test-class x))))
                                           (the list remaining-classes))))
                                   (unless other-class (return))
                                   (setq test-class other-class)))
                               `(IF (TYPEP ,arg-var ',(class-classname test-class))
                                  ,(built-in-subtree
                                     (bc-and class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and x test-class))
                                         (remove test-class remaining-classes)))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and
                                             (nth arg-index
                                               (safe-method-specializers m gf))
                                             test-class))
                                       (the list remaining-methods)))
                                  ,(built-in-subtree
                                     (bc-and-not class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and-not x test-class))
                                         remaining-classes))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and-not
                                             (nth arg-index
                                               (safe-method-specializers m gf))
                                             test-class))
                                       (the list remaining-methods))))))))
                       (built-in-subtree <t> classes remaining-methods))
                     (recursion
                       (cdr remaining-args)
                       remaining-methods
                       (cons `(CLASS-OF ,arg-var) class-of-exprs))))))))))
      (let ((form (recursion arg-order methods '())))
        (values
          ;; bindings
          (nreverse ht-vars)
          ;; lambdabody
          `((,@req-vars ,@(if restp `(&REST ,rest-var) '()))
            (DECLARE
              (INLINE
               ;; for the case differentiations:
               CASE EQL EQ TYPEP
               ;; at the inline-expansion of TYPEP on built-in-classes:
               ARRAYP BIT-VECTOR-P CHARACTERP COMPLEXP CONSP FLOATP
               FUNCTIONP HASH-TABLE-P INTEGERP LISTP NULL NUMBERP
               PACKAGEP PATHNAMEP SYS::LOGICAL-PATHNAME-P RANDOM-STATE-P
               RATIONALP READTABLEP REALP SYS::SEQUENCEP
               CLOS::STD-INSTANCE-P STREAMP SYS::FILE-STREAM-P
               SYS::SYNONYM-STREAM-P SYS::BROADCAST-STREAM-P
               SYS::CONCATENATED-STREAM-P SYS::TWO-WAY-STREAM-P
               SYS::ECHO-STREAM-P SYS::STRING-STREAM-P STRINGP
               CLOS::STRUCTURE-OBJECT-P SYMBOLP VECTORP
               ;; looking up and calling of the effective method:
               CLASS-OF CONS GETHASH CLASS-GETHASH CLASS-TUPLE-GETHASH
               SYS::PUTHASH FUNCALL APPLY))
            (BLOCK ,block-name
              ,form
              ,@(if maybe-no-applicable
                  `((FUNCALL 'NO-METHOD-CALLER 'NO-APPLICABLE-METHOD
                             ',gf))))))))))

;; Our EQUAL hash-function looks into cons-trees only upto depth 4.
;; A tuple of at most 16 elements can be turned into such a tree.
(defun hash-tuple-function (n) ; n>0
  (case n
    (1 '(lambda (t1) t1))
    (2 '(lambda (t1 t2) (cons t1 t2)))
    (3 '(lambda (t1 t2 t3) (cons t1 (cons t2 t3))))
    (4 '(lambda (t1 t2 t3 t4) (cons (cons t1 t2) (cons t3 t4))))
    (5 '(lambda (t1 t2 t3 t4 t5) (cons (cons t1 t2) (cons t3 (cons t4 t5)))))
    (6 '(lambda (t1 t2 t3 t4 t5 t6)
         (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))))
    (7 '(lambda (t1 t2 t3 t4 t5 t6 t7)
         (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))))
    (8 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 t8))) ))
    (9 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 (cons t8 t9))))))
    (10 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
          (cons (cons (cons t1 t2) (cons t3 t4))
           (cons (cons t5 t6) (cons (cons t7 t8) (cons t9 t10))))))
    (11 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons t5 (cons t6 t7))
             (cons (cons t8 t9) (cons t10 t11))))))
    (12 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons (cons t5 t6) (cons t7 t8))
             (cons (cons t9 t10) (cons t11 t12))))))
    (13 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)
           (cons (cons (cons t1 t2) (cons t3 (cons t4 t5)))
            (cons (cons (cons t6 t7) (cons t8 t9))
             (cons (cons t10 t11) (cons t12 t13))))))
    (14 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)
           (cons (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))
            (cons (cons (cons t7 t8) (cons t9 t10))
             (cons (cons t11 t12) (cons t13 t14))))))
    (15 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
           (cons (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))
            (cons (cons (cons t8 t9) (cons t10 t11))
             (cons (cons t12 t13) (cons t14 t15))))))
    (16 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16)
           (cons (cons (cons (cons t1 t2) (cons t3 t4))
                  (cons (cons t5 t6) (cons t7 t8)))
            (cons (cons (cons t9 t10) (cons t11 t12))
             (cons (cons t13 t14) (cons t15 t16))))))
    (t '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 &rest more-t)
          (cons (cons (cons (cons t1 t2) (cons t3 t4))
                 (cons (cons t5 t6) (cons t7 t8)))
           (cons (cons (cons t9 t10) (cons t11 t12))
            (cons (cons t13 t14) more-t)))))))


;; ===================== Generic Function Initialization =====================

;; Checks a generic-function lambda-list and converts it to a signature.
;; Reports errors through errfunc (a function taking a detail object, an
;; error format string and format string arguments).
(defun generic-function-lambda-list-to-signature (lambdalist errfunc)
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (sys::analyze-generic-function-lambdalist lambdalist errfunc)
    (declare (ignore keyvars))
    (let ((reqnum (length reqvars))
          (optnum (length optvars))
          (restp (or (not (eql rest 0)) keyp))) ; &key implies &rest
      (make-signature :req-num reqnum :opt-num optnum
                      :rest-p restp :keys-p keyp
                      :keywords keywords :allow-p allowp))))

; n --> list (0 ... n-1)
(defun countup (n)
  (do* ((count n (1- count))
        (l '() (cons count l)))
       ((eql count 0) l)))

;; Checks an argument-precedence-order list and converts it to a list of
;; indices. reqvars is the required-variables portion of the lambda-list.
;; Reports errors through errfunc (a function taking a detail object, an
;; error format string and format string arguments).
(defun generic-function-argument-precedence-order-to-argorder (argument-precedence-order reqnum reqvars errfunc)
  (let ((indices
          (mapcar #'(lambda (x)
                      (or (position x reqvars)
                          (funcall errfunc x
                                   (TEXT "~S is not one of the required parameters: ~S")
                                   x argument-precedence-order)))
                  argument-precedence-order)))
    ;; Is argument-precedence-order a permutation of reqvars?
    ;; In other words: Is the mapping
    ;;         argument-precedence-order --> reqvars
    ;; resp.   indices                   --> {0, ..., reqnum-1}
    ;; bijective?
    (unless (or (<= (length indices) 1) (apply #'/= indices)) ; injective?
      (funcall errfunc argument-precedence-order
               (TEXT "Some variable occurs twice in ~S")
               argument-precedence-order))
    (unless (eql (length indices) reqnum) ; surjective?
      (let ((missing (set-difference reqvars argument-precedence-order)))
        (funcall errfunc missing
                 (TEXT "The variables ~S are missing in ~S")
                 missing argument-precedence-order)))
    indices))

;; Checks a generic-function lambda-list and argument-precedence-order.
;; Returns three values:
;; 1. the lambda-list's signature,
;; 2. the argument-precedence-order, as a list of variables,
;; 3. the argument-precedence-order, as a list of numbers from 0 to reqnum-1.
;; Reports errors through errfunc (a function taking a detail object, an
;; error format string and format string arguments).
(defun check-gf-lambdalist+argorder (lambdalist argument-precedence-order argument-precedence-order-p errfunc)
  ;; Check the lambda-list.
  (let* ((signature
           (generic-function-lambda-list-to-signature lambdalist
             #'(lambda (detail errorstring &rest arguments)
                 (funcall errfunc detail
                          (TEXT "Invalid generic function lambda-list: ~A")
                          (apply #'format nil errorstring arguments)))))
         (reqnum (sig-req-num signature))
         (reqvars (subseq lambdalist 0 reqnum)))
    (if argument-precedence-order-p
      ;; Check the argument-precedence-order.
      (progn
        (unless (proper-list-p argument-precedence-order)
          (funcall errfunc argument-precedence-order
                   (TEXT "The ~S argument should be a proper list, not ~S")
                   ':argument-precedence-order argument-precedence-order))
        (let ((indices
                (generic-function-argument-precedence-order-to-argorder
                  argument-precedence-order reqnum reqvars
                  #'(lambda (detail errorstring &rest arguments)
                      (funcall errfunc detail
                               (TEXT "Incorrect ~S argument: ~A")
                               ':argument-precedence-order
                               (apply #'format nil errorstring arguments))))))
          (values signature argument-precedence-order indices)))
      (values signature reqvars (countup reqnum)))))

;; Checks a generic-function declspecs list.
(defun check-gf-declspecs (declspecs keyword errfunc)
  (unless (proper-list-p declspecs)
    (funcall errfunc (TEXT "The ~S argument should be a proper list, not ~S")
             keyword declspecs))
  (dolist (declspec declspecs)
    (unless (and (consp declspec) (eq (first declspec) 'OPTIMIZE))
      (funcall errfunc (TEXT "In the ~S argument, only ~S declarations are permitted, not ~S")
               keyword 'optimize declspec))))

;; CLtL2 28.1.6.4., ANSI CL 7.6.4. Congruent Lambda-lists
(defun check-signature-congruence (gf method
                                   &optional (gf-sign (safe-gf-signature gf))
                                             (m-sign (method-signature method)))
  (unless (= (sig-req-num m-sign) (sig-req-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D required parameter~:P")
      method (sig-req-num m-sign) gf (sig-req-num gf-sign)))
  (unless (= (sig-opt-num m-sign) (sig-opt-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D optional parameter~:P")
      method (sig-opt-num m-sign) gf (sig-opt-num gf-sign)))
  (when (and (sig-rest-p m-sign) (not (sig-rest-p gf-sign)))
    (error-of-type 'error
      #1=(TEXT "~S accepts &REST or &KEY, but ~S does not.")
      method gf))
  (when (and (sig-rest-p gf-sign) (not (sig-rest-p m-sign)))
    (error-of-type 'error
      #1# gf method))
  (when (sig-keys-p gf-sign)    ; gf has keywords?
    ;; yes ==> method must accept it
    (unless (if (sig-keys-p m-sign)
              (or (sig-allow-p m-sign) ; keywords match
                  (subsetp (sig-keywords gf-sign) (sig-keywords m-sign)))
              (sig-rest-p m-sign)) ; method must have &rest!
      (error-of-type 'error
        (TEXT "~S does not accept the keywords ~S of ~S")
        method (sig-keywords gf-sign) gf))))

;; CLtL2 28.1.7.2., 28.1.7.4., ANSI CL 7.6.6.2., 7.6.6.4. Method qualifiers
(defun check-method-qualifiers (gf method
                                &optional (method-combination (safe-gf-method-combination gf)))
  (funcall (method-combination-check-method-qualifiers method-combination)
           gf method-combination method))
(defun invalid-method-qualifiers-error (gf method) ; ABI
  (error-of-type 'program-error
    (TEXT "~S method combination, used by ~S, does not allow the method qualifiers ~:S: ~S")
    (method-combination-name (safe-gf-method-combination gf)) gf
    (method-qualifiers method) method))

;; Initialization of a <standard-generic-function> instance.
(defun shared-initialize-<standard-generic-function> (gf situation &rest args
                                                      &key (name nil name-p)
                                                           (lambda-list nil lambda-list-p)
                                                           (argument-precedence-order nil argument-precedence-order-p)
                                                           (method-class nil method-class-p)
                                                           (method-combination nil method-combination-p)
                                                           (documentation nil documentation-p)
                                                           (declarations nil declarations-p)
                                                           (declare nil declare-p)
                                                      &allow-other-keys
                                                      &aux signature argorder)
  (declare (ignore name name-p))
  (apply #'shared-initialize-<generic-function> gf situation args)
  (when (eq situation 't)
    (setf (std-gf-initialized gf) nil))
  (when (and argument-precedence-order-p (not lambda-list-p))
    (error (TEXT "(~S ~S) for generic function ~S: ~S argument specified without a ~S argument.")
           (if (eq situation 't) 'initialize-instance 'shared-initialize)
           'standard-generic-function (funcallable-name gf)
           ':argument-precedence-order ':lambda-list))
  (when lambda-list-p
    ; Check the lambda-list and argument-precedence-order.
    (multiple-value-setq (signature argument-precedence-order argorder)
        (check-gf-lambdalist+argorder lambda-list
          argument-precedence-order argument-precedence-order-p
          #'(lambda (detail errorstring &rest arguments)
              (declare (ignore detail))
              (error (TEXT "(~S ~S) for generic function ~S: ~A")
                     (if (eq situation 't) 'initialize-instance 'shared-initialize)
                     'standard-generic-function (funcallable-name gf)
                     (apply #'format nil errorstring arguments)))))
    (unless (eq situation 't)
      ;; ANSI CL description of ENSURE-GENERIC-FUNCTION says "If ... the new
      ;; value [for the :lambda-list argument] is congruent with the lambda
      ;; lists of all existing methods or there are no methods, the value is
      ;; changed; otherwise an error is signaled.
      (unless (or (safe-gf-undeterminedp gf)
                  (equalp signature (safe-gf-signature gf)))
        (dolist (method (safe-gf-methods gf))
          (check-signature-congruence gf method signature)))))
  (when (or (eq situation 't) method-class-p)
    ; Check the method-class.
    (unless method-class-p
      (setq method-class <standard-method>))
    (unless (defined-class-p method-class)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a class, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-class method-class))
    (unless (subclassp method-class <method>)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a subclass of ~S, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-class 'method method-class)))
  (when (or (eq situation 't) method-combination-p)
    ; Check the method-combination.
    #| ; Not sure whether giving an error here is appropriate.
    (unless method-combination-p
      (error (TEXT "(~S ~S) for generic function ~S: Missing ~S argument.")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-combination))
    |#
    (unless method-combination-p
      (setq method-combination
            (get-method-combination 'STANDARD '(initialize-instance standard-generic-function))))
    (unless (typep-class method-combination <method-combination>)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a ~S object, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-combination 'method-combination method-combination))
    (unless (eq situation 't)
      (unless (eq method-combination (safe-gf-method-combination gf))
        (dolist (method (safe-gf-methods gf))
          (check-method-qualifiers gf method method-combination)))))
  (when (or (eq situation 't) documentation-p)
    ; Check the documentation.
    (unless (or (null documentation) (stringp documentation))
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a string or NIL, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf) ':documentation documentation)))
  (when (or (eq situation 't) declarations-p declare-p)
    ; Check the declarations.
    ; ANSI CL specifies the keyword :DECLARE for ensure-generic-function, but the
    ; MOP p. 50 specifies the keyword :DECLARATIONS for ensure-generic-function-using-class.
    (when (and declarations-p declare-p)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S and ~S arguments cannot be specified together.")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':declarations ':declare))
    (let ((declspecs (if declare-p declare declarations)))
      (check-gf-declspecs declspecs (if declare-p ':declare ':declarations)
        #'(lambda (errorstring &rest arguments)
            (error (TEXT "(~S ~S) for generic function ~S: ~A")
                   (if (eq situation 't) 'initialize-instance 'shared-initialize)
                   'standard-generic-function (funcallable-name gf)
                   (apply #'format nil errorstring arguments))))))
  ; Fill the slots.
  (when lambda-list-p
    (setf (std-gf-lambda-list gf) lambda-list)
    (setf (std-gf-signature gf) signature)
    (setf (std-gf-argorder gf) argorder))
  (when (or (eq situation 't) method-class-p)
    (setf (std-gf-default-method-class gf) method-class))
  (when (or (eq situation 't) method-combination-p)
    (setf (std-gf-method-combination gf) method-combination))
  (when (or (eq situation 't) documentation-p)
    (setf (std-gf-documentation gf) documentation))
  (when (or (eq situation 't) declarations-p declare-p)
    (setf (std-gf-declspecs gf) (if declare-p declare declarations)))
  (when (eq situation 't)
    (setf (std-gf-methods gf) '()))
  (when (or (eq situation 't)
            ; When reinitializing a generic-function, we need to clear the
            ; caches (cf. MOP p. 40 compute-discriminating-function item (ii)).
            ; But we don't need to do it if we know that all that has changed
            ; are the name, method-class, documentation, declarations, declare
            ; initargs. But other, possibly user-defined, initargs can have an
            ; effect on the discriminating function.
            (do ((l args (cddr l)))
                ((endp l) nil)
              (unless (memq (car l) '(:name :method-class :documentation :declarations :declare))
                (return t))))
    (setf (std-gf-effective-method-cache gf) '()))
  ; Now allow the user to call the generic-function-xxx accessor functions.
  (setf (std-gf-initialized gf) t)
  ; And allow the user to call gf.
  (finalize-fast-gf gf)
  gf)

;; ======================== The Set of Methods of a GF ========================

;; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
;; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - INITIALIZE-INSTANCE must be informed about the methods of
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - REINITIALIZE-INSTANCE must be informed about the methods of
;;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-REDEFINED-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-REDEFINED-CLASS and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-DIFFERENT-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS and SHARED-INITIALIZE.
(defvar |#'allocate-instance| nil)
(defvar |#'initialize-instance| nil)
(defvar |#'reinitialize-instance| nil)
(defvar |#'update-instance-for-redefined-class| nil)
(defvar |#'update-instance-for-different-class| nil)
(defvar |#'shared-initialize| nil)

;; Optimization of SLOT-VALUE and its brothers.
(defvar |#'slot-value-using-class| nil)
(defvar |#'(setf slot-value-using-class)| nil)
(defvar |#'slot-boundp-using-class| nil)
(defvar |#'slot-makunbound-using-class| nil)

(defvar *gf-warn-on-replacing-method* t)

;; More general notification.
(defun map-dependents-<generic-function> (gf function)
  (dolist (dependent (gf-listeners gf))
    (funcall function dependent)))

;; CLtL2 28.1.6.3., ANSI CL 7.6.3.
;; Agreement on Parameter Specializers and Qualifiers
(defun methods-agree-p (method1 method2)
  (and (equal (method-qualifiers method1) (method-qualifiers method2))
       (specializers-agree-p (method-specializers method1)
                             (method-specializers method2))))

;; MOP p. 62 says that the lambda-list of a generic function may become
;; determined only at the moment when the first method is added.
(defun gf-lambdalist-from-first-method (m-lambdalist m-signature)
  (let* ((req-num (sig-req-num m-signature))
         (opt-num (sig-opt-num m-signature))
         (rest-p (sig-rest-p m-signature)))
    (values
      ;; The inferred lambda-list:
      (append (subseq m-lambdalist 0 req-num)
              (if (> opt-num 0)
                (cons '&OPTIONAL
                      (mapcar #'(lambda (item) (if (consp item) (first item) item))
                              (subseq m-lambdalist (+ req-num 1) (+ req-num 1 opt-num))))
                '())
              (if rest-p
                (list '&REST
                      (let ((i (position '&REST m-lambdalist)))
                        (if i (nth (+ i 1) m-lambdalist) (gensym))))
               '()))
      ;; Its corresponding signature:
      (make-signature :req-num req-num :opt-num opt-num :rest-p rest-p))))

;; Add a method to a generic function.
(defun std-add-method (gf method)
  (if (safe-gf-undeterminedp gf)
    ;; The first added method determines the generic-function's signature.
    (shared-initialize-<standard-generic-function> gf nil
      :lambda-list (gf-lambdalist-from-first-method (method-lambda-list method)
                                                    (method-signature method)))
    (check-signature-congruence gf method))
  (when (method-generic-function method)
    (error-of-type 'error
      "~S: ~S already belongs to ~S, cannot also add it to ~S"
      'std-add-method method (method-generic-function method) gf))
  (check-method-qualifiers gf method)
  ;; The method is checked. Now add it:
  (warn-if-gf-already-called gf)
  (let ((old-method (find method (safe-gf-methods gf) :test #'methods-agree-p)))
    (when old-method
      (when *gf-warn-on-replacing-method*
        (warn (TEXT "Replacing method ~S in ~S")
              old-method gf))
      ;; Call remove-method without warnings.
      (let ((*dynamically-modifiable-generic-function-names*
              (cons (sys::closure-name gf) *dynamically-modifiable-generic-function-names*)))
        (remove-method gf old-method))
      ;; Ensure that remove-method really has removed the method.
      (when (memq method (safe-gf-methods gf))
        (error (TEXT "Wrong ~S behaviour: ~S has not been removed from ~S")
               'remove-method old-method gf))))
  (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
        ((eq gf |#'initialize-instance|) (note-ii-change method))
        ((eq gf |#'reinitialize-instance|) (note-ri-change method))
        ((eq gf |#'update-instance-for-redefined-class|) (note-uirc-change method))
        ((eq gf |#'update-instance-for-different-class|) (note-uidc-change method))
        ((eq gf |#'shared-initialize|) (note-si-change method)))
  ;; Step 1: Add method to the set.
  (setf (std-gf-methods gf) (cons method (safe-gf-methods gf))
        (method-generic-function method) gf)
  ;; Step 2: Call add-direct-method for each specializer.
  (dolist (specializer (method-specializers method))
    (add-direct-method specializer method))
  ;; Step 3: Clear the effective method cache and the discriminating function.
  ;; (Cf. MOP p. 41 compute-discriminating-function item (iii).)
  (setf (std-gf-effective-method-cache gf) '())
  (finalize-fast-gf gf)
  ;; Step 4: Update the dependents.
  (cond ((eq gf |#'slot-value-using-class|) (note-svuc-change method))
        ((eq gf |#'(setf slot-value-using-class)|) (note-ssvuc-change method))
        ((eq gf |#'slot-boundp-using-class|) (note-sbuc-change method))
        ((eq gf |#'slot-makunbound-using-class|) (note-smuc-change method)))
  (funcall (if *classes-finished*
             #'map-dependents
             #'map-dependents-<generic-function>)
    gf
    #'(lambda (dependent)
        (update-dependent gf dependent 'add-method method)))
  ;; It's not worth updating the seclass of a generic function, since 1. in
  ;; most cases, it can signal a NO-APPLICABLE-METHOD error and thus has
  ;; *seclass-dirty*, 2. the compiler must assume that the seclass doesn't
  ;; change over time, which we cannot guarantee, since generic functions are
  ;; not sealed.
  gf)

;; Preliminary.
(defun add-method (gf method)
  (std-add-method gf method))

;; Remove a method from a generic function.
(defun std-remove-method (gf method)
  (let ((old-method (find method (safe-gf-methods gf))))
    (when old-method
      (warn-if-gf-already-called gf)
      (when (need-gf-already-called-warning-p gf)
        (warn (TEXT "Removing method ~S in ~S")
              old-method gf))
      (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
            ((eq gf |#'initialize-instance|) (note-ii-change method))
            ((eq gf |#'reinitialize-instance|) (note-ri-change method))
            ((eq gf |#'update-instance-for-redefined-class|) (note-uirc-change method))
            ((eq gf |#'update-instance-for-different-class|) (note-uidc-change method))
            ((eq gf |#'shared-initialize|) (note-si-change method)))
      ;; Step 1: Remove method from the set.
      (setf (std-gf-methods gf) (remove old-method (safe-gf-methods gf))
            (method-generic-function old-method) nil
            (method-from-defgeneric old-method) nil)
      ;; Step 2: Call remove-direct-method for each specializer.
      (dolist (specializer (method-specializers method))
        (remove-direct-method specializer method))
      ;; Step 3: Clear the effective method cache and the discriminating function.
      ;; (Cf. MOP p. 41 compute-discriminating-function item (iii).)
      (setf (std-gf-effective-method-cache gf) '())
      (finalize-fast-gf gf)
      ;; Step 4: Update the dependents.
      (cond ((eq gf |#'slot-value-using-class|) (note-svuc-change method))
            ((eq gf |#'(setf slot-value-using-class)|) (note-ssvuc-change method))
            ((eq gf |#'slot-boundp-using-class|) (note-sbuc-change method))
            ((eq gf |#'slot-makunbound-using-class|) (note-smuc-change method)))
      (funcall (if *classes-finished*
                 #'map-dependents
                 #'map-dependents-<generic-function>)
        gf
        #'(lambda (dependent)
            (update-dependent gf dependent 'remove-method method)))))
  gf)

;; Preliminary.
(defun remove-method (gf method)
  (std-remove-method gf method))

;; Find a method in a generic function.
(defun std-find-method (gf qualifiers specializers &optional (errorp t))
  (unless (listp specializers)
    (error-of-type 'error
      (TEXT "~S: the specializers argument is not a list: ~S")
      'find-method specializers))
  (if (safe-gf-undeterminedp gf)
    ;; Signature not known yet, hence no methods installed.
    (assert (null (safe-gf-methods gf)))
    (progn
      (let ((n (sig-req-num (safe-gf-signature gf))))
        (unless (eql (length specializers) n)
          (error-of-type 'error
            (TEXT "~S: the specializers argument has length ~D, but ~S has ~D required parameter~:P")
            'find-method (length specializers) gf n))
        ; Convert (EQL object) -> #<EQL-SPECIALIZER object>:
        (setq specializers
              (mapcar #'(lambda (specializer)
                          (if (and (consp specializer) (eq (car specializer) 'EQL)
                                   (consp (cdr specializer)) (null (cddr specializer)))
                            (intern-eql-specializer (second specializer))
                            specializer))
                      specializers)))
      ;; Simulate
      ;;   (find hypothetical-method (safe-gf-methods gf) :test #'methods-agree-p)
      ;; cf. methods-agree-p
      (dolist (method (safe-gf-methods gf))
        (when (and (equal (method-qualifiers method) qualifiers)
                   (specializers-agree-p (method-specializers method)
                                         specializers))
          (return-from std-find-method method)))))
  (if errorp
    (error-of-type 'error
      (TEXT "~S has no method with qualifiers ~:S and specializers ~:S")
      gf qualifiers specializers)
    nil))

;; ===================== Generic Function Initialization =====================

(defun initialize-instance-<generic-function> (gf &rest args
                                               &key name
                                                    lambda-list
                                                    argument-precedence-order
                                                    method-class
                                                    method-combination
                                                    documentation
                                                    declarations
                                                    declare
                                                    ((methods methods) nil methods-p) ; from DEFGENERIC
                                               &allow-other-keys)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare))
  (if *classes-finished*
    (apply #'%initialize-instance gf args) ; == (call-next-method)
    ;; During bootstrapping, only <standard-generic-function> instances are used.
    (apply #'shared-initialize-<standard-generic-function> gf 't args))
  (when methods-p
    ;; When invoked from DEFGENERIC: Install the defgeneric-originated methods.
    (dolist (method methods) (add-method gf method)))
  gf)

(defun reinitialize-instance-<generic-function> (gf &rest args
                                                 &key name
                                                      lambda-list
                                                      argument-precedence-order
                                                      method-class
                                                      method-combination
                                                      documentation
                                                      declarations
                                                      declare
                                                      ((methods methods) nil methods-p) ; from DEFGENERIC
                                                 &allow-other-keys)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare))
  (when methods-p
    ;; When invoked from DEFGENERIC:
    ;; Remove the old defgeneric-originated methods. Instead of calling
    ;; std-remove-method on each such method, while inhibiting warnings,
    ;; we can just as well remove the methods directly.
    (setf (std-gf-methods gf)
          (remove-if #'(lambda (method)
                         (when (method-from-defgeneric method)
                           ;; Step 1: Remove method from the set.
                           (setf (method-generic-function method) nil)
                           (setf (method-from-defgeneric method) nil)
                           ;; Step 2: Call remove-direct-method for each specializer.
                           (dolist (specializer (method-specializers method))
                             (remove-direct-method specializer method))
                           t))
                     (safe-gf-methods gf))))
  (apply (cond ((eq (class-of gf) <standard-generic-function>)
                #'shared-initialize-<standard-generic-function>)
               (t #'shared-initialize))
         gf nil args)
  (when methods-p
    ;; When invoked from DEFGENERIC: Install the defgeneric-originated
    ;; methods.
    (dolist (method methods) (add-method gf method)))
  ;; Notification of listeners:
  (map-dependents gf
    #'(lambda (dependent)
        (apply #'update-dependent gf dependent args)))
  gf)

(defun make-instance-<standard-generic-function> (class &rest args
                                                  &key name
                                                       lambda-list
                                                       argument-precedence-order
                                                       method-class
                                                       method-combination
                                                       documentation
                                                       declarations
                                                       declare
                                                  &allow-other-keys)
  ;; class = <standard-generic-function>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare))
  (let ((gf (%allocate-instance <standard-generic-function>)))
    (apply #'initialize-instance-<generic-function> gf args)))

(defun allocate-generic-function-instance (class &rest args
                                           &key &allow-other-keys)
  ;; During bootstrapping, only <standard-generic-function> instances are used.
  (declare (ignore class args))
  (%allocate-instance <standard-generic-function>))

(defun make-generic-function-instance (class &rest args ; ABI
                                       &key &allow-other-keys)
  ;; During bootstrapping, only <standard-generic-function> instances are used.
  (apply #'make-instance-<standard-generic-function> class args))

;; Returns an instance of the given generic-function class that is initialized
;; with just the name, without calling user-defined initialize-instance methods.
(defun make-generic-function-prototype (class &rest args &key name) ; ABI
  (declare (ignore name))
  (let ((instance (allocate-generic-function-instance class)))
    (apply #'shared-initialize-<generic-function> instance 't args)))

;; ======================= Installing the Dispatch Code =======================

#||
(defun make-gf (generic-function-class name lambdabody lambda-list argument-precedence-order method-combination user-defined-args methods)
  (let ((final
          (apply #'make-generic-function-instance generic-function-class
            :name name
            :lambda-list lambda-list
            :argument-precedence-order argument-precedence-order
            :method-combination method-combination
            (mapcan #'(lambda (option) (list (first option) (rest option)))
                    user-defined-args)))
        (preliminary
         (eval `(LET ()
                  (DECLARE (COMPILE))
                  (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
    (assert (<= (sys::%record-length preliminary) 3))
    (set-funcallable-instance-function final preliminary)
    (setf (std-gf-methods final) methods)
    final))
||#


#|| ;; Generic functions with primitive dispatch:

 (defun make-slow-gf (generic-function-class name lambda-list argument-precedence-order method-class declspecs documentation user-defined-args methods)
  (let* ((final
           (apply #'make-generic-function-instance generic-function-class
             :name name
             :lambda-list lambda-list
             :argument-precedence-order argument-precedence-order
             :method-class method-class
             :declarations declspecs
             :documentation documentation
             (mapcan #'(lambda (option) (list (first option) (rest option)))
                     user-defined-args)))
         (preliminary
           (eval `(LET ((GF ',final))
                    (DECLARE (COMPILE))
                    (%GENERIC-FUNCTION-LAMBDA (&REST ARGS)
                      (DECLARE (INLINE APPLY))
                      (APPLY 'SLOW-FUNCALL-GF GF ARGS))))))
    (assert (<= (sys::%record-length preliminary) 3))
    (set-funcallable-instance-function final preliminary)
    (setf (std-gf-methods final) methods)
    final))

(flet ((prototype-factory (gf)
           (declare (compile))
           (%generic-function-lambda (&rest args)
             (declare (inline apply))
             (apply 'slow-funcall-gf gf args))))
  (assert (<= (sys::%record-length (prototype-factory 'dummy)) 3))
  (let ((prototype-code (sys::closure-codevec (prototype-factory 'dummy))))
  (defun finalize-slow-gf (gf)
      (set-funcallable-instance-function gf (prototype-factory gf)))
    (defun gf-never-called-p (gf)
      (eq (sys::closure-codevec gf) prototype-code))
    (defun warn-if-gf-already-called (gf) )))

;; Call of a generic function.
;; Without any caching: Compute the effective method at each call.
 (defun slow-funcall-gf (gf &rest args)
  (unless (>= (length args) (sig-req-num (safe-gf-signature gf)))
      (error-of-type 'program-error
        (TEXT "Too few arguments to ~S: ~S")
        gf args))
  ;; Determine the effective method.
  ;; Return the effective method. It will then be applied to the arguments.
  (apply #'compute-applicable-methods-effective-method gf args))

||#

(defun gf-sig-restp (sig)
  (or (sig-rest-p sig) (> (sig-opt-num sig) 0)))

;; Generic functions with optimized dispatch:

;; First optimization: When the generic function is called, the required
;; arguments are not consed up into an argument list, but rather passed on
;; the stack.

(let ((prototype-factory-table
        (make-hash-table :key-type '(cons fixnum boolean) :value-type '(cons function (simple-array (unsigned-byte 8) (*)))
                         :test 'ext:stablehash-equal :warn-if-needs-rehash-after-gc t))
      (uninitialized-prototype-factory
        (eval `#'(LAMBDA (GF)
                   (DECLARE (COMPILE))
                   (%GENERIC-FUNCTION-LAMBDA (&REST ARGS)
                     (DECLARE (INLINE FUNCALL) (IGNORE ARGS))
                     (FUNCALL 'NO-METHOD-CALLER 'NO-APPLICABLE-METHOD GF))))))
  (defun finalize-fast-gf (gf)
    (let ((prototype-factory
            (if (safe-gf-undeterminedp gf)
              ;; gf has uninitialized lambda-list, hence no methods.
              uninitialized-prototype-factory
              (let* ((signature (safe-gf-signature gf))
                     (reqnum (sig-req-num signature))
                     (restp (gf-sig-restp signature))
                     (hash-key (cons reqnum restp)))
                (car
                  (or (gethash hash-key prototype-factory-table)
                      (setf (gethash hash-key prototype-factory-table)
                            (let* ((reqvars (gensym-list reqnum))
                                   (prototype-factory
                                     (eval `#'(LAMBDA (GF)
                                                (DECLARE (COMPILE))
                                                (%GENERIC-FUNCTION-LAMBDA
                                                  (,@reqvars ,@(if restp '(&REST ARGS) '()))
                                                  (DECLARE (INLINE FUNCALL) (IGNORABLE ,@reqvars ,@(if restp '(ARGS) '())))
                                                  (FUNCALL 'INITIAL-FUNCALL-GF GF))))))
                              (assert (<= (sys::%record-length (funcall prototype-factory 'dummy)) 3))
                              (cons prototype-factory
                                    (sys::closure-codevec (funcall prototype-factory 'dummy)))))))))))
      (set-funcallable-instance-function gf (funcall prototype-factory gf))))
  (defun gf-never-called-p (gf)
    (or (safe-gf-undeterminedp gf)
        (let* ((signature (safe-gf-signature gf))
               (reqnum (sig-req-num signature))
               (restp (gf-sig-restp signature))
               (hash-key (cons reqnum restp))
               (prototype-factory+codevec (gethash hash-key prototype-factory-table)))
          (assert prototype-factory+codevec)
          (eq (sys::closure-codevec gf) (cdr prototype-factory+codevec)))))
  (defvar *dynamically-modifiable-generic-function-names*
    ;; A list of names of functions, which ANSI CL explicitly denotes as
    ;; "Standard Generic Function"s, meaning that the user may add methods.
    '(add-method allocate-instance change-class class-name (setf class-name)
      compute-applicable-methods describe-object documentation
      (setf documentation) find-method function-keywords initialize-instance
      make-instance make-instances-obsolete make-load-form method-qualifiers
      no-applicable-method no-next-method print-object reinitialize-instance
      remove-method shared-initialize slot-missing slot-unbound
      update-instance-for-different-class update-instance-for-redefined-class
      ;; Similar functions from the MOP.
      add-dependent remove-dependent map-dependents
      add-direct-method remove-direct-method
      specializer-direct-generic-functions specializer-direct-methods
      add-direct-subclass remove-direct-subclass class-direct-subclasses
      compute-applicable-methods-using-classes
      compute-class-precedence-list
      compute-default-initargs
      compute-direct-slot-definition-initargs
      compute-discriminating-function
      compute-effective-method
      compute-effective-slot-definition
      compute-effective-slot-definition-initargs
      compute-slots
      direct-slot-definition-class
      effective-slot-definition-class
      ensure-class-using-class
      ensure-generic-function-using-class
      reader-method-class
      slot-value-using-class (setf slot-value-using-class)
      slot-boundp-using-class slot-makunbound-using-class
      validate-superclass
      writer-method-class
      ;; Similar functions that are CLISP extensions.
      (setf method-generic-function) no-primary-method))
  (defvar *warn-if-gf-already-called* t)
  (defun need-gf-already-called-warning-p (gf)
    (and *warn-if-gf-already-called* (not (gf-never-called-p gf))
         (not (member (sys::closure-name gf)
                      *dynamically-modifiable-generic-function-names*
                      :test #'equal))))
  (defun warn-if-gf-already-called (gf)
    (when (need-gf-already-called-warning-p gf)
      (warn (TEXT "The generic function ~S is being modified, but has already been called.")
            gf)))
) ; let


;; Second optimization: The actual dispatch-code is calculated at the first
;; call of the function, in order to make successive method definitions not
;; too expensive.

;; First call of a generic function:
(defun initial-funcall-gf (gf)
  (install-dispatch gf)
  gf)

;; Installs the final dispatch-code into a generic function.
(defun install-dispatch (gf)
  (let ((dispatch
          (funcall (cond ((or (eq gf |#'compute-discriminating-function|) ; for bootstrapping
                              (eq gf |#'compute-applicable-methods-using-classes|))
                          #'compute-discriminating-function-<generic-function>)
                         (t #'compute-discriminating-function))
                   gf)))
    ; Some checks, to guarantee that user-defined methods on
    ; compute-discriminating-function don't break our CLOS.
    (unless (functionp dispatch)
      (error (TEXT "Wrong ~S result for generic-function ~S: not a function: ~S")
             'compute-discriminating-function gf dispatch))
    ; Now install it.
    (set-funcallable-instance-function gf dispatch)))
