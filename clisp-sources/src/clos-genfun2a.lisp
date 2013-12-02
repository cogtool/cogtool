;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part 2: Generic function dispatch and execution
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ======================== Avoiding endless recursion ========================

;; Generic functions which are used in implementing the generic function
;; dispatch and execution.
(defvar |#'compute-discriminating-function| nil)
(defvar |#'compute-applicable-methods| nil)
(defvar |#'compute-applicable-methods-using-classes| nil)
(defvar |#'compute-effective-method| nil)
(defvar |#'generic-function-methods| nil)
(defvar |#'generic-function-method-class| nil)
(defvar |#'generic-function-signature| nil)
(defvar |#'generic-function-undeterminedp| nil)
(defvar |#'generic-function-method-combination| nil)
(defvar |#'generic-function-argorder| nil)
(defvar |#'generic-function-declarations| nil)
(defvar |#'method-qualifiers| nil)
(defvar |#'method-specializers| nil)

(defun safe-gf-methods (gf)
  (if (or (eq gf #'generic-function-methods) ; for bootstrapping
          (eq gf |#'generic-function-methods|)
          (eq gf |#'compute-effective-method|)
          (eq gf |#'compute-discriminating-function|)
          (eq gf |#'compute-applicable-methods-using-classes|))
    (std-gf-methods gf)
    (generic-function-methods gf)))

(defun safe-gf-default-method-class (gf)
  (if (or (eq gf |#'generic-function-method-class|)
          (eq gf |#'compute-effective-method|))
    (std-gf-default-method-class gf)
    (generic-function-method-class gf)))

(defun safe-gf-signature (gf)
  (if (or (eq gf #'generic-function-signature) ; for bootstrapping
          (eq gf |#'generic-function-signature|)
          (eq gf |#'generic-function-method-class|)
          (eq gf |#'compute-effective-method|))
    (std-gf-signature gf)
    (generic-function-signature gf)))

(defun safe-gf-undeterminedp (gf)
  (if (or (eq gf #'generic-function-undeterminedp) ; for bootstrapping
          (eq gf |#'generic-function-undeterminedp|))
    (std-gf-undeterminedp gf)
    (generic-function-undeterminedp gf)))

(defun safe-gf-method-combination (gf)
  (if (or (eq gf #'generic-function-method-combination) ; for bootstrapping
          (eq gf |#'generic-function-method-combination|)
          (eq gf |#'compute-effective-method|))
    (std-gf-method-combination gf)
    (generic-function-method-combination gf)))

(defun safe-gf-argorder (gf)
  (if (or (eq gf |#'generic-function-argorder|)
          (eq gf |#'method-qualifiers|)
          (eq gf |#'compute-effective-method|))
    (std-gf-argorder gf)
    (generic-function-argorder gf)))

(defun safe-gf-declspecs (gf)
  (if (or (eq gf |#'generic-function-declarations|)
          (eq gf |#'generic-function-argorder|)
          (eq gf |#'method-qualifiers|)
          (eq gf |#'compute-effective-method|))
    (std-gf-declspecs gf)
    (generic-function-declarations gf)))

(defun safe-method-qualifiers (method gf)
  (if (or (eq gf #'method-qualifiers) ; for bootstrapping
          (eq gf |#'method-qualifiers|)
          (eq gf |#'compute-effective-method|))
    (std-method-qualifiers method)
    (method-qualifiers method)))

(defun safe-method-specializers (method gf)
  (if (or (eq gf #'method-specializers) ; for bootstrapping
          (eq gf |#'method-specializers|)
          (eq gf |#'compute-effective-method|))
    (std-method-specializers method)
    (method-specializers method)))

;; ============================= Method Selection =============================

;; CLtL2 28.1.6.2., ANSI CL 7.6.2. Applicable methods
(defun method-applicable-p (method required-arguments gf)
  (every #'typep required-arguments (safe-method-specializers method gf)))

;; CLtL2 28.1.7.1., ANSI CL 7.6.6.1.2.
;; Sorting the applicable methods by precedence order
;; > methods: A list of methods from the same generic function that are
;;            already known to be applicable for the given required-arguments.
;; > required-argument-classes: the list of classes the required arguments are
;;                              direct instances of.
;; > argument-order: A list of indices in the range 0..req-num-1 that
;;                   determines the argument order.
(defun sort-applicable-methods (methods required-argument-classes argument-order gf)
  (sort (copy-list methods)
        #'(lambda (method1 method2) ; method1 < method2 ?
            (let ((specializers1 (safe-method-specializers method1 gf))
                  (specializers2 (safe-method-specializers method2 gf)))
              (dolist (arg-index argument-order nil)
                (let ((arg-class (nth arg-index required-argument-classes))
                      (psp1 (nth arg-index specializers1))
                      (psp2 (nth arg-index specializers2)))
                  (if (eql-specializer-p psp1)
                    (if (eql-specializer-p psp2)
                      nil         ; (EQL x) = (EQL x)
                      (return t)) ; (EQL x) < <class>  ==>  method1 < method2
                    (if (eql-specializer-p psp2)
                      (return nil) ; <class> > (EQL x)   ==>  method1 > method2
                      ;; two classes: compare the position in the CPL of arg-class:
                      (let* ((cpl (class-precedence-list arg-class))
                             (pos1 (position psp1 cpl))
                             (pos2 (position psp2 cpl)))
                        (cond ((< pos1 pos2) (return t)) ; method1 < method2
                              ((> pos1 pos2) (return nil)) ; method1 > method2
                              ))))))))))

;; Preliminary.
(defun compute-applicable-methods (gf args)
  (compute-applicable-methods-<generic-function> gf args))

(defun compute-applicable-methods-<generic-function> (gf args)
  (if (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    '()
    (let ((req-num (sig-req-num (safe-gf-signature gf))))
      (if (>= (length args) req-num)
        ;; 0. Check the method specializers:
        (let ((methods (safe-gf-methods gf)))
          (dolist (method methods)
            (check-method-only-standard-specializers gf method
              'compute-applicable-methods))
          ;; 1. Select the applicable methods:
          (let ((req-args (subseq args 0 req-num)))
            (setq methods
                  (remove-if-not #'(lambda (method)
                                     (method-applicable-p method req-args gf))
                                 (the list methods)))
            ;; 2. Sort the applicable methods by precedence order:
            (sort-applicable-methods methods (mapcar #'class-of req-args) (safe-gf-argorder gf) gf)))
        (error (TEXT "~S: ~S has ~S required argument~:P, but only ~S arguments were passed to ~S: ~S")
               'compute-applicable-methods gf req-num (length args)
               'compute-applicable-methods args)))))

;; ----------------------------------------------------------------------------

;; compute-applicable-methods-using-classes is just plain redundant, and must
;; be a historical relic of the time before CLOS had EQL specializers (or a
;; brain fart of the PCL authors). But the MOP wants it, so we implement it.

;; Preliminary.
(defun compute-applicable-methods-using-classes (gf req-arg-classes)
  (compute-applicable-methods-using-classes-<generic-function> gf req-arg-classes))

(defun compute-applicable-methods-using-classes-<generic-function> (gf req-arg-classes)
  (unless (and (proper-list-p req-arg-classes) (every #'defined-class-p req-arg-classes))
    (error (TEXT "~S: argument should be a proper list of classes, not ~S")
           'compute-applicable-methods-using-classes req-arg-classes))
  (if (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    '()
    (let ((req-num (sig-req-num (safe-gf-signature gf))))
      (if (= (length req-arg-classes) req-num)
        ;; 0. Check the method specializers:
        (let ((methods (safe-gf-methods gf)))
          (dolist (method methods)
            (check-method-only-standard-specializers gf method
              'compute-applicable-methods-using-classes))
          ;; 1. Select the applicable methods. Note that the arguments are
          ;; assumed to be _direct_ instances of the given classes, i.e.
          ;; classes = (mapcar #'class-of required-arguments).
          (setq methods
                (remove-if-not #'(lambda (method)
                                   (let ((specializers (safe-method-specializers method gf))
                                         (applicable t) (unknown nil))
                                     (mapc #'(lambda (arg-class specializer)
                                               (if (defined-class-p specializer)
                                                 ;; For class specializers,
                                                 ;; (typep arg specializer) is equivalent to
                                                 ;; (subtypep (class-of arg) specializer).
                                                 (unless (subclassp arg-class specializer)
                                                   (setq applicable nil))
                                                 ;; For EQL specializers,
                                                 ;; (typep arg specializer) is certainly false
                                                 ;; if (class-of arg) and (class-of (eql-specializer-object specializer))
                                                 ;; differ. Otherwise unknown.
                                                 (if (eq arg-class (class-of (eql-specializer-object specializer)))
                                                   (setq unknown t)
                                                   (setq applicable nil))))
                                           req-arg-classes specializers)
                                     (when (and applicable unknown)
                                       (return-from compute-applicable-methods-using-classes-<generic-function>
                                         (values nil nil)))
                                     applicable))
                               (the list methods)))
          ;; 2. Sort the applicable methods by precedence order:
          (values (sort-applicable-methods methods req-arg-classes (safe-gf-argorder gf) gf) t))
        (error (TEXT "~S: ~S has ~S required argument~:P, but ~S classes were passed to ~S: ~S")
               'compute-applicable-methods-using-classes gf req-num (length req-arg-classes)
               'compute-applicable-methods-using-classes req-arg-classes)))))

;; ----------------------------------------------------------------------------

;; compute-applicable-methods-for-set
;; is a generalization of compute-applicable-methods[-using-classes].
;; For each argument position you can specify a set of possible required
;; arguments through one of:
;;   (TYPEP class)          [covers direct and indirect instances of class],
;;   (INSTANCE-OF-P class)  [covers only direct instances of class],
;;   (EQL object)           [covers the given object only].
;; Returns 1. the list of applicable methods and 2. a certainty value.
(defun compute-applicable-methods-for-set (gf req-arg-specs)
  (unless (and (proper-list-p req-arg-specs)
               (every #'(lambda (x) (and (consp x) (memq (car x) '(TYPEP INSTANCE-OF-P EQL))))
                      req-arg-specs))
    (error (TEXT "~S: argument should be a proper list of specifiers, not ~S")
           'compute-applicable-methods-for-set req-arg-specs))
  (if (safe-gf-undeterminedp gf)
    ;; gf has uninitialized lambda-list, hence no methods.
    '()
    (let ((req-num (sig-req-num (safe-gf-signature gf))))
      (if (= (length req-arg-specs) req-num)
        ;; 0. Check the method specializers:
        (let ((methods (safe-gf-methods gf)))
          (dolist (method methods)
            (check-method-only-standard-specializers gf method
              'compute-applicable-methods-for-set))
          ;; 1. Select the applicable methods:
          (setq methods
                (remove-if-not #'(lambda (method)
                                   (let ((specializers (safe-method-specializers method gf))
                                         (applicable t) (unknown nil))
                                     (mapc #'(lambda (arg-spec specializer)
                                               (ecase (first arg-spec)
                                                 (TYPEP
                                                   (if (defined-class-p specializer)
                                                     ;; For class specializers,
                                                     ;; (typep arg specializer) is certainly true
                                                     ;; if the known class of arg is a subclass of
                                                     ;; the specializer. Otherwise unknown.
                                                     (unless (subclassp (second arg-spec) specializer)
                                                       (setq unknown t))
                                                     ;; For EQL specializers,
                                                     ;; (typep arg specializer) is certainly false
                                                     ;; if (eql-specializer-object specializer)
                                                     ;; doesn't belong to the known class of arg.
                                                     ;; Otherwise unknown.
                                                     (if (typep (eql-specializer-object specializer) (second arg-spec))
                                                       (setq unknown t)
                                                       (setq applicable nil))))
                                                 (INSTANCE-OF-P ; see ...-using-classes
                                                   (if (defined-class-p specializer)
                                                     ;; For class specializers,
                                                     ;; (typep arg specializer) is equivalent to
                                                     ;; (subtypep (class-of arg) specializer).
                                                     (unless (subclassp (second arg-spec) specializer)
                                                       (setq applicable nil))
                                                     ;; For EQL specializers,
                                                     ;; (typep arg specializer) is certainly false
                                                     ;; if (class-of arg) and (class-of (eql-specializer-object specializer))
                                                     ;; differ. Otherwise unknown.
                                                     (if (eq (second arg-spec) (class-of (eql-specializer-object specializer)))
                                                       (setq unknown t)
                                                       (setq applicable nil))))
                                                 (EQL ; see method-applicable-p
                                                   (unless (typep (second arg-spec) specializer)
                                                     (setq applicable nil)))))
                                           req-arg-specs specializers)
                                     (when (and applicable unknown)
                                       (return-from compute-applicable-methods-for-set
                                         (values nil nil)))
                                     applicable))
                               (the list methods)))
          ;; 2. Sort the applicable methods by precedence order:
          (let ((argument-order (safe-gf-argorder gf)))
            (values
              (sort (copy-list methods)
                    #'(lambda (method1 method2) ; method1 < method2 ?
                        (let ((specializers1 (safe-method-specializers method1 gf))
                              (specializers2 (safe-method-specializers method2 gf)))
                          (dolist (arg-index argument-order nil)
                            (let ((arg-spec (nth arg-index req-arg-specs))
                                  (psp1 (nth arg-index specializers1))
                                  (psp2 (nth arg-index specializers2)))
                              (if (eql-specializer-p psp1)
                                (if (eql-specializer-p psp2)
                                  nil         ; (EQL x) = (EQL x)
                                  (return t)) ; (EQL x) < <class>  ==>  method1 < method2
                                (if (eql-specializer-p psp2)
                                  (return nil) ; <class> > (EQL x)   ==>  method1 > method2
                                  ;; two classes: compare the position in the CPL of
                                  ;; the arg's class, if known:
                                  (unless (eq psp1 psp2)
                                    (cond ((eq psp2 <t>) (return t)) ; method1 < method2
                                          ((eq psp1 <t>) (return nil)) ; method1 > method2
                                          (t (let* ((arg-class
                                                      (ecase (first arg-spec)
                                                        (TYPEP
                                                          ; The precise arg-class is unknown.
                                                          (return-from compute-applicable-methods-for-set
                                                            (values nil nil)))
                                                        (INSTANCE-OF-P (second arg-spec))
                                                        (EQL (class-of (second arg-spec)))))
                                                    (cpl (class-precedence-list arg-class))
                                                    (pos1 (position psp1 cpl))
                                                    (pos2 (position psp2 cpl)))
                                               (cond ((< pos1 pos2) (return t)) ; method1 < method2
                                                     ((> pos1 pos2) (return nil)) ; method1 > method2
                                                     ))))))))))))
              t)))
        (error (TEXT "~S: ~S has ~S required argument~:P")
               'compute-applicable-methods-for-set gf req-num)))))

;; ----------------------------------------------------------------------------

;; There's no real reason for checking the method specializers in
;; compute-applicable-methods, rather than in
;; compute-effective-method-as-function, but that's how the MOP specifies it.
(defun check-method-only-standard-specializers (gf method caller)
  (dolist (spec (safe-method-specializers method gf))
    (unless (or (defined-class-p spec) (typep-class spec <eql-specializer>))
      (error (TEXT "~S: Invalid method specializer ~S on ~S in ~S")
        caller spec method gf))))

;; ================ Combination of Applicable Methods, Part 1 ================

(defun gf-keyword-arguments (restp signature methods)
  ;; CLtL2 28.1.6.4., 28.1.6.5., ANSI CL 7.6.4., 7.6.5.
  ;; Keyword Arguments in Generic Functions
  (when restp
    ;; The generic function has &REST or &KEY, thus try all methods.
    ;; "If the lambda-list of ... the generic function definition
    ;;  contains &allow-other-keys, all keyword arguments are accepted."
    (unless (sig-allow-p signature)
      ;; "The specific set of keyword arguments accepted ...
      ;;  varies according to the applicable methods."
      (let ((signatures (mapcar #'method-signature methods)))
        ;; "A method that has &rest but not &key does not affect the
        ;;   set of acceptable keyword arguments."
        (setq signatures (delete-if-not #'sig-keys-p signatures))
        ;; No &key in the generic function, and no method with &key ==>
        ;; no restriction on the arguments.
        (when (or (sig-keys-p signature) signatures)
          ;; "If the lambda-list of any applicable method ... contains
          ;;  &allow-other-keys, all keyword arguments are accepted."
          (unless (some #'sig-allow-p signatures)
            ;; "The set of keyword arguments accepted for a
            ;;  particular call is the union of the keyword
            ;;  arguments accepted by all applicable methods and
            ;;  the keyword arguments mentioned after &key in the
            ;;  generic function definition."
            (let* ((keywords
                    (remove-duplicates
                     (append (sig-keywords signature)
                             (mapcap #'sig-keywords signatures))
                     :from-end t))
                   (opt-vars (gensym-list (sig-opt-num signature)))
                   (key-vars (gensym-list keywords))
                   (lambdalist-keypart
                     `(&KEY    ; lambdalist-keypart
                       ,@(mapcar #'(lambda (kw var) `((,kw ,var)))
                                 keywords key-vars))))
              (values opt-vars key-vars lambdalist-keypart))))))))

;; ================ Combination of Applicable Methods, Part 2 ================

;; is in the file clos-methcomb2.lisp
