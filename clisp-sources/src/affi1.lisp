;;;; Simple Foreign Function Interface support
;;;; Jörg Höhle 7.8.1996

#+UNICODE
(progn
  (in-package "EXT")
  (export '(custom::*foreign-encoding*) "CUSTOM")
  (export '(custom::*foreign-encoding*) "EXT"))

(in-package "SYSTEM")

(defpackage "AFFI"
  (:use "COMMON-LISP" "EXT")
  (:import-from "SYSTEM"
                sys::mem-read sys::mem-write sys::mem-write-vector
                sys::nzero-pointer-p sys::text)
  (:export "DECLARE-LIBRARY-BASE" "REQUIRE-LIBRARY-FUNCTIONS"
           "OPEN-LIBRARY" "CLOSE-LIBRARY" "WITH-OPEN-LIBRARY"
           "DEFFLIBFUN" "DECLARE-LIBRARY-FUNCTION" "FLIBCALL" "MLIBCALL"
           "NZERO-POINTER-P" "MEM-READ" "MEM-WRITE" "MEM-WRITE-VECTOR"))

(in-package "AFFI")

;; The libraries a-list associates the symbol used to denote the library
;; (e.g. SysBase for exec.library) with an opencount and the OS name (as a
;; string). The symbol's value field contains the library address.

(defvar *libraries-alist* '((SysBase 0 "exec.library")))

(defun reset-libraries-at-init ()
  ;; reset all libray base pointers when starting a new Lisp
  (dolist (library *libraries-alist*)
    (makunbound (first library))))
(pushnew 'reset-libraries-at-init sys::*init-hooks*)

(defun declare-library-base (symbol name)
  "Associate the SYMBOL referencing library NAME and import it."
  (unless (and (keywordp symbol) (stringp name))
    (error (TEXT "Basename ~S not a keyword or libraryname ~S not a string")
      symbol name))
  (setq symbol (intern (symbol-name symbol)
                       (load-time-value (find-package "AFFI"))))
  (let ((found (assoc symbol *libraries-alist* :test #'eq)))
    (cond (found
           (unless (string-equal name (third found))
             ;; how possibly continue with cerror?
             (error (TEXT "Library redefinition: old ~S, new ~S")
                    (third found) name)))
          (t (push (list symbol 0 name) *libraries-alist*))))
  (proclaim `(special ,symbol))
  (import symbol)
  symbol)


(defun check-library-base (symbol)
  (or (assoc symbol *libraries-alist* :test #'eq)
      (error (TEXT "Unknown library: ~S") symbol)))

;;TODO? library version
(defun open-library (symbol) ; ABI
  "Returns library address or NIL if it failed. The library must be known."
  ;;CLISP won't close libraries for you...
  (let ((found (check-library-base symbol)))
    (cond
      ((boundp symbol)
       (incf (second found))            ;open count
       (symbol-value symbol))
      (t                                ;first open
       (let ((base
              (sys::%libcall
               (locally (declare (special SysBase))
                 (if (boundp 'SysBase) SysBase (sys::mem-read 4 '*)))
               '#((-552 . #x1A) * string 2)
               (third found) 0)))
         (when (nzero-pointer-p base)
           (prog1 (setf (symbol-value (first found)) base)
             (setf (second found) 1)))))))) ; field might have been invalid

(defun close-library (symbol) ; ABI
  (let ((found (check-library-base symbol)))
    (cond
      ((not (boundp symbol))
       (error (TEXT "Library ~S is not open") symbol))
      ;;TODO? count<0 Test
      ((zerop (decf (second found)))
       (sys::%libcall
        (locally (declare (special SysBase))
          (if (boundp 'SysBase) SysBase (sys::mem-read 4 '*)))
        '#((-414 . #xA) () *)
        (symbol-value symbol))
       (makunbound symbol))))
  t)

(defun check-library-name (name)
  ;; same as above, but allows base symbol or library name
  (cond ((when (stringp name)
           (rassoc name *libraries-alist* :key #'second
                   :test #'string-equal))) ;case-sensitive like Exec
        (t (check-library-base name)))) ;find it or error

(defmacro with-open-library ((name) &body body)
  "If necessary opens library NAME, and executes BODY, finally closing it.
Returns NIL if the library can't be opened.  Unlike OPEN-LIBRARY, NAME may
be a string, which must be the name of a known library."
  (when (stringp name) ; name must be known at compile-time
    (setq name (car (check-library-name name))))
  `(when (open-library ',name)
    (unwind-protect (multiple-value-prog1 (progn ,@body))
      (close-library ',name))))


;; All known functions symbols are stored in a single hash-table. Each
;; function is associated with its library and call information in a cons
;; pair: <function> -> ( <library> . <call information> )

(defvar *library-functions*
  (make-hash-table :key-type 'symbol :value-type 'cons
                   :test 'stablehash-eq :warn-if-needs-rehash-after-gc t))

(defun import-or-loose (symbol)
  (let ((new (find-symbol (symbol-name symbol))))
    (when (and new (not (eq new symbol)))
      (error "Another symbol ~A already exists" symbol))
    (cond (new)
        (t (import symbol) symbol))))

(defun require-library-functions (name &key (import t))
  "Loads foreign function definitions for library NAME if necessary."
  (let* ((entry (check-library-name name))
         (base (first entry))
         (name (third entry)))
    (require name (make-pathname :type "affi" :defaults name)) ;"exec.affi"
    ;; If require worked, name must be correct, now export symbols
    (typecase import
      (LIST                             ; import only requested functions
       (values
        base
        (loop for fname in import
              collect
          (etypecase fname
           (STRING
            (let* ((common (intern (string-upcase fname) (load-time-value (find-package "AFFI")))) ;TODO? match *fd-readtable* case
                   (info (gethash common *library-functions*)))
              (if (and info (eq (car info) base))
                  (import-or-loose common)
                  (error (TEXT "Unknown function of library ~S: ~S")
                         name common))))))))
      ((EQL T)
       ;; as we don't store functions on a per-library basis, walk over all
       (maphash
        #'(lambda (function info)
            (when (and (eq (car info) base)
                       ;; Assume we only need to import AFFI symbols (others are imported from Lisp)
                       ;; possible problem with "LISP" vs. "COMMON-LISP"
                       (eq (symbol-package function) (load-time-value (find-package "AFFI"))))
              (import-or-loose function)))
        *library-functions*)
       base))))


;; The function call information is used by the C part. It's a simple-
;; vector of n+2 elements for an n-ary function. The first element may be
;; nil or a cons of a library offset and a mask indicating a
;; register-based call. The second element indicates the function return
;; type. The other elements indicate the argument types.

;; ATTENTION: AFFI.D depends on this format!

(defun defflibfun (name library offset mask result-type &rest arg-types) ; ABI
  (check-library-base library)
  (unless (typep offset 'fixnum)        ;TODO not only reg calls
    (error (TEXT "Offset must be a fixnum: ~S") offset))
  (let ((old (gethash name *library-functions*))
        (new (cons library
                   (concatenate
                    'simple-vector
                    (list (cons offset mask) result-type)
                    arg-types))))
    (unless (equalp old new)
      (when old
        (fresh-line *error-output*)
        (format *error-output*
                (TEXT ";; redefining foreign library function ~S~%;;  from ~S to ~S")
                name old new)
        (terpri *error-output*))
      ;; TODO check types
      (setf (gethash name *library-functions*) new)))
  name)


(defun calc-register-mask (regs test)
  (labels
      ((calc (regs accu)
         (if (null regs) accu
             (calc (rest regs)
                   (logior
                    (ash accu 4)
                    (1+ (or (position
                             (first regs)
                             '(:D0 :D1 :D2 :D3 :D4 :D5 :D6 :D7
                               :A0 :A1 :A2 :A3 :A4 :A5 :A6)
                             :test test)
                            (error (TEXT "Unknown register: ~S")
                                   (first regs)))))))))
    (calc (reverse regs) 0)))

;; better-looking definitions (closer to FFI)
(defmacro declare-library-function (name library &rest options)
  (let ((base (first (check-library-name library)))
        (arguments (rest (assoc :arguments options))))
    (dolist (arg arguments)
      (unless (and (symbolp (first arg))  ; variable name
                   (null (nthcdr 3 arg))) ; nothing more
        (error (TEXT "Invalid parameter specification ~S in function ~S")
               arg name)))
    `(defflibfun ',name ',base
      ',(second (assoc :offset options))
      ',(calc-register-mask (mapcar #'third arguments) #'eq)
      ',(second (assoc :return-type options))
      ,.(mapcar #'(lambda (arg) (list 'quote (second arg))) arguments))))


(flet ((function-info (name)
         (or (gethash name *library-functions*)
             (error (TEXT "Unknown library function: ~S") name))))

(defun flibcall (name &rest args)
  "Call library function NAME with any number of ARGS."
  (let ((info (function-info name)))
    (apply #'sys::%libcall
           (symbol-value (car info))    ; library
           (cdr info)
           args)))

(defmacro mlibcall (&whole whole-form
                    name &rest args)
  "Call library function NAME with ARGS."
  (let ((info (function-info name)))
    (if (= (length args) (- (length (cdr info)) 2))
        `(sys::%libcall ,(car info) ',(cdr info) . ,args)
        (sys::error-of-type 'ext:source-program-error
          :form whole-form
          :detail args
          (TEXT "Bad number of arguments for ~S: ~S")
          name (length args)))))

) ; flet

#+UNICODE
(progn
  (define-symbol-macro *foreign-encoding* (system::foreign-encoding))
  (defsetf system::foreign-encoding system::set-foreign-encoding)
)

