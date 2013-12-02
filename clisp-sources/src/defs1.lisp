;;;; some definitions for standard functions in LISP
;;;; 1.8.1989, 2.9.1989, 8.10.1989

(in-package "EXT")
(export '(doseq dohash without-package-lock memoized))

(export '(#-(or UNIX WIN32) custom::*default-time-zone*
          custom::*system-package-list*)
        "CUSTOM")
(ext:re-export "CUSTOM" "EXT")

(in-package "SYSTEM")

;;; functions for symbols (Chapter 10)

(defun copy-symbol (symbol &optional flag) ;; Common LISP, p. 169
  (let ((sym (make-symbol (symbol-name symbol))))
    (when flag
      (when (boundp symbol) (sys::set-symbol-value sym (symbol-value symbol)))
      (when (fboundp symbol) (sys::%putd sym (symbol-function symbol)))
      (sys::%putplist sym (copy-list (symbol-plist symbol)))
      #| ;; No, ANSI CL does not say that the status of the symbol in the
         ;; global environment is copied as well.
      (cond ((constantp symbol) (sys::%proclaim-constant sym (symbol-value symbol)))
            ((sys::special-variable-p symbol) (proclaim `(SPECIAL ,sym)))
            ((sys::global-symbol-macro-p symbol) (sys::%proclaim-symbol-macro sym)))
      |#
      )
    sym))

(let ((gentemp-count 0)) ;; Common LISP, p. 170
  (defun gentemp (&optional (prefix "T") (package *package*))
    (loop
      (setq gentemp-count (1+ gentemp-count))
      (multiple-value-bind (sym flag)
        (intern
          (string-concat prefix
            (write-to-string gentemp-count :base 10 :radix nil :readably nil))
          package)
        (unless flag (return sym))))))


;;; macros for packages (Chapter 11), p. 187-188
(defmacro do-symbols ((var &optional (packageform '*package*) (resultform nil))
                      &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (SYSTEM::MAP-SYMBOLS
              #'(LAMBDA (,var)
                  ,@(if declarations `((DECLARE ,@declarations)) '())
                  (TAGBODY ,@body-rest))
               ,packvar)
             ,resultform))))))

(defmacro do-external-symbols ((var &optional (packageform '*package*)
                                    (resultform nil))
                               &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (SYSTEM::MAP-EXTERNAL-SYMBOLS
               #'(LAMBDA (,var)
                   ,@(if declarations `((DECLARE ,@declarations)) '())
                   (TAGBODY ,@body-rest))
               ,packvar)
             ,resultform))))))

(defmacro do-all-symbols ((var &optional (resultform nil)) &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    `(BLOCK NIL
       (LET ((,var NIL))
         (DECLARE (IGNORABLE ,var) ,@declarations)
         (SYSTEM::MAP-ALL-SYMBOLS
           #'(LAMBDA (,var)
               ,@(if declarations `((DECLARE ,@declarations)) '())
               (TAGBODY ,@body-rest)))
         ,resultform))))

;;; <HS>/Body/mac_with-package-iterator.html
(defmacro with-package-iterator (&whole whole-form
                                 (name pack-list &rest types) &body body)
  (unless types
    (error-of-type 'source-program-error
      :form whole-form
      :detail types             ; == NIL
      (TEXT "missing symbol types (~S/~S/~S) in ~S")
      ':internal ':external ':inherited 'with-package-iterator))
  (dolist (symboltype types)
    (case symboltype
      ((:INTERNAL :EXTERNAL :INHERITED))
      (t (error-of-type 'source-program-error
           :form whole-form
           :detail symboltype
           (TEXT "~S: flag must be one of the symbols ~S, ~S, ~S, not ~S")
           'with-package-iterator ':internal ':external ':inherited
           symboltype))))
  (let ((iterfun (gensym "WPI")))
    `(LET ((,iterfun (PACKAGE-ITERATOR-FUNCTION
                       ,pack-list ',(remove-duplicates types))))
       (MACROLET ((,name () '(FUNCALL ,iterfun)))
         ,@body))))
(defun package-iterator-function (pack-list symbol-types) ; ABI
  (let ((iterstates
          (mapcar #'(lambda (pack) (sys::package-iterator pack symbol-types))
                  (if (listp pack-list) pack-list (list pack-list)))))
    ;; The iterstates list is cdr'ed down during the iteration.
    #'(lambda ()
        (loop
          (if iterstates
            (multiple-value-bind (more symb acc)
                (sys::package-iterate (car iterstates))
              (if more
                (return (values more symb acc (svref (car iterstates) 4)))
                (pop iterstates)))
            (return nil))))))

(defvar *system-package-list*
  '("SYSTEM" "COMMON-LISP" "EXT" "I18N" "GRAY" "CHARSET" "CLOS"
    #+sockets "SOCKET" #+generic-streams "GSTREAM" #+syscalls "POSIX"
    #+ffi "FFI" #+(or) "AFFI" #+screen "SCREEN")
  "The list of packages that will be locked by SAVEINITMEM.
Also the default packages to unlock by WITHOUT-PACKAGE-LOCK.")

;; Unlock the specified packages, execute the BODY, then lock them again.
(defmacro with-no-package-lock-internal (packages &body body)
  (let ((locked-packages (gensym "WOPL-")))
    `(LET ((,locked-packages (REMOVE-IF-NOT #'PACKAGE-LOCK ,packages)))
       (UNWIND-PROTECT
         (PROGN
           (SETF (PACKAGE-LOCK ,locked-packages) NIL)
           ,@body)
         (SETF (PACKAGE-LOCK ,locked-packages) T)))))
(defmacro without-package-lock (packages &body body)
  ;; NB: This is augmented by similar compiler processing,
  ;; see c-WITHOUT-PACKAGE-LOCK.
  `(WITH-NO-PACKAGE-LOCK-INTERNAL
     ,(if packages `',packages '*SYSTEM-PACKAGE-LIST*)
     ,@body))

;;; module administration (Chapter 11.8), CLTL p. 188

(defvar *modules* nil)

; Conversion from module name to string.
; cl-user::abc -> "ABC", cs-cl-user::abc -> "abc".
(defun module-name (name)
  (if (and (symbolp name)
           (symbol-package name)
           (package-case-inverted-p (symbol-package name)))
    (cs-cl:string name)
    (string name)))

(defun provide (name)
  (setq *modules* (adjoin (module-name name) *modules* :test #'string=)))

(defun require (module-name &optional (pathname nil p-given))
  (setq module-name (module-name module-name))
  (unless (member module-name *modules* :test #'string=)
    (unless p-given (setq pathname (pathname module-name)))
    (let (#+CLISP (*load-paths* *load-paths*)
          #-CLISP (*default-pathname-defaults* '#""))
      #+CLISP (pushnew (merge-pathnames "dynmod/" *lib-directory*) *load-paths*
                       :test #'equal)
      #+CLISP (when *load-truename*
                (pushnew (make-pathname :name nil :type nil
                                        :defaults *load-truename*)
                         *load-paths* :test #'equal))
      (if (atom pathname) (load pathname) (mapcar #'load pathname)))))


;;; integer constants (Chapter 12)

;; see file INTLOG.D
(defconstant boole-clr 0)
(defconstant boole-set 15)
(defconstant boole-1 10)
(defconstant boole-2 12)
(defconstant boole-c1 5)
(defconstant boole-c2 3)
(defconstant boole-and 8)
(defconstant boole-ior 14)
(defconstant boole-xor 6)
(defconstant boole-eqv 9)
(defconstant boole-nand 7)
(defconstant boole-nor 1)
(defconstant boole-andc1 4)
(defconstant boole-andc2 2)
(defconstant boole-orc1 13)
(defconstant boole-orc2 11)

;; for input of BYTEs:
(defun make-byte (&key size position) (byte size position))

;; X3J13 vote <79>
(defconstant least-positive-normalized-short-float least-positive-short-float)
(defconstant least-negative-normalized-short-float least-negative-short-float)
(defconstant least-positive-normalized-single-float least-positive-single-float)
(defconstant least-negative-normalized-single-float least-negative-single-float)
(defconstant least-positive-normalized-double-float least-positive-double-float)
(defconstant least-negative-normalized-double-float least-negative-double-float)
(proclaim
  '(constant-notinline
    least-positive-normalized-short-float
    least-negative-normalized-short-float
    least-positive-normalized-single-float
    least-negative-normalized-single-float
    least-positive-normalized-double-float
    least-negative-normalized-double-float))

;;; functions for sequences (Chapter 14)

(defmacro doseq ((var seqform &optional resultform) &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (let ((seqvar (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,seqform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (MAP NIL
                  #'(LAMBDA (,var)
                      ,@(if declarations `((DECLARE ,@declarations)) '())
                      (TAGBODY ,@body-rest))
                  ,seqvar)
             ,resultform))))))


;;; functions for lists (Chapter 15)

;; convert the SET represented as a LIST to a SET represented as a HASH-TABLE
(defun list-to-ht (l0 list &key test test-not key)
  (let ((n1 (list-length-proper l0))
        (n2 (list-length-proper list)))
    (unless test-not
      (let ((ht-test (case test
                       (eq 'fasthash-eq)
                       ((eql nil) 'fasthash-eql)
                       (equal 'fasthash-equal)
                       (equalp 'fasthash-equalp))))
        (when ht-test
          ;; --- boxers or briefs? ---
          ;; when is it worthwhile to use HASH-TABLEs as opposed to LISTS?
          ;; in sequence.d:seq_duplicates() we use a HASH-TABLE
          ;; when the list is longer than 10
          ;; here we use the following heuristic,
          ;; supported by a numeric experiment on Linux
          (unless (and (> n1 15) (> n2 5))
            (return-from list-to-ht nil))
          ;; passed the test, generate the HASH-TABLE
          (let ((ht (make-hash-table :test ht-test :size n2)))
            (unless key (setq key #'identity))
            (do ((tail list (cdr tail)))
                ((endp tail) ht)
              (sys::puthash (funcall key (car tail)) ht tail))))))))

;; Auxiliary version of MEMBER, which applies the :KEY argument also to items
;; used only by the recursive version
;(defun sys::member1 (item list &rest rest &key test test-not key)
;  (declare (ignore test test-not))
;  (apply #'member (if key (funcall key item) item) list rest))

(macrolet ((member? (item)
             (let ((i (gensym "ITEM-")) (k (gensym "KEY-")))
               `(let* ((,i ,item) (,k (if key (funcall key ,i) ,i)))
                  (if ht2
                      ;; values are non-NIL tails,
                      ;; so the first GETHASH value is good enough
                      (gethash ,k ht2)
                      (apply #'member ,k list2 rest))))))

(defun union (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'union (cdr list1) list2 rest))
        (t (cons (car list1) (apply #'union (cdr list1) list2 rest))))
  |# ; iterative
  (let* ((list1-filtered '())
         (ht2 #1=(apply #'list-to-ht list1 list2 rest)))
    (dolist (item list1)
      (unless (member? item)
        (setq list1-filtered (cons item list1-filtered))))
    (nreconc list1-filtered list2)))

(defun nunion (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'nunion (cdr list1) list2 rest))
        (t (rplacd list1 (apply #'nunion (cdr list1) list2 rest))))
  |# ; iterative
  (let* ((first nil) (last nil)
         (ht2 #1#))
    (do ((l list1 (cdr l)))
        ((endp l))
      (unless (member? (car l))
        (if last (rplacd last l) (setq first l))
        (setq last l)))
    (if last (progn (rplacd last list2) first) list2)))

(defun intersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (cons (car list1)
               (apply #'intersection (cdr list1) list2 rest)))
        (t (apply #'intersection (cdr list1) list2 rest)))
  |# ; iterative
  (let* ((list1-filtered '())
         (ht2 #1#))
    (dolist (item list1)
      (when (member? item)
        (setq list1-filtered (cons item list1-filtered))))
    (list-nreverse list1-filtered)))

(defun nintersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (rplacd list1 (apply #'nintersection (cdr list1) list2 rest)))
        (t (apply #'nintersection (cdr list1) list2 rest)))
  |# ; iterative
  (let* ((first nil) (last nil)
         (ht2 #1#))
    (do ((l list1 (cdr l)))
        ((endp l))
      (when (member? (car l))
        (if last (rplacd last l) (setq first l))
        (setq last l)))
    (if last (progn (rplacd last nil) first) nil)))

(defun set-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (cons (car list1)
               (apply #'set-difference (cdr list1) list2 rest)))
        (t (apply #'set-difference (cdr list1) list2 rest)))
  |# ; iterative
  (let* ((list1-filtered '())
         (ht2 #1#))
    (dolist (item list1)
      (unless (member? item)
        (setq list1-filtered (cons item list1-filtered))))
    (list-nreverse list1-filtered)))

(defun nset-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (rplacd list1 (apply #'nset-difference (cdr list1) list2 rest)))
        (t (apply #'nset-difference (cdr list1) list2 rest)))
  |# ; iterative
  (let* ((first nil) (last nil)
         (ht2 #1#))
    (do ((l list1 (cdr l)))
        ((endp l))
      (unless (member? (car l))
        (if last (rplacd last l) (setq first l))
        (setq last l)))
    (if last (progn (rplacd last nil) first) nil)))

(macrolet ((rev-arg (f) `(lambda (x y) (funcall ,f y x))))
  ;; this is done so that the elements of LIST1 and LIST2 are passed
  ;; to TEST and TEST-NOT always in the correct order
(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore key))
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1
                 (cond (test (list* :test (rev-arg test) rest))
                       (test-not (list* :test-not (rev-arg test-not) rest))
                       (rest)))))

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore key))
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'nset-difference list2 list1
                (cond (test (list* :test (rev-arg test) rest))
                      (test-not (list* :test-not (rev-arg test-not) rest))
                      (rest)))))

) ; macrolet rev-arg

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not))
  (do* ((l list1 (cdr l))
        (ht2 #1#))
       ((endp l) t)
    (if (not (member? (car l))) (return nil))))

) ; macrolet member?

;; Like SUBST-IF, only that the substitution element is
;; given by a function and must not be a constant
(defun subst-if-then (newfun testfun tree &key (key #'identity))
  (labels ((subst (tree)
             (if (funcall testfun (funcall key tree))
               (funcall newfun tree)
               (if (consp tree)
                 (let* ((car (car tree)) (cdr (cdr tree))
                        (newcar (subst car)) (newcdr (subst cdr)))
                   (if (and (eq car newcar) (eq cdr newcdr))
                     tree
                     (cons newcar newcdr)))
                 tree))))
    (subst tree)))


;;; functions for hash tables (Chapter 16)

(defmacro dohash ((keyvar valuevar HTform &optional resultform) &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (let ((HTvar (gensym)))
      `(BLOCK NIL
         (LET ((,HTvar ,HTform))
           (LET ((,keyvar NIL) (,valuevar NIL))
             (DECLARE (IGNORABLE ,keyvar ,valuevar) ,@declarations)
             (MAPHASH
               #'(LAMBDA (,keyvar ,valuevar)
                   ,@(if declarations `((DECLARE ,@declarations)) '())
                   (TAGBODY ,@body-rest))
               ,HTvar)
             ,resultform))))))

;; Different hash code algorithms for the same test.
(sys::%putd 'ext:fasthash-eq #'eq)
(sys::%putd 'ext:stablehash-eq #'eq)
(sys::%putd 'ext:fasthash-eql #'eql)
(sys::%putd 'ext:stablehash-eql #'eql)
(sys::%putd 'ext:fasthash-equal #'equal)
(sys::%putd 'ext:stablehash-equal #'equal)


;;; functions for strings (Chapter 18)

(defun string-trim (character-bag string)
  (sys::string-both-trim character-bag character-bag string nil))
(defun cs-cl::string-trim (character-bag string)
  (sys::string-both-trim character-bag character-bag string t))

(defun string-left-trim (character-bag string)
  (sys::string-both-trim character-bag nil string nil))
(defun cs-cl::string-left-trim (character-bag string)
  (sys::string-both-trim character-bag nil string t))

(defun string-right-trim (character-bag string)
  (sys::string-both-trim nil character-bag string nil))
(defun cs-cl::string-right-trim (character-bag string)
  (sys::string-both-trim nil character-bag string t))


;;; functions for pathnames (Chapter 23.1.5)
#+LOGICAL-PATHNAMES
(export '(custom::*load-logical-pathname-translations-database*) "CUSTOM")
#+LOGICAL-PATHNAMES
(ext:re-export "CUSTOM" "EXT")
#+LOGICAL-PATHNAMES
(progn
  (defvar *load-logical-pathname-translations-database*
    '(#p"loghosts" #p"loghosts/"))
  (defun logical-pathname-translations (host)
    (setq host (string-upcase host))
    (or (gethash host *logical-pathname-translations*) ; :test #'equal !
        (error (TEXT "~S: ~S does not name a logical host")
               'logical-pathname-translations host)))
  (defun set-logical-pathname-translations (host translations)
    (setq host (string-upcase host))
    (puthash host *logical-pathname-translations* ; :test #'equal !
             (mapcar #'(lambda (rule)
                         (cons (parse-namestring
                                (first rule) nil
                                (make-logical-pathname
                                 :host host :name :wild :type :wild
                                 :version :wild))
                               (rest rule)))
                     translations)))
  ;; load many hosts from a file, AllegroCL-style
  (defun load-lpt-many (file host &aux (*load-level* (1+ *load-level*)))
    (with-open-file (fi file :if-does-not-exist nil)
      (unless fi (return-from load-lpt-many nil))
      (loading-message (TEXT "Loading logical hosts from file ~A ...") file)
      (do* ((eof fi) (host (read fi nil eof) (read fi nil eof)))
           ((eq host eof) (loading-message (TEXT "Loaded file ~A") file))
        (setq host (string-upcase host))
        (set-logical-pathname-translations host (eval (read fi)))
        (loading-message (TEXT "Defined logical host ~A") host)))
    (gethash host *logical-pathname-translations*))
  ;; load a single host from a file, CMUCL-style
  (defun load-lpt-one (file host &aux (*load-level* (1+ *load-level*)))
    (with-open-file (fi file :if-does-not-exist nil)
      (unless fi (return-from load-lpt-one nil))
      (loading-message (TEXT "Loading logical host from file ~A ...") file)
      (set-logical-pathname-translations host (read fi))
      (loading-message (TEXT "Defined logical host ~A") host))
    (gethash host *logical-pathname-translations*))
  (defun load-logical-pathname-translations (host)
    (setq host (string-upcase host))
    (unless (gethash host *logical-pathname-translations*) ; :test #'equal !
      (let ((from (string-concat #2="LOGICAL_HOST_" host "_FROM"))
            (to (string-concat #2# host "_TO"))
            (ho (string-concat #2# host)))
        (cond ((and (fboundp 'getenv) (getenv from) (getenv to))
               (set-logical-pathname-translations
                host (list (list (getenv from) (getenv to))))
               (return-from load-logical-pathname-translations t))
              ((and (fboundp 'getenv) (getenv ho))
               (set-logical-pathname-translations
                host (read-from-string (getenv ho)))
               (return-from load-logical-pathname-translations t))
              ((dolist (file *load-logical-pathname-translations-database*)
                 (if (pathname-name file)
                   (dolist (ff (search-file file '(nil "host")))
                     (when (load-lpt-many ff host) ; successfully defined?
                       (return-from load-logical-pathname-translations t)))
                   (dolist (ff (search-file file nil))
                     (and (string-equal host (pathname-name ff))
                          (or (null (pathname-type ff))
                              (string-equal "host" (pathname-type ff)))
                          (load-lpt-one ff host) ; successfully defined?
                          (return-from load-logical-pathname-translations
                            t))))))))
      (error (TEXT "No translations for logical host ~S found") host)))
  (set-logical-pathname-translations "SYS"
    '((";*.LISP" "*.lisp") (";*" "*") ("*" "/*"))))


;;; functions for time (Chapter 25.4.1)

;; help function for macro TIME ; ABI
(defun %time (new-real1 new-real2 new-run1 new-run2 new-gc1 new-gc2
              new-space1 new-space2 new-gccount
              old-real1 old-real2 old-run1 old-run2 old-gc1 old-gc2
              old-space1 old-space2 old-gccount)
  (macrolet ((diff4 (newval1 newval2 oldval1 oldval2)
               (if (< internal-time-units-per-second 1000000)
                 ;; TIME_1: UNIX_TIMES
                 `(delta4 ,newval1 ,newval2 ,oldval1 ,oldval2 16)
                 ;; TIME_2: other UNIX, WIN32
                 `(+ (* (- ,newval1 ,oldval1) internal-time-units-per-second)
                     (- ,newval2 ,oldval2)))))
    (let ((Real-Time (diff4 new-real1 new-real2 old-real1 old-real2))
          (Run-Time (diff4 new-run1 new-run2 old-run1 old-run2))
          (GC-Time (diff4 new-gc1 new-gc2 old-gc1 old-gc2))
          (Space (delta4 new-space1 new-space2 old-space1 old-space2 24))
          (GC-Count (- new-gccount old-gccount))
          (stream *trace-output*))
      (fresh-line stream)
      (write-string "Real time: " stream)
      (write (float (/ Real-Time internal-time-units-per-second)) :stream stream)
      (write-string #1=" sec." stream)
      (terpri stream)
      (write-string "Run time: " stream)
      (write (float (/ Run-Time internal-time-units-per-second)) :stream stream)
      (write-string #1# stream)
      (terpri stream)
      (write-string "Space: " stream)
      (write Space :stream stream)
      (write-string " Bytes" stream)
      (when (or (plusp GC-Count) (plusp GC-Time))
        (terpri stream)
        (write-string "GC: " stream) (write GC-Count :stream stream)
        (write-string ", GC time: " stream)
        (write (float (/ GC-Time internal-time-units-per-second)) :stream stream)
        (write-string #1# stream))
      (elastic-newline stream))))

;; (sleep N) pause for N seconds. CLTL p. 447
(defun sleep (time)
  (if (and (realp time) (not (minusp time)))
    (progn
      ; Diese Fallunterscheidung hängt von sys::%sleep in time.d ab.
      #+UNIX
      (if (> time 16700000) ; mehr als 193 Tage?
        (loop (sys::%sleep 86400 0)) ; ja -> Endlosschleife
        (multiple-value-bind (seconds rest) (floor time)
          (sys::%sleep seconds (round (* rest 1000000)))))
      #+WIN32
      (if (> time 4250000) ; mehr als 49 Tage?
        (loop (sys::%sleep 86400 0)) ; ja -> Endlosschleife
        (multiple-value-bind (seconds rest) (floor time)
          (sys::%sleep seconds (round (* rest 1000))))))
    (error-of-type 'type-error
      :datum time :expected-type '(REAL 0 *)
      (TEXT "~S: argument ~S should be a nonnegative number")
      'sleep time)))


;; functions for Zeit-Umrechnung und Zeitzonen (CLTL Chapter 25.4.1)
;; Version 2, beinhaltet mehr Mathematik und basiert auf März-Jahren

; Ein März-Jahr sei die Periode vom 1.3. bis 28/29.2.
; Vorteil: Umrechnung Monat/Tag <--> Jahrtag wird einfacher.
; Skizze:
;   1.1.1900            1.1.1901            1.1.1902
;                                         
;   |-------------------|-------------------|-------------------|
;   |     Jahr 1900     |     Jahr 1901     |     Jahr 1902     |
;   |--|----------------|--|----------------|--|----------------|--|
;      |  März-Jahr 1900   |  März-Jahr 1901   |  März-Jahr 1902   |
;      |-------------------|-------------------|-------------------|
;                                            
;      1.3.1900            1.3.1901            1.3.1902

; (UTag Jahr) = Nummer des Tages 1.3.Jahr (gegenüber 1.1.1900)
; UTag(J) = 365*J + floor(J/4) - floor(J/100) + floor(J/400) - 693901
; damit  UTag(J) - UTag(J-1) = 365 + [1 falls J Schaltjahr]
; und    UTag(1899) = -306
; gelten.
(defun UTag (Jahr)
  (+ (* 365 Jahr) (floor Jahr 4) (- (floor Jahr 100)) (floor Jahr 400) -693901))

; Näherungwert:
; 365+1/4-1/100+1/400 = 365.2425 = 146097/400 .
; Durch Betrachtung einer Wertetabelle der 400-periodischen Funktion
; (J -> UTag(J)-146097/400*J) sieht man:
;   146097/400*J - 693902.4775 <= UTag(J) <= 146097/400*J - 693900.28

; Bestimmt zu einem Tag (0 = 1.1.1900) das März-Jahr und den Tag im März-Jahr.
; (Jahr&Tag UTTag) ==> Jahr, Jahrtag
; mit (= UTTag (+ (UTag Jahr) Jahrtag))
(defun Jahr&Tag (UTTag)
  ; Gesucht ist das größte Jahr mit UTag(Jahr) <= UTTag.
  ; Für dieses Jahr J gilt
  ; 146097/400*J - 693902.4775 <= UTag(J) <= UTTag < UTag(J+1) <= 146097/400*J - 693535.0375,
  ; also 146097*J - 277560991 <= 400*UTTag < 146097*J - 277414015,
  ; also 146097*(J-1900) + 23309 <= 400*UTTag < 146097*(J-1900) + 170285,
  ; also J + 0.159544... <= 1900 + UTTag/(146097/400) < J + 1.165561... .
  (let* ((Jahr (+ 1900 (floor (- UTTag 58) 146097/400)))
         (Jahresanfang (UTag Jahr)))
    ; Wegen 146097*(J-1900) + 109 <= 400*(UTTag-58) < 146097*(J-1900) + 147084,
    ; also J <= 1900 + (UTTag-58)/(146097/400) < J+1.006755...,
    ; ist die Schätzung  Jahr := floor(1900 + (UTTag-58)/(146097/400))
    ; meist richtig und jedenfalls nicht zu klein und um höchstens 1 zu groß.
    (when (< UTTag Jahresanfang) ; zu groß?
      (decf Jahr)
      (setq Jahresanfang (UTag Jahr)))
    (values Jahr (- UTTag Jahresanfang))))

; Bei vielen Betriebssystemen (nicht bei UNIX, WIN32) muss die Zeitzone beim
; Installieren in timezone.lisp eingetragen werden. Hier stehen nur
; Defaultwerte.

#-(or UNIX WIN32)
; lokale Zeitzone
(defvar *default-time-zone* -1) ; Default: 1 h östlich GMT = MEZ
; NB: Zeitzone muss nicht ganzzahlig sein, sollte aber Vielfaches
; einer Sekunde sein.

#-(or UNIX WIN32)
; Funktion, die feststellt, ob bei gegebenem März-Jahr und Tag und Stunde
; Sommerzeit gilt.
(defvar *default-dst-check* ; Default: Sommerzeit nicht explizit bekannt
  #'(lambda (Jahr Jahrtag Stunde) (declare (ignore Jahr Jahrtag Stunde)) nil))

; andere Abbildung  Jahrtag -> Monat  für decode-universal-time:
; Seien Monat und Jahrtag auf den 1. März bezogen
; (d.h. Jahrtag = 0 am 1. März, = 364 am 28. Februar, usw.,
;  und März=0,...,Dezember=9,Januar=10,Februar=11).
; Dann ist
;                Monat = floor(a*Jahrtag+b)
; sofern a und b so gewählt sind, dass die Ungleichungen
;   122*a+b >= 4, 275*a+b >= 9, 30*a+b < 1, 336*a+b < 11
; gelten. Dies ist ein Viereck im Bereich
; 0.032653... = 8/245 <= a <= 7/214 = 0.032710...,
; 0.009345... = 1/107 <= b <= 1/49 = 0.020408...,
; in dem z.B. der Punkt (a=5/153,b=2/153) liegt:
;                Monat = floor((5*Jahrtag+2)/153).

; andere Abbildung  Monat -> Jahrtag
; für encode-universal-time und decode-universal-time:
; Seien Monat und Jahrtag auf den 1. März bezogen
; (d.h. Jahrtag = 0 am 1. März, = 364 am 28. Februar, usw.,
;  und März=0,...,Dezember=9,Januar=10,Februar=11).
; Die Abbildung
;      Monat   0  1  2  3  4   5   6   7   8   9   10  11
;      Jahrtag 0 31 61 92 122 153 184 214 245 275 306 337
; kann man schreiben
;                Jahrtag = floor(a*Monat+b)
; sofern a und b so gewählt sind, dass die Ungleichungen
;   a+b >= 31, 11*a+b >= 337, 4*a+b < 123, 9*a+b < 276
; gelten. Dies ist ein Viereck im Bereich
; 30.5714... = 214/7 <= a <= 245/8 = 30.625,
; 0.375      = 3/8   <= b <= 5/7   = 0.7142...,
; in dem z.B. der Punkt (a=153/5,b=2/5) liegt:
;                Jahrtag = floor((153*Monat+2)/5).
; Dies ist allerdings langsamer als ein Tabellenzugriff.

(macrolet ((Monat->Jahrtag (Monat) ; 0 <= Monat < 12, 0=März,...,11=Februar
             `(svref '#(0 31 61 92 122 153 184 214 245 275 306 337) ,Monat)))

; (encode-universal-time second minute hour date month year [time-zone]),
; CLTL p. 446
(defun encode-universal-time
              (Sekunde Minute Stunde Tag Monat Jahr &optional (Zeitzone nil)
               &aux Monat3 Jahr3 Jahrtag UTTag UT)
  (unless (and (and (integerp Jahr)
                    (progn
                      (when (<= 0 Jahr 99)
                        (multiple-value-bind (i1 i2 i3 i4 i5 Jahrjetzt)
                            (get-decoded-time)
                          (declare (ignore i1 i2 i3 i4 i5))
                          (setq Jahr
                                (+ Jahr (* 100 (ceiling (- Jahrjetzt Jahr 50)
                                                        100))))))
                      ;; 1900-01-01 GMT == 1899-12-31 EST
                      (<= 1899 Jahr)))
               (and (integerp Monat) (<= 1 Monat 12))
               (progn
                 (if (< Monat 3)
                   (setq Jahr3 (1- Jahr)  Monat3 (+ Monat 9))  ; Monat3 10..11
                   (setq Jahr3 Jahr       Monat3 (- Monat 3))) ; Monat3 0..9
                 (and (and (integerp Tag) (<= 1 Tag))
                      (progn
                        (setq Jahrtag (+ (1- Tag) (Monat->Jahrtag Monat3)))
                        (setq UTTag (+ Jahrtag (UTag Jahr3)))
                        (and (if (not (eql Monat3 11))
                               (< Jahrtag (Monat->Jahrtag (1+ Monat3)))
                               (< UTTag (UTag (1+ Jahr3))))
                             (and (integerp Stunde) (<= 0 Stunde 23))
                             (and (integerp Minute) (<= 0 Minute 59))
                             (and (integerp Sekunde) (<= 0 Sekunde 59))
                             (and (progn
                                    (unless Zeitzone
                                      (setq Zeitzone
                                        #-(or UNIX WIN32)
                                        (- *default-time-zone*
                                           (if (funcall *default-dst-check*
                                                        Jahr3 Jahrtag Stunde)
                                               1 0))
                                        #+(or UNIX WIN32)
                                        (default-time-zone
                                            (+ (* 24 UTTag) Stunde) nil)))
                                    (when (floatp Zeitzone)
                                      (setq Zeitzone (rational Zeitzone)))
                                    (or (integerp Zeitzone)
                                        (and (rationalp Zeitzone)
                                             (integerp (* 3600 Zeitzone)))))
                                  (<= -24 Zeitzone 24))
                             (<= 0 (setq UT (+ Sekunde
                                               (* 60 (+ Minute
                                                        (* 60 (+ Stunde Zeitzone
                                                                 (* 24 UTTag)))))))))))))
    (error-of-type 'error
      (TEXT "incorrect date: ~S-~S-~S ~S:~S:~S, time zone ~S")
      Jahr Monat Tag Stunde Minute Sekunde Zeitzone))
  UT)

; (decode-universal-time universal-time [time-zone]), CLTL p. 445
(defun decode-universal-time (UT &optional (time-zone nil)
                              &aux Sommerzeit Zeitzone)
  (if time-zone
    (setq Sommerzeit nil Zeitzone time-zone)
    #-(or UNIX WIN32)
    (setq time-zone *default-time-zone*
          Sommerzeit (let ((UT (- UT (round (* 3600 time-zone)))))
                       (multiple-value-bind (UTTag Stunde) (floor UT (* 3600 24))
                         (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTag)
                           (funcall *default-dst-check* Jahr Jahrtag Stunde))))
          Zeitzone (if Sommerzeit (1- time-zone) time-zone))
    #+(or UNIX WIN32)
    (progn
      (multiple-value-setq (Zeitzone Sommerzeit)
        (default-time-zone (floor UT 3600) t))
      (setq time-zone (if Sommerzeit (1+ Zeitzone) Zeitzone))))
  ; time-zone = Zeitzone ohne Sommerzeitberücksichtigung,
  ; Zeitzone = Zeitzone mit Sommerzeitberücksichtigung.
  (let ((UTSekunden (- UT (round (* 3600 Zeitzone)))))
    (multiple-value-bind (UTMinuten Sekunde) (floor UTSekunden 60)
      (multiple-value-bind (UTStunden Minute) (floor UTMinuten 60)
        (multiple-value-bind (UTTage Stunde) (floor UTStunden 24)
          (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTage)
            (let* ((Monat (floor (+ (* 5 Jahrtag) 2) 153))
                   (Tag (1+ (- Jahrtag (Monat->Jahrtag Monat)))))
              (if (< Monat 10) ; Monat März..Dezember?
                (setq Monat (+ Monat 3)) ; Monat 3..12
                (setq Monat (- Monat 9) Jahr (+ Jahr 1))) ; Monat 1..2
              (values Sekunde Minute Stunde Tag Monat Jahr (mod UTTage 7)
                      Sommerzeit time-zone))))))))
) ; end of macrolet

; (get-decoded-time), CLTL p. 445
(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))


;;; Verschiedenes

; (concat-pnames obj1 obj2) liefert zu zwei Objekten (Symbolen oder Strings)
;  ein Symbol, dessen Printname sich aus den beiden Objekten zusammensetzt.
(defun concat-pnames (obj1 obj2)
  (let ((str (string-concat (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
      (intern (subseq str 1) *keyword-package*)
      (intern str))))

; Gibt object in einen String aus, der nach Möglichkeit höchstens max Spalten
; lang sein soll.
(defun write-to-short-string (object max)
  ; Methode: probiere
  ; level = 0: length = 0,1,2
  ; level = 1: length = 1,2,3,4
  ; level = 2: length = 2,...,6
  ; usw. bis maximal level = 16.
  ; Dabei level möglichst groß, und bei festem level length möglichst groß.
  (if (or (numberp object) (symbolp object)) ; von length und level unbeeinflusst?
    (write-to-string object)
    (macrolet ((minlength (level) `,level)
               (maxlength (level) `(* 2 (+ ,level 1))))
      ; Um level möglist groß zu bekommen, dabei length = minlength wählen.
      (let* ((level ; Binärsuche nach dem richtigen level
               (let ((level1 0) (level2 16))
                 (loop
                   (when (= (- level2 level1) 1) (return))
                   (let ((levelm (floor (+ level1 level2) 2)))
                     (if (<= (string-width (write-to-string object :level levelm :length (minlength levelm))) max)
                       (setq level1 levelm) ; levelm passt, probiere größere
                       (setq level2 levelm) ; levelm passt nicht, probiere kleinere
                 ) ) )
                 level1
             ) )
             (length ; Binärsuche nach dem richtigen length
               (let ((length1 (minlength level)) (length2 (maxlength level)))
                 (loop
                   (when (= (- length2 length1) 1) (return))
                   (let ((lengthm (floor (+ length1 length2) 2)))
                     (if (<= (string-width (write-to-string object :level level :length lengthm)) max)
                       (setq length1 lengthm) ; lengthm passt, probiere größere
                       (setq length2 lengthm) ; lengthm passt nicht, probiere kleinere
                 ) ) )
                 length1
            )) )
        (write-to-string object :level level :length length)
) ) ) )

(defmacro memoized (form)
  "(MEMOIZED form) memoizes the result of FORM from its first evaluation."
  `(LET ((MEMORY
           (IF (EVAL-WHEN (EVAL) T)
             ',(cons nil nil)
             ;; Careful: Different expansions of MEMOIZED forms must yield
             ;; LOAD-TIME-VALUE forms that are not EQ, otherwise compile-file
             ;; will coalesce these LOAD-TIME-VALUE forms. Therefore here we
             ;; explicitly cons up the list and don't use backquote.
             ,(list 'LOAD-TIME-VALUE '(CONS NIL NIL)))))
     (UNLESS (CAR MEMORY)
       (SETF (CDR MEMORY) ,form)
       (SETF (CAR MEMORY) T))
     (CDR MEMORY)))

;; *ERROR-HANDLER* should be NIL or a function which accepts the following
;; arguments:
;;  - NIL (in case of ERROR) or a continue-format-string (in case of CERROR),
;;  - error-format-string,
;;  - more argument list for these two format strings,
;;  and which may return only if the first argument is /= NIL.
(defvar *error-handler* nil)
