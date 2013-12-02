;;; Inspect
;;;
;;; Copyright (C) 2000-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; This code is adapted from CLOCC/CLLIB/inspect.lisp.
;;; Please see http://clocc.sourceforge.net/ and
;;; ftp://clocc.sourceforge.net/pub/clocc/snapshots/cllib.html
;;; for details.
;;;
;;; Please do not fork a separate CLISP version of this file!
;;; All changes should go into CLOCC/CLLIB/inspect.lisp first,
;;; and only then merged into this file.
;;; If you encounter a bug in this file, please set sys::*inspect-debug*
;;; to 5 and report all output with a detailed description of what you did
;;; to <clisp-list> (see http://clisp.cons.org about CLISP mailing lists),
;;; as you do with any other CLISP bugs.
;;; If you would like to fix a bug in this file, please get CLOCC from
;;; the link above, build PORT and CLLIB as described there, and work
;;; on CLOCC/CLLIB/inspect.lisp.
;;; ___no patches to this file will be accepted by the CLISP maintainers___
;;;             - Sam Steingold

;;;
;;; utilities from cllib
;;;

(in-package "CUSTOM")
(common-lisp:export
 '(*inspect-frontend* *inspect-browser* *inspect-print-lines*
   *with-html-output-doctype* *user-mail-address*
   *inspect-print-level* *inspect-print-length* *inspect-length*)
 "CUSTOM")
(ext:re-export "CUSTOM" "EXT")

(common-lisp:in-package "EXT")
(export
 '(current-time with-http-output with-html-output)
 "EXT")

(in-package "SYSTEM")

(defun current-time (&optional (out t))
  "Print the current time to the stream (defaults to T)."
  (multiple-value-bind (se mi ho da mo ye) (get-decoded-time)
    (format out "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            ye mo da ho mi se)))

(defmacro string-beg-with (beg strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV starts with BEG."
  (if (stringp beg)
      (let ((len (length beg)))
        `(and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len)))
      (with-gensyms ("SBW-" len)
        `(let ((,len (length ,beg)))
          (and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len))))))

;; The characters which must be replaced before putting a string into HTML
(defvar *html-chars* '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;")))

(clos:defclass html-stream-out (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)))
(clos:defmethod stream-write-char ((stream html-stream-out) ch)
  (clos:with-slots (target-stream) stream
    (let ((char-cons (assoc ch *html-chars* :test #'char=)))
      (if char-cons (write-string (cdr char-cons) target-stream)
          (write-char ch target-stream)))))
(clos:defmethod stream-line-column ((stream html-stream-out)) nil)
(clos:defmethod stream-finish-output ((stream html-stream-out))
  (clos:with-slots (target-stream) stream (finish-output target-stream)))
(clos:defmethod stream-force-output ((stream html-stream-out))
  (clos:with-slots (target-stream) stream (force-output target-stream)))
(clos:defmethod stream-clear-output ((stream html-stream-out))
  (clos:with-slots (target-stream) stream (clear-output target-stream)))
(clos:defmethod close ((stream html-stream-out) &rest opts)
  (clos:with-slots (target-stream) stream (apply #'close target-stream opts))
  (call-next-method))


(defvar *with-html-output-doctype*
  '("html" "PUBLIC" "\"-//W3C//DTD XHTML 1.0 Strict//EN\""
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""))
(defvar *user-mail-address*
  (concatenate 'string (or (getenv "USER") (getenv "USERNAME") "nobody") "@"
               (let ((st (machine-instance)))
                 (subseq st 0 (position #\Space st)))))

(defmacro with-html-output ((var stream
                             &key (doctype '*with-html-output-doctype*)
                                  (meta '(:http-equiv "Content-Type"
                                          :content "text/html"))
                                  base comment (title "untitled") (footer t)
                                  head)
                            &body body)
  (with-gensyms ("HTML-" raw mailto)
    `(let* ((,raw ,stream)
            (,var (clos::make-instance 'html-stream-out :stream ,raw))
            (,mailto (concatenate 'string "mailto:" *user-mail-address*)))
      (macrolet ((with-tag ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>~%" ,tag)))
                 (with-tagl ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>" ,tag))))
        (unwind-protect
             (progn
               (format ,raw "<!DOCTYPE~{ ~a~}>~%" ,doctype)
               ;; print the comment
               (format ,raw "<!--~% Created on ") (current-time ,raw)
               (format ,raw "~% by ~a~% using `with-open-html'
 Lisp: ~a ~a~@[~%~a~]~% -->~2%"
                       *user-mail-address*
                       (lisp-implementation-type) (lisp-implementation-version)
                       ,comment)
               (when ,base
                 (with-tag (:base :href ,base)))
               (with-tag (:html)
                 (with-tag (:head ,@head)
                   (with-tag (:meta ,@meta))
                   (with-tag (:link :rev "made" :href ,mailto))
                   (with-tag (:title) (princ ,title ,var)))
                 (with-tag (:body)
                   ,@body
                   (when ,footer
                     (with-tag (:p)
                       (with-tag (:hr))
                       (with-tag (:address)
                         (with-tag (:a :href ,mailto)
                           (princ *user-mail-address* ,var)))
                       (with-tagl (:strong) (current-time ,var)))))))
          (when ,var (close ,var))
          (when ,raw (close ,raw)))))))

(defvar *http-encoding*
  (make-encoding #+UNICODE :charset #+UNICODE charset:utf-8
                 :line-terminator :dos))

(defmacro with-http-output ((var raw &rest opts &key keep-alive (debug 0)
                             (return-code 200) (return-name "OK")
                             &allow-other-keys)
                            &body body)
  "Write some HTML to an http client on socket stream RAW.
Supplies some HTTP/1.0 headers and calls `with-html-output'."
  (with-gensyms ("HTTP-" string vector stream sock header line dbg alive)
    `(let* ((,sock ,raw)
            (,dbg ,debug) (,alive ,keep-alive)
            (,string (with-output-to-string (,stream)
                       (with-html-output (,var ,stream ,@(remove-plist opts :keep-alive :debug :return-code :return-name))
                         ,@body)))
            (,vector (ext:convert-string-to-bytes ,string *http-encoding*))
            (,header (list (format nil "HTTP/1.0 ~d ~a"
                                   ,return-code ,return-name)
                           #+UNICODE "Content-type: text/html; charset=utf-8"
                           #-UNICODE "Content-type: text/html"
                           (format nil "Content-length: ~d" (length ,vector))
                           (format nil "Connection: ~:[close~;keep-alive~]"
                                   ,alive))))
       (dolist (,line ,header)
         (write-line ,line ,sock)
         (when (and ,dbg (> ,dbg 0))
           (format t "<- ~a~%" ,line)))
       (terpri ,sock)
       (setf (stream-element-type ,sock) 'unsigned-byte)
       (write-byte-sequence ,vector ,sock)
       (setf (stream-element-type ,sock) 'character)
       (when (and ,dbg (> ,dbg 3))
         (format t "<- ~s~%" ,string))
       (unless ,alive
         (when (and ,dbg (> ,dbg 0))
           (format t "~s: closing ~s~%" 'with-http-output ,sock))
         (close ,sock)))))

(defun flush-http (sock)
  "Read everything from the HTTP socket SOCK, until a blank line."
  (loop :for line = (read-line sock nil nil)
        :while (and line (plusp (length line)))
        :collect line))

(defun http-error (sock url &key (name "Not Found") (code 404)
                   (keep-alive nil) (debug 0))
  "Report a request error."
  (with-http-output (out sock :keep-alive keep-alive :debug debug
                          :return-code code :return-name name)
    (with-tag (:h1) (princ name out))
    (with-tag (:p)
      (format out "The requested URL ~s was not found on this server." url))))

;;;
;;; options
;;;

(defvar *inspect-frontend* :tty) ; the default frontend
(defvar *inspect-browser* nil)  ; the default browser
(defvar *inspect-print-lines* 5) ; default for `*print-lines*'
(defvar *inspect-print-level* 5) ; default for `*print-level*'
(defvar *inspect-print-length* 10) ; default for `*print-length*'
(defvar *inspect-length* 5)     ; the number of sequence elements to print

(defvar *inspect-all* nil) ; all `inspection' objects in this session
(defparameter *inspect-debug* 0) ; debug level
(defvar *inspect-unbound-value*) ; the value for the unbound slots

;;;
;;; backend
;;;

(defstruct (inspection (:conc-name insp-))
  self                          ; the object being inspected
  (id (fill-pointer *inspect-all*) :type fixnum) ; unique in a session
  (title "" :type string)       ; the short description of the object
  (blurb nil :type list)        ; list of strings with general information
  (up nil :type (or null inspection)) ; parent
  (num-slots 0 :type fixnum)    ; the number of slots
  (pos nil :type (or null fixnum)) ; pos in parent
  (nth-slot nil :type (or null (function (integer) (values t t)))) ; val & name
  (set-slot nil :type (or null (function (integer t) t)))) ; set Nth slot

(defun insp-check (insp)
  ;; this should always be okay, nevertheless
  ;; we use `warn' instead of `assert' or `error' because
  ;; the objects being inspected could possibly be modified
  ;; in another thread
  (let ((up (insp-up insp)) (pos (insp-pos insp)) (id (insp-id insp)))
    (unless (eq insp (aref *inspect-all* id))
      (warn "~s: ~s appears corrupted (~d->~d):~%~s~%~s~%"
            'get-insp '*inspect-all* id (insp-id (aref *inspect-all* id))
            insp (aref *inspect-all* id)))
    (when up
      (unless (< -1 pos (insp-num-slots up))
        (warn "~s: pos out of range: ~d ~d~%" 'insp-check pos
              (insp-num-slots up)))
      (unless (eq (funcall (insp-nth-slot up) pos) (insp-self insp))
        (warn "~s: slot ~d of the ~s has changed:~%~s~%"
              'insp-check pos (insp-self up) up)))))

(defun insp-last-slot (insp)
  (1- (insp-num-slots insp)))
(defun insp-num-slots-print (insp)
  (min (insp-last-slot insp) *inspect-length*))
(defun insp-left-p (insp) ; check for the presence of a left neighbor
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< 0 pos))))
(defun insp-right-p (insp) ; check for the presence of a right neighbor
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< pos (insp-last-slot up)))))

(defun set-slot-error (ii obj)
  (error "~s: Cannot set the slot number ~s for object ~s"
         'set-slot-error ii obj))

(defmacro with-nth-hash-slot (ht args1 args2 retform)
  (with-gensyms ("WNGS-" ii jj)
    `(lambda (,ii ,@args1)
      (block with-nth-hash-slot
       (let ((,jj -1))
         (maphash (lambda ,args2
                    (declare (ignorable ,@args2))
                    (when (= ,ii (incf ,jj))
                      (return-from with-nth-hash-slot ,retform)))
                  ,ht))))))

(clos:defgeneric inspect-backend (object &rest opts)
  (:method ((obj array) &rest opts)
    (let* ((siz (array-total-size obj)) (type (array-element-type obj))
           (arr (make-array siz :displaced-to obj :element-type type)))
      (apply #'make-inspection :self obj :title
             (typecase obj (string "String") (vector "Vector") (t "Array"))
             :blurb (list (format nil "dimension~p:~{ ~:d~}"
                                  (array-rank obj) (array-dimensions obj))
                          (format nil "element-type: ~s" type)
                          (if (= 1 (array-rank obj))
                              (if (array-has-fill-pointer-p obj)
                                  (format nil "fill-pointer: ~:d"
                                              (fill-pointer obj))
                                  "no fill pointer")
                              (format nil "total size: ~:d" siz))
                          (multiple-value-bind (di off)
                              (array-displacement obj)
                            (if di (format nil "displaced to (~:d): ~s" off di)
                                "not displaced")))
             :num-slots siz :nth-slot (lambda (ii) (aref arr ii))
             :set-slot (lambda (ii val) (setf (aref arr ii) val))
             opts)))
  (:method ((obj hash-table) &rest opts)
    (let ((count (hash-table-count obj)))
      (apply #'make-inspection :self obj :title "Hash Table"
             :blurb (list (format nil "count: ~:d" count)
                          (format nil "size: ~:d" (hash-table-size obj))
                          (format nil "rehash-size: ~:d"
                                  (hash-table-rehash-size obj))
                          (format nil "rehash-threshold: ~:d"
                                  (hash-table-rehash-threshold obj))
                          (format nil "test: ~:d" (hash-table-test obj)))
             :num-slots count :set-slot
             (with-nth-hash-slot obj (val) (kk vv) (setf (gethash kk obj) val))
             :nth-slot
             (with-nth-hash-slot obj nil (kk vv) (values vv kk)) opts)))
  (:method ((obj cons) &rest opts)
    (multiple-value-bind (len dotted-p) (list-length-dotted obj)
      (apply
       #'make-inspection
       :num-slots (if len (if dotted-p (1+ len) len)
                      (1+ (position obj obj :test #'eq)))
       :nth-slot (lambda (ii) (if (and dotted-p (= ii len)) dotted-p
                                  (nth ii obj)))
       :set-slot (lambda (ii val)
                   (if (and dotted-p (= ii len))
                       (setf (cdr (nthcdr (1- ii) obj)) val)
                       (setf (nth ii obj) val)))
       :blurb (list (if len
                        (if dotted-p
                            (if (> len 1)
                                (format nil "a dotted list of length ~:d" len)
                                (format nil "a cons"))
                            (format nil "a list of length ~:d" len))
                        (format nil "a cyclic list")))
       :self obj :title (format nil "Cons") opts)))
  (:method ((obj symbol) &rest opts)
    (apply #'make-inspection :num-slots 2
           :nth-slot (lambda (ii)
                       (case ii
                         (0 (values (if (boundp obj)
                                        (symbol-value obj)
                                        *inspect-unbound-value*)
                                    :symbol-value))
                         (1 (values (symbol-plist obj) :symbol-plist))))
           :set-slot (lambda (ii val)
                       (case ii
                         (0 (setf (symbol-value obj) val))
                         (1 (setf (symbol-plist obj) val))))
           :blurb (list (format nil "package: ~s" (symbol-package obj)))
           :self obj :title "Symbol" opts))
  (:method ((obj structure-object) &rest opts)
    (let ((slots (clos::slot-names obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (if (clos:slot-boundp obj slot)
                                       (clos:slot-value obj slot)
                                       *inspect-unbound-value*)
                                   slot)))
             :set-slot (lambda (ii val)
                         (setf (clos:slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj clos::standard-object) &rest opts)
    (let ((slots (clos::slot-names obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (if (clos:slot-boundp obj slot)
                                       (clos:slot-value obj slot)
                                       *inspect-unbound-value*)
                                   slot)))
             :set-slot (lambda (ii val)
                         (setf (clos:slot-value obj (nth ii slots)) val))
             :self obj :title "standard object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj ratio) &rest opts)
    (apply #'make-inspection :self obj :title "rational number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (numerator obj) 'numerator)
                 (values (denominator obj) 'denominator)))
           :set-slot #'set-slot-error opts))
  (:method ((obj complex) &rest opts)
    (apply #'make-inspection :self obj :title "complex number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (realpart obj) 'realpart)
                 (values (imagpart obj) 'imagpart)))
           :set-slot #'set-slot-error opts))
  (:method ((obj t) &rest opts)
    (apply #'make-inspection :self obj :title "atom"
           :blurb (list (format nil "type: ~s" (type-of obj))
                        (format nil "class: ~s" (clos::class-of obj))) opts))
  (:method :around ((obj t) &key id &allow-other-keys)
    (or (and (not id) (find obj *inspect-all* :key #'insp-self))
        (let ((insp (clos::call-next-method)))
          (when (> *inspect-debug* 0)
            (format t "~s [id: ~:d, forced: ~s]: ~s~%" 'inspect-backend
                    (insp-id insp) id (insp-self insp)))
          (if id (setf (aref *inspect-all* id) insp)
              (vector-push-extend insp *inspect-all*))
          insp))))

(defun get-insp (id-or-insp com)
  "Get the INSPECTION object from the ID (or inspection object) and COMmand."
  (let ((insp (etypecase id-or-insp
                (inspection id-or-insp)
                (fixnum (aref *inspect-all* id-or-insp)))))
    (when insp
      (insp-check insp)
      (case com
        (:q :q)
        (:s                     ; re-inspect Self
         (inspect-backend (insp-self insp) :up (insp-up insp)
                          :pos (insp-pos insp) :id (insp-id insp)))
        (:u (insp-up insp))
        (:l (when (insp-pos insp)
              (get-insp (insp-up insp) (1- (insp-pos insp)))))
        (:r (when (insp-pos insp)
              (get-insp (insp-up insp) (1+ (insp-pos insp)))))
        (t (when (and (integerp com) (< -1 com (insp-num-slots insp)))
             (inspect-backend (funcall (insp-nth-slot insp) com)
                              :up insp :pos com)))))))

;;;
;;; frontends - common
;;;

(clos:defgeneric print-inspection (insp out frontend &rest opts)
  (:method ((insp inspection) (out stream) (frontend t) &rest opts)
    (error "~s: unknown inspect front end: ~s [~s ~s]"
           'print-inspection frontend out opts)))

(clos:defgeneric inspect-frontend (insp frontend)
  (:method ((insp inspection) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'inspect-frontend frontend)))

(clos:defgeneric inspect-finalize (frontend)
  (:method ((frontend t))))

(defun inspect-read-clean-eval (insp stream)
  ;; `read' a form, destructively replace `:self' with INSP and `:slot'
  ;; with the appropriate `funcall', then `eval'
  ;; this is useful for frontends which provide an eval/modify facility
  (labels ((clean (form)
             (cond ((eq (car form) :self)
                    (setf (car form) (list 'quote (insp-self insp)))
                    (clean-up (cdr form)))
                   ((eq (car form) :slot)
                    (setf (car form) 'funcall
                          (cdr form) (cons (insp-nth-slot insp) (cdr form)))
                    (clean-up (cddr form)))
                   (t (clean-up (car form))
                      (clean-up (cdr form)))))
           (clean-up (form) (when (consp form) (clean form)) form))
    (eval (clean-up (read stream nil nil)))))

;;;
;;; TTY frontend
;;;

(clos:defmethod print-inspection ((insp inspection) (out stream)
                                  (backend (eql :tty)) &rest opts)
  (declare (ignore opts))
  (format out "~&~s:  ~a~%~{ ~a~%~}" (insp-self insp) (insp-title insp)
          (insp-blurb insp))
  (when (insp-nth-slot insp)
    (loop :for ii :from 0 :to (insp-num-slots-print insp)
          :do (multiple-value-bind (val name) (funcall (insp-nth-slot insp) ii)
                (format out "~d~@[ [~a]~]:  ~s~%" ii name val)))))

(clos:defmethod inspect-frontend ((insp inspection) (frontend (eql :tty)))
  (print-inspection insp *terminal-io* frontend)
  (do (com (id (insp-id insp)))
      ((eq com :q))
    (fresh-line)
    (princ "INSPECT-- type :h for help; :q to return to the REPL ---> ")
    (force-output)
    (case (setq com (read *terminal-io* nil :q))
      (:q)
      ((:h :?) (format t " *** commands:~% :h, :?~15t this help
 :p, :a~15t Print the current item Again
 :s~15t re-inspect this item (Self)
 :d~15t Describe the current item~%")
       (when (insp-up insp)
         (format t " :u~15t return UP to the parent~%"))
       (when (insp-left-p insp)
         (format t " :l~15t inspect the left neighbor~%"))
       (when (insp-right-p insp)
        (format t " :r~15t inspect the right neighbor~%"))
       (when (insp-nth-slot insp)
         (format t " number~15t inspect this slot~%"))
       (format t " :e lisp-form~15t eval this form, with these substitutions:
 ~20t (:slot number) is replaced with the appropriate slot value
 ~20t :self is replaced with this object
 :m num lisp~15t Modify this slot
 :q~15t return to the main Read/Eval/Print loop~% ---> ")
       (force-output))
      (:d (describe (insp-self insp)))
      ((:p :a) (print-inspection insp *terminal-io* frontend))
      (:e (handler-case (let ((v (inspect-read-clean-eval insp *terminal-io*)))
                          (format t "~&~S~%" v))
            (error (err) (format t " *** error: ~A" err))))
      (:m (handler-case
              (let ((v (funcall (insp-set-slot insp)
                                (inspect-read-clean-eval insp *terminal-io*)
                                (inspect-read-clean-eval insp *terminal-io*))))
                (format t "~&~S~%" v))
            (error (err) (format t " *** error: ~A" err))))
      (t (cond ((setq insp (get-insp id com))
                (print-inspection insp *terminal-io* frontend)
                (setq id (insp-id insp)))
               (t (format t "command `~s' is not valid here~%" com)
                  (setq insp (get-insp id :s))))))))

;;;
;;; HTTP frontend
;;;

(clos:defmethod print-inspection ((insp inspection) (raw stream)
                                  (backend (eql :http)) &key keep-alive)
  (flet ((href (com) (format nil "/~d/~s" (insp-id insp) com)))
    (with-http-output (out raw :keep-alive keep-alive :debug *inspect-debug*
                           :title (insp-title insp) :footer nil)
      (with-tag (:h1) (princ (insp-title insp) out))
      (with-tag (:ul)
        (dolist (item (insp-blurb insp))
          (with-tag (:li) (princ item out))))
      (with-tag (:font :size "+4")
        (with-tag (:pre) (write (insp-self insp) :stream out)))
      (when (insp-nth-slot insp)
        (with-tag (:ol)
          (loop :for ii :from 0 :to (insp-num-slots-print insp)
                :do (multiple-value-bind (val name)
                        (funcall (insp-nth-slot insp) ii)
                      (with-tag (:li)
                        (with-tag (:a :href (href ii))
                          (princ (or name "inspect") out))
                        (with-tag (:pre) (write val :stream out)))))))
      (with-tag (:hr))
      (with-tag (:h2) (princ "describe:" out))
      (with-tag (:pre) (describe (insp-self insp) out))
      (with-tag (:hr))  ; footer
      (with-tag (:table :width "100%")
        (with-tag (:tr)
          (with-tag (:td :align "left")
            (with-tag (:a :href (href :q)) (princ "quit" out)))
          (when (insp-left-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :l)) (princ "left" out))))
          (when (insp-right-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :r)) (princ "right" out))))
          (when (insp-up insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :u)) (princ "parent" out))))
          (with-tag (:td :align "right")
            (with-tag (:a :href (href :s)) (princ "self" out))))))))

(defun http-command (server &key (debug *inspect-debug*) socket)
  "Accept a connection from the server, return the GET command and the socket."
  (when (> debug 1)
    (format t "~s: server: ~s; socket: ~s~%" 'http-command server socket))
  (let (response id com keep-alive)
    (loop (unless (and socket (open-stream-p socket))
            (setq socket (socket-accept server
                                        :external-format *http-encoding*))
            (when (> debug 1)
              (format t "~s: new socket: ~s~%" 'http-command socket)))
          (setq response (flush-http socket))
          (when response (return))
          ;; connection timed out?
          (close socket))
    (when (> debug 1)
      (dolist (line response)
        (format t "-> ~a~%" line)))
    (dolist (line response)
      (when (string-beg-with "Connection: " line)
        (setq keep-alive (string-equal line "keep-alive" :start1 12))
        (when (> debug 0)
          (format t "~s: connection: ~s (keep-alive: ~s)~%"
                  'http-command (subseq line 12) keep-alive))
        (when keep-alive
          ;; we override `keep-alive' because it makes it impossible to
          ;; switch browsers in the middle of an inspection session
          (setq keep-alive nil)
          (when (> debug 0)
            (format t "~s: overriding keep-alive to NIL~%" 'http-command))))
      (when (string-beg-with "GET /" line)
        (let ((pos (position #\/ line :test #'char= :start 5)))
          (setq id (parse-integer line :start 5 :end pos :junk-allowed t))
          (cond (id
                 (setq com
                       (if (char= #\% (aref line (1+ pos))) ; %xx hex
                           (progn ; "%3a" --> ":"
                             (setf (aref line (+ 3 pos))
                                   (code-char (parse-integer
                                               line :radix 16 :start (+ 2 pos)
                                               :end (+ 4 pos))))
                             (read-from-string line nil nil :start (+ 3 pos)))
                           (read-from-string line nil nil :start (1+ pos))))
                 (when (> debug 0)
                   (format t "~s: command: id=~d com=~s~%"
                           'http-command id com)))
                (t
                 (http-error socket line :debug debug :keep-alive keep-alive)
                 (when (> debug 0)
                   (format t "~s: invalid request: ~s~%"
                           'http-command line)))))))
    (values socket id com keep-alive)))

(clos:defmethod inspect-frontend ((insp inspection) (frontend (eql :http)))
  (let ((server
         (let* ((server (socket-server)) (port (socket-server-port server))
                (host (machine-instance)))
           (when (> *inspect-debug* 0)
             (format t "~&~s [~s]: server: ~s~%"
                     'inspect-frontend frontend server))
           (browse-url (format nil "http://~a:~d/0/:s"
                               (if *inspect-browser* "127.0.0.1"
                                   (subseq host 0 (position #\Space host)))
                               port)
                       :browser *inspect-browser*)
           server))
        sock id com keep-alive)
    (unwind-protect
         (loop (when (eq com :q) (return))
           (setf (values sock id com keep-alive)
                 (http-command server :socket sock))
           (when id
             (if (eq com :q)
               (with-http-output (out sock :keep-alive keep-alive
                                      :debug *inspect-debug*
                                      :title "inspect" :footer nil)
                 (with-tag (:h1) (princ "thanks for using inspect" out))
                 (with-tag (:p) (princ "you may close this window now" out)))
               (if (setq insp (get-insp id com))
                 (print-inspection insp sock frontend :keep-alive keep-alive)
                 (with-http-output (out sock :keep-alive keep-alive
                                        :debug *inspect-debug*
                                        :title "inspect" :footer nil)
                   (with-tag (:h1)
                     (format out "error: wrong command: ~:d/~s" id com))
                   (with-tag (:p)
                     (princ "either this is an old inspect session, or a " out)
                     (with-tag (:a :href "https://sourceforge.net/bugs/?func=addbug&group_id=1802") (princ "bug" out))))))
             (when (> *inspect-debug* 0)
               (format t "~s [~s]: cmd:~d/~s id:~d~%" 'inspect-frontend
                       frontend id com (insp-id insp)))))
      (socket-server-close server)
      (when (open-stream-p sock)
        (do () ((null (read-char-no-hang sock))))
        (close sock)))))
;;;
;;; the juice
;;;

;;;###autoload
(defun inspect (object &key ((:frontend *inspect-frontend*) *inspect-frontend*)
                ((:browser *inspect-browser*) *inspect-browser*))
  (let* ((*print-array* nil) (*print-pretty* t)
         (*print-circle* t) (*print-escape* t)
         (*print-lines* *inspect-print-lines*)
         (*print-level* *inspect-print-level*)
         (*print-length* *inspect-print-length*)
         (*inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
         (tmp-pack (make-package (gensym "INSPECT-TMP-PACKAGE-")))
         (*package* tmp-pack)
         (*inspect-unbound-value* (intern "#<unbound>" tmp-pack)))
    (unwind-protect
         (inspect-frontend (inspect-backend object) *inspect-frontend*)
      (inspect-finalize *inspect-frontend*)
      (delete-package tmp-pack))
    (values)))

;;; inspect.lisp ends here
