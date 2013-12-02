;;; Gray streams, following David N. Gray's STREAM-DEFINITION-BY-USER proposal
;;; ftp://parcftp.xerox.com/pub/cl/cleanup/mail/stream-definition-by-user.mail

(in-package "GRAY")
(common-lisp:export
 '(;; Classes:
   fundamental-stream
   fundamental-input-stream
   fundamental-output-stream
   fundamental-character-stream
   fundamental-binary-stream
   fundamental-character-input-stream
   fundamental-character-output-stream
   fundamental-binary-input-stream
   fundamental-binary-output-stream
   ;; general generic functions:
   stream-position
   ;; Generic functions for character input:
   stream-read-char
   stream-unread-char
   stream-read-char-no-hang
   stream-peek-char
   stream-listen
   stream-read-char-will-hang-p
   stream-read-char-sequence
   stream-read-line
   stream-clear-input
   ;; Generic functions for character output:
   stream-write-char
   stream-line-column
   stream-start-line-p
   stream-write-char-sequence
   stream-write-string
   stream-terpri
   stream-fresh-line
   stream-finish-output
   stream-force-output
   stream-clear-output
   stream-advance-to-column
   ;; Generic functions for binary input:
   stream-read-byte
   stream-read-byte-lookahead
   stream-read-byte-will-hang-p
   stream-read-byte-no-hang
   stream-read-byte-sequence
   ;; Generic functions for binary output:
   stream-write-byte
   stream-write-byte-sequence))
(common-lisp:in-package "SYSTEM")
(import '(close open-stream-p stream-element-type) "GRAY")
(use-package '("GRAY") "EXT")
(ext:re-export "GRAY" "EXT")

;; Classes

(eval-when (compile load eval)
  (defmethod clos:validate-superclass ((class class) (superclass (eql clos::<stream>)))
    (or (call-next-method)
        (eq (clos:class-name class) 'fundamental-stream)))
  (let ((clos::*allow-mixing-metaclasses* t))
    (clos:defclass fundamental-stream (stream clos:standard-object)
      (($open :type boolean :initform t) ; whether the stream is open
       ($reval :type boolean :initform nil) ; whether read-eval is allowed
       ($penl :type boolean :initform nil) ; whether an elastic newline is pending
) ) ) )

(clos:defclass fundamental-input-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-output-stream (fundamental-stream)
  ()
)

; Stuff these classes into the runtime system.
(%defgray
  (vector
    (clos:find-class 'fundamental-stream)        ; for STREAMP to work
    (clos:find-class 'fundamental-input-stream)  ; for INPUT-STREAM-P to work
    (clos:find-class 'fundamental-output-stream) ; for OUTPUT-STREAM-P to work
) )

(clos:defclass fundamental-character-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-binary-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-character-input-stream (fundamental-input-stream fundamental-character-stream)
  (($lastchar :initform nil) ; last character read (and not yet unread)
) )

(clos:defclass fundamental-character-output-stream (fundamental-output-stream fundamental-character-stream)
  ()
)

(clos:defclass fundamental-binary-input-stream (fundamental-input-stream fundamental-binary-stream)
  ()
)

(clos:defclass fundamental-binary-output-stream (fundamental-output-stream fundamental-binary-stream)
  ()
)

;; General generic functions

(clos:defgeneric close (stream &key abort)
  (:method ((stream stream) &rest args)
    (apply #'sys::built-in-stream-close stream args))
  (:method ((stream fundamental-stream) &rest more-args)
    (declare (ignore more-args))
    (clos:with-slots ($open $penl) stream
      (when $penl (setq $penl nil) (write-char #\Newline stream))
      (prog1 $open (setq $open nil))
  ) )
)

(clos:defgeneric open-stream-p (stream)
  (:method ((stream stream))
    (sys::built-in-stream-open-p stream)
  )
  (:method ((stream fundamental-stream))
    (clos:with-slots ($open) stream
      $open
  ) )
)

(clos:defgeneric stream-element-type (stream)
  (:method ((stream stream))
    (sys::built-in-stream-element-type stream)
  )
  (:method ((stream fundamental-stream))
    (clos:no-applicable-method #'stream-element-type stream)
  )
  (:method ((stream fundamental-character-stream))
    'CHARACTER
  )
)
(clos:defgeneric (setf stream-element-type) (new-element-type stream)
  (:method (new-element-type (stream stream))
    (sys::built-in-stream-set-element-type stream new-element-type)
  )
  (:method (new-element-type (stream fundamental-stream))
    (clos:no-applicable-method #'(setf stream-element-type) new-element-type stream)
  )
)

(clos:defgeneric stream-position (stream position)
  (:method ((stream stream) position)
    (if position
        (cl:file-position stream position)
        (cl:file-position stream)))
  (:method ((stream fundamental-stream) position)
    (clos:no-applicable-method #'stream-position stream position)))

;; Generic functions for character input

; We define the methods on fundamental-input-stream, not
; fundamental-character-input-stream, so that people can use
; (setf stream-element-type).

(clos:defgeneric stream-read-char (stream))

(clos:defgeneric stream-unread-char (stream char))

(clos:defgeneric stream-read-char-no-hang (stream)
  (:method ((stream fundamental-input-stream))
    (stream-read-char stream)
  )
)

(clos:defgeneric stream-peek-char (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char stream)))
      (unless (eq c ':EOF) (stream-unread-char stream c))
      c
  ) )
)

(clos:defgeneric stream-listen (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char-no-hang stream)))
      (if (or (eq c 'NIL) (eq c ':EOF))
        nil
        (progn (stream-unread-char stream c) t)
  ) ) )
)

(clos:defgeneric stream-read-char-will-hang-p (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char-no-hang stream)))
      (cond ((eq c 'NIL) t)
            ((eq c ':EOF) nil)
            (t (stream-unread-char stream c) nil)
  ) ) )
)

(clos:defgeneric stream-read-char-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-input-stream) (sequence string) &optional (start 0) (end nil))
    ; sequence is a simple-string, and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) index)
      (let ((c (stream-read-char stream)))
        (when (eq c ':EOF) (return index))
        (setf (char sequence index) c)
  ) ) )
)

(clos:defgeneric stream-read-line (stream)
  (:method ((stream fundamental-input-stream))
    (let ((buffer (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop
        (let ((c (stream-read-char stream)))
          (cond ((eq c ':EOF) (return (values (coerce buffer 'simple-string) t)))
                ((eql c #\Newline) (return (values (coerce buffer 'simple-string) nil)))
                (t (vector-push-extend c buffer))
  ) ) ) ) )
)

(clos:defgeneric stream-clear-input (stream)
  (:method ((stream fundamental-input-stream))
    nil
  )
)

;; Generic functions for character output

; We define the methods on fundamental-output-stream, not
; fundamental-character-output-stream, so that people can use
; (setf stream-element-type).

(clos:defgeneric stream-write-char (stream character))

(clos:defgeneric stream-line-column (stream))

(clos:defgeneric stream-start-line-p (stream)
  (:method ((stream fundamental-output-stream))
    (eql (stream-line-column stream) 0)
  )
)

(clos:defgeneric stream-write-char-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-output-stream) (sequence string) &optional (start 0) (end nil))
    ; sequence is a simple-string, and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) nil)
      (stream-write-char stream (char sequence index))
  ) )
)

(clos:defgeneric stream-write-string (stream string &optional start end)
  (:method ((stream fundamental-output-stream) string &optional (start 0) (end nil))
    (stream-write-char-sequence stream string start end)
    string
  )
)

(clos:defgeneric stream-terpri (stream)
  (:method ((stream fundamental-output-stream))
    (stream-write-char stream #\Newline)
    nil
  )
)

(clos:defgeneric stream-fresh-line (stream)
  (:method ((stream fundamental-output-stream))
    (if (stream-start-line-p stream)
      nil
      (progn (stream-terpri stream) t)
  ) )
)

(clos:defgeneric stream-finish-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-force-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-clear-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-advance-to-column (stream column)
  (:method ((stream fundamental-output-stream) (column real))
    (let ((currcol (stream-line-column stream)))
      (if currcol
        (dotimes (i (- column currcol) t) (stream-write-char stream #\Space))
        nil
  ) ) )
)

;; Generic functions for binary input

(clos:defgeneric stream-read-byte (stream))

(clos:defgeneric stream-read-byte-lookahead (stream))

(clos:defgeneric stream-read-byte-will-hang-p (stream)
  (:method ((stream fundamental-input-stream))
    (eq (stream-read-byte-lookahead stream) 'NIL)
  )
)

(clos:defgeneric stream-read-byte-no-hang (stream)
  (:method ((stream fundamental-input-stream))
    (if (stream-read-byte-lookahead stream)
      (stream-read-byte stream)
      nil
  ) )
)

(clos:defgeneric stream-read-byte-sequence (stream sequence
                                            &optional start end no-hang interactive)
  (:method ((stream fundamental-input-stream) (sequence vector)
            &optional (start 0) (end nil) (no-hang nil) (interactive nil))
    ;; sequence is a (simple-array (unsigned-byte 8) (*)),
    ;; and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) index)
      (let ((x (if (or no-hang (and interactive (> index start)))
                 (stream-read-byte-no-hang stream)
                 (stream-read-byte stream))))
        (when (or (null x) (eq x ':EOF)) (return index))
        (setf (aref sequence index) x)))))

;; Generic functions for binary output

(clos:defgeneric stream-write-byte (stream integer))

(clos:defgeneric stream-write-byte-sequence (stream sequence
                                             &optional start end no-hang interactive)
  (:method ((stream fundamental-output-stream) (sequence vector)
            &optional (start 0) (end nil) (no-hang nil) (interactive nil))
    ;; sequence is a (simple-array (unsigned-byte 8) (*)),
    ;; and start and end are suitable integers.
    ;; if no-hang and you write less than end-start bytes then you should
    ;; return the first unwritten index as the second value
    ;; first value should then be sequence argument
    (when no-hang
      (error "~S: ~S is not supported by the default method"
             'stream-write-byte-sequence :NO-HANG))
    (when interactive
      (error "~S: ~S is not supported by the default method"
             'stream-write-byte-sequence :INTERACTIVE))
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) nil)
      (stream-write-byte stream (aref sequence index)))))

