;;; filling/indenting stream
;;;
;;; Copyright (C) 2004 by Sam Steingold
;;; Copyright (C) 2004 by Bruno Haible
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(in-package "EXT")
(export '(custom::*fill-indent-sexp*) "CUSTOM")
(export '(fill-stream with-fill-stream custom:*fill-indent-sexp*))
(import '(fill-stream with-fill-stream) "SYS")
(in-package "SYSTEM")

(defvar *fill-indent-sexp* #'1+
  "The default indentation of new FILL-STREAMs inside SEXPs.
This should be a number (the actual indentation),
a function of one argument (the text indentation),
NIL (no indentation) or
T (same indentation as the text, i.e., the same effect as #'IDENTITY).")

(declaim (inline right-margin))
(defun right-margin () (or *print-right-margin* sys::*prin-linelength*))

(defclass fill-stream (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)
   (buffer :type string :initform
           (make-array (right-margin) :element-type 'character
                       :fill-pointer 0 :adjustable t))
   (inside-sexp :initform nil :type boolean)
   (sexp-indent :initarg :sexp-indent :initform *fill-indent-sexp*
                :type (or symbol integer function))
   ;; the indentation level variable or number:
   (indent-var :initarg :text-indent :initform 0 :type (or symbol integer))
   (pending-space :initform nil :type boolean)
   (current-indent :initform 0 :type integer) ; current line indentation
   (pending-indent :initform nil :type (or null integer))))

(defun fill-stream-line-position (fill-stream)
  (with-slots (target-stream buffer pending-space) fill-stream
    (let ((pos (sys::line-position target-stream)))
      (if pos
          (+ pos (if pending-space 1 0) (string-width buffer))
          nil))))

(defun fill-stream-text-indent (stream)
  (let ((text-indent-raw (slot-value stream 'indent-var)))
    (etypecase text-indent-raw
      (number text-indent-raw)
      (symbol (symbol-value text-indent-raw)))))

(defun fill-stream-sexp-indent (stream)
  (let* ((sexp-indent-raw (slot-value stream 'sexp-indent))
         (text-indent (fill-stream-text-indent stream))
         (sexp-indent-value
          (etypecase sexp-indent-raw
            (number sexp-indent-raw)
            (symbol (symbol-value sexp-indent-raw))
            (function (funcall sexp-indent-raw text-indent)))))
    (case sexp-indent-value
      ((nil) 0)
      ((t) text-indent)
      (t sexp-indent-value))))

;; SEXP output has 3 cases:
;;  0. SEXP in-line: needs _no_ indentation
;;  1. 1-line SEXP on its own line: needs indentation, no embedded newlines
;;  2. multi-line SEXP in its own block: needs indentation on all lines,
;;     starts with a newline

;; flush the buffer and print a newline (when NEWLINE-P is non-NIL)
(defun fill-stream-flush-buffer (stream newline-p &aux sexp-case)
  (with-slots (target-stream buffer pending-indent current-indent
               pending-space inside-sexp)
      stream
    (flet ((newline ()          ; terpri
             (setq current-indent (fill-stream-text-indent stream)
                   pending-indent current-indent)
             (terpri target-stream)))
      (when (plusp (length buffer)) ; something in the buffer to flush
        ;; fill: if the buffer does not fit on the line, TERPRI
        (let ((pos (fill-stream-line-position stream)))
          (if (and pos (<= (right-margin) pos)) ; does not fit on this line
            (let ((multiline (find #\newline buffer))) ; only inside sexp
              (unless multiline (newline))
              (when inside-sexp ; just finished an S-expression
                (setq newline-p t sexp-case (if multiline 2 1))))
            (setq sexp-case 0)))
        (unless (and newline-p inside-sexp) ; S-expression on its own line(s)
          (cond (pending-indent      ; do the indent
                 (sys::write-spaces pending-indent target-stream)
                 (setq pending-indent nil))
                (pending-space
                 (write-char #\Space target-stream))))
        (setq pending-space nil)
        (if inside-sexp
          (case sexp-case
            (0 (write-char-sequence buffer target-stream))
            (1 (sys::write-spaces (fill-stream-sexp-indent stream)
                                  target-stream)
               (write-char-sequence buffer target-stream))
            (2 (let ((indent (fill-stream-sexp-indent stream)))
                 (do* ((beg 0 (1+ end))
                       (end (position #\Newline buffer)
                            (position #\Newline buffer :start beg)))
                      ((null end)
                       (write-char-sequence buffer target-stream :start beg))
                   (write-char-sequence buffer target-stream
                                        :start beg :end end)
                   (terpri target-stream)
                   (sys::write-spaces indent target-stream)))))
            (write-char-sequence buffer target-stream))
        (setf (fill-pointer buffer) 0))
      (when newline-p (newline)))))

(progn
(defmethod stream-write-char ((stream fill-stream) ch)
  (with-slots #1=(buffer pending-space inside-sexp) stream
    #2=
    (if inside-sexp
        (vector-push-extend ch buffer)
        (case ch
          (#\Newline (fill-stream-flush-buffer stream t))
          ((#\Space #\Tab)
           (when (plusp (length buffer))
             (fill-stream-flush-buffer stream nil))
           (setq pending-space t))
          (t (vector-push-extend ch buffer))))))
(defmethod stream-write-char-sequence ((stream fill-stream) sequence
                                       &optional (start 0) (end nil))
  (with-slots #1# stream
    ;; make sure that the buffer can accommodate the sequence
    (let ((new-size (+ (length sequence) (length buffer))))
      (when (> new-size (array-dimension buffer 0))
        (adjust-array buffer new-size)))
    ;; Same body as in stream-write-char.
    (count-if (lambda (ch) #2#) sequence :start start :end end))
  sequence))

(defmethod stream-line-column ((stream fill-stream))
  (let ((pos (fill-stream-line-position stream)))
    (if pos (max (- pos (slot-value stream 'current-indent)) 0) nil)))
(defmethod stream-start-line-p ((stream fill-stream))
  (let ((pos (fill-stream-line-position stream)))
    (if pos (<= pos (slot-value stream 'current-indent)) nil)))
(defmethod stream-finish-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (finish-output (slot-value stream 'target-stream)))
(defmethod stream-force-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (force-output (slot-value stream 'target-stream)))
(defmethod stream-clear-output ((stream fill-stream))
  (with-slots (target-stream buffer pending-indent pending-space) stream
    (setq pending-indent nil pending-space nil)
    (setf (fill-pointer buffer) 0)
    (clear-output target-stream)))

(defmacro with-fill-stream ((stream-var target-stream &rest opts) &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    `(LET ((,stream-var (MAKE-INSTANCE 'fill-stream :STREAM ,target-stream
                                       ,@opts)))
       (DECLARE (READ-ONLY ,stream-var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (FORCE-OUTPUT ,stream-var)))))

;;; for format, see `format-s-expression'
(fmakunbound 'stream-start-s-expression)
(fmakunbound 'stream-end-s-expression)
(defgeneric stream-start-s-expression (stream)
  (:documentation "return the new binding for *PRINT-RIGHT-MARGIN*")
  (:method ((stream t)) (declare (ignore stream)))
  (:method ((stream fill-stream))
    (fill-stream-flush-buffer stream nil)
    (setf (slot-value stream 'inside-sexp) t)
    (- (right-margin) (fill-stream-sexp-indent stream))))
(defgeneric stream-end-s-expression (stream)
  (:method ((stream t)) (declare (ignore stream)))
  (:method ((stream fill-stream))
    (fill-stream-flush-buffer stream nil)
    (setf (slot-value stream 'inside-sexp) nil)))
