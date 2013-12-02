;;; ===========================================================================
;;; generic stream default methods
;;; Marcus Daniels 16.4.1994

(in-package "GSTREAM")
(common-lisp:export
 '(generic-stream-read-char
   generic-stream-peek-char
   generic-stream-read-char-will-hang-p
   generic-stream-clear-input
   generic-stream-write-char
   generic-stream-write-string
   generic-stream-finish-output
   generic-stream-force-output
   generic-stream-clear-output
   generic-stream-read-byte
   generic-stream-write-byte
   generic-stream-close
   generic-stream-controller
   generic-stream-p
   make-generic-stream))
(common-lisp:in-package "SYSTEM")
(use-package '("GSTREAM") "EXT")
(ext:re-export "GSTREAM" "EXT")

(clos:defclass generic-stream-controller () ())

(clos:defgeneric generic-stream-read-char (controller))
(clos:defgeneric generic-stream-peek-char (controller))
(clos:defgeneric generic-stream-read-char-will-hang-p (controller))
(clos:defgeneric generic-stream-clear-input (controller))
(clos:defgeneric generic-stream-write-char (controller ch))
(clos:defgeneric generic-stream-write-string (controller string start len))
(clos:defgeneric generic-stream-finish-output (controller))
(clos:defgeneric generic-stream-force-output (controller))
(clos:defgeneric generic-stream-clear-output (controller))
(clos:defgeneric generic-stream-read-byte (controller))
(clos:defgeneric generic-stream-write-byte (controller by))
(clos:defgeneric generic-stream-close (controller))

(clos:defmethod generic-stream-read-char ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-peek-char ((controller generic-stream-controller))
  (values (generic-stream-read-char controller) t)
)

(clos:defmethod generic-stream-read-char-will-hang-p ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-clear-input ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-write-char ((controller generic-stream-controller) ch)
  (declare (ignore controller ch))
)

(clos:defmethod generic-stream-write-string ((controller generic-stream-controller) string start len)
  (dotimes (i len)
    (generic-stream-write-char controller (schar string (+ start i)))
) )

(clos:defmethod generic-stream-finish-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-force-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-clear-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-read-byte ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-write-byte ((controller generic-stream-controller) by)
  (declare (ignore controller by))
)

(clos:defmethod generic-stream-close ((controller generic-stream-controller))
  (declare (ignore controller))
)

#| ;; Example:
;; Alias streams just perform the required operation on another given stream.
(defclass alias-controller (generic-stream-controller)
  ((orig-stream :initarg :orig-stream))
)
(defun make-alias-stream (orig-stream)
  (make-generic-stream
    (make-instance 'alias-controller :orig-stream orig-stream)
) )
(defmethod generic-stream-read-char ((controller alias-controller))
  (with-slots (orig-stream) controller
    (read-char orig-stream nil nil)
) )
(defmethod generic-stream-peek-char ((controller alias-controller))
  (with-slots (orig-stream) controller
    (values (peek-char nil orig-stream nil nil) nil)
) )
(defmethod generic-stream-read-char-will-hang-p ((controller alias-controller))
  (with-slots (orig-stream) controller
    (if (listen orig-stream)
      nil
      (let ((ch (read-char-no-hang orig-stream nil t)))
        (cond ((eql ch t) nil) ; EOF
              ((null ch) t) ; nothing available, not EOF
              (t (unread-char ch orig-stream) nil)
) ) ) ) )
(defmethod generic-stream-clear-input ((controller alias-controller))
  (with-slots (orig-stream) controller
    (clear-input orig-stream)
    t
) )
(defmethod generic-stream-write-char ((controller alias-controller) ch)
  (with-slots (orig-stream) controller
    (write-char ch orig-stream)
) )
#| ; not needed, see general method above
(defmethod generic-stream-write-string ((controller alias-controller) string start len)
  (with-slots (orig-stream) controller
    (dotimes (i len)
      (write-char (schar string (+ start i)) orig-stream)
) ) )
|#
(defmethod generic-stream-finish-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (finish-output orig-stream)
) )
(defmethod generic-stream-force-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (force-output orig-stream)
) )
(defmethod generic-stream-clear-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (clear-output orig-stream)
) )
(defmethod generic-stream-read-byte ((controller alias-controller))
  (with-slots (orig-stream) controller
    (read-byte orig-stream nil nil)
) )
(defmethod generic-stream-write-byte (i (controller alias-controller))
  (with-slots (orig-stream) controller
    (write-byte i orig-stream)
) )
(defmethod generic-stream-close ((controller alias-controller))
  ; don't close orig-stream
)
|#

