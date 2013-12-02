;;; https://sourceforge.net/tracker/index.php?func=detail&aid=1310753&group_id=1355&atid=101355
;;; fixed on 2005-10-14:
;;; * stream.d (read_char): fixed recursion: cannot assume that the
;;; constituent streams of constructed streams are built-in

(defclass listener-input-stream (fundamental-input-stream)
  ((s :reader dis-s :initarg :stream :initform *standard-input*)
   (c :accessor dis-last :initform nil)))

(defmethod print-object ((obj listener-input-stream) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (obj out :type t :identity t)
        (write (dis-last obj) :stream out)
        (write-char #\Space out)
        (write (dis-s obj) :stream out))))

(defmethod stream-read-char ((strm listener-input-stream))
  (handler-case
      (or (pop (dis-last strm))
          (let ((s (dis-s strm)))
            (loop
              (if (listen s)
                  (return (read-char s))
                  (sleep 0.2)) ; Sleep to avoid too much cpu use
              (call-listeners))))
    (error (er) (princ er) (disable-listeners))))

(defmethod stream-unread-char ((strm listener-input-stream) chr)
  (push chr (dis-last strm))
  nil)

(defmethod stream-read-char-no-hang ((strm listener-input-stream))
  (call-listeners)
  (handler-case (or (pop (dis-last strm)) (read-char-no-hang (dis-s strm)))
    (error (er) (princ er) (disable-listeners))))

(defmethod stream-listen ((strm listener-input-stream))
  (call-listeners)
  (handler-case (or (not (endp (dis-last strm))) (listen (dis-s strm)))
    (error (er) (princ er) (disable-listeners))))

(defmethod stream-clear-input ((strm listener-input-stream))
  (setf (dis-last strm) nil)
  (handler-case (clear-input (dis-s strm))
    (error (er) (princ er) (disable-listeners))))

(defmethod close ((strm listener-input-stream) &key abort)
  (declare (ignore abort))
  (setf (dis-last strm) nil))

;; Call listeners: do something extra while the system waits for input
(defun call-listeners ())

;; ****************************************************************************
;; Enable/disable the listening to listeners
;; ****************************************************************************

(defun enable-listeners ()
  (typecase *standard-input*
    (listener-input-stream)
    (t (setf *standard-input* (make-instance 'listener-input-stream)))))
(defun disable-listeners ()
  (typecase *standard-input*
    (listener-input-stream
     (setq *standard-input* (dis-s *standard-input*)))))
