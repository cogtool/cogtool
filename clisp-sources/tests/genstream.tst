;;; This test program creates several input and output
;;; `string' streams to test the generic stream facility.
;;; Generic-streams are used as wrappers to the string-streams.
;;; Written by Marcus Daniels, April 1994

(use-package "CLOS")
T

(progn

;; The controller is an instance of this class.
(defclass generic-stream-controller-class (generic-stream-controller)
  (source-stream      ; Input methods get/check-for characters on this stream
   destination-stream ; Output methods put/clear characters on this stream
   ;; Flags are used to indicate the success of the finish/force/clear
   ;; generic-stream methods.
   ;; The semantics of CLISP's `close' function is still CLtL1, so closed check
   ;; is done with a flag too.
   (flags :initform (copy-tree '((finish-output . nil)
                                 (force-output . nil)
                                 (clear-input . nil)
                                 (closed . nil))))))

;; A subclass of the controller class that has a flag for controlling
;; capitalization of characters which it prints.
(defclass generic-stream-controller-class-2 (generic-stream-controller-class)
  ((capitalize :initarg capitalize :initform nil)))

(defmethod convert (ch (controller generic-stream-controller-class))
  (if (standard-char-p ch) ch #\_))

(defmethod convert (ch (controller generic-stream-controller-class-2))
  (call-next-method (if (slot-value controller 'capitalize)
                        (char-upcase ch)
                      ch) controller))

;; Generic-stream methods invoked for respective stream operations..
(defmethod generic-stream-read-char ((controller generic-stream-controller-class))
  (with-slots (source-stream) controller
              (if (listen source-stream) (read-char source-stream))
              ))

(defmethod generic-stream-peek-char ((controller generic-stream-controller-class))
  (with-slots (source-stream) controller
              (if (listen source-stream) (peek-char nil source-stream) nil)
              ))

(defmethod generic-stream-read-char-will-hang-p ((controller generic-stream-controller-class))
  (if (listen (slot-value controller 'source-stream)) nil t))

(defmethod generic-stream-clear-input ((controller generic-stream-controller-class))
  (rplacd (assoc 'clear-input (slot-value controller 'flags)) t))

(defmethod generic-stream-read-byte ((controller generic-stream-controller-class))
  (char-code (read-char (slot-value controller 'source-stream))))

(defmethod generic-stream-write-byte ((controller generic-stream-controller-class) by)
  (write-char (code-char by) (slot-value controller 'destination-stream)))

(defmethod generic-stream-write-char ((controller generic-stream-controller-class) ch)
  (format (slot-value controller 'destination-stream) "~C"
          (convert ch controller)))

(defmethod generic-stream-write-string ((controller generic-stream-controller-class) str start len)
  (format (slot-value controller 'destination-stream)
          "~A" str start len))

(defmethod generic-stream-finish-output ((controller generic-stream-controller-class))
  (rplacd (assoc 'finish-output (slot-value controller 'flags)) t))

(defmethod generic-stream-force-output ((controller generic-stream-controller-class))
  (rplacd (assoc 'force-output (slot-value controller 'flags)) t))

(defmethod generic-stream-close ((controller generic-stream-controller-class))
  (rplacd (assoc 'closed (slot-value controller 'flags)) t))

;;; ------------------------------
;; Creates a generic stream, assigns source-destination stream values
(defun make-test-stream (source-stream destination-stream controller)
  (let ((genstream (make-generic-stream controller)))
    (setf (slot-value controller 'source-stream) source-stream)
    (setf (slot-value controller 'destination-stream) destination-stream)
    genstream))

;; There are several test strings, since several operations are to be
;; independently tested.

(setq str1 "0123")    ; for testing read-char & write-char
(setq str1s "4567")   ; for testing write-string
(setq str2 "abc")     ; for testing read-byte & write-byte
(setq str2c "def")    ; for testing capitalization (subclassing controller)
(setq str3 "xyz")     ; for testing read-char eof-error-p, eof-value

;; A input and output stream for each test string
(setq *string-input-stream-1* (make-string-input-stream str1))
(setq *string-output-stream-1* (make-string-output-stream))
(setq *string-input-stream-1s* (make-string-input-stream str1s))
(setq *string-output-stream-1s* (make-string-output-stream))
(setq *string-input-stream-2* (make-string-input-stream str2))
(setq *string-output-stream-2* (make-string-output-stream))
(setq *string-input-stream-2c* (make-string-input-stream str2c))
(setq *string-output-stream-2c* (make-string-output-stream))
(setq *string-input-stream-3* (make-string-input-stream str3))

;; Create the generic-streams
(setq s1 (make-test-stream
          *string-input-stream-1* *string-output-stream-1*
          (make-instance 'generic-stream-controller-class)))

(setq s1s (make-test-stream
          *string-input-stream-1s* *string-output-stream-1s*
          (make-instance 'generic-stream-controller-class)))

(setq s2 (make-test-stream
          *string-input-stream-2* *string-output-stream-2*
          (make-instance 'generic-stream-controller-class-2)))

(setq s2c (make-test-stream
           *string-input-stream-2c* *string-output-stream-2c*
           (make-instance 'generic-stream-controller-class-2 'capitalize t)))

(setq s3 (make-test-stream
           *string-input-stream-3* nil
           (make-instance 'generic-stream-controller-class)))

;; Test listen, read-char, and write-char methods
(defun copy-string-char (str stream)
  (dotimes (i (length str) t)
    (if (and (listen stream) (char-equal (char str i) (read-char stream)))
        (write-char (char str i) stream)
      (return nil))
    ))

;; Test listen, read-byte, and write-byte methods
(defun copy-string-byte (str stream)
  (dotimes (i (length str) t)
    (if (and (listen stream) (eq (char-code (char str i)) (read-byte stream)))
        (write-byte (char-code (char str i)) stream)
      (return nil))
    ))

;; Compare str to data in string-output-stream (results of wr* methods)
(defun check-output-string (str stream &key capitalize)
  (let* ((strval (if capitalize (string-upcase str) str))
         (controller (generic-stream-controller stream))
         (stream (slot-value controller 'destination-stream))
         (output-string (get-output-stream-string stream)))
        (format t "   ~S ~S ~S" str strval output-string)
        (string= strval output-string)))

;; Report results of read/write-char
(defun check-char (str stream &key capitalize)
  (let* ((test (copy-string-char str stream))
         (testr (check-output-string str stream :capitalize capitalize)))
    (format t "  char-check: ~S listen/read-char: ~S write chk: ~S~%" str test testr)
    (and test testr)))

;; Report results of read/write-byte
(defun check-byte (str stream)
  (let* ((test (copy-string-byte str stream))
         (testr (check-output-string str stream)))
    (format t " byte-check: ~S listen/read-byte: ~S write chk: ~S~%" str test testr)
    (and test testr)))

;; Report results of read/write-string
(defun check-write-string (str stream)
  (write-string str stream)
  (let ((testr (check-output-string str stream)))
    (format t "  write-string-check: ~S write chk: ~S~%" str testr)
    testr))

;; Report flag-set status
(defun check-flag (flag controller)
  (let ((status (cdr (assoc flag (slot-value controller 'flags)))))
    (format t "Checking ~S flag status: ~S~%" flag status)
    status))

(terpri)
nil
)
nil

;;; ----------
(progn
  (format t "Checking READ-CHAR & WRITE-CHAR~%")
  (check-char str1 s1))
t

(progn
  (format t "Checking WRITE-STRING~%")
  (check-write-string str1s s1s))
t

(progn
  (format t "Checking READ-BYTE & WRITE-BYTE~%")
  (check-byte str2 s2))
t

(progn
  (format t "Checking READ-CHAR & WRITE-CHAR with a subclass~%")
  (check-char str2c s2c :capitalize t))
t

;;; ----------
(progn
  (setq controller-instance (generic-stream-controller s1))
  nil)
nil

(progn
  (clear-input s1)
  (check-flag 'clear-input controller-instance))
t

(progn
  (finish-output s1)
  (check-flag 'finish-output controller-instance))
t

(progn
  (force-output s1)
  (check-flag 'force-output controller-instance))
t

(prog2
  (format t "Checking generic-stream-p (T): ")
  (princ (generic-stream-p s1))
  (terpri))
t

(prog2
  (format t "Checking generic-stream-p (NIL): ")
  (princ (generic-stream-p *string-input-stream-1*))
  (terpri))
nil

;;; ----------
(prog2
  (format t "Checking READ-CHAR's eof-error-p, eof-value: ")
  (princ
   (and
    (eql (read-char s3) #\x)
    (eql (read-char s3) #\y)
    (eql (read-char s3) #\z)
    (eql (read-char s3 nil 2) 2)))
  (terpri))
t

;;; ----------
(progn
  (format t "Checking CLOSE (NIL,T)~%")
  (close s1)
  (princ
   (and
    (eq (check-flag 'closed (generic-stream-controller s2)) NIL)
    ;; Can't run generic-stream method anymore, must use saved variable.
    (check-flag 'closed controller-instance))))
t


