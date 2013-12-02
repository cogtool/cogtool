;; -*- Lisp -*-
;; CLISP-specific tests of binary i/o
;; READ-FLOAT, WRITE-FLOAT
;; READ-INTEGER, WRITE-INTEGER
;; <http://clisp.cons.org/impnotes/stream-dict.html#bin-input>
;; <http://clisp.cons.org/impnotes/stream-dict.html#bin-output>
;; <http://clisp.cons.org/impnotes.html#bin-input>
;; <http://clisp.cons.org/impnotes.html#bin-output>

(defun clisp-test-bin-i/o (&key (num 10) (file-name "./foocl")
                           (type 'unsigned-byte) (size 40) (endianness :little)
                           (int-list (ecase type
                                       (unsigned-byte
                                        (loop :with max = (ash 1 size)
                                          :repeat num :collect (random max)))
                                       (signed-byte
                                        (loop :with max = (ash 1 size)
                                          :and top = (ash 1 (1- size))
                                          :repeat num
                                          :collect (- (random max) top)))))
                           (float-list (loop :repeat num :collect
                                             (random 1d0))))
  (let ((eltype (list type size)))
    (with-open-file (foo file-name :direction :output
                         #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede
                         :element-type 'unsigned-byte)
      (dolist (num int-list)
        (write-integer num foo eltype endianness))
      (dolist (num float-list)
        (write-float num foo 'double-float endianness)))
    (unwind-protect
         (with-open-file (foo file-name :direction :input
                              :element-type 'unsigned-byte)
           (list (file-length foo) int-list float-list
                 (loop :for num :in int-list
                       :for nn = (read-integer foo eltype endianness)
                       :collect nn :unless (= nn num) :do
                       (error "~s/~s: wrote: ~s  read: ~s"
                              endianness eltype num nn))
                 (loop :for num :in float-list
                       :for nn = (read-float foo 'double-float
                                             endianness)
                       :collect nn :unless (= nn num) :do
                       (error "~s: wrote: ~s  read: ~s"
                              endianness num nn))))
      (delete-file file-name))))
clisp-test-bin-i/o

(dolist (e '(:little :big))
  (dolist (s '(unsigned-byte signed-byte))
    (clisp-test-bin-i/o :endianness e :type s)))
nil

(let ((vec (make-array 8 :element-type '(unsigned-byte 8)
                         :initial-contents '(#x3f #xf0 0 0 0 0 0 0))))
  (with-open-file (foo "./foocl" :direction :output
                                 #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede
                                 :element-type '(unsigned-byte 8))
    (write-sequence vec foo))
  (unwind-protect
       (with-open-file (foo "./foocl" :direction :input
                            :element-type '(unsigned-byte 8))
         (read-float foo 'double-float :big))
    (delete-file "./foocl")))
1d0

(progn
  (defclass list-input-stream (fundamental-input-stream)
    ((list :initarg :list)))
  (defmethod stream-element-type ((stream list-input-stream)) t)
  (defmethod stream-read-char ((stream list-input-stream))
    (with-slots (list) stream
      (if list
          (let ((ret (pop list)))
            (typecase ret
              (integer (code-char ret))
              (character ret)
              (t (coerce ret 'character))))
          :eof)))
  (defmethod stream-unread-char ((stream list-input-stream) (char character))
    (with-slots (list) stream (push char list)))
  (defmethod stream-read-byte ((stream list-input-stream))
    (with-slots (list) stream
      (if list
          (let ((ret (pop list)))
            (typecase ret
              (integer ret)
              (character (char-code ret))
              (t (coerce ret 'integer))))
          :eof)))
  (defun list->integer (list type endianness)
    (read-integer (make-instance 'list-input-stream :list list)
                  type endianness))
  (defun list->float (list type endianness)
    (read-float (make-instance 'list-input-stream :list list)
                type endianness)))
list->float

(list->float '(#x3f #xf0 0 0 0 0 0 0) 'double-float :big)
1d0

(list->float '(0 0 0 0 0 0 #xf0 #x3f) 'double-float :little)
1d0

(list->integer '(0 1) '(unsigned-byte 16) :big)
1

(list->integer '(1 0) '(unsigned-byte 16) :big)
256

(list->integer '(1 0) '(unsigned-byte 16) :little)
1

(list->integer '(0 1) '(unsigned-byte 16) :little)
256

(progn
  (defclass list-output-stream (fundamental-output-stream)
    ((list :initform nil)))
  (defmethod stream-element-type ((stream list-output-stream)) t)
  (defmethod stream-write-char ((stream list-output-stream) (char character))
    (with-slots (list) stream
      (push char list)))
  (defmethod stream-write-byte ((stream list-output-stream) (byte integer))
    (with-slots (list) stream
      (push byte list)))
  (defun integer->list (integer type endianness)
    (let ((out (make-instance 'list-output-stream)))
      (write-integer integer out type endianness)
      (with-slots (list) out
        (reverse list))))
  (defun float->list (float type endianness)
    (let ((out (make-instance 'list-output-stream)))
      (write-float float out type endianness)
      (with-slots (list) out
        (reverse list)))))
float->list

(float->list 1d0 'double-float :big)
(#x3f #xf0 0 0 0 0 0 0)

(float->list 1d0 'double-float :little)
(0 0 0 0 0 0 #xf0 #x3f)

(integer->list 1 '(unsigned-byte 16) :big)
(0 1)

(integer->list 1 '(unsigned-byte 16) :little)
(1 0)
