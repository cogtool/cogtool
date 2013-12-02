;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;; This file contains some of the system dependent code for CLX

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

(proclaim '(declaration array-register))

#+cmu
(setf (getf ext:*herald-items* :xlib)
      `("    CLX X Library " ,*version*))

;;; The size of the output buffer.  Must be a multiple of 4.
(defparameter *output-buffer-size* 8192)

;;; Number of seconds to wait for a reply to a server request
(defparameter *reply-timeout* nil)

#-(or (not clx-little-endian))
(progn
  (defconstant +word-0+ 0)
  (defconstant +word-1+ 1)

  (defconstant +long-0+ 0)
  (defconstant +long-1+ 1)
  (defconstant +long-2+ 2)
  (defconstant +long-3+ 3))

#-clx-little-endian
(progn
  (defconstant +word-0+ 1)
  (defconstant +word-1+ 0)

  (defconstant +long-0+ 3)
  (defconstant +long-1+ 2)
  (defconstant +long-2+ 1)
  (defconstant +long-3+ 0))

;;; Set some compiler-options for often used code

(eval-when (:execute :compile-toplevel :load-toplevel)

  (defconstant +buffer-speed+ #+clx-debugging 1 #-clx-debugging 3
               "Speed compiler option for buffer code.")
  (defconstant +buffer-safety+ #+clx-debugging 3 #-clx-debugging 0
               "Safety compiler option for buffer code.")

  (defun declare-bufmac ()
    `(declare (optimize (speed ,+buffer-speed+) (safety ,+buffer-safety+))))

;;; It's my impression that in lucid there's some way to make a declaration
;;; called fast-entry or something that causes a function to not do some
;;; checking on args. Sadly, we have no lucid manuals here.  If such a
;;; declaration is available, it would be a good idea to make it here when
;;; +buffer-speed+ is 3 and +buffer-safety+ is 0.
  (defun declare-buffun ()
    #+clx-debugging
    '(declare (optimize (speed 0) (safety 3)))
    #-clx-debugging
    `(declare (optimize (speed ,+buffer-speed+) (safety ,+buffer-safety+))))

  )

(declaim (inline card8->int8 int8->card8
		 card16->int16 int16->card16
		 card32->int32 int32->card32))


(progn

  (defun card8->int8 (x)
    (declare (type card8 x))
    (declare (clx-values int8))
    #.(declare-buffun)
    (the int8 (if (logbitp 7 x)
                  (the int8 (- x #x100))
                  x)))

  (defun int8->card8 (x)
    (declare (type int8 x))
    (declare (clx-values card8))
    #.(declare-buffun)
    (the card8 (ldb (byte 8 0) x)))

  (defun card16->int16 (x)
    (declare (type card16 x))
    (declare (clx-values int16))
    #.(declare-buffun)
    (the int16 (if (logbitp 15 x)
                   (the int16 (- x #x10000))
                   x)))

  (defun int16->card16 (x)
    (declare (type int16 x))
    (declare (clx-values card16))
    #.(declare-buffun)
    (the card16 (ldb (byte 16 0) x)))

  (defun card32->int32 (x)
    (declare (type card32 x))
    (declare (clx-values int32))
    #.(declare-buffun)
    (the int32 (if (logbitp 31 x)
                   (the int32 (- x #x100000000))
                   x)))

  (defun int32->card32 (x)
    (declare (type int32 x))
    (declare (clx-values card32))
    #.(declare-buffun)
    (the card32 (ldb (byte 32 0) x)))

  )

(declaim (inline aref-card8 aset-card8 aref-int8 aset-int8))


(progn

  (defun aref-card8 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card8))
    #.(declare-buffun)
    (the card8 (aref a i)))

  (defun aset-card8 (v a i)
    (declare (type card8 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a i) v))

  (defun aref-int8 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values int8))
    #.(declare-buffun)
    (card8->int8 (aref a i)))

  (defun aset-int8 (v a i)
    (declare (type int8 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a i) (int8->card8 v)))

  )


(progn

  (defun aref-card16 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card16))
    #.(declare-buffun)
    (the card16
      (logior (the card16
                (ash (the card8 (aref a (index+ i +word-1+))) 8))
              (the card8
                (aref a (index+ i +word-0+))))))

  (defun aset-card16 (v a i)
    (declare (type card16 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int16 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values int16))
    #.(declare-buffun)
    (the int16
      (logior (the int16
                (ash (the int8 (aref-int8 a (index+ i +word-1+))) 8))
              (the card8
                (aref a (index+ i +word-0+))))))

  (defun aset-int16 (v a i)
    (declare (type int16 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card32 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card32))
    #.(declare-buffun)
    (the card32
      (logior (the card32
                (ash (the card8 (aref a (index+ i +long-3+))) 24))
              (the card29
                (ash (the card8 (aref a (index+ i +long-2+))) 16))
              (the card16
                (ash (the card8 (aref a (index+ i +long-1+))) 8))
              (the card8
                (aref a (index+ i +long-0+))))))

  (defun aset-card32 (v a i)
    (declare (type card32 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int32 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values int32))
    #.(declare-buffun)
    (the int32
      (logior (the int32
                (ash (the int8 (aref-int8 a (index+ i +long-3+))) 24))
              (the card29
                (ash (the card8 (aref a (index+ i +long-2+))) 16))
              (the card16
                (ash (the card8 (aref a (index+ i +long-1+))) 8))
              (the card8
                (aref a (index+ i +long-0+))))))

  (defun aset-int32 (v a i)
    (declare (type int32 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card29 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card29))
    #.(declare-buffun)
    (the card29
      (logior (the card29
                (ash (the card8 (aref a (index+ i +long-3+))) 24))
              (the card29
                (ash (the card8 (aref a (index+ i +long-2+))) 16))
              (the card16
                (ash (the card8 (aref a (index+ i +long-1+))) 8))
              (the card8
                (aref a (index+ i +long-0+))))))

  (defun aset-card29 (v a i)
    (declare (type card29 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  )

(defsetf aref-card8 (a i) (v)
  `(aset-card8 ,v ,a ,i))

(defsetf aref-int8 (a i) (v)
  `(aset-int8 ,v ,a ,i))

(defsetf aref-card16 (a i) (v)
  `(aset-card16 ,v ,a ,i))

(defsetf aref-int16 (a i) (v)
  `(aset-int16 ,v ,a ,i))

(defsetf aref-card32 (a i) (v)
  `(aset-card32 ,v ,a ,i))

(defsetf aref-int32 (a i) (v)
  `(aset-int32 ,v ,a ,i))

(defsetf aref-card29 (a i) (v)
  `(aset-card29 ,v ,a ,i))

;;; Other random conversions

(defun rgb-val->card16 (value)
  ;; Short floats are good enough
  (declare (type rgb-val value))
  (declare (clx-values card16))
  #.(declare-buffun)
  ;; Convert VALUE from float to card16
  (the card16 (values (round (the rgb-val value) #.(/ 1.0s0 #xffff)))))

(defun card16->rgb-val (value)
  ;; Short floats are good enough
  (declare (type card16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  ;; Convert VALUE from card16 to float
  (the short-float (* (the card16 value) #.(/ 1.0s0 #xffff))))

(defun radians->int16 (value)
  ;; Short floats are good enough
  (declare (type angle value))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (values (round (the angle value) #.(float (/ pi 180 64) 0s0)))))

(defun int16->radians (value)
  ;; Short floats are good enough
  (declare (type int16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  (the short-float (* (the int16 value) #.(float (/ pi 180 64) 0s0))))


;;-----------------------------------------------------------------------------
;; Character transformation
;;-----------------------------------------------------------------------------


;;; This stuff transforms chars to ascii codes in card8's and back.
;;; You might have to hack it a little to get it to work for your machine.

(declaim (inline char->card8 card8->char))

(macrolet ((char-translators ()
	     (let ((alist
                    `(;; The normal ascii codes for the control characters.
                      ,@`((#\Return . 13)
                          (#\Linefeed . 10)
                          (#\Rubout . 127)
                          (#\Page . 12)
                          (#\Tab . 9)
                          (#\Backspace . 8)
                          (#\Newline . 10)
                          (#\Space . 32))

                        ;; The rest of the common lisp charater set with the normal
                        ;; ascii codes for them.
                      (#\! . 33) (#\" . 34) (#\# . 35) (#\$ . 36)
                      (#\% . 37) (#\& . 38) (#\' . 39) (#\( . 40)
                      (#\) . 41) (#\* . 42) (#\+ . 43) (#\, . 44)
                      (#\- . 45) (#\. . 46) (#\/ . 47) (#\0 . 48)
                      (#\1 . 49) (#\2 . 50) (#\3 . 51) (#\4 . 52)
                      (#\5 . 53) (#\6 . 54) (#\7 . 55) (#\8 . 56)
                      (#\9 . 57) (#\: . 58) (#\; . 59) (#\< . 60)
                      (#\= . 61) (#\> . 62) (#\? . 63) (#\@ . 64)
                      (#\A . 65) (#\B . 66) (#\C . 67) (#\D . 68)
                      (#\E . 69) (#\F . 70) (#\G . 71) (#\H . 72)
                      (#\I . 73) (#\J . 74) (#\K . 75) (#\L . 76)
                      (#\M . 77) (#\N . 78) (#\O . 79) (#\P . 80)
                      (#\Q . 81) (#\R . 82) (#\S . 83) (#\T . 84)
                      (#\U . 85) (#\V . 86) (#\W . 87) (#\X . 88)
                      (#\Y . 89) (#\Z . 90) (#\[ . 91) (#\\ . 92)
                      (#\] . 93) (#\^ . 94) (#\_ . 95) (#\` . 96)
                      (#\a . 97) (#\b . 98) (#\c . 99) (#\d . 100)
                      (#\e . 101) (#\f . 102) (#\g . 103) (#\h . 104)
                      (#\i . 105) (#\j . 106) (#\k . 107) (#\l . 108)
                      (#\m . 109) (#\n . 110) (#\o . 111) (#\p . 112)
                      (#\q . 113) (#\r . 114) (#\s . 115) (#\t . 116)
                      (#\u . 117) (#\v . 118) (#\w . 119) (#\x . 120)
                      (#\y . 121) (#\z . 122) (#\{ . 123) (#\| . 124)
                      (#\} . 125) (#\~ . 126))))
	       (cond ((dolist (pair alist nil)
			(when (not (= (char-code (car pair)) (cdr pair)))
			  (return t)))
		      `(progn
			 (defconstant +char-to-card8-translation-table+
                           ',(let ((array (make-array
                                           (let ((max-char-code 255))
                                             (dolist (pair alist)
                                               (setq max-char-code
                                                     (max max-char-code
                                                          (char-code (car pair)))))
                                             (1+ max-char-code))
                                           :element-type 'card8)))
                                  (dotimes (i (length array))
                                    (setf (aref array i) (mod i 256)))
                                  (dolist (pair alist)
                                    (setf (aref array (char-code (car pair)))
                                          (cdr pair)))
                                  array))
			 (defconstant +card8-to-char-translation-table+
                           ',(let ((array (make-array 256)))
                                  (dotimes (i (length array))
                                    (setf (aref array i) (code-char i)))
                                  (dolist (pair alist)
                                    (setf (aref array (cdr pair)) (car pair)))
                                  array))
			 (progn
  			   (defun char->card8 (char)
			     (declare (type base-char char))
			     #.(declare-buffun)
			     (the card8 (aref (the (simple-array card8 (*))
                                                +char-to-card8-translation-table+)
					      (the array-index (char-code char)))))
			   (defun card8->char (card8)
			     (declare (type card8 card8))
			     #.(declare-buffun)
			     (the base-char
                               (or (aref (the simple-vector +card8-to-char-translation-table+)
                                         card8)
                                   (error "Invalid CHAR code ~D." card8))))
			   )
			 (dotimes (i 256)
			   (unless (= i (char->card8 (card8->char i)))
			     (warn "The card8->char mapping is not invertible through char->card8.  Info:~%~S"
				   (list i
					 (card8->char i)
					 (char->card8 (card8->char i))))
			     (return nil)))
			 (dotimes (i (length +char-to-card8-translation-table+))
			   (let ((char (code-char i)))
			     (unless (eql char (card8->char (char->card8 char)))
			       (warn "The char->card8 mapping is not invertible through card8->char.  Info:~%~S"
				     (list char
					   (char->card8 char)
					   (card8->char (char->card8 char))))
			       (return nil))))))
		     (t
		      `(progn
			 (defun char->card8 (char)
			   (declare (type base-char char))
			   #.(declare-buffun)
			   (the card8 (char-code char)))
			 (defun card8->char (card8)
			   (declare (type card8 card8))
			   #.(declare-buffun)
			   (the base-char (code-char card8)))
			 ))))))
  (char-translators))

;;-----------------------------------------------------------------------------
;; Process Locking
;;
;;	Common-Lisp doesn't provide process locking primitives, so we define
;;	our own here, based on Zetalisp primitives.  Holding-Lock is very
;;	similar to with-lock on The TI Explorer, and a little more efficient
;;	than with-process-lock on a Symbolics.
;;-----------------------------------------------------------------------------

;;; MAKE-PROCESS-LOCK: Creating a process lock.

(defun make-process-lock (name)
  #+(or sbcl clisp) (declare (ignore name))
  #-(or sbcl clisp)
  (port::make-lock :name name))


;;; HOLDING-LOCK: Execute a body of code with a lock held.

;;; The holding-lock macro takes a timeout keyword argument.  EVENT-LISTEN
;;; passes its timeout to the holding-lock macro, so any timeout you want to
;;; work for event-listen you should do for holding-lock.

(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
			      &key timeout)
			&body body)
  (declare (ignore lock display whostate timeout))
  `(progn
     ,@body))


;;; WITHOUT-ABORTS

;;; If you can inhibit asynchronous keyboard aborts inside the body of this
;;; macro, then it is a good idea to do this.  This macro is wrapped around
;;; request writing and reply reading to ensure that requests are atomically
;;; written and replies are atomically read from the stream.

#-:lcl3.0
(defmacro without-aborts (&body body)
  `(progn ,@body))
;;; XXX [pve] This needs to be more robust.

;;; PROCESS-BLOCK: Wait until a given predicate returns a non-NIL value.
;;; Caller guarantees that PROCESS-WAKEUP will be called after the predicate's
;;; value changes.

(defun process-block (whostate predicate &rest predicate-args)
  (declare (type function predicate))
  (error "~S(~S ~S ~S) is not implemented"
         'process-block whostate predicate predicate-args))



;;; PROCESS-WAKEUP: Check some other process' wait function.

(declaim (inline process-wakeup))

(defun process-wakeup (process)
  (declare (ignore process))
  #-(or clisp sbcl)
  (port:process-yield))

;;; CURRENT-PROCESS: Return the current process object for input locking and
;;; for calling PROCESS-WAKEUP.

(declaim (inline current-process))

;;; Default return NIL, which is acceptable even if there is a scheduler.

(defun current-process ()
  nil)

;;; WITHOUT-INTERRUPTS -- provide for atomic operations.
;;; XXX [pve]: this thing is used quite a lot
;;; but we should find a better solution.
#-(or cmu)
(defmacro without-interrupts (&body body)
  `(progn ,@body))

#+cmu
(defmacro without-interrupts (&body body)
  `(sys:without-interrupts ,@body))

;;; CONDITIONAL-STORE:

(defmacro conditional-store (place old-value new-value &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(without-interrupts
       (let ,(mapcar #'list vars vals)
         (cond ((eq ,reader-form ,old-value)
                (let ((,(first store-vars) ,new-value) ,@(rest store-vars))
                  ,writer-form)
                t))))))

;;;----------------------------------------------------------------------------
;;; IO Error Recovery
;;;	All I/O operations are done within a WRAP-BUF-OUTPUT macro.
;;;	It prevents multiple mindless errors when the network craters.
;;;
;;;----------------------------------------------------------------------------

(defmacro wrap-buf-output ((buffer) &body body)
  ;; Error recovery wrapper
  `(unless (buffer-dead ,buffer)
     ,@body))

(defmacro wrap-buf-input ((buffer) &body body)
  (declare (ignore buffer))
  ;; Error recovery wrapper
  `(progn ,@body))


;;;----------------------------------------------------------------------------
;;; System dependent IO primitives
;;;	Functions for opening, reading writing forcing-output and closing
;;;	the stream to the server.
;;;----------------------------------------------------------------------------

;;; OPEN-X-STREAM -- for CMU Common Lisp.
;;;
;;; The file descriptor here just gets tossed into the stream slot of the
;;; display object instead of a stream.
;;;

(defparameter *X-unix-socket-path*+
  "/tmp/.X11-unix/X"
  "The location of the X socket")

(defun open-x-stream (host display protocol)
  (declare (ignore protocol)
           (type (integer 0) display))
  (let ((socket
         ;; are we dealing with a localhost?
         (progn #+nil ignore-errors
           (when (or (string= host "")
                     (string= host "unix"))
             ;; ok, try to connect to a AF_UNIX domain socket
             #-clisp
             (port::open-unix-socket (format nil
                                             "~A~D"
                                             *X-unix-socket-path*+
                                             display)
                                     :kind :stream
                                     :bin t)
             ;; clisp has special code for X connections:
             #+clisp
             (sys::make-socket-stream "" display)))))
    (if socket
        socket
        ;; try to connect by hand
        (let ((host (host-address host)))
          (when host
            ;; Fixme: get a descent ip standard in CLX: a vector!
            (let ((ip (format nil
                              "~{~D~^.~}"
                              (rest host))))
              #+clisp
              (socket:socket-connect (+ 6000 display) ip
                                     :element-type '(unsigned-byte 8))
              #-clisp
              (port:open-socket ip
                                (+ 6000 display)
                                ;; binary:
                                t)))))))

;;; BUFFER-READ-DEFAULT - read data from the X stream

;;; BUFFER-READ-DEFAULT for CMU Common Lisp.
;;;
;;;    If timeout is 0, then we call LISTEN to see if there is any input.
;;; Timeout 0 is the only case where READ-INPUT dives into BUFFER-READ without
;;; first calling BUFFER-INPUT-WAIT-DEFAULT.
;;;
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null fixnum) timeout))
  #.(declare-buffun)
  (cond ((and (eql timeout 0)
	      (not (listen (display-input-stream display))))
	 :timeout)
	(t
         (read-sequence vector
                        (display-input-stream display)
                        :start start
                        :end end)
         nil)))

;;; BUFFER-WRITE-DEFAULT - write data to the X stream

(defun buffer-write-default (vector display start end)
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (write-sequence vector
                  (display-output-stream display)
                  :start start
                  :end end)
  nil)

;;; buffer-force-output-default - force output to the X stream

(defun buffer-force-output-default (display)
  ;; The default buffer force-output function for use with common-lisp streams
  (declare (type display display))
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (force-output stream))))

;;; BUFFER-CLOSE-DEFAULT - close the X stream

(defun buffer-close-default (display &key abort)
  ;; The default buffer close function for use with common-lisp streams
  (declare (type display display))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (close stream :abort abort))))

;;; BUFFER-INPUT-WAIT-DEFAULT - wait for for input to be available for the
;;; buffer.  This is called in read-input between requests, so that a process
;;; waiting for input is abortable when between requests.  Should return
;;; :TIMEOUT if it times out, NIL otherwise.

;;; The default implementation

#+allegro
(defparameter *buffer-read-polling-time* 0.5
  "Poll for input every X seconds")

(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null number) timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((listen stream) nil)
	  ((eql timeout 0) :timeout)
          #-allegro
	  (t
	   (if #-clisp (port::wait-for-stream stream timeout)
               #+clisp (multiple-value-bind (sec usec) (floor (or timeout 0))
                         (ext:socket-status stream (and timeout sec)
                                            (round usec 1d-6)))
	       nil
	       :timeout))
          #+allegro
          ((not (null timeout))
           (multiple-value-bind (npoll fraction)
               (truncate timeout *buffer-read-polling-time*)
             (dotimes (i npoll)                 ; Sleep for a time, then listen again
               (sleep *buffer-read-polling-time*)
               (when (listen stream)
                 (return-from buffer-input-wait-default nil)))
             (when (plusp fraction)
               (sleep fraction)                 ; Sleep a fraction of a second
               (when (listen stream)            ; and listen one last time
                 (return-from buffer-input-wait-default nil)))
             :timeout)))))

;;; BUFFER-LISTEN-DEFAULT - returns T if there is input available for the
;;; buffer. This should never block, so it can be called from the scheduler.

;;; The default implementation is to just use listen.

(defun buffer-listen-default (display)
  (declare (type display display))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (if (null stream)
	t
        (listen stream))))


;;;----------------------------------------------------------------------------
;;; System dependent speed hacks
;;;----------------------------------------------------------------------------

;;
;; WITH-STACK-LIST is used by WITH-STATE as a memory saving feature.
;; If your lisp doesn't have stack-lists, and you're worried about
;; consing garbage, you may want to re-write this to allocate and
;; initialize lists from a resource.
;;
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
	      (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements)))
     (declare (type cons ,var)
	      (dynamic-extent ,var))
     ,@body))

(declaim (inline buffer-replace))

(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
	   (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))

(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
				  &body body)
  (let ((local-state (gensym))
	(resets nil))
    (dolist (index indexes)
      (push `(setf (svref ,local-state ,index) (svref ,saved-state ,index))
	    resets))
    `(unwind-protect
          (progn
            ,@body)
       (let ((,local-state (gcontext-local-state ,gc)))
	 (declare (type gcontext-state ,local-state))
	 ,@resets
	 (setf (svref ,local-state ,ts-index) 0))
       (when ,temp-gc
	 (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
       (deallocate-gcontext-state ,saved-state))))

;;;----------------------------------------------------------------------------
;;; How error detection should CLX do?
;;; Several levels are possible:
;;;
;;; 1. Do the equivalent of check-type on every argument.
;;;
;;; 2. Simply report TYPE-ERROR.  This eliminates overhead of all the format
;;;    strings generated by check-type.
;;;
;;; 3. Do error checking only on arguments that are likely to have errors
;;;    (like keyword names)
;;;
;;; 4. Do error checking only where not doing so may dammage the envirnment
;;;    on a non-tagged machine (i.e. when storing into a structure that has
;;;    been passed in)
;;;
;;; 5. No extra error detection code.  On lispm's, ASET may barf trying to
;;;    store a non-integer into a number array.
;;;
;;; How extensive should the error checking be?  For example, if the server
;;; expects a CARD16, is is sufficient for CLX to check for integer, or
;;; should it also check for non-negative and less than 65536?
;;;----------------------------------------------------------------------------

;; The +TYPE-CHECK?+ constant controls how much error checking is done.
;; Possible values are:
;;    NIL      - Don't do any error checking
;;    t        - Do the equivalent of checktype on every argument
;;    :minimal - Do error checking only where errors are likely

;;; This controls macro expansion, and isn't changable at run-time You will
;;; probably want to set this to nil if you want good performance at
;;; production time.
(defconstant +type-check?+ #+clx-debugging t #-clx-debugging nil)

;; TYPE? is used to allow the code to do error checking at a different level from
;; the declarations.  It also does some optimizations for systems that don't have
;; good compiler support for TYPEP.  The definitions for CARD32, CARD16, INT16, etc.
;; include range checks.  You can modify TYPE? to do less extensive checking
;; for these types if you desire.

;;
;; ### This comment is a lie!  TYPE? is really also used for run-time type
;; dispatching, not just type checking.  -- Ram.

(defmacro type? (object type)
  `(typep ,object ,type))

;; X-TYPE-ERROR is the function called for type errors.
;; If you want lots of checking, but are concerned about code size,
;; this can be made into a macro that ignores some parameters.

(defun x-type-error (object type &optional error-string)
  (x-error 'x-type-error
	   :datum object
	   :expected-type type
	   :type-string error-string))


;;-----------------------------------------------------------------------------
;; Error handlers
;;    Hack up KMP error signaling using zetalisp until the real thing comes
;;    along
;;-----------------------------------------------------------------------------

(defun default-error-handler (display error-key &rest key-vals
                                      &key asynchronous &allow-other-keys)
  (declare (type generalized-boolean asynchronous)
	   (dynamic-extent key-vals))
  ;; The default display-error-handler.
  ;; It signals the conditions listed in the DISPLAY file.
  (if asynchronous
      (apply #'x-cerror "Ignore" error-key :display display :error-key error-key key-vals)
      (apply #'x-error error-key :display display :error-key error-key key-vals)))

(defun x-error (condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'error condition keyargs))

(defun x-cerror (proceed-format-string condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'cerror proceed-format-string condition keyargs))

;;; X-ERROR for CMU Common Lisp
;;;
;;; We detect a couple condition types for which we disable event handling in
;;; our system.  This prevents going into the debugger or returning to a
;;; command prompt with CLX repeatedly seeing the same condition.  This occurs
;;; because CMU Common Lisp provides for all events (that is, X, input on file
;;; descriptors, Mach messages, etc.) to come through one routine anyone can
;;; use to wait for input.
;;;
#+(and CMU (not mp))
(defun x-error (condition &rest keyargs)
  (let ((condx (apply #'make-condition condition keyargs)))
    (when (eq condition 'closed-display)
      (let ((disp (closed-display-display condx)))
	(warn "Disabled event handling on ~S." disp)
	(ext::disable-clx-event-handling disp)))
    (error condx)))


(define-condition x-error (error) ())



;;-----------------------------------------------------------------------------
;;  HOST hacking
;;-----------------------------------------------------------------------------

(defun host-address (host &optional (family :internet))
  "Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  and cdr is a list of network address bytes."
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (labels ((no-host-error ()
	     (error "Unknown host ~S" host))
	   (no-address-error ()
	     (error "Host ~S has no ~S address" host family)))

    (let ((hostent (posix::resolve-host-ipaddr (string host))))
      (when (not (posix::hostent-addr-list hostent))
	(no-host-error))
      (ecase family
	((:internet nil 0)
	 (unless (= (posix::hostent-addrtype hostent) 2)
	   (no-address-error))
	 (let ((addr (first (posix::hostent-addr-list hostent))))
	   (etypecase addr
	     (integer
              (list :internet
                    (ldb (byte 8 24) addr)
                    (ldb (byte 8 16) addr)
                    (ldb (byte 8  8) addr)
                    (ldb (byte 8  0) addr)))
 	     (string
              (let ((parts (read-from-string
                            (nsubstitute #\Space #\. (ext:string-concat
                                                      "(" addr ")")))))
                (check-type parts (cons (unsigned-byte 8)
                                        (cons (unsigned-byte 8)
                                              (cons (unsigned-byte 8)
                                                    (cons (unsigned-byte 8)
                                                          NULL)))))
		(cons :internet parts))))))))))


;;-----------------------------------------------------------------------------
;; Whether to use closures for requests or not.
;;-----------------------------------------------------------------------------

;;; If this macro expands to non-NIL, then request and locking code is
;;; compiled in a much more compact format, as the common code is shared, and
;;; the specific code is built into a closure that is funcalled by the shared
;;; code.  If your compiler makes efficient use of closures then you probably
;;; want to make this expand to T, as it makes the code more compact.

(defmacro use-closures ()
  nil)

(defun clx-macroexpand (form env)
  (macroexpand form env))


;;-----------------------------------------------------------------------------
;; Resource stuff
;;-----------------------------------------------------------------------------


;;; Utilities

(defun get-host-name ()
  "Return the same hostname as gethostname(3) would"
  ;; machine-instance probably works on a lot of lisps
  #+(or cmu sbcl) (machine-instance)
  ;; clisp's machine-instance returns also the IP address; don't need it here.
  #+clisp (let ((s (machine-instance))) (subseq s 0 (position #\Space s)))
  ;; resources-pathname was using short-site-name for this purpose
  #+excl (short-site-name)
  #-(or cmu sbcl clisp excl) (error "get-host-name not implemented"))

(defun homedir-file-pathname (name)
  (and #-(or unix mach) (search "Unix" (software-type) :test #'char-equal)
       (merge-pathnames
        (translate-logical-pathname
         (user-homedir-pathname))
        (pathname name))))

;;; DEFAULT-RESOURCES-PATHNAME - The pathname of the resources file to load if
;;; a resource manager isn't running.

(defun default-resources-pathname ()
  (homedir-file-pathname ".Xdefaults"))

;;; RESOURCES-PATHNAME - The pathname of the resources file to load after the
;;; defaults have been loaded.

(defun resources-pathname ()
  (or (let ((string (getenv "XENVIRONMENT")))
	(and string
	     (pathname string)))
      (homedir-file-pathname (concatenate 'string ".Xdefaults-" (get-host-name)))))

;;; AUTHORITY-PATHNAME - The pathname of the authority file.

(defun authority-pathname ()
  (or (let ((xauthority (getenv "XAUTHORITY")))
	(and xauthority
	     (pathname xauthority)))
      (homedir-file-pathname ".Xauthority")))

;;; this particular defaulting behaviour is typical to most Unices, I think
;; #+unix PVE: Well, cygwin?

(defun get-default-display ()
  "Get the default X display as list of (host display-number screen protocol).
In UNIX this is selected using the DISPLAY environment variable, and
may use :internet or :local protocol"
  (let* ((name (or (getenv "DISPLAY")
		   (error "DISPLAY environment variable is not set")))
	 (colon-i (position #\: name))
	 (host (subseq name 0 colon-i))
	 (dot-i (and colon-i (position #\. name :start colon-i)))
	 (display (if colon-i
		      (ignore-errors
			(parse-integer name :start (1+ colon-i) :end dot-i))))
	 (screen (if dot-i
		     (ignore-errors (parse-integer name :start (1+ dot-i)))))
	 (protocol (if (or (string= host "") (string-equal host "unix"))
		       :local
		       :internet)))
    (list host (or display 0) (or screen 0) protocol)))


;;-----------------------------------------------------------------------------
;; GC stuff
;;-----------------------------------------------------------------------------

(defun gc-cleanup ()
  (declare (special *event-free-list*
		    *pending-command-free-list*
		    *reply-buffer-free-lists*
		    *gcontext-local-state-cache*
		    *temp-gcontext-cache*))
  (setq *event-free-list* nil)
  (setq *pending-command-free-list* nil)
  (when (boundp '*reply-buffer-free-lists*)
    (fill *reply-buffer-free-lists* nil))
  (setq *gcontext-local-state-cache* nil)
  (setq *temp-gcontext-cache* nil)
  nil)



;;-----------------------------------------------------------------------------
;; DEFAULT-KEYSYM-TRANSLATE
;;-----------------------------------------------------------------------------

;;; If object is a character, char-bits are set from state.
;;;
;;; [the following isn't implemented (should it be?)]
;;; If object is a list, it is an alist with entries:
;;; (base-char [modifiers] [mask-modifiers])
;;; When MODIFIERS are specified, this character translation
;;; will only take effect when the specified modifiers are pressed.
;;; MASK-MODIFIERS can be used to specify a set of modifiers to ignore.
;;; When MASK-MODIFIERS is missing, all other modifiers are ignored.
;;; In ambiguous cases, the most specific translation is used.


(defun default-keysym-translate (display state object)
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (ignore display state)
	   (clx-values t))
  object)


;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

;;; Types

(deftype pixarray-1-element-type ()
  'bit)

(deftype pixarray-4-element-type ()
  '(unsigned-byte 4))

(deftype pixarray-8-element-type ()
  '(unsigned-byte 8))

(deftype pixarray-16-element-type ()
  '(unsigned-byte 16))

(deftype pixarray-24-element-type ()
  '(unsigned-byte 24))

(deftype pixarray-32-element-type ()
  '(unsigned-byte 32))

(deftype pixarray-1  ()
  '(simple-array pixarray-1-element-type (* *)))

(deftype pixarray-4  ()
  '(#+cmu simple-array #-cmu array pixarray-4-element-type (* *)))

(deftype pixarray-8  ()
  '(simple-array pixarray-8-element-type (* *)))

(deftype pixarray-16 ()
  '(simple-array pixarray-16-element-type (* *)))

(deftype pixarray-24 ()
  '(simple-array pixarray-24-element-type (* *)))

(deftype pixarray-32 ()
  '(simple-array pixarray-32-element-type (* *)))

(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))

(deftype bitmap ()
  'pixarray-1)

;;; WITH-UNDERLYING-SIMPLE-VECTOR

#+CMU
;;; We do *NOT* support viewing an array as having a different element type.
;;; Element-type is ignored.
;;;
(defmacro with-underlying-simple-vector
    ((variable element-type pixarray) &body body)
  (declare (ignore element-type))
  `(lisp::with-array-data ((,variable ,pixarray)
			   (start)
			   (end))
     (declare (ignore start end))
     ,@body))

;;; These are used to read and write pixels from and to CARD8s.

;;; READ-IMAGE-LOAD-BYTE is used to extract 1 and 4 bit pixels from CARD8s.

(defmacro read-image-load-byte (size position integer)
  (unless +image-bit-lsb-first-p+ (setq position (- 7 position)))
  `(the (unsigned-byte ,size)
     (ldb (byte ,size ,position)(the card8 ,integer))))

;;; READ-IMAGE-ASSEMBLE-BYTES is used to build 16, 24 and 32 bit pixels from
;;; the appropriate number of CARD8s.

(defmacro read-image-assemble-bytes (&rest bytes)
  (unless +image-byte-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it
	    `(dpb
	      (the card8 ,byte)
	      (byte 8 ,(incf count 8))
	      (the (unsigned-byte ,count) ,it))))
    `(the (unsigned-byte ,(* (length bytes) 8)) ,it)))


;;; WRITE-IMAGE-LOAD-BYTE is used to extract a CARD8 from a 16, 24 or 32 bit
;;; pixel.

(defmacro write-image-load-byte (position integer integer-size)
  integer-size
  (unless +image-byte-lsb-first-p+ (setq position (- integer-size 8 position)))
  `(the card8
     (ldb
      (byte 8 ,position)
      (the (unsigned-byte ,integer-size) ,integer))))

;;; WRITE-IMAGE-ASSEMBLE-BYTES is used to build a CARD8 from 1 or 4 bit
;;; pixels.

(defmacro write-image-assemble-bytes (&rest bytes)
  (unless +image-bit-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((size (floor 8 (length bytes)))
	(it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it `(dpb
		 (the (unsigned-byte ,size) ,byte)
		 (byte ,size ,(incf count size))
		 (the (unsigned-byte ,count) ,it))))
    `(the card8 ,it)))

;;; The following table gives the bit ordering within bytes (when accessed
;;; sequentially) for a scanline containing 32 bits, with bits numbered 0 to
;;; 31, where bit 0 should be leftmost on the display.  For a given byte
;;; labelled A-B, A is for the most significant bit of the byte, and B is
;;; for the least significant bit.
;;;
;;; legend:
;;; 	1   scanline-unit = 8
;;; 	2   scanline-unit = 16
;;; 	4   scanline-unit = 32
;;; 	M   byte-order = MostSignificant
;;; 	L   byte-order = LeastSignificant
;;; 	m   bit-order = MostSignificant
;;; 	l   bit-order = LeastSignificant
;;;
;;;
;;; format	ordering
;;;
;;; 1Mm	00-07 08-15 16-23 24-31
;;; 2Mm	00-07 08-15 16-23 24-31
;;; 4Mm	00-07 08-15 16-23 24-31
;;; 1Ml	07-00 15-08 23-16 31-24
;;; 2Ml	15-08 07-00 31-24 23-16
;;; 4Ml	31-24 23-16 15-08 07-00
;;; 1Lm	00-07 08-15 16-23 24-31
;;; 2Lm	08-15 00-07 24-31 16-23
;;; 4Lm	24-31 16-23 08-15 00-07
;;; 1Ll	07-00 15-08 23-16 31-24
;;; 2Ll	07-00 15-08 23-16 31-24
;;; 4Ll	07-00 15-08 23-16 31-24


;;; If you can write fast routines that can read and write pixarrays out of a
;;; buffer-bytes, do it!  It makes the image code a lot faster.  The
;;; FAST-READ-PIXARRAY, FAST-WRITE-PIXARRAY and FAST-COPY-PIXARRAY routines
;;; return T if they can do it, NIL if they can't.

;;; FAST-READ-PIXARRAY - fill part of a pixarray from a buffer of card8s

#+(or CMU)
(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height
                                          padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index* x 3))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y)))
	   ((index>= y height))
	(declare (type array-index start y))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y 0) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (setf (aref vector x)
		(read-image-assemble-bytes
                 (aref buffer-bbuf (index+ i 0))
                 (aref buffer-bbuf (index+ i 1))
                 (aref buffer-bbuf (index+ i 2))))))))
  t)

#+CMU
(defun pixarray-element-size (pixarray)
  (let ((eltype (array-element-type pixarray)))
    (cond ((eq eltype 'bit) 1)
	  ((and (consp eltype) (eq (first eltype) 'unsigned-byte))
	   (second eltype))
	  (t
	   (error "Invalid pixarray: ~S." pixarray)))))

#+CMU
;;; COPY-BIT-RECT  --  Internal
;;;
;;;    This is the classic BITBLT operation, copying a rectangular subarray
;;; from one array to another (but source and destination must not overlap.)
;;; Widths are specified in bits.  Neither array can have a non-zero
;;; displacement.  We allow extra random bit-offset to be thrown into the X.
;;;
(defun copy-bit-rect (source source-width sx sy dest dest-width dx dy
			     height width)
  (declare (type array-index source-width sx sy dest-width dx dy height width))
  #.(declare-buffun)
  (lisp::with-array-data ((sdata source)
                          (sstart)
                          (send))
    (declare (ignore send))
    (lisp::with-array-data ((ddata dest)
                            (dstart)
                            (dend))
      (declare (ignore dend))
      (assert (and (zerop sstart) (zerop dstart)))
      (do ((src-idx (index+ (* vm:vector-data-offset vm:word-bits)
                            sx (index* sy source-width))
                    (index+ src-idx source-width))
           (dest-idx (index+ (* vm:vector-data-offset vm:word-bits)
                             dx (index* dy dest-width))
                     (index+ dest-idx dest-width))
           (count height (1- count)))
          ((zerop count))
        (declare (type array-index src-idx dest-idx count))
        (kernel:bit-bash-copy sdata src-idx ddata dest-idx width)))))

#+CMU
(defun fast-read-pixarray-using-bitblt
    (bbuf boffset pixarray x y width height padded-bytes-per-line
          bits-per-pixel)
  (declare (type (array * 2) pixarray))
  #.(declare-buffun)
  (copy-bit-rect bbuf
		 (index* padded-bytes-per-line vm:byte-bits)
		 (index* boffset vm:byte-bits) 0
		 pixarray
		 (index* (array-dimension pixarray 1) bits-per-pixel)
		 x y
		 height
		 (index* width bits-per-pixel))
  t)

(defun fast-read-pixarray (bbuf boffset pixarray
                                x y width height padded-bytes-per-line
                                bits-per-pixel
                                unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset
		 padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
   (let ((function
          (or (and (index= (pixarray-element-size pixarray) bits-per-pixel)
                   #'fast-read-pixarray-using-bitblt)
              (and (index= bits-per-pixel 24)
                   #'fast-read-pixarray-24))))
     (when function
       (read-pixarray-internal
        bbuf boffset pixarray x y width height padded-bytes-per-line
        bits-per-pixel function
        unit byte-lsb-first-p bit-lsb-first-p
        +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+)))))

;;; FAST-WRITE-PIXARRAY - copy part of a pixarray into an array of CARD8s

#+(or CMU)
(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
                                           padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index y start))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y x) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (let ((pixel (aref vector x)))
	    (declare (type pixarray-24-element-type pixel))
	    (setf (aref buffer-bbuf (index+ i 0))
		  (write-image-load-byte 0 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 1))
		  (write-image-load-byte 8 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 2))
		  (write-image-load-byte 16 pixel 24)))))))
  t)

#+CMU
(defun fast-write-pixarray-using-bitblt
    (bbuf boffset pixarray x y width height padded-bytes-per-line
          bits-per-pixel)
  #.(declare-buffun)
  (copy-bit-rect pixarray
		 (index* (array-dimension pixarray 1) bits-per-pixel)
		 x y
		 bbuf
		 (index* padded-bytes-per-line vm:byte-bits)
		 (index* boffset vm:byte-bits) 0
		 height
		 (index* width bits-per-pixel))
  t)

(defun fast-write-pixarray (bbuf boffset pixarray x y width height
                                 padded-bytes-per-line bits-per-pixel
                                 unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
   (let ((function
          (or (and (index= (pixarray-element-size pixarray) bits-per-pixel)
                   #'fast-write-pixarray-using-bitblt)
              (and (index= bits-per-pixel 24)
                   #'fast-write-pixarray-24))))
     (when function
       (write-pixarray-internal
        bbuf boffset pixarray x y width height padded-bytes-per-line
        bits-per-pixel function
        +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+
        unit byte-lsb-first-p bit-lsb-first-p)))))

;;; FAST-COPY-PIXARRAY - copy part of a pixarray into another

(defun fast-copy-pixarray (pixarray copy x y width height bits-per-pixel)
  (declare (type pixarray pixarray copy)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel))
  (progn pixarray copy x y width height bits-per-pixel nil)
  (let* ((pixarray-padded-pixels-per-line
	  (array-dimension pixarray 1))
	 (pixarray-padded-bits-per-line
	  (* pixarray-padded-pixels-per-line bits-per-pixel))
	 (copy-padded-pixels-per-line
	  (array-dimension copy 1))
	 (copy-padded-bits-per-line
	  (* copy-padded-pixels-per-line bits-per-pixel)))
    (when (index= (pixarray-element-size pixarray)
		  (pixarray-element-size copy)
		  bits-per-pixel)
      (copy-bit-rect pixarray pixarray-padded-bits-per-line x y
		     copy copy-padded-bits-per-line 0 0
		     height
		     (index* width bits-per-pixel))
      t)))
