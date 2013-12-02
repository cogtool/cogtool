;; -*- Lisp -*-
;; some tests for RAWSOCK
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "rawsock/test")'
;; relies on some functions in the syscalls module

(listp (show (multiple-value-list (ext:module-info "rawsock" t)) :pretty t)) T

(progn
  (defun to-bytes (string) (ext:convert-string-to-bytes string charset:ascii))
  (defun from-bytes (vec &optional size)
    (ext:convert-string-from-bytes vec charset:ascii :end size))
  (defun make-byte-vector (len)
    (make-array (etypecase len
                  (integer len)
                  (sequence (length len)))
                :element-type '(unsigned-byte 8) :initial-element 0))
  (defun host->sa (host &optional (port 0))
    (let* ((he (posix:resolve-host-ipaddr host)) sa
           (ip (first (posix:hostent-addr-list he)))
           (type (posix:hostent-addrtype he))
           (li (read-from-string
                (concatenate 'string "(" (substitute #\Space #\. ip) ")")))
           (ve (make-byte-vector (nth-value 1 (rawsock::sockaddr-slot :data)))))
      (show he :pretty t)
      (setf port (rawsock:htons port)
            (aref ve 0) (ldb #.(byte 8 0) port)
            (aref ve 1) (ldb #.(byte 8 8) port))
      (replace ve li :start1 2)
      (show ve)
      (setq sa (show (rawsock:make-sockaddr type ve)))
      (assert (equalp ve (rawsock:sockaddr-data sa)))
      (show (list 'rawsock:sockaddr-family
                  (multiple-value-list (rawsock:sockaddr-family sa))))
      (show (mapcar (lambda (addr)
                      (let* ((numeric (rawsock:convert-address type addr))
                             (dotted (rawsock:convert-address type numeric)))
                        (show (list :address addr numeric dotted))
                        (assert (string= addr dotted))))
                    (posix:hostent-addr-list he)))
      sa))
  (defun local-sa-check (sock sa-local)
    (let* ((sa (rawsock:getsockname sock T))
           (data (rawsock:sockaddr-data sa)))
      (show sa)
      (show (list 'port (+ (aref data 1) (ash (aref data 0) 8))))
      (and (= (rawsock:sockaddr-family sa) (rawsock:sockaddr-family sa-local))
           (equalp (subseq data 2)
                   (subseq (rawsock:sockaddr-data sa-local) 2)))))
  (dolist (what '(nil :data :family))
    (show (cons (list 'rawsock::sockaddr-slot what)
                (multiple-value-list (rawsock::sockaddr-slot what)))))
  (defvar *sa-remote*) (defvar *sa-local*)
  (defvar *buffer* (make-byte-vector 1024))
  (defvar *sock*) (defvar *sock1*) (defvar *sock2*)
  (defvar *recv-ret*) (defvar *recvfrom-ret*) #-:win32 (defvar *read-ret*)
  (defun my-recvfrom (so ve sa &optional (status :output) &aux size)
    (when (socket:socket-status (cons so :input) 1)
      (multiple-value-bind (len sa-len sa1) (rawsock:recvfrom so ve sa)
        (assert (eq sa sa1))
        (setq size len)
        (show (list len sa-len sa (subseq ve 0 len)) :pretty t)))
    (assert (eq status (show (socket:socket-status so))))
    (rawsock:sock-close *sock*)
    size)
  T) T

(progn (setq *sa-remote* (host->sa "ftp.gnu.org" 21)) T) T
(progn (setq *sa-local* (host->sa :default)) T) T

(catch 'type-error-handler
  (handler-bind ((type-error #'type-error-handler))
    (rawsock:socket :INET :FOO nil)))
NIL
(catch 'type-error-handler
  (handler-bind ((type-error #'type-error-handler))
    (rawsock:socket :FOO :STREAM nil)))
NIL

(integerp (show (setq *sock* (rawsock:socket :INET :STREAM nil)))) T

(unless (equalp #(127 0 0 1) (subseq (rawsock:sockaddr-data *sa-local*) 2 6))
  (rawsock:bind *sock* *sa-local*)
  (not (local-sa-check *sock* *sa-local*)))
NIL
(rawsock:connect *sock* *sa-remote*) NIL
(equalp (rawsock:getpeername *sock* T) *sa-remote*) T

(listp (show (list (multiple-value-list (socket:socket-stream-local *sock*))
                   (multiple-value-list (socket:socket-stream-peer *sock*)))))
T

(ext:socket-status (list (cons *sock* :input))) (:INPUT)

(let ((size (rawsock:recv *sock* *buffer*)))
  (show (setq *recv-ret* (list size (from-bytes *buffer* size))))
  (ext:socket-status *sock*))
:OUTPUT

#+unix (listp (show (rawsock:socket-option *sock* NIL) :pretty t)) T
#+unix (listp (show (rawsock:socket-option *sock* NIL :level :ALL) :pretty t))T

#+unix (setf (rawsock:socket-option *sock* NIL) '(:debug nil))
#+unix (:DEBUG NIL)

#+unix (setf (rawsock:socket-option *sock* NIL :level :all)
             '(:sol-socket (:debug nil)))
#+unix (:SOL-SOCKET (:DEBUG NIL))

#+unix (setf (rawsock:socket-option *sock* :debug) nil)
#+unix NIL

#+unix (setf (rawsock:socket-option *sock* :debug :level :all)
             '(:sol-socket nil))
#+unix (:SOL-SOCKET NIL)

(ext:socket-status *sock*) :OUTPUT
(ext:socket-stream-shutdown *sock* :io) NIL
(rawsock:sock-close *sock*) 0

;; re-create the socket after it has been closed
(let ((so (rawsock:socket :INET :STREAM nil)))
  (show (list so *sock*))
  (= so *sock*))
T

(unless (equalp #(127 0 0 1) (subseq (rawsock:sockaddr-data *sa-local*) 2 6))
  (rawsock:bind *sock* *sa-local*)
  (not (local-sa-check *sock* *sa-local*)))
NIL
(rawsock:connect *sock* *sa-remote*) NIL
(equalp (rawsock:getpeername *sock* T) *sa-remote*) T

(ext:socket-status (list (cons *sock* :input))) (:INPUT)

#-:win32 ;; on win32, read() cannot be called on a socket!
(let ((size (rawsock:sock-read *sock* *buffer*)))
  (show (setq *read-ret* (list size (from-bytes *buffer* size))))
  (ext:socket-status *sock*))
#-:win32 :OUTPUT

#-:win32 (equal *recv-ret* *read-ret*) #-:win32 T

(rawsock:sock-close *sock*) 0

;; re-create the socket after it has been closed
(let ((so (rawsock:socket :INET :STREAM nil)))
  (show (list so *sock*))
  (= so *sock*))
T

(unless (equalp #(127 0 0 1) (subseq (rawsock:sockaddr-data *sa-local*) 2 6))
  (rawsock:bind *sock* *sa-local*)
  (not (local-sa-check *sock* *sa-local*)))
NIL
(rawsock:connect *sock* *sa-remote*) NIL

(let ((size (my-recvfrom *sock* *buffer* *sa-remote*)))
  (show (setq *recvfrom-ret* (list size (from-bytes *buffer* size))))
  (equal *recv-ret* *recvfrom-ret*))
T

(progn
  (setf (values *sock1* *sock2*)
        ;; :INET works on cygwin but not on Linux
        (rawsock:socketpair #+:win32 :INET #-:win32 :UNIX :STREAM nil))
  (show `((,(rawsock:getpeername *sock1* T) ,(rawsock:getsockname *sock1* T)
           ,*sock1*)
          (,(rawsock:getpeername *sock2* T) ,(rawsock:getsockname *sock2* T)
           ,*sock2*)
          ,(multiple-value-list (socket:socket-stream-local *sock1*))
          ,(multiple-value-list (socket:socket-stream-peer *sock1*))
          ,(multiple-value-list (socket:socket-stream-local *sock2*))
          ,(multiple-value-list (socket:socket-stream-peer *sock2*)))
        :pretty t)
  T) T

(let ((message "abazonk"))
  (rawsock:sock-write *sock1* (to-bytes message))
  (string= message (from-bytes *buffer* (rawsock:sock-read
                                         *sock2* *buffer*))))
T

#-:win32 ;; on win32, read()/write() cannot be called on a socket!
(let* ((message '("I" "love" "you"))
       (char-num (reduce #'+ message :key #'length))
       (buf1 (map 'vector #'to-bytes message))
       (buf2 (map 'vector #'make-byte-vector message)))
  (show (list buf1 buf2))
  ;; assume ASCII-compatible encoding
  (assert (= char-num (rawsock:sock-write *sock1* buf1)))
  (assert (= char-num (rawsock:sock-read *sock2* buf2)))
  (list (equalp buf1 buf2)
        (equalp message (map 'list #'from-bytes buf2))))
#-:win32 (T T)

(list (ext:socket-status *sock1*) (ext:socket-status *sock2*))
(:OUTPUT :OUTPUT)

(list (rawsock:sock-close *sock1*) (rawsock:sock-close *sock2*)) (0 0)

;; lisp implementation of socketpair
(progn (setq *sock1* (rawsock:socket :INET :STREAM nil)
             *sock2* (rawsock:socket :INET :STREAM nil)
             *sa-local* (host->sa :default))
       (rawsock:bind *sock2* *sa-local*)
       (rawsock:sock-listen *sock2* 1)
       ;; figure out what port was assigned:
       (rawsock:getsockname *sock2* *sa-local*)
       NIL)
NIL

(socket:socket-status *sock2* 0) NIL

(rawsock:connect *sock1* *sa-local*) NIL

(socket:socket-status *sock2*) :INPUT

(progn (setq *sock* (rawsock:accept *sock2* *sa-local*))
       (socket:socket-status *sock1*))
:OUTPUT
(socket:socket-status *sock*) :OUTPUT

(rawsock:send *sock* (to-bytes "dog bites man")) 13
(rawsock:recv *sock1* *buffer*) 13
(from-bytes *buffer* 13) "dog bites man"

(rawsock:send *sock1* (to-bytes "man bites dog")) 13
(rawsock:recv *sock* *buffer*) 13
(from-bytes *buffer* 13) "man bites dog"

(rawsock:sock-close *sock*)  0
(rawsock:sock-close *sock1*) 0
(rawsock:sock-close *sock2*) 0

;; message
(when (and (fboundp 'rawsock:sendmsg) (fboundp 'rawsock:recvmsg))
  (let* ((message '("man" "bites" "dog"))
         (len (reduce #'+ message :key #'length))
         (message1
          (rawsock:make-message :addr *sa-local*
                                :iovec (map 'vector #'to-bytes message)))
         (message2
          (rawsock:make-message :addr (rawsock:make-sockaddr :inet)
                                :iovec (map 'vector #'make-byte-vector
                                            message))))
    (show (list :before message1 message2) :pretty t)
    ;; new connectionless-mode sockets
    (setq *sock1* (rawsock:socket :INET :DGRAM nil)
          *sock2* (rawsock:socket :INET :DGRAM nil))
    (rawsock:bind *sock2* *sa-local*)
    (assert (= len (rawsock:sendmsg *sock1* message1)))
    (assert (= len (rawsock:recvmsg *sock2* message2)))
    (show (list :after message1 message2) :pretty t)
    (assert (equalp (rawsock:message-iovec message1)
                    (rawsock:message-iovec message2)))
    (when (fboundp 'rawsock:getnameinfo)
      (show (list 'rawsock:getnameinfo 
                  (multiple-value-list
                   (rawsock:getnameinfo (rawsock:message-addr message1)))
                  (multiple-value-list
                   (rawsock:getnameinfo (rawsock:message-addr message2))))))
    ;; I get "(EFAULT): Bad address" on Linux 2.6.14-1.1637_FC4
    ;; (when (fboundp 'rawsock:sockatmark) 
    ;;   (show (list 'rawsock:sockatmark 
    ;;               (rawsock:sockatmark *sock1*)
    ;;               (rawsock:sockatmark *sock2*))))
    (rawsock:sock-close *sock1*) (rawsock:sock-close *sock2*))
  nil)
NIL

#-win32 (rawsock:sock-write 1 (to-bytes "foo")) #-win32 3

;;;; root only??
(integerp (show (setq *sock* (rawsock:socket :INET :DGRAM 0)))) T

(progn
  (setq *sa-remote*
        (rawsock:make-sockaddr
         :inet '(101 116 104 48 0 0 0 0 0 0 0 0 0 0 0 0)))
  (show *sa-remote*)
  ;;(show (posix:resolve-host-ipaddr
  ;;       (make-array 4 :element-type '(unsigned-byte 8)
  ;;                   :displaced-to (rawsock:sockaddr-data *sa-remote*))))
  (fill *buffer* 0)
  (loop :for (x y) :in '((12 8)(14 #x45) (17 40)(22 #x40)(23 6)(26 10)(29 10))
    :do (setf (aref *buffer* x) y))
  (rawsock:ipcsum *buffer*))
51056

(rawsock:sendto *sock* *buffer* *sa-remote*)
1024

(rawsock:sock-close *sock*) 0

(or (not (fboundp 'rawsock:protocol))
    (rawsock:protocol-p (show (rawsock:protocol "IP") :pretty t))) T
(or (not (fboundp 'rawsock:protocol))
    (listp (show (rawsock:protocol) :pretty t))) T
(or (not (fboundp 'rawsock:network))
    (listp (show (rawsock:network) :pretty t))) T

(or (not (fboundp 'rawsock:getaddrinfo))
    (listp (show (rawsock:getaddrinfo :node "localhost") :pretty t))) T
(or (not (fboundp 'rawsock:getaddrinfo))
    (listp (show (rawsock:getaddrinfo :service "21") :pretty t))) T
(or (not (fboundp 'rawsock:getaddrinfo))
    (listp (show (rawsock:getaddrinfo :service "www") :pretty t))) T

(or (not (fboundp 'rawsock:getnameinfo))
    (listp (show (multiple-value-list (rawsock:getnameinfo *sa-remote*))))) T
(or (not (fboundp 'rawsock:getnameinfo))
    (listp (show (multiple-value-list (rawsock:getnameinfo *sa-local*))))) T

#+unix                          ; for Don Cohen
(when (and (string-equal (posix:uname-sysname (posix:uname)) "linux")
           (zerop (posix:getuid))) ; root?
  (show (setq *sock* (rawsock:socket :INET :PACKET 3)))
  (show (setq *sa-local* (rawsock:make-sockaddr :PACKET)))
  (my-recvfrom *sock* *buffer* *sa-local*)
  nil)
#+unix NIL

#+unix           ; http://article.gmane.org/gmane.lisp.clisp.devel:14852
(when (and (string-equal (posix:uname-sysname (posix:uname)) "linux")
           (zerop (posix:getuid))) ; root?
  (show (setq *sock* (rawsock:socket :INET :RAW :IPPROTO-ICMP)))
  (shell "ping -c 1 localhost") ; generate one icmp packet
  (show (setq *sa-local* (rawsock:make-sockaddr :PACKET 20)))
  (my-recvfrom *sock* *buffer* *sa-local* :IO)
  nil)
#+unix NIL

;; http://article.gmane.org/gmane.lisp.clisp.devel:14865
(progn
  (show (list '*sa-local* (setq *sa-local* (host->sa :default 7777))) :pretty t)
  (show (list '*sock* (setq *sock* (rawsock:socket :INET :DGRAM nil))))
  (rawsock:bind *sock* *sa-local*)
  (loop :for i :below 256 :do (setf (aref *buffer* i) i))
  (rawsock:sendto *sock* *buffer* *sa-local* :end 256))
256

(ext:socket-status (list (cons *sock* :input))) (:INPUT)

(let* ((buf (make-byte-vector 256))
       (sa1 (rawsock:make-sockaddr 0))
       (len (my-recvfrom *sock* buf sa1)))
  (assert (equalp sa1 *sa-local*))
  (loop :for i :below 256 :do (assert (= (aref buf i) i)))
  len)
256
