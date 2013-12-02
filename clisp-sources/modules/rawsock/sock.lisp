;; Module for Raw Sockets / CLISP
;; Fred Cohen, 2003-2004
;; Don Cohen, 2003-2004
;; Sam Steingold 2004-2005
;; <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>

(defpackage #:rawsock
  (:documentation "Raw Socket access")
  (:use #:lisp)
  (:shadowing-import-from "EXPORTING" #:defun #:defstruct)
  (:export #:buffer #:resize-buffer #:accept #:bind #:connect
           #:getpeername #:getsockname #:protocol #:network #:message
           #:sock-listen #:recv #:recvfrom #:recvmsg
           #:send #:sendmsg #:sendto #:socket-option
           #:socket #:socketpair #:sockatmark #:getnameinfo #:getaddrinfo
           #:sock-read #:sock-write #:sock-close
           #:sockaddr #:make-sockaddr #:sockaddr-family #:sockaddr-p
           #:htonl #:htons #:ntohl #:ntohs #:convert-address
           #:configdev #:ipcsum #:icmpcsum #:tcpcsum #:udpcsum))

(in-package "RAWSOCK")
(pushnew :rawsock *features*)
(pushnew "RAWSOCK" custom:*system-package-list* :test #'string=)

(setf (documentation (find-package '#:rawsock) 'sys::impnotes) "rawsock")

(cl:defstruct (sockaddr (:constructor make-sa (%data)))
  (%data #() :read-only t :type (vector (unsigned-byte 8))))

(defstruct (message)
  (addr nil :type sockaddr) ; Optional address.
  (iovec #() :type (vector (vector (unsigned-byte 8)))) ; Scatter/gather array.
  (control #A((unsigned-byte 8) 0 nil) :type (vector (unsigned-byte 8)))
  (flags () :type list))        ; Flags on received message.

(defstruct (addinfo (:constructor make-addrinfo
                                  (flags family type protocol address name)))
  (flags nil :type list)
  (family 0 :type integer)
  (type 0 :type integer)
  (protocol 0 :type integer)
  (address nil :type (or null sockaddr))
  (name nil :type (or null string)))

(defstruct (protocol (:constructor make-protocol (name aliases proto)))
  (name "" :type string)
  (aliases nil :type list)
  (proto 0 :type integer))

(defstruct (network (:constructor make-network (name aliases type net)))
  (name "" :type string)
  (aliases nil :type list)
  (type 0 :type integer)
  (net 0 :type integer))

(defsetf socket-option (&rest args) (value) `(set-socket-option ,value ,@args))

(defun sockaddr-data (sa)
  (let ((%data (sockaddr-%data sa)) (offset #,(sockaddr-slot :data)))
    (make-array (- (length %data) offset) :displaced-to %data
                :displaced-index-offset offset
                :element-type '(unsigned-byte 8))))

(defun open-unix-socket (pathname &optional (type :SOCK_STREAM))
  "Return the socket (fixnum) pointing to this UNIX socket special device."
  (let* ((socket (socket :AF_UNIX type 0))
         (address (make-sockaddr :AF_UNIX
                                 (ext:convert-string-to-bytes
                                  (namestring (ext:absolute-pathname pathname))
                                  ext:*pathname-encoding*))))
    (connect socket address)
    (values socket address)))

(defun open-unix-socket-stream (pathname &rest opts &key (type :SOCK_STREAM)
                                &allow-other-keys)
  "Return the lisp STREAM pointing to this UNIX socket special device.
The return value is already FINALIZEd by CLOSE.
Passes :TYPE to SOCKET and all the other options to MAKE-STREAM."
  (multiple-value-bind (sock address) (open-unix-socket pathname type)
    (setq opts (ext:remove-plist opts :type))
    (let ((stream (apply #'ext:make-stream sock opts)))
      (ext:finalize stream #'close)
      (sock-close sock)
      (values stream address))))

(ext:without-package-lock ("CL")
(defmethod close ((sock integer) &key abort)
  (declare (ignore abort))
  (sock-close sock))
)

(defmethod describe-object ((addr sockaddr) (out stream))
  (call-next-method)
  (when (fboundp 'rawsock:getnameinfo)
    (multiple-value-bind (node service) (rawsock:getnameinfo addr)
      (format out "sockaddr node: ~S, service: ~S~%" node service))))
