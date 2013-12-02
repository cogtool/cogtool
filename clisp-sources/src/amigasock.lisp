;;; Sockets on Amiga
;;; This is not a complete implementation of the socket interface
;;; documented in impnotes.html, but sufficient for a simple HTTP server.

; Not yet implemented: socket-wait socket-stream-peer-host
; Difference w.r.t. Unix: On Unix, (socket-server port) already allows
; remote clients to connect and send us some data _before_ we do a
; socket-accept. This is not easily implementable on Amiga.

(in-package "SYSTEM")

(defstruct (socket-server
            (:constructor socket-server
                          (port)
            )
            (:copier nil)
           )
  (port 0 :type (unsigned-byte 16) :read-only t)
)

(defun test-socket-server (object caller)
  (unless (socket-server-p object)
    (error-of-type 'type-error
      :datum object :expected-type 'socket-server
      (TEXT "~S: ~S is not a SOCKET-SERVER")
      caller object)))

(defun socket-server-close (socket-server)
  (test-socket-server socket-server 'socket-server-close)
  (values)
)

(defun socket-accept (socket-server)
  (test-socket-server socket-server 'socket-accept)
  (let ((port (socket-server-port socket-server)))
    (open (format nil "TCP:~D" port) :direction :io)
) )


(defun socket-connect (port &optional (host "localhost"))
  (assert (typep port '(unsigned-byte 16)))
  (assert (typep host 'string))
  (open (format nil "TCP:~A/~D" host port) :direction :io)
)

(defun socket-service-port (service-name)
  (assert (typep service-name 'string))
  (cdr (assoc service-name
              ; Just the most important ones:
              '(("echo" . 7)
                ("discard" . 9)
                ("systat" . 11)
                ("daytime" . 13)
                ("netstat" . 15)
                ("chargen" . 19)
                ("ftp" . 21)
                ("telnet" . 23)
                ("smtp" . 25)
                ("time" . 37)
                ("nameserver" . 42)
                ("whois" . 43)
                ("domain" . 53)
                ("gopher" . 70)
                ("finger" . 79)
                ("www" . 80)
                ("http" . 80)
                ("pop2" . 109)
                ("pop3" . 110)
                ("nntp" . 119)
                ("irc" . 194)
                ("printer" . 515)
               )
              :test #'string=
) )    )

(defun test-socket-stream (stream)
  (assert (typep stream 'file-stream))
  (let ((name (pathname stream)))
    (assert (string-equal (pathname-device name) "TCP"))
    name
) )

(defun socket-stream-host (socket-stream)
  (let ((name (test-socket-stream socket-stream)))
    (second (pathname-directory name)) ; for a server socket, this is nil
) )

(defun socket-stream-port (socket-stream)
  (let ((name (test-socket-stream socket-stream)))
    (parse-integer (pathname-name name))
) )
