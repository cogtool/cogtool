;
; CLISP FastCGI interface
;
; Copyright (C) 2003 Alma Mater Software, Inc., Tarrytown, NY, USA
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 2 as
; published by the Free Software Foundation; see file GNU-GPL.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;
; $Id: fastcgi.lisp,v 1.6 2005/11/17 23:09:56 sds Exp $

(defpackage "FASTCGI"
  (:documentation "Minimal bindings for FastCGI use from CLISP")
  (:use "LISP" "FFI")
  (:export "GETENV"
	   "SLURP-STDIN" "OUT" "WRITE-STDOUT" "WRITE-STDERR" "NL"
	   "IS-CGI" "ACCEPT" "FINISH" "WITH-LISTENER")
)

(in-package "FASTCGI")
(pushnew :fastcgi *features*)

(setf (documentation (find-package "FASTCGI") 'sys::impnotes) "fastcgi")

(FFI:default-foreign-language :STDC)

; Global: is request active?
(defvar *fastcgi-request-active* nil)

; -----------   Exported functions

; IS-CGI
(defun is-cgi ()
  "Return T iff this is an old-fashioned CGI request rather than FastCGI mode."
  (not (= 0 (fcgi_is_cgi_wrapper))))

; ACCEPT
(defun accept ()
  "Place at the top of an explicit FastCGI server loop.  Returns T iff there is a request to do."
  (setf *fastcgi-request-active* t)
  (= 0 (fcgi_accept_wrapper)))

; FINISH
(defun finish ()
  "Place at the bottom of an explicit FastCGI server loop.  Always returns NIL."
  (check-active-request "FINISH")
  (setf *fastcgi-request-active* nil)
  (fcgi_finish_wrapper)
  nil)

; GETENV
(defun getenv (var)
  "(FASTCGI::GETENV var)  Gets the value of an environment variable, which should be a string"
  (check-active-request "GETENV")
  (fcgi_getenv (to-string var)))

; WRITE-STDOUT
(defun write-stdout (data)
  "(FASTCGI::WRITE-STDOUT string) - Write a string to standard output"
  ;; Do it in chunks since there seems to be FFI problems with large buffers
  (do* ((chunksize 65536)
		(s (to-string data))
		(totlen (length s)))
	   ((= 0 (length s)) totlen)
	   (let ((to-write (min (length s) chunksize)))
		 (fcgi_write_stdout (subseq s 0 to-write) to-write)
		 (setf s (subseq s to-write)))))

; WRITE-STDERR
(defun write-stderr (data)
  "(FASTCGI::WRITE-STDERR string) - Write a string to standard error"
  (let ((s (to-string data)))
    (fcgi_write_stderr s (length s))))

; SLURP-STDIN
(defun slurp-stdin ()
  "(FASTCGI::SLURP-STDIN)  Reads in the entirety of standard input and returns as a string"
  (check-active-request "SLURP-STDIN")
  (do ((result "")
       (eof nil))
      (eof result)
      (let ((buf (fcgi_read_stdin)))
	(if (= 0 (length buf))
	    (setf eof t)
	  (setf result (concatenate 'string result buf))))))

; Output functions

; OUT
(defun out (&rest args)
  "(FASTCGI::OUT args ...) Write arguments to standard output"
  (write-stdout (cat args)))

; NL
; Return newline
(defun nl () "Return a newline" (format nil "~%"))


; ----------------    Internal functions

; CAT
; Concatenate strings
(defun cat (&rest args)
  (apply #'concatenate 'string (mapcar #'to-string (flatten args))))

; FLATTEN
; Flatten list (lifted from Paul Graham)
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

; TO-STRING
; Convert object to a string; NIL -> ""
(defun to-string (s)
  (cond ((null s) "")
        ((stringp s) s)
        ((symbolp s) (symbol-name s))
        (t (format nil "~A" s))))

; CHECK-ACTIVE-REQUEST - Sanity check on use of library function
(defun check-active-request (func)
  (when (not *fastcgi-request-active*)
	(error "You must call FASTCGI:ACCEPT before using any other FastCGI function")))



; --------------   "C" functions
(c-lines "#include \"fastcgi.h\"~%")

; Our wrappers
(def-call-out fcgi_getenv       (:arguments (var c-string))               (:return-type c-string))
(def-call-out fcgi_read_stdin   (:arguments)                              (:return-type c-string))
(def-call-out fcgi_write_stdout (:arguments (data c-string) (length int)) (:return-type int))
(def-call-out fcgi_write_stderr (:arguments (data c-string) (length int)) (:return-type int))

; Direct passthroughs to FCGI library
(def-call-out fcgi_accept_wrapper (:arguments) (:return-type int))
(def-call-out fcgi_finish_wrapper (:arguments) (:return-type nil))
(def-call-out fcgi_is_cgi_wrapper (:arguments) (:return-type int))

; End of fastcgi.lisp
