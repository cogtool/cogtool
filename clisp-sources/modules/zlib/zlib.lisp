;;; zlib interface
;;; <http://www.gzip.org/zlib>, <http://www.zlib.org>
;;;
;;; Copyright (C) 2004 by Joerg Hoehle
;;; Copyright (C) 2004-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "ZLIB" (:use "CL" "EXT" "FFI"))
(in-package "ZLIB")
(export '(compress uncompress compress-bound error-string z-version
          zerror zerror-caller zerror-errno))
(setf (documentation (find-package "ZLIB") 'sys::impnotes) "zlib")

;;; types and constants

;;; foreign function definitions
(default-foreign-language :stdc)

(c-lines "#include <zlib.h>~%")

(def-call-out z-version (:name "zlibVersion")
  (:arguments) (:return-type c-string))

(def-call-out compress-bound (:name "compressBound")
  (:arguments (sourceLen ulong)) (:return-type ulong))

(def-call-out error-string (:name "zError")
  (:arguments (errno int)) (:return-type c-string))

(def-call-out %compress (:name "compress2")
  (:arguments (dest c-pointer :in)
              (destlen (c-ptr ulong) :in-out)
              (source (c-array-ptr uint8))
              (sourcelen ulong)
              (level int))
  (:return-type int))

(def-call-out %uncompress (:name "uncompress")
  (:arguments (dest c-pointer :in)
              (destlen (c-ptr ulong) :in-out)
              (source (c-array-ptr uint8))
              (sourcelen ulong))
  (:return-type int))

;;; errors
(define-condition zerror (error)
  (($errno :type integer :reader zerror-errno :initarg :errno)
   ($caller :type (or symbol (cons (eql setf) (cons symbol null)))
            :reader zerror-caller :initarg :caller))
  (:documentation "an error in a ZLIB library call")
  (:report (lambda (ze out)
             (let ((err (zerror-errno ze)))
               (format out "~S/~D: ~A" (zerror-caller ze) err
                       (error-string err))))))

;;; wrappers
(defun compress (source &key (level -1))
  "Compress the byte vector SOURCE into a new byte vector."
  (let* ((sourcelen (length source))
         (destlen (compress-bound sourcelen)))
    (with-c-var (dest `(c-array uint8 ,destlen))
      (multiple-value-bind (errno actual)
          (%compress (c-var-address dest) destlen source sourcelen level)
        (if (zerop errno)
            ;; CAST not usable because of different size...
            (offset dest 0 `(c-array uint8 ,actual))
            (error 'zerror :caller 'compress :errno errno))))))

(defun uncompress (source destlen)
  "Uncompress the byte vector SOURCE into a new byte vector of length DESTLEN."
  (let* ((sourcelen (length source)))
    (with-c-var (dest `(c-array uint8 ,destlen))
      (multiple-value-bind (errno actual)
          (%uncompress (c-var-address dest) destlen source sourcelen)
        (if (zerop errno)
            ;; CAST not usable because of different size...
            (offset dest 0 `(c-array uint8 ,actual))
            (error 'zerror :caller 'uncompress :errno errno))))))

(pushnew :zlib *features*)
(provide "zlib")
(pushnew "ZLIB" custom:*system-package-list* :test #'string=)
(setf (package-lock "ZLIB") t)
