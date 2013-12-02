;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : system-parameters.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions for setting and accessing system parameters.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2010.11.04 Dan
;;;             : Creation.
;;; 2010.11.16 Dan
;;;             : * Added the simple-system-param-handler macro which creates
;;;             :   a function to set or return a global variable.
;;;             : * Can't use command-output since there may not be a current
;;;             :   model.  Instead always print to *standard-output* when
;;;             :   no parameters provided and don't print details when a
;;;             :   particular parameter is given.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; System parameters are for controling things not specifically tied to a 
;;; module instance in a model i.e. they would affect all models/meta-processes 
;;; and may even do something in the absence of a model.
;;;
;;; Basically, this allows a clean way to modify things that are typically just
;;; a globabl variable which is to be set. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; ssp and ssp-fct
;;;
;;; Works just like sgp except that it only covers the system parameters except
;;; that it only prints the details when no parameters are provided.
;;;
;;; create-system-parameter
;;;
;;; Works like define-parameter except that there is no ownership option (only
;;; the creator gets notifications) and in addition to creating the parameter
;;; one also needs to specify the handler function for setting/getting the value.
;;; The handler function will get two parameters.  The first is whether it is a
;;; set or get operation (t means set and nil means get).  If the first is t then
;;; the second will be the new value, and if the first is nil the second will 
;;; also be nil.  The return value from the handler is what will be returned from 
;;; ssp for that parameter's position.  Create-system-parameter will return the
;;; parameter name if it has been successfully created or nil if it failed.
;;;
;;; remove-system-parameter
;;;
;;; Takes the named parameter out of the system parameter set.  Necessary for
;;; redefinition since create-system-parameter will not overwrite an existing
;;; parameter.  Returns t if successfully removed or nil if there was no parameter
;;; with that name.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Provides some protection and control over setting things instead of just 
;;; having users change system variables directly.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Holds all the system parameters that have been created.

(defvar *act-r-system-parameters-table* (make-hash-table :test #'eq)
  "The table of all system parameters")

(defstruct (act-r-system-parameter (:include act-r-parameter (owner t)))
  handler)
                                   

(defun create-system-parameter (param-name &key (valid-test nil) (default-value nil) (warning "")
                                (documentation "")(handler nil))
  (cond ((not (keywordp param-name))
         (print-warning "Parameter name must be a keyword."))
        ((keywordp default-value)
         (print-warning "default-value cannot be a keyword."))
        ((not (fctornil valid-test))
         (print-warning "valid-test must be a function, the name of a function, or nil."))
        ((not (stringp documentation))
         (print-warning "documentation must be a string."))
        ((not (stringp warning))
         (print-warning "warning must be a string."))
        ((not (and handler (fctornil handler)))
         (print-warning "a handler function must be provided."))
        ((gethash param-name *act-r-system-parameters-table*)
         (print-warning "a system parameter with the name ~s already exists." param-name))
        ((and valid-test default-value (not (funcall valid-test default-value)))
         (print-warning "default-value does not pass the valid-test"))
        (t
         (let ((param (make-act-r-system-parameter :param-name param-name
                                                   :default default-value
                                                   :test valid-test
                                                   :warning warning
                                                   :details documentation
                                                   :handler handler)))
           (setf (gethash param-name *act-r-system-parameters-table*) param)
           (set-system-parameter-value param default-value)
           param-name))))



(defun remove-system-parameter (param-name)
  "Remove a system parameter"
  (when (gethash param-name *act-r-system-parameters-table*)
    (remhash param-name *act-r-system-parameters-table*)
    t))
 
(defmacro ssp (&rest parameters)
  `(ssp-fct ',parameters))

(defun ssp-fct (&optional (parameters-list nil))
  (if (null parameters-list)
      (show-all-system-parameters)
    (if (every #'keywordp parameters-list)
        (get-system-parameters parameters-list)
      (set-system-parameters parameters-list))))

(defun get-system-parameters (params)
  (let ((res nil))
    (dolist (p-name params (reverse res))
      (if (valid-system-parameter-name p-name)
          (let* ((param (get-system-parameter-struct p-name))
                 (val (funcall (act-r-system-parameter-handler param) nil nil)))
            (push val res))
        (push :bad-parameter-name res)))))


(defun get-system-parameter-struct (p-name)
  (gethash p-name *act-r-system-parameters-table*))

(defun valid-system-parameter-name (p-name)
  (gethash p-name *act-r-system-parameters-table*))


(defun set-system-parameters (params)
  (if (evenp (length params))
      (let ((res nil))
        (while params
          (let ((p-name (pop params))
                (p-val (pop params)))
            (push-last (test-and-set-system-parameter-value p-name p-val) res)))
        res)
    (print-warning "Odd number of parameters and values passed to ssp.")))


(defun test-and-set-system-parameter-value (p-name value)
  (let ((param (get-system-parameter-struct p-name)))
    (if param
        (if (or (null (act-r-system-parameter-test param))
                (funcall (act-r-system-parameter-test param) value))
            (set-system-parameter-value param value)
          (progn
            (print-warning "System parameter ~S cannot take value ~A because it must be ~A."
                           p-name value (act-r-parameter-warning param))
            :invalid-value))
      (progn
        (print-warning "Parameter ~s is not the name of an available system parameter" p-name)
        :bad-parameter-name))))

(defun set-system-parameter-value (param value)
  (funcall (act-r-system-parameter-handler param) t value))


(defun show-all-system-parameters ()
  (let ((current-val-table (make-hash-table :test 'eq))
        (name-len 0)
        (default-len 0)
        (value-len 0))
    (maphash #'(lambda (p-name param)
                 (let ((val (funcall (act-r-system-parameter-handler param) nil nil)))
                   
                   (when (> (length (string p-name)) name-len)
                     (setf name-len (length (string p-name))))
                   (when (> (length (format nil "~s" (act-r-system-parameter-default param))) default-len)
                     (setf default-len (length (format nil "~s" (act-r-system-parameter-default param)))))
                   (when (> (length (format nil "~s" val)) value-len)
                     (setf value-len (length (format nil "~s" val))))
                   (setf (gethash (cons p-name val) current-val-table) param)))
                   
             *act-r-system-parameters-table*)
    
    (maphash #'(lambda (param value)
                   
                 (format t "~vS ~vS default: ~vS : ~A~%"
                   (1+ name-len)
                   (car param)
                   value-len
                   (cdr param)
                   default-len
                   (act-r-system-parameter-default value)
                   (act-r-system-parameter-details value)))
             current-val-table)))


(defmacro simple-system-param-handler (var-name)
  `(lambda (set-or-get value)
     (if set-or-get
         (setf ,var-name value)
       ,var-name)))

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
