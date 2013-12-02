;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : parameters.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions for defining and accessing parameters.
;;; 
;;; Bugs        : 
;;;
;;; To do       : Finish the documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.18 Dan
;;;             : Creation.
;;;
;;; 2005.01.04 Dan
;;;             : Took some of the newlines out of show-all-parameters.
;;;             : Shrunk down to max width of 80 chars.
;;; 2005.01.17 Dan
;;;             : * Changed model-output to command-output so that one can
;;;             :   see sgp printing when :v is nil.
;;;             : * Changed the order of sgp's printing so that it's easier
;;;             :   to see the parameter values.
;;; 2005.02.11 Dan
;;;             : * Replaced a reverse with push-last in set-parameters.
;;; 2005.08.10 Dan
;;;             : * Added the remove-modules-parameters and remove-parameter
;;;             :   to support the undefine-module function.
;;;             : * Updated the version to 1.0.
;;; 2006.07.12 Dan
;;;             : * Modified TEST-AND-SET-PARAMETER-VALUE so that when a value
;;;             :   failed the validity test :invalid-value is returned for the
;;;             :   list of parameter values (could return the current setting
;;;             :   instead, but I think an error marker works better).
;;;             : * Also did some reformatting because I find it difficult to
;;;             :   read some of the code if I keep it to only 80 columns.
;;; 2008.07.23 Dan
;;;             : * Added a get-parameter-default-value function so that that
;;;             :   value can be retrieved through a function in the API.
;;; 2008.08.29 Dan
;;;             : * Updated define-parameter to check the valid-test parameter
;;;             :   for validity like the rest.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Internal structures not for external use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Holds all the parameters that have been "registered" with a module.

(defvar *act-r-parameters-table* (make-hash-table :test #'eq)
  "The table of all used parameters")



(defun define-parameter (param-name 
                         &key (owner t) (valid-test nil)
                         (default-value nil) (warning "")
                         (documentation ""))
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
        (t
         (make-act-r-parameter :owner owner
                               :param-name param-name
                               :default default-value
                               :test valid-test
                               :warning warning
                               :details documentation))))



(defun parse-parameters (parameters-list)
  "Make sure that they are parameters and not already owned if ownership 
   requested or that it exists if not owned"
  (if (every #'(lambda (x)
                 (and (act-r-parameter-p x)
                      (or (and (act-r-parameter-owner x) 
                               (not (valid-parameter-name 
                                     (act-r-parameter-param-name x))))
                          (and (not (act-r-parameter-owner x)) 
                               (valid-parameter-name 
                                (act-r-parameter-param-name x))))))
             parameters-list)
      parameters-list
    :error))
                                 

(defun install-parameters (module-name parameters)
  (dolist (x parameters)
    (if (act-r-parameter-owner x)
        (let ((param-copy (copy-act-r-parameter x)))
          (setf (act-r-parameter-owner param-copy) module-name)
          (setf (gethash (act-r-parameter-param-name param-copy) 
                         *act-r-parameters-table*)
            param-copy))
      (push module-name (act-r-parameter-users 
                         (get-parameter-struct 
                          (act-r-parameter-param-name x)))))))



(defun remove-modules-parameters (module-name)
  "Remove all parameters of the module both owned and watched"
  
  (maphash #'(lambda (name param)
               (when (eq module-name
                         (act-r-parameter-owner param))
                 (remhash name *act-r-parameters-table*))
               (setf (act-r-parameter-users param)
                 (remove module-name (act-r-parameter-users param))))
           *act-r-parameters-table*))


(defun remove-parameter (param-name)
  "Remove a specific parameter from the table"
  (remhash param-name *act-r-parameters-table*))
 
(defmacro sgp (&rest parameters)
  `(sgp-fct ',parameters))

(defun sgp-fct (&optional (parameters-list nil))
  (verify-current-mp  
   "sgp called with no current meta-process."
   (verify-current-model
    "sgp called with no current model."
    (set-or-get-parameters parameters-list))))
    

(defun set-or-get-parameters (params)
  (if (null params)
      (show-all-parameters)
    (if (every #'keywordp params)
        (get-parameters params)
      (set-parameters params))))

(defun get-parameters (params &optional (output t))
  (let ((res nil))
    (dolist (p-name params (reverse res))
      (if (valid-parameter-name p-name)
          (let* ((param (get-parameter-struct p-name))
                 (owner (act-r-parameter-owner param))
                 (val (process-parameters owner p-name)))
            (push val res)
            (when output
              (command-output "~S ~S (default ~S) : ~A"
                              p-name
                              val
                              (act-r-parameter-default param)
                              (act-r-parameter-details param))))
        (push :bad-parameter-name res)))))


(defun get-parameter-struct (p-name)
  (gethash p-name *act-r-parameters-table*))

(defun valid-parameter-name (p-name)
  (gethash p-name *act-r-parameters-table*))


(defun set-parameters (params)
  (if (evenp (length params))
      (let ((res nil))
        (while params
          (let ((p-name (pop params))
                (p-val (pop params)))
            (push-last (test-and-set-parameter-value p-name p-val) res)))
        res)
    (print-warning "Odd number of parameters and values passed to sgp.")))


(defun test-and-set-parameter-value (p-name value)
  (let ((param (gethash p-name *act-r-parameters-table*)))
    (if param
        (if (or (null (act-r-parameter-test param))
                (funcall (act-r-parameter-test param) value))
            (set-parameter-value param value)
          (progn
            (print-warning "Parameter ~S cannot take value ~A because it must be ~A."
                           p-name value (act-r-parameter-warning param))
            :invalid-value))
      (progn
        (print-warning "Parameter ~s is not the name of an available parameter" p-name)
        :bad-parameter-name))))

(defun set-parameter-value (param value)
  (let* ((current-value (process-parameters (act-r-parameter-owner param)
                                            (cons (act-r-parameter-param-name param) value))))
    (dolist (s (act-r-parameter-users param) current-value)
      
      (process-parameters s (cons (act-r-parameter-param-name param) current-value)))))



(defun show-all-parameters ()
  (let ((current-val-table (make-hash-table)))
    (maphash #'(lambda (p-name param)
                 (push 
                  (cons param (process-parameters (act-r-parameter-owner param) p-name)) 
                  (gethash (act-r-parameter-owner param) current-val-table)))
             *act-r-parameters-table*)
    (let ((name-len (1+ (apply #'max 
                               (mapcar #'(lambda (x) 
                                           (length (string x)))
                                 (hash-table-keys *act-r-parameters-table*)))))
          (default-len (apply #'max 
                              (with-hash-table-iterator (generator-fn *act-r-parameters-table*)
                                (let ((items nil))
                                  (loop     
                                    (multiple-value-bind (more? key value) (generator-fn)
                                      (declare (ignore key))
                                      (unless more? (return items))
                                      (push (length (format nil "~s" (act-r-parameter-default value)))
                                            items)))))))
                                          
          (value-len (apply #'max 
                            (with-hash-table-iterator (generator-fn current-val-table)
                              (let ((items nil))
                                (loop     
                                  (multiple-value-bind (more? key value) (generator-fn)
                                    (declare (ignore key))
                                    (unless more? (return items))
                                    (dolist (param value)
                                      (push (length (format nil "~S" (cdr param))) 
                                            items)))))))))
      
      
      (maphash #'(lambda (module-name parameters)
                   
                   (command-output "--------------------------------~%~S module" module-name)
                   (command-output "--------------------------------")
                   (dolist (param parameters)
                     
                     (command-output "~vS ~vS default: ~vS : ~A"
                                     name-len
                                     (act-r-parameter-param-name (car param))
                                     value-len
                                     (cdr param)
                                     default-len
                                     (act-r-parameter-default (car param))
                                     (act-r-parameter-details (car param)))))
               current-val-table))))

(defun get-parameter-default-value (param)
  (aif (get-parameter-struct param)
       (act-r-parameter-default it)
       (progn
         (print-warning "Invalid parameter name ~S in call to get-parameter-default-value." param)
         :bad-parameter-name)))

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
