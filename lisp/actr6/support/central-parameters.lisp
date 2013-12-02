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
;;; Filename    : central-parameters.lisp
;;; Version     : 1.1
;;; 
;;; Description : A module to hold parameters that could be used by more than
;;;               one module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.17 Dan
;;;             : Creation.
;;; 2005.01.09 Dan
;;;             : Moved the provide to the end.
;;; 2006.01.17 Dan
;;;             : * Updated the version to 1.0 and modified the description.
;;; 2008.07.23 Dan
;;;             : * Adding the register-subsymbolic-parameters command and the 
;;;             :   necessary changes to the module to record and verify that.
;;; 2008.07.24 [1.1]
;;;             : * Updated the version number because of the change.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Creates a module to hold some of the critical parameters that may
;;; be used by multiple modules:
;;;
;;; :esc Enable Subsymbolic Computation
;;; :ol  Optimized Learning
;;; :er  Enable Randomness
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; The three parameters :esc :ol and :er.
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

(defvar *subsymbolic-parameter-values* nil)

(defstruct central-parameters
  esc ol er)

(defun central-parameters-reset (instance)
  (schedule-event-relative 0 'check-for-esc-nil :maintenance t :output nil 
                           :priority :max :params (list instance)))

(defun check-for-esc-nil (instance)
  (when (and (null (central-parameters-esc instance))
             (some (lambda (param)
                     (let ((current (car (no-output (sgp-fct (list param))))))
                       (and (not (eq current :BAD-PARAMETER-NAME))
                            (not (equalp current (get-parameter-default-value param))))))
                   *subsymbolic-parameter-values*))
    (model-warning "Subsymbolic parameters have been set but :esc is currently nil.")))

(defun register-subsymbolic-parameters (&rest params)
  (dolist (param params)
    (when (and (valid-parameter-name param) 
               (not (find param *subsymbolic-parameter-values*)))
      (push param *subsymbolic-parameter-values*))))

(defun create-central-params (model-name)
  (declare (ignore model-name))
  (make-central-parameters))

(defun central-parameters-params (instance param)
  (cond ((consp param)
         (case (car param)
           (:esc (setf (central-parameters-esc instance) (cdr param)))
           (:ol (setf (central-parameters-ol instance) (cdr param)))
           (:er (setf (central-parameters-er instance) (cdr param)))))
        (t
         (case param
           (:esc (central-parameters-esc instance))
           (:ol (central-parameters-ol instance))
           (:er (central-parameters-er instance))))))

(define-module-fct 'central-parameters nil
  (list
   (define-parameter :esc :owner t :valid-test #'tornil :default-value nil
     :warning "either t or nil" :documentation "Enable Subsymbolic Computations")
   (define-parameter :er :owner t :valid-test #'tornil :default-value nil
     :warning "either t or nil" :documentation "Enable Randomness")
   (define-parameter :ol :owner t :valid-test #'(lambda (x) (or (tornil x) (posnum x)))
     :default-value t :warning "either t, nil, or a positive number"
     :documentation "Optimized Learning"))
  :version "1.1"
  :documentation "a module that maintains parameters used by other modules"
  :creation #'create-central-params
  :params #'central-parameters-params
  :reset #'central-parameters-reset)


(provide "CENTRAL-PARAMETERS")

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
