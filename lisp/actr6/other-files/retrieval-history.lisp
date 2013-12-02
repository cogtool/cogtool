;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : retrieval-history.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code to support the retrieval history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2008.08.14 Dan
;;;             : * Initial creation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Put this file into the other-files directory and the corresponding .tcl file
;;; into the environment/GUI/dialogs directory to use the new tool.
;;;
;;; Open the retrieval history window before running the model or set the
;;; :save-dm-history parameter to t in the model to enable the recording.
;;; 
;;; Once the model has run click the "Get history" button in the retrieval history 
;;; window.  
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-dm-history parameter
;;;  Enables the recording of retrieval history for display (default is nil).
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

(defstruct dm-history-module
  history
  enabled)
  

(defstruct dm-history
  time
  request
  chunks
  params)

(defun dm-request-recorder (request)
  (let ((history (get-module retrieval-history))
        (block (make-dm-history :time (mp-time) :request request)))
    (push block (dm-history-module-history history))
    nil))

(defun dm-retrieval-set-recorder (set)
  (let* ((history (get-module retrieval-history))
         (best (car set))
         (record (car (dm-history-module-history history)))
         (cmdt (no-output (car (sgp :cmdt)))))
    
    (cond ((null set)
           (setf (dm-history-chunks record) (list :retrieval-failure)))
          ((and best (< (no-output (caar (sdp-fct (list best :last-retrieval-activation)))) (no-output (car (sgp :rt)))))
           (setf (dm-history-chunks record) (cons :retrieval-failure set)))
          (t 
           (setf (dm-history-chunks record) set)))
    
    (dolist (x set)
      (unless (eq x :retrieval-failure)
        (let ((s (make-string-output-stream)))
          (sgp-fct (list :cmdt s))
          (sdp-fct (list x))
          (push (cons x (get-output-stream-string s)) (dm-history-params record)))))
      
    (sgp-fct (list :cmdt cmdt)))
  nil)


(defun dm-history-chunk-display (time chunk)
  (let* ((history (get-module retrieval-history))
         (record (find time (dm-history-module-history history) :key 'dm-history-time))
         (params (cdr (assoc chunk (dm-history-params record)))))
         
    (when params
      (pprint-chunks-fct (list chunk))
      (format t "~A" params))))
               
(defun dm-history-chunk-list (time)
  (let* ((history (get-module retrieval-history))
         (record (find time (dm-history-module-history history) :key 'dm-history-time)))
    (dm-history-chunks record)))

(defun dm-history-request-text (time)
  (let* ((history (get-module retrieval-history))
         (record (find time (dm-history-module-history history) :key 'dm-history-time)))
    (pprint-chunk-spec (dm-history-request record))))


(defun dm-history-get-time-list ()
  (let* ((history (get-module retrieval-history))
         )
    (nreverse (mapcar 'dm-history-time (dm-history-module-history history)))))


(defun reset-dm-history-module (module)
  (setf (dm-history-module-history module) nil))
  
(defun params-dm-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-dm-history 
          (no-output
           (progn
             (if (cdr param)
                 (progn
                   (unless (find 'dm-request-recorder (car (sgp :retrieval-request-hook)))
                     (sgp :retrieval-request-hook dm-request-recorder))
                   (unless (find 'dm-retrieval-set-recorder (car (sgp :retrieval-set-hook)))
                     (sgp :retrieval-set-hook dm-retrieval-set-recorder)))
               
               (progn
                 (when (find 'dm-request-recorder (car (sgp :retrieval-request-hook)))
                   (let ((old-hooks (car (sgp :retrieval-request-hook))))
                     (sgp :retrieval-request-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'dm-request-recorder)
                         (sgp-fct (list :retrieval-request-hook x))))))
                 (when (find 'dm-retrieval-set-recorder (car (sgp :retrieval-set-hook)))
                   (let ((old-hooks (car (sgp :retrieval-set-hook))))
                     (sgp :retrieval-set-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'dm-retrieval-set-recorder)
                         (sgp-fct (list :retrieval-set-hook x))))))))
          
             (setf (dm-history-module-enabled instance) (cdr param))))))
    (case param
      (:save-dm-history (dm-history-module-enabled instance)))))

(define-module-fct 'retrieval-history nil 
  (list (define-parameter :save-dm-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the history of all retrieval events."))
  :creation (lambda (x) (declare (ignore x)) (make-dm-history-module))
  :reset 'reset-dm-history-module
  :params 'params-dm-history-module
  :version "1.0"
  :documentation "Module to record retrieval history for display in the environment.")
  

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
