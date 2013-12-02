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
;;; Filename    : buffer-history.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code to support the buffer history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2008.08.20 Dan
;;;             : * Initial creation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Put this file into the other-files directory and the corresponding .tcl file
;;; into the environment/GUI/dialogs directory to use the new tool.
;;;
;;; Open the buffer history window before running the model or set the
;;; :save-buffer-history parameter to t in the model to enable the recording.
;;; 
;;; Once the model has run click the "Get history" button in the buffer history 
;;; window.  Only those buffers specified with the :traced-buffers parameter
;;; of the buffer-trace module will have thier history recorded.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-buffer-history parameter
;;;  Enables the recording of retrieval history for display (default is nil).
;;;  This will also set the :save-buffer-trace parameter to t if it is not
;;;  already set.
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

(defstruct buffer-history-module
  (state-table (make-hash-table))
  (table (make-hash-table))
  enabled)
  

(defstruct buffer-state-record
  busy error)

(defstruct buffer-history
  time
  chunk  ; a string, :cleared, or nil
  status ; a string
  )


(defun buffer-history-recorder (summaries)
  (let ((history (get-module buffer-history))
        (time (buffer-record-time-stamp summaries)))
    (when history
      (dolist (summary (buffer-record-buffers summaries))
        (let* ((name (buffer-summary-name summary))
               (state (gethash name (buffer-history-module-state-table history))))
          (unless state
            (setf (gethash name (buffer-history-module-state-table history))
              (make-buffer-state-record :busy (buffer-summary-busy summary)
                                        :error (buffer-summary-error summary))))
          (when (or ;anything general has changed
                 (buffer-summary-cleared summary)
                 (buffer-summary-modified summary)
                 (buffer-summary-chunk-name summary)
                 (buffer-summary-busy->free summary)
                 (buffer-summary-error->clear summary)
                 (null state)
                 (not (eq (buffer-state-record-busy state) (buffer-summary-busy summary)))
                 (not (eq (buffer-state-record-error state) (buffer-summary-error summary))))
            
            (setf (gethash name (buffer-history-module-table history))
              (push (make-buffer-history :time time
                                         :chunk (cond ((or (buffer-summary-modified summary)
                                                           (buffer-summary-chunk-name summary))
                                                       (capture-model-output (buffer-chunk-fct (list name))))
                                                      ((buffer-summary-cleared summary)
                                                       :cleared)
                                                      (t nil))
                                         :status (capture-model-output (buffer-status-fct (list name))))
                    (gethash name (buffer-history-module-table history))))
            (when state
              (setf (buffer-state-record-busy state) (buffer-summary-busy summary))
              (setf (buffer-state-record-error state) (buffer-summary-error summary)))))))))

               
(defun buffer-history-buffer-list ()
  (let ((history (get-module buffer-history)))
    (hash-table-keys (buffer-history-module-table history))))

(defun buffer-history-text (time buffer)
  (if (and time buffer)
      (let* ((history (get-module buffer-history))
             (data (gethash buffer (buffer-history-module-table history)))
             (chunk (find-if (lambda (x) 
                               (and (<= (buffer-history-time x) time)
                                    (buffer-history-chunk x)))
                             data))
             (status (find-if (lambda (x)
                                (<= (buffer-history-time x) time))
                              data)))
        (concatenate 'string (cond ((and chunk (stringp (buffer-history-chunk chunk))) 
                                    (buffer-history-chunk chunk))
                                   (t (format nil "buffer empty~%")))
          (string #\newline)
          (cond ((and status (stringp (buffer-history-status status)))
                 (buffer-history-status status))
                (t "No buffer status information available"))))
    ""))

(defun buffer-history-time-list ()
  (let ((history (get-module buffer-history))
        (times nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (dolist (x value)
                 (push (buffer-history-time x) times)))
             (buffer-history-module-table history))
    (sort (remove-duplicates times) #'<)))


(defun reset-buffer-history-module (module)
  (clrhash (buffer-history-module-table module))
  (clrhash (buffer-history-module-state-table module)))

  
(defun params-buffer-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-buffer-history 
          (no-output
           (progn
             (if (cdr param)
                 (progn
                   (sgp :save-buffer-trace t)
                   (unless (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                     (sgp :buffer-trace-hook buffer-history-recorder)))
               
               (progn
                 (when (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                   (let ((old-hooks (car (sgp :buffer-trace-hook))))
                     (sgp :buffer-trace-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'buffer-history-recorder)
                         (sgp-fct (list :buffer-trace-hook x))))))))
          
             (setf (buffer-history-module-enabled instance) (cdr param))))))
    (case param
      (:save-buffer-history (buffer-history-module-enabled instance)))))

(define-module-fct 'buffer-history nil 
  (list (define-parameter :save-buffer-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the history of buffer changes."))
  :creation (lambda (x) (declare (ignore x)) (make-buffer-history-module))
  :reset 'reset-buffer-history-module
  :params 'params-buffer-history-module
  :version "1.0"
  :documentation "Module to record buffer change history for display in the environment.")
  

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
