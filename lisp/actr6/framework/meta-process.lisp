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
;;; Filename    : meta-process.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : The meta-process handling functions as defined in the
;;;               ACT-R 6 software framework API.
;;; 
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Improve on the max-time-delta situation for multiple models.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.11 Dan
;;;             : Creation.
;;; 2005.02.28 Dan
;;;             : * Made the with-meta-process macro hygienic.
;;; 2006.02.27 Dan
;;;             : * Added the mp-real-time-management function to allow one to
;;;             :   configure external time sources.
;;; 2006.03.03 Dan
;;;             : * Updated mp-real-time-management to add the max-time-delta
;;;             :   parameter.  This provides a solution for a problem that can
;;;             :   occur when hooking a model up to an asynchronous system.
;;;             :   The problem is that if there aren't any model events to 
;;;             :   process at some point the model just jumps right to its end
;;;             :   time and waits for real time to catch up and asynchronous
;;;             :   events that come in effectively get pushed off until then.
;;;             :   This effectively provides the maximum amount of time that
;;;             :   the model will "skip ahead" without some event occuring.
;;;             :   This still isn't perfect for a multi-model situation because
;;;             :   it only works at the meta-process level and thus one model
;;;             :   could still end up skipping way ahead if other models were
;;;             :   still doing things, but it's better than nothing right now.
;;; 2007.08.03 Dan
;;;             : * Moved the *meta-processes* definition before the macros
;;;             :   that use it to avoid a warning at compile time.
;;; 2008.05.05 Dan
;;;             : * Fixed a bug with a missing parameter to format in 
;;;             :   delete-meta-process-fct.
;;; 2009.11.30 Dan
;;;             : * Make sure to set meta-p-running back to nil on reset because
;;;             :   some abnormal situations could leave that set.
;;; 2009.12.03 Dan
;;;             : * Clear the dynamics and in-slack slots of the meta-process on
;;;             :   reset and adding an allow-dynamics keyword to mp-real-time-management
;;;             :   to enable dynamic event testing.
;;; 2010.03.03 Dan
;;;             : * Added a with-meta-process-eval like with-model has.
;;; 2010.03.05 Dan
;;;             : * Fixed a bug in define-meta-process-fct with the printing of
;;;             :   the warning when the name is invalid.
;;; 2010.09.02 Dan
;;;             : * Added code to allow the coercion of the mp-time variable to
;;;             :   a different float type.
;;;             : * Added commands for checking the accuracy of the mp-time float
;;;             :   type and allow changing it on the fly.
;;; 2010.11.03 Dan
;;;             : * Changing the internal time to be ms and converting it for
;;;             :   what's sent out (mp-time) which means the accuracy checks
;;;             :   and conversion code can go away once this is settled.
;;;             : * Removed the coercion code, but left the size tests in for
;;;             :   now just in case that may be useful in the future since
;;;             :   specifying large event in seconds would still be represented as
;;;             :   floats that would lose precision.
;;; 2010.12.22 Dan
;;;             : * Added mp-modules-events to return a list of all events that
;;;             :   are scheduled for a module - both active and waiting.
;;; 2011.03.25 Dan
;;;             : * Added mp-time-ms as a first step to transitioning everything
;;;             :   internal to milliseconds to fix the last few lingering issues.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The global *meta-processes* and the corresponding struct are not part of 
;;; the API, so should not be touched by module writers or modelers.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; mp-time
;;; mp-time returns the current time of the current meta-process in seconds. 
;;;
;;; mp-models
;;; mp-models returns a list of the names of all the models defined in the current meta-process. 
;;; 
;;; meta-process-names
;;; meta-process-names returns a list of the names of all the existing meta-processes.
;;; 
;;; mp-show-queue 
;;; mp-show-queue prints the events that are on the event queue of the current meta-process 
;;; to *standard-output* in the order that they would be executed.
;;; 
;;; mp-show-waiting
;;; mp-show-waiting prints the events that are in the waiting queue of the current meta-process 
;;; along with a description of the condition for which each needs to be added to the event queue to *standard-output*.
;;; 
;;; mp-print-versions
;;; mp-print-versions prints the version number of the framework and the name, 
;;; version number, and documentation of each module which is currently defined to *standard-output*.
;;; 
;;; define-meta-process (mp-name)
;;; If there is no meta-process with the name mp-name already defined then one is created.  
;;; 
;;; delete-meta-process (mp-name)
;;; If there is a meta-process with the name mp-name, then all of the models in that meta-process 
;;; are deleted and then the meta-process itself is removed.  
;;; 
;;; with-meta-process  (mp-name &body body))
;;; If mp-name is the name of a meta-process then the forms of the body are evaluated in order with the 
;;; current meta-process set to the one named by mp-name. 
;;; 
;;; current-meta-process
;;; current-meta-process returns the name of the current meta-process or nil 
;;; if there is no current meta-process.
;;;
;;; mp-real-time-management (&key (time-function 'get-internal-real-time)
;;;                                   (units-per-second internal-time-units-per-second)
;;;                                   (slack-function 'real-time-slack)
;;;                                   (max-time-delta nil))
;;; mp-real-time-management sets the function and divisor used to determine the
;;; current time in seconds when then real-time flag is specified to run the
;;; meta-process.  The slack function is called continuously while the model
;;; is waiting for the time to advance when there is a discrepancy.  It must take
;;; one parameter which will be the current delta between the model time and 
;;; the currently reported "real time".  The max-time-delta specifies how far
;;; the model will "skip ahead" - the maximum time between any two model events
;;; in simulation time.  When it is nil the delta is unbounded.
;;; The default behavior is tied to the real clock, it calls the sleep function 
;;; if the model has to spin for greater than 150ms, and there is no limit on
;;; how far ahead it can advance in one step.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Using structs for now because I don't need the flexibility of CLOS classes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; The top level tabel that holds all the meta-processes.

;;; Holds all the meta-processes that have been defined and indicates
;;; which is the current one.

(defvar *meta-processes* (make-meta-processes)
  "The table of all defined meta-processes")

(defmacro current-mp ()
  `(mps-current *meta-processes*))

(defun current-mp-fct ()
  (mps-current *meta-processes*))

(defun get-mp (mp-name)
  (gethash mp-name (mps-table *meta-processes*)))

(defun reset-mp (meta-process)
  "Set a meta-process to time 0 and clear the events"
  (setf (meta-p-time meta-process) 0)
  (setf (meta-p-start-time meta-process) nil)
  (setf (meta-p-start-real-time meta-process) nil)
  (setf (meta-p-events meta-process) nil)
  (setf (meta-p-delayed meta-process) nil)
  (setf (meta-p-dynamics meta-process) nil)
  (setf (meta-p-in-slack meta-process) nil)
  (setf (meta-p-running meta-process) nil)
  (setf (meta-p-time-overflow-warning meta-process) nil)
  (mp-real-time-management :mp meta-process))


(defun mp-time-accuracy-limit ()
  (ms->seconds *time-size-current-limit*))

(defun mp-time-change-needed (max-time)
  (if (> max-time *time-size-current-limit*)
      (let ((sufficient (find max-time *time-size-test-list* :key 'car :test (lambda (x y) (<= x (expt 2 y))))))
        (if sufficient
            (values t (cadr sufficient))
          (values t nil)))
    (values nil nil)))


(defmacro verify-current-mp (warning &body body)
  `(if (null (mps-current *meta-processes*))
       (print-warning ,warning)
     (progn ,@body)))

(defun mp-time ()
  "returns the current time of the current meta-process in seconds"
  (verify-current-mp  
   "mp-time called with no current meta-process."
   (ms->seconds (meta-p-time (current-mp)))))

(defun mp-time-ms ()
  "returns the current time of the current meta-process in milliseconds"
  (verify-current-mp
   "mp-time-ms called with no current meta-process."
   (meta-p-time (current-mp))))

(defun mp-real-time-management (&key (mp (current-mp-fct))
                                         (time-function 'get-internal-real-time)
                                         (units-per-second internal-time-units-per-second)
                                     (slack-function 'real-time-slack)
                                     (max-time-delta nil)
                                     (allow-dynamics nil))
  (when mp
    (setf (meta-p-allow-dynamics mp) allow-dynamics)
    (setf (meta-p-time-function mp) time-function)
    (setf (meta-p-units-per-second mp) units-per-second)
    (setf (meta-p-slack-function mp) slack-function)
    (setf (meta-p-max-time-delta mp) (if (numberp max-time-delta) (seconds->ms max-time-delta) max-time-delta))))

(defun mp-models ()  
  "returns a list of the names of all the models in the current meta-process"
  (verify-current-mp  
   "mp-models called with no current meta-process."
   (hash-table-keys (meta-p-models (current-mp)))))

(defun meta-process-names ()
  (hash-table-keys (mps-table *meta-processes*)))


(defun mp-show-queue ()
  (verify-current-mp 
   "mp-show-queue called with no current meta-process."
   (let ((events (meta-p-events (current-mp))))
     (format t "Events in the queue:~%")
     (dolist (evt events (length events))
       (format t "~A~%" (format-event evt))))))


(defun mp-show-waiting ()
  (verify-current-mp 
   "mp-show-waiting called with no current meta-process."
   
   (let ((events (meta-p-delayed (current-mp))))
     (format t "Events waiting to be scheduled:~%")
     (dolist (evt events (length events))
       (format t "~A~%" (format-event evt))))))


(defun mp-modules-events (module)
  (verify-current-mp 
   "mp-modules-events called with no current meta-process."
   (let ((events nil))
     (dolist (evt (meta-p-events (current-mp)))
       (when (eq module (evt-module evt))
         (push-last evt events)))
     
     (dolist (evt (meta-p-delayed (current-mp)))
       (when (eq module (evt-module evt))
         (push-last evt events)))
     
     events)))
     

(defun mp-print-versions ()
  (format t "ACT-R Version Information:~%~va: ~10a ~a~%"
    (max (max-module-name-length) 10)
    "Framework"
    (meta-p-version (gethash 'default (mps-table *meta-processes*)))
    (meta-p-documentation (gethash 'default (mps-table *meta-processes*))))
  (maphash #'(lambda (key value)
               (declare (ignore key))
               (format t "~va: ~10a ~a~%"
                 (max (max-module-name-length) 10)
                 (act-r-module-name value)
                 (act-r-module-version value)
                 (act-r-module-documentation value)))
           (global-modules-table)))
   
    

(defmacro define-meta-process (mp-name)
  `(define-meta-process-fct ',mp-name))

(defun define-meta-process-fct (mp-name)
  (if (not (symbolp mp-name))
      (print-warning "~S is not a symbol and thus not valid as a meta-process name." mp-name)
    (if (gethash mp-name (mps-table *meta-processes*))
        (print-warning "There is already a meta-process named ~S." mp-name)
      (let ((mp (make-meta-process :name mp-name)))
        (setf (gethash mp-name (mps-table *meta-processes*)) mp)
        (incf (mps-count *meta-processes*))
        (setf (mps-current *meta-processes*) nil)
        mp-name))))


(defmacro delete-meta-process (mp-name)
  `(delete-meta-process-fct ',mp-name))

(defun delete-meta-process-fct (mp-name)
  (if (eql mp-name 'default)
      (print-warning "Cannot delete the default meta-process.")
    (if (gethash mp-name (mps-table *meta-processes*))
        (let ((previous-mp (current-mp)))
          (setf (mps-current *meta-processes*) 
            (gethash mp-name (mps-table *meta-processes*)))
          (maphash #'(lambda (key model)
                       (declare (ignore model))
                       (delete-model-fct key))
                   (meta-p-models (gethash mp-name (mps-table *meta-processes*))))
          (remhash mp-name (mps-table *meta-processes*))
          (decf (mps-count *meta-processes*))
          (if (= 1 (mps-count *meta-processes*))
              (setf (mps-current *meta-processes*)
                (gethash 'default (mps-table *meta-processes*)))
            (setf (mps-current *meta-processes*) previous-mp))
          t
          )
      (print-warning "~S does not name a meta-process." mp-name))))


(defmacro with-meta-process (mp-name &body body)
  (let ((mp (gensym))
        (old-mp (gensym)))
    `(let ((,mp (gethash ',mp-name (mps-table *meta-processes*))))
       (if ,mp
           (let ((,old-mp (current-mp)))
             (setf (mps-current *meta-processes*) ,mp)
             (unwind-protect 
                 (progn ,@body)             
               (setf (mps-current *meta-processes*) ,old-mp)))
         
         (print-warning "No actions taken in with-meta-process because ~S does not name a meta-process" 
                        ',mp-name)))))


(defmacro with-meta-process-eval (mp-name &body body)
  (let ((mp (gensym))
        (old-mp (gensym))
        (m (gensym)))
    `(let* ((,m ,mp-name)
            (,mp (gethash ,m (mps-table *meta-processes*))))
       (if ,mp
           (let ((,old-mp (current-mp)))
             (setf (mps-current *meta-processes*) ,mp)
             (unwind-protect 
                 (progn ,@body)             
               (setf (mps-current *meta-processes*) ,old-mp)))
         
         (print-warning "No actions taken in with-meta-process-eval because ~S does not name a meta-process" ,m)))))

(defun with-meta-process-fct (mp-name forms-list)
  (let ((with-mp (gethash mp-name (mps-table *meta-processes*))))
     (if with-mp
         (let ((previous-mp (current-mp))
               (val nil))
           (setf (mps-current *meta-processes*) with-mp)
           (unwind-protect 
               (dolist (x forms-list val)
                 (setf val (eval x)))
           (setf (mps-current *meta-processes*) previous-mp)))
       (print-warning "No actions taken in with-meta-process-fct because ~S does not name a meta-process" 
                      mp-name))))


(defun current-meta-process ()
  (when (current-mp)
    (meta-p-name (current-mp))))


(define-meta-process default)
(setf (mps-current *meta-processes*) (gethash 'default (mps-table *meta-processes*)))

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
