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
;;; Filename    : goal.lisp
;;; Version     : 1.1
;;; 
;;; Description : Implementation of the goal module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.13 Dan
;;;             : Initial creation.
;;;
;;; 2005.01.19 Dan
;;;             : Updated goal-focus to better match what ACT-R 4/5 did.
;;; 2005.02.22 Dan
;;;             : * Added a reset function to the goal to clear the stuffed flag
;;;             : * Changed the stuffed flag to holding the chunk that is set
;;;             :   by the user so that goal-focus can print a "pending" message
;;;             :   when queried between the goal-focus and the model actually
;;;             :   running to set the buffer.
;;; 2005.02.25 Dan
;;;             : * Clean up the "pending" info a little more so that if there
;;;                 is a chunk in the buffer things still "look" right.
;;; 2005.03.18 Dan
;;;             : * The goal buffer will not be strict harvested by default,
;;;             :   so the reset function is going to set that parameter.
;;; 2005.03.21 Dan 
;;;             : * Changing the version to 1.0b1.
;;; 2005.03.23 Dan
;;;             : * In order to set the do-not-harvest parameter correctly it 
;;;             :   do so after the defaults are set so goal uses the second
;;;             :   reset parameter to do its reseting (doesn't need the first).
;;; 2005.03.24 Dan
;;;             : * Undoing the setting of goal on do-not-havrest list for now
;;;             :   under the idea that parsimony is better overall.
;;; 2005.03.27 Dan
;;;             : * Back it goes - goal is once again set to not be strict
;;;             :   harvested by default.
;;; 2005.04.23 Dan
;;;             : * Stuffed is no longer a query for the module, but the 
;;;             :   internal slot has other uses.
;;; 2005.05.20 Dan
;;;             : * Renamed the stuffed slot to delayed and cleaned up how
;;;             :   it gets used becasue there were some situations where 
;;;             :   calling goal-focus resulted in incorrect information.
;;; 2005.07.06 Dan
;;;             : * Updated the docs and changed the version to 1.0.
;;; 2005.07.21 Dan
;;;             : * Changed mod-focus so that it schedules a notice of the
;;;             :   modification.  May want to actually schedule the mod, but
;;;             :   that could break some older/existing stuff so I want to be
;;;             :   safe for now.
;;; 2005.08.10 Dan
;;;             : * Updated goal-query to specify that the instance is ignored.
;;; 2006.07.20 Dan
;;;             : * Changed the goal-focus command so that the clean-up event
;;;             :   is flagged as a maintenance event.
;;; 2007.06.05 Dan
;;;             : * Added buffer modification requests as an option in the goal 
;;;             :   buffer.  Acts just like a production's buffer modification
;;;             :   except that with the + it's attributed to the goal module
;;;             :   instead of procedural i.e. there is no difference in performance
;;;             :   between the RHS actions:
;;;             :   =goal> slot value  
;;;             :   and 
;;;             :   +goal> slot value 
;;;             :   
;;;             :   The only difference will be in the output of the trace and
;;;             :   the buffer summary records.
;;; 2008.09.19 Dan 
;;;             : * Moved the mod-request function to goal-style support
;;;             :   and changed the goal module's definition to use it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The goal module has one buffer called goal.
;;; The source spread parameter of the goal is called :ga and defaults to 1.
;;;
;;; The goal module responds to requests by creating a new chunk and placing it
;;; into the buffer.  The requests must be a unique specification of a chunk.
;;; Thus, no variables are allowed and each slot may be specified at most once.
;;; The new chunk is placed into the buffer at the same time as the request.
;;;
;;; It only responds to the required queries - state {busy, free, error} and 
;;; buffer {empty, full, requested, unrequested}.
;;;
;;; State free will always return t.
;;; State busy will always return nil.
;;; State error will always return nil.
;;; Buffer requested will respond nil if the goal chunk is created with the
;;;   goal-focus command otherwise it will respond t.
;;;
;;; The goal module now DOES respond to buffer modification requests.
;;; A buffer modification request acts just like the corresponding modification
;;; action in the production.  The only difference is that it is now attributed
;;; to the goal module - it still takes 0 time.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; User Functions:
;;;
;;;   (defmacro goal-focus (&optional (chunk-name nil)))
;;;   (defun goal-focus-fct (&optional (chunk-name nil)))
;;; 
;;;   If the chunk-name is passed in then an event is scheduled to place a copy 
;;;   of that chunk into the goal buffer at the current time and t is returned.
;;;
;;;   If no chunk-name is given then the chunk currently in the goal buffer is
;;;   printed and that chunk's name is returned.
;;;
;;;   (defmacro mod-focus (&rest modifications)
;;;   (defun mod-focus-fct (modifications)
;;;
;;;   Modifies the chunk currently in the goal buffer using mod-chunk and the
;;;   modifications provided.
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

;;; Rely on the general functions in the goal-style-module 

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

;;; Only need to record the chunk that will be stuffed into the buffer

(defstruct goal-module delayed)

(defun create-goal-module (model-name)
  (declare (ignore model-name))
  (make-goal-module))


(defun goal-reset (instance)
  (setf (goal-module-delayed instance) nil)
  ; Do NOT strict harvest the goal buffer by default
  (sgp :do-not-harvest goal)
  )

(defun goal-query (instance buffer-name slot value)
  (declare (ignore buffer-name) (ignore instance))
  ;; only valid slot is state
  (case slot
    (state
     (case value
       (busy nil)
       (free t)
       (error nil)
       (t (print-warning "Unknown state query ~S to goal module" value)
          nil)))
    (t (print-warning "Unknown query ~S ~S to the goal module" slot value))))




;;; Actually define the module now

(define-module-fct 'goal '((goal (:ga 1.0)))
  nil
  :version "1.1"
  :documentation "The goal module creates new goals for the goal buffer"
  :creation #'create-goal-module
  :query #'goal-query
  :request #'goal-style-request
  :buffer-mod #'goal-style-mod-request
  :reset (list nil #'goal-reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User functions for the Goal module - since there aren't too many
;;; just putting them right here
;;;

(defmacro goal-focus (&optional (chunk-name nil))
  "Place a chunk into the goal buffer or return either the chunk that is there
   now or the one that will be placed there by a pendng goal-focus"
  `(goal-focus-fct ',chunk-name))

(defun goal-focus-fct (&optional (chunk-name nil))
  "Place a chunk into the goal buffer or return either the chunk that is there
   now or the one that will be placed there by a pending goal-focus"
  (let ((g-module (get-module goal)))
    (if chunk-name
        (if (chunk-p-fct chunk-name)
            (progn
              
              ;; Should it clear it immediately first?
              
              (schedule-set-buffer-chunk 'goal chunk-name 0 :module 'goal 
                                         :priority :max :requested nil)
              (schedule-event-after-module 'goal #'clear-delayed-goal :module 'goal 
                                           :output nil  
                                           :destination 'goal
                                           :maintenance t)
              
              (setf (goal-module-delayed g-module) chunk-name)
              chunk-name)
          ;; This is a serious problem so don't use model-warning
          (print-warning 
           "~S is not the name of a chunk in the current model - goal-focus failed"
           chunk-name))
      
      (let ((chunk (buffer-read 'goal))
            (delayed (goal-module-delayed g-module)))
        (cond ((and (null chunk) (null delayed))
               (command-output "Goal buffer is empty")
               nil)
              ((null chunk)
               (command-output "Will be a copy of ~a when the model runs" 
                               delayed)
               (pprint-chunks-fct (list delayed))
               delayed)
              ((null delayed)
               (pprint-chunks-fct (list chunk))
               chunk)
              (t
               (if (eq delayed (chunk-copied-from-fct chunk))
                   ;; caught it before the delayed chunk was cleared
                   (progn
                     (pprint-chunks-fct (list chunk))
                     chunk)
                 (progn
                   (command-output "Will be a copy of ~a when the model runs" 
                                   delayed)
                   (command-output "Currently holds:")
                   (pprint-chunks-fct (list chunk))
                   delayed))))))))


(defun clear-delayed-goal (instance)
  (setf (goal-module-delayed instance) nil))

(defmacro mod-focus (&rest modifications)
  "Modify the chunk in the goal buffer as if by mod-chunk"
  `(mod-focus-fct ',modifications))

(defun mod-focus-fct (modifications)
  "Modify the chunk in the goal buffer as if by mod-chunk-fct"
  (let ((chunk (buffer-read 'goal)))
    (if chunk
        (progn
          (schedule-event-relative 0 'goal-modification 
                                   :module 'goal
                                   :priority :max
                                   :output 'medium)
          (mod-chunk-fct chunk modifications))
                                   
      (print-warning "No chunk in the goal buffer to modify"))))

(defun goal-modification ()
  "Dummy function for mod-focus event"
  nil)

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
