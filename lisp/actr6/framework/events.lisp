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
;;; Filename    : events.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : The definition of events for the scheduler as described in
;;;               the ACT-R 6 software framework API.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.08.11 Dan
;;;             : Creation.
;;; 2005.01.09 Dan
;;;             : Changing the event trace a little. I don't think it's
;;;             : necessary to print the destination in the trace since
;;;             : it's either the same as the module (and already there)
;;;             : or the module that was the destination will show an
;;;             : event later in response...
;;; 2006.07.16 Dan
;;;             : Changed the API section to note the correct accessor is
;;;             : evt-output.
;;; 2007.10.31 Dan
;;;             : * Fixed the format-event for break events so that if the longest
;;;             :   model or module names are shorter than the string of "-" 
;;;             :   characters printed the string is truncated to fit the trace
;;;             :   column size.
;;; 2011.03.25 Dan
;;;             : * Changed format-event and the format string so that a nil time
;;;             :   doesn't cause a problem (waiting events have nil times).
;;;             : * Adjusted format-event so that a dynamic will indicate that
;;;             :   it may still be moved (not quite the same as waiting).
;;;             : * One step farther in format-event to remove the evt-time call
;;;             :   entirely, and add the dynamic reporting to break events.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Despite the fact that events are simply structs one should not create
;;; them explicitly and only the scheduling fuctions provided in schedule.lisp
;;; should be used to generate them.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; 
;;; evt-time (event)
;;; returns the time in seconds at which the event is scheduled to occur.
;;;
;;; evt-priority (event)
;;; returns the priority of the event.  
;;;
;;; evt-action (event)
;;; returns the action function that will be executed for the event.  
;;;
;;; evt-model (event)
;;; returns the name of the model in which this event was created.
;;;
;;; evt-module (event)
;;; returns the name of the module which created this event.
;;;
;;; evt-destination (event)
;;; returns the destination which was specified for the event.
;;;
;;; evt-params (event)
;;; returns the list of parameters which will be passed to the event's 
;;; action function when the event is executed.
;;;
;;; evt-details (event)
;;; returns the details string that will be displayed in the trace for 
;;; this event if it has one, or nil if it does not.
;;;
;;; evt-output (event)
;;; returns the output value of this event, which indicates whether to 
;;; print this event in the trace.
;;;
;;; format-event (event)
;;; returns a string that contains the text that will be displayed in the 
;;; trace for event if it is executed and has its output set to t.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Because a function with the name 'event-'something is likely to conflict 
;;; with existing functions in some Lisp and the default operation of the 
;;; system is to not separately package the components, the names of the 
;;; accessors begin with evt- instead of event-.
;;;
;;; Using a struct because I don't need anything fancy at this point.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



(defun act-r-event-break-action (mp)
  (setf (meta-p-break mp) t))

(defmethod format-event (event)
  (declare (ignore event))
  nil)

(defconstant +format-event-event-string+ 
    (formatter "~:[~*~*~;~6d.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va ~:[~*~a~{ ~a~}~;~a~*~*~] ~:[~@[Waiting for: ~A~]~;Dynamically adjusted for: ~A~]"))


(defmethod format-event ((event act-r-event))
  (let ((*print-pretty* nil)
        (mp (get-mp (evt-mp event))))
    (multiple-value-bind (sec ms) (when (numberp (evt-mstime event)) (truncate (evt-mstime event) 1000))
      (format nil +format-event-event-string+
        
        (evt-mstime event)
        sec ms
        
        (< 1 (mps-count *meta-processes*))
        (evt-mp event)
        
        (< 1 (meta-p-model-count mp))
        (meta-p-model-name-len mp)
        (evt-model event)
        
        (max-module-name-length)
        (evt-module event)
        
        (evt-details event)
        (evt-details event)
        (evt-action event)
        (evt-params event)
        
        (evt-dynamic event)
        (evt-wait-condition event)))))
  
(defconstant +format-event-break-event-string+ (formatter "~:[~*~*~;~6d.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va BREAK-EVENT ~@[~a ~]~:[~@[Waiting for: ~A~]~;Dynamically adjusted for: ~A~]"))


(defmethod format-event ((event act-r-break-event))
  (let ((*print-pretty* nil)
        (mp (get-mp (evt-mp event))))
    (multiple-value-bind (sec ms) (when (numberp (evt-mstime event)) (truncate (evt-mstime event) 1000))
      (format nil +format-event-break-event-string+
        (evt-mstime event)
        sec ms
        
        (< 1 (mps-count *meta-processes*))
        (evt-mp event)
        (< 1 (meta-p-model-count mp))
        (meta-p-model-name-len mp)
        (subseq "------" 0 (min 6 (meta-p-model-name-len mp)))
        (max-module-name-length)
        (subseq "------" 0 (min 6 (max-module-name-length)))
        (evt-details event)
        (evt-dynamic event)
        (evt-wait-condition event)))))
  

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
