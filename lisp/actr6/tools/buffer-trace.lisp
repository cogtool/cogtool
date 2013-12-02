;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2006 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : buffer-trace.lisp
;;; Version     : 1.0
;;; 
;;; Description : Provide a tool that shows what activities are occuring in
;;;             : the buffers instead of the current "event" based trace and
;;;             : make that information available to the modeler as well if
;;;             : desired.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [] Watch the :trace-filter parameter and warn if it gets set
;;;             :    to a function other than disable-event-trace when the buffer
;;;             :    trace is enabled.
;;;             : [] Better monitor the setting/removing of the post-event hook.
;;; 
;;; ----- History -----
;;; 2006.01.26 Dan
;;;             : * Initial creation.
;;; 2006.02.07 Dan
;;;             : * Fixed an issue with subseq going past the end of the event-details.
;;; 2006.07.18 Dan
;;;             : * Changed schedule-maintenance-event-relative to just
;;;             :   schedule-event-relative because maintenance is now just a 
;;;             :   keyword in all of the scheduling functions.
;;; 2006.09.11 Dan
;;;             : * Changed the parameter test for the trace step to just be posnumornil.
;;; 2007.05.18 Dan
;;;             : * Added the busy->free flag to the buffer summary records because
;;;             :   it's possible for a module to be marked as busy during some 
;;;             :   early events and then become free later at the same time
;;;             :   and that can be useful to know.
;;; 2007.05.22 Dan
;;;             : * Added two parameters for use with the graphic trace tools
;;;             :   in the environment: :buffer-trace-colors and :graphic-column-widths.
;;;             : * Took the a1 off the version number.
;;;             : * Added the additional slot of notes to the buffer summary 
;;;             :   records and added the command add-buffer-trace-notes
;;;             :   to allow one to add them to the record.  
;;; 2007.06.06 Dan
;;;             : * Fixed a bug in the value returned when setting the :traced-buffers
;;;             :   parameter.
;;;             : * Added the error->clear flag to the buffer summary records
;;;             :   because like busy->free that can be an important transition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This module collects information about the buffers in the system as a model
;;; runs (when enabled).  That information can be displayed as a trace while the
;;; model runs, or saved for later use by the modeler.
;;; 
;;;
;;; The module has 5 parameters that control the tracing in general and 2 that
;;; allow configuration of the display through the graphic tracing tools of the
;;; environment:
;;;
;;; :BUFFER-TRACE   default: NIL 
;;;   If this parameter is set to t, then the normal event trace is disabled and
;;;   the buffer trace is printed instead.
;;;
;;; :TRACED-BUFFERS default: T  
;;;   The list of buffers to be traced (all buffers if set to t).  Only those
;;;   buffers specified on this list will have their data recorded.  The order
;;;   of the buffers in the list is the order they will be printed, and if it
;;;   is set to t all buffers will be displayed in alphabetical order.
;;;
;;; :BUFFER-TRACE-STEP  default: NIL 
;;;   When this is set to a number it specifies the maximum amount of time 
;;;   that is allowed to elapse before creating a new buffer summary (there
;;;   may be smaller time steps that correspond to model actions).
;;;
;;; :SAVE-BUFFER-TRACE default: NIL
;;;   When set to t the module will record the summary data so that the modeler
;;;   can use it later (does not alter the trace i.e. if :buffer-trace is nil
;;;   and :save-buffer-trace is t one will still get the event based trace).
;;;
;;; :BUFFER-TRACE-HOOK default: NIL
;;;   Can be set to a function which takes one parameter.  It will be called
;;;   with every buffer-record structure at the time they are available (when
;;;   the clock changes or the run terminates).
;;; 
;;; :buffer-trace-colors default: nil
;;;   Can be set to a list of (Tcl/Tk) colors to use for the display of the
;;;   graphic buffer traces.  The order of the colors corresponds to the order of the
;;;   :traced-buffers list (the first buffer listed will be drawn in the first
;;;   color found in this list).  If :traced-buffers is t, then this parameter
;;;   has no effect. If this list, or any element of it is nil then a
;;;   default color will be chosen for the corresponding item.  The format for 
;;;   specifying a color is as a string with the first element being # and 
;;;   followed by the hex values for the red, green, and blue values respectively.
;;;   Each hex value can be either 1, 2, or 3 digits (corresponding to 8, 16, or 24 bit
;;;   color representations), and all must be represented the same way.  Examples of 
;;;   red would be:  "#f00" "#FF0000" or "#FFF000000".
;;;
;;; :graphic-column-widths default: nil
;;;   Can be set to a list of numbers which represent the width (in pixels) for the 
;;;   display of the buffer columns in the vertical graphic buffer trace.
;;;   The order of the values corresponds to the order of the :traced-buffers 
;;;   list (the first buffer listed will be drawn with a column as wide as the first
;;;   found in this list).  If :traced-buffers is t, then this parameter
;;;   has no effect. If this list, or any element of it is nil then a
;;;   default width will be chosen for the corresponding item.  The default
;;;   width of the columns is computed as (min 190 (/ 960 "number of buffers traced")).
;;;
;;;
;;; The following information is recorded at each event of the model and aggregated
;;; over all events at a given time:
;;; 
;;; Whether the module is busy
;;; Whether the module transitioned from busy to free at this time
;;; Whether the module is in an error
;;; Whether the buffer is full
;;; Whether the buffer is cleared
;;; Whether the chunk in the buffer is modified
;;;
;;; Whether a request is sent to the module
;;; Whether a new chunk is set in the buffer
;;; Any notes which are recorded using add-buffer-trace-notes.
;;;
;;; For the first 6, if the stated condition is true during any event at the
;;; current time the buffer record will indicate t.
;;;
;;; For the requests, each request overwrites any prior request recorded
;;; at that time.  The value recorded is a string of the chunk-type of the
;;; chunk-spec or the details string provided for the event if there was one.
;;; If a chunk is set into the buffer, then the name of that chunk is recorded,
;;; and only the last setting at a specific time is recorded.
;;; The notes are set to the string specified by add-buffer-trace-notes, but
;;; only the last note at a given time will be saved (they overwrite).
;;;
;;; The trace attempts to show all of that information in a textual format.  At each
;;; time step of the model (including extra time steps if needed for the trace-step)
;;; there will be a line of trace printed.  At the start of the line will be the
;;; time of the summary and for each buffer traced there will be a column of 
;;; information in the trace. In the column the first character will be "E" if 
;;; the module is in an error state or a space otherwise.  The second character
;;; will be a "." if there is currently a chunk in the buffer or a space if it
;;; is empty.  The rest of the column will show one of the following things
;;; in their order of priority (truncated to maintain the column width):
;;;   If there is a chunk set in the buffer the name of that chunk
;;;   If there is a request the request is shown between two "+" characters
;;;   If the buffer is modified it will show a series of "=" characters
;;;   If the buffer is cleared it will show a series of "-" characters
;;;   If the module is busy it will show a series of "*" characters
;;;   otherwise it will be filled with spaces.
;;;
;;; Here is an example of a trace when the following sgp is added to the demo2
;;; model (and the run time is reduced from 10 seconds to 1 second):  
;;; (sgp :buffer-trace t :buffer-trace-step .025 :traced-buffers (production goal visual-location visual manual))
;;;
;;;
#|
CG-USER(86): (do-experiment)
           |    PRODUCTION   |       GOAL      | VISUAL-LOCATION |      VISUAL     |      MANUAL     |
     0.000 |  +FIND-UNATTEN+ | .     GOAL      | .     LOC0      |                 |                 |
     0.025 |  ************** | .               | .               |                 |                 |
     0.050 |  +ATTEND-LETTE+ | .    =======    | .     LOC1      |                 |                 |
     0.075 |  ************** | .               | .               |                 |                 |
     0.100 |  ************** | .    =======    | .    -------    |  +MOVE-ATTENTI+ |                 |
     0.125 |                 | .               |                 |  ************** |                 |
     0.150 |                 | .               |                 |  ************** |                 |
     0.175 |                 | .               |                 |  ************** |                 |
     0.185 |  +ENCODE-LETTE+ | .               |                 | .     TEXT0     |                 |
     0.210 |  ************** | .               |                 | .               |                 |
     0.235 |  +   RESPOND  + | .    =======    |                 | .    -------    |                 |
     0.260 |  ************** | .               |                 |                 |                 |
     0.285 |  ************** | .    =======    |                 |                 |  +  PRESS-KEY + |
     0.310 |                 | .               |                 |                 |  ************** |
     0.335 |                 | .               |                 |                 |  ************** |
     0.360 |                 | .               |                 |                 |  ************** |
     0.385 |                 | .               |                 |                 |  ************** |
     0.410 |                 | .               |                 |                 |  ************** |
     0.435 |                 | .               |                 |                 |  ************** |
     0.460 |                 | .               |                 |                 |  ************** |
     0.485 |                 | .               |                 |                 |  ************** |
     0.510 |                 | .               |                 |                 |  ************** |
     0.535 |                 | .               |                 |                 |  ************** |
     0.560 |                 | .               |                 |                 |  ************** |
     0.585 |                 | .               |                 |                 |  ************** |
     0.610 |                 | .               |                 |                 |  ************** |
     0.635 |                 | .               |                 |                 |  ************** |
     0.660 |                 | .               |                 |                 |  ************** |
     0.685 |                 | .               |                 |                 |  ************** |
     0.710 |                 | .               |                 |  ************** |  ************** |
     0.735 |                 | .               |                 |  ************** |  ************** |
     0.760 |                 | .               |                 |  ************** |  ************** |
     0.770 |                 | .               |                 |E                |  ************** |
     0.795 |                 | .               |                 |E                |  ************** |
     0.820 |                 | .               |                 |E                |  ************** |
     0.835 |                 | .               |                 |E                |                 |
     0.860 |                 | .               |                 |E                |                 |
     0.885 |                 | .               |                 |E                |                 |
     0.910 |                 | .               |                 |E                |                 |
     0.935 |                 | .               |                 |E                |                 |
     0.960 |                 | .               |                 |E                |                 |
     0.985 |                 | .               |                 |E                |                 |
     0.985   ------                 Stopped because no events left to process 
"V"
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; get-current-buffer-trace
;;;
;;;    takes no parameters and returns a list of the buffer-record structures
;;;    collected since the save-buffer-trace parameter was set or nil if the
;;;    module isn't found.
;;;
;;;    The buffer-record structures are pretty raw - there're no special accessors
;;;    defined for picking them apart nor are the buffer-summary structures that
;;;    it contains made more user friendly at this point.
;;;
;;;    This can be used if one wants to use other display mechanisms to present
;;;    the data collected.
;;; 
;;; Because the data is presented raw in a saved summary and to the hook function, 
;;; these structures are also part of the API:
;;;
;;;  (defstruct buffer-record time-stamp buffers)
;;;  (defstruct buffer-summary name cleared busy busy->free error full modified request chunk-name notes)
;;;
;;;
;;; add-buffer-trace-notes
;;;
;;;    Takes two parameters which are the name of a buffer and any notes
;;;    to set for the buffer summary of that buffer at the current time.  
;;;
;;;    This can be used to augment the summaries on the fly, and would most likely
;;;    be called from an event hook that was watching for something that needed to
;;;    be noted.
;;;
;;;    There are no restrictions on what can be passed as notes.
;;;    
;;;    Calling this will create a maintenance event which records the notes at
;;;    the current time.
;;;
;;;    The graphic trace tools will display the notes when the mouse is placed over
;;;    the corresponding box in the trace (the notes will be printed using the Lisp
;;;    format string "~a").
;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Grown out of Scott's graphic module tracer and the old environment's PERT
;;; style trace.  The idea being that instead of looking at the specific actions
;;; of a module one can "watch" the buffers since they're the interface to
;;; the module.  As long as a module takes requests through the buffers and 
;;; responds to the state queries appropriately it can be monitored.
;;;
;;; The addition of the production buffer was necessary so that the procedural
;;; module could be queried and report "requests" (production firings) like
;;; any other module.  It is a bit strange, and not really a buffer of the
;;; theory (note it doesn't end in 'al') but may end up being so as work on
;;; meta-cognitive processing continues - being able to monitor the state 
;;; of the prodceural system may be an important thing to do.
;;;
;;; If there are multiple models running with the buffer trace turned on one
;;; will probably want to direct those models' outputs to different streams
;;; because the trace doesn't make any effort to differentiate which model a
;;; summary line corresponds to (unlike the event trace which prints the model
;;; name at the start).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Because the event hooks are applied at the meta-process level instead of
;;; at the model level there needs to be something outside of the module instance
;;; to track setting/removing the hooks for efficiency.  However, at this
;;; time, a quick and dirty approach is being used which essentially just
;;; ignores the need for such a thing, but it's here so that it's ready when/if
;;; I decide to come back and clean it up.

(defvar *buffer-trace-module-mp-table* (make-hash-table ))


(defstruct (buffer-trace-module (:conc-name btm-))
  trace 
  buffers 
  save 
  hooks
  time-step
  column-width
  graphic-column-widths
  colors
  traced-buffers
  enabled
  current-summary 
  saved-records
  next-step-time)

(defstruct buffer-record
  time-stamp
  buffers)


(defstruct buffer-summary
  name
  cleared
  busy
  busy->free
  error
  error->clear
  full
  modified
  request
  chunk-name
  notes)


(defun buffer-trace-time-step-event ()
  )

(defun disable-event-trace (evt)
  (declare (ignore evt))
  nil)



(defun format-buffer-record (br w)
  (with-output-to-string (s)
    (format s "~10,3f " (buffer-record-time-stamp br))
    
    (dolist (x (buffer-record-buffers br))
      
      (format s "|~:[ ~;E~]~:[ ~;.~]~v:@<~a~> " 
        (buffer-summary-error x) 
        (buffer-summary-full x)
        (1- w)
        
        (cond ((buffer-summary-chunk-name x)
               (if (>= (length (buffer-summary-chunk-name x)) w)
                   (subseq (buffer-summary-chunk-name x) 0 (1- w))
                 (buffer-summary-chunk-name x)))
              
              ((buffer-summary-request x)
               (if (> (length (buffer-summary-request x)) (- w 3))
                   (format nil "+~v:@<~a~>+" (- w 3) (subseq (buffer-summary-request x) 0 (- w 3)))
                 (format nil "+~v:@<~a~>+" (- w 3) (buffer-summary-request x))))
              ((buffer-summary-modified x)
               (format nil "~v:@<~v,1,0,'=a~>" (1- w) (floor w 2) ""))
              ((buffer-summary-cleared x)
               (format nil "~v:@<~v,1,0,'-a~>" (1- w) (floor w 2) ""))
              ((buffer-summary-busy x)
               (format nil "~v,1,0,'*a" (1- w) ""))
              (t
               (format nil "~va" (1- w) "")))))
    (format s "|")
    (model-output (get-output-stream-string s))))

(defun buffer-trace-event-recorder (evt)
  (let ((btm (get-module buffer-trace))
        (new nil))
    
 
    (when (and btm (btm-enabled btm))
      
      (if  (eq (evt-action evt) 'buffer-trace-time-step-event)
          (setf (btm-next-step-time btm) nil)
        (when (and (numberp (btm-time-step btm))
                   (btm-next-step-time btm)
                   (< (ms-round (- (evt-time (btm-next-step-time btm)) (evt-time evt))) (btm-time-step btm)))
          
          (setf new t) ;; need to generate a new one...
          (delete-event (btm-next-step-time btm))
          (setf (btm-next-step-time btm) nil)))
      
      
      
      (when (null (btm-current-summary btm))
        (when (btm-trace btm)
          (with-output-to-string (s)
            (format s "           ")
            (dolist (x (btm-traced-buffers btm))
              (format s "| ~v:@<~a~> " (btm-column-width btm) x))
            (format s "|")
            
            (model-output (get-output-stream-string s))
            ))
        
        ;; create a new one and set as current
        (setf (btm-current-summary btm)
          (make-buffer-record :time-stamp (evt-time evt)))
        (setf (buffer-record-buffers (btm-current-summary btm))
          (mapcar (lambda (x) (make-buffer-summary :name x))
            (btm-traced-buffers btm)))
        
        (setf new t))
      
      (unless (= (buffer-record-time-stamp (btm-current-summary btm)) (evt-time evt))
        
        (dolist (hook (btm-hooks btm))
          (funcall hook (btm-current-summary btm)))
        
        (when (btm-trace btm)
          (format-buffer-record (btm-current-summary btm) (btm-column-width btm)))
        
        (when (btm-save btm)
          (push-last (btm-current-summary btm) (btm-saved-records btm)))
        
        
        (setf (btm-current-summary btm)
          (make-buffer-record :time-stamp (evt-time evt)))
        
        (setf (buffer-record-buffers (btm-current-summary btm))
          (mapcar (lambda (x) (make-buffer-summary :name x))
            (btm-traced-buffers btm)))
        
        )
      
      ;; Update the records
      
      ;; First pull any meaningful info out of the evt itself
      
      (case (evt-action evt)
        ((set-buffer-chunk overwrite-buffer-chunk)
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-chunk-name it) (string (second (evt-params evt))))))
           
           
           )
        (mod-buffer-chunk
         
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-modified it) t)))         
         )
        (clear-buffer
         
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-cleared it) t)))
         
         )
        (module-request
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-request it)
                    (if (and (>= (length (evt-details evt)) 15)
                             (string-equal "module-request " (subseq (evt-details evt) 0 15)))
                        (string (chunk-spec-chunk-type (second (evt-params evt))))
                      (evt-details evt)))))
         
         )
        (record-buffer-trace-notes
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-notes it)
                    (second (evt-params evt)))))
         )
        
        (module-mod-request
         (let ((bn (car (evt-params evt))))
           (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key #'buffer-summary-name)
                  (setf (buffer-summary-request it)
                    (if (and (>= (length (evt-details evt)) 19)
                             (string-equal "module-mod-request " (subseq (evt-details evt) 0 19)))
                        "buffer modify"
                      (evt-details evt)))))

         ))
      
      ;; Now for each one set busy, error, and full
      
      (dolist (x (buffer-record-buffers (btm-current-summary btm)))
        (if (query-buffer (buffer-summary-name x) '((state . busy)))
            (setf (buffer-summary-busy x) t)
          (when (buffer-summary-busy x)
              (setf (buffer-summary-busy->free x) t)))
        (if (query-buffer (buffer-summary-name x) '((state . error)))
            (setf (buffer-summary-error x) t)
          (when (buffer-summary-error x)
              (setf (buffer-summary-error->clear x) t)))
        (when (query-buffer (buffer-summary-name x) '((buffer . full)))
          (setf (buffer-summary-full x) t)))
      
      ;; Now, just check to see if it should stop or add a time-step check event
      
      (if (or (act-r-break-event-p evt)
              (eq 'run-terminated (evt-action evt)))
          ;; This is a terminating event
          
          (progn
            (dolist (hook (btm-hooks btm))
              (funcall hook (btm-current-summary btm)))
            
            (when (btm-trace btm)
              (format-buffer-record (btm-current-summary btm) (btm-column-width btm)))
            
            (when (btm-save btm)
              (push-last (btm-current-summary btm) (btm-saved-records btm)))
            
            
            (setf (btm-current-summary btm) nil)
            
            ;; kill any pending time-step-events...
            (when (btm-next-step-time btm)
              (delete-event (btm-next-step-time btm)))
            )
        
        ;; not a terminator, so check to see if a time-step event is necessary
        (when (and (or new 
                       (eq (evt-action evt) 'buffer-trace-time-step-event))
                   (numberp (btm-time-step btm))
                   (null (btm-next-step-time btm)))
          (setf (btm-next-step-time btm)
            (schedule-event-relative (btm-time-step btm)
                                     'buffer-trace-time-step-event
                                     :maintenance t
                                     :output nil
                                     :details nil
                                     :priority :max)))))))


(defun get-current-buffer-trace ()
  (let ((btm (get-module buffer-trace)))
    (when btm
      (btm-saved-records btm))))


(defun record-buffer-trace-notes (buffer notes)
  "Dummy function for recording event"
  (declare (ignore buffer notes)))

(defun add-buffer-trace-notes (buffer notes)
  (schedule-event-relative 0 'record-buffer-trace-notes
                           :maintenance t :priority :max
                           :output nil :params (list buffer notes)))
                           

(defun reset-buffer-trace-module (btm)
  (setf (btm-enabled btm) nil)
  (setf (btm-current-summary btm) nil)
  (setf (btm-saved-records btm) nil)
  (setf (btm-traced-buffers btm) nil)
  (setf (btm-next-step-time btm) nil))

(defun buffer-trace-params (btm param)
  (cond ((consp param)
         
         (case (car param)
           (:traced-buffers 
            (if (eq t (cdr param))
                (progn
                  (setf (btm-traced-buffers btm) (sort (buffers) #'string< :key #'symbol-name))
                  (setf (btm-buffers btm) t))
              (progn
                (setf (btm-buffers btm) (cdr param))
                (setf (btm-traced-buffers btm) (cdr param))))
            
            (setf (btm-column-width btm)
              (apply 'max (mapcar #'(lambda (x) (length (symbol-name x))) (btm-traced-buffers btm))))
            (btm-buffers btm))
           
           (:buffer-trace-step 
            (setf (btm-time-step btm) (cdr param)))
           
           (:buffer-trace-colors 
            (setf (btm-colors btm) (cdr param)))
           (:graphic-column-widths 
            (setf (btm-graphic-column-widths btm) (cdr param)))
           
           (:buffer-trace 
            (setf (btm-trace btm) (cdr param))
            (setf (btm-enabled btm)
              (or (btm-save btm) (btm-trace btm) (btm-hooks btm)))
            (when (btm-enabled btm)
              ;; eventually will need to record this for later removal
              (add-post-event-hook 'buffer-trace-event-recorder nil))
            
            ;; Should check to see if it's overwriting one but for now
            ;; just smash it.
            
            (if (cdr param)
                (no-output (sgp-fct (list :trace-filter 'disable-event-trace)))
              (no-output (sgp-fct (list :trace-filter nil)))))
           
           (:save-buffer-trace 
             (setf (btm-save btm) (cdr param))
            (setf (btm-enabled btm)
              (or (btm-save btm) (btm-trace btm) (btm-hooks btm)))
            (when (btm-enabled btm)
              ;; eventually will need to record this for later removal
              (add-post-event-hook 'buffer-trace-event-recorder nil)))
           
           (:buffer-trace-hook
            (if (cdr param)
                (if (member (cdr param) (btm-hooks btm))
                    (print-warning 
                      "Setting parameter ~s failed because ~s already on the hook."
                     :buffer-trace-hook
                      (cdr param))
                  (push (cdr param) (btm-hooks btm)))
              (setf (btm-hooks btm) nil))
            
            (setf (btm-enabled btm)
              (or (btm-save btm) (btm-trace btm) (btm-hooks btm)))
            (when (btm-enabled btm)
              ;; eventually will need to record this for later removal
              (add-post-event-hook 'buffer-trace-event-recorder nil)))))
        (t 
         (case param
           (:buffer-trace-hook (btm-hooks btm))
           (:save-buffer-trace (btm-save btm))
           (:buffer-trace-colors (btm-colors btm))
           (:graphic-column-widths (btm-graphic-column-widths btm))
           (:buffer-trace-step (btm-time-step btm))
           (:traced-buffers (btm-buffers btm))
           (:buffer-trace (btm-trace btm))))))



(define-module-fct 'buffer-trace nil
  (list (define-parameter :buffer-trace
          :valid-test #'tornil 
          :warning "t or nil."
          :default-value nil
          :documentation "Display the trace as a buffer summary instead of as an event list.")
        (define-parameter :traced-buffers
          :valid-test #'(lambda (x) 
                          (or (eq t x) 
                              (and (listp x) 
                                   (every (lambda (y) (find y (buffers))) x))))
          :warning "t or a list of valid buffer names."
          :default-value t
          :documentation "The list of buffers to be traced (all buffers if set to t).")
        (define-parameter :buffer-trace-step
          :valid-test #'posnumornil
          :warning "a positive number or nil."
          :default-value nil
          :documentation "The maximum amount of time allowed to elapse before creating a buffer summary.")
        
        (define-parameter :buffer-trace-colors
            :valid-test #'(lambda (x) (and (listp x)
                                           (every (lambda (y) 
                                                    (or (null y)
                                                        (and (stringp y)
                                                             (or (= (length y) 4)
                                                                 (= (length y) 7)
                                                                 (= (length y) 10))
                                                             (char-equal #\# (aref y 0)))))
                                                  x)))
          :warning "a list of color strings or nil."
          :default-value nil
          :documentation "The colors used to draw the buffer data using the graphic tracing tool.")
        
        (define-parameter :graphic-column-widths
            :valid-test #'(lambda (x) (and (listp x)
                                           (every #'posnumornil x)))
          :warning "a list of positive numbers or nil."
          :default-value nil
          :documentation "The pixel width of the columns drawn for the buffers using the graphic tracing tool.")
        
        
        (define-parameter :save-buffer-trace
          :valid-test #'tornil 
          :warning "t or nil."
          :default-value nil
          :documentation "Whether to save the buffer summary for a run or not.")
        (define-parameter :buffer-trace-hook
          :valid-test #'fctornil
          :warning "a function or nil."
          :default-value nil
          :documentation "A function to call with each buffer summary."))
  :version "1.0"
  :documentation "A module that provides a buffer based tracing mechanism."
  :creation #'(lambda (x) (declare (ignore x)) (make-buffer-trace-module))
  :reset #'reset-buffer-trace-module
  :params #'buffer-trace-params
  ; :delete - eventually want to worry about coming off of the
  ; event-hook list, but not at this point.
   )
            

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
