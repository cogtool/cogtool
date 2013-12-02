;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2007 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : env-graphic-trace.lisp
;;; Version     : 1.0
;;; 
;;; Description : Support for a quick and dirty graphic display of the 
;;;               buffer trace info through the environment connection.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Check that all module state changes get flagged correctly
;;;             :     for "unusual" modules and situations.
;;; 
;;; ----- History -----
;;;
;;; 2007.04.30 Dan
;;;             : Initially added it to the archive.
;;; 2007.05.21 Dan
;;;             : * Modified the vertical trace to be much faster because
;;;             :   it sends all the data in one update which the environment
;;;             :   parses as a list for displaying.  Will change the horizontal 
;;;             :   once testing verifies that the vertical is stable like this).
;;; 2007.05.22 Dan
;;;             : * Reference the parameters that were added to the buffer trace
;;;             :   module for display purposes.
;;;             : * Made the horizontal trace use the fast transmit mechanism.
;;;             : * Fixed the default color list referencing so that it wraps
;;;             :   around if needed.
;;; 2007.06.06 Dan
;;;             : * Added the use of error->clear to the buffer summary parsing
;;;             :   to better handle requests that occur immediately following
;;;             :   an error (retrieval failures had some oddities in the trace).
;;; 2007.07.30 Dan
;;;             : * Adding a new production parameter called color and then
;;;             :   using that to set the color in the graphic traces if set to
;;;             :   a valid color string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; To use this control in the environment you must set the :save-buffer-trace 
;;; parameter to t explicitly in the model.
;;;
;;; Then, whenever the model is run, one of the graphic trace tools on the "Control
;;; Panel" can be opened and the display button will draw a graphic trace of the
;;; model's most recent run (from time 0 until the time at which the display is requested).  
;;;
;;; It may take a while to draw the trace because each box has to be requested
;;; and sent through the socket (eventually that could be simplified, but for now
;;; that's how it's going to be).  While the drawing is going on it will show
;;; "Busy" in the lower left corner of the window, and when it finishes it will
;;; show "Done".
;;;
;;; The details will be shown for all of the buffers which are set with the 
;;; :traced-buffers parameter (the default is all buffers).  They will be shown
;;; in the order specified (left->right for the vertical trace or top->bottom
;;; for the horizontal trace).
;;;
;;; In the graphic trace the box is drawn when the buffer reports that the 
;;; module is busy, whenever a request is sent through the buffer, or whenver a
;;; chunk is placed into the buffer.  The request is drawn at the top of the 
;;; box and the chunk placed into the buffer is drawn at the bottom (if there is
;;; such a chunk).  The one exception to that is 0-time events (visual-location
;;; and goal requests for example).  For 0-time events only the chunk name is 
;;; shown after the line representing the event in the vertical trace.
;;;
;;; If you place the mouse cursor over a box (or the chunk name if it is outside
;;; of the box like a 0-time event will be) in the trace it will show the start
;;; and stop times for that box in the lower left corner of the display window.
;;;
;;; The "+" and "-" buttons in the control can be used to rescale the image along
;;; the time dimension.
;;; 
;;; The "Remove Text" button will erase all of the request and chunk names from
;;; the image.  The only way to restore them after that is to redisplay the
;;; whole thing.
;;;
;;; The save button will write the image out to a Postscript file because that's the
;;; easy default available in Tk.  There are lots of ps->pdf converters out there
;;; so hopefully that's not an issue for people, but eventually it could be made
;;; to generate something "nicer" if people demand it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; None (accessed through the environment side viewer).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Quick and dirty - the rectangle drawing info is actually created on the Lisp
;;; side and pushed to Tk for drawing.  A much nicer mechanism would be to just
;;; send the descriptive info so that more display control would be available on
;;; the environment side (like stretching columns or reordering things). 
;;; If there's a signifigant demand (and time) at some point this could be made
;;; very nice, but that'll require modifications to the environment server control
;;; code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;; Give the option to color the productions
(extend-productions color)

(defparameter *vert-trace-list* nil)
(defparameter *horiz-trace-list* nil)
(defvar *vert-started* nil)
(defvar *horiz-started* nil)

(defun vert-graphic-trace-return (x)
  (declare (ignore x))
  (let ((data (parse-trace-list (get-current-buffer-trace) :vertical))
        (result nil))
    (push (list 'size 0 (* 1000 (mp-time))) data)
    (dolist (x data)
      (push-last (format nil "~{~S ~}" x) result))
    result))

(defun hor-graphic-trace-return (x)
  (declare (ignore x))
  (let ((data (parse-trace-list (get-current-buffer-trace) :horizontal))
        (result nil))
    (push (list 'size 0 (* 1000 (mp-time))) data)
    (dolist (x data)
      (push-last (format nil "~{~S ~}" x) result))
    result))
  #|
  (if *horiz-trace-list*
      (progn
        (setf *horiz-started* t)
        (pop *horiz-trace-list*))
    (if *horiz-started*
        (progn
          (setf *horiz-started* nil)
          '(finished))
      (progn
        (setf *horiz-trace-list* (parse-trace-list (get-current-buffer-trace ) :horizontal))
        (list 'size 0 (* 1000 (mp-time)))))))
|#

(defparameter *gt-colors* '("#ff0000" "#8080ff" "#008000" "#ff8000" "#7f7f7f" "#ff00ff" "#ffff00" 
                            "#c08000" "#f0f0f0" "#c000c0" "#00c050" "#f0c0ff" "#c05850"))

(defstruct gt-rect start end request chunk notes)

(defun create-color-list (colors buffers defaults)
  (let ((results (copy-list colors))
        (default-colors defaults))
    
    (when (< (length defaults) (length buffers))
      (dotimes (i (ceiling (length buffers) (length defaults)))
        (setf default-colors (append default-colors defaults))))
    
    (if colors
        (progn
          (dotimes (i (min (length colors) (length buffers)))
            (when (null (nth i results))
              (setf (nth i results) (nth i default-colors))))
          
          (when (< (length results) (length buffers))
            (setf results (append results default-colors)))
          results)
      
      default-colors)))

(defun create-widths-list (widths buffers defaults)
 (let ((results (copy-list widths))
        (default-widths defaults))
    
    (when (< (length defaults) (length buffers))
      (dotimes (i (ceiling (length buffers) (length defaults)))
        (setf default-widths (append default-widths defaults))))
    
    (if widths
        (progn
          (dotimes (i (min (length widths) (length buffers)))
            (when (null (nth i results))
              (setf (nth i results) (nth i default-widths))))
          
          (when (< (length results) (length buffers))
            (setf results (append results default-widths)))
          results)
      
      default-widths)))                   

(defun parse-trace-list (trace dir)
  (let* ((b (no-output (car (sgp :traced-buffers))))
         (buffers (if (listp b) b (buffers)))
         (y 0)
         (y-inc (min 50 (floor (/ 380 (length buffers)))))
         (x-coord 0)
         (x-inc (min 190 (floor (/ 960 (length buffers)))))
         (all-data nil)
         (colors (no-output (car (sgp :buffer-trace-colors))))
         (widths (no-output (car (sgp :graphic-column-widths))))
         (color-list (create-color-list (if (and (listp b) colors) colors nil) buffers *gt-colors*))
         (widths-list (create-widths-list (if (and (listp b) widths) widths nil) buffers (make-list (length buffers) :initial-element x-inc))))
    
    (let ((buffer-index 0))
    (dolist (x buffers)
      ;(format t "Buffer: ~S~%" x)
      (let ((rects nil)
            (current-rect nil))
          (dolist (z trace)
          ;(format t "Record: ~S~%" z)
                      
          (let ((record (find x (buffer-record-buffers z) :key 'buffer-summary-name)))
            ;(format t "Time: ~6,3f   summary: ~S~%" (buffer-record-time-stamp z)  record)
            (if current-rect
                (progn
                  (awhen (buffer-summary-chunk-name record)
                         (setf (gt-rect-chunk current-rect) it))
                  
                  (awhen (buffer-summary-notes record)
                         (setf (gt-rect-notes current-rect) it))
                  
                  (when (or (null (buffer-summary-busy record))
                            (buffer-summary-busy->free record)
                            (buffer-summary-request record))
                    
                    (setf (gt-rect-end current-rect) (buffer-record-time-stamp z))
                    (push current-rect rects)
                    (if (buffer-summary-request record)
                        (setf current-rect (make-gt-rect :start (buffer-record-time-stamp z)
                                                         :request (buffer-summary-request record)))
                      (setf current-rect nil))))
              
              (if (buffer-summary-busy record)
                  (if (and (buffer-summary-request record) 
                           (or (buffer-summary-chunk-name record)
                               (and (buffer-summary-error record)
                                    (not (buffer-summary-error->clear record)))
                               (buffer-summary-busy->free record)))
                      (push (make-gt-rect :start (buffer-record-time-stamp z)
                                          :end (buffer-record-time-stamp z)
                                          :request (buffer-summary-request record)
                                          :chunk (buffer-summary-chunk-name record)
                                          :notes (buffer-summary-notes record))
                            rects)
                    (setf current-rect (make-gt-rect :start (buffer-record-time-stamp z)
                                                     :request (buffer-summary-request record)
                                                     :chunk (buffer-summary-chunk-name record)
                                                     :notes (buffer-summary-notes record))))
                (if (buffer-summary-request record)
                    (push (make-gt-rect :start (buffer-record-time-stamp z) :end (buffer-record-time-stamp z)
                                        :request (buffer-summary-request record)
                                        :chunk (buffer-summary-chunk-name record)
                                        :notes (buffer-summary-notes record))
                          rects)
                  (when (buffer-summary-chunk-name record)
                    (push (make-gt-rect :start (buffer-record-time-stamp z) :end (buffer-record-time-stamp z)
                                        :request (buffer-summary-request record)
                                        :chunk (buffer-summary-chunk-name record)
                                        :notes (buffer-summary-notes record))
                          rects)))))))
        
        (dolist (z rects)
          ;(pprint z)
          (if (eq dir :horizontal)
              (push (list 'rectangle (* 1000 (gt-rect-start z)) y (* 1000 (aif (gt-rect-end z) it (mp-time))) (+ y y-inc) 
                          
                          (if (and (eq x 'production)
                                   (production-color (read-from-string (gt-rect-request z)))
                                   (stringp (production-color (read-from-string (gt-rect-request z)))))
                              (production-color (read-from-string (gt-rect-request z)))
                            (nth buffer-index color-list))
                          
                          (gt-rect-request z) (gt-rect-chunk z) 
                          (if (gt-rect-notes z)
                              (format nil "~a" (gt-rect-notes z))
                            nil))
                    all-data)
            
            (push (list 'rectangle x-coord (* 1000 (gt-rect-start z)) 
                        (+ x-coord (nth buffer-index widths-list)) (* 1000 (aif (gt-rect-end z) it (mp-time)))  
                                                
                        (if (and (eq x 'production)
                                   (production-color (read-from-string (gt-rect-request z)))
                                   (stringp (production-color (read-from-string (gt-rect-request z)))))
                              (production-color (read-from-string (gt-rect-request z)))
                            (nth buffer-index color-list)) 
                        
                        (gt-rect-request z) (gt-rect-chunk z) 
                        (if (gt-rect-notes z)
                              (format nil "~a" (gt-rect-notes z))
                            nil))
                    all-data)))
      
        (if (eq dir :horizontal)
            (push (list 'label x (+ y (floor (/ y-inc 2))) (nth buffer-index color-list)) all-data)
          (push (list 'label x (+ x-coord (floor (/ (nth buffer-index widths-list) 2))) (nth buffer-index color-list) (nth buffer-index widths-list)) all-data)))
        
            
      (incf y y-inc)
      (incf x-coord (nth buffer-index widths-list))
      (incf buffer-index)))
    
     all-data))


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
