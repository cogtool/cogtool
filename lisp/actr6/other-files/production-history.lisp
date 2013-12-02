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
;;; Filename    : production-history.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code to support the production trace tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider making a module for this to allow configuration
;;;             :     of the display (colors, widths, restricted productions, 
;;;             :     etc) and provide a simple parameter switch to enable.
;;; 
;;; ----- History -----
;;; 2008.08.06 Dan
;;;             : * Initial creation.
;;; 2008.08.06 Dan
;;;             : * Making it a module and doing some optimizing on how it
;;;             :   stores/creates the data to send because it can't handle 
;;;             :   long runs (zbrodoff tutorial model often results in an
;;;             :   allocation error).
;;; 2008.08.07 Dan
;;;             : * Another stability improvement.  It now breaks the grid
;;;             :   up into smaller updates to send over to the environment
;;;             :   to avoid having to create one very large string.  Slows
;;;             :   down the displaying, but shouldn't result in any allocation
;;;             :   issues.
;;; 2008.08.12 Dan
;;;             : * Added the :p-history-colors parameter to allow one to
;;;             :   change the colors used in the grid.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Put this file into the other-files directory and the corresponding .tcl file
;;; into the environment/GUI/dialogs directory to use the new tool.
;;;
;;; Open the production history window before running the model or set the
;;; :save-p-history parameter to t in the model to enable the recording.
;;; 
;;; Once the model has run click the "Get history" button in the bottom left
;;; corner of the production history window.  It will draw a grid with the
;;; rows being the productions in the model and the columns being the times at
;;; which a conflict-resolution event occurred.
;;; The cells in the grid are color coded based on what happened during the 
;;; conflict resolution event at the time indicated for the column.
;;; 
;;; If the production was the one selected the cell will be green.
;;; If the production was in the conflict set, but not selected then the cell
;;; will be orange.
;;; If the production was not in the conflict set then the cell will be red.
;;; If the production did not exist at that time the cell will be white.
;;;
;;; Placing the cursor over a cell will cause some details for that production
;;; during that conflict resolution event to be printed at the bottom of the
;;; window.
;;;
;;; For the green and orange cells it will print the utility value for the
;;; production at that time.  For the red cells it will print the whynot 
;;; information indicating the condition that caused the production to not be
;;; in the conflict set.  There is no extra information to print for a white
;;; cell.
;;;
;;; Clicking on the name of a production in the grid will open a new procedural
;;; viewer dialog with that production selected.
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-p-history parameter
;;;  Enables the recording of production history for display (default is nil).
;;;
;;; :draw-blank-columns parameter
;;;  When set to t (the default value) all conflict resolution events get drawn
;;;  in the environment tool.  If set to nil then conflict resolution events
;;;  which had a null conflict set don't get drawn.
;;; 
;;; :p-history-colors
;;;  This parameter can be used to change the colors displayed in the grid.
;;;  If it is nil (the default) then the green, orange, and red defaults are 
;;;  used for selected, conflict set, and mismatched respectively.  It can be
;;;  set to a list of 3 values where each value is either a color string or nil.
;;;  A valid color string starts with the character # and is followed by 3, 6,
;;;  9 hex digits.  Those digits represent the components of the color to use
;;;  and specify the Red, Green, and Blue values respectively using the same
;;;  number of bits for each (thus either 8, 16, or 24 bits per color).  An
;;;  example would be "#00F" for blue or "#44DA22" which is the green color 
;;;  used by default.
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

(defstruct p-history-module
  history
  enabled
  why-not-list
  draw-blanks
  current-data
  color-list)
  

(defstruct p-history
  time
  selected
  matched
  mismatched
  info)

(defun production-history-recorder (cs)
  (let* ((history (get-module production-history))
         (best (car cs))
         (mismatched (set-difference (all-productions) cs))
         (block (make-p-history :mismatched mismatched :time (mp-time))))
    (no-output
     (let ((ut (sgp :ut)))
       (when (and best 
                  (or (not (numberp ut))
                      (and (numberp ut) (>= (caar (spp-fct (list best :utility))) ut))))
         (setf (p-history-selected block) best))
       (dolist (x cs)
         (push (cons x (caar (spp-fct (list x :utility))))
               (p-history-info block)))
       
       (dolist (x mismatched)
         (let* ((reason (production-failure-reason x))
                (index (position reason (p-history-module-why-not-list history) :test #'string-equal)))
           (unless index
             (setf index (length (p-history-module-why-not-list history)))
             (push-last reason (p-history-module-why-not-list history)))
           (push (cons x index) (p-history-info block)))))
     
    (if (p-history-selected block)
        (setf (p-history-matched block)
          (cdr cs))
        (setf (p-history-matched block)
          cs))
      
    (push block (p-history-module-history history))
    nil)))

(defun production-history-graph-data (item)
  (declare (ignore item))
  (let ((history (get-module production-history)))
    
    (when (null (p-history-module-current-data history))
      (parse-production-history-graph-data))
    
    (let ((data (p-history-module-current-data history)))
      (if (> (length data) 200)
          (let ((results (subseq data 0 200)))
            (setf (p-history-module-current-data history) (subseq data 200))    
            (mapcar (lambda (x) (format nil "~{~S ~}" x)) results))
        (progn
          (setf (p-history-module-current-data history) nil)    
          (mapcar (lambda (x) (format nil "~{~S ~}" x)) (push (list 'done) data)))))))

(defun parse-production-history-graph-data ()
  (let* ((results nil)
         (history (get-module production-history))
         (p-names (all-productions))
         (columns 0)
         (name-size (apply 'max (mapcar (lambda (x) (length (symbol-name x))) p-names))))
    
    
    (dolist (x (p-history-module-history history))
      (when (or (p-history-module-draw-blanks history)
                (or (p-history-selected x) (p-history-matched x)))
        (let ((col (list 'column (p-history-time x))))
          (dolist (y p-names)
            
            (cond ((eq y (p-history-selected x))
                   (push-last 0 col)
                   (push-last (cdr (assoc y (p-history-info x))) col))  ;;"#44DA22"
                  ((find y (p-history-matched x))
                   (push-last 1 col)
                   (push-last (cdr (assoc y (p-history-info x))) col))  ;;"#FCA31D"                
                  ((find y (p-history-mismatched x))
                   (push-last 2 col)
                   (push-last (cdr (assoc y (p-history-info x))) col)))) ;;"#E1031E"                
          (incf columns)
          (push col results))))
    
    (push (cons 'labels p-names) results)
    (push (list 'colors 
                (aif (nth 0 (p-history-module-color-list history)) it "#44DA22")
                (aif (nth 1 (p-history-module-color-list history)) it "#FCA31D")
                (aif (nth 2 (p-history-module-color-list history)) it "#E1031E"))
          results)
    (push (cons 'reasons (p-history-module-why-not-list history)) results)
    (push (list 'size (* 20 (1+ (length p-names))) 20  (* 9 name-size) 80 (* 80 columns)) results)
    
    (setf (p-history-module-current-data history) results)
    nil))


(defun reset-p-history-module (module)
  (setf (p-history-module-history module) nil)
  (setf (p-history-module-why-not-list module) nil))
  
(defun params-p-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-p-history 
          (no-output
           (progn
             (if (cdr param)
                (no-output 
                 (unless (find 'production-history-recorder (car (sgp :conflict-set-hook)))
                   (sgp :conflict-set-hook production-history-recorder)))
              
               (when (find 'production-history-recorder (car (sgp :conflict-set-hook)))
               (let ((old-hooks (car (sgp :conflict-set-hook))))
                 (sgp :conflict-set-hook nil)
                 (dolist (x old-hooks)
                   (unless (eq x 'production-history-recorder)
                     (sgp-fct (list :conflict-set-hook x)))))))
          
             (setf (p-history-module-enabled instance) (cdr param)))
           ))
        (:p-history-colors 
         (setf (p-history-module-color-list instance) (cdr param)))
        (:draw-blank-columns 
          (setf (p-history-module-draw-blanks instance) (cdr param))))
    (case param
      (:save-p-history (p-history-module-enabled instance))
      (:p-history-colors (p-history-module-color-list instance))
      (:draw-blank-columns (p-history-module-draw-blanks instance)))))

(define-module-fct 'production-history nil 
  (list (define-parameter :save-p-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the utility and whynot history of all conflict-resolution events.")
        (define-parameter :p-history-colors 
            :valid-test (lambda (x) 
                          (or (null x)
                              (and (listp x) (<= (length x) 3)
                                   (every (lambda (y) (or (null y) (stringp y))) x))))
          :default-value nil
          :warning "nil or a list of up to 3 color strings" 
          :documentation "The colors to use for the selected, other matched, and mismatched cells respectively.") 
        (define-parameter :draw-blank-columns :valid-test 'tornil :default-value t
          :warning "T or nil" 
          :documentation "Whether or not to draw the columns which have no matched productions."))
  :creation (lambda (x) (declare (ignore x)) (make-p-history-module))
  :reset 'reset-p-history-module
  :params 'params-p-history-module
  :version "1.0"
  :documentation "Module to record production history for display in the environment.")
  

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
