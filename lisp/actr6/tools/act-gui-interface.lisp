;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-gui-interface.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Contains the functions that implement the abstract GUI
;;;             : interface used by the tutorial units and the misc functions
;;;             : that go with them (permute-list, correlation and 
;;;             : mean-deviation).  I'm calling it the ACT-R GUI interface
;;;             : (AGI) as suggested by Mike.
;;;             : It relies on the UWI (at least for now).
;;; Bugs        : 
;;; To Do       : Consider making it support multiple interfaces to go with
;;;             : multiple models.
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Renamed this file from uniform-interface-exp to 
;;;             : act-gui-interface.
;;;             : Added comments.
;;; 2002.12.17 Dan
;;;             : Modified correlation and mean-deviation so that
;;;             : the output keyword parameter is "more useful" -
;;;             : specifying a stream works right now (it doesn't try to
;;;             : open a file for it) and specifying nil suppress
;;;             : all output.
;;; 2002.12.19 Dan
;;;             : Updated add-text-to-exp-window so that it now includes
;;;             : a color option.
;;; 04.04.13 Dan [2.2]  (previous two changes also "new" as of 2.2)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : reset version to 1.0a1
;;;             : added the packaging switches
;;;             : changed permute-list to use act-r-random
;;;
;;; 04.12.17 Dan
;;;             : Added get-time as a replacement for pm-get-time.
;;;   
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2006.09.07 Dan
;;;             : * Changed permute-list so that it's safe when passed nil or
;;;             :   a non-list.
;;; 2007.07.13 Dan
;;;             : * Added color as an option to add-button-to-exp-window.
;;; 2007.12.13 Dan
;;;             : * Adding an add-items-to-exp-window function to compliment
;;;             :   the remove-... and to avoid the need to call the UWI
;;;             :   function when such an action is necessary.
;;; 2009.09.10 Dan
;;;             : * Moved permute-list to the random module's file.
;;; 2010.03.08 Dan
;;;             : * Changed close-exp-window so that it checks to see if the
;;;             :   window is still there before trying to close it.  Avoids
;;;             :   a problem where a user has closed an environment side
;;;             :   window.
;;; 2010.07.31 Dan
;;;             : * Changed add-line-to-exp-window to constrain the x,y positions
;;;             :   to be fixnums to avoid problems in ACL and LispWorks if 
;;;             :   a float or rational is used instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; *LIBRARY-EXPERIMENT-WINDOW*  [Global Variable]
;;; Description : This variable is used to hold the window that's opened with
;;;             : the AGI function open-exp-window.

(defvar *library-experiment-window* nil "Global AGI window")


;;; GET-TIME
;;; Return time in milliseconds
;;; If the model is enabled use model time, otherwise use 
;;; get-internal-real-time (which means it's only meaningful as a relative
;;; time outside of the model).

(defun get-time ()
  (if *actr-enabled-p*
      (round (* 1000 (mp-time)))
    ;; just to be safe use internal-time-units-per-second
    (round (* 1000 (/ (get-internal-real-time) 
                      internal-time-units-per-second)))))
    
;;; OPEN-EXP-WINDOW  [Function]
;;; Description : This function opens a window, either real, virtual, or
;;;             : visible-virtual as requested.  If there's already a window
;;;             : with those specs open it's cleared and used.

(defun open-exp-window (title &key (width 300) (height 300) (visible t) 
                                  (x 300) (y 300))
  "Open an experiment window"
  (if (open-rpm-window? *library-experiment-window*)
      (if (and (string-equal title (rpm-window-title 
                                    *library-experiment-window*))
               (eql visible (rpm-window-visible-status 
                             *library-experiment-window*))) 
          (progn
            (remove-all-items-from-rpm-window *library-experiment-window*)
            *library-experiment-window*)
        (progn
          (close-exp-window)
          
          (setf *library-experiment-window* (make-rpm-window 
                                             :visible visible 
                                             :title title
                                             :width width 
                                             :height height
                                             :x x
                                             :y y))))
    (setf *library-experiment-window* (make-rpm-window 
                                       :visible visible 
                                       :title title
                                       :width width 
                                       :height height
                                       :x x
                                       :y y)))
  (select-rpm-window *library-experiment-window*)
  *library-experiment-window*)

;;; SELECT-EXP-WINDOW  [Function]
;;; Description : Brings the *library-experiment-window* to the front.

(defun select-exp-window ()
  "select the experiment window"
  (select-rpm-window *library-experiment-window*))

;;; CLOSE-EXP-WINDOW  [Function]
;;; Description : Closes the *library-experiment-window*.

(defun close-exp-window ()
  "Close the experiment window"
  (when *library-experiment-window*
    (close-rpm-window *library-experiment-window*)
    (setf *library-experiment-window* nil)))

;;; CLEAR-EXP-WINDOW  [Function]
;;; Description : Removes all items from *library-experiment-window*.

(defun clear-exp-window ()
  "Erases everything in the experiment window"
  (remove-all-items-from-rpm-window *library-experiment-window*))

;;; ADD-ITEMS-TO-EXP-WINDOW  [Function]
;;; Description : Adds the requested items into *library-experiment-window*.

(defun add-items-to-exp-window (&rest items)
  "Add the specified items to the experiment window"
  (apply #'add-visual-items-to-rpm-window 
         (cons *library-experiment-window* items)))

;;; REMOVE-ITEMS-FROM-EXP-WINDOW  [Function]
;;; Description : Removes the requested items from *library-experiment-window*.

(defun remove-items-from-exp-window (&rest items)
  "Remove the specified items from the experiment window"
  (apply #'remove-visual-items-from-rpm-window 
         (cons *library-experiment-window* items)))

;;; ADD-TEXT-TO-EXP-WINDOW  [Function]
;;; Description : Build a text item based on the parameters supplied and
;;;             : add it to *library-experiment-window*.

(defun add-text-to-exp-window (&key (x 0) (y 0) (text "") (height 20) 
                                    (width 75) (color 'black))
  "Create and display a text item in the experiment window"
 (let ((item (make-static-text-for-rpm-window 
                                   *library-experiment-window* 
                                   :text text 
                                   :x x
                                   :y y
                                   :width width
             	                   :height height
                                   :color color)))
   (add-visual-items-to-rpm-window *library-experiment-window* item)
   item))

;;; ADD-BUTTON-TO-EXP-WINDOW  [Function]
;;; Description : Build a button item based on the parameters supplied and
;;;             : add it to *library-experiment-window*.

(defun add-button-to-exp-window (&key (x 0) (y 0) (text "Ok") 
                                          (action nil) (height 18) 
                                          (width 60) (color 'gray))
  "Create and display a button item in the experiment window"
  (let ((item (make-button-for-rpm-window *library-experiment-window*
                                                              :x x
                                                              :y y
                                                              :text text
                                                              :action action
                                                              :height height
                                          :width width :color color)))
    (add-visual-items-to-rpm-window *library-experiment-window* item)
    item))

;;; ADD-LINE-TO-EXP-WINDOW  [Function]
;;; Description : Build a line item based on the parameters supplied and
;;;             : add it to *library-experiment-window*.

(defun add-line-to-exp-window (start-pt end-pt &optional (color 'black))
  "Create and display a line item in the experiment window"
  (let ((item (make-line-for-rpm-window *library-experiment-window*
                                        (mapcar 'round start-pt) (mapcar 'round end-pt) color)))
    (add-visual-items-to-rpm-window *library-experiment-window* item)
    item))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; The miscelaneous functions used in the tutorial.
;;;; ---------------------------------------------------------------------- ;;;;



;;; This is the correlation and deviation functions from the scripting
;;; extensions file and the necessary support.  I figured since they are
;;; still used they should be put here because the scripting extensions 
;;; aren't part of ACT-R 5, but making people load the scripting file
;;; separately is a pain...  I also changed mean-deviation so that it
;;; actually returned the deviation.

(defstruct data labels array)

(defmacro /-safe (number &rest dividers)
  `(/ ,number ,@(let ((max nil))
                  (dolist (divider dividers max)
                    (push-last `(if (zerop ,divider) 1 ,divider) max)))))

(defun numbers-list (structure)
  (let ((list nil))
    (when (data-p structure) (setf structure (data-array structure)))
    (cond ((arrayp structure)
           (dotimes (i (array-total-size structure))
             (let ((data (row-major-aref structure i)))
               (when (numberp data) (push data list)))))
          ((listp structure)
           (dolist (data structure)
             (cond ((listp data)
                    (setf list (append (nreverse (numbers-list data)) list)))
                   ((numberp data)
                    (push data list)))))
          ((numberp structure)
           (push structure list))
          (t (format t "~&UNKNOWN DATA FORMAT ~S NOT COMPATIBLE WITH NUMBERS LIST.~%"
                     structure)))
    (nreverse list)))

(defun square-data (x)
  (* x x))

(defun sum-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum data))))

(defun square-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum (square-data data)))))

(defun product-list (list1 list2)
  (let ((sum 0.0))
    (loop
      (when (or (null list1) (null list2)) (return sum))
      (incf sum (* (pop list1) (pop list2))))))

(defun mean-deviation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO MEAN-DEVIATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (sqrt (/ (+ (square-list results-list) (square-list data-list)
                              (* -2.0 (product-list results-list data-list)))
                           n))))
      (format output "~&MEAN DEVIATION: ~6,3F~%" result)
      (when opened (close output))
      
      result)))

(defun correlation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (average-results (/-safe (sum-list results-list) n))
         (average-data (/-safe (sum-list data-list) n))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO CORRELATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (/-safe (- (/-safe (product-list results-list data-list) n)
                       (* average-results average-data))
                    (* (sqrt (- (/-safe (square-list results-list) n)
                                (square-data average-results)))
                       (sqrt (- (/-safe (square-list data-list) n)
                                (square-data average-data)))))))
      (format output "~&CORRELATION: ~6,3F~%"
            result)
    (when opened (close output))
      result)))


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
