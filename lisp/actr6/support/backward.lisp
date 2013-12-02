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
;;; Filename    : backward.lisp
;;; Version     : 1.0
;;; 
;;; Description : Maps ACT-R 5 functions to the ACT-R 6 counterpart.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2005.01.12 Dan
;;;             : File creation.
;;; 2005.01.26 Dan
;;;             : * Added commands from ACT-R 4/5 related to declarative
;;;             :   memory that have either been renamed or just depricated.
;;; 2005.05.02 Dan
;;;             : * Moved some commands from motor to here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; If one wants or needs the old names for commands then just call this:
;;; (require-compiled "BACKWARD" "ACT-R6:support;backward")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; All of them...
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



;;; Commands for the device interface

(defun pm-install-window (window)
  "Installs <window> as the action window for the PM layer.  Included purely fo backward compatibility only. Use INSTALL-DEVICE instead."
  (install-device window))

(defun pm-install-device (device)
  "Installs <device> as the active device for the perceptual-motor layer."
  (install-device device))

(defun pm-proc-display (&key clear)
  "Processes the current display."
  (process-display (current-device-interface) (get-module :vision) clear))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands from declarative memory

(defmacro add-ia (&rest settings)
  `(add-sji-fct ',settings))

(defun add-ia-fct (settings)
  (add-sji-fct settings))


(defmacro ia (chunkj chunki)
  "ACT-R 5 function to get IA value"
  `(sji-fct ',chunkj ',chunki))

(defun ia-fct (chunkj chunki)
  "ACT-R 5 function to get IA value"
  (sji-fct chunkj chunki))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motor module commands

(defun pm-start-hand-at-mouse ()
  "Starts the right hand on the mouse instead of the 'home row' location"
  (start-hand-at-mouse))

(defmacro pm-set-cursor-position (x y)
  "Sets the position of the cursor."
  `(set-cursor-position-fct ,(vector x y)))

(defmacro pm-prepare-motor (&rest lis)
  "Tells the Motor Module to prepare the supplied movement. [left in for backward compatibility]"
  `(pm-prepare-mvmt-mth (get-module :motor) ',lis))

(defun pm-set-cursor-position-fct (xyloc)
  (set-cursor-location-fct xyloc))

(defmacro pm-set-hand-location (hand &rest loc)
  "Sets the location of the given hand to LOC"
  `(set-hand-location-fct ',hand ',loc))

(provide "BACKWARD")

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