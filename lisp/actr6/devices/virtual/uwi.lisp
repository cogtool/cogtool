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
;;; Filename    : uniform-interface-virtual.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : wirtual view functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from virtual-view 
;;;             : to this file where it belongs.
;;;             : Added the visible-virtuals-avaialable? function.
;;;             : Actually documented the code and added defgenerics with
;;;             : documentation for the uwi.
;;; 2002.12.19 Dan
;;;             : Modified make-static-text-for-rpm-window to take a
;;;             : color parameter.
;;; 04.04.13 Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;; 2007.07.13 Dan
;;;             : * Adding color as an option for button items in make-
;;;                 button-for
;;; 2008.07.01 Dan
;;;             : * Added code so that closing a virtual window will force all
;;;             :   the associated chunks to be purged:
;;;             :    -  closing a virtual window will first remove all of its
;;;             :       objects.
;;;             :    -  if it has a loc-chunk (a cursor) it is purged.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; RPM-WINDOW  [Class]
;;; Description : This is the base UWI window class.

(defclass rpm-window ()
  ())

;;; RPM-VIRTUAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce a virtual window.

(defclass rpm-virtual-window (rpm-window virtual-window)
  ((open :initform t :accessor window-open?)))

;;; DEVICE-HANDLE-KEYPRESS  [Method]
;;; Description : The method called when a key is pressed.  It
;;;             : calls the rpm-window-key-event-handler which is
;;;             : to be defined by the modeler and calls the next method
;;;             : which will be the virtual-window's method.

(defmethod device-handle-keypress ((vw rpm-virtual-window) key)
  (rpm-window-key-event-handler vw key)
  (call-next-method))

;;; RPM-WINDOW-KEY-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-key-event-handler ((vw rpm-virtual-window) key)
  (declare (ignore key))
  (call-next-method))

(defmethod rpm-window-key-event-handler ((vw rpm-window) key)
  (declare (ignore key)))

;;; DEVICE-HANDLE-CLICK  [Method]
;;; Description : The method called when the mouse is clicked.  It
;;;             : calls the next method which will be the virtual-window's 
;;;             : method to set the position and call the appropriate 
;;;             : click event handler if there is one then calls the
;;;             : rpm-window-click-event-handler which is to be defined by 
;;;             : the modeler.

(defmethod device-handle-click ((vw rpm-virtual-window))
  (call-next-method)
  (rpm-window-click-event-handler vw (cursor-pos vw)))


;;; RPM-WINDOW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-click-event-handler ((vw rpm-virtual-window) pos)
  (declare (ignore pos))
  (call-next-method))

(defmethod rpm-window-click-event-handler ((device rpm-window) position)
  (declare (ignore position)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defgeneric open-rpm-window? (window)
  (:documentation  "Returns t if the window is currently open"))

(defmethod open-rpm-window? ((win rpm-virtual-window))
  (window-open? win))

(defmethod open-rpm-window? ((win null))
  nil)

;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defgeneric close-rpm-window (window)
  (:documentation  "Close an rpm-window"))

(defmethod close-rpm-window ((win rpm-virtual-window))
  (remove-all-items-from-rpm-window win)
  (setf (window-open? win) nil)
  (when (loc-chunks win) 
    (purge-chunk-fct (loc-chunks win))
    (setf (loc-chunks win) nil)))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defgeneric select-rpm-window (window)
  (:documentation "Bring an rpm-window to the front"))

(defmethod select-rpm-window ((win rpm-virtual-window))
  (window-select win))

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

(defgeneric add-visual-items-to-rpm-window (window &rest x)
  (:documentation "Add items to the rpm-window for display"))

(defmethod add-visual-items-to-rpm-window ((win rpm-virtual-window) &rest items)
  (dolist (item items)
    (add-subviews win item)))


;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

(defgeneric remove-visual-items-from-rpm-window (window &rest x)
  (:documentation "Remove items from the display in the window"))

(defmethod remove-visual-items-from-rpm-window ((win rpm-virtual-window) &rest items)
  (dolist (item items)
    (remove-subviews win item)))


;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subvies of the window and redisplay it.

(defgeneric remove-all-items-from-rpm-window (window)
  (:documentation "Remove all items from the window"))

(defmethod remove-all-items-from-rpm-window ((win rpm-virtual-window))
  (apply #'remove-subviews win (subviews win)))

;;; RPM-WINDOW-TITLE  [Method]
;;; Description : Return the title of the window.

(defgeneric rpm-window-title (window)
  (:documentation "Return the title of the window"))

(defmethod rpm-window-title ((win rpm-virtual-window))
  (window-title win))

;;; RPM-WINDOW-VISIBLE-STATUS  [Method]
;;; Description : Return nil to indicate that this is a virtual window.

(defgeneric rpm-window-visible-status (window)
  (:documentation "Return whether this window is real or virtual"))

(defmethod rpm-window-visible-status ((win rpm-virtual-window))
  nil)

;;; MAKE-RPM-WINDOW  [Function]
;;; Description : Make and return a window based on the parameters supplied.
;;;             : Visible determines wheter or not it should be a real or
;;;             : virtual and if the environment is connected it will use a 
;;;             : visible-virtual for the real window unless the user explicitly
;;;             : specifies the class to use.

(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") (width 100) (height 100) (x 0 ) (y 0))
  (if visible
      (if (and (visible-virtuals-available?) (null class))
          (make-instance 'visible-virtual-window :window-title title :width width :height height :x-pos x :y-pos y)
        (format t "Cannot make a visible window, you must use virtual.~%"))
      (make-instance (if class class 'rpm-virtual-window) :window-title title :width width :height height :x-pos x :y-pos y)))

;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defgeneric make-button-for-rpm-window (window &key x y text action height width color)
  (:documentation "Returns a button built with the parameters supplied"))

(defmethod make-button-for-rpm-window ((win rpm-virtual-window) &key (x 0) (y 0) (text "Ok")  (action nil) (height 18)  (width 60) (color 'gray))
  (make-instance 'button-vdi
    :x-pos x :y-pos y
    :dialog-item-text text
    :action action
    :height height
    :width width
    :color color))


;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.

(defgeneric make-static-text-for-rpm-window (window &key x y text height width color)
  (:documentation "Returns a text item built with the parameters supplied"))

(defmethod make-static-text-for-rpm-window ((win rpm-virtual-window) &key (x 0) (y 0) (text "") (height 20) (width 80) (color 'black))
  (make-instance 'static-text-vdi
    :x-pos x :y-pos y
    :dialog-item-text text
    :height height
    :width width
    :color color
    ))


;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defgeneric make-line-for-rpm-window (window start-pt end-pt &optional color)
  (:documentation "returns a view which will draw a line with the parameters supplied"))

(defmethod make-line-for-rpm-window ((wind rpm-virtual-window) start-pt end-pt &optional (color 'black))
  (make-instance 'v-liner
    :color color
    :x-pos (first start-pt)
    :y-pos (second start-pt)
    :width (first end-pt)
    :height (second end-pt)))


;;; ALLOW-EVENT-MANAGER  [Method]
;;; Description : Call the system dependent event processing function. 
;;;             : For a virtual don't do anything.

(defgeneric allow-event-manager (window)
  (:documentation "Call the system dependent event processing function"))

(defmethod allow-event-manager ((win rpm-virtual-window)))

;;; VISIBLE-VIRTUALS-AVAILABLE?  [Function]
;;; Description : Return nil indicating that the visible-virtuals are not
;;;             : enabled.

(defun visible-virtuals-available? () 
  "Return whether or not the visible-virtuals are available"
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
