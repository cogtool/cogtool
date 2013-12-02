;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Authors     : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2005 Mike Byrne & Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : device-interface.lisp
;;; Version     : 1.2
;;; 
;;; Description : File for managing the device interface.
;;; 
;;; Bugs        : None known.
;;; 
;;; Todo        : Nothing pending.
;;; 
;;; ----- History -----
;;; 2004.10.19 Dan [Moved into ACT-R 6]
;;;             : Lots of changes - first being a reset of version to 1.0a1
;;;
;;;             : Added the package switches for the act-r package and
;;;             : the "clean" packaging
;;;
;;;             : Removed the following:
;;;                      pm-get-time, pm-timed-event, pm-delayed-event
;;;                         new functions replace these: mp-time and all of the
;;;                         new scheduling functions
;;;                         may want to put back as depricated to ease transition
;;;
;;;                      reset-module 
;;;                         because a model reset calls reset-device directly
;;;                      
;;;                      run-events, new-message 
;;;                         everything goes through the central scheduler
;;;                         instead of separate event queues
;;;
;;;                      pm-install-module, update-module, silent-events
;;;                         master-process functionality replaced
;;;
;;;                      process-display and update-cursor-feat
;;;                         moved specific methods to the vision module since 
;;;                         that's really more the critical class and the
;;;                         device preceeds the specific modules now
;;;
;;;             : Added:
;;;                      current-device-interface, install-device, 
;;;                      current-device, proc-display
;;;
;;;             : Moved these from elsewhere to here
;;;                      pm-angle-to-pixels, pm-pixels-to-angle  
;;; 2005.01.11 mdb
;;;             : Added a couple toplevel functions for backward compatibility.        
;;;
;;; 2005.01.12 Dan
;;;             : Made the device an actual module so that it can have its
;;;             : own parameters.
;;;             :
;;;             : Moved the newly added backward compatibility functions to the
;;;             : backward file in support.
;;; 2005.01.13 Dan
;;;             : * Added the the two new methods lock-device and unlock-device
;;;             :   along with the appropriate slots and changes to reset-device
;;;             :   to allow other modules to block and unblock proc-display 
;;;             :   from actually going.  [Right now, that's a problem with
;;;             :   productions potentially jamming vision if a proc-display
;;;             :   happens between the time of the query for free and the
;;;             :   time that the +visual command is actually sent.]
;;; 2005.02.17 Dan
;;;             : * Fixed the references in the *pixels-per-inch-* settings
;;;             :   so that ACL gets the packaging correct.
;;; 2005.05.11 Dan
;;;             : * Added the :vwt paramter and corresponding virtual-trace
;;;             :   slot to the device interface to control the output of the
;;;             :   << ... >> printing from the vw-output command used by the
;;;             :   virtual window device.
;;; 2005.08.10 Dan
;;;             : * Removed a duplicate definition of prod-display.
;;; 2005.11.02 Dan
;;;             : * Updated unlock-device so that tracking updates can be
;;;             :   recognized and handled as well as full proc-displays.
;;; 2006.03.07 Dan
;;;             : * Fixed an old bug which I don't know how it got back in...
;;;             :   the theta for hyphen in the key mapping should be -1.11.
;;; 2006.03.08 Dan
;;;             : * Made the key-to-loc and key-to-command tables consistent
;;;             :   with respect to - and hyphen.  Both tables now have entries
;;;             :   for both symbols mapped to the same things.
;;; 2006.09.11 Dan
;;;             : * Changed valid test for :mouse-fitts-coeff from posnum to nonneg.
;;; 2006.12.14 Dan
;;;             : * Changed current-device-interface so that it only returns
;;;             :   one value.
;;;             : * Reset all the versions to 1.1...
;;; 2006.12.18 Dan
;;;             : * Removed the :speech-hook parameter since it was depricated
;;;             :   long ago.
;;;             : * Also removed the :output-speech parameter because it doesn't
;;;             :   really have a purpose now - the default devices all have a
;;;             :   device-speak-string method even if it doesn't do anything.
;;; 2006.12.28 Dan
;;;             : * Made proc-display better check for current mp/model/device.
;;; 2007.01.09 Dan
;;;             : * Make output-key check the keyboard array boundaries to avoid
;;;             :   errors when bad key-locations come in.
;;; 2007.05.24 Dan
;;;             : * Removed the device-hook slot and call from the update-device
;;;             :   method since it was depricated long ago.
;;; 2007.06.20 Dan
;;;             : * Changed dmo-to-xy to xy-loc for use with the new vision
;;;             :   module.
;;;             : * Replaced the generic build-features-for and cursor-to-feature
;;;             :   with build-vis-loc-for and cursor-to-vis-loc.
;;; 2007.07.16 Dan
;;;             : * Fixed a bug in the default device-speak-string method that
;;;             :   would cause it to throw the "wrong" error.
;;; 2008.08.11 Dan [1.2]
;;;             : * Added a new parameter :stable-loc-names which controls
;;;             :   whether or not the virtual device sorts the subviews to
;;;             :   guarantee the same names on different runs.  The default is
;;;             :   t which means it still does the sorting.  Setting it to nil
;;;             :   should improve performance when using the virtual devices.
;;;             : * Also took the package setting out of the mode line at the
;;;             :   top since there isn't a specific package for any of the
;;;             :   source files.
;;; 2009.09.08 Dan
;;;             : * Fixed the key-to-command mapping for the period.
;;; 2010.08.26 Dan
;;;             : * Why is the space bar represented as space in the the
;;;             :   key->command table but spc in the key->loc table, and
;;;             :   why is it entered multiple times when they're all just
;;;             :   overwriting the same entry?  Adding an entry for space
;;;             :   into the key->loc table at 6,6 so there's at least a
;;;             :   way to relate things.
;;; 2011.01.19 Dan
;;;             : * Changed unlock-device to use unlock-tracking instead of
;;;             :   update-tracking since that has been depricated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; unless it's MCL define the pixels-per-inch
#+(or (not :mcl) :openmcl)
(progn
  (defvar *pixels-per-inch-x* 72)
  (defvar *pixels-per-inch-y* 72))

;;; if it's ACL with the IDE (Windows) then find the true units-per-inch
#+(and :ALLEGRO-IDE (not :ACTR-ENV-ALONE))
(multiple-value-bind (x y) 
                     (cg:stream-units-per-inch  (cg:screen cg:*system*))
  (setf *pixels-per-inch-x* x)
  (setf *pixels-per-inch-y* y))


;;;; ---------------------------------------------------------------------- ;;;;
;;; The functions for ACT-R 6

(defun current-device-interface ()
  "Return the device-interface for current model in the current meta-process"
  (values (get-module :device)))

(defun install-device (device)
  "Set the device with which a model will interact"
  (verify-current-mp  
   "install-device called with no current meta-process."
   (verify-current-model
    "install-device called with no current model."
    (setf (device (current-device-interface)) device))))

(defun current-device ()
  "Return the device for the current model in the current meta-process"
  (verify-current-mp  
   "current-device called with no current meta-process."
   (verify-current-model
    "current-device called with no current model."
    (device (current-device-interface)))))

(defun proc-display (&key clear)
  "Processes the current display."
  (verify-current-mp  
   "proc-display called with no current meta-process."
   (verify-current-model
    "proc-display called with no current model."
    (if (current-device-interface)
        (process-display (current-device-interface) (get-module :vision) clear)
      (print-warning "No device interface available to process")))))


;;;; ---------------------------------------------------------------------- ;;;;
;;; roll in everything that was in "environment-interface"

(defvar *actr-enabled-p* t)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Move these here

(defun pm-pixels-to-angle (pixels)
  "Convert <pixels> to degress of visual angle."
  (pixels->angle-mth (current-device-interface) pixels))


(defun pm-angle-to-pixels (angle)
  "Convert visual <angle> in degress to pixels."
  (angle->pixels-mth (current-device-interface) angle))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Base DEVICE-INTERFACE class and some quicky methods.

(defclass device-interface ()
  ((pixels-per-inch :accessor ppi :initarg :ppi
                    :initform (/ (+ *pixels-per-inch-x* 
                                    *pixels-per-inch-y*) 2.0))
   (viewing-distance :accessor viewing-distance :initform 15.0)
   (device :accessor device :initform nil)
   (key-closure-time :accessor key-closure-time :initform 0.010)
   (microphone-delay :accessor microphone-delay :initform 0.100)
   (keyboard :accessor keyboard :initform (make-instance 'virtual-keyboard))
   (with-cursor-p :accessor with-cursor-p :initform nil)
   (input-q :accessor input-q :initform nil)
   (mouse-fitts-coeff :accessor mouse-fitts-coeff :initform 0.1)
   (show-focus-p :accessor show-focus-p :initarg :show-focus-p :initform nil)
   (trace-mouse-p :accessor trace-mouse-p :initarg :trace-mouse-p :initform nil)
   (mouse-trace :accessor mouse-trace :initarg :mouse-trace :initform nil)
   (needs-mouse-p :accessor needs-mouse-p :initarg :needs-mouse-p :initform t)
   (true-cursor-loc :accessor true-cursor-loc :initarg :true-cursor-loc
                    :initform #(0 0))
   (locks :accessor locks :initform 0)
   (pending-procs :accessor pending-procs :initform nil)
   (virtual-trace :accessor virtual-trace :initform nil)
   (version-string :accessor version-string :initarg :version-string 
                   :initform "1.2")
   (stable-names :accessor stable-names :initarg :stable-names
                 :initform t)
   ))


(defmethod my-name ((mod device-interface))
  :DEVICE)

(defgeneric angle->pixels-mth (devin angle)
  (:documentation  "Determine the number of pixels subtending a visual angle."))

(defmethod angle->pixels-mth ((devin device-interface) (angle number))
  (round (* (* (viewing-distance devin) (tan (deg->rad angle))) 
            (ppi devin))))


(defgeneric pixels->angle-mth (devin pixels)
  (:documentation  "Determine the amount of visual angle subtended by <pixels>."))

(defmethod pixels->angle-mth ((devin device-interface) (pixels number))
  (rad->deg (atan (/ (/ pixels (ppi devin)) (viewing-distance devin)))))


(defgeneric find-viewing-dist-mth (devin angle pixels)
  (:documentation  "Given the number of pixels an angle subtends, what's the viewing distance?"))

(defmethod find-viewing-dist-mth ((devin device-interface) angle pixels)
  (floor 
   (/ pixels (* (tan (deg->rad angle)) (ppi devin)))))





;;;; ---------------------------------------------------------------------- ;;;;
;;;; Interacting with the Master Process


(defmethod reset-device ((devin device-interface))
  (setf (input-q devin) nil)
  (setf (mouse-trace devin) nil)
  (setf (locks devin) 0)
  (setf (pending-procs devin) nil))

(defgeneric update-device (devin time)
  (:documentation  "Update the device at <time>."))

;;; the default method has to do the following:
;;; [1] Update the attentional focus
;;; [2] Call the device hook
;;; [3] Make sure the cursor synch is maintained
;;; [4] Call the device update method

(defmethod update-device ((devin device-interface) time)
  (when (show-focus-p devin)
    (let ((vis-m (get-module :vision)))
      (when (and vis-m (current-marker vis-m))
        (device-update-attended-loc (device devin) 
                                    (xy-loc (current-marker vis-m))))))
  
  (synch-mouse devin)
  (device-update (device devin) time))


(defgeneric device-update-attended-loc (device xyloc)
  (:documentation  "Tell the device to update the attended location, which is passed in."))

(defmethod device-update-attended-loc (device xyloc)
  (declare (ignore device xyloc))
  nil)


(defgeneric device-update (device time)
  (:documentation  "Update the device at <time>.  A method should be defined for this if the device is dynamic."))

(defmethod device-update (device time)
  (declare (ignore device time))
  nil)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module interface functions


(defun update-device-module (device old-time new-time)
  "Call the device's update-device method with the new time"
  (declare (ignore old-time))
  (update-device device new-time))


(defun params-device-module (device param)
  (if (consp param)
    (case (car param)
     (:pixels-per-inch
       (setf (ppi device) (cdr param)))
      (:process-cursor
       (setf (with-cursor-p device) (cdr param)))
      (:show-focus
       (setf (show-focus-p device) (cdr param))) 
      (:viewing-distance
       (setf (viewing-distance device) (cdr param)))
      
      (:mouse-fitts-coeff
       (setf (mouse-fitts-coeff device) (cdr param)))
      
      (:needs-mouse
       (setf (needs-mouse-p device) (cdr param)))
      (:vwt
       (setf (virtual-trace device) (cdr param)))
      (:stable-loc-names
       (setf (stable-names device) (cdr param)))
      )
    (case param
     (:pixels-per-inch
      (ppi device))
      (:process-cursor
       (with-cursor-p device))
      (:show-focus
       (show-focus-p device)) 
      (:viewing-distance
       (viewing-distance device))
      (:mouse-fitts-coeff
       (mouse-fitts-coeff device))
      (:needs-mouse
       (needs-mouse-p device))
      (:vwt
       (virtual-trace device))
      (:stable-loc-names
       (stable-names device))
      )))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module definition

(define-module-fct :device
    nil
  (list 
   ;; From vision
   (define-parameter :pixels-per-inch
     :valid-test #'posnum 
     :default-value 72.0
     :warning "a non-negative number"
     :documentation "Pixels per inch of display")
   (define-parameter :process-cursor
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Should there be a visicon feature for the cursor?")
   (define-parameter :show-focus
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Show the focus ring on the GUI?")
   (define-parameter :viewing-distance
     :valid-test #'posnum 
     :default-value 15.0
     :warning "a non-negative number"
     :documentation "Distance of the eyes from the display, in inches.")
   
   ;; From motor
   
   (define-parameter :mouse-fitts-coeff
     :valid-test #'nonneg 
     :default-value 0.1
     :warning "a non-negative number"
     :documentation "b coefficient in Fitts's equation for aimed movements.")
   (define-parameter :needs-mouse
     :valid-test #'tornil 
     :default-value t
     :warning "T or NIL"
     :documentation "Does ACT-R control the mouse?")
   
   ;; New general parameter
   
   (define-parameter :vwt
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Virtual Window trace controls the << ... >> outputs from virtual windows")
   
   (define-parameter :stable-loc-names
     :valid-test #'tornil 
     :default-value t
     :warning "T or NIL"
     :documentation "Whether or not to sort the virtual window's subviews to guarantee the names always line up")
   
   )
  :version "1.2"
  :documentation "The device interface for a model"
  :creation #'(lambda (x)
                (declare (ignore x))
                (make-instance 'device-interface))
  :reset #'reset-device
  :params #'params-device-module
  :update #'update-device-module)



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Proc screen stuff

;;; PROCESS-DISPLAY      [Method]
;;; Description : Replacement for old PROC-SCREEN functionality.  A few things
;;;             : have to be done to process the display.  First, make sure
;;;             : a display is attached.  Then, set up the previous icon.
;;;             : This is NIL if we're clearing the display.  Then, ask the
;;;             : device for a new icon.  Then check the new icon against the
;;;             : old one so we don't lose state information.  Update the
;;;             : attended location, make sure everyone is current on the
;;;             : mouse location.  Return the length of the icon, since ya
;;;             : gotta return something.

(defgeneric process-display (devin vis-mod &optional clear)
  (:documentation  "Rebuild the Vision Module's icon based on the current display."))

(defgeneric synch-mouse (devin)
  (:documentation  "Make sure everyone agress on the current cursor position."))

(defmethod synch-mouse ((devin device-interface))
  (when (and (device devin)
             (needs-mouse-p devin)
             (not (vpt= (true-cursor-loc devin)
                        (get-mouse-coordinates (device devin)))))
    (device-move-cursor-to (device devin) (true-cursor-loc devin)))
  (let ((vis-m (get-module :vision)))
    (when vis-m
      (update-cursor-feat devin vis-m))))


(defgeneric update-cursor-feat (devin vis-mod)
  (:documentation  "Updates the feature in the icon with the current cursor position."))

(defgeneric key->cmd (devin key)
  (:documentation  "Given a key, return the appropriate Motor Module command to type it."))

(defmethod key->cmd ((devin device-interface) key)
  (key-to-command (keyboard devin) key))


;;; Generic methods which will need to be overridden for specific devices.
;;; These are defined for MCL windows.

(defgeneric get-mouse-coordinates (device)
  (:documentation  "Return the mouse coordinates in #(x y) form."))

(defmethod get-mouse-coordinates (device)
  (error "No method defined for GET-MOUSE-COORDINATES on object ~S." device))

(defgeneric build-vis-locs-for (obj vis-mod)
  (:documentation  "Return a list of visual-location chunks for an object."))

(defmethod build-vis-locs-for (obj vis-mod)
  (declare (ignore vis-mod))
  (print-warning "No build-vis-locs-for defined on item ~s - no feature generated." obj))

(defgeneric cursor-to-vis-loc (device)
  (:documentation  "Reaturn a visual-location chunk reprsenting the current cursor."))

(defmethod cursor-to-vis-loc (device)
  (error "No method definded for CURSOR-TO-VIS-LOC on object ~S." device))  


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Output functions:  mouse movements, keypress stuff.


(defgeneric output-key (devin keyloc)
  (:documentation  "Request that the device register a key output for the key at a given location."))

(defmethod output-key ((devin device-interface) (keyloc vector))
  (let* ((invalid (or (< (svref keyloc 1) 0)
                      (> (svref keyloc 1) 6)
                      (< (svref keyloc 0) 0)
                      (and (> (svref keyloc 0) 22)
                           (not (= (svref keyloc 0) 28)))))
         (the-key (if invalid nil (loc->key (keyboard devin) keyloc))))
    (if (eq the-key 'mouse)
        (device-handle-click (device devin))
      (progn 
        (when (null the-key)
          (print-warning "Invalid key location pressed ~s" keyloc))
        (device-handle-keypress (device devin) the-key)))))


(defgeneric output-speech (devin text)
  (:documentation  "Requests that the device output the provided text as speech."))

(defmethod output-speech ((devin device-interface) (text string))
  (device-speak-string (device devin) text))


(defgeneric move-cursor-absolute (devin xyloc)
  (:documentation  "Request that the device move the cursor to the absolute location <xyloc>."))

(defmethod move-cursor-absolute ((devin device-interface) xyloc)
  (when (trace-mouse-p devin)
    (push (cons (mp-time) xyloc) (mouse-trace devin)))
  (device-move-cursor-to (device devin) xyloc)
  (setf (true-cursor-loc devin) xyloc)
  (synch-mouse devin))


(defgeneric move-cursor-polar (devin rtheta)
  (:documentation  "Request that the device move the cursor by <r> in direction <theta>."))

(defmethod move-cursor-polar ((devin device-interface) rtheta)
  (let ((newloc (polar-move-xy (get-mouse-coordinates (device devin)) rtheta)))
    (device-move-cursor-to (device devin) newloc)
    (setf (true-cursor-loc devin) newloc))
  (synch-mouse devin))


(defgeneric output-click (devin)
  (:documentation  "Output a mouse click to the device."))

(defmethod output-click ((devin device-interface))
  (device-handle-click (device devin)))


;;; Generic methods which will need to be overridden for specific devices.
;;; These are defined for MCL windows.

(defgeneric device-handle-keypress (device key)
  (:documentation  "Handle the press of the given key."))

(defmethod device-handle-keypress (device key)
  (declare (ignore key))
  (error "No method defined for DEVICE-HANDLE-KEYPRESS for object ~S." device))

(defgeneric device-move-cursor-to (device xyloc)
  (:documentation  "Move the cursor to a specified location."))

(defmethod device-move-cursor-to (device xyloc)
  (declare (ignore xyloc))
  (error "No method defined for DEVICE-MOVE-CURSOR-TO for object ~S." device))

(defgeneric device-handle-click (device)
  (:documentation  "Handle a click request."))

(defmethod device-handle-click (device)
  (error "No method defined for DEVICE-HANDLE-CLICK for object ~S." device))

(defgeneric device-speak-string (device string)
  (:documentation  "Handle a speech request."))

(defmethod device-speak-string (device string)
  (declare (ignore string))
  (error "No method defined for DEVICE-SPEAK-STRING for object ~S." device))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; The virtual keyboard.  Override one or more of these methods to 
;;;; represent different keyboards.

(defclass virtual-keyboard ()
   ((key->cmd-ht :accessor key->cmd-ht 
                  :initform (make-hash-table :test #'equal))
    (key->loc-ht :accessor key->loc-ht
                  :initform (make-hash-table :test #'equal))
    (loc->key-array :accessor loc->key-arr 
                     :initform (make-array '(23 7) :initial-element nil))))


(defmethod initialize-instance :after ((vk virtual-keyboard) &key)
  (populate-key-to-command-ht (key->cmd-ht vk))
  (populate-key-to-loc-ht (key->loc-ht vk))
  (populate-loc-to-key-array (loc->key-arr vk)))


;;; LOC-TO-KEY      [Method]
;;; Description : Given a location, return the corresponding character.
;;;             : Accessed via the 'virtual keyboard' array.

(defgeneric loc->key (vk loc)
  (:documentation  "Given an location, return the corresponding key"))

(defmethod loc->key ((vk virtual-keyboard) (loc vector))
  (if (vpt= loc #(28 2))
    'mouse
    (aref (loc->key-arr vk) (px loc) (py loc))))


(defgeneric key-to-command (vk key)
  (:documentation  "Given a key, return the appropriate command."))

(defmethod key-to-command ((vk virtual-keyboard) key)
  (gethash key (key->cmd-ht vk)))


(defmethod key-to-loc ((vk virtual-keyboard) key)
  (gethash key (key->loc-ht vk)))


(defgeneric populate-key-to-command-ht (ht)
  (:documentation  "Populates the hash table that maps keys to motor commands"))

(defmethod populate-key-to-command-ht ((ht hash-table))
  (setf (gethash 'space ht) '(punch :hand left :finger thumb))
  (setf (gethash 'backquote ht) 
        '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash 'tab ht) 
        '(peck-recoil :hand left :finger pinkie :r 1.41 :theta -2.36))
  (setf (gethash '1 ht) 
        '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'Q ht) 
        '(peck-recoil :hand left :finger pinkie :r 1 :theta -1.57))
  (setf (gethash 'A ht) '(punch :hand left :finger pinkie))
  (setf (gethash 'Z ht) 
        '(peck-recoil :hand left :finger pinkie :r 1 :theta 1.57))
  (setf (gethash '2 ht) 
        '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash 'W ht) 
        '(peck-recoil :hand left :finger ring :r 1 :theta -1.57))
  (setf (gethash 'S ht) 
        '(punch :hand left :finger ring))
  (setf (gethash 'X ht) 
        '(peck-recoil :hand left :finger ring :r 1 :theta 1.57))
  (setf (gethash '3 ht) 
        '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash 'E ht) 
        '(peck-recoil :hand left :finger middle :r 1 :theta -1.57))
  (setf (gethash 'D ht) '(punch :hand left :finger middle))
  (setf (gethash 'C ht) 
        '(peck-recoil :hand left :finger middle :r 1 :theta 1.57))
  (setf (gethash '4 ht) 
        '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash 'R ht) 
        '(peck-recoil :hand left :finger index :r 1 :theta -1.57))
  (setf (gethash 'F ht) '(punch :hand left :finger index))
  (setf (gethash 'V ht) 
        '(peck-recoil :hand left :finger index :r 1 :theta 1.57))
  (setf (gethash '5 ht) 
        '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash 'T ht) 
        '(peck-recoil :hand left :finger index :r 1.41 :theta -0.79))
  (setf (gethash 'G ht) 
        '(peck-recoil :hand left :finger index :r 1 :theta 0))
  (setf (gethash 'B ht) 
        '(peck-recoil :hand left :finger index :r 1.41 :theta 0.79))
  (setf (gethash '6 ht) 
        '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash 'Y ht) 
        '(peck-recoil :hand right :finger index :r 1.41 :theta -2.36))
  (setf (gethash 'H ht) 
        '(peck-recoil :hand right :finger index :r 1 :theta 3.14))
  (setf (gethash 'N ht) 
        '(peck-recoil :hand right :finger index :r 1.41 :theta 2.36))
  (setf (gethash '7 ht) 
        '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash 'U ht) 
        '(peck-recoil :hand right :finger index :r 1 :theta -1.57))
  (setf (gethash 'J ht) 
        '(punch :hand right :finger index))
  (setf (gethash 'M ht) 
        '(peck-recoil :hand right :finger index :r 1 :theta 1.57))
  (setf (gethash '8 ht) 
        '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash 'I ht) 
        '(peck-recoil :hand right :finger middle :r 1 :theta -1.57))
  (setf (gethash 'K ht) '(punch :hand right :finger middle))
  (setf (gethash 'comma ht) 
        '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash '9 ht) 
        '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash 'O ht) 
        '(peck-recoil :hand right :finger ring :r 1 :theta -1.57))
  (setf (gethash 'L ht) 
        '(punch :hand right :finger ring))
  (setf (gethash 'period ht) 
        '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash '0 ht) 
        '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'P ht) 
        '(peck-recoil :hand right :finger pinkie :r 1 :theta -1.57))
  (setf (gethash 'semicolon ht) '(punch :hand right :finger pinkie))
  (setf (gethash 'slash ht) 
        '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash 'hyphen ht) 
        '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  
  (setf (gethash '- ht) 
        '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  
  (setf (gethash '[ ht) 
        '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash 'quote ht) 
        '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash 'return ht) 
        '(peck-recoil :hand right :finger pinkie :r 2 :theta 0))
  ht
  )

(defgeneric populate-key-to-loc-ht (ht)
  (:documentation  "Build a hash table mapping keys to locations."))

(defmethod populate-key-to-loc-ht ((ht hash-table))
  ;; function key row
  (setf (gethash 'escape ht) #(0 0))
  (setf (gethash 'F1 ht) #(2 0))
  (setf (gethash 'F2 ht) #(3 0))
  (setf (gethash 'F3 ht) #(4 0))
  (setf (gethash 'F4 ht) #(5 0))
  (setf (gethash 'F5 ht) #(7 0))
  (setf (gethash 'F6 ht) #(8 0))
  (setf (gethash 'F7 ht) #(9 0))
  (setf (gethash 'F8 ht) #(10 0))
  (setf (gethash 'F9 ht) #(12 0))
  (setf (gethash 'F10 ht) #(13 0))
  (setf (gethash 'F11 ht) #(14 0))
  (setf (gethash 'F12 ht) #(15 0))
  (setf (gethash 'F13 ht) #(17 0))
  (setf (gethash 'F14 ht) #(18 0))
  (setf (gethash 'F15 ht) #(19 0))
  ;; numeric key row
  (setf (gethash 'backquote ht) #(0 2))
  (setf (gethash 1 ht) #(1 2))
  (setf (gethash 2 ht) #(2 2))
  (setf (gethash 3 ht) #(3 2))
  (setf (gethash 4 ht) #(4 2))
  (setf (gethash 5 ht) #(5 2))
  (setf (gethash 6 ht) #(6 2))
  (setf (gethash 7 ht) #(7 2))
  (setf (gethash 8 ht) #(8 2))
  (setf (gethash 9 ht) #(9 2))
  (setf (gethash 0 ht) #(10 2))
  (setf (gethash '- ht) #(11 2))
  (setf (gethash 'hyphen ht) #(11 2))
  (setf (gethash '= ht) #(12 2))
  (setf (gethash 'delete ht) #(13 2))
  (setf (gethash 'help ht) #(15 2))
  (setf (gethash 'home ht) #(16 2))
  (setf (gethash 'pageup ht) #(17 2))
  (setf (gethash 'clear ht) #(19 2))
  (setf (gethash '= ht) #(20 2))
  (setf (gethash '/ ht) #(21 2))
  (setf (gethash '* ht) #(22 2))
  ;; QWERTY row
  (setf (gethash 'tab ht) #(0 3))
  (setf (gethash 'Q ht) #(1 3))
  (setf (gethash 'W ht) #(2 3))
  (setf (gethash 'E ht) #(3 3))
  (setf (gethash 'R ht) #(4 3))
  (setf (gethash 'T ht) #(5 3))
  (setf (gethash 'Y ht) #(6 3))
  (setf (gethash 'U ht) #(7 3))
  (setf (gethash 'I ht) #(8 3))
  (setf (gethash 'O ht) #(9 3))
  (setf (gethash 'P ht) #(10 3))
  (setf (gethash '[ ht) #(11 3))
  (setf (gethash '] ht) #(12 3))
  (setf (gethash 'backslash  ht) #(13 3))
  (setf (gethash 'forward-delete ht) #(15 3))
  (setf (gethash 'end ht) #(16 3))
  (setf (gethash 'page-up ht) #(17 3))
  (setf (gethash 'keypad-7 ht) #(19 3))
  (setf (gethash 'keypad-8 ht) #(20 3))
  (setf (gethash 'keypad-9 ht) #(21 3))
  (setf (gethash 'keypad-hyphen ht) #(22 3))
  ;; "A" row
  (setf (gethash 'caps-lock ht) #(0 4))
  (setf (gethash 'A ht) #(1 4))
  (setf (gethash 'S ht) #(2 4))
  (setf (gethash 'D ht) #(3 4))
  (setf (gethash 'F ht) #(4 4))
  (setf (gethash 'G ht) #(5 4))
  (setf (gethash 'H ht) #(6 4))
  (setf (gethash 'J ht) #(7 4))
  (setf (gethash 'K ht) #(8 4))
  (setf (gethash 'L ht) #(9 4))
  (setf (gethash 'semicolon ht) #(10 4))
  (setf (gethash 'quote ht) #(11 4))
  (setf (gethash 'return ht) #(12 4))
  (setf (gethash 'keypad-4 ht) #(19 4))
  (setf (gethash 'keypad-5 ht) #(20 4))
  (setf (gethash 'keypad-6 ht) #(21 4))
  (setf (gethash 'keypad-plus ht) #(22 4))
  ;; "Z" row
  (setf (gethash 'shift ht) #(0 5))
  (setf (gethash 'Z ht) #(1 5))
  (setf (gethash 'X ht) #(2 5))
  (setf (gethash 'C ht) #(3 5))
  (setf (gethash 'V ht) #(4 5))
  (setf (gethash 'B ht) #(5 5))
  (setf (gethash 'N ht) #(6 5))
  (setf (gethash 'M ht) #(7 5))
  (setf (gethash 'comma ht) #(8 5))
  (setf (gethash 'period ht) #(9 5))
  (setf (gethash 'dot  ht) #(9 5))
  (setf (gethash '/ ht) #(10 5))
  (setf (gethash 'right-shift ht) #(11 5))
  (setf (gethash 'up-arrow ht) #(16 5))
  (setf (gethash 'keypad-1 ht) #(19 5))
  (setf (gethash 'keypad-2 ht) #(20 5))
  (setf (gethash 'keypad-3 ht) #(21 5))
  (setf (gethash 'keypad-enter ht) #(22 5))
  ;; space bar row
  (setf (gethash 'left-control ht) #(0 6))
  (setf (gethash 'left-option ht) #(1 6))
  (setf (gethash 'left-command ht) #(2 6))
  (setf (gethash 'spc ht) #(3 6))
  (setf (gethash 'spc ht) #(4 6))
  (setf (gethash 'spc ht) #(5 6))
  (setf (gethash 'spc ht) #(6 6))
  (setf (gethash 'space ht) #(6 6))
  (setf (gethash 'spc ht) #(7 6))
  (setf (gethash 'spc ht) #(8 6))
  (setf (gethash 'spc ht) #(9 6))
  (setf (gethash 'spc ht) #(10 6))
  (setf (gethash 'right-command ht) #(11 6))
  (setf (gethash 'right-option ht) #(12 6))
  (setf (gethash 'right-control ht) #(13 6))
  (setf (gethash 'left-arrow ht) #(15 6))
  (setf (gethash 'down-arrow ht) #(16 6))
  (setf (gethash 'right-arrow ht) #(17 6))
  (setf (gethash 'keypad-0 ht) #(19 6))
  (setf (gethash 'keypad-period ht) #(21 6))
  (setf (gethash 'enter ht) #(22 6))
  ht)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; various toplevel commands, mostly for backward compatibility

  

(defmethod lock-device ((devin device-interface))
  "Place a lock on proc-display to prevent it from actually occuring"
  (incf (locks devin)))

(defmethod unlock-device ((devin device-interface))
  "Remove one of the locks from proc-display and run it now if all locks are
   removed and there were any blocked calls"
  (unless (zerop (locks devin))
    (decf (locks devin)))
  
  ;; Check for tracking first
  
  (when (and (zerop (locks devin)) (find :tracking (pending-procs devin)))
    (unlock-tracking)
    (setf (pending-procs devin) (remove :tracking (pending-procs devin))))
  
  
  (when (and (zerop (locks devin)) (pending-procs devin))
    (proc-display  :clear (some #'identity (pending-procs devin)))
    (setf (pending-procs devin) nil)))


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
