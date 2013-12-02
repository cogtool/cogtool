;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2010 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : motor.lisp
;;; Version     : 2.3
;;; 
;;; Description : Source code for the ACT-R 6 Motor Module.
;;; 
;;; Bugs        : 
;;; To do       : [X] Consider a mechanism for extending the commands that are
;;;             :     accepted by the motor module without having to redefine the
;;;             :     the request function.
;;;             : [ ] Should a jam signal an error state?
;;;             : [ ] Let subtypes of prepare be passed in as a request
;;;             :     and generalize how it pulls the keyword values out of
;;;             :     the slots so that new styles with different features
;;;             :     can also be prepared.
;;;             : [ ] Figure out a good way for move-cursor to be abstracted
;;;             :     away from needing to access the internals of vision.
;;; 
;;; ----- History -----
;;; 2004.12.23 Dan [First pass at moving to ACT-R 6]
;;;             : Changed name to motor and reset version to 1.0a1
;;;             :
;;;             : Eliminated the state chunk
;;;             :
;;;             : Making the requests explicit instead of using a mapping
;;;             :   mechanism as was done in ACT-R 5 (at least for now).
;;;             : 
;;;             : Added :from :motor to the queue-command calls that
;;;             :  are events for the device.
;;;             :
;;;             : Doesn't flag any error states at this point (jams or 
;;;             : otherwise) nor does it put a "response" chunk into the buffer
;;;             : which has been a suggestion for a while.
;;;             :
;;;             : Removed the defgeneric for loc->key, check-specs, and
;;;             :   polar-move-xy.
;;;             :
;;;             : Renamed reset-module reset-pm-module.
;;;             :
;;; 2004.12.28 Dan
;;;             : Added pm-start-hand-at-mouse here. [Should all the names
;;;             : have the pm- taken off?]
;;;             :
;;;             : In move-cursor, because the chunk name of the copy passed
;;;             : as the location doesn't match the original
;;;             : stored by vision, loc-to-feat ends up returning a random
;;;             : feature because (gethash (id loc) (found-locs vis-m))
;;;             : ends up returning nil.
;;;             : So instead, since all move-cursor cares about is the xy
;;;             : I'm pulling that out directly in a dummy feature.
;;;             : May have to do a similar thing with the object, but
;;;             : I'm not sure on that one.
;;; 2005.01.11 mdb
;;;             : * Changed some of the request/query interaction.
;;;             : * Added some toplevel commands.
;;;             : * Added doc strings for parameters.
;;; 2005.01.12 Dan
;;;             : * Moved the device's parameters to the device module.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;; 2005.02.10 Dan
;;;             : * Switched expt to expt-coerced in compute-exec-time and
;;;             :   log to log-coerced in fitts.
;;; 2005.04.23 Dan
;;;             : * Added the status printing function to the module's buffer
;;;             :   definition.
;;;             : * Noticed that the last-command query is unhandled...
;;; 2005.05.02 Dan
;;;             : * Added another command from the old rpm-toplevel file - 
;;;             :   set-hand-location and moved the pm- functions to the
;;;             :   backwards.lisp file.
;;; 2005.05.11 Dan
;;;             : * Added ":output 'medium" to the output-key command
;;;             :   events so they show in the medium trace level.
;;;             : * Probably want to do that for other things too at some
;;;             :   point...
;;; 2005.09.28 Dan
;;;             : * Slight change in the reset-motor-module function so that
;;;             :   the chunk-types are defined before calling reset-pm-module
;;;             :   to allow for extensions to define the reset-pm-module
;;;             :   method for adding chunk-types and let them use the motor-
;;;             :   command chunk-type.
;;; 2005.12.16 Dan
;;;             : * Added a new command to the manual to prepare features:
;;;             :   +manual> isa prepare style <style>.
;;;             :   effectively the same as calling prepare-motor, but it
;;;             :   only allows for the style to be prepared.  May want to
;;;             :   extnd that at some point, but currently one would have to
;;;             :   prespecify all possible features to be accepted in the
;;;             :   chunk-type which isn't ideal...
;;; 2006.08.22 Dan
;;;             : * Fixed the prepare request because it wasn't actually 
;;;             :   making the module busy during the preparation - I called 
;;;             :   the wrong pm method to do the preparation.
;;; 2006.08.24 Dan
;;;             : * Undoing an old change (from 2004.12.28) because the
;;;             :   assumption was wrong (that only the xy matters) and now
;;;             :   loc-to-feat works right anyway.  So, move-cursor does use
;;;             :   loc-to-feat again.
;;; 2006.08.29 Dan
;;;             : * Added a features slot to the prepare chunk-type so that
;;;             :   the rest of the features can be passed into a prepare
;;;             :   request - it must be a list of the features as would be
;;;             :   passed to the prepare command not counting the style (which
;;;             :   must be specified in the style slot:
;;;             :   +manual> isa prepare style punch features (:hand right)
;;;             :   as an example.
;;; 2006.09.08 Dan
;;;             : * Changed some of the param tests to nonneg instead of posnum.
;;;             : * Cleaned up noisy-loc? so that it doesn't call act-r-noise
;;;             :   with 0.
;;; 2006.09.13 Dan
;;;             : * Changed the defstyle of peck to add the keywords so that
;;;             :   an explict peck request will work (not sure if there is
;;;             :   a bug in defstyle but this was sufficient to fix things).
;;; 2006.09.15 Dan
;;;             : * Changed move-finger to test that r was not 0 instead of
;;;             :   theta before progressing (which I assume is how it should
;;;             :   be).
;;; 2007.01.05 Dan
;;;             : * Synched all the versions to 2.3 (file, object, module).
;;;             : * Changed the module doc string to remove the "first pass".
;;; 2007.01.08 Dan
;;;             : * Adding in the execute request - since prepartion now allows
;;;             :   for a full movement specification it seems to make sense to
;;;             :   actually have this now.
;;; 2007.01.09 Dan
;;;             : * Fixed a bug with home-hands - the quoted lists caused problems
;;;             :   in some Lisps (treated like a constant so once changed it
;;;             :   stayed that way).
;;;             : * Patched press-key so that if an invalid key is specified
;;;             :   it prints a warning instead of throwing an error.
;;;             : * Made point-hand-at-key check to make sure that there's a
;;;             :   valid position for the key before issueing the command.
;;;             : * Changed the prepare request so that hand, finger, r, and theta
;;;             :   are supplied directly instead of in a list because all the
;;;             :   "styles" only use those arguments.
;;;             : * Made start-hand-at-mouse, set-cursor-position, and set-hand-
;;;             :   location all better check for current mp/model/module.
;;;             : * Changed the home position for the left thumb to be offset
;;;             :   by (1 2) instead of (-1 2) which is the setting of the right
;;;             :   thumb (they shouldn't have been the same and the values for
;;;             :   the right thumb seemed correct to me...)
;;; 2007.01.15 Dan
;;;             : * Added a saftey check to the fitts method to catch situations
;;;             :   where a 0 or - width is provided to print a warning and then
;;;             :   assume 1 pixel width for the target.
;;; 2007.01.16 Dan
;;;             : * Fixed a warning in the handling of a prepare request.
;;; 2007.06.04 Dan
;;;             : * Added the extend-manual-requests command to allow people to
;;;             :   add new requests to the manual system without having to 
;;;             :   modify the request and reset functions.
;;; 2007.06.21 Dan
;;;             : * Modified move-cursor to deal with the new chunk based feature
;;;             :   representation.
;;; 2008.06.11 Dan 
;;;             : * Changed noisy-loc? to more accurately reflect the 4%
;;;             :   chance of missing by only computing one distance instead
;;;             :   of doing it separately for each axis.
;;; 2008.10.28 Dan
;;;             : * Fixed some issues with move-cursor relative to the possible
;;;             :   deletion of chunks by the vision module.
;;; 2010.10.06 mdb
;;;             : * Changed movement to use mininmum jerk velocity profile for
;;;             :   cursor moves. 
;;;             : * Allow incremental mouse moves to update at schedules
;;;             :   other than 50 ms.
;;; 2010.10.06 mdb
;;;             : * Added missing MINJERK-DIST function.
;;; 2011.02.07 Dan
;;;             : * Updated move-cursor to to correspond to the change in how
;;;             :   vision stores it's info, but really need to abstract that
;;;             :   since motor shouldn't have to be dependent on the internals
;;;             :   of vision and need to call its methods.
;;;             : * Also added a warning to move-cursor if it aborts because of
;;;             :   a 0 pixel move.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; DAN 
;;; Start by making sure the dmi and general-pm files have been loaded
;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "DMI" "ACT-R6:support;dmi")
(require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")



;;;; ---------------------------------------------------------------------- ;;;;
;;;; the Motor Module class itself
;;;; ---------------------------------------------------------------------- ;;;;


(defclass motor-module (pm-module)
  ((left-hand :accessor left-hand :initarg :left-hand
              :initform (make-instance 'hand))
   (right-hand :accessor right-hand :initarg :right-hand
               :initform (make-instance 'hand))
   (eff-cursor-loc :accessor eff-cursor-loc :initarg :eff-cursor-loc
                   :initform #(0 0))
   (feature-prep-time :accessor feat-prep-time  :initarg :feat-prep-time 
                      :initform 0.050)
   (movement-initiation-time :accessor init-time :initarg :init-time
                             :initform 0.050)
   (default-target-width :accessor default-target-width 
     :initarg :default-target-width :initform 1.0)
   (peck-fitts-coeff :accessor peck-fitts-coeff :initarg :peck-fitts-coeff
                     :initform 0.075) 
   (min-fitts-time :accessor min-fitts-time :initarg :min-fitts-time 
                   :initform 0.100)
   (incremental-mouse-p :accessor incremental-mouse-p 
                        :initarg :incremental-mouse-p :initform nil)
   (cursor-noise :accessor cursor-noise :initarg :cursor-noise
                 :initform nil)
   (new-requests-table :accessor new-requests-table :initform (make-hash-table)
                       :allocation :class)
   )
  (:default-initargs
    :version-string "2.3"
    :name :MOTOR))



(defmethod initialize-instance :after ((mtr-mod motor-module) &key)
  (home-hands mtr-mod)
  
  ; DAN state chunk not necessary
  ;(setf (state-dmo mtr-mod)
  ;      (make-dme 'motor-state 'module-state
  ;                '(module :motor modality free processor free preparation free
  ;                  execution free)
  ;                :where :external))
  
  )


;;; DAN renamed method reset-pm-module

;;; RESET-PM-MODULE      [Method]
;;; Date        : 97.03.31
;;; Description : Resetting a motor module includes clearing the motor
;;;             : feature memory and resetting the hands to the 'home row'
;;;             : position.

(defmethod reset-pm-module :after ((mtr-mod motor-module))
  (setf (eff-cursor-loc mtr-mod) #(0 0))
  (home-hands mtr-mod))




;;;; ---------------------------------------------------------------------- ;;;;
;;;; the HAND class
;;;; ---------------------------------------------------------------------- ;;;;

(defclass hand ()
  ((location :accessor loc)
   (finger-offsets :accessor fingers :initarg :fingers)))


(defgeneric finger-loc (the-hand finger)
  (:documentation  "Return the absolute XY coordinate of <finger>"))

(defmethod finger-loc ((the-hand hand) finger)
  (let ((fngr-offset (second (assoc finger (fingers the-hand)))))
    (vector (+ (px (loc the-hand)) (px fngr-offset))
            (+ (py (loc the-hand)) (py fngr-offset)))))


(defgeneric move-finger (the-hand finger r theta)
  (:documentation  "Returns new XY loc of finger after a move of r, theta.  Also modifies finger offset."))

(defmethod move-finger ((the-hand hand) finger r theta)
  (unless (= r 0.)
    (setf (second (assoc finger (fingers the-hand)))
          (polar-move-xy (second (assoc finger (fingers the-hand))) 
                         (vector r theta))))
  (finger-loc the-hand finger))


(defgeneric move-hand (the-hand r theta)
  (:documentation  "Moves the hand to a new location"))

(defmethod move-hand ((the-hand hand) r theta)
  (setf (loc the-hand) (polar-move-xy (loc the-hand) (vector r theta))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Support methods
;;;; ---------------------------------------------------------------------- ;;;;


#|  Not called by anything, oddly enough
(defmethod current-cursor-loc ()
  (get-mouse-coordinates (device (device-interface *mp*))))
|#

;;; MOVE-A-FINGER      [Method]
;;; Date        : 97.02.14
;;; Description : Moves a finger on the specified hand.

(defgeneric move-a-finger (mtr-mod hand finger r theta)
  (:documentation  "Moves a finger, returning the new XY location"))

(defmethod move-a-finger ((mtr-mod motor-module) hand finger r theta)
  (ecase hand
    (right (move-finger (right-hand mtr-mod) finger r theta))
    (left (move-finger (left-hand mtr-mod) finger r theta))))


;;; MOVE-A-HAND      [Method]
;;; Date        : 97.02.25
;;; Description : Moves a hand to the specified location

(defgeneric move-a-hand (mtr-mod hand r theta)
  (:documentation  "Moves a hand, returning the new XY location"))

(defmethod move-a-hand ((mtr-mod motor-module) hand r theta)
  (ecase hand
    (right (move-hand (right-hand mtr-mod) r theta))
    (left (move-hand (left-hand mtr-mod) r theta))))


;;; FINGER-LOC-M      [Method]
;;; Date        : 97.02.17
;;; Description : Module-level call to determine the location of a finger.

(defgeneric finger-loc-m (mtr-mod hand finger)
  (:documentation  "Return the XY location of the specified finger"))

(defmethod finger-loc-m ((mtr-mod motor-module) hand finger)
  (ecase hand
    (right (finger-loc (right-hand mtr-mod) finger))
    (left (finger-loc (left-hand mtr-mod) finger))))


;;; HOME-HANDS      [Method]
;;; Date        : 97.02.20
;;; Description : Sets the hand and finger locations to the keyboard home row.

(defgeneric home-hands (mtr-mod)
  (:documentation  "Sets the hand and finger locations to home row locations"))

(defmethod home-hands ((mtr-mod motor-module))
  (setf (loc (right-hand mtr-mod)) #(7 4))
  (setf (loc (left-hand mtr-mod)) #(4 4))
  (setf (fingers (right-hand mtr-mod))
    (list (list 'index #(0 0)) 
          (list 'middle #(1 0)) 
          (list 'ring #(2 0)) 
          (list 'pinkie #(3 0))
          (list 'thumb #(-1 2))))
  (setf (fingers (left-hand mtr-mod))
    (list (list 'index #(0 0)) 
          (list 'middle #(-1 0)) 
          (list 'ring #(-2 0)) 
          (list 'pinkie #(-3 0))
          (list 'thumb #(1 2))))
  )




;;; LOC-TO-KEY      [Method]
;;; Date        : 97.02.26
;;; Description : Given a location, return the corresponding character.
;;;             : Accessed via the 'virtual keyboard' array.

;; Dan removed because the defgeneric is in the device-interface

;(defgeneric loc->key (mtr-mod loc)
;  (:documentation  "Given an location, return the corresponding key"))

(defmethod loc->key ((mtr-mod motor-module) (loc vector))
  (if (vpt= loc #(28 2))
    'mouse
    (aref (loc->key-arr mtr-mod) (px loc) (py loc))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utility functions


(defun distance= (d1 d2)
  "Two distances are not equal if they are more than two degrees apart."
  (when (and d1 d2)
    (> 2.0 (abs (- d1 d2)))))


(defun direction= (d1 d2)
  "Two directions are not equal if they are more than pi/4 apart."
  (when (and d1 d2)
    (> (/ pi 4) (abs (- d1 d2)))))


(defun device->order (device)
  (ecase device
    (MOUSE 0)
    (JOYSTICK-1 1)
    (JOYSTICK-2 2)))

; DAN removed becaus it's in general-pm
; and the notes indicate it should have been removed previously
;(defun check-specs (&rest specs)
;  "If there is an invalid specification, return something, else NIL"
;  (member nil specs))


(defgeneric xy-to-polar (from to)
  (:documentation  "Given a starting and ending location in xy, return polar difference"))

(defmethod xy-to-polar ((from vector) (to vector))
  (let ((distance (dist from to)))
    (vector distance
            (if (zerop distance)
              0.0
              (atan (- (py to) (py from)) 
                    (- (px to) (px from)))))))


(defmethod xy-to-polar ((from list) (to list))
  (xy-to-polar (coerce from 'vector) (coerce to 'vector)))


;;; DAN
; These are now in misc-utils
;
;(defgeneric polar-move-xy (loc move)
;  (:documentation  "Given an xy location and a polar displacement, return new xy"))
;
;(defmethod polar-move-xy ((loc vector) (move vector))
;  (round-xy
;   (list (+ (px loc) (* (px move) (cos (py move))))
;         (+ (py loc) (* (px move) (sin (py move)))))))
;
;(defmethod polar-move-xy ((loc list) (move list))
;  (polar-move-xy (coerce loc 'vector) (coerce move 'vector)))


;;; FITTS      [Function]
;;; Date        : 97.02.14, delta 99.06.18
;;; Description : Return Fitts law time for a given coefficient and distance,
;;;             : optionally supplying the target width (default is 1)

(defgeneric fitts (mtr-mod coef d &optional w)
  (:documentation  "Fitts law time for movement"))

(defmethod fitts ((mtr-mod motor-module) coef d &optional (w 1.0))
  (when (or (zerop w) (minusp w))
    (print-warning "Fitts time computation received a negative or zero width - assuming 1 pixel wide.")
    (setf w (pm-pixels-to-angle 1)))
  (max (min-fitts-time mtr-mod)         ; 99.06.18
       (* coef (log-coerced (+ (/ d w) 0.5) 2))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; PUNCH class and methods

(defStyle punch () hand finger)


(defmethod compute-exec-time ((mtr-mod motor-module) (self punch))
  (+ (init-time mtr-mod) (key-closure-time 
                          ;DAN
                          ;(device-interface *mp*))))
                          (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self punch))
  (+ (init-time mtr-mod) (* 2 (burst-time mtr-mod))))


(defmethod feat-differences ((p1 punch) (p2 punch))
  (cond ((not (eq (hand p1) (hand p2))) 2)
        ((not (eq (finger p1) (finger p2))) 1)
        (t 0)))


(defmethod queue-output-events ((mtr-mod motor-module) (self punch))
  (queue-command 
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (move-a-finger mtr-mod (hand self) (finger self) 0 0)
   ;;; DAN
   :from :motor
   :output 'medium))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; HAND-FINGER-R-THETA class and methods


(defStyle hfrt-movement () hand finger r theta)

(defmethod feat-differences ((m1 hfrt-movement) (m2 hfrt-movement))
  (let ((maxfeats (num-possible-feats m1)))
    (cond ((not (eq (hand m1) (hand m2))) (1- maxfeats))
          ((not (eq (finger m1) (finger m2))) (- maxfeats 2))
          (t
           (let ((nfeats 0))
             (unless (distance= (r m1) (r m2)) (incf nfeats))
             (unless (direction= (theta m1) (theta m2)) (incf nfeats))
             nfeats)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; PECK class and methods

(defStyle peck hfrt-movement hand finger r theta) 

(defmethod compute-exec-time ((mtr-mod motor-module) (self peck))
  (+ (init-time mtr-mod)
     (max (burst-time mtr-mod)
          (rand-time 
           (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self))))))   ; 99.06.18


(defmethod queue-output-events ((mtr-mod motor-module) (self peck))
  (queue-command
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self))
   ;;; DAN
   :from :motor
   :output 'medium))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; PECK-RECOIL class and methods

(defclass peck-recoil (hfrt-movement)
  ((move-time :accessor move-time))
  (:default-initargs
    :style-name :PECK))


(defmethod compute-exec-time ((mtr-mod motor-module) (self peck-recoil))
  (setf (move-time self)
        (max (burst-time mtr-mod)
             (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self))))   ; 99.06.18
  (+ (init-time mtr-mod)
     (max (burst-time mtr-mod) 
          (rand-time (move-time self)))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self peck-recoil))
  (+ (exec-time self) (burst-time mtr-mod)
     (max (burst-time mtr-mod)
          (rand-time (move-time self)))))


(defmethod queue-output-events ((mtr-mod motor-module) (self peck-recoil))
  (queue-command
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                  (vector (r self) (theta self)))
   ;;; DAN
   :from :motor
   :output 'medium))


(defmethod peck-recoil ((mtr-mod motor-module) &key hand finger r theta)
  (unless (or (check-jam mtr-mod) (check-specs hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod
                      (make-instance 'peck-recoil :hand hand :finger finger
                                     :r r :theta theta))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; PLY classes and methods

(defclass ply (hfrt-movement)
  ((fitts-coeff :accessor fitts-coeff :initarg :fitts-coeff)
   (target-width :accessor target-width :initarg :target-width
                 :initform 
                 ;DAN
                 ;(default-target-width (motor-m *mp*))
                 (when (get-module :motor)
                   (default-target-width (get-module :motor)))))
  (:default-initargs
    :finger :dummy
    :style-name :PLY
    :fitts-coeff (peck-fitts-coeff (get-module :motor))))

(defmethod queue-output-events ((mtr-mod motor-module) (self ply))
  (queue-command
   :where :MOTOR :command 'MOVE-A-finger :time (exec-time self)
   :params (list (hand self) (finger self) (r self) (theta self))))

(defmethod compute-exec-time ((mtr-mod motor-module) (self ply))
  (+ (init-time mtr-mod)
     (rand-time (fitts mtr-mod (fitts-coeff self) (r self)
                       (target-width self)))))


(defmethod num-possible-feats :around ((self ply))
  (- (call-next-method) 1))


(defclass hand-ply (ply)
  ()
  (:default-initargs
      :fitts-coeff 
      ; DAN
      ;(when *mp* (mouse-fitts-coeff (device-interface *mp*)))))
      (when (current-device-interface)
        (mouse-fitts-coeff (current-device-interface )))))


(defmethod queue-output-events ((mtr-mod motor-module) (self hand-ply))
  (queue-command
   :where :MOTOR :command 'MOVE-A-HAND :time (exec-time self)
   :params (list (hand self) (r self) (theta self))))


(defclass cursor-ply (ply)
  ((target-coords :accessor target-coords :initarg :target-coords)
   (control-order :accessor control-order :initarg :control-order :initform 0))
  (:default-initargs
    :hand 'RIGHT
    :fitts-coeff 
    ;DAN
    ;(when *mp* (mouse-fitts-coeff (device-interface *mp*)))))
    (when (current-device-interface)
      (mouse-fitts-coeff (current-device-interface )))))

(defmethod compute-exec-time :before ((mtr-mod motor-module) 
                                         (self cursor-ply))
  (setf (fitts-coeff self) 
        (* (expt-coerced 2 (control-order self)) (fitts-coeff self))))


;;; QUEUE-OUTPUT-EVENTS      [Method]
;;; Date        : 99.04.02
;;; Description : New output queueing for cursor moves.  The move is broken
;;;             : up into pieces to provide the illusion of smooth movement.
;;;             : Queue up the pieces.  The last submovement doesn't move
;;;             : with a polar move, though, because small rounding errors
;;;             : accumulate and the target will be missed by a couple
;;;             : pixels.  

(defmethod queue-output-events ((mtr-mod motor-module) (self cursor-ply))
  ;; if making incremental moves, do all the necessary computations there
  (when (incremental-mouse-p mtr-mod)
    (let* ((d (exec-time self))
           (steptime (if (numberp (incremental-mouse-p mtr-mod))
                         (incremental-mouse-p mtr-mod)
                         .05))
           (nsteps (max 1 (round d steptime)))
           (startpos (eff-cursor-loc mtr-mod)) (curdist 0) (px-move nil))
      (when (> nsteps 1)
        (dotimes (idx (- nsteps 1))
          (setf curdist (minjerk-dist (* (1+ idx) steptime) (r self) d))
          (setf px-move (polar-move-xy startpos 
                                       (vector (pm-angle-to-pixels curdist)
                                               (theta self))))
          (queue-command 
           :command 'move-cursor-absolute :where :device :from :motor
           :time (* (1+ idx) steptime)
           :params (list px-move)))
        )))
  ;; make the final movement
  (queue-command 
   :command 'move-cursor-absolute :where :device :from :motor
   :time (exec-time self) 
   :params (list (target-coords self)))
  ;; update cursor location for computing the following movement
  (setf (eff-cursor-loc mtr-mod) (target-coords self)))


(defun minjerk-dist (tm a d)
  "Returns distance moved at time tm, given max distance a and total move time d."
  (cond ((>= tm d) a)
        ((< tm 0) (error "Negative time passed to MINJERK-DIST"))
        ((= tm 0) 0)
        ((<= d 0) 0)
        (t
         (let ((td (/ tm d)))
           (* a
              (+ (* 10 (expt td 3)) (* -15 (expt td 4)) (* 6 (expt td 5))))))))


(defmethod point-hand ((mtr-mod motor-module) &key hand r theta twidth)
  (unless (or (check-jam mtr-mod) (check-specs hand r theta))
    (prepare-movement mtr-mod
                      (make-instance 'hand-ply :hand hand :r r :theta theta
                                     :target-width 
                                     (aif twidth it 
                                          (default-target-width mtr-mod))))))


;;; MOVE-CURSOR      [Method]
;;; Date        : 99.04.02
;;; Description : To get the movement time right, the effective width of the
;;;             : target has to be computed.  Gotta have the target object
;;;             : for that, which should be available through the supplied
;;;             : location or object.

(defmethod move-cursor ((mtr-mod motor-module) &key loc object
                          (device 'MOUSE))
  (unless (or (vpt= (loc (right-hand mtr-mod)) #(28 2))
              (and (eq (exec-s mtr-mod) 'BUSY) 
                   (eq (type-of (last-prep mtr-mod)) 'HAND-PLY)
                   (eq (hand (last-prep mtr-mod)) 'RIGHT)))
    (model-warning "MOVE-CURSOR requested when hand not at mouse!")
    (return-from move-cursor nil))
  (unless (or (check-jam mtr-mod) (check-specs (or loc object) device))
    (let ((r-theta nil)
          (feat nil) 
          (w nil)
          (vision (get-module :vision)))
      
      (setf feat  ;; always refer back to the visicon chunks if possible
        (cond ((and object (chunk-visicon-entry object) (chunk-p-fct (gethash (chunk-visicon-entry object) (visicon vision))))
               (gethash (chunk-visicon-entry object) (visicon vision)))
              ((and object (chunk-slot-value-fct object 'screen-pos)
                    (chunk-type-subtype-p-fct (chunk-chunk-type-fct (chunk-slot-value-fct object 'screen-pos)) 'visual-location))
               (if (chunk-p-fct (gethash (chunk-visicon-entry (chunk-slot-value-fct object 'screen-pos)) (visicon vision)))
                    (gethash (chunk-visicon-entry (chunk-slot-value-fct object 'screen-pos)) (visicon vision))
                 (chunk-slot-value-fct object 'screen-pos)))
              ((and loc (chunk-visicon-entry loc) (chunk-p-fct (gethash (chunk-visicon-entry loc) (visicon vision))))
               (gethash (chunk-visicon-entry loc) (visicon vision)))
              ((and loc (chunk-type-subtype-p-fct (chunk-chunk-type-fct loc) 'visual-location))
               loc)
              (t 
               (print-warning "No valid location could be generated from ~s or ~s when trying to move the mouse." object loc)
               (return-from move-cursor nil))))
                 
                
      (setf r-theta (xy-to-polar (eff-cursor-loc mtr-mod) (xy-loc feat)))
      (if (= 0 (vr r-theta))        ; r=0 is a no-op 
          (model-warning "Move-cursor action aborted because cursor is at requested target ~S" (if object object loc))
        (progn
          (setf w (approach-width feat (vtheta r-theta)))
          (prepare-movement mtr-mod
                            (make-instance 'cursor-ply
                              :r (pm-pixels-to-angle (vr r-theta))
                              :theta (vtheta r-theta)
                              :target-width w
                              :target-coords (noisy-loc? mtr-mod (xy-loc feat) w)
                              :control-order (device->order device))))))))


(defgeneric noisy-loc? (mtr-mod xy-loc w)
  (:documentation "If the Motor Module is set up for it, make the output location noisy."))


;;; NOISY-LOC?      [Method]
;;; Description : Adds noise to the output location if noise is on.  Uses 
;;;             : logistic rather than normal [like other ACT-R].  7.8 is
;;;             : the width of the 96% interval of the unit logistic [after
;;;             : MacKenzie].

(defmethod noisy-loc? ((mm motor-module) (xy-loc vector) (w number))
  (if (not (cursor-noise mm))
    xy-loc
    (let ((pixw (pm-angle-to-pixels w)))
      (if (zerop pixw)
          xy-loc
        (polar-move-xy xy-loc (vector (act-r-noise (/ pixw 7.8)) 
                                      (act-r-random pi)))))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Re-mapped methods


;;; CLICK-MOUSE      [Method]
;;; Date        : 97.02.13
;;; Description : Clicking the mouse is really just a punch with the right
;;;             : index finger.  

(defgeneric click-mouse (mtr-mod)
  (:documentation  "Execute a mouse click operation (a punch)"))

(defmethod click-mouse ((mtr-mod motor-module))
  (if (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (punch mtr-mod :hand 'right :finger 'index)
    (pm-warning "CLICK-MOUSE requested when hand not at mouse!")))


;;; left in for backwards compatibility only
(defmethod move-mouse ((mtr-mod motor-module) location)
  (move-cursor mtr-mod :device 'mouse :loc location))


;;; PRESS-KEY      [Method]
;;; Date        : 97.02.18
;;; Description : Allows execution of key presses without computing movements
;;;             : to the keys and such.  Just does it via hash table lookup,
;;;             : all the 'intelligence' is in the hash table.

(defgeneric press-key (mtr-mod key)
  (:documentation  "High-level interface to press a key: Look up the command and execute it."))

(defmethod press-key ((mtr-mod motor-module) key)
  (when (stringp key)
    (setf key (read-from-string key)))
  (let ((command (key->cmd 
                  ;;; DAN
                  ;(device-interface *mp*) 
                  
                  (current-device-interface )
                                    
                  key)))
    (if (null (first command))
        (print-warning "No press-key mapping available for key ~s." key)
      (apply (first command) mtr-mod (rest command)))))


(defgeneric point-hand-at-key (mtr-mod &key hand to-key)
  (:documentation  "Move the hand to a new location, specified by the key at which the index finger should point."))

(defmethod point-hand-at-key ((mtr-mod motor-module) &key hand to-key)
  (when (stringp to-key)
    (setf to-key (read-from-string to-key)))
  (let ((new-loc (key-to-loc (keyboard (current-device-interface)) to-key)))
    (if new-loc
        (let* ((cur-hand (ecase hand 
                           (right (right-hand mtr-mod))
                           (left (left-hand mtr-mod))))
               (new-polar (xy-to-polar (loc cur-hand) new-loc)))
          (point-hand mtr-mod :hand hand :r (vr new-polar) 
                      :theta (vtheta new-polar)))
      (print-warning "No key mapping available for key ~s" to-key))))


;;; HAND-TO-MOUSE      [Method]
;;; Date        : 97.02.25
;;; Description : Moves the right hand to the mouse.  Yes, I know the mouse
;;;             : location is hard-coded, but that's the way life is.  Just
;;;             : translates the movement into a POINT-HAND.

(defgeneric hand-to-mouse (mtr-mod)
  (:documentation  "Moves the right hand to the mouse"))

(defmethod hand-to-mouse ((mtr-mod motor-module))
  (unless (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (let ((polar (xy-to-polar (loc (right-hand mtr-mod)) #(28 2))))
      (point-hand mtr-mod :hand 'right :r (vr polar) 
                  :theta (vtheta polar) :twidth 4.0))))


;;; HAND-TO-HOME      [Method]
;;; Date        : 97.02.25
;;; Description : Same thing as HAND-TO-MOUSE but in the other direction.

(defgeneric hand-to-home (mtr-mod)
  (:documentation  "Moves the right hand to the home row position from the mouse loc"))

(defmethod hand-to-home ((mtr-mod motor-module))
  (unless (equal (loc (right-hand mtr-mod)) #(7 4))
    (let ((polar (xy-to-polar (loc (right-hand mtr-mod)) #(7 4))))
      (point-hand mtr-mod :hand 'right :r (vr polar) 
                  :theta (vtheta polar) :twidth 4.0))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dan 
;;; everything below here is additional stuff for the ACT-R 6 interface




(defun query-motor-module (motor buffer slot value)
  (if (and (eq slot 'state) (eq value 'error))
    nil
    (generic-state-query motor buffer slot value)))




(defmethod pm-module-request ((motor motor-module) buffer-name chunk-spec)
  (case (chunk-spec-chunk-type chunk-spec)
    (clear ;; replaces the implicit clear from -manual
     (schedule-event-relative 0 'clear :module :motor :destination :motor
                              :output 'medium)
     )
    (execute
     (schedule-event-relative 0 'execute :module :motor :destination :motor
                              :output 'medium)
     )
    (click-mouse
     (schedule-event-relative 0 'click-mouse :module :motor 
                              :destination :motor
                              :output 'low)
     )
    (hand-to-mouse
     (schedule-event-relative 0 'hand-to-mouse :module :motor 
                              :destination :motor
                              :output 'low)
     )
    (hand-to-home
     (schedule-event-relative 0 'hand-to-home :module :motor 
                              :destination :motor
                              :output 'low)
     )
    
    (move-cursor
     (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'object) 
                       :motor 'move-cursor 'object)
                      nil))
           (location (if (slot-in-chunk-spec-p chunk-spec 'loc)
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'loc)
                          :motor 'move-cursor 'loc)
                         nil))
           (device (if (slot-in-chunk-spec-p chunk-spec 'device)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'device)
                       :motor 'move-cursor 'device)
                      nil))
           )
       
       
       (when (or object location)
         (if device
           (schedule-event-relative 
            0 
            'move-cursor 
            :params (list motor :object object 
                          :loc location
                          :device device
                          )
            :module :motor
            :output 'low)
           (schedule-event-relative 
            0 
            'move-cursor 
            :destination :motor
            
            :params (list :object object 
                          :loc location)
            :module :motor
            :output 'low)))))
    
    ((peck peck-recoil) 
     (let* ((cmd (chunk-spec-chunk-type chunk-spec))
            (hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor cmd 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor cmd 'finger)
                       nil))
            (r (if (slot-in-chunk-spec-p chunk-spec 'r)
                  (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'r)
                   :motor cmd 'r)
                  nil))
            (theta (if (slot-in-chunk-spec-p chunk-spec 'theta)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'theta)
                       :motor cmd 'theta)
                      nil)))
       
       (when (and hand finger r theta)
         (schedule-event-relative 
          0 
          cmd
          :destination :motor
          :params (list :hand hand :finger finger :r r :theta theta)
          :module :motor
          :output 'low))))
    
    (point-hand-at-key 
     (let ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                    (verify-single-explicit-value 
                     (chunk-spec-slot-spec chunk-spec 'hand) 
                     :motor 'point-hand-at-key 'hand)
                    nil))
           (to-key (if (slot-in-chunk-spec-p chunk-spec 'to-key)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'to-key)
                       :motor 'point-hand-at-key 'to-key)
                      nil)))
       
       (when (and hand to-key)
         (schedule-event-relative 
          0 
          'point-hand-at-key
          :destination :motor
          :params (list :hand hand :to-key to-key)
          :module :motor
          :output 'low))))
    (punch 
     (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor 'punch 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor 'punch 'finger)
                       nil)))
       
       (when (and hand finger)
         (schedule-event-relative 
          0 
          'punch
          :destination :motor
          :params (list :hand hand :finger finger)
          :module :motor
          :output 'low))))
    (press-key 
     (let* ((key (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'key) 
                   :motor 'press-key 'key)))
       
       (when key
         (schedule-event-relative 
          0 
          'press-key
          :destination :motor
          :params (list key)
          :module :motor
          :output 'low))))
    
    
    (prepare
     (let* ((style (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'style) 
                    :motor 'prepare 'style))
            (hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor 'prepare 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor 'prepare 'finger)
                       nil))
            (r (if (slot-in-chunk-spec-p chunk-spec 'r)
                  (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'r)
                   :motor 'prepare 'r)
                  nil))
            (theta (if (slot-in-chunk-spec-p chunk-spec 'theta)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'theta)
                       :motor 'prepare 'theta)
                      nil)))
       
       (if style
         (schedule-event-relative 
          0 
          'prepare
          :destination :motor
          :params (append (list style) (mapcan (lambda (name value) (when value (list name value)))
                                         '(:hand :finger :r :theta) (list hand finger r theta)))
          :module :motor
          :output 'low)
         (print-warning "Style required for a prepare request to the manual module"))))
    
    (t
     (aif (gethash (chunk-spec-chunk-type chunk-spec) (new-requests-table motor))
          (funcall (cdr it) motor chunk-spec)
          (print-warning "Invalid command ~a sent to the ~s buffer" 
                         (chunk-spec-chunk-type chunk-spec)
                         buffer-name)))))
    
    
(defun reset-motor-module (instance)
   
  (chunk-type motor-command)
  (chunk-type (click-mouse (:include motor-command)))
  (chunk-type (hand-to-mouse (:include motor-command)))
  (chunk-type (hand-to-home (:include motor-command)))
  (chunk-type (move-cursor (:include motor-command)) object loc device)
  (chunk-type (peck (:include motor-command)) hand finger r theta)
  (chunk-type (peck-recoil (:include motor-command)) hand finger r theta)
  (chunk-type (point-hand-at-key (:include motor-command)) hand to-key)
  (chunk-type (press-key (:include motor-command)) key)
  (chunk-type (punch (:include motor-command)) hand finger)
  
  (chunk-type (prepare (:include motor-command)) style hand finger r theta)
  (chunk-type (execute (:include motor-command)))
  
  ;; Moved so that extensions which define new motor
  ;; commands can specialize the reset method to add the 
  ;; required chunk-types and include motor-command.
  
  (reset-pm-module instance)
  
  ; Not sure yet what chunks it needs...
  ;(define-chunks 
  ;    )
  
  
  ;; Define the chunk-types for the user specified extensions
  
  (maphash (lambda (key value)
             (declare (ignore key))
             (let ((chunk-type (car value)))
               (unless (chunk-type-fct chunk-type)
                 (print-warning "Failed to extend motor capabilities with chunk-type: ~s" chunk-type))))
           (new-requests-table instance))
                   
  
  )

(defun params-motor-module (motor param)
  (if (consp param)
      (case (car param)
        (:cursor-noise
         (setf (cursor-noise motor) (cdr param)))
        (:default-target-width
         (setf (default-target-width motor) (cdr param))) 
        (:incremental-mouse-moves
         (setf (incremental-mouse-p motor) (cdr param)))
        (:min-fitts-time
         (setf (min-fitts-time motor) (cdr param)))
        (:motor-burst-time
         (setf (burst-time motor) (cdr param)))
        (:motor-initiation-time
         (setf (init-time motor) (cdr param)))
        (:motor-feature-prep-time
         (setf (feat-prep-time motor) (cdr param)))
         
        (:peck-fitts-coeff
         (setf (peck-fitts-coeff motor) (cdr param)))
        )
    (case param
      (:cursor-noise
       (cursor-noise motor))
      (:default-target-width
       (default-target-width motor))  
      (:incremental-mouse-moves
       (incremental-mouse-p motor))
      (:min-fitts-time
       (min-fitts-time motor))
      (:motor-burst-time
       (burst-time motor))
      (:motor-initiation-time
       (init-time motor))
      (:motor-feature-prep-time
        (feat-prep-time motor))
      
      (:peck-fitts-coeff
        (peck-fitts-coeff motor))
      )))


(define-module-fct :motor 
    (list (list 'manual nil nil '(modality preparation execution processor last-command)
                   #'(lambda () 
                       (print-module-status (get-module :motor)))))
  (list 
   (define-parameter :cursor-noise
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Is there noise in the final cursor location.")
   (define-parameter :default-target-width
     :valid-test #'nonneg 
     :default-value 1.0
     :warning "a non-negative number"
     :documentation 
     "Effective width, in degrees visual angle, of targets with undefined widths.")
   (define-parameter :incremental-mouse-moves
     :valid-test #'posnumorbool 
     :default-value nil
     :warning "T, NIL, or a non-negative number"
     :documentation "Output mouse moves in stages?")
   (define-parameter :min-fitts-time
     :valid-test #'nonneg 
     :default-value 0.1
     :warning "a non-negative number"
     :documentation "Minimum movement time for an aimed [Fitts's] movement.")
   (define-parameter :motor-burst-time
     :valid-test #'nonneg 
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Minimum time for any movement.")
   (define-parameter :motor-initiation-time
     :valid-test #'nonneg 
     :default-value .05
     :warning "a non-negative number"
     :documentation "Time to initiate a motor movement.")
   (define-parameter :motor-feature-prep-time
     :valid-test #'nonneg 
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Time to prepare a movement feature.")
   
   (define-parameter :peck-fitts-coeff
     :valid-test #'nonneg 
     :default-value 0.075
     :warning "a non-negative number"
     :documentation "b coefficient in Fitts's equation for PECK movements.")
   
   )
  :version "2.3"
  :documentation "Module to provide a model with virtual hands"
  :creation #'(lambda (x) 
                (declare (ignore x)) (make-instance 'motor-module))
  :reset #'reset-motor-module
  :query #'query-motor-module
  :request 'pm-module-request
  :params #'params-motor-module
  )

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Other toplevel commands

(defun start-hand-at-mouse ()
  "Starts the right hand on the mouse instead of the 'home row' location"
  (verify-current-mp
   "No current meta-process.  Cannot set hand at mouse."
   (verify-current-model 
    "No current model.  Cannot set hand at mouse."
    (aif (get-module :motor)
         (let* ((the-hand (right-hand it)))
           (setf (loc the-hand) #(28 2))
           t)
         (print-warning "No motor module found. Cannot set hand at mouse.")))))

(defmacro prepare-motor (&rest lis)
  "Tells the Motor Module to prepare the supplied movement."
  `(pm-prepare-mvmt-mth (get-module :motor) ',lis))

(defmacro set-cursor-position (x y)
  "Sets the position of the cursor."
  `(set-cursor-position-fct ,(vector x y)))

(defun set-cursor-position-fct (xyloc)
  (verify-current-mp
   "No current meta-process.  Cannot set cursor position."
   (verify-current-model 
    "No current model.  Cannot set cursor position."
    (if (and (get-module :motor) (current-device-interface))
        (progn
          (setf (eff-cursor-loc (get-module :motor)) xyloc
            (true-cursor-loc (current-device-interface)) xyloc)
          (synch-mouse (current-device-interface))
          (true-cursor-loc (current-device-interface)))
      (if (current-device-interface)
          (print-warning "No motor module found.  Cannot set cursor position.")
        (print-warning "No device interface available.  Cannot set cursor position."))))))


(defmacro set-hand-location (hand &rest loc)
  "Sets the location of the given hand to LOC"
  `(set-hand-location-fct ',hand ',loc))

(defun set-hand-location-fct (hand loc)
  "Function to set the location of the given hand to LOC"
  (verify-current-mp
   "No current meta-process.  Cannot set hand location."
   (verify-current-model 
    "No current model.  Cannot set hand location."
    (aif (get-module :motor)
         (progn
           (setf loc (coerce loc 'vector))
           (ecase hand
             (right (setf (loc (right-hand it)) loc))
             (left (setf (loc (left-hand it)) loc)))
           t)
         (print-warning "No motor module found.  Cannot set hand location.")))))



(defmacro extend-manual-requests (chunk-type function-name)
  `(extend-manual-requests-fct ',chunk-type ',function-name))

(defun extend-manual-requests-fct (chunk-type function-name)
  (cond ((not (listp chunk-type))
         (print-warning "Invalid chunk-type specification ~s.  Manual requests not extended." chunk-type))
        ((not (fboundp function-name))
         (print-warning "~s does not name a function.  Manual requests not extended." function-name))
        (t
         (let ((ct-name (if (listp (first chunk-type)) (car (first chunk-type)) (first chunk-type)))
               (dummy-module (make-instance 'motor-module)))
           (if (gethash ct-name (new-requests-table dummy-module))
               (print-warning "Request ~s is already an extension of the manual buffer.  To redefine you must remove it first with remove-manual-request." ct-name)
             (progn
               (setf (gethash ct-name (new-requests-table dummy-module))
                 (cons (copy-list chunk-type) function-name))
               t))))))

(defmacro remove-manual-request (chunk-type)
  `(remove-manual-request-fct ',chunk-type))

(defun remove-manual-request-fct (chunk-type)
  (let ((dummy-module (make-instance 'motor-module)))
    (if (gethash chunk-type (new-requests-table dummy-module))
        (remhash chunk-type (new-requests-table dummy-module))
      (print-warning "~s is not a previously extended request for the manual module." chunk-type))))
           
    
    
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
