;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2003 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : general-pm.lisp
;;; Version     : 1.0
;;; 
;;; Description : Base class for the perceptual-motor modules.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : [] Strip out waiting-for-proc-p stuff?
;;; 
;;; ----- History -----
;;; 01.07.27 mdb
;;;             : Started 5.0 conversion. 
;;; 02.01.21 mdb
;;;             : Removed obsolete PROC-S function, renamed slot value function
;;;             : to be PROC-S.  Added INITIAITON-COMPLETE call.
;;; 2002.05.07 mdb [b6]
;;;             : Processor wasn't being set to BUSY while preparation was
;;;             : ongoing, which made Bad Things (tm) happen.  Fixed.
;;; 2002.06.05 mdb
;;;             : Added step-hook call in RUN-MODULE to support the environment.
;;; 2002.06.27 mdb [b7]
;;;             : Moved CHECK-SPECS here and made it print an actual informative
;;;             : warning message.  Wild.
;;; 2003.01.21 mdb [2.1.1]
;;;             : Updated the DM state a smidge less often.
;;; 2003.02.06 mdb
;;;             : Added a VERSION-STRING slot to the base module so each one 
;;;             : can track version numbers separately, in anticipation of 
;;;             : some more separate handling under ACT-R 6.0.
;;; 2003.04.30 mdb [2.1.2]
;;;             : Fixed bug in prepare-only motor movements not leaving
;;;             : processor free.
;;;
;;; 2004.10.20 Dan [First pass at moving things to ACT-R 6]
;;;             : Changed name to general-pm and reset version to 1.0a1
;;;             : Placed it in with the support code
;;;             :   modules that use it should have this call in them:
;;;             :   (require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")
;;;             : Flagged my changes with comments starting with DAN
;;;             :
;;;             : Removed:
;;;             :   run-module    
;;;             :   new-message
;;;             :   pm-install-module
;;;             :   update-dm-state
;;;             :   print-input-queue
;;;             :   silent-events
;;;             :
;;;             : update-module gets called a little differently and if
;;;             :   possible I'd prefer to remove it.
;;;             :
;;;             : Renamed reset-module to reset-pm-module
;;;             :
;;;             : Put the spec class and methods in here
;;;             :
;;;             : Did not adjust the class definition though some things are
;;;             :  no longer necessary
;;; 2005.01.07 mdb 
;;;             : * Changed the class def to remove some obsolete stuff.
;;;             : * Added GENERIC-STATE-QUERY method.
;;;
;;; 2005.01.09 Dan
;;;             : Moved the provide to the end.
;;; 2005.01.12 Dan
;;;             : * Added the old-time and new-time parameters to update-module
;;;             :   which breaks backward compatibility but makes things 
;;;             :   cleaner for moving the device into a module.
;;;             : * Put the state case into the generic-state-query method
;;; 2005.04.23 Dan
;;;             : * Removed the stuffed slot from the attn-module class.
;;;             : * Added the print-module-status method for displaying the
;;;             :   query data for a module - a lot like print-module-state.
;;; 2005.05.11 Dan
;;;             : * Added the output parameter to queue-command so that I
;;;             :   can control the detail level for generated events.
;;;             :   Really, queue-command should be phased out, but for now
;;;             :   it's easier to just keep it around...
;;; 2005.07.22 Dan
;;;             : * Added the last-command reporting to the print-module-status
;;;             :   and the check to generic-query
;;;             : * Added the pm-module-request after method to make sure that
;;;             :   the last-cmd slot gets set for all modules.
;;; 2005.07.25 Dan
;;;             : * Changed the reset method so that last-command starts at none
;;;             :   which is the same as the value it gets on a clear.
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare ignored parameters in update-
;;;             :   module method (can that go away yet?).
;;;             : * Also added an ignore to queue-command for sent-by-act.
;;;             : * Updated version to 1.0.
;;; 2006.12.18 Dan
;;;             : * Took modality out of the print-module-status method.
;;; 2007.01.08 Dan
;;;             : * Took the ~% off the end of the jammed warning.
;;; 2007.05.24 Dan
;;;             : * Took the source-activation slot out of the attn-module class.
;;;             : * Removed the unnecessary partially-clear-attended method stuff.
;;; 2007.02.04 Dan
;;;             : * Adjusted preparation-complete so that it only clears the 
;;;             :   processor state if the previous movement initiation has
;;;             :   passed.  Doesn't occur in many situations, but when the
;;;             :   prepare reqeusts are made it can lead to some unexpected
;;;             :   free states of the module's processor.
;;; 2010.02.15 Dan
;;;             : * Took the :allocation class out of several slots of the
;;;             :   pm-module class because that could interfere with changing
;;;             :   things when running multiple models.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN 
;;; start with some general mapping macros to simplify some things
;;;

(defmacro pm-warning (&rest args)
  `(model-warning ,@args))


;;; This can just throw away the time references which are often 
;;; calls like (mp-time *mp*) which just isn't going to work

(defmacro pm-output (time &rest args)
  `(model-output ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN
;;;
;;; moved the spec class and methods to this file

(defclass spec ()
  ((check-slots :accessor check-slots :initform nil :initarg :check-slots)))


(defmethod initialize-instance :after ((self spec) &key)
  (let (val)
    (dovector (slotname (check-slots self))        ; was dolist
      (setf val (slot-value self slotname))
      (when (and val (listp val) (symbol-function (first val)))
        (setf (slot-value self slotname) 
              (apply (first val) (rest val)))))))


(defgeneric match-spec-p (ts obj)
  (:documentation "Returns T if <obj> matches <spec>."))

(defmethod match-spec-p ((ts spec) (obj standard-object))
  (dovector (slotname (check-slots ts) t)
    (unless (slot-match-p ts obj slotname)
      (return-from match-spec-p nil))))


;;; SLOT-MATCH-P      [Function]
;;; Date        : 99.06.18
;;; Description : A slot matches the spec if the spec is :IGNORE, or if the
;;;             : the spec is a value and it EQUALs the value.  If the spec
;;;             : is a function, just return the result of the function
;;;             : call.

(defgeneric slot-match-p (spec object slotname)
  (:documentation "Determines if an object matches a spec on a particular slot."))

(defmethod slot-match-p ((ts spec) (obj standard-object)
                           (slotname symbol))
  (let ((condition (slot-value ts slotname))
        (value (slot-value obj slotname)))
    (cond ((eq condition :IGNORE) t)
          ((functionp condition)
           (funcall condition (slot-value obj slotname)))
          (t (equal condition value)))))


(defgeneric objs-match-spec (lst ts)
  (:documentation "Returns a list of objects from <lst> that match the spec <ts>."))

(defmethod objs-match-spec ((ls list) (ts spec))
  (remove-if #'(lambda (obj)
                 (not (match-spec-p ts obj))) ls))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Perceptual/motor Modules base class
;;;; ---------------------------------------------------------------------- ;;;;

;;; PM-MODULE      [Class]
;;; Date        : 97.01.15, delta 2003.02.06
;;; Description : Base class for the various modules, includes input
;;;             : queue and basic state information.

(defclass pm-module ()
  ((input-queue :accessor input-q :initform nil)
   (modality-state :accessor mode-s :initform 'FREE :initarg :modality)
   (processor-state :accessor proc-s :initform 'FREE :initarg :processor)
   (preparation-state :accessor prep-s :initform 'FREE :initarg :preparation)
   (execution-state :accessor exec-s :initform 'FREE :initarg :execution)
   (state-change-flag :accessor state-change :initarg :state-change
                      :initform nil)
   ;(state-dmo :accessor state-dmo :initarg :state-dmo :initform nil)
   (module-name :accessor my-name :initarg :name :initform nil)
   (last-command :accessor last-cmd :initform nil :initarg :last-command)
   (last-prep :accessor last-prep :initarg :last-prep :initform nil)
   (exec-queue :accessor exec-queue :initarg :exec-queue :initform nil)
   (feature-prep-time :accessor feat-prep-time  :initarg :feat-prep-time 
                      :initform 0.050)
   (movement-initiation-time :accessor init-time :initarg :init-time
                             :initform 0.050)
   (init-stamp :accessor init-stamp :initarg :init-stamp :initform -0.1)
   (burst-time :accessor burst-time :initarg :burst-time :initform 0.050
               )
   (waiting-for-proc-p :accessor waiting-for-proc-p
                       :initarg :waiting-for-proc-p :initform nil)
   (version-string :accessor version-string :initarg :version-string 
                   :initform "")
))



;;; CHANGE-STATE      [Method]
;;; Date        : 97.02.10
;;; Description : Change one or more of a module's state flags.

(defgeneric change-state (module &key proc exec prep last)
  (:documentation  "Change one or more of a module's state flags."))

(defmethod change-state ((module pm-module) &key proc exec prep last)
  (when proc (setf (proc-s module) proc))
  (when exec (setf (exec-s module) exec))
  (when prep (setf (prep-s module) prep))
  (when last (setf (last-cmd module) last))
  (if (or (eq (proc-s module) 'busy) (eq (exec-s module) 'busy)
          (eq (prep-s module) 'busy))
    (setf (mode-s module) 'busy)
    (setf (mode-s module) 'free))
  
  ;;; DAN Don't need to maintain a state "chunk" for ACT-R anymore
  ;(update-dm-state module)
  
  (setf (state-change module) t))



;;; TEST-MOD-MSG      [Method]
;;; Date        : 97.01.24
;;; Description : Runs a test to make sure the module is processing messages
;;;             : Correctly.

(defgeneric test-mod-msg (module &rest params)
  (:documentation  "Prints out the PM module name, the parameters passed, and the time."))

(defmethod test-mod-msg ((module pm-module) &rest params)
  
  (pm-output (mp-time *mp*) "Module ~S ran test with params ~S"
             (my-name module) params))
  

;;; CLEAR      [Method]
;;; Date        : 97.03.03
;;; Description : Clears a module's state, takes one feature prep time.

#| CLEAR is already a generic function in MCL.
(defgeneric clear (module)
  (:documentation  "Clears a PM module."))
|#

(defmethod clear ((module pm-module))
  (when (not (check-jam module))
    (change-state module :prep 'busy)
    (queue-command :time 0.050 :where (my-name module) :command 'change-state
                   :params '(:last none :prep free))
    (setf (last-prep module) nil)
    (setf (exec-queue module) nil)
    (setf (init-stamp module) -0.1)
    ))


;;; CHECK-JAM      [Method]
;;; Date        : 97.02.18
;;; Description : Modules can't take certain types of commands if they are
;;;             : already busy, and this checks the preparation state of a
;;;             : module for just this problem. 

(defgeneric check-jam (module)
  (:documentation "Returns NIL if the PM module is free, otherwise prints an error message and returns T."))

(defmethod check-jam ((module pm-module))
  (if (not (eq (prep-s module) 'busy))
    nil
    (progn
      (pm-warning "Module ~S jammed at time ~S"
                  (my-name module) ;;; DAN replaced (mp-time *mp*))
                  (mp-time))
      t)))


;;; RESET-PM-MODULE      [Method]
;;; Date        : 97.02.18
;;; Description : When a module needs to be reset, that means both that all
;;;             : state indicators should be set to FREE and the input queue
;;;             : should be cleared.

(defgeneric reset-pm-module (module)
  (:documentation "Resets a PM module to base state:  all flags free, empty input queue."))

(defmethod reset-pm-module ((module pm-module))
  (setf (proc-s module) 'free)
  (setf (exec-s module) 'free)
  (setf (prep-s module) 'free)
  (setf (mode-s module) 'free)
  (setf (last-cmd module) 'none)
  
  ;;; DAN removed 
  ;(update-dm-state module)  
  
  (setf (input-q module) nil)
  (setf (last-prep module) nil)
  (setf (exec-queue module) nil)
  (setf (init-stamp module) -0.1)
  )

;;; UPDATE-MODULE      [Method]
;;; Date        : 98.05.28
;;; Description : Called each time the PS is run.  This will update the 
;;;             : module's state.  If a specific module has other things
;;;             : that need to be updated besides the DME state of that
;;;             : module, define :BEFORE or :AFTER methods.
;;;
;;; DAN 
;;; Only seems to be used by vision and device interface.
;;; The device interface is now built in and I think the
;;; vision module could be reworked.
;;; For now, I've added a hook for the updates on time 
;;; changes, but if vision can work without it I'd prefer
;;; to remove this.


(defgeneric update-module (module old-time new-time)
  (:documentation "Update the state of a PM module."))

(defmethod update-module ((mod pm-module) old-time new-time)
  ;(update-dm-state mod)
  (declare (ignore old-time) (ignore new-time)))


;;; PRINT-MODULE-STATE      [Method]
;;; Date        : 98.05.28
;;; Description : For debugging help, this prints the state of the module
;;;             : to stdout.

(defgeneric print-module-state (module)
  (:documentation "Prints a representation of a PM module's state."))

(defmethod print-module-state ((mod pm-module))
  (format t "~& State of module ~S" (my-name mod))
  (format t "~% Modality:     ~S" (mode-s mod))
  (format t "~% Preparation:  ~S" (prep-s mod))
  (format t "~% Processor:    ~S" (proc-s mod))
  (format t "~% Execution:    ~S" (exec-s mod))
  (format t "~% Last command: ~S" (last-cmd mod)))


(defgeneric pm-module-request (module buffer-name chunk-spec)
  (:documentation "Handles a request from a buffer."))


;;; This after method is used to make sure that all commands
;;; processed record the command in the module.

(defmethod pm-module-request :after ((module pm-module) 
                                     buffer-name 
                                     chunk-spec)
  (declare (ignore buffer-name))
  (let ((last-cmd (chunk-spec-chunk-type chunk-spec)))
    (when (and last-cmd (not (eql (last-cmd module) last-cmd)))
      (change-state module :last last-cmd))))


;;; PRINT-MODULE-STATUS 

(defgeneric print-module-status (module)
  (:documentation "Prints the module's state in query form"))

(defmethod print-module-status ((mod pm-module))
  ;(command-output "  modality free         : ~S"
  ;                (eq (mode-s mod) 'free))
  ;(command-output "  modality busy         : ~S"
  ;                (eq (mode-s mod) 'busy))
  (command-output "  preparation free      : ~S"
                  (eq (prep-s mod) 'free))
  (command-output "  preparation busy      : ~S"
                  (eq (prep-s mod) 'busy))
  (command-output "  processor free        : ~S"
                  (eq (proc-s mod) 'free))
  (command-output "  processor busy        : ~S"
                  (eq (proc-s mod) 'busy))
  (command-output "  execution free        : ~S"
                  (eq (exec-s mod) 'free))
  (command-output "  execution busy        : ~S"
                  (eq (exec-s mod) 'busy))
  (command-output "  last-command          : ~S"
                  (last-cmd mod)))
  
  



(defgeneric check-state (module &key modality preparation 
                                   execution processor last-command)
  (:documentation "Does a quick test of the state of a PM module, returning T iff all the specified states match."))


(defmethod check-state ((mod pm-module) &key modality preparation 
                          execution processor last-command)
  (cond ((and modality (not (eq modality (mode-s mod)))) nil)
        ((and preparation (not (eq preparation (prep-s mod)))) nil)
        ((and execution (not (eq execution (exec-s mod)))) nil)
        ((and processor (not (eq processor (proc-s mod)))) nil)
        ((and last-command (not (eq last-command (last-cmd mod)))) nil)
        (t t)))


(defgeneric generic-state-query (module buffer slot value)
  (:documentation "Handles BUSY/FREE tests on STATE, MODALITY, EXECUTION, PREPARATION and PROCESSOR."))

(defmethod generic-state-query ((module pm-module) buffer slot value)
  (case slot
       ((state modality)
         (case value
          (busy
           (eq (mode-s module) 'busy))
          (free
           (eq (mode-s module) 'free))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (execution
        (case value
          (busy
           (eq (exec-s module) 'busy))
          (free
           (eq (exec-s module) 'free))
          (t (pm-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (preparation
        (case value
          (busy
           (eq (prep-s module) 'busy))
          (free
           (eq (prep-s module) 'free))
          (t (pm-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (processor
        (case value
          (busy
           (eq (proc-s module) 'busy))
          (free
           (eq (proc-s module) 'free))
          (t (pm-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
    (last-command 
     (eql (last-cmd module) value))
    
    ))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; preparation and execution stuff



;;; PREPARATION-COMPLETE      [Method]
;;; Date        : 98.07.22
;;; Description : When movement preparation completes: change the prep
;;;             : state, check to see if the movement just prepared wants
;;;             : to execute right away, and then possibly execute a 
;;;             : movement.

(defgeneric preparation-complete (module)
  (:documentation "Method to be called when movement preparation is complete."))

(defmethod preparation-complete ((module pm-module))
  (change-state module :prep 'free)
  (when (last-prep module)
    (if (exec-immediate-p (last-prep module))
      (setf (exec-queue module)
            (append (exec-queue module) (mklist (last-prep module))))
      (when (and (plusp (init-stamp module))
                 (>= (mp-time) (+ (init-stamp module) (init-time module))))
        (change-state module :proc 'FREE))))
  (maybe-execute-movement module))


;;; FINISH-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : When a movement completes, FREE the execution state, and
;;;             : check to see if there were any movements queued.

(defgeneric finish-movement (module)
  (:documentation "Method called when a movement finishes completely."))

(defmethod finish-movement ((module pm-module))
  (change-state module :exec 'free)
  (maybe-execute-movement module))


;;; MAYBE-EXECUTE-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : If there is a movement queued and the motor state is FREE,
;;;             : then execute the movment.  Also, free the processor state
;;;             : with an event if necessary.

(defgeneric maybe-execute-movement (module)
  (:documentation "If there are any movements in <module>'s execution queue, execute one."))

(defmethod maybe-execute-movement ((module pm-module))
  (when (and (exec-queue module) (eq (exec-s module) 'FREE))
    (perform-movement module (pop (exec-queue module)))))


;;; PREPARE      [Method]
;;; Date        : 98.08.21
;;; Description : Build a movement style instance via APPLY, set it to not
;;;             : automatically execute itself, and prepare it.

(defgeneric prepare (module &rest params)
  (:documentation "Prepare a movement to be executed, but don't execute it. The first of <params> should be the name of a movement style class."))

(defmethod prepare ((module pm-module) &rest params)
  (let ((inst (apply #'make-instance params)))
    (setf (exec-immediate-p inst) nil)
    (prepare-movement module inst)))


;;; EXECUTE      [Method]
;;; Date        : 98.08.21
;;; Description : Executing the previously prepared command requires
;;;             : [1] A previously-prepared command, and
;;;             : [2] No command currently being prepared.
;;;             : If those are OK, put the current style instance in the
;;;             : execution queue and go for it.

(defgeneric execute (module)
  (:documentation "Tells <module> to execute the last movement prepared."))

(defmethod execute ((module pm-module))
  (cond ((not (last-prep module))
         (pm-warning "Motor Module has no movement to EXECUTE."))
        ((eq (prep-s module) 'BUSY)
         (pm-warning "Motor Module cannot EXECUTE features being prepared."))
        (t
         (setf (exec-queue module)
               (append (exec-queue module) (mklist (last-prep module))))
         (maybe-execute-movement module))))


;;; PM-PREPARE-MOTOR-MTH      [Method]
;;; Date        : 98.09.24
;;; Description : If RPM is to begin a run with features already prepared,
;;;             : this is the method to do it.  Create a movement instance,
;;;             : kill the exec-immediate, and set the last prepared movement
;;;             : to the created movement.

(defgeneric pm-prepare-mvmt-mth (module params)
  (:documentation "Create the movement specified in <params>, which should begin with the name of a movement style, and consider it prepared. To be called only at model initialization."))

(defmethod pm-prepare-mvmt-mth ((module pm-module) params)
  (let ((inst (apply #'make-instance params)))
    (setf (exec-immediate-p inst) nil)
    (setf (last-prep module) inst)))





;;;; ---------------------------------------------------------------------- ;;;;
;;;; MOVEMENT-STYLE class and methods
;;;; ---------------------------------------------------------------------- ;;;;

(defclass movement-style ()
  ((fprep-time :accessor fprep-time :initform nil :initarg :fprep-time)
   (exec-time :accessor exec-time :initform nil :initarg :exec-time)
   (finish-time :accessor finish-time :initform nil :initarg :finish-time)
   (exec-immediate-p :accessor exec-immediate-p :initform t
                     :initarg :exec-immediate-p)
   (num-features :accessor num-features :initform nil
                 :initarg :num-features)
   (style-name :accessor style-name :initarg :style-name :initform nil)
   (feature-slots :accessor feature-slots :initarg :feature-slots 
                  :initform nil)
   (set-proc-p :accessor set-proc-p :initarg :set-proc-p :initform nil)
))


;;; PREPARE-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : Change the prep state, compute the feature prep time,
;;;             : note that we're the last feature the MM has prepared,
;;;             : and queue the preparation complete event.

(defgeneric prepare-movement (module movement)
  (:documentation "Tell <module> to prepare <movement>."))

(defmethod prepare-movement ((module pm-module) (mvmt movement-style))
  (change-state module :prep 'BUSY :proc 'BUSY)
  (setf (fprep-time mvmt) 
        (rand-time (compute-prep-time module mvmt)))
  (setf (last-prep module) mvmt)
  (queue-command :command 'preparation-complete :where (my-name module)
                 :time (fprep-time mvmt) :randomize nil)
  (when (and (waiting-for-proc-p module) (null (exec-queue module))
             (exec-immediate-p mvmt))
    (setf (set-proc-p mvmt) t)
    (queue-command :time (+ (fprep-time mvmt) (init-time module))
                   :where (my-name module) :command 'change-state 
                   :params '(:proc free) :randomize nil)))



;;; COMPUTE-PREP-TIME      [Method]
;;; Date        : 98.07.22
;;; Description : Computing the prep time.  If this is a different kind of
;;;             : movement or a totall new movement, then just return the
;;;             : number of features times the time per feature.  If the
;;;             : old movement is similar, compute the differences (a 
;;;             : method for this must be supplied).

(defgeneric compute-prep-time (module movement)
  (:documentation "Return the feature preparation time for <movement>."))

(defmethod compute-prep-time ((module pm-module) (mvmt movement-style))
  (if (or (null (last-prep module))
          (not (eq (style-name mvmt) (style-name (last-prep module)))))
    (* (feat-prep-time module) (num-to-prepare mvmt))
    (* (feat-prep-time module)
       (feat-differences mvmt (last-prep module)))))




;;; PERFORM-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : Performing a movement has several pieces to it.  First,
;;;             : bookkeeping (exec state and start time).  Next we need
;;;             : to compute times.  Then, queue the events (movement
;;;             : specific) that reflect our output, and finally queue
;;;             : the event indicating completion of the movement.

(defgeneric perform-movement (module movement)
  (:documentation "Have <module> perform <movement>."))

(defmethod perform-movement ((module pm-module) (mvmt movement-style))
  (queue-command :time (init-time module) :where (my-name module) 
                 :command 'INITIATION-COMPLETE)
  (change-state module :proc 'BUSY :exec 'BUSY)
  
  ;;; DAN
  ;(setf (init-stamp module) (mp-time *mp*))
  
  (setf (init-stamp module) (mp-time))
  
  (setf (exec-time mvmt) (compute-exec-time module mvmt))
  (setf (finish-time mvmt) (compute-finish-time module mvmt))
  (queue-output-events module mvmt)
  (queue-finish-event module mvmt))


(defmethod initiation-complete ((module pm-module))
  (change-state module :proc 'FREE))


;;; COMPUTE-FINISH-TIME      [Method]
;;; Date        : 98.07.22
;;; Description : Default finish time is simply execution time plus the
;;;             : burst time--some styles will need to override this.

(defgeneric compute-finish-time (module movement)
  (:documentation "Return the finish time of <movement>."))

(defmethod compute-finish-time ((module pm-module) (mvmt movement-style))
  "Return the finish time of the movement."
  (+ (burst-time module) (exec-time mvmt)))


;;; QUEUE-FINISH-EVENT      [Method]
;;; Date        : 98.07.22
;;; Description : Queue the event that frees the exec of the MM.

(defgeneric queue-finish-event (module movement)
  (:documentation "Queue the FINISH-MOVEMENT associated with <movement>."))

(defmethod queue-finish-event ((module pm-module) (mvmt movement-style))
  (queue-command :time (finish-time mvmt) :command 'finish-movement
                 :where (my-name module)))


;;; Stubs that require overrides.

(defgeneric compute-exec-time (module movement)
  (:documentation "Return the execution time of <movement>."))

(defmethod compute-exec-time ((module pm-module) (mvmt movement-style))
  (error "No method defined for COMPUTE-EXEC-TIME."))


(defgeneric queue-output-events (module movement)
  (:documentation "Queue the events--not including the FINISH-MOVEMENT--that <movement> will generate."))

(defmethod queue-output-events ((module pm-module) (mvmt movement-style))
  (error "No method defined for QUEUE-OUTPUT-EVENTS."))


(defgeneric feat-differences (movement1 movement2)
  (:documentation "Return the number of different features that need to be prepared."))

(defmethod feat-differences ((move1 movement-style) (move2 movement-style))
  ;(declare (ignore move1 move2))
  (error "No method defined for FEAT-DIFFERENCES."))




(defgeneric num-possible-feats (movement)
  (:documentation "Return the maximum number of features that could possibly need to be prepared."))

(defmethod num-possible-feats ((mvmt movement-style))
  (1+ (length (feature-slots mvmt))))


(defgeneric num-to-prepare (movement)
  (:documentation "Return the number of features actually needed to prepare <movement>."))

(defmethod num-to-prepare ((mvmt movement-style))
  (1+ (length (remove :DUMMY
                      (remove nil
                              (mapcar #'(lambda (name)
                                          (slot-value mvmt name))
                                      (feature-slots mvmt)))))))



(defmacro defstyle (name base-class &rest params)
  "Macro that defines new motor movement styles.  Pass in the name and the base 
class [if NIL is passed, it will default to MOVEMENT-STYLE] and the base 
parameters.  This will create a class and a method for any PM Module for the 
class."
  `(progn
     (defclass ,name (,(if (not base-class) 'movement-style base-class))
       ,(build-accessors params)
       (:default-initargs
         :style-name ,(sym->key name)
         :feature-slots ',params))
     (defmethod ,name ((module pm-module) &key ,@params)
       (unless (or (check-jam module) (check-specs ',name ,@params))
         (prepare-movement module
                           (make-instance ',name
                             ,@(build-initializer params)))))))

(defun check-specs (name &rest specs)
  "If there is an invalid specification, return something, else NIL"
  (when (member nil specs)
    (pm-warning "NIL specification passed to a PM command ~S: ~S" name specs)
    t))

;;; BUILD-ACCESSORS      [Function]
;;; Date        : 98.11.02
;;; Description : Helper function for DEFSTYLE.

(defun build-accessors (params)
  "From a list of parameters, a list of slot definitions."
  (let ((accum nil))
    (dolist (param params (nreverse accum))
      (push (list param :accessor param :initarg (sym->key param)
                  :initform nil) accum))))


;;; BUILD-INITIALIZER      [Function]
;;; Date        : 98.11.02
;;; Description : Helper function for DEFSTYLE.

(defun build-initializer (params)
  "From a list of parameters, build a list for the make-instance initializer."
  (let ((accum nil))
    (dolist (param params (nreverse accum))
      (push (sym->key param) accum)
      (push param accum))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Attentional modules
;;;; ---------------------------------------------------------------------- ;;;;

;;; ATTN-MODULE      [Class]
;;; Date        : 00.06.09
;;; Description : Class for modules that have attentional capability.
;;;             : CURRENTLY-ATTENDED hold the focus object
;;;             : object gets
;;;             : CURRENT-MARKER denotes the location/event currently attended

(defclass attn-module (pm-module)
  ((currently-attended :accessor currently-attended 
                       :initarg :currently-attended :initform nil)
   (current-marker :accessor current-marker :initarg :current-marker 
                   :initform nil)
   
   ;; mdb moved from vision module definition 2005.01.07
   (loc-failure :accessor loc-failure :initform nil)
   (attend-failure :accessor attend-failure :initform nil)
   ; Dan not needed (stuffed :accessor stuffed :initform nil)
   )
  )


(defmethod reset-pm-module ((module attn-module))
  (call-next-method)
  (clear-attended module)
  (setf (current-marker module) nil)
  )  

(defmethod clear ((module attn-module))
  (call-next-method)
  (setf (current-marker module) nil)
  (clear-attended module))


(defgeneric clear-attended (module)
  (:documentation "Set <module> so that it is attending nothing."))

(defmethod clear-attended ((module attn-module))
  (setf (currently-attended module) nil))

(defgeneric set-attended (module object)
  (:documentation "Note that <module> is now attending <object>."))

(defmethod set-attended ((module attn-module) obj)
  (setf (currently-attended module) obj))


;;; DAN 
;;; Functions that were moved from other places that are still needed


;;; DAN 
;;; moved from master-process and modified to schedule the event directly
;;; in the meta-process schedule queue.
;;;
;;; QUEUE-COMMAND      [Function]
;;; Date        : 98.01.21
;;; Description : More generalized version, used by RPM functions.  Takes
;;;             : all kinds of funky parameters to describe events.

(defun queue-command (&key time where command (params nil) 
                           (sent-by-act nil) (randomize nil)
                           (priority 0)
                           ;;; DAN 
                           ;;; added this for clarity so that
                           ;;; modules get credit for sending the command
                           ;;; somewhere else
                           ;;; Only seems to exist right now in speech and
                           ;;; motor talking to the :device
                           (from nil)
                           ;;; DAN
                           ;;; Adding this so that I can better control
                           ;;; which traces an event gets shown in.
                           ;;; Really, queue-command needs to go away
                           ;;; and be replaced in the original code ...
                           (output t)
                           )
  "Schedule a command in the MP's schedule queue."
;  (new-message *mp*
;               (make-instance 'input-queue-entry
;                 :time-tag (if randomize (rand-time time) time)
;                 :destination where
;                 :act-p sent-by-act
;                 :params (cons command (mklist params))
;                 :priority priority)))
  (declare (ignore sent-by-act))
  (schedule-event-relative (if randomize (rand-time time) time)
                           command
                           :destination where
                           :params (mklist params)
                           :priority priority
                           :output output
                           ;;; Assuming that the sender and reciever are one and 
                           ;;; the same unless the from is given
                           ;;; which needs to be added to the speech and motor calls
                           ;;; that do that type of thing.
                           
                           :module (if (null from) where from)))


(provide "GENERAL-PM")

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
