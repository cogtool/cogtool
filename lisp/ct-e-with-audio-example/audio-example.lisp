;; For Design: Design 1
;; For Task: Task 1

;; When loaded, print what the current features are. Note that this refers
;; to Common Lisp features, not ACT-R features, which are something else
;; entirely!
(print *features*)

#-EMMA (compile-and-load (translate-logical-pathname "ACT-R6:extras;emma.lisp"))

;; Debug flag -- set to 1 for basic tracing, 2 for verbose, and 0 for errors only
(defparameter *cogtool-debug* 0)

;; Debugging tool -- if non-nil whynot-fct will be called on this production name at end of run
(defparameter *why-not* nil)

(defmacro cogtool-debug (level message &rest args)
  ;; Note that this is a macro, so neither message nor args are eval'd
  ;; unless we are at or above the given debug level.
  `(when (>= *cogtool-debug* ,level)
     (format t "~&~5TCogTool: ~@?~%" ,message ,@args)))

;; Note that the following is ignored if this variable has already been initialized
(defvar *system-wait-blocks-only-vision* nil)

;; Note that the following is ignored if this variable has already been initialized
(defvar *cogtool-files-to-load* nil)

(defun load-pending-cogtool-files ()
  (loop for file in *cogtool-files-to-load*
        do (load file :verbose t)
        finally (setq *cogtool-files-to-load* nil)))

;; Note that the following is ignored if this variable has already been initialized
(defvar *widget-value-is-title* nil)

;; Note that the following is ignored if this variable has already been initialized
(defvar *infer-transitions* nil)

;; Note that the following is ignored if this variable has already been initialized
(defvar *cogtool-random-seed* '(1 0))

;; Declare a global variable, in which we will store the current design
(defvar *cogtool-design* nil)

;; Set up a disembodied plist to pass side-band information from CogTool.
(defvar *cogtool-plist* nil)

(defmacro get-cogtool-plist (key &optional default)
  `(getf *cogtool-plist* ,key ,@(and default (list default))))

;; When updating the plist we defer changes to :destination-frame, as it needs to be kept around in
;; some circumstances.
(defun set-cogtool-plist (&rest keys 
			  &key (destination-frame (getf *cogtool-plist* :destination-frame) df-supplied)
			       key
			       hand
			       spoken-text
			       trace-text
			       delay-duration
			       delay-label
			       old-system-wait
			       old-system-wait-label
			  &allow-other-keys)
  (cogtool-debug 2 "Setting plist at time ~A: ~S" (mp-time) keys)
  (unless df-supplied
    (setq keys (list* :destination-frame destination-frame keys)))
  (when (and destination-frame (null (curframe *cogtool-design*)))
    ;; sets the initial frame
    (transition destination-frame *cogtool-design*))
  (when (or key hand)
    (emit-trace 'MOTOR 'START-PRESS-KEY hand key))
  (setq *cogtool-plist* keys)
  (when (and delay-duration (> delay-duration 0.0))
    (setf (pending-waits *cogtool-design*) (nconc (pending-waits *cogtool-design*) (list (cons delay-duration delay-label)))))
  (when spoken-text
    (speak-text spoken-text trace-text))
  (when old-system-wait
    (cogtool-debug 2 "old-sysem-wait: ~A" old-system-wait)
    (system-wait-start old-system-wait (or old-system-wait-label "WAIT"))))

(defun speak-text (text &optional trace-text)
    (cogtool-debug 2 "loudspeaker text = ~S" text)
    (when trace-text
      (emit-trace "AUDIO" 'LOUDSPEAKER trace-text))
    (let ((result (new-word-sound text (mp-time) 'loudspeaker)))
      (cogtool-debug (if result 1 0)
		     "~A spoken text ~A."
		     (if result "Successfully enqueued" "Failed to enqueue")
		     text)))

(defun clear-cogtool-plist-element (key)
  (when (get-cogtool-plist key)
    (let ((result (copy-seq *cogtool-plist*)))
      (setf (getf result key) nil)
      (setq *cogtool-plist* result))))

(defun emit-trace (pseudo-module &rest args)
  (let ((*print-pretty* nil)
	(details (format nil #.(formatter "~{~S~^ ~}") args)))
    (fresh-line t)
    (format t +format-event-event-string+
	    (mp-time)
	    nil nil
	    nil nil nil
	    (max-module-name-length) (or pseudo-module :cogtool)
	    details details
	    nil nil nil nil)))

;;; Add some additional keys to the keyboard

(defmethod populate-key-to-command-ht :after ((ht hash-table))
  (setf (gethash '= ht) 
        '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78)) ;; root(8) == 2.83
  (setf (gethash '] ht) 
        '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46)) ;; root(5) == 2.24 
  (setf (gethash 'backslash ht) 
        '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))  ;; root(10) == 3.16    
  (setf (gethash 'delete ht) 
        '(peck-recoil :hand right :finger pinkie :r 3.87 :theta -0.59)))  ;; root(15) == 3.87

;;; movement style for a single graffiti gesture

(defstyle graffiti-gesture () key)

(defmethod compute-exec-time ((mtr-mod motor-module) (self graffiti-gesture))
  (+ (init-time mtr-mod) .580))

(defmethod queue-output-events ((mtr-mod motor-module) (self graffiti-gesture))
  (queue-command
     :where :device
     :command #'output-graffiti
     :time (exec-time self)
     :params (key self)))
      
(defgeneric output-graffiti (devin key)
  (:documentation  "Request that the device register a graffiti output for the  given key."))

(defmethod output-graffiti ((devin device-interface) key)
    (if (eq key 'mouse)
      () ;;; Check to see if the key is actually gesturable.
      (device-handle-graffiti (device devin) key)))
   
(defgeneric device-handle-graffiti (device key)
  (:documentation  "Handle the graffiti entry of the given key."))

(defmethod device-handle-graffiti (device key)
  (declare (ignore key))
  (error "No method defined for device-handle-graffiti for object ~S." device))

(defmethod feat-differences ((move1 graffiti-gesture) (move2 graffiti-gesture))
  0)
  
(defmethod pm-module-request :around ((motor motor-module) buffer-name chunk-spec)
  (case (chunk-spec-chunk-type chunk-spec)

    ((graffiti-gesture)
     (let ((key (and (slot-in-chunk-spec-p chunk-spec 'key) 
		     (verify-single-explicit-value 
		      (chunk-spec-slot-spec chunk-spec 'key) 
		      :motor 'graffiti-gesture 'key))))
       (when key
         (schedule-event-relative 
	  0 
	  'graffiti-gesture
	  :destination :motor
	  :params (list :key key)
	  :module :motor
	  :output 'low))))

    ((press-mouse-button)
     (schedule-event-relative 0 'press-mouse-button :module :motor 
                              :destination :motor
                              :output 'low))

    ((release-mouse-button)
     (schedule-event-relative 0 'release-mouse-button :module :motor 
                              :destination :motor
                              :output 'low))

    ((drag-cursor)
     (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
		       (verify-single-explicit-value 
			(chunk-spec-slot-spec chunk-spec 'object) 
			:motor 'drag-cursor 'object)
		       nil))
           (location (if (slot-in-chunk-spec-p chunk-spec 'loc)
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'loc)
                          :motor 'drag-cursor 'loc)
                         nil))
           (device (if (slot-in-chunk-spec-p chunk-spec 'device)
		       (verify-single-explicit-value 
			(chunk-spec-slot-spec chunk-spec 'device)
			:motor 'drag-cursor 'device)
		       nil)))
       (when (or object location)
         (if device
	     (schedule-event-relative 
	      0 
	      'drag-cursor 
	      :params (list motor :object object 
			    :loc location
			    :device device
			    )
	      :module :motor
	      :output 'low)
	     (schedule-event-relative 
	      0 
	      'drag-cursor 
	      :destination :motor
	      :params (list :object object :loc location)
	      :module :motor
	      :output 'low)))))

    ((finger-down)
     (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
		      (verify-single-explicit-value 
		       (chunk-spec-slot-spec chunk-spec 'hand) 
		       :motor 'finger-down 'hand)
		      nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
			(verify-single-explicit-value 
			 (chunk-spec-slot-spec chunk-spec 'finger)
			 :motor 'finger-down 'finger)
			nil)))
       (when (and hand finger)
         (schedule-event-relative 
          0 
          'finger-down
          :destination :motor
          :params (list :hand hand :finger finger)
          :module :motor
          :output 'low))))

    ((finger-up)
     (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
		      (verify-single-explicit-value 
		       (chunk-spec-slot-spec chunk-spec 'hand) 
		       :motor 'finger-up 'hand)
		      nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
			(verify-single-explicit-value 
			 (chunk-spec-slot-spec chunk-spec 'finger)
			 :motor 'finger-up 'finger)
			nil)))
       
       (when (and hand finger)
         (schedule-event-relative 
          0 
          'finger-up
          :destination :motor
          :params (list :hand hand :finger finger)
          :module :motor
          :output 'low))))

    (t (call-next-method))))

;;; TODO redefining this method is rather an ugly way of doing this, but
;;;      it'll do for now.

(fmakunbound 'reset-motor-module) ;; suppress the "redefining..." warning
(defun reset-motor-module (instance)
   
  (chunk-type motor-command)
  (chunk-type (click-mouse (:include motor-command)))
;;;;;;;;;;;;;;;;;;;;;BEJ: PRESS-MOUSE-BUTTON copied from CLICK-MOUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (chunk-type (press-mouse-button (:include motor-command)))
;;;;;;;;;;;;;;;;;;;;;BEJ: RELEASE-MOUSE-BUTTON copied from CLICK-MOUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (chunk-type (release-mouse-button (:include motor-command)))
  (chunk-type (hand-to-mouse (:include motor-command)))
  (chunk-type (hand-to-home (:include motor-command)))
  (chunk-type (move-cursor (:include motor-command)) object loc device)
;;;;;;;;;;;;;;;;;;;;;BEJ: DRAG-CURSOR copied from MOVE-CURSOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (chunk-type (drag-cursor (:include motor-command)) object loc device)
  (chunk-type (peck (:include motor-command)) hand finger r theta)
  (chunk-type (peck-recoil (:include motor-command)) hand finger r theta)
  (chunk-type (point-hand-at-key (:include motor-command)) hand to-key)
  (chunk-type (press-key (:include motor-command)) key)
  (chunk-type (punch (:include motor-command)) hand finger)
;;;;;;;;;;;;;;;;;;;;;BEJ: FINGER-DOWN copied from PUNCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (chunk-type (finger-down (:include motor-command)) hand finger)
;;;;;;;;;;;;;;;;;;;;;BEJ: FINGER-UP copied from PUNCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (chunk-type (finger-up (:include motor-command)) hand finger)
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

(undefine-module :motor)
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
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
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
     :default-value 0.001
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
  :reset 'reset-motor-module
  :query 'query-motor-module
  :request 'pm-module-request
  :params 'params-motor-module
  )

;;;; ---------------------------------------------------------------------- ;;;;
;;;; FINGER-DOWN class and methods

(defStyle finger-down () hand finger)
;;;BEJ: copied from the PUNCH style


(defmethod compute-exec-time ((mtr-mod motor-module) (self finger-down))
  (+ (init-time mtr-mod) (key-closure-time 
  ;;;BEJ: I think key-closure-time will inherit from the device-interface class in framework/device-interface.lisp, 0.010s
                          (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self finger-down))
;  (+ (init-time mtr-mod) (* 2 (burst-time mtr-mod))))
  (+ (init-time mtr-mod) (burst-time mtr-mod))) ;;;BEJ: half as long as punch
  
;;;BEJ: I don't understand why the punch method (from which this is copied) is overriding the
;;;;;;; the compute-finish-time method in general-pm.lisp
;;;;;;; HYPOTHESIS: because Mike didn't bother to define exec-time for punch, since it
;;;;;;; didn't depend on the device (like Fitts's Law does), but can be calculated from directly
;;;;;;; from the burst time, which is the minimum time required for the 
;;;;;;; execution of any motor module movement (from the reference manual).

(defmethod feat-differences ((fd1 finger-down) (fd2 finger-down))
  (cond ((not (eq (hand fd1) (hand fd2))) 2)
        ((not (eq (finger fd1) (finger fd2))) 1)
        (t 0)))


(defmethod queue-output-events ((mtr-mod motor-module) (self finger-down))
  (queue-command 
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (move-a-finger mtr-mod (hand self) (finger self) 0 0)
   ;;; DAN
   :from :motor
   :output 'medium))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; FINGER-UP class and methods

(defStyle finger-up () hand finger)
;;;BEJ: copied from the PUNCH style


(defmethod compute-exec-time ((mtr-mod motor-module) (self finger-up))
  (+ (init-time mtr-mod) (key-closure-time 
  ;;;BEJ: I think key-closure-time will inherit from the device-interface class in framework/device-interface.lisp, 0.010s
  ;;;BEJ: For now, I am using key-closure-time as a stand-in for key-open time. Our devices don't do anything with
  ;;;BEJ: key-open yet, so it seems to only adds time to the model, not do anything in the interface.
  ;;;BEJ: BUT - I am unhappy that key-closure-time seems to add time to the model.
  ;;;BEJ: This parameter seems to be associated with the device and when it reacts to the finger movement,
  ;;;BEJ: NOT influence the time of the finger movement.
     (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self finger-up))
;  (+ (init-time mtr-mod) (* 2 (burst-time mtr-mod))))
  (+ (init-time mtr-mod) (burst-time mtr-mod))) ;;;BEJ: half as long as punch
  
;;;BEJ: I don't understand why the punch method (from which this is copied) is overriding the
;;;;;;; the compute-finish-time method in general-pm.lisp
;;;;;;; HYPOTHESIS: because Mike didn't bother to define exec-time for punch, since it
;;;;;;; didn't depend on the device (like Fitts's Law does), but can be calculated from directly
;;;;;;; from the burst time, which is the minimum time required for the 
;;;;;;; execution of any motor module movement (from the reference manual).


(defmethod feat-differences ((fu1 finger-up) (fu2 finger-up))
  (cond ((not (eq (hand fu1) (hand fu2))) 2)
        ((not (eq (finger fu1) (finger fu2))) 1)
        (t 0)))


(defmethod queue-output-events ((mtr-mod motor-module) (self finger-up))
  (queue-command 
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (move-a-finger mtr-mod (hand self) (finger self) 0 0)
   ;;; DAN
   :from :motor
   :output 'medium))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; ---------------------------------------------------------------------- ;;;;

;;; PRESS-MOUSE-BUTTON      [Method]
;;; Date        : 07.06.20
;;; Description : Pressing down the mouse button is just a FINGER-DOWN with the right
;;;             : index finger.  

(defgeneric press-mouse-button (mtr-mod)
  (:documentation  "Execute a press on the mouse button operation (a finger-down)"))

(defmethod press-mouse-button ((mtr-mod motor-module))
  (if (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (finger-down mtr-mod :hand 'right :finger 'index)
    (pm-warning "PRESS-MOUSE-BUTTON requested when hand not at mouse!")))

;;;; ---------------------------------------------------------------------- ;;;;

;;;; ---------------------------------------------------------------------- ;;;;

;;; RELEASE-MOUSE-BUTTON      [Method]
;;; Date        : 07.06.20
;;; Description : releasing the mouse button is just a FINGER-UP with the right
;;;             : index finger.  

(defgeneric release-mouse-button (mtr-mod)
  (:documentation  "Execute a release of the mouse button operation (a finger-up)"))

(defmethod release-mouse-button ((mtr-mod motor-module))
  (if (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (finger-up mtr-mod :hand 'right :finger 'index)
    (pm-warning "RELEASE-MOUSE-BUTTON requested when hand not at mouse!")))

;;;; ---------------------------------------------------------------------- ;;;;

;;;; ---------------------------------------------------------------------- ;;;;


;;;;;;;;;;;;;;;;;BEJ: the following is totally messed up.
;;;;;;;;;;;;;;;;;     CogTool should do drag with three separate productions: press, move, release.
;;;;;;;;;;;;;;;;;     not this composite thing.
;;;;;;;;;;;;;;;;;     27jun07 - that's what I am going to go off an do, rather than trying to get this to work.


;;; DRAG-CURSOR      [Method]
;;; Date        : 07.06.22
;;; Description : A drag-cursor is a composite of press-mouse-button
;;;               move-cursor and release-mouse-button

(defmethod drag-cursor ((mtr-mod motor-module) &key loc object
                          (device 'MOUSE))
(print "DRAG-CURSOR method, right BEFORE the PRESS-MOUSE-BUTTON")
;   (press-mouse-button mtr-mod) 
(print "DRAG-CURSOR method, AFTER the PRESS-MOUSE-BUTTON, but BEFORE the MOVE-CURSOR")
;(print "DRAG-CURSOR method, AFTER the PRESS-MOUSE-BUTTON, but BEFORE the (setf (prep-s mtr-mod) 'free)")
;;;;;;;;;; BEJ: I know this is the wrong thing to do, but I am using this to understand
;;;;;;;;;;      HOW to manipulate the motor processes on my way to also understanding
;;;;;;;;;;      WHEN to manipulate the motor processes
;(setf (prep-s mtr-mod) 'free)
;(print "DRAG-CURSOR method, AFTER the (setf (prep-s mtr-mod) 'free), but BEFORE the MOVE-CURSOR")
   (move-cursor mtr-mod :loc loc :object object :device device)
;(print "DRAG-CURSOR method, AFTER the MOVE-CURSOR, but BEFORE the second (setf (prep-s mtr-mod) 'free)")
;(setf (prep-s mtr-mod) 'free)
;(print "DRAG-CURSOR method, AFTER the second (setf (prep-s mtr-mod) 'free), but BEFORE the RELEASE-MOUSE-BUTTON")
(print "DRAG-CURSOR method, AFTER the MOVE-CURSOR, but BEFORE the RELEASE-MOUSE-BUTTON")
;   (release-mouse-button mtr-mod)
(print "DRAG-CURSOR method, AFTER the RELEASE-MOUSE-BUTOTON")
   )

(defun frame-module-query (module buff slot value)
  (let ((frame (curframe *cogtool-design*)))
    (cogtool-debug 2 "frame query ~S ~S ~S" slot value (and frame (name frame)))
    (case slot
      (state
	(case value
	  (error nil)
	  (busy nil)
	  (free t)
	  (t (print-warning "Bad state query to ~S buffer" b))))
      (name
         (and
	   (or (visible-p *cogtool-design*) *system-wait-blocks-only-vision*)
	   (string-equal value (and frame (name frame)))))
      (status
        (cogtool-debug 2 "frame query on status ~S" (visible-p *cogtool-design*))
	(case value
	  (visible (visible-p *cogtool-design*))
	  (t (print-warning "Bad status query to ~S buffer" b))))
      (t (print-waring "Invalid slot ~S in query to buffer ~S" slot buff)))))

(define-module-fct 'frame '((frame nil nil (name status))) ()
  :version "1.0"
  :documentation "Frame interface for CogTool"
  :query 'frame-module-query)

;;; Add additional information to some trace lines, mapping ACT-R's
;;; inscrutable things like LOC0-0 to CogTool Widget names and the like.
;;; This works by noticing trace calls for particular event actions,
;;; and then calling a function to generate a list of additional things
;;; that are appended to the end of the normal trace line.

(defparameter *additional-event-param-functions* (make-hash-table))

(defmethod format-event :around ((event act-r-event))
  (let* ((value (call-next-method))
	 (fn (gethash (evt-action event) *additional-event-param-functions*))
	 (add (and fn (funcall fn (evt-params event)))))
    (if add
	(format nil "~A~{~S ~}" value add)
	value)))

(defmacro def-additional-params (action (params-arg) &body body)
  `(setf (gethash ',action *additional-event-param-functions*)
	 #'(lambda (,params-arg) ,@body)))

(def-additional-params move-attention (params)
  (let ((loc (getf (rest params) :location)))
    (and loc (list (chunk-slot-value-fct loc 'widget-name) (chunk-slot-value-fct loc 'display-label)))))

(defvar *use-finger-default-value* nil)

(def-additional-params move-cursor (params)
  (let ((loc (getf params :loc)))
    (and loc (list (chunk-slot-value-fct loc 'widget-name)
		   (chunk-slot-value-fct loc 'display-label)
		   (get-cogtool-plist :use-finger *use-finger-default-value*)))))

(def-additional-params click-mouse (params)
  (list (get-cogtool-plist :use-finger *use-finger-default-value*)))

;;; (def-additional-params encoding-complete (params)
;;;   (let* ((dmo (first params))
;;; 	 (id (and dmo (id dmo))))
;;;     (and id (list (chunk-slot-value-fct id 'value)))))

(def-additional-params set-buffer-chunk (params)
  (and (eq (first params) 'visual-location)
       (list (chunk-slot-value-fct (second params) 'value))))

;;;;;;;;;; Define cogtool model classes

(defclass cogtool-device (device-interface) 
  ((frames :accessor frames :initform (make-hash-table :test #'equal)
           :documentation "hashtable of (string => frame)")
   ;; init position really happens cogtool-run-model
   (cursor-x :accessor cursor-x :initform 0)
   (cursor-y :accessor cursor-y :initform 0)
   (curframe :accessor curframe :initform nil)
   (visible-p :accessor visible-p :initform t)
   (pending-waits :accessor pending-waits :initform nil)
   (executing-wait :accessor executing-wait :initform nil)))

(defclass cogtool-frame () 
  ((name :accessor name :initarg :name :initform ""
         :documentation "name of the frame")
   (widgets :accessor widgets :initarg :widgets :initform nil
            :documentation "list of widgets")
   (speaker-text :accessor speaker-text :initarg :speaker-text :initform nil
		 :documentation "text to be spoken when entering this frame")
   ;; we do not initialize the vis-locs when creating the frame, as
   ;; ACT-R chunks can only be created in the context of a model, which
   ;; doesn't exist yet
   (vis-locs :accessor vis-locs :initform nil
	     :documentation "cached list of visual-location chunks")))

(defclass cogtool-widget () 
  ((name :accessor name :initarg :name :initform ""
         :documentation "name of the widget")
   (title :accessor title :initarg :title :initform ""
         :documentation "title of the widget")
   (x :accessor x :initarg :x :initform 0) ; x,y are the upper left corner, not center!
   (y :accessor y :initarg :y :initform 0)
   (width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (wtype :accessor wtype :initarg :wtype :initform "cogtool-button")
   (auxiliary-label :accessor auxiliary-label :initarg :auxiliary-label :initform nil)
   (has-remote-label :accessor has-remote-label :initarg :has-remote-label :initform nil)
   (remote-label-of :accessor remote-label-of :initarg :remote-label-of :initform nil)
   (member-of-groups :accessor member-of-groups :initarg :member-of-groups :initform nil)
   ;; we never actually use transitions in CogTool per se, but need to keep them around
   ;; for Leonghwee's SNIF-ACT stuff
   (transitions :accessor transitions :initarg :transitions
		:initform (make-hash-table :test #'equal)
		:documentation "hashtable of (action => transition)")))

;; we never actually use transitions in CogTool per se, but need to keep them around
;; for Leonghwee's SNIF-ACT stuff
(defclass cogtool-transition ()
  ((target :accessor target :initarg :target :initform ""
	   :documentation "name of the target frame")))

(defun resolve-widget-refs (frame groups)
  (let ((wtab (make-hash-table :test #'equal :size (length (widgets frame))))
	(gtab (make-hash-table :test #'equal :size (length groups))))
    (loop for w in (widgets frame)
	  do (setf (gethash (name w) wtab) w))
    (loop for g in groups
	  do (setf (gethash (name g) gtab) g))
    (loop for w in (widgets frame)
	  do (let ((remote-label (has-remote-label w))
		   (containing-groups (member-of-groups w)))
	       (when remote-label
		 (setq remote-label (gethash remote-label wtab))
		 (setf (has-remote-label w) remote-label)
		 (setf (remote-label-of remote-label) w))
	       (when containing-groups
		 (setf (member-of-groups w) (loop for g in containing-groups
						  collect (gethash g gtab))))))))

;;;;;;;;;; Actual ACT-R device-interface methods

(defmethod build-vis-locs-for ((design cogtool-device) (vis-mod vision-module))
  (if (or (not (visible-p design)) (null (curframe design)) (vision-suppressed))
      (progn
	(cogtool-debug 1 "Clearing visual icon at time: ~D" (mp-time))
        (clear vis-mod)
        nil)
      ;; tell the vis-mod about all widgets
      (progn
	(cogtool-debug 1 "Building visual icon at time: ~D" (mp-time))
	(frame-vis-locs (curframe design)))))

(defun frame-vis-locs (frame)
  (or (vis-locs frame)
      (setf (vis-locs frame) (make-frame-vis-locs frame))))

(defun make-frame-vis-locs (frame)
  (cogtool-debug 2 "Making vis-locs for ~S" frame)
  (let ((result '()) (groups '()))
    (loop for widget in (widgets frame)
	  do (cogtool-debug 2 "vis-loc info for ~S, ~S, ~S" widget (name widget) (wtype widget))
	  do (if (member-of-groups widget)
		 (loop for g in (member-of-groups widget)
		       do (progn
			    (unless (member g groups)
			      (cogtool-debug 2 "vis-loc info for group ~S, ~S" g (name g))
			      (push g groups)
			      (push (frame-vis-loc-template g) result))
			    (push (frame-vis-loc-template widget g) result)))
	         (push (frame-vis-loc-template widget) result)))
    (define-chunks-fct result)))

(defun frame-vis-loc-template (thing &optional group)
  (let ((name (name thing)))
    `(,(intern name)
      isa cogtool-visual-location
      value ,(if *widget-value-is-title*
		 (title (or (remote-label-of thing) thing))
	         (if (remote-label-of thing) (name (remote-label-of thing)) name))
      widget-name ,name
      display-label ,(title thing)
      screen-x ,(+ (x thing) (floor (width thing) 2))
      screen-y ,(+ (y thing) (floor (height thing) 2))
      height ,(height thing)
      width ,(width thing)
      kind ,(wtype thing)
      auxiliary-label ,(auxiliary-label thing)
      remote-label-of ,(and (remote-label-of thing) (name (remote-label-of thing)))
      has-remote-label ,(and (has-remote-label thing) (name (has-remote-label thing)))
      ,@(when group `(member-of-group ,group)))))

;; Sets the cursor to a new location.
(defmethod device-move-cursor-to ((design cogtool-device) (xyloc vector))
  (emit-trace nil 'DEVICE-MOVE-CURSOR-TO (px xyloc) (py xyloc))
  ;; save position info
  (setf (cursor-x design) (px xyloc))
  (setf (cursor-y design) (py xyloc))
  ;; Follow a transition if needed.
  (transition (get-cogtool-plist :destination-frame) design))

;; Gets the current location of the cursor.
(defmethod get-mouse-coordinates ((design cogtool-device))
  (cogtool-debug 2 "Getting mouse coords x: ~D y: ~D" 
            (cursor-x design) (cursor-y design))
  (vector (cursor-x design) (cursor-y design)))

(defmethod device-handle-graffiti ((design cogtool-device) key)
  (emit-trace nil 'DEVICE-EMIT-GRAFFITI key)
  (transition (get-cogtool-plist :destination-frame) design))

(defmethod device-handle-keypress ((design cogtool-device) key)
  (emit-trace nil 'DEVICE-PRESS-KEY key)
  (transition (get-cogtool-plist :destination-frame) design))

(defmethod device-handle-click ((design cogtool-device))
  (emit-trace nil 'DEVICE-CLICK)
  (if *infer-transitions* (infer-transition design))
  (transition (get-cogtool-plist :destination-frame) design))

;; infer transition base on cursor position of click
(defun infer-transition (design)
  (let ((curwidget (lookup-widget (cursor-x design) (cursor-y design) (curframe design))))  
    (if curwidget
      (progn 
	  	  (cogtool-debug 1 "Clicked on widget: ~A" (name curwidget))          
        ;; lookup transition by action (left click)
        (let ((trans (gethash '((click left)) (transitions curwidget))))
          (if trans
            ;; if exists, follow transition
            (progn
		          (cogtool-debug 2 "Recognized click")
              (transition (target trans) design))
              ;; no such click transition
            (progn
		          (cogtool-debug 2 "Unrecognized clicks")))))
      ;; XXX: click on empty space
	    (cogtool-debug 1 "Clicked on nothing at x: ~D y: ~D" (cursor-x design) (cursor-y design)))))

;; find a widget by location within a frame
;; number number cogtool-frame => cogtool-widget
(defun lookup-widget (x y frame)
  (cogtool-debug 2 "Looking for a widget at x: ~D y: ~D in frame: ~A" x y (name frame))
  ;; stupid theta(n) linear search
  (let ((hits nil))
    (dolist (widget (widgets frame))
      ;; first check x coord
      (if (and (>= x (x widget)) (< x (+ (width widget) (x widget))))
          ;; x is in the widget box -- check y coord
          (if (and (>= y (y widget)) (< y (+ (height widget) (y widget))))
            ;; y is in the widget box -- save to hits list
            (setf hits (cons widget hits))
            ;; y is not in the widget box -- ignore it
          )
          ;; x is not in the widget box -- ignore it
      )
    )
    
    (cogtool-debug 2 "Found hits: ~S" (mapcar #'name hits))
    
    ;; Used to resolve based on the level property of widgets
    ;; For now, just return the first one in the hits list
    (first hits))
)

(defmethod device-speak-string ((design cogtool-device) string)
  (emit-trace nil 'DEVICE-SPEAK-STRING string)
  (schedule-event-relative
    (get-articulation-time string)
    #'(lambda () (transition (get-cogtool-plist :destination-frame) design))
    :module 'cogtool
    :details (format nil "DEVICE-SPEAK-STRING-DONE (~S)" string)))

;; Actually perform the transition to the specified frame
(defun transition (frame-name design)
  ;; do the transition
  (cogtool-debug 1 "transition called on ~S" frame-name)
  ;; only do the transition if not nil.
  (when (and frame-name (or (null (curframe design)) (not (equal frame-name (name (curframe design))))))
    (let ((frame (gethash frame-name (frames design))))
      (fresh-line)
      (emit-trace nil 'TRANSITION-TO (name frame))
      (setf (curframe design) frame)
      (when (and *infer-transitions* (speaker-text frame) (not (equal (speaker-text frame) "")))
	(speak-text (speaker-text frame))))
    (proc-display)
    (clear-cogtool-plist-element :destination-frame))
  (process-pending-wait))

(defun process-pending-wait ()
  (or (executing-wait *cogtool-design*)
      (let ((wait (pop (pending-waits *cogtool-design*))))
	(when wait
	  (let ((delay (car wait)) (label (cdr wait)))
	    (cogtool-debug 2 "delay: ~A" delay)
	    (system-wait-start delay label))))))

(defun system-wait-start (delay label)
  (emit-trace nil 'START-SYSTEM-WAIT delay label)
  (setf (visible-p *cogtool-design*) nil)
  (setf (executing-wait *cogtool-design*)
	(schedule-event-relative
	 delay
	 #'system-wait-finished
	 :priority :max
	 :module 'cogtool
	 :details (format nil "SYSTEM-WAIT-DONE (~A: ~D)" label delay))))

(defun system-wait-finished ()
  (emit-trace nil 'END-SYSTEM-WAIT)
  (setf (executing-wait *cogtool-design*) nil)
  (unless (process-pending-wait)
    (setf (visible-p *cogtool-design*) t)
    (proc-display)))

;; A hack for allowing the field of vision to go blank (the user
;; is wearing glasses that turn opaque) for 1.5 seconds at a time.
;; This thoroughly bizarre behavior turns out to be used in
;; some driving experiments I don't understand at all...I sure hope
;; I don't drive as if I were blind half the time!

(defvar *vision-suppression-interval* 0) ;; change to 1.5 for driving stuff
(defvar *vision-starts-suppressed* nil)

(when (> *vision-suppression-interval* 0)
  (format t "~%**** Vision suppressed every %A seconds ****`2%"))

(defun vision-suppressed ()
  (and (> *vision-suppression-interval* 0)
       (if (evenp (floor (mp-time) *vision-suppression-interval*))
	   *vision-starts-suppressed*
	   (not *vision-starts-suppressed*))))

;; If we are using EMMA, don't let it put in any noise, just use
;; the mean of any normally noisey values.

#+emma
(fmakunbound 'add-gaussian-noise) ; suppress any "redefining..." warning
#+emma
(defun add-gaussian-noise (x stddev)
  "Override the version in EMMA to add no noise."
  x)

#+emma
(defmethod initiate-eye-move :before ((eye-mod emma-vis-mod) (recog-time number) (r-theta vector))
  (emit-trace :vision 'PREPARE-EYE-MOVEMENT))

;;;
;;;  Run Code
;;;

;; This is the default set of values passed to sgp below; by setting 
;; *overridden-global-parameters* to a list of alternating keywords and values
;; before klm-p is called, one or more of those default global parameter values
;; can be overridden.
(defparameter *default-global-parameters* `(:v t
					    :trace-detail high
					    :esc t
					    :lf 0
					    :motor-feature-prep-time 0.001
					    ,@ #+emma '(:visual-encoding-factor 0.006 :visual-encoding-exponent 0.4) #-emma nil
					    ))

(when *cogtool-random-seed*
  (setq *default-global-parameters* `(:seed ,*cogtool-random-seed* ,@*default-global-parameters*)))

(defvar *overridden-global-parameters* '())

(defvar *start-with-mouse* nil)

(defvar *timeout* 600)

(defvar *cogtool-result* nil)

(defun cogtool-run-model ()
    (cogtool-debug 1 "Called cogtool-run-model")
    (define-design)
    (cogtool-debug 2 "Device designed")
    (let ((d *cogtool-design*))
      (cogtool-debug 2 "Intalling device ~S" d) 
      (install-device d))
    (when *start-with-mouse* 
      (cogtool-debug 1 "Moving hand to mouse") 
      (start-hand-at-mouse)) 
    (cogtool-debug 1 "Setting cursor position to 0, 0") 
    (set-cursor-position 0 0) 
    (proc-display) 
    (let ((*cogtool-plist*)) 
      (cogtool-debug 2 "Running model")
      (let ((result (run *timeout*)))
	(cond
	  ((null *why-not*))
	  ((eq *why-not* t) (whynot))
	  ((symbolp *why-not*) (whynot-fct (list *why-not*)))
	  (t (whynot-fct *why-not*)))
	result)))

(defmacro define-cogtool-model ((&key start-with-mouse timeout initial-frame-name) &rest forms)
  (let ((params (loop :with result := (copy-seq *default-global-parameters*)
		  :for (key val) :on *overridden-global-parameters* :by #'cddr
		  :do (setf (getf result key) val)
		  :finally (return result))))
    `(define-model cogtool-model
	 (cogtool-debug 1 "Setting params to ~A" ',params)
       (sgp ,@params)
       (sgp)
       (chunk-type (cogtool-visual-location (:include visual-location))
		   widget-name display-label auxiliary-label remote-label-of has-remote-label member-of-groups)
       (chunk-type (cogtool-button (:include visual-object)) ())
       (chunk-type (cogtool-link (:include visual-object)) ())
       (chunk-type (cogtool-context-menu (:include visual-object)) ())
       (chunk-type (cogtool-menu (:include visual-object)) ())
       (chunk-type (cogtool-submenu (:include visual-object)) ())
       (chunk-type (cogtool-menu-item (:include visual-object)) ())
       (chunk-type (cogtool-text-box (:include visual-object)) ())
       (chunk-type (cogtool-text (:include visual-object)) ())
       (chunk-type (cogtool-pull-down-list (:include visual-object)) ())
       (chunk-type (cogtool-pull-down-item (:include visual-object)) ())
       (chunk-type (cogtool-list-box-item (:include visual-object)) ())
       (chunk-type (cogtool-radio-button (:include visual-object)) ())
       (chunk-type (cogtool-checkbox (:include visual-object)) ())
       (chunk-type (cogtool-graffiti (:include visual-object)) ())
       (chunk-type (cogtool-non-interactive (:include visual-object)) ())
       (chunk-type (cogtool-group (:include visual-object)) ())
       (chunk-type (drag (:include motor-command)) hand)
       (chunk-type (graffiti-gesture (:include motor-command)) key)
       (chunk-type klm state)
       (chunk-type stop)
       (add-dm
	 (goal isa klm state 1)
	 (graffiti-gesture isa motor-command)
	 (cogtool-button isa cogtool-button)
	 (cogtool-link isa cogtool-link)
	 (cogtool-context-menu isa cogtool-context-menu)
	 (cogtool-menu isa cogtool-menu)
	 (cogtool-submenu isa cogtool-submenu)
	 (cogtool-menu-item isa cogtool-menu-item)
	 (cogtool-text-box isa cogtool-text-box)
	 (cogtool-text isa cogtool-text)
	 (cogtool-pull-down-list isa cogtool-pull-down-list)
	 (cogtool-pull-down-item isa cogtool-pull-down-item)
	 (cogtool-list-box-item isa cogtool-list-box-item)
	 (cogtool-radio-button isa cogtool-radio-button)
	 (cogtool-checkbox isa cogtool-checkbox)
	 (cogtool-graffiti isa cogtool-graffiti)
	 (cogtool-non-interactive isa cogtool-non-interactive)
	 (cogtool-group isa cogtool-group))
       (setq *start-with-mouse* ,start-with-mouse)
       (setq *timeout* ,timeout)
       ,@forms
       (goal-focus goal))))

(cogtool-debug 2 "Clearing ACT-R state") 
(clear-all) 

(terpri)
(princ "Cogtool Version: (Revision: 3331)")

;; ==== Design ====

(defvar *frame-definitions* nil)

(defun cogtool-frame-fn-1 ()

  (let ((frames (frames *cogtool-design*)) frame widget groups)

  ;; ==== New Frame ====

    (setq frame (make-instance 'cogtool-frame :name "Frame 1" :speaker-text "Now is the winter of our discontent made glorious summer by this sun of York"))

    (setq groups '())

    (resolve-widget-refs frame groups)

    (setf (gethash "Frame 1" frames) frame)))

(push #'cogtool-frame-fn-1 *frame-definitions*)

(defun cogtool-frame-fn-2 ()

  (let ((frames (frames *cogtool-design*)) frame widget groups)

  ;; ==== New Frame ====

    (setq frame (make-instance 'cogtool-frame :name "Frame 2"))

    (setq groups '())

    (resolve-widget-refs frame groups)

    (setf (gethash "Frame 2" frames) frame)))

(push #'cogtool-frame-fn-2 *frame-definitions*)

(setq *frame-definitions* (nreverse *frame-definitions*))

(defun define-design ()
  (setq *cogtool-design* (make-instance 'cogtool-device))
  (loop for f in *frame-definitions* do (funcall f)))


;; ==== Script ==== 

(setq *infer-transitions* t) ; note that this is also t in CT-E, though not in normal CogTool

(define-cogtool-model (:start-with-mouse t :timeout 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (p start) adds the goal chunk ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p start
  =goal>
    isa	klm
    state 1
==>
  !eval! (transition "Frame 1" *cogtool-design*)
  +goal>
    isa	klm
    state 2
)

(p listen
  =goal>
    isa klm
    state 2
  =aural-location>
    isa audio-event
    kind word
    location loudspeaker
  ?aural>
    state free
==>
  +aural>
    isa sound
    event =aural-location
  +goal>
    isa klm
    state 3)

(p wait-for-listening-to-finish
  =goal>
    isa klm
    state 3
  ?aural>
    state free
  ?manual>
    preparation free
==>
  +manual>
    isa press-key
    key F
  +goal>
    isa klm
    state stop)

)

(load-pending-cogtool-files)

(setq *cogtool-result* (cogtool-run-model))
