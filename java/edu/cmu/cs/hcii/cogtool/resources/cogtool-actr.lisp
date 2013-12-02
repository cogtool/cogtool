;#-EMMA (compile-and-load (translate-logical-pathname "ACT-R6:extras;emma;emma.lisp"))

;; Debug flag -- set to 1 for basic tracing, 2 for verbose, and 0 for errors only
(defparameter *cogtool-debug* 0)

;; Debugging tool -- if non-nil whynot-fct will be called on this production name at end of run
(defparameter *why-not* nil)

(defvar *suppress-trace* nil)

(defmacro cogtool-debug (level message &rest args)
  ;; Note that this is a macro, so neither message nor args are eval'd
  ;; unless we are at or above the given debug level.
  `(when (>= *cogtool-debug* ,level)
     (format t "~&~5TCogTool: ~@?~%" ,message ,@args)))

(defmacro cogtool-debug-when (level &body body)
  `(when (>= *cogtool-debug* ,level)
     ,@body))

;; Note that the following is ignored if this variable has already been initialized
(defvar *system-wait-blocks-only-vision* nil)

;; Note that the following is ignored if this variable has already been initialized
(defvar *cogtool-files-to-load* nil)

(defun load-pending-cogtool-files ()
  (when *suppress-trace*
    (push :suppress-trace *features*))
  (loop for file in *cogtool-files-to-load*
        do (load file :verbose (not *suppress-trace*))
        finally (setq *cogtool-files-to-load* nil)))

(defvar *log-file* nil)
(defvar *log-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Begin SANLab-CM Modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; I believe CogTool, because it runs its productions in strictly sequential
;;; lock-step, has no real need of move-ids, but we must still emit them
;;; so SANLab-CM can parse the trace. We just peg them all to zero. This is
;;; substantially simpler for us, as we don't have to override anywhere near
;;; as many ACT-R functions and methods, and thus will have an easier time
;;; tracking future changes in ACT-R.

;;; Since we never generate real move-ids, next-id is never called
; (let ((id 0)) (defun next-id () (incf id)))
(defun next-id () (error "next-id unexpectedly called"))

(fmakunbound 'prepare-movement)

(defgeneric prepare-movement (module movement)
  (:documentation "Tell <module> to prepare <movement>."))

(defmethod move-id (arg)
  0)

(defmethod prepare-movement ((module pm-module) (mvmt movement-style))
  ;(format t "~%>>>entering prepare-movement ~s ~s" mvmt (move-id mvmt))
  (change-state module :prep 'BUSY :proc 'BUSY)
  (setf (fprep-time mvmt) 
        (rand-time (compute-prep-time module mvmt)))
  (setf (last-prep module) mvmt)
  (queue-command :command 'preparation-complete :where (my-name module)
                 :params (list :move-id (move-id mvmt))
                 :time (fprep-time mvmt) :randomize nil)
  (when (and (waiting-for-proc-p module) (null (exec-queue module))
             (exec-immediate-p mvmt))
    (setf (set-proc-p mvmt) t)
    (queue-command :time (+ (fprep-time mvmt) (init-time module))
                   :where (my-name module) :command 'change-state 
                   :params '(:proc free) :randomize nil))
  ;(format t "~%>>>returning from prepare-movement"))
  )

(defclass cogtool-peck-recoil (peck-recoil)
  ((move-id :accessor move-id :initarg :move-id :initform 0)))

(defmethod peck-recoil ((mtr-mod motor-module) &key hand finger r theta (move-id 0))
  (unless (or (check-jam mtr-mod) (check-specs hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod
                      (make-instance 'cogtool-peck-recoil :hand hand :finger finger
                                     :r r :theta theta :move-id move-id))))

(fmakunbound 'hand-to-home)

(defgeneric hand-to-home (mtr-mod &key move-id) 
  (:documentation  "Moves the right hand to the home row position from the mouse loc"))

(defmethod hand-to-home ((mtr-mod motor-module) &key (move-id 0))
  (unless (equal (loc (right-hand mtr-mod)) #(7 4))
    (let ((polar (xy-to-polar (loc (right-hand mtr-mod)) #(7 4))))
      (point-hand mtr-mod :hand 'right :r (vr polar) 
                  :theta (vtheta polar) :twidth 4.0
                  :move-id move-id :allow-other-keys t))))

(fmakunbound 'preparation-complete)

(defgeneric preparation-complete (module &key move-id)
  (:documentation "Method to be called when movement preparation is complete."))

(defmethod preparation-complete ((module pm-module) &key (move-id 0))
  (declare (ignore move-id))
  (change-state module :prep 'free)
  (when (last-prep module)
    (if (exec-immediate-p (last-prep module))
      (setf (exec-queue module)
            (append (exec-queue module) (mklist (last-prep module))))
      (when (and (plusp (init-stamp module))
                 (>= (mp-time) (+ (init-stamp module) (init-time module))))
        (change-state module :proc 'FREE))))
  (maybe-execute-movement module))

(defgeneric perform-movement (module movement)
  (:documentation "Have <module> perform <movement>."))

(defmethod perform-movement ((module pm-module) (mvmt movement-style))
  (queue-command :time (init-time module) :where (my-name module) 
                 :params (list :move-id (move-id mvmt))
                 :command 'INITIATION-COMPLETE)
  (change-state module :proc 'BUSY :exec 'BUSY)
  
  ;;; DAN
  ;(setf (init-stamp module) (mp-time *mp*))
  
  (setf (init-stamp module) (mp-time))
  
  (setf (exec-time mvmt) (compute-exec-time module mvmt))
  (setf (finish-time mvmt) (compute-finish-time module mvmt))
  (queue-output-events module mvmt)
  (queue-finish-event module mvmt))

#+EMMA
(defmethod encoding-complete ((eye-mod emma-vis-mod) loc scale &key (requested t) (move-id 0))
  (declare (symbol scale) (ignore move-id))
  (setf (moving-attention eye-mod) NIL)
  (change-state eye-mod :exec 'free :proc 'FREE)
  
  (let ((return-obj
         (if (and (shift-target eye-mod)
                  (object-present-p eye-mod (shift-target eye-mod)))
             (shift-target eye-mod)
           (get-obj-at-location eye-mod loc scale))))
    (unless return-obj
      (clear-attended eye-mod)
      (setf (attend-failure eye-mod) t)
      (schedule-event-relative 0 'no-visual-object-found :maintenance t :module :vision :output 'medium :details "No visual-object found")
      (return-from encoding-complete nil))
    (set-attended eye-mod (chunk-visicon-entry return-obj))
    (attend-to-object eye-mod return-obj :requested requested)
    return-obj))

(fmakunbound 'initiation-complete)

(defmethod initiation-complete ((module pm-module) &key (move-id 0))
  (declare (ignore move-id))
  (change-state module :proc 'FREE))

(defgeneric queue-finish-event (module movement)
  (:documentation "Queue the FINISH-MOVEMENT associated with <movement>."))


(defmethod queue-finish-event ((module pm-module) (mvmt movement-style))
  (queue-command :time (finish-time mvmt) :command 'finish-movement
                 :params (list :move-id (move-id mvmt))
                 :where (my-name module)))

(fmakunbound 'finish-movement)

(defgeneric finish-movement (module &key move-id) 
  (:documentation "Method called when a movement finishes completely."))

(defmethod finish-movement ((module pm-module) &key (move-id 0))
  (declare (ignore move-id))
  (change-state module :exec 'free)
  (maybe-execute-movement module))

(defclass cogtool-hand-ply (hand-ply)
  ((move-id :accessor move-id :initarg :move-id :initform 0)))

(defmethod queue-output-events ((mtr-mod motor-module) (self cogtool-hand-ply))
  (queue-command
   :where :MOTOR :command 'MOVE-A-HAND :time (exec-time self)
   :params (list (hand self) (r self) (theta self) :move-id (move-id self))))

(defmethod queue-output-events ((mtr-mod motor-module) (self punch))
  (queue-command 
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (list (move-a-finger mtr-mod (hand self) (finger self) 0 0) :move-id (move-id self))
   ;;; DAN
   :from :motor
   :output 'medium))

(defmethod queue-output-events ((mtr-mod motor-module) (self cogtool-peck-recoil))
  (queue-command
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (list (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                        (vector (r self) (theta self)))
         :move-id (move-id self))
   ;;; DAN
   :from :motor
   :output 'medium))

(fmakunbound 'press-key)

(defgeneric press-key (mtr-mod key &key move-id)
  (:documentation  "High-level interface to press a key: Look up the command and execute it."))

(defmethod press-key ((mtr-mod motor-module) key &key (move-id 0))
  (when (stringp key)
    (setf key (read-from-string key)))
  (let ((command (key->cmd 
                  ;;; DAN
                  ;(device-interface *mp*) 
                  
                  (current-device-interface )
                                    
                  key)))
    (if (null (first command))
        (print-warning "No press-key mapping available for key ~s." key)
      (apply (first command) mtr-mod (append (rest command) (list :allow-other-keys t :move-id move-id))))))

(fmakunbound 'output-key)

(defgeneric output-key (devin keyloc &key move-id) 
  (:documentation  "Request that the device register a key output for the key at a given location."))

(defmethod output-key ((devin device-interface) (keyloc vector) &key (move-id 0))
  (declare (ignore move-id))
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
                   force-transition
              &allow-other-keys)
  (cogtool-debug 2 "Setting plist at time ~A: ~S" (mp-time) keys)
  (unless df-supplied
    (setq keys (list* :destination-frame destination-frame keys)))
  (when (and destination-frame (or (null (curframe *cogtool-design*)) force-transition)) 
    ;; sets the initial frame
    (transition destination-frame *cogtool-design*))
  (when (or key hand)
    (incf (pending-keystrokes *cogtool-design*))
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

; ACT-R changed this, breaking our version. Which makes it seem like the right thing to do is to use
; our own rather than ACT-R's, to insulate us from further changes. A bit of a two-edge sword, of course,
; since if ACT-R actually changes what stuff looks like we won't pick that up for free; but in that case
; we've got bigger problems since our parsing of the results in our Java code will go all to pieces. So, on
; balance, it seems best to just stick with the old ACT-R format string.
(defconstant +old-actr-format-event-event-string+ 
    (formatter "~10,3f ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va ~:[~*~a~@[ ~a~]~{ ~a~}~;~a~*~*~*~] ~@[Waiting for: ~A~]"))

(defun emit-trace (pseudo-module &rest args)
  (unless *suppress-trace*
    (let ((*print-pretty* nil)
      (details (format nil #.(formatter "~{~S~^ ~}") args)))
      (fresh-line t)
      (format t +old-actr-format-event-event-string+
          (mp-time)
          nil nil
          nil nil nil
          (max-module-name-length) (or pseudo-module :cogtool)
          details details
          nil nil nil nil))))

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
;;;      (id (and dmo (id dmo))))
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
   (pending-keystrokes :accessor pending-keystrokes :initform 0)
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
   (textual-cue :accessor textual-cue :initarg :textual-cue :initform nil)
   (has-remote-label :accessor has-remote-label :initarg :has-remote-label :initform nil)
   (remote-label-of :accessor remote-label-of :initarg :remote-label-of :initform nil)
   (member-of-groups :accessor member-of-groups :initarg :member-of-groups :initform nil)
   ;; we never actually use transitions in CogTool per se, but need to keep them around
   ;; for Leonghwee's SNIF-ACT stuff
   (transitions :accessor transitions :initarg :transitions
        :initform (make-hash-table :test #'equal)
        :documentation "hashtable of (action => transition)")
   (is-back-button :accessor back-button-p :initarg :is-back-button :initform nil)))

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
    (loop for w in (append groups (widgets frame))
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
    (let ((result (frame-vis-locs (curframe design))))
      (cogtool-debug 2 "vis-locs = ~S" result)
      (cogtool-debug-when 3 (pprint-chunks))
      result))))

(defun frame-vis-locs (frame)
  (or (vis-locs frame)
      (setf (vis-locs frame) (make-frame-vis-locs frame))))

(defun make-frame-vis-locs (frame)
  (cogtool-debug 1 "Making vis-locs for ~S" frame)
  (cogtool-debug 3 "Widgets: ~S" (widgets frame))
  (let ((groups (let ((tab (make-hash-table)))
          (labels ((find-groups (thing)
                      (loop for g in (member-of-groups thing)
                    do (when g
			 (setf (gethash g tab) t)
			 (find-groups g)))))
            (mapc #'find-groups (widgets frame)))
          (loop for g being each hash-key of tab
            collect g))))
    (cogtool-debug 3 "Groups: ~S" groups)
    (loop for thing in (append groups (widgets frame))
      do (cogtool-debug 2 "vis-loc info for ~S, ~A, ~A, ~S" thing (name thing) (wtype thing) (member-of-groups thing))
      nconc (if (member-of-groups thing)
          (loop for g in (member-of-groups thing)
            collect (frame-vis-loc-template thing g))
          (list (frame-vis-loc-template thing)))
      into result
      finally (return (define-chunks-fct result)))))
      
;;; Note that this function is redefined in ct-explorer-support.lisp.
;;; If you change it here, consider whether or not those changes need
;;; to be propgated thither.      
(defun frame-vis-loc-template (thing &optional group)
  (let ((name (name thing)))
    `(,(intern name)
      isa cogtool-visual-location
      value ,(if *widget-value-is-title* (title thing) name)
      widget-name ,name
      display-label ,(title thing)
      screen-x ,(+ (x thing) (floor (width thing) 2))
      screen-y ,(+ (y thing) (floor (height thing) 2))
      height ,(height thing)
      width ,(width thing)
      kind ,(wtype thing)
      is-back-button ,(back-button-p thing)
      textual-cue ,(textual-cue thing)
      remote-label-of ,(remote-label-of thing)
      has-remote-label ,(has-remote-label thing)
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
  (cogtool-debug 2 "Handling keypress: ~S" key)
  (emit-trace nil 'DEVICE-PRESS-KEY key)
  (when (zerop (decf (pending-keystrokes design)))
    (transition (get-cogtool-plist :destination-frame) design)))

(defmethod device-handle-click ((design cogtool-device))
  (emit-trace nil 'DEVICE-CLICK)
  (if *infer-transitions* (infer-transition design))
  (transition (get-cogtool-plist :destination-frame) design))

;; infer transition base on cursor position of click
(defun infer-transition (design)
  (cogtool-debug 2 "Inferring transition")
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
     :details (format nil "SYSTEM-WAIT-DONE ~S ~D" label delay))))

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
                        :trace-filter cogtool-trace-filter
                        :esc t
                        :lf 0
                        :motor-feature-prep-time 0.001
                        ,@ #+emma '(:visual-encoding-factor 0.006 :visual-encoding-exponent 0.4) #-emma nil
                        ))

(defun cogtool-trace-filter (evt)
  (not *suppress-trace*))

(defvar *overridden-global-parameters* '())

(defvar *start-with-mouse* nil)

(defvar *timeout* 600)

(defvar *cogtool-result* nil)

(defun cogtool-run-model ()
  (let ((start (get-internal-run-time)))
    (labels ((f ()
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
          (let ((result (run *timeout*)) (end (get-internal-run-time)))
            (format t "Processor time required: ~,2F seconds.~%" (* (- end start) #.(/ 1.0 internal-time-units-per-second)))
            (cond
             ((null *why-not*))
             ((eq *why-not* t) (whynot))
             ((symbolp *why-not*) (whynot-fct (list *why-not*)))
             (t (whynot-fct *why-not*)))
            result))))
      (if *log-file*
        (with-open-file (*log-stream* *log-file* :direction :output :if-exists :append :if-does-not-exist :create) (f))
    (f)))))

(defmacro define-cogtool-model ((&key start-with-mouse timeout initial-frame-name) &rest forms)
  (let ((params (loop :with result := (copy-seq *default-global-parameters*)
          :for (key val) :on *overridden-global-parameters* :by #'cddr
          :do (setf (getf result key) val)
          :finally (return result))))
    `(define-model cogtool-model
       (cogtool-debug 1 "Setting params to ~A" ',params)
       (sgp ,@params)
       (set-audloc-default :location loudspeaker :onset highest)
       ;; The following file will typically not exist, but can be created to
       ;; exert a little last minute control over things, if desired. The pathname
       ;; probably only works on Macintosh, though.
       (load "/tmp/act-r-kludge.lisp" :verbose t :print t :if-does-not-exist nil)
       (unless *suppress-trace*
     (print *features*)
     (sgp))
       (chunk-type (cogtool-visual-location (:include visual-location))
           widget-name display-label textual-cue remote-label-of has-remote-label member-of-group is-back-button)
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
       (chunk-type klm state suppress-re-encoding)
       (chunk-type stop)
       (add-dm
     (goal isa klm state 1 suppress-re-encoding nil)
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

