;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2005 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : audio.lisp
;;; Version     : 2.3
;;; 
;;; Description : Source for RPM's Audition Module
;;; 
;;; Bugs        : * 
;;; 
;;; Todo        : * [x] Should there be audio buffer stuffing as well?
;;;             : * [X] Add a set-audioloc-defaults command like the set-visloc-
;;;             :       default to give more control over stuffing so that
;;;             :       the :hear-newest-only parameter can go away.
;;;             : * [ ] Change the request function so that modifiers and ranges
;;;             :       are accepted and add pitch as an option (alternatively
;;;             :       convert the audicon over to holding chunks and use the
;;;             :       more general chunk matching tools...)
;;;             : * [ ] Should it automatically stuff aural-location when there
;;;             :       are events in the audicon and the buffer is cleared 
;;;             :       instead of waiting for a change before doing so?
;;;             : * [ ] Deal with possible long run timing issues due to 
;;;             :       floating point imprecision since detection time involves 
;;;             :       an addition of a "current" time which could be too big
;;;             :       to represent milliseconds with a delay that's likely to be
;;;             :       in the 50-200ms range.
;;; ----- History -----
;;; 
;;; 2005.01.07 mdb [act6a1]
;;;             : Transition to ACT6 stuff.
;;; 2005.01.08 Dan
;;;             : More updates to move to 6
;;;             :
;;;             : Changed aural-location to audio-event in request-audio-module
;;;             : (alternatively audio-event could be changed to aural-location
;;;             : in reset-audio-module but this way is backward compatible 
;;;             : to ACT-R 5)
;;;             :
;;;             : Removed the :offset parameter in the scheduling of find-sound
;;;             : in request-audio-module because it isn't defined in find-sound
;;;             :
;;;             : Changed find-sound to put the chunk into the buffer
;;;             :
;;;             : Changed find-sound because the chunk that comes in doesn't
;;;             : have the same name as the one in the audicon because it will
;;;             : be a copy from the buffer.  
;;;             :
;;;             : To get around that (for now at least) I've added an id slot
;;;             : to the audio-event chunk-type which will always have the
;;;             : original value.
;;;             :
;;;             : The event->dmo method was modified to set that value.
;;;             : 
;;;             : Changed audio-encoding-complete so that it sets the chunk
;;;             : in the aural buffer.  
;;;             :
;;;             : Related to the issue above with sound event names - the 
;;;             : sound put into the buffer has its event set to the "original" 
;;;             : event name (the id slot of the audio-event) which doesn't 
;;;             : correspond to the name of a chunk in DM, but it will match
;;;             : an audio-event with that value in its id slot (assuming the
;;;             : aural-location buffer has cleared so that the chunk goes to
;;;             : DM).
;;;             : 
;;;             : That seems reasonable for now at least.  
;;;             :
;;;             : Put the aural-location stuffing in:
;;;             ; Added the :around method for new-sound-event
;;;             : Added the stuff-sound-buffer function.
;;;             :
;;; 2005.01.09 Dan
;;;             : Added the clearing of the audicon to the reset-audio-module
;;;             : function.
;;;             : Added the word chunk to the audicon.
;;; 2005.01.10 Dan
;;;             : Maintain the stuffed slot of the audio module now since
;;;             : I added the buffer stuffing back in.
;;; 2005.01.11 mdb
;;;             : Put in parameter doc strings.
;;; 2005.01.21 Dan
;;;             : * Removed use of buffer-chunk and replaced with buffer-read.
;;; 2005.01.21 Dan
;;;             : * Wrapped the proclaim in an eval-when because otherwise
;;;             :   it may not actually affect the compilation.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium" to some of the events that are
;;;             :   scheduled to play friendly with the new detail level.
;;; 2005.04.23 Dan
;;;             : * Updated find-sound so that it indicates whether the chunk
;;;             :   being put into the buffer was stuffed or not.
;;;             : * Changed stuff-sound-buffer to indicate that.
;;;             : * Removed the check of stuffed from query-audio-module.
;;;             : * Added attended as a possible query but I'm unsure if I've
;;;             :   got the testing quite right...
;;; 2005.04.29 Dan
;;;             : * Added a print-audicon command that works basically like
;;;             :   print-visicon for visual - it prints a table of info for
;;;             :   the sound-events currently in the audicon.
;;; 2005.07.22 Dan
;;;             : * Updated the module definition to use the pm-module-request
;;;             :   method and renamed the audio-module-request function 
;;;             :   accordingly.
;;; 2005.08.03 Dan
;;;             : * Added a find-sound-failure event to the trace when find-
;;;             :   sound fails.  Also adjusted the find-sound event scheduling
;;;             :   so that it gets output in the medium level trace detail.
;;; 2005.08.10 Dan
;;;             : * Commented out the offset value in the audio-event request
;;;             :   because it wasn't used.
;;; 2005.12.14 Dan
;;;             : * Added :sound-decay-time parameter which seems to have been
;;;             :   lost in the move to 6.
;;; 2006.01.04 Dan
;;;             : * Removed a duplicate instance of :tone-recode-delay in the
;;;             :   case of the parameter handling in params-audio-module and 
;;;             :   replaced it with :tone-detect-delay (which is what it 
;;;             :   should have been). 
;;; 2006.03.24 Dan
;;;             : * Added a new parameter called :hear-newest-only and changed 
;;;             :   stuff-sound-buffer to include :onset :highest when that's
;;;             :   set because it seems like often one might want the newest 
;;;             :   sound to be the one that gets stuffed into the buffer. 
;;;             :   The default is nil to keep it compatible with the old
;;;             :   version for now, but should it default to t?
;;; 2006.05.03 Dan
;;;             : * Fixed a bug in pm-module-request for aural-location requests
;;;             :   in specifying the location (it was testing the onset slot.)
;;; 2006.05.03 Dan
;;;             : * Turns out that loc-failure and attend-failure weren't
;;;             :   really being set/cleared so add that in now.
;;; 2006.05.03 Dan
;;;             : * Fixed attend-sound so that it checks the current-audicon
;;;             :   so that old sounds get purged as needed.
;;; 2006.09.08 Dan
;;;             : * Changed several parameter tests from posnum to nonneg.
;;; 2006.11.20 Dan
;;;             : * Fixed the bug in stuff-sound-buffer because it should be
;;;             :   :onset 'highest not :highest...
;;;             : * Changed the version to 2.2 (dropped the a1) and updated the
;;;             :   doc string so that it doesn't say "first pass".
;;; 2006.12.20 Dan 
;;;             : * Changed the version in the class definition too...
;;; 2007.01.03 Dan
;;;             : * Changed the scheduling of the stuff-sound-buffer event
;;;             :   to :output nil and :maintenance t because the set-buffer-chunk 
;;;             :   ... requested nil shows that the stuffing occurs already - 
;;;             :   no point in two events showing for the same thing.
;;;             : * Also changed the test for the empty buffer slightly because
;;;             :   buffer-read is a bit of a hack for that purpose.
;;;             : * Took attended out of the audio-event chunk-type and made it
;;;             :   a request parameter instead (actually just started using the 
;;;             :   request parameter that was already there).
;;; 2007.01.04 Dan
;;;             : * Took the string column out of the audicon printing because
;;;             :   its going to be the same as content when it's provided.
;;;             : * Similarly, changed the content column to be printed with
;;;             :   ~s instead of ~a so that strings are differentiated from
;;;             :   symbols for content.
;;;             : * Added a column for detectable to the audicon printing.
;;;             : * Changed attend-sound to check exec-s instead of check-jam
;;;             :   because the attend-sound doesn't set the preparation state
;;;             :   to busy (which is what check-jam looks at).
;;;             : * Use randomize-time in scheduling the attend-sound-failure
;;;             :   event.
;;;             : * Removed the update function from the module definition
;;;             :   because it didn't do anything.
;;;             : * Fixed new-sound-event to better check for model/mp/module
;;;             :   and report a warning if not available - also switched the
;;;             :   return value to t/nil instead of returning the stuff-sound
;;;             :   scheduler event.
;;;             : * Converted the new-*-sound methods to functions so that
;;;             :   parameter validation is handled explicitly.  So that
;;;             :   ACT-R warnings can be printed instead of Lisp errors being
;;;             :   generated when invalid values are used (yes there are more
;;;             :   "CLOS-y" ways of doing it, but call me old fashioned...).
;;;             : * Took the optional instr parameter out of new-other-sound
;;;             :   because the string component of the sound-events doesn't
;;;             :   really have a purpose. [Or am I missing something?]
;;; 2007.01.10 Dan
;;;             : * Added location and kind optional parameters to new-other-sound.
;;;             : * Changed the call to get-articulation-time because the 
;;;             :   speech module is no longer needed as a parameter (though the
;;;             :   module itself still needs to exist).
;;; 2007.01.18 Dan
;;;             : * Turns out that stuff-sound-buffer can't be a maintenance
;;;             :   event because that event needs to be able to trigger
;;;             :   conflict-resolution because until the schedule-set-buffer-chunk
;;;             :   event gets scheduled other model events could have pushed
;;;             :   conflict-resolution into the future... (probably doesn't
;;;             :   make sense to anyone else, but I know what it means)
;;; 2007.03.29 Dan
;;;             : * Incremented version to 2.3.
;;;             : * Added the set-audloc-default command and depricated the
;;;             :   hear-newest-only parameter because of that.
;;; 2007.04.03 Dan
;;;             : * Some cleanup of the set-audloc-default command - better
;;;             :   testing of params and validity (allowing none for example).
;;; 2008.06.03 Dan
;;;             : * Added an after method for clear to clear the error states
;;;             :   as is done for vision, but maybe that needs to move up to
;;;             :   the attn-module class method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "DMI" "ACT-R6:support;dmi")
(require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")

#+:allegro (eval-when (:compile-toplevel :Load-toplevel :execute)
             (setf *enable-package-locked-errors* nil))

(eval-when (:compile-toplevel :Load-toplevel :execute)
  (proclaim '(optimize (speed 3) (space 0))))


(defclass audio-module (attn-module)
  ((audicon :accessor audicon :initarg :audicon :initform nil)
   (digit-detect-delay :accessor digit-detect-delay :initarg :digit-dtct-dly
                       :initform 0.300)
   (digit-recode-delay :accessor digit-recode-delay :initarg :digit-rec-dly
                       :initform 0.500)
   (digit-duration :accessor digit-duration :initarg :digit-duration 
                   :initform 0.600)
   (tone-detect-delay :accessor tone-detect-delay :initarg :tone-dtct-dly
                      :initform 0.050)
   (tone-recode-delay :accessor tone-recode-delay :initarg :tone-rec-dly 
                      :initform 0.285)
   (sound-decay-time :accessor decay-time :initarg :decay-time
                     :initform 3.000)
   (default-spec :accessor default-spec :initform (list :attended nil)))
   (:default-initargs
       :version-string "2.3"
     :name :AUDIO))


#|
(defmethod reset-module :after ((aud-mod audio-module))
  (setf (audicon aud-mod) nil))
|#

(defmethod initialize-instance :after ((aud-mod audio-module) &key)
#|
  (setf (state-dmo aud-mod)
        (make-dme 'audio-state 'module-state
                  '(module :audio modality free processor free preparation free
                    execution free)
                  :where :external))

|#  )


(defmethod silent-events ((aud-mod audio-module))
  (awhen (next-detectable-sound aud-mod (mp-time))
    (schedule-event-relative it 'detectable-audicon
                             :destination :audio  :module :audio )))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Sound events.
;;;; ---------------------------------------------------------------------- ;;;;

(defclass sound-event ()
  ((onset :accessor onset :initarg :onset 
          :initform (mp-time))
   (offset :accessor offset :initarg :offset :initform nil)
   (string :accessor snd-string :initarg :string :initform nil)
   (duration :accessor duration :initarg :duration :initform 0)
   (content :accessor content :initarg :content :initform nil)
   (content-delay :accessor delay :initarg :delay :initform nil)
   (kind :accessor kind :initarg :kind :initform 'SPEECH)
   (attended-p :accessor attended-p :initform nil :initarg :attended-p)
   (location :accessor location :initarg :location :initform 'EXTERNAL)
   (sname :accessor sname :initform (new-name-fct "SOUND") :initarg :sname)
   (ename :accessor ename :initform (new-name-fct "AUDIO-EVENT"))
   (recode :accessor recode :initarg :recode :initform nil)
   (pitch :accessor pitch :initform 'middle :initarg :pitch)
   (snd-dmo :accessor snd-dmo :initform nil)
   (evt-dmo :accessor evt-dmo :initform nil)
   ))


(defmethod initialize-instance :after ((self sound-event) &key)
  (unless (offset self)
    (when (and (numberp (onset self)) (numberp (duration self)))
      (setf (offset self) (+ (onset self) (duration self))))))

(defgeneric detect-at-time (evt)
  (:documentation "Returns the time at which an event becomes detectable."))

(defmethod detect-at-time ((evt sound-event))
  (ms-round (+ (onset evt) (delay evt))))


(defgeneric detectable-p (evt)
  (:documentation  "Returns T if the given sound event is detectable."))

(defmethod detectable-p ((evt sound-event))
  (>= (mp-time) (detect-at-time evt)))


(defgeneric finished-p (evt)
  (:documentation  "Returns T if the given sound-event is finished."))

(defmethod finished-p ((evt sound-event))
  (>= (mp-time) (offset evt)))


(defgeneric detectable-time (evt)
  (:documentation  "Returns the time at which the given sound event will become detectable."))

(defmethod detectable-time ((evt sound-event))
  (+ (onset evt) (delay evt)))


(defclass digit-sound-evt (sound-event)
  ()
  (:default-initargs
    :kind 'DIGIT
    :duration  (rand-time (digit-duration (get-module :audio)))
    :delay (rand-time (digit-detect-delay (get-module :audio)))
    :recode (digit-recode-delay (get-module :audio))
    :sname (new-name-fct "DIGIT")))


(defmethod initialize-instance :after ((self digit-sound-evt) &key)
  (setf (content self) (snd-string self)))


;;; TONE-SOUND-EVENT      [Class]
;;; Date        : 97.04.03
;;; Description : Class for tone events.
;;;             : The CONTENT slot should be the tone frequency.

(defclass tone-sound-evt (sound-event)
  ()
  (:default-initargs
    :string ""
    :kind 'TONE
    :content 1000
    :delay (rand-time (tone-detect-delay (get-module :audio)))
    :recode (tone-recode-delay (get-module :audio))
    :sname (new-name-fct "TONE")))


(defmethod initialize-instance :after ((self tone-sound-evt) &key)
  (cond ((> (content self) 1500) (setf (pitch self) 'high))
        ((< (content self) 900) (setf (pitch self) 'low))))


(defclass word-sound-evt (sound-event)
  ()
  (:default-initargs
    :kind 'WORD
    :delay (rand-time (digit-detect-delay (get-module :audio)))
    :sname (new-name-fct "WORD")
    :duration 0
    :recode nil
    ))

(defmethod initialize-instance :after ((self word-sound-evt) &key)
  (when (or (null (duration self)) (zerop (duration self)))
    (setf (duration self)
          (get-articulation-time (snd-string self))))
  (unless (recode self)
    (setf (recode self)
          ;; change the value below to make "hearing" faster
          (ms-round (max
                     (/ (duration self) 2)
                     (- (duration self) 0.150)))))
  (setf (content self) (snd-string self))
  (setf (offset self) (+ (onset self) (duration self)))
  )

(defclass sound-event-spec (sound-event spec)
  ()
  (:default-initargs
    :check-slots #(onset kind attended-p pitch location)
    :onset :IGNORE
    :kind :IGNORE
    :attended-p :IGNORE
    :pitch :IGNORE
    :offset :IGNORE
    :location :IGNORE
    ))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Toplevel commands.
;;;; ---------------------------------------------------------------------- ;;;;

;;; DAN
;;; Need to have the buffer stuffing still happen for aural-location because
;;; unit 3's sperling model depends on it.

(defmethod new-sound-event :around ((evt sound-event))
  (let ((evt (call-next-method)))
    (when evt
      (schedule-event (detect-at-time evt) 'stuff-sound-buffer
                      :module :audio
                      :destination :audio
                      :output nil)
      t)))


(defun stuff-sound-buffer  (audio-mod) 
  (when (query-buffer 'aural-location '((buffer . empty)))
    (apply 'find-sound  (append (list audio-mod) (default-spec audio-mod) '(:stuffed t)))))

(defgeneric new-sound-event (evt)
  (:documentation "Handles the bookkeeping when a new sound event is created."))

(defmethod new-sound-event ((evt sound-event))
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (aif (get-module :audio)
         (progn
           (push evt (audicon it))
           evt)
         (print-warning "No Audio module found.  Cannot create a new sound.")))))


(defun new-digit-sound (digit &optional (onset (mp-time)))
  "Creates and adds a digit sound <digit>, a number, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp digit))
           (print-warning "Digit must be a number.  No new digit sound created."))
          ((not (numberp onset))
           (print-warning "Onset must be a number.  No new digit sound created."))
          (t
           (new-sound-event (make-instance 'digit-sound-evt :onset (ms-round onset) :string digit)))))))


(defun new-tone-sound (freq duration &optional (onset (mp-time)))
  "Creates and adds a tone sound of <freq>, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp freq))
           (print-warning "Freq must be a number.  No new tone sound created."))
          ((not (numberp duration))
           (print-warning "Duration must be a number.  No new tone sound created."))
          ((not (numberp onset))
           (print-warning "Onset must be a number.  No new tone sound created."))
          (t
           (new-sound-event (make-instance 'tone-sound-evt :onset (ms-round onset) 
                              :duration duration :content freq)))))))

(defun new-other-sound (content duration delay recode 
                        &optional (onset (mp-time)) (location 'external) (kind 'speech))
  "Creates and adds a sound <content>, lasting <duration>, with content delay <delay>, with recode time <recode>, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp duration))
           (print-warning "Duration must be a number.  No new sound created."))
          ((not (numberp delay))
           (print-warning "Delay must be a number.  No new sound created."))
          ((not (numberp recode))
           (print-warning "Recode must be a number.  No new sound created."))
          ((not (numberp onset))
           (print-warning "Onset must be a number.  No new sound created."))
          (t
           (new-sound-event (make-instance 'sound-event :onset (ms-round onset) 
                              :duration duration :content content 
                              :delay delay :recode recode :location location
                              :kind kind)))))))

(defun new-word-sound (word &optional (onset (mp-time)) (location 'external))
  "Creates and adds a word with optional onset time."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (stringp word))
           (print-warning "Word must be a string.  No new word sound created."))
          ((not (numberp onset))
           (print-warning "Onset must be a number.  No new word sound created."))
          (t
           (new-sound-event (make-instance 'word-sound-evt :onset (ms-round onset) 
                              :string word :location location)))))))


;;; FIND-SOUND      [Method]
;;; Date        : 97.08.18, delta 99.08.30
;;; Description : Parallels the Vision Module's FIND-LOCATION, this one finds
;;;             : audio events (not sounds) and returns a PS-specific DME.

(defgeneric find-sound (aud-mod &key attended kind onset pitch)
  (:documentation  "Given a set of specifications, return a sound event which matches."))

(defmethod find-sound ((aud-mod audio-module) &key (attended :IGNORE) 
                       (kind :IGNORE) onset pitch (location :ignore)
                       (stuffed nil))
  (let ((event-ls nil)
        (found-evt nil)
        (spec (make-instance 'sound-event-spec
                 :attended-p attended  :kind kind :location location
                 :onset (if (or (null onset) (symbolp onset)) 
                          :IGNORE onset)
                 :pitch (if (or (null pitch) (symbolp pitch)) 
                          :IGNORE pitch))))
    
    (setf (loc-failure aud-mod) nil)
    
    ;; find features matching the spec
    (setf event-ls (objs-match-spec (detectable-audicon aud-mod) spec))
    ;; some filtering
    (case onset
      (lowest (setf event-ls (objs-min-slotval event-ls 'onset)))
      (highest (setf event-ls (objs-max-slotval event-ls 'onset))))
    (case pitch
      (lowest (setf event-ls (objs-min-slotval event-ls 'pitch)))
      (highest (setf event-ls (objs-min-slotval event-ls 'pitch))))
    (if event-ls
        (progn
          (setf found-evt (random-item event-ls))
          (when found-evt
            (unless (evt-dmo found-evt)
              (event->dmo found-evt))
            
            ;;DAN
            ;; instead of returning it set it into the aural-location buffer
            ;; (dmo-to-psdme (evt-dmo found-evt))
            
            (schedule-set-buffer-chunk 'aural-location 
                                       (dmo-to-psdme (evt-dmo found-evt))
                                       0 :module :audio 
                                       :requested (not stuffed)
                                       
                                       ; Need this so that stuffing 
                                       ; can get things in before procedural
                                       ; can run conflict-resolution
                                       :priority 10)
            
            
            ))
      (schedule-event-relative 0 'find-sound-failure :module :audio :destination :audio 
                               :output 'medium :details "find-sound-failure"))))

(defun find-sound-failure (audio)
  "function to indicate a failure in the trace and set the error flag"
  (setf (loc-failure audio) t)
  nil)


;;; ATTEND-SOUND      [Method]
;;; Date        : 97.08.18
;;; Description : Parallels the Vision Module's MOVE-ATTENTION, this one
;;;             : attends an audio event, ultimately building a chunk based
;;;             : on the content of the sound.

(defgeneric attend-sound (aud-mod &key event)
  (:documentation  "Shift auditory attention to the given sound event."))

(defmethod attend-sound ((aud-mod audio-module) &key event)
  (if (eq (exec-s aud-mod) 'BUSY)
    (pm-warning "Auditory attention shift requested at ~S while one was already in progress."
                (mp-time))
    (progn
  
    ;DAN 
    ; This won't work because the event that comes in is going to
    ; have a different name than the one that went into the audicon
    ; because it will be the name of the copy from the buffer.
    ; 
    ;(let ((s-event (find event (audicon aud-mod)
    ;                       :test #'(lambda (x y) (eq x (ename y))))))
    
    ; For now, using an id slot in the audio-event to keep the connection
    
    (let ((s-event (find (chunk-slot-value-fct event 'id) (current-audicon aud-mod)
                         :test #'(lambda (x y) (eq x (ename y))))))
      
      (setf (attend-failure aud-mod) nil)
      (change-state aud-mod :exec 'busy)
      
      (if s-event ;; add in a test to make sure - could have a failure
          (progn
            (setf (attended-p s-event) t)
            (setf (current-marker aud-mod) s-event)
            (queue-command
             :time (recode s-event) :where :AUDIO :command 'audio-encoding-complete
             :randomize t :params s-event))
        (progn
          ;; assume digit delay for a failure
          (schedule-event-relative (randomize-time (digit-recode-delay aud-mod))
                                   #'attend-sound-failure
                                   :module :audio
                                   :destination :audio
                                   :output 'medium
                                   :details "attend-sound-failure")))))))


(defun attend-sound-failure (audio)
  "Flag that an error occured"
  (setf (attend-failure audio) t)
  (change-state audio :exec 'free))

#|
;;; LISTEN-FOR      [Method]
;;; Date        : 97.08.18, delta 99.08.30
;;; Description : Combination of FIND and ATTEND.  Does a FIND, and it if
;;;             : finds anything, immediately attends it.

(defgeneric listen-for (aud-mod &key onset kind attended pitch)
  (:documentation  "Checks the audicon for appropriate sounds.  If one is found, attend to it."))

(defmethod listen-for ((aud-mod audio-module) &key onset (kind :ignore) 
                         (attended :ignore) pitch)
  (multiple-value-bind (psdme found-evt)
                       (find-sound aud-mod :attended attended :kind kind
                                   :onset onset :pitch pitch)
    (declare (ignore psdme))
    (when found-evt
      (attend-event aud-mod found-evt))))
|#

;;;; ---------------------------------------------------------------------- ;;;;
;;;; support for toplevel commands
;;;; ---------------------------------------------------------------------- ;;;;


;;; ATTEND-EVENT      [Method]
;;; Date        : 97.04.11
;;; Description : When a sound is found by LISTEN-FOR, this may get called on
;;;             : the event.  This method handles state-setting and queueing
;;;             : of the appropriate actions.  Two situations are possible
;;;             : with the sound event:  the content is not yet available 
;;;             : [that is, the content-delay for the event has not passed]
;;;             : or it is.  If not, set preparation to busy until content
;;;             : becomes available.  Then, after recode time, actually add
;;;             : the item to declarative memory.

(defgeneric attend-event (aud-mod sevt)
  (:documentation  "When LISTEN-FOR picks up an event, this handles it."))

(defmethod attend-event ((aud-mod audio-module) (sevt sound-event))
  (let ((curr-time (mp-time))
        (detect-time (+ (onset sevt) (delay sevt))))
    (setf (current-marker aud-mod) sevt)
    (cond ((< curr-time detect-time)  		; sound not yet 'bufferized'
           (change-state aud-mod :prep 'busy)
           (queue-command
            :time (- detect-time curr-time) :where :AUDIO 
            :command 'change-state :params '(:exec busy :prep free))
           (queue-command
            :time (- (+ detect-time (rand-time (recode sevt))) curr-time)
            :where :AUDIO :command 'audio-encoding-complete
            :params sevt))
          (t
           (change-state aud-mod :exec 'busy)
           (queue-command
            :time (recode sevt) :where :AUDIO :command 'audio-encoding-complete
            :randomize t :params sevt)))))



(defgeneric audio-encoding-complete (aud-mod sevt)
  ;DAN
  ;(:documentation  "Actually add a sound to declarative memory."))
  (:documentation  "Put the sound into the aural buffer."))


(defmethod audio-encoding-complete ((aud-mod audio-module) 
                                         (sevt sound-event))
  (change-state aud-mod :exec 'free)
  (setf (attended-p sevt) t)
  (unless (snd-dmo sevt)
    
    ;; DAN
    ;; Similar to the issue in find-sound, the name of the
    ;; audio event doesn't match the actual chunk name that
    ;; was in the aural-location buffer so setting event
    ;; to the ename of svet is going to cause problems
    ;; since that slot value isn't going to match a chunk in DM.
    ;; For now, the solution is that the id slot of the event
    ;; is what's constant.
    
    (let ((the-dmo
           (make-dme (sname sevt) 'sound 
                     `(kind ,(kind sevt) content ,(content sevt)
                            event ,(chunk-slot-value-fct (ename sevt) 'id))
                     :where :external)))
      (setf (snd-dmo sevt) the-dmo)))
  
  ;; DAN 
  ;; set the aural buffer
  
  (schedule-set-buffer-chunk 'aural (sname sevt) 0 :module :audio)
  
  
  (set-attended aud-mod (snd-dmo sevt)))

(defmethod clear :after ((aud-mod audio-module))
  (setf (loc-failure aud-mod) nil)
  (setf (attend-failure aud-mod) nil))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Audio utilities
;;;; ---------------------------------------------------------------------- ;;;;


(defgeneric purge-old-sounds (aud-mod)
  (:documentation  "Removes sounds that have decayed from the audicon"))

(defmethod purge-old-sounds ((aud-mod audio-module))
  (setf (audicon aud-mod)
        (remove-if #'(lambda (e) (< (+ (offset e) (decay-time aud-mod))
                                     (mp-time)))
                   (audicon aud-mod))))


(defun earliest-onset (evt-lis)
  "Returns the sound event in the list with earliest onset."
  (let ((best (onset (first evt-lis)))
        (outlis (list (first evt-lis))))
    (dolist (evt (rest evt-lis) outlis)
      (cond ((= (onset evt) best) (push evt outlis))
            ((< (onset evt) best) (setf best (onset evt))
             (setf outlis (list evt)))))))


(defun latest-onset (evt-lis)
  "Returns the sound event in the list with latest onset."
  (let ((best (onset (first evt-lis)))
        (outlis (list (first evt-lis))))
    (dolist (evt (rest evt-lis) outlis)
      (cond ((= (onset evt) best) (push evt outlis))
            ((> (onset evt) best) (setf best (onset evt))
             (setf outlis (list evt)))))))


(defgeneric current-audicon (aud-mod)
  (:documentation  "Returns the audicon, assuming all events that currently exist are in there."))

(defmethod current-audicon ((aud-mod audio-module))
  (purge-old-sounds aud-mod)
  (remove-if #'(lambda (x) (> (onset x) (mp-time))) (audicon aud-mod)))


(defgeneric detectable-audicon (aud-mod)
  (:documentation  "Returns the audicon, but only those events that are currently detectable."))

(defmethod detectable-audicon ((aud-mod audio-module))
  (purge-old-sounds aud-mod)
  (remove-if #'(lambda (x) (not (detectable-p x))) (audicon aud-mod)))



(defgeneric event->dmo (evt)
  (:documentation  "Translate a sound event to the corresponding DMO."))

(defmethod event->dmo ((evt sound-event))
  (let ((dmo
         (make-dme (ename evt) 'audio-event 
                   `(onset ,(onset evt)
                              location ,(location evt) kind ,(kind evt)
                              ;;DAN 
                              ; adding this at least for now to deal with the
                              ; fact that chunk names change in the buffers
                              id ,(ename evt)
                              )
                   :where :external)))
    (when (finished-p evt)
      (set-attributes dmo `(offset ,(offset evt))))
    (setf (evt-dmo evt) dmo)))


(defgeneric next-detectable-sound (aud-mod current-time)
  (:documentation  "Return the time when the next sound in the audicon is detectable."))

(defmethod next-detectable-sound ((aud-mod audio-module) current-time)
  (let ((onsets (mapcar #'detectable-time (audicon aud-mod))))
    (setf onsets (sort onsets #'<))
    (dolist (event-time onsets nil)
      (when (> event-time current-time)
        (return-from next-detectable-sound event-time)))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; ACT6 integration stuff
;;;; ---------------------------------------------------------------------- ;;;;

(defun reset-audio-module (instance)
  (reset-pm-module instance)

  (chunk-type audio-event onset offset pitch kind location id)
  (chunk-type sound kind content event)
  (chunk-type audio-command)
  
  ;;DAN
  ; I think this needs to happen here
  
  (setf (audicon instance) nil)
  ;(setf (stuffed instance) nil)
  
  ;; handle the failure flags
  
  (setf (loc-failure instance) nil)
  (setf (attend-failure instance) nil)
  
  
  (define-chunks
    (digit isa chunk)
    (speech isa chunk)
    (tone isa chunk)
    (word isa chunk)))



(defun query-audio-module (aud-mod buffer slot value)
  (case buffer
    (aural
     (if (member slot '(preparation execution processor modality))
       (generic-state-query aud-mod buffer slot value)
       (case slot
         (state
          (case value
            (busy
             (eq (mode-s aud-mod) 'busy))
            (free
             (eq (mode-s aud-mod) 'free))
            (error
             (attend-failure aud-mod))
            (t (print-warning 
                "Invalid query made of the ~S buffer with slot ~S and value ~S" 
                buffer slot value))))
         (t (print-warning 
                "Invalid query made of the ~S buffer with slot ~S and value ~S" 
                buffer slot value)))))
    (aural-location
     (case slot
       (state
        (case value
          (busy nil) ;; aural-location requests are always free
          (free t)
          (error (loc-failure aud-mod))
          (t (pm-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (attended
        (awhen (buffer-read 'aural-location)
               (let ((s-event (find (chunk-slot-value-fct (buffer-read 'aural-location) 'id) 
                                    (audicon aud-mod)
                                    :test #'(lambda (x y) (eq x (ename y))))))
                 (when s-event
                   (eq value (attended-p s-event))))))))))
                 
         


(defmethod pm-module-request ((aud-mod audio-module) buffer-name chunk-spec)
  ;(declare (ignore aud-mod))
  (case buffer-name
    (aural
     (case (chunk-spec-chunk-type chunk-spec)
       (clear
        (schedule-event-relative 0 'clear :module :audio :destination :audio
                                 :output 'medium))
       (sound
        (let ((event (when (slot-in-chunk-spec-p chunk-spec 'event) 
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'event) 
                         :audio 'sound 'event))))
          (when event
            (schedule-event-relative 0 'attend-sound
                                     :params (list :event event)
                                     :module :audio  :destination :audio
                                     :details (mkstr 'attend-sound " " event)
                                     :output 'medium))))
       ;; should we support LISTEN-FOR anymore?  Hmm...
       (t
        (print-warning "Invalid command ~a sent to the aural buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))
    (aural-location
     (case (chunk-spec-chunk-type chunk-spec)
       (;; DAN 
        ;;aural-location
        audio-event
        (let ((attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec :attended) 
                             :audio 'aural-location :attended)
                            :IGNORE))
              (kind (if (slot-in-chunk-spec-p chunk-spec 'kind) 
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'kind) 
                        :audio 'aural-location 'kind)
                       :IGNORE))
              (location (if (slot-in-chunk-spec-p chunk-spec 'location) 
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec 'location) 
                             :audio 'aural-location 'location)
                            :IGNORE))
              (onset (when (slot-in-chunk-spec-p chunk-spec 'onset) 
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'onset) 
                         :audio 'aural-location 'onset))))
          ;(setf (stuffed aud-mod) nil)
          (schedule-event-relative 0 'find-sound :module :audio 
                                   :output 'medium
                                   :destination :audio 
                                   :details ;(format nil "~s" 'find-sound)
                                   (mkstr 'find-sound)
                                   :params (list :kind kind :attended attended 
                                                 :location location
                                                 :onset onset
                                                 ;; Dan 
                                                 ;; this isn't a valid
                                                 ;; keyword for find-sound
                                                 ;:offset offset
                                                 ))))
       (t
        (print-warning "Invalid command ~a sent to the aural-location buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))))
 


(defun params-audio-module (aud-mod param)
  (if (consp param)
    (case (first param)
      (:digit-detect-delay
       (setf (digit-detect-delay aud-mod) (rest param)))
      (:digit-duration
       (setf (digit-duration aud-mod) (rest param)))
      (:digit-recode-delay
       (setf (digit-recode-delay aud-mod) (rest param)))
      (:sound-decay-time 
       (setf (decay-time aud-mod) (rest param)))
      (:tone-detect-delay
       (setf (tone-detect-delay aud-mod) (rest param)))
      (:tone-recode-delay
       (setf (tone-recode-delay aud-mod) (rest param)))
      (:hear-newest-only
       (when (rest param)
         (print-warning ":hear-newest-only parameter is depricated - use set-audloc-default instead"))))
    (case param
      (:digit-detect-delay
       (digit-detect-delay aud-mod))
      (:digit-duration
       (digit-duration aud-mod))
      (:digit-recode-delay
       (digit-recode-delay aud-mod))
      (:sound-decay-time 
       (decay-time aud-mod))
      (:tone-detect-delay
       (tone-detect-delay aud-mod))
      (:tone-recode-delay
       (tone-recode-delay aud-mod))
      (:hear-newest-only
       nil))))
      

(define-module-fct :audio 
    (list (list 'aural-location nil '(:attended) 
                '(attended)
                           #'(lambda ()
                               (command-output "  attended nil          : ~S"
                                               (query-buffer 'aural-location
                                                             '((attended . nil))))
                               (command-output "  attended t            : ~S"
                                               (query-buffer 'aural-location
                                                             '((attended . t)))))) 
          (list 'aural nil nil '(modality preparation execution processor) 
                  #'(lambda () (print-module-status (get-module :audio)))))
  (list 
   (define-parameter :digit-detect-delay
     :valid-test #'nonneg
     :default-value 0.3
     :warning "a non-negative number"
     :documentation "Lag between onset and detectability for digits")
   (define-parameter :digit-duration
     :valid-test #'nonneg
     :default-value 0.6
     :warning "a non-negative number"
     :documentation "Default duration for digit sounds.")
   (define-parameter :digit-recode-delay
     :valid-test #'nonneg
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Recoding delay for digit sound content.")
   (define-parameter :sound-decay-time
     :valid-test #'nonneg
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "The amount of time after a sound has finished it takes for the sound to be deleted from the audicon")
   (define-parameter :tone-detect-delay
     :valid-test #'nonneg
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Lag between sound onset and detectability for tones")
   (define-parameter :tone-recode-delay
     :valid-test #'nonneg
     :default-value 0.285
     :warning "a non-negative number"
     :documentation "Recoding delay for tone sound content.")
   (define-parameter :hear-newest-only
     :valid-test #'tornil
     :default-value nil
     :warning "T or nil"
     :documentation "Whether to stuff only the newest unattended audio-event from the audicon into the aural-location buffer."))
  :version "2.3"
  :documentation "A module which gives the model an auditory attentional system"
  :creation #'(lambda (x) (declare (ignore x))
               (make-instance 'audio-module))
  :reset #'reset-audio-module
  :query #'query-audio-module
  :request #'pm-module-request
  :params #'params-audio-module
  ;:update #'update-module
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-audicon ()
  (let ((module (get-module :audio)))
    (if module
        (progn
          (format t "~%Sound event    Att  Detectable  Kind           Content           location     onset     offset  Sound ID")
          (format t "~%-----------    ---  ----------  -------------  ----------------  --------     -----     ------  --------")
          
          (dolist (x (current-audicon module))
            (print-audio-feature x))) 
      (print-warning "No audio module found"))))



(defgeneric print-audio-feature (feat)
  (:documentation  "Print out an ASCII representation of the audicon."))


(defmethod print-audio-feature ((feat sound-event))
  (format t "~%~15a~5A~12A~15A~18s~10a~8,3f   ~8,3f  ~a"
    (ename feat)
    (attended-p feat)
    (detectable-p feat)
    (kind feat)
    (content feat)
    (location feat)
    (onset feat)
    (offset feat)
    ;(snd-string feat)
    (sname feat)))


(defmacro set-audloc-default (&rest params)
  "Macro to set the default specification for aural location buffer stuffing."
  `(set-audloc-default-fct ',params))

(defun set-audloc-default-fct (params)
  "Function to set the default specification for aural location buffer stuffing."
  (verify-current-mp
   "No current meta-process.  Cannot set audloc defaults."
   (verify-current-model 
    "No current model.  Cannot set audloc defaults."
    (if (get-module :audio)
        (if params
            (let ((specs (verify-audio-specs params)))
              (if specs
                  (progn
                    (setf (default-spec (get-module :audio)) specs)
                    t)
                (print-warning "No valid specs provided so defaults are unchanged.")))
          (progn
            (setf (default-spec (get-module :audio)) nil)
            t))
      (print-warning "No audio module found.  Cannot set audloc defaults.")))))

(defun verify-audio-specs (params)
  (if (oddp (length params))
      (print-warning "Odd number of elements provided cannot set audloc defaults")
    (do* ((result nil)
          (used nil)
          (items params (cddr items))
          (keyword (first items) (first items))
          (value (second items) (second items)))
         ((null items) result)
      (if (member keyword '(:kind :attended :onset :pitch :location))
        (if (member keyword used)
          (print-warning "Each property can only be used once.  Ignoring duplicate setting ~s ~s."
                         keyword value)
        (progn
          (push keyword used)
          (push value result)
          (push keyword result)))
        
        (print-warning "Property ~s is not valid as an audio spec.  It is being ignored." keyword)))))

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
