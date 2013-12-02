;; This file contains two version of CT-E:
;;   1) the older, which has been in generic CogTool for most of 2010
;;   2) and Leonghwee's latest, from December 2010
;; As currently configured, by default (2) will be used. To use (1)
;; instead comment out the following line which defines the NEW-CT-E
;; feature. 

#.(progn (push :new-ct-e *features*) nil)

;;; Model refinements
(defparameter *modified-mean-prev-page* t)
(defparameter *modified-mean-current-page* t)
(defparameter *update-confidence* t)
(defvar *use-back-button* t) ;;; t means model will look for a Back button and click on it to go-back, nil means model will either stop or "magically" go back to the previous frame it came from.
(defvar *allow-magic-go-backs* nil)

(defparameter *mixed-visual-search* nil) ;;; t means model may choose to focus next on the nearest group instead of doing a strict header-first process
(defparameter *non-header-process-conf-scale* 3) ;;; numeric value only applies if  *mixed-visual-search* is t

(defvar *use-back-button-history-in-half-flatten-layout* t) ;;; in the half-flatten layout, back buttons have no transitions, so have to use exploration history when set to t
(defparameter *recognize-equivalent-links-in-half-flatten-layout* nil)
(defparameter *handle-groups-in-half-flatten-layout* nil)
;; TODO should these be in generic CogTool? They're specific to Leonghwee's research, right?
(setf *2nd-level-pages* '("Art, Language & Literature" "Life Science" "History" "Geography" "Religion & Philosophy" "Physical Science & Technology" "Social Science" "Sports, Hobbies, & Pets" "Performing Arts"))

(defparameter *consider-familarity* nil)
(defparameter *modified-previously-chosen-widgets* nil)
(defparameter *infoscent_variabilty_between_runs* nil)
(defparameter *second-best-prev-page* nil)

(defparameter *print-header-kludge* t)

(defparameter *corrected-k-update* t)

;;; Model parameters
(defparameter *similarity-scale-factor* 50)
(defparameter *infoscent-noise* (if *cogtool-random-seed* 0 1))
(defparameter *go-back-cost* 1)

;;; Task parameters
(defvar *CT-E_timeout* 1300)

;;; Model log file
(defparameter *file* nil)
;; TODO for historical reasons the log file's path is built up by concatenation of strings; some day it should be fixed to use proper CL pathnames
(defvar *log-file-directory* "~/") 

(defparameter *model-run* 0)

(defparameter *CT-E-Debug* nil)

(defun reset-trial-parameters () ; parameters to be reset at the start of each new trial or run of the model
	(setf *widget-infoscent-stack* nil) ; list of lists of infoscent values seen on previous pages or frames
	(setf *history* nil) ; enable go-back to previous page without back button on page
	(setf *current-group* nil) ; for ease of printing to model log file
	(setf *step-index* 1)
	(set-visloc-default isa cogtool-visual-location
		member-of-group	nil
		is-back-button nil
		:attended new
		:nearest current)
	(if *infoscent_variabilty_between_runs* (setf *assessed-infoscent* (make-hash-table :test #'equalp)))
	(setf *cummulative-step-index* (make-hash-table :test #'equalp))
	(setf *non-cummulative-step-index* (make-hash-table :test #'equalp))
)

(defun reset-frame-parameters () ; parameters to be reset at the start of each visit to a page or frame
	(spp read-another-widget :u 0)
	(spp choose-best-widget :u 0)
	(spp go-back :u 0)
	(setf *widget-infoscent-list* nil) ; list of infoscent values seen on this frame or group
	(setf *number-widgets-read* 0)
	(setf *best-widget-scent* 0)
	(setf *current-widget-scent* 0)
	(remove-visual-finsts)
)

; Do NOT set :seed to nil here. That is an illegal value for ACT-R's random seed. Instead
; ensure that *cogtool-random-seed* is initialized to nil in the code passed by Java; that
; will prevent the code in cogtool-model.lisp from overriding the default random-seed that
; ACT-R sets.
(loop for (key val) on '(:visual-finst-span 100 ;;; "Perfect" visual search memory for each frame
			 :visual-num-finsts 100 ;;; "Perfect" visual search memory for each frame
			 :act nil
			 :bll 0.5
			 :rt -2
			 :ans 1
			 :ol nil
			 :er t
			 )
      by #'cddr
      do (setf (getf *overridden-global-parameters* key) val))
(when *cogtool-random-seed*
  (loop for (key val) on '(:ans nil ; overwrites value set above
			   :er nil  ; overwrites value set above
			   :pas nil
			   :egs 0
			   :randomize-time nil)
	by #'cddr
	do (setf (getf *overridden-global-parameters* key) val)))

(define-cogtool-model (:start-with-mouse t :timeout *CT-E_timeout* :initial-frame-name *CogTool-Explorer-start-frame*)

(chunk-type search-task
			goal-text
			best-cue ;;; holds widget text for facilitating trace output; to move outside of model
			best-loc
			best-widget ;;; holds widget name for facilitating trace output; to move outside of model
			group
			state
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (p start) adds the goal chunk ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p start
	=goal>
		isa			klm
		state 		1
==>
	!bind! =goal-text *task-description*

	!eval! (reset-trial-parameters)
	!eval! (reset-frame-parameters)
	!eval! (transition *CogTool-Explorer-start-frame* *cogtool-design*)
	!eval! (if *CT-E-Debug* (print-visicon))

	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(when *print-header-kludge*
		(format *file* "Trial;Step;Step in this visit to group or frame;Cumulative step in group or frame;In Frame;In Group;Group Chosen;Widget Chosen;State;Timestamp;#Widgets looked at~%")
		(setq *print-header-kludge* nil)))
	
	+goal>
		isa			search-task  
		goal-text 	=goal-text
		best-cue 	"dummy-text"
	;	best-cue	nil
		best-loc 	dummy-loc ;;;Will be updated to the first link assessed when model runs.
		best-widget	dummy-widget
		group		nil
		state 		find
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Search Productions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(p find-any-unattended)

(p find-unattended-in-frame-group
	=goal>
		isa 			search-task
		group			nil
		state 			find

	?visual-location>
		buffer			empty
	
	?visual>
		state			free
==>
	+visual-location>
		isa 			cogtool-visual-location
		member-of-group	nil
		remote-label-of nil ;;; there is a need to remove this restriction in the PFIS task modeling
		is-back-button          nil
		:attended 		nil
		:nearest 		current
    
	=goal>
		state			finding
)

(p find-unattended-in-group
	=goal>
		isa 			search-task
		group			=group
		state 			find

	?visual-location>
		buffer			empty
	
	?visual>
		state			free
==>
	+visual-location>
		isa 			cogtool-visual-location
		member-of-group	=group
		remote-label-of nil ;;; there is a need to remove this restriction in the PFIS task modeling
		is-back-button          nil
		:attended 		nil
		:nearest 		current
    
	=goal>
		state			finding
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handle Visual Stuffing or Search Outcomes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(p stuffed-with-any-unattended)

(p stuffed-with-unattended-in-frame-group
	=goal>
		isa 			search-task
		group			nil
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
		remote-label-of nil ;;; there is a need to remove this restriction in the PFIS task modeling
		is-back-button          nil
		member-of-group	nil
	
	?visual>
		state			free
==>
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don

	+visual>
		isa				move-attention
		screen-pos		=visual-location
	
	=goal>
		state			read-link
)

(p stuffed-with-unattended-in-group
	=goal>
		isa 			search-task
		group			=group
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
		remote-label-of nil ;;; there is a need to remove this restriction in the PFIS task modeling
		is-back-button          nil
		member-of-group	=group
	
	?visual>
		state			free
==>
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don

	+visual>
		isa				move-attention
		screen-pos		=visual-location
	
	=goal>
		state			read-link
)

(p skip-stuffed-remote-label
	=goal>
		isa 			search-task
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
	  - remote-label-of nil	
	
	?visual>
		state			free
==>
	-visual-location>
)

(p skip-stuffed-back-button
	=goal>
		isa 			search-task
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
		is-back-button          t
	
	?visual>
		state			free
==>
	-visual-location>
)

(p skip-stuffed-unattended-in-wrong-group
	=goal>
		isa 			search-task
		group			=group
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
	  - member-of-group	=group	
	
	?visual>
		state			free
==>
	-visual-location>
)

(p skip-stuffed-unattended-in-group
	=goal>
		isa 			search-task
		group			nil
		state			find
		
	=visual-location>
		isa				cogtool-visual-location
	  - member-of-group	nil	
	
	?visual>
		state			free
==>
	-visual-location>
)

(p no-more-unattended
	=goal>
		isa			search-task
	  - best-cue	"dummy-text"
;	  - best-cue	nil
		best-widget	=best-widget
		state 		finding
   
	?visual-location>
		state 		error
==>
	!bind! =num-links-read (eval *number-widgets-read*) ; for debugging	
	#-suppress-trace !output! #-suppress-trace (>>> Choose widget =best-widget after reading =num-links-read links] <<<) ; for debugging
	
	=goal>
		state		choose-best-widget
)

(p no-unselected
	=goal>
		isa			search-task
		best-cue	"dummy-text"
;		best-cue	nil
		state		finding
   
	?visual-location>
		state		error
==>
	!bind! =state (if (= 0 (length *widget-infoscent-stack*)) 'stop 'go-back-to-previous)

	!eval! (if (and *update-confidence* (> (length *widget-infoscent-stack*) 1)) (push 0.01 (first (second *widget-infoscent-stack*))))

	#-new-ct-e !eval! #-new-ct-e (if *current-group*
		(store-non-cummulative-step-index *current-group* 1)
		(store-non-cummulative-step-index (name (curframe *cogtool-design*)) 1))
	
	=goal>
		state		=state
)

(p look-at-unattended
	=goal>
		isa			search-task
		state		finding
    
	=visual-location>
		isa			cogtool-visual-location
		is-back-button          nil
   
	?visual>
		state		free
==>
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don

	+visual>
		isa			move-attention
		screen-pos	=visual-location
    
	=goal>
		state		read-link
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read, check if is a chosen link and assess link ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p target-link-not-on-this-page
	=goal>
		isa			search-task
		state		read-link
   
	=visual-location>
		isa			cogtool-visual-location
		display-label "Where the target link should be"
		
	=visual>
		isa			visual-object
		screen-pos	=visual-location
==>
	!bind! =state (if (= 0 (length *widget-infoscent-stack*)) 'stop 'go-back-to-previous)

	!eval! (if (and *update-confidence* (> (length *widget-infoscent-stack*) 1)) (push 0.01 (first (second *widget-infoscent-stack*))))
	
	=goal>
		state		=state
)

(p read-widget
	=goal>
		isa			search-task
		state		read-link
		
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
	  - display-label "Where the target link should be"
		#+new-ct-e textual-cue	#+new-ct-e =textual-cue
		widget-name	=widget-name
   
	=visual>
		isa			visual-object
		screen-pos	=visual-location
==>
	;!bind! =frame-name (name (curframe *cogtool-design*))
      
;	!bind! =widget-name (if *recognize-equivalent-links-in-half-flatten-layout* =textual-cue =widget-name)
	
	#+new-ct-e !bind! #+new-ct-e =new-widget-name #+new-ct-e (if (and *handle-groups-in-half-flatten-layout* (search "Subordinate in" =widget-name))
						   (subseq =widget-name 15)
						   (if *recognize-equivalent-links-in-half-flatten-layout* =textual-cue =widget-name))
						   	
	+retrieval>
		isa 		visual-object
		value		=new-widget-name
		status		chosen

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		
	=visual> ; prevent implicit clearing of the visual buffer to maintain the chunk for modification in production click-mouse
   
	=goal>
		state		assess-link
)

(p widget-was-previously-chosen
	!eval! (not *modified-previously-chosen-widgets*)

	=goal>
		isa			search-task
		state		assess-link
   
	?retrieval>
		buffer		full
==>   
	-visual-location> ; so that find-unattended-* will fire, otherwise stuffed-* will fire over and over again

	-visual> ;;; is this necessary?

	=goal>
		state		find    
)

(p modified-widget-was-previously-chosen
	!eval! (eval *modified-previously-chosen-widgets*)

	=goal>
		isa			search-task
		best-widget	=best-widget
		state		assess-link
   
	?retrieval>
		buffer		full
==>
	#-suppress-trace !output! #-suppress-trace (>>> Assessing =best-widget [previously selected] <<<) ; for debugging

	!eval! (if *corrected-k-update* (incf *number-widgets-read*))
	
	!eval! (update-prod-parameters 0.01 *best-widget-scent*)

	=goal>
		state		whats-next
		
	!bind! =num-widgets-read (if *corrected-k-update* (eval *number-widgets-read*) (incf *number-widgets-read*))
	#-suppress-trace !output! #-suppress-trace (>>> Best widget after assessing =num-widgets-read widgets is =best-widget <<<) ; for debugging
)

(p assess-widget
	=goal>
		isa			search-task
		goal-text	=goal-text   
		best-cue	=best-cue
		best-loc	=best-loc
		best-widget	=best-widget
		state		assess-link
   
	?retrieval>
		state		error     

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location	
		textual-cue	=textual-cue
		widget-name	=widget-name
		display-label =display-label
		
	=visual>  
		isa			visual-object
		screen-pos	=visual-location
==>
	!bind! =widget-name-or-display-label (if (equalp =display-label "") =widget-name =display-label)
	#-suppress-trace !output! #-suppress-trace (>>> Assessing =widget-name-or-display-label <<<) ; for debugging

	!eval! (if *corrected-k-update* (incf *number-widgets-read*))
	
	!bind! =new-best-cue (find-best-cue =goal-text =best-cue =textual-cue :update-prod t) ; compare inforscent values and update production utilities
	!bind! =new-best-loc (if (equalp =new-best-cue =textual-cue) =visual-location =best-loc) ; update visual-location of best link
	!bind! =new-best-widget (if (equalp =new-best-cue =textual-cue) =widget-name-or-display-label =best-widget) ; update widget-name of best link

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
	
	=visual>
	  
	=goal>
		best-cue	=new-best-cue
		best-loc	=new-best-loc
		best-widget	=new-best-widget
		state		whats-next

	!bind! =num-links-read (if *corrected-k-update* (eval *number-widgets-read*) (incf *number-widgets-read*))
	#-suppress-trace !output! #-suppress-trace (>>> Best widget after assessing =num-links-read widgets is =new-best-widget <<<) ; for debugging
)

(spp assess-widget :at 0.275)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The 3 competing productions as per SNIF-ACT 2 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p read-another-widget
	=goal>
		ISA			search-task
		state		whats-next     
==>
	-visual-location> ; so that find-unattended-* will fire, otherwise stuffed-* will fire over and over again
	
	-visual> ;;; is this necessary?
   
	=goal>
		state		find
)

(p choose-best-widget
	=goal>  
		ISA			search-task
		best-widget	=best-widget
		state		whats-next

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
		
	=visual>  
		isa			visual-object
==>   
	!bind! =num-links-read (eval *number-widgets-read*) ; for debugging	
	#-suppress-trace !output! #-suppress-trace (>>> Choosing best widget =best-widget after assessing =num-links-read widgets <<<)

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
    
	=visual>
	
	=goal>
		state		choose-best-widget
)

(p go-back
	=goal>
		ISA			search-task
		state		whats-next
==>	
	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (if *current-group* (fetch-non-cummulative-step-index *current-group*) (fetch-non-cummulative-step-index (name (curframe *cogtool-design*))))) ";"
											#-new-ct-e (write-to-string (if *current-group* (fetch-cummulative-step-index *current-group*) (fetch-cummulative-step-index (name (curframe *cogtool-design*))))) ";"
											(if *current-group* "" (name (curframe *cogtool-design*))) ";"
											(if *current-group* *current-group* "") ";"
											";"
											"Voluntary GO-BACK" ";"
											"FIND" ";"
											(write-to-string (mp-time)) ";"
											(write-to-string *number-widgets-read*) "~%")))										
	!eval! (incf *step-index*)
	#-new-ct-e !eval! #-new-ct-e (if *current-group*
		(progn
			(store-non-cummulative-step-index *current-group* 1)
			(store-cummulative-step-index *current-group* (+ (fetch-cummulative-step-index *current-group*) 1)))
		(progn
			(store-non-cummulative-step-index (name (curframe *cogtool-design*)) 1)
			(store-cummulative-step-index (name (curframe *cogtool-design*)) (+ (fetch-cummulative-step-index (name (curframe *cogtool-design*))) 1))))
			
	
	
	=goal>
		state		go-back-to-previous
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Productions for Motor actions after decision taken ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(p look-at-best-widget-if-not-already-looking-at-it
	=goal>  
		ISA			search-task
		best-widget	=best-widget
		best-loc	=best-loc
		state		choose-best-widget 

	?visual>
		state		free   
	  
	=visual>
		isa			visual-object
	  - screen-pos	=best-loc
==>
	#-suppress-trace !output! #-suppress-trace (>>> Look at best widget =best-widget <<<)

	+visual-location> =best-loc
	
	+visual>
		isa			move-attention
		screen-pos	=best-loc
   
	=goal>
		state		choose-best-widget
)

(p look-at-best-widget-after-no-more-widgets-to-look-at
	=goal>  
		ISA			search-task
		best-widget	=best-widget
		best-loc	=best-loc 
		state		choose-best-widget 
	   
	?visual>
		state		free
		buffer		empty ;;; apparently the visual buffer gets cleared when the request to visual-location fails
==>
	#-suppress-trace !output! #-suppress-trace (>>> Look at best widget =best-widget <<<)

	+visual-location> =best-loc	
	
	+visual>
		isa			move-attention
		screen-pos	=best-loc
   
	=goal>
		state		choose-best-widget
)

;(p choose-best-widget-when-it-is-a-remote-label
;	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
;		isa			cogtool-visual-location
;	  - remote-label-of nil	
;		widget-name =widget-name
;==>
;)

(p change-focus-within-group
	=goal>  
		ISA			search-task
		best-loc	=best-loc
		group		=group
		state		choose-best-widget
   
	?visual>
		state		free
		
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
		kind		cogtool-group
		widget-name =widget-name
		#+new-ct-e textual-cue #+new-ct-e =textual-cue
   
	=visual>
		isa			visual-object
;		screen-pos  =visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc
==>
	#-suppress-trace !output! #-suppress-trace (>>> Focus on best group =widget-name <<<)

	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)

	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (if *current-group* (fetch-non-cummulative-step-index *current-group*) (fetch-non-cummulative-step-index (name (curframe *cogtool-design*))))) ";"
											#-new-ct-e (write-to-string (if *current-group* (fetch-cummulative-step-index *current-group*) (fetch-cummulative-step-index (name (curframe *cogtool-design*))))) ";"
											";"
											=group ";"
											(trim =widget-name) ";"
											";"
											"FIND;"
											(write-to-string (mp-time)) ";"
											(write-to-string *number-widgets-read*) "~%")))
	
	!eval! (push (list (name (curframe *cogtool-design*)) =group) *history*)
	!eval! (incf *step-index*)
	#-new-ct-e !eval! #-new-ct-e (store-cummulative-step-index *current-group* (+ (fetch-cummulative-step-index *current-group*) 1))
	#-new-ct-e !eval! #-new-ct-e (store-non-cummulative-step-index *current-group* (+ (fetch-non-cummulative-step-index *current-group*) 1))

	#+new-ct-e !bind! #+new-ct-e =value #+new-ct-e (if *recognize-equivalent-links-in-half-flatten-layout* (subseq =widget-name 15) =widget-name)

	=visual>
		value		=widget-name
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM

	!eval! (reset-frame-parameters)
	!eval! (setf *current-group* =widget-name)
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-location
		group		=widget-name
		state		find  
)
   
(p change-focus-within-frame-group
	=goal>  
		ISA			search-task
		best-loc	=best-loc
		group		nil
		state		choose-best-widget
   
	?visual>
		state		free
		
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
		kind		cogtool-group
		widget-name =widget-name
		#+new-ct-e textual-cue #+new-ct-e =textual-cue
   
	=visual>
		isa			visual-object
;		screen-pos  =visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc
==>
	#-suppress-trace !output! #-suppress-trace (>>> Focus on best group =widget-name <<<)

	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)

	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (fetch-non-cummulative-step-index (name (curframe *cogtool-design*)))) ";"
											#-new-ct-e (write-to-string (fetch-cummulative-step-index (name (curframe *cogtool-design*)))) ";"
											(name (curframe *cogtool-design*)) ";"
											";"
											(trim =widget-name) ";"
											";"
											"FIND;"
											(write-to-string (mp-time)) ";"
											(write-to-string *number-widgets-read*) "~%")))
	
	!eval! (push (list (name (curframe *cogtool-design*)) nil) *history*)
	!eval! (incf *step-index*)
	#-new-ct-e !eval! #-new-ct-e (store-cummulative-step-index (name (curframe *cogtool-design*)) (+ (fetch-cummulative-step-index (name (curframe *cogtool-design*))) 1))
	#-new-ct-e !eval! #-new-ct-e (store-non-cummulative-step-index (name (curframe *cogtool-design*)) (+ (fetch-non-cummulative-step-index (name (curframe *cogtool-design*))) 1))

	#+new-ct-e !bind! #+new-ct-e =value #+new-ct-e (if *recognize-equivalent-links-in-half-flatten-layout* (subseq =widget-name 15) =widget-name)

	=visual>
		value		#-new-ct-e =widget-name #+new-ct-e =value
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM

	!eval! (reset-frame-parameters)
	!eval! (setf *current-group* =widget-name)
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-location
		group		=widget-name
		state		find  
)
   
(p move-mouse
	!eval! (not *use-finger-default-value*)

	=goal>  
		ISA			search-task
		best-loc	=best-loc
		state		choose-best-widget
   
	?visual>
		state		free

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
	  - kind		cogtool-group
		
	=visual>
		isa			visual-object
;		screen-pos	=visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc
	  
	?manual>   
		state		free 
==>   
	+manual>
		isa			move-cursor
		loc			=visual-location

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		
	=visual>
		
	=goal>
		state		click
)

(p click-mouse-within-group
	=goal>
		ISA			search-task
		best-loc	=best-loc
		group		=group
		state		click

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location	
		widget-name	=widget-name
		#+new-ct-e textual-cue #+new-ct-e =textual-cue
		
	=visual>
		isa			visual-object
;		screen-pos	=visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc
   
	?manual>   
		state		free
==>
	!bind! =frame-name (name (curframe *cogtool-design*))
	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)
	!eval! (push (list =frame-name =group) *history*)
	
	#+new-ct-e !bind! #+new-ct-e =new-widget-name #+new-ct-e (if *recognize-equivalent-links-in-half-flatten-layout* =textual-cue =widget-name)

	=visual>
		value		=new-widget-name
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM
   
	+manual>
		isa			click-mouse
   
	=goal>
		state		wait
)

(p click-mouse-within-frame_group
	=goal>
		ISA			search-task
		best-loc	=best-loc
		group		nil
		state		click

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location	
		widget-name	=widget-name
		#+new-ct-e textual-cue #+new-ct-e =textual-cue
		
	=visual>
		isa			visual-object
;		screen-pos	=visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc
   
	?manual>   
		state		free
==>
	!bind! =frame-name (name (curframe *cogtool-design*))
	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)
	!eval! (push (list =frame-name nil) *history*)
	
	#+new-ct-e !bind! #+new-ct-e =new-widget-name #+new-ct-e (if *recognize-equivalent-links-in-half-flatten-layout* =textual-cue =widget-name)
	
	=visual>
		value		=new-widget-name
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM
   
	+manual>
		isa			click-mouse
   
	=goal>
		state		wait
)

(p tap-finger-within-group
	!eval! (eval *use-finger-default-value*)

	=goal>  
		ISA			search-task
		best-loc	=best-loc  
		group		=group
		state		choose-best-widget
   
	?visual>
		state		free

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
	  - kind		cogtool-group
		widget-name	=widget-name
		#+new-ct-e textual-cue #+new-ct-e =textual-cue
		
	=visual>
		isa			visual-object
;		screen-pos	=visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc 
   
	?manual>   
		state		free
==>
	!bind! =frame-name (name (curframe *cogtool-design*))
	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)
	!eval! (push (list =frame-name =group) *history*)

	#+new-ct-e !bind! #+new-ct-e =new-widget-name #+new-ct-e (if *recognize-equivalent-links-in-half-flatten-layout* =textual-cue =widget-name)

	=visual>
		value		#-new-ct-e =widget-name #+ new-ct-e =new-widget-name
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM
   
	+manual>
		isa			move-cursor
		loc			=visual-location
   
	=goal>
		state		wait
)

(p tap-finger-within-frame-group
	!eval! (eval *use-finger-default-value*)

	=goal>  
		ISA			search-task
		best-loc	=best-loc  
		group		nil
		state		choose-best-widget
   
	?visual>
		state		free

	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
	  - kind		cogtool-group
		widget-name	=widget-name
		textual-cue =textual-cue

	=visual>
		isa			visual-object
;		screen-pos	=visual-location ; LHS fails to match if this line is un-commented
		screen-pos	=best-loc 
   
	?manual>   
		state		free
==>
	!bind! =frame-name (name (curframe *cogtool-design*))
	!eval! (push (list *best-widget-scent*) *widget-infoscent-list*) ;;; Apr 1 - To support *modified-mean-prev-page* and *modified-mean-current-page*
	!eval! (push *widget-infoscent-list* *widget-infoscent-stack*)
	!eval! (push (list =frame-name nil) *history*)

	!bind! =value (if *recognize-equivalent-links-in-half-flatten-layout* (subseq =widget-name 15) =widget-name)

	=visual>
		value		=widget-name
		status		chosen
   
	-visual> ; ensures modified chunk in visual buffer enters DM
   
	+manual>
		isa			move-cursor
		loc			=visual-location
   
	=goal>
		state		wait
)


(p check-for-target-frame-within-frame-group
	=goal>
		ISA			search-task
		best-widget =best-widget
		group		nil
		state		wait
   
	?manual>   
		state		free
==>
	!eval! (if *use-finger-default-value* (infer-transition *cogtool-design*)) ; CogTool device currently only call infer-transition on a mouse click
   
	!bind! =state (if (equal (name (curframe *cogtool-design*)) (first *CogTool-Explorer-target-frames*)) 'found 'find) ;;; needs modification to support multiple target frames

	; update log file
	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (if (second (first *history*)) (fetch-non-cummulative-step-index (second (first *history*))) (fetch-non-cummulative-step-index (first (first *history*))))) ";"
											#-new-ct-e (write-to-string (if (second (first *history*)) (fetch-cummulative-step-index (second (first *history*))) (fetch-cummulative-step-index (first (first *history*))))) ";"
											(if (second (first *history*)) "" (first (first *history*))) ";"
											(if (second (first *history*)) (second (first *history*)) "") ";"
											";"
											(trim =best-widget) ";"
											(string =state) ";"
											(write-to-string (mp-time)) ";"
											(write-to-string *number-widgets-read*) "~%")))
	
	!eval! (incf *step-index*)
	
	#-new-ct-e !eval! #-new-ct-e (if (second (first *history*))
		(store-cummulative-step-index (second (first *history*)) (+ (fetch-cummulative-step-index (second (first *history*))) 1))
		(store-cummulative-step-index (first (first *history*)) (+ (fetch-cummulative-step-index (first (first *history*))) 1)))
	#-new-ct-e !eval! #-new-ct-e (if (second (first *history*))
		(store-non-cummulative-step-index (second (first *history*)) (+ (fetch-non-cummulative-step-index (second (first *history*))) 1))
		(store-non-cummulative-step-index (first (first *history*)) (+ (fetch-non-cummulative-step-index (first (first *history*))) 1)))
	
	; "knowledge" to handle a self-transition link as essentially null
	!eval! (if (equal (name (curframe *cogtool-design*)) (first (first *history*))) (progn (pop *history*) (pop *widget-infoscent-stack*)))
	
	!eval! (reset-frame-parameters)
	
	; knowledge to focus on the subordinate group upon visiting a 2nd-level page
	#+new-ct-e !bind! #+new-ct-e =group #+new-ct-e (if (and *handle-groups-in-half-flatten-layout* (find (name (curframe *cogtool-design*)) *2nd-level-pages* :test #'equal))
		              (progn (push (first *widget-infoscent-stack*) *widget-infoscent-stack*)
			                 (push (list (name (curframe *cogtool-design*)) nil) *history*)
					         (concatenate 'string "Subordinate in " (name (curframe *cogtool-design*))))
					  nil)
	#+new-ct-e !eval! #+new-ct-e (setf *current-group* =group)

	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-loc
		best-widget dummy-widget
		#+new-ct-e group #+new-ct-e =group
		state		=state   		
)

(p check-for-target-frame-within-group
	=goal>
		ISA			search-task
		best-widget =best-widget
		group		=group
		state		wait
   
	?manual>   
		state		free
==>
	!eval! (if *use-finger-default-value* (infer-transition *cogtool-design*)) ; CogTool device currently only call infer-transition on a mouse click
   
	!bind! =state (if (equal (name (curframe *cogtool-design*)) (first *CogTool-Explorer-target-frames*)) 'found 'find) ;;; needs modification to support multiple target frames

	; update log file
	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (if (second (first *history*)) (fetch-non-cummulative-step-index (second (first *history*))) (fetch-non-cummulative-step-index (first (first *history*))))) ";"
											#-new-ct-e (write-to-string (if (second (first *history*)) (fetch-cummulative-step-index (second (first *history*))) (fetch-cummulative-step-index (first (first *history*))))) ";"
											(if (second (first *history*)) "" (first (first *history*))) ";"
											(if (second (first *history*)) (second (first *history*)) "") ";"
											";"
											(trim =best-widget) ";"
											(string =state) ";"
											(write-to-string (mp-time)) ";"
											(write-to-string *number-widgets-read*) "~%")))
	
	!eval! (incf *step-index*)
	
	#-new-ct-e !eval! #-new-ct-e (if (second (first *history*))
		(store-cummulative-step-index (second (first *history*)) (+ (fetch-cummulative-step-index (second (first *history*))) 1))
		(store-cummulative-step-index (first (first *history*)) (+ (fetch-cummulative-step-index (first (first *history*))) 1)))
	#-new-ct-e !eval! #-new-ct-e (if (second (first *history*))
		(store-non-cummulative-step-index (second (first *history*)) (+ (fetch-non-cummulative-step-index (second (first *history*))) 1))
		(store-non-cummulative-step-index (first (first *history*)) (+ (fetch-non-cummulative-step-index (first (first *history*))) 1)))
	
	; "knowledge" to handle a self-transition link as essentially null
	!eval! (if (equal (name (curframe *cogtool-design*)) (first (first *history*))) (progn (pop *history*) (pop *widget-infoscent-stack*)))
	!eval! (setf *current-group* (if (equal (name (curframe *cogtool-design*)) (first (first *history*))) =group nil))
	!bind! =new-group (if (equal (name (curframe *cogtool-design*)) (first (first *history*))) =group nil)
	
	!eval! (reset-frame-parameters)
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-loc
		best-widget dummy-widget
		group		=new-group
		state		=state   		
)

(p go-back-to-previous-group
	=goal>
		isa			search-task
	  - group		nil
		state		go-back-to-previous
==>
	!eval! (pop *widget-infoscent-stack*)
	!eval! (setf *current-group* (second (first *history*)))
	!bind! =group (second (pop *history*))
	
	!eval! (reset-frame-parameters)
;	!eval! (setf *last-screen-change* (mp-time))
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-loc
		best-widget dummy-widget
		group		=group
		state		find
)

(p go-back-to-another-group-using-non-header-process
	!eval! (eval *mixed-visual-search*)

	=goal>
		isa			search-task
		group		=current-group
		state		go-back-to-previous
		
	?visual>
		state			free
==>	
	!eval! (setf *current-group* (second (first *history*)))
	!bind! =previous-group (second (first *history*))

	+visual-location>
		isa 			cogtool-visual-location
		kind			cogtool-group
	  - widget-name		=current-group
		member-of-group	=previous-group
		:nearest 		current
	
	=goal>
		group			=previous-group
		state			non-header-process
)

(p look-at-another-group-in-non-header-process
	=goal>
		isa			search-task
		state		non-header-process
		
	=visual-location>
		isa 		cogtool-visual-location
		kind		cogtool-group		
		
	?visual>
		state		free
==>
	=visual-location>
	
	+visual>
		isa			move-attention
		screen-pos	=visual-location
   
	=goal>
		state		look-at-another-group
)

(p focus-on-another-group-in-non-header-process
	=goal>
		isa			search-task
		group		nil
		state		look-at-another-group
		
	?visual>
		state		free
		
	=visual-location> ; temporary solution until cogtool-visual-location slots are moved to cogtool-visual-object by Don
		isa			cogtool-visual-location
		kind		cogtool-group
		widget-name =widget-name
   
	=visual>
		isa			visual-object
==>
	#-suppress-trace !output! #-suppress-trace (>>> Focus on another group =widget-name <<<)
	
	!eval! (with-open-file (*file* (concatenate 'string *log-file-directory*
		(substitute #\- #\? (subseq *task-description* 0 (min 9 (length *task-description*))))
		"_C=" (write-to-string *go-back-cost*)
		"_K=" (write-to-string *SNIF-ACT-k-value*) ".txt")
		:direction :output
		:if-does-not-exist :create
		:if-exists :append)
		(format *file* (concatenate 'string (write-to-string (1+ *model-run*)) ";"
											(write-to-string *step-index*) ";"
											#-new-ct-e (write-to-string (fetch-non-cummulative-step-index (name (curframe *cogtool-design*)))) ";"
											#-new-ct-e (write-to-string (fetch-cummulative-step-index (name (curframe *cogtool-design*)))) ";"
											(name (curframe *cogtool-design*)) ";"
											";"
											(trim =widget-name) ";"
											";"
											"FIND;"
											(write-to-string (mp-time)) ";"
											(write-to-string 0) "~%")))
	
	!eval! (incf *step-index*)

	#-new-ct-e !eval! #-new-ct-e (store-cummulative-step-index (name (curframe *cogtool-design*)) (+ (fetch-cummulative-step-index (name (curframe *cogtool-design*))) 1))
	#-new-ct-e !eval! #-new-ct-e (store-non-cummulative-step-index (name (curframe *cogtool-design*)) (+ (fetch-non-cummulative-step-index (name (curframe *cogtool-design*))) 1))	
	
;	=visual>
;		value		=widget-name
;		status		chosen
   
;	-visual> ; ensures modified chunk in visual buffer enters DM

	!eval! (update-infoscent-stack-in-non-header-process)
	!eval! (reset-frame-parameters)
	!eval! (setf *current-group* =widget-name)
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-location
		group		=widget-name
		state		find  
)

(p go-back-to-previous-frame
	!eval! (eval *use-back-button*)

	=goal>
		isa			search-task
		group		nil
		state		go-back-to-previous

	?visual>
		state		free
==>
	+visual-location>
		isa 			cogtool-visual-location
		is-back-button          t

	=goal>
		state locate-back-button
)

(p look-at-back-button
	=goal>
		isa			search-task
		state		locate-back-button

	?visual>
		state		free

	=visual-location>
		isa			cogtool-visual-location
		is-back-button          t
==>
	=visual-location>

	+visual>
		isa			move-attention
		screen-pos	=visual-location
   
	=goal>
		state		look-at-back-button
)

(p move-mouse-to-back-button
	=goal>
		isa			search-task
		state		look-at-back-button

	?visual>
		state		free

	=visual-location>
		isa			cogtool-visual-location		
		
	=visual>
		isa			visual-object
		
	?manual>
		state		free 
==>   
	+manual>
		isa			move-cursor
		loc			=visual-location

	=goal>
		state		move-mouse-to-back-button		
)

(p click-back-button
	=goal>
		isa			search-task
		state		move-mouse-to-back-button
		
	?manual>
		state		free 
==>
	+manual>
		isa			click-mouse
	
	=goal>
		state		click-back-button
)

(p back-button-clicked
	=goal>
		isa			search-task
		state		click-back-button
		
	?manual>
		state		free 
==>
	!eval! (pop *widget-infoscent-stack*)
	!eval! (setf *current-group* (second (first *history*)))
	!bind! =group (second (first *history*))
	#-new-ct-e !eval! #-new-ct-e (pop *history*)
	#+new-ct-e !eval! #+new-ct-e (if *use-back-button-history-in-half-flatten-layout* (transition (first (pop *history*)) *cogtool-design*) (pop *history*))
		
	!eval! (reset-frame-parameters)
;	!eval! (setf *last-screen-change* (mp-time))
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-loc
		best-widget dummy-widget
		group		=group
		state		find
)

(p go-back-to-previous-frame-direct
	!eval! (and *allow-magic-go-backs* (not *use-back-button*))

	=goal>
		isa			search-task
		group		nil
		state		go-back-to-previous
==>
	!eval! (pop *widget-infoscent-stack*)
	!eval! (setf *current-group* (second (first *history*)))
	!bind! =group (second (first *history*))
	!eval! (transition (first (pop *history*)) *cogtool-design*)
		
	!eval! (reset-frame-parameters)
;	!eval! (setf *last-screen-change* (mp-time))
	
	=goal>
		best-cue	"dummy-text"
		best-loc 	dummy-loc
		best-widget dummy-widget
		group		=group
		state		find
)

);end of define-model

(dotimes (*model-run* (1- *number-of-runs*))
	(setq *cogtool-result* (cogtool-run-model))
	(format t "~A~%<<< CT-Explorer: between runs marker >>>" *cogtool-result*)
	(reset))
	
(setf *model-run* (1- *number-of-runs*))
