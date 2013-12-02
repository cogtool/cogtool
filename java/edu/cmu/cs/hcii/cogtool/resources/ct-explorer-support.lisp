(setf *widget-value-is-title* t) ;;LT
(setf *infer-transitions* t) ;;LT

(print 'in-cte-support)

(defparameter *code-trace* (not *suppress-trace*))

(defparameter *update-prod-param-verbose* (not *suppress-trace*))

;;; Note that this function is originally defined, though differently,
;;; in cogtool-actr.lisp. If changed here, consider whether or not to
;;; propogate those changes thither.
(fmakunbound 'frame-vis-loc-template) ; suppress redef warning
(defun frame-vis-loc-template (thing &optional group)
  (cogtool-debug 2 "frame-vis-loc-template called: ~A, ~A, ~S, ~S" (and thing (name thing)) (and group (name group)) thing group)
  (let ((name (name thing))
    (hrl (has-remote-label thing)))
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
      textual-cue ,(textual-cue thing)
      remote-label-of ,(and (remote-label-of thing) (name (remote-label-of thing)))
      has-remote-label ,(and hrl (if (stringp hrl) hrl (name hrl)))
      ,@(when group `(member-of-group ,(name group))))))

(defun cte-noise (x)
  (if *cogtool-random-seed* 0 (act-r-noise x)))    

(defun find-best-cue (query best-link link-text &key (update-prod t) (noise *infoscent-noise*))
  (progn
    (if *code-trace* (format t "~S " link-text))
    (setf *current-widget-scent* (profitability query (if (stringp link-text) link-text (car link-text)) noise nil))  
    (if *code-trace* (format t "scent = ~S" *current-widget-scent*))

    (when (>= *current-widget-scent* *best-widget-scent*)
      (setf *best-widget-scent* *current-widget-scent*))
    (when update-prod
      (update-prod-parameters *current-widget-scent* *best-widget-scent*))
    (if (>= *current-widget-scent* *best-widget-scent*) link-text best-link)))

(defun profitability (query text &optional (noise *infoscent-noise*) (remove-stop-word-p t))
  (if (not (string= text "dummy-text"))
	(if *infoscent_variabilty_between_runs*
		(let ((prev-value (fetch-assessed-infoscent text)))
			(if prev-value prev-value
				(let ((new-value (link-activation query text noise remove-stop-word-p)))
					(store-assessed-infoscent text new-value) new-value)))
		(link-activation query text noise remove-stop-word-p))
    0.0))

(defun link-activation (query link &optional (noise *infoscent-noise*) (remove-stop-word-p t))
    ;(max 0.01 (+ (max 0 (* (fetch-pmi query link) *similarity-scale-factor*)) (cte-noise noise))))
	(if *consider-familarity*
		(max 0.01 (+ (* (fetch-pmi query link) *similarity-scale-factor* (fetch-familarity link)) (cte-noise noise)))
		(max 0.01 (+ (* (fetch-pmi query link) *similarity-scale-factor*) (cte-noise noise)))))
    ;(+ (max 0 (fetch-pmi (trim query) (trim link))) (cte-noise noise)))

(defun update-prod-parameters (cur-link-val best-link-val)
  (let ((read-value (car (car (no-output (spp-fct (list 'read-another-widget :u))))))
        (choose-value (car (car (no-output (spp-fct (list 'choose-best-widget :u))))))
        (go-back-value (car (car (no-output (spp-fct (list 'go-back :u)))))))
    (when (> cur-link-val  0) ;SNIF-ACT: read-another-widget may fire a lot otherwise ;This is not be necessary anymore, as infoscent values are not negative.
      
      (push cur-link-val *widget-infoscent-list*)
      (when (>= (length *widget-infoscent-stack*) 1)
	(unless *suppress-trace*
	  (format t ", go-back_formula_first_operand = ~S, go-back_formula_second_operand = ~S"
		    (if *second-best-prev-page* (second-best-previous-page (first *widget-infoscent-stack*)) (mean-previous-page (first *widget-infoscent-stack*)))
		    (mean-current-page (first *widget-infoscent-stack*) *widget-infoscent-list*)))
        (spp-fct (list 'go-back :u (- (- 
		(if *second-best-prev-page* (second-best-previous-page (first *widget-infoscent-stack*)) (mean-previous-page (first *widget-infoscent-stack*)))
		(mean-current-page (first *widget-infoscent-stack*) *widget-infoscent-list*)
		) *go-back-cost*))))
            
      (spp-fct (list 'read-another-widget :u (/ (+ read-value cur-link-val) (1+ *number-widgets-read*))))
      (spp-fct (list 'choose-best-widget :u (/ (+ choose-value best-link-val) (+ (1+ *number-widgets-read*) *SNIF-ACT-k-value*))))
      
      (when *update-prod-param-verbose*
        (spp read-another-widget :u)
        (spp choose-best-widget :u)
        (spp go-back :u))    
      )))

(defun mean-previous-page (prev-page-list)
  (let ((numerator (if *modified-mean-prev-page* (- (apply #'+ (rest prev-page-list)) (first (last (first prev-page-list)))) (apply #'+ (rest prev-page-list))))
        (denominator (if *modified-mean-prev-page* (- (length prev-page-list) 2) (- (length prev-page-list) 1))))
    (if (> denominator 0) (/ numerator denominator) 0)))

(defun second-best-previous-page (prev-page-list)
  (let ((temp-list (copy-seq (rest prev-page-list))))
    (second (sort temp-list #'>))))

(defun mean-current-page (prev-page-list curr-page-list)
	(if *modified-mean-current-page*
		(mean (append (first prev-page-list) curr-page-list))
		(if *update-confidence*
			(mean (append (butlast (first prev-page-list)) curr-page-list))
			(mean curr-page-list))))
	
(defun mean (num-list)
  (let ((sum (apply #'+ num-list))
        (len (length num-list)))
    (if (> len 0) (/ sum len) 0)))
	
(defun update-infoscent-stack-in-non-header-process ()
  (let* ((prev-page-list (first *widget-infoscent-stack*))
		 (selected-infoscent (first (last (first prev-page-list))))
		 (position-selected (position selected-infoscent prev-page-list))
		 (mis-previous (mean-previous-page prev-page-list)))
	(progn
		(setf (nth position-selected (first *widget-infoscent-stack*)) (* mis-previous *non-header-process-conf-scale*))
		(setf (first (first *widget-infoscent-stack*)) (list (* mis-previous *non-header-process-conf-scale*))))))

;;;---------------------------------------------------------------------------
;;; String parsing/handling utilities
;;;
;;; Takes a string and parses it up using whitespace as delimiter.
;;; Takes keywords :SPACE and :TAB which indicate whether spaces
;;; or tabs are considered to be whitespace (default for both is t)
;;; NOTE: (parse-terms <string> :space nil :tab t) or
;;; equivalently (parse-terms <string> :space nil)
;;; is useful for parsing tab-delimited files.

(defun parse-terms (string &key (space t) (tab t))
  (flet ((whitespace (x) (or (and (char-equal x #\space) space)
			     (and (char-equal x #\tab) tab))))
    (let ((end (length string)))
      (do* ((start 0 (1+ next-space))
            (next-space (or (position-if #'whitespace string)
			    end)
			(if (position-if #'whitespace
					 (subseq string (1+ next-space)))
			    (+ (position-if #'whitespace
					    (subseq string (1+ next-space)))
			       (1+ next-space))
			  end))
            (terms (list (subseq string start next-space))
		   (cons (subseq string start next-space) terms)))
          ((= next-space end) (reverse terms))))))


(defun remove-duplicate-terms (string)
  (apply 'concatenate 
	 'string 
	 (do ((elts (remove-duplicates (parse-terms string) :test #'equalp) (cdr elts))
	      (new))
	     ((null (cdr elts))
	      (push (car elts) new)
	      (reverse new))
	   (setq new (cons " " (cons (car elts) new))))))

(defparameter *delimit-char-list* (list #\space #\tab)) ;#\- #\. #\@ #\\ #\/

(defun delimit-char-p (txt index)
  (let ((result nil)
	;(delimiting-char-lst delimit-char-list)
	)
    (dolist (c *delimit-char-list*)
      (if (equal (char txt index) c) (setq result t)))
    result))

(defun parse-elt-txt (link-txt &optional (remove-stop-wd-p t))
  (let ((txt link-txt)
	(last_index 0)
	(w_lst nil))
    ;get rid of spaces before start of txt
    (cond ((> (length txt) 0)
	   (loop (when (or (not (delimit-char-p txt last_index))
			   (>= (1+ last_index) (length txt)))
		   (setq txt (subseq txt last_index (length txt)))
		   (return))
	     (incf last_index))

	;get rid of spaces AFTER end of txt
	   (setq txt (reverse txt) last_index 0)
	   (loop (when (or (not (delimit-char-p txt last_index))
			   (>= (1+ last_index) (length txt)))
		   (setq txt (subseq txt last_index (length txt)))
		   (return))
	     (incf last_index))

	   (setq txt (reverse txt) last_index 0)

	;parse the txt, push individual words into lst
	   (dotimes (x (length txt))
	     (if (and (> x last_index) (delimit-char-p txt x))
		 (progn (push (subseq txt last_index x) w_lst)
			(setq last_index (1+ x))
			;get rid of double spaces
			(loop (when (not (or (equal (char txt last_index) #\space)(equal (char txt last_index) #\tab))) (return))
			  (incf last_index))
			)))
	   
	   (push (subseq txt last_index (length txt)) w_lst)
	   ))
    (if remove-stop-wd-p
	(setq w_lst (remove-if #'(lambda(x) (not (fetch-pmi x x))) w_lst))) ;remove stop words -- defined as 0 term frequency in database
    (reverse w_lst) 
    ))

;;; takes a list of terms1 and a list of terms2 returns a list of all
;;; possible pairings of terms where term1 does not equal term2
;;; of their elements, and where both term1 and term2 have entries in the term DB
(defun make-pairs (terms1 terms2)
   (let ((result))
      (dolist (w1 terms1)
	(when (base-strength w1)
	  (dolist (w2 terms2)
	    (unless (equalp w1 w2)
	      (when (base-strength w2)
		(push (list w1 w2) result))))))
     result))


;;;------------------------------------------------------------------------
;;; Strengths
;;;------------------------------------------------------------------------


(defconstant *maximum-total-documents* 7468414) ;from PARC GLSA server
(defconstant *minimum-base-strength* (log (/ 1.0 *maximum-total-documents*)))


(defun normalize-strength (strength)
  (- strength *minimum-base-strength*))

(defun base-strength (word)
  (let ((freq (fetch-term-freq word))
        (strength *minimum-base-strength*))
    (if freq
        (progn
          (when (> freq 0)
          (let* ((Pr[i] (/ freq *maximum-total-documents*))
                 (Pr[-i] (- 1 Pr[i])))
            (setq strength (log (/ Pr[i] Pr[-i])))))
          (normalize-strength strength))
      nil)
    )
  )

;(defun base (word)
;  (let ((freq (fetch-term-freq word))
;        (strength *minimum-base-strength*))
;    (when (> freq 0)
;      (let* ((Pr[i] (/ freq *maximum-total-documents*))
;             (Pr[-i] (- 1 Pr[i])))
;	(setq strength (log (/ Pr[i] Pr[-i])))))
;    strength))

;(defun base (word)
;  (fetch-pmi word word)) ; returning nil will raise an error

(defun base (word)
  (log (/ *maximum-total-documents* (fetch-term-freq word))))

;(defun base-strength (word)
;    (let ((pmi (fetch-pmi word word)))
;      (if pmi (* (+ pmi 3) 3.33) nil))) ; returning nil will raise an error

(defun association-strength (word1 word2)
  (let ((pmi (fetch-pmi word1 word2)))
    (if pmi (* (+ pmi 1) 5) nil))) ; returning nil will raise an error
    
;;;------------------------------------------------------------------------
;;; Term Frequency and PMI Data Files
;;;------------------------------------------------------------------------

(defvar *frequencies* (make-hash-table :test #'equalp))
(defvar *pmi* (make-hash-table :test #'equalp)) ;double hash table for pmi data

(defmacro store-term-freq (word freq)
  `(setf (gethash (symbol-name ',word) *frequencies*) ,freq))

(defun fetch-term-freq (word)
  (gethash word *frequencies*))

(defun fetch-pmi (word1 word2)
  (let ((h-table2 (gethash word1 *pmi*)))
    (if h-table2
        (gethash word2 h-table2)
      nil)))

;(defmacro store-pmi (w1 w2 freq)
;  `(progn (store-pmi-fct (symbol-name ',w1) (symbol-name ',w2) ,freq)
;         (store-pmi-fct (symbol-name ',w2) (symbol-name ',w1) ,freq)))

(defun store-score (w1 w2 freq)
  (progn (store-pmi-fct w1 w2 freq)
         (store-pmi-fct w2 w1 freq)))
  
(defun store-pmi-fct (word1 word2 freq)
  (let ((h-table2 (gethash word1 *pmi*)))
    (if h-table2
        (setf (gethash word2 h-table2) freq)
      (progn
        (setf (gethash word1 *pmi*) (make-hash-table :test #'equalp))
        (setf (gethash word2 (gethash word1 *pmi*)) freq)))
    freq))

;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

(defun trim (string)
  (string-trim '(#\Space #\Tab) string))
  
;; Overwrite find-current-locs-with-spec in vision.lisp to enable fluctuation factor when testing for nearest
(defmethod find-current-locs-with-spec ((vis-mod vision-module) spec)
  "Assume that it's a valid visual-location chunk-spec with at most 1
   attended slot specification and one nearest spec"
  
  (let* ((main-spec (strip-request-parameters-from-chunk-spec spec))
         (attended (when (slot-in-chunk-spec-p spec :attended)
                     (car (chunk-spec-slot-spec spec :attended))))
         (slots (chunk-spec-slot-spec main-spec))
         (current (current-marker vis-mod))
         (current-type (when current (chunk-chunk-type-fct current)))
         (nearest (when (slot-in-chunk-spec-p spec :nearest)
                    (car (chunk-spec-slot-spec spec :nearest))))
         (min-max-tests nil))
    
      
    ;; Remap all current values to the current chunk
    
    (if current
        (dolist (x slots)
          (when (eq (third x) 'current)
            (if (find (second x) (chunk-type-slot-names-fct current-type))
                (setf (third x) (chunk-slot-value-fct current (second x)))
              (progn
                (print-warning "Current visual-location does not have a slot named ~S so it is ignored in the request."
                               (second x))
                (setf slots (remove x slots))))))
      (dolist (x slots)
        (when (eq (third x) 'current)
          (print-warning "There is no currently attended location.  So, request specifying ~S as current is being ignored."
                         (second x))
          (setf slots (remove x slots)))))
    
    ;; Remove all tests for highest and lowest for later
    
    (dolist (x slots)
      (when (or (eq (third x) 'lowest)
                (eq (third x) 'highest))
        (push-last x min-max-tests)
        (setf slots (remove x slots))))
    
    ;; update the finsts an new markers
    
    (when attended
      (update-new vis-mod)
      (check-finsts vis-mod))
  
    ;; find the chunks that match
    
    (let ((possible-chunks (if attended
                               (matching-attended-chunks attended (visicon-chunks vis-mod t))
                             (visicon-chunks vis-mod t)))
          (changed nil))
      
      ;; Hack to reassign value slots as needed before testing
      
      (dolist (check possible-chunks)
        (when (chunk-real-visual-value check)
          (push (cons check (chunk-slot-value-fct check 'value)) changed)
          (fast-set-chunk-slot-value-fct check 'value (chunk-real-visual-value check)))) 
      
    (let ((matching-chunks (find-matching-chunks (slot-specs-to-chunk-spec (chunk-spec-chunk-type main-spec) slots)
                                                 :chunks possible-chunks :variable-char #\&)))
      ;; apply all of the lowest/highest constraints
      ;; in the order provided
      
      (dolist (x min-max-tests)
        (let ((value nil)
              (truth (first x))
              (slot (second x))
              (test (third x)))
          
          ;; find the min/max value
          (dolist (y matching-chunks)
            (let ((cur-val (fast-chunk-slot-value-fct y slot)))
              (unless (numberp cur-val)
                (setf value :fail)
                (print-warning "Cannot apply ~S constraint because not all chunks have a numerical value." x)
                (return))
              (when (or (null value) 
                        (and (eq test 'lowest)
                             (< cur-val value))
                      (and (eq test 'highest)
                           (> cur-val value)))
                (setf value cur-val))))
          
          (setf matching-chunks (remove-if-not (lambda (z)
                                                 (if (eq truth '=)
                                                     (= value z)
                                                   (not (= value z))))
                                               matching-chunks
                                               :key (lambda (z) 
                                                      (fast-chunk-slot-value-fct z slot))))))
      
      ;; if there's a nearest constraint then
      ;; apply that filter now
      
      (when (and nearest matching-chunks)
                
        (if (or (eq (third nearest) 'current)
                (eq (third nearest) 'current-x)
                (eq (third nearest) 'current-y)
                (and (chunk-p-fct (third nearest))
                     (chunk-type-subtype-p-fct (chunk-chunk-type-fct (third nearest)) 'visual-location)))
        
            (let ((value nil)
                  (truth (first nearest))
                  (test (third nearest))
                  (matches nil)
                  (current-loc (aif (current-marker vis-mod) 
                                    it 
                                    (progn 
                                      (model-warning "No location has yet been attended so current is assumed to be at 0,0.")
                                      (car (define-chunks (isa visual-location screen-x 0 screen-y 0)))))))
          
          ;; find the min value
          (dolist (y matching-chunks)
            (let ((cur-val (cond ((eq test 'current)
                                  (* (+ 1 (cte-noise 0.1654)) (dist (xy-loc y) (xy-loc current-loc)))) ;;;;; Modified
                                 ((eq test 'current-x)
                                  (abs (- (fast-chunk-slot-value-fct y 'screen-x) (fast-chunk-slot-value-fct current-loc 'screen-x))))
                                 ((eq test 'current-y)
                                  (abs (- (fast-chunk-slot-value-fct y 'screen-y) (fast-chunk-slot-value-fct current-loc 'screen-y))))
                                 (t
                                  (dist (xy-loc y) (xy-loc test))))))
              (if (or (null value) 
                      (< cur-val value))
                  (progn
                    (setf value cur-val)
                    (setf matches (list y)))
                (when (= cur-val value)
                  (push y matches)))))
          
              (setf matching-chunks matches))
          
          (progn
            (print-warning "Nearest test in a visual-location request must be current or a chunk that is a subtype of visual-location.")
            (print-warning "Ignoring nearest request for ~S." (third nearest))))
          ) 
      
      ;; undo the value slots that were changed for matching purposes
      
      (dolist (restore changed)
        (fast-set-chunk-slot-value-fct (car restore) 'value (cdr restore)))
            
      matching-chunks))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Familarity and Assessed Infoscent;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *familarity* (make-hash-table :test #'equalp))

(defun store-familarity (word value)
  (setf (gethash word *familarity*) value))

(defun fetch-familarity (word)
  (let ((value (gethash word *familarity*)))
	(if value value 1)))

;; TODO Does this really belong in generic CogTool CT-E? It seems specific to Leonghwee's research
	
(store-familarity "Legends & Folklore" 0.54)
(store-familarity "Anatomy & Physiology" 0.51)
(store-familarity "Mythology" 0.17)
(store-familarity "Scripture" 0.1)
(store-familarity "The Occult" 0.08)
(store-familarity "Paleontology" 0.06)
(store-familarity "Anthropology" 0.5)
(store-familarity "Archaeology" 0.1)

(defun store-assessed-infoscent (text infoscent)
  (setf (gethash text *assessed-infoscent*) infoscent))

(defun fetch-assessed-infoscent (text)
  (gethash text *assessed-infoscent*))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cummulative and non-Cummulative step counters in log file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *cummulative-step-index* (make-hash-table :test #'equalp))
(defvar *non-cummulative-step-index* (make-hash-table :test #'equalp))

(defun store-cummulative-step-index (text index)
  (setf (gethash text *cummulative-step-index*) index))

(defun fetch-cummulative-step-index (text)
  (let ((index (gethash text *cummulative-step-index*)))
	(if index index 1)))
  
(defun store-non-cummulative-step-index (text index)
  (setf (gethash text *non-cummulative-step-index*) index))

(defun fetch-non-cummulative-step-index (text)
  (let ((index (gethash text *non-cummulative-step-index*)))
	(if index index 1)))