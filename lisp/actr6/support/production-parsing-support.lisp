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
;;; Filename    : production-parsing-support.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions and code that's used by both p and p* parsing.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider collapsing multiple conditions for the
;;;             :     same buffer into one spec and test for conflicts (how
;;;             :     often does that happen though?)
;;;             : [ ] Does sort-for-binding actually do anything useful now?
;;; 
;;; ----- History -----
;;; 2009.09.10 Dan [1.0]
;;;             : * Initial creation.
;;; 2009.09.18 Dan
;;;             : * Now all the real work of the production parsing is here and
;;;             :   p and p* are done by a single set of code.
;;; 2009.09.28 Dan
;;;             : * Added the additional parsing necessary for handling the 
;;;             :   searchable buffers.
;;; 2010.01.12 Dan
;;;             : * Augmented the search condition to include the buffer's
;;;             :   variable so that doesn't have to be recreated every
;;;             :   conflict-resolution.
;;; 2010.04.23 Dan
;;;             : * Modified the condition creation so that results are always
;;;             :   explicitly t or nil (where appropriate).
;;;             : * Changed number-test to make sure it returns t or nil.
;;;             : * Instead of chunk-slot-equal as a test in a condition now
;;;             :   use safe-chunk-slot-equal which makes sure to return t or
;;;             :   nil explicitly.
;;; 2010.05.04 Dan
;;;             : * Moved the test to determine if a searched buffer is used
;;;             :   more than once so that it actually can catch that and reject
;;;             :   the production.
;;; 2010.07.27 Dan
;;;             : * Fixed a bug which broke the ability to use a variablized
;;;             :   slot in a request in a p*.
;;; 2010.07.27 Dan
;;;             : * Fixed a bug that would allow a p to have p* actions if they
;;;             :   were identical to ones cached for a p*.
;;; 2010.07.28 Dan
;;;             : * Fixed a bug introduced with the last fix that would break
;;;             :   for some requests which used modifiers.
;;; 2010.09.30 Dan 
;;;             : * Properly order the binds on the RHS and make sure that they
;;;             :   come before any other actions that use them.
;;; 2010.09.30 Dan
;;;             : * Fixed a bug in circular-references introduced when mv-bind
;;;             :   was added that broke the check for normal binds.
;;; 2011.01.21 Dan
;;;             : * Ensure that all non-reordered binds stay in 'production 
;;;             :   order' which, while not technically specified, is how
;;;             :   people expect it to happen.
;;; 2011.01.26 Dan
;;;             : * Give a warning if RHS binds/evals are going to be executed
;;;             :   in an order other than specified in the production.
;;; 2011.02.09 Dan
;;;             : * Changed stable-sorting with bind-order to a special sorting function
;;;             :   because it was easier than trying to fix bind-order to get 
;;;             :   things right when there were intermixed dependencies.
;;; 2011.02.11 Dan
;;;             : * Updated that new sort function to use splice-into-position-des
;;;             :   instead of splcie-into-list-des because the latter unpacks
;;;             :   list elements being inserted.
;;; 2011.04.01 Dan
;;;             : * Adjusted the RHS action sorting some more so that evals and
;;;             :   outputs remain in original order with binds if there's no
;;;             :   dependencies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Moved common code for production parsing to support to avoid some warnings
;;; at load time and as a first step to a better integration of p and p*.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Because of the way that conditions are tested in the procedural matching
;;; functions the results which are being tested for must be eq to the 
;;; values returned from the testing functions.  In particular, that means
;;; that I can't just return something like (eq val1 val2) from a test function
;;; if t is the result that I'm looking for because eq (and all of the Lisp
;;; equality/testing functions) are only required to return a generalized-boolean
;;; with anything non-nil representing true.
;;;
;;; I've chosen to handle this in the test functions instead of changing the
;;; matching code because I don't see a fast and clean replacement for the current: 
;;; (eq (result ...) (funcall ...)) test that's there now which handles either
;;; t or nil directly and also allows for the possibility of other values with future
;;; updates.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defun safe-chunk-slot-equal (c1 c2)
  (when (chunk-slot-equal c1 c2) t))

(defun create-production (prod definition dynamicp)
  (let ((production (make-production :text (copy-tree definition) :dynamic dynamicp)))
    
    (unless (symbolp (car definition))
      (print-warning "Production name must be a symbol.")
      (print-warning "No production defined for ~S." definition)
      (return-from create-production nil))
    
    (setf (production-name production) (pop definition))
    
    (when (stringp (car definition))
      (setf (production-documentation production) (pop definition)))
    
    (aif (position '==> definition)
         (let* ((pre-lhs (parse-conditions prod (subseq definition 0 it)))
                (lhs (if (eq :error pre-lhs) :error (sort-for-binding pre-lhs)))
                (rhs (unless (eq lhs :error)
                       (parse-actions prod (subseq definition (1+ it)) lhs dynamicp)))
                (searched-buffers nil))
           (when (or (eq lhs :error)
                     (eq rhs :error))
             (print-warning "No production defined for ~s." (production-text production))
             (return-from create-production nil))
           
           (setf (production-lhs production) lhs)
           (setf (production-rhs production) rhs)
           
           (setf (production-lhs-buffers production)
             (remove-duplicates (mapcan #'(lambda (x)
                                            (when (eq (caar x) #\=)
                                              (list (cdar x))))
                                  lhs)))
           (setf (production-rhs-buffers production)
             (remove-duplicates (mapcan #'(lambda (x)
                                            (unless (eq (caar x) #\!)
                                              (list (cdar x))))
                                  rhs)))
           
           
           (dolist (x (production-lhs-buffers production))
             (unless (assoc x (procedural-buffer-indices prod))
               (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
               (incf (procedural-buffer-lookup-size prod))))                            
           
           (dolist (x (production-rhs-buffers production))
             (unless (assoc x (procedural-buffer-indices prod))
               (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
               (incf (procedural-buffer-lookup-size prod))))
           
           
           (let ((variables (mapcan 'find-variables
                              (mapcar 'second (append lhs rhs))))
                 (lhs-variables (mapcan 'find-variables (mapcar 'second lhs)))
                 (safe-bindings nil)
                 (slot-variables nil))
             
             (setf (production-variables production)
               (remove-duplicates 
                (append variables
                        (mapcar #'(lambda (x)
                                    (intern (concatenate 'string "=" (symbol-name x))))
                          (production-lhs-buffers production)))))
             
             (setf (production-bindings production) variables)
             
             (let ((constants nil)
                   (vars nil)
                   (var2s nil)
                   (others nil)
                   (binds nil)
                   (selection nil)
                   (implicit nil)
                   (search nil)
                   (search-bind nil)
                   (search-other nil)
                   (var-class nil)
                   (var-table (make-hash-table))
                   )
               
               (dolist (buffer-name (production-lhs-buffers production))
                 (let ((bn buffer-name)  ;; closure voodoo
                       (bi (cdr (assoc buffer-name (procedural-buffer-indices prod))))
                       (bv (intern (concatenate 'string "=" (symbol-name buffer-name)))))
                   
                   (setf (production-bindings production)
                     (remove bv (production-bindings production)))
                   
                   (if (searchable-buffer buffer-name)
                       (setf (gethash bv var-table) 'search)
                     (setf (gethash bv var-table) 'buffer))
                   
                   (if (searchable-buffer buffer-name)
                       (progn
                         
                         (push (make-cr-condition :type 'bind-buffer :buffer bn :bi bi :value bv) search-bind)
                         (push #'(lambda ()
                                   (overwrite-buffer-chunk bn (cdr (assoc bv (production-bindings production)))))
                               (production-conflict-code production)))
                     
                     (push (make-cr-condition :type 'bind-buffer :buffer bn :bi bi :value bv) vars))))
               
               ;; new first step 
               ;; classify all of the variables as to how they are used
               ;; so that things can be put into the right places later.
               ;; 
               ;; A variable can be bound through one of: buffer, bind, fixed-slot, var-slot, search, search-slot
               
               (dolist (cond lhs)
                 (let* ((c cond)  ;; so the closures bind correctly
                        (buffer (cdar c))
                        (bi (cdr (assoc buffer (procedural-buffer-indices prod)))))
                   (case (caar c)
                     (#\=
                      (let* ((var-spec-list (fourth c))
                             (var2-spec-list (eighth c)))
                        
                        (dolist (v var-spec-list)
                          (let ((var (third v)))
                            (if (searchable-buffer buffer)
                                (unless (gethash var var-table)
                                  (setf (gethash var var-table) 'search-slot))
                              
                              (unless (gethash var var-table)
                                (setf (gethash var var-table) 'fixed-slot)))))
                        
                        
                        (dolist (v var2-spec-list)
                          (when (chunk-spec-variable-p (third v))
                            (let ((var (third v)))
                              (if (searchable-buffer buffer)
                                  (case (gethash (second v) var-table)
                                    ((buffer bind fixed-slot var-slot)
                                     (unless (gethash var var-table)
                                       (setf (gethash var var-table) 'search-slot)))
                                    (t
                                     (print-warning "No production defined for ~s because indirection not allowed among search buffer slots." (production-text production))
                                     (return-from create-production nil)))
                                (unless (gethash var var-table)
                                  (setf (gethash var var-table) 'var-slot))))))))
                     
                     (#\!
                      (case (cdar c)
                        ((bind safe-bind)
                         (let ((var (car (second c))))
                           (unless (gethash var var-table)
                             (setf (gethash var var-table) 'bind))))
                        ((mv-bind)
                         
                         (let ((bind-vars (car (second c))))
                           (dolist (x bind-vars)
                             (unless (gethash x var-table)
                               (setf (gethash x var-table) 'bind))))))))))
               
               
               ;(inspect var-table)
               
               
               (dolist (cond lhs)
                 (let* ((c cond)  ;; so the closures bind correctly
                        (buffer (cdar c))
                        (bi (cdr (assoc buffer (procedural-buffer-indices prod)))))
                   (case (caar c)
                     (#\=
                      (let* ((constant (third c))
                             (ct (chunk-spec-chunk-type constant))
                             (var-spec-list (fourth c))
                             (var2-spec-list (eighth c))
                             (others-spec-list (sixth c))
                             (slot-vars (seventh c)))
                        
                        (setf slot-variables (append slot-vars slot-variables))
                        
                        (when (and (not dynamicp) slot-vars)
                          (print-warning "Slot name variables not allowed in non-dynamic productions.")
                          (print-warning "No production defined for ~s." (production-text production))
                          (return-from create-production nil))
                        
                        (if (searchable-buffer buffer)
                            (let ((search-specs (list (make-cr-condition :type 'isa :buffer buffer :bi bi :value ct))))
                              
                              ;;; guarantee a search buffer is only used as a single condition
                              
                              (if (find buffer searched-buffers)
                                  (progn
                                    (print-warning "Search buffer ~s is specified as more than one condition." buffer)
                                    (print-warning "No production defined for ~s." (production-text production))
                                    (return-from create-production nil))
                                (push buffer searched-buffers))
                                                            
                              ;; don't put anything on the lists for the tree and put all of the 
                              ;; tests into the search spots or save them for building the chunk-spec
                              ;; at the end
                              
                              (push-last (list 'buffer-search buffer) selection)
                              
                              (dolist (spec (chunk-spec-slot-spec constant))
                                ;; just collect the constants
                                (case (car spec)
                                  
                                  (= (push-last (make-cr-condition :type 'slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec))) search-specs))
                                  
                                  (> (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) search-specs)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) search-specs))
                                  
                                  (< (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) search-specs)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) search-specs))
                                  
                                  (>= (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) search-specs)
                                      (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) search-specs))
                                  
                                  (<= (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) search-specs)
                                      (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) search-specs))
                                  
                                  (- (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) search-specs))))
                              
                              
                              (dolist (v var-spec-list)
                                (let ((var (third v))
                                      (binding-slot (second v)))
                                  
                                  ;; all slots with a variable should let the search know it must be full whether or not this is the binding
                                  
                                  (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot binding-slot :si (get-slot-index ct binding-slot) :result nil) search-specs)
                                  
                                  (when (find var (production-bindings production))
                                    (setf (production-bindings production)
                                      (remove var (production-bindings production)))
                                    
                                    (setf others-spec-list (remove v others-spec-list :test 'equal))
                                    
                                    ;;; create the binding condition for it to set the value
                                    (push-last (make-cr-condition :type 'bind-slot :buffer buffer :bi bi :slot binding-slot :si (get-slot-index ct binding-slot) :value var) search-bind)
                                    )))
                              
                              
                              (dolist (v var2-spec-list)  ;; for a search buffer this is essentially the same as the 
                                ;; regular bindings since indirection is only through previously bound slots  
                                (when (chunk-spec-variable-p (third v))
                                  (let ((var (third v))  ;; binding the variable from the search
                                        (binding-slot (second v)))
                                    
                                    (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second v) :result nil) search-specs)
                                    
                                    (when (find var (production-bindings production))
                                      (setf (production-bindings production)
                                        (remove var (production-bindings production)))
                                      
                                      (setf others-spec-list (remove v others-spec-list :test 'equal))
                                      
                                      ;;; create the binding condition for it to set the value
                                      (push-last (make-cr-condition :type 'bind-var-slot :buffer buffer :bi bi :slot binding-slot :value var) search-bind)
                                      ))))
                              
                              (dolist (spec others-spec-list) ;; slots that contained variables which aren't bindings
                                (let* ((type (if (chunk-spec-variable-p (second spec)) 'test-var-slot 'test-slot))
                                       (condition 
                                        (case (car spec) 
                                          (=  (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe-chunk-slot-equal :result t))
                                          (>  (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe> :result t))
                                          (<  (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe< :result t))
                                          (>= (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe>= :result t))
                                          (<= (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe<= :result t))                                          
                                          (-  (make-cr-condition :type type :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (when (eq type 'test-slot) (get-slot-index ct (second spec))) :test 'safe-chunk-slot-equal :result nil)))))
                                  
                                  ;; If all the variables are bound outside of searches then it goes into the
                                  ;; main search spec.  Otherwise it goes on the others list.
                                  ;; Either the slot, value, or both is a variable otherwise it'd be in the constants.
                                  
                                  (if (or 
                                       ;; only the value is a variable
                                       (and (eq type 'test-slot) (not (eq 'search (gethash (third spec) var-table))) (not (eq 'search-slot (gethash (third spec) var-table))))
                                       ;; only the slot is a variable 
                                       (and (eq type 'test-var-slot) (not (chunk-spec-variable-p (third spec))) (not (eq 'search (gethash (second spec) var-table))) (not (eq 'search-slot (gethash (second spec) var-table))))
                                       ;; both variables
                                       (and (eq type 'test-var-slot) (chunk-spec-variable-p (third spec)) (not (eq 'search (gethash (second spec) var-table))) (not (eq 'search-slot (gethash (second spec) var-table)))
                                            (not (eq 'search (gethash (third spec) var-table))) (not (eq 'search-slot (gethash (third spec) var-table)))))
                                      (push-last condition search-specs)
                                    (push-last condition search-other))))
                              
                              (push (make-cr-condition :type 'search :buffer buffer :bi bi :value search-specs :test (intern (concatenate 'string "=" (symbol-name buffer)))) search))
                          
                          (progn ;; The standard production parsing from before
                            
                            
                            (push-last (make-cr-condition :type 'isa :buffer buffer :bi bi :value ct) constants)
                            
                            (dolist (type (cdr (chunk-type-supertypes-fct ct)))
                              (push (make-cr-condition :type 'isa :buffer buffer :bi bi :value type) implicit))
                            
                            (push-last (list 'buffer-read buffer) selection)
                            
                            
                            (dolist (spec (chunk-spec-slot-spec constant))
                              
                              (case (car spec)
                                
                                (= 
                                 ;; add the slot value for the "wide" test
                                 (push-last (make-cr-condition :type 'slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec))) constants)
                                 ;; note that the slot should be full or empty for the implicit tests if needed
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result (if (third spec) nil t)) implicit)
                                 ;; implicit test for numbers to allow better filtering when any of the relative tests used
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result (if (numberp (third spec)) t nil)) implicit))
                                
                                (> 
                                 ;; add the specific test
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) constants)
                                 ;; Explicitly this must be a number
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                 ;; implicitly that means it's not <=
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result nil) implicit)
                                 ;; note that the slot should be full for the implicit tests if needed
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                
                                (< ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) nil) constants))
                                 ;; add the specific test
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) constants)
                                 ;; Explicitly this must be a number
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                 ;; implicitly that means it's not >=
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result nil) implicit)
                                 ;; note that the slot should be full for the implicit tests if needed
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                
                                (>= ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) t) constants))
                                 ;; add the specific test
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) constants)
                                 ;; Explicitly this must be a number
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                 ;; implicitly that means it's not <
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result nil) implicit)
                                 ;; note that the slot should be full for the implicit tests if needed
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                
                                (<= ;(push (list 'slot buffer bi (second spec)  #'safe> (third spec) nil) constants))
                                 ;; add the specific test
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) constants)
                                 ;; Explicitly this must be a number
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                 ;; implicitly that means it's not >
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result nil) implicit)
                                 ;; note that the slot should be full for the implicit tests if needed
                                 (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                
                                
                                (- 
                                 ;; negation test is on the others
                                 (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) others)
                                 ;; if it's '- <slot> nil' then that's got an implicit test the slot must be full
                                 (when (null (third spec))
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                 )))
                            
                            
                            (dolist (v var-spec-list)
                              (let ((var (third v)))
                                (when (find var (production-bindings production))
                                  (setf (production-bindings production)
                                    (remove var (production-bindings production)))
                                  
                                  (setf others-spec-list (remove v others-spec-list :test 'equal))
                                  
                                  (push var safe-bindings)
                                  
                                  (let ((binding-slot (second v)))
                                    (push-last (make-cr-condition :type 'bind-slot :buffer buffer :bi bi :slot binding-slot :si (get-slot-index ct binding-slot) :value var) vars)
                                    
                                    ;; if there isn't already an explicit test for it not being nil 
                                    ;; add one 
                                    (unless (find (list buffer binding-slot) constants :test #'equal 
                                                  :key (lambda (x) 
                                                         (when (or (and (eq (cr-condition-type x) 'slot) ;; slot with a non-nil value
                                                                        (cr-condition-value x))
                                                                   (and (eq (cr-condition-type x) 'test-slot) ;; test-slot with either inequality test or not equal nil
                                                                        (or (not (eq (cr-condition-test x) 'safe-chunk-slot-equal))
                                                                            (and (null (cr-condition-value x)) (null (cr-condition-result x)))
                                                                            (and (cr-condition-value x) (cr-condition-result x)))))
                                                           
                                                           (list (cr-condition-buffer x) (cr-condition-slot x)))))
                                      
                                      (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot binding-slot :si (get-slot-index ct binding-slot) :result nil) constants))))))
                            
                            
                            (dolist (v var2-spec-list)
                              (when (chunk-spec-variable-p (third v))
                                (let ((var (third v)))
                                  (when (find var (production-bindings production))
                                    (setf (production-bindings production)
                                      (remove var (production-bindings production)))
                                    
                                    (setf others-spec-list (remove v others-spec-list :test 'equal))
                                    
                                    (let ((binding-slot (second v)))
                                      
                                      (push (make-cr-condition :type 'bind-var-slot :buffer buffer :bi bi :slot binding-slot :value var) var2s))))))
                            
                            (dolist (spec others-spec-list) ;; slots that contained variables
                              (if (chunk-spec-variable-p (second spec)) 
                                  (case (car spec)  ;; nothing implicit is known nor is a slot index available
                                    
                                    ;; If the value is a variable check to see if it is a search variable for
                                    ;; the non = tests (the = test couldn't be because it would be a var-slot instead)
                                    
                                    (= (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe-chunk-slot-equal :result t) others))
                                    (> (if (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                           (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe> :result t) search-other)
                                         (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe> :result t) others)))
                                    (< (if (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                           (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe< :result t) search-other)
                                         (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe< :result t) others)))
                                    (>= (if (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                            (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe>= :result t) search-other)
                                          (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe>= :result t) others)))
                                    (<= (if (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                            (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe<= :result t) search-other)
                                          (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe<= :result t) others)))
                                    (- (if (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                           (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :result nil) search-other)
                                         (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :result nil) others))))
                                
                                
                                (case (car spec) ; treat it just like a normal
                                  
                                  ;; Need to test the slot value because it could be a search bound variable
                                  ;; in which case the test must go onto the search-other list instead.
                                  ;; that isn't necessary for the = test here either since that would be
                                  ;; a fixed-slot binding instead.
                                  
                                  ;; The value must be a variable in this case otherwise it would be a constant test
                                  
                                  (and (chunk-spec-variable-p (third spec)) (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table))))
                                  
                                  (= 
                                   ;; add the slot value for the "wide" test
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe-chunk-slot-equal :result t) others)
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (> 
                                   ;; add the specific test
                                   (if (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table)))
                                       (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) search-other)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) others))
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (< ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) nil) constants))
                                   ;; add the specific test
                                   (if (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table)))
                                       (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) search-other)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) others))
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (>= ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) t) constants))
                                   ;; add the specific test
                                   (if (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table)))
                                       (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) search-other)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) others))
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (<= ;(push (list 'slot buffer bi (second spec)  #'safe> (third spec) nil) constants))
                                   ;; add the specific test
                                   (if (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table)))
                                       (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) search-other)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) others))
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (- 
                                   ;; negation test with a variable gives no implicit info
                                   (if (or (eq 'search (gethash (third spec) var-table)) (eq 'search-slot (gethash (third spec) var-table)))
                                       (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) search-other)
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'safe-chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) others))))))))))
                     (#\?
                      (let ((constant-queries (third c))
                            (variable-queries (fourth c)))
                        
                        ;(pprint constant-queries)
                        ;(pprint variable-queries)
                        (dolist (q constant-queries)
                          (push-last (make-cr-condition :type 'query :buffer buffer :slot (second q) :value (third q) :result (if (eq '= (first q)) t nil)) constants))
                        
                        (dolist (q variable-queries)
                          (push-last (make-cr-condition :type 'query :buffer buffer :slot (second q) :value (third q) :result (if (eq '= (first q)) t nil)) search-other))
                        
                        (push-last
                         (list 'query-buffer buffer                             
                               (mapcar #'(lambda (x)
                                           (cons (second x) (third x)))
                                 (append constant-queries variable-queries)))
                         selection)))
                     (t
                      (case (cdar c)
                        ((eval safe-eval)
                         (push-last (make-cr-condition :type 'eval :value (car (second c))) search-other)
                         )
                        
                        
                        ((bind safe-bind)
                         
                         (unless (find (car (second c)) (production-bindings production))
                           (print-warning "Cannot have two explicit bindings for variable ~S" (car (second c)))
                           (print-warning "No production defined for ~s." (production-text production))
                           (return-from create-production nil))
                         
                         (setf (production-bindings production)
                           (remove (car (second c))
                                   (production-bindings production)))
                         (push (cons
                                (cons (car (second c)) (find-variables (second (second c))))
                                (make-cr-condition :type 'bind :value (car (second c)) :result (second (second c))))
                               binds))
                        ((mv-bind)
                         
                         (let ((bind-vars (car (second c))))
                           (dolist (x bind-vars)
                             (unless (find x (production-bindings production))
                               (print-warning "Cannot have two explicit bindings for variable ~S" x)
                               (print-warning "No production defined for ~s." (production-text production))
                               
                               (return-from create-production nil)))
                           
                           (dolist (x bind-vars)
                             (setf (production-bindings production)
                               (remove x (production-bindings production))))
                           
                           (push (cons
                                  (cons bind-vars (find-variables (second (second c))))
                                  (make-cr-condition :type 'mv-bind :value bind-vars :result (second (second c))))
                                 binds))))))))
               
               
               ;;; Make sure everything necessary is bound
               
               ;(format t "Variables: ~s bindings: ~s~%" lhs-variables (production-bindings production))
               
               (awhen (some (lambda (x) (find x (production-bindings production))) lhs-variables)
                      (print-warning "No production defined for ~s because ~s is not bound on the LHS." (production-text production) it)
                      (return-from create-production nil))
               
               
               ;;; Make sure that none of the slot name variables are bound in an unsafe manner
               
               (awhen (find-if-not (lambda (x) (find x safe-bindings)) slot-variables)
                      (print-warning "No production defined for ~s because slot-name variable ~s is not bound in a constant named slot i.e. there is more than one level of indirection." (production-text production) it)
                      (return-from create-production nil))
               
               
               ;;; Check the explicit bindings for circularities
               
               (when (circular-references binds)
                 (print-warning "No production defined for ~s because there are circular references in the explicit LHS !bind!'s." (production-text production))
                 (return-from create-production nil))
               
               
               (setf binds (sort-for-bind-order (reverse binds)))
               
               ;; split the binds if any need search vars...
               
               (let ((split (position-if (lambda (x) 
                                           (some (lambda (y)
                                                   (or (eq (gethash y var-table) 'search)
                                                       (eq (gethash y var-table) 'search-slot)))
                                                 (cdr x))) binds :key #'car)))
                 
                 (if split
                     (progn
                       
                       (setf search-bind (append search-bind (mapcar #'cdr (subseq binds split))))
                       (setf binds (mapcar #'cdr (subseq binds 0 split))))
                   (setf binds (mapcar #'cdr binds))))
               
               
               ;;; build the whole conditions list
               
               
               (setf (production-constants production) constants)
               (setf (production-binds production) (append vars var2s binds))
               (setf (production-others production) others)
               (setf (production-searches production) search)
               (setf (production-search-binds production) search-bind)
               (setf (production-search-others production) search-other)
               
               (setf (production-selection-code production) selection)
               (setf (production-implicit production) implicit)
               ))
           
           ;(pprint (production-conditions production))
           
           
           
           (let ((rhs-binds nil)
                 )
             (dolist (action rhs)
               (let ((a action))
               
               
               (case (caar a)
                 (#\=
                  (cond ((null (second a))
                         ;; That's a dummy action to keep the
                         ;; buffer alive
                         )
                        ((= (length (second a)) 1)
                         ;; an overwrite
                         
                         (push-last 
                          (list #'(lambda () 
                                    (schedule-overwrite-buffer-chunk (cdar a)
                                                                     (replace-variables (first (second a))
                                                                                        (production-bindings production))
                                                                     0
                                                                     :module 'procedural
                                                                     :priority 90
                                                                     :output (procedural-rhst prod)))
                                a)
                          (production-actions production)))
                        (t
                         
                         ;; make sure non-dynamic production doesn't have dynamic slots
                         
                         (unless dynamicp
                           (do ((data (second a) (cddr data)))
                               ((null data))
                         
                             (when (chunk-spec-variable-p (car data))
                               (print-warning "Slot name variables not allowed in buffer modification actions of non-dynamic productions.")
                               (print-warning "No production defined for ~s." (production-text production))
                               (return-from create-production nil))))
                         
                         ;; a true buffer modification
                         (push-last 
                          (list #'(lambda () 
                                    (let ((expansion (replace-variables (second a) (production-bindings production)))
                                          (buffer (cdar a)))
                                      (when dynamicp 
                                        (extend-buffer-chunk buffer expansion))
                                      (schedule-mod-buffer-chunk buffer
                                                                 expansion
                                                                 0
                                                                 :module 'procedural
                                                                 :priority 100
                                                                 :output (procedural-rhst prod))))
                                a)
                          (production-actions production)))))
                 
                 (#\-
                  (push-last 
                   (list #'(lambda () 
                             (schedule-clear-buffer (cdar a)
                                                    0
                                                    :module 'procedural
                                                    :priority 10
                                                    :output (when (procedural-rhst prod) 'medium)))
                         a)
                   (production-actions production)))
                 
                 (#\+
                  (cond ((eq (car (second a)) 'isa)
                         ;; a full request
                         
                         ;; make sure non-dynamic production doesn't have dynamic slots
                         
                         (unless dynamicp
                           
                             (when (some 'chunk-spec-variable-p (chunk-spec-slots (define-variable-chunk-spec-fct (second a))))
                               (print-warning "Slot name variables not allowed in buffer request actions of non-dynamic productions.")
                               (print-warning "No production defined for ~s." (production-text production))
                               (return-from create-production nil)))
                         

                         (push-last 
                          (list #'(lambda () 
                                    (schedule-module-request (cdar a)
                                                             (define-chunk-spec-fct 
                                                                 (replace-variables (second a) (production-bindings production)))
                                                             0
                                                             :module 'procedural
                                                             :priority 50
                                                             :output (procedural-rhst prod)))
                                a)
                          (production-actions production)))
                        ((= (length (second a)) 1)
                         ;; a direct request
                         
                         (push-last 
                          (list #'(lambda () 
                                    
                                    (schedule-event-relative 0 #'(lambda () (schedule-module-request (cdar a)
                                                                                                     (chunk-name-to-chunk-spec 
                                                                                                      (car (replace-variables (second a)
                                                                                                                              (production-bindings production))))
                                                                                                     0
                                                                                                     :module 'procedural
                                                                                                     :priority 50
                                                                                                     :output (procedural-rhst prod)))
                                                             :module 'procedural
                                                             :priority 99
                                                             :output nil))
                                a)
                          (production-actions production)))
                        
                        (t
                         ;; a buffer modification request
                         (unless dynamicp
                           (do ((data (second a) (cddr data)))
                               ((null data))
                         
                             (when (chunk-spec-variable-p (car data))
                               (print-warning "Slot name variables not allowed in buffer request actions of non-dynamic productions.")
                               (print-warning "No production defined for ~s." (production-text production))
                               (return-from create-production nil))))
                         
                         (push-last 
                          (list #'(lambda () 
                                    (schedule-module-mod-request (cdar a)
                                                                 (replace-variables (second a)
                                                                                    (production-bindings production))
                                                                 0
                                                                 :verify (not dynamicp)
                                                                 :module 'procedural
                                                                 :priority 50
                                                                 :output (procedural-rhst prod)))
                                a)
                          (production-actions production)))))
                 
                 (t
                  (case (cdar a)
                    ((eval safe-eval)
                     (push-last (cons
                            (cons nil (find-variables (second a)))
                            (list #'(lambda ()
                                      (eval (replace-variables-for-eval (car (second a))
                                                                        (production-bindings production))))
                                  a))
                            rhs-binds)
                     )
                    
                    ((bind safe-bind)
                     (setf (production-bindings production)
                       (remove (car (second a)) (production-bindings production)))
                     (push-last 
                      
                      (cons
                       (cons (car (second a)) (find-variables (second (second a))))
                       (list #'(lambda ()
                                  (bind-variable (car (second a))
                                                 (eval (replace-variables-for-eval (second (second a))
                                                                                   (production-bindings production)))
                                                 production))
                             a))
                      rhs-binds)
                     
                      
                     )
                    
                    ((mv-bind)
                     (let ((bind-vars (car (second a))))
                       (dolist (x bind-vars)
                         (unless (find x (production-bindings production))
                           (print-warning "Cannot have two explicit bindings for variable ~S" x)
                           (print-warning "No production defined for ~s." (production-text production))
                           
                           (return-from create-production nil)))
                       
                       (dolist (x bind-vars)
                         (setf (production-bindings production)
                           (remove x (production-bindings production))))
                       
                       (push-last 
                        (cons
                         (cons (car (second a)) (find-variables (second (second a))))
                         (list 
                          #'(lambda ()
                              (let ((vals (multiple-value-list 
                                           (eval (replace-variables-for-eval (second (second a)) (production-bindings production))))))
                                (dolist (x bind-vars)
                                  (bind-variable x (if vals (pop vals) nil) production))))
                          a))
                        rhs-binds))
                     
                     )
                    
                    (output
                     (push-last (cons
                            (cons nil (find-variables (second a)))
                            (list #'(lambda ()
                                          (print-production-output 
                                           (second a)
                                           (replace-variables (second a)
                                                              (production-bindings production))))
                                      a))
                           rhs-binds)
                     )
                    (stop
                     (push-last (list #'(lambda ()
                                          (schedule-break-relative 0 :priority :min
                                                                   :details "Stopped by !stop!"))
                                      a)
                                (production-actions production))))))))
           
           
           
           ;;; verify that all variables are bound in the actions
           
             (dolist (action rhs)
               (awhen (some #'(lambda (x)
                                (find x (production-bindings production)))
                            (find-variables  (if (or (equal (car action) (cons #\! 'bind))
                                                     (equal (car action) (cons #\! 'safe-bind))
                                                     (equal (car action) (cons #\! 'mv-bind)))
                                                 (progn  (cdr (second action)))
                                               (progn  (second action)))))
                      
                      (print-warning "Unbound variable ~s on RHS in ~s." it (second action))
                      (print-warning "No production defined for ~s." (production-text production))
                      
                      (return-from create-production nil)))
             
           
             ;;; check rhs bindings 
             
             
             ;;; Check the explicit bindings for circularities
             
             (when (circular-references rhs-binds)
               (print-warning "No production defined for ~s because there are circular references in the explicit RHS !bind!'s." (production-text production))
               (return-from create-production nil))
             
             (let ((original-binds (copy-list rhs-binds)))
               
               (setf rhs-binds (sort-for-bind-order rhs-binds))
               (unless (equalp rhs-binds original-binds)
                 
                 (model-warning "RHS !bind!, !eval!, and/or !output! actions of production ~s had to be reordered because of interdependencies of variables.~% If those actions have side effects check the actions carefully to ensure proper operation." (production-name production))))
             
             ;; make sure the bindings happen first
             
             (setf (production-actions production) (append (mapcar 'cdr rhs-binds) (production-actions production)))
             )
           
           ;;; Add the implicit clears
           
           (dolist (y lhs)
             (let ((x y))
               (when (eql #\= (caar x))
                 (unless (or (find (cdar x) (procedural-unharvested-buffers prod))
                             (find (cdar x) rhs :key #'cdar))
                   
                   (push-last 
                    (list #'(lambda () 
                              (schedule-clear-buffer (cdar x)
                                                     0
                                                     :module 'procedural
                                                     :priority 10
                                                     :output (when (procedural-rhst prod) 'medium)))
                          (list 'implicitly 'clear (cdar x)))
                    (production-actions production))))))
           
           (dolist (y rhs)
             (let ((x y))
               (when (and (eql #\+ (caar x)) 
                          (or
                           (eq 'isa (car (second x)))
                           (= (length (second x)) 1))
                          (not (find (cons #\- (cdar x)) rhs :key #'car :test #'equal)))
                 
                 (push-last 
                  (list #'(lambda () 
                            (schedule-clear-buffer (cdar x)
                                                   0
                                                   :module 'procedural
                                                   :priority 10
                                                   :output (when (procedural-rhst prod) 'medium)))
                        (list 'clear 'on 'request (cdar x)))
                  (production-actions production))))) 
           
           ;;; Parse LHS for unknown chunks
           
           (dolist (x lhs)
             (cond ((eql #\= (caar x))
                    (dolist (slot (fifth x))
                      (unless (or (chunk-spec-variable-p (third slot))
                                  (chunk-p-fct (third slot))
                                  (stringp (third slot))
                                  (listp (third slot))
                                  (numberp (third slot))
                                  (eq t (third slot)))
                        (create-undefined-chunk (third slot)))))
                   ((eql #\? (caar x))
                    (dolist (slot (third x))
                      (unless (or (chunk-spec-variable-p (third slot))
                                  (chunk-p-fct (third slot))
                                  (numberp (third slot))
                                  (stringp (third slot))
                                  (listp (third slot))
                                  (numberp (third slot))
                                  (eq t (third slot)))
                        (create-undefined-chunk (third slot)))))))
           
           ;;; Parse RHS for unknown chunks
           ;;; Only in = modifications and direct requests though
           ;;; anything in a full + is up to the module to
           ;;; handle i.e. this is where I see something
           ;;; like retrieval variables being not an =
           ;;; working in about 2 minutes...
           
           (dolist (x rhs)
             (cond ((eql #\= (caar x))
                    (if (= (length (second x)) 1)
                        (cond ((or (chunk-spec-variable-p (car (second x)))
                                   (chunk-p-fct (car (second x))))
                               ;;; nothing because that's safe
                               )
                              ((symbolp (car (second x)))
                               (create-undefined-chunk (car (second x))))
                              (t
                               (print-warning "No production defined for ~s because ~s is not a variable or chunk name in a buffer overwrite action." (production-text production) (car (second x)))
                               (return-from create-production nil)))
                      (dolist (val (mapcar #'cdr (query-list-to-conses (second x)))) ;; I know it's a slot-value pair list
                        
                        (unless (or (chunk-spec-variable-p val)
                                    (chunk-p-fct val)
                                    (numberp val)
                                    (stringp val)
                                    (listp val)
                                    (numberp val)
                                    (eq t val))
                          (create-undefined-chunk val)))))
                   ((eql #\+ (caar x))
                    (when (= (length (second x)) 1) ;; don't create chunks in full requests
                      (cond ((or (chunk-spec-variable-p (car (second x)))
                                 (chunk-p-fct (car (second x))))
                             ;;; nothing because that's safe
                             )
                            ((symbolp (car (second x)))
                             (create-undefined-chunk (car (second x))))
                            (t
                             (print-warning "No production defined for ~s because ~s is not a variable or chunk name in a direct request action." (production-text production) (car (second x)))
                             (return-from create-production nil)))))))
           
           ;; Replace the special case for vision and
           ;; use the warning mechanism that's available now
           
           (dolist (x rhs)
             (let ((y x))
               (when (and (eql #\+ (caar x))
                          (or (= (length (second x)) 1) ;; a direct request
                              (eq (car (second x)) 'isa)) ;; a full request
                          (require-module-warning? (cdar x)))
                 (if (= (length (second x)) 1)
                     
                     (push #'(lambda ()
                               (module-warning (cdar y) (chunk-chunk-type-fct (car (replace-variables (second y) (production-bindings production))))))
                           (production-conflict-code production))
                   (push #'(lambda ()
                             (module-warning (cdar y) (second (second y))))
                         (production-conflict-code production))))))
           
           ;(pprint (production-actions production))
           
           (awhen (get-production-internal (production-name production) prod)
                  (print-warning "Production ~S already exists and it is being redefined." (production-name production))
                  (remove-production it prod))
           
           (add-production production prod) 
           
           ;; If a new production is created and conflict resolution 
           ;; is waiting put it back on the queue
           
           (un-delay-conflict-resolution)
           
           ;; record which buffers are used for searches 
           
           (dolist (x searched-buffers)
             (pushnew x (procedural-used-search-buffers prod)))
           
           
           (production-name production))
         (progn
           (print-warning "Production is missing the ==> separator.")
           (print-warning "No production defined for ~s." (production-text production))))))




(defun parse-conditions (procedural definition)
  (aif (gethash definition (procedural-condition-parse-table procedural))
       it
  (let ((segments (segment-production definition)))
    (if (eq segments :error)
        (progn
          (print-warning "First item on LHS is not a valid command")
          :error)
      (do* ((segs segments (cdr segs))
            (seg (car segs) (car segs))
            (cmd (parse-command (car seg) :operators '(#\? #\=)
                                :commands '(eval safe-eval bind safe-bind mv-bind)) 
                 (parse-command (car seg) :operators '(#\? #\=)
                                :commands '(eval safe-eval bind safe-bind mv-bind)))
            (conditions nil))
            
           ((null seg) (setf (gethash definition (procedural-condition-parse-table procedural)) conditions))
               
        (case (car cmd)
          (#\=
           (if (< (length (cdr seg)) 2)
               (progn
                 (print-warning "Missing chunk-type test in condition for ~s buffer." (cdr cmd))
                 (return-from parse-conditions :error))
             (aif (define-variable-chunk-spec-fct (cdr seg))
                  (progn
                    (when (some #'keywordp (chunk-spec-slots it))
                      (print-warning "Request parameters not allowed in buffer tests: ~s" seg)
                      (return-from parse-conditions :error))
                    (let* ((slot-specs (chunk-spec-slot-spec it))
                           (constants)
                           (variables)
                           (var2)
                           (others)
                           (slot-vars))
                      
                      (dolist (spec slot-specs)
                        
                        (when (chunk-spec-variable-p (second spec))
                          (push (second spec) slot-vars))
                        
                        
                        (cond ((and (not (chunk-spec-variable-p (second spec)))
                                    (not (chunk-spec-variable-p (third spec))))
                               (push-last spec constants))
                              ((not (eq '= (first spec)))
                               (push-last spec others))
                              ((chunk-spec-variable-p (second spec))
                               (push-last spec var2)
                               (push-last spec others))
                              ((chunk-spec-variable-p (third spec))
                               (push-last spec variables)
                               (push-last spec others))
                              (t
                               (push-last spec constants))))
                      
                      
                      
                      (push-last (list cmd (cdr seg) 
                                       (define-chunk-spec-fct (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) constants))
                                       variables
                                       slot-specs 
                                       others ;(when others (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) others))
                                       slot-vars
                                       var2)
                                 conditions)))
                  (progn
                    (print-warning "Invalid syntax in ~s condition." (car seg))
                    (return-from parse-conditions :error)))))
          (#\?
           (let ((queries (process-query-specs (cdr cmd) (cdr seg))))
             
             (cond ((eq queries :error)
                    (print-warning "Invalid buffer query ~S." seg)
                    (return-from parse-conditions :error))
                   
                   (t
                    (let ((constant-queries (remove-if #'chunk-spec-variable-p queries :key #'third))
                          (variable-queries (remove-if-not #'chunk-spec-variable-p queries :key #'third)))
                      (push-last (list cmd (cdr seg) constant-queries variable-queries) conditions))))))
          (t
           (case (cdr cmd)
             ((bind safe-bind)
              (if (and (= (length seg) 3)
                       (chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid bind command: ~s" seg)
                  (return-from parse-conditions :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-conditions :error))))
             ((eval safe-eval)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid eval command: ~s" seg)
                  (return-from parse-conditions :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-conditions :error))))))))))

(defun parse-actions (procedural definition conditions dynamicp)
  (aif (gethash (cons definition conditions) (procedural-action-parse-table procedural))
       it
       (let ((segments (segment-production definition)))
    (if (eq segments :error)
        (progn
          (print-warning "First item on RHS is not a valid command")
          :error)
      (do* ((segs segments (cdr segs))
            (seg (car segs) (car segs))
            (cmd (parse-command (car seg) :operators '(#\- #\+  #\=)) 
                 (parse-command (car seg) :operators '(#\- #\+  #\=)))
            (actions nil))
           
           ((null seg) (setf (gethash (cons definition conditions) (procedural-action-parse-table procedural)) actions))
        (when (null cmd)
          (print-warning "Invalid command on RHS: ~S" (car seg))
          (return-from parse-actions :error))
        
        (case (car cmd)
          (#\-
           (if (= (length seg) 1)
               (push-last (list cmd) actions)
             (progn
               (print-warning "Invalid - buffer command: ~s" seg)
               (return-from parse-actions :error))))
          
          (#\=
           (cond ((= (length seg) 1)
                  (push-last (list cmd) actions))
                 ((= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions))
                 (t
                  (let* ((lhs-binding (find (cons #\= (cdr cmd)) conditions
                                           :test #'equal :key #'car))
                         (chunk-type (if lhs-binding
                                         (chunk-spec-chunk-type (third lhs-binding))
                                       nil)))
                    (if chunk-type
                        (if (or (and (not dynamicp) (valid-chunk-mod-spec chunk-type (cdr seg)))
                                (and dynamicp (valid-variable-chunk-mod-spec chunk-type (cdr seg))))
                            (push-last (list cmd (cdr seg)) actions)
                          (progn
                            (print-warning "Invalid buffer modification ~s." seg)
                            (return-from parse-actions :error)))
                      (progn
                        (print-warning "Cannot modify buffer ~s if not matched on LHS." (cdr cmd))
                        (return-from parse-actions :error)))))))
                    
           (#\+
           (cond ((= (length seg) 1)
                  (print-warning "Buffer request ~s requires some parameters." (car seg))
                  (return-from parse-actions :error))
                 ((= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions))
                 (t
                  (if (eq (second seg) 'isa)
                      ;; This is a full request
                      (progn
                        (aif (define-variable-chunk-spec-fct (cdr seg))
                             (progn
                               (unless (every (lambda (x)
                                                ; check for the variables
                                                (or (and dynamicp (chunk-spec-variable-p x))
                                                    (valid-buffer-request-slot 
                                                     (cdr cmd) 
                                                     (chunk-spec-chunk-type it) x)))
                                                (chunk-spec-slots it))
                                 
                                 (print-warning  "Invalid slot value in request: ~s" seg)
                                 (return-from parse-actions :error))
                  
                               (push-last (list cmd (cdr seg)) actions))
                             (progn
                               (print-warning "Invalid syntax in action ~s." seg)
                               (return-from parse-actions :error))))
                    ;; otherwise it's assumed to be a modification request
                    (progn
                      (let* ((lhs-binding (find (cons #\= (cdr cmd)) conditions
                                                :test #'equal :key #'car))
                             (chunk-type (if lhs-binding
                                             (chunk-spec-chunk-type (third lhs-binding))
                                           nil)))
                        (if chunk-type
                            (if (or (and (not dynamicp) (valid-chunk-mod-spec chunk-type (cdr seg)))
                                    (and dynamicp (valid-variable-chunk-mod-spec chunk-type (cdr seg))))
                                (push-last (list cmd (cdr seg)) actions)
                              (progn
                                (print-warning "Invalid buffer modification ~s." seg)
                                (return-from parse-actions :error)))
                          (progn
                            (print-warning "Cannot modify buffer ~s if not matched on LHS." 
                             (cdr cmd))
                            (return-from parse-actions :error)))))))))
          
          (t
           (case (cdr cmd)
             ((bind safe-bind)
              (if (and (= (length seg) 3)
                       (chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid bind command: ~s" seg)
                  (return-from parse-actions :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-actions :error))))
             ((eval safe-eval output)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid ~s command: ~s" (cdr cmd) seg)
                  (return-from parse-actions :error))))
             (stop
              (if (= (length seg) 1)
                  (push-last (list cmd) actions)
                (progn
                  (print-warning "Invalid stop command: ~s" seg)
                  (return-from parse-actions :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-actions :error))))))))))


(defun sort-for-binding (condition-list)
  (stable-sort (copy-tree condition-list) (lambda (x y) 
                                            (or (and (eq (car x) #\!) (not (eq (car y) #\!)))  ;; eval and binds first
                                                (and (eq (car x) #\=) (not (searchable-buffer (cdr x)))   ;; non-search buffers 
                                                     (eq (car y) #\=) (searchable-buffer (cdr y)))))      ;; before search buffers
                                            :key #'car))

(defun circular-references (bindings)
  (let ((checks (mapcar #'car bindings)))
    
    ;;; Actually modify the dependencies info so that bind-order can use it...

    (dolist (x checks)
      (dolist (y (cdr x))
        (awhen (find y checks :test (lambda (i j) (if (listp (car j))
                                                      (find i (car j))
                                                    (eq i (car j)))))
                                                             
               (setf (cdr x) (append (cdr x) (cdr it))))))
    
    (some (lambda (x) 
            (if (listp (car x))
                (some (lambda (z) (find z (cdr x))) (car x))
              (find (car x) (cdr x)))) checks)))


(defun number-test (val ignore)
  (declare (ignore ignore))
  (if (numberp val)
      t
    nil))


#| doesn't always work right
(defun bind-order (x y)
  (if (and (null (cdr x)) (cdr y))
      t
    (if (listp (car x))
        (some (lambda (z) (find z (cdr y))) (car x))
      (find (car x) (cdr y)))))
|#

(defun sort-for-bind-order (ordering)
  (let ((result nil))
    (dolist (x ordering result)
      (aif (position-if (lambda (y) (find (caar x) (cdar y))) result)
           (setf result (splice-into-position-des result it x))
           (push-last x result)))))

(defun process-query-specs (buffer specs)
  (let ((slots nil))
    (loop 
      (when (null specs) (return slots))
      (let ((spec nil))
        (if (member (car specs) '(= -)) ;; for now only allow = and - queries
            (push (pop specs) spec)
          (push '= spec))
        (when (null specs) (return :error))
        (unless (valid-buffer-query buffer (car specs)) 
          (return :error))
        (push-last (pop specs) spec)
        (when (null specs) (return :error))
        (push-last (pop specs) spec)
        (push spec slots)))))


(defun print-production-output (original data)
  (let ((vals (car data)))
        (cond ((atom vals)
         (model-output "~s" vals))
        ((stringp (caar original))
         (model-output "~?" (car vals) (cdr vals)))
        (t
         (model-output "~{~S ~}" vals)))))

(defun find-variables (arg)
  (cond ((listp arg) (remove-duplicates (mapcan 'find-variables arg)))
        ((chunk-spec-variable-p arg) (list arg))
        (t nil)))

(defun replace-variables (arg bindings)
  (cond ((and (consp arg) (eq (last arg) arg))  ;; detect that it's something like (a . b)
         (cons (replace-variables (car arg) bindings)
               (replace-variables (cdr arg) bindings)))
         
         ((listp arg) 
         (mapcar #'(lambda (x)
                     (replace-variables x bindings))
           arg))
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (cdr it)
              arg))
        (t arg)))



(defun replace-variables-for-eval (arg bindings)
  (cond ((listp arg) 
               
         (cond ((listp (car arg))
                ;; If the head is a list just parse the whole thing
                (mapcar #'(lambda (x)
                            (replace-variables-for-eval x bindings))
                  arg))
               ((eq (car arg) 'quote)
               ;; If it's already quoted don't requote
                (mapcar #'(lambda (x)
                            (replace-variables x bindings))
                  arg))
               (t
                (cons (replace-variables (car arg) bindings)
                      (mapcar #'(lambda (x)
                                  (replace-variables-for-eval x bindings))
                        (cdr arg))))))
               
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (if (or (chunk-spec-variable-p (cdr it))
                      (stringp (cdr it)))
                  (cdr it)
                (list 'quote (cdr it)))
              arg))
        (t arg)))



(defun segment-production (definition)
  (when definition
    (do ((res nil (push-last where res))
         (where (position-if 'parse-command definition )
                (position-if 'parse-command definition :start (1+ where))))
        ((null where)
         (if (and res (zerop (car res)))
             (mapcar #'(lambda (start end)
                         (subseq definition start end))
               res (push-last nil (cdr res)))
           :error)))))

(defun parse-command (cmd &key (operators '(#\? #\= #\- #\+))
                          (commands '(eval safe-eval bind safe-bind mv-bind stop output)))
  (when (symbolp cmd)
    (let* ((name (symbol-name cmd))
           (len (length name))
           (first-char (aref name 0))
           (last-char (aref name (1- len))))
      (when (> len 2)
        (let ((cmd-name (intern (subseq name 1 (1- len)))))
          (cond ((and (eql first-char #\!) (eql last-char #\!)
                      (find cmd-name commands))
                 (cons #\! cmd-name))
                ((and (eql last-char #\>)
                      (find cmd-name (buffers))
                      (find first-char operators))
                 
                 (cons first-char cmd-name))
                
                ;; Don't worry about warnings here - just parse it
                (t nil)))))))
  
(defun query-list-to-conses (queries)
  (do* ((s (copy-tree queries))
        (slot (pop s) (pop s))
        (value (pop s) (pop s))
        (tests (list (cons slot value))
               (push-last (cons slot value) tests)))
       ((null s) tests)))


(defun valid-chunk-mod-spec (chunk-type modifications-list)
  (if (oddp (length modifications-list))
      (print-warning "Odd length modifications list.")
    (do ((slots nil (cons (car s) slots))
         (s modifications-list (cddr s)))
        ((null s) 
         (and (every #'(lambda (slot)
                         (valid-slot-name slot (get-chunk-type chunk-type)))
                     slots)
              (= (length slots) (length (remove-duplicates slots))))))))
    
(defun valid-buffer-query (buffer-name slot)
  (let ((buf (buffer-instance buffer-name)))
    (find slot (act-r-buffer-queries buf)))) 

(defun valid-buffer-request-param (buffer-name slot)
  (let ((buf (buffer-instance buffer-name)))
    (find slot (act-r-buffer-requests buf))))

(defun valid-buffer-request-slot (buffer-name chunk-type slot)
  (if (keywordp slot)
      (valid-buffer-request-param buffer-name slot)
    (valid-slot-name slot (get-chunk-type chunk-type))))

(provide "PRODUCTION-PARSING")

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
