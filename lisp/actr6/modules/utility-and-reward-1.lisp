;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell & John Anderson
;;; Copyright   : (c) 2006 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : utility-and-reward-1.lisp
;;; Version     : 2.2
;;; 
;;; Description : The procedural utility computation functions and a module
;;;             : for handling the "reward" given to a production.
;;;             :
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2006.05.22 Dan
;;;             : * Initial creation.
;;; 2006.08.24 Dan
;;;             : * Added the "null" learning event marker (the way setting
;;;             :   both success and failure worked in the old system) - if
;;;             :   trigger-reward is passed nil it just clears the history.
;;; 2006.09.08 Dan
;;;             : * Changed the :at parameter check from posnum to nonneg.
;;; 2006.10.06 Dan
;;;             : * Changed some print-warning calls to model-warning so that
;;;             :   they turn off with the model-warning parameter.
;;; 2006.11.15 Dan
;;;             : * Changed trigger-reward so that it tests its parameter
;;;             :   before generating the event instead of waiting for the 
;;;             :   propigate-reward function to catch problems.
;;;             : * Changed the name of the module from utility-2 to utility
;;;             :   in preparation for making it the default module.
;;;             : * Changed the doc string for the module and updated the
;;;             :   version to 2.0.
;;; 2007.10.26 Dan [2.1]
;;;             : * Added the Utility threshold parameter back into the system.
;;; 2008.02.26 Dan
;;;             : * Fixed a bug in propigate-reward which allowed utility 
;;;             :   learning to go on even when :esc was set to nil.
;;; 2008.07.22 Dan
;;;             : * Changed compute-utility so that it has an optional parameter
;;;             :   which indicates whether or not to store the computed value.
;;; 2008.07.23 Dan
;;;             : * Added the require of "CENTRAL-PARAMETERS" since it does use
;;;             :   it - don't rely on procedural or declarative to load it first.
;;;             : * Added call to the new register-subsymbolic-parameters to
;;;             :   note which parameters should trigger the warning.
;;; 2008.08.01 Dan
;;;             : * Procedural now owns :dat.
;;; 2009.08.12 Dan
;;;             : * Added a utility-offset hook which is called after the normal
;;;             :   utility for a production is computed and set (without noise).  
;;;             :   If it returns a number that's added into the utility value.
;;; 2011.02.16 Dan [2.2]
;;;             : * Added a utility learning trace parameter which prints out
;;;             :   the utility changes when a reward occurs.
;;;             : * Also changed linear-update-utility to take the utility module
;;;             :   as a parameter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description:
;;;
;;; See the new-utility.doc file for all the details, but the general idea is
;;; that instead of tracking P and C and effectively boolean successes/failures
;;; we collapse things into one value called utility and have the successes and
;;; failures represented numerically as a single reward value.
;;; [Note the variable noise value discussed in the doc is not implemented at this
;;; time.]
;;;
;;; Thus, a production only has a utility value, U, which is adjusted over time based
;;; on the rewards received.  The equation for the learning of production i is now:
;;;
;;; Ui(n) = Ui(n-1) + A[Ri(n) - Ui(n-1)]
;;; 
;;; A := the learning rate set by a parameter (defaults to .2).
;;; Ri(n) := the effective reward value of production i at time n.  Like the old 
;;; mechanim, the learning only occurs when directly triggered and all productions 
;;; that have fired since the last "learning event" are updated.  The effective
;;; reward is the reward value at time n minus the time since the selection of
;;; production i.
;;; Ui(0) := set by a parameter (defaults to 0).
;;;
;;; The new meechanism only needs the following general parameters:
;;;
;;; dat := default action time, same as before - the default time to fire a 
;;;        production.
;;; egs := the expected gain s value, same as before.
;;;
;;; utility-hook := a function that can bypass the utility computation for
;;;                 productions, same as before.
;;;
;;; ul := utility learning, either t or nil to indicate whether or not to 
;;;       apply the equation above.  There is no decaying version as there is with
;;;       the old parameter pl at this time.
;;; alpha := the learning rate for the equation above.  Note this differs from
;;;          the alpha parameter in the old system because that only applied to
;;;          the production compilation mechanism.
;;; iu := the default U(0) value for an initial (user defined) production.
;;; nu := the default U(0) value for a newly learned (production compilation) production.
;;;
;;; In addition, this parameter from the old system is now available again
;;; 
;;; ut := the utility threshold.  Productions with a utility less than this
;;;       value will not be chosen during conflict resolution.
;;;
;;; ult := The utility learing trace parameter.  When a reward occurs
;;;        display the utility changes which occur.
;;;
;;; For a production, only the following parameters are needed for spp:
;;;
;;; at := The action time of the production, how long between the production's
;;;       selection and when it fires.  Can be set directly, defaults to :dat.
;;; name := returns the name of the production.  Cannot be set directly.
;;; u := the current U(n) value for the production.  Can be set directly.
;;; utility := the last computed utility value of the production during 
;;;            conflict resolution.  Cannot be set directly.
;;; reward := a reward value to apply when this production fires if set to a
;;;           non-nil value (the default is nil).  Can be set directly.
;;;
;;;
;;; Note: in the old system there was a parameter called value which
;;; was used to order productions if :esc was nil, but now the u value is used
;;; regardless of the setting of esc.  However learning cannot take palce if esc
;;; is nil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; since this module uses :esc make sure that module is available
(require-compiled "CENTRAL-PARAMETERS" "ACT-R6:support;central-parameters")

;;; The structure that is the utility module

(defstruct utility
  egs esc dat ul ut
  iu nu alpha
  utility-hook 
  history
  offsets
  trace)

;;; Functions necessary to support the production parameters


(defun get-default-action-time (p)
  (declare (ignore p))
  (car (no-output (sgp :dat))))

(defun get-default-u (p)
  (declare (ignore p))
  (car (no-output (sgp :iu))))

;;; The parameters added to productions for utility purposes


(extend-productions utility)
(extend-productions u :default-function get-default-u)
(extend-productions at :default-function get-default-action-time)
(extend-productions reward)

;;; The functions that are required by the procedural system

(defun note-production-selection (production)
  (let ((u (get-module utility)))
    (when (and u (utility-ul u))
      (push-last (cons production (mp-time)) (utility-history u)))))


(defun productions-action-time (production)
  (production-at production))

(defun compute-utility (production &optional (save nil))
  (let ((u (get-module utility)))
    (when u
      (let* ((over-ride (awhen (utility-utility-hook u)
                          (funcall it production)))
             (utility (if (numberp over-ride)
                          over-ride
                        (+ (if (and (utility-esc u) (utility-egs u) (not (zerop (utility-egs u))))
                               (act-r-noise (utility-egs u))
                             0)
                           (production-u production)
                           (if (utility-offsets u)
                               (reduce #'+ (mapcar  (lambda (x) 
                                             (let ((val (funcall x production)))
                                               (if (numberp val)
                                                   val
                                                 0)))
                                             (utility-offsets u)))
                             0)))))
        (if save
            (setf (production-utility production) utility)
          utility)))))


(defun minimum-utility ()
  (let ((u (get-module utility)))
    (when u
      (utility-ut u))))

(defun linear-update-utility (module production reward)
  (let ((old (production-u production))
        (alpha (utility-alpha module)))
    (setf (production-u production) (+ old (* alpha (- reward old))))))

(defun trigger-reward (value)
  (cond ((null (current-model))
         (print-warning "No current model.  Trigger-reward has no effect."))
        ((numornil value)
         (schedule-event-relative 0 'propagate-reward :module 'utility
                                  :priority :max :params (list value)
                                  :output 'medium)
         t)
        (t
         (print-warning "Trigger-reward must be called with a number or nil."))))
                           
(defun propagate-reward (value)  
  (let ((c (if (numberp value) (- value (mp-time)) 0))
        (u (get-module utility)))
    
    (if (numberp value)
        (if (and (utility-esc u) (utility-ul u))
            (progn 
              (dolist (p-t (utility-history u))
                (when (utility-trace u)
                  (model-output "  Updating utility of production ~S" (car p-t))
                  (model-output "    U(n-1) = ~f   R(n) = ~f [reward ~f - ~f seconds since selection] alpha = ~f" (production-u (car p-t))
                                (ms-round (+ c (cdr p-t))) value (ms-round (- (mp-time) (cdr p-t))) (utility-alpha u)))
                (linear-update-utility u (car p-t) (+ c (cdr p-t)))
                (when (utility-trace u)
                  (model-output "    U(n) = ~f" (production-u (car p-t)))))
              (setf (utility-history u) nil))
          (print-warning "Trigger-reward can only be used if utility learning is enabled."))
      (if value
          (print-warning "Trigger-reward must be called with a numeric value or nil not ~S." value)
        (progn
          (when (utility-trace u)
            (model-output "  Null reward clears utility learning history."))
          (setf (utility-history u) nil))))))


(defun learn-parameters (production)
  (awhen (production-reward production)
         (trigger-reward it)))


(defun initialize-utility-for-compiled-production (new-p p1 p2)
  (let* ((at1 (production-at p1))
         (at2 (production-at p2))
         (at (max at1 at2))
         (r1 (production-reward p1))
         (r2 (production-reward p2))
         (reward (or (and (numberp r1)
                          (numberp r2)
                          (max r1 r2))
                     (and (numberp r1) r1)
                     (and (numberp r2) r2))))
    (setf (production-at new-p) at)
    (setf (production-reward new-p) reward)
    (setf (production-u new-p) (car (no-output (sgp :nu))))))


(defun update-utility-for-compiled-production (p3 p1 p2)
  (declare (ignore p2))
  (let ((u (get-module utility)))
    (when (and u (utility-ul u))
      
      (when (utility-trace u)
        (model-output "  Updating utility of production ~S from production compilation" p3)
        (model-output "    U(n-1) = ~f   R(n) = ~f [U(n) for first parent] alpha = ~f" (production-u p3)
                      (production-u p1) (utility-alpha u)))
      
      (linear-update-utility u p3 (production-u p1))
      
      (when (utility-trace u)
        (model-output "    U(n) = ~f" (production-u p3))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here's the actual module and associated support code

(defun utility-module-params (u param)
  (cond ((consp param)
         
         ;; Changing utility parameters may lead to
         ;; to possibly trigger conflict-resolution if it were waiting
         
         (un-delay-conflict-resolution)
         
         (when (and (> (length (all-productions)) 1)
                    (member (car param) '(:esc :dat :ul :iu)))
           (model-warning "Changing procedural parameters when productions exist unsupported.")
           (model-warning "Results may not be what one expects."))
         
         (case (car param)
           (:esc (setf (utility-esc u) (cdr param)))
           
           (:egs (setf (utility-egs u) (cdr param)))
           (:ul (setf (utility-ul u) (cdr param)))
           (:dat (setf (utility-dat u) (cdr param)))
           (:iu (setf (utility-iu u) (cdr param)))
           (:nu (setf (utility-nu u) (cdr param)))
           
           (:alpha (setf (utility-alpha u) (cdr param)))
           (:ult (setf (utility-trace u) (cdr param)))
           
           (:ut (setf (utility-ut u) (cdr param)))
           
           (:utility-hook 
            (when (and (cdr param) (utility-utility-hook u))
              (print-warning 
               "Utility-hook was set to ~S and is being overwritten"
               (utility-utility-hook u)))
            (setf (utility-utility-hook u) (cdr param)))
           
           (:utility-offsets
            (if (cdr param)
              (if (member (cdr param) (utility-offsets u))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :activation-offsets
                 (cdr param))
                (push (cdr param) (utility-offsets u)))
              (setf (utility-offsets u) nil)))
           ))
        (t 
         (case param
           (:egs  (utility-egs u))
           (:ul (utility-ul u))
           
           (:iu (utility-iu u))
           (:nu (utility-nu u))
           
           (:ut (utility-ut u))

           (:alpha (utility-alpha u))
           (:ult (utility-trace u))
           (:utility-hook (utility-utility-hook u))
           (:utility-offsets (utility-offsets u))))))


(defun reset-utility-module (u)
  (setf (utility-history u) nil))


(define-module-fct 'utility nil
  (list (define-parameter :esc :owner nil)
        
        (define-parameter :egs :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Expected Gain S")
        (define-parameter :ul :valid-test #'tornil :default-value nil
          :warning "T or nil" :documentation "Utility learning switch")
        (define-parameter :dat :owner nil)
        (define-parameter :iu :valid-test #'numberp :default-value 0
          :warning "a number" :documentation "default U(0) value for an initial (user defined) production")
        (define-parameter :nu :valid-test #'numberp :default-value 0
          :warning "a number" :documentation "default U(0) value for a newly learned production")
        
        (define-parameter :ut :valid-test #'numornil :default-value nil
          :warning "a number or nil" :documentation "Utility Threshold")

        (define-parameter :alpha :default-value .2
          :valid-test #'numberp :warning "a number"
          :documentation "Production learning rate")
        
        (define-parameter :ult :valid-test #'tornil :default-value nil
          :warning "T or nil" :documentation "Utility learning trace")
        
        (define-parameter :utility-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation hook")
        
        (define-parameter :utility-offsets :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Add additional utility equation components"))
  
  :version "2.2" 
  :documentation  "A module that computes production utilities"
    
  :creation (lambda (x) (declare (ignore x)) (make-utility))
  :reset #'reset-utility-module
  :params #'utility-module-params)

(register-subsymbolic-parameters :egs :ul :iu :nu :alpha)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section is all support for spp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Taken and modified from ACT-R 5 code

(defun production-parameter-fct (production-name &optional parameters)
  "Returns the value of the production parameter(s), or print all if none given."
  (let ((esc (car (no-output (sgp :esc))))
        (ul (car (no-output (sgp :ul))))
        (value nil)
        (values nil))
    (cond (production-name
           (command-output "Parameters for production ~S:" production-name)
           
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                      (case parameter
                        (:name production-name)
                        (:utility (production-utility production-name))
                        (:u (production-u production-name))
                        (:at (production-at production-name))
                        (:reward (production-reward production-name))
                        
                        (t (print-warning "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
                           :error)))
                    (push-last value values)
                    (command-output " ~S ~6,3F" parameter value))
                  values)
                 (t
                  (when esc 
                    (command-output " :utility ~6,3F" (production-utility production-name)))

                  (command-output " :u  ~6,3F" (production-u production-name))
                  (command-output " :at ~6,3F" (production-at production-name))
                  
                    
                  (when (and esc ul)
                    (command-output " :reward ~6,3F" (production-reward production-name)))
                  
                  production-name)))
          (t :error))))

;;; This name is dangerously close to what I use in the sgp code, but
;;; for now I'll leave it as is.

(defmacro set-parameter (slot parameter test warning &rest housekeeping)
  "Sets parameter of production p in slot if value passes test, otherwise issue warning."
  `(cond (,test
          (setf (,slot p) value)
          ,@housekeeping
          value)
         (t
          (print-warning
           "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
           ,parameter value ,warning)
          :error)))


(defun parameters-fct (p parameters)
  "Sets the parameters of the production (internal - user should use spp)."
  
  ;; Changing procedural parameters reschedules conflict resolution
  ;; if it's waiting to happen
         
  (un-delay-conflict-resolution)
         
  ;; Having the name of the production be p and the
  ;; value to be set called value are critical to
  ;; the functioning of the set-parameter macro...
  
  (let ((values nil)
        (ul (car (no-output (sgp :ul)))))
    (if p
        (loop
          
          (unless parameters 
            (return values))
        
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          
          ;; not sure about this, but I'll leave it in for now
          
          (when (and (listp value) 
                     (eq (first value) 'quote))
            (setf value (second value)))  
          
          (push-last
           (case parameter
             (:name
              (print-warning "PARAMETER NAME CANNOT BE SET.")
              :error)
             (:utility
              (print-warning "PARAMETER UTILITY CANNOT BE SET.")
              :error)
             
             
             (:u
              (set-parameter production-u :u
                             (numberp value) "a number"))
             
             (:at
              (set-parameter production-at :at
                             (nonneg value)
                             "a positive number"))
             
             (:reward
              (if ul
                  (set-parameter production-reward :reward
                                 (numberp value)
                                 "a number")
                (print-warning "PARAMETER REWARD CAN ONLY BE SET WHEN UL IS T.")))
             
             (t
              (print-warning
               "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
              :error))
           values)))
      :error)))


(defmacro spp (&rest production-parameters)
  "Inspects and sets production parameters."
  `(spp-fct ',production-parameters))


(defun spp-fct (parameters)
  "Inspects and sets production parameters."
  (let ((results nil))  
    (if (null parameters) ; print all parameters for all productions
        (dolist (production (all-productions))
          (push-last (production-parameter-fct production) results))
      (dolist (description (if (or (keywordp (first parameters))
                                   (keywordp (second parameters))
                                   (and (listp (first parameters))
                                        (null (second parameters))
                                        (not (keywordp 
                                              (second 
                                               (first parameters))))))
                               (list parameters) parameters))
        (when (atom description) (setf description (list description)))
        (if (keywordp (first description))
            (dolist (production (all-productions))
              (push-last
               (if (and (cdr description) 
                        (not (keywordp (second description))))
                   (parameters-fct production description)
                 (production-parameter-fct production description))
               results))
          
          (dolist (production (if (atom (first description))
                                  (list (first description))
                                (first description)))
            (if (get-production production)
                (push-last
                 (if (and (cddr description) 
                          (not (keywordp (third description))))
                     (parameters-fct production (rest description))
                   (production-parameter-fct production (rest description)))
                 results)
              (progn
                (model-warning "Spp cannot adjust parameters because production ~S does not exist" production)
                (push-last :error results)))))))
    results))




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