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
;;; Filename    : model.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions that support the abstraction of a model
;;; 
;;; Bugs        : 
;;;
;;; To do       : Finish the documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.20.08 Dan
;;;             : Creation
;;; 2005.01.12 Dan
;;;             : Don't need to special case the device because it's now an
;;;             : actual module.
;;; 2005.02.11 Dan
;;;             : * Changed some reset-model to use clrhash instead of 
;;;             :   creating new tables.
;;; 2005.02.28 Dan
;;;             : * Made the with-model macro hygienic.
;;; 2005.03.23 Dan
;;;             : * Update the model reset and creation to use the two reset 
;;;             :   functions that are now part of the module definition - one 
;;;             :   before the parameter are reset and one after.
;;; 2006.07.05 Dan
;;;             : * Fixed a bug in the delete-model-fct function in the format 
;;;             :   command for printing that there was no model.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in delete-model which could result in the deleted
;;;             :   model being left as the current model after deletion.
;;; 2007.04.19 Dan
;;;             : * Fixed another bug in delete-model which left the current-model
;;;             :   set even when the last model currently defined was deleted.
;;; 2008.08.21 Dan
;;;             : * Added a new with-model-eval macro that sits half way between
;;;             :   the current macro and the -fct.  This macro does evaluate the
;;;             :   first parameter to get the name, but splices in the body
;;;             :   to evaluate.  Should be more convenient in many circumstances.
;;; 2008.10.22 Dan [1.0]
;;;             : * Updated the reset function to handle the new chunk-ref table
;;;             :   and setting its flags to default values.
;;;             : * Finally took the a1 off the version.
;;; 2008.12.01 Dan
;;;             : * Added the code to call a third module reset function after
;;;             :   the user code has been evaled.
;;; 2008.12.08 Dan
;;;             : * Added new-chunk-type-size and largest-chunk-type-size to
;;;             :   record and get the largest possible chunk size.
;;; 2008.12.15 Dan
;;;             : * Added the code to call the third reset function when the
;;;             :   model is initially created too.
;;; 2009.09.09 Dan
;;;             : * Updated reset-model to also clear the chunk-set from all of
;;;             :   the multi-buffers.
;;; 2009.10.08 Dan
;;;             : * Updated define-model to clear the chunk-set as well since
;;;             :   reset doesn't happen at initial definition time.
;;; 2009.12.03 Dan
;;;             : * Delete-model needs to take events out of the meta-process's 
;;;             :   dynamics list as well.
;;; 2010.08.17 Dan
;;;             : * Better cleanup in define-model when there's an error during
;;;             :   the definition code.
;;; 2010.11.16 Dan
;;;             : * Added the :mcts and :mctrt system parameters for configuring
;;;             :   the model's chunk table because those can speed things up 
;;;             :   significantly in some circumstances (for example ACL works
;;;             :   better with :mctrt of .6 and :mcts set large if there are
;;;             :   going to be a lot of chunks created).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Model structure is not for use outside of the framework.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; define-model
;;; 
;;; (defmacro define-model (name &body model-code))
;;; (defun define-model-fct (name model-code-list))
;;; 
;;; name a symbol that will be the name for the new model
;;; model-code any number of forms that will be evaluated for the model
;;; model-code-list a list of forms to evaluate for the model
;;; 
;;; define-model is used to create a new model in the current meta-process.
;;; 
;;; The name must not already be used for a model in the current meta-process. If the name is not a symbol or is already used to name a model in the current meta-process a warning will be displayed and the model will not be defined (the old model with that name will remain unchanged if one existed).
;;; 
;;; When a model is first defined the following sequence of events will occur:
;;; 
;;; - Create a new model with that name
;;; - with that new model as the current model
;;;    - create the default chunk-types 
;;;    - create the default chunks
;;;    - create a new instance of each module 
;;;        - call its create function if it exists
;;;        - call its reset function if it exists
;;;    - evaluate the forms of the model in the order provided
;;;   
;;; If a model is successfully created then its name is returned otherwise define-model returns nil.
;;; 
;;; Every model will need to have a call to define-model before issuing any of the model commands because there is no default model in a meta-process.  However, if one is working with only a single model then all that is necessary is to provide a name - it is not necessary to enclose all of the model code.
;;; 
;;; current-model
;;; 
;;; (defun current-model ())
;;; 
;;; current-model returns the name of the current model in the current meta-process or nil if there is no current model or no current meta-process.
;;; 
;;; delete-model
;;; 
;;; (defmacro delete-model (&optional model-name))
;;; (defun delete-model-fct (&optional model-name))
;;; 
;;; model-name a symbol that names a model
;;; 
;;; If model-name is not provided the name of the current-model is used.  
;;; 
;;; If model-name is the name of a model in the current meta-process then the following sequence of events will occur:
;;; 
;;;  - the model with that name is set to the current model
;;;  - all events generated by that model are removed from the event queue
;;;  - each module of the model is deleted
;;;  - the model is removed from the set of models in the current meta-process
;;; 
;;; If model-name is valid then t is returned.
;;; 
;;; If model-name is not valid or there is no current meta-process then a warning is printed, nothing is done and nil is returned.
;;; 
;;; with-model
;;; 
;;; (defmacro with-model (model-name &body body))
;;; (defun with-model-fct (model-name forms-list))
;;; 
;;; model-name a symbol that names a model in the current meta-process
;;; body any number of forms to execute
;;; forms-list a list of forms to execute
;;; 
;;; If model-name is the name of a model in the current meta-process then the forms are evaluated in order with the current model set to the one named by model-name.  The value of the last form evaluated is returned.
;;; 
;;; If model-name does not name a model in the current meta-process, or there is no current meta-process then none of the forms are evaluated, a warning is printed and nil is returned.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun current-model ()
  (when (current-model-struct)
    (act-r-model-name (current-model-struct))))

(defun largest-chunk-type-size ()
  (act-r-model-largest-chunk-type (current-model-struct)))

(defun new-chunk-type-size (size)
  (let ((model (current-model-struct)))
    (when (> size (act-r-model-largest-chunk-type model))
      (setf (act-r-model-largest-chunk-type model) size))))



(defvar *model-chunk-table-size* nil)
(defvar *model-chunk-table-rehash-threshold* nil)

(create-system-parameter :mcts :valid-test 'posnumornil :default-value nil :warning "positive number or nil"
                         :documentation "initial size of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-size*))

(create-system-parameter :mctrt :valid-test (lambda (x) (typep x '(or null (real 0 1)))) :default-value nil 
                                             :warning "a real number [0-1] or nil"
                         :documentation "rehash-threshold of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-rehash-threshold*))



(defmacro define-model (name &body model-code)
  `(define-model-fct ',name ',model-code))

(defun define-model-fct (name model-code-list)
  (verify-current-mp  
   "define-model called with no current meta-process."
   (cond ((not (symbolp name))
          (print-warning "Model name must be a symbol, ~S is not valid.  No model defined." name))
         ((null name)
          (print-warning "Nil is not a valid name for a model.  No model defined."))
         ((valid-model-name name)
          (print-warning "~S is already the name of a model in the current meta-process.  Cannot be redefined." name))
         (t
          (let ((new-model (make-act-r-model :name name))
                (mp (current-mp)))
            
            
            (when (or *model-chunk-table-size* *model-chunk-table-rehash-threshold*)
              (if *model-chunk-table-size*
                  (if *model-chunk-table-rehash-threshold*
                      (progn
                        (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*))
                        (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*)))
                    (progn
                      (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size*))
                      (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size*))))
                (progn
                  (setf (act-r-model-chunks-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))
                  (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*)))))
                        
            
            (setf (gethash name (meta-p-models mp)) new-model)
            (setf (meta-p-current-model mp) new-model)
            (incf (meta-p-model-count mp))
            
            
            ;(setf (act-r-model-device new-model) 
            ;  (make-instance 'device-interface))
            
            (when (> (length (format nil "~S" name)) (meta-p-model-name-len mp))
              (setf (meta-p-model-name-len mp) (length (format nil "~S" name))))
            
            (create-model-default-chunk-types-and-chunks)
            
            (maphash #'(lambda (module-name val)
                         (declare (ignore val))
                         (setf (gethash module-name (act-r-model-modules-table new-model))
                           (instantiate-module module-name name)))
                     (global-modules-table))
            
             ;; instantiate the buffers
            
            (maphash #'(lambda (buffer-name buffer-struct)
                         (let ((buffer (copy-act-r-buffer buffer-struct)))
                                       
                            (when (act-r-buffer-multi buffer)
                              (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                           
                           (setf (gethash buffer-name (act-r-model-buffers new-model)) buffer)))
                     *buffers-table*)
            
            
            (maphash #'(lambda (module-name val)
                         (declare (ignore val))
                         (reset-module module-name))
                     (global-modules-table))
            
            
            (maphash #'(lambda (parameter-name parameter)
                 (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
             *act-r-parameters-table*)
            
            
            (maphash #'(lambda (module-name val)
                         (declare (ignore val))
                         (secondary-reset-module module-name))
                     (global-modules-table))
            
            (let ((errored nil))
              (dolist (form model-code-list)
                (unwind-protect 
                    (handler-case (eval form)
                      (error (condition) 
                        (setf errored t)
                        (print-warning "Error encountered in model form:~%~S~%Invoking the debugger." form)
                        (print-warning "You must exit the error state to continue.")
                        (invoke-debugger condition)))
                  (when errored
                    (remhash name (meta-p-models mp))
                    (print-warning "Model ~s not defined." name)
                    (decf (meta-p-model-count mp))
                    
                    ;; delete any events that may have been scheduled by modules
                    ;; or code prior to the error
                    
                    (setf (meta-p-events mp)
                      (remove name (meta-p-events mp) :key #'evt-model))
                    
                    (setf (meta-p-delayed mp)
                      (remove name (meta-p-delayed mp) :key #'evt-model))
                    
                    (setf (meta-p-dynamics mp)
                      (remove name (meta-p-dynamics mp) :key #'(lambda (x) (evt-model (car x)))))
                    
                    
                    ;; remove the modules which were created
                    
                    (maphash #'(lambda (module-name instance)
                            (declare (ignore instance))
                            (delete-module module-name))
                        (global-modules-table))
                    
                    (return-from define-model-fct nil)))))
            
            (setf (act-r-model-code new-model) model-code-list)
            
            (maphash #'(lambda (module-name val)
                 (declare (ignore val))
                 (tertiary-reset-module module-name))
             (global-modules-table))
             
            (unless (= 1 (meta-p-model-count mp))
              (setf (meta-p-current-model mp) nil))
              
            name)))))


(defun create-model-default-chunk-types-and-chunks ()
  (chunk-type-fct (list 'chunk))
  (define-chunks-fct (list '(free isa chunk)
                           '(busy isa chunk)
                           '(error isa chunk)
                           '(empty isa chunk)
                           '(full isa chunk)
                           '(requested isa chunk)
                           '(unrequested isa chunk))))
  


(defmacro delete-model (&optional (model-name nil provided))
  `(if ,provided
       (delete-model-fct ',model-name)
     (delete-model-fct (current-model))))
  
(defun delete-model-fct (model-name)
  (verify-current-mp  
   "delete-model called with no current meta-process.~%No model deleted."
   (let ((mp (current-mp)))
     (if model-name
         (if (gethash model-name (meta-p-models mp))
             (let ((model (gethash model-name (meta-p-models mp)))
                   (saved-current (meta-p-current-model mp)))
               (setf (meta-p-current-model mp) model)
               
               (setf (meta-p-events mp)
                 (remove model-name (meta-p-events mp) :key #'evt-model))
               
               (setf (meta-p-delayed mp)
                 (remove model-name (meta-p-delayed mp) :key #'evt-model))
               
               (setf (meta-p-dynamics mp)
                 (remove model-name (meta-p-dynamics mp) :key #'(lambda (x) (evt-model (car x)))))
               
               (maphash #'(lambda (module-name instance)
                            (declare (ignore instance))
                            (delete-module module-name))
                        (global-modules-table))
               
               (decf (meta-p-model-count mp))
               (remhash model-name (meta-p-models mp))
               (cond ((zerop (meta-p-model-count mp))
                      (setf (meta-p-current-model mp) nil))
                     ((= 1 (meta-p-model-count mp))
                      (setf (meta-p-current-model mp)
                        (gethash (car (hash-table-keys (meta-p-models mp))) (meta-p-models mp))))
                      (t (setf (meta-p-current-model mp) saved-current)))
               
               t)
           (print-warning "No model named ~S in current meta-process." model-name))
       (print-warning "No current model to delete.")))))

(defmacro with-model (model-name &body body)
  (let ((mp (gensym))
        (previous-model (gensym)))
    `(let ((,mp (current-mp)))
     (if ,mp
         (if (valid-model-name ',model-name)
             (let ((,previous-model (current-model-struct)))
               (setf (meta-p-current-model (current-mp)) 
                 (gethash ',model-name (meta-p-models ,mp)))
               (unwind-protect (progn ,@body)
                 (setf (meta-p-current-model (current-mp)) ,previous-model)))
           (print-warning "~S does not name a model in the current meta-process" ',model-name))
       (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defmacro with-model-eval (model-name &body body)
  (let ((mp (gensym))
        (previous-model (gensym))
        (model (gensym)))
    `(let ((,mp (current-mp)))
       (if ,mp
           (let ((,model ,model-name)) 
             (if (valid-model-name ,model)
                 (let ((,previous-model (current-model-struct)))
                   (setf (meta-p-current-model (current-mp)) 
                     (gethash ,model (meta-p-models ,mp)))
                   (unwind-protect (progn ,@body)
                     (setf (meta-p-current-model (current-mp)) ,previous-model)))
               (print-warning "~S does not name a model in the current meta-process" ,model)))
         (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defun with-model-fct (model-name forms-list)
  (let ((mp (current-mp)))
     (if mp
         (if (valid-model-name model-name)
             (let ((previous-model (current-model-struct))
                   (val nil))
               (setf (meta-p-current-model (current-mp)) 
                 (gethash model-name (meta-p-models mp)))
               (unwind-protect (dolist (x forms-list val)
                                 (setf val (eval x)))
               (setf (meta-p-current-model (current-mp)) previous-model)
               ))
           (print-warning "~S does not name a model in the current meta-process" model-name))
       (print-warning "No actions taken in with-model because there is no current meta-process"))))


(defun valid-model-name (name)
    "Returns t if name is the name of a model in the current meta-process - there must be a current mp"
  (if (gethash name (meta-p-models (current-mp)))
      t
    nil))

(defun reset-model (mp model)
 
  (let ((previous-model (meta-p-current-model mp)))
    (setf (meta-p-current-model mp) model)
    
    (clrhash (act-r-model-chunk-types-table model))
    (clrhash (act-r-model-chunks-table model))
    (clrhash (act-r-model-chunk-ref-table model))
    
    (setf (act-r-model-chunk-update model) t)
    (setf (act-r-model-dynamic-update model) t)
    (setf (act-r-model-delete-chunks model) nil)
    
    (setf (act-r-model-largest-chunk-type model) 0)
    
    (maphash #'(lambda (buffer-name buffer)
                 (declare (ignore buffer-name))
                 (setf (act-r-buffer-chunk buffer) nil)
                 (when (act-r-buffer-multi buffer)
                   (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5))))
             (act-r-model-buffers model))
    
    (create-model-default-chunk-types-and-chunks)
    
    (maphash #'(lambda (module-name instance)
                 (declare (ignore instance))
                 (reset-module module-name))
             (global-modules-table))
    
    (maphash #'(lambda (parameter-name parameter)
                 (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
             *act-r-parameters-table*)
    
    
    (maphash #'(lambda (module-name val)
                 (declare (ignore val))
                 (secondary-reset-module module-name))
             (global-modules-table))    
    
    (dolist (form (act-r-model-code model))
      (eval form))
    
    (maphash #'(lambda (module-name val)
                 (declare (ignore val))
                 (tertiary-reset-module module-name))
             (global-modules-table))
    
    (setf (meta-p-current-model mp) previous-model)))
  

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
