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
;;; Filename    : top-level.lisp
;;; Version     : 1.0.a1
;;; 
;;; Description : The framework's top level user commands that aren't
;;;               part of another section.
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Documentation.
;;;             : * Make reload/clear-all smart about compiled files so that 
;;;             :   it knows to check the .lisp if the current file is a
;;;             :   compiled one.
;;;             : [X] Why doesn't clear-all use the reset-mp command?
;;; 
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.29 Dan
;;;             : * Added an optional parameter to reload that if specified as
;;;             :   non-nil will use compile-and-load to load the file.
;;; 2005.02.11 Dan
;;;             : * Changed clear-all to use clrhash instead of building new
;;;             :   tables.
;;; 2005.02.22 Dan
;;;             : * Modified reset so it better reports what's happening in
;;;             :   other than normal circumstances.
;;; 2009.10.27 Dan
;;;             : * Fixed clear-all so that it also restores the meta-process
;;;             :   timing code to the default if it has been changed by mp-
;;;             :   real-time-management.
;;; 2009.12.03 Dan
;;;             : * Clear the meta-process dynamics and in-slack on a clear-all too.
;;; 2009.12.04 Dan
;;;             : * Use reset-mp in clear-all to avoid duplicate code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
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


(defvar *recorded-load-file* nil)


(defun clear-all ()
  
  (maphash #'(lambda (name meta-process)
               (declare (ignore meta-process))
               (unless (eq name 'default)
                 (delete-meta-process-fct name)))
           (mps-table *meta-processes*))
  
  ;;; Only 1 meta-process left at this point - the default
  ;;; It needs to be explicitly reset to initial state
  
  
  
  (let ((mp (current-mp)))
    
    
    (maphash #'(lambda (name model)
                 (declare (ignore model))
                 (delete-model-fct name))
             (meta-p-models mp))
    
    
    ;; This resets the scheduler and real-time management
    
    (reset-mp mp)
    
      
    (setf (meta-p-current-model mp) nil)
    (setf (meta-p-model-count mp) 0)
    (setf (meta-p-model-name-len mp) 0)
    (setf (meta-p-pre-events mp) nil)
    (setf (meta-p-post-events mp) nil)
    (setf (meta-p-next-hook-id mp) 0)

    (clrhash (meta-p-hook-table mp))
      
    
    (clrhash (meta-p-models mp)))
  
  
  (setf *recorded-load-file* *load-truename*)
  nil) 


(defun reset ()
  (verify-current-mp  
   "reset called with no current meta-process."
   (let ((mp (current-mp)))
     
     ;;; special case this for nicer backward compatibility
     
     (cond ((and (= (mps-count *meta-processes*) 1)
                 (= (length (hash-table-keys (meta-p-models mp))) 1)
                 (null (act-r-model-code (current-model-struct ))))
            
            (if *recorded-load-file*
                (progn
                  (model-warning "Resetting an empty model results in a reload")
                  (reload))
              (progn
                (print-warning "CANNOT RESET an empty model that wasn't loaded.")
                (print-warning "RESET had no effect!"))))
           
           (t 
            (reset-mp mp)
            
            (maphash #'(lambda (name model)
                         (declare (ignore name))
                         (reset-model mp model))
                     (meta-p-models mp))))
     
     (meta-p-name mp))))


(defun reload (&optional (compile nil))
  (if *recorded-load-file*
      (if compile
          (if (string= (pathname-type *recorded-load-file*)
                       (pathname-type *.lisp-pathname*))
              (compile-and-load *recorded-load-file*)
            (progn 
              (print-warning 
               "To use the compile option the pathname must have type ~a."
               (pathname-type *.lisp-pathname*))
              (load *recorded-load-file*)))
        (load *recorded-load-file*))
    (progn 
      (print-warning "No load file recorded")
      :none)))



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
