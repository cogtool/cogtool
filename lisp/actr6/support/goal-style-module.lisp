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
;;; Filename    : goal-style-module.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions that allow one to easily create a module that
;;;             : acts like the basic ACT-R goal module/buffer.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.26 Dan
;;;             : Initial creation.
;;;
;;; 2004.12.13 Dan
;;;             : Added the optional delay to the goal-style-request.
;;;             : Made sure line lengths were max 80 chars.
;;; 2005.01.09 Dan
;;;            : Moved the provide to the end.
;;; 2005.01.17 Dan
;;;            : * Removed the call to format in the scheduling.
;;; 2005.04.23 Dan
;;;            : * Updated the query function because it doesn't need to
;;;            :   respond to "buffer stuffed" anymore.
;;; 2005.08.10 Dan
;;;            : * Updated the query function to specify the ignored params.
;;;            : * Updated the version to 1.0.
;;; 2006.10.23 Dan
;;;            : * Changed the temp goal chunk "clean up" so that it's now
;;;            :   done with a maintenance event and it also uses the release-name
;;;            :   command to kill the symbol name in addition to the chunk struct.
;;; 2007.06.18 Dan
;;;            : * Removed chunk-spec-to-chunk-def because it's now an official
;;;            :   command and was moved to the chunk-spec file.
;;; 2008.09.19 Dan
;;;            : * Added the goal-style-mod-request function here and gave it
;;;            :   an optional delay time too.
;;; 2010.12.08 Dan
;;;            : * Added a priority to the goal-style-mod-request as an additional
;;;            :   optional parameter because the modification needs to take place
;;;            :   prior to the buffer being cleared (whether explicitly or implicitly).
;;;            :   The default priority is now 20 (since the clearing is 10).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; With these functions one can create a new module whos buffer acts like
;;; the basic goal module/buffer i.e. the module responds to requests by 
;;; creating a new chunk and placing it into the buffer.  
;;; 
;;; The requests must be a unique specification of a chunk.
;;;
;;; Thus, no variables are allowed and each slot may be specified at most once.
;;; The new chunk is placed into the buffer at the same time as the request.
;;; Optionally, one could provide a delay on the time it takes to put the
;;; chunk into the buffer by providing their own request function that calls
;;; the goal-style-request function with the optional delay parameter.
;;;
;;; It only responds to the required queries - state {busy, free, error}.
;;;
;;; State free will always return t.
;;; State busy will always return nil.
;;; State error will always return nil.
;;;
;;; The goal module does not respond to buffer modification requests.
;;;
;;; To create a basic goal style module, just place a file into the modules 
;;; directory that contains the following:
#|

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(define-module <the name of your module here>
  (<the name of your buffer here which must be the same as the module name>)
  nil
  :version "Something about the version of your module"
  :documentation "Something about the details of your module"
  :query goal-style-query
  :request goal-style-request)

|#

;;; If one wants a delay on the time it takes to put the chunk into the
;;; buffer then the code would look like this (assuming a fixed delay 
;;; time was desired).

;;; Note this shows the use of define-module-fct instead of the macro
;;; for comparison to the previous one.  Either version can be used.
#|

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(defun my-goal-style-request (instance buffer-name chunk-spec)
  (goal-style-request instance buffer-name chunk-spec <delay time>))

(define-module-fct '<the name of your module here>
  '(<the name of your buffer here which must be the same as the module name>)
  nil
  :version "Something about the version of your module"
  :documentation "Something about the details of your module"
  :query #'goal-style-query
  :request #'my-goal-style-request)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;  (defun goal-style-query (instance buffer-name slot value)
;;;
;;;  This can be used as the query function for a module that will respond to
;;;  the required queries in a static manner.  It will respond as follows:
;;;
;;;    State free will always return t.
;;;    State busy will always return nil.
;;;    State error will always return nil.
;;;
;;;
;;;  (defun goal-style-request (instance buffer-name chunk-spec 
;;;                             &optional (delay 0)))
;;;
;;;  This can be used as the request function of a module to allow it to
;;;  operate like the goal module i.e. create new chunks in response to a
;;;  request.  The instance is not used and it is assumed that the module
;;;  has the same name as the buffer.
;;;
;;;
;;;  (defun goal-style-mod-request (instance buffer mods &optional (delay 0))
;;;  
;;;  This can be used as a modification request function of a module to allow
;;;  it to handle modification requests like the goal module does - perform
;;;  an immediate modification of the chunk in the buffer.  It assumes that
;;;  the module and buffer have the same name and the instance of the module
;;;  is ignored.
;;;
;;; 
;;;  (defun create-new-buffer-chunk (buffer-name chunk-description 
;;;                                  &key (priority -1000)))
;;;
;;;  A function that creates a new chunk based on chunk-description and then
;;;  schedules that it be placed into the buffer called buffer-name with the
;;;  specified priority.
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


      
(defun goal-style-query (instance buffer-name slot value)
  ;  The instance is always nil and the only slot is state
  (declare (ignore instance) (ignore slot))
  ;(case slot
  ;  (state
  ; State is the only valid query
  (case value
       (busy nil)
       (free t)
       (error nil)
       (t (print-warning "Unknown state query ~S to ~S buffer" 
                         value buffer-name)
          nil)))

(defun goal-style-request (instance buffer-name chunk-spec &optional (delay 0))
  (declare (ignore instance))
  (let ((chunk-description (chunk-spec-to-chunk-def chunk-spec)))
    (if chunk-description
        (schedule-event-relative delay 'create-new-buffer-chunk 
                                 :module buffer-name
                                 :priority -100 
                                 :details 
                                 (concatenate 'string
                                   (symbol-name 'create-new-buffer-chunk)
                                   " "
                                   (symbol-name buffer-name)
                                   " "
                                   (symbol-name (first chunk-description))
                                   " "
                                   (symbol-name (second chunk-description)))
                                 :params (list buffer-name chunk-description))
      
      (print-warning "Invalid request made of the ~A buffer." buffer-name))))

(defun create-new-buffer-chunk (buffer-name chunk-description 
                                            &key (priority -1000))
  (let ((chunk-name (car (define-chunks-fct (list chunk-description)))))
    (schedule-set-buffer-chunk buffer-name chunk-name 0 
                               :module buffer-name :priority priority)
    ;; because the chunk is only being created to be copied into the buffer
    ;; just get rid of it after that happens to keep the chunk count
    ;; down 
    (schedule-event-relative 0 'clean-up-goal-chunk :module :none :output nil 
                             :priority (1- priority) :params (list chunk-name)
                             :details "Clean-up unneeded chunk"
                             :maintenance t)))


(defun clean-up-goal-chunk (name)
  (delete-chunk-fct name)
  (release-name-fct name))


(defun goal-style-mod-request (instance buffer mods &optional (delay 0) (priority 20))
  (declare (ignore instance))
  (schedule-mod-buffer-chunk buffer mods delay :module buffer :priority priority))

(provide "GOAL-STYLE-MODULE")

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
