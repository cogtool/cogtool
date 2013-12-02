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
;;; Filename    : buffers.lisp
;;; Version     : 1.1a
;;; 
;;; Description : Functions that define the operation of buffers.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [] Finish documentation.
;;;               [] Investigate the copy sematics and probably optimize things.
;;;               [] Crazy idea - why not treat buffer parameters the same as
;;;                  chunk parameters and allow them to be user defined?
;;;               [] Have all the schedule-* functions allow a details keyword.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.20 Dan
;;;             : Creation
;;;
;;; 2004.12.13 Dan
;;;             : Added :details to the scheduled buffer events to clean up the
;;;             : traces.
;;;             : Reduced lines down to max of 80 chars.
;;; 2005.01.17 Dan
;;;             : * Removed calls to format in the scheduling.
;;; 2005.01.18 Dan
;;;             : * Removed call to get-parameters.
;;; 2005.02.01 Dan
;;;             : * Modified buffer-chunk so it prints the chunks as well when
;;;             :   specific buffers requested.
;;; 2005.02.03 Dan
;;;             : * Changed the default output for some functions to 'medium
;;;             :   or 'low to play friendly with the new detail level.
;;; 2005.02.04 Dan
;;;             : * Taking advantage of the fast-* chunk accessors.
;;; 2005.04.19 Dan
;;;             : * Added buffers-module-name to add to the API.
;;; 2005.04.23 Dan
;;;             : * Added the buffer-status command to print out the queries
;;;             :   for buffers.  Works basically like buffer-chunk.
;;;             : * Added the status-printing option to buffer definition
;;;             :   list (the fifth item) so a module can print extra status
;;;             :   info with buffer-status.
;;;             : * Added the requested keyword to the set-buffer-chunk 
;;;             :   command because the requested (formerly stuffed) status
;;;             :   of the buffer is being maintained internally now.
;;;             : * Updated the query-buffer command to handle buffer
;;;             :   requested/unrequested instead of passing it to the module.
;;;             : * For now, an overwrite always sets the requested to nil,
;;;             :   but maybe that needs to be user defineable too.
;;; 2005.05.11 Dan
;;;             : * Adjusted the params in the schedule-set-buffer-chunk
;;;             :   call so that requested chunks don't have to show that
;;;             :   in the trace - only unrequested chunks are marked.
;;; 2005.08.10 Dan
;;;             : * Added the uninstall-buffers function to support the 
;;;             :   undefine-modules function.  This is not designed to be used
;;;             :   any other way - buffer removal is a dangerous thing.
;;;             : * Updated the version to 1.0.
;;; 2005.08.16 Dan
;;;             : * Changes necessary to allow query of error t/nil without
;;;             :   any change to the module code - a module just has to 
;;;             :   respond to the "state error" query and the query-module
;;;             :   function handles the mapping now.  The change here is
;;;             :   to add error to the *required-buffer-queries* list
;;;             :   and to now test that on doesn't try to override a required
;;;             :   query.
;;; 2006.01.17 Dan
;;;             : * Updated the module version to 1.0 since there haven't been 
;;;             :   any problems it's time to drop the "a".
;;; 2006.01.25 Dan
;;;             : * Added an option to allow details in schedule-module-request
;;;             :   to overwrite the default.   Should make that change for all
;;;             :   of the schedule- functions at some point, but for now I 
;;;             :   only need it for the request one...
;;; 2006.07.16 Dan
;;;             : * Fixed an issue with buffer-chunk-fct because now that
;;;             :   more chunk accessors print a warning for nil it has to
;;;             :   be more careful to avoid lots of warnings.
;;; 2006.11.20 Dan
;;;             : * Added require-module-warning? and module-warning to support
;;;             :   the warning mechanism added to the modules.
;;; 2007.01.30 Dan
;;;             : * Changed set-buffer-chunk so that it returns the name of the
;;;             :   copied chunk that was put into the buffer on a success and 
;;;             :   nil on any failure (instead of the requested value like it was).
;;; 2007.04.10 Dan
;;;             : * Cleaned up the warning strings for several of the schedule-*
;;;             :   functions because they had references to the function without
;;;             :   the schedule- on the front.
;;; 2007.04.13 Dan
;;;             : * Added a check for the value of the :cbct parameter to determine
;;;             :   if buffer copy actions should show a line in the trace.
;;; 2007.05.03 Dan
;;;             : * Fixed a bug in parse-buffers (which is used for module definition)
;;;             :   which would allow one to create a buffer that had a buffer
;;;             :   spread parameter that matched the name of a parameter in
;;;             :   another module.
;;; 2008.05.02 Dan
;;;             : * Added the show-buffer-chunks command for use by run-step.
;;;             :   May want to consider that a user command eventually.
;;; 2008.07.13 Dan
;;;             : * Fixed the return value for overwrite-buffer-chunk and added
;;;             :   a requested keyword parameter to it like set-buffer-chunk has
;;;             :   except the default value is nil.
;;; 2008.07.14 Dan 
;;;             : * Adjusted the event for schedule-overwrite... so that it 
;;;             :   only prints requested when it's nil like set-buffer-chunk.
;;; 2008.10.30 Dan
;;;             : * Took out the sgp for :cbct and use (show-copy-buffer-trace)
;;;             :   instead.
;;; 2009.02.10 Dan
;;;             : * Added an optional parameter to module-mod-request and
;;;             :   schedule-module-mod-request to allow 'invalid' modifications
;;;             :   to be passed to the module, but the default is to still check.
;;; 2009.09.09 Dan [1.1a]
;;;             : * Very experimental addition - buffers which can have a set
;;;             :   of potential chunks.  Those potential chunks can be put into
;;;             :   the buffer without copying.  The important thing to note is that
;;;             :   there can still only be one chunk "in the buffer" at any point 
;;;             :   in time.  Not all chunks can be used in that fashion.  Chunks
;;;             :   which have been cleared from a buffer are invalid for a buffer set 
;;;             :   because modules other than the owner may now have reference to
;;;             :   them.  It's also possible for a module to mark its chunks as
;;;             :   invalid for being put into any buffer set by setting the
;;;             :   chunk's buffer-set-invalid parameter to t.  If a chunk that
;;;             :   is already in a potential set gets marked as invalid it will
;;;             :   copied if it is attempted to be set in the buffer.
;;;             :   Eventually, but not yet, productions will be able to search 
;;;             :   through that set of chunks to find a matching one insead of only
;;;             :   matching the "current" chunk in the buffer.
;;;             :   The new constructs will be called a multi-buffer and a searchable
;;;             :   buffer.  A searchable buffer will always be a multi-buffer, 
;;;             :   but not all multi-buffers have to be searchable.  The type
;;;             :   of a buffer can be specified when creating it for a module 
;;;             :   (see the updates to define-module in the modules file) and a 
;;;             :   standard buffer will be the default.
;;;             : * There are 3 new commands which can be used for multi-buffers:
;;;             :   - store-m-buffer-chunk which takes a buffer name and a chunk
;;;             :   name and adds that chunk to the set of chunks available for
;;;             :   that buffer if the chunk isn't marked as invalid for a buffer set.
;;;             :   - remove-m-buffer-chunk which takes a buffer name and a chunk
;;;             :   name and removes that chunk from the set of chunks available
;;;             :   for that buffer.
;;;             :   - get-m-buffer-chunks takes a buffer name and then returns
;;;             :   a list of the chunk names for the chunks in that buffer's
;;;             :   set of available chunks if it is a multi-buffer.
;;;             : * The following commands are modified to work with multi-buffers:
;;;             :   - set-buffer-chunk - if the buffer is a multi-buffer and the
;;;             :   chunk being set is in the current set for that buffer then
;;;             :   it is not copied before putting it into the buffer.  See
;;;             :   also the change to clear-buffer since setting a buffer has
;;;             :   an implicit clearing.
;;;             :   - overwrite-buffer-chunk - same as set-buffer-chunk except
;;;             :   of course that there is no implicit clear with overwrite.
;;;             :   - clear-buffer - if the buffer is a multi-buffer and the chunk 
;;;             :   being cleared is in the current set it is removed from
;;;             :   the current set.  Whether or not the chunk is coming from a
;;;             :   multi-buffer it will have its buffer-set-invalid parameter
;;;             :   set to t.
;;;             :   buffer-chunk - the buffer-chunk command will also show
;;;             :   the set of potential chunks when displaying info about a multi-
;;;             :   buffer.
;;;             : * One other change has been made - buffer-instance no longer
;;;             :   does the mp and model tests since it's always called from
;;;             :   code that's already done so.
;;; 2009.09.29 Dan
;;;             : * Adding an erase-buffer command which takes a chunk out of a
;;;             :   buffer without clearing it.  Essentially a null overwrite.
;;;             :   Useful for modules which work with multi-buffers and want to
;;;             :   do a "soft clear" without losing the chunk to DM.  It does
;;;             :   not have a corresponding schedule- function since it's 
;;;             :   intended only for modules' internal use.
;;; 2010.01.14 Dan
;;;             : * Extending the multi buffer options so that one can have a
;;;             :   multi-buffer which does copy its chunks.  When creating a
;;;             :   module now the options for the buffer are :multi, :search,
;;;             :   :multi-copy, and :search-copy.  The :multi-copy option 
;;;             :   doesn't seem all that useful, but the :search-copy could
;;;             :   allow a module to keep a set of chunks to search over and
;;;             :   not have to be "careful" about how they are used.
;;;             : * Added a new command for working with multi-buffers:
;;;             :   remove-all-m-buffer-chunks.  It takes a buffer name and if
;;;             :   that names a multi-buffer it removes all chunks from its 
;;;             :   current set.
;;; 2010.01.18 Dan
;;;             : * Fixed a typo in parse-buffers.
;;; 2010.12.08 Dan
;;;             : * Added a check to parse-buffers to make sure a buffer's name
;;;             :   doesn't end in >, doesn't start with =, and that it isn't a 
;;;             :   keyword because those sorts of things will cause problems
;;;             :   in production parsing.
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

;; Flag to prevent a chunk that's been cleared from a buffer set being 
;; put back into one.

(extend-chunks buffer-set-invalid)

(defconstant *required-buffer-queries* '(state buffer error))

(defvar *buffers-table* (make-hash-table))


(defun buffers ()
  (hash-table-keys *buffers-table*))


(defun buffer-exists (name)
  (multiple-value-bind (buffer present)
      (gethash name *buffers-table*)
    (declare (ignore buffer))
    present))

(defun parse-buffers (buffer-list)
  (let ((res nil))
    (dolist (buffer-def buffer-list res)
      
      ;; Adding a pre-check to make sure the name is a symbol, not a keyword, and that it
      ;; doesn't end in > to avoid crazy issues with production parsing
      
      (let ((name (if (atom buffer-def) buffer-def (car buffer-def))))
        
        (unless (and (symbolp name) (not (keywordp name)))
          (print-warning "Buffer name ~s is not valid because a buffer name must be a non-keyword symbol" name)
          (return-from parse-buffers :error))
        (let ((sym (symbol-name name)))
          (when (eql #\> (aref sym (1- (length sym))))
            (print-warning "Buffer name ~s is not valid because a buffer name must not end with the character '>'" name)
            (return-from parse-buffers :error))
          (when (eql #\= (aref sym 0))
            (print-warning "Buffer name ~s is not valid because a buffer name must not start with the character '='" name)
            (return-from parse-buffers :error))))

      (cond ((or (atom buffer-def)
                 (and (listp buffer-def) (= (length buffer-def) 1)))
             (when (listp buffer-def)
               (setf buffer-def (car buffer-def)))
             (cond ((buffer-exists buffer-def)
                    (print-warning "Buffer name ~S already used, cannot reuse it.")
                    (return-from parse-buffers :error))
                   (t
                    (push (make-act-r-buffer 
                           :name buffer-def
                           :queries *required-buffer-queries*
                           :copy t
                           :parameter-name 
                           (read-from-string (format nil ":~S-activation" buffer-def)))
                          res))))
            ((and (listp buffer-def)
                  (buffer-exists (car buffer-def)))
             (print-warning "Buffer name ~S already used, cannot reuse it.")
             (return-from parse-buffers :error))
            
            ((not (listp buffer-def))
             (print-warning "Invalid buffer specification: ~S" buffer-def)
             (return-from parse-buffers :error))

            ((<= (length buffer-def) 6)
             (let (param-name param-default requests queries print-status multi searchable
                   (copy t))
               (if (and (second buffer-def) (listp (second buffer-def)))
                   (cond ((or 
                           (> (length (second buffer-def)) 2)
                           (not (keywordp (first (second buffer-def))))
                           (not (numberp (second (second buffer-def)))))
                          (print-warning "Invalid buffer specification: ~S" buffer-def)
                          (return-from parse-buffers :error))
                         (t
                          (setf param-name (first (second buffer-def)))
                          (setf param-default (second (second buffer-def)))))
                 (cond ((keywordp (second buffer-def))
                        (setf param-name (second buffer-def)))
                       ((null (second buffer-def))
                        (setf param-name
                          (read-from-string (format nil ":~S-activation" (first buffer-def)))))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (third buffer-def)
                 (cond ((and (listp (third buffer-def))
                             (every #'keywordp (third buffer-def)))
                        (setf requests (third buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (fourth buffer-def)
                 (cond ((and (listp (fourth buffer-def))
                             (every #'(lambda (x) 
                                        (and (symbolp x) (not (keywordp x))
                                             (not (find x *required-buffer-queries*))))
                                    (fourth buffer-def)))
                        (setf queries (fourth buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (fifth buffer-def)
                 (cond ((fctornil (fifth buffer-def))
                        (setf print-status (fifth buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (print-warning "status function not valid")
                        (return-from parse-buffers :error))))
               
               (when (sixth buffer-def)
                 (case (sixth buffer-def)
                   (:multi (setf multi t)
                           (setf copy nil))
                   (:search
                    (setf multi t)
                    (setf searchable t)
                    (setf copy nil))
                   (:multi-copy 
                    (setf multi t))
                   (:search-copy
                    (setf multi t)
                    (setf searchable t))
                   (t
                    (print-warning "Invalid multi-buffer specification ~S for buffer ~s." buffer-def (first buffer-def))
                    (return-from parse-buffers :error))))
                   

               
               (when (valid-parameter-name param-name) 
                 (print-warning "Buffer spread parameter ~S is already the name of a parameter." param-name)
                 (return-from parse-buffers :error))
                                
               (push (make-act-r-buffer 
                      :name (first buffer-def)
                      :queries (append queries *required-buffer-queries*)
                      :requests requests
                      :parameter-name param-name
                      :spread param-default
                      :status-printing print-status
                      :multi multi
                      :searchable searchable
                      :copy copy
                      :chunk-set (make-hash-table :test 'eq :size 5)
                      :requested nil)
                     res)))
            (t
             (print-warning "Invalid buffer specification: ~S" buffer-def)
             (return-from parse-buffers :error))))))


                   
(defun install-buffers (module-name buffers)
  (dolist (buffer buffers)
    (setf (gethash (act-r-buffer-name buffer) *buffers-table*) buffer)
    (setf (act-r-buffer-module buffer) module-name)
    (install-parameters 'buffer-params 
                        (list (define-parameter (act-r-buffer-parameter-name buffer)
                                  :owner t :valid-test #'numberp
                                  :default-value (if (null (act-r-buffer-spread buffer))
                                                     0
                                                   (act-r-buffer-spread buffer))
                                  :warning "a number"
                                  :documentation (format nil "source spread for the ~S buffer" 
                                                 (act-r-buffer-name buffer)))))))


(defun uninstall-buffers (buffers)
  "Necessary for undefining a module"
  (dolist (buffer buffers)
    (remhash (act-r-buffer-name buffer) *buffers-table*)
    (remove-parameter (act-r-buffer-parameter-name buffer))))  


(defun create-buffer-param-module (model-name)
  (declare (ignore model-name))
  (make-hash-table))

(defun buffer-params-handler (instance param)
  (if (consp param)
      (setf (gethash (car param) instance) (cdr param))
    (gethash param instance)))


(define-module buffer-params nil nil :version "1.0"
  :documentation "Module to hold and control the buffer parameters"
  :creation create-buffer-param-module
  :params buffer-params-handler)



(defun buffer-instance (buffer-name)
  (gethash buffer-name (act-r-model-buffers (current-model-struct))))


(defun store-m-buffer-chunk (buffer-name chunk)
  (verify-current-mp  
   "store-m-buffer-chunk called with no current meta-process."
   (verify-current-model
    "store-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "store-m-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-multi buffer))
             (print-warning "store-m-buffer-chunk cannot store a chunk in buffer ~s because it is not a multi-buffer" buffer-name))
            ((null (chunk-p-fct chunk))
             (print-warning "store-m-buffer-chunk cannot store ~s in the buffer set because it does not name a chunk" chunk))
            ((chunk-buffer-set-invalid chunk)
             (print-warning "store-m-buffer-chunk cannot store ~s in a buffer set because it has been marked as invalid for a buffer set most likely because it has been previously cleared from a buffer" chunk))
            (t
             (setf (gethash chunk (act-r-buffer-chunk-set buffer)) t)
             chunk))))))

(defun remove-m-buffer-chunk (buffer-name chunk)
  (verify-current-mp  
   "remove-m-buffer-chunk called with no current meta-process."
   (verify-current-model
    "remove-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "remove-m-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-multi buffer))
             (print-warning "remove-m-buffer-chunk cannot remove a chunk from buffer ~s because it is not a multi-buffer" buffer-name))
            ((null (chunk-p-fct chunk))
             (print-warning "remove-m-buffer-chunk cannot remove ~s from the buffer set because it does not name a chunk" chunk))
            (t
             (remhash chunk (act-r-buffer-chunk-set buffer))
             chunk))))))

(defun get-m-buffer-chunks (buffer-name)
  (verify-current-mp  
   "remove-m-buffer-chunk called with no current meta-process."
   (verify-current-model
    "remove-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "get-m-buffer-chunks called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-multi buffer))
             (print-warning "get-m-buffer-chunks cannot return a chunk set for buffer ~s because it is not a multi-buffer" buffer-name))
            (t
             (hash-table-keys (act-r-buffer-chunk-set buffer))))))))

(defun remove-all-m-buffer-chunks (buffer-name)
  (verify-current-mp  
   "remove-all-m-buffer-chunks called with no current meta-process."
   (verify-current-model
    "remove-all-m-buffer-chunks called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "remove-all-m-buffer-chunks called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-multi buffer))
             (print-warning "remove-all-m-buffer-chunks cannot remove a chunk from buffer ~s because it is not a multi-buffer" buffer-name))
            (t
             (clrhash (act-r-buffer-chunk-set buffer))
             t))))))

(defmacro buffer-chunk (&rest buffer-names)
  `(buffer-chunk-fct ',buffer-names))

(defun buffer-chunk-fct (buffer-names-list)
  (verify-current-mp  
   "buffer-chunk called with no current meta-process."
   (verify-current-model
    "buffer-chunk called with no current model."
    (let ((res nil))
      (dolist (buffer-name (if buffer-names-list
                               buffer-names-list
                             (buffers))
                           res)
        (let ((buffer (buffer-instance buffer-name)))
          (if buffer
              (let ((chunk (act-r-buffer-chunk buffer)))
                (command-output "~S: ~S ~@[[~s]~]~@[ {~{~s~^ ~}}~]" buffer-name chunk
                                (when chunk (chunk-copied-from-fct chunk))
                                (when (act-r-buffer-multi buffer)
                                  (get-m-buffer-chunks buffer-name)))
                (when buffer-names-list
                  (pprint-chunks-fct (list chunk)))
                (push-last (if buffer-names-list
                               chunk
                             (cons buffer-name chunk))
                           res))
            (push-last (if buffer-names-list
                           :error
                         (cons :error nil)) 
                       res))))))))
        

(defun show-buffer-chunks ()
  (dolist (buffer-name (buffers))
    (let* ((chunk (buffer-read buffer-name)))
          (if chunk
              (progn
                (command-output "~S:" buffer-name)
                (pprint-chunks-fct (list chunk)))
            (command-output "~S: empty" buffer-name)))))


(defmacro buffer-status (&rest buffer-names)
  `(buffer-status-fct ',buffer-names))

(defun buffer-status-fct (buffer-names-list)
  (verify-current-mp  
   "buffer-status called with no current meta-process."
   (verify-current-model
    "buffer-status called with no current model."
    (let ((res nil))
      (dolist (buffer-name (if buffer-names-list
                               buffer-names-list
                             (buffers))
                           res)
        (let ((buffer (buffer-instance buffer-name)))
          (if buffer
              (progn
                (command-output "~S:" buffer-name)
                (command-output "  buffer empty          : ~S"
                                (query-buffer buffer-name '((buffer . empty))))
                (command-output "  buffer full           : ~S"
                                (query-buffer buffer-name '((buffer . full))))
                (command-output "  buffer requested      : ~S"
                                (query-buffer buffer-name '((buffer . requested))))
                (command-output "  buffer unrequested    : ~S"
                                (query-buffer buffer-name '((buffer . unrequested))))
                (command-output "  state free            : ~S"
                                (query-buffer buffer-name '((state . free))))
                (command-output "  state busy            : ~S"
                                (query-buffer buffer-name '((state . busy))))
                (command-output "  state error           : ~S"
                                (query-buffer buffer-name '((state . error))))
                
                (awhen (act-r-buffer-status-printing buffer)
                       (funcall it))
                
                (push-last buffer-name res))
            (push-last :error res))))))))

(defun buffer-read (buffer-name)  
  (verify-current-mp  
   "buffer-read called with no current meta-process."
   (verify-current-model
    "buffer-read called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if buffer 
          (act-r-buffer-chunk buffer) 
        (print-warning "Buffer-read called with an invalid buffer name ~S" buffer-name))))))


(defun schedule-buffer-read (buffer-name time-delta &key (module :none) (priority 0) (output t))
  (verify-current-mp  
   "schedule-buffer-read called with no current meta-process."
   (verify-current-model
    "schedule-buffer-read called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-buffer-read called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-buffer-read called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-buffer-read called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'buffer-read-action 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name)
                                      :output output)))))))


(defun buffer-read-report (buffer-name &key (module :none))
  (verify-current-mp  
   "buffer-read-report called with no current meta-process."
   (verify-current-model
    "buffer-read-report called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "buffer-read-report called with an invalid buffer name ~S" buffer-name))
            (t
             (schedule-event-relative 0 'buffer-read-action 
                                      :module module
                                      :priority :max 
                                      :params (list buffer-name)
                                      :output t)
             (act-r-buffer-chunk buffer)))))))


(defun buffer-read-action (buffer-name)
  (declare (ignore buffer-name)))


(defun query-buffer (buffer-name queries-list)
  (verify-current-mp  
   "query-buffer called with no current meta-process."
   (verify-current-model
    "query-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "query-buffer called with an invalid buffer name ~S" buffer-name))
            ((not (every #'(lambda (x)
                             (member (car x) (act-r-buffer-queries buffer)))
                         queries-list))
             (print-warning "Invalid query-buffer ~S.  Available queries to buffer ~S are ~S."
                            queries-list buffer-name (act-r-buffer-queries buffer)))
            (t
             (do ((module (act-r-buffer-module buffer))
                  (queries queries-list (cdr queries)))
                 ((null queries) t)
               (cond ((eq (caar queries) 'buffer)
                      (case (cdar queries) 
                        (full 
                         (when (null (act-r-buffer-chunk buffer))
                           (return-from query-buffer nil)))
                        (empty
                         (unless (null (act-r-buffer-chunk buffer))
                           (return-from query-buffer nil)))
                        (requested
                         (when (or (null (act-r-buffer-chunk buffer))
                                   (null (act-r-buffer-requested buffer)))
                           (return-from query-buffer nil)))
                        (unrequested
                         (unless (and (act-r-buffer-chunk buffer)
                                      (null (act-r-buffer-requested buffer)))
                           (return-from query-buffer nil)))
                        (t
                         (model-warning "Unknown buffer query ~S" (cdar queries))
                         (return-from query-buffer nil))))
                     (t
                      (unless (query-module module buffer-name (caar queries) (cdar queries))
                   (return-from query-buffer nil)))))))))))
                                              

(defun schedule-query-buffer (buffer-name queries-list time-delta &key (module :none) (priority 0) (output t))
  (verify-current-mp  
   "schedule-query-buffer called with no current meta-process."
   (verify-current-model
    "schedule-query-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-query-buffer called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-query-buffer called with non-nimber time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-query-buffer called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'query-buffer-action 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name queries-list)
                                      :details
                                      (concatenate 'string
                                        (symbol-name 'query-buffer-action)
                                        " "
                                        (symbol-name buffer-name))
                                      :output output)))))))


(defun query-buffer-report (buffer-name queries-list &key (module :none))
  (verify-current-mp  
   "query-buffer-report called with no current meta-process."
   (verify-current-model
    "query-buffer-report called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "query-buffer-report called with an invalid buffer name ~S" buffer-name))
            ((not (every #'(lambda (x)
                             (member (car x) (act-r-buffer-queries buffer)))
                         queries-list))
             (print-warning "Invalid query-buffer ~S.  Available queries to buffer ~S are ~S."
                            queries-list buffer-name (act-r-buffer-queries buffer)))
            (t
             (schedule-event-relative 0 'query-buffer-action 
                                      :module module
                                      :priority :max 
                                      :params (list buffer-name queries-list)
                                      :details 
                                      (concatenate 'string
                                        (symbol-name 'query-buffer-action)
                                        " "
                                        (symbol-name buffer-name))
                                      :output t)
             (query-buffer buffer-name queries-list)))))))


(defun query-buffer-action (buffer-name queries)
  (declare (ignore buffer-name queries)))


(defun clear-buffer (buffer-name)
  (verify-current-mp  
   "clear-buffer called with no current meta-process."
   (verify-current-model
    "clear-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "clear-buffer called with an invalid buffer name ~S" buffer-name))
            (t
             (let ((chunk (act-r-buffer-chunk buffer)))
               (when chunk
                 (setf (act-r-buffer-chunk buffer) nil)
                 (setf (chunk-buffer-set-invalid chunk) t)
                 (when (act-r-buffer-multi buffer)
                   (remhash chunk (act-r-buffer-chunk-set buffer)))
                 (dolist (module (notified-modules))
                   (notify-module module buffer-name chunk)))
               chunk)))))))


(defun schedule-clear-buffer (buffer-name time-delta &key (module :none) (priority 0) (output 'low))
  (verify-current-mp  
   "schedule-clear-buffer called with no current meta-process."
   (verify-current-model
    "schedule-clear-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-clear-buffer called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-clear-buffer called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-clear-buffer called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'clear-buffer 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name)
                                      :output output)))))))


(defun erase-buffer (buffer-name)
  (verify-current-mp  
   "erase-buffer called with no current meta-process."
   (verify-current-model
    "erase-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "erase-buffer called with an invalid buffer name ~S" buffer-name)
        (let ((chunk (act-r-buffer-chunk buffer)))
          (setf (act-r-buffer-chunk buffer) nil)
          chunk))))))

(defun set-buffer-chunk (buffer-name chunk-name &key (requested t))
  "Forces a copy unless it's a multi-buffer and this chunk is in the set"
  (verify-current-mp  
   "set-buffer-chunk called with no current meta-process."
   (verify-current-model
    "set-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "set-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "set-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            (t
             (when (act-r-buffer-chunk buffer)
               (clear-buffer buffer-name))
             (setf (act-r-buffer-requested buffer) requested)
             (let ((copy-name (if (or (act-r-buffer-copy buffer) (chunk-buffer-set-invalid chunk-name) (null (gethash chunk-name (act-r-buffer-chunk-set buffer))))
                                  (copy-chunk-fct chunk-name)
                                chunk-name)))
               (when (and (show-copy-buffer-trace) (not (eq copy-name chunk-name)))
                 (schedule-event-relative 0 'show-buffer-copy :maintenance t :module 'buffer 
                                          :priority :max 
                                          :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string copy-name)) 
                                          :output 'medium))
               (setf (act-r-buffer-chunk buffer) copy-name))))))))


;; A dummy function for the event action
(defun show-buffer-copy ())

(defun schedule-set-buffer-chunk (buffer-name chunk-name time-delta &key (module :none) (priority 0) (output 'low) (requested t))
  (verify-current-mp  
   "set-buffer-chunk called with no current meta-process."
   (verify-current-model
    "set-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-set-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "schedule-set-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            ((not (numberp time-delta))
             (print-warning "schedule-set-buffer-chunk called with time-delta that is not a number: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-set-buffer-chunk called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'set-buffer-chunk 
                                      :module module
                                      :priority priority 
                                      :params (if requested
                                                  (list buffer-name chunk-name)
                                                (list buffer-name chunk-name :requested requested))
                                      :output output)))))))


(defun overwrite-buffer-chunk (buffer-name chunk-name &key (requested nil))
  "Also forces a copy of the chunk unless it's in the set of a multi-buffer"
  (verify-current-mp  
   "overwrite-buffer-chunk called with no current meta-process."
   (verify-current-model
    "overwrite-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "overwrite-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "overwrite-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            (t
             (setf (act-r-buffer-requested buffer) requested)
             (let ((copy-name (if (or (act-r-buffer-copy buffer) (chunk-buffer-set-invalid chunk-name) (null (gethash chunk-name (act-r-buffer-chunk-set buffer))))
                                  (copy-chunk-fct chunk-name)
                                chunk-name)))
               (when (and (show-copy-buffer-trace) (not (eq copy-name chunk-name)))
                 (schedule-event-relative 0 'show-buffer-copy :maintenance t :module 'buffer 
                                          :priority :max 
                                          :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string copy-name)) 
                                          :output 'medium))
               (setf (act-r-buffer-chunk buffer) copy-name))))))))


(defun schedule-overwrite-buffer-chunk (buffer-name chunk-name time-delta &key (module :none) (priority 0) (output 'low) (requested nil))
  (verify-current-mp  
   "overwrite-buffer-chunk called with no current meta-process."
   (verify-current-model
    "overwrite-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            ((not (numberp time-delta))
             (print-warning "schedule-overwrite-buffer-chunk called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta  'overwrite-buffer-chunk 
                                      :module module
                                      :priority priority 
                                      :params (if requested
                                                  (list buffer-name chunk-name)
                                                (list buffer-name chunk-name :requested requested))
                                      :output output)))))))


(defun module-warning (buffer-name chunk-type)
  (verify-current-mp  
   "module-warning called with no current meta-process."
   (verify-current-model
    "module-warning called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "module-warning called with an invalid buffer name ~S" buffer-name))
            ((null (chunk-type-p-fct chunk-type))
             (print-warning "module-warning called with an invalid chunk-type ~S" chunk-type))
            (t
             (warn-module (act-r-buffer-module buffer) buffer-name chunk-type)))))))


(defun require-module-warning? (buffer-name)
  (verify-current-mp  
   "require-module-warning? called with no current meta-process."
   (verify-current-model
    "require-module-warning? called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "require-module-warning? called with an invalid buffer name ~S" buffer-name))
            (t
             (warn-module? (act-r-buffer-module buffer))))))))


(defun module-request (buffer-name chunk-spec)
  (verify-current-mp  
   "module-request called with no current meta-process."
   (verify-current-model
    "module-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "module-request called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-chunk-spec-p chunk-spec))
             (print-warning "module-request called with an invalid chunk-spec ~S" chunk-spec))
            (t
             (request-module (act-r-buffer-module buffer) buffer-name chunk-spec)))))))


(defun schedule-module-request (buffer-name chunk-spec time-delta &key (module :none) (priority 0) (output 'medium) (details nil))
  (verify-current-mp  
   "schedule-module-request called with no current meta-process."
   (verify-current-model
    "schedule-module-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-module-request called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-chunk-spec-p chunk-spec))
             (print-warning "schedule-module-request called with an invalid chunk-spec ~S" chunk-spec))
            ((not (numberp time-delta))
             (print-warning "schedule-module-request called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-module-request called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta  'module-request 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name chunk-spec)
                                      :details
                                      (if (stringp details)
                                          details
                                        (concatenate 'string (symbol-name 'module-request) " " (symbol-name buffer-name)))
                                      :output output)))))))

(defun module-mod-request (buffer-name modification &optional (verify t))
  (verify-current-mp  
   "module-mod-request called with no current meta-process."
   (verify-current-model
    "module-mod-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "module-mod-request called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-chunk buffer))
             (print-warning "module-mod-request called with no chunk in buffer ~s" buffer-name))
            ((and verify 
                  (null (valid-chunk-modification (act-r-buffer-chunk buffer) modification)))
             (print-warning "module-mod-request called with an invalid modification ~S" modification))
            (t
             (buffer-mod-module (act-r-buffer-module buffer) buffer-name modification)))))))

(defun schedule-module-mod-request (buffer-name modification time-delta &key (module :none) (priority 0) (output 'medium) (verify t))
  (verify-current-mp  
   "schedule-module-mod-request called with no current meta-process."
   (verify-current-model
    "schedule-module-mod-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-module-mod-request called with invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-chunk buffer))
             (print-warning "schedule-module-mod-request called with no chunk in buffer ~s" buffer-name))
            ((and verify
                  (null (valid-chunk-modification (act-r-buffer-chunk buffer) modification)))
             (print-warning "schedule-module-mod-request called with an invalid modification ~S" modification))
            ((not (numberp time-delta))
             (print-warning "~s called with a non-number time-delta: ~S" 'schedule-module-mod-request time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-module-mod-request called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta  'module-mod-request 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name modification verify)
                                      :details
                                      (concatenate 'string (symbol-name 'module-mod-request) " " (symbol-name buffer-name))
                                      :output output)))))))


(defun mod-buffer-chunk (buffer-name modifications)
  (verify-current-mp  
   "mod-buffer-chunk called with no current meta-process."
   (verify-current-model
    "mod-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "mod-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-chunk buffer))
             (print-warning "mod-buffer-chunk called with no chunk in buffer ~s" buffer-name))
            ((null (valid-chunk-modification (act-r-buffer-chunk buffer) modifications))
             (print-warning "mod-buffer-chunk called with an invalid modification ~S" modifications))
            (t 
             (fast-mod-chunk-fct (act-r-buffer-chunk buffer) modifications)))))))
            

(defun schedule-mod-buffer-chunk (buffer-name modifications time-delta &key (module :none) (priority 0) (output 'low))
  (verify-current-mp  
   "schedule-mod-buffer-chunk called with no current meta-process."
   (verify-current-model
    "schedule-mod-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-mod-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-buffer-chunk buffer))
             (print-warning "schedule-mod-buffer-chunk called with no chunk in buffer ~s" buffer-name))
            ((null (valid-chunk-modification (act-r-buffer-chunk buffer) modifications))
             (print-warning "schedule-mod-buffer-chunk called with an invalid modification ~S" modifications))
            ((not (numberp time-delta))
             (print-warning "schedule-mod-buffer-chunk called with non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-mod-buffer-chunk called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta  'mod-buffer-chunk 
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name modifications)
                                      :details 
                                      (concatenate 'string (symbol-name 'mod-buffer-chunk) " " (symbol-name buffer-name))
                                      :output output)))))))
 
  
(defun valid-chunk-modification (chunk-name modifications)
  (let ((c (get-chunk chunk-name)))
    (when c
      (if (oddp (length modifications))
          nil
        (let ((slots nil)
              (slots-and-values nil))
          (do ((s modifications (cddr s)))
              ((null s))
            (push (car s) slots)
            (push (list (car s) (second s)) slots-and-values))
          (and (every #'(lambda (slot)
                          (valid-slot-name slot (act-r-chunk-chunk-type c)))
                      slots)
               (= (length slots) (length (remove-duplicates slots)))))))))

   
(defun buffer-spread (buffer-name)
  (verify-current-mp  
   "buffer-spread called with no current meta-process."
   (verify-current-model
    "buffer-spread called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "buffer-spread called with an invalid buffer name ~S" buffer-name))
            (t
             (car (no-output (sgp-fct (list (act-r-buffer-parameter-name buffer)))))))))))


(defun buffers-module-name (buffer-name)
  (verify-current-mp  
   "buffers-module-name called with no current meta-process."
   (verify-current-model
    "buffers-module-name called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "invalid buffer name ~S" buffer-name))
            (t
             (let ((module (act-r-buffer-module buffer)))
               (if module
                   module
                 (print-warning "Could not find a module for buffer ~S" buffer-name)))))))))

(defun searchable-buffer (buffer-name)
  (verify-current-mp  
   "searchable-buffer called with no current meta-process."
   (verify-current-model
    "searchable-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "invalid buffer name ~S" buffer-name))
            (t
             (act-r-buffer-searchable buffer)))))))

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
