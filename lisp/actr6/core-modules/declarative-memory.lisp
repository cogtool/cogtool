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
;;; Filename    : declarative-memory.lisp
;;; Version     : 1.1
;;; 
;;; Description : Implements the declarative memory module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Better storage of activation quantities (computed act.,
;;;             :     base-level, and Sji's) for reference since right now
;;;             :     it's all "on the fly" and only available in the trace.
;;;             : [ ] Consider a hook before the module merges cleared buffer
;;;             :     chunks into DM so that someone could have a chance to
;;;             :     touch them first, or maybe prioritize the notify fn's
;;;             :     so that people could hook into things better there.
;;; 
;;; ----- History -----
;;;
;;; 2004.10.15 Dan
;;;             : Creation.
;;; 2004.12.06 Dan
;;;             : Fixed some issues with the hook fns when reset.
;;;             : Brought the line length down to 80.
;;; 2004.12.18 Dan
;;;             : Added the tracing info.
;;; 2005.01.09 Dan
;;;             : Playing around with some optimization ideas for merging
;;;             : since that really kills models with lots of buffer action...
;;;             :
;;;             : The first thought is to hash the chunk contents - the down
;;;             : side is that any mod-chunk to a chunk already in DM is going
;;;             : to cause problems with that.
;;;             : 
;;;             : Add the chunk-hash-table to dm structure.
;;;             : Modify the add-chunk-into-dm and merge-chunk-into-dm
;;;             : to take advantage of that.
;;;             :
;;;             : Seems to work and cuts zbrodoff time almost in half.
;;;             :
;;;             : So, adding it as an option with a new parameter for those
;;;             : that still want to mod-chunk DM.
;;; 2005.01.12 Dan
;;;             : * Updated version to 1.0a2
;;;             : * Shifted the commands to a dm-commands file in tools leaving
;;;             :   this file as solely responsible for the module interface.
;;; 2005.01.17 Dan
;;;             : * Removed calls to format in the scheduling functions.
;;; 2005.01.18 Dan
;;;             : * Added the ability to reset the dm finsts by passing reset
;;;             :   as the value of the :recently-retrieved request parameter.
;;; 2005.01.21 Dan
;;;             : * Did some minor optimizing by having DM organize its chunks
;;;             :   by chunk-type.  That seemed to be the bottle neck in most
;;;             :   of the tutorial models, so trying to speed that up some. 
;;;             : * Don't worry about setting fan unless spreading activation
;;;             :   is enabled.
;;; 2005.01.26 Dan
;;;             : * Changed chunk-base-level usage so that it gets set every
;;;             :   time a base-level is computed regardless of circumstances.
;;;             : * Added the spource-spread parameter to chunks to hold the
;;;             :   activation spread component of the computation.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium" and ":output 'low" to some of the 
;;;             :   events that are scheduled to play friendly with the 
;;;             :   new detail level.
;;; 2005.02.04 Dan
;;;             : * Taking advantage of the fast-* chunk accessors.
;;; 2005.02.10 Dan
;;;             : * Switched to the use of expt-coerced, exp-coerced, and 
;;;             :   log-coerced.
;;; 2005.04.01 Dan
;;;             : * Changed the fast-mergeing mechanism to work over chunks
;;;             :   that have merged (but unchanged) chunks in their slots
;;;             :   by using true-chunk-name hash-chunk-contents.
;;; 2005.04.14 Dan
;;;             : * Fixed the parameter setting for the hook functions so that
;;;             :   a value of nil clears it completely.
;;; 2005.04.23 Dan
;;;             : * Added recently-retrieved as a possible query and added
;;;             :   a buffer-status function to show it.
;;;             : * However, the query may not be what people want - the
;;;             :   query returns whether the chunk in the buffer has a
;;;             :   declarative-finst on it at the time of the query which 
;;;             :   may not be the same as when the request was made, but
;;;             :   this seems like the "right" thing to return.
;;; 2005.06.10 Dan
;;;             : * Fixed a bug - terminating an ongoing retrieval didn't
;;;             :   properly clear the events if they had already been 
;;;             :   scheduled to occur.
;;; 2005.06.14 Dan
;;;             : * Fixed an issue with start-retrieval related to :recently-
;;;             :   retrieved reset because what happened is that it cleared
;;;             :   the finsts, but then wanted to find one that had a finst
;;;             :   that matched "reset" which always resulted in failure.
;;; 2005.06.16 Dan
;;;             : * Changed the reference count to only store as many as
;;;             :   are needed...
;;; 2005.06.28 Dan
;;;             : * Removed the :sa parameter and instead changed :mas to
;;;             :   act as both the switch and the value.
;;; 2005.07.29 Dan
;;;             : * Fixed base-level-activation so that if it tries to compute
;;;             :   the activation for a chunk with no references (not yet in
;;;             :   DM) for some reason it prints a warning and returns a very
;;;             :   large negative value.
;;; 2005.08.01 Dan
;;;             : * Fixed a bug that resulted in BLC being added repeatedly
;;;             :   to the activation value.
;;;             :   Side effect is that right now sdp doesn't show the base-
;;;             :   level value (other than the blc) when bll is on.
;;;             :   See the Todo above...
;;;             : * Updated the version to 1.0.
;;; 2005.08.02 Dan
;;;             : * Modified the priority of start-retrieval so that it doesn't
;;;             :   start until the goal buffer has been set.
;;; 2006.06.01 Dan
;;;             : * Added a note to the to do list as a reminder of some thoughts
;;;             :   on an issue that someone might want/need in the future 
;;;             :   (dealing with chunks before they merge into DM) based on 
;;;             :   Mike's intrest in it now.
;;; 2006.07.10 Dan
;;;             : * Changed a call to true-chunk-name to true-chunk-name-fct in
;;;             :   hash-chunk-contents because true-chunk-name is now the macro.
;;; 2006.07.12 Dan
;;;             : * Actually removed the :sa parameter to get rid of the warning.
;;; 2006.08.08 Dan
;;;             : * Fixed spreading-activation so that it doesn't print a warning
;;;             :   when a buffer that should spread activation is empty.
;;; 2006.08.30 Dan
;;;             : * Fixed a bug when partial matching is on - it wasn't taking
;;;             :   the comparison tests (>, <, >=, and <=) into account for a
;;;             :   retrieval request!
;;;             : * Also removed some of the comments that were no longer valid
;;;             :   which were there from the "sample" module details in the
;;;             :   old framework spec.
;;;             : * Reordered how the activation trace prints for partial
;;;             :   matching so things are a bit cleaner when there's a hook.
;;; 2006.09.08 Dan
;;;             : * Changed some parameter checks from posnum to nonneg and
;;;             :   updated the warnings appropriately.
;;; 2006.11.28 Dan
;;;             : * Took an unnecessary get-module out of the query function.
;;; 2006.11.29 Dan
;;;             : * The :pm parameter is now depricated - use :mp as both the
;;;             :   flag and value like :bll and :mas.
;;; 2006.11.30 Dan
;;;             : * Removed the fan parameter from chunks since it wasn't used
;;;             :   for anything now (the fan-list is what's important).
;;;             : * Updated chunks-similarity to use chunk-slot-equal instead
;;;             :   of equal for comparing non-chunk values.
;;; 2006.12.01 Dan
;;;             : * Cleaned up some comments in/around the base-level calculation.
;;; 2006.12.04 Dan
;;;             : * Added the last-base-level chunk parameter and changed the default
;;;             :   for the base-level chunk parameter to nil.
;;;             : * Modified the base-level calculation to set last-base-level and
;;;             :   so that the user setting overrides the :blc when :bll is nil
;;;             :   instead of being addative (makes it like ACT-R 4/5 now).
;;; 2006.12.05 Dan
;;;             : * Minor formatting changes - no real change.
;;; 2006.12.06 Dan
;;;             : * Added the retrieval-activation and retrieval-time 
;;;             :   parameters to the chunk to record the activation value
;;;             :   that it had during a retrieval request and the time at which
;;;             :   that request occured.
;;;             : * Updated the version to 1.1.
;;; 2007.08.15 Dan
;;;             : * Changed start-retrieval so that the chunk which will be
;;;             :   retrieved is the first element in the list passed to the
;;;             :   retrieval-set-hook function.
;;; 2008.02.27 Dan
;;;             : * Changed the parameter check to only throw the warning
;;;             :   when the "critical" parameters are adjusted.
;;; 2008.07.12 Dan 
;;;             : * Changed the text printed for the activation trace during
;;;             :   similarity calculations to avoid the potential confusion
;;;             :   that the similarity values are actually changing.
;;; 2008.07.12 Dan
;;;             : * Added the :w-hook parameter to allow one to differentially
;;;             :   specify the Wkj values.
;;; 2008.07.23 Dan
;;;             : * Added call to the new register-subsymbolic-parameters to
;;;             :   note which declarative parameters should trigger the warning.
;;; 2008.08.14 Dan
;;;             : * Fixed a bug with the retrieval-set-hook - when there was a
;;;             :   retrieval failure it got called with (nil) instead of just
;;;             :   nil.
;;; 2008.11.13 Dan
;;;             : * Added a secondary reset function to set the :dcsc-hook
;;;             :   parameter so that fast-mergeing works right in the context
;;;             :   of chunk normalizing i.e. the module can rehash the chunks
;;;             :   that change.
;;; 2008.12.10 Dan
;;;             : * Changed how the fan values are recorded to improve both
;;;             :   space and time.  Instead of a list on the j's now it's 
;;;             :   split into a fan-out and a fan-in.  The fan-out is just a
;;;             :   count of the total fan 'out' of the chunk (the length of
;;;             :   the old fan list) and the fan-in is the list of the chunk's
;;;             :   slot contents at the time it entered DM with thier counts.
;;;             :   So, now it doesn't have to search or count item on the fan-list
;;;             :   to do the calculation.  It instead searchs the fan-in list
;;;             :   which presumably is shorter than the old fan-list for most
;;;             :   chunks in most models and even if its not it doesn't have
;;;             :   to do the counting every time since that was set initially.
;;;             : * It also now addresses the issue of chunks which merge into DM
;;;             :   after they have had references credited to them.  Previously,
;;;             :   those other references were lost if the chunk actually merged
;;;             :   with an existing chunk in DM, but now they get tracked 
;;;             :   appropriately.
;;; 2008.12.11 Dan
;;;             : * Fixed a bug with the last change - a chunk didn't get saved
;;;             :   on its own fan-in list.
;;;             : * There also isn't likely any space savings in general - only
;;;             :   when there's duplicate instances of a value in slots (a 
;;;             :   fan-in other than 1).
;;; 2009.08.05 Dan
;;;             : * Added a new hook for the activation calculations.  The
;;;             :   :activation-offsets hook holds a list of functions to call
;;;             :   during activation computation.  A function on the hook will 
;;;             :   be called with the chunk name during the activation calculation
;;;             :   and if the function returns a number that value will be
;;;             :   added to the activation value for the chunk.  
;;;             :   This hook will make it easier to extend the activation
;;;             :   calculation without having to completely redefine one of the
;;;             :   existing components via the other hooks.
;;; 2009.08.13 Dan
;;;             : * Added an optional parameter to chunks-similarity to control
;;;             :   whether or not the activation trace info is displayed since
;;;             :   it can also be called by the user in which case the trace
;;;             :   is undesirelable (and since it's model output can't be shut
;;;             :   off with no-output).
;;; 2009.09.09 Dan
;;;             : * With the addition of the multi-buffers and buffer sets DM
;;;             :   needs to flag any chunk it has as invalid for use in such a
;;;             :   set to avoid unintentional modification of DM chunks.
;;; 2010.08.30 Dan
;;;             : * Extending the :act parameter to take a level like trace-detail
;;;             :   so one can get a smaller set of info when desired.  High is
;;;             :   the same as t and nil is still off.  Medium turns off all of
;;;             :   the "doesn't match" indicators but otherwise shows everything
;;;             :   and low only shows the total activation.
;;; 2011.02.09 Dan
;;;             : * Added a merge-dm command which works like add-dm except that
;;;             :   the chunks are merged in as if cleared from a buffer at the
;;;             :   current time.  It merges them in an order that guarantees 
;;;             :   any chunks used in slots of other chunks get merged first,
;;;             :   but if there are circularities it just does them in the 
;;;             :   order provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The declarative memory module has one buffer called retrieval.
;;;
;;; The declarative memory module collects the chunks that have been cleared
;;; from all buffers.  It merges newly cleared chunks with those that have been
;;; previously cleared.  This set of chunks is refered to as the declarative
;;; memory (DM for short).  Requests to the declarative module are attempts to
;;; find a chunk in DM which matches the request.  If such a chunk is found
;;; it is placed into the retrieval buffer.  If no such chunk is found, then
;;; it reports an error state.  It can only process one request at a time.  If
;;; a new request comes in prior to the completion of a previous request the
;;; older request is terminated immediately. The timing of a request's
;;; completion along with how the matching chunk is found are controled by
;;; several parameters and the following equations:
;;;
;;;
;;; In addition, to that, there is one request parameter which may be used -
;;; :recently-retrieved.  It may be passed a value of t or nil.  The declarative
;;; memory module records which chunks it has returned as the result of a
;;; request and the recently-retrieved request parameter may be used to exclude
;;; chunks based on that information.  There are two parameters that control
;;; how the recently-retrieved designation occurs.  The :dm-finsts parameter
;;; indicates how many chunks will be marked as recently-retrieved and the
;;; :dm-finsts-decay parameter indicates for how many seconds each of those
;;; designations will persist.
;;;
;;; The declarative memory module does not support buffer modification requests.
;;;
;;; The declarative memory module responds to the required queries as follows:
;;;
;;; State free will respond t if there is no request pending or nil if there is
;;;       i.e. the module is not free between the time of a request and when
;;;       the chunk from that request is placed into the buffer.
;;; State busy will respond t if there is a pending request and nil if not.
;;; State error will respond with t if no chunk matching the most recent request 
;;;       could be found or nil otherwise.  The error t will not be indicated 
;;;       until after the time for failure has passed.
;;; Buffer stuffed will respond nil i.e. the declarative module never stuffs
;;;       the buffer.
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
;;; One thing that is going in from the beginning is lots of hooks into the
;;; equations.  Every component of the activation equation will have an
;;; "over-ride" function basically like the similarity-hook-fn of the 
;;; older system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Relies on :esc, :ol, and :er so make sure they exist first

(require-compiled "CENTRAL-PARAMETERS" "ACT-R6:support;central-parameters")

;;; Start by defining a structure to hold the instance of the module

(defstruct dm "an instance of the declarative memory module"
  (chunks (make-hash-table))  ; the set of chunks that are in declarative memory
  (busy nil)    ; record whether the module is busy
  (failed nil)  ; record whether the last request failed
  
  ;;; keep track of the central parameters internally for ease of access
  
  esc er ol
  
  ;;; slots for the various parameters from previous versions
  ;;; that control the subsymbolc
  
  blc ans pas lf le mp ms md rt bll mas 
  
  ;;; add a new parameter to act as a switch for spreading
  ;;; since relying on ga to be zero won't work
  
  sa
    
  ;;; only one of the old traces still really matters
  ;;; or in some cases is even possible - basically show all
  ;;; components that are enabled with this switch
  
  act
  
  ;;; parameters for the declarative finsts count and duration 
  
  num-finsts finst-span
  
  ;;; a list to hold the finsts
  
  finsts
  
  ;;; replace the global hook functions with parameters
  ;;; 
  ;;; Start with the "low-level" hooks that over-ride
  ;;; internal values
  
  sim-hook ;; the old similarity hook 
           ;; called with the two values and returns similarity or nil
  sji-hook ;; same thing now for Sji values - passed the two chunk names 
  
  w-hook   ;; Allows one to override the Wkj values in spreading
           ;; activation.
  
  ;;; Higher level hooks over-ride the entire internal
  ;;; computiation for each component 
  ;;; the chunk name is what gets passed to all
  
  bl-hook         ;; let the user redefine the base-level computation
  spreading-hook  ;; let the user redefine the spreading component
  partial-matching-hook ;; redefinition of the partial matching component
  noise-hook      ;; redefine the transient noise computation
  
  offsets         ;; adds optional additional components to the
                  ;; activation equation
  
  ;;; some retrieval hooks like the conflict resolution
  ;;; system has.  Could get some of this from the main
  ;;; event hooks, but seems cleaner to just hook where
  ;;; one wants when possible.  All of these parameters
  ;;; have a list of functions internally which get called.
  ;;; Each setting is pushed onto that list.  No way to remove
  ;;; one other than through a reset.  This is so something
  ;;; like the environment can add such a hook without the
  ;;; user being able to "break" things.
  
  retrieval-request-hook ;; called at the initation of the request with the spec
  
  retrieval-set-hook     ;; called with the chunks that matched the spec. The
                         ;; activations have already been computed but a non-nil
                         ;; return overrides that - like the conflict-set hook.
                         ;; If the return value is a cons of chunk-name and time
                         ;; that is used instead of the computation and if it is
                         ;; just a number, then a failure is scheduled with that
                         ;; latency.
                         ;; If more than one returns a non-nil a warning is
                         ;; signaled and none of those effects occur.
  
  retrieved-chunk-hook   ;; called with the retrieved chunk or nil on failure.
                         ;; The call occurs at the time of the actual retrieval
                         ;; but before the buffer setting - the general event 
                         ;; hook should be used to detect that.
  
  
  ;;; a couple of hooks to support user extensions
  
  chunk-merge-hook  ;; called after a chunk has been merged into dm
  chunk-add-hook    ;; called after a chunk has been newly added to dm
  
  
  ;;; A hash-table of chunks in DM referenced by contents
  ;;; to speed the merging calculation
  
  chunk-hash-table
  
  ;;; A parameter to control whether the hash-table based merging is used
  
  fast-merge
  
  )


;;; Add the necessary parameters to the chunk definitions

;;; Set to the computed value when necessary

(extend-chunks activation :default-value 0)



;;; Record the chunks in which a chunk occurs

;;; Splitting this into a fan-in and a fan-out now
;;; to potentially save time and space in the computation
;;; and storage.
;;; Also fixes the 'bug' that the older version ignored -
;;; chunk merging of sources after a chunk which uses
;;; the source has also merged.

(extend-chunks in-dm :default-value nil)

(defun merge-fan-outs (c1 c2)
  (+ (chunk-fan-out c1) (chunk-fan-out c2)))

(extend-chunks fan-out :default-value 0 :merge-function merge-fan-outs)

(extend-chunks c-fan-out :default-value 0 :copy-from-chunk-function chunk-fan-out)

(extend-chunks fan-in :default-value nil)


;;; Not really going to be "creation" time but entry to DM time
;;; which is what will be used in the computations.

(extend-chunks creation-time :default-value 0)


;;; This holds the user set base-level which is only meaningful when
;;; :bll is nil.

(extend-chunks base-level :default-value nil
               :copy-function identity)

;;; This holds the last computed base-level value

(extend-chunks last-base-level :default-value 0
               :copy-function identity)


;;; holds the computed spreading activation component of the chunk

(extend-chunks source-spread :default-value 0
               :copy-function identity)


;;; Merging results in one new reference for the "existing" chunk

(defun merge-reference-list (chunk1 chunk2)
  (declare (ignore chunk2))
  (let* ((dm (get-module declarative))
         (ol (dm-ol dm)))
    (cond ((null ol)
           (cons (mp-time) (chunk-reference-list chunk1)))
          ((eq ol t)
           nil)
          (t ;; ol is a number
           (subseq (cons (mp-time) (chunk-reference-list chunk1))
                   0 (min ol (1+ (length (chunk-reference-list chunk1)))))))))

(defun merge-reference-count (chunk1 chunk2)
  (declare (ignore chunk2))
  (+ 1 (chunk-reference-count chunk1)))


(extend-chunks reference-list :default-value nil
               :copy-function copy-list
               :merge-function merge-reference-list)

(extend-chunks reference-count :default-value 0
               :copy-function identity
               :merge-function merge-reference-count)

;;; Keep the similarities with the chunks
;;; at least for now.

(extend-chunks similarities :default-value nil
               :copy-function copy-tree)

;;; compute the permanent noise as needed

(defun default-permanent-noise (chunk)
  (declare (ignore chunk))
  
  (let ((dm (get-module declarative)))
    (if (and dm (dm-pas dm))
        (act-r-noise (dm-pas dm))
      0.0)))

(extend-chunks permanent-noise :default-function
               default-permanent-noise
               :copy-function identity)


;;; store user define Sji values with the chunk

(extend-chunks sjis :default-value nil
               :copy-function copy-tree)


;;; store the last activation used in a retrieval request and
;;; the time that request occured

(extend-chunks retrieval-activation :default-value nil)
(extend-chunks retrieval-time :default-value nil)


;;; fast-merge assumption violated by :dcnn so need
;;; to be able to rehash when that happens

(extend-chunks fast-merge-key)

(defun dm-fm-rh (chunk)
  (awhen (chunk-fast-merge-key chunk)
         (let ((dm (get-module declarative))
               (new-key (hash-chunk-contents chunk)))
           (remhash it (dm-chunk-hash-table dm))
           (setf (chunk-fast-merge-key chunk) new-key)
           (setf (gethash new-key (dm-chunk-hash-table dm)) chunk))))

;;; A function for converting a chunk to a list of it's info

(defun hash-chunk-contents (chunk)
  (let* ((ct (chunk-chunk-type-fct chunk))
         (res (list ct)))
    (dolist (slot (chunk-type-slot-names-fct ct) res)
      (push (true-chunk-name-fct (fast-chunk-slot-value-fct chunk slot)) res))))



(defun reset-dm-module (dm)
  
  ;; Set all of the slots of this instance to their initial values.
  
  (clrhash (dm-chunks dm))
  (setf (dm-busy dm) nil)
  (setf (dm-failed dm) nil)
  
  (setf (dm-finsts dm) nil)
  
  ;; parameters will be handled on thier own
  
  
  ;;;
  (setf (dm-chunk-hash-table dm) 
    (make-hash-table :test #'equal))
  
  )

(defun secondary-reset-dm-module (dm)

  (sgp :dcsc-hook dm-fm-rh))



(defun dm-query-request (dm buffer slot value)
  (case slot
    (state
     (case value
       (busy
        (dm-busy dm))
       (free
        (not (dm-busy dm)))
       (error
        (dm-failed dm))
       (t (print-warning  
           "Invalid query made of the ~S buffer with slot ~S and value ~S" 
           buffer slot value))))
    (recently-retrieved
       (setf (dm-finsts dm)
         (remove-if #'(lambda (time)
                        (> (- (mp-time) time)
                           (dm-finst-span dm)))
                    (dm-finsts dm) :key #'cdr))
       
       (and (buffer-read buffer)
            (chunk-copied-from-fct (buffer-read buffer))
            (if value 
                (find (chunk-copied-from-fct (buffer-read buffer))
                      (dm-finsts dm)
                      :key #'car)
              (not (find (chunk-copied-from-fct (buffer-read buffer))
                         (dm-finsts dm)
                         :key #'car)))))))
    
    


(defun dm-request (dm buffer request)
  (declare (ignore buffer)) ;; It is always going to be retrieval
  
  ;; If the module has not completed the last request
 
  (when (dm-busy dm)
    
    ;; Report a warning about that and remove the unexecuted event 
    ;; from the queue.
    
    (model-warning "A retrieval event has been aborted by a new request")
    (delete-event (dm-busy dm)))
  
  ;; Clear the failed attempt flag of the module
  
  (setf (dm-failed dm) nil)
  
  ;; Schedule an event to start the retrieval at the current time
  ;; but with a priority of -2000 and save that as the busy flag
  ;; instead of immediately attempting the retrieval.
  
  ;; Not important for this demonstration, but in the context of 
  ;; a request being made from the RHS of a production this would be
  ;; important to ensure that any buffer modifications have had a chance
  ;; to occur so that the "correct" sources are used for activation spreading.
  
  (setf (dm-busy dm)
    (schedule-event-relative 0 'start-retrieval
                             :module 'declarative
                             :destination 'declarative
                             :details (symbol-name 'start-retrieval)
                             :priority -2000
                             :params (list request)
                             :output 'medium)))


(defun dm-act-level (act level)
  (and act
       (case level
         (high (or (eq act 'high) (eq act t)))
         (medium (not (eq act 'low)))
         (low t))))
                  
;;; Start-retrieval
;;;
;;; This function is called to actually attempt a retrieval.
;;;
;;; The parameters it receives are an instance of the module and the 
;;; chunk-spec of the request.
;;;
;;; It either schedules the setting of the retrieval buffer or indication of
;;; an error depending on whether or not it finds a chunk that matches the
;;; request.
;;;
;;; There are several parameters that determine how the "best" matching chunk
;;; is selected and how long that action will take.


(defun start-retrieval (dm request)
  
  (dolist (x (dm-retrieval-request-hook dm))
    (funcall x request))
  
  (let* ((ct (chunk-spec-chunk-type request))
         (chunk-list (apply #'append 
                            (mapcar #'(lambda (x) 
                                        (gethash x (dm-chunks dm)))
                              (chunk-type-subtypes-fct ct)))))
        
    (when (member :recently-retrieved (chunk-spec-slots request))
      (let ((recent (chunk-spec-slot-spec request :recently-retrieved)))
        (cond ((> (length recent) 1)
               (print-warning "Invalid retrieval request.")
               (print-warning 
                ":recently-retrieved parameter used more than once.")
               (return-from start-retrieval))
              ((not (or (eq '- (caar recent)) (eq '= (caar recent))))
               (print-warning "Invalid retrieval request.")
               (print-warning 
                ":recently-retrieved parameter's modifier can only be = or -.")
               (return-from start-retrieval))
              ((not (or (eq t (third (car recent)))
                        (eq nil (third (car recent)))
                        (and (eq 'reset (third (car recent)))
                             (eq '= (caar recent)))))
               (print-warning "Invalid retrieval request.")
               (print-warning 
                ":recently-retrieved parameter's value can only be t, nil or reset.")
               (return-from start-retrieval))
              
              (t ;; it's a valid request
               
               ;; remove any old finsts 
               
               (setf (dm-finsts dm)
                 (remove-if #'(lambda (time)
                                (> (- (mp-time) time)
                                   (dm-finst-span dm)))
                            (dm-finsts dm) :key #'cdr))
               
               (if (eq 'reset (third (car recent)))
                 (setf (dm-finsts dm) nil)
               
                 (cond ((or (and (eq t (third (car recent)))   ;; = request t
                                 (eq (caar recent) '=)) 
                            (and (null (third (car recent)))   ;; - request nil
                                 (eq (caar recent) '-)))
                        
                        ;; only those chunks marked are available
                        (setf chunk-list (mapcar #'car (dm-finsts dm)))
                        (when (dm-act-level (dm-act dm) 'high)
                          (model-output "Only recently retrieved chunks: ~s" chunk-list)))
                       (t
                        ;; simply remove the marked items
                        ;; may be "faster" to do this later
                        ;; once the set is trimed elsewise, but
                        ;; for now keep things simple
                        
                        (when (dm-act-level (dm-act dm) 'high)
                          (model-output "Removing recently retrieved chunks:"))
                        
                        (setf chunk-list 
                          (remove-if #'(lambda (x)
                                         (when (member x (dm-finsts dm) 
                                                       :key #'car
                                                       :test #'eq-chunks-fct)
                                           (when (dm-act-level (dm-act dm) 'high)
                                             (model-output "~s" x))
                                           t))
                                     chunk-list))))))))
      
      (setf request (strip-request-parameters-from-chunk-spec request)))
    
    (let ((best-val nil)
          (best nil)
          (return-val nil)
          (chunk-set 
           (cond ((or (null (dm-esc dm)) (null (dm-mp dm)))
                  ;; for tracing purposes should probably try them all
                  ;; individually instead of doing it in one call
                  ;(find-matching-chunks request :chunks chunk-list)
                  
                  (let ((found nil))
                    (dolist (name chunk-list found)
                      (if (match-chunk-spec-p name request)
                          (progn
                            (when (dm-act-level (dm-act dm) 'medium)
                              (model-output "Chunk ~s matches" name)) 
                            (push-last name found))
                        (when (dm-act-level (dm-act dm) 'high)
                          (model-output "Chunk ~s does not match" name)))))
                  )
                 (t
                  ;; with esc and pm on then want to use
                  ;; everything that fits the general pattern:
                  ;; correct type
                  ;; slots with a binding not nil
                  ;; empty slots are empty
                  ;; >, <, >=, and <= tests met
                  
                  (find-matching-chunks 
                   (define-chunk-spec-fct 
                       (append (list 'isa (chunk-spec-chunk-type request))
                               (mapcan #'(lambda (x)
                                           (cond ((eq (car x) '=)
                                                  (if (third x)
                                                      (list '- (second x) nil)
                                                    (list '= (second x) nil)))
                                                 ((eq (car x) '-)
                                                  (unless (third x) x))
                                                 ;;; make sure the comparison tests match
                                                 (t x)))
                                 (chunk-spec-slot-spec request))))
                   :chunks chunk-list)))))
      
            
      (if (dm-esc dm)
          (dolist (x chunk-set)
            (compute-activation dm x request)
            
            (setf (chunk-retrieval-activation x) (chunk-activation x))
            (setf (chunk-retrieval-time x) (mp-time))
            
            (cond ((null best-val)
                   (setf best-val (chunk-activation x))
                   (push x best)
                   (when (dm-act-level (dm-act dm) 'medium)
                     (model-output "Chunk ~s has the current best activation ~f" 
                                   x best-val)))
                  ((= (chunk-activation x) best-val)
                   (push x best)
                   (when (dm-act-level (dm-act dm) 'medium)
                     (model-output 
                      "Chunk ~s matches the current best activation ~f" 
                      x best-val)))
                  ((> (chunk-activation x) best-val)
                   (setf best-val (chunk-activation x))
                   (setf best (list x))
                   (when (dm-act-level (dm-act dm) 'medium)
                     (model-output 
                      "Chunk ~s is now the current best with activation ~f" 
                      x best-val)))))
        (setf best chunk-set))
      
      (when (> (length best) 1)
        (if (dm-er dm)
            (let ((b (random-item best)))
              (setf best (cons b (remove b best))))
          (setf best (sort (copy-list best) #'string<))))
      
      
      (when (car (dm-retrieval-set-hook dm))
        (let ((chunk-set-with-best (when best (cons (car best) (remove (car best) chunk-set)))))
          (dolist (x (dm-retrieval-set-hook dm))
            (let ((val (funcall x chunk-set-with-best)))
              (when val
                (if return-val
                    (progn 
                      (print-warning 
                       "multiple set-hook functions returned a value - none used")
                      (setf return-val :error))
                  (setf return-val val)))))))
      
      (cond ((consp return-val)
             (setf (dm-busy dm) (schedule-event-relative (cdr return-val) 'retrieved-chunk
                                                         :module 'declarative 
                                                         :destination 'declarative 
                                                         :params (list (car return-val))
                                                         :details (concatenate 'string
                                                                    (symbol-name 'retrieved-chunk)
                                                                    " "
                                                                    (symbol-name (car return-val)))
                                                         :output 'medium))
             
             (when (dm-act-level (dm-act dm) 'low)
                     (model-output 
                      "Retrieval-set-hook funciton forced retrieval of" 
                      (car return-val))))
            
            ((numberp return-val)
             (setf (dm-busy dm) (schedule-event-relative return-val 'retrieval-failure
                                                         :module 'declarative 
                                                         :destination 'declarative 
                                                         :output 'low))
             (when (dm-act-level (dm-act dm) 'low)
                     (model-output 
                      "Retrieval-set-hook funciton forced retrieval failure")))
            
            ((or (null best) 
                 (and (dm-esc dm)
                      (< best-val (dm-rt dm))))
             (setf (dm-busy dm) (schedule-event-relative 
                                 (if (dm-esc dm)
                                     (compute-activation-latency dm (dm-rt dm))
                                   0)
                                 'retrieval-failure
                                 :module 'declarative 
                                 :destination 'declarative
                                 :output 'low))
               
             (when (and (dm-act-level (dm-act dm) 'low) (null best))
               (model-output 
                "No matching chunk found retrieval failure"))
             
             (when (and (dm-act-level (dm-act dm) 'low) best)
               (model-output 
                "No chunk above the retrieval threshold: ~f" (dm-rt dm)))
             )
            
            ((= (length best) 1)
             (setf (dm-busy dm) (schedule-event-relative 
                                 (if (dm-esc dm)
                                     (compute-activation-latency dm (chunk-activation (car best)))
                                   0)
                                 'retrieved-chunk
                                 :module 'declarative 
                                 :destination 'declarative 
                                 :params best
                                 :details 
                                 (concatenate 'string
                                   (symbol-name 'retrieved-chunk)
                                   " "
                                   (symbol-name (car best)))
                                 :output 'medium))
             (when (dm-act-level (dm-act dm) 'low)
               (model-output 
                "Chunk ~s with activation ~f is the best"
                (car best) (chunk-activation (car best)))))
            (t
             (let ((best1 (car best)))
               
               (setf (dm-busy dm) (schedule-event-relative 
                                   (if (dm-esc dm)
                                       (compute-activation-latency dm (chunk-activation best1))
                                     0)
                                   'retrieved-chunk
                                   :module 'declarative 
                                   :destination 'declarative 
                                   :params (list best1)
                                   :details 
                                   (concatenate 'string
                                     (symbol-name 'retrieved-chunk)
                                     " "
                                     (symbol-name best1))
                                   :output 'medium))
               
               (when (dm-act-level (dm-act dm) 'low)
                 (model-output 
                  "Chunk ~s chosen among the chunks with activation ~f"
                  best1 (chunk-activation best1)))))))))
  

;;; Retrieved-chunk
;;;
;;; Called as an event when a chunk has been retrieved and is ready to be placed
;;; into the buffer.
;;;
;;; The parameters are an instance of the module and the name of the chunk 
;;; to put in the buffer.

(defun retrieved-chunk (dm chunk)
  
  ;; Clear the busy flag
  
  (setf (dm-busy dm) nil)
  
  
  (when (car (dm-retrieved-chunk-hook dm))
    (dolist (x (dm-retrieved-chunk-hook dm))
      (funcall x chunk)))
  
  ;; Schedule an event to put the chunk into the buffer right now instead of
  ;; placing it there directly to comply with the guideline that buffer changes
  ;; should be scheduled.
  
  (schedule-set-buffer-chunk 'retrieval chunk 0 
                             :module 'declarative 
                             :priority :max)
  
  ;; update the marker for having retrieved this chunk
  
  (update-declarative-finsts dm chunk)
  )


(defun update-declarative-finsts (dm chunk)
  (setf (dm-finsts dm) (remove chunk (dm-finsts dm) :key #'car 
                               :test #'eq-chunks-fct))
  (push (cons chunk (mp-time)) (dm-finsts dm))
  (setf (dm-finsts dm) (subseq (dm-finsts dm) 0 (min (length (dm-finsts dm))
                                                     (dm-num-finsts dm)))))

;;; Retrieval-failure
;;;
;;; Called as an event when a chunk failed to be found in response to a request.
;;;
;;; The parameter is an instance of the module.

(defun retrieval-failure (dm)
  
  ;; Clear the busy flag and set the failure flag.
  
  (setf (dm-busy dm) nil)
  
  (when (car (dm-retrieved-chunk-hook dm))
    (dolist (x (dm-retrieved-chunk-hook dm))
      (funcall x nil)))
  
  (setf (dm-failed dm) t))


;;; Dm-params
;;;

(defun dm-params (dm param)
  (cond ((consp param)
         
         (when (and (hash-table-keys (dm-chunks dm))
                    (find (car param) '(:esc :ol :fast-merge :mp :pm :bll :mas)))
           (print-warning 
            "Changing declarative parameters with chunks in dm not supported.")
           (print-warning 
            "Results may not be what one expects."))
         
         (case (car param)
           (:esc (setf (dm-esc dm) (cdr param)))
           (:er (setf (dm-er dm) (cdr param)))
           (:ol (setf (dm-ol dm) (cdr param)))
           
           (:blc (setf (dm-blc dm) (cdr param)))
           (:ans (setf (dm-ans dm) (cdr param)))
           (:pas (setf (dm-pas dm) (cdr param)))
           (:lf (setf (dm-lf dm) (cdr param)))
           (:le (setf (dm-le dm) (cdr param)))
           (:mp (setf (dm-mp dm) (cdr param)))
           (:ms (setf (dm-ms dm) (cdr param)))
           (:md (setf (dm-md dm) (cdr param)))
           (:rt (setf (dm-rt dm) (cdr param)))
           (:bll (setf (dm-bll dm) (cdr param)))
           (:mas (setf (dm-mas dm) (cdr param))
                 (setf (dm-sa dm) (cdr param)))
           (:pm (setf (dm-mp dm) 
                  (if (cdr param) 
                      (progn
                        (model-warning "The :pm parameter is now depricated.  Like :bll and :mas, the :mp parameter is now both a flag and a value.")
                        (model-warning "Setting :pm will change :mp to 1.0.")
                        1.0)
                    nil)))
           
           (:act (setf (dm-act dm) (cdr param)))
           
           (:declarative-num-finsts (setf (dm-num-finsts dm) (cdr param)))
           (:declarative-finst-span (setf (dm-finst-span dm) (cdr param)))
                   
           (:sim-hook (setf (dm-sim-hook dm) (cdr param)))
           (:sji-hook (setf (dm-sji-hook dm) (cdr param)))
           (:w-hook (setf (dm-w-hook dm) (cdr param)))
                      
           (:bl-hook (setf (dm-bl-hook dm) (cdr param)))
           (:spreading-hook (setf (dm-spreading-hook dm) (cdr param)))
           (:partial-matching-hook (setf (dm-partial-matching-hook dm) 
                                     (cdr param)))
           (:noise-hook (setf (dm-noise-hook dm) (cdr param)))
           
           
           (:fast-merge (setf (dm-fast-merge dm) (cdr param)))

           (:activation-offsets
            (if (cdr param)
              (if (member (cdr param) (dm-offsets dm))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :activation-offsets
                 (cdr param))
                (push (cdr param) (dm-offsets dm)))
              (setf (dm-offsets dm) nil)))
           
           (:retrieval-request-hook 
            (if (cdr param)
              (if (member (cdr param) (dm-retrieval-request-hook dm))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :retrieval-request-hook
                 (cdr param))
                (push (cdr param) (dm-retrieval-request-hook dm)))
              (setf (dm-retrieval-request-hook dm) nil)))
           
           (:retrieval-set-hook 
            (if (cdr param)
                (if (member (cdr param) (dm-retrieval-set-hook dm))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :retrieval-set-hook
                     (cdr param))
                  (push (cdr param) (dm-retrieval-set-hook dm)))
              (setf (dm-retrieval-set-hook dm) nil)))
           
           (:retrieved-chunk-hook 
            (if (cdr param)
                (if (member (cdr param) (dm-retrieved-chunk-hook dm))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :retrieved-chunk-hook
                     (cdr param))
                  (push (cdr param) (dm-retrieved-chunk-hook dm)))
              (setf (dm-retrieved-chunk-hook dm) nil)))
           
           (:chunk-merge-hook 
            (if (cdr param)
                (if  (member (cdr param) (dm-chunk-merge-hook dm))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :chunk-merge-hook
                     (cdr param))
                  (push (cdr param) (dm-chunk-merge-hook dm)))
              (setf (dm-chunk-merge-hook dm) nil)))
           
           (:chunk-add-hook 
            (if (cdr param)
                (if (member (cdr param) (dm-chunk-add-hook dm))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :chunk-add-hook
                     (cdr param))
                  (push (cdr param) (dm-chunk-add-hook dm)))
              (setf (dm-chunk-add-hook dm) nil)))))
        (t 
         (case param
           
           (:blc (dm-blc dm))
           (:ans (dm-ans dm))
           (:pas (dm-pas dm))
           (:lf (dm-lf dm))
           (:le (dm-le dm))
           (:mp (dm-mp dm))
           (:ms (dm-ms dm))
           (:md (dm-md dm))
           (:rt (dm-rt dm))
           (:bll (dm-bll dm))
           (:mas (dm-mas dm))
           (:pm (if (dm-mp dm) t nil))
           (:sa (progn
                  (print-warning "The :SA parameter is no longer used") 
                  nil))
           (:act (dm-act dm))
           
           (:declarative-num-finsts (dm-num-finsts dm))
           (:declarative-finst-span (dm-finst-span dm))
        
           (:sim-hook (dm-sim-hook dm))
           (:sji-hook (dm-sji-hook dm))
           (:w-hook (dm-w-hook dm))
         
           (:bl-hook (dm-bl-hook dm))
           (:spreading-hook (dm-spreading-hook dm))
           (:partial-matching-hook (dm-partial-matching-hook dm))
           (:noise-hook (dm-noise-hook dm))
           
           (:activation-offsets (dm-offsets dm))
           
           (:fast-merge (dm-fast-merge dm))
           
           (:retrieval-request-hook (dm-retrieval-request-hook dm))
           (:retrieval-set-hook (dm-retrieval-set-hook dm))
           (:retrieved-chunk-hook (dm-retrieved-chunk-hook dm))
           
           (:chunk-merge-hook (dm-chunk-merge-hook dm))
           (:chunk-add-hook (dm-chunk-add-hook dm))))))





;;; Merge-chunk-into-dm
;;;
;;; This function will be called automatically each time a buffer is cleared.
;;;
;;; The parameters are an instance of the module, the name of the buffer that 
;;; was cleared, and the name of the chunk that was in the buffer.
;;;
;;; This module adds that chunk to declarative memory and increments its 
;;; reference count.  If a matching chunk already exists in declarative memory,
;;; then those chunks are merged together.  If this is the first occurrence of 
;;; the chunk, then its initial parameters are set accordingly.

(defun merge-chunk-into-dm (dm buffer chunk)
  (declare (ignore buffer))  ;; don't care which buffer it came from
  
  ;; Find any existing matching chunk

  (let ((existing 
         (if (dm-fast-merge dm)
             (gethash (hash-chunk-contents chunk) (dm-chunk-hash-table dm))
           (find chunk (gethash (chunk-chunk-type-fct chunk) (dm-chunks dm))
                 :test #'equal-chunks-fct))))
        
    (if existing
        (progn
          
          (merge-chunks-fct existing chunk)  ;; merging functions handle params
          
          (when (car (dm-chunk-merge-hook dm))
            (dolist (x (dm-chunk-merge-hook dm))
              (funcall x chunk))))
      
      ;; otherwise add it to the list
      
      (add-chunk-into-dm dm chunk))))

;; add-chunk-into-dm
;;;
;;; works like merge-chunk-into-dm but without doing any merging i.e. it
;;; makes the chunk part of dm and sets it's initial parameters regardless
;;; of whether it is a perfect match to an existing member
;;;

(defun add-chunk-into-dm (dm chunk)
  
  (push chunk (gethash (chunk-chunk-type-fct chunk) (dm-chunks dm)))
  
  (when (dm-fast-merge dm)
    (let ((key (hash-chunk-contents chunk)))
      (setf (chunk-fast-merge-key chunk) key)
      (setf (gethash key (dm-chunk-hash-table dm)) chunk)))
  
  ;; set the parameters
  
  (setf (chunk-in-dm chunk) t)
  
  (setf (chunk-creation-time chunk) (mp-time))
  (setf (chunk-reference-list chunk) (list (mp-time)))
  (setf (chunk-reference-count chunk) 1)
  
  ;; mark it as invalid for a buffer set now
  
  (setf (chunk-buffer-set-invalid chunk) t)
  
  ;; when spreading activation is on set the fan-out and fan-in values
  
  (when (dm-sa dm)
    
    ;; Increment its fan-out for itself
    
    (incf (chunk-fan-out chunk))
  
    ;; set the fan-in values
    
    (let ((new-fans (remove-if-not 'chunk-p-fct (mapcan #'(lambda (slot)
                                                            (when (chunk-p-fct 
                                                                   (fast-chunk-slot-value-fct chunk slot))
                                                              (list (fast-chunk-slot-value-fct chunk slot))))
                                                  (chunk-type-slot-names-fct 
                                                   (chunk-chunk-type-fct chunk))))))
      
      (dolist (j new-fans)
        (incf (chunk-fan-out j)))
      
      (setf (chunk-fan-in chunk)
        (mapcar (lambda (x) (cons x (count x new-fans))) (remove-duplicates new-fans))))
  
    (aif (assoc chunk (chunk-fan-in chunk))
         (incf (cdr it))
         (push (cons chunk 1) (chunk-fan-in chunk))))
  
  (when (car (dm-chunk-add-hook dm))
    (dolist (x (dm-chunk-add-hook dm))
      (funcall x chunk))))


;;; Add-dm
;;; Add-dm-fct
;;;
;;; User level function for creating chunks and placing them directly into the
;;; declarative memory list of the declarative memory module of the current 
;;; model.
;;;
;;; It takes a parameter which is a chunk definition list like define-chunk-fct
;;; takes.  Those chunks are created and then added to the declarative memory 
;;; list with the current creation time and 1 reference.

(defmacro add-dm (&rest chunk-list)
  `(add-dm-fct ',chunk-list))

(defun add-dm-fct (chunk-definitions)
  
  ;; Need to find the current instance of the declarative module
  
  (let ((dm (get-module declarative)))  

    ;; if there is one, create the chunks and set the parameters

    (if (dm-p dm)  

        ;; pass the list of chunk defs off to define-chunks 
        ;; to do the creation

        (let ((chunks (define-chunks-fct chunk-definitions)))

          ;; Then iterate over those chunks and add them to the module

          (dolist (chunk chunks chunks)
            (add-chunk-into-dm dm chunk)))
          
       ;; otherwise report a warning to the meta-process because there may not
       ;; be a current model 

      (print-warning 
       "Could not create chunks because no declarative module was found"))))       

;;; merge-dm
;;; merge-dm-fct
;;;
;;; User level function for creating chunks and merging them into the declarative 
;;; memory of the current model.
;;;
;;; It takes a parameter which is a chunk definition list like define-chunk-fct
;;; takes.  Those chunks are created and then merged into the declarative memory 
;;; of the model in the same way a cleared buffer is:  if the chunk is a match
;;; to an existing chunk then that existing chunk gets a new reference, but if
;;; there is no matching chunk then that chunk is added as a new element at the
;;; current time.


;;; Eventually this should use the same functions as procedural, but
;;; I can't require procedural-support for use here since that has
;;; problems since the procedural structs haven't been built yet...

(defun circular-references-dm (bindings)
  (let ((checks (mapcar #'car bindings)))
   
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


(defun sort-for-binding-dm (ordering)
  (let ((result nil))
    (dolist (x ordering result)
      (aif (position-if (lambda (y) (find (caar x) (cdar y))) result)
           (progn 
             (format t "Splicing ~S into ~s at position ~d~%" x result it)
             (setf result (splice-into-position-des result it x)))
           (push-last x result)))))
        

(defmacro merge-dm (&rest chunk-list)
  `(merge-dm-fct ',chunk-list))

(defun merge-dm-fct (chunk-definitions)
  
  ;; Need to find the current instance of the declarative module
  
  (let ((dm (get-module declarative)))  

    ;; if there is one, create the chunks and set the parameters

    (if (dm-p dm)  

        ;; pass the list of chunk defs off to define-chunks 
        ;; to do the creation

        (let* ((chunks (define-chunks-fct chunk-definitions))
               (ordering (mapcar (lambda (x) 
                                   (list (cons x (mapcan (lambda (y)
                                                     (when (and (chunk-p-fct y)
                                                                (find y chunks))
                                                       (list y)))
                                             (mapcar (lambda (z) (fast-chunk-slot-value-fct x z)) (chunk-type-slot-names-fct  (chunk-chunk-type-fct x)))))))
                           chunks)))
          
          (if (circular-references-dm ordering)
              (progn
                (model-warning "Chunks in call to merge-dm have circular references.")
                (model-warning "  Because of that there is no safe order for merging and they will be merged in the order provided.")
                (dolist (chunk chunks chunks)
                  (merge-chunk-into-dm dm nil chunk)))
            
            (progn
              (setf chunks (mapcar 'caar (sort-for-binding-dm ordering)))
              (dolist (chunk chunks chunks)
                (merge-chunk-into-dm dm nil chunk)))))
          
       ;; otherwise report a warning to the meta-process because there may not
       ;; be a current model 

      (print-warning 
       "Could not create chunks because no declarative module was found"))))  





;;; Call define-module to hook the module into the framework.

;;; Indicate that it is to be named declarative and that it
;;; has a buffer called retrieval.


(define-module-fct 'declarative 
    (list (list 'retrieval nil '(:recently-retrieved) '(recently-retrieved)
                 #'(lambda ()
                     (command-output "  recently-retrieved nil: ~S"
                                     (query-buffer 'retrieval 
                                                   '((recently-retrieved . nil))))
                     (command-output "  recently-retrieved t  : ~S"
                                     (query-buffer 'retrieval 
                                                   '((recently-retrieved . t))))
                               ))) 
  (list (define-parameter :esc :owner nil)
        (define-parameter :er :owner nil)
        (define-parameter :ol :owner nil)
        
        (define-parameter :blc :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Base Level Constant")
        (define-parameter :ans :valid-test #'posnumornil :default-value nil
          :warning "a positive number or nil" :documentation "Activation Noise S")
        (define-parameter :pas :valid-test #'posnumornil :default-value nil
          :warning "a positive number or nil" 
          :documentation "Permanent Activation noise S")
        (define-parameter :lf :valid-test #'nonneg :default-value 1.0
          :warning "a non-negative number" :documentation "Latency Factor")
        (define-parameter :le :valid-test #'nonneg :default-value 1.0
          :warning "a non-negative number" :documentation "Latency Exponent")
        (define-parameter :mp :valid-test #'numornil :default-value nil
          :warning "a number or nil" :documentation "Mismatch Penalty")
        (define-parameter :ms :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Maximum Similarity")
        (define-parameter :md :valid-test #'numberp :default-value -1.0
          :warning "a number" :documentation "Maximum Difference")
        (define-parameter :rt :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Retrieval Threshold")
        (define-parameter :bll :valid-test #'posnumornil :default-value nil
          :warning "a positive number or nil" :documentation "Base Level Learning")        
        (define-parameter :mas :valid-test #'numornil :default-value nil
          :warning "a number or nil" :documentation "Maximum Associative Strength")
        (define-parameter :pm :valid-test #'tornil :default-value nil
          :warning "T or nil" :documentation "Depricated - use :mp as both the flag and value instead (like :bll and :mas)")
        (define-parameter :act :valid-test (lambda (val)
                                             (or (null val) 
                                                 (eq val t) 
                                                 (eq val 'high) 
                                                 (eq val 'medium) 
                                                 (eq val 'low)))
          :default-value nil
          :warning "T, nil, high, medium, or low" :documentation "Activation Trace")
        
        (define-parameter :fast-merge :valid-test #'tornil :default-value t
          :warning "T or nil" 
          :documentation "Whether or not to use the fast merge mechanism")
        
        
        (define-parameter :declarative-num-finsts
            :valid-test #'posnum :default-value 4
          :warning "positive number" 
          :documentation "Number of declarative finst markers")
        
        
        (define-parameter :declarative-finst-span
            :valid-test #'posnum :default-value 3.0
          :warning "positive number" 
          :documentation "Duration of declarative finst markers in seconds")

        
        (define-parameter :sim-hook :valid-test #'fctornil :default-value nil
          :warning "a function or nil" :documentation "Similarity hook")
        (define-parameter :sji-hook :valid-test #'fctornil :default-value nil
          :warning "a function or nil" :documentation "Sji hook")
        (define-parameter :w-hook :valid-test #'fctornil :default-value nil
          :warning "a function or nil" :documentation "Wkj hook")
        
        (define-parameter :bl-hook :valid-test #'fctornil :default-value nil
          :warning "a function or nil" 
          :documentation "Baselevel component hook")
        (define-parameter :spreading-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Spreading component hook")
        (define-parameter :partial-matching-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Partial matching component hook")
        (define-parameter :noise-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" :documentation "Noise component hook")
        
        (define-parameter :activation-offsets :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Add additional activation equation components")
        
        (define-parameter :retrieval-request-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Retrieval notification hook")
        (define-parameter :retrieval-set-hook 
            :valid-test #'fctornil :default-value nil
          :warning "a function or nil" 
          :documentation "Prospective retrievals hook")
        (define-parameter :retrieved-chunk-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Retrieval completion hook")
        
        (define-parameter :chunk-merge-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Hook called when a chunk is merged into dm")
        (define-parameter :chunk-add-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Hook called when a chunk is added to dm"))
  
  :version "1.1" 
  :documentation 
  "The declarative memory module stores chunks from the buffers for retrieval"
  
  ;; The creation function returns a new dm structure
  ;; that doesn't require knowing the current model's name
  
  :creation (lambda (x) (declare (ignore x)) (make-dm))
    
  :reset '(reset-dm-module secondary-reset-dm-module)
  :query 'dm-query-request
  :request 'dm-request
  :params 'dm-params
  :notify-on-clear 'merge-chunk-into-dm
  )

;;; Note which parameters should signal a warning if :esc is nil

(register-subsymbolic-parameters :blc :ans :pas :lf :le :mp :ms :md :rt :bll :mas :pm :sa)

;;; Functions to compute activations and latency


(defun compute-activation (dm chunk request)
  
  (when (dm-act-level (dm-act dm) 'medium)
    (model-output "Computing activation for chunk ~s" chunk))
  
  (setf (chunk-activation chunk) 
    (+ (base-level-activation dm chunk)
       (spreading-activation dm chunk)
       (partial-matching dm chunk request)
       (activation-noise dm chunk)
       (activation-offsets dm chunk)))
  
  (when (dm-act-level (dm-act dm) 'low)
    (model-output "Chunk ~s has an activation of: ~f" chunk (chunk-activation chunk))))


(defun activation-offsets (dm chunk)
  (let ((offset 0))
    (if (dm-offsets dm)
        (dolist (x (dm-offsets dm) offset)
          (let ((res (funcall x chunk)))
            (when (numberp res)
              (when (dm-act-level (dm-act dm) 'medium)
                (model-output "Adding offset from ~a: ~f" x res))
              (incf offset res))))
      offset)))
                   
(defun base-level-activation (dm chunk)
  
  (when (dm-act-level (dm-act dm) 'medium)
    (model-output "Computing base-level"))
  
  (let ((base-level nil))
    
    (when (dm-bl-hook dm)
      (setf base-level (funcall (dm-bl-hook dm) chunk)))
    
    (cond ((numberp base-level)
           (when (dm-act-level (dm-act dm) 'medium)
             (model-output "base-level hook returns: ~f" base-level)))
          (t
           (setf base-level 
             (cond ((dm-bll dm)
                    (+ (progn
                         (when (dm-act-level (dm-act dm) 'medium)
                           (model-output "Starting with blc: ~f" (dm-blc dm)))
                         (dm-blc dm))
                       (cond ((zerop (chunk-reference-count chunk))
                              (model-warning "Cannot compute base-level for a chunk with no references.")
                              -999999.0)
                             (t ;; just use the ACT-R 5 function basically as is for now
                              (compute-references dm (chunk-reference-count chunk)
                                                  (chunk-reference-list chunk) (chunk-creation-time chunk)
                                                  (- (dm-bll dm)))))))
                   (t ;; bll nil
                    (if (chunk-base-level chunk)
                        (progn
                          (when (dm-act-level (dm-act dm) 'medium)
                            (model-output "User provided chunk base-level: ~f" (chunk-base-level chunk)))
                          (chunk-base-level chunk))
                      (progn
                        (when (dm-act-level (dm-act dm) 'medium)
                          (model-output "Starting with blc: ~f" (dm-blc dm)))
                        (dm-blc dm)))))
             
             #|(+ (progn
                                 (when (dm-act dm)
                                   (model-output "Starting with blc: ~f" (dm-blc dm)))
                                 (dm-blc dm))
                               (cond ((null (dm-bll dm))
                                      (when (dm-act dm)
                                        (model-output "User provided chunk base-level: ~f" (chunk-base-level chunk)))
                                      (chunk-base-level chunk))
                                     ((zerop (chunk-reference-count chunk))
                                      (model-warning "Cannot compute base-level for a chunk with no references.")
                                      -999999.0)
                                     (t ;; just use the ACT-R 5 function basically as is for now
                                      (compute-references dm (chunk-reference-count chunk)
                                                          (chunk-reference-list chunk) (chunk-creation-time chunk)
                                                          (- (dm-bll dm))))))
             
             |#
             )
           (when (dm-act-level (dm-act dm) 'medium)
             (model-output "Total base-level: ~f" base-level))))
    (setf (chunk-last-base-level chunk) base-level)))


#|
Interesting note on spreading activation and the retrieval buffer.
Currently, the retrieval buffer will never be an active source of
activation because the buffer will clear before the request is made.
That is the desired current implementation - no buffers get treated
special and all production actions are allowed to occur before the
retrieval is attempted.

That may need to be revisited at some point, but for now parsimony
of operation is more important.

|#

(defun spreading-activation (dm chunk)
  (setf (chunk-source-spread chunk)
    (if (dm-sa dm)
        (let ((sa nil))
          (when (dm-act-level (dm-act dm) 'medium)
            (model-output "Computing activation spreading from buffers"))
          
          (when (dm-spreading-hook dm)
            (setf sa (funcall (dm-spreading-hook dm) chunk)))
          
          (cond ((numberp sa)
                 (when (dm-act-level (dm-act dm) 'medium)
                   (model-output "spreading activation hook returns: ~f" sa))
                 sa)
                (t
                 
                 (let ((total-spread 0.0))
                   (dolist (buffer (buffers))
                     (unless (or (zerop (buffer-spread buffer))
                                 (null (buffer-read buffer))) 
                       (let ((buffer-chunk (buffer-read buffer)))
                         
                         (when (dm-act-level (dm-act dm) 'medium)
                           (model-output "  Spreading ~f from buffer ~s chunk ~s" 
                                         (buffer-spread buffer) buffer buffer-chunk))
                         
                         (let ((js (mapcan #'(lambda (slot)
                                               (when (chunk-p-fct (fast-chunk-slot-value-fct buffer-chunk slot))
                                                 (list (cons (fast-chunk-slot-value-fct buffer-chunk slot) slot))))
                                     (chunk-type-slot-names-fct (chunk-chunk-type-fct (buffer-read buffer))))))
                           
                           (when (dm-act-level (dm-act dm) 'medium)
                             (model-output "    sources of activation are: ~s" (mapcar #'car js)))
                           
                           (dolist (j js)
                             (let* ((sji (compute-sji dm (car j) chunk))
                                    (level (if (dm-w-hook dm)
                                               (let ((val (funcall (dm-w-hook dm) buffer (cdr j))))
                                                 (if (numberp val)
                                                     (progn 
                                                       (when (dm-act-level (dm-act dm) 'medium)
                                                         (model-output "    Wkj hook returns level ~f" val))
                                                       val)
                                                     (/ (buffer-spread buffer) (length js))))
                                               (/ (buffer-spread buffer) (length js))))
                                    (total (* level sji)))
                               
                               (when (dm-act-level (dm-act dm) 'medium)
                                 (model-output "    Spreading activation  ~f from source ~s level  ~f times Sji ~f"
                                               total (car j) level sji))
                               
                               (incf total-spread total)))))))
                   (when (dm-act-level (dm-act dm) 'medium)
                     (model-output "Total spreading activation: ~f" total-spread)) 
                   total-spread))))
      0.0)))


(defun compute-sji (dm j i)
  (let ((sji (if (dm-sji-hook dm)
                 (funcall (dm-sji-hook dm) j i)
               nil)))
    (if (numberp sji)
        sji
      (if (assoc j (chunk-sjis i) :test #'eq-chunks-fct)
          (cdr (assoc j (chunk-sjis i) :test #'eq-chunks-fct))
        (aif (assoc j (chunk-fan-in i) :test #'eq-chunks-fct)
             (- (dm-mas dm) (log-coerced (chunk-fan-j-to-i j it)))
             0.0)))))

(defun chunk-fan-j-to-i (j i-spread)
  (if (chunk-in-dm j)
      (/ (chunk-fan-out j) (cdr i-spread))
    (/ (+ (chunk-fan-out j) (max 1 (chunk-c-fan-out j))) (cdr i-spread))))


(defun partial-matching (dm chunk request)
  (if (dm-mp dm)
      (progn
        (when (dm-act-level (dm-act dm) 'medium)
          (model-output "Computing partial matching component"))
        (let ((pm (when (dm-partial-matching-hook dm)
                    (funcall (dm-partial-matching-hook dm) chunk request))))
          
          (cond ((numberp pm) 
                 (when (dm-act-level (dm-act dm) 'medium)
                   (model-output "partial matching hook returns: ~f" pm))
                 pm)
                (t
                 
                 (let ((total-sim 0.0))
                   (dolist (k (chunk-spec-slot-spec request))
                     (when (and (or (eq (car k) '=)
                                    (eq (car k) '-))
                                (not (chunk-spec-variable-p (third k))))
                       (when (dm-act-level (dm-act dm) 'medium)
                         (model-output "  comparing slot ~S"
                                       (second k))
                         (model-output "  Requested: ~s ~s  Chunk's slot value: ~s"
                                       (first k)
                                       (third k) 
                                       (fast-chunk-slot-value-fct 
                                        chunk
                                        (second k))))
                       
                       (let* ((sim (chunks-similarity dm 
                                                      (third k) 
                                                      (fast-chunk-slot-value-fct chunk (second k))
                                                      t))
                              
                              
                              (sim-dif (case (car k) 
                                         (= (* (dm-mp dm) sim))
                                         
                                         (- (cond ((= sim (dm-ms dm))
                                                   (* (dm-mp dm) (dm-md dm)))
                                                  ; ACT-R 5 doesn't do this, but
                                                  ; it seems like maybe it should
                                                  ;((= sim (dm-md dm))
                                                  ; (* (dm-mp dm) (dm-ms dm)))
                                                  (t
                                                   (when (dm-act-level (dm-act dm) 'medium)
                                                     (model-output 
                                                      "  negation test with similarity not ms has no effect"))
                                                   0))))))
                         
                         (when (dm-act-level (dm-act dm) 'medium)
                           (model-output "  effective similarity value is ~f" sim-dif))
                         
                         (incf total-sim sim-dif)
                         
                         )))
                   (when (dm-act-level (dm-act dm) 'medium)
                     (model-output "Total similarity score ~f" total-sim))
                   
                   total-sim)))))
    0.0))


(defun chunks-similarity (dm chunk1 chunk2 &optional (trace nil))
  (let ((sim (if (dm-sim-hook dm)
                 (funcall (dm-sim-hook dm) chunk1 chunk2)
               nil)))
    (cond ((numberp sim)
        (when (and (dm-act-level (dm-act dm) 'medium) trace)
          (model-output "  similarity hook returns: ~f" sim))
           sim)
          (t (setf sim (cond ((not (and (chunk-p-fct chunk1)
                                        (chunk-p-fct chunk2)))
                              (if (chunk-slot-equal chunk1 chunk2)
                                  (dm-ms dm) (dm-md dm)))
                             ((assoc chunk1 (chunk-similarities chunk2) :test #'eq-chunks-fct)
                              (cdr (assoc chunk1 (chunk-similarities chunk2) :test #'eq-chunks-fct)))
                             ((eq-chunks-fct chunk1 chunk2)
                              (dm-ms dm))
                             (t (dm-md dm))))
             (when (and (dm-act-level (dm-act dm) 'medium) trace)
               (model-output "  similarity: ~f" sim))
             sim))))


(defun activation-noise (dm chunk)
  (let ((noise (when (dm-noise-hook dm)
                   (funcall (dm-noise-hook dm) chunk))))
                 
    (cond ((numberp noise)
           (when (dm-act-level (dm-act dm) 'medium)
             (model-output "noise hook returns: ~f" noise))
           noise)
          (t
           (setf noise (if (dm-ans dm)
                           (act-r-noise (dm-ans dm))
                         0.0))
    
           (when (dm-act-level (dm-act dm) 'medium)
             (model-output "Adding transient noise ~f" noise)
             (model-output "Adding permanent noise ~f" (chunk-permanent-noise chunk)))
      
           (+ noise (chunk-permanent-noise chunk))))))
    

(defun compute-activation-latency (dm activation)
  (* (dm-lf dm)
     (exp-coerced (* -1 (dm-le dm) activation))))


;;; Christian's function from ACT-R 5

(defun compute-references (dm n references creation-time minus-decay)
  "Computes generalized decay formula from number and list of references,
   creation time and minus the decay rate."
  
  (when (dm-act-level (dm-act dm) 'medium)
    (model-output "Computing base-level from ~d references ~S" n references)
    (model-output "  creation time: ~f decay: ~f  Optimized-learning: ~s" creation-time (- minus-decay) (dm-ol dm)))
  
  (let ((value 0.0)
        (last-reference 0.0))
    (when references
      (dolist (reference references)
        (incf value (expt-coerced (max .05 (- (mp-time) reference))
                             minus-decay))
        (setf last-reference reference)))
    
    (when (dm-ol dm)
      (let ((denominator (+ 1.0 minus-decay)))
        (if (numberp (dm-ol dm))
          (when (> n (dm-ol dm))
            (incf value (/ (* (- n (dm-ol dm))
                              (- (expt-coerced (- (mp-time) creation-time) denominator)
                                 (expt-coerced (- (mp-time) last-reference) denominator)))
                           (* (max .05 (- last-reference creation-time)) denominator))))
          
          (setf value (/ (* n (expt-coerced (max .05 (- (mp-time) creation-time)) minus-decay))
                         denominator)))))
    
    (when (dm-act-level (dm-act dm) 'medium)
      (model-output "base-level value: ~f" (log-coerced value)))
    (log-coerced value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some postentially useful Declarative module accessing tools


(defun all-dm-chunks (dm)
  (apply #'append
         (mapcar #'(lambda (x)
                     (gethash x (dm-chunks dm)))
           (hash-table-keys (dm-chunks dm)))))

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
