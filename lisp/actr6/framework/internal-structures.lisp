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
;;; Filename    : internal-structures.lisp
;;; Version     : 1.1
;;; 
;;; Description : All of the defstructs for the internal code.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.07 Dan
;;;             : Created.
;;; 2005.01.05 Dan
;;;             : Changed the version on the meta-process so that it indicates
;;;             : the svn revision because it's going to be used to create a
;;;             : snapshot for the website.
;;; 2005.01.10 Dan
;;;             : Same as above - added r20 and this time it's actually going
;;;             : out to the site!
;;; 2005.01.12 Dan
;;;             : Because device is becoming a module it doesn't need to be
;;;             : a slot in the model.
;;; 2005.01.15 Dan
;;;             : * Taking the r20 out of the meta-process version and uping
;;;             :   it to 1.0a2.
;;;             : * Moving to 80 charater width.
;;;             : * Adding the copied-from slot to chunks.
;;; 2005.01.16 Dan
;;;             : * Removed the print-functions for chunks and chunk-types
;;;             :   because users shouldn't be seeing those and there's no
;;;             :   need to hide the details.
;;; 2005.01.21 Dan
;;;             : * Added the merge-list slot to chunks to help speed up the
;;;             :   merging action.
;;; 2005.01.27 Dan
;;;             : * Added the filter slot to the printing module structure.
;;; 2005.01.29 Dan
;;;             : * Added r33 to the meta-process version for distribution
;;;             :   on the ACT-R website.
;;; 2005.01.31 Dan 
;;;             : * Removed the r33 from the version and updated it to 1.0a3.
;;; 2005.02.02 Dan
;;;             : * Added the detail slot to the printing module.
;;;             : * Changed the default output for break events to be low
;;;             :   for use wth the detail level.
;;; 2005.02.11 Dan
;;;             : * Changed the make-hash-tables in the chunk structure to
;;;             :   limit the size to just a little bigger than needed and
;;;             :   in the meta-process to 5 for models.
;;; 2005.03.23 Dan
;;;             : * Added the secondary-reset slot to the module structure.
;;; 2005.04.08 Dan
;;;             : * Added r67 to meta-process version for distribution on
;;;             :   the website.
;;; 2005.04.14 Dan
;;;             : * Added the suppress-cmds slot to the printing module to get
;;;             :   around a problem with no-output and trying to read the :cmdt
;;;             :   parameter...
;;; 2005.04.20 Dan
;;;             : * Took the r67 off of the meta-process version.
;;; 2005.05.11 Dan
;;;             : * Changed the version to 1.0b1 [r79]. 
;;; 2005.05.12 Dan
;;;             : * Removed the [r79] from the version.
;;; 2005.06.10 Dan
;;;             : * Changed the version to 1.0b2 [r120]. 
;;; 2005.06.11 Dan
;;;             : * Changed version to 1.0b2
;;; 2005.07.12 Dan
;;;             : * Changed the framework version to 1.0 [r130].
;;; 2005.07.13 Dan
;;;             : * Removed the r130 from the version number.
;;; 2005.08.30 Dan
;;;             : * Changed the framework version to 1.0 [r144].
;;; 2005.08.30 Dan
;;;             : * Oops, mis-encoded the file with mac line endings, so
;;;             :   now changing to [r145].
;;; 2005.09.01 Dan
;;;             : * Taking the [r145] off.
;;; 2005.09.08 Dan
;;;             : * Added the model-warnings slot to the printing module 
;;;             :   struct to support suppression of all model warnings.
;;; 2005.11.16 Dan
;;;             : * Changing framework version to 1.0 [r168].
;;; 2005.11.17 Dan
;;;             : * Changing framework version to back to 1.0.
;;; 2006.01.16 Dan
;;;             : * Changed the version to [r187] for release.
;;; 2006.01.17 Dan
;;;             : * Changing framework version to 1.1.
;;; 2006.01.18 Dan
;;;             : * Added the extended-slots slot to the chunk-type structure
;;;             :   so that one can differentiate between the original slots
;;;             :   and any that are added on the fly.
;;;             : * Added the show-all-slots slot to the printing module to hold
;;;             :   the new parameter.
;;; 2006.01.30 Dan
;;;             : * Adding the maintenance event type for use in things like
;;;             :   terminating events and periodic events.  The schedule-event-
;;;             :   after functions will have a keyword that specifies whether
;;;             :   or not to consider maintenance events that defaults to nil.
;;; 2006.02.27 Dan
;;;             : * Added slots to the meta-process to handle the configuration
;;;             :   of the real time management.
;;; 2006.03.03 Dan
;;;             : * Added the max-time-delta slot to the meta-process.
;;; 2006.03.06 Dan
;;;             : * Changed the version to [r204] for release.
;;; 2006.03.06 Dan
;;;             : * Removed the [r204] from the version.
;;; 2006.03.14 Dan
;;;             : * Changed version to [r212] for web release.
;;; 2006.03.14 Dan
;;;             : * Removed the [r212].
;;; 2006.03.15 Dan
;;;             : * Changed version to [r216] for web release.
;;; 2006.03.15 Dan
;;;             : * Removed the [r216].
;;; 2006.03.21 Dan
;;;             : * Changed version to [r219] for web release.
;;; 2006.03.21 Dan
;;;             : * Changed version to [r220] for web release.
;;; 2006.03.28 Dan
;;;             : * Changed version to [r222] for web release.
;;; 2006.03.28 Dan
;;;             : * Removed the [r222].
;;; 2006.07.10 Dan
;;;             : * Changed version to [r248] for web release.
;;; 2006.07.10 Dan
;;;             : * Removed the [r248].
;;; 2006.11.20 Dan
;;;             : * Added the warn slot to the act-r-module structure.
;;; 2007.01.15 Dan
;;;             : * Changed the version setting in the meta-process structure
;;;             :   to be the value of *actr-version-string* so that I don't
;;;             :   have to touch this file to mark the version changes.
;;; 2007.04.13 Dan
;;;             : * Added a new slot to the printing module struct to hold
;;;             :   the cbct parameter.
;;; 2008.10.20 Dan
;;;             : * Added new slots to the model structure to hold the flags
;;;             :   for normalizing.
;;; 2008.11.03 Dan
;;;             : * Updated the chunk and chunk parameter structures to work 
;;;             :   with the new array representation of the parameters.
;;; 2008.11.13 Dan
;;;             : * Added the dynamic-update-hooks slot to the model struct.
;;; 2008.12.01 Dan
;;;             : * Added the tertiary-reset slot to the module structure.
;;; 2008.12.08 Dan
;;;             : * Added the the largest-chunk-type slot to the model struct
;;;             :   to keep track of the largest possible chunk size.
;;; 2008.12.10 Dan
;;;             : * Added the copy-from-chunk slot to the chunk parameter
;;;             :   struct.
;;; 2009.02.13 Dan
;;;             : * Added the base-name slot to chunks to support the new
;;;             :   short copy names.
;;;             : * Added the short-copy-names slot to the model structure too.
;;; 2009.04.29 Dan
;;;             : * Adding a slot to the meta-process so I can detect recursive
;;;             :   calls to run and signal a warning.
;;; 2009.09.09 Dan
;;;             : * Added the multi, searchable and chunk-set slots to the buffer 
;;;             :   struct.
;;; 2009.12.03 Dan
;;;             : * Adding the dynamics, allow-dynamics, and in-slack to the meta-process 
;;;             :   to provide more flexibility with real-time when a slack-hook 
;;;             :   is used and add the dynamic tag to events.
;;; 2010.01.14 Dan
;;;             : * Adding the copy slot to the buffer struct.
;;; 2010.09.02 Dan
;;;             : * Added a slot to the meta-process to record whether or not the
;;;             :   time overflow warning has been displayed or not.
;;; 2010.11.03 Dan
;;;             : * Changed the event structure's time slot to be mstime because
;;;             :   that's what will be used elsewhere.  To go with that have
;;;             :   added an evt-time function which makes the conversion to
;;;             :   keep the API right.
;;; 2010.12.22 Dan
;;;             : * Added a run-notify slot to the module structures to support
;;;             :   the new run-notify option for modules.
;;; 2011.01.11 Dan
;;;             : * Added a run-over-notify slot to the module structures since
;;;             :   a module may want to know about both situations.
;;; 2011.03.25 Dan
;;;             : * Added a safety check to evt-time so that if there's not a
;;;             :   time it doesn't throw an error.
;;; 2011.04.01 Dan
;;;             : * Changed the initial value for the meta-process time to be 0 
;;;             :   instead of 0.0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; These are not for general use!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; NONE!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; I had some odd compiling order issues with the defstructs and defmacros 
;;; so for now the easy fix was to make sure that they are all 
;;; available from the start.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct act-r-buffer 
  "The internal structure for a buffer"
  name
  chunk  ;; holds the chunk name not struct - copy issues and such...
  module
  spread
  queries
  requests
  parameter-name
  requested
  status-printing
  multi
  searchable
  chunk-set
  copy)

(defstruct act-r-chunk-spec 
  "The internal structure of a chunk-spec"
  type slots)

(defstruct act-r-slot-spec 
  "The internal structure of a chunk-spec's slot specification"
  (modifier '=) name value)

(defstruct act-r-chunk-type ; (:print-function print-chunk-type))
  "The internal structure of a chunk-type"
  name documentation supertypes subtypes slots extended-slots)

(defstruct act-r-chunk ; (:print-function print-chunk))
  "The internal structure of a chunk"
  name base-name
  documentation 
  chunk-type 
  slot-value-lists 
  copied-from
  merged-chunks
  parameter-values )

(defstruct act-r-chunk-parameter
  "The internal structure of a chunk parameter"
  name index default-value default-function merge copy copy-from-chunk accessor)

(defstruct (act-r-event (:conc-name evt-)) 
  "Internal ACT-R event"
  mstime priority action model mp module destination params details (output t)
  wait-condition dynamic)

(defun evt-time (event)
  (when (numberp (evt-mstime event))
    (* (evt-mstime event) .001)))

(defstruct (act-r-maintenance-event (:include act-r-event (output 'low)))
  "Events for system maintenance")

(defstruct (act-r-break-event 
            (:include act-r-maintenance-event (action #'act-r-event-break-action))) 
  "The ACT-R break events"
  )

(defstruct (act-r-periodic-event (:include act-r-maintenance-event)) 
  "special event that repeatedly schedules a user's event"
  id)

(defstruct (meta-processes (:conc-name mps-))
  "The internal structure that holds meta-processes"
  (table (make-hash-table))
  (count 0)
  current)

(defstruct (meta-process (:conc-name meta-p-))
  "The internal representation of the meta-process"
  name
  (time 0)
  start-time
  start-real-time
  (models (make-hash-table :size 5))
  current-model
  (model-count 0)
  (model-name-len 0)
  events
  delayed
  dynamics
  allow-dynamics
  in-slack
  break
  pre-events
  post-events
  
  (time-function 'get-internal-real-time)
  (units-per-second internal-time-units-per-second)
  (slack-function 'real-time-slack)
  
  max-time-delta
  
  (next-hook-id 0)
  (hook-table (make-hash-table))
  (version *actr-version-string*)
  (documentation "")
  (running nil)
  time-overflow-warning)

(defstruct act-r-model
  "The internal structure of a model"
  (modules-table (make-hash-table)) 
  (buffers (make-hash-table))
  (chunks-table (make-hash-table))
  (chunk-ref-table (make-hash-table))
  (chunk-types-table (make-hash-table))
  (largest-chunk-type 0)
  name 
  code
  (chunk-update t)
  (dynamic-update t)
  delete-chunks
  dynamic-update-hooks
  short-copy-names
  ;device
  )

(defstruct act-r-modules
  "The internal structure that holds the modules"
  (table (make-hash-table))
  (count 0)
  (name-len 0)
  notify
  update
  run-notify
  run-over-notify)

(defstruct act-r-module 
  "The internal structure of a module"
  name buffers version documentation creation reset query
  request buffer-mod params delete notify-on-clear update
  secondary-reset tertiary-reset warn search offset run-notify run-over-notify)

(defstruct act-r-parameter 
  "The internal structure of a parameter"
  param-name 
  default 
  test 
  warning 
  details
  owner
  users)

(defstruct printing-module
  "The internal structure for an instance of the printing module"
  (v (make-act-r-output :stream t))
  (c (make-act-r-output :stream t))
  (suppress-cmds nil)
  (filter nil)
  (detail 'high)
  (model-warnings t)
  (show-all-slots nil)
  (cbct nil))

(defstruct act-r-output
  "The internal structure of an output stream for the printing module"
  stream file)

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
