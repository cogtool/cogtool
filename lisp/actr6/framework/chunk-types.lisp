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
;;; Filename    : chunk-types.lisp
;;; Version     : 1.0
;;; 
;;; Description : Definition of chunk-types and function that manipulate them.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider changing more of the commands to return two values
;;;             :     with the second one indicating whether the chunk-type was
;;;             :     valid as I did for chunk-type-slot-names-fct.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.16 Dan
;;;             : * Removed the print-chunk-type function since I don't want to
;;;             :   hide the structure since users shouldn't see them anyway.
;;; 2005.01.17 Dan
;;;             : * Changed pprint-chunk-type to use command-output and a
;;;             :   compiled format string.
;;; 2005.01.18 Dan
;;;             : * Made it so chunk-type returns the name and not the struct.
;;; 2005.01.21 Dan
;;;             : * Fixed a bug with maintaining the subtypes information.
;;; 2005.02.04 Dan
;;;             : * Changed member to find for speed. (?)
;;; 2005.02.24 Dan
;;;             : * Changed pprint-chunk-type becasue some Lisps don't take a
;;;             :   preformatted format string with the ~? directive.
;;; 2005.03.25 Dan
;;;             : * Changed chunk-type-fct so that when it builds a chunk-type
;;;             :   as a subtype the slot ordering is maintained. 
;;; 2005.09.01 Dan
;;;             : * Added extend-chunk-type-slots to support the experimental
;;;             :   change to p* that will allow a RHS modification to add
;;;             :   new slots to a chunk.  This should NOT be used in general
;;;             :   or by any other system/module/model at this time.
;;;             : * Had to patch chunk-type-fct to copy the slots list because
;;;             :   otherwise the macro calls inside of the existing code
;;;             :   get thumped by extend-... making the change persistent.
;;; 2006.01.18 Dan
;;;             : * Modified extend-chunk-type-slots to also record the new
;;;             :   slot names in a separate list.
;;;             : * Added the extended-slot-name-p function to allow one to see
;;;             :   whether or not a given slot name was one of the originals.
;;; 2006.03.02 Dan [1.0]
;;;             : * Fixed an issue with recording the subtype info that caused
;;;             :   problems with retrievals when there were more than 2 levels
;;;             :   of inheritance.
;;; 2008.05.01 Dan
;;;             : * Fixed a typo in one of the warning messages.
;;;             : * Changed chunk-type-slot-names-fct so that it returns two
;;;             :   values where the first is the list of slot names and the 
;;;             :   second is t or nil to indicate whether the chunk-type named
;;;             :   was valid.  This allows one to distinguish a return value
;;;             :   of nil for a chunk-type with no slots from a failure due to
;;;             :   an invalid chunk-type name.
;;; 2008.12.08 Dan
;;;             : * Added the calls to new-chunk-type-size so the model code
;;;             :   can keep track of the largest possible chunk size.
;;; 2009.11.17 Dan
;;;             : * Fixed an issue in how subtypes are created if they specify
;;;             :   slots which already exist in the parent because the current
;;;             :   procedural matching code relies on the order of the slots
;;;             :   and that's easier to address here than it is there.
;;; 2010.06.14 Dan
;;;             : * Renamed pprint-chunk-type to pprint-ct and made pprint-chunk-
;;;             :   type and pprint-chunk-type-fct user level commands that take
;;;             :   a chunk-type name.
;;; 2010.11.18 Dan
;;;             : * Explicitly prevent the creation of a chunk-type which has
;;;             :   a slot named isa to avoid any problems with production 
;;;             :   parsing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; chunk-type structure for internal use only.
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
;;; Saving both the super and sub type information in the chunk type structure
;;; for potential use in the matching or elsewhere, but may not need both when
;;; all is done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

#| Don't want to hide the details since users shouldn't see these

(defun print-chunk-type (chunk-type stream depth)
  "Print a chunk-type as just its name."
  (declare (ignore depth))
  (format stream "~A" (act-r-chunk-type-name chunk-type)))
|#

(defun get-chunk-type (name)
  "Internal command to get a chunk-type structure from its name"
  (verify-current-mp  
   "get-chunk-type called with no current meta-process."
   (verify-current-model
    "get-chunk-type called with no current model."
    (gethash name (act-r-model-chunk-types-table (current-model-struct))))))            

(defmacro chunk-type (&rest name-and-slots)
  "The user macro to define a new chunk-type."
  `(chunk-type-fct ',name-and-slots))
     
(defun chunk-type-fct (name-and-slots)
  "The user function to define a new chunk-type"
  (verify-current-mp  
   "chunk-type called with no current meta-process."
   (verify-current-model
    "chunk-type called with no current model."
    (cond ((null name-and-slots)
           (print-all-chunk-types))
          ((not (listp name-and-slots))
           (print-warning 
            "chunk-type-fct must be passed a list which defines a chunk-type."))
          (t
           (let* ((name-description (car name-and-slots))
                  (name (if (consp name-description)
                            (car name-description) name-description))
                  (super-type (if (consp name-description)
                                  (cdr name-description) nil))
                  (documentation (when (stringp (second name-and-slots))
                                   (second name-and-slots)))
                  (slots (if documentation (cddr name-and-slots) 
                           (cdr name-and-slots))))
             
             (when (get-chunk-type name)
               (print-warning 
                "Chunk-type ~S is already defined and redefinition is not allowed."
                name)
               (return-from chunk-type-fct nil))
             
             ; check type hierarchy
             (when super-type
               (unless (null (cdr super-type))
                 (print-warning 
                  "Too many options specified for chunk-type ~S. NO chunk-type created." 
                  name)
                 (return-from chunk-type-fct nil))
               
               (if (and (eq (caar super-type) :include) 
                        (null (cddar super-type)))
                   (if (get-chunk-type (cadar super-type))
                       (setf super-type (get-chunk-type (cadar super-type)))
                     (progn
                       (print-warning 
                        "Unknown supertype ~S specified for type ~S." 
                        (cadar super-type) name)
                       (return-from chunk-type-fct nil)))
                 (progn
                   (print-warning "Unknown option ~S specified for type ~S." 
                                  (car super-type) name)
                   (return-from chunk-type-fct nil))))
             
             (dolist (slot slots)
               (unless (or (and (atom slot) (not (eq slot 'isa)))
                           (and (listp slot)
                                (= (length slot) 2)
                                (not (eq (car slot) 'isa))))
                 (print-warning 
                  "Unacceptable slot specification ~S for chunk-type ~S.  Chunk-type not created." 
                  slot name)
                 (return-from chunk-type-fct nil)))
             
             (unless (= (length slots) (length (remove-duplicates slots)))
               (print-warning 
                "Duplicate slot specifications in ~S for chunk-type ~S.  Chunk-type not created."
                slots name)
               (return-from chunk-type-fct nil))
             
             (when super-type
               ;; Maintain the order of the parent slots in the subtype
               (let ((all-slots nil))
                 (dolist (parent-slot (act-r-chunk-type-slots super-type))
                   (aif (find (chunk-type-slot-name parent-slot) slots :key 'chunk-type-slot-name)
                        (progn
                          (push it all-slots)
                          (setf slots (remove it slots)))
                        (push parent-slot all-slots)))
                 (setf slots (revappend all-slots slots))))
             
             (let ((ct (make-act-r-chunk-type 
                        :name name 
                        :documentation documentation
                        :slots (copy-tree slots)
                        :subtypes (list name)
                        :supertypes 
                        (if super-type
                            (cons name 
                                  (act-r-chunk-type-supertypes super-type))
                          (list name)))))
               
               (new-chunk-type-size (length slots))
               
               (when super-type
                 (dolist (parent (act-r-chunk-type-supertypes super-type))
                   (push name (act-r-chunk-type-subtypes 
                               (get-chunk-type parent)))))
               
               (setf (gethash name 
                              (act-r-model-chunk-types-table 
                               (current-model-struct))) ct)
               name)))))))
  

(defun chunk-type-slot-name (slot)
  "Internal function for parsing chunk-types"
  (if (atom slot)
      slot
    (car slot)))

(defun print-all-chunk-types ()
  "Internal function for printing all chunk-types" 
  (let ((res nil))
    (maphash #'(lambda (name chunk-type)
                 (declare (ignore name))
                 (push (pprint-ct chunk-type) res))
             (act-r-model-chunk-types-table (current-model-struct)))
    (reverse res)))

(defconstant *pprint-chunk-type-string*
     (formatter "~S~@[ <- ~s~]~@[ ~S~]~%~{~{   ~s~@[ (~s)~]~%~}~}~%")
  "Internal compiled format string used to print out chunk-types")

(defun pprint-ct (chunk-type)
  "Pretty prints a chunk-type."
  (command-output  
   (format nil *pprint-chunk-type-string*
                  (act-r-chunk-type-name chunk-type)
                  (second (act-r-chunk-type-supertypes chunk-type))
                  (act-r-chunk-type-documentation chunk-type)
                  (mapcar #'(lambda (slot)
                              (if (listp slot)
                                  slot
                                (list slot nil)))
                    (act-r-chunk-type-slots chunk-type))))
  (act-r-chunk-type-name chunk-type))


(defmacro pprint-chunk-type (chunk-type)
  `(pprint-chunk-type-fct ',chunk-type))

(defun pprint-chunk-type-fct (chunk-type)
  (verify-current-mp  
   "pprint-chunk-type called with no current meta-process."
   (verify-current-model
    "pprint-chunk-type called with no current model."
    (aif (get-chunk-type chunk-type)
         (pprint-ct it)
         (print-warning "~s does not name a chunk-type in the current model." chunk-type)))))

(defmacro chunk-type-p (chunk-type-name?)
  "Predicate macro for verifying that a chunk-type of a given name exists"
  `(chunk-type-p-fct ',chunk-type-name?))

(defun chunk-type-p-fct (chunk-type-name?)
  "Predicate function for verifying that a chunk-type of a given name exists"
  (if (get-chunk-type chunk-type-name?)
      t nil))

(defmacro chunk-type-subtype-p (chunk-subtype? chunk-supertype)
  "Predicate macro for testing that one chunk-type isa a subtype of another"
  `(chunk-type-subtype-p-fct ',chunk-subtype? ',chunk-supertype))

(defun chunk-type-subtype-p-fct (chunk-subtype? chunk-supertype)
  "Predicate function for testing that one chunk-type isa a subtype of another"
  (let ((ct (get-chunk-type chunk-subtype?)))
    (when ct 
      (find chunk-supertype (act-r-chunk-type-supertypes ct)))))


(defmacro chunk-type-supertypes (chunk-type-name)
  "Macro to return the list of supertypes for a given chunk-type"
  `(chunk-type-supertypes-fct ',chunk-type-name))

(defun chunk-type-supertypes-fct (chunk-type-name)
  "Function to return the list of supertypes for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-supertypes ct))))


(defmacro chunk-type-subtypes (chunk-type-name)
  "Macro to return the list of subtypes for a given chunk-type"
  `(chunk-type-subtypes-fct ',chunk-type-name))

(defun chunk-type-subtypes-fct (chunk-type-name)
  "Function to return the list of subtypes for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-subtypes ct))))


(defmacro chunk-type-slot-names (chunk-type-name)
  "Macro to return the list of valid slot names for a given chunk-type"
  `(chunk-type-slot-names-fct ',chunk-type-name))

(defun chunk-type-slot-names-fct (chunk-type-name)
  "Function to return the list of valid slot names for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (if ct 
      (values (mapcar #'chunk-type-slot-name (act-r-chunk-type-slots ct)) t)
      (values nil nil))))

(defun ct-slot-names (chunk-type)
  "Internal function for parsing chunk-type structures"
  (mapcar #'chunk-type-slot-name (act-r-chunk-type-slots chunk-type)))


(defmacro chunk-type-slot-default (chunk-type-name slot-name)
  "Macro to return the default value for a slot in a chunk-type"
  `(chunk-type-slot-default-fct ',chunk-type-name ',slot-name))

(defun chunk-type-slot-default-fct (chunk-type-name slot-name)
  "Function to return the default value for a slot in a chunk-type"
    (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (let ((slot (find slot-name (act-r-chunk-type-slots ct) 
                        :key #'chunk-type-slot-name)))
        (when (listp slot)
          (second slot))))))


(defun ct-slot-default (chunk-type slot-name)
  "Internal function for parsing chunk-type structures"
  (let ((slot (find slot-name (act-r-chunk-type-slots chunk-type) 
                    :key #'chunk-type-slot-name)))
    (when (listp slot)
      (second slot))))


(defmacro chunk-type-documentation (chunk-type-name)
  "Macro to return the documentation string for a chunk-type"
  `(chunk-type-documentation-fct ',chunk-type-name))

(defun chunk-type-documentation-fct (chunk-type-name)
  "Function to return the documentation string for a chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-documentation ct))))

(defun valid-slot-name (slot chunk-type)
  "Internal function for testing chunk-type structures"
  (find slot (act-r-chunk-type-slots chunk-type) :key #'chunk-type-slot-name))

(defun valid-chunk-type-slot (chunk-type-name slot)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (valid-slot-name slot ct))))

(defun extend-chunk-type-slots (chunk-type slot-name)
  (let ((ct (get-chunk-type chunk-type)))
    (when ct
      (unless (valid-slot-name slot-name ct)
        (push-last slot-name (act-r-chunk-type-slots ct))
        (push-last slot-name (act-r-chunk-type-extended-slots ct))
        
        (new-chunk-type-size (length (act-r-chunk-type-slots ct)))
        
        (dolist (sub-type (act-r-chunk-type-subtypes ct))
          (extend-chunk-type-slots sub-type slot-name))))))

(defun extended-slot-name-p (slot-name chunk-type-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (find slot-name (act-r-chunk-type-extended-slots ct)))))

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
