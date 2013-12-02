;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2003 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : dmi.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : The "Declarative Memory Interface"
;;;             : Defines a class ("declarative memory object," or DMO)
;;;             : and numerous methods to provide an interface layer
;;;             : between the PM stuff and the specific production
;;;             : system in terms of referring to declarative
;;;             : memory elements.  Cleans up a bunch of the PM code.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 01.07.27 mdb
;;;             : Removed CLEAR-LOC for 2.1/ACT5
;;; 2003.01.21 mdb
;;;             : Removed PS-PTR slot and made it a method instead. 
;;; 
;;; 04.10.20 Dan [First past at moving things to ACT-R 6]
;;;            : Moved it in with the support code
;;;            :   modules that use it should have this call in them:
;;;            :   (require-compiled "DMI" "ACT-R6:support;dmi")
;;;            : Reset version number to 1.0a1
;;;            : Moved the methods from actr-interface to here, updated
;;;            :   them for ACT-R 6 and removed the methods that just printed
;;;            :   the "no method defined" message
;;;            : One thing though is that they aren't going directly to
;;;            :   declarative memory but just to chunks which will then
;;;            :   be deposited into buffers which will then end up in DM
;;;            : Modified the matching-dmos method because there will always be 
;;;            :   multiple copies as the chunks get moved into the buffers and 
;;;            :   modifying one of those chunks "behind the scenes" is not 
;;;            :   kosher.  For now I've just replaced it with essentially a
;;;            :   nop so that a new one is used everytime but a good solution
;;;            :   would probably be to keep an internal collection of the
;;;            :   chunks that the module explicitly creates and then search
;;;            :   over those.
;;;            :
;;; 04.12.17 Dan
;;;            : Changed make-dme so that it doesn't recreate an existing
;;;            :   chunk.  This is basically a hack around something that may
;;;            :   need to be fixed in vision.  Shouldn't affect DM since
;;;            :   the dme chunk isn't going to be a buffer's copy and thus
;;;            :   not one that ends up in DM.
;;; 2005.01.09 Dan
;;;            : Moved the provide to the end.
;;; 2005.03.25 Dan
;;;            : * Changed xy-to-dmo to remove the attended slot from the
;;;            :   chunk that gets made.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defclass dmo ()
  (;(ps-ptr :accessor ps-ptr :initform nil :initarg :ps-ptr)
   (id :accessor id :initform (new-name-fct "DMO") :initarg :id)
   (dmtype :accessor dmtype :initarg :dmtype :initform nil)
   (pm-obj :accessor pm-obj :initform nil :initarg :pm-obj)))


;;; generic methods 

(defmethod print-object ((self dmo) stream)
  (print-unreadable-object (self stream :type t)
    (princ (id self) stream)))

(defgeneric ps-ptr (self)
  (:documentation "Return the production system [hence PS] declarative memory."))



(defgeneric find-loc-dmo (loc)
  (:documentation "Given an XY location, return the DMO representing that location, if present."))

(defmethod find-loc-dmo ((loc vector))
  (random-item 
   (matching-dmos 'visual-location
                  `(screen-x ,(px loc) screen-y ,(py loc)))))

(defmethod find-loc-dmo ((loc list))
  (find-loc-dmo (coerce loc 'vector)))


(defgeneric xy-to-dmo (loc attended-p)
  (:documentation "Given an XY loc and an attended value, find the declarative memory representaion for that location, or build it.  Either way, set the attended flag of the DMO."))

(defmethod xy-to-dmo ((loc vector) (attended-p symbol))
  (let ((loc-dmo (find-loc-dmo loc))
        (name (new-name-fct "LOC")))
    (if loc-dmo
      (if attended-p 
        (set-attributes loc-dmo '(attended t))
        loc-dmo)
      (make-dme name 'visual-location  
                `(screen-x ,(px loc) screen-y ,(py loc)
                           ;attended ,attended-p 
                           objects nil)
                :where :external))))


(defmethod xy-to-dmo ((loc list) (attended-p symbol))
  (xy-to-dmo (coerce loc 'vector) attended-p))


#|
(defgeneric clear-loc (dmo)
  (:documentation "Clears the <objects> slot of a DMO."))

(defmethod clear-loc ((self dmo))
  (set-attributes self '(objects nil)))
|#

(defgeneric dmo-to-xy (dmo)
  (:documentation "Takes a location DMO and returns the XY location."))

(defmethod dmo-to-xy ((self dmo))
  (vector (get-attribute self 'screen-x) (get-attribute self 'screen-y)))



;;; Must be overridden with PS-specific methods.

(defgeneric make-dme (id dmtype attrs &key obj where)
  (:documentation "Creates both a PS-specific DM rep and an abstract DMO for a specification."))



(defgeneric get-attribute (dmo attrname)
  (:documentation "Retuns the value of a DMO's specified attribute."))


(defgeneric set-attributes (dmo attrs)
  (:documentation "Sets the value of one or more attributes of a DMO."))


(defgeneric psdme-to-dmo (psdme)
  (:documentation "Given a PS-specific DME, return the corresponding DMO."))

(defmethod psdme-to-dmo ((psdme null))
  nil)


(defgeneric dmo-to-psdme (dmo)
  (:documentation "Return the PS-specific DME form for a given DMO."))

(defmethod dmo-to-psdme ((self null))
  nil)


(defgeneric matching-dmos (dmtype attrs)
  (:documentation "Return a list of DMOs matching the given spec."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN
;;; Methods from actr-interface updated for ACT-R 6 
;;; note: they don't exactly go to DM now but are just chunks


(defmethod ps-ptr ((self dmo))
  ;;; The chunk name is the "pointer" for ACT-R 6
  ;;; because chunk structures are "off-limits"
  ;(get-safe-wme (id self))
  (id self))

(defmethod make-dme (id (dmtype symbol) (attrs list) &key (obj nil) 
                          (where nil))
  (declare (ignore where))
  
  ; (no-output (add-dm-fct `((,id isa ,dmtype ,@attrs)) :reset-ia nil))
  
  ;; The mod-chunk thing is a bad idea...
  ;; but necessary for now.
  
  (if (get-chunk id)
      (mod-chunk-fct id attrs)
    
    (define-chunks-fct `((,id isa ,dmtype ,@attrs))))
  
  (make-instance 'dmo :dmtype dmtype :id id :pm-obj obj))


(defmethod get-attribute ((self dmo) (attrname symbol))
  ; (no-output (chunk-slot-value-fct (ps-ptr self) attrname))
  (chunk-slot-value-fct (ps-ptr self) attrname))

(defmethod set-attributes ((self dmo) (attrs list))
  ;(no-output (mod-chunk-fct (ps-ptr self) attrs))
  (mod-chunk-fct (ps-ptr self) attrs)
  self)


(defmethod psdme-to-dmo (psdme)
  ;(setf psdme (get-safe-wme psdme))
  ;(make-instance 'dmo :dmtype (wme-type psdme) :id (wme-name psdme))
  (make-instance 'dmo :dmtype (chunk-chunk-type-fct psdme) :id psdme)
  )


(defmethod dmo-to-psdme ((self dmo))
  ;(no-output (get-safe-wme (id self))))
  (id self))
  

(defmethod matching-dmos ((dmtype symbol) (attrs list))
  #|(let ((psdmes 
         (no-output (sdm-fct `(isa ,dmtype ,@attrs)))))
    (when psdmes
      (mapcar #'psdme-to-dmo psdmes)))
  
    ;;; The good solution would be something like
      
     (let ((psdmes (find-matching-chunks   
                       (define-chunk-spec-fct  
                         `(isa ,dmetype ,@attrs))
                      :chunks (internal-module-chunk-list))))
        (when psdmes
          (mapcar #'psdme-to-dmo psdmes)))
   |#
  nil)


(provide "DMI")

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