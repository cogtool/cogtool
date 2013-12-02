;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : p-star-cmd.lisp
;;; Version     : 1.1
;;; 
;;; Description : Functions that work with the procedural module to allow
;;;             : definition of productions with bound-variable slot names.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Modifiy the ordering of conditions so that one can
;;;             :     use an explicit bind for a variablized slot as long as
;;;             :     there aren't circularities.  Not quite as easy as I
;;;             :     inititial thought because you need to differentiate
;;;             :     explicit binds based on whether they should occur before 
;;;             :     or after the variablized slot tests.
;;; 
;;; ----- History -----
;;; 2005.02.05 Dan
;;;             : * Creation - no 80 column limit because it really makes it 
;;;                 hard for me to work with this!
;;; 2005.02.28 Dan
;;;             : * Made the replace-variables-for-eval fix in p* as well.
;;; 2005.04.26 Dan
;;;             : * Restored the setting of lhs and rhs of the production to
;;;             :   the parsed code for use by compilation.
;;; 2005.05.03 Dan
;;;             : * Added the production-?hs-buffers setting for compilation.
;;; 2005.05.04 Dan
;;;             : * Fixed the issue with direct requests not clearing the
;;;             :   buffer.
;;; 2005.05.11 Dan
;;;             : * Changed the output parameter for buffer clearing so that
;;;             :   it prints in the medium traces as well.
;;; 2005.06.19 Dan
;;;             : * Seems to be a bug wrt condition ordering if there are
;;;             :   "cross buffer" conditions.
;;; 2005.06.21 Dan
;;;             : * Fixed the issue noted above.  What happened is that a 
;;;             :   variablized slot with a variablized value was being allowed
;;;             :   to rebind the variable even if it had a different value.
;;;             :   Doesn't do that now.
;;; 2005.09.01 Dan
;;;             : * Making the changes necessary to allow a RHS = in a p* to
;;;             :   extend the chunk-type definition.
;;;             : * Had to add a flag to the procedural module (can't use a
;;;             :   global and p*'s don't have their own module) called
;;;             :   check-p*-mods to let compilation generate correctly in
;;;             :   all cases.
;;;             ; * Adding a test of the above to valid-variable-chunk-mod-spec
;;;             :   to allow production compilation to create instantiated
;;;             :   p*'s for slots that don't exist yet.
;;;             : * Adding the extend-buffer-chunk function and putting it into
;;;             :   the RHS = action function before the mod.
;;; 2005.09.09 Dan
;;;             : * Changed references of chunk-to-chunk-spec to chunk-name-to-
;;;             :   chunk-spec and made sure to call it appropriately.
;;; 2005.09.14 Dan
;;;             : * Change to the priority of RHS actions. = is now set to a
;;;             :   higher priority than +.  See log in procedural.lisp for
;;;             :   the detailed rational.
;;; 2005.12.05 Dan
;;;             : * Fixed an issue with ordering when a variablized slot needs
;;;             :   to create a binding, but the buffer that binds the slot
;;;             :   name occurs in a buffer test after the buffer with the
;;;             :   variablized slot.  Added replace-slot-variables to handle
;;;             :   that and modified p*-fct appropriately.
;;;             :   - Should probably just fix replace-variables, but don't
;;;             :   want to deal with the possible issues that could cause.
;;; 2005.12.06 Dan
;;;             : * Previous fix is not quite sufficient because there's
;;;             :   still an issue with the binding of that variablized slot's
;;;             :   value.  Changing the matching process slightly so that
;;;             :   it's a more explicit 2 steps of binding and now it rejects
;;;             :   p*'s that violate the two step rule.
;;; 2005.12.07 Dan
;;;             : * Following the changes to the p command, p*'s are now
;;;             :   more restrictive with respect to !bind! and most problem
;;;             :   conditions (circular references, unbound vars, etc) should
;;;             :   now be flagged.  The biggest change is that a !bind! cannot
;;;             :   be used to set a variable for a slot name anymore - all
;;;             :   explicit binds take place after all the buffer bindings.
;;; 2005.12.12 Dan
;;;             : * The last update caused some problems for production
;;;             :   compilation because it modified the order of the lhs data
;;;             :   in the p* relative to p.  That's been corrected now.
;;; 2006.01.18 Dan
;;;             : * Eliminated some warnings that occured during conflict 
;;;             :   resolution because a variablized slot didn't name a valid
;;;             :   slot in the chunk-type at that point.  Now, it does a
;;;             :   round of slot validation first.
;;; 2006.01.31 Dan
;;;             : * Fixed a bug that was introduced with the elimination of
;;;             :   the warnings that really broke the p* mechanism...
;;; 2006.03.21 Dan
;;;             : * Fixed a bug that slipped through when I update the
;;;             :   utility mechanisms.
;;; 2006.10.11 Dan
;;;             : * Changed the priority of the buffer-overwrite action to be
;;;             :   90 instead of 100 (the priority of the buffer mod action)
;;;             :   because the modifications must occur first, but if they
;;;             :   have the same priority then they could be reversed which
;;;             :   can lead to run-time issues and isn't right with respect
;;;             :   to how productions should work.
;;; 2006.11.09 Dan 
;;;             : * Added the define-p* and define-p*-fct commands for consistency.
;;;             : * Added the setting of the dynamic slot in the production
;;;             :   structure of a p*.
;;;             : * Modified the call to print-production-output in the parsing
;;;             :   of an !output! command so that the "old style" format string
;;;             :   usage doesn't get triggered implicitly.
;;;             : * Fixed a bug in production parsing that would lead to run time
;;;             :   errors if a buffer overwrite action didn't specify a chunk
;;;             :   or at least a variable (the variable could still bind to a
;;;             :   non-chunk at run time).
;;; 2006.11.10 Dan
;;;             : * Additional update to the parsing like the last one - now
;;;             :   direct requests must be a variable or chunk.
;;;             : * Fixed a cut and paste error related to the last change.
;;; 2006.11.20 Dan
;;;             : * Removed the special case implicit action for visual requests in
;;;             :   the production definition and replaced it with the new 
;;;             :   (generic) module-warning mechanims.
;;; 2007.06.18 Dan
;;;             : * Changed calls to slot-specs-to-chunk-spec-list to add the -fct
;;;             :   since it's now an "official" command.
;;; 2008.03.24 Dan
;;;             : * Start of the work to add the !mv-bind! to productions.
;;;             : * Added the failure reason for !bind! so whynot shows that it
;;;             :   fails for a binding to nil.
;;;             : * Sort the conditions of the productions now so that explicit
;;;             :   binds always precede buffer bindings (thought I did that
;;;             :   already).
;;; 2008.03.25 Dan
;;;             : * Added the new !mv-bind! option for productions.  It works
;;;             :   like !bind! except that it can bind multiple values in one
;;;             :   call.  Here is an example:
;;;             :   !mv-bind! (=var1 =var2) (some-function)
;;;             :   that would bind the variables =var1 and =var2 to the first
;;;             :   and second values returned by some-function.  If there are
;;;             :   not as many return values as variables to be bound the extra
;;;             :   are bound to nil which means the production will not match 
;;;             :   if it's a LHS !mv-bind!.
;;;             : * Fixed a bug that would let one rebind a variable with an
;;;             :   explicit !bind! on the RHS of a production.
;;; 2008.03.28 Dan
;;;             : * Changed the order of the conditions so that the slot tests
;;;             :   actually precede the variablized slot bindings.  Doesn't
;;;             :   affect the matching but it does clean up the whynot info.
;;;             : * Fixed !mv-bind! for the RHS.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list since
;;;             :   the macro was removed from the chunk-spec code.
;;; 2008.07.09 Dan
;;;             : * Patched the p*-fct to not error when there are bad lhs
;;;             :   conditions.
;;; 2008.11.03 Dan
;;;             : * Updated the parsing of a p* so that the LHS code uses
;;;             :   the new cr-buffer-read option in the conflict resolution
;;;             :   code.
;;; 2008.11.10 Dan
;;;             : * Fixed the bug introduced on 08/07/09 that was fixed for
;;;             :   the p command a while ago.
;;; 2008.11.25 Dan
;;;             : * Changed p*-fct to use add-production and remove-production
;;;             :   to better abstract away from the internals of the module.
;;;             : * Changed calls to get-production to use get-production-internal
;;;             :   to avoid the unnecessary hash-table look-up when possible.
;;; 2008.12.08 Dan [1.1]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions.  No longer create lambdas for the conditions.  
;;;             :   Instead, create more specific single condition tests.
;;; 2009.02.10 Dan
;;;             : * Changed the module modification requests so that they don't
;;;             :   verify things before passing it along to the module.  That
;;;             :   allows a module to perform chunk-type extensions in the 
;;;             :   requests.
;;; 2009.05.04 Dan
;;;             : * Fixed an issue with symbols in variablized slots on the LHS
;;;             :   not being defined as chunks when necessary.
;;; 2009.05.05 Dan
;;;             : * Changed the inequality tests so that they all use the "right"
;;;             :   test instead of only testing > and negating and then add the
;;;             :   opposite test to the implicit ones for the tree.
;;; 2009.05.22 Dan
;;;             : * Fixed a bug in production parsing which would allow something
;;;             :   like this to "work" (p* name =goal> free ==> ...).
;;; 2009.06.03 Dan 
;;;             : * Added explicit conditions for the inequality tests to check
;;;             :   if a value is a number and an implicit check if a slot has
;;;             :   a constant to possibly help in building the conflict tree.
;;; 2009.09.10 Dan
;;;             : * Added the require-compiled for the code that's now in production-
;;;             :   parsing-support.lisp and clipped out some old code that's not
;;;             :   used anymore.
;;; 2009.09.18 Dan
;;;             : * P and p* have been combined into a single system now, but
;;;             :   leaving this here to keep the p* particular stuff still
;;;             :   separate and for the history info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; p* and p*-fct which work like p and p-fct but allow one to use variables
;;; in the place of slot names as long as those variables get bound elsewhere.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; There is only allowed to be one level of indirection, either within or
;;; across buffers.
;;;
;;; Thus, this is acceptable:
;;; 
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot-name  =next-slot
;;;   =next-slot  =some-value
;;;
;;; as is this:
;;;
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot2-name =slot-val
;;; =retrieval>
;;;   isa memory
;;;   =slot-name  =val2
;;;   slot =slot2-name
;;;
;;; but this is NOT allowed:
;;; 
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot-name   =next-slot
;;;   =next-slot   =some-value
;;;   =some-value  =value
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

(require-compiled "PRODUCTION-PARSING" "ACT-R6:support;production-parsing-support")

#|


General idea is that instead of a 4 pass system like p has:

test all constant slot/values

bind variablized slot values

perform explicit binds

test queries, call evals and test remaining slot specs



it's 5 passes:

check constant slots and constant values

bind variablized slot values in constant slots

bind variablized slot values in variablized slots

perform explicit binds

test queries, call evals and test remaining slot specs

|#


(defmacro define-p* (&rest definition)
  "Production definition."
  `(p*-fct ',definition))

(defun define-p*-fct (definition)
  (p*-fct definition))

(defmacro p* (&rest definition)
  "Production definition."
  `(p*-fct ',definition))


(defun p*-fct (definition)
  (let ((prod (get-module procedural)))  
    (if (procedural-p prod)  
        (create-production prod definition t)  
      (print-warning "No procedural modulue found cannot create production."))))


(defun define-variable-chunk-spec-fct (specifications-list)
  "Allows variables in the slot-name position, but the return value isn't
   really a valid chunk-spec for purposes of testing chunks"
  (verify-current-mp  
   "define-variable-chunk-spec-fct called with no current meta-process."
   (verify-current-model
    "define-variable-chunk-spec-fct called with no current model."
    (cond ((null specifications-list)
           (print-warning "No specification in call to define-chunk-spec."))
          ((= (length specifications-list) 1)
           (if (get-chunk (car specifications-list))
               (chunk-name-to-chunk-spec (car specifications-list))
             (print-warning 
              "define-chunk-spec's 1 parameter doesn't name a chunk: ~S" 
              specifications-list)))
          
          ((not (eq (car specifications-list) 'isa))
           (print-warning 
            "First element to define-chunk-spec isn't the symbol ISA. ~s" 
            specifications-list))
          ((not (get-chunk-type (second specifications-list)))
           (print-warning 
            "Second element in define-chunk-spec isn't a chunk-type. ~S" 
            specifications-list))
          (t
           (let* ((ct (get-chunk-type (second specifications-list)))
                  (new-spec (make-act-r-chunk-spec :type ct))
                  (slots (process-variable-slots-specs ct (cddr specifications-list))))
             (unless (eq slots :error)
               (setf (act-r-chunk-spec-slots new-spec) slots)
               new-spec)))))))


(defun process-variable-slots-specs (chunk-type specs)
  (let ((slots nil))
    (loop 
      (when (null specs)
        (return slots))
      (let ((spec (make-act-r-slot-spec)))
        (when (find (car specs) '(= - > < >= <=))
          (setf (act-r-slot-spec-modifier spec) (pop specs)))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (unless (or (chunk-spec-variable-p (car specs)) ;; let this go through...
                    (valid-slot-name (car specs) chunk-type) 
                    (keywordp (car specs)))
          (print-warning "Invalid slot-name ~S in call to define-chunk-spec." 
                         (car specs))
          (return :error))
        (setf (act-r-slot-spec-name spec) (pop specs))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (setf (act-r-slot-spec-value spec) (pop specs))
        (push spec slots)))))





(defun valid-variable-chunk-mod-spec (chunk-type modifications-list)
  (if (oddp (length modifications-list))
      (print-warning "Odd length modifications list.")
    (if (procedural-check-p*-mods (get-module procedural))
      (do ((slots nil (cons (car s) slots))
           (s modifications-list (cddr s)))
          ((null s) 
           (and (every #'(lambda (slot)
                           (or (chunk-spec-variable-p slot)
                               (valid-slot-name slot (get-chunk-type chunk-type))))
                       slots)
                (= (length slots) (length (remove-duplicates slots))))))
      t)))
  



(defun extend-buffer-chunk (buffer-name mod-list)
  (let ((chunk (buffer-read buffer-name)))
      (cond ((null chunk)
             (print-warning 
              "extend-buffer-chunk called with no chunk in buffer ~S"
              buffer-name))
            (t
             (let ((m-list (copy-list mod-list))
                   (new-slots nil)
                   (ct (chunk-chunk-type-fct chunk))
                   (procedural (get-module procedural)))
               (while m-list
                 (let ((new? (pop m-list)))
                   (unless (valid-chunk-type-slot ct new?)
                     (push new? new-slots))
                   (pop m-list)))
               (dolist (new-slot new-slots)
                 (extend-chunk-type-slots ct new-slot)
                 
                 (schedule-event-relative 0 'extending-chunk-type 
                                      :module 'procedural
                                      :priority 51 ; just before the modification itself
                                      :params (list ct new-slot)
                                      :output (procedural-rhst procedural))))))))


(defun extending-chunk-type (ct slot)
  "Dummy function to show chunk extension in the schedule"
  (declare (ignore ct slot)))


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
