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
;;; Filename    : procedural-cmds.lisp
;;; Version     : 1.2
;;; 
;;; Description : User functions for the procedural module.
;;; 
;;; Bugs        : * [should be fixed now]
;;;             :   Doesn't properly check that a variable has a binding before
;;;             :   use on the LHS.  Doesn't break things, but it doesn't warn
;;;             :   in the case of productions that will never fire.
;;;
;;;             : * [fixed]
;;;             :   RHS direct requests don't schedule an implicit clear because
;;;             :   the test for that is that the second item be ISA (to avoid
;;;             :   clearing on a mod-request).  SO, was that by design or an
;;;             :   over-generalization?  IF it's a bug, fix in p* as well.


;;; To do       : * [done]
;;;             :   Try redoing the matching so that all bindings occur before
;;;             :   all other tests so that buffer ordering isn't important
;;;             :   other than to determine which buffer is used for the
;;;             :   binding.
;;;             : * Save the variable name in the parse-condition (probably
;;;             :   as a third element to the first or something) so that
;;;             :   it doesn't have to be created and interned in p.
;;;             : * In general that can be done in the parsing so that 
;;;             :   there's no need to call find-variables in p.
;;;             : * Signal a warning when there is a !eval! in the slot
;;;             :   value position because a list is valid but it won't
;;;             :   do the evaling which could confuse those moving things
;;;             :   from 5.
;;;
;;;
;;; ----- History -----
;;; 2005.01.15 Dan
;;;             : * Moved things to here from procedural in core-modules.
;;; 2005.01.16 Dan
;;;             : * Added the checks for delayed conflict resolution so that
;;;             :   user changes can force it back into the queue (either a
;;;             :   new/changed production or setting a parameter with spp).
;;; 2005.01.17 Dan
;;;             : * Changed model-output to command-output in spp and its
;;;             :   associated functions.
;;; 2005.01.19 Dan
;;;             : * Added most of the ACT-R 4/5 user commands.
;;; 2005.02.03 Dan
;;;             : * Moved production-firing-only here from the printing module.
;;; 2005.02.04 Dan
;;;             : * Complete reworking of how production parsing occurs to
;;;             :   attempt to cache more stuff (define-chunk-specs) at parse
;;;             :   time.
;;;             : * Undid the 80 column limit because it really makes it hard
;;;             :   for me to work with this.
;;; 2005.02.10 Dan
;;;             : * Use the new parse tables in parse-conditions and parse-
;;;             :   action to help make a reset faster.
;;; 2005.02.13 Dan 
;;;             : * Minor change to add more #' to the lambdas.
;;; 2005.02.14 Dan
;;;             : * Fixed bind-variable so that it returns nil wnen a
;;;                 variable gets bound to nil so it breaks the matching.
;;; 2005.02.25 Dan
;;;             : * Added replace-variables-for-eval because the values passed
;;;             :   to a function in an eval need to be quoted.
;;; 2005.03.18 Dan
;;;             : * Modfying the p command so that it only does strict
;;;             :   harvesting for the buffers not on the do-not-harvest
;;;             :   list.
;;; 2005.04.12 Dan
;;;             : * Fixed a small typo (LHS->RHS) in a warning in parse-actions. 
;;; 2005.04.26 Dan
;;;             : * Restored the setting of lhs and rhs of the production to
;;;             :   the parsed code for use by compilation.
;;; 2005.05.03 Dan
;;;             : * Possible bug has shown up (see above) and may need to be
;;;             :   fixed...
;;; 2005.05.04 Dan
;;;             : * Fixed the issue with direct requests not clearing the
;;;             :   buffer.
;;; 2005.05.11 Dan
;;;             : * Changed the output parameter for buffer clearing so that
;;;             :   it prints in the medium traces as well.
;;; 2005.05.17 Dan
;;;             : * Added the delete-production function.  Not intended as a
;;;             :   user command...
;;; 2005.06.01 Dan
;;;             : * Made pp/pp-fct more tolerant of bad production names.
;;; 2005.06.02 Dan
;;;             : * Made replace-variables-for-eval smarter about replacing
;;;             :   a variable with another variable because compilation can
;;;             :   result in that and it's not desireable to have it quoted.
;;; 2005.06.18 Dan
;;;             : * Modified spp so it prints a warning if a named production
;;;             :   does not exist.
;;; 2005.09.09 Dan
;;;             : * Changed reference of chunk-to-chunk-spec to chunk-name-to-
;;;             :   chunk-spec and made sure to call it appropriately.
;;; 2005.09.14 Dan
;;;             : * Change to the priority of RHS actions. = is now set to a
;;;             :   higher priority than +.  See log in procedural.lisp for
;;;             :   the detailed rational.
;;; 2005.11.09 Dan
;;;             : * Fixed a bug in pmatches which occured when more than one
;;;             :   production matched the current state - it threw an error.
;;;             :   That also caused whynot to break because it uses pmatches.
;;; 2005.12.07 Dan
;;;             : * Restricting the potential uses of !bind!.  Instead of 
;;;             :   having it participate in bindings at the same level as the
;;;             :   buffers, it now only occurs explicitly after all buffer
;;;             :   tests have been allowed to do their bindings and warns
;;;             :   (rejecting the production) if there is an attempt to rebind
;;;             :   a particular variable.  It also now tests for circualar
;;;             :   references among !binds! and orders them as needed.
;;; 2005.12.15 Dan 
;;;             : * Modified replace-variables to handle something like
;;;             :   (cons 'a 'b) properly because I'm using that in the
;;;             :   compilation code now.  If this seems to impact performance
;;;             :   then I'll need to just make a special version for that
;;;             :   case.
;;; 2005.12.22 Dan
;;;             : * Removed the spp code to put in the utility-and-reward file.
;;; 2006.03.09 Dan
;;;             : * Get-production has changed to not require the procedural
;;;             :   module so make that change as needed.
;;; 2006.03.10 Dan
;;;             : * Changed pp to not exclude disabled productions and going to
;;;             :   make print-production mark them as such instead.
;;; 2006.03.13 Dan 
;;;             : * Fixed pmatches to deal with the new utility computations.
;;; 2006.03.14 Dan
;;;             : * Added the define-p macro which does the same thing as p,
;;;             :   but the define- name provides some consistency with every-
;;;             :   thing else (define-model, define-chunks, define-module, etc)
;;;             :   and helps people who use MCL as the editor.
;;; 2006.03.14 Dan
;;;             : * Added the function all-productions because I realized that
;;;             :   using pp is a bad idea since it invokes the cost of the
;;;             :   production printer each time. 
;;; 2006.03.21 Dan
;;;             : * Fixed a bug with un-delay-conflict-resolution that slipped
;;;             :   in when I updated the utility mechanims.
;;; 2006.10.11 Dan
;;;             : * Changed the priority of the buffer-overwrite action to be
;;;             :   90 instead of 100 (the priority of the buffer mod action)
;;;             :   because the modifications must occur first, but if they
;;;             :   have the same priority then they could be reversed which
;;;             :   can lead to run-time issues and isn't right with respect
;;;             :   to how productions should work.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in all-productions which caused errors if there
;;;             :   were more than one model defined and it was called with no
;;;             :   current model (not an issue for running models).
;;;             : * Made penable, pdisable, pbreak, and punbreak report nicer
;;;             :   warnings when there is no current model.
;;;             : * Fixed the penable macro so it actually called penable-fct!
;;;             : * Changed penable-fct so that like punbreak if it's passed 
;;;             :   nil all productions are enabled.
;;; 2006.11.08 Dan
;;;             : * Changed whynot so it better reports the warning that there
;;;             :   is no current model.
;;; 2006.11.09 Dan
;;;             : * Changed print-production-output so that it can distinguish
;;;             :   between an explicit string as the first element on a list
;;;             :   and when it just happens to be a string in the current 
;;;             :   binding (to differentiate when it should use the string
;;;             :   as a format string and when it should just output it as
;;;             :   is).
;;;             : * Modified the call to print-production-output in the parsing
;;;             :   of an !output! command so that the "old style" format string
;;;             :   usage doesn't get triggered implicitly (using the change
;;;             :   described above).
;;;             : * Fixed a bug in production parsing that would lead to run time
;;;             :   errors if a buffer overwrite action didn't specify a chunk
;;;             :   or at least a variable (the variable could still bind to a
;;;             :   non-chunk at run time).
;;; 2006.11.10 Dan
;;;             : * Additional update to the parsing like the last one - now
;;;             :   direct requests must be a variable or chunk.
;;; 2006.11.20 Dan
;;;             : * Removed the special case implicit action for visual requests in
;;;             :   the production definition and replaced it with the new 
;;;             :   (generic) module-warning mechanims.
;;; 2007.06.11 Dan
;;;             : * Fixed a bug in REPLACE-VARIABLES-FOR-EVAL which would quote
;;;             :   strings that had been bound to the variable in the replacement.
;;; 2007.06.18 Dan
;;;             : * Moved slot-specs-to-chunk-spec-list to the chunk-spec file
;;;             :   and then changed the references in here to add the -fct.
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
;;; 2008.03.27 Dan
;;;             : * Fixed a nasty bug I introduced in production parsing with
;;;             :   the sorting of conditions for binding pruposes.
;;; 2008.03.28 Dan
;;;             : * Fixed !mv-bind! for the RHS.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list since
;;;             :   the macro was removed from the chunk-spec code.
;;; 2008.07.09 Dan 
;;;             : * Changed the p function so that invalid lhs conditions don't
;;;             :   lead to an error during the sort.
;;; 2008.07.22 Dan
;;;             : * Updated whynot so that it notes if a production was below
;;;             :   the utility threshold the last time it was in the conflict
;;;             :   set.
;;; 2008.08.01 Dan
;;;             : * Updated pmatches to work the same as conflict-resolution
;;;             :   since they use the same 'sorter'.
;;; 2008.08.05 Dan
;;;             : * Added the production-failure-reason function needed by the
;;;             :   new production history tool here to keep the code which
;;;             :   accesses the native production structure in the 'system'
;;;             :   code.
;;; 2008.10.23 Dan
;;;             : * Fixed a bug introduced with the "fix" on 2008/07/09 that
;;;             :   could cause an invalid LHS condition to allow the production
;;;             :   to still be created with a null LHS.
;;; 2008.11.03 Dan
;;;             : * Updated the parsing of productions so that the LHS code uses
;;;             :   the new cr-buffer-read option in the conflict resolution
;;;             :   code.
;;; 2008.11.25 Dan
;;;             : * Changed things to use add-production and remove-production
;;;             :   to better abstract away from the internals of the module.
;;;             : * Changed calls to get-production to use get-production-internal
;;;             :   to avoid the unnecessary hash-table look-up when possible.
;;;             : * Abstracting away from procedural-productions with productions-
;;;             :   list so less code is dependent on the procedural structure.
;;;             : * Clear-productions now iteratively removes the productions
;;;             :   instead of just erasing the list.
;;; 2008.12.08 Dan [1.1]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions.  No longer create lambdas for the conditions.  
;;;             :   Instead, create more specific single condition tests.
;;;             : * Whynot information can now be more specific - lists which
;;;             :   slot in a buffer caused the mismatch.
;;;             : * Pmatches (and thus whynot too) no longer computes the
;;;             :   utilities and thus it doesn't sort the conflict-set based
;;;             :   on those values.
;;; 2009.05.05 Dan
;;;             : * Changed the inequality tests so that they all use the "right"
;;;             :   test instead of only testing > and negating and then add the
;;;             :   opposite test to the implicit ones for the tree.
;;; 2009.05.22 Dan
;;;             : * Fixed a bug in production parsing which would allow something
;;;             :   like this to "work" (p name =goal> free ==> ...).
;;; 2009.06.03 Dan 
;;;             : * Added explicit conditions for the inequality tests to check
;;;             :   if a value is a number and an implicit check if a slot has
;;;             :   a constant to possibly help in building the conflict tree.
;;; 2009.08.14 Dan [1.2]
;;;             : * Updated whynot and pmatches to work better with the partial 
;;;             :   matching of productions -- they report partial match info.
;;;             :   when appropriate.
;;; 2009.09.10 Dan
;;;             : * Moved code common from this and p-star-cmd into production-parsing-
;;;             :   support.lisp in the support directory and added the require-
;;;             :   compiled for that.
;;; 2009.09.18 Dan
;;;             : * P and p* creation is now done with a single set of code which
;;;             :   is in the production-parsing-support.lisp file.
;;; 2009.10.15 Dan
;;;             : * Updated whynot & pmatches to work right with the possibility
;;;             :   of buffer search.  If the check is made at the same time as
;;;             :   the last conflict-resolution call then the buffer chunks
;;;             :   searched over is the cached set and ordering used in that
;;;             :   conflict-resolution.  Otherwise, a new set and ordering is
;;;             :   requested from the owning module.
;;; 2010.01.18 Dan
;;;             : * Pmatches-internal now uses the new :report keyword for 
;;;             :   conflict-tests to suppress the :crt info if it is enabled.
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

(require-compiled "PRODUCTION-PARSING" "ACT-R6:support;production-parsing-support")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The user functions mostly from ACT-R 5

(defun all-productions ()
  (let ((prod (get-module procedural)))
    (when prod
      (mapcar #'production-name (productions-list prod)))))

(defmacro pp (&rest productions)
  `(pp-fct ',productions))

(defun pp-fct (productions)
  (let ((prod (get-module procedural)))
    (if prod
        (let ((res nil)
              (p (if (null productions) 
                     (mapcar #'production-name (productions-list prod))
                   productions)))
          (dolist (p-name p)
            (let ((production (get-production-internal p-name prod)))
              (if production
                  (progn
                    (print-production production)
                    (push p-name res))
                (print-warning "No production named ~S is defined" p-name))))
          (reverse res))
      (print-warning "No procedural module found"))))

(defun clear-productions ()
  (let ((prod (get-module procedural)))
    (if prod
        (progn
          (print-warning "Clearing the productions is not recommended")
          (dolist (p (productions-list prod))
            (remove-production p prod)))
      (print-warning "No procedural module was found."))))


(defmacro pbreak (&rest productions)
  `(pbreak-fct ',productions))

(defun pbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production productions)
          (aif (get-production production)
               (setf (production-break it) t)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-break (get-production production))
              (push production res)))))
    (print-warning "There is no current model - pbreak cannot be used.")))


(defmacro punbreak (&rest productions)
  `(punbreak-fct ',productions))

(defun punbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production (if (null productions)
                                (all-productions)
                              productions))
          (aif (get-production production)
               (setf (production-break it) nil)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-break (get-production production))
              (push production res)))))
    (print-warning "There is no current model - punbreak cannot be used.")))
  



(defmacro pdisable (&rest productions)
  `(pdisable-fct ',productions))

(defun pdisable-fct (productions)
  (if (current-model)
      (progn
        (dolist (production productions)
          (aif (get-production production)
               (setf (production-disabled it) t)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-disabled (get-production production))
              (push production res)))))
    (print-warning "There is no current model - pdisable cannot be used.")))

(defmacro penable (&rest productions)
  `(penable-fct ',productions))

(defun penable-fct (productions)
  (if (current-model)
      (progn
        (dolist (production (if (null productions)
                                (all-productions)
                              productions))
          (aif (get-production production)
               (setf (production-disabled it) nil)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-disabled (get-production production))
              (push production res)))))
    (print-warning "There is no current model - penable cannot be used.")))

(defmacro whynot (&rest productions)
  `(whynot-fct ',productions))


(defun whynot-fct (productions)
  (if (current-model) 
      (let* ((procedural (get-module procedural))
             (conflict-set (no-output (pmatches-internal procedural))))
        
        (dolist (production-name (if (null productions)
                                     (all-productions)
                                   productions))
          
          (let ((production (get-production production-name)))
            (if (null production)
                (command-output "~%~s does not name a production." production-name)
              (if (production-disabled production)
                  (command-output "~%Production ~s is disabled." production-name)
                (if (member production-name conflict-set)
                    (if (and (procedural-ppm procedural) (production-partial-matched-slots production))
                        (progn ;; It's only a partial match
                          (command-output "~%Production ~s partially matches the current state:" production-name)
                          (print-instantiation production)
                          (let ((ut (car (no-output (sgp :ut)))))
                            (when (and (numberp ut)
                                       (numberp (production-utility production-name))
                                       (< (production-utility production-name) ut))
                              (command-output "Utility was below the threshold the last time it was in the conflict set."))))
                      (progn ;; It's a complete match
                        
                        (command-output "~%Production ~s matches:" production-name)
                        (print-instantiation production)
                        (let ((ut (car (no-output (sgp :ut)))))
                          (when (and (numberp ut)
                                     (numberp (production-utility production-name))
                                     (< (production-utility production-name) ut))
                            (command-output "Utility was below the threshold the last time it was in the conflict set.")))))
                  (progn
                    (command-output "~%Production ~s does NOT match." production-name)
                    
                    (print-production production)
                    
                    (command-output "It fails because: ")
                    (command-output (failure-reason-string (production-failure-condition production) procedural production))))))))
        conflict-set)
    (print-warning "Whynot called with no current model.")))


(defun production-failure-reason (p-name)
  (let ((procedural (get-module procedural))
        (production (get-production p-name)))
    (if (and production (production-failure-condition production))
        (failure-reason-string (production-failure-condition production) procedural production)
      "")))

(defun pmatches ()
  (let ((procedural (get-module procedural)))
    (if procedural
        (pmatches-internal procedural)
      (print-warning "No procedural module found"))))
    
(defun pmatches-internal (procedural)
  
  (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
  (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested))
  
  (unless (and (numberp (procedural-last-cr-time procedural)) (= (procedural-last-cr-time procedural) (mp-time)))
    (clrhash (procedural-search-buffer-table procedural)))
  
    (let* ((conflict-set nil)
         (hook-set nil)
         (best nil)
         (best-ut (minimum-utility))
         (mu best-ut)
         (offsets-table (make-hash-table))
         
         (saved-search-chunks (make-hash-table)))
    
    
    (dolist (b (procedural-used-search-buffers procedural))
      (aif (buffer-read b)
           (setf (gethash b saved-search-chunks) it)
           (setf (gethash b saved-search-chunks) :clear)))
    
    
    (dolist (production (procedural-productions procedural))
      
      (setf (production-bindings production) nil)
      (setf (production-failure-condition production) nil)
      
      (setf (procedural-current-p procedural) production)
      (setf (production-partial-matched-slots production) nil)
      
      (unless (production-disabled production)
                
        (when (and (conflict-tests procedural (production-constants production) production 'test-constant-condition :report nil)
                   (conflict-tests procedural (production-binds production) production 'test-and-perfrom-bindings :report nil)
                   (conflict-tests procedural (production-others production) production 'test-other-condition :report nil)
                   (conflict-tests procedural (production-searches production) production 'test-search-buffers :report nil)
                   (conflict-tests procedural (production-search-binds production) production 'test-and-perfrom-bindings :report nil)
                   (conflict-tests procedural (production-search-others production) production 'test-other-condition :report nil)
                   ) 
          
          (push-last production conflict-set))))
      
      
      (dolist (b (procedural-used-search-buffers procedural))
        (let ((val (gethash b saved-search-chunks)))
          (when val
            (if (eq val :clear)
                  (erase-buffer b)
              (overwrite-buffer-chunk b val)))))
             
      
      (dolist (production conflict-set)
        (print-instantiation production))
    
      (mapcar #'production-name conflict-set)))
  
  

                                         
                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This section is production parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun define-p-fct (definition)
  (p-fct definition))

(defmacro p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun p-fct (definition)
  (let ((prod (get-module procedural)))  
    (if (procedural-p prod)  
        (create-production prod definition nil) 
      (print-warning "No procedural modulue found cannot create production."))))


(defun delete-production (prod-name)
  (let ((procedural (get-module procedural)))
    (if procedural
        (remove-production (get-production-internal prod-name procedural) procedural)
      (print-warning "No procedural module found.  Cannot delete production ~S." prod-name))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A trace filter to restrict things to production firing only.

(defun production-firing-only (event)
  "Filter to show only production firing in the trace"
  (eq (evt-action event) 'production-fired))

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
