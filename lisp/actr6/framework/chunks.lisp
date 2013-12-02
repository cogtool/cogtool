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
;;; Filename    : chunks.lisp
;;; Version     : 1.2
;;; 
;;; Description : Definition of chunks and the function that manipulate them.
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Finish the documentation.
;;;             : * This one is a big target for benchmarking and optimizing.
;;;             : * Should merge-chunks impact chunk-copied-from?
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.16 Dan
;;;             : * Added chunk-copied-from.
;;;             : * Reduced most things to 80 columns (I don't want to split
;;;             :   the format string because I've had problems with the ~
;;;             :   new-line breaking with "non-native" line endings).
;;;             : * Added doc strings.
;;;             : * Modified pprint-a-chunk so it can print with or without
;;;             :   the parameters.
;;;             : * Removed the print-chunk-type function since I don't want to
;;;             :   hide the structure since users shouldn't see them anyway.
;;; 2005.01.17 Dan
;;;             : * Switched to using command-output for printing.
;;;             : * Renamed pprint-chunk pprint-chunkS and took away its
;;;             :   printing of chunk parameters and added pprint-chunks-plus 
;;;             :   to display chunks with chunk parameters. 
;;; 2005.01.21 Dan
;;;             : * Updated merge-chunks-fct to work more efficiently.
;;; 2005.01.24 Dan
;;;             : * Fixed some bugs I introduced with the changes to pprint-
;;;             :   chunks and pprint-chunks-plus - I changed their return 
;;;             :   value which broke other things...
;;; 2005.02.04 Dan
;;;             : * Added the fast-* chunk accessors to eliminate the 
;;;             :   excessive calling of valid-slot-name.
;;; 2005.02.09 Dan
;;;             : * Fixed a bug that the fast-* stuff introduced with respect
;;;             :   to printing chunks.
;;; 2005.02.11 Dan
;;;             : * Some general clean up in define-chunks-fct.
;;; 2005.03.24 Dan
;;;             : * Changed the pprint-a-chunk function because it turns out
;;;             :   that some Lisps don't like using the pre-formatted format
;;;             :   string with the ~? directive.
;;; 2005.03.25 Dan
;;;             : * Changed pprint-a-chunk so that the slots print in the
;;;             :   same order as the chunk-type.
;;; 2005.04.01 Dan
;;;             : * Added true-chunk-name to help with an issue in merging
;;;             :   and may want to use it in printing and elsewhere...
;;; 2005.04.07 Dan
;;;             : * Fixed a minor issue with define-chunks and how it creates
;;;             :   the name for a chunk without one specified.
;;; 2005.05.07 Dan
;;;             : * Changed copy-chunk-fct so that instead of naming the new
;;;             :   chunk based on the chunk-type it bases it on the actual
;;;             :   name of the chunk being copied.  I think this is easier
;;;             :   to follow in the traces, but maybe it's more confusing.
;;;             :   We'll find out from experience I guess and then determine
;;;             :   which is better...
;;; 2005.06.11 Dan
;;;             : * DOH!  I remember again why I used the chunktype for the
;;;             :   name of the copy - because vision for example uses things
;;;             :   like loc1 which now when copied ends up as loc10 which
;;;             :   of course looks like "loc"+"10" instead of "loc1"+"0".
;;;             :   So, I've changed it so that it adds a - between the
;;;             :   chunk's name and the number so that would be loc1-0.             
;;; 2005.08.10 Dan
;;;             : * Minor clean-up in define-chunks to remove unused variables
;;;             :   in the let.
;;;             : * Updated version to 1.0.
;;; 2005.09.14 Dan
;;;             : * Fixed a bug in the output of a warning in define-chunks-fct
;;;             :   because invalid slot names weren't printed.
;;; 2005.11.17 Dan
;;;             : * Fixed some bugs in define-chunks-fct and pprint-a-chunk
;;;             :   related to default slot values in the chunk-type.
;;; 2006.01.03 Dan
;;;             : * Modified extend-chunks to remove the explicit compile call
;;;             :   (but still result in a compiled function at all times) to
;;;             :   hopefully get around the CMUCL issue.
;;; 2006.01.18 Dan
;;;             : * Modified the chunk printing function so that it can suppress
;;;             :   the "unfilled" extended slots of a chunk if desired.
;;; 2006.02.20 Dan
;;;             : * Fixed a bug in extend-chunks that causes problems with chunk 
;;;             :   parameters when merged when the ACT-R is both compiled and
;;;             :   loaded in the same session i.e. if one loads a previously
;;;             :   compiled version there's no problem so it shouldn't have
;;;             :   caused too many problems.
;;; 2006.07.06 Dan
;;;             : * Fixed a bug in define-chunks-fct.  When a chunk-type 
;;;             :   specified a default value for a slot which was a symbol (thus
;;;             :   interepreted as a chunk name) nothing ever created such a
;;;             :   chunk if it wasn't defined.  It doesn't make sense to do it
;;;             :   at the time of the chunk-type definition (because you may not
;;;             :   be able to create the chunk you want first) so it now happens
;;;             :   when such a slot value gets set (just like it does for any
;;;             :   non-chunk name symbols in the specified chunk slots).
;;; 2006.07.10 Dan
;;;             : * Added get-chunk-warn for use in several of the "user" functions
;;;             :   because they don't provide a warning if the chunk-name is
;;;             :   invalid, but since get-chunk is used for other purposes, 
;;;             :   I don't want to change it directly.
;;;             : * Added changed true-chunk-name to true-chunk-name-fct and
;;;             :   added a macro for true-chunk-name to make it user accessible.
;;; 2006.07.11 Dan
;;;             : * Made merge-chunks "safe" because previously it would merge
;;;             :   un-equal chunks as long as both items were really chunks.
;;;             :   Didn't cause problems since DM did the check first anyway,
;;;             :   but may be an issue if other modules were to use it.
;;; 2006.08.08 Dan
;;;             : * Put a test into define-chunks-fct so that it doesn't result
;;;             :   in errors for malformed add-dm/define-chunks calls, but just
;;;             :   prints a warning.
;;; 2006.10.10 Dan
;;;             : * Added the normalize-chunk-names command which goes through
;;;             :   all of the model's chunks and replaces any refrence to a
;;;             :   chunk name in a slot with the chunk's "true" name and then
;;;             :   optionally releases any non-true name i.e. the name that
;;;             :   was "merged away".  Generally, this probably won't see 
;;;             :   much use, but cleaning up the references may be useful at
;;;             :   times, and if a model creates so many names that the symbol
;;;             :   table becomes a memory limiter clearing those out maybe
;;;             :   necessary.
;;; 2006.10.17 Dan
;;;             : * Minor bug fix in normalize-chunk-names for the unintern 
;;;             :   clause.
;;; 2006.10.20 Dan
;;;             : * More clean-up added to normalize-chunk-names - should free
;;;             :   up more memory in the unintern case now.
;;; 2007.01.04 Dan
;;;             : * Minor tweak to chunk-copied-from-fct to make sure that the
;;;             :   "copied-from" chunk still exists - which may not be the case
;;;             :   for something like a goal or imaginal requests which delete 
;;;             :   the original.
;;; 2007.01.15 Dan
;;;             : * Bug from that last update fixed - use chunk-p-fct instead
;;;             :   of chunk-p...
;;; 2007.07.13 Dan
;;;             : * Performance enhancement for normalize-chunk-names - it
;;;             :   can skip checking the chunks for which the name change is
;;;             :   being done.  Duh!
;;; 2008.04.15 Dan
;;;             : * Performance improvement for delete-chunk-fct.  Assume that
;;;             :   the only way to get "eq" chunks in the table is through
;;;             :   merging so don't need to search the whole table to find
;;;             :   them for deletion - just use the merge-list from the chunk's
;;;             :   truename as the set of chunks to delete.
;;; 2008.04.16 Dan
;;;             : * Minor tweaks to normalize-chunk-names: if there aren't any
;;;             :   merged chunks it terminates early (unlikely situation) and
;;;             :   it now uses the fast- chunk component accessors.
;;; 2008.07.01 Dan
;;;             : * Added purge-chunk command to both delete and release the
;;;             :   name of the chunk.
;;; 2008.07.30 Dan
;;;             : * Changed an append to an nconc in merge-chunks-fct because
;;;             :   there's no need to copy the lists and performance wise it
;;;             :   makes a difference in the long run.
;;; 2008.07.31 Dan
;;;             : * Moved chunk-slot-equal from chunk-spec to here and removed
;;;             :   the equivalent equal-compare-slot-values function since
;;;             :   there don't need to be two such functions.  
;;;             : * Also improved chunk-slot-equal so that it doesn't need to
;;;             :   use eq-chunks-fct which may save 10% or more time wise for
;;;             :   models becuase it removes duplicate lookups.
;;;             : * Added the testing of val1 and val2 back into the chunk
;;;             :   case of chunk-slot-equal since a nil can short-circuit the
;;;             :   chunk lookup - slows down the chunk only cases but speeds
;;;             :   up the nil tests which is probably more common in the 
;;;             :   average model.
;;; 2008.10.08 Dan
;;;             : * Improvement to normalize-chunk-names so it doesn't have to
;;;             :   look up non-chunks.  
;;; 2008.10.20 Dan [1.1]
;;;             : * Made changes to add the option of having chunk merging 
;;;             :   work like the older ACT-R versions where it essentially 
;;;             :   normalizes as it goes. It can be enabled via the :dcnn 
;;;             :   parameter and is on by default.
;;; 2008.10.30 Dan
;;;             : * Tweaked chunk-slot-equal to make it a little more efficient.
;;; 2008.11.03 Dan [1.2]
;;;             : * Changed the internals of how chunk parameters get stored
;;;             :   from a hash-table to an array and added extra lists so that
;;;             :   copying and merging don't need to loop over all the parameters.
;;; 2008.11.04 Dan
;;;             : * Fixed a minor bug with how the parameter copy list gets
;;;             :   created because compiling and loading the chunks file would
;;;             :   lead to calling the copy functions twice.
;;; 2008.11.11 Dan
;;;             : * Fixed a bug in the chunk copy code.
;;; 2008.11.13 Dan
;;;             : * Modified chunk normalizing so that it calls the hook fn's
;;;             :   when it changes chunk slot values.
;;; 2008.12.10 Dan
;;;             : * Added the :copy-from-chunk-function keyword to extend
;;;             :   chunks because sometimes having access to the original 
;;;             :   chunk may be useful when copying a parameter.
;;; 2009.02.13 Dan
;;;             : * Modified chunk-copy-fct to better control for size since
;;;             :   the ANSI CL spec doesn't require this to be true:
;;;             :
;;;             :  (let* ((ht1 (make-hash-table))
;;;             :         (s1 (hash-table-size ht1))
;;;             :         (ht2 (make-hash-table :size s1))
;;;             :         (s2 (hash-table-size ht2)))
;;;             :    (= s1 s2))
;;;             :
;;;             :   which can result in runaway memory usage if a chunk gets
;;;             :   copied, then the copy gets copied, and so on, in a Lisp which 
;;;             :   "rounds up" the size (ACL and possibly others).
;;; 2009.02.13 Dan
;;;             : * Modified chunk-copy-fct to use the new option of "short
;;;             :   chunk copy names".  So, instead of A-0-0-0-0 one would have
;;;             :   A-3 instead.
;;; 2009.04.23 Dan
;;;             : * Fixed a bug introduced in chunk-slot-equal the last time it 
;;;             :   was updated which caused t to match any non-chunk value if
;;;             :   t was also not explicitly defined as a chunk.
;;; 2010.04.30 Dan
;;;             : * Updated delete-chunk-fct so that it doesn't print a double
;;;             :   warning for deleting a chunk which is still used and also
;;;             :   fixed an unnecessary ' in the warning.
;;; 2010.08.16 Dan
;;;             : * Fixed a bug in delete-chunk-fct that would throw an error
;;;             :   if a non-chunk were passed in.
;;; 2010.08.17 Dan
;;;             : * Changed the extend-chunk macro so that the accessor and setf
;;;             :   funtions for a parameter include the parameter in the warning
;;;             :   when there's a bad chunk name provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Globals and underlying chunk structures are not for general use.
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


#| Don't want to hide this anymore
(defun print-chunk (chunk stream depth)
  "Print a chunk as just its name."
  (declare (ignore depth))
  (format stream "~A" (act-r-chunk-name chunk)))
|#

(defvar *chunk-parameters-count* 0)

(defvar *chunk-parameter-undefined* (gentemp "Unused-Param"))

(defvar *chunk-parameters-list* nil 
  "Internal list of parameters that have been added to chunks")

(defvar *chunk-parameters-copy-list* nil
  "Internal list of parameters that have a copy function")

(defvar *chunk-parameters-merge-list* nil
  "Internal list of parameters that have a merge function")


(defun chunk-parameter-default (param chunk-name)
  "Return a default value for a parameter in a chunk"
  (if (act-r-chunk-parameter-default-function param)
      (funcall (act-r-chunk-parameter-default-function param) chunk-name)
    (act-r-chunk-parameter-default-value param)))


(defconstant *pprint-chunk-string*
    (formatter 
     "~S~:[ (~s)~;~*~]~%~@[~S~%~]  ISA ~S~%~:{   ~s  ~s~%~}")
  "compiled format string for printing chunks")

(defconstant *pprint-chunk-parameters-string*
    (formatter "~@[  --chunk parameters--~%~:{   ~s  ~s~%~}~]~%")
  "compiled format string for printing chunk parameters")

(defun pprint-a-chunk (chunk-name &optional (w-params t))
  "Internal function for printing a chunk"
  (let ((chunk (get-chunk chunk-name)))
    (if chunk
        (progn
          (command-output
           (format nil
               *pprint-chunk-string*
             chunk-name
             (eql chunk-name (act-r-chunk-name chunk))
             (act-r-chunk-name chunk)
             (act-r-chunk-documentation chunk)
             (act-r-chunk-type-name (act-r-chunk-chunk-type chunk))  
             (mapcan #'(lambda (slot-name) 
                         (multiple-value-bind (value exists) (gethash slot-name (act-r-chunk-slot-value-lists chunk))
                             (when (or exists
                                       (car (no-output (sgp-fct (list :show-all-slots))))
                                       (not (extended-slot-name-p slot-name
                                                                  (act-r-chunk-type-name (act-r-chunk-chunk-type chunk)))))
                               (list (list slot-name value)))))
               (ct-slot-names (act-r-chunk-chunk-type chunk)))
             ))
          (when w-params
            (command-output
             (format nil *pprint-chunk-parameters-string*
               (mapcar #'(lambda (param)
                           (list (act-r-chunk-parameter-name param)
                                 (funcall (act-r-chunk-parameter-accessor param) 
                                          chunk-name)))
                 *chunk-parameters-list*)))
            
            )
          chunk-name)
      
      :error)))

(defmacro pprint-chunks (&rest chunk-names)
  "Print the chunks"
  `(pprint-chunks-fct ',chunk-names))

(defun pprint-chunks-fct (&optional chunk-names-list)
  "Print the chunks"
  (verify-current-mp  
   "pprint-chunks called with no current meta-process."
   (verify-current-model
    "pprint-chunks called with no current model."
    (let ((res nil))
      (dolist (chunk (if (null chunk-names-list) (chunks) chunk-names-list) res)
        (push-last (pprint-a-chunk chunk nil) res))))))


(defun chunk-back-links (chunk-name)
  (gethash chunk-name (act-r-model-chunk-ref-table (current-model-struct))))

(defun set-chunk-back-links (chunk-name val)
  (setf (gethash chunk-name (act-r-model-chunk-ref-table (current-model-struct))) val))

(defsetf chunk-back-links set-chunk-back-links)


(defmacro pprint-chunks-plus (&rest chunk-names)
  "Print the chunks and their chunk parameters"
  `(pprint-chunks-plus-fct ',chunk-names))

(defun pprint-chunks-plus-fct (&optional chunk-names-list)
  "Print the chunks and their parameters"
  (verify-current-mp  
   "pprint-chunks-plus called with no current meta-process."
   (verify-current-model
    "pprint-chunks-plus called with no current model."
    (let ((res nil))
      (dolist (chunk (if (null chunk-names-list) (chunks) chunk-names-list) res)
        (push-last (pprint-a-chunk chunk t) res))))))


(defun chunks ()
  "Returns a list of the names of all currently defined chunks"
  (verify-current-mp  
   "chunks called with no current meta-process."
   (verify-current-model
    "chunks called with no current model."
    (hash-table-keys (act-r-model-chunks-table (current-model-struct))))))

(defun get-chunk (name)
  "Internal function for getting the chunk structure from its name"
  (verify-current-mp  
   "get-chunk called with no current meta-process."
   (verify-current-model
    "get-chunk called with no current model."
    (gethash name (act-r-model-chunks-table (current-model-struct))))))


(defun get-chunk-warn (name)
  "Internal function for getting the chunk structure from its name"
  (verify-current-mp  
   "get-chunk called with no current meta-process."
   (verify-current-model
    "get-chunk called with no current model."
    (let ((c (gethash name (act-r-model-chunks-table (current-model-struct)))))
      (if c c
        (print-warning "~s does not name a chunk in the current model." name))))))

(defmacro chunk-p (chunk-name?)
  "Check a name to see if it names a chunk"
  `(chunk-p-fct ',chunk-name?))

(defun chunk-p-fct (chunk-name?)
  "Check a name to see if it names a chunk"
  (if (get-chunk chunk-name?)
      t nil))


(defmacro chunk-chunk-type (chunk-name)
  "Return the name of the chunk-type for a chunk"
  `(chunk-chunk-type-fct ',chunk-name))

(defun chunk-chunk-type-fct (chunk-name)
  "Return the name of the chunk-type for a chunk"
    (let ((c (get-chunk-warn chunk-name)))
    (when c
      (act-r-chunk-type-name (act-r-chunk-chunk-type c)))))


(defmacro chunk-documentation (chunk-name)
  "Return the documentation string for a chunk"
  `(chunk-documentation-fct ',chunk-name))

(defun chunk-documentation-fct (chunk-name)
  "Return the documentation string for a chunk"
    (let ((c (get-chunk-warn chunk-name)))
    (when c
      (act-r-chunk-documentation c))))

  
(defun create-undefined-chunk (name)
  "Create a new chunk with the given name of chunk-type chunk with a warning"
  (model-warning "Creating chunk ~S of default type chunk" name)
  (define-chunks-fct (list (list name 'isa 'chunk))))


(defmacro copy-chunk (chunk-name)
  "Create a new chunk which is a copy of the given chunk"
  `(copy-chunk-fct ',chunk-name))

(defun copy-chunk-fct (chunk-name)
  "Create a new chunk which is a copy of the given chunk"
    (let ((chunk (get-chunk-warn chunk-name)))
    (when chunk
      (when (use-short-copy-names)
        (unless (act-r-chunk-base-name chunk)
          (setf (act-r-chunk-base-name chunk) (concatenate 'string (symbol-name chunk-name) "-"))))
      
      
      (let* ((new-name (new-name-fct (if (use-short-copy-names)
                                         (act-r-chunk-base-name chunk)
                                       (concatenate 'string (symbol-name chunk-name) "-"))))
             (new-chunk (make-act-r-chunk 
                         :name new-name
                         :base-name (act-r-chunk-base-name chunk)
                         :merged-chunks (list new-name)
                         :chunk-type (act-r-chunk-chunk-type chunk)
                         :parameter-values (make-array *chunk-parameters-count*
                                                       :initial-element *chunk-parameter-undefined*)
                         ;(copy-seq (act-r-chunk-parameter-values chunk)) 
                         :slot-value-lists 
                         (make-hash-table :size (hash-table-count (act-r-chunk-slot-value-lists chunk))))))
        
        ;; Copy the slot-value hash table 
        (maphash #'(lambda (key value)
                     (setf (gethash key (act-r-chunk-slot-value-lists new-chunk))
                       value))
                 (act-r-chunk-slot-value-lists chunk))
        
        ;; Create the back links as needed
        
        (when (update-chunks-on-the-fly)
          (dolist (slot-name (act-r-chunk-type-slots (act-r-chunk-chunk-type chunk)))
            (let ((old (gethash slot-name (act-r-chunk-slot-value-lists chunk))))
              (when (chunk-p-fct old)
                (push (cons new-name slot-name) (chunk-back-links old))))))
        
        ;; update its parameters for only those that need it
        
        (dolist (param *chunk-parameters-copy-list*)
          (if (act-r-chunk-parameter-copy param)
              (let ((current (aref (act-r-chunk-parameter-values chunk) (act-r-chunk-parameter-index param))))
                (setf (aref (act-r-chunk-parameter-values new-chunk) (act-r-chunk-parameter-index param))
                  (funcall (act-r-chunk-parameter-copy param) 
                           (if (eq current *chunk-parameter-undefined*)
                               (chunk-parameter-default param chunk-name)
                             current))))
            (setf (aref (act-r-chunk-parameter-values new-chunk) (act-r-chunk-parameter-index param))
              (funcall (act-r-chunk-parameter-copy-from-chunk param) 
                       chunk-name))))
                        
                                              
        ;; Put it into the main table
        
        (setf (gethash new-name (act-r-model-chunks-table (current-model-struct)))
          new-chunk)
        
        ;; note the original
        
        (setf (act-r-chunk-copied-from new-chunk) chunk-name)
        
        new-name))))

(defmacro chunk-copied-from (chunk-name)
  "Return the name of the chunk from which the provided chunk was copied"
  `(chunk-copied-from-fct ',chunk-name))

(defun chunk-copied-from-fct (chunk-name)
  "Return the name of the chunk from which the provided chunk was copied"
  (let ((chunk (get-chunk-warn chunk-name)))
    (when chunk
      (let ((copied-from (act-r-chunk-copied-from chunk)))
        (when (and copied-from (chunk-p-fct copied-from) (equal-chunks-fct chunk-name copied-from))
          copied-from)))))


(defmacro define-chunks (&rest chunk-defs)
  "Create chunks in the current model"
  `(define-chunks-fct ',chunk-defs))

(defun define-chunks-fct (chunk-def-list)
  "Create chunks in the current model"
  
  ;; Do it in 2 passes like the old add-dm because there could be 
  ;; circular references which should be allowed
  
  (verify-current-mp  
   "define-chunks called with no current meta-process."
   (verify-current-model
    "define-chunks called with no current model."
    (let ((chunk-list nil))
        ;; first pass just create the chunks
        (dolist (chunk-def chunk-def-list)
          (if (listp chunk-def)
              (let (name doc type slots slots-and-values
                         (pos (position 'isa chunk-def)))
                (cond ((not (find 'isa chunk-def))
                       (print-warning 
                        "Invalid chunk definition: ~S has no ISA specified."
                        chunk-def))
                      ((> (count 'isa chunk-def) 1)
                       (print-warning 
                        "Invalid chunk definition: ~S has more than one ISA."
                        chunk-def))
                      ((= (1+ pos) (length chunk-def))
                       (print-warning 
                        "Invalid chunk definition: ~S no chunk-type specified after ISA." 
                        chunk-def))
                      ((not (get-chunk-type 
                             (nth (1+ pos) chunk-def)))
                       (print-warning 
                        "Invalid chunk definition: ~S chunk-type specified does not exist."
                        chunk-def))
                      (t
                       (setf type 
                         (get-chunk-type 
                          (nth (1+ pos) chunk-def)))
                       (setf slots-and-values 
                         (subseq chunk-def (+ 2 pos)))
                       (cond ((> pos 2)
                              (print-warning 
                               "Invalid chunk definition: ~S too many specifiers before ISA." 
                               chunk-def))
                             (t
                              (cond ((= pos 0)
                                     (setf name (new-name-fct (symbol-name (act-r-chunk-type-name type)))))
                                    ((= pos 1)
                                     (setf name (first chunk-def)))
                                    ((= pos 2)
                                     (setf name (first chunk-def))
                                     (setf doc (second chunk-def))))
                              
                              (cond ((or (null name) (not (symbolp name)))
                                     (print-warning 
                                      "Invalid chunk definition: ~S chunk name is not a valid symbol." 
                                      chunk-def))
                                    ((and doc (not (stringp doc)))
                                     (print-warning 
                                      "Invalid chunk definition: ~S documentation is not a string."
                                      chunk-def))
                                    ((oddp (length slots-and-values))
                                     (print-warning 
                                      "Invalid chunk definition: ~S slot and values list is an odd length."
                                      chunk-def))
                                    ((chunk-p-fct name)
                                     (print-warning 
                                      "Invalid chunk definition: ~S names a chunk which already exists."
                                      chunk-def))
                                    (t
                                     (do ((s slots-and-values (cddr s)))
                                         ((null s))
                                       (if (valid-slot-name (car s) type)
                                           (push (car s) slots)
                                         (progn
                                           (print-warning 
                                            "Invalid chunk definition: ~S invalid slot name ~s."
                                            chunk-def (car s))
                                           (setf s nil)
                                           (setf slots :error))))
                                     (cond ((eq slots :error))
                                           ; Don't worry about this...
                                           ;((not 
                                           ;  (= (length slots) 
                                           ;     (length (remove-duplicates slots))))
                                           ; (print-warning 
                                           ;  "Invalid chunk definition: ~S slot name used more than once."
                                           ;  chunk-def))
                                           (t 
                                            (let ((c (make-act-r-chunk 
                                                      :name name
                                                      :merged-chunks (list name)
                                                      :documentation doc
                                                      :chunk-type type
                                                      :parameter-values (make-array *chunk-parameters-count*
                                                                                    :initial-element *chunk-parameter-undefined*)
                                                      :slot-value-lists slots-and-values)))
                                              (push-last c chunk-list)
                                              
                                              ;; enter it into the main chunk table
                                              (setf (gethash name (act-r-model-chunks-table (current-model-struct)))
                                                c)))))))))))
            (model-warning "~S is not a list in call to define-chunks-fct" chunk-def)))
      
      ;; second pass create slot-value list and define parameters
      
      (dolist (chunk chunk-list)
        
        (let ((slots-table 
               (make-hash-table :size (length (ct-slot-names (act-r-chunk-chunk-type chunk)))))
              (ct (act-r-chunk-chunk-type chunk)))
          
          (do* ((all-slots (ct-slot-names ct))
                (s (act-r-chunk-slot-value-lists chunk) (cddr s))
                (slot-name (car s) (car s))
                (slot-value (cadr s) (cadr s)))
               ((null s)
                (dolist (sn all-slots)
                  (awhen (ct-slot-default ct sn)
                         (when (and (symbolp it)
                                    (not (chunk-p-fct it))
                                    (not (numberp it))
                                    (not (eq t it)))
                           (create-undefined-chunk it))
                         
                         (setf (gethash sn slots-table) it))))
            
            ;(push (list (car s) (second s)) slots))
            
            (setf all-slots (remove slot-name all-slots))
            
            (when (and slot-value (symbolp slot-value) 
                       (not (chunk-p-fct slot-value))
                       (not (numberp slot-value))
                       (not (eq t slot-value)))
              (create-undefined-chunk slot-value))
            
            ;; if updates are happening on the fly map the value to
            ;; the "true" name
  
            (when (and (chunk-p-fct slot-value) (update-chunks-on-the-fly))
              (setf slot-value (true-chunk-name-fct slot-value))
              ;; make the back links
              (push (cons (act-r-chunk-name chunk) slot-name) (chunk-back-links slot-value)))
            
            (setf (gethash slot-name slots-table) slot-value))
          
          (setf (act-r-chunk-slot-value-lists chunk) slots-table))
        
        ;;; Don't do this because
        ;;; - it doesn't work if a default function references a module 
        ;;; which doesn't exist when creating chunks in other modules at 
        ;;; model start up time.
        ;;; - performance testing suggests that the extra if in the
        ;;; accessor is cheap relative to cost of initializing the
        ;;; parameters for chunks which don't need them set.
        
        #|(dolist (param *chunk-parameters-list*)
          (setf (aref (act-r-chunk-parameter-values chunk) (act-r-chunk-parameter-index param))
            (chunk-parameter-default param (act-r-chunk-name chunk))))|#
        )
      
      (mapcar #'act-r-chunk-name chunk-list)))))
        

(defun chk-slot-value (chunk slot-name)
  "Internal function for getting the value of a slot in a chunk structure"
  ;(second (find slot-name (act-r-chunk-slot-value-lists chunk) :key #'car))
  (gethash slot-name (act-r-chunk-slot-value-lists chunk))
  )

(defmacro chunk-slot-value (chunk-name slot-name)
  "Return the value of a slot for the named chunk"
  `(chunk-slot-value-fct ',chunk-name ',slot-name))

(defun chunk-slot-value-fct (chunk-name slot-name)
  "Return the value of a slot for the named chunk"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if (valid-slot-name slot-name (act-r-chunk-chunk-type c))
          (chk-slot-value c slot-name)
        (print-warning 
         "chunk ~S does not have a slot called ~S." chunk-name slot-name)))))


(defmacro set-chunk-slot-value (chunk-name slot-name value)
  "Set the value of a chunk's slot"
  `(set-chunk-slot-value-fct ',chunk-name ',slot-name ',value))

(defun set-chunk-slot-value-fct (chunk-name slot-name value)
  "Set the value of a chunk's slot"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if (valid-slot-name slot-name (act-r-chunk-chunk-type c))
          (set-chk-slot-value c slot-name value)
        (print-warning "chunk ~S does not have a slot called ~S." chunk-name slot-name)))))
  

(defun set-chk-slot-value (c slot-name value)
  "internal chunk slot setting function"
  ;; changing the chunk breaks it as a copy
  (setf (act-r-chunk-copied-from c) nil)
  
  ;; If the value in the slot now is a chunk
  ;; remove this chunk from the back links of that
  ;; chunk
  
  (when (update-chunks-on-the-fly)
    (let ((old (gethash slot-name (act-r-chunk-slot-value-lists c))))
      (when (chunk-p-fct old)
        (setf (chunk-back-links old) 
          (remove (cons (act-r-chunk-name c) slot-name) (chunk-back-links old) :test #'equal)))))
  
  ;; If the new value should be a chunk but isn't
  ;; create one for it
  
  (when (and value (symbolp value) 
             (not (chunk-p-fct value))
             (not (numberp value))
             (not (eq t value)))
    (create-undefined-chunk value))
  
  ;; if updates are happening on the fly map the value to
  ;; the "true" name
  
  (when (and (chunk-p-fct value) (update-chunks-on-the-fly))
    (setf value (true-chunk-name-fct value))
    ;; If it's a chunk save the back link to this chunk
    (push (cons (act-r-chunk-name c) slot-name) (chunk-back-links value)))
  
  ;; Set the new slot value
  
  (setf (gethash slot-name (act-r-chunk-slot-value-lists c)) value))


(defmacro mod-chunk (chunk-name &rest modifications)
  "Modify the slot values of a chunk"
  `(mod-chunk-fct ',chunk-name ',modifications))

(defun mod-chunk-fct (chunk-name modifications-list)
  "Modify the slot values of a chunk"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if (oddp (length modifications-list))
          (print-warning "Odd length modifications list in call to mod-chunk.")
        (let ((slots nil)
              (slots-and-values nil))
          (do ((s modifications-list (cddr s)))
              ((null s))
            (push (car s) slots)
            (push (list (car s) (second s)) slots-and-values))
          (cond ((not (every #'(lambda (slot)
                                 (valid-slot-name 
                                  slot (act-r-chunk-chunk-type c)))
                             slots))
                 (print-warning "Invalid slot name in modifications list."))
                ((not (= (length slots) (length (remove-duplicates slots))))
                 (print-warning 
                  "Slot name used more than once in modifications list."))
                (t
                 (dolist (slot-value slots-and-values chunk-name)
                   (set-chk-slot-value 
                    c (first slot-value) (second slot-value))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to potentially speed things way up provide un-checked but fast accessors
;;; to the chunk info...


(defun fast-chunk-slot-value-fct (chunk-name slot-name)
  "Return the value of a slot for the named chunk without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (chk-slot-value c slot-name))))
      
(defun fast-set-chunk-slot-value-fct (chunk-name slot-name value)
  "Set the value of a chunk's slot without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (set-chk-slot-value c slot-name value))))


(defun fast-mod-chunk-fct (chunk-name modifications-list)
  "Modify the slot values of a chunk without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (unless (oddp (length modifications-list))
        (loop 
          (when (null modifications-list) (return))
          (set-chk-slot-value 
           c
           (pop modifications-list)
           (pop modifications-list)))
        chunk-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro delete-chunk (chunk-name)
  "Delete a chunk from a model"
  `(delete-chunk-fct ',chunk-name))

(defun delete-chunk-fct (chunk-name)
  "Delete a chunk from a model"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (let ((tn (act-r-chunk-name c)))
        ;; If this chunk has back-links from others to it then warn because
        ;; that's likely a problem
        
        (when (update-chunks-on-the-fly) 
          (when (chunk-back-links chunk-name)
            (model-warning "Chunk ~s is being deleted but it is still used as a slot value in other chunks." chunk-name))
          
          (when (and (chunk-back-links tn) (not (eq tn chunk-name)))
            (model-warning "Chunk ~s is being deleted but its true name ~s is still used as a slot value in other chunks." chunk-name tn))
          
          ;; Delete all of the back-links to this chunk
          
          (dolist (slot-name (act-r-chunk-type-slots (act-r-chunk-chunk-type c)))
            (let ((old (gethash slot-name (act-r-chunk-slot-value-lists c))))
              (when (chunk-p-fct old)
                (setf (chunk-back-links old) 
                  (remove (cons tn slot-name) (chunk-back-links old) :test #'equal))))))
        
        ;; Take all the related chunks out of the main hash-table
        
        (dolist (x (act-r-chunk-merged-chunks c))
          (remhash x (act-r-model-chunks-table (current-model-struct)))
          
          ;; Take them out of the meta-data table too
          
          (when (update-chunks-on-the-fly)
            (remhash x (act-r-model-chunk-ref-table (current-model-struct)))))
        
        chunk-name))))

(defmacro purge-chunk (chunk-name)
  "delete a chunk and release its name"
  `(purge-chunk-fct ',chunk-name))

(defun purge-chunk-fct (chunk-name)
  (let ((name (delete-chunk-fct chunk-name)))
    (when name
      (release-name-fct name))))

(defmacro merge-chunks (chunk-name1 chunk-name2)
  "Merge two chunks into a single representation"
  `(merge-chunks-fct ',chunk-name1 ',chunk-name2))

(defun merge-chunks-fct (chunk-name1 chunk-name2)
  "Merge two chunks into a single representation"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (when (and c1 c2)
      (unless (chunk-equal-test c1 c2)
        (return-from merge-chunks-fct nil))
      (unless (eq c1 c2)
        
        ;; update the parameters for c1
        
        (dolist (param *chunk-parameters-merge-list*)
          (setf (aref (act-r-chunk-parameter-values c1) (act-r-chunk-parameter-index param))
            (funcall (act-r-chunk-parameter-merge param) chunk-name1 chunk-name2)))
        
        
        ;; For any chunks which had been merged with c2 also remap them
        ;; and indicate them in c1
        
        (dolist (x (act-r-chunk-merged-chunks c2))
          (setf (gethash x (act-r-model-chunks-table (current-model-struct))) c1)
          (push x (act-r-chunk-merged-chunks c1)))
                
        
        ;; When name-remapping is on
        
        (when (update-chunks-on-the-fly)
          
          ;; delete all back-links to the c2 chunk
          
          (dolist (slot-name (act-r-chunk-type-slots (act-r-chunk-chunk-type c2)))
            (let ((old (gethash slot-name (act-r-chunk-slot-value-lists c2))))
              (when (chunk-p-fct old)
                (setf (chunk-back-links old) 
                  (remove (cons chunk-name2 slot-name) (chunk-back-links old) :test #'equal)))))
          
          ;; replace all the slot values which hold chunk-name2 with chunk-name1
          
          (dolist (x (chunk-back-links chunk-name2))
            (fast-set-chunk-slot-value-fct (car x) (cdr x) chunk-name1)
            (dolist (notify (notify-on-the-fly-hooks))
              (funcall notify (car x))))
          
          (setf (chunk-back-links chunk-name2) nil)))
      
      chunk-name1)))


(defmacro eq-chunks (chunk-name1 chunk-name2)
  "Return t if two chunks have the same underlying representation"
  `(eq-chunks-fct ',chunk-name1 ',chunk-name2))

(defun eq-chunks-fct (chunk-name1 chunk-name2)
  "Return t if two chunks have the same underlying representation"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (and c1 c2 (eq c1 c2))))

(defmacro true-chunk-name (chunk-name)
  "Return the prototypical name of a chunk in the event of merging"
  `(true-chunk-name-fct ',chunk-name))

(defun true-chunk-name-fct (chunk-name)
  "Return the prototypical name of a chunk in the event of merging"
  (let ((c (get-chunk chunk-name)))
    (if c
        (act-r-chunk-name c)
      chunk-name)))

(defmacro equal-chunks (chunk-name1 chunk-name2)
  "Return t if two chunks are of the same chunk-type and have equal slot values"
  `(equal-chunks-fct ',chunk-name1 ',chunk-name2))

(defun equal-chunks-fct (chunk-name1 chunk-name2)
  "Return t if two chunks are of the same chunk-type and have equal slot values"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (chunk-equal-test c1 c2)))


(defun chunk-equal-test (c1 c2)
  "Internal function for comparing the equality of two chunks"
  (and c1 c2 (or (eq c1 c2)
                   (and (eq (act-r-chunk-chunk-type c1)
                            (act-r-chunk-chunk-type c2))
                        (every #'(lambda (slot-name) 
                                   (chunk-slot-equal
                                    (chk-slot-value c1 slot-name)
                                    (chk-slot-value c2 slot-name)))
                               (ct-slot-names (act-r-chunk-chunk-type c1)))))))


(defun chunk-slot-equal (val1 val2)
  (if (eq val1 val2)
      t
    (let (c1 c2)
      (cond ((and (setf c1 (get-chunk val1))
                  (setf c2 (get-chunk val2)))
             (eq c1 c2))
            ((stringp val1) 
             (and (stringp val2) (string-equal val1 val2)))
            (t (equalp val1 val2))))))


(defmacro extend-chunks (parameter-name &key (default-value nil)
                                        (default-function nil)
                                        (merge-function nil)
                                        (copy-function nil)
                                        (copy-from-chunk-function nil))
  "Add new parameters to all chunks"
  (let ((accessor-name (intern (concatenate 'string "CHUNK-"  (string-upcase parameter-name))))
        (setf-name (intern (concatenate 'string "CHUNK-" (string-upcase parameter-name) "-SETF")))
        (index (gensym))
        (exists (gensym))
        (param (gensym)))
    (if (find parameter-name *chunk-parameters-list* :key #'act-r-chunk-parameter-name)
         (progn
           (print-warning "Parameter ~s already defined for chunks." parameter-name)
           :duplicate-parameter)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when (or (fboundp ',setf-name) (fboundp ',accessor-name))
           (print-warning "The following ~:[~;2 ~]warning~:[~;s~] can be ignored for the main ACT-R modules provided, but may be a serious problem if seen otherwise." 
                          (and (fboundp ',setf-name) (fboundp ',accessor-name))
                          (and (fboundp ',setf-name) (fboundp ',accessor-name))))
         (when (fboundp ',accessor-name)
           (print-warning "Function ~s already exists and is being redefined." ',accessor-name))
         (when (fboundp ',setf-name)
           (print-warning "Function ~s already exists and is being redefined." ',setf-name))
         (let* ((,index *chunk-parameters-count*)
                (,exists (find ',parameter-name *chunk-parameters-list* :key #'act-r-chunk-parameter-name))
                (,param (make-act-r-chunk-parameter :name ',parameter-name
                                                    :index ,index
                                                    :default-value ',default-value
                                                    :default-function ',default-function
                                                    :merge ',merge-function
                                                    :copy ',copy-function
                                                    :copy-from-chunk ',copy-from-chunk-function
                                                    :accessor ',accessor-name)))
           (if ,exists
              (progn
                (setf ,index (act-r-chunk-parameter-index ,exists))
                (setf *chunk-parameters-list* (remove ,exists *chunk-parameters-list*))
                (setf *chunk-parameters-copy-list* (remove ,exists *chunk-parameters-copy-list*))
                (setf *chunk-parameters-merge-list* (remove ,exists *chunk-parameters-merge-list*))
                (setf (act-r-chunk-parameter-index ,param) (act-r-chunk-parameter-index ,exists)))
              (incf *chunk-parameters-count*))
         
           (push ,param *chunk-parameters-list*)
           
           (if ',copy-function 
               (push ,param *chunk-parameters-copy-list*)
             (when ',copy-from-chunk-function
               (push ,param *chunk-parameters-copy-list*)))
           
           (when ',merge-function
             (push ,param *chunk-parameters-merge-list*))
           
         (defun ,accessor-name (chunk-name)
           (let ((c (get-chunk chunk-name)))
             (if c
                 (let ((v (aref (act-r-chunk-parameter-values c) ,index)))
                   (if (eq v *chunk-parameter-undefined*)
                       (setf (aref (act-r-chunk-parameter-values c) ,index)
                         (chunk-parameter-default ,param chunk-name))
                     v))
               (print-warning "Chunk ~s does not exist in attempt to access ~a." chunk-name ',accessor-name))))
         (defun ,setf-name (chunk-name new-value)
           (let ((c (get-chunk chunk-name)))
             (if c
                 (setf (aref (act-r-chunk-parameter-values c) ,index) new-value)
               (print-warning "Chunk ~s does not exist in attempt to set ~a." chunk-name ',accessor-name))))
         (defsetf ,accessor-name ,setf-name)
         ',accessor-name)))))



(defun normalize-chunk-names (&optional (unintern? nil))
  (if (current-model-struct)
      (if (update-chunks-on-the-fly) 
          
          ;; Use the meta-data table to do the work
          (maphash (lambda (key value)
                     (when (not (eq key (act-r-chunk-name value))) ;; not a used chunk
                       
                       ;; Square up all names for unused chunks
                       
                       (awhen (chunk-back-links key)
                              (let ((tn (true-chunk-name-fct key)))
                                (dolist (x it)
                                  (fast-set-chunk-slot-value-fct (car x) (cdr x) tn)
                                  (dolist (notify (notify-on-the-fly-hooks))
                                    (funcall notify (car x)))))
                              (setf (chunk-back-links key) nil))
                       
                       ;; release names of unused chunks
                       
                       (when unintern?
                         
                         ;; Take it out of the main hash-table
                         (remhash key (act-r-model-chunks-table (current-model-struct)))
                         
                         ;; Take it out of the meta-data table too
                         (remhash key (act-r-model-chunk-ref-table (current-model-struct)))
                         
                         ;; unintern the name
                         (release-name-fct key))))
                   (act-r-model-chunks-table (current-model-struct)))
        
        ;; Without the meta-data do it the hard way
        (let ((possible-removals nil))
          (maphash (lambda (key value)
                     (when (not (eq key (act-r-chunk-name value)))
                       (push key possible-removals)))
                   (act-r-model-chunks-table (current-model-struct)))
          
          ;; clean up the chunk references
          ;; this could take a while
          
          (when possible-removals
            (maphash (lambda (chunk val)
                       (when (eq chunk (act-r-chunk-name val))
                         (dolist (slot (chunk-type-slot-names-fct (chunk-chunk-type-fct chunk)))
                           (let ((value (fast-chunk-slot-value-fct chunk slot)))
                             (when (and (chunk-p-fct value) (member value possible-removals))
                               (fast-set-chunk-slot-value-fct chunk slot (true-chunk-name-fct value))
                               (dolist (notify (notify-on-the-fly-hooks))
                                 (funcall notify chunk)))))))
                     (act-r-model-chunks-table (current-model-struct))))
           
           (when unintern?
             (dolist (x possible-removals)
               (remhash x (act-r-model-chunks-table (current-model-struct)))
               (release-name-fct x)))))
       
       (print-warning "No current model in which to normalize chunk names.")))


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
