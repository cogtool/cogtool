;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell & Mike Byrne
;;; Copyright   : (c) 2004-10 Dan Bothell/Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : misc-utils.lisp
;;; Version     : 1.0
;;; 
;;; Description : Various useful functions that don't seem to belong anywhere
;;;               else (many of which come directly from the old RPM code).
;;; 
;;; Bugs        : * The "-output" commands break if passed strings with format
;;;             :   control sequences in them.
;;;
;;; To do       : * Add most if not all of these to the official API.
;;;             : * Add all the ones copied from rpm to the public API section.
;;;             : * Handle different warning levels.
;;;             : * Fix an issue with MCL double printing model warnings because
;;;             :   *error-output* isn't the same as *standard-output* even 
;;;             :   though both print to the listener - grrr.
;;;             : * Possibly split the "-output" commands into macros and functions
;;;             :   like everything else because right now they're funny in that
;;;             :   they're macros but they do result in evaluating the parameters.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.12 Dan
;;;             : Creation
;;; 2005.01.17 Dan
;;;             : * Updated model-output and added command-output. I don't think
;;;             :   it needs the no-output command, but maybe it will.
;;; 2005.02.10 Dan
;;;             : * Added the expt-coerced, log-coerced, and sqrt-coerced 
;;;             :   for performance improvements in Lisps that use doubles when 
;;;             :   the default read format is singles.  
;;; 2005.02.21 Dan
;;;             : * Added some to do stuff.
;;; 2005.02.28 Dan
;;;             : * Note that the -coerced macros don't really do anything 
;;;             :   in MCL 5 or ACL, so probalby not necessary anymore...
;;;             : * Made a bunch of the output macros hygienic.
;;;             : * Added back meta-p-output to print to all models' :v stream
;;;             :   but only once per stream.
;;; 2005.04.14 Dan
;;;             : * Made use of printing-module-suppress-cmds in no-output and
;;;             :   command-output to fix an issue with reading :cmdt in the
;;;             :   context of a no-output.
;;; 2005.05.11 Dan
;;;             : * Added *one-stream-hack* to be used in model-warning to
;;;             :   get around an issue in MCL with it doubling warnings due
;;;             :   to *error-output* not equaling *standard-output* even
;;;             :   though they're the same place.  
;;;             :   NOT a good solution, but makes things look nicer for now...
;;; 2005.07.22 mdb
;;;             : * Changed WITHIN, GREATER-THAN, and LESS-THAN to return NIL
;;;             :   if compared against non-numbers.
;;;             : * Added NOT-EQUAL function to support negations.
;;; 2005.08.10 Dan
;;;             : * Changed no-output because it didn't need return0val and it
;;;             :   generated a warning.
;;;             : * Updated version to 1.0.
;;; 2005.09.08 Dan
;;;             : * Added support for a new paramter called :model-warnings
;;;             :   in the printing module.  When it is nil all the calls to
;;;             :   model-warning result in no output.
;;; 2005.10.19 Dan
;;;             : * Changed dovector slightly to assign an initial value of
;;;             :   nil to the variable in the let explicitily to get around
;;;             :   an issue with that in CMUCL.
;;; 2005.10.21 Dan
;;;             : * Doh! Realized the problem isn't the let issue but that 
;;;             :   CMUCL already defines dovector and doesn't like overwritting
;;;             :   it.  Fixed that now.
;;; 2006.03.13 Dan
;;;             : * Fixed no-outupt because it could fail when there were
;;;             :   nested calls.
;;; 2006.05.22 Dan
;;;             : * Noticed that Mike isn't listed as an author even though
;;;             :   many of these come from his older files.
;;; 2006.06.29 Dan
;;;             : * Added components provided by Don Morrison to allow it to be 
;;;             :   loaded into CLisp v2.38 - just added clisp to the switch to
;;;             :   not define this method (defmethod random-item ((seq vector)).
;;; 2006.07.12 Dan
;;;             : * Modified meta-p-output so that it always returns nil.
;;; 2006.09.08 Dan
;;;             : * Cleaned up the definition of posnum and added a corresponding
;;;             :   nonneg because zero isn't positive and there are situations
;;;             :   where that distinction matters (and those modules are now
;;;             :   also being updated to use nonneg).
;;; 2008.02.04 Dan
;;;             : * Adding the suppress-warnings command to provide a quick way
;;;             :   to eliminate all warnings.  Works like no-output i.e. wrap
;;;             :   it around the code you don't want printing warnings.
;;; 2008.02.28 Dan
;;;             : * Had to adjust how suppress-warnings works to comply with
;;;             :   the strict interpretation of the CL spec in SBCL.
;;; 2008.05.02 Dan
;;;             : * Fixed a bug with suppress-warnings because it was restoring
;;;             :   the model-warnings parameter with the wrong parameter's value.
;;; 2008.08.01 Dan
;;;             : * Changed the append in splice-into-list-des to nconc to 
;;;             :   actually make it destructive - splice-into-list copies it
;;;             :   and works non-destructive if needed.
;;; 2008.08.06 Dan
;;;             : * Changed fctornil to only call fboundp with symbols so that
;;;             :   it doesn't throw an error in such cases.
;;; 2008.08.20 Dan
;;;             : * Added the capture-model-output command.  It works like 
;;;             :   no-output except that it saves the suppressed output
;;;             :   and returns it in a string.
;;; 2008.10.09 Dan
;;;             : * Neq and dovector are already defined in the newest OpenMcl 
;;;             :   (Clozure Common Lisp) so adding a switch for those.
;;; 2010.03.27 Dan
;;;             : * Changed the safe[> | < | >= |<=] testers so that they will
;;;             :   always return t or nil instead of a general true.
;;; 2010.05.03 Dan
;;;             : * Modified model-warning so that it prints the name of the 
;;;             :   model in the warning if there is more than one model defined.
;;; 2010.05.13 Dan
;;;             : * Rewrote push-last so that it uses a macro defined with
;;;             :   define-modify-macro to do the setting to avoid the need to
;;;             :   evaluate place twice.  It's a minor performance improvement 
;;;             :   but it does add up in longer runs.
;;; 2010.06.04 Dan
;;;             : * Changed the nconcf macro in the updated push-last since
;;;             :   that name conflicts with an internal name in LispWorks.
;;; 2010.09.01 Dan
;;;             : * Changed ms-round to use fround so that the floating point
;;;             :   type of its parameter is preserved.
;;;             : * Added variables to track the current and available floating
;;;             :   point types and their sizes.
;;; 2010.10.06 mdb
;;;             : * Added function POSNUMORBOOL for use by incremental mouse
;;;             :   moves.
;;; 2010.11.03 Dan
;;;             : * Added seconds->ms and ms->seconds functions to go along with 
;;;             :   the conversion of the internal clock to be an integer count 
;;;             :   of ms.
;;;             : * Changed *time-size-current-limit* to be in ms since that's
;;;             :   what it will be comapred to if I put that back into the
;;;             :   scheduling code.
;;; 2011.02.11 Dan
;;;             : * Added a splice-into-position-des which works like splice-into-
;;;             :   list-des except that it doesn't "unpack" a list item being
;;;             :   spliced in.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; While
;;; (defmacro while (test &body body)
;;; 
;;; test a form to evaluate
;;; body any number of forms
;;;
;;; while the test evaluates to a non-nil value continue to evaluate the forms
;;; of the body.
;;;
;;; returns nil.
;;;
;;; Push-last
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;
;;; Print-warning
;;; (defmacro print-warning (control-string &rest args))
;;;
;;; control-string is a control-string as would be passed to the format function
;;; args are the arguments to use in that control string
;;;
;;; control-string and args are passed to format on the stream *error-output* 
;;; with the text "#|Warning: " proceeding it and "|#" after it so that it would 
;;; appear as a comment if the stream were to be read.
;;;
;;; nil is returned.  
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

;;; WHILE      [Macro]
;;; From _On Lisp_.
;;; Already defined in ACL with the IDE or all versions 6.0 or newer.

;;; (defmacro while (test &body body)
;;; 
;;; test a form to evaluate
;;; body any number of forms
;;;
;;; while the test evaluates to a non-nil value continue to evaluate the forms
;;; of the body.
;;;
;;; returns nil.

#-(or :allegro-ide (and :allegro-version>= (version>= 6)))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


;;; AIF      [Macro]
;;; Date        : 97.02.09
;;; Description : From _On Lisp_, anaphoric if.  That is, can use variable
;;;             : "it" to refer to result of the test-form.

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))



;;; push-last
;;;
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;


(define-modify-macro  act-r_nconcf (&rest args) nconc)

(defmacro push-last (item place)
  `(act-r_nconcf ,place (list ,item)))

;(defmacro push-last (item place)
;  `(setf ,place (nconc ,place (list ,item))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expt-coerced, exp-coerced, log-coerced, and sqrt-coerced
;;;
;;; These are for improved speed in Lisps that use doubles for math so that
;;; it can coerce them to single when that's the setting of 
;;; *read-default-float-format*.  Really makes a difference in MCL...
;;; Actually, in MCL 5 it makes no difference, and may want to just 
;;; eliminate this...

(defmacro expt-coerced (base power)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(expt ,base ,power)
    `(coerce (expt ,base ,power) ,*read-default-float-format*)))

(defmacro exp-coerced (arg)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(exp ,arg)
    `(coerce (exp ,arg) ,*read-default-float-format*)))

(defmacro log-coerced (arg &optional (base nil basep))
  "Computes log and coerce to *read-default-float-format* if needed
   doesn't accept a base however"
  (if (typep (log 1.0) *read-default-float-format*)
      (if basep
          `(log ,arg ,base)
        `(log ,arg))
    (if basep
        `(coerce (log ,arg ,base) ,*read-default-float-format*)
      `(coerce (log ,arg) ,*read-default-float-format*))))

(defmacro sqrt-coerced (arg)
  "Computes sqrt and coerce to *read-default-float-format* if needed"
  (if (typep (sqrt 2.0) *read-default-float-format*)
    `(sqrt ,arg)
    `(coerce (sqrt ,arg) ,*read-default-float-format*)))



;;; print-warning
;;;
;;; (defmacro print-warning (control-string &rest args))
;;;
;;; control-string is a control-string as would be passed to the format function
;;; args are the arguments to use in that control string
;;;
;;; control-string and args are passed to format on the stream *error-output* 
;;; with the text "#|Warning: " proceeding it and "|#" after it so that it would 
;;; appear as a comment if the stream were to be read.
;;;
;;; nil is returned.  

(defmacro print-warning (message &rest arguments)
  "Outputs a warning of message and arguments."
  `(format *error-output* "~&#|Warning: ~@? |#~%" ,message ,@arguments))


(defun hash-table-keys (ht)
  "Return the list of current keys in a hash-table"
  (let ((keys nil))
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push key keys))
             ht)
    keys))


(defun ms-round (x)
  "Rounds a time to the nearest millisecond"
  (declare (number x))
  (/ (fround (* x 1000)) 1000.0))

(defun seconds->ms (x)
  "Rounds a time in seconds to an integer number of milliseconds"
  (round x .001))

(defun ms->seconds (x)
  "Converts an integer number of milliseconds to a floating point number of seconds"
  (/ x 1000.0))

(defun fctornil (x)
  "Checks if a symbol is a function, function name, or nil"
  (or (null x)
      (functionp x)
      (and (symbolp x) (fboundp x))))

(defun tornil (x)
  "Checks if a symbol is T or NIL"
  (or (eq x t) (eq x nil)))

(defun posnum (x)
  "Returns true only if <x> is a positive number"
  (and (numberp x) (plusp x)))

(defun nonneg (x)
  "Returns true only if <x> is a non-negative number"
  (and (numberp x) (>= x 0.)))


(defun numornil (x)
  "Returns true only if <x> is a number or nil"
  (or (null x) (numberp x)))

(defun posnumornil (x)
  "Returns true only if <x> is a positive number or nil"
  (or (null x) (posnum x)))

(defun nonnegornil (x)
  "Returns true only if <x> is a non-negative number or nil"
  (or (null x) (nonneg x)))

(defun numorbool (x)
  "Returns true only if <x> is a number, T or nil"
  (or (tornil x) (numberp x)))

(defun posnumorbool (x)
  "Returns true only if <x> is a positive number, T or nil"
  (or (tornil x) (posnum x)))

(defun safe> (val1 val2)
  "Return t if val1 and val2 are numbers and val1 > val2"
  (and (numberp val1) (numberp val2) (> val1 val2) t))

(defun safe>= (val1 val2)
  "Return t if val1 and val2 are numbers and val1 >= val2"
  (and (numberp val1) (numberp val2) (>= val1 val2) t))

(defun safe< (val1 val2)
  "Return t if val1 and val2 are numbers and val1 < val2"
  (and (numberp val1) (numberp val2) (< val1 val2) t))

(defun safe<= (val1 val2)
  "Return t if val1 and val2 are numbers and val1 <= val2"
  (and (numberp val1) (numberp val2) (<= val1 val2) t))


;;; SPLICE-INTO-LIST      [Function]
;;; Date        : 97.01.15
;;; Description : 

(defun splice-into-list (lis position item)
  (let ((temp (copy-list lis)))
    (splice-into-list-des temp position item)))
      
    
;;; SPLICE-INTO-LIST-DES      [Function]
;;; Date        : 97.01.15
;;; Description : 

(defun splice-into-list-des (lis position item)
  (if (= position 0)
      (push item lis)
    (if (listp item)
        (nconc (subseq lis 0 position) item (nthcdr position lis))   
      (nconc (subseq lis 0 position) (list item) (nthcdr position lis)))))
  

;;; splice-into-position-des
;;; Doesn't unpack a list like splice-into-list-des does.

(defun splice-into-position-des (lis position item)
  (if (= position 0)
      (push item lis)
    (nconc (subseq lis 0 position) (list item) (nthcdr position lis))))


;;; MKLIST      [Function]
;;; Description : From Graham's _On Lisp_, make sure we have a list.

(defun mklist (obj)
  "If the object is not a list, return a list containing the object"
  (if (listp obj) obj (list obj)))


;;; Theoretically, these are part of the printing module, but 
;;; since they are macros that are used by lots of the internal
;;; functions they need to be defined early in the loading.

(defmacro model-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym)))
    `(multiple-value-bind (,module ,present)
       (get-module-fct 'printing-module)
     (when (and ,present (act-r-output-stream (printing-module-v ,module)))
       (format (act-r-output-stream (printing-module-v ,module)) 
           "~&~@?~%" ,control-string ,@args)))))

(defmacro command-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when (and ,present 
                  (not (printing-module-suppress-cmds ,module))
                  (act-r-output-stream (printing-module-c ,module)))
         (format (act-r-output-stream (printing-module-c ,module)) 
             "~&~@?~%" ,control-string ,@args)))))


(defmacro no-output (&rest commands)
  "Suppress command output while evaluating ACT-R commands"
  (let ((module (gensym))
        (present (gensym))
        (current (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((,current (printing-module-suppress-cmds ,module)))
                         
           (setf (printing-module-suppress-cmds ,module) t) 
           (unwind-protect 
               (progn ,@commands)
             (setf (printing-module-suppress-cmds ,module) ,current)))))))
     
(defmacro suppress-warnings (&rest commands)
  "Suppress all ACT-R warnings while evaluating ACT-R commands"
  (let ((module (gensym))
        (present (gensym))
        (current (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((*error-output* (make-broadcast-stream))
               (,current (printing-module-model-warnings ,module)))
                         
           (setf (printing-module-model-warnings ,module) nil) 
           (unwind-protect 
               (progn ,@commands)
             (setf (printing-module-model-warnings ,module) ,current)))))))
  

(defmacro capture-model-output (&rest commands)
  "Return the string of all model output from commands"
  (let ((module (gensym))
        (present (gensym))
        (current-v (gensym))
        (current-c (gensym))
        (out-stream (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((,out-stream (make-string-output-stream))
               (,current-v (printing-module-v ,module))
               (,current-c (printing-module-c ,module)))
                         
           (setf (printing-module-v ,module) (make-act-r-output :stream ,out-stream))
           (setf (printing-module-c ,module) (make-act-r-output :stream ,out-stream))
           
           (unwind-protect 
               (progn ,@commands)
             (progn
               (setf (printing-module-v ,module) ,current-v)
               (setf (printing-module-c ,module) ,current-c)))
           (get-output-stream-string ,out-stream))))))

  
;;; Put this in for now because while the output goes to the 
;;; same place, the streams aren't equal between *error-output*
;;; and *standard-output* so it ends up doubling the model warnings.
;;; I do NOT like this solution, but for now it's the easiest/only
;;; way I can come up with.

(defparameter *one-stream-hack* #+:digitool t
                                #-:digitool nil)

(defmacro model-warning (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (stream (gensym)))
    `(multiple-value-bind (,module ,present)
       (get-module-fct 'printing-module)
     (when (and ,present (act-r-output-stream (printing-module-v ,module)))
       (let ((,stream (act-r-output-stream (printing-module-v ,module))))
         (cond ((null (printing-module-model-warnings ,module))
                ;; just suppress the warnings
                nil)
               ((or (null ,stream)
                    (eq ,stream *error-output*)
                    *one-stream-hack*
                    (and (eq ,stream t) (eql *error-output* *standard-output*)))
                (format *error-output* 
                    "~&#|Warning~:[~*~; (in model ~a)~]: ~@? |#~%" (> (length (mp-models)) 1) (current-model) ,control-string ,@args))
               (t
                (format *error-output* 
                    "~&#|Warning~:[~*~; (in model ~a)~]: ~@? |#~%" (> (length (mp-models)) 1) (current-model) ,control-string ,@args)
                (format ,stream 
                    "~&#|Warning~:[~*~; (in model ~a)~]: ~@? |#~%" (> (length (mp-models)) 1) (current-model) ,control-string ,@args)
                nil)))))))


(defmacro meta-p-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (stream (gensym))
        (model (gensym))
        (used-streams (gensym))
        (key (gensym))
        (previous-model (gensym)))
    `(if (current-mp)
         (progn
           (let ((,used-streams nil)
                 (,previous-model (current-model-struct)))
             (maphash (lambda (,key ,model) 
                        (declare (ignore ,key))
                        
                        (setf (meta-p-current-model (current-mp)) 
                          ,model)
                        
                        (multiple-value-bind (,module ,present)
                            (get-module-fct 'printing-module)
                          (when (and ,present (act-r-output-stream (printing-module-v ,module)))
                            (let ((,stream (act-r-output-stream (printing-module-v ,module))))
                              
                              (unless (member ,stream ,used-streams)
                                (push ,stream ,used-streams)
                                (format ,stream 
                                    "~&~@?~%" ,control-string ,@args))))))
                      (meta-p-models (current-mp)))
             (setf (meta-p-current-model (current-mp)) ,previous-model))
           nil)
       (print-warning "No current meta-process in call to meta-p-output"))))

(defun rad->deg (r)
  "Converts radians into degrees."
  (declare (number r))
  (* r (/ 180 pi)))


(defun deg->rad (d)
  "Converts degrees into radians."
  (declare (number d))
  (* (/ pi 180) d))

(defmacro px (vpt)
  "X coordinate of an XY vector."
  `(svref ,vpt 0))

(defmacro py (vpt)
  "Y coordinate of an XY vector."
  `(svref ,vpt 1))

(defmacro vr (vrt)
  "R component of an r-theta vector."
  `(svref ,vrt 0))

(defmacro vtheta (vrt)
  "Theta component of an r-theta vector."
  `(svref ,vrt 1))


(defun vpt= (vpt1 vpt2)
  (and (= (px vpt1) (px vpt2))
       (= (py vpt1) (py vpt2))))

(defun round-xy (loc)
  (map 'vector #'round loc))

(defgeneric polar-move-xy (loc move)
  (:documentation  
   "Given an xy location and a polar displacement, return new xy"))

(defmethod polar-move-xy ((loc vector) (move vector))
  (round-xy
   (list (+ (px loc) (* (px move) (cos (py move))))
         (+ (py loc) (* (px move) (sin (py move)))))))

(defmethod polar-move-xy ((loc list) (move list))
  (polar-move-xy (coerce loc 'vector) (coerce move 'vector)))

;;; DIST      [Function]
;;; Description : Computes the distance between two locations (xy pairs, not
;;;             : chunks) using the  'real' hypoteneuse distance.

(defgeneric dist (loc1 loc2)
  (:documentation "Computes the distance in pixels between two XY locations"))

(defmethod dist ((loc1 vector) (loc2 vector))
  (sqrt-coerced (+ (expt-coerced (- (px loc1) (px loc2)) 2)
                   (expt-coerced (- (py loc1) (py loc2)) 2))))

(defmethod dist ((loc1 list) (loc2 list))
  (dist (coerce loc1 'vector) (coerce loc2 'vector)))



(defgeneric objs-match-slotval (lst slot-name value)
  (:documentation 
   "Takes a list of CLOS objects and returns a list containing those items which have the slot <slot-name> equal to <value>."))

(defmethod objs-match-slotval ((ls list) (slot-name symbol) value)
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (equal value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                  (value number))
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (= value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                  (value symbol))
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (eq value (slot-value obj slot-name))
          (push obj accum))))))


(defgeneric objs-min-slotval (lst slot-name)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the object(s) with the lowest value for that slot."))

(defmethod objs-min-slotval ((ls list) (slot-name symbol))
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((< current best) 
               (setf best current)
               (setf out-ls (list obj))))))))


(defgeneric objs-max-slotval (lst slot-name)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the object(s) with the highest value for that slot."))

(defmethod objs-max-slotval ((ls list) (slot-name symbol))
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((> current best) 
               (setf best current)
               (setf out-ls (list obj))))))))

(defgeneric objs-nearest-slotval (lst slot-name val)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the objects with the slot value closest to the supplied value."))


(defmethod objs-nearest-slotval ((lst list) (slot-name symbol) 
                                     (val number))
  (let ((best (abs (- val (slot-value (first lst) slot-name))))
        (current nil)
        (out-lst  (list (first lst))))
    (dolist (obj (rest lst) (nreverse out-lst))
      (setf current (abs (- val (slot-value obj slot-name))))
      (cond ((= current best) (push obj out-lst))
            ((< current best)
             (setf best current)
             (setf out-lst (list obj)))))))


;;; MKSTR      [Function]
;;; Date        : 97.07.02
;;; Description : From Graham's _On Lisp_, makes sure we have a string.

(defun mkstr (&rest args)
  "Return a concatenated string representation of the arguments"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defgeneric random-item (seq)
  (:documentation "Returns a random item from a sequence using act-r-random."))

(defmethod random-item ((seq list))
  (nth (act-r-random (length seq)) seq))

#+:mcl
(defmethod random-item ((seq simple-vector))
  (svref seq (act-r-random (length seq))))

#-(or :mcl :clisp)
(defmethod random-item ((seq vector))
  (svref seq (act-r-random (length seq))))


(defmethod random-item ((seq sequence))
  (elt seq (act-r-random (length seq))))

(defmethod random-item ((seq null))
  (declare (ignore seq))
  nil)

(defun sym->key (symbol)
  "Given a symbol, return the corresponding keyword."
  (read-from-string (mkstr ":" symbol)))


;;; FLATTEN      [Function]
;;; Description : From Graham's _On Lisp_, takes a nested list and turns it
;;;             : into a flat one.  "Fast" version.

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))


#-(or :mcl :cmu :clozure-common-lisp) (defmacro dovector ((varsym vec &optional ret) &body body)
  (let ((idx (gensym)))
    `(let ((,varsym nil))
       (dotimes (,idx (length ,vec) ,ret)
         (setq ,varsym (aref ,vec ,idx))
         ,@body
         ))))


(defgeneric within (min max)
  (:documentation 
   "Returns a closure that will test whether the argument is betwen <min> and <max>, inclusive."))

(defmethod within ((min number) (max number))
  (lambda (val)
    (and (numberp val) (<= val max) (>= val min))))

;;(defmethod within ((min list) (max list))
;;  (within (check-fct min) (check-fct max)))

;;(defmethod within ((min number) (max list))
;;  (within min (check-fct max)))

;;(defmethod within ((min list) (max number))
;;  (within (check-fct min) max))


(defgeneric greater-than (criterion)
  (:documentation 
   "Returns a closure that will return whether or not the argument is greater than <criterion>."))

(defmethod greater-than ((criterion number))
  (lambda (val)
    (and (numberp val) (> val criterion))))

;;(defmethod greater-than ((criterion list))
;;  (greater-than (check-fct criterion)))



(defgeneric less-than (criterion)
  (:documentation 
   "Returns a closure that will return whether or not the argument is less than <criterion>."))

(defmethod less-than ((criterion number))
  (lambda (val)
    (and (numberp val) (< val criterion))))


;;(defmethod less-than ((criterion list))
;;  (less-than (check-fct criterion)))


(defun not-equal (x)
  (declare (inline not-equal))
  (lambda (val)
    (not (equal x val))))


#-(or :mcl :clozure-common-lisp)
(defun neq (x y)
  "The NOT of EQ."
  (declare (inline neq))
  (not (eq x y)))


(defmethod string-to-lines ((s string))
  (aif (position #\Newline s)
    (append (mklist (subseq s 0 it))
            (string-to-lines (subseq s (1+ it) (length s))))
    (list s)))



;;; A specific value of nil may be important to some things, so that's why it
;;; returns a second value of t on success.

(defun verify-single-explicit-value (slot-specs module cmd slot)
  (cond ((zerop (length slot-specs))
         (print-warning 
          "~a command to ~s module requires a value for the ~a slot." 
          cmd module slot))
        ((> (length slot-specs) 1)
         (print-warning 
          "~a slot may only be specified once in a ~a command to the ~s module." 
          slot cmd module))
        ((not (eql '= (caar slot-specs)))
         (print-warning 
          "~a slot may only have the = modifier in a ~a command to the ~s module." 
          slot cmd module))
        ((chunk-spec-variable-p (third (car slot-specs)))
         (print-warning 
          "~a slot must be explict - not a variable in a ~a command to the ~s module." 
          slot cmd module))
        (t
         (values (third (car slot-specs)) t))))   


(defun find-float-time-limit (size &optional (delta .05))
  (do* ((offset (coerce delta size))
        (low (* .5 offset))
        (high (+ offset low))
        (e 0 (1+ e))
        (test (coerce (expt 2 e) size) (coerce (expt 2 e) size)))
       ((or (> e 100) (< (- (+ test offset) test) low) (> (- (+ test delta) test) high)) (1- e))))

(defvar *time-size-test-list* (sort (let ((res nil)
                                          (vals (list 'short-float 'single-float 'double-float 'long-float)))
                                      (dolist (type (pushnew *read-default-float-format* vals) res)
                                        (let ((size (find-float-time-limit type)))
                                          (aif (find size res :key 'car)
                                               (progn
                                                 (push-last type (cdr it)))
                                               (push (list size type) res))))) #'< :key 'car))

(defvar *time-size-current-limit* (seconds->ms (expt 2 (find-float-time-limit *read-default-float-format*))))
(defvar *time-size-current-type* *read-default-float-format*)                                    


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
