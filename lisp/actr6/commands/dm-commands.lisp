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
;;; Filename    : dm-commands.lisp
;;; Version     : 1.0a3
;;; 
;;; Description : ACT-R commands relavent to Declarative Memory
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Fix issues with merged-copies not showing up as being in DM.
;;; 
;;; ----- History -----
;;;
;;; 2005.01.11 Dan
;;;             : Created this file to hold stuff that was in declarative-
;;;             : memory in core-modules.
;;; 2005.01.17 Dan
;;;             : * Updated dm and sdm to use pprint-chunks-fct instead of
;;;             :   the now non-existent pprint-chunk-fct.
;;;             : * Fixed dm and sdm so that they don't print anything in the
;;;             :   case that there are no chunks to print instead of all 
;;;             :   existing chunks.
;;; 2005.01.18 Dan
;;;             : * Added more of the ACT-R 5 commands.
;;; 2005.01.21 Dan
;;;             : * Changed references to dm-chunks to reflect the change to
;;;             :   the new representation as a hash table by chunk-type.
;;; 2005.01.26 Dan
;;;             : * Added sdp functionality.
;;;             : * Removed calls with "old" names and moved them to
;;;             :   backward.lips in support.
;;; 2005.01.31 Dan
;;;             : * Fixed dm so that it only prints chunks from DM even when
;;;             :   names are provided.
;;; 2005.05.06 Dan
;;;             : * Fixed a bug in add-sji-fct where it called chunk-p instead
;;;             :   of chunk-p-fct.
;;; 2005.06.10 Dan
;;;             : * Changed chunk-parameters so that the references returned
;;;             :   is sensitive to the current setting of :ol i.e. it hides
;;;             :   the reference list when :ol is t and truncates it to the
;;;             :   appropriate size when :ol is a number.
;;; 2005.06.15 Dan
;;;             : * Updated the dm command so that chunks which have merged
;;;             :   into dm still print out if asked for, but it doesn't
;;;             :   show all merged names if called with no parameters.
;;; 2005.07.06 Dan
;;;             : * Changed sdm so that the isa isn't required.
;;; 2005.07.29 Dan
;;;             : * Fixed sdp so that it only works for chunks that are actually
;;;             :   in DM.
;;; 2005.08.01 Dan
;;;             : * Fixed an issue with BLC in the main module file that
;;;             :   reflects on the usage of sdp here.  Basically, BLC is added
;;;             :   to all base-level calculations, but no values are stored
;;;             :   in the chunk-base-level other than user settings.
;;; 2005.08.03 Dan
;;;             : * Added the print-dm-finsts command.
;;; 2005.09.15 Dan
;;;             : * Cleaned up sdm so that it checks the chunk-type and prints
;;;             :   a reasonable warning when an invalid chunk-type is given.
;;; 2006.06.20 Dan
;;;             : * Fixed a bug in set-base-level that lead to an error if 
;;;             :   :ol was nil and a creation time wasn't provided for a chunk.
;;; 2006.07.10 Dan
;;;             : * Changed call to true-chunk-name to true-chunk-name-fct since
;;;             :   that's being moved to a macro/-fct for user's access.
;;; 2006.11.29 Dan
;;;             : * Removed use of dm-pm and replaced it with dm-mp because the
;;;             :   :pm parameter is being depricated and :mp is both the flag
;;;             :   and value now like :bll and :mas.
;;;             : * Changed a warning in sdm to be a little clearer.
;;; 2006.11.30 Dan
;;;             : * Fixed add-sji-fct so that it removes an older setting
;;;             :   when adding a new one.
;;;             : * Changed clear-dm so that it returns t if the chunks were
;;;             :   cleared and nil if not.
;;;             : * Fixed clear-dm so that it also clears the merge table.
;;; 2006.12.01 Dan
;;;             : * Renamed the internal function set-base-level to set-bl
;;;             :   to avoid any issues with the user function set-base-levels.
;;;             : * Updates to sdp to better handle base-level reporting.
;;; 2006.12.05 Dan
;;;             : * Modified sdp to use the chunk-last-base-level parameter
;;;             :   setting instead of calling for a recomputation of the
;;;             :   base-level.
;;;             : * Modified the base-level setting code (set-base-levels and 
;;;             :   set-all-base-levels) so that when :ol is a number the
;;;             :   appropriate number of references are created.
;;;             : * Fixed set-base-levels and set-all-base-levels so that they
;;;             :   now return the base-level activations.
;;; 2006.12.06 Dan
;;;             : * Changed set-all-base-levels to return t/nil because if a
;;;             :   creation time isn't specified there's no particular base-level 
;;;             :   that's meaningful to return - they could all be different.
;;;             : * Added tests in set-base-levels-fct and set-all-base-levels
;;;             :   to make sure that the level and creation-time are numbers.
;;;             : * Turn off the activation trace when updating activations in sdp.
;;; 2006.12.07 Dan
;;;             : * Added the new sdp parameters :reference-list, :reference-count,
;;;             :   :retrieval-activation and :retrieval-time.  The first two of
;;;             :   those effectively make :references unnecessary and it will
;;;             :   no longer be shown (and shouldn't be used now either).
;;; 2006.12.08 Dan
;;;             : * Cleaned up the return values of sjis and similarities in sdp
;;;             :   so "default" values are returned in addition to user settings.
;;; 2007.04.13 Dan
;;;             : * Fixed a bug with sdm-fct which left the return value as nil
;;;             :   if there wasn't an isa even if matching chunks were found.
;;; 2007.04.19 Dan
;;;             : * Renamed the :retrieval-activation and :retrieval-time parameters
;;;             :   to :last-... because :retrieval-activation is also a general
;;;             :   parameter so that avoids confusion there and adding the "last"
;;;             :   on the front might make their interpretation more obvious.
;;;             : * Also moved them to the end of the sdp output for the chunks.
;;;             : * Fixed sdp so that it doesn't print invalid parameters as if
;;;             :   they existed when requested (even though there was a warning
;;;             :   the output made it look like it was still ok).
;;; 2007.06.18 Dan
;;;             : * Added the reset-declarative-finsts command so people can
;;;             :   clear the finsts from code if needed.
;;; 2008.09.13 Dan
;;;             : * Updated all the commands so that they will take the merged
;;;             :   names and treat them "right" (the to do above).
;;; 2010.06.15 Dan
;;;             : * Modified sdp so that it only recomputes the activation when
;;;             :   the :activation or :base-level parameter is requested.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The user functions from previous versions


(defmacro add-sji (&rest settings)
  "Macro to add an sji value between chunks"
  `(add-sji-fct ',settings))

(defun add-sji-fct (settings)
  "Function to add an sji value between chunks"
  (let ((returns nil))
    (dolist (x settings (reverse returns))
      (if (and (listp x)
               (= (length x) 3)
               (chunk-p-fct (first x))
               (chunk-p-fct (second x))
               (numberp (third x)))
          (let ((sji (third x))
                (chunk-i (second x))
                (chunk-j (first x)))
            (push sji returns)
            (setf (chunk-sjis chunk-i) (remove chunk-j (chunk-sjis chunk-i) :key #'car))
            (push (cons chunk-j sji) (chunk-sjis chunk-i)))
        (progn
          (print-warning "Bad Sji setting in ~S" x)
          (push :error returns))))))


(defmacro sji (chunkj chunki)
  "Macro to return the current Sji value between chunkj and chunki"
  `(sji-fct ',chunkj ',chunki))

(defun sji-fct (chunkj chunki)
  "Function to return current Sji value between chunkj and chunki"
  (let ((dm (get-module declarative)))
    (if dm
        (compute-sji dm chunkj chunki)
      (print-warning "No declarative memory module found"))))


(defmacro similarity (chunkj chunki)
  "Macro to return the current similarity value between chunkj and chunki"
  `(similarity-fct ',chunkj ',chunki))

(defun similarity-fct (chunkj chunki)
  "Function to return current similarity value between chunkj and chunki"
  (let ((dm (get-module declarative)))
    (if dm
        (chunks-similarity dm chunkj chunki)
      (print-warning "No declarative memory module found"))))


(defun clear-dm ()
  "User function to clear declarative memory - not recommended"
  (let ((dm (get-module declarative)))
    (if dm
        (progn
          (clrhash (dm-chunks dm))
          (clrhash (dm-chunk-hash-table dm))
          (print-warning "All the chunks cleared from DM.")
          t)
      (print-warning "No declarative memory module found"))))


(defmacro dm (&rest chunks)
  "Macro to print and return chunks in declarative memory"
  `(dm-fct ',chunks))

(defun dm-fct (chunk-list)
  "Function to print and return chunks in declarative memory"
    (let ((dm (get-module declarative)))
    (if dm
        (if chunk-list
            (let ((c-l (remove-if-not 
                       #'(lambda (x)
                           (member (true-chunk-name-fct x)
                                   (all-dm-chunks dm)))
                       chunk-list)))
              (when c-l
                (pprint-chunks-fct c-l)))
          (when (hash-table-keys (dm-chunks dm))
            (pprint-chunks-fct (all-dm-chunks dm))))
      (print-warning "No declarative memory module found"))))
   
   
(defmacro get-base-level (&rest chunks)
  "Macro to return the base-level activation of a chunk in dm"
  `(get-base-level-fct ',chunks))

(defun get-base-level-fct (chunks)
  "Function to return the base-level activation of a chunk in dm"
  (let ((dm (get-module declarative))
        (returns nil))
    (if dm
        (let ((dm-chunks (all-dm-chunks dm)))
          (dolist (chunk chunks (reverse returns))
            (let ((c (true-chunk-name-fct chunk)))
              (if (member c dm-chunks)
                  (push (base-level-activation dm c) returns)
                (push :error returns)))))
      (print-warning "No declarative memory module found"))))
      

(defmacro sdm (&rest spec)
  "Macro to search for and print chunks in declarative memory"
  `(sdm-fct ',spec))

(defun sdm-fct (spec)
  "Function to search for and print chunks in declarative memory"
  (let ((dm (get-module declarative)))
    (if dm
        (cond ((null spec) (dm))
              ((eq (car spec) 'isa)
               (if (chunk-type-p-fct (second spec))
                   (let* ((chunk-spec (define-chunk-spec-fct spec))
                          (chunks (find-matching-chunks 
                                   chunk-spec
                                   :chunks (apply #'append 
                                                  (mapcar #'(lambda (x)
                                                              (gethash 
                                                               x 
                                                               (dm-chunks dm)))
                                                    (chunk-type-subtypes-fct 
                                                     (chunk-spec-chunk-type 
                                                      chunk-spec)))))))
                     (when chunks
                       (pprint-chunks-fct chunks)))
                 (model-warning "Invalid chunk-type ~S passed to sdm" (second spec))))
               
              (t
               (if (evenp (length spec))
                   (let ((slot-names)
                         (valid-chunk-types)
                         (chunk-list))
                     (dotimes (i (/ (length spec) 2))
                       (push (nth (* i 2) spec) slot-names))
                     (dolist (ct (no-output (chunk-type)))
                       (when (every #'(lambda (x)
                                        (find x (chunk-type-slot-names-fct ct)))
                                    slot-names)
                         (push ct valid-chunk-types)))
                     (dolist (ct valid-chunk-types chunk-list)
                       (let* ((chunk-spec (define-chunk-spec-fct (append (list 'isa ct) spec)))
                              (chunks (find-matching-chunks 
                                       chunk-spec
                                       :chunks (gethash ct (dm-chunks dm)))))
                         (when chunks
                           (setf chunk-list (append chunk-list (pprint-chunks-fct chunks)))))))
                 (model-warning "Specification list to sdm without an isa is not an even length"))))
              
      (print-warning "No declarative memory module found"))))

(defmacro set-similarities (&rest settings)
  "Macro to set the similarity value between chunks"
  `(set-similarities-fct ',settings))

(defun set-similarities-fct (settings)
  "Function to set the similarity value between chunks"
  (let ((returns nil))
    (dolist (x settings (reverse returns))
      (if (and (listp x)
               (= (length x) 3)
               (chunk-p-fct (first x))
               (chunk-p-fct (second x))
               (numberp (third x)))
          (let ((similarity (third x))
                (chunk-i (second x))
                (chunk-j (first x)))
            (push similarity returns)
            
            (awhen (assoc chunk-j (chunk-similarities chunk-i))
                   (setf (chunk-similarities chunk-i)
                     (remove it (chunk-similarities chunk-i))))
            
            (awhen (assoc chunk-i (chunk-similarities chunk-j))
                   (setf (chunk-similarities chunk-j)
                     (remove it (chunk-similarities chunk-j))))
            
            (push (cons chunk-j similarity) 
                  (chunk-similarities chunk-i))
            (push (cons chunk-i similarity) 
                  (chunk-similarities chunk-j))
            )
        (progn
          (print-warning "Bad similarity setting in ~S" x)
          (push :error returns))))))

(defmacro set-base-levels (&rest settings)
  "Macro to set the base-level activation of chunks"
  `(set-base-levels-fct ',settings))

(defun set-base-levels-fct (settings)
  "Function to set the base-level activation of chunks"
  (let ((base-levels nil)
        (dm (get-module declarative)))
    (if dm
        (dolist (setting settings (reverse base-levels))
          (if (and (chunk-p-fct (car setting)) (member (true-chunk-name-fct (car setting)) (all-dm-chunks dm))
                   (numberp (second setting)) (or (null (third setting)) (numberp (third setting))))
              (push (set-bl dm (true-chunk-name-fct (car setting)) (second setting) (third setting)) base-levels)
            (progn 
              (cond ((not (and (chunk-p-fct (car setting)) (member (car setting) (all-dm-chunks dm))))
                     (print-warning "~S does not name a chunk in DM." (car setting)))
                    ((not (numberp (second setting)))
                     (print-warning "Invalid level in setting ~S" setting))
                    (t    
                     (print-warning "Invalid creation-time in setting ~S" setting)))
              (push :error base-levels))))
      (print-warning "No declarative memory module found"))))
  

(defun set-all-base-levels (base-level &optional creation-time)
  "Function to set the base-level activation of all dm chunks"
  (let ((dm (get-module declarative)))
    (if dm
        (if (and (numberp base-level) (or (null creation-time) (numberp creation-time)))
            (dolist (chunk (all-dm-chunks dm) t)
              (set-bl dm chunk base-level creation-time))
          (if (numberp base-level)
              (print-warning "Invalid creation-time ~S" creation-time)
            (print-warning "Invalid level ~S" base-level)))
      (print-warning "No declarative memory module found"))))


(defun set-bl (dm chunk base-level creation-time)
  "Internal function to set the base level of a chunk"
  (when (numberp creation-time)
    (setf (chunk-creation-time chunk) creation-time))
  (cond ((dm-bll dm)
         (setf (chunk-reference-count chunk) base-level)
         (setf (chunk-reference-list chunk)
           (adapt-references dm base-level (chunk-creation-time chunk)))
         (base-level-activation dm chunk))
        (t
         (setf (chunk-base-level chunk) base-level))))

(defun even-references (start end n &optional (m n))
  "Distributes m references evenly along n intervals between start and end."
  (when (plusp n)
    (let ((decrement (/ (- end start) n))
          (time end)
          (times nil))
      (dotimes (i (round m) times)
        (decf time decrement)
        (push-last time times)))))

(defun adapt-references (dm references creation-time)
  (cond ((eq (dm-ol dm) t) nil)
        ((null (dm-ol dm))
         (even-references creation-time (mp-time) references))
        (t
         (even-references creation-time (mp-time) references (min (dm-ol dm) references)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDP functionality based on ACT-R 4/5 code.
;;; May need some adjustment at some point...

(defmacro sdp (&rest settings)
  "Macro to inspect and set declarative memory's chunks' parameters."
  `(sdp-fct ',settings))


(defun sdp-fct (parameters)
  "Function to inspect and set declarative memory's chunks' parameters."
  (let ((results nil)
        (dm (get-module declarative)))
    (if dm
        (if (null parameters) ; print all parameters for all wmes
            (dolist (chunk (all-dm-chunks dm) results)
              (push-last (dm-chunk-parameters-fct dm chunk) results))
          
          (dolist (description (if (or (keywordp (first parameters))
                                       (keywordp (second parameters))
                                       (and (listp (first parameters))
                                            (null (second parameters))
                                            (not (keywordp 
                                                  (second 
                                                   (first parameters))))))
                                   (list parameters) parameters)
                               results)
            (when (atom description) (setf description (list description)))
            (if (keywordp (first description))
                (dolist (chunk (all-dm-chunks dm) results)
                  (push-last
                   (if (and (cdr description)
                            (not (keywordp (second description))))
                       (set-dm-chunk-parameters-fct dm chunk description)
                     (dm-chunk-parameters-fct dm chunk description))
                   results))
              (dolist (chunk (if (atom (first description))
                                 (list (first description))
                               (first description)))
                (let ((c (true-chunk-name-fct chunk)))
                  (if (find c (all-dm-chunks dm))
                    (push-last
                     (if (and (cddr description)
                              (not (keywordp (third description))))
                         (set-dm-chunk-parameters-fct dm c (rest description))
                       (dm-chunk-parameters-fct dm c (rest description)))
                     results)
                  (progn
                    ; Should it print a warning?
                    (print-warning "~S does not name a chunk in DM." chunk)
                    (push-last :error results))))))))
      
      (print-warning "No declarative memory module found"))))


(defun dm-chunk-parameters-fct (dm chunk &optional parameters)
  "Internal function to return the value of declarative chunk parameter(s), or  
   print them all if none specified."
  (let ((value nil)
        (values nil))
    (cond (chunk
           (command-output "Declarative parameters for chunk ~S:" chunk)
           
           ;;; Update activations ignoring partial matching
           ;;; by making the request spec only the chunk-type.
           ;;; also turn off the activation trace 
           
           (when (and (dm-esc dm) (or (find :activation parameters) (find :base-level parameters) (null parameters)))
             (let ((saved-trace (dm-act dm)))
               (setf (dm-act dm) nil)
               (compute-activation dm chunk (define-chunk-spec-fct (list 'isa (chunk-chunk-type-fct chunk))))
               (setf (dm-act dm) saved-trace)))
           
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                          (case parameter
                            (:name chunk)
                            (:activation (chunk-activation chunk))
                            (:base-level (chunk-last-base-level chunk))
                            (:creation-time (chunk-creation-time chunk))
                            (:references 
                             (cond ((null (dm-ol dm))
                                    (cons (chunk-reference-count chunk)
                                          (chunk-reference-list chunk)))
                                   ((numberp (dm-ol dm))
                                    (cons (chunk-reference-count chunk)
                                          (subseq (chunk-reference-list chunk) 0 (dm-ol dm))))
                                   (t
                                    (list (chunk-reference-count chunk)))))
                                   
                            (:source-spread (chunk-source-spread chunk))
                            (:sjis (get-all-chunk-sjis dm chunk))
                            (:permanent-noise (chunk-permanent-noise chunk))
                            (:similarities (get-all-chunk-similarities dm chunk))
                            (:retrieval-time 
                             (print-warning ":retrieval-time parameter has been renamed :last-retrieval-time")
                             :error)
                            (:retrieval-activation 
                             (print-warning ":retrieval-activation parameter has been renamed :last-retrieval-activation")
                             :error)
                            (:last-retrieval-activation (chunk-retrieval-activation chunk))
                            (:last-retrieval-time (chunk-retrieval-time chunk))
                            (:reference-count (chunk-reference-count chunk))
                            (:reference-list (chunk-reference-list chunk))
                            (t (print-warning 
                                "~A is not a declarative parameter for chunks."
                                parameter)
                               :error)))
                    (push-last value values)
                    ; ~f falls back to ~a when not a number...
                    (unless (eq value :error)
                      (command-output " ~S ~6,3F" parameter value)))
                  values)
                 
                 
                 (t
                  (when (dm-esc dm)
                    (command-output 
                     " :Activation ~6,3F~% :Permanent-Noise ~6,3F~% :Base-Level ~6,3F"
                     (chunk-activation chunk) 
                     (chunk-permanent-noise chunk)
                     (chunk-last-base-level chunk))
                    (when (dm-bll dm)
                      (command-output " :Creation-Time ~6,3F" (chunk-creation-time chunk))
                      (when (dm-ol dm)
                        (command-output " :Reference-Count ~6,3F" (chunk-reference-count chunk)))
                      (unless (eq t (dm-ol dm))
                        (command-output " :Reference-List ~S" (chunk-reference-list chunk))))
                    (when (dm-sa dm)
                      (command-output" :Source-Spread ~6,3F~% :Sjis ~6,3F" (chunk-source-spread chunk) (get-all-chunk-sjis dm chunk)))
                    (when (dm-mp dm)
                      (command-output " :Similarities ~6,3F" (get-all-chunk-similarities dm chunk)))
                    (command-output 
                     " :Last-Retrieval-Activation ~6,3F~% :Last-Retrieval-Time ~6,3F"
                     (chunk-retrieval-activation chunk) 
                     (chunk-retrieval-time chunk)))
                  chunk)))
          (t :error))))


(defun get-all-chunk-sjis (dm chunk)
  (let ((sjis nil)
        (js (mapcan #'(lambda (slot)
                        (when (chunk-p-fct (fast-chunk-slot-value-fct chunk slot))
                          (list (fast-chunk-slot-value-fct chunk slot))))
              (chunk-type-slot-names-fct (chunk-chunk-type-fct chunk)))))
    
    (setf sjis (mapcar #'(lambda (j) (cons j (compute-sji dm j chunk))) (remove-duplicates (cons chunk js))))
    (dolist (x (chunk-sjis chunk))
      (setf sjis (remove (car x) sjis :key #'car))
      (push x sjis))
    sjis))


(defun get-all-chunk-similarities (dm chunk)
  (let ((sims (chunk-similarities chunk)))
    (unless (find chunk sims :key #'car)
      (push (cons chunk (dm-ms dm)) sims))
    sims))

(defun set-dm-chunk-parameters-fct (dm chunk parameters)
  "Internal function to set the value of declarative chunk parameter(s)."
  (let ((values nil))
    (if chunk
      (loop
        (unless parameters 
          (return values))
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          ; not sure what this was for, but don't think it's still necessary
          ;(when (and (listp value) (eq (first value) 'quote))
          ;  (setf value (second value)))  
          
          (push-last
           (case parameter
             (:name
              (print-warning "CHUNK NAME CANNOT BE SET.")
              :error)
             (:activation
              (print-warning "CHUNK ACTIVATION CANNOT BE SET DIRECTLY.")
              :error)
             (:last-retrieval-activation
              (print-warning "CHUNK LAST-RETRIEVAL-ACTIVATION CANNOT BE SET DIRECTLY.")
              :error)
             (:last-retrieval-time
              (print-warning "CHUNK LAST-RETRIEVAL-TIME CANNOT BE SET DIRECTLY.")
              :error)
             
             (:base-level
              (cond ((dm-bll dm)
                     (print-warning 
                      "CHUNK BASE-LEVEL CANNOT BE SET DIRECTLY WHEN BASE LEVEL LEARNING IS ENABLED")
                     (print-warning "SET CREATION-TIME AND/OR REFERENCES INSTEAD.")
                     :error)
                    ((numberp value)
                     (setf (chunk-base-level chunk) value))
                    (t
                     (print-warning "CHUNK BASE-LEVEL MUST BE SET TO A NUMBER.")
                     :error)))
             (:creation-time
              (cond ((and (numberp value) (<= value (mp-time)))
                     (setf (chunk-creation-time chunk) value))
                    (t
                     (print-warning
                      "CHUNK CREATION-TIME MUST BE SET TO A NUMBER LESS THAN OR EQUAL TO THE CURRENT TIME.")
                     :error)))
             
             (:reference-count
              (cond ((null (dm-bll dm))
                     (print-warning "WHEN BLL DISABLED BASE-LEVEL SHOULD BE SET DIRECTLY")
                     :error)
                    ((null (dm-ol dm))
                     (print-warning "WHEN OL IS DISABLED BASE-LEVEL MUST BE SET THROUGH THE REFERENCE-LIST")
                     :error)
                    ((not (numberp value))
                     (print-warning "CHUNK REFERENCE-COUNT MUST BE SET TO A NUMBER")
                     :error) 
                    ((numberp (dm-ol dm)) 
                     (cond ((= (length (chunk-reference-list chunk)) value)
                            ;; do nothing
                            )
                           ((> (length (chunk-reference-list chunk)) value)
                            (setf (chunk-reference-list chunk) (subseq (chunk-reference-list chunk) 0 value)))
                           ((= (length (chunk-reference-list chunk)) (dm-ol dm))
                            ;; do nothing
                            )
                           (t 
                            (setf (chunk-reference-list chunk) (adapt-references dm value (chunk-creation-time chunk)))))
                     (setf (chunk-reference-count chunk) value))
                    (t ;; dm-ol = t so just set the number...
                     (setf (chunk-reference-count chunk) value))))
             
             (:reference-list
              (cond ((null (dm-bll dm))
                     (print-warning "WHEN BLL DISABLED BASE-LEVEL SHOULD BE SET DIRECTLY")
                     :error)
                    ((eq t (dm-ol dm))
                     (print-warning "WHEN OL IS T BASE-LEVEL MUST BE SET THROUGH THE REFERENCE-COUNT")
                     :error)
                    ((not (listp value))
                     (print-warning "CHUNK REFERENCE-LIST MUST BE SET TO A LIST OF NUMBERS")
                     :error)
                    ((not (every 'numberp value))
                     (print-warning "CHUNK REFERENCE-LIST MUST BE SET TO A LIST OF NUMBERS")
                     :error)
                    ((numberp (dm-ol dm))
                     (if (> (length value) (dm-ol dm))
                         (setf (chunk-reference-list chunk) (copy-list (subseq value 0 (dm-ol dm))))
                       (setf (chunk-reference-list chunk) (copy-list value)))
                     (when (< (chunk-reference-count chunk) (length (chunk-reference-list chunk)))
                       (setf (chunk-reference-count chunk) (length (chunk-reference-list chunk))))
                     (chunk-reference-list chunk))
                    (t ;; dm-ol = nil
                     (setf (chunk-reference-count chunk) (length value))
                     (setf (chunk-reference-list chunk) (copy-list value)))))
             
             (:references
              (cond ((listp value)
                     (setf (chunk-reference-count chunk) (length value))
                     
                     (setf (chunk-reference-list chunk)
                     
                       (cond ((null (dm-ol dm))
                              (copy-list value))
                             ((dm-ol dm) ;; it's a number so ignore the
                              ;; items in the list and even space them -
                              ;; that's what 4/5 does...
                              (adapt-references dm
                               (length value) 
                               (chunk-creation-time chunk)))
                             (t nil)))
                     
                     (cons (chunk-reference-count chunk)
                           (chunk-reference-list chunk)))
                    
                    ((numberp value)
                     (setf (chunk-reference-count chunk) value)
                     (setf (chunk-reference-list chunk)
                       (adapt-references dm
                        value 
                        (chunk-creation-time chunk)))
                       
                     (cons (chunk-reference-count chunk)
                           (chunk-reference-list chunk)))
                    (t
                     (print-warning 
                      "CHUNK REFERENCES MUST BE SET TO A NUMBER OR A LIST.")
                     :error)))
             
             (:source-spread
              (print-warning "CHUNK SOURCE-SPREAD CANNOT BE SET DIRECTLY: SET BUFFER CHUNKS AND/OR Sjis INSTEAD.")
              :error)
             
             (:sjis
              (cond ((listp value)
                     (dolist (sji-pair value)
                       (let ((chunkj (car sji-pair))
                             (sji (if (numberp (cdr sji-pair))
                                      (cdr sji-pair)
                                    (cadr sji-pair))))
                         (if (numberp sji)
                             (if (chunk-p-fct chunkj)
                                 (add-sji-fct (list (list chunkj chunk sji)))
                               (print-warning "~S is not a chunk"))
                           (print-warning "Sji VALUE ~S IS NOT A NUMBER." sji))))
                     (get-all-chunk-sjis dm chunk))
                    (t
                     (print-warning 
                      "CHUNK Sjis MUST BE SET USING A LIST OF CHUNK-NUMBER PAIRS.")
                     :error)))
             
             (:permanent-noise
              (cond ((numberp value)
                     (setf (chunk-permanent-noise chunk) value))
                    (t
                     (print-warning
                      "CHUNK PERMANENT-NOISE MUST BE SET TO A NUMBER.")
                     :error)))
             
             
             (:similarities
              (cond ((listp value)
                     
                     (dolist (similarity-pair value)
                       (let ((chunkj (car similarity-pair))
                             (similarity (if (numberp (cdr similarity-pair))
                                             (cdr similarity-pair)
                                           (cadr similarity-pair))))
                         (if (numberp similarity)
                             (if (chunk-p-fct chunkj)
                               (progn
                                 (awhen (assoc chunkj (chunk-similarities chunk))
                                        (setf (chunk-similarities chunk)
                                          (remove it (chunk-similarities chunk))))
                                 
                                 (push (cons chunkj similarity) 
                                       (chunk-similarities chunk)))
                               (print-warning "~S is not the name of a chunk." chunkj))
                           (print-warning "CHUNK SIMILARITY VALUE ~S IS NOT A NUMBER." similarity))))
                     
                     (get-all-chunk-similarities dm chunk))
                    (t
                     (print-warning 
                      "CHUNK SIMILARITIES MUST BE SET USING A LIST OF CHUNK-NUMBER PAIRS.")
                     :error)))
             
             
             (t (print-warning "NO PARAMETER ~s DEFINED FOR CHUNKS."
                             parameter)
                :error))
           values)))
      :error)))

;;; 
;;; New user level functions
;;;

(defun reset-declarative-finsts ()
  (let ((dm (get-module declarative)))
    (if dm
        (setf (dm-finsts dm) nil)
      (print-warning "No declarative module found - cannot reset the finsts."))))

(defun print-dm-finsts ()
  (let ((dm (get-module declarative)))
    (when dm
      ;; get-module will report a warning if it's not found
      
      (let ((max-name-len (max 11
                               (if (dm-finsts dm)
                                   (apply #'max 
                                          (mapcar #'(lambda (x)
                                                      (length (symbol-name (car x))))
                                            (dm-finsts dm)))
                                 0))))
        
        (command-output  "~%~va    Time Stamp~%~v,1,0,'-a"
          max-name-len "Chunk name" 
          (+ 14 max-name-len) "")
        
        (dolist (x (dm-finsts dm) (dm-finsts dm))
          (command-output  "~va    ~8,3f" max-name-len
            (car x) (cdr x)))))))

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
