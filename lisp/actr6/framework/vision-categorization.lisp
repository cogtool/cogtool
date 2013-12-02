;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author       Mike Byrne
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2001 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename     vision-categorization.lisp
;;; Version      1.0
;;; 
;;; Description  Class and constants for different feature sets.
;;; 
;;; Bugs         
;;; 
;;; Todo         
;;; 
;;; ----- History -----
;;; 99.06.10 Mike Byrne
;;;               Header date.
;;; 99.07.30 mdb
;;;              Added GETFEATS.
;;; 99.08.08 mdb
;;;             : Removed keywords from feature lists, added PROB-BEST-CHAR.
;;; 99.09.08 mdb
;;;             : Corrected RM set from Mike Matessa's comments.
;;; 00.06.09 mdb
;;;             : Conversion of "new-feats.lisp" file to 
;;;             : "vision-categorization" for inclusion in RPM 2.0.
;;; 01.06.29 mdb
;;;             : Added defgenerics and doc strings.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;; 2006.01.03 Dan
;;;             : * Removed the package from the mode line at the top.
;;; 2007.12.07 Dan [1.0]
;;;             : * Fixed a bug in cliplast because it didn't specify the
;;;             :   optional parameters before the keywords in read-from-string.
;;;             :   Which didn't really matter since it was just :start 0
;;;             :   but it doesn't hurt to be safe and it avoids a warning in LW.
;;;             : * Put the :'s in the mode line at the top to avoid a warning
;;;             :   with LW.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defclass char-feats ()
  ((feature-ls :accessor feature-ls :initarg :feat-ls)
   (fhash :accessor fhash :initform (make-hash-table :test #'equal))
   (letters :accessor letters :allocation :class
            :initform '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                          "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
   (digits :accessor digits :allocation :class
           :initform '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
   (average-match-prob :accessor average-match-prob :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (number-of-features :accessor number-of-features :initform nil)
   (true->icon :accessor true->icon :initarg :true->icon :initform #'identity)
   ))


(defmethod print-object ((self char-feats) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S" (name self))))


(defgeneric numfeats (chr-feat-set)
  (:documentation  "Return the number of features in the feature set."))

(defmethod numfeats ((self char-feats))
  (aif (number-of-features self) it (length (feature-ls self))))


(defgeneric has-match-p (chr-feat-set str1 str2)
  (:documentation  "Given two letters, return the list of features that appear in both."))

(defmethod has-match-p ((self char-feats) (s1 string) (s2 string))
  (intersection (gethash s1 (fhash self)) (gethash s2 (fhash self))))


(defgeneric prob-match (chr-feat-set str1 str2)
  (:documentation  "Return the probabily that a random feature in s1 will appear in s2."))

(defmethod prob-match ((self char-feats) (s1 string) (s2 string))
  (if (string= s1 s2)
    -1
    (float (/ (length (remove-duplicates (has-match-p self s1 s2))) 
              (length (gethash s1 (fhash self)))))))


(defgeneric setfeats (chr-feat-set str feat-lst)
  (:documentation  "Set the feature list for a string (character)."))

(defmethod setfeats ((self char-feats) (s1 string) (feat-ls list))
  (setf (gethash s1 (fhash self)) feat-ls))


(defgeneric getfeats (chr-feat-set chr)
  (:documentation "Returns the feature list for a character."))

(defmethod getfeats ((self char-feats) (s string))
  (gethash s (fhash self)))

(defmethod getfeats ((self char-feats) (c character))
  (gethash (mkstr c) (fhash self)))


(defgeneric get-icon-feats (chr-feat-set char)
  (:documentation "Return the icon feature list for a given character."))

(defmethod get-icon-feats ((self char-feats) char)
  (let ((ti (true->icon self)))
    (cond ((functionp ti) (mapcar ti (getfeats self char)))
          ((listp ti) (mapcar #'(lambda (feat)
                                  (second (assoc feat ti)))
                              (getfeats self char)))
          (t (getfeats self char)))))


(defgeneric exact-character (char-feat-set feat-lst)
  (:documentation "Return the character exactly matching a feature set."))

(defmethod exact-character ((self char-feats) (feat-ls list))
  (setf feat-ls (mapcar #'sym->key feat-ls))
  (let (realfeats)
    (dolist (char (chars self))
      (setf realfeats (getfeats self char))
      (when (= (length realfeats) (length feat-ls)
               (length (intersection feat-ls realfeats)))
        (return-from exact-character char)))))


(defgeneric prob-best-character (char-feat-set feat-lst)
  (:documentation "Return the character with the best probability of match to a feature list."))

(defmethod prob-best-character ((self char-feats) (feat-ls list))
  (let ((bestval 0) (bestchar nil) cur-val)
    (dolist (char (chars self))
      (setf cur-val (feat-ls-prob-match self feat-ls (getfeats self char)))
      (when (= cur-val bestval)
        (push char bestchar))
      (when (> cur-val bestval)
        (setf bestval cur-val)
        (setf bestchar (mklist char))))
    (values (random-item bestchar) (* 100. bestval))))


(defgeneric feat-ls-prob-match (char-feat-set lst1 lst2)
  (:documentation "Return the probability that two feature lists represent a match."))

(defmethod feat-ls-prob-match ((self char-feats) (l1 list) (l2 list))
  (let* ((overlap (length (remove-duplicates (intersection l1 l2))))
         (num-miss (+ (- (length l1) overlap)
                        (- (length l2) overlap))))
    (float (* (expt 2/3 (- (numfeats self) num-miss))
              (expt 1/3 num-miss)))))


(defmethod chars ((char-feat-set char-feats))
  (append (letters char-feat-set) (digits char-feat-set)))


(defgeneric avg-match-prob (char-feat-set)
  (:documentation "Returns the average match probability for an entire feature set."))

(defmethod avg-match-prob ((self char-feats))
  (aif (average-match-prob self) 
    it
    (let ((accum))
      (dolist (targ (chars self))
        (dolist (item (chars self))
          (when (not (string= targ item))
            (push (prob-match self targ item) accum))))
      (setf (average-match-prob self)
            (/ (reduce #'+ accum) (length accum))))))
                      


(defgeneric print-match-table (char-feat-set)
  (:documentation "Print out a table of matc probabilities."))

(defmethod print-match-table ((self char-feats))
  (dolist (targ (append (letters self) (digits self)))
    (format t "~%~A	" targ)
    (dolist (char (append (letters self) (digits self)))
      (format t "~3,2F	" (prob-match self targ char)))))


(defgeneric print-num-feats (char-feat-set)
  (:documentation "Print the number of features for each character."))

(defmethod print-num-feats ((self char-feats))
  (dolist (targ (append (letters self) (digits self)))
    (format t "~%~A has ~S" targ (length (gethash targ (fhash self))))))


(defgeneric check-integrity (thing)
  (:documentation "Check the internal integrity of <thing>."))

(defmethod check-integrity ((self char-feats))
  (let ((feat-ls))
    (dolist (char (append (letters self) (digits self)))
      (setf feat-ls (gethash char (fhash self)))
      (awhen (set-difference feat-ls (feature-ls self))
        (format t "~&~A has a feature not in the set ~S" char it))
      (dolist (item (append (letters self) (digits self)))
        (when (and (not (string= char item))
                   (equal feat-ls (gethash item (fhash self))))
          (format t "~&~A and ~A have identical features" char item)))
      (let ((bestchar (prob-best-character self feat-ls)))
        (when (not (string= char bestchar))
          (format t "~&~A has ~A as its best char." char bestchar)))
      )))


(defgeneric feature-freq (char-feat-set)
  (:documentation "Print the frequency of occurrence for each feature in the set."))

(defmethod feature-freq ((self char-feats))
  (let ((counter))
    (dolist (feat (feature-ls self))
      (setf counter 0)
      (dolist (char (chars self))
        (when (member feat (gethash char (fhash self)))
          (incf counter)))
      (format t "~&Feature ~S occurs in ~S chars" feat counter))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; The Gibson (1969) feature set

(defun build-gibson-set ()
  "Returns the Gibson (1969) character feature set."
  (let ((tmp (make-instance 'char-feats :name :GIBSON :feat-ls
                             '(HORIZONTAL VERTICAL DIAGONAL-SLASH 
                               DIAGONAL-BACK DIAGONAL-SLASH CLOSED-CURVE
                               OPEN-V-CURVE OPEN-H-CURVE INTERSECTION
                               CYCLIC-CHANGE SYMMETRY VERTICAL-DISCONT
                               HORIZONTAL-DISCONT))))
    (setfeats tmp "A" '(SYMMETRY HORIZONTAL DIAGONAL-SLASH DIAGONAL-BACK INTERSECTION VERTICAL-DISCONT))
    (setfeats tmp "B" '(SYMMETRY CYCLIC-CHANGE INTERSECTION HORIZONTAL VERTICAL CLOSED-CURVE ))
    (setfeats tmp "C" '(SYMMETRY OPEN-H-CURVE ))
    (setfeats tmp "D" '(SYMMETRY VERTICAL CLOSED-CURVE ))
    (setfeats tmp "E" '(HORIZONTAL-DISCONT SYMMETRY CYCLIC-CHANGE HORIZONTAL VERTICAL INTERSECTION ))
    (setfeats tmp "F" '(HORIZONTAL-DISCONT VERTICAL-DISCONT HORIZONTAL VERTICAL INTERSECTION ))
    (setfeats tmp "G" '(HORIZONTAL OPEN-H-CURVE ))
    (setfeats tmp "H" '(VERTICAL-DISCONT SYMMETRY HORIZONTAL VERTICAL INTERSECTION ))
    (setfeats tmp "I" '(VERTICAL-DISCONT SYMMETRY VERTICAL ))
    (setfeats tmp "J" '(OPEN-V-CURVE OPEN-H-CURVE ))
    (setfeats tmp "K" '(VERTICAL-DISCONT SYMMETRY CYCLIC-CHANGE VERTICAL DIAGONAL-SLASH DIAGONAL-BACK INTERSECTION ))
    (setfeats tmp "L" '(HORIZONTAL-DISCONT HORIZONTAL VERTICAL ))
    (setfeats tmp "M" '(CYCLIC-CHANGE VERTICAL-DISCONT SYMMETRY VERTICAL DIAGONAL-SLASH DIAGONAL-BACK ))
    (setfeats tmp "N" '(VERTICAL-DISCONT VERTICAL DIAGONAL-BACK ))
    (setfeats tmp "O" '(SYMMETRY CLOSED-CURVE ))
    (setfeats tmp "P" '(VERTICAL-DISCONT INTERSECTION VERTICAL CLOSED-CURVE ))
    (setfeats tmp "Q" '(intersection DIAGONAL-BACK CLOSED-CURVE ))
    (setfeats tmp "R" '(VERTICAL-DISCONT INTERSECTION VERTICAL DIAGONAL-BACK CLOSED-CURVE ))
    (setfeats tmp "S" '(CYCLIC-CHANGE OPEN-H-CURVE ))
    (setfeats tmp "T" '(HORIZONTAL-DISCONT VERTICAL-DISCONT SYMMETRY HORIZONTAL VERTICAL INTERSECTION ))
    (setfeats tmp "U" '(SYMMETRY OPEN-V-CURVE ))
    (setfeats tmp "V" '(SYMMETRY DIAGONAL-SLASH DIAGONAL-BACK ))
    (setfeats tmp "W" '(SYMMETRY CYCLIC-CHANGE DIAGONAL-SLASH DIAGONAL-BACK ))
    (setfeats tmp "X" '(SYMMETRY INTERSECTION DIAGONAL-SLASH DIAGONAL-BACK ))
    (setfeats tmp "Y" '(VERTICAL-DISCONT SYMMETRY VERTICAL DIAGONAL-SLASH DIAGONAL-BACK ))
    (setfeats tmp "Z" '(HORIZONTAL-DISCONT HORIZONTAL DIAGONAL-SLASH ))
    
    (setfeats tmp "1" '(HORIZONTAL-DISCONT HORIZONTAL VERTICAL INTERSECTION))
    (setfeats tmp "2" '(HORIZONTAL-DISCONT HORIZONTAL OPEN-H-CURVE))
    (setfeats tmp "3" '(SYMMETRY OPEN-H-CURVE CYCLIC-CHANGE))
    (setfeats tmp "4" '(VERTICAL-DISCONT HORIZONTAL-DISCONT HORIZONTAL VERTICAL DIAGONAL-SLASH INTERSECTION))
    (setfeats tmp "5" '(HORIZONTAL-DISCONT HORIZONTAL OPEN-H-CURVE CYCLIC-CHANGE))
    (setfeats tmp "6" '(HORIZONTAL-DISCONT CLOSED-CURVE OPEN-H-CURVE))
    (setfeats tmp "7" '(VERTICAL-DISCONT HORIZONTAL DIAGONAL-SLASH))
    (setfeats tmp "8" '(SYMMETRY CLOSED-CURVE INTERSECTION))
    (setfeats tmp "9" '(VERTICAL-DISCONT CLOSED-CURVE OPEN-H-CURVE))
    (setfeats tmp "0" '(SYMMETRY CLOSED-CURVE DIAGONAL-SLASH))
    tmp))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; The Briggs & Hocevar (1975) feature set

(defun build-bh-set ()
  "Returns the Briggs & Hocevar (1975) character feature set."
  (let ((tmp (make-instance 'char-feats :name :BRIGGS-HOCEVAR :feat-ls
                             '(H-TOP H-CENTER H-BOTTOM SINGLE-V DOUBLE-V 
                               ANG-OPEN-TOP ANG-OPEN-DOWN ANG-OPEN-HORIZ 
                               SM-CURVE-CONVEX-RIGHT SM-CURVE-CONVEX-BOTTOM 
                               LARGE-CURVE CONTINUOUS-CURVE CLOSED-CURVE
                               SM-CURVE-CONVEX-LEFT))))
    
    (setfeats tmp "A" '(H-CENTER ANG-OPEN-DOWN))
    (setfeats tmp "B" '(H-TOP H-CENTER H-BOTTOM SINGLE-V SM-CURVE-CONVEX-RIGHT CLOSED-CURVE))
    (setfeats tmp "C" '(LARGE-CURVE CONTINUOUS-CURVE ))
    (setfeats tmp "D" '(SINGLE-V LARGE-CURVE CLOSED-CURVE))
    (setfeats tmp "E" '(H-TOP H-CENTER H-BOTTOM SINGLE-V ))
    (setfeats tmp "F" '(H-TOP H-CENTER SINGLE-V ))
    (setfeats tmp "G" '(SM-CURVE-CONVEX-RIGHT LARGE-CURVE ))
    (setfeats tmp "H" '(H-CENTER DOUBLE-V ))
    (setfeats tmp "I" '(SINGLE-V ))
    (setfeats tmp "J" '(SINGLE-V SM-CURVE-CONVEX-BOTTOM ))
    (setfeats tmp "K" '(SINGLE-V ANG-OPEN-TOP ANG-OPEN-DOWN ANG-OPEN-HORIZ ))
    (setfeats tmp "L" '(H-BOTTOM SINGLE-V ))
    (setfeats tmp "M" '(DOUBLE-V ANG-OPEN-DOWN ))
    (setfeats tmp "N" '(DOUBLE-V ANG-OPEN-TOP ANG-OPEN-DOWN ))
    (setfeats tmp "O" '(LARGE-CURVE CONTINUOUS-CURVE CLOSED-CURVE))
    (setfeats tmp "P" '(H-TOP H-CENTER SINGLE-V SM-CURVE-CONVEX-RIGHT CLOSED-CURVE))
    (setfeats tmp "Q" '(LARGE-CURVE CLOSED-CURVE))
    (setfeats tmp "R" '(H-TOP H-CENTER SINGLE-V ANG-OPEN-DOWN SM-CURVE-CONVEX-RIGHT CLOSED-CURVE))
    (setfeats tmp "S" '(SM-CURVE-CONVEX-RIGHT CONTINUOUS-CURVE))
    (setfeats tmp "T" '(H-TOP SINGLE-V ))
    (setfeats tmp "U" '(DOUBLE-V SM-CURVE-CONVEX-BOTTOM ))
    (setfeats tmp "V" '(ANG-OPEN-TOP ))
    (setfeats tmp "W" '(ANG-OPEN-TOP ANG-OPEN-DOWN ))
    (setfeats tmp "X" '(ANG-OPEN-TOP ANG-OPEN-DOWN ANG-OPEN-HORIZ ))
    (setfeats tmp "Y" '(SINGLE-V ANG-OPEN-TOP ANG-OPEN-HORIZ ))
    (setfeats tmp "Z" '(H-TOP H-BOTTOM ANG-OPEN-HORIZ ))
    
    (setfeats tmp "1" '(H-BOTTOM SINGLE-V ANG-OPEN-DOWN))
    (setfeats tmp "2" '(H-BOTTOM SM-CURVE-CONVEX-LEFT))
    (setfeats tmp "3" '(SM-CURVE-CONVEX-LEFT))
    (setfeats tmp "4" '(SINGLE-V CLOSED-CURVE H-CENTER))
    (setfeats tmp "5" '(SM-CURVE-CONVEX-LEFT H-TOP))
    (setfeats tmp "6" '(CLOSED-CURVE SM-CURVE-CONVEX-RIGHT))
    (setfeats tmp "7" '(H-TOP ANG-OPEN-HORIZ))
    (setfeats tmp "8" '(CLOSED-CURVE CONTINUOUS-CURVE))
    (setfeats tmp "9" '(CLOSED-CURVE SM-CURVE-CONVEX-LEFT))
    (setfeats tmp "0" '(CLOSED-CURVE CONTINUOUS-CURVE LARGE-CURVE ANG-OPEN-HORIZ))
    tmp))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Original Rumelhart and McClelland feature set

(defun build-rm-orig-set ()
  "Returns the Rumelhart & McClelland (1981) character feature set."
  (let ((tmp (make-instance 'char-feats :name :RM-ORIG 
                             :feat-ls 
                             '(horizontal-t0 vertical-l1
                               back-diagonal-u1 vertical-c1 front-diagonal-u1
                               vertical-r1 horizontal-c1 horizontal-c2
                               vertical-l2 front-diagonal-l2 vertical-c2
                               back-diagonal-l2 vertical-r2 horizontal-l0
                               left-facing symmetric right-facing))))
    
    (setfeats tmp "A" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 VERTICAL-R2 SYMMETRIC))
    (setfeats tmp "B" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-R1 HORIZONTAL-C2
                        VERTICAL-C2 VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "C" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-L2 HORIZONTAL-L0
                        RIGHT-FACING))
    (setfeats tmp "D" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-R1 VERTICAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "E" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 VERTICAL-L2
                        HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "F" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 VERTICAL-L2
                        RIGHT-FACING))
    (setfeats tmp "G" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C2 VERTICAL-L2
                        VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "H" '(VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 VERTICAL-R2 SYMMETRIC))
    (setfeats tmp "I" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-C2 HORIZONTAL-L0
                        SYMMETRIC))
    (setfeats tmp "J" '(VERTICAL-R1 VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        LEFT-FACING))
    (setfeats tmp "K" '(VERTICAL-L1 FRONT-DIAGONAL-U1 HORIZONTAL-C1
                        VERTICAL-L2 BACK-DIAGONAL-L2 RIGHT-FACING))
    (setfeats tmp "L" '(VERTICAL-L1 VERTICAL-L2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "M" '(VERTICAL-L1 BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1
                        VERTICAL-R1 VERTICAL-L2 VERTICAL-R2 SYMMETRIC))
    (setfeats tmp "N" '(VERTICAL-L1 BACK-DIAGONAL-U1 VERTICAL-R1 VERTICAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 SYMMETRIC))
    (setfeats tmp "O" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 VERTICAL-L2
                        VERTICAL-R2 HORIZONTAL-L0 SYMMETRIC))
    (setfeats tmp "P" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 RIGHT-FACING))
    (setfeats tmp "Q" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 VERTICAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        RIGHT-FACING))
    (setfeats tmp "R" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 BACK-DIAGONAL-L2
                        RIGHT-FACING))
    (setfeats tmp "S" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 SYMMETRIC))
    (setfeats tmp "T" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-C2 SYMMETRIC))
    (setfeats tmp "U" '(VERTICAL-L1 VERTICAL-R1 VERTICAL-L2 VERTICAL-R2
                        HORIZONTAL-L0 SYMMETRIC))
    (setfeats tmp "V" '(VERTICAL-L1 FRONT-DIAGONAL-U1 VERTICAL-L2
                        FRONT-DIAGONAL-L2 SYMMETRIC))
    (setfeats tmp "W" '(VERTICAL-L1 VERTICAL-R1 VERTICAL-L2 FRONT-DIAGONAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 SYMMETRIC))
    (setfeats tmp "X" '(BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1 FRONT-DIAGONAL-L2
                        BACK-DIAGONAL-L2 SYMMETRIC))
    (setfeats tmp "Y" '(BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1 VERTICAL-C2
                        SYMMETRIC))
    (setfeats tmp "Z" '(HORIZONTAL-T0 FRONT-DIAGONAL-U1 FRONT-DIAGONAL-L2
                        HORIZONTAL-L0 SYMMETRIC))
    
    (setfeats tmp "1" '(VERTICAL-R1 VERTICAL-R2 LEFT-FACING))
    (setfeats tmp "2" '(HORIZONTAL-T0 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 HORIZONTAL-L0 LEFT-FACING))
    (setfeats tmp "3" '(HORIZONTAL-T0 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 LEFT-FACING))
    (setfeats tmp "4" '(VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 LEFT-FACING))
    (setfeats tmp "5" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1
                        BACK-DIAGONAL-L2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "6" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING))
    (setfeats tmp "7" '(HORIZONTAL-T0 VERTICAL-R1 VERTICAL-R2 LEFT-FACING))
    (setfeats tmp "8" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        SYMMETRIC))
    (setfeats tmp "9" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-R2 HORIZONTAL-L0 LEFT-FACING))
    (setfeats tmp "0" '(HORIZONTAL-T0 VERTICAL-L1 BACK-DIAGONAL-U1 VERTICAL-R1
                        VERTICAL-L2 BACK-DIAGONAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        SYMMETRIC))
    tmp))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; "Clean" Rumelhart and McClelland (??) feature set

(defun cliplast (sym)
  "Removes the last character from a symbol."
  (let ((str (mkstr sym)))
    (read-from-string str nil nil :start 0 :end (- (length str) 1))))

(defun build-rm-clean-set ()
  (let ((tmp (make-instance 'char-feats :name :RM-CLEAN :true->icon #'cliplast
                            :feat-ls 
                            '(horizontal-t0 vertical-l1
                              back-diagonal-u1 vertical-c1 front-diagonal-u1
                              vertical-r1 horizontal-c1 horizontal-c2
                              vertical-l2 front-diagonal-l2 vertical-c2
                              back-diagonal-l2 vertical-r2 horizontal-l0
                              left-facing+ symmetric+ right-facing+))))
    
    (setfeats tmp "A" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 VERTICAL-R2 SYMMETRIC+))
    (setfeats tmp "B" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-R1 HORIZONTAL-C2
                        VERTICAL-C2 VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "C" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-L2 HORIZONTAL-L0
                        RIGHT-FACING+))
    (setfeats tmp "D" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-R1 VERTICAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "E" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 VERTICAL-L2
                        HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "F" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 VERTICAL-L2
                        RIGHT-FACING+))
    (setfeats tmp "G" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C2 VERTICAL-L2
                        VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "H" '(VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 VERTICAL-R2 SYMMETRIC+))
    (setfeats tmp "I" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-C2 HORIZONTAL-L0
                        SYMMETRIC+))
    (setfeats tmp "J" '(VERTICAL-R1 VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        LEFT-FACING+))
    (setfeats tmp "K" '(VERTICAL-L1 FRONT-DIAGONAL-U1 HORIZONTAL-C1
                        VERTICAL-L2 BACK-DIAGONAL-L2 RIGHT-FACING+))
    (setfeats tmp "L" '(VERTICAL-L1 VERTICAL-L2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "M" '(VERTICAL-L1 BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1
                        VERTICAL-R1 VERTICAL-L2 VERTICAL-R2 SYMMETRIC+))
    (setfeats tmp "N" '(VERTICAL-L1 BACK-DIAGONAL-U1 VERTICAL-R1 VERTICAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 SYMMETRIC+))
    (setfeats tmp "O" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 VERTICAL-L2
                        VERTICAL-R2 HORIZONTAL-L0 SYMMETRIC+))
    (setfeats tmp "P" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 RIGHT-FACING+))
    (setfeats tmp "Q" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 VERTICAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        RIGHT-FACING+))
    (setfeats tmp "R" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 BACK-DIAGONAL-L2
                        RIGHT-FACING+))
    (setfeats tmp "S" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 SYMMETRIC+))
    (setfeats tmp "T" '(HORIZONTAL-T0 VERTICAL-C1 VERTICAL-C2 SYMMETRIC+))
    (setfeats tmp "U" '(VERTICAL-L1 VERTICAL-R1 VERTICAL-L2 VERTICAL-R2
                        HORIZONTAL-L0 SYMMETRIC+))
    (setfeats tmp "V" '(VERTICAL-L1 FRONT-DIAGONAL-U1 VERTICAL-L2
                        FRONT-DIAGONAL-L2 SYMMETRIC+))
    (setfeats tmp "W" '(VERTICAL-L1 VERTICAL-R1 VERTICAL-L2 FRONT-DIAGONAL-L2
                        BACK-DIAGONAL-L2 VERTICAL-R2 SYMMETRIC+))
    (setfeats tmp "X" '(BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1 FRONT-DIAGONAL-L2
                        BACK-DIAGONAL-L2 SYMMETRIC+))
    (setfeats tmp "Y" '(BACK-DIAGONAL-U1 FRONT-DIAGONAL-U1 VERTICAL-C2
                        SYMMETRIC+))
    (setfeats tmp "Z" '(HORIZONTAL-T0 FRONT-DIAGONAL-U1 FRONT-DIAGONAL-L2
                        HORIZONTAL-L0 SYMMETRIC+))
    
    (setfeats tmp "1" '(VERTICAL-R1 VERTICAL-R2 LEFT-FACING+))
    (setfeats tmp "2" '(HORIZONTAL-T0 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 HORIZONTAL-L0 LEFT-FACING+))
    (setfeats tmp "3" '(HORIZONTAL-T0 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 HORIZONTAL-L0 LEFT-FACING+))
    (setfeats tmp "4" '(VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-R2 LEFT-FACING+))
    (setfeats tmp "5" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1
                        BACK-DIAGONAL-L2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "6" '(HORIZONTAL-T0 VERTICAL-L1 HORIZONTAL-C1 HORIZONTAL-C2
                        VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0 RIGHT-FACING+))
    (setfeats tmp "7" '(HORIZONTAL-T0 VERTICAL-R1 VERTICAL-R2 LEFT-FACING+))
    (setfeats tmp "8" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        SYMMETRIC+))
    (setfeats tmp "9" '(HORIZONTAL-T0 VERTICAL-L1 VERTICAL-R1 HORIZONTAL-C1
                        HORIZONTAL-C2 VERTICAL-R2 HORIZONTAL-L0 LEFT-FACING+))
    (setfeats tmp "0" '(HORIZONTAL-T0 VERTICAL-L1 BACK-DIAGONAL-U1 VERTICAL-R1
                        VERTICAL-L2 BACK-DIAGONAL-L2 VERTICAL-R2 HORIZONTAL-L0
                        SYMMETRIC+))
    tmp))



#|
;;;; ---------------------------------------------------------------------- ;;;;
;;;; XXXX () feature set

(defun build-XX-set ()
  
  (let ((tmp (make-instance 'char-feats feat-ls
                            '())))
    (setfeats tmp "A" '())
    (setfeats tmp "B" '())
    (setfeats tmp "C" '())
    (setfeats tmp "D" '())
    (setfeats tmp "E" '())
    (setfeats tmp "F" '())
    (setfeats tmp "G" '())
    (setfeats tmp "H" '())
    (setfeats tmp "I" '())
    (setfeats tmp "J" '())
    (setfeats tmp "K" '())
    (setfeats tmp "L" '())
    (setfeats tmp "M" '())
    (setfeats tmp "N" '())
    (setfeats tmp "O" '())
    (setfeats tmp "P" '())
    (setfeats tmp "Q" '())
    (setfeats tmp "R" '())
    (setfeats tmp "S" '())
    (setfeats tmp "T" '())
    (setfeats tmp "U" '())
    (setfeats tmp "V" '())
    (setfeats tmp "W" '())
    (setfeats tmp "X" '())
    (setfeats tmp "Y" '())
    (setfeats tmp "Z" '())
    
    (setfeats tmp "1" '())
    (setfeats tmp "2" '())
    (setfeats tmp "3" '())
    (setfeats tmp "4" '())
    (setfeats tmp "5" '())
    (setfeats tmp "6" '())
    (setfeats tmp "7" '())
    (setfeats tmp "8" '())
    (setfeats tmp "9" '())
    (setfeats tmp "0" '())
    tmp))

|#


;;;; ---------------------------------------------------------------------- ;;;;

(defun all-feature-sets ()
  (list (build-gibson-set) (build-bh-set) (build-rm-orig-set) 
        (build-rm-clean-set)))


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
