;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : motor-compilation.lisp
;;; Version     : 1.0
;;; 
;;; Description : Production compilation MOTOR style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;; 2010.12.10 Dan
;;;             : * Changed m-b-c1 since buffer empty is not a sufficient case
;;;             :   for safety since the buffer is always empty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun COMPOSE-MOTOR-BUFFER (p1 p1-s p2 p2-s buffer)
  (declare (ignore p2))
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; There are no buffer tests or modification actions allowed.
  ;;
  ;; The constraints are:
  ;;
  ;;   If the first production doesn't make a query 
  ;;      or make a request (0)
  ;;      any query and action are used from the second
  ;;   If the first production makes a request without a query (4)
  ;;      there is no query and the action from the first is used
  ;;   If the first has a query and no request (16)
  ;;      the query from the first and the action
  ;;      of the second are used 
  ;;   If the first has both a query and a request (20)
  ;;      the query and action from the first are used
  
  
  (let* ((bn (intern (concatenate 'string (symbol-name buffer) ">")))
         (b+ (intern (concatenate 'string "+" (symbol-name bn))))
         (b? (intern (concatenate 'string "?" (symbol-name bn))))
         
         (q1 (copy-tree (find b? (first p1-s) :key #'car)))
         (q2 (copy-tree (find b? (first p2-s) :key #'car)))
         
         (a1+ (copy-tree (find b+ (second p1-s) :key #'car)))
         (a2+ (copy-tree (find b+ (second p2-s) :key #'car))))
    
    (case (aif (cdr (assoc buffer (production-buffer-indices p1))) it 0)
      (0 
       (list (when q2 (list q2))  
             (when a2+ (list a2+))))
      (4
       (list nil
             (when a1+ (list a1+))))
      (16 
       (list (when q1 (list q1))  
             (when a2+ (list a2+))))
      
      (20
       (list (when q1 (list q1))  
             (when a1+ (list a1+)))))))

(defun M-B-C1 (buffer p1 p2)
  "Compilation check for queries such that p2 only uses 'state busy' 
  since buffer empty is meaningless for motor style buffers"
  (declare (ignore p1))
  (let ((queries (mapcan #'third  (copy-tree (remove-if-not 
                                              #'(lambda (x)
                                                  (equal (car x) (cons #\? buffer)))
                                              (production-lhs p2))))))
    (every #'(lambda (x)      
               (and (eq (first x) '=)
                    (eq (second x) 'state)
                    (eq (third x) 'busy)))
           queries)))


(defun M-B-C2 (buffer p1 p2)
  "queries in p1 and p2 must be the same
   NOTE: this doesn't take into account any variables at this time"
  (let ((queries-1 (remove-duplicates
                    (mapcan #'third  (copy-tree (remove-if-not 
                                                 #'(lambda (x)
                                                     (equal (car x) (cons #\? buffer)))
                                                 (production-lhs p1))))
                    :test #'equal))
        (queries-2 (remove-duplicates 
                    (mapcan #'third  (copy-tree (remove-if-not 
                                                 #'(lambda (x)
                                                     (equal (car x) (cons #\? buffer)))
                                                 (production-lhs p2))))
                    :test #'equal)))
    
    (= (length queries-1) (length queries-2) 
       (length (remove-duplicates (append queries-1 queries-2) :test #'equal)))))

(define-compilation-type MOTOR ((20 16 M-B-C1)
                                (20 0 T)
                                (16 20 M-B-C2)
                                (16 16 M-B-C2)
                                (16 4 T)
                                (16 0 T)
                                (4 16 M-B-C1)
                                (4 0 T)
                                (0 20 T)
                                (0 16 T)
                                (0
                                 4
                                 T)) :DEFAULT NIL COMPOSE-MOTOR-BUFFER NIL NIL NIL)

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
