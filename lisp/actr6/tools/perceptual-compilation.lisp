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
;;; Filename    : perceptual-compilation.lisp
;;; Version     : 1.0
;;; 
;;; Description : Production compilation PERCEPTUAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun COMPOSE-PERCEPTUAL-BUFFER (p1 p1-s p2 p2-s buffer)
  (declare (ignore p2))
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; There are no buffer modification actions allowed.
  ;;
  ;; The constraints are:
  ;;
  ;;
  ;;   If the first production doesn't mention the buffer 
  ;;      or make a request (0)
  ;;      any = condition, query and action are used from the second
  ;;   If the first production makes a request without a query (4, 12)
  ;;      any = condition in the first production is used, there
  ;;      are no queries and the action from the first is used
  ;;   If the first production tests the buffer but doesn't make
  ;;      any queries or requests (8)
  ;;      any = condition in the first is used along with any 
  ;;      query from the second and the action of the second
  ;;   If the first a query and no request (16, 24)
  ;;      any = condition in either (there can be at most 1) is used 
  ;;      along with the query from the first and the action
  ;;      of the second
  ;;   If the first has both a query and a request (20, 28)
  ;;      the = condition query and action from the first are used
  
  
  (let* ((bn (intern (concatenate 'string (symbol-name buffer) ">")))
         (b= (intern (concatenate 'string "=" (symbol-name bn))))
         (b+ (intern (concatenate 'string "+" (symbol-name bn))))
         (b? (intern (concatenate 'string "?" (symbol-name bn))))
         
         (c1 (copy-tree (find b= (first p1-s) :key #'car)))
         (c2 (copy-tree (find b= (first p2-s) :key #'car)))
         (q1 (copy-tree (find b? (first p1-s) :key #'car)))
         (q2 (copy-tree (find b? (first p2-s) :key #'car)))
         
         (a1+ (copy-tree (find b+ (second p1-s) :key #'car)))
         (a2+ (copy-tree (find b+ (second p2-s) :key #'car))))
    
    (case (aif (cdr (assoc buffer (production-buffer-indices p1))) it 0)
      (0 
       (list (append (when c2 (list c2)) (when q2 (list q2)))  
             (when a2+ (list a2+))))
      ((4 12)
       (list (when c1 (list c1))
             (when a1+ (list a1+))))
      (8
       (list (append (when c1 (list c1)) (when q2 (list q2)))
             (when a2+ (list a2+))))
      ((16 24)
       (list (append (awhen (buffer-condition-union c1 c2 nil) (list it)) (when q1 (list q1)))
             (when a2+ (list a2+))))
      ((20 28)
       (list (append (when c1 (list c1)) (when q1 (list q1)))
             (when a1+ (list a1+)))))))

(defun P-B-C1 (buffer p1 p2)
  "Compilation check for queries such that p2 only uses 'buffer empty' or
   'state busy'"
  (declare (ignore p1))
  (let ((queries (mapcan #'third  (copy-tree (remove-if-not 
                                              #'(lambda (x)
                                                  (equal (car x) (cons #\? buffer)))
                                              (production-lhs p2))))))
    (every #'(lambda (x)      
               (and (eq (first x) '=)
                    (or (and (eq (second x) 'state)
                             (eq (third x) 'busy))
                        (and (eq (second x) 'buffer)
                             (eq (third x) 'empty)))))
           
           queries)))


(defun P-B-C2 (buffer p1 p2)
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

(define-compilation-type PERCEPTUAL ((28 16 P-B-C1)
                                     (28 0 T)
                                     (24 20 P-B-C2)
                                     (24 16 P-B-C2)
                                     (24 4 T)
                                     (24 0 T)
                                     (20 16 P-B-C1)
                                     (20 0 T)
                                     (16 28 P-B-C2)
                                     (16 24 P-B-C2)
                                     (16 20 P-B-C2)
                                     (16 16 P-B-C2)
                                     (16 12 T)
                                     (16 8 T)
                                     (16 4 T)
                                     (16 0 T)
                                     (12 16 P-B-C1)
                                     (12 0 T)
                                     (8 20 T)
                                     (8 16 T)
                                     (8 4 T)
                                     (8 0 T)
                                     (4 16 P-B-C1)
                                     (4 0 T)
                                     (0 28 T)
                                     (0 24 T)
                                     (0 20 T)
                                     (0 16 T)
                                     (0 12 T)
                                     (0 8 T)
                                     (0 4 T)) (VISUAL-LOCATION
                                               VISUAL
                                               AURAL-LOCATION
                                               AURAL
                                               TEMPORAL) NIL COMPOSE-PERCEPTUAL-BUFFER NIL NIL NIL)

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
