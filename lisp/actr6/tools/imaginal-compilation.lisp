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
;;; Filename    : imaginal-compilation.lisp
;;; Version     : 1.0
;;; 
;;; Description : Production compilation IMAGINAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;;
;;; ----- History -----
;;;
;;; 2010.12.10 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;; 2010.12.13 Dan
;;;             : * Put the control functions and tests in place.
;;; 2010.12.15 Dan
;;;             : * Added module to the mapping functions args list.
;;; 2010.12.20 Dan
;;;             : * Added some additional code to handle things better when 
;;;             :   :ppm is enabled -- creates productions that match more closely
;;;             :   to the same conditions as the original pair did and does 
;;;             :   the "same" thing.
;;;             : * Allow subtypes to be composed in the consistency check.
;;; 2011.01.07 Dan
;;;             : * Fixed a bug with map-imaginal-buffer since it needs to be a
;;;             :   let* for ppm to be used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun MAP-IMAGINAL-BUFFER (module p1 p1-s p2 p2-s buffer)
 "map references from p1 to p2 for imaginal style buffer"
  ;; Possible cases: 
  ;;    a RHS + to a LHS = (includes when the RHS has both + and =)
  ;;    a RHS = to a LHS = 
  ;;    a LHS = with null RHS to a LHS =
  ;;    a RHS * to a LHS =
 
  (let* ((p1-style (cdr (assoc buffer (production-buffer-indices p1))))
         (p2-style (cdr (assoc buffer (production-buffer-indices p2))))
         (ppm (compilation-module-ppm module))
         (bindings (when ppm 
                     (append (second (compilation-module-previous module))
                             (production-compilation-instan (production-name p2))))))
    
    (cond (;; The RHS + to LHS = case
           (and (find p1-style '(4 12 13 20 28 29))
                (find p2-style '(8 9 12 13 24 25 28 29 40 56)))
           
           ;; Map the RHS +'s with the LHS ='s
           
           ;; here the slots of interest are just the intersection 
           ;; of the two sets
           ;;
           
           (let* ((mappings nil)
                  (p1-slots (cadr (find (intern (concatenate 'string "+" (symbol-name buffer) ">")) (second p1-s) :key #'car)))
                  (p2-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key #'car)))
                  (interesting-slots (intersection (mapcan #'(lambda (x)
                                                               (when (eq (car x) '=)
                                                                 (list (second x))))
                                                     p1-slots)
                                                   (mapcan #'(lambda (x)
                                                               (when (eq (car x) '=)
                                                                 (list (second x))))
                                                     p2-slots))))
             
             
             ;(format t "P1-slots ~S~%p2-slots ~S~%Interesting slots ~S~%"
             ;  p1-slots
             ;  p2-slots 
             ;  interesting-slots)
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not #'(lambda (x) (and (eq (first x) '=) (eq (second x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not #'(lambda (x) (and (eq (first x) '=) (eq (second x) slot))) p2-slots))
                   (if (constant-value-p (third p2slots))
                       (if ppm
                           (if (constant-value-p (third p1slots))
                               (push (cons (third p1slots) (third p2slots)) mappings)
                             (push (find (third p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (third p1slots) (third p2slots)) mappings))
                     (push (cons (third p2slots) (third p1slots)) mappings)))))
             
             mappings))
          
          (;; The RHS = to a LHS = case
           (and (find p1-style '(9 25))
                (find p2-style '(8 9 12 13 24 25 28 29)))
           
           ;; Map the RHS ='s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (p1-slotsa (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key #'car)))
                  (p1-slotsb (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (second p1-s) :key #'car)))
                  (p2-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key #'car)))
                  
                  (p1-slots (append (remove-if #'(lambda (x)
                                                   (or (not (eq (car x) '=))
                                                       (find (second x) p1-slotsb :key #'car)))
                                               p1-slotsa)
                                    (mapcar #'(lambda (x)
                                                (append (list '=) x))
                                      p1-slotsb)))
                  (interesting-slots  (intersection (mapcan #'(lambda (x) (list (second  x)))
                                                      p1-slots)
                                                    (mapcan #'(lambda (x)
                                                                (when (eq (car x) '=)
                                                                  (list (second x))))
                                                      p2-slots))))
             
             
             ; (format t "P1-slotsa ~S~%P1-slotsb ~S~%P1-slots  ~S~%p2-slots ~S~%Interesting slots ~S~%"
             ;   p1-slotsa
             ;   p1-slotsb
             ;   p1-slots
             ;   p2-slots 
             ;   interesting-slots)
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not #'(lambda (x) (eq (second x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not #'(lambda (x) (eq (second x) slot)) p2-slots))
                   (if (constant-value-p (third p2slots))
                       (if ppm
                           (if (constant-value-p (third p1slots))
                               (push (cons (third p1slots) (third p2slots)) mappings)
                             (push (find (third p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (third p1slots) (third p2slots)) mappings))
                     (push (cons (third p2slots) (third p1slots)) mappings)))))
             
             mappings)
           
           
           )
          
          (;; The RHS * to a LHS = case
           (and (find p1-style '(40 56))
                (find p2-style '(8 24 40 56)))
           
           ;; Map the RHS *'s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  
                  (request (intern (concatenate 'string "+" (symbol-name buffer) ">")))
                  
                  (p1-slotsa (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key #'car)))
                  (p1-slotsb (cadr (find-if (lambda (x) (and (eq (car x) request) (neq (caaadr x) 'isa)))  (second p1-s))))
                  (p2-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key #'car)))
                  
                  (p1-slots (append (remove-if #'(lambda (x)
                                                   (or (not (eq (car x) '=))
                                                       (find (second x) p1-slotsb :key #'car)))
                                               p1-slotsa)
                                    (mapcar #'(lambda (x)
                                                (append (list '=) x))
                                      p1-slotsb)))
                  (interesting-slots  (intersection (mapcan #'(lambda (x) (list (second  x)))
                                                      p1-slots)
                                                    (mapcan #'(lambda (x)
                                                                (when (eq (car x) '=)
                                                                  (list (second x))))
                                                      p2-slots))))
             
             
             ; (format t "P1-slotsa ~S~%P1-slotsb ~S~%P1-slots  ~S~%p2-slots ~S~%Interesting slots ~S~%"
             ;   p1-slotsa
             ;   p1-slotsb
             ;   p1-slots
             ;   p2-slots 
             ;   interesting-slots)
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not #'(lambda (x) (eq (second x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not #'(lambda (x) (eq (second x) slot)) p2-slots))
                   (if (constant-value-p (third p2slots))
                       (if ppm
                           (if (constant-value-p (third p1slots))
                               (push (cons (third p1slots) (third p2slots)) mappings)
                             (push (find (third p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (third p1slots) (third p2slots)) mappings))
                     (push (cons (third p2slots) (third p1slots)) mappings)))))
             
             mappings))
                    
          (;; The LHS = RHS null to a LHS = case
           (and (find p1-style '(8 24))
                (find p2-style '(8 9 12 13 24 25 28 29 40 56)))
           
           ;; Map the LHS ='s with the LHS ='s
           
           
           ;; The slots of interest are the ones at the intersection of the
           ;; two sets - the mappings are then done for those
           ;; such that 
           ;;   - if it's a variable in both then p2 vars go to p1 vars 
           ;;   - if it's a constant in one then it goes from the var to the constant
           ;;     (note that buffer variables are considered constants and not variables)
           ;;   - if it's a constant in both we're in trouble if they aren't equal
           ;;     because how did they fire...
           ;;
           ;; When there is more than one option we have to add both but they need to
           ;; be evaluated in the order of variables before constants (that's handled
           ;; elsewhere though)
           
           
           (let* ((mappings nil)
                  (p1-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key #'car)))
                  (p2-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key #'car)))
                  (interesting-slots (intersection (mapcan #'(lambda (x)
                                                               (when (eq (car x) '=)
                                                                 (list (second x))))
                                                     p1-slots)
                                                   (mapcan #'(lambda (x)
                                                               (when (eq (car x) '=)
                                                                 (list (second x))))
                                                     p2-slots))))
             
             
             ;(format t "P1-slots ~S~%p2-slots ~S~%Interesting slots ~S~%"
             ;  p1-slots
             ;  p2-slots 
             ;  interesting-slots)
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not #'(lambda (x) (and (eq (first x) '=) (eq (second x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not #'(lambda (x) (and (eq (first x) '=) (eq (second x) slot))) p2-slots))
                   (if (constant-value-p (third p2slots))
                       (if ppm
                           (if (constant-value-p (third p1slots))
                               (push (cons (third p1slots) (third p2slots)) mappings)
                             (push (find (third p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (third p1slots) (third p2slots)) mappings))
                     (push (cons (third p2slots) (third p1slots)) mappings)))))
             
             mappings
             
             ))
          
          (t
           nil))))


(defun COMPOSE-IMAGINAL-BUFFER (p1 p1-s p2 p2-s buffer)
    (declare (ignore p2))
  ;; Generally:
  ;;   If the first has a + (4, 12, 13, 20, 28 29) then
  ;;      the conditions are those of the first including a query
  ;;      the actions are the = or * of the first if there is one and
  ;;      the + will be the + of the first (can't be a + in the second)
  ;;      with the = or * of the second unioned in and overriding
  
  ;;   If the first has no actions (0, 8, 16 24)
  ;;      the buffer conditions are the union of those in the first
  ;;      and those of the second with the query from the first being
  ;;      used if there is one (16 24) otherwise the query from the second
  ;;      is used if there is one
  ;;      the actions are those of the second
  
  ;;   Otherwise (9 40 25 56)
  ;;      the conditions are the union of those in the first
  ;;      and those from the second that are not set by the 
  ;;      actions of the first If there is a query in the first it is
  ;;      used (25 56) otherwise a query from the second is used
  ;;      the actions are the = or * from the first with the = or * from
  ;;      the second unioned in and overriding and
  ;;      the + of the second if there is one
  ;;
  
  
  (let* ((bn (intern (concatenate 'string (symbol-name buffer) ">")))
         (b= (intern (concatenate 'string "=" (symbol-name bn))))
         (b+ (intern (concatenate 'string "+" (symbol-name bn))))
         (b? (intern (concatenate 'string "?" (symbol-name bn))))
         
         (c1 (copy-tree (find b= (first p1-s) :key #'car)))
         (c2 (copy-tree (find b= (first p2-s) :key #'car)))
         (q1 (copy-tree (find b? (first p1-s) :key #'car)))
         (q2 (copy-tree (find b? (first p2-s) :key #'car)))
         
         (a1= (copy-tree (find b= (second p1-s) :key #'car)))
         (a2= (copy-tree (find b= (second p2-s) :key #'car)))
         
         (a1+ (copy-tree (find-if (lambda (x) (and (eq (car x) b+) (eq (caaadr x) 'isa))) (second p1-s))))
         (a2+ (copy-tree (find-if (lambda (x) (and (eq (car x) b+) (eq (caaadr x) 'isa))) (second p2-s))))
         (a1* (copy-tree (find-if (lambda (x) (and (eq (car x) b+) (neq (caaadr x) 'isa))) (second p1-s))))
         (a2* (copy-tree (find-if (lambda (x) (and (eq (car x) b+) (neq (caaadr x) 'isa))) (second p2-s)))))
         
         
    ;(format t "~%~{~S~%~}" (list bn b= b+ c1 c2 a1= a2= a1+ a2+ a1* a2*))
    
    
    (case (aif (cdr (assoc buffer (production-buffer-indices p1))) it 0)
      ((4 12 13 20 28 29)
       ;(pprint (list 4 12 13 20 28 29))
       (list (append (when c1 (list c1)) (when q1 (list q1)))
             (append (when (or a1= a1*) (if a1= (list a1=) (list a1*))) ;; can't have both with current description
                     (cond ((and a1+ a2=)
                            (awhen (buffer+-union a1+ a2=) (list it)))
                           ((and a1+ a2*)
                            (awhen (buffer+-union a1+ a2*) (list it)))
                           (a1+
                            (list a1+))
                           (t nil)))))
      ((0 8 16 24)
       ;(pprint (list 0 8 16 24))
       (list (append (awhen (buffer-condition-union c1 c2 a1=) (list it))  ;; a1= is always nil, though  so why use it?
                     (if q1 (list q1) (if q2 (list q2) nil)))
             (append (when a2= (list a2=)) (when a2* (list a2*)) (when a2+ (list a2+)))))
      ((9 40 25 56)
       ;(pprint (list 9 40 25 56))
       
       (list (append (awhen (buffer-condition-union c1 c2 (if a1= a1= a1*)) (list it))
                     (if q1 (list q1) (if q2 (list q2) nil)))
             (append (cond ((or a1= a2=) ;; if there's at least one = union those                             
                            (awhen (buffer=-union a1= a2=) (list it)))
                           ((or a1* a2*) ;; if there's at least one * union those
                            (awhen (buffer=-union a1* a2*) (list it)))
                           (t nil)) ;; can't have a mix of = and * so just ignore otherwise
                           (when a2+ (list a2+))))))))


(defun CHECK-IMAGINAL-CONSISTENCY (buffer module p1 p2)
  (case (get-buffer-index p1 buffer)
    ((4 12 13 20 28 29) ;; a RHS +
     (check-consistency module (find (cons #\+ buffer) (production-rhs p1) :key #'car :test #'equal)
                        (second (compilation-module-previous module))
                        (find (cons #\= buffer) (production-lhs p2) :key #'car :test #'equal)
                        (production-bindings p2)
                        :allow-subtypes t))
       
    ((9 25) ;; a RHS =
     (check-consistency module (cons (chunk-spec-chunk-type (third (find (cons #\= buffer) (production-lhs p1) :key #'car :test #'equal)))
                                     (find (cons #\= buffer) (production-rhs p1) :key #'car :test #'equal))
                        (second (compilation-module-previous module))
                        (find (cons #\= buffer) (production-lhs p2) :key #'car :test #'equal)
                        (production-bindings p2)
                        :allow-subtypes t))
    
    
    ((40 56) ;; a RHS *
     (check-consistency module (cons (chunk-spec-chunk-type (third (find (cons #\= buffer) (production-lhs p1) :key #'car :test #'equal)))
                                     (find-if (lambda (x) (and (equal (cons #\+ buffer) (car x)) (neq 'isa (caadr x)))) (production-rhs p1)))
                        (second (compilation-module-previous module))
                        (find (cons #\= buffer) (production-lhs p2) :key #'car :test #'equal)
                        (production-bindings p2)
                        :allow-subtypes t))
    
    
    (t
     t
     )))

(defun PRE-INSTANTIATE-IMAGINAL (buffer-and-index p2)
  t ;; Should always be done, right???
  )


(defun I-B-C3 (buffer p1 p2)
"Compilation check queries in p2 for 'state free'"
  (declare (ignore p1))
  (let ((queries (mapcan #'third (copy-tree (remove-if-not 
                                             #'(lambda (x)
                                                 (equal (car x) (cons #\? buffer)))
                                             (production-lhs p2))))))
    (every #'(lambda (x)      
               (and (eq (first x) '=)
                    (eq (second x) 'state)
                    (eq (third x) 'free)))
           queries)))


(defun I-B-C1 (buffer p1 p2)
"Compilation check for queries such that p2 only uses 'buffer empty' or 'state busy'"
  (declare (ignore p1))
  (let ((queries (mapcan #'third (copy-tree (remove-if-not 
                                             #'(lambda (x)
                                                 (equal (car x) (cons #\? buffer)))
                                             (production-lhs p2))))))
    (every #'(lambda (x)      
               (eq (first x) '=)
               (or (and (eq (second x) 'state)
                        (eq (third x) 'busy))
                   (and (eq (second x) 'buffer)
                        (eq (third x) 'empty))))
           
           queries)))

(defun NO-RHS-IMAGINAL-REF (buffer p1 p2)
  "Can't compile if the variable naming the buffer is used in the actions of p2"
  (not (recursive-find (intern (concatenate 'string "=" (symbol-name buffer)))
                       (second (production-standard-rep p2))))) 

(defun I-B-C4 (buffer p1 p2)
  (and (i-b-c3 buffer p1 p2)
       (no-rhs-imaginal-ref buffer p1 p2)))

(defun I-B-C2 (buffer p1 p2)
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

(define-compilation-type IMAGINAL ((56 56 I-B-C3)
                                   (56 40 T)
                                   (56 24 I-B-C3)
                                   (56 16 I-B-C1)
                                   (56 8 T)
                                   (56 0 T)
                                   (40 56 I-B-C3)
                                   (40 40 T)
                                   (40 24 I-B-C3)
                                   (40 16 I-B-C1)
                                   (40 8 T)
                                   (40 0 T)
                                   (29 25 I-B-C3)
                                   (29 24 I-B-C3)
                                   (29 16 I-B-C1)
                                   (29 9 NO-RHS-IMAGINAL-REF)
                                   (29 8 NO-RHS-IMAGINAL-REF)
                                   (29 0 T)
                                   (28 56 I-B-C4)
                                   (28 40 NO-RHS-IMAGINAL-REF)
                                   (28 25 I-B-C3)
                                   (28 24 I-B-C3)
                                   (28 16 I-B-C1)
                                   (28 9 NO-RHS-IMAGINAL-REF)
                                   (28 8 NO-RHS-IMAGINAL-REF)
                                   (28 0 T)
                                   (25 29 I-B-C2)
                                   (25 28 I-B-C2)
                                   (25 25 I-B-C2)
                                   (25 24 I-B-C2)
                                   (25 20 I-B-C2)
                                   (25 16 I-B-C2)
                                   (25 13 T)
                                   (25 12 T)
                                   (25 9 T)
                                   (25 8 T)
                                   (25 4 T)
                                   (25 0 T)
                                   (24 56 I-B-C2)
                                   (24 40 T)
                                   (24 29 I-B-C2)
                                   (24 28 I-B-C2)
                                   (24 25 I-B-C2)
                                   (24 24 I-B-C2)
                                   (24 20 I-B-C2)
                                   (24 16 I-B-C2)
                                   (24 13 T)
                                   (24 12 T)
                                   (24 9 T)
                                   (24 8 T)
                                   (24 4 T)
                                   (24 0 T)
                                   (20 56 I-B-C4)
                                   (20 40 NO-RHS-IMAGINAL-REF)
                                   (20 25 I-B-C3)
                                   (20 24 I-B-C3)
                                   (20 16 I-B-C1)
                                   (20 9 NO-RHS-IMAGINAL-REF)
                                   (20 8 NO-RHS-IMAGINAL-REF)
                                   (20 0 T)
                                   (16 56 I-B-C2)
                                   (16 40 T)
                                   (16 29 I-B-C2)
                                   (16 28 I-B-C2)
                                   (16 25 I-B-C2)
                                   (16 24 I-B-C2)
                                   (16 20 I-B-C2)
                                   (16 16 I-B-C2)
                                   (16 13 T)
                                   (16 12 T)
                                   (16 9 T)
                                   (16 8 T)
                                   (16 4 T)
                                   (16 0 T)
                                   (13 25 I-B-C3)
                                   (13 24 I-B-C3)
                                   (13 16 I-B-C1)
                                   (13 9 NO-RHS-IMAGINAL-REF)
                                   (13 8 NO-RHS-IMAGINAL-REF)
                                   (13 0 T)
                                   (12 56 I-B-C4)
                                   (12 40 NO-RHS-IMAGINAL-REF)
                                   (12 25 I-B-C3)
                                   (12 24 I-B-C3)
                                   (12 16 I-B-C1)
                                   (12 9 NO-RHS-IMAGINAL-REF)
                                   (12 8 NO-RHS-IMAGINAL-REF)
                                   (12 0 T)
                                   (9 29 T)
                                   (9 28 T)
                                   (9 25 T)
                                   (9 24 T)
                                   (9 20 T)
                                   (9 16 T)
                                   (9 13 T)
                                   (9 12 T)
                                   (9 9 T)
                                   (9 8 T)
                                   (9 4 T)
                                   (9 0 T)
                                   (8 56 T)
                                   (8 40 T)
                                   (8 29 T)
                                   (8 28 T)
                                   (8 25 T)
                                   (8 24 T)
                                   (8 20 T)
                                   (8 16 T)
                                   (8 13 T)
                                   (8 12 T)
                                   (8 9 T)
                                   (8 8 T)
                                   (8 4 T)
                                   (8 0 T)
                                   (4 56 I-B-C4)
                                   (4 40 NO-RHS-IMAGINAL-REF)
                                   (4 25 I-B-C3)
                                   (4 24 I-B-C3)
                                   (4 16 I-B-C1)
                                   (4 9 NO-RHS-IMAGINAL-REF)
                                   (4 8 NO-RHS-IMAGINAL-REF)
                                   (4 0 T)
                                   (0 56 T)
                                   (0 40 T)
                                   (0 29 T)
                                   (0 28 T)
                                   (0 25 T)
                                   (0 24 T)
                                   (0 20 T)
                                   (0 16 T)
                                   (0 13 T)
                                   (0 12 T)
                                   (0 9 T)
                                   (0 8 T)
                                   (0
                                    4
                                    T)) (IMAGINAL) MAP-IMAGINAL-BUFFER COMPOSE-IMAGINAL-BUFFER CHECK-IMAGINAL-CONSISTENCY PRE-INSTANTIATE-IMAGINAL NIL)

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
