;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : conflict-tree.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code for creating and searching a simple decision tree of
;;;               production conditions.
;;; 
;;; Bugs        :
;;;
;;; To do       : [ ] Consider handling conditions other than constants.
;;;             : [ ] See if it can infer other things about non-equal or
;;;             :     numeric values in creating the tree.
;;;             : [ ] Consider extending the tree when adding new productions
;;;             :     on the fly - track the used tests and call build-...
;;;             : [ ] Make the negative limit a parameter somewhere.
;;; 
;;; ----- History -----
;;; 2008.12.23 Dan [1.0]
;;;            : * First complete version of the tree building/searching code
;;;            :   for use in production selection.
;;; 2008.01.07 Dan
;;;            : * Fixed print-conflict-tree.
;;; 2009.06.03 Dan
;;;            : * Added a hard limit to how many negative splits can occur
;;;            :   sequentially when building the tree (3).  If it gets that
;;;            :   far it just gives up on the branch and creates a leaf.
;;; 2010.04.26 Dan
;;;            : * Tree-condition-equal now uses eql for the value tests to
;;;            :   make sure numerical slot conditions can be considered
;;;            :   the same (doesn't affect the matching itself).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Builds a tree from production conditions.  Initially uses all the productions
;;; in the model definition with the ID3 algorithm to try and grow a small tree.  
;;; If productions get added after the model is defined (through compilation or 
;;; otherwise) it will only augment the tree if needed (a new branch at a slot 
;;; node) otherwise they just end up in an existing leaf.
;;;
;;; One assumption in the matching is that chunk names used in constant slot 
;;; tests (those hard coded into the productions) will always have thier true
;;; name.  It's not likely people have models where that's not the case, but
;;; it is technically possible to build such a model and it would not work
;;; correctly with the tree based matching.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; print-conflict-tree
;;; 
;;;     Has no parameters.  Can be called to print out the tree in the current
;;;     model's procedural module.  The tree is printed one node per line with
;;;     indenting and numbering indicating depth within a branch.
;;;
;;; conflict-tree-stats
;;;
;;;     Takes no parameters.  Will return a list of cons cells with some info
;;;     about the tree in the current procedural module.  The cons cells have
;;;     a description of the stat as the car and the value as the cdr.  The
;;;     current stats computed are:
;;;     :depth - deepest node in the tree
;;;     :min-depth - shortest branch in the tree
;;;     :total-nodes - count of all nodes (internal and terminal) in the tree
;;;     :terminal - count of only terminal nodes (leaves)
;;;     :non-empty - number of leaf nodes with 1 or more productions that
;;;                  are valid
;;;     :sets - the number of different sets of productions found at in the
;;;             non-empty leaf nodes
;;;
;;; > (conflict-tree-stats )
;;; ((:DEPTH . 24) (:MIN-DEPTH . 3) (:TOTAL-NODES . 4670) (:TERMINAL . 3422) (:NON-EMPTY . 3373) (:SETS . 1073))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; For info. gain calculation it considers each production to be a separate
;;; classification which makes entropy and gain simply based on production count.
;;; Additional assumption in building the tree is that it stops expanding if
;;; it finds that all possible splits are equal and no better than the current
;;; split (0 or less info. gain).  It doesn't just stop if the gain is negative
;;; because a "bad" split can sometimes be benifical anyway.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defstruct conflict-node
  parent branch (valid nil) entropy)

(defstruct (root-node (:include conflict-node)) child)
(defstruct (leaf-node (:include conflict-node)))

(defstruct (test-node (:include conflict-node))
  condition buffer value)

(defstruct (binary-test-node (:include test-node))
   true false)

(defstruct (isa-node (:include binary-test-node))
  buffer-index)

(defstruct (query-node (:include binary-test-node))
  query)

(defstruct (test-slot-node (:include binary-test-node))
  buffer-index slot slot-index test)


(defstruct (wide-test-node (:include test-node))
  (children (make-hash-table :size 10 :test 'equalp)))

(defstruct (slot-node (:include wide-test-node))
  buffer-index slot slot-index)



;;; Find the valid productions given the tree and the procedural module
;;; returns the list of valid productions and a table with the conditions tested

(defmethod get-valid ((node conflict-node) prod )
  (get-valid (select-child node prod) prod ))

(defmethod get-valid ((node test-node) prod )
  (get-valid (select-child node prod) prod ))

(defmethod get-valid ((node leaf-node) prod )
  (declare (ignore prod))
  (conflict-node-valid node))

(defmethod get-valid ((node null) prod)
  (declare (ignore prod))
  node)


;;; pick the next node to check

(defmethod select-child ((node root-node) prod)
  (declare (ignore prod))
  (root-node-child node))

(defmethod select-child ((node isa-node) prod)
  (let ((type (aif (cr-buffer-read prod (isa-node-buffer node) (isa-node-buffer-index node))
                   (chunk-chunk-type-fct it)
                   nil)))
    (if (and type (chunk-type-subtype-p-fct type (isa-node-value node)))
        (isa-node-true node)
      (isa-node-false node))))

(defmethod select-child ((node test-slot-node) prod)
  (let ((buffer-val (cr-buffer-slot-read prod (test-slot-node-buffer node) (test-slot-node-buffer-index node) (test-slot-node-slot-index node))))
    (if (funcall (test-slot-node-test node) buffer-val (test-slot-node-value node))
        (test-slot-node-true node)
      (test-slot-node-false node))))


(defmethod select-child ((node query-node) prod)
  (if (query-buffer (query-node-buffer node) (list (cons (query-node-query node) (query-node-value node))))
      (binary-test-node-true node)
    (binary-test-node-false node)))

(defmethod select-child ((node slot-node) prod)
  (let ((buffer-val (cr-buffer-slot-read prod (slot-node-buffer node) (slot-node-buffer-index node) (slot-node-slot-index node))))
    (aif (gethash buffer-val (slot-node-children node))
         it
         (gethash :other (slot-node-children node)))))




;;; Print out the tree for debugging purposes

(defun print-conflict-tree ()
  (walk-tree (procedural-conflict-tree (get-module procedural))))

(defun walk-tree (node)
  (print-tree node nil 0 ))

(defun print-tree (node branch depth)
    
  (cond ((root-node-p node)
         (format t "---->[root ~s]~%" (conflict-node-valid node))
         (awhen (root-node-child node)
                (print-tree it t (+ depth 2))))
        ((leaf-node-p node)
         (format t "~vt-~d- ~s ->[leaf ~s]~%" depth (/ depth 2) branch (conflict-node-valid node))
         )
        ((binary-test-node-p node)
         (format t "~vt-~d- ~s ->[~s " depth (/ depth 2) branch (cr-condition-type (test-node-condition node)))
         (case (cr-condition-type (test-node-condition node))
           (isa (format t " ~a ~s]~%" (test-node-buffer node) (test-node-value node)))
           (query (format t " ~a ~s ~s]~%" (test-node-buffer node) (query-node-query node) (test-node-value node)))
           (test-slot (format t " ~a ~s ~s ~s]~%" (test-node-buffer node) (test-slot-node-slot node) (test-slot-node-test node) (test-node-value node))))
         (awhen (binary-test-node-true node)
                (print-tree it t (+ depth 2)))
         (awhen (binary-test-node-false node)
                (print-tree it nil (+ depth 2))))
        ((wide-test-node-p node)
         (format t "~vt-~d- ~s ->[~s ~a ~s]~%" depth (/ depth 2) branch (cr-condition-type (test-node-condition node)) (test-node-buffer node) (slot-node-slot node))
         (maphash (lambda (branch node)
                    (print-tree node branch (+ depth 2)))
                  (wide-test-node-children node)))))
   

;;; Compute tree statistics for comparison and analysis

(defvar *tree-data* nil)

(defun conflict-tree-stats ()
  (setf *tree-data* nil)
  (mapcar #'cons '(:depth :min-depth :total-nodes :terminal :non-empty :sets) 
    (append (multiple-value-list (get-tree-stats (procedural-conflict-tree (get-module procedural))))
            (list (length *tree-data*)) (list (length (remove nil *tree-data*)))
            (list (length (remove-duplicates *tree-data* :test 'equalp))))))

(defmethod get-tree-stats ((node binary-test-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (awhen (binary-test-node-true node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (setf min-depths md)
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (awhen (binary-test-node-false node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (when (or (null min-depths) (< md min-depths))
               (setf min-depths md))
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))

(defmethod get-tree-stats ((node wide-test-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (maphash (lambda (branch node)
               (declare (ignore branch))
               (multiple-value-bind (d md n)
                   (get-tree-stats node)
                 (incf nodes n)
                 (setf min-depths md)
                 (when (> d max-depth)
                   (setf max-depth d))))
             (wide-test-node-children node))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))

(defmethod get-tree-stats ((node root-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (awhen (root-node-child node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (setf min-depths md)
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))


(defmethod get-tree-stats ((node leaf-node))
  (push (leaf-node-valid node) *tree-data*)
  (values 1 1 1))

#| don't generally build incrementally now so not updating this for the current representations 
   unless something changes to make it useful

(defun get-production-stats ()
  (let ((c nil))
    (dolist (p (productions-list (get-module procedural)))
      (setf c (append c (production-constants p))))
    (list (cons :total-constants (length c)) 
          (cons :unique-constants (length (remove-duplicates c :test 'equalp))) 
          (cons :unique-tests (length (remove-duplicates c :test (lambda (x y) (equal (butlast x) (butlast y)))))))))

(defun detail-adding-ps (ps)
  (dolist (x ps)
    (eval x)
  (format t "~s: ~s ~s~%" (second x) (get-production-stats) (conflict-tree-stats))))

|#


;;; Build the tree incrementally when a production is parsed
;;; only happens after the initial tree creation - which should only be production compilation
;;; for most models.
;;; 
;;; Don't add any more nodes than necessary i.e. if it gets to a root add the first condition and stop
;;;
;;; Input: - current-tree 
;;;        - list of cr-condition structs valid for constants
;;;               so the type is one of: isa, query, slot, or test-slot
;;;        - production name



         

(defmethod add-to-tree ((node leaf-node) conditions production)
  (if conditions
      (create-branch node (list (car conditions)) production)
    (push-last production (leaf-node-valid node))))


(defmethod add-to-tree ((node root-node) conditions production)
  
  (push-last production (conflict-node-valid node))
  
  (aif (root-node-child node)
       (add-to-tree it conditions production)
       (let ((new-node (make-leaf-node :parent node :branch t :valid nil)))
         (setf (root-node-child node) new-node)
         (add-to-tree new-node conditions production))))
  

(defmethod add-to-tree ((node isa-node) conditions production)
         
  (aif (find (isa-node-condition node) conditions :test 'cr-condition-equal)
       ;; then it's a true test
       (add-to-tree (isa-node-true node) (remove it conditions) production)
       ;; check if there's some other test of the type on this buffer
       (let* ((current (isa-node-condition node))
              (other-types (mapcar #'cr-condition-value (remove-if-not (lambda (a) 
                                                                        (and (eq (cr-condition-type a) (cr-condition-type current))
                                                                             (eq (cr-condition-buffer a) (cr-condition-buffer current))))
                                                                      conditions))))
         
        ; (format t "Other-types are: ~S~%" other-types)
         
         (if (and other-types (notany (lambda (x) (chunk-type-subtype-p-fct (cr-condition-value current) x)) other-types))
             ;; there is a mismatch so progress down the false branch
             ;; requires that all possible tests fail to be safe
             (add-to-tree (isa-node-false node) conditions production)
           ;; otherwise add it to both branches - if there's any possibility it could match
           (progn
             (add-to-tree (isa-node-true node) conditions production)
             (add-to-tree (isa-node-false node) conditions production))))))


(defmethod add-to-tree ((node binary-test-node) conditions production) ;; query and test-slot
  
  (aif (find (binary-test-node-condition node) conditions :test 'cr-condition-equal)
       ;; this test matches so progress down the proper branch
       (add-to-tree (if (cr-condition-result it) 
                        (binary-test-node-true node) 
                      (binary-test-node-false node))
                    (remove it conditions) production)

        ;; otherwise add it to both branches
         (progn
           (add-to-tree (binary-test-node-true node) conditions production)
           (add-to-tree (binary-test-node-false node) conditions production))))


(defmethod add-to-tree ((node slot-node) conditions production) 

 (aif (find (slot-node-condition node) conditions :test 'cr-condition-equal)
      (if (gethash (cr-condition-value it) (slot-node-children node))
           ;; push it down the existing branch
          (add-to-tree (gethash (cr-condition-value it) (slot-node-children node)) (remove it conditions) production)
          ;; doesn't have a branch so create one then add it
        (let ((default (copy-conflict-tree (gethash :other (slot-node-children node)) node)))
          (setf (gethash (cr-condition-value it) (slot-node-children node)) default)
          (setf (conflict-node-branch default) (cr-condition-value it))
          (add-to-tree default (remove it conditions) production)))
      ;; otherwise add it to every branch
      (maphash (lambda (key value)
                 (add-to-tree value conditions production))
               (slot-node-children node))))



(defmethod create-branch ((node leaf-node) conditions production)
  ;(format t "create-branch ~20s from ~20s~%" branch (type-of node))
  (let* ((condition (car conditions))
         (new-node (case (cr-condition-type condition)
                     (isa 
                      (make-isa-node :parent (conflict-node-parent node)
                                     :branch (conflict-node-branch node)
                                     :buffer (cr-condition-buffer condition)
                                     :buffer-index (cr-condition-bi condition)
                                     :value (cr-condition-value condition)
                                     :condition condition))
                     (query
                      (make-query-node :parent (conflict-node-parent node)
                                       :branch (conflict-node-branch node)
                                       :buffer (cr-condition-buffer condition)
                                       :query  (cr-condition-slot condition)
                                       :value (cr-condition-value condition)
                                       :condition condition))
                     (test-slot
                      (make-test-slot-node :parent (conflict-node-parent node)
                                           :branch (conflict-node-branch node)
                                           :buffer (cr-condition-buffer condition)
                                           :buffer-index (cr-condition-bi condition)
                                           :slot (cr-condition-slot condition)
                                           :slot-index (cr-condition-si condition)
                                           :value (cr-condition-value condition)
                                           :test (cr-condition-test condition)
                                           :condition condition))
                     (slot
                      (make-slot-node :parent (conflict-node-parent node)
                                      :branch (conflict-node-branch node)
                                      :buffer (cr-condition-buffer condition)
                                      :buffer-index (cr-condition-bi condition)
                                      :slot (cr-condition-slot condition)
                                      :slot-index (cr-condition-si condition)
                                      :condition condition)))))
    
    (if (binary-test-node-p new-node)
        (progn
          (setf (binary-test-node-true new-node) (make-leaf-node :parent new-node :branch t :valid (copy-list (conflict-node-valid node))))
          (setf (binary-test-node-false new-node) (make-leaf-node :parent new-node :branch nil :valid (copy-list (conflict-node-valid node))))
          )
      (progn
        (let ((other-tree (make-leaf-node :parent new-node :branch :other :valid (copy-list (conflict-node-valid node)))))
          (setf (gethash :other (wide-test-node-children new-node)) other-tree))))
    
    
    ;; splice this in as the child of the leaf's parent

    (cond ((binary-test-node-p (conflict-node-parent node))
           (if (conflict-node-branch node)
               (setf (binary-test-node-true (conflict-node-parent node)) new-node)
             (setf (binary-test-node-false (conflict-node-parent node)) new-node)))
          ((root-node-p (conflict-node-parent node))
           (setf (root-node-child (conflict-node-parent node)) new-node))
          ((wide-test-node-p (conflict-node-parent node))
           (setf (gethash (conflict-node-branch new-node) (wide-test-node-children (conflict-node-parent node))) new-node)))
                               
    ;; now call the add code for this new-node
    
    (add-to-tree new-node conditions production)))



;;; Code to copy a tree making sure not to save things that shouldn't be saved

(defmethod copy-conflict-tree ((node leaf-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    new-node))


(defmethod copy-conflict-tree ((node root-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (awhen (root-node-child node)
           (setf (root-node-child new-node) (copy-conflict-tree it new-node)))
    new-node))


(defmethod copy-conflict-tree ((node binary-test-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (awhen (binary-test-node-true node)
           (setf (binary-test-node-true new-node) (copy-conflict-tree it new-node)))
    (awhen (binary-test-node-false node)
           (setf (binary-test-node-false new-node) (copy-conflict-tree it new-node)))
    new-node))

(defmethod copy-conflict-tree ((node wide-test-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (setf (wide-test-node-children new-node) (make-hash-table :test 'equalp :size (hash-table-size (wide-test-node-children node))))
    (maphash (lambda (key value)
               (setf (gethash key (wide-test-node-children new-node)) (copy-conflict-tree value new-node)))
             (wide-test-node-children node))
    new-node))

;;;

(defun tree-condition-equal (a b)
  (and (eq (cr-condition-type a) (cr-condition-type b))
       (eq (cr-condition-buffer a) (cr-condition-buffer b))
       (case (cr-condition-type a)
         (isa t)
         (slot (= (cr-condition-si a) (cr-condition-si b)))
         (query (and (eq (cr-condition-slot a) (cr-condition-slot b))
                     (eql (cr-condition-value a) (cr-condition-value b))))
         (test-slot (and (eq (cr-condition-test a) (cr-condition-test b))
                         (= (cr-condition-si a) (cr-condition-si b))
                         (eql (cr-condition-value a) (cr-condition-value b))))
         (t nil))))



(defun split-productions-with-condition (c conditions)
  (let ((vals
    (cond ((eq 'slot (cr-condition-type c))
           (let* ((r1 (mapcan (lambda (x) (copy-list (append (second x) (third x)))) conditions))
                  (r2 (remove-if-not (lambda (x) (tree-condition-equal c x)) r1))
                  (r3 (remove-duplicates (mapcar 'cr-condition-value r2) :test 'equalp))
                  (r4 (cons :other r3))
                  (results (mapcar (lambda (x) (list x nil)) r4)))
             
            ; (format t "Results are:  r1:~S~%r2:~S~%r3:~S~%r4:~S~%r5:~S~%" r1 r2 r3 r4 results)
           ;  (break)
             
             (dolist (x conditions)
               (aif (find c (append (second x) (third x)) :test 'cr-condition-equal)
                    (push-last (list (first x) (remove c (second x) :test 'cr-condition-equal) (remove c (third x) :test 'cr-condition-equal))
                          (second (find (cr-condition-value it) results :key #'car :test #'equalp)))
                    (dolist (y results)
                      (push-last x (second y)))))
             results))
          ((eq 'isa (cr-condition-type c))
           (let ((results (list (list t nil) (list nil nil))))
             (dolist (x conditions)
               (aif (find c (append (second x) (third x)) :test 'cr-condition-equal)
                    ;; then it's a true test
                    (push-last (list (first x) (remove c (second x) :test 'cr-condition-equal) (remove c (third x) :test 'cr-condition-equal))
                              (second (first results)))
                    ;; check if there's some other test of the type on this buffer
                    (let ((other-types (mapcar #'cr-condition-value (remove-if-not (lambda (y) (tree-condition-equal y c)) (append (second x) (third x))))))
                      
                      (if (and other-types (notany (lambda (y) (chunk-type-subtype-p-fct (cr-condition-value c) y)) other-types))
                          ;; there is a mismatch so progress down the false branch
                          ;; requires that all possible tests fail to be safe
                          (push-last x (second (second results)))
                        ;; otherwise add it to both branches - if there's any possibility it could match
                        (progn
                          (push-last x (second (first results)))
                          (push-last x (second (second results))))))))
             results))
          
          (t ; slot-test and queries are easier
           
           (let ((results (list (list t nil) (list nil nil))))
             (dolist (x conditions)
               (aif (find c (append (second x) (third x)) :test 'cr-condition-equal)
                    (if (cr-condition-result it)
                        (push-last (list (first x) (remove c (second x) :test 'cr-condition-equal) (remove c (third x) :test 'cr-condition-equal))
                              (second (first results)))
                      (push-last (list (first x) (remove c (second x) :test 'cr-condition-equal) (remove c (third x) :test 'cr-condition-equal))
                            (second (second results))))
                    (progn
                      (push-last x (second (first results)))
                      (push-last x (second (second results))))))
             results)))))
    
    (values (- (log (length conditions) 2) (score-tree-cases vals (length conditions))) vals)))

(defun score-tree-cases (vals s)
  (reduce #'+ (mapcar (lambda (x) (if (null (second x)) 0 (* (/ (length (second x)) s) (log (length (second x)) 2)))) vals) :initial-value 0))

(defun build-tree-from-productions (branch parent conditions negative)
  (let* ((constants (mapcan (lambda (x) (copy-list (second x))) conditions))
         (valid-conditions (remove-duplicates constants :test #'tree-condition-equal)))
    
 ;   (format t "Branch: ~s (~S)~%Constants: ~S~%Valid-conditions are: ~S~%" branch (mapcar #'car conditions) constants valid-conditions)
 ;   (break)
    
    (if (or (null valid-conditions) (= (length conditions) 1))
        (make-leaf-node :parent parent :branch branch :valid (mapcar #'first conditions))
     (let ((best nil)
           (val nil)
           (groups nil)
           (all-same t)
           (last nil))
       (dolist (x valid-conditions)
         (multiple-value-bind (v g) (split-productions-with-condition x conditions)
           
           ;(format t "~S: ~S~%" v x)
           
           (unless (or (null last) (= last v))
             (setf all-same nil))
           
           (setf last v)
           
           (when (or (null val)
                     (> v val))
             (setf val v)
             (setf groups g)
             (setf best x))))
       
       ; (format t "Best(~3s): ~S ~S ~%~%" all-same val best)
       
       
       (if (or (and negative (minusp val) (> negative 3)) ;; Only make a few negative splits
               (and all-same (<= val 0.0))) ;; doesn't seem like any improvements left
           
           (make-leaf-node :parent parent :branch branch :valid (mapcar #'first conditions))
       
       (let ((new-node (case (cr-condition-type best)
                         (isa 
                          (make-isa-node :parent parent
                                         :branch branch
                                         :buffer (cr-condition-buffer best)
                                         :buffer-index (cr-condition-bi best)
                                         :value (cr-condition-value best)
                                         :condition best))
                         (query
                          (make-query-node :parent parent
                                           :branch branch
                                           :buffer (cr-condition-buffer best)
                                           :query  (cr-condition-slot best)
                                           :value (cr-condition-value best)
                                           :condition best))
                         (test-slot
                          (make-test-slot-node :parent parent
                                               :branch branch
                                               :buffer (cr-condition-buffer best)
                                               :buffer-index (cr-condition-bi best)
                                               :slot (cr-condition-slot best)
                                               :slot-index (cr-condition-si best)
                                               :value (cr-condition-value best)
                                               :test (cr-condition-test best)
                                               :condition best))
                         (slot
                          (make-slot-node :parent parent
                                          :branch branch
                                          :buffer (cr-condition-buffer best)
                                          :buffer-index (cr-condition-bi best)
                                          :slot (cr-condition-slot best)
                                          :slot-index (cr-condition-si best)
                                          :condition best)))))
    
         (if (binary-test-node-p new-node)
             (progn
               (setf (binary-test-node-true new-node) 
                 (build-tree-from-productions t new-node (second (first groups)) (if (minusp val) 
                                                                                     (if negative (1+ negative) 0)
                                                                                   0)))
               
               
               (setf (binary-test-node-false new-node) (build-tree-from-productions nil new-node (second (second groups)) (if (minusp val) 
                                                                                                                          (if negative (1+ negative) 0)
                                                                                                                        0))))
           (progn
             (dolist (x groups)
               (setf (gethash (first x) (wide-test-node-children new-node))
                 (build-tree-from-productions (first x) new-node (second x) (if (minusp val) 
                                                                                (if negative (1+ negative) 0)
                                                                              0))))))
         new-node))))))
    

;;; Interface to the procedural module

(defun add-production-to-tree (p procedural)
  (add-to-tree (procedural-conflict-tree procedural) 
               (remove-duplicates (append (production-constants p) (production-implicit p)) :test 'cr-condition-equal) 
               (production-name p)))

(defun remove-production-from-tree (p procedural)
  (model-warning "Remove productions not recommended when :conflict-tree is set to t - tree removal not implemented."))

(defun build-conflict-tree (procedural)
  
  #| simple - production at a time method
     bad idea in general for multiple reasons
     
  (dolist (p (productions-list procedural))
    (add-production-to-tree p procedural)
    (conflict-tree-stats ))
  |#
  
  (setf (root-node-valid (procedural-conflict-tree procedural)) (mapcar #'production-name (productions-list procedural)))
  (setf (root-node-child (procedural-conflict-tree procedural))
    (build-tree-from-productions t (procedural-conflict-tree procedural) 
                                 (mapcar (lambda (x) (list (production-name x) (copy-list (production-constants x)) (copy-list (production-implicit x))))
                                   (productions-list procedural))
                                 nil))
  )

(defun get-valid-productions (procedural)
  (get-valid (procedural-conflict-tree procedural) procedural))


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
