;;;;
;;;;     Title: A sample sokoban implementation using CLX
;;;;   Created: Tue Feb 27 15:43:28 1996
;;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;; Copyright: (c) copyright 1996 by Gilbert Baumann, distributed under GPL.

;;;; History
;;;;
;;;; This is inspirited by some Sokoban implementation for UNIX. (Which I do not
;;;; any longer have, and from which I do not know the originator anymore,
;;;; sorry.) As I was working on a Windows NT system [No,No!  Not for fun, to
;;;; earn money; I consider it important to mention that!]  I implemented a
;;;; Windows version of sokoban using the orginal screens and pixmaps from the
;;;; UNIX version. However now I wanted to reimplemenet this version in LISP,
;;;; just to show, that it *is* possible to implement rather fast reacting X11
;;;; applications in LISP and to bypass the long recompilation times of CLISP
;;;; (30-45 mins on my system).
;;;;
;;;; The pixmaps and screens are from the original X11 sokoban. But I found the
;;;; same screens on many other implementations. Also some ancient PC implementation,
;;;; which uses CGA graphics (The first one -- perhaps not, the idea of sokoban
;;;; is too good to be invented on an PC) The pixmaps seem to be unique to the
;;;; X11 sokoban I talked about. Could anybody please help me out to find the
;;;; source to be able to acknowledge the author of the pixmaps? Also if anybody
;;;; has some information on the history of this game please tell me.
;;;;
;;;; BTW -- The numerical notation of field values is indeed just copied from
;;;; the Windows version, as far as I could recall the original sokoban
;;;; implemention uses these too, but I am not sure. This makes this code look
;;;; like MacLISP, doesn't it?

;;;; Todos
;;;;  - mouse actions
;;;;  - high score

;;;; Bugs
;;;;  - does only run on colour screens. Not realy but is unusable on b/w unless
;;;;    there a b/w pixmaps.
;;;;  - maximum field size is hard wired to 20x20. (This is not in the LISP spirit!)
;;;;  - sometimes the programm could not count correctly ...

(in-package :clx-demos)

;;;; First a lot of global variables ...
(defvar *pixmaps* nil)                  ;array of pixmaps according to below indices
(defvar *field* nil)                    ;the field
(defvar *changes* nil)                  ;A map of T/NIL saying, which field changed
(defvar *display* nil)                  ;The connection to the X server
(defvar *window* nil)                   ;The window sokoban is living in
(defvar *gcontext* nil)                 ;Graphics context used by sokoban
(defvar *man-x* nil)                    ;X position of the man
(defvar *man-y* nil)                    ;Y position of the man (actually a nose)
(defvar *undos* nil)                    ;undo information
(defvar *level* 0)                      ;current level
(defvar *n-objects* 0)                  ;number of balls to be saved
(defvar *said-congrat-p* nil)
(defvar *said-proceed-p* nil)
(defvar *findmap* nil)

(defvar *sokoban-state-file* '#"~/.sokoban-state.lisp") ;Change if you want
(defvar *state-vars*
  '(*man-x* *man-y* *undos* *field* *level* *n-objects*))

(defvar *pixmap-names*
  '("lonewall" "southwall" "westwall" "llcorner"
    "northwall" "vertiwall" "ulcorner" "west_twall"
    "eastwall" "lrcorner" "horizwall" "south_twall"
    "urcorner" "east_twall" "north_twall" "centerwall"
    "object" "treasure"
    "floor" "goal"
    "man_down" "save_man_down"))

(defvar *man-pixmap-names*
  '("man_left"  "save_man_left"
    "man_right" "save_man_right"
    "man_up"    "save_man_up"
    "man_down"  "save_man_down"))

(defvar *man-pixmaps* nil)
(defvar *man-direction* 0)

;; I am afraid that I am better at compiler hacking than at pathname hacking ...
(defvar *xpm-directory*
  (make-pathname :name nil :type nil :defaults
                 (merge-pathnames #p"xpms/" *load-truename*)))
(defvar *screen-directory*
  (make-pathname :name nil :type nil :defaults
                 (merge-pathnames '#"screens/" *load-truename*)))

;; BTW - This is my personal style to write enum constants with %-prefix notation.
(defvar %object 16)
(defvar %treasure 17)
(defvar %floor 18)
(defvar %goal 19)
(defvar %man 20)
(defvar %saveman 21)
(defvar %badmove (* 20 21))

(defun field (x y)
  "Retrieves the field x/y"
  (cond ((and (<= 0 x 19) (<= y 19)) (aref *field* x y))
        (t %floor)))                    ;fake entry

(defun (setf field) (value x y)
  (setf (aref *field* x y) value)
  (setf (aref *changes* x y) T)
  value)

(defun load-screen (level)
  "Loads the screen belonging to level 'level'."
  (with-open-file (in (merge-pathnames (format nil "screen.~D" level) *screen-directory*))
    (do ((l (read-line in nil nil) (read-line in nil nil))
         (r nil (cons l r)))
        ((null l) (nreverse r))) ))

(defun find-outers ()
  "Goes thru' the board and finds all fields which are outside and sets the shape mask accordingly"
  (let ((map (make-array '(20 20)))
        (maxx 0)
        (maxy 0))
    (labels ((ff (x y)
                 (unless (or (aref map x y) (< (field x y) %object))
                   (setf (aref map x y) T)
                   (ff (1+ x) y) (ff (1- x) y) (ff x (1- y)) (ff x (1+ y)))))
      (ff *man-x* *man-y*)
      (let ((*rects* nil))
        (dotimes (x 20)
          (dotimes (y 20)
            (unless (and (= (field x y) %floor) (not (aref map x y)))
              (setq maxx (max x maxx)
                    maxy (max y maxy)
                    *rects* (list* (* x 40) (* y 40) 40 40 *rects*)))))
        (setf (xlib:drawable-width *window*)  (* 40 (1+ maxx))
              (xlib:drawable-height *window*) (* 40 (1+ maxy)))
        (xlib:shape-combine *window* *rects*))))
  (xlib:display-force-output *display*))

(defun init-field (&optional (level *level*))
  "Does all initialisation work needed when going to a different level."
  (let ((screen (load-screen level)))
    (format T "~%;This is the ~:R level." level)
    (setf (xlib:wm-name *window*) (format nil "Sokoban - ~:(~:R~) Level" *level*))
    (labels ((screen-field (x y)
                           (if (<= 0 y (1- (length screen)))
                               (if (<= 0 x (1- (length (nth y screen))))
                                   (aref (nth y screen) x)
                                 #\Space)
                             #\Space))
             (is-wall (ch) (char= ch #\#)))
      (setq *field* (make-array '(20 20))
            *undos* nil
            *n-objects* 0
            *said-congrat-p* nil)
      (dotimes (x 20)
        (dotimes (y 20)
          (setf (field x y)
                (ecase (screen-field x y)
                       (#\Space %floor)
                       (#\$ (incf *n-objects* 1) %object)
                       (#\@ (setf *man-x* x *man-y* y) %man)
                       (#\. %goal)
                       (#\* %treasure)
                       (#\#
                        (let ((val 0))
                          (when (is-wall (screen-field x (1- y))) (incf val 1))
                          (when (is-wall (screen-field (1+ x) y)) (incf val 2))
                          (when (is-wall (screen-field x (1+ y))) (incf val 4))
                          (when (is-wall (screen-field (1- x) y)) (incf val 8))
                          val)) )))) )
    (find-outers)))

(defun init-sokoban ()
  "Initialized the whole beast, opens display, creates window ..."
  (setq *display* (x-open-display))
  (let* ((root-window (xlib:screen-root (car (xlib:display-roots *display*))))
         (make-pixmap (lambda (name)
                        (xpm::read-file-to-pixmap root-window
                          (make-pathname :name name :type "xpm"
                                         :defaults *xpm-directory*)))))
    (setq *changes* (make-array '(20 20))
          *pixmaps* (map 'vector make-pixmap *pixmap-names*)
          *man-pixmaps* (map 'vector make-pixmap *man-pixmap-names*)
          *window* (xlib:create-window :parent root-window :x 0 :y 0
                                       :width 400 :height 400
                                       :background (aref *pixmaps* %floor)
                                       :event-mask '(:exposure :button-press
                                                     :button-release
                                                     :key-press :key-release))
          *gcontext* (xlib:create-gcontext :drawable *window*))
    (setf (xlib:wm-icon-name *window*) "Sokoban")
    (xlib:map-window *window*)
    (load-state)
    (find-outers)))

(defun direction-index (dx dy)
  (cond ((minusp dx) 0)                 ;left
        ((plusp dx)  1)                 ;right
        ((minusp dy) 2)                 ;up
        ((plusp dy)  3)                 ;down
        (t 0)))                         ;safety

(defun update-direction (dx dy)
  (let ((di (direction-index dx dy)))
    (setf (aref *pixmaps* %man) (aref *man-pixmaps* (* di 2))
          (aref *pixmaps* %saveman) (aref *man-pixmaps* (1+ (* di 2))))))

(defun ready-p ()
  (zerop *n-objects*))

(defun move (dx dy)
  (let ((nx (+ *man-x* dx))
        (ny (+ *man-y* dy)))
    (cond ((>= (field nx ny) %floor)
           (decf (field *man-x* *man-y*) 2)
           (incf *man-x* dx)
           (incf *man-y* dy)
           (incf (field *man-x* *man-y*) 2)
           (push (list dx dy 2 nil 2) *undos*)
           (update-direction dx dy))
          ((>= (field nx ny) %object)
           (let ((nnx (+ nx dx))
                 (nny (+ ny dy)))
             (when (>= (field nnx nny) %floor)
               ;;Ok its legal ...
               (when (and (= (field nx ny) %object)
                          (= (field nnx nny) %goal))
                 (decf *n-objects*))
               (incf (field nx ny) 4)   ;remove object and add man
               (decf (field nnx nny) 2) ;add object
               (decf (field *man-x* *man-y*) 2) ;remove man
               (setf *man-x* nx *man-y* ny)
               (push (list dx dy 4 2 2) *undos*)
               (update-direction dx dy))))
          (t
           ;;Illegal. Should we make some annoying noise here?
           ;;But I hate programs, which make too much noise.
           ) )))

(defun undo ()
  (let ((ui (pop *undos*)))
    (cond ((eq ui :barrier)
           (do () ((eq (car *undos*) :barrier)) (undo))
           (pop *undos*))
          (t
           (when ui
             (when (cadddr ui);push-p?
               (when (and (= (field *man-x* *man-y*) %man)
                          (= (field (+ *man-x* (car ui)) (+ *man-y* (cadr ui))) %treasure))
                 (incf *n-objects*)) )
             (decf (field *man-x* *man-y*) (caddr ui))
             (when (cadddr ui)
               (incf (field (+ *man-x* (car ui)) (+ *man-y* (cadr ui))) (cadddr ui)))
             (incf (field (- *man-x* (car ui)) (- *man-y* (cadr ui))) (car (cddddr ui)))
             (decf *man-x* (car ui))
             (decf *man-y* (cadr ui))
             (update-direction (- (car ui)) (- (cadr ui))) )))))

(defun undo-til-push ()
  (let (ui)
    (loop
     (setq ui (car *undos*))
     (undo)
     (update)
     (xlib:display-finish-output *display*)
     (when (or (null *undos*) (cadddr ui))
       (return)))))

(defun restart-sokoban ()
  (do ()
      ((null *undos*))
    (undo)
    (update)
    (xlib:display-finish-output *display*)))

(defun stats ()
  (format T "~%; Statistics: You made ~R moves so far.
;             This is ~:R level.
;             There ~[are~;is~:;are~] ~:*~R ball~:*~P remaining to be goaled."
          (length *undos*) *level* *n-objects*))

(defun valid-p (x y) (<= 0 x 19) (<= 0 y 19))

(defun find-target (x y pathlen)
  (cond ((not (valid-p x y)))   ; we escaped into space
        ((< (field x y) %floor)) ; we could not walk here
        ((<= (aref *findmap* x y) pathlen)) ; there is already some better way
        (t
         (setf (aref *findmap* x y) pathlen)
         (cond ((and (= x *man-x*) (= y *man-y*))) ;we reached our goal!
               (t
                (find-target (1- x) y (1+ pathlen))
                (find-target (1+ x) y (1+ pathlen))
                (find-target x (1- y) (1+ pathlen))
                (find-target x (1+ y) (1+ pathlen))))) ))

(defun walk-to (sx sy)
  (let ((x (floor sx 40))
        (y (floor sy 40)))
    (setq *findmap* (make-array '(20 20) :initial-element %badmove))
    ;;Flood fill search to find a shortest path to the push point.
    (find-target x y 0)
    (cond ((= (aref *findmap* *man-x* *man-y*) %badmove)
           ;; if we didn't make it back to the players position,
           ;; there is no valid path to that place.
           nil)
          (t ;; We made it back, so let's walk the path we just built up
           (push :barrier *undos*)
           (let ((cx *man-x*) (cy *man-y*))
             (do ()
                 ((zerop (aref *findmap* cx cy))
                  (push :barrier *undos*)
                  t)
               (cond ((= (aref *findmap* (1- cx) cy)
                         (1- (aref *findmap* cx cy)))
                      (decf cx)
                      (move -1 0))
                     ((= (aref *findmap* (1+ cx) cy)
                         (1- (aref *findmap* cx cy)))
                      (incf cx)
                      (move 1 0))
                     ((= (aref *findmap* cx (1- cy))
                         (1- (aref *findmap* cx cy)))
                      (decf cy)
                      (move 0 -1))
                     ((= (aref *findmap* cx (1+ cy))
                         (1- (aref *findmap* cx cy)))
                      (incf cy)
                      (move 0 1))
                     (t ;;If we get here, something is SERIOUSLY wrong,
                      ;; so we should abort
                      (error "Ups!")) )
               (update)
               (xlib:display-finish-output *display*)))))))

(defun sokoban-usage ()
  "Print a short description on how to play sokoban."
  (format T "

Object of the game is to move all balls into the striped area. Use the cursor
keys to move the man (err the nose). You could push the balls around, but only
one ball at a time. (The nose is not strong enaugh to move more balls).

Recover from mistakes:
  u -- undo one move
       [u wie \"Ungeschehen\" wie meine Schwester, die kleinere, immer sagt.]
  v -- undo til the last push made. (push = moving a ball)
  r -- restart the current level

Proceeding:
  n -- proceed to next level, you must have saved all balls to do this
  f -- cheat! goto next level unconditionally

Other keys:
  q -- quit sokoban; the current state will be saved
  k -- kill sokoban state will *not* be saved
  a -- print current statistics
  h -- show this brief guide again
  d -- toggle debugging information

If you quit sokoban using 'q' the current state will be saved in
~A and recovered next time you play sokoban."
       *sokoban-state-file*))

(defvar *sokoban-debug* t)

(defun sokoban ()
  (when (or (null *display*) (closed-display-p *display*))
    (init-sokoban)
    (sokoban-usage))
  (block event-loop
    (xlib:event-case (*display*)
      (:button-press (code window x y)
        (case code
          (1 (walk-to x y) nil)
          (3 (undo) (update) nil)
          (otherwise
           (when *sokoban-debug*
             (format t "~&; ~s: (~s ~s ~s ~s)~%"
                     :button-press code window x y)))))
      (:key-press (code window)
        (case (xlib:keycode->keysym *display* code 0)
          (65361 #|LEFT|#  (move -1 0))
          (65362 #|UP|#    (move 0 -1))
          (65363 #|RIGHT|# (move 1 0))
          (65364 #|DOWN|#  (move 0 1))
          (#o165 #|u|# (undo))
          (#o166 #|v|# (undo-til-push))
          (#o162 #|r|# (restart-sokoban))
          (#o163 #|s|# (save-state))
          (#o161 #|q|# (return-from event-loop t))
          (#o153 #|k|# (return-from sokoban 'killed))
          (#o141 #|a|# (stats))
          (#o144 #|d|# (setq *sokoban-debug* (not *sokoban-debug*)))
          (#o150 #|h|# (sokoban-usage))
          (#o156 #|n|#
           (cond ((ready-p)
                  (incf *level*)
                  (init-field))
                 (T
                  (format T "~%;You are not yet ready! (consider restart with `r'.)"))))
          (#o146 #|f|# ;force
           (incf *level*)
           (init-field))
          (otherwise
           (when *sokoban-debug*
             (format t "~&; ~s: ~s ~s/~s~%" :key-press window code
                     (xlib:keycode->keysym *display* code 0)))))
        (update)
        (when (ready-p)
          (unless *said-congrat-p*
            (format T "~%; Congratulations! -- you are ready.~A" (code-char 7))
            (format T "~%; Statistics: You needed ~R moves." (length *undos*))
            (setq *said-congrat-p* t))
          (unless *said-proceed-p*
            (format T "~%; Proceed to next move with 'n'.")
            (setq *said-proceed-p* t)) )
        nil)
      (:exposure (x y width height count)
        (when *sokoban-debug*
          (format t "~&; ~s: (~s ~s ~s ~s ~s)~&" :exposure
                  x y width height count))
        (when (= count 0) (update t))
        nil)))
  (save-state)
  (xlib:unmap-window *window*)
  (xlib:display-finish-output *display*)
  (xlib:close-display *display*))

(defun update (&optional all-p)
  (dotimes (x 20)
    (dotimes (y 20)
      (let ((changed-p (aref *changes* x y))
            (value (aref *field* x y)))
        (when (and (or all-p changed-p)
                   (or (/= value %floor) changed-p))
          (setf (aref *changes* x y) nil)
          (xlib:copy-area (aref *pixmaps* value)
                          *gcontext* 0 0 40 40 *window* (* x 40) (* y 40)))))))

(defun save-state ()
  (with-open-file (o *sokoban-state-file* :direction :output)
    (with-standard-io-syntax
      (dolist (k *state-vars*)
        (write `(setq ,k ',(symbol-value k)) :stream o)
        (terpri o))))
  (format T "~%; Saved state"))

(defun load-state ()
  (cond ((probe-file *sokoban-state-file*)
         (format T "~%;Retrieving old state")
         (with-standard-io-syntax
           (load *sokoban-state-file*))
         (format T "~%;This is the ~:R level, you have made already ~R move~P."
                 *level* (length *undos*) (length *undos*))
         (setf (xlib:wm-name *window*)
               (format nil "Sokoban - ~:(~:R~) Level" *level*)))
        (t
         (format T "~%;You are beginning fresh.")
         (setq *level* 1)
         (init-field))) )

;; These functions should realy been compiled:
;; '(mapcar #'compile '(init-field ready-p update find-outers field find-target walk-to))

(format t "~& Call (clx-demos:sokoban).~%")

(provide "sokoban")
