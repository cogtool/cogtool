;;;;
;;;;     Title: The famous swirling vectors using CLX
;;;;   Created: Wed Feb 14 15:51:39 1996
;;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;; Copyright: None, since this code is not worth it.

;;;; -- TODO --
;;;;
;;;; o react on resize events
;;;; o possibly react on iconify events by stoping
;;;; o maybe pressing 'q' should terminate it
;;;; o window documentation line is needed
;;;; o maybe add a root window option
;;;; o or a spline option?!
;;;;

(in-package :clx-demos)

(defvar *offset* 3)
(defvar *delta* 6)

(defun check-bounds (val del max)
  (cond ((< val 0)   (+ (random *delta*) *offset*))
        ((> val max) (- (+ (random *delta*) *offset*)))
        (t           del)))

;; IHMO this is worth to be added to the standard.
(defun make-circular (x) (nconc x x))

(defstruct qix
  lines dims deltas coords)

(defun gen-qix (nlines width height)
  (make-qix :lines  (make-circular (make-list nlines))
            :dims   (list width height width height)
            :deltas (list #3=(+ *offset* (random *delta*)) #3# #3# #3#)
            :coords (list #1=(random width) #2=(random height) #1# #2#) ))

(defun step-qix (qix win gc white-pixel black-pixel)
  (when (car (qix-lines qix))
    (setf (xlib:gcontext-foreground gc) white-pixel)
    (apply #'xlib:draw-line win gc (car (qix-lines qix)))
    (setf (xlib:gcontext-foreground gc) black-pixel))
  (map-into (qix-coords qix) #'+ (qix-coords qix) (qix-deltas qix))
  (map-into (qix-deltas qix) #'check-bounds
            (qix-coords qix) (qix-deltas qix) (qix-dims qix))
  (apply #'xlib:draw-line win gc (qix-coords qix))
  ;; push 'em into
  (unless (car (qix-lines qix)) (setf (car (qix-lines qix)) (make-list 4)))
  (map-into (car (qix-lines qix)) #'identity (qix-coords qix))
  (setf (qix-lines qix) (cdr (qix-lines qix))) )

(defun draw-qix (dpy win gc width height white-pixel black-pixel
                 delay nqixs nlines)
  (let ((qixs nil) (n nlines))
    (dotimes (k nqixs) (push (gen-qix nlines width height) qixs))
    (loop
     (dolist (k qixs)
       (step-qix k win gc white-pixel black-pixel))
     (xlib:display-force-output dpy)
     (sleep delay)
     (decf n)
     (if (<= n 0) (return)))))

(defun qix (&key host display dpy
            (width 400) (height 400) (delay 0.05) (nqixs 3) (nlines 80))
  (unless dpy
    (setf (values host display) (x-host-display)))
  (let* ((dp1 (or dpy (xlib:open-display host :display display)))
         (scr (first (xlib:display-roots dp1)))
         (root-win (xlib:screen-root scr))
         (white-pixel (xlib:screen-white-pixel scr))
         (black-pixel (xlib:screen-black-pixel scr))
         (win (xlib:create-window :parent root-win :x 10 :y 10
                                  :width width :height height
                                  :background white-pixel))
         (gcon (xlib:create-gcontext :drawable win
                                     :foreground black-pixel
                                     :background white-pixel)))
    (xlib:map-window win)
    (xlib:display-finish-output dp1)
    (format t "~&Qix uses the following parameters:~%  :dpy ~s
  :host ~s :display ~s
  :width ~d :height ~d :delay ~f :nqixs ~d :nlines ~d~%"
            dp1 host display width height delay nqixs nlines)
    (draw-qix dp1 win gcon width height white-pixel black-pixel
              delay nqixs nlines)
    (xlib:unmap-window win)
    (xlib:display-finish-output dp1)
    ;;clean-up
    (unless dpy (xlib:close-display dp1))))

;; since we have no herald, simply dump it:
(format t "~& The famous swirling vectors.~%
  (clx-demos:qix :host :display :dpy :width :height :delay :nqixs :nlines)
~% Call (clx-demos:qix) or (clx-demos:qix :delay 0).~%")

(provide "qix")
