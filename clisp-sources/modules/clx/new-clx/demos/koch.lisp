;;; Draw Koch snowflake
;;;
;;; Copyright (C) 2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: koch.lisp,v 1.1 2005/09/28 22:23:24 sds Exp $
;;; $Source: /cvsroot/clisp/clisp/modules/clx/new-clx/demos/koch.lisp,v $

(in-package :clx-demos)

(defun koch-point (cx width/2 height/2 scale)
  (list (round (+ width/2 (* scale width/2 (realpart cx))))
        (round (+ height/2 (* scale height/2 (imagpart cx))))))

;; this assumes clockwize traversal
(defun koch-new-points (x1 y1 x5 y5)
  (let* ((vx (round (- x5 x1) 3)) (vy (round (- y5 y1) 3))
         (x2 (+ x1 vx)) (y2 (+ y1 vy))
         (x4 (- x5 vx)) (y4 (- y5 vy))
         (cx (* (complex vx vy) #.(cis (/ pi -3))))
         (x3 (round (+ x2 (realpart cx))))
         (y3 (round (+ y2 (imagpart cx)))))
    (and (or (/= x1 x2) (/= y1 y2))
         (or (/= x2 x3) (/= y2 y3))
         (or (/= x3 x4) (/= y3 y4))
         (or (/= x4 x5) (/= y4 y5))
         (list x2 y2 x3 y3 x4 y4))))

(defun koch-update (list)
  "Update the list of points.
Returns the new list and an indicator of whether we are done or not."
  (let ((len (length list)))
    (when (= len 3)               ; init
      (return-from koch-update
        (values (let ((width/2 (/ (first list) 2))
                      (height/2 (/ (second list) 2))
                      (scale (third list)))
                  (nconc (koch-point #C(0 -1) width/2 height/2 scale)
                         (koch-point #.(cis (* pi 1/6)) width/2 height/2 scale)
                         (koch-point #.(cis (* pi 5/6)) width/2 height/2 scale)
                         (koch-point #C(0 -1) width/2 height/2 scale)))
                nil)))
    (do* ((tail list) x1 y1 (x5 (first list)) (y5 (second list))
          (ret (list y5 x5)))
        ((endp (cddr tail)) (values (nreverse ret) nil))
      (setq x1 x5 y1 y5 tail (cddr tail) x5 (first tail) y5 (second tail))
      (let ((new (koch-new-points x1 y1 x5 y5)))
        (unless new (return-from koch-update (values ret t))) ; done
        (setq ret (list* y5 x5 (nreconc new ret)))))))

(defun koch-usage ()
  (format t "~&Usage:~% q - quit~% h - help~%"))

(defun koch-show (name value)
  (format t "~&;; ~A=~S~%" name value))

(defun koch-events (dpy)
  (xlib:event-case (dpy)
    (:button-press (code window x y)
      (format t "~&;; ~S (~S ~S ~S ~S)~%" :button-press code window x y))
    (:key-press (code window)
      (let ((sym (xlib:keycode->keysym dpy code 0)))
        (format t "~&;; ~S (~S ~S ~:D ~:O ~:X)"
                :key-press code window sym sym sym)
        (case sym
          (#o161 #|q|# (return-from koch-events t))
          (#o150 #|h|# (koch-usage)))))
    (:exposure (x y width height count)
      (format t "~&;; ~S: (~S ~S ~S ~S ~S)~%"
              :exposure x y width height count))))

(defun koch (&key (width 1000) (height 1000) (delay 1) (x 10) (y 10)
             (scale 0.8) (font "fixed"))
  (let* ((dpy (x-open-display))
         (screen (car (xlib:display-roots dpy)))
         (root (xlib:screen-root screen))
         (white-pixel (xlib:screen-white-pixel screen))
         (black-pixel (xlib:screen-black-pixel screen))
         (win (xlib:create-window
               :parent root :x x :y y :width width :height height
               :event-mask '(:exposure :button-press :button-release
                             :key-press :key-release)
               :background white-pixel))
         (fnt (xlib:open-font dpy font))
         (gc (xlib:create-gcontext
              :drawable win :font fnt
              :foreground black-pixel :background white-pixel)))
    (koch-show "dpy" dpy) (koch-show "screen" screen) (koch-show "root" root)
    (koch-show "white-pixel" white-pixel) (koch-show "black-pixel" black-pixel)
    (koch-show "win" win) (koch-show "font" fnt) (koch-show "gc" gc)
    (setf (xlib:wm-icon-name win) "Koch Snowflake"
          (xlib:wm-name win) "Koch Snowflake")
    (xlib:map-window win)
    (loop :with done-p :and points = (list width height scale) :and s1 :and s2
      :with previous :for iteration :upfrom 0 :do
      (setf (values points done-p) (koch-update points))
      (when done-p (loop-finish))
      (when previous ; remove old junk
        (setf (xlib:gcontext-foreground gc) white-pixel)
        (xlib:draw-glyphs win gc 30 30 s1)
        (xlib:draw-glyphs win gc 30 50 s2)
        (xlib:draw-lines win gc previous) ; remove old lines
        (setf (xlib:gcontext-foreground gc) black-pixel))
      (setq previous points
            s1 (format nil "iteration: ~:D" iteration)
            s2 (format nil "vertexes: ~:D" (1- (/ (length points) 2))))
      (format t "~&;; ~A; ~A~%" s1 s2)
      (xlib:draw-glyphs win gc 30 30 s1)
      (xlib:draw-glyphs win gc 30 50 s2)
      (xlib:draw-lines win gc points)
      (xlib:display-finish-output dpy)
      (sleep delay))
    (koch-events dpy)
    (xlib:close-font fnt)
    (xlib:unmap-window win)
    (xlib:display-finish-output dpy)
    (xlib:close-display dpy)))

(format t "~& Koch snoflake:~%
  (clx-demos:koch :width :height :delay :x :y :scale :font)
~% Call (clx-demos:koch).~%")

(provide :koch)
