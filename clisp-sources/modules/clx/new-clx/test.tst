;; -*- Lisp -*-
;; some tests for clx/new-clx
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "clx/new-clx/test")'

(defparameter *dpy* (show (xlib:open-display ""))) *dpy*

(xlib:closed-display-p *dpy*) NIL

(listp (show (xlib:display-plist *dpy*))) T

(stringp (show (xlib:display-host *dpy*))) T

;(listp (show (multiple-value-list (xlib:pointer-control *dpy*)))) T
(listp (show (xlib:pointer-mapping *dpy*))) T

(multiple-value-bind (kc% b% bp bd lm gar arm) (xlib:keyboard-control *dpy*)
  (show (list kc% b% bp bd lm gar arm) :pretty t)
  (xlib:change-keyboard-control
   *dpy* :KEY-CLICK-PERCENT kc%
   :BELL-PERCENT b% :BELL-PITCH bp :BELL-DURATION bd
   :KEY 80 :AUTO-REPEAT-MODE (if (plusp (aref arm 80)) :on :off)))
NIL

(listp (show (multiple-value-list (xlib:display-keycode-range *dpy*)))) T

(integerp (show (xlib:display-max-request-length *dpy*))) T

(integerp (show (xlib:display-motion-buffer-size *dpy*))) T

(listp (show (xlib:display-pixmap-formats *dpy*) :pretty t)) T

(symbolp (show (xlib:display-byte-order *dpy*))) T

(listp (show (multiple-value-list (xlib:display-protocol-version *dpy*)))) T

(listp (show (multiple-value-list (xlib:display-vendor *dpy*)))) T

(let ((map (show (xlib:keyboard-mapping *dpy*) :pretty t)))
  (show (array-dimensions map))
  (list (eq map (xlib:keyboard-mapping *dpy* :data map))
        (xlib:change-keyboard-mapping
         *dpy* map :first-keycode (xlib:display-min-keycode *dpy*))))
(T NIL)

(let ((modifiers (show (multiple-value-list (xlib:modifier-mapping *dpy*)))))
  (apply #'xlib:set-modifier-mapping *dpy*
         (mapcan #'list '(:SHIFT :LOCK :CONTROL :MOD1 :MOD2 :MOD3 :MOD4 :MOD5)
                 modifiers)))
:SUCCESS

(show (multiple-value-list (xlib:keysym->keycodes *dpy* 65))) (38)
(show (multiple-value-list (xlib:keysym->keycodes *dpy* #xFF52))) (98) ; Up
(show (xlib:keysym "Up")) #xFF52

(show (xlib:keysym-name (show (xlib:keysym "Down")))) "Down"

(let ((access (show (xlib:access-control *dpy*))))
  (assert (eq access (setf (xlib:access-control *dpy*) access)))
  t) T

(listp (show (xlib:access-hosts *dpy*) :pretty t)) T
(xlib:add-access-host *dpy* "localhost") NIL
(listp (show (xlib:access-hosts *dpy*) :pretty t)) T
(xlib:remove-access-host *dpy* "localhost") NIL
(listp (show (xlib:access-hosts *dpy*) :pretty t)) T

(xlib:bell *dpy* 50) NIL        ; signal that we are almost done

(xlib:display-force-output *dpy*) NIL
(xlib:display-finish-output *dpy*) NIL
(xlib:display-p (show (xlib:close-display *dpy*))) T
