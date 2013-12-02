;;;; Copyright: (c) copyright 1996 by Gilbert Baumann, distributed under GPL.
;;;; Some parts are from the MIT-CLX Distribution, copyrighted by
;;;; Texas Instruments Incorporated, but freely distributable
;;;; for details see image.lisp or the MIT-CLX distribution.

(defpackage "XLIB"
  ;; (:use "COMMON-LISP" "CLOS")
  (:import-from "SYS" "STRING-CONCAT"))

(provide "clx")

(in-package :xlib)

(defvar *displays* nil)
(push :clx *features*)
(push :clx-ansi-common-lisp *features*)
(declaim (declaration values))

(defconstant *version* "CLISP-CLX 1997-06-12")


;;;; --------------------------------------------------------------------------
;;;;  Exports
;;;; --------------------------------------------------------------------------
(export
 '(*version* access-control access-error access-hosts activate-screen-saver
   add-access-host add-resource add-to-save-set alist alloc-color
   alloc-color-cells alloc-color-planes alloc-error allow-events angle
   arc-seq array-index atom-error atom-name bell bit-gravity bitmap
   bitmap-format bitmap-format-lsb-first-p bitmap-format-p
   bitmap-format-pad bitmap-format-unit bitmap-image boole-constant boolean
   card16 card29 card32 card8 card8->char change-active-pointer-grab
   change-keyboard-control change-keyboard-mapping change-pointer-control
   change-property char->card8 char-ascent char-attributes char-descent
   char-left-bearing char-right-bearing char-width character->keysyms
   character-in-map-p circulate-window-down circulate-window-up clear-area
   close-display close-down-mode close-font closed-display color color-blue
   color-green color-p color-red color-rgb colormap colormap-display
   colormap-equal colormap-error colormap-id colormap-p colormap-plist
   colormap-visual-info connection-failure convert-selection copy-area
   copy-colormap-and-free copy-gcontext copy-gcontext-components copy-image
   copy-plane create-colormap create-cursor create-gcontext
   create-glyph-cursor create-image create-pixmap create-window cursor
   cursor-display cursor-equal cursor-error cursor-id cursor-p cursor-plist
   cut-buffer declare-event decode-core-error default-error-handler
   default-keysym-index default-keysym-translate define-error
   define-extension define-gcontext-accessor define-keysym
   define-keysym-set delete-property delete-resource destroy-subwindows
   destroy-window device-busy device-event-mask device-event-mask-class
   discard-current-event discard-font-info display display-after-function
   display-authorization-data display-authorization-name
   display-bitmap-format display-byte-order display-default-screen
   display-display display-error-handler display-finish-output
   display-force-output display-host display-image-lsb-first-p
   display-invoke-after-function display-keycode-range display-max-keycode
   display-max-request-length display-min-keycode
   display-motion-buffer-size display-nscreens display-p
   display-pixmap-formats display-plist display-protocol-major-version
   display-protocol-minor-version display-protocol-version
   display-release-number display-report-asynchronous-errors
   display-resource-id-base display-resource-id-mask display-roots
   display-vendor display-vendor-name display-xid draw-arc draw-arcs
   draw-direction draw-glyph draw-glyphs draw-image-glyph draw-image-glyphs
   draw-line draw-lines draw-point draw-points draw-rectangle
   draw-rectangles draw-segments drawable drawable-border-width
   drawable-depth drawable-display drawable-equal drawable-error
   drawable-height drawable-id drawable-p drawable-plist drawable-root
   drawable-width drawable-x drawable-y error-key event-case event-cond
   event-handler event-key event-listen event-mask event-mask-class
   extension-opcode find-atom font font-all-chars-exist-p font-ascent
   font-default-char font-descent font-direction font-display font-equal
   font-error font-id font-max-byte1 font-max-byte2 font-max-char
   font-min-byte1 font-min-byte2 font-min-char font-name font-p font-path
   font-plist font-properties font-property fontable force-gcontext-changes
   free-colormap free-colors free-cursor free-gcontext free-pixmap gcontext
   gcontext-arc-mode gcontext-background gcontext-cache-p
   gcontext-cap-style gcontext-clip-mask gcontext-clip-ordering
   gcontext-clip-x gcontext-clip-y gcontext-dash-offset gcontext-dashes
   gcontext-display gcontext-equal gcontext-error gcontext-exposures
   gcontext-fill-rule gcontext-fill-style gcontext-font gcontext-foreground
   gcontext-function gcontext-id gcontext-join-style gcontext-key
   gcontext-line-style gcontext-line-width gcontext-p gcontext-plane-mask
   gcontext-plist gcontext-stipple gcontext-subwindow-mode gcontext-tile
   gcontext-ts-x gcontext-ts-y get-external-event-code get-image
   get-property get-raw-image get-resource get-search-resource
   get-search-table get-standard-colormap get-wm-class
   global-pointer-position grab-button grab-key grab-keyboard grab-pointer
   grab-server grab-status icon-sizes iconify-window id-choice-error
   illegal-request-error image image-blue-mask image-depth image-green-mask
   image-height image-name image-pixmap image-plist image-red-mask
   image-width image-x image-x-hot image-x-p image-xy image-xy-bitmap-list
   image-xy-p image-y-hot image-z image-z-bits-per-pixel image-z-p
   image-z-pixarray implementation-error input-focus install-colormap
   installed-colormaps int16 int32 int8 intern-atom invalid-font
   keyboard-control keyboard-mapping keycode->character keycode->keysym
   keysym keysym->character keysym->keycodes keysym-in-map-p keysym-set
   kill-client kill-temporary-clients length-error list-extensions
   list-font-names list-fonts list-properties lookup-color lookup-error
   make-color make-event-handlers make-event-keys make-event-mask
   make-resource-database make-state-keys make-state-mask make-wm-hints
   make-wm-size-hints map-resource map-subwindows map-window mapping-notify
   mask16 mask32 match-error max-char-ascent max-char-attributes
   max-char-descent max-char-left-bearing max-char-right-bearing
   max-char-width merge-resources min-char-ascent min-char-attributes
   min-char-descent min-char-left-bearing min-char-right-bearing
   min-char-width missing-parameter modifier-key modifier-mapping
   modifier-mask motion-events name-error no-operation open-display
   open-font pixarray pixel pixmap pixmap-display pixmap-equal pixmap-error
   pixmap-format pixmap-format-bits-per-pixel pixmap-format-depth
   pixmap-format-p pixmap-format-scanline-pad pixmap-id pixmap-p
   pixmap-plist point-seq pointer-control pointer-event-mask
   pointer-event-mask-class pointer-mapping pointer-position process-event
   put-image put-raw-image query-best-cursor query-best-stipple
   query-best-tile query-colors query-extension query-keymap query-pointer
   query-tree queue-event read-bitmap-file read-resources recolor-cursor
   rect-seq remove-access-host remove-from-save-set reparent-window
   repeat-seq reply-length-error reply-timeout request-error
   reset-screen-saver resource-database resource-database-timestamp
   resource-error resource-id resource-key rgb-colormaps rgb-val
   root-resources rotate-cut-buffers rotate-properties screen
   screen-backing-stores screen-black-pixel screen-default-colormap
   screen-depths screen-event-mask-at-open screen-height
   screen-height-in-millimeters screen-max-installed-maps
   screen-min-installed-maps screen-p screen-plist screen-root
   screen-root-depth screen-root-visual screen-root-visual-info
   screen-save-unders-p screen-saver screen-white-pixel screen-width
   screen-width-in-millimeters seg-seq selection-owner send-event
   sequence-error set-input-focus
   set-modifier-mapping  set-screen-saver
   set-standard-colormap set-standard-properties
   set-wm-class set-wm-properties set-wm-resources state-keysym-p
   state-mask-key store-color store-colors stringable text-extents
   text-width timestamp transient-for translate-coordinates
   translate-default translation-function #-cmu type-error undefine-keysym
   unexpected-reply ungrab-button ungrab-key ungrab-keyboard ungrab-pointer
   ungrab-server uninstall-colormap unknown-error unmap-subwindows
   unmap-window value-error visual-info visual-info-bits-per-rgb
   visual-info-blue-mask visual-info-class visual-info-colormap-entries
   visual-info-display visual-info-green-mask visual-info-id visual-info-p
   visual-info-plist visual-info-red-mask warp-pointer
   warp-pointer-if-inside warp-pointer-relative
   warp-pointer-relative-if-inside win-gravity window
   window-all-event-masks window-background window-backing-pixel
   window-backing-planes window-backing-store window-bit-gravity
   window-border window-class window-colormap window-colormap-installed-p
   window-cursor window-display window-do-not-propagate-mask window-equal
   window-error window-event-mask window-gravity window-id window-map-state
   window-override-redirect window-p window-plist window-priority
   window-save-under window-visual window-visual-info with-display
   with-event-queue with-gcontext with-server-grabbed with-state
   withdraw-window wm-client-machine wm-colormap-windows wm-command
   wm-hints wm-hints-flags wm-hints-icon-mask wm-hints-icon-pixmap
   wm-hints-icon-window wm-hints-icon-x wm-hints-icon-y
   wm-hints-initial-state wm-hints-input wm-hints-p wm-hints-window-group
   wm-icon-name wm-name wm-normal-hints wm-protocols wm-resources
   wm-size-hints wm-size-hints-base-height wm-size-hints-base-width
   wm-size-hints-height wm-size-hints-height-inc wm-size-hints-max-aspect
   wm-size-hints-max-height wm-size-hints-max-width
   wm-size-hints-min-aspect wm-size-hints-min-height
   wm-size-hints-min-width wm-size-hints-p
   wm-size-hints-user-specified-position-p
   wm-size-hints-user-specified-size-p wm-size-hints-width
   wm-size-hints-width-inc wm-size-hints-win-gravity wm-size-hints-x
   wm-size-hints-y wm-zoom-hints write-bitmap-file write-resources xatom
   x-error
   keysym-name
   trace-display suspend-display-tracing resume-display-tracing
   untrace-display show-trace
   display-trace ; for backwards compatibility describe-request describe-event describe-reply
   closed-display-p
   describe-error describe-trace))

;;; SHAPE extension
(export '(shape-version shape-combine shape-offset shape-extents shape-rectangles))


;;;; --------------------------------------------------------------------------
;;;;  Types
;;;; --------------------------------------------------------------------------
;;;;
;;;; Lots of deftypes randomly gathers from MIT-CLX implementation
;;;;

(deftype card4 ()       '(unsigned-byte 4))    ;not exported
(deftype card8 ()       '(unsigned-byte 8))
(deftype card16 ()      '(unsigned-byte 16))
(deftype card24 ()      '(unsigned-byte 24))   ;not exported
(deftype card29 ()      '(unsigned-byte 29))
(deftype card32 ()      '(unsigned-byte 32))

(deftype int8 ()        '(signed-byte 8))
(deftype int16 ()       '(signed-byte 16))
(deftype int32 ()       '(signed-byte 32))

(deftype rgb-val ()     '(real 0 1))
(deftype stringable ()  '(or string symbol))
(deftype fontable ()    '(or stringable font))

(deftype array-index () `(integer 0 ,array-dimension-limit))

(deftype angle ()       '(real #.(* -2 pi) #.(* 2 pi)))
(deftype mask32 ()      'card32)
(deftype mask16 ()      'card16)
(deftype pixel ()       '(unsigned-byte 32))
(deftype image-depth () '(integer 0 32))
(deftype resource-id () 'card29)
(deftype keysym ()      'card32)

(deftype alist (key-type-and-name datum-type-and-name)
  (declare (ignore key-type-and-name datum-type-and-name))
  'list)

(deftype repeat-seq (&rest elts) elts 'sequence)
(deftype point-seq () '(repeat-seq (int16 x) (int16 y)))
(deftype seg-seq () '(repeat-seq (int16 x1) (int16 y1) (int16 x2) (int16 y2)))
(deftype rect-seq () '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)))
(deftype arc-seq ()
  '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)
               (angle angle1) (angle angle2)))

(deftype timestamp () '(or null card32))
(deftype bit-gravity () '(member :forget :north-west :north :north-east :west :center :east :south-west :south :south-east :static))

(deftype boole-constant ()
  `(member ,boole-clr ,boole-and ,boole-andc2 ,boole-1
           ,boole-andc1 ,boole-2 ,boole-xor ,boole-ior
           ,boole-nor ,boole-eqv ,boole-c2 ,boole-orc2
           ,boole-c1 ,boole-orc1 ,boole-nand ,boole-set))

(deftype device-event-mask ()
  '(or mask32 list)) ;;  '(or integer (list device-event-mask-class)))

(deftype device-event-mask-class ()
  '(member :key-press :key-release :button-press :button-release :pointer-motion
           :button-1-motion :button-2-motion :button-3-motion :button-4-motion
           :button-5-motion :button-motion))

(deftype draw-direction ()
  '(member :left-to-right :right-to-left))

(deftype error-key ()
  '(member :access :alloc :atom :colormap :cursor :drawable :font :gcontext :id-choice
           :illegal-request :implementation :length :match :name :pixmap :value :window))

(deftype gcontext-key ()
  '(member :function :plane-mask :foreground :background
           :line-width :line-style :cap-style :join-style :fill-style
           :fill-rule :tile :stipple :ts-x :ts-y :font :subwindow-mode
           :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes
           :arc-mode))

(deftype event-key ()
  '(member :key-press :key-release :button-press :button-release :motion-notify
           :enter-notify :leave-notify :focus-in :focus-out :keymap-notify
           :exposure :graphics-exposure :no-exposure :visibility-notify
           :create-notify :destroy-notify :unmap-notify :map-notify :map-request
           :reparent-notify :configure-notify :gravity-notify :resize-request
           :configure-request :circulate-notify :circulate-request :property-notify
           :selection-clear :selection-request :selection-notify
           :colormap-notify :client-message :mapping-notify))

(deftype event-mask-class ()
  '(member :key-press :key-release :owner-grab-button :button-press :button-release
           :enter-window :leave-window :pointer-motion :pointer-motion-hint
           :button-1-motion :button-2-motion :button-3-motion :button-4-motion
           :button-5-motion :button-motion :exposure :visibility-change
           :structure-notify :resize-redirect :substructure-notify :substructure-redirect
           :focus-change :property-change :colormap-change :keymap-state))

(deftype event-mask ()
  '(or mask32 list)) ;; (OR integer (LIST event-mask-class))

(deftype grab-status ()
  '(member :success :already-grabbed :invalid-time :not-viewable))

(deftype modifier-key ()
  '(member :shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5))

(deftype modifier-mask ()
  '(or (member :any) mask16 list)) ;;  '(or (member :any) integer (list modifier-key)))

(deftype state-mask-key ()
  '(or modifier-key (member :button-1 :button-2 :button-3 :button-4 :button-5)))

(deftype translation-function ()
  '(function (sequence array-index array-index (or null font) vector array-index)
             (values array-index (or null int16 font) (or null int32))))

(deftype win-gravity ()
  '(member :unmap :north-west :north :north-east :west :center :east :south-west :south :south-east :static))

(deftype xatom () '(or string symbol))

(deftype pointer-event-mask-class ()
  '(member :button-press :button-release
           :enter-window :leave-window :pointer-motion :pointer-motion-hint
           :button-1-motion :button-2-motion :button-3-motion :button-4-motion
           :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask ()
  '(or mask32 list)) ;;  '(or integer (list pointer-event-mask-class)))


;; ***************************************************************************
;; ****************************** C A U T I O N ******************************
;; ***************************************************************************
;;   THE LAYOUT OF THESE STRUCTURE DEFINITIONS HAS TO BE IN SYNC WITH CLX.D
;; ***************************************************************************

(defstruct bitmap-format unit pad lsb-first-p)
(defstruct pixmap-format depth bits-per-pixel scanline-pad)

(defstruct (color (:constructor make-color-internal (red green blue))
                  (:copier nil))
  ;; Short floats are good enough (no consing)
  (red   0.0s0 :type rgb-val)
  (green 0.0s0 :type rgb-val)
  (blue  0.0s0 :type rgb-val))

(defstruct (visual-info (:copier nil))
  id
  class
  red-mask green-mask blue-mask
  bits-per-rgb
  colormap-entries
  ;; There appears also a plist and a display slot in the MIT-CLX, but not in the manual
  ;; To what should we be compatible?!
  ;; plist display
  )

(defstruct (display (:predicate nil)
                    (:constructor nil)
                    (:copier nil)
                    (:conc-name %display-))
  foreign-pointer ;; these two slots are for use in clx.d only.
  hash-table      ;; .. so leave hands off here!
  plist
  after-function
  error-handler)

;; ***************************************************************************
;; ... CAUTION ending here.
;; ***************************************************************************

(defun make-color (&key (red 1.0s0) (green 1.0s0) (blue 1.0s0)
                   &allow-other-keys)
  (make-color-internal red green blue))

(defun color-rgb (color)
  (values (color-red color) (color-green color) (color-blue color)))

(defclass xlib-object ()
  ((plist :initarg :plist :initform nil)
   (display :initarg :display)))

(defclass xid-object  (xlib-object)           ((id  :initarg :id)))
(defclass ptr-object  (xlib-object)           ((ptr :initarg :ptr)))

(defclass drawable    (xid-object)            ())
(defclass window      (drawable)              ())
(defclass pixmap      (drawable)              ())
(defclass cursor      (xid-object)            ())
(defclass colormap    (xid-object)            (#|(visual-info :initarg :visual-info :accessor colormap-visual-info)|#))
(defclass gcontext    (ptr-object)            ((%dashes) (%clip-mask) (%timestamp :accessor gcontext-internal-timestamp :initform 0)))
(defclass screen      (ptr-object)            ())
(defclass font        (xid-object)
  ((font-info :initform nil :initarg :font-info)
   (name :initarg :name)
   (encoding :initform nil :initarg :encoding)))



;;;; --------------------------------------------------------------------------
;;;;  Setf Methods
;;;; --------------------------------------------------------------------------
(defsetf ACCESS-CONTROL               SET-ACCESS-CONTROL)
(defsetf CLOSE-DOWN-MODE (display) (mode)
  `(SET-CLOSE-DOWN-MODE ,mode ,display))
(defsetf DISPLAY-AFTER-FUNCTION       SET-DISPLAY-AFTER-FUNCTION)
(defsetf DISPLAY-ERROR-HANDLER        SET-DISPLAY-ERROR-HANDLER)
(defsetf DISPLAY-PLIST                SET-DISPLAY-PLIST)
(defsetf DRAWABLE-BORDER-WIDTH        SET-DRAWABLE-BORDER-WIDTH)
(defsetf DRAWABLE-HEIGHT              SET-DRAWABLE-HEIGHT)
(defsetf DRAWABLE-PLIST               SET-DRAWABLE-PLIST)
(defsetf DRAWABLE-WIDTH               SET-DRAWABLE-WIDTH)
(defsetf DRAWABLE-X                   SET-DRAWABLE-X)
(defsetf DRAWABLE-Y                   SET-DRAWABLE-Y)
(defsetf FONT-PATH                    SET-FONT-PATH)
(defsetf FONT-PLIST                   SET-FONT-PLIST)
(defsetf GCONTEXT-ARC-MODE            SET-GCONTEXT-ARC-MODE)
(defsetf GCONTEXT-BACKGROUND          SET-GCONTEXT-BACKGROUND)
(defsetf GCONTEXT-CACHE-P             SET-GCONTEXT-CACHE-P)
(defsetf GCONTEXT-CAP-STYLE           SET-GCONTEXT-CAP-STYLE)
(defsetf GCONTEXT-CLIP-MASK (gcontext &optional ordering) (clip-mask)
  `(SET-GCONTEXT-CLIP-MASK ,clip-mask ,gcontext ,ordering))
(defsetf GCONTEXT-CLIP-X              SET-GCONTEXT-CLIP-X)
(defsetf GCONTEXT-CLIP-Y              SET-GCONTEXT-CLIP-Y)
(defsetf GCONTEXT-DASH-OFFSET         SET-GCONTEXT-DASH-OFFSET)
(defsetf GCONTEXT-DASHES              SET-GCONTEXT-DASHES)
(defsetf GCONTEXT-EXPOSURES           SET-GCONTEXT-EXPOSURES)
(defsetf GCONTEXT-FILL-RULE           SET-GCONTEXT-FILL-RULE)
(defsetf GCONTEXT-FILL-STYLE          SET-GCONTEXT-FILL-STYLE)
(defsetf GCONTEXT-FONT (gcontext &optional pseudo-font-p) (font)
  `(SET-GCONTEXT-FONT ,font ,gcontext ,pseudo-font-p))
(defsetf GCONTEXT-FOREGROUND          SET-GCONTEXT-FOREGROUND)
(defsetf GCONTEXT-FUNCTION            SET-GCONTEXT-FUNCTION)
(defsetf GCONTEXT-JOIN-STYLE          SET-GCONTEXT-JOIN-STYLE)
(defsetf GCONTEXT-LINE-STYLE          SET-GCONTEXT-LINE-STYLE)
(defsetf GCONTEXT-LINE-WIDTH          SET-GCONTEXT-LINE-WIDTH)
(defsetf GCONTEXT-PLANE-MASK          SET-GCONTEXT-PLANE-MASK)
(defsetf GCONTEXT-PLIST               SET-GCONTEXT-PLIST)
(defsetf GCONTEXT-STIPPLE             SET-GCONTEXT-STIPPLE)
(defsetf GCONTEXT-SUBWINDOW-MODE      SET-GCONTEXT-SUBWINDOW-MODE)
(defsetf GCONTEXT-TILE                SET-GCONTEXT-TILE)
(defsetf GCONTEXT-TS-X                SET-GCONTEXT-TS-X)
(defsetf GCONTEXT-TS-Y                SET-GCONTEXT-TS-Y)
(defsetf PIXMAP-PLIST                 SET-PIXMAP-PLIST)
(defsetf POINTER-MAPPING              SET-POINTER-MAPPING)
(defsetf SCREEN-PLIST                 SET-SCREEN-PLIST)
(defsetf SELECTION-OWNER (display selection &optional time) (owner)
  `(SET-SELECTION-OWNER ,owner ,display ,selection ,time))
(defsetf WINDOW-BACKGROUND            SET-WINDOW-BACKGROUND)
(defsetf WINDOW-BACKING-PIXEL         SET-WINDOW-BACKING-PIXEL)
(defsetf WINDOW-BACKING-PLANES        SET-WINDOW-BACKING-PLANES)
(defsetf WINDOW-BACKING-STORE         SET-WINDOW-BACKING-STORE)
(defsetf WINDOW-COLORMAP              SET-WINDOW-COLORMAP)
(defsetf WINDOW-CURSOR                SET-WINDOW-CURSOR)
(defsetf WINDOW-BIT-GRAVITY           SET-WINDOW-BIT-GRAVITY)
(defsetf WINDOW-BORDER                SET-WINDOW-BORDER)
(defsetf WINDOW-EVENT-MASK            SET-WINDOW-EVENT-MASK)
(defsetf WINDOW-GRAVITY               SET-WINDOW-GRAVITY)
(defsetf WINDOW-DO-NOT-PROPAGATE-MASK SET-WINDOW-DO-NOT-PROPAGATE-MASK)
(defsetf WINDOW-OVERRIDE-REDIRECT     SET-WINDOW-OVERRIDE-REDIRECT)
(defsetf WINDOW-PLIST                 SET-WINDOW-PLIST)
(defsetf WINDOW-PRIORITY (window &optional sibling) (mode)
  `(SET-WINDOW-PRIORITY ,mode ,window ,sibling))
(defsetf WINDOW-SAVE-UNDER            SET-WINDOW-SAVE-UNDER)

;; for CLUE
(defsetf GCONTEXT-DISPLAY SET-GCONTEXT-DISPLAY)


;;;; --------------------------------------------------------------------------
;;;;  Macros
;;;; --------------------------------------------------------------------------

(defmacro EVENT-COND ((display &key timeout peek-p discard-p (force-output-p t))
                      &body clauses)
  (let ((slots (gensym)))
    ;; FIXME this implementation is not 100%
    `(process-event ,display
       :timeout ,timeout
       :peek-p ,peek-p
       :discard-p ,discard-p
       :force-output-p ,force-output-p
       :handler
       (lambda (&rest ,slots &key event-key &allow-other-keys)
         ;; (print slots)
         (cond ,@(mapcar
                  (lambda (clause)
                    (let ((event-or-events (car clause))
                          (binding-list (cadr clause))
                          (test-form (caddr clause))
                          (body-forms (cdddr clause)))
                      (cond ((member event-or-events '(t otherwise))
                             ;;Special case
                             `((and t
                                    ,@(if test-form
                                          (list `(apply #'(lambda (&key ,@binding-list &allow-other-keys) ,test-form) ,slots))
                                          nil))
                               ,@(if body-forms
                                     (list `(apply (lambda (&key ,@binding-list &allow-other-keys)
                                                     ,@body-forms)
                                                   ,slots))
                                     nil)))
                            (t ;; Make-up keywords from the event-keys
                             (unless (listp event-or-events)
                               (setq event-or-events (list event-or-events)))
                             (setq event-or-events
                                   (mapcar #'kintern event-or-events))
                             `((and ,(if (cdr event-or-events)
                                         `(member event-key ',event-or-events)
                                         `(eq event-key ',(car event-or-events)))
                                    ,@(if test-form
                                          (list `(apply #'(lambda (&key ,@binding-list &allow-other-keys) ,test-form) ,slots))
                                          nil))
                               ,@(if body-forms
                                     (list `(apply #'(lambda (&key ,@binding-list &allow-other-keys)
                                                       ,@body-forms)
                                                   ,slots))
                                     nil))))))
                  clauses))))))

(defmacro EVENT-CASE ((&rest args) &body clauses)
  ;; Event-case is just event-cond with the whole body in the test-form
  `(event-cond ,args
               ,@(mapcar
                   #'(lambda (clause)
                       `(,(car clause) ,(cadr clause) (progn ,@(cddr clause))))
                   clauses)))

(defmacro WITH-STATE ((drawable) &body body)
  `(progn ,drawable ,@body))

(defmacro WITH-EVENT-QUEUE ((display) &body body)
  `(progn ,display ,@body))

(defmacro WITH-GCONTEXT ((gcontext &rest options) &body body)
  (let ((saved (gensym)) (gcon (gensym)) (g0 (gensym)) (g1 (gensym))
        (comps 0)
        (setf-forms nil)
        dashes? clip-mask?)
    (do ((q options (cddr q)))
        ((null q))
        (cond ((eq (car q) :dashes)    (setf dashes? t))
              ((eq (car q) :clip-mask) (setf clip-mask? t)))
        (setf comps      (logior comps (%gcontext-key->mask (car q)))
              setf-forms (nconc setf-forms
                                (list (list (find-symbol (string-concat "GCONTEXT-" (symbol-name (car q))) :xlib)
                                            gcon)
                                      (cadr q)))) )
    `(LET* ((,gcon ,gcontext)
            (,saved (%SAVE-GCONTEXT-COMPONENTS ,gcon ,comps))
            ,@(if dashes?    (list `(,g0 (GCONTEXT-DASHES    ,gcon))))
            ,@(if clip-mask? (list `(,g1 (GCONTEXT-CLIP-MASK ,gcon)))) )
       (UNWIND-PROTECT
           (PROGN
             (SETF ,@setf-forms)
             ,@body)
         (PROGN
           (%RESTORE-GCONTEXT-COMPONENTS ,gcon ,saved)
           ,@(if dashes?    (list `(SETF (GCONTEXT-DASHES ,gcon) ,g0)) )
           ,@(if clip-mask? (list `(SETF (GCONTEXT-CLIP-MASK ,gcon) ,g1)) )))) ))

(defmacro WITH-SERVER-GRABBED ((display) &body body)
  ;; The body is not surrounded by a with-display.
  (let ((disp (if (symbolp display) display (gensym))))
    `(let ((,disp ,display))
       (declare (type display ,disp))
       (unwind-protect
           (progn
             (grab-server ,disp)
             ,@body)
         (ungrab-server ,disp)))))


;;;; --------------------------------------------------------------------------
;;;;  Window Manager Property functions
;;;; --------------------------------------------------------------------------

(defun wm-name (window)
  (get-property window :WM_NAME :type :STRING :result-type 'string :transform #'card8->char))

(defsetf wm-name (window) (name)
  `(set-string-property ,window :WM_NAME ,name))

(defun set-string-property (window property string)
  (change-property window property (string string) :STRING 8 :transform #'char->card8)
  string)

(defun wm-icon-name (window)
  (get-property window :WM_ICON_NAME :type :STRING
                :result-type 'string :transform #'card8->char))

(defsetf wm-icon-name (window) (name)
  `(set-string-property ,window :WM_ICON_NAME ,name))

(defun wm-client-machine (window)
  (get-property window :WM_CLIENT_MACHINE :type :STRING :result-type 'string :transform #'card8->char))

(defsetf wm-client-machine (window) (name)
  `(set-string-property ,window :WM_CLIENT_MACHINE ,name))

(defun get-wm-class (window)
  (let ((value (get-property window :WM_CLASS :type :STRING :result-type 'string :transform #'card8->char)))
    (when value
      (let* ((name-len (position (load-time-value (card8->char 0)) (the string value)))
             (name (subseq (the string value) 0 name-len))
             (class (subseq (the string value) (1+ name-len) (1- (length value)))))
        (values (and (plusp (length name)) name)
                (and (plusp (length class)) class))))))

(defun set-wm-class (window resource-name resource-class)
  (set-string-property window :WM_CLASS
                       (string-concat
                        (string (or resource-name ""))
                        (load-time-value
                         (make-string 1 :initial-element (card8->char 0)))
                        (string (or resource-class ""))
                        (load-time-value
                         (make-string 1 :initial-element (card8->char 0)))))
  (values))

(defun wm-command (window)
  ;; Returns a list whose car is the command and
  ;; whose cdr is the list of arguments
  (do* ((command-string (get-property window :WM_COMMAND :type :STRING
                                      :result-type 'string :transform #'card8->char))
        (command nil)
        (start 0 (1+ end))
        (end 0)
        (len (length command-string)))
       ((>= start len) (nreverse command))
    (setq end (position (load-time-value (card8->char 0)) command-string :start start))
    (push (subseq command-string start end) command)))

(defsetf wm-command set-wm-command)
(defun set-wm-command (window command)
  ;; Uses PRIN1 inside the ANSI common lisp form WITH-STANDARD-IO-SYNTAX
  ;; (or equivalent), with elements of command separated by NULL
  ;; characters.  This enables
  ;; (with-standard-io-syntax (mapcar #'read-from-string (wm-command window)))
  ;; to recover a lisp command.
  (set-string-property
    window :WM_COMMAND
    (with-output-to-string (stream)
      (with-standard-io-syntax
        (dolist (c command)
          (prin1 c stream)
          (write-char (load-time-value (card8->char 0)) stream)))))
  command)

;;-----------------------------------------------------------------------------
;; WM_HINTS

;; Some of the functions below need decode-type and encode-type,
;; I provide here a limited implementation to get these functions working.
;;
(defmacro decode-type (type value)
  (cond ((eq type 'pixmap) `(lookup-pixmap %buffer ,value))
        ((eq type 'window) `(lookup-window %buffer ,value))
        ((and (consp type) (eq (car type) 'member))
         `(aref ',(coerce (cdr type) 'vector) ,value))
        (t (error "Unknown type ~S." type)) ))

(defmacro encode-type (type value)
  (cond ((eq type 'pixmap)   `(pixmap-id ,value))
        ((eq type 'window)   `(window-id ,value))
        ((eq type 'card16)   `,value)
        ((eq type 'colormap) `(colormap-id ,value))
        ((eq type 'rgb-val)  `(round (the rgb-val ,value)
                               (load-time-value (/ 1.0s0 #xffff))))
        ((and (consp type) (eq (car type) 'member))
         `(position ,value ',(cdr type)))
        (t (error "Unknown type ~S." type)) ))

(defstruct wm-hints
  (input nil )
  (initial-state nil )
  (icon-pixmap nil )
  (icon-window nil )
  (icon-x nil )
  (icon-y nil )
  (icon-mask nil )
  (window-group nil )
  (flags 0)    ;; Extension-hook.  Exclusive-Or'ed with the FLAGS field
  ;; may be extended in the future
  )

(defun wm-hints (window)
  (let ((prop (get-property window :WM_HINTS :type :WM_HINTS
                            :result-type 'vector)))
    (when prop
      (decode-wm-hints prop (window-display window)))))

(defsetf wm-hints set-wm-hints)
(defun set-wm-hints (window wm-hints)
  (change-property window :WM_HINTS (encode-wm-hints wm-hints) :WM_HINTS 32)
  wm-hints)

(defun decode-wm-hints (vector display)
  (let ((input-hint 0)
        (state-hint 1)
        (icon-pixmap-hint 2)
        (icon-window-hint 3)
        (icon-position-hint 4)
        (icon-mask-hint 5)
        (window-group-hint 6))
    (let ((flags (aref vector 0))
          (hints (make-wm-hints))
          (%buffer display))
      (setf (wm-hints-flags hints) flags)
      (when (logbitp input-hint flags)
        (setf (wm-hints-input hints) (decode-type (member :off :on)
                                                  (aref vector 1))))
      (when (logbitp state-hint flags)
        (setf (wm-hints-initial-state hints)
              (decode-type (member :dont-care :normal :zoom :iconic :inactive)
                           (aref vector 2))))
      (when (logbitp icon-pixmap-hint flags)
        (setf (wm-hints-icon-pixmap hints) (decode-type pixmap (aref vector 3))))
      (when (logbitp icon-window-hint flags)
        (setf (wm-hints-icon-window hints) (decode-type window (aref vector 4))))
      (when (logbitp icon-position-hint flags)
        (setf (wm-hints-icon-x hints) (aref vector 5)
              (wm-hints-icon-y hints) (aref vector 6)))
      (when (logbitp icon-mask-hint flags)
        (setf (wm-hints-icon-mask hints) (decode-type pixmap (aref vector 7))))
      (when (and (logbitp window-group-hint flags) (> (length vector) 7))
        (setf (wm-hints-window-group hints) (aref vector 8)))
      hints)))

(defun encode-wm-hints (wm-hints)
  (let ((input-hint         #b1)
        (state-hint         #b10)
        (icon-pixmap-hint   #b100)
        (icon-window-hint   #b1000)
        (icon-position-hint #b10000)
        (icon-mask-hint     #b100000)
        (window-group-hint  #b1000000)
        (mask               #b1111111)
        )
    (let ((vector (make-array 9 :initial-element 0))
          (flags 0))
      (declare (type (simple-vector 9) vector)
               (type card16 flags))
      (when (wm-hints-input wm-hints)
        (setf flags input-hint
              (aref vector 1) (encode-type (member :off :on) (wm-hints-input wm-hints))))
      (when (wm-hints-initial-state wm-hints)
        (setf flags (logior flags state-hint)
              (aref vector 2) (encode-type (member :dont-care :normal :zoom :iconic :inactive)
                                           (wm-hints-initial-state wm-hints))))
      (when (wm-hints-icon-pixmap wm-hints)
        (setf flags (logior flags icon-pixmap-hint)
              (aref vector 3) (encode-type pixmap (wm-hints-icon-pixmap wm-hints))))
      (when (wm-hints-icon-window wm-hints)
        (setf flags (logior flags icon-window-hint)
              (aref vector 4) (encode-type window (wm-hints-icon-window wm-hints))))
      (when (and (wm-hints-icon-x wm-hints) (wm-hints-icon-y wm-hints))
        (setf flags (logior flags icon-position-hint)
              (aref vector 5) (encode-type card16 (wm-hints-icon-x wm-hints))
              (aref vector 6) (encode-type card16 (wm-hints-icon-y wm-hints))))
      (when (wm-hints-icon-mask wm-hints)
        (setf flags (logior flags icon-mask-hint)
              (aref vector 7) (encode-type pixmap (wm-hints-icon-mask wm-hints))))
      (when (wm-hints-window-group wm-hints)
        (setf flags (logior flags window-group-hint)
              (aref vector 8) (wm-hints-window-group wm-hints)))
      (setf (aref vector 0) (logior flags (logandc2 (wm-hints-flags wm-hints) mask)))
      vector)))


;;-----------------------------------------------------------------------------
;; WM_SIZE_HINTS

;; XXX

;; This code is buggy. My interpretation of chnage-property and get-property is
;; that they only deal with unsigned data, but the as obsolete marked fields x
;; and y are signed, and the code below does not take care.  Running it
;; interpreted, hence with type checks gets errors.

(defstruct wm-size-hints
  (user-specified-position-p nil :type boolean) ;; True when user specified x y
  (user-specified-size-p nil :type boolean)     ;; True when user specified width height
  (x nil #|:type (or null int16)|#)                     ;; Obsolete
  (y nil #|:type (or null int16)|#)                     ;; Obsolete
  (width nil #|:type (or null card16)|#)                ;; Obsolete
  (height nil #|:type (or null card16)|#)               ;; Obsolete
  (min-width nil :type (or null card16))
  (min-height nil :type (or null card16))
  (max-width nil :type (or null card16))
  (max-height nil :type (or null card16))
  (width-inc nil :type (or null card16))
  (height-inc nil :type (or null card16))
  (min-aspect nil :type (or null number))
  (max-aspect nil :type (or null number))
  (base-width nil :type (or null card16))
  (base-height nil :type (or null card16))
  (win-gravity nil :type (or null win-gravity))
  (program-specified-position-p nil :type boolean) ;; True when program specified x y
  (program-specified-size-p nil :type boolean)     ;; True when program specified width height
  )

(defun wm-normal-hints (window)
  (declare (type window window))
  (declare (values wm-size-hints))
  (decode-wm-size-hints (get-property window :WM_NORMAL_HINTS :type :WM_SIZE_HINTS :result-type 'vector)))

(defsetf wm-normal-hints set-wm-normal-hints)
(defun set-wm-normal-hints (window hints)
  (declare (type window window)
           (type wm-size-hints hints))
  (declare (values wm-size-hints))
  (change-property window :WM_NORMAL_HINTS (encode-wm-size-hints hints) :WM_SIZE_HINTS 32)
  hints)

;;; OBSOLETE
(defun wm-zoom-hints (window)
  (declare (type window window))
  (declare (values wm-size-hints))
  (decode-wm-size-hints (get-property window :WM_ZOOM_HINTS :type :WM_SIZE_HINTS :result-type 'vector)))

;;; OBSOLETE
(defsetf wm-zoom-hints set-wm-zoom-hints)
;;; OBSOLETE
(defun set-wm-zoom-hints (window hints)
  (declare (type window window)
           (type wm-size-hints hints))
  (declare (values wm-size-hints))
  (change-property window :WM_ZOOM_HINTS (encode-wm-size-hints hints) :WM_SIZE_HINTS 32)
  hints)

(defun decode-wm-size-hints (vector)
  (declare (type (or null (simple-vector *)) vector))
  (declare (values (or null wm-size-hints)))
  (when vector
    (let ((flags (aref vector 0))
          (hints (make-wm-size-hints)))
      (declare (type card16 flags)
               (type wm-size-hints hints))
      (setf (wm-size-hints-user-specified-position-p hints) (logbitp 0 flags))
      (setf (wm-size-hints-user-specified-size-p hints) (logbitp 1 flags))
      (setf (wm-size-hints-program-specified-position-p hints) (logbitp 2 flags))
      (setf (wm-size-hints-program-specified-size-p hints) (logbitp 3 flags))
      (when (logbitp 4 flags)
        (setf (wm-size-hints-min-width hints) (aref vector 5)
              (wm-size-hints-min-height hints) (aref vector 6)))
      (when (logbitp 5 flags)
        (setf (wm-size-hints-max-width hints) (aref vector 7)
              (wm-size-hints-max-height hints) (aref vector 8)))
      (when (logbitp 6 flags)
        (setf (wm-size-hints-width-inc hints) (aref vector 9)
              (wm-size-hints-height-inc hints) (aref vector 10)))
      (when (logbitp 7 flags)
        (setf (wm-size-hints-min-aspect hints) (/ (aref vector 11) (aref vector 12))
              (wm-size-hints-max-aspect hints) (/ (aref vector 13) (aref vector 14))))
      (when (> (length vector) 15)
        ;; This test is for backwards compatibility since old Xlib programs
        ;; can set a size-hints structure that is too small.  See ICCCM.
        (when (logbitp 8 flags)
          (setf (wm-size-hints-base-width hints) (aref vector 15)
                (wm-size-hints-base-height hints) (aref vector 16)))
        (when (logbitp 9 flags)
          (setf (wm-size-hints-win-gravity hints)
                (decode-type (member :unmap :north-west :north :north-east :west
                                     :center :east :south-west :south :south-east :static)
                             (aref vector 17)))))
      ;; Obsolete fields
      (when (or (logbitp 0 flags) (logbitp 2 flags))
        (setf (wm-size-hints-x hints) (aref vector 1)
              (wm-size-hints-y hints) (aref vector 2)))
      (when (or (logbitp 1 flags) (logbitp 3 flags))
        (setf (wm-size-hints-width hints) (aref vector 3)
              (wm-size-hints-height hints) (aref vector 4)))
      hints)))

(defun encode-wm-size-hints (hints)
  (declare (type wm-size-hints hints))
  (declare (values simple-vector))
  (let ((vector (make-array 18 :initial-element 0))
        (flags 0))
    (declare (type (simple-vector 18) vector)
             (type card16 flags))
    (when (wm-size-hints-user-specified-position-p hints)
      (setf (ldb (byte 1 0) flags) 1))
    (when (wm-size-hints-user-specified-size-p hints)
      (setf (ldb (byte 1 1) flags) 1))
    (when (wm-size-hints-program-specified-position-p hints)
      (setf (ldb (byte 1 2) flags) 1))
    (when (wm-size-hints-program-specified-size-p hints)
      (setf (ldb (byte 1 3) flags) 1))
    (when (and (wm-size-hints-min-width hints) (wm-size-hints-min-height hints))
      (setf (ldb (byte 1 4) flags) 1
            (aref vector 5) (wm-size-hints-min-width hints)
            (aref vector 6) (wm-size-hints-min-height hints)))
    (when (and (wm-size-hints-max-width hints) (wm-size-hints-max-height hints))
      (setf (ldb (byte 1 5) flags) 1
            (aref vector 7) (wm-size-hints-max-width hints)
            (aref vector 8) (wm-size-hints-max-height hints)))
    (when (and (wm-size-hints-width-inc hints) (wm-size-hints-height-inc hints))
      (setf (ldb (byte 1 6) flags) 1
            (aref vector 9) (wm-size-hints-width-inc hints)
            (aref vector 10) (wm-size-hints-height-inc hints)))
    (let ((min-aspect (wm-size-hints-min-aspect hints))
          (max-aspect (wm-size-hints-max-aspect hints)))
      (when (and min-aspect max-aspect)
        (setf (ldb (byte 1 7) flags) 1
              min-aspect (rationalize min-aspect)
              max-aspect (rationalize max-aspect)
              (aref vector 11) (numerator min-aspect)
              (aref vector 12) (denominator min-aspect)
              (aref vector 13) (numerator max-aspect)
              (aref vector 14) (denominator max-aspect))))
    (when (and (wm-size-hints-base-width hints)
               (wm-size-hints-base-height hints))
      (setf (ldb (byte 1 8) flags) 1
            (aref vector 15) (wm-size-hints-base-width hints)
            (aref vector 16) (wm-size-hints-base-height hints)))
    (when (wm-size-hints-win-gravity hints)
      (setf (ldb (byte 1 9) flags) 1
            (aref vector 17) (encode-type
                               (member :unmap :north-west :north :north-east :west
                                       :center :east :south-west :south :south-east :static)
                               (wm-size-hints-win-gravity hints))))
    ;; Obsolete fields
    (when (and (wm-size-hints-x hints) (wm-size-hints-y hints))
      (unless (wm-size-hints-user-specified-position-p hints)
        (setf (ldb (byte 1 2) flags) 1))
      (setf (aref vector 1) (wm-size-hints-x hints)
            (aref vector 2) (wm-size-hints-y hints)))
    (when (and (wm-size-hints-width hints) (wm-size-hints-height hints))
      (unless (wm-size-hints-user-specified-size-p hints)
        (setf (ldb (byte 1 3) flags) 1))
      (setf (aref vector 3) (wm-size-hints-width hints)
            (aref vector 4) (wm-size-hints-height hints)))
    (setf (aref vector 0) flags)
    vector))

;;-----------------------------------------------------------------------------
;; Icon_Size

;; Use the same intermediate structure as WM_SIZE_HINTS

(defun icon-sizes (window)
  (let ((vector (get-property window :WM_ICON_SIZE :type :WM_ICON_SIZE :result-type 'vector)))
    (when vector
      (make-wm-size-hints
       :min-width   (aref vector 0)
        :min-height (aref vector 1)
        :max-width  (aref vector 2)
        :max-height (aref vector 3)
        :width-inc  (aref vector 4)
        :height-inc (aref vector 5)))))

(defsetf icon-sizes set-icon-sizes)
(defun set-icon-sizes (window wm-size-hints)
  (let ((vector (vector (wm-size-hints-min-width wm-size-hints)
                        (wm-size-hints-min-height wm-size-hints)
                        (wm-size-hints-max-width wm-size-hints)
                        (wm-size-hints-max-height wm-size-hints)
                        (wm-size-hints-width-inc wm-size-hints)
                        (wm-size-hints-height-inc wm-size-hints))))
    (change-property window :WM_ICON_SIZE vector :WM_ICON_SIZE 32)
    wm-size-hints))

;;-----------------------------------------------------------------------------
;; WM-Protocols

(defun wm-protocols (window)
  (map 'list #'(lambda (id) (atom-name (window-display window) id))
       (get-property window :WM_PROTOCOLS :type :ATOM)))

(defsetf wm-protocols set-wm-protocols)
(defun set-wm-protocols (window protocols)
  (change-property window :WM_PROTOCOLS
                   (map 'list #'(lambda (atom) (intern-atom (window-display window) atom))
                        protocols)
                   :ATOM 32)
  protocols)

;;-----------------------------------------------------------------------------
;; WM-Colormap-windows

(defun wm-colormap-windows (window)
  (values (get-property window :WM_COLORMAP_WINDOWS :type :WINDOW
                        :transform #'(lambda (id)
                                       (lookup-window (window-display window) id)))))

(defsetf wm-colormap-windows set-wm-colormap-windows)
(defun set-wm-colormap-windows (window colormap-windows)
  (change-property window :WM_COLORMAP_WINDOWS colormap-windows :WINDOW 32
                   :transform #'window-id)
  colormap-windows)

;;-----------------------------------------------------------------------------
;; Transient-For

(defun transient-for (window)
  (let ((prop (get-property window :WM_TRANSIENT_FOR :type :WINDOW :result-type 'list)))
    (and prop (lookup-window (window-display window) (car prop)))))

(defsetf transient-for set-transient-for)
(defun set-transient-for (window transient)
  (declare (type window window transient))
  (change-property window :WM_TRANSIENT_FOR (list (window-id transient)) :WINDOW 32)
  transient)

;;-----------------------------------------------------------------------------
;; Set-WM-Properties

(defun set-wm-properties (window &rest options &key
                          name icon-name resource-name resource-class command
                          client-machine hints normal-hints zoom-hints
                          ;; the following are used for wm-normal-hints
                          (user-specified-position-p nil usppp)
                          (user-specified-size-p nil usspp)
                          (program-specified-position-p nil psppp)
                          (program-specified-size-p nil psspp)
                          x y width height min-width min-height max-width max-height
                          width-inc height-inc min-aspect max-aspect
                          base-width base-height win-gravity
                          ;; the following are used for wm-hints
                          input initial-state icon-pixmap icon-window
                          icon-x icon-y icon-mask window-group)
  ;; Set properties for WINDOW.
  (when name (setf (wm-name window) name))
  (when icon-name (setf (wm-icon-name window) icon-name))
  (when client-machine (setf (wm-client-machine window) client-machine))
  (when (or resource-name resource-class)
    (set-wm-class window resource-name resource-class))
  (when command (setf (wm-command window) command))
  ;; WM-HINTS
  (if (dolist (arg '(:input :initial-state :icon-pixmap :icon-window
                            :icon-x :icon-y :icon-mask :window-group))
        (when (getf options arg) (return t)))
      (let ((wm-hints (if hints (copy-wm-hints hints) (make-wm-hints))))
        (when input (setf (wm-hints-input wm-hints) input))
        (when initial-state (setf (wm-hints-initial-state wm-hints) initial-state))
        (when icon-pixmap (setf (wm-hints-icon-pixmap wm-hints) icon-pixmap))
        (when icon-window (setf (wm-hints-icon-window wm-hints) icon-window))
        (when icon-x (setf (wm-hints-icon-x wm-hints) icon-x))
        (when icon-y (setf (wm-hints-icon-y wm-hints) icon-y))
        (when icon-mask (setf (wm-hints-icon-mask wm-hints) icon-mask))
        (when window-group
          (setf (wm-hints-window-group wm-hints) window-group))
        (setf (wm-hints window) wm-hints))
      (when hints (setf (wm-hints window) hints)))
  ;; WM-NORMAL-HINTS
  (if (dolist (arg '(:x :y :width :height :min-width :min-height :max-width :max-height
                        :width-inc :height-inc :min-aspect :max-aspect
                        :user-specified-position-p :user-specified-size-p
                        :program-specified-position-p :program-specified-size-p
                        :base-width :base-height :win-gravity))
        (when (getf options arg) (return t)))
      (let ((size (if normal-hints (copy-wm-size-hints normal-hints) (make-wm-size-hints))))
        (when x (setf (wm-size-hints-x size) x))
        (when y (setf (wm-size-hints-y size) y))
        (when width (setf (wm-size-hints-width size) width))
        (when height (setf (wm-size-hints-height size) height))
        (when min-width (setf (wm-size-hints-min-width size) min-width))
        (when min-height (setf (wm-size-hints-min-height size) min-height))
        (when max-width (setf (wm-size-hints-max-width size) max-width))
        (when max-height (setf (wm-size-hints-max-height size) max-height))
        (when width-inc (setf (wm-size-hints-width-inc size) width-inc))
        (when height-inc (setf (wm-size-hints-height-inc size) height-inc))
        (when min-aspect (setf (wm-size-hints-min-aspect size) min-aspect))
        (when max-aspect (setf (wm-size-hints-max-aspect size) max-aspect))
        (when base-width (setf (wm-size-hints-base-width size) base-width))
        (when base-height (setf (wm-size-hints-base-height size) base-height))
        (when win-gravity (setf (wm-size-hints-win-gravity size) win-gravity))
        (when usppp
          (setf (wm-size-hints-user-specified-position-p size) user-specified-position-p))
        (when usspp
          (setf (wm-size-hints-user-specified-size-p size) user-specified-size-p))
        (when psppp
          (setf (wm-size-hints-program-specified-position-p size) program-specified-position-p))
        (when psspp
          (setf (wm-size-hints-program-specified-size-p size) program-specified-size-p))
        (setf (wm-normal-hints window) size))
      (when normal-hints (setf (wm-normal-hints window) normal-hints)))
  (when zoom-hints (setf (wm-zoom-hints window) zoom-hints))
  )

;;; OBSOLETE
(defun set-standard-properties (window &rest options)
  (apply #'set-wm-properties window options))

;;-----------------------------------------------------------------------------
;; Colormaps

(defstruct (standard-colormap (:copier nil) (:predicate nil))
  (colormap nil :type (or null colormap))
  (base-pixel 0 :type pixel)
  (max-color nil :type (or null color))
  (mult-color nil :type (or null color))
  (visual nil :type (or null visual-info))
  (kill nil :type (or (member nil :release-by-freeing-colormap)
                      drawable gcontext cursor colormap font)))

(defun card16->rgb-val (value)
  (declare (type card16 value))
  (declare (values short-float))
  (the short-float (* (the card16 value) (load-time-value (/ 1.0s0 #xffff)))))

(defun rgb-colormaps (window property)
  (declare (type window window)
           (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
                         :RGB_GREEN_MAP :RGB_BLUE_MAP) property))
  (let ((prop (get-property window property :type :RGB_COLOR_MAP :result-type 'vector)))
    (declare (type (or null simple-vector) prop))
    (when prop
      (list (make-standard-colormap
              :colormap (lookup-colormap (window-display window) (aref prop 0))
              :base-pixel (aref prop 7)
              :max-color (make-color :red   (card16->rgb-val (aref prop 1))
                                     :green (card16->rgb-val (aref prop 3))
                                     :blue  (card16->rgb-val (aref prop 5)))
              :mult-color (make-color :red   (card16->rgb-val (aref prop 2))
                                      :green (card16->rgb-val (aref prop 4))
                                      :blue  (card16->rgb-val (aref prop 6)))
              :visual (and (<= 9 (length prop))
                           (visual-info (window-display window) (aref prop 8)))
              :kill (and (<= 10 (length prop))
                         (let ((killid (aref prop 9)))
                           (if (= killid 1)
                               :release-by-freeing-colormap
                               (lookup-resource-id (window-display window)
                                                   killid)))))))))

(defsetf rgb-colormaps set-rgb-colormaps)
(defun set-rgb-colormaps (window property maps)
  (declare (type window window)
           (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
                         :RGB_GREEN_MAP :RGB_BLUE_MAP) property)
           (type list maps))
  (let ((prop (make-array (* 10 (length maps)) :element-type 'card32))
        (index -1))
    (dolist (map maps)
      (setf (aref prop (incf index))
            (encode-type colormap (standard-colormap-colormap map)))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-red (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-red (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-green (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-green (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-blue (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
            (encode-type rgb-val (color-blue (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
            (standard-colormap-base-pixel map))
      (setf (aref prop (incf index))
            (visual-info-id (standard-colormap-visual map)))
      (setf (aref prop (incf index))
            (let ((kill (standard-colormap-kill map)))
              (etypecase kill
                (symbol
                  (ecase kill
                    ((nil) 0)
                    ((:release-by-freeing-colormap) 1)))
                (drawable (drawable-id kill))
                (gcontext (gcontext-id kill))
                (cursor (cursor-id kill))
                (colormap (colormap-id kill))
                (font (font-id kill))))))
    (change-property window property prop :RGB_COLOR_MAP 32)))

;;; OBSOLETE
(defun get-standard-colormap (window property)
  (declare (type window window)
           (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
                         :RGB_GREEN_MAP :RGB_BLUE_MAP) property))
  (declare (values colormap base-pixel max-color mult-color))
  (let ((prop (get-property window property :type :RGB_COLOR_MAP :result-type 'vector)))
    (declare (type (or null simple-vector) prop))
    (when prop
      (values (lookup-colormap (window-display window) (aref prop 0))
              (aref prop 7)                     ;Base Pixel
              (make-color :red   (card16->rgb-val (aref prop 1))        ;Max Color
                          :green (card16->rgb-val (aref prop 3))
                          :blue  (card16->rgb-val (aref prop 5)))
              (make-color :red   (card16->rgb-val (aref prop 2))        ;Mult color
                          :green (card16->rgb-val (aref prop 4))
                          :blue  (card16->rgb-val (aref prop 6)))))))

;;; OBSOLETE
(defun set-standard-colormap (window property colormap base-pixel max-color mult-color)
  (declare (type window window)
           (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
                         :RGB_GREEN_MAP :RGB_BLUE_MAP) property)
           (type colormap colormap)
           (type pixel base-pixel)
           (type color max-color mult-color))
  (let ((prop (apply #'vector (encode-type colormap colormap)
                     (encode-type rgb-val (color-red max-color))
                     (encode-type rgb-val (color-red mult-color))
                     (encode-type rgb-val (color-green max-color))
                     (encode-type rgb-val (color-green mult-color))
                     (encode-type rgb-val (color-blue max-color))
                     (encode-type rgb-val (color-blue mult-color))
                     base-pixel)))
    (change-property window property prop :RGB_COLOR_MAP 32)))


;;;; --------------------------------------------------------------------------
;;;;  Cut-Buffers
;;;; --------------------------------------------------------------------------

(defun cut-buffer (display &key (buffer 0) (type :STRING) (result-type 'string)
                   (transform #'card8->char) (start 0) end)
  ;; Return the contents of cut-buffer BUFFER
  (let* ((root (screen-root (first (display-roots display))))
         (property (aref '#(:CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
                            :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7)
                         buffer)))
    (get-property root property :type type :result-type result-type
                  :start start :end end :transform transform)))

(defun (setf cut-buffer) (data display &key (buffer 0) (type :STRING) (format 8)
                               (start 0) end (transform #'char->card8))
  (let* ((root (screen-root (first (display-roots display))))
         (property (aref '#(:CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
                                         :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7)
                         buffer)))
    (change-property root property data type format :transform transform :start start :end end)
    data))

(defun rotate-cut-buffers (display &optional (delta 1) (careful-p t))
  ;; Positive rotates left, negative rotates right (opposite of actual protocol request).
  ;; When careful-p, ensure all cut-buffer properties are defined, to prevent errors.
  (let* ((root (screen-root (first (display-roots display))))
         (buffers '#(:cut_buffer0 :cut_buffer1 :cut_buffer2 :cut_buffer3
                     :cut_buffer4 :cut_buffer5 :cut_buffer6 :cut_buffer7)))
    (when careful-p
      (let ((props (list-properties root)))
        (dotimes (i 8)
          (unless (member (aref buffers i) props)
            (setf (cut-buffer display :buffer i) "")))))
    (rotate-properties root buffers delta)))


;;;; --------------------------------------------------------------------------
;;;;  Printers
;;;; --------------------------------------------------------------------------

;;; NOTE:
;;;   I used here a (funcall #,#'fun ..) klugde, but by clisp-1996-07-22 this now considered
;;;   illegal, so I save the untraced functions by copying them. This allows me to trace all or arbitrary
;;;   xlib functions without getting into infinite recursion.

(setf (fdefinition '%untraced-color-blue) #'color-blue
      (fdefinition '%untraced-color-green) #'color-green
      (fdefinition '%untraced-color-red) #'color-red
      (fdefinition '%untraced-colormap-visual-info) #'colormap-visual-info
      (fdefinition '%untraced-display-display) #'display-display
      (fdefinition '%untraced-display-host) #'display-host
      (fdefinition '%untraced-display-protocol-major-version)
      #'display-protocol-major-version
      (fdefinition '%untraced-display-protocol-minor-version)
      #'display-protocol-minor-version
      (fdefinition '%untraced-display-release-number) #'display-release-number
      (fdefinition '%untraced-display-vendor-name) #'display-vendor-name
      (fdefinition '%untraced-drawable-height) #'drawable-height
      (fdefinition '%untraced-drawable-width) #'drawable-width
      (fdefinition '%untraced-drawable-x) #'drawable-x
      (fdefinition '%untraced-drawable-y) #'drawable-y
      (fdefinition '%untraced-visual-info-class) #'visual-info-class)

(defmethod print-object ((color color) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (color out :type t :identity t)
        (write (%untraced-color-red color) :stream out)
        (write-string " " out)
        (write (%untraced-color-green color) :stream out)
        (write-string " " out)
        (write (%untraced-color-blue color) :stream out))))

(defmethod print-object ((dpy display) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (dpy out :type t :identity t)
        (if (closed-display-p dpy)
            (write 'closed-display :stream out)
            (format out "~A:~D (~A R~D) X~D.~D"
                    (%untraced-display-host dpy)
                    (%untraced-display-display dpy)
                    (%untraced-display-vendor-name dpy)
                    (%untraced-display-release-number dpy)
                    (%untraced-display-protocol-major-version dpy)
                    (%untraced-display-protocol-minor-version dpy))))))

(defmethod print-object ((xo xid-object) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (xo out :type t :identity t)
        (with-slots (id display) xo
          (format out "~A #x~8,'0X"
                  (if (closed-display-p display)
                      'closed-display
                      (%untraced-display-host display))
                  id)))))

(defmethod print-object ((cm colormap) (out stream))
  (with-slots (id display) cm
    (if (or *print-readably* (closed-display-p display)) (call-next-method)
        (print-unreadable-object (cm out :type t :identity t)
          (let* ((visinfo (%untraced-colormap-visual-info cm))
                 (vclass  (if visinfo (%untraced-visual-info-class visinfo)
                              "unknown visual class")))
            (format out "~A #x~8,'0X ~A" (%untraced-display-host display)
                    id vclass))))))

(defmethod print-object ((fo font) (out stream))
  (with-slots (id name display) fo
    (if (or *print-readably* (closed-display-p display)) (call-next-method)
        (print-unreadable-object (fo out :type t :identity t)
          (format out "~A ~A #x~8,'0X" (%untraced-display-host display)
                  name id)))))

(defmethod print-object ((dr drawable) (out stream))
  (with-slots (id display) dr
    (if (or *print-readably* (closed-display-p display)) (call-next-method)
        (print-unreadable-object (dr out :type t :identity t)
          (format out "~Dx~D+~D+~D ~A #x~8,'0X"
                  (%untraced-drawable-width dr) (%untraced-drawable-height dr)
                  (%untraced-drawable-x dr) (%untraced-drawable-y dr)
                  (%untraced-display-host display) id)))))


;;;; --------------------------------------------------------------------------
;;;;  Misc
;;;; --------------------------------------------------------------------------


;;;; --------------------------------------------------------------------------
;;;;  Stuff, which is realy some internals of CLX,
;;;;  but needed by some programs ...
;;;; --------------------------------------------------------------------------

(defconstant *STATE-MASK-VECTOR*
  '#(:shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5 :button-1 :button-2 :button-3 :button-4 :button-5))
(defconstant *GCONTEXT-COMPONENTS*
  '(:DRAWABLE :FUNCTION :PLANE-MASK :FOREGROUND :BACKGROUND
    :LINE-WIDTH :LINE-STYLE :CAP-STYLE :JOIN-STYLE :FILL-STYLE
    :FILL-RULE :ARC-MODE :TILE :STIPPLE :TS-X :TS-Y :FONT
    :SUBWINDOW-MODE :EXPOSURES :CLIP-X :CLIP-Y :CLIP-MASK
    :CLIP-ORDERING :DASH-OFFSET :DASHES :CACHE-P))
(defun make-gcontext (&rest ignore)
  (warn "~S~@[~S~] is an internal function!" 'make-gcontext ignore)
  (make-instance 'gcontext))

;; What has that to do with graphics?!
(defun kintern (name) (intern (string name) #,(find-package :keyword)))

;;;;From depdefs.lisp
;;;;
;;; This defines a type which is a subtype of the integers.
;;; This type is used to describe all variables that can be array indices.
;;; It is here because it is used below.
;;; This is inclusive because start/end can be 1 past the end.
;; Note: These are ignorant version of these macros!
(defmacro index+ (&rest numbers) `(+ ,@numbers))
(defmacro index-logand (&rest numbers) `(logand ,@numbers))
(defmacro index-logior (&rest numbers) `(logior ,@numbers))
(defmacro index- (&rest numbers) `(- ,@numbers))
(defmacro index* (&rest numbers) `(* ,@numbers))
(defmacro index1+ (number) `(1+ ,number))
(defmacro index1- (number) `(1- ,number))
(defmacro index-incf (place &optional (delta 1)) `(setf ,place (index+ ,place ,delta)));Hmm?
(defmacro index-decf (place &optional (delta 1)) `(setf ,place (index- ,place ,delta)));Hmm?
(defmacro index-min (&rest numbers) `(min ,@numbers))
(defmacro index-max (&rest numbers) `(max ,@numbers))
(defmacro index-floor (number divisor) `(floor ,number ,divisor))
(defmacro index-ceiling (number divisor) `(ceiling ,number ,divisor))
(defmacro index-truncate (number divisor) `(truncate ,number ,divisor))
(defmacro index-mod (number divisor) `(mod ,number ,divisor))
(defmacro index-ash (number count) `(ash ,number ,count))
(defmacro index-plusp (number) `(plusp ,number))
(defmacro index-zerop (number) `(zerop ,number))
(defmacro index-evenp (number) `(evenp ,number))
(defmacro index-oddp  (number) `(oddp  ,number))
(defmacro index> (&rest numbers) `(> ,@numbers))
(defmacro index= (&rest numbers) `(= ,@numbers))
(defmacro index< (&rest numbers) `(< ,@numbers))
(defmacro index>= (&rest numbers) `(>= ,@numbers))
(defmacro index<= (&rest numbers) `(<= ,@numbers))

(defun read-bitmap-file (pathname)
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname))
  (declare (values image))
  (with-open-file (fstream pathname :direction :input)
    (let ((line "")
          (properties nil)
          (name nil)
          (name-end nil))
      (declare (type string line)
               (type stringable name)
               (type list properties))
      ;; Get properties
      (loop
        (setq line (read-line fstream))
        (unless (char= (aref line 0) #\#) (return))
        (flet ((read-keyword (line start end)
                 (kintern
                   (substitute
                     #\- #\_
                     (#-excl string-upcase
                      #+excl correct-case
                      (subseq line start end))
                     :test #'char=))))
          (when (null name)
            (setq name-end (position #\_ line :test #'char= :from-end t)
                  name (read-keyword line 8 name-end))
            (unless (eq name :image)
              (setf (getf properties :name) name)))
          (let* ((ind-start (index1+ name-end))
                 (ind-end (position #\Space line :test #'char=
                                    :start ind-start))
                 (ind (read-keyword line ind-start ind-end))
                 (val-start (index1+ ind-end))
                 (val (parse-integer line :start val-start)))
            (setf (getf properties ind) val))))
      ;; Calculate sizes
      (multiple-value-bind (width height depth left-pad)
          (flet ((extract-property (ind &rest default)
                   (prog1 (apply #'getf properties ind default)
                          (remf properties ind))))
            (values (extract-property :width)
                    (extract-property :height)
                    (extract-property :depth 1)
                    (extract-property :left-pad 0)))
        (declare (type (or null card16) width height)
                 (type image-depth depth)
                 (type card8 left-pad))
        (unless (and width height) (error "Not a BITMAP file"))
        (let* ((bits-per-pixel
                 (cond ((index> depth 24) 32)
                       ((index> depth 16) 24)
                       ((index> depth 8)  16)
                       ((index> depth 4)   8)
                       ((index> depth 1)   4)
                       (t                  1)))
               (bits-per-line (index* width bits-per-pixel))
               (bytes-per-line (index-ceiling bits-per-line 8))
               (padded-bits-per-line
                 (index* (index-ceiling bits-per-line 32) 32))
               (padded-bytes-per-line
                 (index-ceiling padded-bits-per-line 8))
               (data (make-array (* padded-bytes-per-line height)
                                 :element-type 'card8))
               (line-base 0)
               (byte 0))
          #|(declare (type array-index bits-per-line bytes-per-line
                         padded-bits-per-line padded-bytes-per-line
                         line-base byte)
                   (type buffer-bytes data))|#
          (progn
            (flet ((parse-hex (char)
                     (second
                       (assoc char
                              '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
                                (#\4  4) (#\5  5) (#\6  6) (#\7  7)
                                (#\8  8) (#\9  9) (#\a 10) (#\b 11)
                                (#\c 12) (#\d 13) (#\e 14) (#\f 15))
                              :test #'char-equal))))
             (locally
              (declare (inline parse-hex))
              ;; Read data
              ;; Note: using read-line instead of read-char would be 20% faster,
              ;;       but would cons a lot of garbage...
              (dotimes (i height)
                (dotimes (j bytes-per-line)
                  (loop (when (eql (read-char fstream) #\x) (return)))
                  (setf (aref data (index+ line-base byte))
                        (index+ (index-ash (parse-hex (read-char fstream)) 4)
                                (parse-hex (read-char fstream))))
                  (incf byte))
                (setq byte 0
                      line-base (index+ line-base padded-bytes-per-line))))))
          ;; Compensate for left-pad in width and x-hot
          (index-decf width left-pad)
          (when (and (getf properties :x-hot) (plusp (getf properties :x-hot)))
            (index-decf (getf properties :x-hot) left-pad))
          (create-image
            :width width :height height
            :depth depth :bits-per-pixel bits-per-pixel
            :data data :plist properties :format :z-pixmap
            :bytes-per-line padded-bytes-per-line
            :unit 32 :pad 32 :left-pad left-pad
            :byte-lsb-first-p t :bit-lsb-first-p t))))))


;; These functions are used by clue.

(defun encode-event-mask (keys)
  (apply #'make-event-mask keys))

;;These two could be provided.
;;(defun save-id (display id object) "Register a resource-id from another display.")
;;(defmacro deallocate-resource-id (display id type) "Deallocate a resource-id for OBJECT in DISPLAY")

;;(defun x-type-error (object type &optional error-string))
;;(defun get-display-modifier-mapping (display))

;; actually exported.
;; (defun mapping-notify (display request start count)
;;     "Called on a mapping-notify event to update the keyboard-mapping cache in DISPLAY")


;;; Error handler, we probably want a proper condition hierarchy, but for a first approach this may be enough:

(defun default-error-handler (display error-code &key current-sequence sequence
                              major minor resource-id atom-id value)
  (cerror "Ignore this error and proceed."
          "Asynchronous ~A on display ~S, sequence ~D. Opcode is ~D.~D. Current sequence is ~D.~A"
          error-code display sequence major minor current-sequence
          ;; at most one of RESOURCE-ID, ATOM-ID, and VALUE is available
          (cond (resource-id (format nil " Resource ID is #x~8,'0x." resource-id))
                (value (format nil " Bad value is ~D." value))
                (atom-id (format nil " Bad atom ID is #x~8,'0x." atom-id))
                (t ""))))

(define-condition x-error (error)
  ((caller :reader x-error-caller :initarg :caller)))

(define-condition closed-display (x-error)
  ((display :reader closed-display-display :initarg :display))
  (:report
   (lambda (condition stream)
     (format stream "~s: used closed display ~s"
             (x-error-caller condition)
             (closed-display-display condition)))))

(define-condition request-error (x-error) ())
(define-condition resource-error (request-error) ())
(define-condition access-error (request-error) ())
(define-condition alloc-error (request-error) ())
(define-condition atom-error (request-error) ())
(define-condition colormap-error (resource-error) ())
(define-condition connection-failure (x-error) ())
(define-condition cursor-error (resource-error) ())
(define-condition device-busy (x-error) ())
(define-condition drawable-error (resource-error) ())
(define-condition font-error (resource-error) ())
(define-condition gcontext-error (resource-error) ())
(define-condition id-choice-error (resource-error) ())
(define-condition implementation-error (resource-error) ())
(define-condition length-error (resource-error) ())
(define-condition lookup-error (x-error) ())
(define-condition match-error (request-error) ())
(define-condition missing-parameter (x-error) ())
(define-condition name-error (request-error) ())
(define-condition pixmap-error (resource-error) ())
(define-condition reply-length-error (x-error) ())
(define-condition reply-timeout (x-error) ())
(define-condition sequence-error (x-error) ())
(define-condition server-disconnect (x-error) ())
(define-condition unexpected-reply (x-error) ())
(define-condition unknown-error (request-error) ())
(define-condition value-error (request-error) ())
(define-condition window-error (resource-error) ())

(pushnew "XLIB" custom:*system-package-list* :test #'string=)
(pushnew "XPM" custom:*system-package-list* :test #'string=)

;; some functions are not implemented:
(macrolet ((undefined (name)
             `(define-compiler-macro ,name (&whole form &rest args)
                (declare (ignore args))
                (progn (warn "~S is not implemented: ~S" ',name form)
                       form))))
(undefined DISPLAY-RESOURCE-ID-BASE)
(undefined DISPLAY-RESOURCE-ID-MASK)
(undefined DISPLAY-TRACE)
(undefined DRAW-GLYPH)
(undefined DRAW-IMAGE-GLYPH)
(undefined TRANSLATE-DEFAULT)
(undefined QUEUE-EVENT)
(undefined CHANGE-KEYBOARD-MAPPING)
(undefined KEYBOARD-MAPPING)
)
