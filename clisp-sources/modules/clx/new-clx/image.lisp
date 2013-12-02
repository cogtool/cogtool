;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX Image functions

;;;
;;;                      TEXAS INSTRUMENTS INCORPORATED
;;;                               P.O. BOX 2909
;;;                            AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;;; Hacked by me (Gilbert Baumann) to fit to my re-implementation of
;;;; CLX but most parts left unmodified (sans stripping some annoying
;;;; declares), so I leave the original copyright here.

(require "clx")

(in-package :xlib)

;;; An error signalling macro use to specify that keyword arguments are required.
(defmacro required-arg (name)
  `(progn (error "Missing parameter ~S" ',name)))

(defvar *image-unit* 32)
(defvar *image-pad* 32)
(defvar *image-bit-lsb-first-p* t)      ;what about these on big-endian systems? --GB
(defvar *image-byte-lsb-first-p* t)
(deftype buffer-bytes () `(simple-array (unsigned-byte 8) (*)))

(deftype pixarray-1-element-type ()  'bit)
(deftype pixarray-4-element-type ()  '(unsigned-byte 4))
(deftype pixarray-8-element-type ()  '(unsigned-byte 8))
(deftype pixarray-16-element-type () '(unsigned-byte 16))
(deftype pixarray-24-element-type () '(unsigned-byte 24))
(deftype pixarray-32-element-type () '(unsigned-byte 32))
(deftype pixarray-1  ()  '(array pixarray-1-element-type (* *)))
(deftype pixarray-4  ()  '(array pixarray-4-element-type (* *)))
(deftype pixarray-8  ()  '(array pixarray-8-element-type (* *)))
(deftype pixarray-16 ()  '(array pixarray-16-element-type (* *)))
(deftype pixarray-24 ()  '(array pixarray-24-element-type (* *)))
(deftype pixarray-32 ()  '(array pixarray-32-element-type (* *)))
(deftype pixarray () '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))
(deftype bitmap () 'pixarray-1)

(defstruct (image (:constructor nil) (:copier nil) (:predicate nil))
  ;; Public structure
  (width 0 :type card16 :read-only t)
  (height 0 :type card16 :read-only t)
  (depth 1 :type card8 :read-only t)
  (plist nil :type list))

;; Image-Plist accessors:
(defun image-name (image) (getf (image-plist image) :name))
(defun image-x-hot (image) (getf (image-plist image) :x-hot))
(defun image-y-hot (image) (getf (image-plist image) :y-hot))
(defun image-red-mask (image) (getf (image-plist image) :red-mask))
(defun image-blue-mask (image) (getf (image-plist image) :blue-mask))
(defun image-green-mask (image) (getf (image-plist image) :green-mask))
(defun (setf image-name) (new-value image)
  (setf (getf (image-plist image) :name) new-value))
(defun (setf image-x-hot) (new-value image)
  (setf (getf (image-plist image) :x-hot) new-value))
(defun (setf image-y-hot) (new-value image)
  (setf (getf (image-plist image) :y-hot) new-value))
(defun (setf image-red-mask) (new-value image)
  (setf (getf (image-plist image) :red-mask) new-value))
(defun (setf image-blue-mask) (new-value image)
  (setf (getf (image-plist image) :blue-mask) new-value))
(defun (setf image-green-mask) (new-value image)
  (setf (getf (image-plist image) :green-mask) new-value))

(defun print-image (image stream depth)
  (declare (type image image)
           (ignore depth))
  (print-unreadable-object (image stream :type t)
    (when (image-name image)
      (write-string (string (image-name image)) stream)
      (write-string " " stream))
    (prin1 (image-width image) stream)
    (write-string "x" stream)
    (prin1 (image-height image) stream)
    (write-string "x" stream)
    (prin1 (image-depth image) stream)))

(defconstant *empty-data-x* '#.(make-sequence '(array card8 (*)) 0))

(defconstant *empty-data-z*
             '#.(make-array '(0 0) :element-type 'pixarray-1-element-type))

(defstruct (image-x (:include image) (:copier nil)
                    ;(:print-function print-image)
                    )
  ;; Use this format for shoveling image data
  ;; Private structure. Accessors for these NOT exported.
  (format :z-pixmap :type (member :bitmap :xy-pixmap :z-pixmap))
  (bytes-per-line 0 :type card16)
  (bits-per-pixel 1 :type (member 1 4 8 16 24 32))
  (bit-lsb-first-p *image-bit-lsb-first-p* :type boolean)       ; Bit order
  (byte-lsb-first-p *image-byte-lsb-first-p* :type boolean)     ; Byte order
  (data *empty-data-x* :type (array card8 (*)))                 ; row-major
  (unit *image-unit* :type (member 8 16 32))                    ; Bitmap unit
  (pad *image-pad* :type (member 8 16 32))                      ; Scanline pad
  (left-pad 0 :type card8))                                     ; Left pad

(defstruct (image-xy (:include image) (:copier nil)
                     ;(:print-function print-image)
                     )
  ;; Public structure
  ;; Use this format for image processing
  (bitmap-list nil :type list)) ;; list of bitmaps

(defstruct (image-z (:include image) (:copier nil)
                    ;(:print-function print-image)
                    )
  ;; Public structure
  ;; Use this format for image processing
  (bits-per-pixel 1 :type (member 1 4 8 16 24 32))
  (pixarray *empty-data-z* :type pixarray))

(defun create-image (&key width height depth
                     (data (required-arg data))
                     plist name x-hot y-hot
                     red-mask blue-mask green-mask
                     bits-per-pixel format bytes-per-line
                     (byte-lsb-first-p (load-time-value (not sys::*big-endian*)))
                     (bit-lsb-first-p (load-time-value (not sys::*big-endian*)))
                     unit pad left-pad)
  ;; Returns an image-x image-xy or image-z structure, depending on the
  ;; type of the :DATA parameter.
  (declare
    (type (or null card16) width height)        ; Required
    (type (or null card8) depth)                ; Defualts to 1
    (type (or buffer-bytes                      ; Returns image-x
              list                              ; Returns image-xy
              pixarray) data)                   ; Returns image-z
    (type list plist)
    (type (or null stringable) name)
    (type (or null card16) x-hot y-hot)
    (type (or null pixel) red-mask blue-mask green-mask)
    (type (or null (member 1 4 8 16 24 32)) bits-per-pixel)

    ;; The following parameters are ignored for image-xy and image-z:
    (type (or null (member :bitmap :xy-pixmap :z-pixmap))
          format)                               ; defaults to :z-pixmap
    (type (or null card16) bytes-per-line)
    (type boolean byte-lsb-first-p bit-lsb-first-p)
    (type (or null (member 8 16 32)) unit pad)
    (type (or null card8) left-pad))
  (let ((image
          (etypecase data
            (buffer-bytes                       ; image-x
              (let ((data data))
                (declare (type buffer-bytes data))
                (unless depth (setq depth (or bits-per-pixel 1)))
                (unless format
                  (setq format (if (= depth 1) :xy-pixmap :z-pixmap)))
                (unless bits-per-pixel
                  (setq bits-per-pixel
                        (cond ((eq format :xy-pixmap) 1)
                              ((index> depth 24) 32)
                              ((index> depth 16) 24)
                              ((index> depth 8)  16)
                              ((index> depth 4)   8)
                              ((index> depth 1)   4)
                              (t                  1))))
                (unless width (required-arg width))
                (unless height (required-arg height))
                (unless bytes-per-line
                  (let* ((pad (or pad 8))
                         (bits-per-line (index* width bits-per-pixel))
                         (padded-bits-per-line
                           (index* (index-ceiling bits-per-line pad) pad)))
                    (declare (type array-index pad bits-per-line
                                   padded-bits-per-line))
                    (setq bytes-per-line (index-ceiling padded-bits-per-line 8))))
                (unless unit (setq unit *image-unit*))
                (unless pad
                  (setq pad
                        (dolist (pad '(32 16 8))
                          (when (and (index<= pad *image-pad*)
                                     (zerop
                                       (index-mod
                                         (index* bytes-per-line 8) pad)))
                            (return pad)))))
                (unless left-pad (setq left-pad 0))
                (make-image-x
                  :width width :height height :depth depth :plist plist
                  :format format :data data
                  :bits-per-pixel bits-per-pixel
                  :bytes-per-line bytes-per-line
                  :byte-lsb-first-p byte-lsb-first-p
                  :bit-lsb-first-p bit-lsb-first-p
                  :unit unit :pad pad :left-pad left-pad)))
            (list                               ; image-xy
              (let ((data data))
                (declare (type list data))
                (unless depth (setq depth (length data)))
                (when data
                  (unless width (setq width (array-dimension (car data) 1)))
                  (unless height (setq height (array-dimension (car data) 0))))
                (make-image-xy
                  :width width :height height :plist plist :depth depth
                  :bitmap-list data)))
            (pixarray                           ; image-z
              (let ((data data))
                (declare (type pixarray data))
                (unless width (setq width (array-dimension data 1)))
                (unless height (setq height (array-dimension data 0)))
                (unless bits-per-pixel
                  (setq bits-per-pixel
                        (etypecase data
                          (pixarray-32 32)
                          (pixarray-24 24)
                          (pixarray-16 16)
                          (pixarray-8   8)
                          (pixarray-4   4)
                          (pixarray-1   1)))))
              (unless depth (setq depth bits-per-pixel))
              (make-image-z
                :width width :height height :depth depth :plist plist
                :bits-per-pixel bits-per-pixel :pixarray data)))))
    (declare (type image image))
    (when name (setf (image-name image) name))
    (when x-hot (setf (image-x-hot image) x-hot))
    (when y-hot (setf (image-y-hot image) y-hot))
    (when red-mask (setf (image-red-mask image) red-mask))
    (when blue-mask (setf (image-blue-mask image) blue-mask))
    (when green-mask (setf (image-green-mask image) green-mask))
    image))

(defun bitmap-image (&optional plist &rest patterns)
  ;; Create an image containg pattern
  ;; PATTERNS are bit-vector constants (e.g. #*10101)
  ;; If the first parameter is a list, its used as the image property-list.
  (declare (type (or list bit-vector) plist)
           (type list patterns)) ;; list of bitvector
  (unless (listp plist)
    (push plist patterns)
    (setq plist nil))
  (let* ((width (length (first patterns)))
         (height (length patterns))
         (bitarray (make-array (list height width) :element-type 'bit))
         (row 0))
    (declare (type card16 width height row)
             (type pixarray-1 bitarray))
    (dolist (pattern patterns)
      (declare (type simple-bit-vector pattern))
      (dotimes (col width)
        (declare (type card16 col))
        (setf (aref bitarray row col) (the bit (aref pattern col))))
      (incf row))
    (create-image :width width :height height :plist plist :data bitarray)))

(defun image-pixmap (drawable image &key gcontext width height depth)
  ;; Create a pixmap containing IMAGE. Size defaults from the image.
  ;; DEPTH is the pixmap depth.
  ;; GCONTEXT is used for putting the image into the pixmap.
  ;; If none is supplied, then one is created, used then freed.
  (declare (type drawable drawable)
           (type image image)
           (type (or null gcontext) gcontext)
           (type (or null card16) width height)
           (type (or null card8) depth))
  (let* ((image-width (image-width image))
         (image-height (image-height image))
         (image-depth (image-depth image))
         (width (or width image-width))
         (height (or height image-height))
         (depth (or depth image-depth))
         (pixmap (create-pixmap :drawable drawable
                               :width width
                               :height height
                               :depth depth))
         (gc (or gcontext (create-gcontext
                            :drawable pixmap
                            :foreground 1
                            :background 0))))
    (unless (= depth image-depth)
      (if (= image-depth 1)
          (unless gcontext (xlib::required-arg gcontext))
        (error "Pixmap depth ~d incompatible with image depth ~d"
               depth image-depth)))
    (put-image pixmap gc image :x 0 :y 0 :bitmap-p (and (= image-depth 1) gcontext))
    ;; Tile when image-width is less than the pixmap width, or
    ;; the image-height is less than the pixmap height.
    ;; ??? Would it be better to create a temporary pixmap and
    ;; ??? let the server do the tileing?
    (do ((x image-width (+ x image-width)))
        ((>= x width))
      (copy-area pixmap gc 0 0 image-width image-height pixmap x 0)
      (incf image-width image-width))
    (do ((y image-height (+ y image-height)))
        ((>= y height))
      (copy-area pixmap gc 0 0 image-width image-height pixmap 0 y)
      (incf image-height image-height))
    (unless gcontext (free-gcontext gc))
    pixmap))

