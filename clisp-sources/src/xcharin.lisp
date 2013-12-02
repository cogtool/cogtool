;; Extended character input

;; This file provides the data type used by the keyboard stream,
;; and CLtL1 compatible character functions operating on font and bits.

(in-package "EXT")
(export '(char-bits char-font char-font-limit char-bits-limit
          char-control-bit char-meta-bit char-super-bit char-hyper-bit
          make-char char-bit set-char-bit char-key))

(in-package "SYSTEM")

(defconstant char-font-limit 16) ; for backward compatibility only
(defconstant char-bits-limit 16)
                   ;; CLtL1, p. 233, 234
(defconstant char-int-limit     ; CLISP specific
  (* char-code-limit char-font-limit char-bits-limit))

(defstruct input-character
  (char nil :type (or null character))
  (bits 0   :type (integer 0 #.(1- char-bits-limit)))
  (font 0   :type (integer 0 #.(1- char-font-limit)))
  (key  nil :type (or null character symbol))
)

(defconstant char-control-bit 1)
(defconstant char-meta-bit 2)
(defconstant char-super-bit 4)
(defconstant char-hyper-bit 8)
                   ;; CLtL1, p. 243

(defun fehler-char-arg (arg caller)
  (error-of-type 'type-error
    :datum arg :expected-type 'character
    (TEXT "~: argument ~S is not a character")
    caller arg
) )

(defun char-bits (char)
  (cond ((typep char 'character) 0)
        ((input-character-p char) (input-character-bits char))
        (t (fehler-char-arg char 'char-bits))
) )

(defun char-font (char)
  (cond ((typep char 'character) 0)
        ((input-character-p char) (input-character-font char))
        (t (fehler-char-arg char 'char-font))
) )

(defun char-key (char)
  (cond ((typep char 'character) char)
        ((input-character-p char) (input-character-key char))
        (t (fehler-char-arg char 'char-key))))

(defun test-font-arg (arg caller)
  (if (integerp arg)
    arg
    (error-of-type 'type-error
      :datum arg :expected-type 'integer
      (TEXT "~S: the font argument should be an integer, not ~S")
      caller arg
) ) )

(defun test-bits-arg (arg caller)
  (if (integerp arg)
    arg
    (error-of-type 'type-error
      :datum arg :expected-type 'integer
      (TEXT "~S: the bits argument should be an integer, not ~S")
      caller arg
) ) )

(defun make-char (char &optional (bits 0) (font 0))
  (test-font-arg font 'make-char)
  (test-bits-arg bits 'make-char)
  (unless (typep char 'character) (fehler-char-arg char 'make-char))
  (if (and (<= 0 bits) (< bits char-bits-limit)
           (<= 0 font) (< font char-font-limit))
    (make-input-character :char char :bits bits :font font)
    nil
) )

(defun test-bitname-arg (arg caller)
  (case arg
    (:CONTROL char-control-bit)
    (:META    char-meta-bit)
    (:SUPER   char-super-bit)
    (:HYPER   char-hyper-bit)
    (t (error-of-type 'type-error
         :datum arg :expected-type '(MEMBER :CONTROL :META :SUPER :HYPER)
         (TEXT "~S: the only bit names are ~S, ~S, ~S, ~S, not ~S")
         caller ':CONTROL ':META ':SUPER ':HYPER arg
) ) )  )

(defun char-bit (char name)
  (let ((bitmask (test-bitname-arg name 'char-bit)))
    (cond ((typep char 'character) nil)
          ((input-character-p char)
           (not (zerop (logand (input-character-bits char) bitmask))))
          (t (fehler-char-arg char 'char-bit))
) ) )

(defun set-char-bit (char name newvalue)
  (let ((bitmask (test-bitname-arg name 'char-bit))
        (new
          (cond ((typep char 'character) (make-input-character :char char))
                ((input-character-p char) (copy-input-character char))
                (t (fehler-char-arg char 'set-char-bit))
       )) )
    (if newvalue
      (setf (input-character-bits new)
            (logior (input-character-bits new) bitmask))
      (setf (input-character-bits new)
            (logand (input-character-bits new) (lognot bitmask)))
    )
    new
) )

;; Characters which had names in earlier versions of CLISP:
; WIN32_CHARNAMES
;  charname_hyper_13     (make-input-character :key :Enter)
;  charname_hyper_16     (make-input-character :key :Insert)
;  charname_hyper_17     (make-input-character :key :End)
;  charname_hyper_18     (make-input-character :key :Down)
;  charname_hyper_19     (make-input-character :key :PgDn)
;  charname_hyper_20     (make-input-character :key :Left)
;  charname_hyper_21     (make-input-character :key :Center)
;  charname_hyper_22     (make-input-character :key :Right)
;  charname_hyper_23     (make-input-character :key :Home)
;  charname_hyper_24     (make-input-character :key :Up)
;  charname_hyper_25     (make-input-character :key :PgUp)
;  charname_hyper_127    (make-input-character :key :Delete)
;  charname_hyper_a      (make-input-character :key :F1)
;  charname_hyper_b      (make-input-character :key :F2)
;  charname_hyper_c      (make-input-character :key :F3)
;  charname_hyper_d      (make-input-character :key :F4)
;  charname_hyper_e      (make-input-character :key :F5)
;  charname_hyper_f      (make-input-character :key :F6)
;  charname_hyper_g      (make-input-character :key :F7)
;  charname_hyper_h      (make-input-character :key :F8)
;  charname_hyper_i      (make-input-character :key :F9)
;  charname_hyper_j      (make-input-character :key :F10)
;  charname_hyper_k      (make-input-character :key :F11)
;  charname_hyper_l      (make-input-character :key :F12)
; UNIX_CHARNAMES
;  charname_hyper_16     (make-input-character :key :Insert)
;  charname_hyper_17     (make-input-character :key :End)
;  charname_hyper_18     (make-input-character :key :Down)
;  charname_hyper_19     (make-input-character :key :PgDn)
;  charname_hyper_20     (make-input-character :key :Left)
;  charname_hyper_21     (make-input-character :key :Center)
;  charname_hyper_22     (make-input-character :key :Right)
;  charname_hyper_23     (make-input-character :key :Home)
;  charname_hyper_24     (make-input-character :key :Up)
;  charname_hyper_25     (make-input-character :key :PgUp)
;  charname_hyper_a      (make-input-character :key :F1)
;  charname_hyper_b      (make-input-character :key :F2)
;  charname_hyper_c      (make-input-character :key :F3)
;  charname_hyper_d      (make-input-character :key :F4)
;  charname_hyper_e      (make-input-character :key :F5)
;  charname_hyper_f      (make-input-character :key :F6)
;  charname_hyper_g      (make-input-character :key :F7)
;  charname_hyper_h      (make-input-character :key :F8)
;  charname_hyper_i      (make-input-character :key :F9)
;  charname_hyper_j      (make-input-character :key :F10)
;  charname_hyper_k      (make-input-character :key :F11)
;  charname_hyper_l      (make-input-character :key :F12)
