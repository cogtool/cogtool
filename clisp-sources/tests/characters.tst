;; -*- Lisp -*-

CHAR-CODE-LIMIT
#+XCL 128 #+(or (and CLISP (not UNICODE)) AKCL ECL CMU SBCL OpenMCL) 256 #+(or ALLEGRO LISPWORKS) 65536 #+(and CLISP UNICODE) 1114112 #-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(STANDARD-CHAR-P #\a)
T

(STANDARD-CHAR-P #\$)
T

(STANDARD-CHAR-P #\.)
T

(STANDARD-CHAR-P #\A)
T

(STANDARD-CHAR-P 1)
ERROR

(STANDARD-CHAR-P #\\)
T

(STANDARD-CHAR-P #\5)
T

(STANDARD-CHAR-P #\))
T

(STANDARD-CHAR-P #\%)
T

(STANDARD-CHAR-P #\BACKSPACE)
#+XCL T #-XCL NIL

(STANDARD-CHAR-P #\PAGE)
#+XCL T #-XCL NIL

(STANDARD-CHAR-P #\RETURN)
#+XCL T #-XCL NIL

(GRAPHIC-CHAR-P #\a)
T

(GRAPHIC-CHAR-P #\$)
T

(GRAPHIC-CHAR-P #\.)
T

(GRAPHIC-CHAR-P #\A)
T

(GRAPHIC-CHAR-P 1)
ERROR

(GRAPHIC-CHAR-P #\\)
T

(GRAPHIC-CHAR-P #\5)
T

(GRAPHIC-CHAR-P #\))
T

(GRAPHIC-CHAR-P #\%)
T

(GRAPHIC-CHAR-P #\BACKSPACE)
NIL

(GRAPHIC-CHAR-P #\PAGE)
NIL

(GRAPHIC-CHAR-P #\RETURN)
NIL

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\a)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\$)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\.)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\A)
T

#-(or CMU SBCL OpenMCL LISPWORKS)
(STRING-CHAR-P 1)
#-(or CMU SBCL OpenMCL LISPWORKS)
ERROR

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\\)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\5)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\))
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\%)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\BACKSPACE)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\PAGE)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\RETURN)
T

(ALPHA-CHAR-P #\a)
T

(ALPHA-CHAR-P #\$)
NIL

(ALPHA-CHAR-P #\.)
NIL

(ALPHA-CHAR-P #\A)
T

(ALPHA-CHAR-P 1)
ERROR

(ALPHA-CHAR-P #\\)
NIL

(ALPHA-CHAR-P #\5)
NIL

(ALPHA-CHAR-P #\))
NIL

(ALPHA-CHAR-P #\%)
NIL

(ALPHA-CHAR-P #\BACKSPACE)
NIL

(ALPHA-CHAR-P #\PAGE)
NIL

(ALPHA-CHAR-P #\RETURN)
NIL

(UPPER-CASE-P #\a)
NIL

(UPPER-CASE-P #\$)
NIL

(UPPER-CASE-P #\.)
NIL

(UPPER-CASE-P #\A)
T

(UPPER-CASE-P 1)
ERROR

(UPPER-CASE-P #\\)
NIL

(UPPER-CASE-P #\5)
NIL

(UPPER-CASE-P #\))
NIL

(UPPER-CASE-P #\%)
NIL

(UPPER-CASE-P #\BACKSPACE)
NIL

(UPPER-CASE-P #\PAGE)
NIL

(UPPER-CASE-P #\RETURN)
NIL

(LOWER-CASE-P #\a)
T

(LOWER-CASE-P #\$)
NIL

(LOWER-CASE-P #\.)
NIL

(LOWER-CASE-P #\A)
NIL

(LOWER-CASE-P 1)
ERROR

(LOWER-CASE-P #\\)
NIL

(LOWER-CASE-P #\5)
NIL

(LOWER-CASE-P #\))
NIL

(LOWER-CASE-P #\%)
NIL

(LOWER-CASE-P #\BACKSPACE)
NIL

(LOWER-CASE-P #\PAGE)
NIL

(LOWER-CASE-P #\RETURN)
NIL

(BOTH-CASE-P #\a)
T

(BOTH-CASE-P #\$)
NIL

(BOTH-CASE-P #\.)
NIL

(BOTH-CASE-P #\A)
T

(BOTH-CASE-P 1)
ERROR

(BOTH-CASE-P #\\)
NIL

(BOTH-CASE-P #\5)
NIL

(BOTH-CASE-P #\))
NIL

(BOTH-CASE-P #\%)
NIL

(BOTH-CASE-P #\BACKSPACE)
NIL

(BOTH-CASE-P #\PAGE)
NIL

(BOTH-CASE-P #\RETURN)
NIL

(DIGIT-CHAR-P #\a)
NIL

(DIGIT-CHAR-P #\$)
NIL

(DIGIT-CHAR-P #\.)
NIL

(DIGIT-CHAR-P #\A)
NIL

(DIGIT-CHAR-P 1)
ERROR

(DIGIT-CHAR-P #\\)
NIL

(DIGIT-CHAR-P #\5)
5

(DIGIT-CHAR-P #\))
NIL

(DIGIT-CHAR-P #\%)
NIL

(DIGIT-CHAR-P #\BACKSPACE)
NIL

(DIGIT-CHAR-P #\PAGE)
NIL

(DIGIT-CHAR-P #\RETURN)
NIL

(DIGIT-CHAR-P #\5 4)
NIL

(DIGIT-CHAR-P #\5 8)
5

(DIGIT-CHAR-P #\E 16)
14

(DIGIT-CHAR-P #\R 35)
27

(DIGIT-CHAR-P #\5 4)
NIL

(DIGIT-CHAR-P #\5 5)
NIL

(DIGIT-CHAR-P #\5 6)
5

(DIGIT-CHAR-P #\1 2)
1

(ALPHANUMERICP #\a)
T

(ALPHANUMERICP #\$)
NIL

(ALPHANUMERICP #\.)
NIL

(ALPHANUMERICP #\A)
T

(ALPHANUMERICP 1)
ERROR

(ALPHANUMERICP #\\)
NIL

(ALPHANUMERICP #\5)
T

(ALPHANUMERICP #\))
NIL

(ALPHANUMERICP #\%)
NIL

(ALPHANUMERICP #\BACKSPACE)
NIL

(ALPHANUMERICP #\PAGE)
NIL

(ALPHANUMERICP #\RETURN)
NIL

(ALPHANUMERICP #\5 4)
ERROR

(ALPHANUMERICP #\5 8)
ERROR

(ALPHANUMERICP #\E 16)
ERROR

(ALPHANUMERICP #\R 35)
ERROR

(CHAR= #\d #\d)
T

(CHAR/= #\d #\d)
NIL

(CHAR= #\d #\x)
NIL

(CHAR/= #\d #\x)
T

(CHAR= #\d #\D)
NIL

(CHAR/= #\d #\D)
T

(CHAR= #\d #\d #\d #\d)
T

(CHAR/= #\d #\d #\d #\d)
NIL

(CHAR= #\d #\d #\x #\d)
NIL

(CHAR/= #\d #\d #\x #\d)
NIL

(CHAR= #\d #\y #\x #\c)
NIL

(CHAR/= #\d #\y #\x #\c)
T

(CHAR= #\d #\c #\d)
NIL

(CHAR/= #\d #\c #\d)
NIL

(CHAR< #\d #\x)
T

(CHAR<= #\d #\x)
T

(CHAR< #\d #\d)
NIL

(CHAR<= #\d #\d)
T

(CHAR< #\a #\e #\y #\z)
T

(CHAR<= #\a #\e #\y #\z)
T

(CHAR< #\a #\e #\e #\y)
NIL

(CHAR<= #\a #\e #\e #\y)
T

(CHAR> #\e #\d)
T

(CHAR>= #\e #\d)
T

(CHAR> #\d #\c #\b #\a)
T

(CHAR>= #\d #\c #\b #\a)
T

(CHAR> #\d #\d #\b #\a)
NIL

(CHAR>= #\d #\d #\b #\a)
T

(CHAR> #\e #\d #\b #\c #\a)
NIL

(CHAR>= #\e #\d #\b #\c #\a)
NIL

(CHAR> #\z #\A)
T

(CHAR> #\Z #\a)
NIL

(CHAR< #\9 #\a)
T

(CHAR> #\9 #\a)
NIL

(CHAR> #\z #\0)
T

(CHAR< #\z #\0)
NIL

(CHAR-EQUAL #\d #\d)
T

(CHAR-NOT-EQUAL #\d #\d)
NIL

(CHAR-EQUAL #\d #\x)
NIL

(CHAR-NOT-EQUAL #\d #\x)
T

(CHAR-EQUAL #\d #\D)
T

(CHAR-NOT-EQUAL #\d #\D)
NIL

(CHAR-EQUAL #\d #\d #\d #\d)
T

(CHAR-NOT-EQUAL #\d #\d #\d #\d)
NIL

(CHAR-EQUAL #\d #\d #\x #\d)
NIL

(CHAR-NOT-EQUAL #\d #\d #\x #\d)
NIL

(CHAR-EQUAL #\d #\y #\x #\c)
NIL

(CHAR-NOT-EQUAL #\d #\y #\x #\c)
T

(CHAR-EQUAL #\d #\c #\d)
NIL

(CHAR-NOT-EQUAL #\d #\c #\d)
NIL

(CHAR-LESSP #\d #\x)
T

(CHAR-NOT-GREATERP #\d #\x)
T

(CHAR-LESSP #\d #\d)
NIL

(CHAR-NOT-GREATERP #\d #\d)
T

(CHAR-LESSP #\a #\e #\y #\z)
T

(CHAR-NOT-GREATERP #\a #\e #\y #\z)
T

(CHAR-LESSP #\a #\e #\e #\y)
NIL

(CHAR-NOT-GREATERP #\a #\e #\e #\y)
T

(CHAR-GREATERP #\e #\d)
T

(CHAR-NOT-LESSP #\e #\d)
T

(CHAR-GREATERP #\d #\c #\b #\a)
T

(CHAR-NOT-LESSP #\d #\c #\b #\a)
T

(CHAR-GREATERP #\d #\d #\b #\a)
NIL

(CHAR-NOT-LESSP #\d #\d #\b #\a)
T

(CHAR-GREATERP #\e #\d #\b #\c #\a)
NIL

(CHAR-NOT-LESSP #\e #\d #\b #\c #\a)
NIL

(CHAR-GREATERP #\z #\A)
T

(CHAR-GREATERP #\Z #\a)
T

(CHAR-LESSP #\9 #\a)
T

(CHAR-GREATERP #\9 #\a)
NIL

(CHAR-GREATERP #\z #\0)
T

(CHAR-LESSP #\z #\0)
NIL

(CHAR-EQUAL #\A #\a)
T

(CHAR-UPCASE #\a)
#\A

(CHAR-UPCASE #\A)
#\A

(CHAR-UPCASE #\5)
#\5

(CHAR-UPCASE #\;)
#\;

(CHAR-UPCASE #\=)
#\=

(CHAR= (CHAR-DOWNCASE (CHAR-UPCASE #\x)) #\x)
T

(CHAR-DOWNCASE #\A)
#\a

(CHAR-DOWNCASE #\a)
#\a

(CHAR-DOWNCASE #\%)
#\%

(CHAR-DOWNCASE #\+)
#\+

(CHAR-DOWNCASE #\-)
#\-

(CHAR= (CHAR-UPCASE (CHAR-DOWNCASE #\X)) #\X)
T

(DIGIT-CHAR 7)
#\7

(DIGIT-CHAR 12)
NIL

(DIGIT-CHAR (QUOTE A))
ERROR

(DIGIT-CHAR 12 16)
#\C

(DIGIT-CHAR 6 2)
NIL

(DIGIT-CHAR 1 2)
#\1

CHAR-CONTROL-BIT
#-(or CMU SBCL OpenMCL) 1 #+(or CMU SBCL OpenMCL) ERROR

CHAR-META-BIT
#-(or CMU SBCL OpenMCL) 2 #+(or CMU SBCL OpenMCL) ERROR

CHAR-SUPER-BIT
#-(or CMU SBCL OpenMCL) 4 #+(or CMU SBCL OpenMCL) ERROR

CHAR-HYPER-BIT
#-(or CMU SBCL OpenMCL) 8 #+(or CMU SBCL OpenMCL) ERROR

(char-name #\Space) "Space"

(char-name #\Newline) "Newline"

(let ((wrong-codes nil))
  (dotimes (code char-code-limit)
    (let ((c (code-char code)))
      (unless (and #-CLISP (or (graphic-char-p c) (char-name c))
                   (or #-CLISP (null (char-name c))
                       (eql c (name-char (char-name c)))))
        (push code wrong-codes))))
  wrong-codes)
NIL

;; from GCL ansi-test
(loop :for i :from 0 :below char-code-limit
  :for x = (code-char i)
  :unless (or (not (characterp x))
              (if (or (digit-char-p x) (alpha-char-p x))
                  (alphanumericp x)
                  (not (alphanumericp x))))
  :collect (list i x :digit (digit-char-p x) :alpha (alpha-char-p x)
                 :alphanumericp (alphanumericp x)))
nil

;; Check that sys::char-invertcase is an involution
#+CLISP
(locally (declare (compile))
  (loop :for i :from 0 :below char-code-limit
        :for x = (code-char i)
        :unless (eq (sys::char-invertcase (sys::char-invertcase x)) x)
        :collect x))
#+CLISP
NIL
