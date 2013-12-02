;; -*- Lisp -*-
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "regexp/test")'

(listp (show (multiple-value-list (ext:module-info "regexp" t)) :pretty t)) T

(let ((rc (regexp:regexp-compile "a(a)*" :extended t)))
  (prog1 (multiple-value-list (regexp:regexp-exec rc "a"))
    (gc) (gc)))
(#S(REGEXP:MATCH :START 0 :END 1) NIL)

(ext:letf ((*apropos-matcher* #'regexp:regexp-matcher)
           (*misc-encoding* charset:utf-8)) ; handle non-ASCII symbols
  (apropos-list "regexp.*r$"))
(REGEXP:REGEXP-MATCHER)

#+ffi
(REGEXP:REGEXP-EXEC (ffi:foreign-pointer (ffi:unsigned-foreign-address 0)) "a")
#+ffi ERROR

;;; SDS: WARNING: the following tests are checking the underlying regexp
;;; implementation rather than CLISP regexp interface.
;;; a test failure should be reported to the regexp maintainer.
;;; a segmentation fault should be reported to <clisp-list>

;;; note also that some original tests had empty alternatives in grouping
;;; which is forbidden by POSIX
;;; <http://www.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap09.html>:
;;; "A vertical-line appearing first or last in an ERE, or immediately
;;;  following a vertical-line or a left-parenthesis, or immediately
;;;  preceding a right-parenthesis, produces undefined results."
;;; "Outside a bracket expression, a left-parenthesis immediately
;;;  followed by a right-parenthesis produces undefined results."

;; The following list of tests is taken from regexp-test-suite.lisp in
;; Kenneth Parker's regex package
;; http://www.geocities.com/mparker762/clawk.html#regex

;; Here is the licence of Parker's package:

#|
Copyright (c) 2000,2001,2002 Kenneth Michael Parker
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; This is the original header of regexp-test-suite.lisp:

;; Name             : regexp-test-suite.cl
;; Date             : 2002-03-01
;; Author           : Sébastien SAINT-SEVIN
;; Purpose          : testing module for regular expressions
;;
;; Modified by KMP to run under both rightmost and leftmost matches.

(defun re-test (pattern string) ;sds
  (mapcar (lambda (match)
            (and match (regexp:match-string string match)))
          (multiple-value-list
           (regexp:regexp-exec (regexp:regexp-compile pattern :extended t)
                               string))))
RE-TEST

;; *******************************************************
;; the tests that follows are from:
;; -------------------------------------------------------
;; (c) Sudhir Shenoy, 1996
;;
;; The tests here are from:
;;
;; (a) Tom Lord's GNU rx package
;; (b) from the Zebu parser generator package
;;     (modified to use new syntax)
;; -------------------------------------------------------
;; All have been slightly modified to follow the syntax
;; I use in this module - Sébastien Saint-Sevin, 2002
;; -------------------------------------------------------
(re-test "a*a*" "aaaaaa") ("aaaaaa")
(re-test "a*a*a*" "aaaaaa") ("aaaaaa")
(re-test "a*a*a*a*" "aaaaaa") ("aaaaaa")
(re-test "a*a*a*a*a*" "aaaaaa") ("aaaaaa")
(re-test "a*a*a*a*a*a*" "aaaaaa") ("aaaaaa")
(re-test "a*a*a*a*a*a*a*" "aaaaaa") ("aaaaaa")
(re-test "" "") ("")
(re-test "b{0,6}" "") ("")
(re-test "ab{0,0}c" "abc") ()
(re-test "ab{1,1}c" "abbc") ()
(re-test "ab{3,7}c" "abbbbbbbbc") ()
(re-test "ab{3,7}c" "abbbbbbbbbc") ()
(re-test "ab{3,7}c" "abbbbbbbbbbc") ()
(re-test "ab{3,7}c" "abbbbbbbbbbbc") ()
(re-test "b{2,7}" "bb") ("bb")
(re-test "b{1,6}" "") ()
(re-test "b{1,6}" "b") ("b")
(re-test "b{2,7}" "b") ()
(re-test "ab{0,7}c" "ac") ("ac")
(re-test "ab{1,7}c" "abc") ("abc")
(re-test "ab{2,7}c" "abbc") ("abbc")
(re-test "ab{3,7}c" "abbbc") ("abbbc")
(re-test "ab{3,7}c" "abbbbc") ("abbbbc")
(re-test "ab{3,7}c" "abbbbbc") ("abbbbbc")
(re-test "ab{3,7}c" "abbbbbbc") ("abbbbbbc")
(re-test "ab{3,7}c" "abbbbbbbc") ("abbbbbbbc")
(re-test "ab{3,7}c" "abbbbbbbbc") ()
(re-test "ab{3,7}c" "abbc") ()
(re-test "ab{3,7}c" "abc") ()

(re-test "(a|b)*c|(a|ab)*c" "xc") ("c" NIL NIL)
(re-test "(a)*" "b") ("" NIL)
(re-test "(..)*(...)*" "a") ("" NIL NIL)

;;the following fails coz sshenoy's engine is a posix NFA
(re-test "(..)*(...)*" "abc") ("abc" NIL "abc")
;; ("ab" "ab" "")

(re-test "^" "") ("")
(re-test "$" "") ("")
(re-test "^$" "") ("")
(re-test "^a$" "a") ("a")
(re-test "abc" "abc") ("abc")
(re-test "abc" "xbc") ()
(re-test "abc" "axc") ()
(re-test "abc" "abx") ()
(re-test "abc" "xabcy") ("abc")
(re-test "abc" "ababc") ("abc")
(re-test "ab*c" "abc") ("abc")
(re-test "ab*bc" "abc") ("abc")
(re-test "ab*bc" "abbc") ("abbc")
(re-test "ab*bc" "abbbbc") ("abbbbc")
(re-test "ab+bc" "abbc") ("abbc")
(re-test "ab+bc" "abc") ()
(re-test "ab+bc" "abq") ()
(re-test "ab+bc" "abbbbc") ("abbbbc")
(re-test "ab?bc" "abbc") ("abbc")
(re-test "ab?bc" "abc") ("abc")
(re-test "ab?bc" "abbbbc") ()
(re-test "ab?c" "abc") ("abc")
(re-test "^abc$" "abc") ("abc")
(re-test "^abc$" "abcc") ()
(re-test "^abc" "abcc") ("abc")
(re-test "^abc$" "aabc") ()
(re-test "abc$" "aabc") ("abc")
(re-test "^" "abc") ("")
(re-test "$" "abc") ("")
(re-test "a.c" "abc") ("abc")
(re-test "a.c" "axc") ("axc")
(re-test "a.*c" "axyzc") ("axyzc")
(re-test "a.*c" "axyzd") ()

(re-test "a[bc]d" "abc") ()
(re-test "a[bc]d" "abd") ("abd")
(re-test "a[b-d]e" "abd") ()
(re-test "a[b-d]e" "ace") ("ace")
(re-test "a[b-d]" "aac") ("ac")
(re-test "a[-b]" "a-") ("a-")
(re-test "a[b-]" "a-") ("a-")

;;*** following is supposed to compile but what should it match ?
;;*** I don't know and that is why I reject the pattern.
;; sds: gnulib rejects these
(re-test "a[b-a]" "-") ERROR ; ("a[b-a]"): "Invalid range end"
(re-test "a[]b" "-") ERROR   ; ("a[]b"): "Unmatched [ or [^"
(re-test "a[" "-") ERROR     ; ("a["):  "Unmatched [ or [^"
(re-test "a]" "a]") ("a]")
(re-test "a[]]b" "a]b") ("a]b")

(re-test "a[^bc]d" "aed") ("aed")
(re-test "a[^bc]d" "abd") ()
(re-test "a[^-b]c" "adc") ("adc")
(re-test "a[^-b]c" "a-c") ()
(re-test "a[^]b]c" "a]c") ()
(re-test "a[^]b]c" "adc") ("adc")
(re-test "ab|cd" "abc") ("ab")
(re-test "ab|cd" "abcd") ("ab")

(re-test "()ef" "def") ("ef" "")
(re-test "()*" "-") ("" "")
(re-test "*a" "-") ERROR    ; ("*a"): "Invalid preceding regular expression"
(re-test "^*" "-") ERROR    ; ("^*"): "Invalid preceding regular expression"
(re-test "$*" "-") ERROR    ; ("$*"): "Invalid preceding regular expression"
(re-test "(*)b" "-") ERROR  ; ("(*)b"): "Invalid preceding regular expression"
(re-test "$b" "b") ()

(re-test "a\\(b" "a(b") ("a(b")
(re-test "a\\(*b" "ab") ("ab")
(re-test "a\\(*b" "a((b") ("a((b")
(re-test "a\\\\b" "a\\b") ("a\\b")
(re-test "(abc" "-") ERROR ; ("(abc"): "Unmatched ( or \\("
(re-test "((a))" "abc") ("a" "a" "a")
(re-test "(a)b(c)" "abc") ("abc" "a" "c")
(re-test "a+b+c" "aabbabc") ("abc")

(re-test "a**" "-") ("")
(re-test "(a*)*" "-") ("" "")
(re-test "(a*)+" "-") ("" "")
;; non-POSIX (re-test "(a|)*" "-") ("" "")
(re-test "(a*|b)*" "-") ("" "")
(re-test "(a+|b)*" "ab") ("ab" #-:regex-left "b" #+:regex-left "a")
(re-test "(a+|b)+" "ab") ("ab" #-:regex-left "b" #+:regex-left "a")
(re-test "(a+|b)?" "ab") ("a"  "a")
(re-test "[^ab]*" "cde") ("cde")
(re-test "(^)*" "-") ("" NIL)
;; non-POSIX (re-test "(ab|)*" "-") ("" "")
(re-test ")(" "-") ; ()
ERROR ; (")("): "Unmatched ( or \\("
(re-test "" "abc") ("")
(re-test "abc" "") ()
(re-test "a*" "") ("")
(re-test "([abc])*d" "abbbcd") ("abbbcd" #-:regex-left "c" #+:regex-left "a")
(re-test "([abc])*bcd" "abcd") ("abcd"   "a")
(re-test "a|b|c|d|e" "e") ("e")
(re-test "(a|b|c|d|e)f" "ef") ("ef" "e")
(re-test "((a*|b))*" "-") ("" "" "")
(re-test "abcd*efg" "abcdefg") ("abcdefg")
(re-test "ab*" "xabyabbbz") ("ab")
(re-test "ab*" "xayabbbz") ("a")
(re-test "(ab|cd)e" "abcde") ("cde" "cd")
(re-test "[abhgefdc]ij" "hij") ("hij")
(re-test "^(ab|cd)e" "abcde") ()
;; non-POSIX (re-test "(abc|)ef" "abcdef") ("ef" "")
(re-test "(a|b)c*d" "abcd") ("bcd" "b")
(re-test "(ab|ab*)bc" "abc") ("abc" "a")
(re-test "a([bc]*)c*" "abc") ("abc"  "bc")
(re-test "a([bc]*)(c*d)" "abcd") ("abcd" "bc" "d")
(re-test "a([bc]+)(c*d)" "abcd") ("abcd" "bc" "d")
(re-test "a([bc]*)(c+d)" "abcd") ("abcd" "b" "cd")
(re-test "a[bcd]*dcdcde" "adcdcde") ("adcdcde")
(re-test "a[bcd]+dcdcde" "adcdcde") ()
(re-test "(ab|a)b*c" "abc") ("abc"  "ab")
(re-test "((a)(b)c)(d)" "abcd") ("abcd" "abc" "a" "b" "d")
(re-test "[a-zA-Z_][a-zA-Z0-9_]*" "alpha") ("alpha")
(re-test "^a(bc+|b[eh])g|.h$" "abh") ("bh" NIL)
(re-test "(bc+d$|ef*g.|h?i(j|k))" "effgz") ("effgz" "effgz" NIL)
(re-test "(bc+d$|ef*g.|h?i(j|k))" "ij") ("ij" "ij" "j")
(re-test "(bc+d$|ef*g.|h?i(j|k))" "effg") ()
(re-test "(bc+d$|ef*g.|h?i(j|k))" "bcdd") ()
(re-test "(bc+d$|ef*g.|h?i(j|k))" "reffgz") ("effgz" "effgz" NIL)


(re-test "((((((((((a))))))))))" "a") ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a")
(re-test "(((((((((a)))))))))" "a") ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a")
(re-test "multiple words of text" "uh-uh") ()
(re-test "multiple words" "multiple words, yeah") ("multiple words")
(re-test "(.*)c(.*)" "abcde") ("abcde" "ab" "de")
(re-test "\\((.*), (.*)\\)" "(a, b)") ("(a, b)" "a" "b")
(re-test "[k]" "ab") ()
(re-test "abcd" "abcd") ("abcd")
(re-test "a(bc)d" "abcd") ("abcd" "bc")
(re-test "a[-]?c" "ac") ("ac")
(re-test "a[-]?c" "ac") ("ac")
(re-test "a[-]?c" "ac") ("ac")
(re-test "[ -~]*" "abc") ("abc")
(re-test "[ -~ -~]*" "abc") ("abc")
(re-test "[ -~ -~ -~]*" "abc") ("abc")
(re-test "[ -~ -~ -~ -~]*" "abc") ("abc")
(re-test "[ -~ -~ -~ -~ -~]*" "abc") ("abc")
(re-test "[ -~ -~ -~ -~ -~ -~]*" "abc") ("abc")
(re-test "[ -~ -~ -~ -~ -~ -~ -~]*" "abc") ("abc")

;; Tests from from the Zebu package (originally for nregex.lisp)
(re-test "(na)x+" "naxna") ("nax" "na")
(re-test "(na)x+na" "naxna123") ("naxna" "na")
(re-test "(na)x+" "naxxos") ("naxx" "na")
(re-test "(na)x+" "naxos") ("nax" "na")
(re-test "(na)x+" "naos") ()
(re-test "(na)x*" "naxxos") ("naxx" "na")
(re-test "(na)x*" "naxos") ("nax" "na")
(re-test "(na)x*" "naos") ("na" "na")
(re-test "[0-9]+" "123ab") ("123")
(re-test "[a-zA-Z]+" "aAbb123") ("aAbb")
(re-test "[0-9a-z]+" "1234&&*") ("1234")
(re-test "[0-9a-z]+" "1234a&&*") ("1234a")
(re-test "[0-9a-zA-Z]+" "a1234a") ("a1234a")
(re-test "[0-9a-zA-Z&]+" "aAbb123&&*") ("aAbb123&&")
(re-test "[0-9]+\\.[0-9]*" "0.123cm") ("0.123")

(re-test "{[^}
]*}" "{M.D. Harrison and A. Monk (Ed.)} \n\t foo: 2")
;; ("{M.D. Harrison and A. Monk (Ed.)}")
ERROR                           ;  "Invalid content of \\{\\}"
(re-test "{[^}
]*}" "{M.D. Harrison and
A. Monk (Ed.)} \n\t foo: 2")
ERROR                           ; "Invalid content of \\{\\}"
(re-test "{[^}
]*}" "{M.D. Harrison and {A. Monk} (Ed.)} \n\t foo: 2")
;; ("{M.D. Harrison and {A. Monk}")
ERROR                           ; "Invalid content of \\{\\}"

(re-test "ca?r" "car") ("car")
(re-test "ca?r" "cr") ("cr")
(re-test "c[ad]+r" "caaar") ("caaar")
(re-test "c[ad]+r" "caaar aa1") ("caaar")
(re-test "c[ad]+r$" "caaar") ("caaar")
(re-test ".*" "") ("")
(re-test ".*" "aa") ("aa")
(re-test "c[ad]?r" "cr") ("cr")
(re-test "c[ad]?r" "car") ("car")
(re-test "c[ad]?r" "cdr") ("cdr")
(re-test "c[0-9]?r" "cr") ("cr")
(re-test "c[0-9]?r" "c9rxx") ("c9r")
(re-test "c[0-9]?r" "crxx") ("cr")
(re-test "a|b" "a") ("a")
(re-test "ab.yz" "ab yz") ("ab yz")
(re-test "ab.yz"   "ab
yz") ("ab
yz")

(re-test "(abc){1,2}" "abcabc") ("abcabc" "abc")
(re-test "(abc){1,2}x*(def)y*def" "abcabcxxxxdefyyyyyyydef$%%%%%")
("abcabcxxxxdefyyyyyyydef" "abc" "def")
(re-test "a|bc*" "a") ("a")
(re-test "[A-Z]+" "ABCY") ("ABCY")
(re-test "[0-9]+\\.[0-9]*(e[+-]?[0-9]+)" "12.3e4  k") ("12.3e4" "e4")
(re-test "[0-9]+\\.[0-9]*(e[+-]?[0-9]+)" "12.3e-4  k") ("12.3e-4" "e-4")
(re-test "[0-9]+\\.[0-9]*(e[+-]?[0-9]+)?" "12.3  k") ("12.3" NIL)

;; The Gadaffi tests
;; Note that the first group matches NULL because it is always sucked
;; up by the preceding .* in case of a successful match.
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Qaddafi") ("Muammar Qaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mo'ammar Gadhafi") ("Mo'ammar Gadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Kaddafi") ("Muammar Kaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Qadhafi") ("Muammar Qadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moammar El Kadhafi") ("Moammar El Kadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Gadafi") ("Muammar Gadafi" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar al-Qadafi") ("Mu'ammar al-Qadafi" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moamer El Kazzafi") ("Moamer El Kazzafi" NIL "zz")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moamar al-Gaddafi") ("Moamar al-Gaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar Al Qathafi") ("Mu'ammar Al Qathafi" NIL "th")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Al Qathafi") ("Muammar Al Qathafi" NIL "th")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mo'ammar el-Gadhafi") ("Mo'ammar el-Gadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moamar El Kadhafi") ("Moamar El Kadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar al-Qadhafi") ("Muammar al-Qadhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar al-Qadhdhafi") ("Mu'ammar al-Qadhdhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar Qadafi") ("Mu'ammar Qadafi" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moamar Gaddafi") ("Moamar Gaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar Qadhdhafi") ("Mu'ammar Qadhdhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Khaddafi") ("Muammar Khaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar al-Khaddafi") ("Muammar al-Khaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'amar al-Kadafi") ("Mu'amar al-Kadafi" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Ghaddafy") ("Muammar Ghaddafy" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Ghadafi") ("Muammar Ghadafi" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Ghaddafi") ("Muammar Ghaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muamar Kaddafi") ("Muamar Kaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Quathafi") ("Muammar Quathafi" NIL "th")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muammar Gheddafi") ("Muammar Gheddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Muamar Al-Kaddafi") ("Muamar Al-Kaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moammar Khadafy ") ("Moammar Khadafy" NIL "d")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Moammar Qudhafi") ("Moammar Qudhafi" NIL "dh")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mu'ammar al-Qaddafi") ("Mu'ammar al-Qaddafi" NIL "dd")
(re-test "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
         "Mulazim Awwal Mu'ammar Muhammad Abu Minyar al-Qadhafi")
("Mu'ammar Muhammad Abu Minyar al-Qadhafi" NIL "dh")

;; tests involving back-refs
(re-test "((a|b{10,11})|(b))*-\\2" "aaab-a")
("aaab-a" #-:regex-left "b" #+:regex-left "a" "a" "b")
(re-test "(a)*-\\1" "aaa-a") ("aaa-a" "a")
(re-test "(a)*-\\1b" "aaa-b") ()
(re-test "([xyz])(-\\2)" "x-y") ERROR ; "Invalid back reference"
(re-test "(([xyz])(-\\2))" "x-y") ()
(re-test "(([xyz])(-\\2)*)*" "x-y") ("x" "x" "x" NIL)
(re-test "(([xyz])(-\\2)*)*" "x-") ("x" "x" "x" NIL)
(re-test "(([xyz])(-\\2)*)*" "xy-yz-y")
#-:regex-left ("xy-yz" "z" "z" "-y")
#+:regex-left ("xy" "y" "y" NIL)

;; kmp -- this *should* match
(re-test "((.*)\\1)+" "xxxxxx") ERROR ; "Invalid back reference"
;; (re-test "((.*)\\1)+" "xxxxxx")
;; #-:regex-left ("xxxxxx" "" "")
;; #+:regex-left ("xxxxxx" "xxxxxx" "xxx")

(re-test "(a*)\\1\\1(a*)\\2\\2\\2" "aaaaaa") ("aaaaaa" "aa" "")
(re-test "(a*)(a*)\\1\\2" "aaaa") ("aaaa" "aa" "")
(re-test "(a*)\\1(a*)\\2\\2" "aaaa") ("aaaa" "aa" "")
(re-test "(a*)\\1\\1(a*)" "aaaaaa") ("aaaaaa" "aa" "")
(re-test "(a*)\\1\\1(a*)\\2" "aaaaaa") ("aaaaaa" "aa" "")
(re-test "(a*)\\1\\1(a*)\\2\\2" "aaaaaa") ("aaaaaa" "aa" "")
(re-test "(.*)\\1\\1(.*)\\2\\2\\2" "aaaaaa") ("aaaaaa" "aa" "")
;;the following fails coz sshenoy's engine is a posix NFA
(re-test "(.*)\\1\\1(.*)\\2\\2\\2" "aaaaaaa") ("aaaaaaa" "a" "a")
;; alternative: ("aaaaaa" "aa" "")
;;the following fails coz sshenoy's engine is a posix NFA
(re-test "(.*)\\1\\1(.*)\\2\\2\\2" "aaaaa") ("aaaa" "" "a")
;; alternative: ("aaa" "a" "")
(re-test "(.*)\\1\\1" "aaa") ("aaa" "a")
(re-test "(.*)*\\1" "xx") ("xx" #-:regex-left "x" #+:regex-left "")
(re-test "(....).*\\1" "beriberi") ("beriberi" "beri")

;; Some tests for class matches (my own)
(re-test "[[:alpha:]_][[:alnum:]_]*" "c_identifier") ("c_identifier")
(re-test "[[:xdigit:]]*" "12aBcD89") ("12aBcD89")
;; undefined behavior:
;;(re-test "[[:xdigit]+" "0[x:dig") ("[x:dig")

;; *******************************************************
;; the tests that follows are from:
;; -------------------------------------------------------
;; Sébastien Saint-Sevin, 2002
;; -------------------------------------------------------

;; some basics
(re-test ".*" "aa") ("aa")
(re-test ".+" "aa") ("aa")

;; alternate
;; non-POSIX (re-test "(hello|man|)" "") ("" "")
(re-test "(a+|b)" "aaa") ("aaa" "aaa")
(re-test "(a+|b)" "b") ("b" "b")

;; character classes
(re-test "[abc]{1,3}" "bcaa") ("bca")

(re-test "a[\\-]?c" "ac") ("ac")
(re-test "a[\\-]?c" "a-c") ("a-c")
(re-test "a[-]?c" "ac") ("ac")
(re-test "a[-]?c" "a-c") ("a-c")
(re-test "a[-b]?c" "abc") ("abc")
(re-test "a[b-]?c" "acc") ("ac")

(re-test "a[^\\-]?c" "ac") ("ac")
(re-test "a[^\\-]?c" "a-c") ()
(re-test "a[^-]?c" "ac") ("ac")
(re-test "a[^-]?c" "azc") ("azc")
(re-test "a[^-b]?c" "adc") ("adc")
(re-test "a[^b-]?c" "acc") ("acc")

;; posix character classes

;; greedy quantifiers
(re-test "a*" "aaaa") ("aaaa")
(re-test "a+" "aaaa") ("aaaa")
(re-test "a{2,3}" "aaaa") ("aaa")

;; nongreedy quantifiers
;; the correct behavior is #+non-greedy which means that the regex engine has
;; non-greedy support; the 1998 regex that comes with clisp does not support it
(re-test "a*?" "aaaa") #+non-greedy ("") #-non-greedy ("aaaa")
(re-test "a+?" "aaaa") #+non-greedy ("a") #-non-greedy ("aaaa")
(re-test "a{2,3}?" "aaaa") #+non-greedy ("aa") #-non-greedy ("aaa")
(re-test "a+?bb*?" "baaaabaaabbbaaaaa") #+non-greedy("aaaab") #-non-greedy("b")
(re-test "a+?bb+?" "baaaabaaabbbaaaaa") #+non-greedy("aaabb") #-non-greedy("b")
(re-test "[abc]{10,20}?" "xxxbcbcbabcaabcbabcbcbabcbcaabcabxxx")
#+non-greedy ("bcbcbabcaa") #-non-greedy ("")

;; grouping

;; nonregister grouping

;; greedy quantifiers + backrefs
(re-test "^(x)+$" "xx") ("xx" "x")
(re-test "^(x)+\\1$" "xx") ("xx" "x")
(re-test "^(x){1,2}$" "xx") ("xx" "x")
(re-test "^(x){1,2}\\1$" "xx") ("xx" "x")
(re-test "^(x)+[^x]+\\1$" "xxaax") ("xxaax" "x")
(re-test "^x*(x)[^x]+\\1$" "xxaax") ("xxaax" "x")

(re-test "(x)+\\1" "xxxx") ("xxxx" "x")
(re-test "(x){1,2}" "xxxx") ("xx" "x")
;; kmp By the letter, (x) can only match one character.  To get this
;;     affect, the pattern should be "(x{1,2})\\1"
(re-test "(x){1,2}\\1" "xxxx") ("xxx" "x")

(re-test "(x)+[^x]+\\1" "xxaax") ("xxaax" "x")
(re-test "x*(x)[^x]+\\1" "xxaax") ("xxaax" "x")

;; nongreedy quantifiers + backrefs
(re-test "(x)+?\\1" "xxxx") (#+non-greedy "xx" #-non-greedy "xxxx" "x")
(re-test "(x){1,2}?" "xxxx") (#+non-greedy "x" #-non-greedy "xx" "x")
(re-test "(x){1,2}?\\1" "xxxx") (#+non-greedy "xx" #-non-greedy "xxx" "x")
(re-test "(x)+?[^x]+\\1" "xxaax") ("xxaax" "x")
(re-test "x*?(x)[^x]+\\1" "xxaax") ("xxaax" "x")

;; misc
;; kmp it is legal for a* to match nothing
(re-test "(a*)*" "aaaa") ("aaaa" #-:regex-left "aaaa" #+:regex-left "")
;; kmp it is legal for a* to match nothing
(re-test "(a*)+" "aaaa") ("aaaa" #-:regex-left "aaaa" #+:regex-left "")
(re-test "(a+)*" "aaaa") ("aaaa" "aaaa")

;; https://sourceforge.net/tracker/?func=detail&atid=101355&aid=1364177&group_id=1355
(regexp:regexp-split "|" "a|b" :extended t) error
