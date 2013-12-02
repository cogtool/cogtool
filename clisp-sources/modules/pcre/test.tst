;; -*- Lisp -*-
;; some tests for PCRE
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "pcre/test")'

(listp (show (multiple-value-list (ext:module-info "pcre" t)) :pretty t)) T

(multiple-value-bind (ve ma mi) (pcre:pcre-version)
  (format t "~&Version: ~S (~D.~D)~%Options:~{~%  ~25@A  ~S~}~%" ve ma mi
          (and (fboundp 'pcre:pcre-config) (pcre:pcre-config))))
NIL

(if (<= 4 (nth-value 1 (pcre:pcre-version)))
    (let* ((d (pcre:pcre-compile "(?P<date> (?P<year>(\\d\\d)?\\d\\d) - (?P<month>\\d\\d) - (?P<day>\\d\\d) )" :extended t :study t))
           (s "today is 2003-12-15!")
           (v (pcre:pcre-exec d s)))
      (list v (pcre:pattern-info d :options) (pcre:pattern-info d :nametable)
            (pcre:pattern-info d :capturecount) (pcre:pcre-exec d "")
            (pcre:match-string v "year" s d) (pcre:match-string v "month" s d)
            (pcre:match-string v "day" s d) (pcre:match-string v "date" s d)))
    ;; pre-4 versions do not support named subpatterns
    '(#(#S(PCRE::MATCH :START 9 :END 19) #S(PCRE::MATCH :START 9 :END 19)
        #S(PCRE::MATCH :START 9 :END 13) #S(PCRE::MATCH :START 9 :END 11)
        #S(PCRE::MATCH :START 14 :END 16) #S(PCRE::MATCH :START 17 :END 19))
      (:EXTENDED :UTF8)
      (("date" . 1) ("day" . 5) ("month" . 4) ("year" . 2))
      5 NIL "2003" "12" "15" "2003-12-15"))
(#(#S(PCRE::MATCH :START 9 :END 19) #S(PCRE::MATCH :START 9 :END 19)
   #S(PCRE::MATCH :START 9 :END 13) #S(PCRE::MATCH :START 9 :END 11)
   #S(PCRE::MATCH :START 14 :END 16) #S(PCRE::MATCH :START 17 :END 19))
 (:EXTENDED :UTF8)
 (("date" . 1) ("day" . 5) ("month" . 4) ("year" . 2))
 5 NIL "2003" "12" "15" "2003-12-15")

(let* ((p (pcre:pcre-compile "(a|(z))(bc)"))
       (r (pcre:pcre-exec p "abc")))
  (format t "~&~S~%" (pcre:pattern-info p))
  (list r (pcre:match-strings r "abc")
        (pcre:pattern-info p :options)))
(#(#S(PCRE::MATCH :START 0 :END 3) #S(PCRE::MATCH :START 0 :END 1) NIL
   #S(PCRE::MATCH :START 1 :END 3))
 #("abc" "a" NIL "bc")
 (:UTF8))

(let ((cp (pcre:pcre-compile "a(a)*b" :extended t)))
  (pcre:pcre-exec cp "ab"))
#(#S(PCRE:MATCH :START 0 :END 2))

(let ((cp (pcre:pcre-compile "a(a)*(b)" :extended t)))
  (pcre:pcre-exec cp "ab"))
#(#S(PCRE:MATCH :START 0 :END 2) NIL #S(PCRE:MATCH :START 1 :END 2))

(let ((*apropos-matcher* #'pcre:pcre-matcher)) (apropos-list "pcre.*r$"))
(PCRE:PCRE-MATCHER)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun re-test (pattern string) ;sds
  (map 'list (lambda (match)
               (and match (pcre:match-substring match string)))
       (pcre:pcre-exec (pcre:pcre-compile pattern :extended nil :dotall t)
                       string)))
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

(re-test "(a|b)*c|(a|ab)*c" "xc") ("c")
(re-test "(a)*" "b") ("")
(re-test "(..)*(...)*" "a") ("")

;;the following fails coz sshenoy's engine is a posix NFA
(re-test "(..)*(...)*" "abc") ("ab" "ab")
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
(re-test "*a" "-") ERROR ; PCRE:PCRE-COMPILE("*a") at 0: "nothing to repeat"
(re-test "^*" "-") ERROR ; PCRE:PCRE-COMPILE("^*") at 1: "nothing to repeat"
(re-test "$*" "-") ERROR ; PCRE:PCRE-COMPILE("$*") at 1: "nothing to repeat"
(re-test "(*)b" "-") ERROR ; PCRE:PCRE-COMPILE("(*)b") at 1: "nothing to repeat"
(re-test "$b" "b") ()

(re-test "a\\(b" "a(b") ("a(b")
(re-test "a\\(*b" "ab") ("ab")
(re-test "a\\(*b" "a((b") ("a((b")
(re-test "a\\\\b" "a\\b") ("a\\b")
(re-test "(abc" "-") ERROR ; ("(abc"): "Unmatched ( or \\("
(re-test "((a))" "abc") ("a" "a" "a")
(re-test "(a)b(c)" "abc") ("abc" "a" "c")
(re-test "a+b+c" "aabbabc") ("abc")

(re-test "a**" "-") ERROR ; PCRE:PCRE-COMPILE("a**") at 2: "nothing to repeat"
(re-test "(a*)*" "-") ("" "")
(re-test "(a*)+" "-") ("" "")
;; non-POSIX (re-test "(a|)*" "-") ("" "")
(re-test "(a*|b)*" "-") ("" "")
(re-test "(a+|b)*" "ab") ("ab" #-:regex-left "b" #+:regex-left "a")
(re-test "(a+|b)+" "ab") ("ab" #-:regex-left "b" #+:regex-left "a")
(re-test "(a+|b)?" "ab") ("a"  "a")
(re-test "[^ab]*" "cde") ("cde")
(re-test "(^)*" "-") ("" "")
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
(re-test "^a(bc+|b[eh])g|.h$" "abh") ("bh")
(re-test "(bc+d$|ef*g.|h?i(j|k))" "effgz") ("effgz" "effgz")
(re-test "(bc+d$|ef*g.|h?i(j|k))" "ij") ("ij" "ij" "j")
(re-test "(bc+d$|ef*g.|h?i(j|k))" "effg") ()
(re-test "(bc+d$|ef*g.|h?i(j|k))" "bcdd") ()
(re-test "(bc+d$|ef*g.|h?i(j|k))" "reffgz") ("effgz" "effgz")

(re-test "((((((((((a))))))))))" "a") ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a")
(re-test "(((((((((a)))))))))" "a") ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a")
(re-test "multiple words of text" "uh-uh") ()
;; with :extended t, does NOT match!
(re-test "multiple words" "multiple words, yeah") ("multiple words")
(re-test "(.*)c(.*)" "abcde") ("abcde" "ab" "de")
;; with :extended t, " b" instead of "b"
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
("{M.D. Harrison and A. Monk (Ed.)}")
(re-test "{[^}
]*}" "{M.D. Harrison and
A. Monk (Ed.)} \n\t foo: 2") ()
(re-test "{[^}
]*}" "{M.D. Harrison and {A. Monk} (Ed.)} \n\t foo: 2")
("{M.D. Harrison and {A. Monk}")

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
;; PCRE: need :DOTALL to match here
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
(re-test "[0-9]+\\.[0-9]*(e[+-]?[0-9]+)?" "12.3  k") ("12.3")

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
(re-test "([xyz])(-\\2)" "x-y") ()
(re-test "(([xyz])(-\\2))" "x-y") ()
(re-test "(([xyz])(-\\2)*)*" "x-y") ("x" "x" "x")
(re-test "(([xyz])(-\\2)*)*" "x-") ("x" "x" "x")
(re-test "(([xyz])(-\\2)*)*" "xy-yz-y")
#-:regex-left ("xy-yz" "z" "z" "-y")
#+:regex-left ("xy" "y" "y" NIL)

;; kmp -- this *should* match
(re-test "((.*)\\1)+" "xxxxxx") ()
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
(re-test "(.*)\\1\\1(.*)\\2\\2\\2" "aaaaaaa") ("aaaaaa" "aa" "")
;; alternative: ("aaaaaaa" "a" "a")
;;the following fails coz sshenoy's engine is a posix NFA
(re-test "(.*)\\1\\1(.*)\\2\\2\\2" "aaaaa") ("aaa" "a" "")
;; alternative: ("aaaa" "" "a")
(re-test "(.*)\\1\\1" "aaa") ("aaa" "a")
(re-test "(.*)*\\1" "xx") ("xx" #-:regex-left "" #+:regex-left "x")
(re-test "(....).*\\1" "beriberi") ("beriberi" "beri")

;; Some tests for class matches (my own)
(re-test "[[:alpha:]_][[:alnum:]_]*" "c_identifier") ("c_identifier")
(re-test "[[:xdigit:]]*" "12aBcD89") ("12aBcD89")
;; In the following pattern, because :] is missing, the pattern is
;; interpreted as an ordinary range
(re-test "[[:xdigit]+" "0[x:dig") ("[x:dig")

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
(re-test "a*?" "aaaa") #+pcre ("") #-pcre ("aaaa")
(re-test "a+?" "aaaa") #+pcre ("a") #-pcre ("aaaa")
(re-test "a{2,3}?" "aaaa") #+pcre ("aa") #-pcre ("aaa")
(re-test "a+?bb*?" "baaaabaaabbbaaaaa") #+pcre("aaaab") #-pcre("b")
(re-test "a+?bb+?" "baaaabaaabbbaaaaa") #+pcre("aaabb") #-pcre("b")
(re-test "[abc]{10,20}?" "xxxbcbcbabcaabcbabcbcbabcbcaabcabxxx")
#+pcre ("bcbcbabcaa") #-pcre ("")

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
(re-test "(x)+?\\1" "xxxx") (#+pcre "xx" #-pcre "xxxx" "x")
(re-test "(x){1,2}?" "xxxx") (#+pcre "x" #-pcre "xx" "x")
(re-test "(x){1,2}?\\1" "xxxx") (#+pcre "xx" #-pcre "xxx" "x")
(re-test "(x)+?[^x]+\\1" "xxaax") ("xxaax" "x")
(re-test "x*?(x)[^x]+\\1" "xxaax") ("xxaax" "x")

;; misc
;; kmp it is legal for a* to match nothing
(re-test "(a*)*" "aaaa") ("aaaa" #-:regex-left "" #+:regex-left "aaaa")
;; kmp it is legal for a* to match nothing
(re-test "(a*)+" "aaaa") ("aaaa" #-:regex-left "" #+:regex-left "aaaa")
(re-test "(a+)*" "aaaa") ("aaaa" "aaaa")
