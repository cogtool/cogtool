;; -*- lisp -*-

(CHAR  "abcdef-dg1ndh" 0)
#\a

(CHAR  "abcdef-dg1ndh" 1)
#\b

(CHAR  "abcdef-dg1ndh" 6)
#\-

(CHAR  "abcdef-dg1ndh" 20)
error

(CHAR  "abcdef-dg1ndh")
error

(CHAR  "abcdef-dg1ndh" -3)
error

(CHAR)
error

(CHAR 2)
error

(CHAR  "abcde" 2 4)
error

(CHAR 'A 0)
#+XCL #\A #-XCL ERROR

(CHAR 'ANNA 0)
#+XCL #\A #-XCL ERROR

(SCHAR 'A 0)
#+XCL #\A #-XCL ERROR

(SCHAR 'ANNA 0)
#+XCL #\A #-XCL ERROR

(SCHAR  "abcdef-dg1ndh" 0)
#\a

(SCHAR  "abcdef-dg1ndh" 1)
#\b

(SCHAR  "abcdef-dg1ndh" 6)
#\-

(SCHAR  "abcdef-dg1ndh" 20)
error

(SCHAR  "abcdef-dg1ndh")
error

(SCHAR  "abcdef-dg1ndh" -3)
error

(SCHAR2)
error

(SCHAR2 2)
error

(SCHAR  "abcde" 2 4)
error

(STRING=  "foo" "foo")
T

(STRING=  "foo" "Foo")
NIL

(STRING=  "foo" "FOO")
NIL

(STRING=  "foo" "bar")
NIL

(STRING=  "together" "frog" :START1 1 :END1 3 :START2 2)
T

(STRING=  "abcdef" "defghi" :START1 3 :END2 3)
T

(STRING=  "abcdefghi" "uvdefmgnj" :START1 3 :END1 6 :START2 2 :END2
5)
T

(STRING=  "abcdefg" "abcdefg" :END2 4)
NIL

(STRING=  "abcdef" "abcdef" :START1 1 :END1 4 :START2 4 :END2 1)
error

(STRING-EQUAL  "foo" "foo")
T

(STRING-EQUAL  "foo" "Foo")
T

(STRING-EQUAL  "foo" "FOO")
T

(STRING-EQUAL  "foo" "bar")
NIL

(STRING-EQUAL  "absDEfg-HijM1#r" "udEFG-hIfvd" :START1 3 :END1 10 :START2
1 :END2
8)
T

(STRING-EQUAL  "ABCdefg" "abcDEFG")
T

(STRING-EQUAL  "ABCdefg" "abcDEFG" :START1 3)
NIL

(STRING-EQUAL  "AbCdEf" "aBcDeF" :START1 5 :END1 3)
error

(STRING<  "" "abcdefgh")
0

(STRING<  "a" "abcdefgh")
1

(STRING<  "abc" "abcdefgh")
3

(STRING<  "cabc" "abcdefgh")
NIL

(STRING<  "abcdefgh" "abcdefgh")
NIL

(STRING<  "xyzabc" "abcdefgh")
NIL

(STRING<  "abc" "xyzabcdefgh")
0

(STRING<  "abcdefgh" "abcdefgh" :END1 4)
4

(STRING<  "xyzabc" "abcdefgh" :START1 3)
6

(STRING<  "abc" "xyzabcdefgh" :START2 3)
3

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<  "abcdefgh" "")
NIL

(STRING<  "abcdefgh" "a")
NIL

(STRING<  "abcdefgh" "abc")
NIL

(STRING<  "abcdefgh" "cabc")
0

(STRING<  "abcdefgh" "xyzabc")
0

(STRING<  "xyzabcdefgh" "abc")
NIL

(STRING<  "abcdefgh" "abcdefgh" :END2 4)
NIL

(STRING<  "xyzabc" "abcdefgh" :START2 3)
NIL

(STRING<  "abc" "xyzabcdefgh" :START2 3)
3

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<  "abcdef" "bcdefgh")
0

(STRING<  "abcdef" "abcdefgh" :START2 2)
0

(STRING<  "abcdef" "bngdabcdef" :START2 9 :END2 5)
error

(STRING>  "" "abcdefgh")
NIL

(STRING>  "a" "abcdefgh")
NIL

(STRING>  "abc" "abcdefgh")
NIL

(STRING>  "cabc" "abcdefgh")
0

(STRING>  "abcdefgh" "abcdefgh")
NIL

(STRING>  "xyzabc" "abcdefgh")
0

(STRING>  "abc" "xyzabcdefgh")
NIL

(STRING>  "abcdefgh" "abcdefgh" :END1 4)
NIL

(STRING>  "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING>  "abcdefgh" "")
0

(STRING>  "abcdefgh" "a")
1

(STRING>  "abcdefgh" "abc")
3

(STRING>  "abcdefgh" "cabc")
NIL

(STRING>  "abcdefgh" "xyzabc")
NIL

(STRING>  "xyzabcdefgh" "abc")
0

(STRING>  "abcdefgh" "abcdefgh" :END2 4)
4

(STRING>  "xyzabc" "abcdefgh" :START2 3)
0

(STRING>  "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING>  "abcde" "bc")
NIL

(STRING>  "bcdef" "abcde")
0

(STRING>  "bcdef" "abcdef")
0

(STRING>  "abcdefghij" "abcdefgh" :START1 1)
1

(STRING>  "ghijkl" "xyzabcd" :START2 6 :END2 4)
error

(STRING<  "" "abcdefgh")
0

(STRING<=  "a" "abcdefgh")
1

(STRING<=  "abc" "abcdefgh")
3

(STRING<=  "aaabce" "aaabcdefgh")
NIL

(STRING<=  "cabc" "abcdefgh")
NIL

(STRING<=  "abcdefgh" "abcdefgh")
8

(STRING<=  "xyzabc" "abcdefgh")
NIL

(STRING<=  "abc" "xyzabcdefgh")
0

(STRING<=  "abcdefgh" "abcdefgh" :END1 4)
4

(STRING<=  "xyzabc" "abcdefgh" :START1 3)
6

(STRING<=  "abc" "xyzabcdefgh" :START2 3)
3

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<=  "abcdefgh" "")
NIL

(STRING<=  "abcdefgh" "a")
NIL

(STRING<=  "abcdefgh" "abc")
NIL

(STRING<=  "abcdefgh" "cabc")
0

(STRING<=  "abcdefgh" "xyzabc")
0

(STRING<=  "xyzabcdefgh" "abc")
NIL

(STRING<=  "abcdefgh" "abcdefgh" :END2 4)
NIL

(STRING<=  "xyzabc" "abcdefgh" :START2 3)
NIL

(STRING<=  "abc" "xyzabcdefgh" :START2 3)
3

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<=  "abcdef" "bcdefgh")
0

(STRING<=  "abcdef" "abcdefgh" :START2 2)
0

(STRING<=  "abcdef" "bngdabcdef" :START2 9 :END2 5)
error


(STRING>= "" "abcdefgh")
NIL

(STRING>= "a" "abcdefgh")
NIL

(STRING>= "abc" "abcdefgh")
NIL

(STRING>= "cabc" "abcdefgh")
0

(STRING>= "abcdefgh" "abcdefgh")
8

(STRING>= "xyzabc" "abcdefgh")
0

(STRING>= "abc" "xyzabcdefgh")
NIL

(STRING>= "abcdefgh" "abcdefgh" :END1 4)
NIL

(STRING>= "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING>= "abcdefgh" "")
0

(STRING>= "abcdefgh" "a")
1

(STRING>= "abcdefgh" "abc")
3

(STRING>= "abcdefgh" "cabc")
NIL

(STRING>= "abcdefgh" "xyzabc")
NIL

(STRING>= "xyzabcdefgh" "abc")
0

(STRING>= "abcdefgh" "abcdefgh" :END2 4)
4

(STRING>= "xyzabc" "abcdefgh" :START2 3)
0

(STRING>= "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING>= "bcdef" "abcdef")
0

(STRING>= "abcdefghij" "abcdefgh" :START1 1)
1

(STRING>= "ghijkl" "xyzabcd" :START2 6 :END2 4)
ERROR

(STRING/= "" "abcdefgh")
0

(STRING/= "a" "abcdefgh")
1

(STRING/= "abc" "abcdefgh")
3

(STRING/= "cabc" "abcdefgh")
0

(STRING/= "abcdefgh" "abcdefgh")
NIL

(STRING/= "xyzabc" "abcdefgh")
0

(STRING/= "abc" "xyzabcdefgh")
0

(STRING/= "abcdefgh" "abcdefgh" :END1 4)
4

(STRING/= "xyzabc" "abcdefgh" :START1 3)
6

(STRING/= "abc" "xyzabcdefgh" :START2 3)
3

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING/= "abcdefgh" "")
0

(STRING/= "abcdefgh" "a")
1

(STRING/= "abcdefgh" "abc")
3

(STRING/= "abcdefgh" "cabc")
0

(STRING/= "abcdefgh" "xyzabc")
0

(STRING/= "xyzabcdefgh" "abc")
0

(STRING/= "abcdefgh" "abcdefgh" :END2 4)
4

(STRING/= "xyzabc" "abcdefgh" :START2 3)
0

(STRING/= "abc" "xyzabcdefgh" :START2 3)
3

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING/= "abcdefghi" "uvdefmgnj" :START1 3 :END1 6 :START2 2 :END2 5)
NIL

(STRING/= "abcdefg" "abcdefg" :END2 4)
4

(STRING/= "abcdef" "abcdef" :START1 1 :END1 4 :START2 4 :END2 1)
ERROR

(STRING-LESSP "" "abcDEFgh")
0

(STRING-LESSP "a" "Abcdefgh")
1

(STRING-LESSP "abc" "aBcDEfgh")
3

(STRING-LESSP "cABc" "aBCDefgh")
NIL

(STRING-LESSP "abCDeFgh" "abCDEfgh")
NIL

(STRING-LESSP "xyzAbc" "ABcCDfgh")
NIL

(STRING-LESSP "aBC" "xYZAbcdEfgh")
0

(STRING-LESSP "abcDEfgh" "abcDEfgh" :END1 4)
4

(STRING-LESSP "XYZabc" "ABcdefgh" :START1 3)
6

(STRING-LESSP "aBc" "xyZABcdefgh" :START2 3)
3

(STRING-LESSP "abc" "xyzabCDEcdefgh" :START2 3 :END2 8)
3

(STRING-LESSP "abc" "xyzABcdefgh" :START2 3 :END2 5)
NIL

(STRING-LESSP "abcdefgh" "")
NIL

(STRING-LESSP "Abcdefgh" "a")
NIL

(STRING-LESSP "ABCdefgh" "abc")
NIL

(STRING-LESSP "ABCdefgh" "cabc")
0

(STRING-LESSP "abcdefgh" "xyzABC")
0

(STRING-LESSP "xyzABCdefgh" "abc")
NIL

(STRING-LESSP "abcdEFgh" "abcdeFGh" :END2 4)
NIL

(STRING-LESSP "xyzaBC" "abCDefgh" :START2 3)
NIL

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3)
3

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-LESSP "aBCDef" "bcdefgh")
0

(STRING-LESSP "aBCDef" "abcdefgh" :START2 2)
0

(STRING-LESSP "aBCDef" "bngdabcdef" :START2 9 :END2 5)
ERROR

(STRING-GREATERP "" "abcdefgh")
NIL

(STRING-GREATERP "A" "abcdefgh")
NIL

(STRING-GREATERP "ABc" "abcdefgh")
NIL

(STRING-GREATERP "CAbc" "abcdefgh")
0

(STRING-GREATERP "abcdefgh" "abcDEFgh")
NIL

(STRING-GREATERP "xyzabc" "abCDEfgh")
0

(STRING-GREATERP "ABC" "xyzabcdefgh")
NIL

(STRING-GREATERP "ABCdefgh" "abcdefgh" :END1 4)
NIL

(STRING-GREATERP "xyzaBc" "ABCdefgh" :START1 3)
NIL

(STRING-GREATERP "abc" "xyzABcdefgh" :START2 3)
NIL

(STRING-GREATERP "abc" "xyzABcdefgh" :START2 3 :END2 8)
NIL

(STRING-GREATERP "abc" "xyZAbcdefgh" :START2 3 :END2 5)
2

(STRING-GREATERP "abcdefgh" "")
0

(STRING-GREATERP "Abcdefgh" "a")
1

(STRING-GREATERP "ABCdefgh" "abc")
3

(STRING-GREATERP "ABCdefgh" "cabc")
NIL

(STRING-GREATERP "ABCdefgh" "xyzabc")
NIL

(STRING-GREATERP "xyzabcdefgh" "Abc")
0

(STRING-GREATERP "abcdefgh" "aBCDefgh" :END2 4)
4

(STRING-GREATERP "xyzabc" "abcdEFgh" :START2 3)
0

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING-GREATERP "bCDEf" "abcde")
0

(STRING-GREATERP "bcDEF" "abcdef")
0

(STRING-GREATERP "abCDEfghij" "abcdefgh" :START1 1)
1

(STRING-GREATERP "ghijKl" "xyzabcd" :START2 6 :END2 4)
ERROR

(STRING-NOT-GREATERP  "" "abcdefgh")
0

(STRING-NOT-GREATERP  "A" "abcdefgh")
1

(STRING-NOT-GREATERP  "aBC" "abcdefgh")
3

(STRING-NOT-GREATERP  "CABc" "abcdefgh")
NIL

(STRING-NOT-GREATERP  "abcDEFgh" "abcdefgh")
8

(STRING-NOT-GREATERP  "xyzabc" "ABcdefgh")
NIL

(STRING-NOT-GREATERP  "abc" "xyzABcdefgh")
0

(STRING-NOT-GREATERP  "ABCDEFgh" "abcdefgh" :END1 4)
4

(STRING-NOT-GREATERP  "xyzabc" "aBCDefgh" :START1 3)
6

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3)
3

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-NOT-GREATERP  "abcdefgh" "")
NIL

(STRING-NOT-GREATERP  "Abcdefgh" "a")
NIL

(STRING-NOT-GREATERP  "ABCdefgh" "abc")
NIL

(STRING-NOT-GREATERP  "ABCdefgh" "cabc")
0

(STRING-NOT-GREATERP  "ABCdefgh" "xyzabc")
0

(STRING-NOT-GREATERP  "xyzABCdefgh" "abc")
NIL

(STRING-NOT-GREATERP  "abcdeFgh" "abcdefgh" :END2 4)
NIL

(STRING-NOT-GREATERP  "xyzABC" "abcdefgh" :START2 3)
NIL

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3)
3

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-NOT-GREATERP  "abcDEF" "bcdefgh")
0

(STRING-NOT-GREATERP  "abcDEF" "abcdefgh" :START2 2)
0

(STRING-NOT-GREATERP  "abcdef" "bngDAbcdef" :START2 9 :END2 5)
error

(STRING-NOT-LESSP  "" "abcdefgh")
NIL

(STRING-NOT-LESSP  "a" "Abcdefgh")
NIL

(STRING-NOT-LESSP  "ABC" "abcdefgh")
NIL

(STRING-NOT-LESSP  "CABc" "abcdefgh")
0

(STRING-NOT-LESSP  "ABCdefgh" "abcdefgh")
8

(STRING-NOT-LESSP  "xyzABC" "abcdefgh")
0

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh")
NIL

(STRING-NOT-LESSP  "ABCdefgh" "abcdefgh" :END1 4)
NIL

(STRING-NOT-LESSP  "xyzABC" "abcdefgh" :START1 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING-NOT-LESSP  "abcdefgh" "")
0

(STRING-NOT-LESSP  "Abcdefgh" "a")
1

(STRING-NOT-LESSP  "ABCdefgh" "abc")
3

(STRING-NOT-LESSP  "abCDEfgh" "cabc")
NIL

(STRING-NOT-LESSP  "aBCdefgh" "xyzabc")
NIL

(STRING-NOT-LESSP  "xyzABcdefgh" "abc")
0

(STRING-NOT-LESSP  "abCDEfgh" "abcdefgh" :END2 4)
4

(STRING-NOT-LESSP  "xyzABc" "abcdefgh" :START2 3)
0

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING-NOT-LESSP  "bCDef" "abcdef")
0

(STRING-NOT-LESSP  "ABCdefghij" "abcdefgh" :START1 1)
1

(STRING-NOT-LESSP  "ghIjkl" "xyzabcd" :START2 6 :END2 4)
error

(STRING-NOT-EQUAL  "" "abcdefgh")
0

(STRING-NOT-EQUAL  "A" "abcdefgh")
1

(STRING-NOT-EQUAL  "ABc" "abcdefgh")
3

(STRING-NOT-EQUAL  "cABc" "abcdefgh")
0

(STRING-NOT-EQUAL  "ABCdefgh" "abcdefgh")
NIL

(STRING-NOT-EQUAL  "xyzABc" "abcdefgh")
0

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh")
0

(STRING-NOT-EQUAL  "ABCdefgh" "abcdefgh" :END1 4)
4

(STRING-NOT-EQUAL  "xyzaBC" "abcdefgh" :START1 3)
6

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3)
3

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
3

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
2

(STRING-NOT-EQUAL  "abcdefgh" "")
0

(STRING-NOT-EQUAL  "Abcdefgh" "a")
1

(STRING-NOT-EQUAL  "aBCdefgh" "abc")
3

(STRING-NOT-EQUAL  "abcdefgh" "cABc")
0

(STRING-NOT-EQUAL  "abcdefgh" "xyzAbc")
0

(STRING-NOT-EQUAL  "xyzabcdefgh" "ABC")
0

(STRING-NOT-EQUAL  "abcdefgh" "abcDEFgh" :END2 4)
4

(STRING-NOT-EQUAL  "xyzabc" "aBCDefgh" :START2 3)
0

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3)
3

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3 :END2 8)
3

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3 :END2 5)
2

(STRING/=  "abcdefghi" "uvdEFmgnj" :START1 3 :END1 6 :START2 2 :END2 5)
4

(STRING/=  "abcdefg" "abcDEfg" :END2 4)
3

(STRING/=  "abcdef" "abCDef" :START1 1 :END1 4 :START2 4 :END2 1)
error

(STRING-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
"garbanzo beans"

(STRING-TRIM   " (*)" " ( *three(siily) words* ) ")
"three(siily) words"

(STRING-TRIM   (QUOTE A) "ababa")
error

(STRING-TRIM   (QUOTE (A)) "ababa")
#+(or XCL SBCL) error #+(or CLISP GCL ECL ALLEGRO CMU OpenMCL LISPWORKS) "ababa" #-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(STRING-TRIM   "a" "ababa")
"bab"

(STRING-TRIM   "c e" "    ceabceabce    c")
"abceab"

(STRING-TRIM   (QUOTE (#\a)) "abcd")
"bcd"

(STRING-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-TRIM   (QUOTE (#\a)) "abcda")
"bcd"

(STRING-LEFT-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
"garbanzo beans
   "

(STRING-LEFT-TRIM   " (*)" " ( *three(siily) words* ) ")
"three(siily) words* ) "

(STRING-LEFT-TRIM   (QUOTE A) "ababa")
error

(STRING-LEFT-TRIM   (QUOTE (A)) "ababa")
#+(or XCL SBCL) error #+(or CLISP GCL ECL ALLEGRO CMU OpenMCL LISPWORKS) "ababa" #-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(STRING-LEFT-TRIM   "a" "ababa")
"baba"

(STRING-LEFT-TRIM   "c e" "    ceabceabce    c")
"abceabce    c"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "abcd")
"bcd"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "abcda")
"bcda"

(STRING-RIGHT-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
" garbanzo beans"

(STRING-RIGHT-TRIM   " (*)" " ( *three(siily) words* ) ")
" ( *three(siily) words"

(STRING-RIGHT-TRIM   (QUOTE A) "ababa")
error

(STRING-RIGHT-TRIM   (QUOTE (A)) "ababa")
#+(or XCL SBCL) error #+(or CLISP GCL ECL ALLEGRO CMU OpenMCL LISPWORKS) "ababa" #-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(STRING-RIGHT-TRIM   "a" "ababa")
"abab"

(STRING-RIGHT-TRIM   "c e" "    ceabceabce    c")
"    ceabceab"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "abcd")
"abcd"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "abcda")
"abcd"

(STRING-UPCASE  "abCD efGh-ij")
"ABCD EFGH-IJ"

(STRING-UPCASE  "abCD efGh-ij" :START 5)
"abCD EFGH-IJ"

(STRING-UPCASE  "abCD efGh-ij" :END 5)
"ABCD efGh-ij"

(STRING-UPCASE  "abCD efGh-ij" :START 1 :END 6)
"aBCD EfGh-ij"

(STRING-UPCASE  "abCD efGh-ij" :START 6 :END 1)
error

(STRING-UPCASE  "abCD efGh-ij" :START 3 :END 3)
"abCD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij")
"abcd efgh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 3)
"abCd efgh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :END 3)
"abcD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 3 :END 3)
"abCD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 1 :END 6)
"abcd efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 6 :END 1)
error

(STRING-CAPITALIZE  "abcd def g hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "abCd dEf G hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "Abcd Def G Hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 6)
"abcd dEf G Hi"

(STRING-CAPITALIZE  "abcd def g hi" :END 6)
"Abcd Def g hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 2 :END 10)
"abCd Def G hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 10 :END 2)
error

(STRING-CAPITALIZE  "don't")
"Don'T"

(STRING-CAPITALIZE  "DON'T")
"Don'T"

(STRING-CAPITALIZE  "34a 5BC")
"34a 5bc"

(STRING 65)
#+ECL "A" #-ECL error

(STRING  (QUOTE A))
"A"

(STRING  #\a)
"a"

(STRING  "abc")
"abc"

(NSTRING-UPCASE  "abCD efGh-ij")   "ABCD EFGH-IJ"

(NSTRING-UPCASE  "abCD efGh-ij" :START 5)   "abCD EFGH-IJ"

(NSTRING-UPCASE  "abCD efGh-ij" :END 5)   "ABCD efGh-ij"

(NSTRING-UPCASE  "abCD efGh-ij" :START6 :END 1)   ERROR

(NSTRING-UPCASE  "abCD efGh-ij" :START 3 :END 3)   "abCD efGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij")   "abcd efgh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :START 3)   "abCd efgh-ij"

(NSTRING-UPCASE  "abCD efGh-ij" :START 1 :END 6)   "aBCD EfGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :END 3)   "abcD efGh-ij"

(NSTRING-DOWNCASE  "abCd efGh-ij" :START 3 :END 3)   "abCd efGh-ij"

(NSTRING-DOWNCASE  "abCd efGh-ij" :START 1 :END 6)   "abcd efGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :START 6 :END 1)   ERROR

(NSTRING-DOWNCASE  "abCD efGh-ij" :START NIL :END NIL)
#+(or XCL AKCL ECL) "abcd efgh-ij" #-(or XCL AKCL ECL) ERROR

(NSTRING-UPCASE  "abDC efGh-oj")   "ABDC EFGH-OJ"

(NSTRING-UPCASE "abCD efGh-ij" :START 1 :END 6)   "aBCD EfGh-ij"

(NSTRING-UPCASE  "abCD efGh-fg" :START 1 :END 6)   "aBCD EfGh-fg"

(NSTRING-UPCASE "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 1)   ERROR

(NSTRING-UPCASE  "abCD efGh-ef" :START NIL :END NIL)
#+(or XCL AKCL ECL) "ABCD EFGH-EF" #-(or XCL AKCL ECL) ERROR

(NSTRING-DOWNCASE  "saBG efGh-ef")   "sabg efgh-ef"

(NSTRING-DOWNCASE  "dfGV efGh-ef" :START 1 :END 6)   "dfgv efGh-ef"

(NSTRING-DOWNCASE  "fgCD efGf-ef" :START 1 :END 3)   "fgcD efGf-ef"

(NSTRING-DOWNCASE  "dfCF edFg-fg" :START NIL :END NIL)
#+(or XCL AKCL ECL) "dfcf edfg-fg" #-(or XCL AKCL ECL) ERROR

(NSTRING-DOWNCASE  "fgHG edgf-fg" :START 5 :END 1)   ERROR

(NSTRING-DOWNCASE  "scDF edFG-ef" :START 1)   "scdf edfg-ef"

(NSTRING-DOWNCASE  "fgHG edFG-ef" :END 4)   "fghg edFG-ef"

(NSTRING-CAPITALIZE  "fg hgf fgh")   "Fg Hgf Fgh"

(LET ((X "ABCDEF"))
  (NSTRING-DOWNCASE X)
  X)
"abcdef"

(let* ((s "abcdefgh")
       (d (make-array 5 :displaced-to s :displaced-index-offset 3
                        :element-type (array-element-type s))))
  (string-upcase d :start 2 :end 4))
"deFGh"

(let* ((s "abcdefgh")
       (d (make-array 5 :displaced-to s :displaced-index-offset 3
                        :element-type (array-element-type s))))
  (and (eq (nstring-upcase d :start 2 :end 4) d) d))
"deFGh"

(nstring-upcase (make-array 0 :element-type nil))
""                      ; actually, #A(NIL (0)), but EQUAL will catch it

#+CLISP
(let* ((s (format nil "A~CB" (code-char 0)))
       (d (make-array 2 :displaced-to s :displaced-index-offset 1
                        :element-type 'character)))
  (list (ext:string-width s :start 0 :end 1)
        (ext:string-width s :start 1 :end 2)
        (ext:string-width s :start 2 :end 3)
        (ext:string-width s :start 0 :end 2)
        (ext:string-width s :start 0 :end 3)
        (ext:string-width s :start 1 :end 3)
        (ext:string-width d :start 0 :end 1)
        (ext:string-width d :start 1 :end 2)
        (ext:string-width d :start 0 :end 2)
        (ext:string-width d)))
#+CLISP (1 0 1 1 2 1 0 1 1 1)

(setq x (make-array 10 :fill-pointer 5 :element-type 'character
                       :initial-contents "abcdefghij"))
"abcde"

(char x 7)
#\h

(elt x 7)
error

(setf (char x 7) #\H)
#\H

(char x 7)
#\H

(reverse x)
"edcba"

(nreverse x)
"edcba"

x
"edcba"

(makunbound 'x)
X

(let* ((x (make-array 10 :fill-pointer 4 :element-type 'character
                         :initial-element #\space :adjustable t))
       (y (make-array 10 :fill-pointer 4 :element-type 'character
                         :displaced-to x)))
  (adjust-array x '(5))
  (char y 5))
error

#+(and clisp unicode)
(let ((s (make-array 10 :element-type 'character :initial-element #\a)))
  (list
   (multiple-value-list (sys::string-info s)) ; 8-bit string
   (progn (setf (aref s 3) (code-char 12345)) ; 16-bit char --> realloc
          (multiple-value-list (sys::string-info s)))
   (progn (gc)                  ; GC --> un-realloc
          (multiple-value-list (sys::string-info s)))
   (progn (setf (aref s 3) (code-char 123456)) ; 32-bit char --> realloc
          (multiple-value-list (sys::string-info s)))
   (progn (gc)                  ; GC --> un-realloc
          (multiple-value-list (sys::string-info s)))))
#+(and clisp unicode)
((8 NIL NIL) (16 NIL T) (16 NIL NIL) (32 NIL T) (32 NIL NIL))

#+(and clisp unicode)
(length (make-array '3 :element-type 'character
                       :initial-contents '(#\U65E5 #\U672C #\U8A9E)))
#+(and clisp unicode)
3

#+(and clisp unicode)
(let ((x (make-array '(3 3) :element-type 'character
                            :initial-contents '((#\U65E5 #\U672C #\U8A9E)
                                                (#\T #\U00FC #\r)
                                                (#\k #\U00E7 #\e)))))
  (gc)
  (array-dimensions x))
#+(and clisp unicode)
(3 3)

(symbolp (make-symbol (make-array 0 :element-type nil))) T
(string= "" (make-array 0 :element-type nil))            T

#+clisp (string-invertcase "AbCdEfGh" :start 1 :end 5)
#+clisp "ABcDefGh"
