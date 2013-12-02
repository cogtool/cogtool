;; -*- lisp -*-

(defun check-xgcd (a b)
  (multiple-value-bind (g u v) (xgcd a b)
    (if (= g (+ (* a u) (* b v))) g
        (format t "~& ~d~% ~d~%  ==> ~d~% ~d~% ~d~%" a b g u v))))
check-xgcd

(check-xgcd 2346026393680644703525505657 17293822570713318399)
11

(check-xgcd 77874422 32223899)
1

(check-xgcd 560014183 312839871)
1

(check-xgcd 3 2)
1

(check-xgcd 2 3)
1

(check-xgcd -2 3)
1

(check-xgcd 576561 -5)
1

(check-xgcd 974507656412513757857315037382926980395082974811562770185617915360
           -1539496810360685510909469177732386446833404488164283)
1

(isqrt #x3FFFFFFFC000000000007F)
#x7FFFFFFFBFF

;; floating-point functions

(< 0.0 least-positive-short-float 1.0)
t

(> most-positive-short-float 1.0)
t

(<= 2.83674s36 10801894066935227181020469351465354205855744/17827)
t

;; transcendental functions

(expt -5s0 2s0) #c(25s0 0s0)
(expt -5f0 2f0) #c(25f0 0f0)
(expt -5d0 2d0) #c(25d0 0d0)
(expt -5l0 2l0) #c(25l0 0l0)
(expt -5 2)     25
(expt 5s0 3s0)  125s0
(expt 5f0 3f0)  125f0
(expt 5d0 3d0)  125d0
(expt 5l0 3l0)  125l0
(expt 5 3)      125
(= 1d-1 (setq z #C(1d-1 0d0)))  T
(* z (expt z z)) #C(0.07943282347242815d0 0.0)
z               #C(1d-1 0d0)

(log 8s0 2s0)   3s0
(log 8f0 2f0)   3f0
(log 8d0 2d0)   3d0
(log 8l0 2l0)   3l0
(log -8 -2)     #C(1.0928407f0 -0.42078725f0)
(log -8s0 -2s0) #C(1.09283s0 -0.42078s0)
(log -8f0 -2f0) #C(1.0928407f0 -0.42078725f0)
(log -8d0 -2d0) #C(1.0928406470908163d0 -0.4207872484158604d0)
(log z)         #C(-2.3025850929940455d0 0d0)
z               #C(1d-1 0d0)

(cis 10)    #c(-0.8390715 -0.5440211)
(cis 123)   #c(-0.8879689 -0.45990348)

(cis #c(0.0d0 1.0d0))
#+CLISP #c(0.36787944117144233d0 0.0d0)
#-CLISP ERROR

(zerop (+               (cis 123) (cis -123)  (* -2 (cos 123))))  T
(zerop (+ (* #c(0 1) (- (cis 123) (cis -123))) (* 2 (sin 123))))  T

(exp #c(0 0))      1
(exp #c(0 1))      #C(0.5403023 0.84147096)
(exp #c(1 1))      #C(1.468694 2.2873552)
(exp #c(1 1d0))    #C(1.4686939399158851d0 2.2873552871788423d0)
(exp #c(1d0 1d0))  #C(1.4686939399158851d0 2.2873552871788423d0)
(exp #c(1l0 1))    #C(1.4686939399158851572L0 2.2873552871788423912L0)
(exp #c(0 1d0))    #C(0.5403023058681398d0 0.8414709848078965d0)
(exp 1)            2.7182817
(exp 1s0)          2.7183s0
(exp 1f0)          2.7182817
(exp 1d0)          2.718281828459045d0
(exp 1l0)          2.7182818284590452354L0

(sin 0d0)   0d0
(sinh 0d0)  0d0
(tan 0d0)   0d0
(tanh 0d0)  0d0

(tan 1.57f0) 1255.8483f0
(tan 1.57d0) 1255.7655915007895d0
(tan z)      #C(0.10033467208545055d0 0d0)
(= (tan z) (tan (realpart z)))   T
(tanh z)     #C(0.09966799462495582d0 0d0)
(= (tanh z) (tanh (realpart z))) T
(atan #c(1 2))  #C(1.3389726f0 0.4023595f0)
(tan  #c(1 2))  #C(0.033812825f0 1.0147936f0)
(tanh #c(20 2)) #C(1f0 0f0)
(cosh #c(1 2))     #C(-0.64214814f0       1.0686074f0)
(cosh #c(1d0 2d0)) #C(-0.64214812471552d0 1.0686074213827783d0)
(log -3/4)      #C(-0.2876821f0 3.1415927f0)
(log (/ most-positive-double-float 2))  709.0895657128241d0

;; Check that exact values are returned when possible.

(eql (sqrt 0) 0)
t
(eql (sqrt 1) 1)
t
(eql (sqrt 9) 3)
t
(eql (sqrt -9) #c(0 3))
t
(eql (sqrt #c(-7 24)) #c(3 4))
t

(eql (exp 0) 1)
t

(eql (log 1) 0)
t

(eql (expt #c(10 11) 1) #c(10 11))
t
(eql (expt 0 1/2) 0)
t
(eql (expt 1 1/2) 1)
t
(eql (expt 9 1/2) 3)
t
(eql (expt -9 1/2) #c(0 3))
t
(eql (expt #c(-7 24) 1/2) #c(3 4))
t
(eql (expt 729 1/6) 3)
t
(eql (expt -3 -1) -1/3)
t
(eql (expt #c(3 4) -1) #c(3/25 -4/25))
t

(eql (conjugate #c(4 -1)) #c(4 1))
t
(eql (conjugate #c(3 0.4)) #c(3 -0.4))
t

(eql (sin 0) 0)
t
(eql (realpart (sin #c(0 3.0))) 0)
t

(eql (cos 0) 1)
t
(eql (imagpart (cos #c(0 3.0))) 0)
t

(eql (tan 0) 0)
t
(eql (realpart (tan #c(0 3.0))) 0)
t

(eql (asin 0) 0)
t
(eql (realpart (asin #c(0 3.0))) 0)
t

(eql (acos 1) 0)
t
(eql (realpart (acos 2.0)) 0)
t

(eql (atan 0) 0)
t
(eql (realpart (atan #c(0 0.5))) 0)
t

(eql (sinh 0) 0)
t
(eql (realpart (sinh #c(0 3.0))) 0)
t

(eql (cosh 0) 1)
t
(eql (imagpart (cosh #c(0 3.0))) 0)
t

(eql (tanh 0) 0)
t
(eql (realpart (tanh #c(0 3.0))) 0)
t

(eql (asinh 0) 0)
t
(eql (realpart (asinh #c(0 0.6))) 0)
t

(eql (acosh 1) 0)
t
(eql (realpart (acosh -0.4)) 0)
t

(eql (atanh 0) 0)
t
(eql (realpart (atanh #c(0 3.0))) 0)
t

(sqrt 1)    1
(sqrt 1d0)  1.0d0
(sqrt -1)   #C(0 1)
(sqrt -1d0) #C(0 1.0d0)

(abs (sqrt -1))    1
(phase (sqrt -2))  1.5707964
(signum (sqrt -2)) #C(0 1.0)

(asin 1)  1.5707964
(asin 2)  #C(1.5707964 -1.316958)
(acos 1)  0
(acos 2)  #C(0 1.316958)

(atan 1)  0.7853981
(atan 2)  1.1071488
(atan 2 3) 0.58800256

(sinh 10) 11013.232
(cosh 10) 11013.233

(tanh 10)  1.0
(tanh 3)   0.9950548
(asinh 1)  0.88137364
(acosh 1)  0
(acosh 3)  1.762747
(atanh 3)    #C(0.3465736 -1.5707964)
(atanh 0.9)  1.4722193

;; bits

(loop :for z :from 1 :to 1000
  :for z*z = (* z z) :for 2^z = (ash 1 z) :for z*2^z = (ash z z)
  :for z^z = (expt z z)
  :unless (logtest z z) :collect (list 'logtest z)
  :unless (logtest z*z z*z) :collect (list 'logtest z 'z*z z*z)
  :unless (logtest 2^z 2^z) :collect (list 'logtest z '2^z 2^z)
  :unless (logtest z*2^z z*2^z) :collect (list 'logtest z 'z*2^z z*2^z)
  :unless (logtest z*z z*z) :collect (list 'logtest z 'z*z z*z))
nil

(loop :for z :from 0 :to 1000
  :for z*z = (* z z) :for 2^z = (ash 1 z) :for z*2^z = (ash z z)
  :for z^z = (expt z z)
  :unless (= z (logand z z)) :collect (list 'logand z)
  :unless (= z*z (logand z*z z*z)) :collect (list 'logand z 'z*z z*z)
  :unless (= 2^z (logand 2^z 2^z)) :collect (list 'logand z '2^z 2^z)
  :unless (= z*2^z (logand z*2^z z*2^z)) :collect (list 'logand z 'z*2^z z*2^z)
  :unless (= z*z (logand z*z z*z)) :collect (list 'logand z 'z*z z*z))
nil

(mod-expt 1432 634 3456)
2944

(mod-expt 640785284696442065785559134003308932264708355179002798538113
          671286301850793527622248679786362012411973295201562077406347
          541607700526106309999871171548445806906603126622271198261079)
184927654951560197998922671105024055618160643054333015564836

(defun check-sqrt (num eps)
  (let ((s (sqrt num)))
    (> (* eps 2) (abs (/ (- s (/ num s)) s)))))
check-sqrt

(check-sqrt   MOST-POSITIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  LEAST-POSITIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  LEAST-NEGATIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt   MOST-NEGATIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  MOST-POSITIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt LEAST-POSITIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt LEAST-NEGATIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt  MOST-NEGATIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt  MOST-POSITIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt LEAST-POSITIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt LEAST-NEGATIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt  MOST-NEGATIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt    MOST-POSITIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt   LEAST-POSITIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt   LEAST-NEGATIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt    MOST-NEGATIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T

(defun check-mult (num) (= num (* 1 num) (/ num 1))) check-mult

(check-mult   MOST-POSITIVE-SHORT-FLOAT) T
(check-mult  LEAST-POSITIVE-SHORT-FLOAT) T
(check-mult  LEAST-NEGATIVE-SHORT-FLOAT) T
(check-mult   MOST-NEGATIVE-SHORT-FLOAT) T
(check-mult  MOST-POSITIVE-SINGLE-FLOAT) T
(check-mult LEAST-POSITIVE-SINGLE-FLOAT) T
(check-mult LEAST-NEGATIVE-SINGLE-FLOAT) T
(check-mult  MOST-NEGATIVE-SINGLE-FLOAT) T
(check-mult  MOST-POSITIVE-DOUBLE-FLOAT) T
(check-mult LEAST-POSITIVE-DOUBLE-FLOAT) T
(check-mult LEAST-NEGATIVE-DOUBLE-FLOAT) T
(check-mult  MOST-NEGATIVE-DOUBLE-FLOAT) T
(check-mult    MOST-POSITIVE-LONG-FLOAT) T
(check-mult   LEAST-POSITIVE-LONG-FLOAT) T
(check-mult   LEAST-NEGATIVE-LONG-FLOAT) T
(check-mult    MOST-NEGATIVE-LONG-FLOAT) T

(loop :for x :in '(1.0s0 1.0f0 1.0d0 1.0l0) :for eps :in
  (list short-float-epsilon single-float-epsilon double-float-epsilon
        long-float-epsilon)
  :for eps2 = (* eps 9/10) :unless
  (equal
   (funcall (compile nil `(lambda nil (list (+ (+ ,x ,eps2) ,eps2)
                                            (+ ,eps2 (+ ,eps2 ,x))))))
   (list x x))
  :collect (list x eps eps2))
NIL

#| generated by:
 (loop :for f :in '(exp sin asin cos atan cosh sinh asinh tanh
                   acos acosh log atanh tan)
  :do (loop :for type :in '(short-float single-float double-float long-float)
        :for x = (complex (coerce 1 type) (coerce 2 type))
        :do (format t "(~S ~S)~24T~S~%" f x (funcall f x)))
  (terpri))
|#

(EXP #C(1s0 2s0))   #C(-1.13121s0 2.47174s0)
(EXP #C(1f0 2f0))   #C(-1.1312044f0 2.4717267f0)
(EXP #C(1d0 2d0))   #C(-1.1312043837568135d0 2.4717266720048188d0)
(EXP #C(1L0 2L0))   #C(-1.1312043837568136384L0 2.4717266720048189277L0)

(SIN #C(1s0 2s0))   #C(3.16577s0 1.9596s0)
(SIN #C(1f0 2f0))   #C(3.1657784f0 1.959601f0)
(SIN #C(1d0 2d0))   #C(3.165778513216168d0 1.9596010414216058d0)
(SIN #C(1L0 2L0))   #C(3.1657785132161681467L0 1.9596010414216058971L0)

(ASIN #C(1s0 2s0))  #C(0.427067s0 1.52856s0)
(ASIN #C(1f0 2f0))  #C(0.42707857f0 1.5285708f0)
(ASIN #C(1d0 2d0))  #C(0.4270785863924762d0 1.5285709194809982d0)
(ASIN #C(1L0 2L0))  #C(0.4270785863924761255L0 1.5285709194809981613L0)

(COS #C(1s0 2s0))   #C(2.0327s0 -3.0519s0)
(COS #C(1f0 2f0))   #C(2.032723f0 -3.0518978f0)
(COS #C(1d0 2d0))   #C(2.0327230070196656d0 -3.0518977991518d0)
(COS #C(1L0 2L0))   #C(2.0327230070196655295L0 -3.0518977991518000574L0)

(ATAN #C(1s0 2s0))  #C(1.33897s0 0.40236s0)
(ATAN #C(1f0 2f0))  #C(1.3389726f0 0.4023595f0)
(ATAN #C(1d0 2d0))  #C(1.3389725222944935d0 0.40235947810852507d0)
(ATAN #C(1L0 2L0))  #C(1.3389725222944935611L0 0.40235947810852509365L0)

(COSH #C(1s0 2s0))  #C(-0.64215s0 1.0686s0)
(COSH #C(1f0 2f0))  #C(-0.64214814f0 1.0686074f0)
(COSH #C(1d0 2d0))  #C(-0.64214812471552d0 1.0686074213827783d0)
(COSH #C(1L0 2L0))  #C(-0.64214812471551996483L0 1.0686074213827783395L0)

(SINH #C(1s0 2s0))  #C(-0.489056s0 1.40312s0)
(SINH #C(1f0 2f0))  #C(-0.48905626f0 1.4031192f0)
(SINH #C(1d0 2d0))  #C(-0.4890562590412937d0 1.4031192506220407d0)
(SINH #C(1L0 2L0))  #C(-0.4890562590412936736L0 1.403119250622040588L0)

(ASINH #C(1s0 2s0)) #C(1.46936s0 1.06343s0)
(ASINH #C(1f0 2f0)) #C(1.4693518f0 1.0634403f0)
(ASINH #C(1d0 2d0)) #C(1.4693517443681856d0 1.0634400235777521d0)
(ASINH #C(1L0 2L0)) #C(1.4693517443681852733L0 1.0634400235777520562L0)

(TANH #C(1s0 2s0))  #C(1.16673s0 -0.243458s0)
(TANH #C(1f0 2f0))  #C(1.1667362f0 -0.2434582f0)
(TANH #C(1d0 2d0))  #C(1.16673625724092d0 -0.24345820118572525d0)
(TANH #C(1L0 2L0))  #C(1.1667362572409198818L0 -0.2434582011857252527L0)

(ACOS #C(1s0 2s0))  #C(1.14374s0 -1.52856s0)
(ACOS #C(1f0 2f0))  #C(1.1437178f0 -1.5285708f0)
(ACOS #C(1d0 2d0))  #C(1.1437177404024204d0 -1.5285709194809982d0)
(ACOS #C(1L0 2L0))  #C(1.1437177404024204937L0 -1.5285709194809981613L0)

(ACOSH #C(1s0 2s0)) #C(1.52856s0 1.14372s0)
(ACOSH #C(1f0 2f0)) #C(1.5285709f0 1.1437178f0)
(ACOSH #C(1d0 2d0)) #C(1.5285709194809982d0 1.1437177404024204d0)
(ACOSH #C(1L0 2L0)) #C(1.528570919480998161L0 1.1437177404024204937L0)

(LOG #C(1s0 2s0))   #C(0.80471s0 1.10715s0)
(LOG #C(1f0 2f0))   #C(0.804719f0 1.1071488f0)
(LOG #C(1d0 2d0))   #C(0.8047189562170503d0 1.1071487177940904d0)
(LOG #C(1L0 2L0))   #C(0.8047189562170501873L0 1.107148717794090503L0)

(ATANH #C(1s0 2s0)) #C(0.173286s0 1.1781s0)
(ATANH #C(1f0 2f0)) #C(0.1732868f0 1.1780972f0)
(ATANH #C(1d0 2d0)) #C(0.17328679513998632d0 1.1780972450961724d0)
(ATANH #C(1L0 2L0)) #C(0.17328679513998632736L0 1.1780972450961724645L0)

(TAN #C(1s0 2s0))   #C(0.033813s0 1.0148s0)
(TAN #C(1f0 2f0))   #C(0.033812825f0 1.0147936f0)
(TAN #C(1d0 2d0))   #C(0.03381282607989669d0 1.0147936161466335d0)
(TAN #C(1L0 2L0))   #C(0.033812826079896690283L0 1.0147936161466335681L0)

;; based on pfd's gcl suite
(defun test-function (func max min &key (repeat 1000) (except ()))
  (loop :for type :in '(short-float single-float double-float long-float)
    :for bad = (mapcar (lambda (x) (coerce x type)) except)
    :nconc (loop :for x = (+ (random (coerce (- max min) type)) min)
             :for x-bad = (member x bad :test #'=)
             :for r = (or x-bad (funcall func x))
             :repeat repeat
             :unless (or x-bad (typep r type) (typep r `(complex ,type)))
             :collect (list x bad r func type))))
TEST-FUNCTION

(loop :for f :in '(exp sin asin cos atan cosh sinh asinh tanh)
  :nconc (test-function f 10 -10))
NIL
(test-function 'acos 1 -1)                NIL
(test-function 'acosh 20 1)               NIL
(test-function 'log 10 -10 :except '(0))  NIL
(test-function 'atanh 10 -10 :except '(-1 1)) NIL
(test-function
 'tan 10 -10
 :except (nconc (loop :for x :from (/ pi 2) :by pi :to 10 :collect x)
                (loop :for x :from (/ pi -2) :by pi :downto -10 :collect x)))
NIL

;; http://www.lisp.org/HyperSpec/Body/fun_boole.html#boole
;; The order of the values in this `table' are such that
;; (logand (boole (aref boole-n-vector n) #b0101 #b0011) #b1111) => n
(let ((boole-n-vector
       (vector boole-clr   boole-and  boole-andc1 boole-2
               boole-andc2 boole-1    boole-xor   boole-ior
               boole-nor   boole-eqv  boole-c1    boole-orc1
               boole-c2    boole-orc2 boole-nand  boole-set)))
  (list (loop :for n :from 0 :to 15
          :unless (= n (logand (boole (aref boole-n-vector n) #b0101 #b0011)
                               #b1111))
          :collect n)
        (flet ((boole-n (n integer &rest more-integers)
                 (apply #'boole (elt boole-n-vector n) integer more-integers)))
          (loop :for n :from #b0000 :to #b1111 :collect (boole-n n 5 3)))))
(NIL (0 1 2 3 4 5 6 7 -8 -7 -6 -5 -4 -3 -2 -1))


(loop :for i :from 1 :to 100 :collect (list i (integer-length (ext:! i))))
((1 1) (2 2) (3 3) (4 5) (5 7) (6 10) (7 13) (8 16) (9 19) (10 22) (11 26)
 (12 29) (13 33) (14 37) (15 41) (16 45) (17 49) (18 53) (19 57) (20 62)
 (21 66) (22 70) (23 75) (24 80) (25 84) (26 89) (27 94) (28 98) (29 103)
 (30 108) (31 113) (32 118) (33 123) (34 128) (35 133) (36 139) (37 144)
 (38 149) (39 154) (40 160) (41 165) (42 170) (43 176) (44 181) (45 187)
 (46 192) (47 198) (48 203) (49 209) (50 215) (51 220) (52 226) (53 232)
 (54 238) (55 243) (56 249) (57 255) (58 261) (59 267) (60 273) (61 279)
 (62 285) (63 290) (64 296) (65 303) (66 309) (67 315) (68 321) (69 327)
 (70 333) (71 339) (72 345) (73 351) (74 358) (75 364) (76 370) (77 376)
 (78 383) (79 389) (80 395) (81 402) (82 408) (83 414) (84 421) (85 427)
 (86 434) (87 440) (88 447) (89 453) (90 459) (91 466) (92 473) (93 479)
 (94 486) (95 492) (96 499) (97 505) (98 512) (99 519) (100 525))

(multiple-value-list (integer-decode-float 1d23)) (5960464477539062 24 1)
(prin1-to-string 1d22) "1.0d22"
(prin1-to-string 1d23) "9.999999999999999d22"
(prin1-to-string 1d24) "1.0d24"
(format nil "~G" 1d22) "10000000000000000000000.    "
(format nil "~G" 1d23) "100000000000000000000000.    "
(format nil "~G" 1d24) "1000000000000000000000000.    "
(format nil "~F" 1d22) "10000000000000000000000.0"
(format nil "~F" 1d23) "100000000000000000000000.0"
(format nil "~F" 1d24) "1000000000000000000000000.0"
(format nil "~E" 1d22) "1.0d+22"
(format nil "~E" 1d23) "1.0d+23"
(format nil "~E" 1d24) "1.0d+24"
