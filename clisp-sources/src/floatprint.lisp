;; Printing of Floating-Point-Numbers with PRINT and FORMAT
;; Michael Stoll 10.2.1990 - 26.3.1990
;; Bruno Haible 8.9.1990 - 10.9.1990
;; Translation: Stefan Kain 2003-04-26
;;   The German variable names 'unten' and 'oben' where translated with
;;   'below' resp. 'above' in English!
;;   wlog == without loss of generality
;; Bruno Haible 2004-03-27: Fixed printing of short floats like 1782592s0.

;; basic idea:
;; Each real-number /= 0 represents an (open) interval. We print the
;; decimal number with as few digits as possible, that is situated in
;; this interval.
;; In order to also treat big exponents, powers of 2 are approximately
;; turned into powers of 10. If necessary, the computing accuracy is
;; increased. Here we utilize long-floats of arbitrary precision.

(in-package "SYSTEM")

;; based on:
;; (sys::log2 digits) returns ln(2) with at least DIGITS mantissa bits.
;; (sys::log10 digits) returns ln(10) with at least DIGITS mantissa bits.
;; (sys::decimal-string integer) returns for an integer >0
;;   a simple-string with its decimal presentation.
;; (substring string start [end]) like SUBSEQ, however faster for strings.

;; the main function for conversion of floats into the decimal system:
;; For a float X we calculate a Simple-String AS and three Integers K,E,S
;; with the following properties:
;; s = sign(x).
;; If x/=0, consider |x| instead of x. Thus, wlog, let x>0.
;;   Let x1 and x2 be the next smaller resp. the next bigger number for x
;;   of the same floating-point-format. Consequently, the number x represents
;;   the open interval from (x+x1)/2 to (x+x2)/2.
;;   A is an integer >0, with exactly K decimal places (K >= 1), and
;;   (x+x1)/2 < a*10^(-k+e) < (x+x2)/2 .
;;   K is minimal, so A is not divisible by 10.
;;   If fixed-point-adjust is true and 1 <= |x| < 10^7, more precision
;;   is provided by assuming that K needs only to be >= E+1, and no effort
;;   is made to minimize K below E+1 if it would discard nonzero digits.
;; if x=0, then a=0, k=1, e=0.
;; AS is the sequence of digits of A, of length K.
(defun decode-float-decimal (x fixed-point-adjust)
  (declare (type float x))
  (multiple-value-bind (binmant binexpo sign) (integer-decode-float x)
    (if (eql binmant 0) ; x=0 ?
      (values "0" 1 0 0) ; a=0, k=1, e=0, s=0
      ;; x/=0, so sign is the sign of x and
      ;; |x| = 2^binexpo * float(binmant,x) . From now on, let x>0, wlog.
      ;; x = 2^binexpo * float(binmant,x) .
      (let* ((l (integer-length binmant)) ; number of bits of binmant
             (2*binmant (ash binmant 1)) ; 2*binmant
             (above (1+ 2*binmant)) ; upper interval boundary is
                                ; (x+x2)/2 = 2^(binexpo-1) * above
             (below (1- 2*binmant)) ; lower interval boundary is
             (belowshift 0)) ; (x+x1)/2 = 2^(binexpo-1-belowshift) * below
        (when (eql (integer-length below) l)
          ;; normally, integerlength(below) = 1+integerlength(binmant).
          ;; Here, integerlength(below) = l = integerlength(binmant),
          ;; thus, binmant was a power of two. In this case the tolerance
          ;; upwards is 1/2 unit, but the tolerance downwards is only
          ;; 1/4 unit: (x+x1)/2 = 2^(binexpo-2) * (4*binmant-1)
          (setq below (1- (ash 2*binmant 1)) belowshift 1))
        ;; determine d (integer) and a1,a2 (integer, >0) thus that
        ;; the integer a with (x+x1)/2 < 10^d * a < (x+x2)/2 are exactly
        ;; the integer a with a1 <= a <= a2 and 0 <= a2-a1 < 20 .
        ;; Therefore, convert 2^e := 2^(binexpo-1) into the decimal system.
        (let* ((e (- binexpo 1))
               (e-gross (> (abs e) (ash l 1))) ; if |e| is very large, >2*l ?
               g f     ; auxiliary variables in case that |e| is large
               ten-d  ; auxiliary variable 10^|d| in case that |e| is small
               d a1 a2); result variables
          (if e-gross ; is |e| really big?
            ;; As 2^e can work only approximately, we need safety bits.
            (prog ((h 16)) ; number of safety bits, must be >= 3
              new-safety-bits
              ;; target: 2^e ~= 10^d * f/2^g, with 1 <= f/2^g < 10.
              (setq g (+ l h)) ; number of valid bits of f
              ;; Estimate d = floor(e*lg(2))
              ;; with help of the rational approximations of lg(2):
              ;;  0 1/3 3/10 28/93 59/196 146/485 643/2136 4004/13301
              ;;  8651/28738 12655/42039 21306/70777 76573/254370 97879/325147
              ;;  1838395/6107016 1936274/6432163 13456039/44699994
              ;;  15392313/51132157 44240665/146964308 59632978/198096465
              ;;  103873643/345060773 475127550/1578339557 579001193/1923400330
              ;; e>=0 : choose lg(2) < a/b < lg(2) + 1/e,
              ;;        then d <= floor(e*a/b) <= d+1 .
              ;; e<0  : then lg(2) - 1/abs(e) < a/b < lg(2),
              ;;        then d <= floor(e*a/b) <= d+1 .
              ;; It is known that abs(e) <= 2^31 + 2^20 .
              ;; Let d be := floor(e*a/b)-1.
              (setq d (1- (if (minusp e)
                            (if (>= e -970)
                              (floor (* e 3) 10) ; 3/10
                              (floor (* e 21306) 70777)) ; 21306/70777
                            (if (<= e 22000)
                              (floor (* e 28) 93) ; 28/93
                              (floor (* e 12655) 42039))))) ; 12655/42039
              ;; The true d is either matched by this estimation
              ;; or undervalued by 1.
              ;; In other "words": 0 < e*log(2)-d*log(10) < 2*log(10).
              ;; now, calculate f/2^g as exp(e*log(2)-d*log(10)) .
              ;; As f < 100*2^g < 2^(g+7), we need g+7 bits relative accuracy
              ;; of the result, i.e g+7 bits absolute accuracy of
              ;; e*log(2)-d*log(10) . With l'=integer-length(e)
              ;;  log(2): g+7+l' bits abs. accuracy, g+7+l' bits rel. acc.,
              ;;  log(10): g+7+l' bits abs. accuracy, g+7+l'+2 bits rel. acc.
              (let ((f/2^g (let ((gen (+ g (integer-length e) 9))) ; accuracy
                             (exp (- (* e (sys::log2 gen))
                                     (* d (sys::log10 gen)))))))
                ;; the calculated f/2^g is >1, <100.
                ;; multiply with 2^g and round to an integer number:
                (setq f (round (scale-float f/2^g g)))) ; returns f
              ;; Possibly correct f and d:
              (when (>= f (ash 10 g)) ; f >= 10*2^g ?
                (setq f (floor f 10) d (+ d 1)))
              ;; Now 2^e ~= 10^d * f/2^g, with 1 <= f/2^g < 10 and
              ;; f an Integer, that deviates from the true value at most by 1:
              ;; 10^d * (f-1)/2^g < 2^e < 10^d * (f+1)/2^g
              ;; we make the open interval now smaller
              ;; from (x+x1)/2 = 2^(binexpo-1-belowshift) * below
              ;; to (x+x2)/2 = 2^(binexpo-1) * above
              ;; into a closed interval
              ;; from 10^d * (f+1)/2^(g+belowshift) * below
              ;; to 10^d * (f-1)/2^g * above
              ;; and search therein numbers of the form a*10^d with integer a.
              ;; since   above - below/2^belowshift >= 3/2
              ;; and  above + below/2^belowshift <= 4*binmant+1 < 2^(l+2) <= 2^(g-1)
              ;; the interval-length is
              ;; = 10^d * ((f-1)*above - (f+1)*below/2^belowshift) / 2^g
              ;; = 10^d * ( f * (above - below/2^belowshift)
              ;;            - (above + below/2^belowshift) ) / 2^g
              ;; >= 10^d * (2^g * 3/2 - 2^(g-1)) / 2^g
              ;; = 10^d * (3/2 - 2^(-1)) = 10^d
              ;; and hence, there is at least one number of this form
              ;; in this interval.
              ;; The numbers of the form 10^d * a in this intervall are those
              ;; with a1 <= a <= a2, and a2 = floor((f-1)*above/2^g) and
              ;; a1 = ceiling((f+1)*below/2^(g+belowshift))
              ;;    = floor(((f+1)*below-1)/2^(g+belowshift))+1 .
              ;; We have now seen, that a1 <= a2 .
              (setq a1 (1+ (ash (1- (* (+ f 1) below)) (- (+ g belowshift)))))
              (setq a2 (ash (* (- f 1) above) (- g)))
              ;; We can also nest the open interval
              ;; from (x+x1)/2 = 2^(binexpo-1-belowshift) * below
              ;; to (x+x2)/2 = 2^(binexpo-1) * above
              ;; into the (closed) interval
              ;; from 10^d * (f-1)/2^(g+belowshift) * below
              ;; to 10^d * (f+1)/2^g * above
              ;; Herein are the numbers of the form 10^d * a
              ;; with a1' <= a <= a2', with a1' <= a1 <= a2 <= a2' and that
              ;; can be calculated with a1' and a2' analogous to a1 and a2.
              ;; As (f-1)*above/2^g and (f+1)*above/2^g differ by 2*above/2^g
              ;; < 2^(l+2-g) < 1, a2 and
              ;; a2' differ by at most 1.
              ;; Likewise, if 'above' is replaced by 'below/2^belowshift':
              ;; a1' and a1 differ by at most 1.
              ;; If a1' < a1 or a2 < a2' , then the power-of-2-approximation-
              ;; 10^d * f/2^g for 2^e has not been accurate enough,
              ;; and we have to repeat everything with increased h.
              ;; Exception (wenn even no higer precision helps):
              ;;   If the upper or lower interval boundary (x+x2)/2 resp.
              ;;   (x+x1)/2 has itself the shape 10^d * a with integer a.
              ;;   This is tested via:
              ;;     (x+x2)/2 = 2^e * above == 10^d * a  with integer a, if
              ;;    - for e>=0, (then 0 <= d <= e): 5^d | above,
              ;;    - for e<0,  (then e <= d < 0): 2^(d-e) | above, which is
              ;;                the case only for d-e=0 .
              ;;    (x+x1)/2 = 2^(e-belowshift) * below == 10^d * a
              ;;     with integer a, if
              ;;    - for e>0, (then 0 <= d < e): 5^d | below,
              ;;    - for e<=0, (then e <= d <= 0): 2^(d-e+belowshift) | below,
              ;;                which is only the case for d-e+belowshift=0.
              ;; As we deal here with higher |e| , this exceptional cause
              ;; cannot happen here at all!
              ;; Then in case of e>=0: e>=2*l and l>=11 imply
              ;;   e >= (l+2)*ln(10)/ln(5) + ln(10)/ln(2),
              ;;   d >= e*ln(2)/ln(10)-1 >= (l+2)*ln(2)/ln(5),
              ;;   5^d >= 2^(l+2),
              ;;   and because of 0 < below < 2^(l+2) and 0 < above < 2^(l+1)
              ;;   below and above are not divisible by 5^d.
              ;; And in case e<=0: From -e>=2*l and l>=6 can be implied
              ;;   -e >= (l+2)*ln(10)/ln(5),
              ;;   d-e >= e*ln(2)/ln(10)-1-e = (1-ln(2)/ln(10))*(-e)-1
              ;;          = (-e)*ln(5)/ln(10)-1 >= l+1,
              ;;   2^(d-e) >= 2^(l+1),
              ;;   and because of 0 < below < 2^(l+1+belowshift), below is not
              ;;   divisible by 2^(d-e+belowshift) , and because of
              ;;   0 < above < 2^(l+1), above is not divisible by 2^(d-e) .
              (when (or (< (1+ (ash (1- (* (- f 1) below))
                                    (- (+ g belowshift)))) ; a1'
                           a1)
                        (< a2 (ash (* (+ f 1) above) (- g)))) ; a2<a2'
                (setq h (ash h 1)) ; double h
                (go new-safety-bits)) ; and repeat everything
              ;; Now a1 is the smallest and a2 is the biggest value, that is
              ;; possible for a.
              ;; Because of  above - below/2^belowshift <= 2
              ;; the above interval-length is
              ;; = 10^d * ((f-1)*above - (f+1)*below/2^belowshift) / 2^g
              ;; < 10^d * ((f-1)*above - (f-1)*below/2^belowshift) / 2^g
              ;; = 10^d * (f-1)/2^g * (above - below/2^belowshift)
              ;; < 10^d * 10 * 2,
              ;; so there are at most 20 possible values for a.
              )                   ; PROG is done
            ;; |e| is relatively small -> can calculate 2^e and 10^d exactly
            (if (not (minusp e))
              ;; e >= 0. Estimate d = floor(e*lg(2)) as above.
              ;;  e<=2*l<2^21.
              (progn
                (setq d (if (<= e 22000)
                          (floor (* e 28) 93) ;  28/93
                          (floor (* e 4004) 13301))) ;  4004/13301
                ;; The true d is either matched by this estimate
                ;; or overestimated by 1, but we can find this out easily.
                (setq ten-d (expt 10 d)) ; ten-d = 10^d
                (when (< (ash 1 e) ten-d) ; if 2^e < 10^d,
                  (setq d (- d 1) ten-d (floor ten-d 10))) ; correct estimate
                ;; Now 10^d <= 2^e < 10^(d+1) and ten-d = 10^d.
                (if (and fixed-point-adjust
                         (<= binmant (ash 9999999 (- binexpo))))
                  ;; |x| < 10^7 and we want fixed-point notation. Force d
                  ;; to stay <= -1, i.e. return more digits than we would do
                  ;; in scientific notation.
                  (progn
                    ;; Set d = -1 and a1 = a2 = 2^e * 2*binmant / 10^d.
                    ;; Or, since we know that e>=0 implies that these a1 and a2
                    ;; end in a trailing zero and the caller can add zeroes
                    ;; at the end on his own:
                    ;; Set d = 0 and a1 = a2 = 2^e * 2*binmant / 10^d.
                    (setq d 0)
                    (setq a1 (setq a2 (ash binmant (1+ e)))))
                  (progn
                    ;; let a1 be the smallest integer
                    ;;      a > 2^(e-belowshift) * below / 10^d,
                    ;; let a2 be the largest integer a < 2^e * above / 10^d.
                    ;; a1 = 1+floor(below*2^e/(2^belowshift*10^d)),
                    ;; a2 = floor((above*2^e-1)/10^d).
                    (setq a1 (1+ (floor (ash below e) (ash ten-d belowshift))))
                    (setq a2 (floor (1- (ash above e)) ten-d)))))
              ;; e < 0. Estimate d = floor(e*lg(2)) like above.
              ;; |e|<=2*l<2^21.
              (progn
                (setq d (if (>= e -970)
                          (floor (* e 3) 10) ;  3/10
                          (floor (* e 643) 2136))) ;  643/2136
                ;; The true d is either matched by this estimate
                ;; or overestimated by 1, but we can find this out easily.
                (setq ten-d (expt 10 (- d))) ; ten-d = 10^(-d)
                (when (<= (integer-length ten-d) (- e)) ; if 2^e < 10^d,
                  (setq d (- d 1) ten-d (* ten-d 10))) ; correct estimate
                ;; now 10^d <= 2^e < 10^(d+1) and ten-d = 10^(-d).
                ;; let a1 be the smallest integer
                ;;      a > 2^(e-belowshift) * below / 10^d,
                ;; let a2 be the largest integer a < 2^e * above / 10^d.
                ;; a1 = 1+floor(below*10^(-d)/2^(-e+belowshift)),
                ;; a2 = floor((above*10^(-d)-1)/2^(-e))
                (setq a1 (1+ (ash (* below ten-d) (- e belowshift))))
                (setq a2 (ash (1- (* above ten-d)) e)))))
          ;; Now the integer a's with (x+x1)/2 < 10^d * a < (x+x2)/2 are
          ;; exactly the integer a's with a1 <= a <= a2. There are at most 20.
          ;; These are reduced to a single one with three steps:
          ;; 1. Does the region contain a number a that is divisible by 10?
          ;;    yes -> set a1:=ceiling(a1/10), a2:=floor(a2/10), d:=d+1.
          ;;    After that, the region a1 <= a <= a2 contains at most 10
          ;;    possible values for a.
          ;; 2. If now one of the possible values is divisible by 10
          ;;    (there can be only one... :-) ),
          ;;    it is chosen, the others are forgotten.
          ;; 3. Else, the closest value to x is chosen among the other
          ;;    possible values.
          (prog ((d-shift nil) ; flag, if d was incremented in the first step
                 a)            ; the chosen a
            ;; 1.
            (let ((b1 (ceiling a1 10))
                  (b2 (floor a2 10)))
              (if (<= b1 b2) ; still a number divisible by 10?
                (setq a1 b1 a2 b2 d (+ d 1) d-shift t)
                (go no-10-more)))
            ;; 2.
            (when (>= (* 10 (setq a (floor a2 10))) a1)
              ;; Still a number divisible by 10 -> divide by 10.
              (setq d (+ d 1)) ; increase d, ten-d not needed anymore
              ;; Now convert a into a decimal string
              ;; and then discard the zeroes at the tail:
              (let* ((as (sys::decimal-string a)) ; digit sequence for a>0
                     (las (length as)) ; length of the digit sequence
                     (k las) ; length without the discarded zeroes at the tail
                     (ee (+ k d))) ; a * 10^d = a * 10^(-k+ee)
                (loop
                  (let ((k-1 (- k 1)))
                    (unless (eql (schar as k-1) #\0) ; a 0 at the end?
                      (return))
                    ;; yes -> a := a / 10 (but is not needed anymore),
                    ;; d := d+1 (but is not needed anymore),
                    (setq k k-1))) ; k := k-1.
                (when (< k las) (setq as (substring as 0 k)))
                (return (values as k ee sign))))
            ;; 3.
            no-10-more
            (setq a
              (if (eql a1 a2) ; a1=a2 -> there is no more choice:
                a1
                (if e-gross ; a1<a2 -> choose 10^d*a closest to x:
                  ;; choose a = round(f*2*binmant/2^g/(1oder10)) (any rounding)
                  ;;   = ceiling(floor(f*2*binmant/(1oder10)/2^(g-1))/2)
                  (ash (1+ (ash (let ((z (* f 2*binmant)))
                                  (if d-shift (floor z 10) z))
                                (- 1 g)))
                       -1)
                  ;; |e| small -> analogous as a2 was computed above
                  (if (not (minusp e))
                    ;; e>=0: a = round(2^e*2*binmant/10^d)
                    (round (ash 2*binmant e) ; 10^d according to d-shift
                           (if d-shift (* 10 ten-d) ten-d))
                    ;; e<0, so d<0, now (because of step 1) d<=0.
                    ;; a = round(2*binmant*10^(-d)/2^(-e))
                    (ash (1+ (ash
                               (* 2*binmant ; 10^(-d) according to d-shift
                                  (if d-shift (floor ten-d 10) ten-d))
                               (+ e 1)))
                         -1)))))
            (let* ((as (sys::decimal-string a)) ; digit sequence of a
                   (k (length as)))
              (return (values as k (+ k d) sign)))))))))

;; output function for PRINT/WRITE of floats:
(defun write-float-decimal (stream arg #| &optional (plus-sign-flag nil) |# )
  (unless (floatp arg)
    (error-of-type 'type-error
      :datum arg :expected-type 'float
      (TEXT "argument is not a float: ~S")
      arg))
  (multiple-value-bind (mantstring mantlen expo sign)
      (decode-float-decimal arg t)
    ;; arg in decimal representation: +/- 0.mant * 10^expo, whereby
    ;;  mant is the mantissa: as Simple-String mantstring of length mantlen,
    ;;  expo is the decimal-exponent,
    ;;  sign is the sign (-1 or 0 or 1).
    (if (eql sign -1) ; arg < 0 ?
      (write-char #\- stream)
      #| (if plus-sign-flag (write-char #\+ stream)) |#
    )
    (let ((flag (<= -2 expo 7))) ; arg=0 or 10^-3 <= (abs arg) < 10^7 ?
      ;; What to print? definition by cases:
      ;; flag set -> "fixed-point notation":
      ;;  expo <= 0 -> zero, decimal dot, -expo zeroes, all digits
      ;;  0 < expo < mantlen ->
      ;;     the first expo digits, decimal dot, the remaining digits
      ;;  expo >= mantlen -> all digits, expo-mantlen zeroes, decimal dot, zero
      ;;  if possible, no exponent; if necessary, exponent 0.
      ;; flag deleted -> "scientific notation":
      ;;   first digit, decimal dot, the remaining digits, with mantlen=1
      ;;    a zero exponent.
      (if (and flag (not (plusp expo)))
        ;; "fixed-point notation" with expo <= 0
        ;; first null and decimal dot, then -expo zeroes, then all digits
        (progn
          (write-char #\0 stream)
          (write-char #\. stream)
          (do ((i expo (+ i 1)))
              ((eql i 0))
            (write-char #\0 stream))
          (write-string mantstring stream)
          (setq expo 0)) ; exponent to print is 0
        ;; "fixed-point notation" with expo > 0 or "scientific notation"
        (let ((scale (if flag expo 1)))
          ;; the decimal dot is shifted by scale digits to the right,
          ;; i.e. there are scale digits in front of the dot. scale > 0.
          (if (< scale mantlen)
            ;; first scale digits, then decimal dot, then remaining digits:
            (progn
              (write-string mantstring stream :end scale)
              (write-char #\. stream)
              (write-string mantstring stream :start scale))
            ;; scale>=mantlen -> there are no digits behind the dot.
            ;; all digits, then scale-mantlen zeroes, then dot and zero
            (progn
              (write-string mantstring stream)
              (do ((i mantlen (+ i 1)))
                  ((eql i scale))
                (write-char #\0 stream))
              (write-char #\. stream)
              (write-char #\0 stream)))
          (decf expo scale))) ; the exponent to print is smaller by scale.
      ;; now let's go for the exponent:
      (let ((e-marker
              (cond ((and (not *PRINT-READABLY*)
                          (case *READ-DEFAULT-FLOAT-FORMAT*
                            (SHORT-FLOAT (short-float-p arg))
                            (SINGLE-FLOAT (single-float-p arg))
                            (DOUBLE-FLOAT (double-float-p arg))
                            (LONG-FLOAT (long-float-p arg))))
                     #\E)
                    ((short-float-p arg) #\s)
                    ((single-float-p arg) #\f)
                    ((double-float-p arg) #\d)
                    ((long-float-p arg) #\L))))
        (unless (and flag (eql e-marker #\E)) ; poss. omit Exponent entirely
          (write-char e-marker stream)
          (write expo :base 10 :radix nil :readably nil :stream stream))))))
