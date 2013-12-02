;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author      : John Anderson & Dan Bothell
;;; Copyright   : (c) 2007
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : bold.lisp
;;; Version     : 2.0
;;;
;;; Description : Computes predictions of the BOLD response based on activity
;;;               of the buffers in a module.
;;;
;;; Bugs        :
;;;
;;; To do       :
;;;
;;; ----- History -----
;;;
;;; 2007.06.05 Dan
;;;             : * Initial creation.
;;; 2007.06.25 John
;;;             : * Previously, all data was displayed with a maximum point of 1.   
;;;             :   However, this makes it impossible to compare two conditions 
;;;             :   where the response is suppose to be larger in one of the conditions.   
;;;             :   Therefore, I changed the normalizing process so that it now  
;;;             :   assumes the gamma distribution with constant area of 1.   This  
;;;             :   means that if one region is active longer it will add up to 
;;;             :   produce a bigger response.
;;;             :   Right now the normalization only works for integer exponents.
;;;             :   If one replaced bold_factorial by a more general gamma function  
;;;             :   one could have continuous exponents.
;;; 2007.07.16 Dan
;;;             : * Adding in the support code to provide some graphing tools on
;;;             :   the environment side.
;;; 2007.07.18 Dan
;;;             : * Added the reset function to handle the caching of data for
;;;             :   the environment.
;;;             : * Added the code that draws the ROI in the brain images in
;;;             :   the environment.
;;; 2007.07.20 Dan
;;;             : * Cleaning up the graphing and brain region code to allow
;;;             :   for "normalizing" within regions instead of across all
;;;             :   as an option.
;;; 2007.08.02 Dan
;;;             : * Added the function to create the data lists needed for the
;;;             :   crude 3d viewer.
;;;             : * Cleaned up the viewer functions so they don't crash the
;;;             :   inspectors when there's no data.
;;; 2007.08.08 Dan
;;;             : * Sped things up considerably by taking out the sgp calls
;;;             :   and just passing the module's parameters into the critical
;;;             :   computation functions.
;;; 2007.08.10 Dan
;;;             : * Added the calls to handle the "on the fly" scanning and
;;;             :   a mechanism that records the max value in a region which
;;;             :   persists across a reset (but not clear-all).  The max value
;;;             :   computation allows for the scaling on the fly assuming that
;;;             :   there was a previous run with reasonable max values.
;;; 2007.08.13 Dan
;;;             : * Changed bold-brain-3d-data so that it puts out the data
;;;             :   in lists the same way the real-time viewer uses so that the
;;;             :   same display code can be used by either viewer.
;;; 2007.08.14 Dan
;;;             : * Modified the brain data functions so that they send out the
;;;             :   floating point value instead of scaling 0-20 so that the
;;;             :   display can show that 0-1.0 number.
;;; 2007.08.23 Dan
;;;             : * Changed the test on the parameter from fixnump to integerp
;;;             :   since not all Lisps provide a fixnump but integerp is in
;;;             :   the ANSI spec.
;;; 2007.08.31 Dan
;;;             : * Added start and end times as optional parameters for the 
;;;             :   predict-bold-response which restricts the predictions to
;;;             :   the range specified.
;;;             : * Added the :bold-settle parameter which controls the window
;;;             :   in which the predictions are computed.
;;; 2007.09.04 Dan
;;;             : * Make sure that the start time is a multiple of bold-inc to
;;;             :   ensure consistency in the results when checking ranges, and if
;;;             :   the value provided isn't then it uses the closest lower value
;;;             :   which is an increment (and gives a warning).
;;; 2007.09.04 Dan
;;;             : * Modified parse-bold-predictions-for-graph so that it can
;;;             :   draw data from a subset of the time sequence (provided by
;;;             :   start and end times in the new dialog.
;;; 2007.10.10 Dan [1.1]
;;;             : * Adding a saftey check to predict-bold-response to catch
;;;             :   when there isn't a sample's worth of time available.
;;; 2008.04.29 Dan [1.2]
;;;             : * Changed the default values for the scale and exponent to
;;;             :   .75 and 6 respectively.
;;; 2010.07.14 Dan [2.0]
;;;             : * Adding a negative component to the hemodynamic curve.
;;;             :   It is now composed of two gamma distributions one of which
;;;             :   is subtracted from the other.  There are separate scale and
;;;             :   exponent parameters for each distribution and there are two
;;;             :   additional parameters used as the coefficients in combining
;;;             :   the curves.  The predicted values are now computed based on:
;;;             :
;;;             :       (c1*positive - c2*negative)/ (c1 - c2)
;;;             :
;;;             :   where c1 is the :bold-positive parameter, c2 is the :bold-negative
;;;             :   parameter, positive is the distribution generated from the :bold-scale
;;;             :   and :bold-exp parameters, and negative is the distribution generated
;;;             :   from the :neg-bold-scale and :neg-bold-exp parameters.
;;;             :
;;;             :   The defaults for c1 and c2 are 1 and 0 which leaves the calculation
;;;             :   the same as it was previously.
;;;             :   
;;;             :   One suggestion for useful values would be to set all of the
;;;             :   parameters like this which would be the defaults used by SPM
;;;             :   for computing hemodynamic curves:
;;;             :   
;;;             :    :bold-exp 5 
;;;             :    :bold-scale 1 
;;;             :    :neg-bold-exp 15 
;;;             :    :neg-bold-scale 1 
;;;             :    :bold-positive 6 
;;;             :    :bold-negative 1
;;;             :        
;;; 2010.07.16 Dan
;;;             : * Changed bold-point to ignore possible underflow errors and just
;;;             :   return 0.
;;; 2010.07.20 Dan
;;;             : * Updated the environment code that generates Tcl/Tk colors to
;;;             :   just use black for negatives as well.
;;; 2011.03.04 Dan
;;;             : * Added a bold-demand-functions function that works like predict-
;;;             :   bold-response except it doesn't convolve with the hemodynamic 
;;;             :   response curve. So the results are 1 if the buffer was busy during 
;;;             :   that time and 0 if not with the times being reported at the start 
;;;             :   of an increment instead of the midpoints.  So if one sets 
;;;             :   :bold-inc to .001 it would report 0/1 for free/busy for the
;;;             :   buffer over the whole run.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; After running a model, you can call the predict-bold-response function to get a
;;; printout of the BOLD predictions for all of the buffers that were traced
;;; during the run.  It will return a list of lists where each sub-list is the
;;; data for a buffer which was traced.  The car of the list is the buffer's name
;;; and the rest is the BOLD values spaced by the :bold-inc time (where the first
;;; point occurs at :bold-inc/2 not 0).  Wrapping the call with no-output will 
;;; prevent the printing if one only wants to get the data returned.
;;;
;;; To use the tool you must set the parameter :save-buffer-trace to t in the
;;; model.  The buffers set with the :traced-buffers parameter will be the ones
;;; for which the predictions will be printed.
;;;
;;; There are two mechanisms that can be used to determine the bold response -
;;; either point based which assumes each request occurs instantaneously at the
;;; time of the request or interval based which considers all the time which
;;; the module spends as busy during the processing of the request.  For most
;;; modules the interval based approach seems the more reasonable one, but for
;;; a module like the goal module, which spends no time busy with requests, the
;;; point prediction would have to be used to produce any predictions.  There
;;; is a parameter which controls how each buffer is computed, and by default
;;; only the goal buffer uses the point based prediction (visual-location is
;;; another buffer for which there is no time spent busy with requests, but
;;; predictions for visual processing are typically done by tracking the visual
;;; buffer).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Commands:
;;;
;;; predict-bold-response {start {end}}
;;;
;;;  Prints out and returns the BOLD response predictions based on module requests 
;;;  through buffers after a model has been run.  The optional parameters start and
;;;  end can be provided to specify the starting and ending times for which the 
;;;  predictions should be generated.  The defaults are 0 for the start and the
;;;  current model time for the end if not provided.  The start time should be 
;;;  a multiple of the time increment (the :bold-inc parameter).  If it is not,
;;;  then a warning will be printed and the closest lower multiple will be used.
;;;
;;; Parameters:
;;;
;;; :bold-scale (default .75)
;;;
;;;  The scale parameter used in the computation of the positive component of the BOLD response.
;;;
;;; :bold-exp (default 6)
;;;
;;;  The exponenet parameter used in the computation of the positive component of the BOLD response.
;;;
;;; :neg-bold-scale (default 1)
;;;
;;;  The scale parameter used in the computation of the negative component of the BOLD response.
;;;
;;; :bold-exp (default 15)
;;;
;;;  The exponenet parameter used in the computation of the negative component of the BOLD response.
;;;
;;; :bold-positive (default 1)
;;;
;;;  The coefficient for the positive component of the BOLD response.
;;;
;;; :bold-negative (default 0)
;;;
;;;  The coefficient for the negative component of the BOLD response.
;;;
;;; :point-predict (default (goal))
;;;
;;;  The list of buffers for which the point prediction mechanism will be used.
;;;
;;; :bold-settle (default 40)
;;;
;;;  The time in seconds used as the window of time for computing the 
;;;  bold value - the value at time T is based on the events that have
;;;  happened between time  (- T :bold-settle) and time T.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct bold-module
   scale exp inc settle point env-cache v cmdt buffers max-table c1 c2 neg-exp neg-scale)

(defun bold_cumulative (time time1 time2 scale exp)
   (let ((average (/ (+ time1 time2) 2)))
     (* (+ (bold_point (- time time1) scale exp) (* 2 (bold_point (- time average) scale exp)) (bold_point (- time time2) scale exp)) (- time2 time1) .25)))


(defun bold_cumulative* (time time1 time2 inc scale exp)
  (setf time2 (min time time2))
  (do ((x (- time2 inc) (- x inc))
       (y time2 x)
       (sum 0 (+ sum (bold_cumulative time x y scale exp))))
      ((< x time1) (+ sum (bold_cumulative time time1 y scale exp)))))

(defun bold_cumulative-bold (times time inc scale exp)
   (do ((temp times (cdr temp))
        (sum 0 (+ sum (bold_cumulative* time (first (car temp)) (second (car temp)) inc scale exp))))
       ((or (null temp) (> (caar temp) time)) sum)))

(defun bold_point (time s exp)
  (let* ((scale (/ time s))
         (result (ignore-errors (* (expt scale exp) (exp (- scale))))))
     (if result result 0)))

(defun bold_value (times time scale exp)
   (do ((temp times (cdr temp))
        (sum 0 (+ sum (bold_point (- time (car temp)) scale exp))))
       ((or (null temp) (> (car temp) time)) sum)))

(defun bold_point-predict (times bm start end)
  (let ((pos (bold_point-predict_result times bm start end (bold-module-exp bm) (bold-module-scale bm)))
        (neg (unless (zerop (bold-module-c2 bm))
               (bold_point-predict_result times bm start end (bold-module-neg-exp bm) (bold-module-neg-scale bm)))))
    
    (if neg
        (let ((c1 (bold-module-c1 bm))
              (c2 (bold-module-c2 bm)))
          (mapcar (lambda (p n) (/ (- (* c1 p) (* c2 n)) (- c1 c2))) pos neg))
      pos)))

(defun bold_point-predict_result (times bm start end exp scale)
  (let* (results
          (inc (bold-module-inc bm))
          (settle (bold-module-settle bm))
          (time (+ start (/ inc 2))))
     (while (<= time (- end (/ inc 2)))
       (setf times (member (- time settle) times :test '<))
       (setf results (cons (bold_value times time scale exp) results))
       (incf time inc))
     (bold_normalize-list-values (reverse results) bm exp scale)))

(defun bold_interval-predict (times bm start end)
  (let ((pos (bold_interval-predict_result times bm start end (bold-module-exp bm) (bold-module-scale bm)))
        (neg (unless (zerop (bold-module-c2 bm))
               (bold_interval-predict_result times bm start end (bold-module-neg-exp bm) (bold-module-neg-scale bm)))))
    
    (if neg
        (let ((c1 (bold-module-c1 bm))
              (c2 (bold-module-c2 bm)))
          (mapcar (lambda (p n) (/ (- (* c1 p) (* c2 n)) (- c1 c2))) pos neg))
      pos)))
  
(defun bold_interval-predict_result (times bm start end exp scale)
  (let* (results
         (inc (bold-module-inc bm))
          (settle (bold-module-settle bm))
          (time (+ start (/ inc 2))))
     (while (<= time (- end (/ inc 2)))
       (setf times (member (- time settle) times :test '< :key 'car))
       (setf results (cons (bold_cumulative-bold times time inc scale exp) results))
       (incf time inc))
     (bold_normalize-list-values (reverse results) bm exp scale)))

(defun bold_factorial (n)
   (do* ((i n (1- i))
         (v i (* v i)))
        ((= i 1) v)))

(defun bold_normalize-list-values (lis bm exp scale)
   (when lis
     (let ((top (* (bold_factorial exp) scale)))
       (if (zerop top)
           lis
         (mapcar #'(lambda (x) (/ x top)) lis)))))

(defun parse-trace-lists-for-bold (bm)
   (let* ((trace (get-current-buffer-trace))
          (b (bold-module-buffers bm))
          (buffers (if (listp b) b (buffers)))
          (all-data nil))

     (dolist (x buffers)
       (let ((rects nil)
             (current-rect nil))
           (dolist (z trace)
             (let ((record (find x (buffer-record-buffers z) :key 'buffer-summary-name)))
               (if current-rect
                  (when (or (null (buffer-summary-busy record))
                             (buffer-summary-busy->free record)
                             (buffer-summary-request record))

                    (push (cons current-rect (list (buffer-record-time-stamp z))) rects)
                    (if (buffer-summary-request record)
                         (setf current-rect (buffer-record-time-stamp z))
                       (setf current-rect nil)))

                 (if (buffer-summary-busy record)
                   (if (and (buffer-summary-request record)
                            (or (buffer-summary-chunk-name record)
                                (and (buffer-summary-error record)
                                     (not (buffer-summary-error->clear record)))
                                (buffer-summary-busy->free record)))
                       (push (cons (buffer-record-time-stamp z) (list (buffer-record-time-stamp z))) rects)
                     (setf current-rect (buffer-record-time-stamp z)))
                   (if (buffer-summary-request record)
                       (push (cons (buffer-record-time-stamp z) (list (buffer-record-time-stamp z))) rects)
                     (when (buffer-summary-chunk-name record)
                       (push (cons (buffer-record-time-stamp z) (list (buffer-record-time-stamp z))) rects)))))))

         (push (cons x (reverse rects)) all-data)))

      all-data))


(defun predict-bold-response (&optional (start 0) (end (mp-time)))
  (let* ((bm (get-module bold))
         (data (parse-trace-lists-for-bold bm))
         (bold nil)
         (point (bold-module-point bm))
         (inc (bold-module-inc bm)))
    
    (if (< (- end start) inc)
        (print-warning "Sample time too short for BOLD predictions - must be at least :bold-inc seconds (currently ~s)" inc)
      (progn
        (unless (zerop (mod start inc))
          (setf start (* inc (floor start inc)))
          (model-warning "Start time should be a multiple of :bold-inc (~S).  Using start time of ~S."
                         inc start))
        
        (dolist (x data)
          (if (find (car x) point)
              (push (cons (car x) (bold_point-predict (mapcar #'car (cdr x)) bm start end)) bold)
            (push (cons (car x) (bold_interval-predict (cdr x) bm start end)) bold)))
        
        ;; Cache the max values for each buffer so that they can
        ;; be used in normalizing things later if desired
        
        (dolist (x bold)
          (let* ((buffer (car x))
                 (data (cdr x))
                 (max (when data (apply #'max data))))
            (when (or (null (gethash buffer (bold-module-max-table bm)))
                      (> max (gethash buffer (bold-module-max-table bm))))
              (setf (gethash buffer (bold-module-max-table bm)) max))))
        (output-bold-response-data bold bm start end)
        bold))))


(defun bold-demand-functions (&optional (start 0) (end (mp-time)))
  (let* ((bm (get-module bold))
         (data (parse-trace-lists-for-bold bm))
         (bold nil)
         (point (bold-module-point bm))
         (inc (bold-module-inc bm)))
    
    (if (< (- end start) inc)
        (print-warning "Sample time too short for BOLD predictions - must be at least :bold-inc seconds (currently ~s)" inc)
      (progn
        (unless (zerop (mod start inc))
          (setf start (* inc (floor start inc)))
          (model-warning "Start time should be a multiple of :bold-inc (~S).  Using start time of ~S."
                         inc start))
        
        (dolist (x data)
          (push (cons (car x) (bold_demand (cdr x) bm start end)) bold))
        
        (output-bold-demand-data bold bm start end)
        bold))))


(defun bold_demand (times bm start end)
  (let* ((inc (bold-module-inc bm))
         (results (make-list (ceiling (- end start) inc) :initial-element 0)))
    (dolist (x times)
      (if (= (car x) (cadr x))
          (setf (nth (floor (car x) inc) results) 1)
        (dotimes (i (ceiling (- (cadr x) (car x)) inc))
          (setf (nth (+ i (floor (car x) inc)) results) 1))))
    results))
    
  
(defun output-bold-demand-data (data bm start end)
  (when (and data (bold-module-v bm) (bold-module-cmdt bm))
    (do* ((times (list 'time))
          (inc (bold-module-inc bm))
          (x start (+ x inc)))
         ((> x end) (push (reverse times) data))
      (push x times))    
    
    (let* ((transposed (apply 'mapcar 'list data))
           (max-len (+ 2 (apply 'max (mapcar (lambda (x) (length (symbol-name x))) (first transposed))))))
      
      (command-output "~?" (format nil "~~{~~~d,@a~~}" max-len) transposed)
      (command-output (format nil "~~{~~{~~~d,3f~~}~~%~~}" max-len) (cdr transposed)))))


        

(defun output-bold-response-data (data bm start end)
  (when (and data (bold-module-v bm) (bold-module-cmdt bm))
    (do* ((times (list 'time))
          (inc (bold-module-inc bm))
          (x (+ start (/ inc 2)) (+ x inc)))
         ((> x end) (push (reverse times) data))
      (push x times))    
    
    (let* ((transposed (apply 'mapcar 'list data))
           (max-len (+ 2 (apply 'max (mapcar (lambda (x) (length (symbol-name x))) (first transposed))))))
      
      (command-output "~?" (format nil "~~{~~~d,@a~~}" max-len) transposed)
      (command-output (format nil "~~{~~{~~~d,3f~~}~~%~~}" max-len) (cdr transposed)))))

(defun reset-bold-module (bm)
  (setf (bold-module-env-cache bm) (make-hash-table)))

(defun create-bold-module (name) 
  (declare (ignore name))
  (let ((bm (make-bold-module)))
    (setf (bold-module-max-table bm) (make-hash-table))
    bm))



(defun handle-bold-params (instance param)
   (cond ((consp param)
          (case (car param)
            (:v
            (setf (bold-module-v instance) (cdr param)))
            (:cmdt
             (setf (bold-module-cmdt instance) (cdr param)))
            (:traced-buffers
             (setf (bold-module-buffers instance) (cdr param)))
            (:bold-scale
             (setf (bold-module-scale instance) (cdr param)))
            (:bold-exp
             (setf (bold-module-exp instance) (cdr param)))
            (:bold-settle
             (setf (bold-module-settle instance) (cdr param)))
            
            (:neg-bold-exp
             (setf (bold-module-neg-exp instance) (cdr param)))
            (:neg-bold-scale
             (setf (bold-module-neg-scale instance) (cdr param)))
            (:bold-positive
             (setf (bold-module-c1 instance) (cdr param)))
            (:bold-negative
             (setf (bold-module-c2 instance) (cdr param)))
            
            (:bold-inc
             (setf (bold-module-inc instance) (cdr param)))
            (:point-predict
             (setf (bold-module-point instance) (cdr param)))))
         (t
          (case param
            (:bold-scale
             (bold-module-scale instance))
            (:bold-exp
             (bold-module-exp instance))
            
            (:neg-bold-exp
             (bold-module-neg-exp instance))
            (:neg-bold-scale
             (bold-module-neg-scale instance))
            (:bold-positive
             (bold-module-c1 instance))
            (:bold-negative
             (bold-module-c2 instance))
            
            (:bold-settle
             (bold-module-settle instance))
            (:bold-inc
             (bold-module-inc instance))
            (:point-predict
             (bold-module-point instance))))))

(define-module-fct 'bold nil
   (list
    (define-parameter :v :owner nil)
    (define-parameter :cmdt :owner nil)
    (define-parameter :traced-buffers :owner nil)
    (define-parameter :bold-scale
      :valid-test #'numberp
      :warning "a number"
      :default-value 0.75
      :documentation "Scale parameter for computing the BOLD response.")
    (define-parameter :bold-exp
      :valid-test #'integerp
      :warning "an integer"
      :default-value 6
      :documentation "Exponenet parameter for computing the BOLD response.")
    
    (define-parameter :neg-bold-scale
      :valid-test #'numberp
      :warning "a number"
      :default-value 1
      :documentation "Scale parameter for computing a negative component of the BOLD response.")
    (define-parameter :neg-bold-exp
      :valid-test #'integerp
      :warning "an integer"
      :default-value 15
      :documentation "Exponenet parameter for computing a negative component of the BOLD response.")
    
    (define-parameter :bold-positive
      :valid-test #'nonneg
      :warning "a non-negative number"
      :default-value 1
      :documentation "Factor for the positive component of the hemodynamic response curve.")
    (define-parameter :bold-negative
      :valid-test #'nonneg
      :warning "a non-negative number"
      :default-value 0
      :documentation "Factor for the negative component of the hemodynamic response curve.")
    

    
    (define-parameter :bold-inc
      :valid-test #'posnum
      :warning "a positive number"
      :default-value 1.5
      :documentation "Time increment in seconds for computing the BOLD response.")
    (define-parameter :bold-settle
      :valid-test #'posnum
      :warning "a positive number"
      :default-value 40
      :documentation "Time window in seconds for computing the BOLD response.")
    (define-parameter :point-predict
      :valid-test #'(lambda (x) (and (listp x) (every (lambda (y) (find y (buffers))) x)))
      :warning "a list of buffer names"
      :default-value (list 'goal)
      :documentation "List of buffers for which the point based computation should be used to compute the BOLD response."))

   :creation 'create-bold-module
  :reset 'reset-bold-module
  :params #'handle-bold-params
   :version "2.0"
   :documentation "A module to produce BOLD response predictions from buffer request activity.")

(defun bold-data-buffer-max (buffer)
  (let ((bm (get-module bold)))
    (gethash buffer (bold-module-max-table bm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The code below supports the tools available through the environment.
;;;; 

(defparameter *pixels-per-second-for-bold* 100)
(defparameter *bold-vertical-scale* 400)

(defun cache-bold-data (dialog)
  (let ((bm (get-module bold)))
    (aif (gethash dialog (bold-module-env-cache bm))
         it
         (setf (gethash dialog (bold-module-env-cache bm)) (predict-bold-response)))))

(defun uncache-bold-data (dialog)
  (let ((bm (get-module bold)))
    (remhash dialog (bold-module-env-cache bm)))
  (list 'gone))

(defun parse-bold-predictions-for-graph (chart buffer local start end)
  
  (let* ((d (no-output (cache-bold-data chart)))
         (all-data (when (> (length (car d)) 1)
                     (if local
                         (normalize-bold-data-for-env-local d)
                       (normalize-bold-data-for-env d))))
         (time-inc (car (no-output (sgp :bold-inc))))
         (buffer-data (cdr (find buffer all-data :key #'car)))
         (data nil)
         (s-index (if (= start -1) 0 (min (floor start time-inc) (length buffer-data))))
         (e-index (if (= end -1) (length buffer-data) (min (floor end time-inc) (length buffer-data))))
         (s-time (* s-index time-inc))
         (e-time (* e-index time-inc))
         )
    
    (if (> s-index e-index) 
        (push (list 'text "No data: start time > end time" 40 40 "#f00") data)
      
      (progn 
        (setf buffer-data (subseq buffer-data s-index e-index))     
        (if buffer-data
            (progn
              
              (push (list 'size (round (+ 50 (* (1+ (length buffer-data)) time-inc *pixels-per-second-for-bold*)))
                          (+ *bold-vertical-scale* 40))
                    data)
              (push (list 'line 40 20 40 (+ 20 *bold-vertical-scale*) "#000") data)
              (push (list 'line 40 (+ 20 *bold-vertical-scale*) 
                          (round (+ 40 (* (length buffer-data) time-inc *pixels-per-second-for-bold*))) 
                          (+ 20 *bold-vertical-scale*) "#000") data)
              (dotimes (i 21)
                (push (list 'line 35 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 20)))
                            45 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 20)))
                            "#000") 
                      data)
                (push (list 'text_y (format nil "~4,2f" (* i .05)) 35 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 20)))
                            "#000") 
                      data))
              (dotimes (i (1+ (length buffer-data)))
                (push (list 'line (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 15 *bold-vertical-scale*)
                            (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 25 *bold-vertical-scale*)
                            "#000") 
                      data)
                (push (list 'text_x (format nil "~4,2f" (+ s-time (* i time-inc)))
                            (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 25 *bold-vertical-scale*)
                            "#000") 
                      data))
          
          (do ((p1 (butlast buffer-data) (cdr p1))
               (p2 (cdr buffer-data) (cdr p2))
               (inc (round (* *pixels-per-second-for-bold* time-inc)))
               (time (+ 40 (round (* *pixels-per-second-for-bold* (/ time-inc 2))))
                     (round (+ time inc))))
              ((null p1))
               (push (list 'line time (- (+ 20 *bold-vertical-scale*)
                                         (* (car p1) *bold-vertical-scale*))
                           (+ time inc) (- (+ 20 *bold-vertical-scale*)
                                           (* (car p2) *bold-vertical-scale*)) "#f00")
                     data)))
      (push (list 'text "No Data Available" 40 40 "#f00") data))))
  
  (let ((result nil))
    (dolist (x data)
      (push (format nil "~{~S ~}" x) result))
    result)))



(defun normalize-bold-data-for-env (data)
  (let ((max (apply #'max (mapcar (lambda (x) (apply #'max (cdr x))) data)))
        (new-data (copy-tree data)))
    (when (zerop max)
      (setf max 1.0))
    (dotimes (i (1- (length (car data))))
      (dolist (j new-data)
        (setf (nth (1+ i) j) (/ (nth (1+ i) j) max))))
    new-data))


(defun normalize-bold-data-for-env-local (data)
  (let ((new-data (copy-tree data)))
    (dolist (j new-data)
      (let ((max (apply #'max (cdr j))))
        (when (zerop max)
          (setf max 1.0))
        (dotimes (i (1- (length j)))
          (setf (nth (1+ i) j) (/ (nth (1+ i) j) max)))))
  new-data))


(defun bold-brain-data-results (chart local)
  (let* ((d (no-output (cache-bold-data chart)))
         (result nil)
         (size (length (first d)))
         )
    
    (if (= 1 size)
        (make-list 8 :initial-element (format nil "{} \"#000\""))
      (let ((data (if local
                      (normalize-bold-data-for-env-local d)
                    (normalize-bold-data-for-env d))))
        
        
        (do ((region '(manual goal vocal imaginal retrieval production aural visual) (cdr region))
             (color '("#~2,'0x0000" "#00~2,'0x00" "#0000~2,'0x" "#~2,'0x~2,'0x00" "#00~2,'0x~2,'0x" "#~2,'0x00~2,'0x" "#00~2,'0x00" "#0000~2,'0x") (cdr color))
             )
            ((null region))
          (let ((nums (assoc (car region) data)))
            (if nums
                (push (format nil "{} ~{~s ~}" 
                        (mapcar (lambda (x) 
                                  (if (= 2 (count #\x (car color)))
                                      (format nil (car color)
                                        (max 0 (floor (* (floor (* x 20)) (/ 255 20))))
                                        (max 0 (floor (* (floor (* x 20)) (/ 255 20)))))
                                    (format nil (car color) (max 0 (floor (* (floor (* x 20)) (/ 255 20)))))))
                          (cdr nums)))
                      result)
              (push (format nil "~{~a ~}" (make-list size :initial-element "{}")) result))))
        (reverse result)))))

(defun bold-brain-3d-data ()
  (let* ((d  (no-output (predict-bold-response)))
         (result nil)
         (size (length (first d)))
         )
    (if (= 1 size)
        (list "none" "none" "none" "none" "none" "none" "none" "none")
      (let ((data (normalize-bold-data-for-env-local d)))
        (dolist (region '(manual goal vocal imaginal retrieval production aural visual))
          (let ((nums (assoc region data)))
            (if nums
                (push (cons 'none (mapcar (lambda (x) 
                                   x)
                          (cdr nums)))
                     result)
              (push (make-list size :initial-element "none") result))))))
        (mapcar #'(lambda (x) (format nil "~{~s ~}" x)) (apply 'mapcar 'list (reverse result)))))


;; Code for the run-time viewer of bold data in the environment

(defvar *brain-scan-event* nil)

(defun remove-brain-scan (event)
  (declare (ignore event))
  (when *brain-scan-event*
    (delete-event *brain-scan-event*)
    (setf *brain-scan-event* nil))
  "close")

(defun start-brain-scan (event)
  (if (subtypep (type-of event) 'environment-handler)
    (let* ((bm (get-module bold))
           (inc (bold-module-inc bm)))
      
      (setf *brain-scan-event* (schedule-periodic-event inc 'brain-scan :module 'bold :priority :max :params (list event) :details "Brain-scan" :initial-delay inc :maintenance t))
      "{none none none none none none none none}")
    event))
        

(defun brain-scan (handler)
  (let ((bm (get-module bold))
        (d (no-output (predict-bold-response)))
        (result ""))
        
        (dolist (region '(manual goal vocal imaginal retrieval production aural visual))
          (let ((nums (assoc region d)))
            (setf result (concatenate 'string result 
                           (if nums
                               (let* ((val (car (last nums)))
                                      (max (gethash region (bold-module-max-table bm))))
                                 (if (numberp val)
                                     (princ-to-string 
                                      (if (and max (numberp max) (not (zerop max)))
                                          (/ val max)
                                        val))
                                   "none"))
                                 "none")
                           " "))))
            
    (update-handler handler (concatenate 'string "{" result "}"))))



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
