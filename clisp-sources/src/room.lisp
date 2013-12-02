;;;; Room, Space

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; ROOM

(definternational room-format (t ENGLISH))
(deflocalized room-format ENGLISH
  (list (formatter "Class~VT# Instances  Size (bytes)  Average size~%")
        (formatter "-----~VT-----------  ------------  ------------~%")
        (formatter      "~VT~8D     ~9D  ~13,3F~%")
) )

(defun room (&optional (kind :default))
  (unless (or (eq kind 'nil) (eq kind ':default) (eq kind 't))
    (error (TEXT "~S: argument must be ~S, ~S or ~S, not ~S")
           'room 'nil 't ':default kind
  ) )
  ; Get the figures now, because (heap-statistics) causes heap allocation.
  (multiple-value-bind (used room static) (sys::%room)
    (when (eq kind 't)
      (let ((stat (heap-statistics)))
        ; stat = #( ... (classname num-instances . num-bytes) ...)
        (setq stat (sort stat #'> :key #'cddr))
        (let* ((localinfo (localized 'room-format))
               (header-line (first localinfo))
               (separator-line (second localinfo))
               (data-line (third localinfo))
               (midcol (- (or *print-right-margin* sys::*prin-linelength*) 39))
               (total-instances 0)
               (total-bytes 0))
          (fresh-line)
          (format t header-line midcol)
          (format t separator-line midcol)
          (dotimes (i (length stat))
            (let* ((stat-record (svref stat i))
                   (classname (car stat-record))
                   (instances (cadr stat-record))
                   (bytes (cddr stat-record)))
              (when (plusp instances)
                (prin1 classname)
                (format t data-line midcol
                          instances bytes
                          (/ (float bytes 0d0) instances)
                )
                (incf total-instances instances)
                (incf total-bytes bytes)
          ) ) )
          (format t separator-line midcol)
          (write-string (TEXT "Total")
          )
          (format t data-line midcol
                    total-instances total-bytes
                    (/ (float total-bytes 0d0) total-instances)
          )
    ) ) )
    (unless (eq kind 'nil)
      (terpri)
      (format t (TEXT "Bytes permanently allocated:   ~9D~%Bytes currently in use:        ~9D~%Bytes available until next GC: ~9D")
                static used room)
      (terpri)
    )
    (values used room)
) )

;;-----------------------------------------------------------------------------
;; SPACE

;; Recall the macro SPACE, making (space form) roughly equivalent to
;
; (multiple-value-bind (var1 var2 var3 var4) (%space1)
;   (let ((*gc-statistics* (1+ (max *gc-statistics* 0))))
;     (setq var3 (multiple-value-list form))
;     (setq var4 (%space2))
;   )
;   (%space var1 var2 var3 var4)
;   (values-list var3)
; )

;; It works like this:
;; 1. Do a GC, to eliminate unused objects, and call (heap-statistics)
;;    to get a statistics of all live objects.
;; 2. Enable the GC reclaimage statistics: Call (gc-statistics),
;;    then bind *gc-statistics* to at least 1.
;; 3. Evaluate the form.
;; 4. Convert the form's values to a list.
;; 5. Call (gc), so that the GC eliminates temporarily allocated objects.
;; 6. Call (gc-statistics) again. We can now unbind *gc-statistics*.
;; 7. Call (heap-statistics) again, to get a statistics of all now live
;;    objects.
;; ==> The difference of the two (gc-statistics) calls will comprise exactly
;;     the temporary allocations of the form.
;; ==> The difference of the two (heap-statistics) calls will comprise
;;     - the resulting data structure of the first call to (heap-statistics),
;;     - the permanent allocations of the form,
;;     - the list allocated to hold the form's values,
;;     - the data structures which form the ldifference between the two
;;       calls to (gc-statistics).

(defun %space1 () ; ABI
  (gc)
  (values
    (heap-statistics)
    (gc-statistics)
) )

; %space1 returns two values, so var3 and var4 are initially bound to nil.

(defun %space2 () ; ABI
  (gc)
  (gc-statistics)
)

(defun %space (heap-stat-1 gc-stat-1 vallist gc-stat-2) ; ABI
  (let ((heap-stat-2 (heap-statistics)))
    ;; Now we have all the statistics, and are free to do any kind
    ;; of allocations.
    (let ((ht (make-hash-table :key-type 't :value-type '(cons cons cons)
                               :test 'fasthash-eq)))
      ;; For each type, (gethash type ht) contains a cons
      ;;   (heap-stat-record . gc-stat-record),
      ;; where both records are conses (n-instances . n-bytes).
      (flet ((add (statv accessor incrementer)
               ;; Adds the contents of statv to the hash table.
               ;; accessor = #'car or #'cdr, incrementer = #'+ or #'-.
               (dotimes (i (length statv))
                 (let ((stat-record (svref statv i)))
                   (let ((name (car stat-record))
                         (n-instances (cadr stat-record))
                         (n-bytes (cddr stat-record)))
                     (unless (and (zerop n-instances) (zerop n-bytes))
                       (let ((htx (gethash name ht)))
                         (unless htx
                           (setf (gethash name ht)
                                 (setf htx (cons (cons 0 0) (cons 0 0)))
                         ) )
                         (let ((record (funcall accessor htx)))
                           (setf (car record)
                                 (funcall incrementer (car record) n-instances)
                           )
                           (setf (cdr record)
                                 (funcall incrementer (cdr record) n-bytes)
                           )
            )) ) ) ) ) ) )
        ;; The difference between the two (gc-statistics) calls.
        (do ((l gc-stat-2 (cdr l)))
            ((eq l gc-stat-1))
          (add (car l) #'cdr #'+)
        )
        ;; The difference between the two (heap-statistics) calls.
        (add heap-stat-2 #'car #'+)
        (add heap-stat-1 #'car #'-)
        ;; Adjust for the statistics data structures themselves.
        (add (heap-statistics-statistics heap-stat-1) #'car #'-)
        (add (list-statistics vallist) #'car #'-)
        (add (gc-statistics-statistics gc-stat-1 gc-stat-2) #'car #'-)
      )
      ;; Now transform the hash table into a list, and sort it.
      (let ((statlist '()))
        (maphash #'(lambda (name htx) (push (cons name htx) statlist)) ht)
        ;; statlist is now a list of (name heap-stat-record . gc-stat-record).
        (setq statlist
              (sort statlist #'>
                    :key #'(lambda (r) (+ (cdr (cadr r)) (cdr (cddr r))))
        )     )
        ;; Display the statistics.
        (space-tabulate statlist)
        ;; Done. The return value is ignored.
) ) ) )

(definternational space-format (t ENGLISH))
(deflocalized space-format ENGLISH
  (list (formatter      "~VT     Permanent            Temporary~%")
        (formatter "Class~VTinstances   bytes    instances   bytes~%")
        (formatter "-----~VT--------- ---------  --------- ---------~%")
        (formatter      "~VT~9D ~9D  ~9D ~9D~%")
) )

(defun space-tabulate (statlist)
  (let* ((localinfo (localized 'space-format))
         (header-line1 (first localinfo))
         (header-line2 (second localinfo))
         (separator-line (third localinfo))
         (data-line (fourth localinfo))
         (midcol (- (or *print-right-margin* sys::*prin-linelength*) 40))
         (total-perm-instances 0)
         (total-perm-bytes 0)
         (total-temp-instances 0)
         (total-temp-bytes 0)
         (stream *trace-output*))
    (fresh-line stream)
    (format stream header-line1 midcol)
    (format stream header-line2 midcol)
    (format stream separator-line midcol)
    (dolist (stat-record statlist)
      (let* ((classname (car stat-record))
             (permstat (cadr stat-record))
             (tempstat (cddr stat-record))
             (perm-instances (car permstat))
             (perm-bytes (cdr permstat))
             (temp-instances (car tempstat))
             (temp-bytes (cdr tempstat)))
        (unless (and (zerop perm-instances) (zerop perm-bytes)
                     (zerop temp-instances) (zerop temp-bytes)
                )
          (prin1 classname stream)
          (format stream data-line midcol
                  perm-instances perm-bytes
                  temp-instances temp-bytes
          )
          (incf total-perm-instances perm-instances)
          (incf total-perm-bytes perm-bytes)
          (incf total-temp-instances temp-instances)
          (incf total-temp-bytes temp-bytes)
    ) ) )
    (format stream separator-line midcol)
    (write-string (TEXT "Total")
                  stream
    )
    (format stream data-line midcol
            total-perm-instances total-perm-bytes
            total-temp-instances total-temp-bytes
    )
) )
