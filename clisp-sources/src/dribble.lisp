;;;; Dribble

(in-package "EXT")
(export '(make-dribble-stream dribble-stream-p dribble-stream
          dribble-stream-source dribble-stream-target dribble-toggle))

(in-package "SYSTEM")

(defun make-dribble-stream (source target)
  (make-two-way-stream (make-echo-stream source target)
                       (make-broadcast-stream source target)))
(defun dribble-stream (stream)
  (and (sys::two-way-stream-p stream)
       (let ((in (two-way-stream-input-stream stream))
             (out (two-way-stream-output-stream stream)))
         (and (sys::echo-stream-p in) (sys::broadcast-stream-p out)
              (let ((source (echo-stream-input-stream in))
                    (target (echo-stream-output-stream in))
                    (broadcast-list (broadcast-stream-streams out)))
                (when (and (eq source (pop broadcast-list))
                           (eq target (pop broadcast-list)))
                  (values source target)))))))
(defun dribble-stream-p (obj) (not (null (dribble-stream obj))))
;; should this be integrated into CLOS and the rest of CLISP?
;; right now DRIBBLE-STREAM is not a recognizable subtype of TWO-WAY-STREAM.
;; should it be?  should is be printed specially?
(deftype dribble-stream () '(satisfies dribble-stream-p))
(defun check-dribble-stream (obj caller)
  (loop
    (multiple-value-bind (so ta) (dribble-stream obj)
      (when so (return-from check-dribble-stream (values so ta))))
    (setq obj (check-value
               nil (make-condition 'simple-type-error
                     :format-control (TEXT "~S: ~S should be a ~S")
                     :format-arguments (list caller obj 'dribble-stream)
                     :datum obj :expected-type 'dribble-stream)))))
(defun dribble-stream-source (ds)
  (check-dribble-stream ds 'dribble-stream-source))
(defun dribble-stream-target (ds)
  (nth-value 1 (check-dribble-stream ds 'dribble-stream-target)))
(defun dribble-toggle (stream &optional file)
  (multiple-value-bind (source target) (dribble-stream stream)
    (if source
      (if file                  ; already dribbling
        (warn (TEXT "Already dribbling ~S to ~S") source target)
        (progn
          (fresh-line target)
          (format target (TEXT ";; Dribble of ~S finished ") source)
          (funcall (date-format) target (multiple-value-list (get-decoded-time)))
          (terpri target)
          (values source target)))
      (if file                    ; not dribbling
        (let ((target
                (if (and (streamp target)
                         (open-stream-p file) (output-stream-p file))
                  file
                  (open file :direction :output
                             :if-exists :append
                             :if-does-not-exist :create))))
          (format target (TEXT ";; Dribble of ~S started ") stream)
          (funcall (date-format) target (multiple-value-list (get-decoded-time)))
          (terpri target)
          (values (make-dribble-stream stream target) target))
        (warn (TEXT "Currently not dribbling from ~S.") stream)))))

(defun dribble (&optional file)
  (multiple-value-bind (source target) (dribble-toggle *terminal-io* file)
    (when (streamp source)         ; no warning
      (unless file (close target)) ; dribble off
      (setq *terminal-io* source))
    target))
