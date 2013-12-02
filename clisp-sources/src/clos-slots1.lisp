;;;; Common Lisp Object System for CLISP: Slots
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


#||
;; The access functions could look like this, if we use
;; SLOT-VALUE-USING-CLASS.

 ;; Access to the slots of objects of the metaclass <standard-class>.
 ;; Note that all functions that refer to #<UNBOUND> must be compiled.
 (defun std-slot-value (instance slot-name)
   (declare (compile))
   (let* ((class (class-of instance))
          (slot-location (gethash slot-name (class-slot-location-table class))))
     ((lambda (value)
        (if (eq value unbound)
          (slot-unbound class instance slot-name)
          value))
      (cond ((null slot-location)
             (slot-missing class instance slot-name 'slot-value))
            ((atom slot-location) ; access local slot
             (sys::%record-ref instance slot-location))
            (t ; access shared slot
             (svref (cv-shared-slots (car slot-location))
                    (cdr slot-location)))))))
 (defun std-setf-slot-value (instance slot-name new-value)
   (let* ((class (class-of instance))
          (slot-location (gethash slot-name (class-slot-location-table class))))
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'setf new-value))
           ((atom slot-location) ; access local slot
            (sys::%record-store instance slot-location new-value))
           (t ; access shared slot
            (setf (svref (cv-shared-slots (car slot-location))
                         (cdr slot-location))
                  new-value)))))
 (defun std-slot-boundp (instance slot-name)
   (declare (compile))
   (let* ((class (class-of instance))
          (slot-location (gethash slot-name (class-slot-location-table class))))
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'slot-boundp))
           ((atom slot-location) ; access local slot
            (not (eq (sys::%record-ref instance slot-location) unbound)))
           (t ; access shared slot
            (not (eq (svref (cv-shared-slots (car slot-location))
                            (cdr slot-location))
                     unbound))))))
 (defun std-slot-makunbound (instance slot-name)
   (declare (compile))
   (let* ((class (class-of instance))
          (slot-location (gethash slot-name (class-slot-location-table class))))
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'slot-makunbound))
           ((atom slot-location) ; access local slot
            (sys::%record-store instance slot-location unbound))
           (t ; access shared slot
            (setf (svref (cv-shared-slots (car slot-location))
                         (cdr slot-location))
                  unbound)))))
 (defun std-slot-exists-p (instance slot-name)
   (and (gethash slot-name (class-slot-location-table (class-of instance))) t))

 ;; General access to slots:
 (defun slot-value (object slot-name)
   (let ((class (class-of object)))
     ;; Treat metaclass <standard-class> separately for efficiency reasons
     ;; and because of bootstrapping.
     (if (eq (class-of class) <standard-class>)
       (std-slot-value object slot-name)
       (slot-value-using-class class object slot-name))))
 (defun (setf slot-value) (new-value object slot-name)
   (let ((class (class-of object)))
     ;; Treat metaclass <standard-class> separately for efficiency reasons
     ;; and because of bootstrapping.
     (if (eq (class-of class) <standard-class>)
       (std-setf-slot-value object slot-name new-value)
       (setf-slot-value-using-class new-value class object slot-name))))
 (defun slot-boundp (object slot-name)
   (let ((class (class-of object)))
     ;; Treat metaclass <standard-class> separately for efficiency reasons
     ;; and because of bootstrapping.
     (if (eq (class-of class) <standard-class>)
       (std-slot-boundp object slot-name)
       (slot-boundp-using-class class object slot-name))))
 (defun slot-makunbound (object slot-name)
   (let ((class (class-of object)))
     ;; Treat metaclass <standard-class> separately for efficiency reasons
     ;; and because of bootstrapping.
     (if (eq (class-of class) <standard-class>)
       (std-slot-makunbound object slot-name)
       (slot-makunbound-using-class class object slot-name))))
 (defun slot-exists-p (object slot-name)
   (let ((class (class-of object)))
     ;; Treat metaclass <standard-class> separately for efficiency reasons
     ;; and because of bootstrapping.
     (if (eq (class-of class) <standard-class>)
       (std-slot-exists-p object slot-name)
       (slot-exists-p-using-class class object slot-name))))

 (defun slot-value-using-class (class object slot-name)
   (no-slot-error class object slot-name))
 (defun setf-slot-value-using-class (new-value class object slot-name)
   (declare (ignore new-value))
   (no-slot-error class object slot-name))
 (defun slot-boundp-using-class (class object slot-name)
   (no-slot-error class object slot-name))
 (defun slot-makunbound-using-class (class object slot-name)
   (no-slot-error class object slot-name))
 (defun slot-exists-p-using-class (class object slot-name)
   (no-slot-error class object slot-name))

 (defun no-slot-error (class object slot-name)
   (declare (ignore slot-name))
   (error-of-type 'error
     (TEXT "instance ~S of class ~S has no slots (wrong metaclass)")
     object class))
||#

;; For efficiency - we want to circumvent the test for <standard-class> -,
;; all classes (no matter if standard- or built-in-) get a
;; slot-location-table.
;; Furthermore, we can deal here with unbound only very badly.
;; Hence,
;;   slot-value, set-slot-value, slot-boundp, slot-makunbound, slot-exists-p
;; are now contained already in RECORD.D.

(defsetf slot-value set-slot-value)

;; WITH-SLOTS
(defmacro with-slots (&whole whole-form
                      slot-entries instance-form &body body)
  (let ((vars '())
        (slots '()))
    (unless (listp slot-entries)
      (error-of-type 'ext:source-program-error
        :form whole-form
        :detail slot-entries
        (TEXT "~S: not a list of slots: ~S")
        'with-slots slot-entries))
    (dolist (slot slot-entries)
      (let ((var slot))
        (when (consp slot)
          (unless (eql (length slot) 2)
            (error-of-type 'ext:source-program-error
              :form whole-form
              :detail slot
              (TEXT "~S: invalid slot and variable specification ~S")
              'with-slots slot))
          (setq var (first slot) slot (second slot))
          (unless (symbolp var)
            (error-of-type 'ext:source-program-error
              :form whole-form
              :detail var
              (TEXT "~S: variable ~S should be a symbol")
              'with-slots var)))
        (unless (symbolp slot)
          (error-of-type 'ext:source-program-error
            :form whole-form
            :detail slot
            (TEXT "~S: slot name ~S should be a symbol")
            'with-slots slot))
        (push var vars)
        (push slot slots)))
    (multiple-value-bind (body-rest declarations) (sys::parse-body body)
      (let ((instance-var (gensym)))
        `(LET ((,instance-var ,instance-form))
           (SYMBOL-MACROLET
             ,(mapcar #'(lambda (var slot)
                          `(,var (SLOT-VALUE ,instance-var ',slot)))
                      (nreverse vars) (nreverse slots))
             ,@(if declarations `((DECLARE ,@declarations)))
             ,@body-rest))))))

;; WITH-ACCESSORS
(defmacro with-accessors (&whole whole-form
                          slot-entries instance-form &body body)
  (unless (listp slot-entries)
    (error-of-type 'ext:source-program-error
      :form whole-form
      :detail slot-entries
      (TEXT "~S: not a list of slots: ~S")
      'with-accessors slot-entries))
  (dolist (slot-entry slot-entries)
    (unless (and (consp slot-entry) (eql (length slot-entry) 2))
      (error-of-type 'ext:source-program-error
        :form whole-form
        :detail slot-entry
        (TEXT "~S: invalid slot and accessor specification ~S")
        'with-accessors slot-entry))
    (unless (symbolp (first slot-entry))
      (error-of-type 'ext:source-program-error
        :form whole-form
        :detail (first slot-entry)
        (TEXT "~S: variable ~S should be a symbol")
        'with-accessors (first slot-entry)))
    (unless (symbolp (second slot-entry))
      (error-of-type 'ext:source-program-error
        :form whole-form
        :detail (second slot-entry)
        (TEXT "~S: accessor name ~S should be a symbol")
        'with-accessors (second slot-entry))))
  (multiple-value-bind (body-rest declarations) (sys::parse-body body)
    (let ((instance-var (gensym)))
      `(LET ((,instance-var ,instance-form))
         (SYMBOL-MACROLET
           ,(mapcar #'(lambda (slot-entry)
                        `(,(first slot-entry)
                           (,(second slot-entry) ,instance-var)))
                    slot-entries)
           ,@(if declarations `((DECLARE ,@declarations)))
           ,@body-rest)))))

;; Low-level instance access. MOP p. 100, p. 55.
;; In standard-instance-access and funcallable-standard-instance-access,
;; - the instance can be any standard-object instance (funcallable or not),
;; - the instance can be a non-updated obsolete instance,
;; - the location can refer to a local slot or to a shared slot,
;; - when the slot is not bound, #<UNBOUND> is returned.
;; A setter function is also provided.
(setf (fdefinition 'funcallable-standard-instance-access)
      #'standard-instance-access)
(system::%put 'funcallable-standard-instance-access 'SYSTEM::SETF-FUNCTION
              '|(SETF STANDARD-INSTANCE-ACCESS)|)
