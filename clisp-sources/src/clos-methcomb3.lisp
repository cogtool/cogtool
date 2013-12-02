;;;; Common Lisp Object System for CLISP
;;;; Method Combination
;;;; Part n-2: make/initialize-instance methods.
;;;; Bruno Haible 2004-06-10

(in-package "CLOS")


;;; Lift the initialization protocol.

(defmethod initialize-instance ((combination method-combination) &rest args
                                 &key name
                                      documentation
                                      check-options
                                      expander
                                      check-method-qualifiers
                                      call-next-method-allowed
                                      declarations
                                      qualifiers
                                      operator
                                      identity-with-one-argument
                                      long-expander
                                      arguments-lambda-list
                                      options)
  (declare (ignore name documentation check-options expander
                   check-method-qualifiers call-next-method-allowed
                   declarations qualifiers operator identity-with-one-argument
                   long-expander arguments-lambda-list options))
  (apply #'initialize-instance-<method-combination> combination args))

(defmethod reinitialize-instance ((instance method-combination) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: It is not allowed to reinitialize ~S")
         'reinitialize-instance instance))
