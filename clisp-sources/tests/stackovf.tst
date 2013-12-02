;; Testing stack overflow behaviour.

;; CLISP typically recovers well from endless recursion when the
;; SP-consumption : STACK-consumption ratio is low. When it is high,
;; as in the function below, it crashes. This test is good for testing
;; the stack overflow catching mechanisms (guard page, alternate stack).

(defun f (n)
  (if (>= n -9)
    (if (>= n -8)
      (if (>= n -7)
        (if (>= n -6)
          (if (>= n -5)
            (if (>= n -4)
              (if (>= n -3)
                (if (>= n -2)
                  (if (>= n -1)
                    (if (>= n 0)
                      (* (f (1+ n)) n)
) ) ) ) ) ) ) ) ) ) )
F

(f 0)
ERROR

