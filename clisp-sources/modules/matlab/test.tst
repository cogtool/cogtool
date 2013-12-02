;; -*- Lisp -*-
;; some tests for Matlab
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "matlab/test")'

(matlab:invert-matrix #2a((1 2) (0 2)))
#2A((1d0 -1d0) (0d0 5d-1))

(let ((mx #2a((1 2 3) (4 5 6))))
  (matlab:copy-lisp-to-matlab mx "foo")
  (matlab:copy-matlab-to-lisp "foo" (make-array (array-dimensions mx))))
#2A((1d0 2d0 3d0) (4d0 5d0 6d0))

(let ((mx #2a((1 2 3) (4 5 6))))
  (matlab:copy-lisp-to-matlab mx "foo")
  (matlab:engEvalString matlab:*engine* "bar=foo'")
  (matlab:copy-matlab-to-lisp
   "bar" (make-array (reverse (array-dimensions mx)))))
#2A((1d0 4d0) (2d0 5d0) (3d0 6d0))

(let ((buffer (ffi:allocate-shallow '(ffi:c-array-max ffi:character 1024))))
  (unwind-protect
       (progn (matlab:engOutputBuffer matlab:*engine* buffer 1024)
              (matlab:engEvalString matlab:*engine* "sin(pi)")
              (ffi:foreign-value buffer))
    (matlab:engOutputBuffer matlab:*engine* NIL 0)
    (ffi:foreign-free buffer)))
"ans =
  1.2246e-016
"

(defvar *matfile* "tmp.mat") *matfile*

(matlab:with-MATfile (mf *matfile* "w")
  (list (matlab:matPutVariable
         mf "foo" (matlab:copy-lisp-to-mxArray #2a((1 2 3) (4 5 6))))
        (matlab:matPutVariable
         mf "bar" (matlab:copy-lisp-to-mxArray #2a((7 8) (9 0))))))
(0 0)

(matlab:with-MATfile (mf *matfile* "r")
  (let ((dir (matlab:matfile-content mf)))
    (list dir
          (map 'vector
               (lambda (var)
                 (let ((mx (matlab:matGetVariableInfo mf var)))
                   (list var
                         (ffi:enum-from-value 'matlab:mxClassID
                                              (matlab:mxGetClassID mx))
                         (matlab:mxGetNumberOfDimensions mx)
                         (matlab:mxGetNumberOfElements mx)
                         (matlab:mxGetElementSize mx)
                         (matlab:mxGetM mx) (matlab:mxGetN mx)
                         (matlab:mxGetData mx))))
               dir)
          (map 'vector
               (lambda (var)
                 (list var (matlab:copy-mxArray-to-lisp
                            (matlab:matGetVariable mf var))))
               dir))))
(#("foo" "bar")
 #(("foo" matlab:mxDOUBLE_CLASS 2 6 8 2 3 NIL)
   ("bar" matlab:mxDOUBLE_CLASS 2 4 8 2 2 NIL))
 #(("foo" #2A((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0)))
   ("bar" #2A((7.0d0 8.0d0) (9.0d0 0.0d0)))))

(not (delete-file *matfile*)) NIL
