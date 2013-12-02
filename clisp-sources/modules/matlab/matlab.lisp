;;; Matlab API interface
;;; <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>
;;;
;;; Copyright (C) 2004-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "MATLAB"
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:shadowing-import-from "EXPORTING"
           #:defconstant #:defun #:defmacro #:defvar
           #:def-c-type #:def-c-enum #:def-c-struct #:def-c-var #:def-call-out))

(in-package "MATLAB")

(setf (documentation (find-package "MATLAB") 'sys::impnotes) "matlab")

;;; types and constants


;;; foreign function definitions

(ffi:default-foreign-language :stdc)

;;; --- Engine ---
(c-lines "#include <engine.h>~%")
(def-c-type Engine c-pointer)
;; int engClose(Engine *ep);
(def-call-out engClose (:arguments (ep Engine)) (:return-type int))
;; Engine *engOpen(const char *startcmd);
(def-call-out engOpen (:arguments (startcmd c-string)) (:return-type Engine))

;; int engEvalString(Engine *ep, const char *string);
(def-call-out engEvalString (:arguments (ep Engine) (cmd c-string))
  (:return-type int))
;; int engOutputBuffer(Engine *ep, char *p, int n);
(def-call-out engOutputBuffer (:arguments (ep Engine) (p c-pointer) (n int))
  (:return-type int))

(def-c-type mxArray c-pointer)
;; mxArray *engGetVariable(Engine *ep, const char *name);
(def-call-out engGetVariable (:arguments (ep Engine) (name c-string))
  (:return-type mxArray))
;; int engPutVariable(Engine *ep, const char *name, const mxArray *mp);
(def-call-out engPutVariable
    (:arguments (ep Engine) (name c-string) (mp mxArray))
  (:return-type int))

;;; windows-only
;; int engGetVisible(Engine *ep, bool *value);
#+win32
(def-call-out engGetVisible
    (:arguments (ep Engine) (value (c-ptr boolean) :out))
  (:return-type int))
;; int engSetVisible(Engine *ep, bool value);
#+win32
(def-call-out engSetVisible (:arguments (ep Engine) (value boolean))
  (:return-type int))

;;; windows-only
;; Engine *engOpenSingleUse(const char *startcmd, void *dcom, int *retstatus);
#+win32
(def-call-out engOpenSingleUse
    (:arguments (startcmd c-string) (dcom c-pointer)
                (retstatus (c-ptr int) :out))
  (:return-type Engine))

;;; --- MAT-File ---
(c-lines "#include <mat.h>~%")
(def-c-type MATFile c-pointer)
;; int matClose(MATFile *mfp);
(def-call-out matClose (:arguments (mfp MATFile)) (:return-type int))
;; MATFile *matOpen(const char *filename, const char *mode);
(def-call-out matOpen (:arguments (filename c-string) (mode c-string))
  (:return-type MATFile))

;; char **matGetDir(MATFile *mfp, int *num);
(def-call-out matGetDir (:arguments (mfp MATFile) (num (c-ptr int) :out))
  (:return-type c-pointer)) ; release with mxFree()

;; FILE *matGetFp(MATFile *mfp);
(def-call-out matGetFp (:arguments (mfp MATFile)) (:return-type c-pointer))

;; mxArray *matGetNextVariable(MATFile *mfp, const char *name);
(def-call-out matGetNextVariable
    (:arguments (mfp MATFile) (name (c-ptr c-string) :out))
  (:return-type mxArray))

;; mxArray *matGetNextVariableInfo(MATFile *mfp, const char *name);
(def-call-out matGetNextVariableInfo
    (:arguments (mfp MATFile) (name (c-ptr c-string) :out))
  (:return-type mxArray))

;; mxArray *matGetVariable(MATFile *mfp, const char *name);
(def-call-out matGetVariable (:arguments (mfp MATFile) (name c-string))
  (:return-type mxArray))
;; mxArray *matGetVariableInfo(MATFile *mfp, const char *name);
(def-call-out matGetVariableInfo (:arguments (mfp MATFile) (name c-string))
  (:return-type mxArray))
;; int matPutVariable(MATFile *mfp, const char *name, const mxArray *mp);
(def-call-out matPutVariable
    (:arguments (mfp MATFile) (name c-string) (mp mxArray))
  (:return-type int))
;; int matDeleteVariable(MATFile *mfp, const char *name);
(def-call-out matDeleteVariable (:arguments (mfp MATFile) (name c-string))
  (:return-type int))
;; int matPutVariableAsGlobal(MATFile*mfp, const char*name, const mxArray*mp);
(def-call-out matPutVariableAsGlobal
    (:arguments (mfp MATFile) (name c-string) (mp mxArray))
  (:return-type int))



;;; --- MEX ---
(c-lines "#include <mex.h>~%")
;; int mexAtExit(void (*ExitFcn)(void));
(def-call-out mexAtExit (:arguments (func (c-function (:arguments))))
  (:return-type int))
;; extern int mexCallMATLAB(
;;     int         nlhs,                   /* number of expected outputs */
;;     mxArray     *plhs[],                /* pointer array to outputs */
;;     int         nrhs,                   /* number of inputs */
;;     mxArray     *prhs[],                /* pointer array to inputs */
;;     const char  *fcn_name               /* name of function to execute */
;;     );
(def-call-out mexCallMATLAB
    (:arguments (nlhs int) (plhs (c-ptr (c-array-max mxArray 50)) :out)
                (nrhs int) (prhs (c-array-max mxArray 50))
                (fcn_name c-string))
  (:return-type int))
;; Issue error message and return to MATLAB prompt
;; extern void mexErrMsgTxt(const char *error_msg);
(def-call-out mexErrMsgTxt (:arguments (error_msg c-string))
  (:return-type nil))
;; Issue formatted error message with corresponding error identifier and
;; return to MATLAB prompt.
;; extern void mexErrMsgIdAndTxt(
;;    const char * identifier, /* string with error message identifier */
;;    const char * err_msg,    /* printf-style format */
;;    ...                      /* any additional arguments */
;;    );
(def-call-out mexErrMsgIdAndTxt
    (:arguments (identifier c-string) (err_msg c-string))
  (:return-type nil))
;; Invoke an unidentified warning. Such warnings can only be affected by
;; the M-code 'warning * all', since they have no specific identifier.
;; extern void mexWarnMsgTxt(const char *warn_msg);
(def-call-out mexWarnMsgTxt (:arguments (warn_msg c-string))
  (:return-type nil))
;; Invoke a warning with message identifier 'identifier' and message
;; derived from 'fmt' and subsequent arguments. The warning may either
;; get printed as is (if it is set to 'on'), or not actually get printed
;; (if set to 'off'). See 'help warning' in MATLAB for more details.
;; extern void mexWarnMsgIdAndTxt(
;;     const char * identifier,    /* string with warning message identifer */
;;     const char * warn_msg,      /* printf-style format */
;;     ...                         /* any additional arguments */
;;     );
(def-call-out mexWarnMsgIdAndTxt
    (:arguments (identifier c-string) (warn_msg c-string))
  (:return-type nil))

;; Parse and execute MATLAB syntax in string.
;; Returns zero if successful, and a non zero value if an error occurs.
;; extern int mexEvalString(const char *str /* matlab command string */);
(def-call-out mexEvalString (:arguments (command c-string))
  (:return-type int))
;; mexFunction is the user defined C routine which is called upon
;; invocation of a mex function.
;; void mexFunction(
;;     int           nlhs,      /* number of expected outputs */
;;     mxArray       *plhs[],   /* array of pointers to output arguments */
;;     int           nrhs,      /* number of inputs */
;;     const mxArray *prhs[]    /* array of pointers to input arguments */
;; );
;(def-call-out mexFunction
;    (:arguments (nlhs int) (plhs (c-ptr (c-array-max mxArray 50)) :out)
;                (nrhs int) (prhs (c-array-max mxArray 50)))
;  (:return-type nil))
;; Return the name of a the MEXfunction currently executing.
;; extern const char *mexFunctionName(void);
(def-call-out mexFunctionName (:arguments) (:return-type c-string))
;; API interface which mimics the "get" function
;; extern const mxArray *mexGet(double handle, const char *property);

(def-call-out mexGet (:arguments (handle double-float) (property c-string))
  (:return-type mxArray))
;; mex equivalent to MATLAB's "set" function
;; extern int mexSet(double handle, const char *property, mxArray *value);
(def-call-out mexSet
    (:arguments (handle double-float) (property c-string) (value mxArray))
  (:return-type int))

;; return a copy of the array value with the specified variable name in
;; the specified workspace
;; extern mxArray *mexGetVariable(const char *workspace, const char *name);
(def-call-out mexGetVariable (:arguments (workspace c-string) (name c-string))
  (:return-type mxArray))
;; return a pointer to the array value with the specified variable name
;; in the specified workspace
;; extern const mxArray *mexGetVariablePtr(const char *workspace, const char *name);
(def-call-out mexGetVariablePtr
    (:arguments (workspace c-string) (name c-string))
  (:return-type mxArray))

;; Tell whether or not a mxArray is in MATLAB's global workspace.
;; extern bool mexIsGlobal(const mxArray *pA);
(def-call-out mexIsGlobal (:arguments (arr mxArray)) (:return-type boolean))

;; Lock a MEX-function so that it cannot be cleared from memory.
;; extern void mexLock(void);
(def-call-out mexLock (:arguments) (:return-type nil))
;; Unlock a locked MEX-function so that it can be cleared from memory.
;; extern void mexUnlock(void);
(def-call-out mexUnlock (:arguments) (:return-type nil))
;; Return true if the MEX-function is currently locked, false otherwise.
;; extern bool mexIsLocked(void);
(def-call-out mexIsLocked (:arguments) (:return-type boolean))

;; Remove all components of an array plus the array header itself
;; from MATLAB's memory allocation list.  The array will now
;; persist between calls to the mex function.  To destroy this
;; array, you will need to explicitly call mxDestroyArray().
;; extern void mexMakeArrayPersistent(mxArray *pa);
(def-call-out mexMakeArrayPersistent (:arguments (arr mxArray))
  (:return-type nil))
;; Remove memory previously allocated via mxCalloc from MATLAB's
;; memory allocation list.  To free this memory, you will need to
;; explicitly call mxFree().
;; extern void mexMakeMemoryPersistent(void *ptr);
(def-call-out mexMakeMemoryPersistent (:arguments (ptr c-pointer))
  (:return-type nil))

;; mex equivalent to MATLAB's "disp" function
;; extern int mexPrintf(const char *fmt, ...);

;; Place a copy of the array value into the specified workspace with the
;; specified name
;; extern int mexPutVariable(const char *workspace,const char *name,const mxArray *parray);
(def-call-out mexPutVariable
    (:arguments (workspace c-string) (name c-string) (arr mxArray))
  (:return-type int))

;; set or clear mexCallMATLAB trap flag (if set then an error in
;; mexCallMATLAB is caught and mexCallMATLAB will return a status value,
;; if not set an error will cause control to revert to MATLAB)
;; extern void mexSetTrapFlag(int flag);
(def-call-out mexSetTrapFlag (:arguments (flag int)) (:return-type nil))

;;; --- MX --- <FIXME:incomplete>
(c-lines "#include <matrix.h>~%")
(defconstant mxMAXNAM 64)

;; extern int mxAddField(mxArray array_ptr, const char *field_name);
(def-call-out mxAddField (:arguments (array_ptr mxArray) (field_name c-string))
  (:return-type int))

;; void mxFree(void *ptr);
(def-call-out mxFree (:arguments (ptr c-pointer)) (:return-type nil))

;; typedef enum mxComplexity {mxREAL=0, mxCOMPLEX};
(def-c-enum mxComplexity (mxREAL 0) mxCOMPLEX})

;; mxArray *mxCreateDoubleMatrix(int m, int n, mxComplexity ComplexFlag);
(def-call-out mxCreateDoubleMatrix (:return-type (c-pointer mxArray))
  (:arguments (m int) (n int) (complexflag mxComplexity)))
;; void mxDestroyArray(mxArray *array_ptr);
(def-call-out mxDestroyArray (:return-type nil)
  (:arguments (array_ptr (c-pointer mxArray))))

;; mxArray *mxCreateDoubleScalar(double value);
(def-call-out mxCreateDoubleScalar (:return-type (c-pointer mxArray))
  (:arguments (value double-float)))

;; double mxGetEps(void);
(def-call-out mxGetEps (:return-type double-float) (:arguments))

;; double *mxGetPr(const mxArray *array_ptr);
(def-call-out mxGetPr (:return-type (c-pointer double-float))
  (:arguments (array_ptr (c-pointer mxArray))))
;; double *mxGetPi(const mxArray *array_ptr);
(def-call-out mxGetPi (:return-type (c-pointer double-float))
  (:arguments (array_ptr (c-pointer mxArray))))
;; void mxSetPr(mxArray *array_ptr, double *pr);
(def-call-out mxSetPr (:return-type nil)
  (:arguments (array_ptr (c-pointer mxArray))
              (data (c-pointer double-float))))
;; void mxSetPi(mxArray *array_ptr, double *pi);
(def-call-out mxSetPi (:return-type nil)
  (:arguments (array_ptr (c-pointer mxArray))
              (data (c-pointer double-float))))
;; get/set an individial array element
;; real
(c-lines "double mx_aref_r (const mxArray *array_ptr, int i, int j, int n) { return mxGetPr(array_ptr)[i+n*j]; }~%")
(def-call-out mx-aref-r (:return-type double-float) (:name "mx_aref_r")
  (:arguments (array_ptr (c-pointer mxArray))
              (i int) (j int) (n int)))
(c-lines "void set_mx_aref_r (const mxArray *array_ptr, int i, int j, int n, double val) { mxGetPr(array_ptr)[i+n*j] = val; }~%")
(ffi:def-call-out set_mx_aref_r (:return-type nil)
  (:arguments (array_ptr (c-pointer mxArray)) (i int) (j int) (n int)
              (val double-float)))
(defsetf mx-aref-r set_mx_aref_r)
;; imaginary
(c-lines "double mx_aref_i (const mxArray *array_ptr, int i, int j, int n) { return mxGetPr(array_ptr)[i+n*j]; }~%")
(def-call-out mx-aref-i (:return-type double-float) (:name "mx_aref_i")
  (:arguments (array_ptr (c-pointer mxArray))
              (i int) (j int) (n int)))
(c-lines "void set_mx_aref_i (const mxArray *array_ptr, int i, int j, int n, double val) { mxGetPr(array_ptr)[i+n*j] = val; }~%")
(ffi:def-call-out set_mx_aref_i (:return-type nil)
  (:arguments (array_ptr (c-pointer mxArray)) (i int) (j int) (n int)
              (val double-float)))
(defsetf mx-aref-i set_mx_aref_i)
;; void *mxGetData(const mxArray *array_ptr);
(def-call-out mxGetData (:return-type c-pointer)
  (:arguments (array_ptr (c-pointer mxArray))))

;; int mxGetNumberOfDimensions(const mxArray *array_ptr);
(def-call-out mxGetNumberOfDimensions (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; const int *mxGetDimensions(const mxArray *array_ptr);
(def-call-out mxGetDimensions (:return-type (c-pointer int))
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetNumberOfElements(const mxArray *array_ptr);
(def-call-out mxGetNumberOfElements (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetElementSize(const mxArray *array_ptr);
(def-call-out mxGetElementSize (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetNumberOfFields(const mxArray *array_ptr);
(def-call-out mxGetNumberOfFields (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetNzmax(const mxArray *array_ptr);
(def-call-out mxGetNzmax (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetM(const mxArray *array_ptr);
(def-call-out mxGetM (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))
;; int mxGetN(const mxArray *array_ptr);
(def-call-out mxGetN (:return-type int)
  (:arguments (array_ptr (c-pointer mxArray))))

;; double mxGetScalar(const mxArray *array_ptr);
(def-call-out mxGetScalar (:return-type double-float)
  (:arguments (array_ptr (c-pointer mxArray))))

;; bool mxIsCell(const mxArray *array_ptr);
(def-call-out mxIsCell (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsChar(const mxArray *array_ptr);
(def-call-out mxIsChar (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsComplex(const mxArray *array_ptr);
(def-call-out mxIsComplex (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsDouble(const mxArray *array_ptr);
(def-call-out mxIsDouble (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsEmpty(const mxArray *array_ptr);
(def-call-out mxIsEmpty (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsInt8(const mxArray *array_ptr);
(def-call-out mxIsInt8 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsInt16(const mxArray *array_ptr);
(def-call-out mxIsInt16 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsInt32(const mxArray *array_ptr);
(def-call-out mxIsInt32 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsInt64(const mxArray *array_ptr);
(def-call-out mxIsInt64 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsLogical(const mxArray *array_ptr);
(def-call-out mxIsLogical (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsLogicalScalarTrue(const mxArray *array_ptr);
(def-call-out mxIsLogicalScalarTrue (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsNumeric(const mxArray *array_ptr);
(def-call-out mxIsNumeric (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsSparse(const mxArray *array_ptr);
(def-call-out mxIsSparse (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsSingle(const mxArray *array_ptr);
(def-call-out mxIsSingle (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsStruct(const mxArray *array_ptr);
(def-call-out mxIsStruct (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsUint8(const mxArray *array_ptr);
(def-call-out mxIsUint8 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsUint16(const mxArray *array_ptr);
(def-call-out mxIsUint16 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsUint32(const mxArray *array_ptr);
(def-call-out mxIsUint32 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsUint64(const mxArray *array_ptr);
(def-call-out mxIsUint64 (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray))))
;; bool mxIsClass(const mxArray *array_ptr, const char *name);
(def-call-out mxIsClass (:return-type boolean)
  (:arguments (array_ptr (c-pointer mxArray)) (name c-string)))

(def-c-enum mxClassID
  (mxUNKNOWN_CLASS 0)
  mxCELL_CLASS
  mxSTRUCT_CLASS
  mxLOGICAL_CLASS
  mxCHAR_CLASS
  mxSPARSE_CLASS                ; OBSOLETE! DO NOT USE
  mxDOUBLE_CLASS
  mxSINGLE_CLASS
  mxINT8_CLASS
  mxUINT8_CLASS
  mxINT16_CLASS
  mxUINT16_CLASS
  mxINT32_CLASS
  mxUINT32_CLASS
  mxINT64_CLASS                 ; place holder - future enhancements
  mxUINT64_CLASS                ; place holder - future enhancements
  mxFUNCTION_CLASS
  mxOPAQUE_CLASS
  mxOBJECT_CLASS)
;; mxClassID mxGetClassID(const mxArray *array_ptr);
(def-call-out mxGetClassID (:return-type mxClassID)
  (:arguments (array_ptr (c-pointer mxArray))))
;; const char *mxGetClassName(const mxArray *array_ptr);
(def-call-out mxGetClassName (:return-type c-string)
  (:arguments (array_ptr (c-pointer mxArray))))


(provide "matlab")
