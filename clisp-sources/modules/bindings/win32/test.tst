;; -*- Lisp -*-
;; some tests for WIN32
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "bindings/win32/test")'

(defmacro show-mv (form) `(listp (show (multiple-value-list ,form) :pretty t)))
SHOW-MV

(stringp (show (win32:GetCommandLineA))) T
(integerp (show (win32:GetLastError))) T
(integerp (show (win32:GetCurrentProcessId))) T
(integerp (show (win32:GetCurrentThreadId))) T

(let ((version (win32:GetProcessVersion 0)))
  (listp (show (list (ldb (byte 16 0) version)
                     (ldb (byte 16 16) version)))))
T

(if (fboundp 'win32:GetProcessIdOfThread) ; longhorn
    (win32:with-handle (thread (win32:GetCurrentThread))
      (= (win32:GetProcessIdOfThread thread)
         (win32:GetCurrentProcessId)))
    t)
T

(if (fboundp 'win32:GetThreadId) ; longhorn
    (win32:with-handle (thread (win32:GetCurrentThread))
      (= (win32:GetThreadId thread)
         (win32:GetCurrentThreadId)))
    t)
T

(if (fboundp 'win32:GetProcessId) ; longhorn
    (win32:with-handle (process (win32:GetCurrentProcess))
      (= (win32:GetProcessId process)
         (win32:GetCurrentProcessId)))
    t)
T

(win32:with-handle (thread (win32:GetCurrentThread))
  (show-mv (win32:GetThreadPriority thread)))
T

(win32:with-handle (thread (win32:GetCurrentThread))
  (show-mv (win32:GetThreadPriorityBoost thread)))
T

(win32:with-handle (process (win32:GetCurrentProcess))
  (show-mv (win32:GetProcessPriorityBoost process)))
T

(win32:with-handle (handle (win32:GetModuleHandleA "kernel32"))
  (show-mv (win32:GetModuleFileNameA handle win32:MAX_PATH)))
T

(show-mv (win32:GetConsoleTitleA win32:BUFSIZ)) T
(show-mv (win32:GetConsoleScreenBufferInfo (win32:GetConsoleWindow))) T
(show-mv (win32:GetConsoleScreenBufferInfo
          (win32:GetStdHandle win32:STD_OUTPUT_HANDLE))) T
(show-mv (win32:GetSystemDirectoryA win32:MAX_PATH)) T
(show-mv (win32:GetWindowsDirectoryA win32:MAX_PATH)) T
(show-mv (win32:GetCurrentDirectoryA win32:MAX_PATH)) T
(let ((version (win32:GetVersion)))
  (listp (show (list (ldb (byte 16 0) version)
                     (ldb (byte 16 16) version)))))
T
(show-mv (win32:GetUserNameA win32:UNLEN)) T
(show-mv (win32:GetComputerNameA win32:MAX_COMPUTERNAME_LENGTH)) T

(defun check-all (enum-type function buf-size)
  (format t "~&;; ~s:~%" function)
  (maphash (lambda (key val)
             (let ((res (multiple-value-list (funcall function key buf-size))))
               (format t " ~S -> ~S~@[ ~S~]~%" val res
                       (unless (car res) (w32:GetLastError)))))
           (ffi::enum-table enum-type)))
CHECK-ALL

(check-all 'w32:EXTENDED_NAME_FORMAT 'w32:GetUserNameExA w32:UNLEN) NIL
(check-all 'w32:EXTENDED_NAME_FORMAT 'w32:GetComputerObjectNameA w32:UNLEN) NIL
(check-all 'w32:COMPUTER_NAME_FORMAT 'w32:GetComputerNameExA
           w32:MAX_COMPUTERNAME_LENGTH) NIL
