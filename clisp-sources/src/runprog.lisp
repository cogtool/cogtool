;;;; Running external programs

(in-package "EXT")
#+WIN32 (export '(execute))
#+(or UNIX WIN32) (export '(run-shell-command run-program))
(in-package "SYSTEM")

;;-----------------------------------------------------------------------------

#+(or UNIX WIN32)
;; UNIX:
; Must quote the program name and arguments since Unix shells interpret
; characters like #\Space, #\', #\<, #\>, #\$ etc. in a special way. This
; kind of quoting should work unless the string contains #\Newline and we
; call csh. But we are lucky: only /bin/sh will be used.
;; WIN32:
; Must quote program name and arguments since Win32 interprets characters
; like #\Space, #\Tab, #\\, #\" (but not #\< and #\>) in a special way:
; - Space and Tab are interpreted as delimiters. They are not treated as
;   delimiters if they are surrounded by double quotes: "...".
; - Unescaped double quotes are removed from the input. Their only effect is
;   that within double quotes, space and tab are treated like normal characters.
; - Backslashes not followed by double quotes are not special.
; - But 2*n+1 backslashes followed by a double quote become
;   n backslashes followed by a double quote (n >= 0):
;     \" -> "
;     \\\" -> \"
;     \\\\\" -> \\"
; The high-level Win32 command interpreter cmd.exe (but not the low-level
; function CreateProcess()) also interprets #\&, #\<, #\>, #\| as special
; delimiters and makes #\^ disappear. To avoid this, quote them like spaces.
(labels (#+UNIX
         (shell-simple-quote (string)
           (shell-quote string)
         )
         #+UNIX
         (shell-quote (string) ; surround a string by single quotes
           (if (eql (length string) 0)
             "''"
             (let ((qchar nil) ; last quote character: nil or #\' or #\"
                   (qstring (make-array 10 :element-type 'character
                                           :adjustable t :fill-pointer 0)))
               (map nil #'(lambda (c)
                            (let ((q (if (eql c #\') #\" #\')))
                              (unless (eql qchar q)
                                (when qchar (vector-push-extend qchar qstring))
                                (vector-push-extend (setq qchar q) qstring)
                              )
                              (vector-push-extend c qstring)))
                        string
               )
               (when qchar (vector-push-extend qchar qstring))
               qstring
         ) ) )
         #+WIN32
         (shell-simple-quote (string) ; protect against spaces only
           ; Also protect the characters which are special for the command
           ; interpreter. This is needed only if the command interpreter
           ; will be called, but doesn't hurt if CreateProcess() will be
           ; called directly.
           (if (or (eql (length string) 0)
                   (some #'(lambda (c)
                             (or ; space?
                                 (<= (char-code c) 32)
                                 ; special delimiter?
                                 (eql c #\&)
                                 (eql c #\<)
                                 (eql c #\>)
                                 (eql c #\|)
                                 (eql c #\^)
                           ) )
                         string
               )   )
             (string-concat "\"" string "\"")
             string
         ) )
         #+WIN32
         (shell-quote (string) ; full protection
           (let ((quote-around
                   (or (eql (length string) 0)
                       (some #'(lambda (c)
                                 (or ; space?
                                     (<= (char-code c) 32)
                                     ; special delimiter?
                                     (eql c #\&)
                                     (eql c #\<)
                                     (eql c #\>)
                                     (eql c #\|)
                                     (eql c #\^)
                               ) )
                             string)))
                 (qstring (make-array 10 :element-type 'character
                                         :adjustable t :fill-pointer 0))
                 (backslashes 0))
             (when quote-around
               (vector-push-extend #\" qstring)
             )
             (map nil #'(lambda (c)
                          (when (eql c #\")
                            (dotimes (i (+ backslashes 1))
                              (vector-push-extend #\\ qstring)
                          ) )
                          (vector-push-extend c qstring)
                          (if (eql c #\\)
                            (incf backslashes)
                            (setq backslashes 0)
                        ) )
                      string
             )
             (when quote-around
               (dotimes (i backslashes)
                 (vector-push-extend #\\ qstring)
               )
               (vector-push-extend #\" qstring)
             )
             qstring
         ) )
         ; conversion to a string that works for a pathname as well
         (xstring (object)
           (if (pathnamep object)
             (namestring (absolute-pathname object))
             (if (symbolp object)
               (princ-to-string object)
               (string object)))))
  #+WIN32
  (defun execute (programfile &rest arguments)
    (shell
      (apply #'string-concat
             (shell-simple-quote (xstring programfile))
             (mapcan #'(lambda (argument)
                         (list " " (shell-quote (xstring argument)))
                       )
                     arguments
  ) ) )      )
  (defun run-shell-command (command &key (input ':terminal) (output ':terminal)
                                         (if-output-exists ':overwrite)
                                         (wait t)
                                         #+UNIX (may-exec nil)
                                         #+WIN32 (indirectp t)
                           )
    (case input
      ((:TERMINAL :STREAM) )
      (t (if (eq input 'NIL)
           (setq input #+UNIX "/dev/null" #+WIN32 "nul")
           (setq input (xstring input))
         )
         (setq command (string-concat command " < " (shell-quote input)))
         #+WIN32 (setq indirectp t)
    ) )
    (case output
      ((:TERMINAL :STREAM) )
      (t (if (eq output 'NIL)
           (setq output #+UNIX "/dev/null" #+WIN32 "nul"
                 if-output-exists ':OVERWRITE
           )
           (progn
             (setq output (xstring output))
             (when (and (eq if-output-exists ':ERROR) (probe-file output))
               (setq output (pathname output))
               (error-of-type 'file-error
                 :pathname output
                 (TEXT "~S: File ~S already exists")
                 'run-shell-command output
         ) ) ) )
         (setq command
               (string-concat command
                 (ecase if-output-exists
                   ((:OVERWRITE :ERROR) " > ")
                   (:APPEND " >> ")
                 )
                 (shell-quote output)
         )     )
         #+WIN32 (setq indirectp t)
    ) )
    #-WIN32
    (unless wait
      (setq command (string-concat command " &")))
    #+WIN32
    (unless wait
      (setq indirectp t)
      (setf command (string-concat "start " command)))
    #+UNIX
    (when may-exec
      ; Wenn die ausführende Shell die "/bin/sh" ist und command eine
      ; "simple command" im Sinne von sh(1), können wir ein wenig optimieren:
      (setq command (string-concat "exec " command))
    )
    #+WIN32
    (when indirectp
      (setq command (string-concat (shell-name) " /c " command))
    )
    (if (eq input ':STREAM)
      (if (eq output ':STREAM)
        (make-pipe-io-stream command)
        (make-pipe-output-stream command)
      )
      (if (eq output ':STREAM)
        (make-pipe-input-stream command)
        (shell command) ; unter UNIX evtl. " &" anfügen, um Hintergrund-Prozess zu bekommen
    ) )
  )
  (defun run-program (program &key (arguments '())
                                   (input ':terminal) (output ':terminal)
                                   (if-output-exists ':overwrite)
                                   (wait t)
                                   #+WIN32 (indirectp nil)
                     )
    (run-shell-command
      (apply #'string-concat
             (shell-simple-quote (xstring program))
             (mapcan #'(lambda (argument)
                         (list " " (shell-quote (xstring argument)))
                       )
                     arguments
      )      )
      #+UNIX :may-exec #+UNIX t
      #+WIN32 :indirectp #+WIN32 indirectp
      :wait wait
      :input input :output output :if-output-exists if-output-exists
  ) )
)
