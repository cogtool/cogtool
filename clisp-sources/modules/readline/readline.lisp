;;; readline & history interface
;;; http://cnswww.cns.cwru.edu/~chet/readline/readline.html
;;; http://cnswww.cns.cwru.edu/~chet/readline/history.html
;;;
;;; Copyright (C) 2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "READLINE"
  (:use "CL" "EXT" "FFI")
  (:shadowing-import-from "EXPORTING"
     #:defconstant #:defun #:defmacro #:defvar #:def-c-type #:def-c-enum
     #:def-c-struct #:def-c-var #:def-c-const #:def-call-out)
  (:documentation "
Interface to the GNU readline and history library. It allows you to
- use readline functionality for user input in your programs
  (see functions readline, add-history, and stream *readline-input-stream*)
- expand readline functionality with new functions that can be bound to keys
  (see add-defun, add-funmap-entry)
- run things on background while waiting for user input (see event-hook)
- more stuff (read readline info pages)"))

(in-package "READLINE")

(setf (documentation (find-package "READLINE") 'sys::impnotes) "readline-mod")

;;; foreign function definitions
(default-foreign-language :stdc)
(eval-when (compile) (setq *foreign-guard* t))

(c-lines "#include <config.h>~%") ; local readline config
(c-lines "#include <stdio.h>~%")

;;; ------ readline ------

(c-lines "#include <readline/readline.h>~%")

(def-c-type command-func-t
    (c-function (:arguments (rep int) (char int))
                (:return-type int)))
(def-c-type compentry-func-t
    (c-function (:arguments (rep c-string) (char int))
                (:return-type c-string)))
(def-c-type completion-func-t
    (c-function (:arguments (rep c-string) (char int) (rep int))
                (:return-type (c-ptr c-string))))
(def-c-type readline-hook-function (c-function (:return-type int)))
(def-c-type readline-vcpfunc (c-function (:arguments (text c-string))))
(def-c-type keymap c-pointer)

;;; Basic behavior
(def-call-out readline (:name "readline")
  (:documentation
    "Prompt and return string read from readline library or nil as eof.")
  (:arguments (prompt c-string))
  (:return-type c-string :malloc-free))

(def-call-out set-prompt (:name "rl_set_prompt") ; untested
  (:arguments (prompt c-string))
  (:return-type int))

(def-call-out initialize (:name "rl_initialize")
  (:arguments) (:return-type int))

(def-call-out read-init-file (:name "rl_read_init_file")
  (:arguments (name c-string))
  (:return-type int))

;;; Function naming

(def-call-out add-defun (:name "rl_add_defun")
  (:arguments (name c-string :in :malloc-free)
              (callback command-func-t) (key int))
  (:return-type int)
  (:documentation "Bind function to a name and key. You can use
name in ~/.inputrc. This is preferred way of adding new functions."))

;;; Keymaps
(def-call-out make-bare-keymap (:name "rl_make_bare_keymap") ; untested
  (:documentation "Make empty keymap.")
  (:arguments) (:return-type keymap))

(def-call-out copy-keymap (:name "rl_copy_keymap") ; untested
  (:arguments (map keymap) (:return-type keymap)))

(def-call-out make-keymap (:name "rl_make_keymap") ; untested
  (:documentation "Make simple keymap - chars bound to self-insert etc.")
  (:arguments) (:return-type keymap))

(def-call-out discard-keymap (:name "rl_discard_keymap") ; untested
  (:documentation "Discard allocated keymap.")
  (:arguments (map keymap)) (:return-type))

(def-call-out get-keymap (:name "rl_get_keymap") ; untested
  (:documentation "Return current keymap")
  (:arguments) (:return-type keymap))

(def-call-out set-keymap (:name "rl_set_keymap") ; untested
  (:documentation "Set keymap as current")
  (:arguments (map keymap)) (:return-type))

(def-call-out get-keymap-by-name (:name "rl_get_keymap_by_name") ; untested
  (:documentation "Get keymap with given name (e.g., emacs, vi)")
  (:arguments (name c-string)) (:return-type keymap))

(def-call-out get-keymap-name (:name "rl_get_keymap_by_name") ; untested
  (:arguments (keymap keymap)) (:return-type c-string))

;;; Binding Keys

(def-call-out bind-key (:name "rl_bind_key")
  (:arguments (key int) (callback command-func-t))
  (:return-type int))

(def-call-out bind-key-in-map (:name "rl_bind_key_in_map") ; untested
  (:arguments (key int) (callback command-func-t) (map keymap))
  (:return-type int))

(def-call-out bind-key-if-unbound (:name "rl_bind_key_if_unbound") ; untested
  (:arguments (key int) (callback command-func-t))
  (:return-type int))

(def-call-out bind-key-if-unbound-in-map (:name "rl_bind_key_if_unbound_in_map") ; untested
  (:arguments (key int) (callback command-func-t) (map keymap))
  (:return-type int))

(def-call-out unbind-key (:name "rl_unbind_key")
  (:arguments (key int))
  (:return-type int))

(def-call-out unbind-key-in-map (:name "rl_unbind_key_in_map") ; untested
  (:arguments (key int) (map keymap))
  (:return-type int))

(def-call-out unbind-function-in-map (:name "rl_unbind_function_in_map") ; untested
  (:arguments (fn command-func-t) (map keymap))
  (:return-type int))

(def-call-out unbind-command-in-map (:name "rl_unbind_command_in_map") ; untested
  (:arguments (command c-string) (map keymap))
  (:return-type int))

(def-call-out bind-keyseq (:name "rl_bind_keyseq")
  (:arguments (keyseq c-string)
              (callback command-func-t))
  (:return-type int))

(def-call-out bind-keyseq-in-map (:name "rl_bind_keyseq_in_map")
  (:arguments (keyseq c-string) (callback command-func-t) (map keymap))
  (:return-type int))

; set-key is equivalent to bind-keyseq-in-map

(def-call-out bind-keyseq-if-unbound (:name "rl_bind_keyseq_if_unbound"); untested
  (:arguments (keyseq c-string)
              (callback command-func-t))
  (:return-type int))

(def-call-out bind-keyseq-if-unbound-in-map (:name "rl_bind_keyseq_if_unbound_in_map"); untested
  (:arguments (keyseq c-string) (callback command-func-t) (map keymap))
  (:return-type int))

(def-call-out generic-bind (:name "rl_generic_bind") ; untested
  (:arguments (type int) (keyseq c-string) (data c-pointer) (map keymap))
  (:return-type int))

(def-call-out parse-and-bind (:name "rl_parse_and_bind")
  (:arguments (line c-string))
  (:return-type int))

;;; Associating Function Names and Bindings

(def-call-out named-function (:name "rl_named_function")
  (:arguments (name c-string))
  (:return-type command-func-t))

(def-call-out function-of-keyseq (:name "rl_function_of_keyseq")
  (:arguments (keyseq c-string) (map keymap) (type (c-ptr int) :out))
  (:return-type command-func-t))

(def-call-out invoking-keyseqs (:name "rl_invoking_keyseqs")
  (:arguments (function command-func-t))
  (:return-type (c-array-ptr c-string)))

(def-call-out invoking-keyseqs-in-map (:name "rl_invoking_keyseqs_in_map") ; untested
  (:arguments (function command-func-t) (map keymap))
  (:return-type (c-array-ptr c-string)))

(def-call-out function-dumper (:name "rl_function_dumper")
  (:arguments (readable int))
  (:return-type))

(def-call-out list-funmap-names (:name "rl_list_funmap_names")
  (:arguments)
  (:return-type))

;;; !!! Returned array should be freed, but if I :malloc-free it, clisp
;;; tries to free the c-string too. Bad.
(def-call-out funmap-names (:name "rl_funmap_names") ; FIXME: leaks
  (:arguments)
  (:return-type (c-array-ptr c-string)))

(def-call-out add-funmap-entry  (:name "rl_add_funmap_entry") ; untested
  (:arguments (name c-string :in :malloc-free) (callback command-func-t))
  (:return-type int)
  (:documentation "Bind function to a name known to readline."))


;;; Allowing undoing

;;; constants used by add-undo.
(def-c-enum undo_code UNDO_DELETE UNDO_INSERT UNDO_BEGIN UNDO_END)

(def-call-out begin-undo-group  (:name "rl_begin_undo_group") ; untested
  (:arguments) (:return-type int))

(def-call-out end-undo-group (:name "rl_end_undo_group") ; untested
  (:arguments) (:return-type int))

(def-call-out add-undo (:name "rl_add_undo") ; untested
  (:arguments (what int) (start int) (end int) (text c-string))
  (:return-type))

(def-call-out free-undo-list (:name "rl_free_undo_list") ; untested
  (:arguments) (:return-type))

(def-call-out do-undo  (:name "rl_do_undo") ; untested
  (:arguments) (:return-type int))

(def-call-out modifying (:name "rl_modifying") ; untested
  (:arguments (start int) (end int))
  (:return-type int))

;;; Redisplay

(def-call-out redisplay (:name "rl_redisplay")
  (:arguments) (:return-type int))

(def-call-out forced-update-display (:name "rl_forced_update_display")
  (:arguments) (:return-type int))

(def-call-out on-new-line (:name "rl_on_new_line")
  (:arguments) (:return-type int))

(def-call-out on-new-line-with-prompt (:name "rl_on_new_line_with_prompt") ; untested
  (:arguments ) (:return-type int))

(def-call-out reset-line-state (:name "rl_reset_line_state") ; untested
  (:arguments) (:return-type int))

(def-call-out crlf (:name "rl_crlf") ; untested
  (:arguments) (:return-type int))

(def-call-out show-char (:name "rl_show_char") ; untested
  (:arguments (char int)) (:return-type int))

(def-call-out message (:name "rl_message") ; untested
  (:arguments (text c-string))
  (:return-type int)
  (:documentation
    "Prints message (given as a format string - beware of %) in message area."))

(def-call-out clear-message (:name "rl_clear_message") ; untested
  (:arguments) (:return-type int))

(def-call-out save-prompt (:name "rl_save_prompt") ; untested
  (:arguments) (:return-type))

(def-call-out restore-prompt (:name "rl_restore_prompt") ; untested
  (:arguments) (:return-type))

(def-call-out expand-prompt (:name "rl_expand_prompt") ; untested
  (:arguments (prompt c-string)) (:return-type int))

;;; Modifying text

(def-call-out insert-text (:name "rl_insert_text")
  (:arguments (text c-string))
  (:return-type int))

(def-call-out delete-text (:name "rl_delete_text") ; untested
  (:arguments (start int) (end int))
  (:return-type int))

(def-call-out copy-text (:name "rl_copy_text") ; untested
  (:arguments (start int) (end int))
  (:return-type int))

(def-call-out kill-text (:name "rl_kill_text") ; untested
  (:arguments (start int) (end int))
  (:return-type int))

(def-call-out push-macro-input (:name "rl_push_macro_input")
  (:arguments (macro c-string))
  (:return-type int))

;;; Character input

(def-call-out read-key (:name "rl_read_key") ; untested
  (:arguments) (:return-type int))

(def-call-out getc (:name "rl_getc") ; untested
  (:arguments (stream c-pointer)) (:return-type int))

(def-call-out stuff-char (:name "rl_stuff_char") ; untested
  (:arguments (char int)) (:return-type int))

(def-call-out execute-next (:name "rl_execute_next") ; untested
  (:arguments (char int)) (:return-type int))

(def-call-out clear-pending-input (:name "rl_clear_pending_input") ; untested
  (:arguments) (:return-type int))

(def-call-out set-keyboard-input-timeout (:name "rl_set_keyboard_input_timeout")
  (:arguments (microseconds int))
  (:return-type int)) ; returns old value

;;; Terminal management

(def-call-out prep-terminal (:name "rl_prep_terminal") ; untested
  (:arguments (meta-flag int)) (:return-type))

(def-call-out deprep-terminal (:name "rl_deprep_terminal") ; untested
  (:arguments) (:return-type))

(def-call-out tty-set-default-bindings (:name "rl_tty_set_default_bindings") ; untested
  (:arguments (map keymap)) (:return-type))

(def-call-out reset-terminal (:name "rl_reset_terminal") ; untested
  (:arguments (terminal-name c-string)) (:return-type int))


;;; Utility

(def-call-out replace-line (:name "rl_replace_line") ; untested
  (:arguments (new-line c-string) (clear-undo int))
  (:return-type))

(def-call-out extend-line-buffer (:name "rl_extend_line_buffer") ; untested
  (:arguments (len int))
  (:return-type int))

(def-call-out ding (:name "rl_ding")
  (:arguments) (:return-type int))

(def-call-out display-match-list (:name "rl_display_match_list")
  (:arguments (matches (c-array-ptr c-string)) (len int) (max int))
  (:return-type))

;;; Miscellaneous functions

(def-call-out variable-bind (:name "rl_variable_bind")
  (:arguments (variable c-string) (value c-string))
  (:return-type int))

(def-call-out macro-dumper (:name "rl_macro_dumper") ; untested
  (:arguments (readable int))
  (:return-type))

(def-call-out variable-dumper (:name "rl_variable_dumper") ; untested
  (:arguments (readable int))
  (:return-type))

(def-call-out set-paren-blink-timeout (:name "rl_set_paren_blink_timeout") ; untested
  (:arguments (u int))
  (:return-type int))

(def-call-out get-termcap (:name "rl_get_termcap") ; untested
  (:arguments (cap c-string))
  (:return-type c-string))

;;; Signal Handling
(def-call-out resize-terminal (:name "rl_resize_terminal")
  (:arguments) (:return-type nil))

(def-call-out set-screen-size (:name "rl_set_screen_size")
  (:arguments (rows int) (cols int)) (:return-type nil))

(def-call-out get-screen-size (:name "rl_get_screen_size")
  (:arguments (rows (c-ptr int) :out) (cols (c-ptr int) :out))
  (:return-type nil))

;;; Alternate interface
(def-call-out callback-handler-install (:name "rl_callback_handler_install")
  (:arguments (prompt c-string) (lhandler readline-vcpfunc)))
(def-call-out callback-read-char (:name "rl_callback_read_char"))
(def-call-out callback-handler-remove (:name "rl_callback_handler_remove"))



;;; variables

(def-c-var line-buffer (:name "rl_line_buffer") (:type c-string)
           (:documentation "The line gathered so far."))
(def-c-var point (:name "rl_point") (:type int)
           (:documentation "The offset of current position in line-buffer"))
(def-c-var end (:name "rl_end") (:type int)
           (:documentation "The offset of current position in line-buffer"))
(def-c-var mark (:name "rl_mark") (:type int)
  (:documentation "The MARK (saved position) in the current line."))
(def-c-var done (:name "rl_done") (:type int)
  (:documentation
    "Non-zero value causes Readline to return the current line immediately."))
(def-c-var num-chars-to-read (:name "rl_num_chars_to_read") (:type int)
  (:documentation "Return after accepting so many characters."))
(def-c-var pending-input (:name "rl_pending_input") (:type int)
  (:documentation "Setting this to a value makes it next keystroke read."))
(def-c-var dispatching (:name "rl_dispatching") (:type int)
  (:read-only t)
  (:documentation "Non-zero if function is being called from a key binding."))
(def-c-var erase-empty-line (:name "rl_erase_empty_line") (:type int)
  (:documentation
    "Erase current line when a newline is typed on an otherwise-empty line."))
(def-c-var prompt (:name "rl_prompt") (:type c-string)
  (:read-only t)
  (:documentation "The prompt readline uses."))
(def-c-var already-prompted (:name "rl_already_prompted") (:type int)
  (:documentation
    "Set this if you do not wish prompt printed again by readline."))
(def-c-var library-version (:name "rl_library_version")
  (:documentation
   "The version of this incarnation of the readline library, e.g., \"4.2\".")
  (:type c-string) (:read-only t))
(def-c-var readline-version (:name "rl_readline_version")
  (:type int) (:read-only t)
  (:documentation
   "The version of this incarnation of the readline library, e.g., 0x0402."))
(def-c-var gnu-readline-p (:name "rl_gnu_readline_p") (:type int))
(def-c-var terminal-name (:name "rl_terminal_name") (:type c-string))

(def-c-var instream (:name "rl_instream") (:type c-pointer))
(def-c-var outstream (:name "rl_outstream") (:type c-pointer))
(def-c-var last-func (:name "rl_last_func") (:type command-func-t))
(def-c-var startup-hook (:name "rl_startup_hook")
   (:type readline-hook-function))
(def-c-var pre-input-hook (:name "rl_pre_input_hook")
   (:type readline-hook-function))
(def-c-var event-hook (:name "rl_event_hook") (:type readline-hook-function))
(def-c-var getc-function (:name "rl_getc_function")
   (:type (c-function (:arguments (instream c-pointer)) (:return-type int)))
   (:documentation "This function is used to read from input stream."))
(def-c-var editing-mode (:name "rl_editing_mode") (:type int))
(def-c-var insert-mode (:name "rl_insert_mode") (:type int))
(def-c-var readline-name (:name "rl_readline_name")
  (:type c-string) (:alloc :malloc-free))
(def-c-var readline-state (:name "rl_readline_state") (:type int))

(def-c-const state-none (:name "RL_STATE_NONE"))
(def-c-const state-initializing (:name "RL_STATE_INITIALIZING"))
(def-c-const state-initialized (:name "RL_STATE_INITIALIZED"))
(def-c-const state-termprepped (:name "RL_STATE_TERMPREPPED"))
(def-c-const state-readcmd (:name "RL_STATE_READCMD"))
(def-c-const state-metanext (:name "RL_STATE_METANEXT"))
(def-c-const state-dispatching (:name "RL_STATE_DISPATCHING"))
(def-c-const state-moreinput (:name "RL_STATE_MOREINPUT"))
(def-c-const state-isearch (:name "RL_STATE_ISEARCH"))
(def-c-const state-nsearch (:name "RL_STATE_NSEARCH"))
(def-c-const state-search (:name "RL_STATE_SEARCH"))
(def-c-const state-numericarg (:name "RL_STATE_NUMERICARG"))
(def-c-const state-macroinput (:name "RL_STATE_MACROINPUT"))
(def-c-const state-macrodef (:name "RL_STATE_MACRODEF"))
(def-c-const state-overwrite (:name "RL_STATE_OVERWRITE"))
(def-c-const state-completing (:name "RL_STATE_COMPLETING"))
(def-c-const state-sighandler (:name "RL_STATE_SIGHANDLER"))
(def-c-const state-undoing (:name "RL_STATE_UNDOING"))
(def-c-const state-inputpending (:name "RL_STATE_INPUTPENDING"))
(def-c-const state-ttycsaved (:name "RL_STATE_TTYCSAVED"))
(def-c-const state-done (:name "RL_STATE_DONE"))

;;; ------ history ------

(c-lines "#include <readline/history.h>~%")

;;; History List Management

(def-call-out using-history (:name "using_history")
  (:arguments) (:return-type nil))

(def-call-out add-history (:name "add_history")
  (:arguments (line c-string)) (:return-type nil))

(def-call-out clear-history (:name "clear_history")
  (:arguments) (:return-type nil))

(def-call-out stifle-history (:name "stifle_history")
  (:arguments (count int)) (:return-type nil))

(def-call-out unstifle-history (:name "unstifle_history")
  (:arguments) (:return-type int))

(def-call-out history-stifled-p (:name "history_is_stifled")
  (:arguments) (:return-type int))

;;; Information About the History List

(def-call-out where-history (:name "where_history")
  (:arguments) (:return-type int))

(def-call-out history-total-bytes (:name "history_total_bytes")
  (:arguments) (:return-type int))

;;; Moving Around the History List

(def-call-out history-set-pos (:name "history_set_pos")
  (:arguments (pos int)) (:return-type int))

;;; Searching the History List

(def-call-out history-search (:name "history_search")
  (:arguments (string c-string) (direction int)) (:return-type int))

(def-call-out history-search-prefix (:name "history_search_prefix")
  (:arguments (string c-string) (direction int)) (:return-type int))

(def-call-out history-search-pos (:name "history_search_pos")
  (:arguments (string c-string) (direction int) (pos int)) (:return-type int))

;;; Managing the History File

(def-call-out read-history (:name "read_history")
  (:arguments (file c-string)) (:return-type int))

(def-call-out read-history-range (:name "read_history_range")
  (:arguments (file c-string) (start int) (end int)) (:return-type int))

(def-call-out write-history (:name "write_history")
  (:arguments (file c-string)) (:return-type int))

(def-call-out append-history (:name "append_history")
  (:arguments (count int) (file c-string)) (:return-type int))

(def-call-out history-truncate-file (:name "history_truncate_file")
  (:arguments (file c-string) (nlines int)) (:return-type int))

;;; done with ffi

;;; Define input stream that will read from readline-enabled input.
;;; This is not strictly interface to readline capability, but it makes the
;;; interface more usable - you can, e.g., (read *readline-input-stream*)

(defun readline-reader ()
  "Read a single line using GNU readline with the standard CLISP prompt."
  (let ((string (readline (ext:string-concat (sys::prompt-start)
                                             (sys::prompt-body)
                                             (sys::prompt-finish)))))
    (declare (type (or null string) string))
    (when string                ; string=NIL ==> EOF
      (cond ((zerop (length string)) #1=#.(string #\NewLine))
            (t (add-history string)
               (ext:string-concat string #1#))))))

(defvar *readline-input-stream*
  (make-buffered-input-stream #'readline-reader nil)
  "Use this input stream to allow readline editing.")

(pushnew :readline *features*)
(provide "readline")
(pushnew "READLINE" custom:*system-package-list* :test #'string=)
