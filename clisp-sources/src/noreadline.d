/*
 * A substitute for the GNU readline library.
 * Bruno Haible 1992-2005
 * Sam Steingold 1998-2005
 */
/* These are the only things we need from lispbibl.c: */
#define global
#ifdef __cplusplus
  #define NULL  0
#else
  #define NULL  ((void*) 0L)
#endif

typedef int rl_command_func_t (int, int);
typedef char **rl_completion_func_t (const char *, int, int);
typedef char *rl_compentry_func_t (const char *, int);

global int rl_gnu_readline_p = 0; /* readline library not used */

global char* rl_readline_name;
global rl_completion_func_t* rl_attempted_completion_function;
global rl_compentry_func_t* rl_completion_entry_function;

global char* rl_basic_word_break_characters;
global char* rl_basic_quote_characters;
global char* rl_completer_quote_characters;

global char* rl_line_buffer;
global int rl_already_prompted;

global char* readline (char* prompt);
global char* readline (char* prompt) { return NULL; }

global void rl_deprep_terminal(void);
global void rl_deprep_terminal(void) {}

global char* filename_completion_function (const char* text, int state);
global char* filename_completion_function (const char* text, int state)
{ return NULL; }

global void add_history (char* line);
global void add_history (char* line) {}

global rl_command_func_t* rl_named_function (char* string);
global rl_command_func_t* rl_named_function (char* string) { return NULL; }

global int rl_bind_key (int key, rl_command_func_t* function);
global int rl_bind_key (int key, rl_command_func_t* function) { return 0; }

global int rl_variable_bind (const char *variable, const char *value);
global int rl_variable_bind (const char *variable, const char *value)
{ return 0; }

global int rl_set_paren_blink_timeout (int timeout);
global int rl_set_paren_blink_timeout (int timeout) { return 0; }

global int rl_add_defun (const char *name, rl_command_func_t *func, int key);
global int rl_add_defun (const char *name, rl_command_func_t *func, int key)
{ return 0; }
