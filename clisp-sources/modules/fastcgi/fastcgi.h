/* fastcgi.h - Function declarations for FastCGI exported routines

   $Id: fastcgi.h,v 1.1 2003/08/07 19:58:48 hin Exp $
*/

extern char * fcgi_getenv(char *);
extern char * fcgi_read_stdin();
extern int    fcgi_write_stdout(char *, int);
extern int    fcgi_write_stderr(char *, int);
extern int    fcgi_accept_wrapper();
extern int    fcgi_is_cgi_wrapper();
extern void   fcgi_finish_wrapper();
