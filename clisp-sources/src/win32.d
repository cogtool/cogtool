/*
 * Include file for WIN32_NATIVE version of CLISP
 * Bruno Haible 1997-2005
 * Sam Steingold 1999-2006
 */

/* control characters constants */

#define BEL  7                  /* bell */
/* define NL  10             -- new line, see LISPBIBL.D */
#define RUBOUT 127              /* Rubout = Delete */
#define CRLFstring  "\r\n"      /* C-String - CR/LF */

/* Many Win32 API functions are declared differently when UNICODE is defined,
 in a way which does not work on Win95.
 We do not want this, so undefine it now. */
#ifdef UNICODE
  #define UNICODE_SAVED
  #undef UNICODE
#endif

/* for _clisp.c */
#define STDC_HEADERS 1
#define HAVE_PERROR_DECL

/* Declaration of operating system functions */
#define WIN32_LEAN_AND_MEAN  /* avoid including junk */
#undef unused /* `unused' is used in function declarations. */
#ifdef __MINGW32__
  #define ULONGLONG OS_ULONGLONG
  #define ULONG OS_ULONG
  #include <windows.h>
  #undef ULONG
  #undef ULONGLONG
  #define unused_void (void)
#else
  #include <windows.h>
  #define unused_void
#endif

/* Declaration of operating system types,
   in particular needed for ssize_t and before <sys/timeb.h> */
#include <sys/types.h>

/* Shell object handling for shell link resolution */
#include <objbase.h>
#include <shlobj.h>

/* ShellExecute declaration */
#include <shellapi.h>
#define unused unused_void      /* restore the unused declaration */

/* Table of system error messages */
#include <winerror.h>
/* extern DWORD GetLastError (void);
   extern void SetLastError (DWORD ErrCode);
   extern DWORD FormatMessage (DWORD Flags, LPCVOID Source, DWORD MessageId, DWORD LanguageId, LPTSTR Buffer, DWORD Size, va_list* Arguments);
   extern int WSAGetLastError (void); */
#define OS_errno GetLastError()
#define OS_set_errno(e) SetLastError(e)
/* used by error.d, spvw.d, stream.d, pathname.d, socket.d */

/* Table of ANSI C error messages */
#include <errno.h>
/* used by error.d, stream.d */

#define HAVE_STRERROR 1
/* used by errunix.d */

/* Getting memory. */
#include <stdlib.h>
#include <malloc.h>
extern void* malloc (size_t size);
extern void free (void* memblock);
/* used by spvw.d */

/* Normal program exit */
nonreturning_function(extern, _exit, (int status));
/* used by spvw.d */

/* Abrupt program termination */
/* win32aux.d overwrites abort() */
nonreturning_function(extern, abort, (void));
/* used by spvw.d, debug.d, eval.d, io.d */

/* Type of a file handle */
#define Handle  HANDLE
#define INVALID_HANDLE  INVALID_HANDLE_VALUE
#define FOREIGN_HANDLE          /* box them */

/* File handles of standard input, standard output, standard error */
extern Handle stdin_handle;
extern Handle stdout_handle;  /* see win32aux.d */
extern Handle stderr_handle;
extern void init_win32 (void);
extern void done_win32 (void);
/* used by spvw.d, stream.d */

/* Signal handling
   extern BOOL SetConsoleCtrlHandler (BOOL (*) (DWORD CtrlType), BOOL add);
   extern HANDLE CreateEvent (SECURITY_ATTRIBUTES* EventAttributes, BOOL ManualReset, BOOL InitialState, LPCTSTR Name);
   extern BOOL PulseEvent (HANDLE Event);
   extern HANDLE CreateThread (SECURITY_ATTRIBUTES* ThreadAttributes, DWORD StackSize, THREAD_START_ROUTINE* StartAddress, void* Parameter, DWORD CreationFlags, DWORD* ThreadId);
   extern DWORD WaitForSingleObject (HANDLE Handle, DWORD Milliseconds);
   extern DWORD WaitForMultipleObjects (DWORD Count, CONST HANDLE * Handles, BOOL WaitAll, DWORD Milliseconds);
   extern BOOL TerminateThread (HANDLE Thread, DWORD ExitCode);
   extern BOOL GetExitCodeThread (HANDLE Thread, DWORD* ExitCode);
   extern DWORD SuspendThread (HANDLE Thread);
   extern DWORD ResumeThread (HANDLE Thread);
   extern BOOL GetThreadContext (HANDLE Thread, LPCONTEXT Context);
   extern BOOL SetThreadContext (HANDLE Thread, CONST CONTEXT * Context);
   extern HANDLE GetCurrentProcess (void);
   extern HANDLE GetCurrentThread (void);
   extern BOOL DuplicateHandle (HANDLE SourceProcessHandle, HANDLE SourceHandle, HANDLE TargetProcessHandle, LPHANDLE TargetHandle, DWORD DesiredAccess, BOOL InheritHandle, DWORD Options);
 used by win32aux.d
   This is the Ctrl-C handler. It is executed in the main thread and must
   not return! */
extern void interrupt_handler (void);
/* Install our intelligent Ctrl-C handler.
   This should be called only once, and only from the main thread. */
extern void install_sigint_handler (void);
/* used by spvw.d */

/* Environment variables - getenv()
 (note that win32 maintains two separate enviroment block,
  one accessed by getenv and referred to by environ
  and the other for GetEnvironmentStrings() &Co) */
#include <stdlib.h>
/* used by pathname.d, misc.d */

/* Locale definition function */
#include <locale.h>
extern_C char *setlocale (int category, const char *locale);
#ifdef _MSC_VER
  #define HAVE_LC_MESSAGES 0
#elif defined(__MINGW32__)
  #define HAVE_LC_MESSAGES 0
#else  /* place appropriate checks here */
  #define HAVE_LC_MESSAGES 1
#endif
/* used by spvw_ctype.d */

/* Character set conversion
   extern BOOL CharToOem (LPCTSTR Str, LPSTR Dst);
   extern BOOL OemToChar (LPCTSTR Str, LPSTR Dst);
 used by win32aux.d */

/* Set working directory
   extern BOOL SetCurrentDirectory (LPCTSTR PathName);
 used by pathname.d */

/* Retrieve working directory
   extern DWORD GetCurrentDirectory (DWORD BufferLength, LPTSTR Buffer);
   extern DWORD GetFullPathName (LPCTSTR FileName, DWORD BufferLength, LPTSTR Buffer, LPTSTR* FilePart);
   The actual value of _MAX_PATH is irrelevant, because we retry the calls to
   GetCurrentDirectory() and GetFullPathName() if the buffer is too small. */
#ifndef _MAX_PATH
  #define _MAX_PATH 1024
#endif
/* used by pathname.d */

/* Retrieve information about a file
   extern DWORD GetLogicalDrives (void); // broken!
   extern UINT GetDriveType (LPCTSTR RootPathName);
   extern DWORD GetFileAttributes (LPCTSTR FileName);
   extern DWORD GetFileType (HANDLE File);
   extern DWORD GetFileSize (HANDLE File, LPDWORD FileSizeHigh);
   extern BOOL GetFileInformationByHandle (HANDLE File, BY_HANDLE_FILE_INFORMATION* FileInformation);
 used by pathname.d, stream.d */

struct file_id {        /* Unique ID for an open file on this machine */
  DWORD nFileIndexHigh;
  DWORD nFileIndexLow;
};
/* if file NAMESTRING exists, fill file_id and call function on it,
   otherwise return NULL */
extern void* with_file_id (char *namestring, void *data,
                           void* (*func) (struct file_id *fid, void *data));
/* fill FI for an existing file handle */
typedef DWORD errno_t;
extern errno_t handle_file_id (HANDLE fh, struct file_id *fi);
/* if the file IDs are identical, return 1, otherwise return 0 */
extern int file_id_eq (struct file_id *fi1, struct file_id *fi2);

/* Delete a file
   extern BOOL DeleteFile (LPCTSTR FileName);
 used by pathname.d */

/* Rename a file
   extern BOOL MoveFile (LPCTSTR ExistingFileName, LPCTSTR NewFileName);
 used by pathname.d */

/* Directory search
   extern HANDLE FindFirstFile (LPCTSTR FileName, LPWIN32_FIND_DATA FindFileData);
   extern BOOL FindNextFile (HANDLE FindFile, LPWIN32_FIND_DATA FindFileData);
   extern BOOL FindClose (HANDLE FindFile);
 used by pathname.d */

/* Create a directory
   extern BOOL CreateDirectory (LPCTSTR PathName, SECURITY_ATTRIBUTES* SecurityAttributes);
 used by pathname.d */

/* Delete a directory
   extern BOOL RemoveDirectory (LPCTSTR PathName);
 used by pathname.d */

/* Working with open files
   extern HANDLE CreateFile (LPCTSTR FileName, DWORD DesiredAccess, DWORD ShareMode, SECURITY_ATTRIBUTES* SecurityAttributes, DWORD CreationDistribution, DWORD FlagsAndAttributes, HANDLE TemplateFile);
   extern HANDLE GetStdHandle (DWORD StdHandle);
   extern DWORD GetFileSize (HANDLE File, LPDWORD FileSizeHigh);
   extern DWORD SetFilePointer (HANDLE File, LONG DistanceToMove, LONG* DistanceToMoveHigh, DWORD MoveMethod);
   extern BOOL ReadFile (HANDLE File, void* Buffer, DWORD BytesToRead, DWORD* BytesRead, OVERLAPPED* Overlapped);
   extern BOOL WriteFile (HANDLE File, const void* Buffer, DWORD BytesToWrite, DWORD* BytesWritten, OVERLAPPED* Overlapped);
   extern BOOL GetConsoleMode (HANDLE ConsoleHandle, LPDWORD Mode);
   extern BOOL ReadConsole (HANDLE ConsoleInput, void* Buffer, DWORD BytesToRead, DWORD* BytesRead, void* Reserved);
   extern BOOL GetNumberOfConsoleInputEvents (HANDLE ConsoleInput, LPDWORD NumberOfEvents);
   extern BOOL PeekConsoleInput (HANDLE ConsoleInput, PINPUT_RECORD Buffer, DWORD Length, LPDWORD NumberOfEventsRead);
   extern BOOL ReadConsoleInput (HANDLE ConsoleInput, PINPUT_RECORD Buffer, DWORD Length, LPDWORD NumberOfEventsRead);
   extern BOOL WriteConsoleInput (HANDLE ConsoleInput, CONST INPUT_RECORD * Buffer, DWORD Length, LPDWORD NumberOfEventsWritten);
   extern BOOL WriteConsole (HANDLE ConsoleOutput, const void* Buffer, DWORD BytesToWrite, DWORD* BytesWritten, void* Reserved);
   extern HANDLE CreateEvent (SECURITY_ATTRIBUTES* EventAttributes, BOOL ManualReset, BOOL InitialState, LPCTSTR Name);
   extern BOOL ResetEvent (HANDLE Event);
   extern BOOL GetOverlappedResult (HANDLE File, OVERLAPPED* Overlapped, DWORD* NumberOfBytesTransferred, BOOL Wait);
   extern BOOL CloseHandle (HANDLE Object);
   //extern BOOL DuplicateHandle (HANDLE SourceProcessHandle, HANDLE SourceHandle, HANDLE TargetProcessHandle, HANDLE* TargetHandle, DWORD DesiredAccess, BOOL InheritHandle, DWORD Options);
   //extern BOOL FlushFileBuffers (HANDLE File);
   extern BOOL PeekNamedPipe (HANDLE NamedPipe, void* Buffer, DWORD BufferSize, DWORD* BytesRead, DWORD* TotalBytesAvail, DWORD* BytesLeftThisMessage);
   extern BOOL PurgeComm (HANDLE File, DWORD Flags);
   extern BOOL FlushConsoleInputBuffer (HANDLE ConsoleInput); */
#define uAsciiChar uChar.AsciiChar
/* used by spvw.d, stream.d, pathname.d, win32aux.d
   My private error code when Ctrl-C has been pressed. */
#define ERROR_SIGINT ERROR_SUCCESS
/* Like ReadConsoleInput with Length==1, but is interruptible by Ctrl-C. */
extern BOOL ReadConsoleInput1 (HANDLE ConsoleInput, PINPUT_RECORD Buffer, LPDWORD NumberOfEventsRead);
/* The following functions deal with all kinds of file/pipe/console handles */
extern int fd_read_wont_hang_p (HANDLE fd);
#ifdef MICROSOFT
typedef long ssize_t;
#endif
extern ssize_t fd_read (HANDLE fd, void* buf, size_t nbyte, perseverance_t persev);
extern ssize_t fd_write (HANDLE fd, const void* buf, size_t nbyte, perseverance_t persev);
#define safe_read(fd,buf,nbyte)  fd_read(fd,buf,nbyte,persev_partial)
#define full_read(fd,buf,nbyte)  fd_read(fd,buf,nbyte,persev_full)
#define safe_write(fd,buf,nbyte)  fd_write(fd,buf,nbyte,persev_partial)
#define full_write(fd,buf,nbyte)  fd_write(fd,buf,nbyte,persev_full)
/* Changing the position within a file. */
/* _off_t is sint32, but the Win32 APIs support 64-bit file offsets. */
#define off_t  sint64
#undef SIZEOF_OFF_T  /* on mingw, it was defined in unixconf.h */
#define SIZEOF_OFF_T  8
#ifdef __MINGW32__
  #include <io.h>
  #define lseek clisp_lseek /* avoid collision with prototype in <mingw/io.h> */
#endif
extern off_t lseek (HANDLE fd, off_t offset, DWORD mode);
#undef SEEK_SET
#undef SEEK_CUR
#undef SEEK_END
#define SEEK_SET  FILE_BEGIN
#define SEEK_CUR  FILE_CURRENT
#define SEEK_END  FILE_END
/* used by spvw.d, stream.d */

/* Socket connections */
#ifdef __MINGW32__
/* this kills a warning in </usr/include/w32api/winsock.h>
 and </usr/include/w32api/winsock2.h>:
 "fd_set and associated macros have been defined in sys/types.
  This may cause runtime problems with W32 sockets"
 Bruno said:
 I think this warning means that read(), write() don't work on sockets
 in mingw32.  Like on BeOS.  CLISP already handles this.
 See the #ifs around stream.d:low_write_unbuffered_socket() etc. */
#define USE_SYS_TYPES_FD_SET
#endif
  #include <winsock2.h>
  #include <ws2tcpip.h>
#ifdef __MINGW32__
#undef USE_SYS_TYPES_FD_SET
#endif
/* extern int WSAStartup (WORD VersionRequested, WSADATA* WSAData);
   extern int WSAGetLastError (void);
   extern void WSASetLastError (int Error);
   extern int WSACancelBlockingCall (void); */
#define SOCKLEN_T  int
/* extern SOCKET socket (int af, int type, int protocol);
   extern int bind (SOCKET s, const struct sockaddr * addr, int addrlen);
   extern int listen (SOCKET s, int backlog);
   extern SOCKET accept (SOCKET s, struct sockaddr * addr, int * addrlen);
   extern int connect (SOCKET s, const struct sockaddr * addr, int addrlen);
   extern int setsockopt (SOCKET s, int level, int optname, const char * optval, int option);
   extern int recv (SOCKET s, char* buf, int len, int flags);
   extern int send (SOCKET s, const char* buf, int len, int flags);
   extern int select (int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, const struct timeval * timeout);
   extern int closesocket (SOCKET s);
   extern int gethostname (char* name, int namelen);
   extern struct hostent * gethostbyname (const char* name);
   extern struct hostent * gethostbyaddr (const char* addr, int len, int type);
   extern struct servent * getservbyname (const char* name, const char* proto);
   extern struct servent * getservbyport (int port, const char* proto);
   extern int getpeername (SOCKET s, struct sockaddr * addr, int * addrlen);
   Type of a socket
   define SOCKET  unsigned int
   Error value for functions returning a socket
   define INVALID_SOCKET  (SOCKET)(-1)
   Error value for functions returning an `int' status
   define SOCKET_ERROR  (-1)
   Accessing the error code */
#define sock_errno  WSAGetLastError()
#define sock_errno_is(val)  (WSAGetLastError() == WSA##val)
#define sock_set_errno(val)  WSASetLastError(WSA##val)
/* Error values when sock_read, sock_write return 0: */
#define WSAEAGAIN WSAEWOULDBLOCK
#define WSAENOENT WSAESHUTDOWN
/* Signalling a socket related error
   extern void SOCK_error (void);
   Reading and writing from a socket */
extern int sock_read (SOCKET fd, void* buf, size_t nbyte, perseverance_t persev);
extern int sock_write (SOCKET fd, const void* buf, size_t nbyte, perseverance_t persev);
/* Interruptible wait for something on socket */
typedef enum { socket_wait_read, socket_wait_write, socket_wait_except } socket_wait_event;
extern int interruptible_socket_wait (SOCKET socket_handle, socket_wait_event waitwhat, struct timeval * timeout_ptr);
/* ioctl for sockets */
#define ioctl ioctlsocket
/* Wrapping and unwrapping of a socket in a Lisp object */
#define allocate_socket(fd)  allocate_handle((Handle)(fd))
#define TheSocket(obj)  (SOCKET)TheHandle(obj)
/* Autoconfiguration macros */
#define HAVE_GETHOSTNAME
#ifndef MAXHOSTNAMELEN
  #define MAXHOSTNAMELEN 64
#endif
#define HAVE_GETHOSTBYNAME
#define CONNECT_NAME_T struct sockaddr
#define CONNECT_CONST const
#define CONNECT_ADDRLEN_T int
#define HAVE_IPV4
#define HAVE_IPV6
#undef HAVE_NETINET_IN_H
#undef HAVE_ARPA_INET_H
#define RET_INET_ADDR_TYPE unsigned long
#define INET_ADDR_SUFFIX
#define INET_ADDR_CONST const
#undef HAVE_NETINET_TCP_H
#define SETSOCKOPT_CONST const
#define SETSOCKOPT_ARG_T char*
#define SETSOCKOPT_OPTLEN_T int
#define HAVE_SHUTDOWN
#ifdef HAVE_SHUTDOWN
  #define SHUT_RD   SD_RECEIVE
  #define SHUT_WR   SD_SEND
  #define SHUT_RDWR SD_BOTH
#endif
/* Do not define HAVE_SELECT because select() works on sockets only.
 used by error.d, misc.d, socket.d, stream.d
 requires linking with wsock32.lib */

/* Hacking the terminal */
#ifdef __MINGW32__
  # include <io.h>
  #define isatty clisp_isatty /* avoid collision with prototype in <mingw/io.h> */
#endif
extern int isatty (HANDLE handle); /* see win32aux.d */
/* used by stream.d */

/* Date and time
   Don't use GetSystemTime(), because it's unreliable. (See comment in
   MSVC4.0 crt/src/time.c.) Better use GetLocalTime().
   //extern void GetLocalTime (SYSTEMTIME* SystemTime);
   But GetLocalTime() ignores the TZ environment variable, so use _ftime(). */
#include <sys/timeb.h>
#ifdef MICROSOFT
  #define timeb _timeb
  #define ftime _ftime
#endif
/* extern void ftime (struct timeb *); */
#include <time.h>
/* extern struct tm * localtime (time_t*);
   extern struct tm * gmtime (time_t*);
   extern BOOL FileTimeToLocalFileTime (const FILETIME* FileTime, FILETIME* LocalFileTime);
   extern BOOL FileTimeToSystemTime (const FILETIME* LocalFileTime, SYSTEMTIME* LocalSystemTime);
 used by time.d */

/* Pausing
   extern void Sleep (DWORD Milliseconds);
 used by win32aux.d
   Sleep a certain time.
   Return true after normal termination, false if interrupted by Ctrl-C. */
extern BOOL msleep (DWORD milliseconds);
extern unsigned int sleep (unsigned int seconds);
/* used by time.d, socket.d */

/* Calling programs
  extern BOOL CreateProcess (LPCTSTR ApplicationName, LPTSTR CommandLine,
      LPSECURITY_ATTRIBUTES ProcessAttributes,
      LPSECURITY_ATTRIBUTES ThreadAttributes,
      BOOL InheritHandles, DWORD CreationFlags, LPVOID Environment,
      LPCTSTR CurrentDirectory, LPSTARTUPINFO StartupInfo,
      LPPROCESS_INFORMATION ProcessInformation);
  extern BOOL GetExitCodeProcess (HANDLE Process, LPDWORD ExitCode);
  extern BOOL CreatePipe (PHANDLE ReadPipe, PHANDLE WritePipe,
      LPSECURITY_ATTRIBUTES PipeAttributes, DWORD Size);
  extern BOOL DuplicateHandle (HANDLE SourceProcessHandle, HANDLE SourceHandle,
      HANDLE TargetProcessHandle, LPHANDLE TargetHandle,
      DWORD DesiredAccess, BOOL InheritHandle, DWORD Options);
  used by win32aux.d, pathname.d, stream.d */
extern BOOL MyCreateProcess (LPTSTR CommandLine, HANDLE StdInput,
                             HANDLE StdOutput, HANDLE StdError,
                             LPPROCESS_INFORMATION ProcessInformation);
/* used by pathname.d, stream.d */

/* Getting "random" numbers */
#if defined(__MINGW32__)
/* Not defined in any header. */
extern STDCALL DWORD CoGetCurrentProcess (void);
#else
  #include <objbase.h>
/* extern DWORD CoGetCurrentProcess (void); */
#endif
/* used by lisparit.d
 requires linking with ole32.lib */

/* Getting information about the machine.
   extern void GetSystemInfo (LPSYSTEM_INFO SystemInfo); */
#if defined(BORLAND)
  #define wProcessorArchitecture u.s.wProcessorArchitecture
#endif
/* used by misc.d */

/* Getting more information about the machine.
   extern LONG RegOpenKeyEx (HKEY Key, LPCTSTR SubKey, DWORD Options, REGSAM Desired, PHKEY Result);
   extern LONG RegQueryValueEx (HKEY Key, LPTSTR ValueName, LPDWORD Reserved, LPDWORD Type, LPBYTE Data, LPDWORD cbData);
   extern LONG RegCloseKey (HKEY Key);
 used by misc.d
 requires linking with advapi32.lib */

/* Examining the memory map.
   extern DWORD VirtualQuery (LPCVOID Address, PMEMORY_BASIC_INFORMATION Buffer, DWORD Length); */
extern void DumpProcessMemoryMap (void); /* see win32aux.d */
/* used by spvw.d */

/* Getting virtual memory
   //extern void GetSystemInfo (LPSYSTEM_INFO SystemInfo);
   extern LPVOID VirtualAlloc (LPVOID Address, DWORD Size, DWORD AllocationType, DWORD Protect);
   extern BOOL VirtualFree (LPVOID Address, DWORD Size, DWORD FreeType);
   extern BOOL VirtualProtect (LPVOID Address, DWORD Size, DWORD NewProtect, PDWORD OldProtect);
   //extern HANDLE CreateFileMapping (HANDLE File, LPSECURITY_ATTRIBUTES FileMappingAttributes, DWORD Protect, DWORD MaximumSizeHigh, DWORD MaximumSizeLow, LPCTSTR Name);
   //extern LPVOID MapViewOfFileEx (HANDLE FileMappingObject, DWORD DesiredAccess, DWORD FileOffsetHigh, DWORD FileOffsetLow, DWORD NumberOfBytesToMap, LPVOID BaseAddress);
   //extern BOOL UnmapViewOfFile (LPCVOID BaseAddress); */
#define HAVE_WIN32_VM
/* Damit kann man munmap() und mprotect() selber schreiben. mmap() wird
   emuliert, weil MapViewOfFileEx() zu viele Nachteile hat. Siehe spvw.d. */
/* #define HAVE_MMAP */
#define HAVE_MUNMAP
#define HAVE_WORKING_MPROTECT
#define PROT_NONE  PAGE_NOACCESS
#define PROT_READ  PAGE_READONLY
#define PROT_READ_WRITE PAGE_READWRITE
/* PROT_WRITE, PROT_EXEC not used
 used by spvw.d */

/* Now it's time to enable our UNICODE macro again. */
#ifdef UNICODE_SAVED
  #define UNICODE
#endif

