  # Calls a function, passing it information about a Win32 error code.
  # get_OS_error_info(errcode,func);
  # > errcode: error code
  # > func: will be called with name and msg (if available) as arguments
    typedef void OS_error_info_callback (const char* name, const char* msg);
    local void get_OS_error_info (DWORD errcode, OS_error_info_callback* func)
    {
      var const char* errorname = NULL;
      var const char* errormsg = NULL;
      switch (errcode) {
        #ifdef ERROR_SUCCESS
        case ERROR_SUCCESS:
          errorname = "ERROR_SUCCESS";
          errormsg = "The operation completed successfully.";
          break;
        #endif
        #ifdef ERROR_INVALID_FUNCTION
        case ERROR_INVALID_FUNCTION:
          errorname = "ERROR_INVALID_FUNCTION";
          errormsg = "Incorrect function.";
          break;
        #endif
        #ifdef ERROR_FILE_NOT_FOUND
        case ERROR_FILE_NOT_FOUND:
          errorname = "ERROR_FILE_NOT_FOUND";
          errormsg = "The system cannot find the file specified.";
          break;
        #endif
        #ifdef ERROR_PATH_NOT_FOUND
        case ERROR_PATH_NOT_FOUND:
          errorname = "ERROR_PATH_NOT_FOUND";
          errormsg = "The system cannot find the path specified.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_OPEN_FILES
        case ERROR_TOO_MANY_OPEN_FILES:
          errorname = "ERROR_TOO_MANY_OPEN_FILES";
          errormsg = "The system cannot open the file.";
          break;
        #endif
        #ifdef ERROR_ACCESS_DENIED
        case ERROR_ACCESS_DENIED:
          errorname = "ERROR_ACCESS_DENIED";
          errormsg = "Access is denied.";
          break;
        #endif
        #ifdef ERROR_INVALID_HANDLE
        case ERROR_INVALID_HANDLE:
          errorname = "ERROR_INVALID_HANDLE";
          errormsg = "The handle is invalid.";
          break;
        #endif
        #ifdef ERROR_ARENA_TRASHED
        case ERROR_ARENA_TRASHED:
          errorname = "ERROR_ARENA_TRASHED";
          errormsg = "The storage control blocks were destroyed.";
          break;
        #endif
        #ifdef ERROR_NOT_ENOUGH_MEMORY
        case ERROR_NOT_ENOUGH_MEMORY:
          errorname = "ERROR_NOT_ENOUGH_MEMORY";
          errormsg = "Not enough storage is available to process this command.";
          break;
        #endif
        #ifdef ERROR_INVALID_BLOCK
        case ERROR_INVALID_BLOCK:
          errorname = "ERROR_INVALID_BLOCK";
          errormsg = "The storage control block address is invalid.";
          break;
        #endif
        #ifdef ERROR_BAD_ENVIRONMENT
        case ERROR_BAD_ENVIRONMENT:
          errorname = "ERROR_BAD_ENVIRONMENT";
          errormsg = "The environment is incorrect.";
          break;
        #endif
        #ifdef ERROR_BAD_FORMAT
        case ERROR_BAD_FORMAT:
          errorname = "ERROR_BAD_FORMAT";
          errormsg = "An attempt was made to load a program with an incorrect format.";
          break;
        #endif
        #ifdef ERROR_INVALID_ACCESS
        case ERROR_INVALID_ACCESS:
          errorname = "ERROR_INVALID_ACCESS";
          errormsg = "The access code is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_DATA
        case ERROR_INVALID_DATA:
          errorname = "ERROR_INVALID_DATA";
          errormsg = "The data is invalid.";
          break;
        #endif
        #ifdef ERROR_OUTOFMEMORY
        case ERROR_OUTOFMEMORY:
          errorname = "ERROR_OUTOFMEMORY";
          errormsg = "Not enough storage is available to complete this operation.";
          break;
        #endif
        #ifdef ERROR_INVALID_DRIVE
        case ERROR_INVALID_DRIVE:
          errorname = "ERROR_INVALID_DRIVE";
          errormsg = "The system cannot find the drive specified.";
          break;
        #endif
        #ifdef ERROR_CURRENT_DIRECTORY
        case ERROR_CURRENT_DIRECTORY:
          errorname = "ERROR_CURRENT_DIRECTORY";
          errormsg = "The directory cannot be removed.";
          break;
        #endif
        #ifdef ERROR_NOT_SAME_DEVICE
        case ERROR_NOT_SAME_DEVICE:
          errorname = "ERROR_NOT_SAME_DEVICE";
          errormsg = "The system cannot move the file to a different disk drive.";
          break;
        #endif
        #ifdef ERROR_NO_MORE_FILES
        case ERROR_NO_MORE_FILES:
          errorname = "ERROR_NO_MORE_FILES";
          errormsg = "There are no more files.";
          break;
        #endif
        #ifdef ERROR_WRITE_PROTECT
        case ERROR_WRITE_PROTECT:
          errorname = "ERROR_WRITE_PROTECT";
          errormsg = "The media is write protected.";
          break;
        #endif
        #ifdef ERROR_BAD_UNIT
        case ERROR_BAD_UNIT:
          errorname = "ERROR_BAD_UNIT";
          errormsg = "The system cannot find the device specified.";
          break;
        #endif
        #ifdef ERROR_NOT_READY
        case ERROR_NOT_READY:
          errorname = "ERROR_NOT_READY";
          errormsg = "The device is not ready.";
          break;
        #endif
        #ifdef ERROR_BAD_COMMAND
        case ERROR_BAD_COMMAND:
          errorname = "ERROR_BAD_COMMAND";
          errormsg = "The device does not recognize the command.";
          break;
        #endif
        #ifdef ERROR_CRC
        case ERROR_CRC:
          errorname = "ERROR_CRC";
          errormsg = "Data error (cyclic redundancy check)";
          break;
        #endif
        #ifdef ERROR_BAD_LENGTH
        case ERROR_BAD_LENGTH:
          errorname = "ERROR_BAD_LENGTH";
          errormsg = "The program issued a command but the  command length is incorrect.";
          break;
        #endif
        #ifdef ERROR_SEEK
        case ERROR_SEEK:
          errorname = "ERROR_SEEK";
          errormsg = "The drive cannot locate a specific area or track on the disk.";
          break;
        #endif
        #ifdef ERROR_NOT_DOS_DISK
        case ERROR_NOT_DOS_DISK:
          errorname = "ERROR_NOT_DOS_DISK";
          errormsg = "The specified disk or diskette cannot be accessed.";
          break;
        #endif
        #ifdef ERROR_SECTOR_NOT_FOUND
        case ERROR_SECTOR_NOT_FOUND:
          errorname = "ERROR_SECTOR_NOT_FOUND";
          errormsg = "The drive cannot find the sector requested.";
          break;
        #endif
        #ifdef ERROR_OUT_OF_PAPER
        case ERROR_OUT_OF_PAPER:
          errorname = "ERROR_OUT_OF_PAPER";
          errormsg = "The printer is out of paper.";
          break;
        #endif
        #ifdef ERROR_WRITE_FAULT
        case ERROR_WRITE_FAULT:
          errorname = "ERROR_WRITE_FAULT";
          errormsg = "The system cannot write to the specified device.";
          break;
        #endif
        #ifdef ERROR_READ_FAULT
        case ERROR_READ_FAULT:
          errorname = "ERROR_READ_FAULT";
          errormsg = "The system cannot read from the specified device.";
          break;
        #endif
        #ifdef ERROR_GEN_FAILURE
        case ERROR_GEN_FAILURE:
          errorname = "ERROR_GEN_FAILURE";
          errormsg = "A device attached to the system is not functioning.";
          break;
        #endif
        #ifdef ERROR_SHARING_VIOLATION
        case ERROR_SHARING_VIOLATION:
          errorname = "ERROR_SHARING_VIOLATION";
          errormsg = "The process cannot access the file because it is being used by another process.";
          break;
        #endif
        #ifdef ERROR_LOCK_VIOLATION
        case ERROR_LOCK_VIOLATION:
          errorname = "ERROR_LOCK_VIOLATION";
          errormsg = "The process cannot access the file because another process has locked a portion of the file.";
          break;
        #endif
        #ifdef ERROR_WRONG_DISK
        case ERROR_WRONG_DISK:
          errorname = "ERROR_WRONG_DISK";
          errormsg = "The wrong diskette is in the drive. Insert %2 (Volume Serial Number: %3) into drive %1.";
          break;
        #endif
        #ifdef ERROR_SHARING_BUFFER_EXCEEDED
        case ERROR_SHARING_BUFFER_EXCEEDED:
          errorname = "ERROR_SHARING_BUFFER_EXCEEDED";
          errormsg = "Too many files opened for sharing.";
          break;
        #endif
        #ifdef ERROR_HANDLE_EOF
        case ERROR_HANDLE_EOF:
          errorname = "ERROR_HANDLE_EOF";
          errormsg = "Reached end of file.";
          break;
        #endif
        #ifdef ERROR_HANDLE_DISK_FULL
        case ERROR_HANDLE_DISK_FULL:
          errorname = "ERROR_HANDLE_DISK_FULL";
          errormsg = "The disk is full.";
          break;
        #endif
        #ifdef ERROR_NOT_SUPPORTED
        case ERROR_NOT_SUPPORTED:
          errorname = "ERROR_NOT_SUPPORTED";
          errormsg = "The network request is not supported.";
          break;
        #endif
        #ifdef ERROR_REM_NOT_LIST
        case ERROR_REM_NOT_LIST:
          errorname = "ERROR_REM_NOT_LIST";
          errormsg = "The remote computer is not available.";
          break;
        #endif
        #ifdef ERROR_DUP_NAME
        case ERROR_DUP_NAME:
          errorname = "ERROR_DUP_NAME";
          errormsg = "A duplicate name exists on the network.";
          break;
        #endif
        #ifdef ERROR_BAD_NETPATH
        case ERROR_BAD_NETPATH:
          errorname = "ERROR_BAD_NETPATH";
          errormsg = "The network path was not found.";
          break;
        #endif
        #ifdef ERROR_NETWORK_BUSY
        case ERROR_NETWORK_BUSY:
          errorname = "ERROR_NETWORK_BUSY";
          errormsg = "The network is busy.";
          break;
        #endif
        #ifdef ERROR_DEV_NOT_EXIST
        case ERROR_DEV_NOT_EXIST:
          errorname = "ERROR_DEV_NOT_EXIST";
          errormsg = "The specified network resource or device is no longer available.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_CMDS
        case ERROR_TOO_MANY_CMDS:
          errorname = "ERROR_TOO_MANY_CMDS";
          errormsg = "The network BIOS command limit has been reached.";
          break;
        #endif
        #ifdef ERROR_ADAP_HDW_ERR
        case ERROR_ADAP_HDW_ERR:
          errorname = "ERROR_ADAP_HDW_ERR";
          errormsg = "A network adapter hardware error occurred.";
          break;
        #endif
        #ifdef ERROR_BAD_NET_RESP
        case ERROR_BAD_NET_RESP:
          errorname = "ERROR_BAD_NET_RESP";
          errormsg = "The specified server cannot perform the requested operation.";
          break;
        #endif
        #ifdef ERROR_UNEXP_NET_ERR
        case ERROR_UNEXP_NET_ERR:
          errorname = "ERROR_UNEXP_NET_ERR";
          errormsg = "An unexpected network error occurred.";
          break;
        #endif
        #ifdef ERROR_BAD_REM_ADAP
        case ERROR_BAD_REM_ADAP:
          errorname = "ERROR_BAD_REM_ADAP";
          errormsg = "The remote adapter is not compatible.";
          break;
        #endif
        #ifdef ERROR_PRINTQ_FULL
        case ERROR_PRINTQ_FULL:
          errorname = "ERROR_PRINTQ_FULL";
          errormsg = "The printer queue is full.";
          break;
        #endif
        #ifdef ERROR_NO_SPOOL_SPACE
        case ERROR_NO_SPOOL_SPACE:
          errorname = "ERROR_NO_SPOOL_SPACE";
          errormsg = "Space to store the file waiting to be printed is not available on the server.";
          break;
        #endif
        #ifdef ERROR_PRINT_CANCELLED
        case ERROR_PRINT_CANCELLED:
          errorname = "ERROR_PRINT_CANCELLED";
          errormsg = "Your file waiting to be printed was deleted.";
          break;
        #endif
        #ifdef ERROR_NETNAME_DELETED
        case ERROR_NETNAME_DELETED:
          errorname = "ERROR_NETNAME_DELETED";
          errormsg = "The specified network name is no longer available.";
          break;
        #endif
        #ifdef ERROR_NETWORK_ACCESS_DENIED
        case ERROR_NETWORK_ACCESS_DENIED:
          errorname = "ERROR_NETWORK_ACCESS_DENIED";
          errormsg = "Network access is denied.";
          break;
        #endif
        #ifdef ERROR_BAD_DEV_TYPE
        case ERROR_BAD_DEV_TYPE:
          errorname = "ERROR_BAD_DEV_TYPE";
          errormsg = "The network resource type is not correct.";
          break;
        #endif
        #ifdef ERROR_BAD_NET_NAME
        case ERROR_BAD_NET_NAME:
          errorname = "ERROR_BAD_NET_NAME";
          errormsg = "The network name cannot be found.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_NAMES
        case ERROR_TOO_MANY_NAMES:
          errorname = "ERROR_TOO_MANY_NAMES";
          errormsg = "The name limit for the local computer network adapter card was exceeded.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_SESS
        case ERROR_TOO_MANY_SESS:
          errorname = "ERROR_TOO_MANY_SESS";
          errormsg = "The network BIOS session limit was exceeded.";
          break;
        #endif
        #ifdef ERROR_SHARING_PAUSED
        case ERROR_SHARING_PAUSED:
          errorname = "ERROR_SHARING_PAUSED";
          errormsg = "The remote server has been paused or is in the process of being started.";
          break;
        #endif
        #ifdef ERROR_REQ_NOT_ACCEP
        case ERROR_REQ_NOT_ACCEP:
          errorname = "ERROR_REQ_NOT_ACCEP";
          errormsg = "No more connections can be made to this remote computer at this time because there are already as many connections as the computer can accept.";
          break;
        #endif
        #ifdef ERROR_REDIR_PAUSED
        case ERROR_REDIR_PAUSED:
          errorname = "ERROR_REDIR_PAUSED";
          errormsg = "The specified printer or disk device has been paused.";
          break;
        #endif
        #ifdef ERROR_FILE_EXISTS
        case ERROR_FILE_EXISTS:
          errorname = "ERROR_FILE_EXISTS";
          errormsg = "The file exists.";
          break;
        #endif
        #ifdef ERROR_CANNOT_MAKE
        case ERROR_CANNOT_MAKE:
          errorname = "ERROR_CANNOT_MAKE";
          errormsg = "The directory or file cannot be created.";
          break;
        #endif
        #ifdef ERROR_FAIL_I24
        case ERROR_FAIL_I24:
          errorname = "ERROR_FAIL_I24";
          errormsg = "Fail on INT 24";
          break;
        #endif
        #ifdef ERROR_OUT_OF_STRUCTURES
        case ERROR_OUT_OF_STRUCTURES:
          errorname = "ERROR_OUT_OF_STRUCTURES";
          errormsg = "Storage to process this request is not available.";
          break;
        #endif
        #ifdef ERROR_ALREADY_ASSIGNED
        case ERROR_ALREADY_ASSIGNED:
          errorname = "ERROR_ALREADY_ASSIGNED";
          errormsg = "The local device name is already in use.";
          break;
        #endif
        #ifdef ERROR_INVALID_PASSWORD
        case ERROR_INVALID_PASSWORD:
          errorname = "ERROR_INVALID_PASSWORD";
          errormsg = "The specified network password is not correct.";
          break;
        #endif
        #ifdef ERROR_INVALID_PARAMETER
        case ERROR_INVALID_PARAMETER:
          errorname = "ERROR_INVALID_PARAMETER";
          errormsg = "The parameter is incorrect.";
          break;
        #endif
        #ifdef ERROR_NET_WRITE_FAULT
        case ERROR_NET_WRITE_FAULT:
          errorname = "ERROR_NET_WRITE_FAULT";
          errormsg = "A write fault occurred on the network.";
          break;
        #endif
        #ifdef ERROR_NO_PROC_SLOTS
        case ERROR_NO_PROC_SLOTS:
          errorname = "ERROR_NO_PROC_SLOTS";
          errormsg = "The system cannot start another process at this time.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_SEMAPHORES
        case ERROR_TOO_MANY_SEMAPHORES:
          errorname = "ERROR_TOO_MANY_SEMAPHORES";
          errormsg = "Cannot create another system semaphore.";
          break;
        #endif
        #ifdef ERROR_EXCL_SEM_ALREADY_OWNED
        case ERROR_EXCL_SEM_ALREADY_OWNED:
          errorname = "ERROR_EXCL_SEM_ALREADY_OWNED";
          errormsg = "The exclusive semaphore is owned by another process.";
          break;
        #endif
        #ifdef ERROR_SEM_IS_SET
        case ERROR_SEM_IS_SET:
          errorname = "ERROR_SEM_IS_SET";
          errormsg = "The semaphore is set and cannot be closed.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_SEM_REQUESTS
        case ERROR_TOO_MANY_SEM_REQUESTS:
          errorname = "ERROR_TOO_MANY_SEM_REQUESTS";
          errormsg = "The semaphore cannot be set again.";
          break;
        #endif
        #ifdef ERROR_INVALID_AT_INTERRUPT_TIME
        case ERROR_INVALID_AT_INTERRUPT_TIME:
          errorname = "ERROR_INVALID_AT_INTERRUPT_TIME";
          errormsg = "Cannot request exclusive semaphores at interrupt time.";
          break;
        #endif
        #ifdef ERROR_SEM_OWNER_DIED
        case ERROR_SEM_OWNER_DIED:
          errorname = "ERROR_SEM_OWNER_DIED";
          errormsg = "The previous ownership of this semaphore has ended.";
          break;
        #endif
        #ifdef ERROR_SEM_USER_LIMIT
        case ERROR_SEM_USER_LIMIT:
          errorname = "ERROR_SEM_USER_LIMIT";
          errormsg = "Insert the diskette for drive %1.";
          break;
        #endif
        #ifdef ERROR_DISK_CHANGE
        case ERROR_DISK_CHANGE:
          errorname = "ERROR_DISK_CHANGE";
          errormsg = "Program stopped because alternate diskette was not inserted.";
          break;
        #endif
        #ifdef ERROR_DRIVE_LOCKED
        case ERROR_DRIVE_LOCKED:
          errorname = "ERROR_DRIVE_LOCKED";
          errormsg = "The disk is in use or locked by another process.";
          break;
        #endif
        #ifdef ERROR_BROKEN_PIPE
        case ERROR_BROKEN_PIPE:
          errorname = "ERROR_BROKEN_PIPE";
          errormsg = "The pipe has been ended.";
          break;
        #endif
        #ifdef ERROR_OPEN_FAILED
        case ERROR_OPEN_FAILED:
          errorname = "ERROR_OPEN_FAILED";
          errormsg = "The system cannot open the device or file specified.";
          break;
        #endif
        #ifdef ERROR_BUFFER_OVERFLOW
        case ERROR_BUFFER_OVERFLOW:
          errorname = "ERROR_BUFFER_OVERFLOW";
          errormsg = "The file name is too long.";
          break;
        #endif
        #ifdef ERROR_DISK_FULL
        case ERROR_DISK_FULL:
          errorname = "ERROR_DISK_FULL";
          errormsg = "There is not enough space on the disk.";
          break;
        #endif
        #ifdef ERROR_NO_MORE_SEARCH_HANDLES
        case ERROR_NO_MORE_SEARCH_HANDLES:
          errorname = "ERROR_NO_MORE_SEARCH_HANDLES";
          errormsg = "No more internal file identifiers available.";
          break;
        #endif
        #ifdef ERROR_INVALID_TARGET_HANDLE
        case ERROR_INVALID_TARGET_HANDLE:
          errorname = "ERROR_INVALID_TARGET_HANDLE";
          errormsg = "The target internal file identifier is incorrect.";
          break;
        #endif
        #ifdef ERROR_INVALID_CATEGORY
        case ERROR_INVALID_CATEGORY:
          errorname = "ERROR_INVALID_CATEGORY";
          errormsg = "The IOCTL call made by the application program is not correct.";
          break;
        #endif
        #ifdef ERROR_INVALID_VERIFY_SWITCH
        case ERROR_INVALID_VERIFY_SWITCH:
          errorname = "ERROR_INVALID_VERIFY_SWITCH";
          errormsg = "The verify-on-write switch parameter value is not correct.";
          break;
        #endif
        #ifdef ERROR_BAD_DRIVER_LEVEL
        case ERROR_BAD_DRIVER_LEVEL:
          errorname = "ERROR_BAD_DRIVER_LEVEL";
          errormsg = "The system does not support the command requested.";
          break;
        #endif
        #ifdef ERROR_CALL_NOT_IMPLEMENTED
        case ERROR_CALL_NOT_IMPLEMENTED:
          errorname = "ERROR_CALL_NOT_IMPLEMENTED";
          errormsg = "This function is only valid in Win32 mode.";
          break;
        #endif
        #ifdef ERROR_SEM_TIMEOUT
        case ERROR_SEM_TIMEOUT:
          errorname = "ERROR_SEM_TIMEOUT";
          errormsg = "The semaphore timeout period has expired.";
          break;
        #endif
        #ifdef ERROR_INSUFFICIENT_BUFFER
        case ERROR_INSUFFICIENT_BUFFER:
          errorname = "ERROR_INSUFFICIENT_BUFFER";
          errormsg = "The data area passed to a system call is too small.";
          break;
        #endif
        #ifdef ERROR_INVALID_NAME
        case ERROR_INVALID_NAME:
          errorname = "ERROR_INVALID_NAME";
          errormsg = "The filename, directory name, or volume label syntax is incorrect.";
          break;
        #endif
        #ifdef ERROR_INVALID_LEVEL
        case ERROR_INVALID_LEVEL:
          errorname = "ERROR_INVALID_LEVEL";
          errormsg = "The system call level is not correct.";
          break;
        #endif
        #ifdef ERROR_NO_VOLUME_LABEL
        case ERROR_NO_VOLUME_LABEL:
          errorname = "ERROR_NO_VOLUME_LABEL";
          errormsg = "The disk has no volume label.";
          break;
        #endif
        #ifdef ERROR_MOD_NOT_FOUND
        case ERROR_MOD_NOT_FOUND:
          errorname = "ERROR_MOD_NOT_FOUND";
          errormsg = "The specified module could not be found.";
          break;
        #endif
        #ifdef ERROR_PROC_NOT_FOUND
        case ERROR_PROC_NOT_FOUND:
          errorname = "ERROR_PROC_NOT_FOUND";
          errormsg = "The specified procedure could not be found.";
          break;
        #endif
        #ifdef ERROR_WAIT_NO_CHILDREN
        case ERROR_WAIT_NO_CHILDREN:
          errorname = "ERROR_WAIT_NO_CHILDREN";
          errormsg = "There are no child processes to wait for.";
          break;
        #endif
        #ifdef ERROR_CHILD_NOT_COMPLETE
        case ERROR_CHILD_NOT_COMPLETE:
          errorname = "ERROR_CHILD_NOT_COMPLETE";
          errormsg = "The %1 application cannot be run in Win32 mode.";
          break;
        #endif
        #ifdef ERROR_DIRECT_ACCESS_HANDLE
        case ERROR_DIRECT_ACCESS_HANDLE:
          errorname = "ERROR_DIRECT_ACCESS_HANDLE";
          errormsg = "Attempt to use a file handle to an open disk partition for an operation other than raw disk I/O.";
          break;
        #endif
        #ifdef ERROR_NEGATIVE_SEEK
        case ERROR_NEGATIVE_SEEK:
          errorname = "ERROR_NEGATIVE_SEEK";
          errormsg = "An attempt was made to move the file pointer before the beginning of the file.";
          break;
        #endif
        #ifdef ERROR_SEEK_ON_DEVICE
        case ERROR_SEEK_ON_DEVICE:
          errorname = "ERROR_SEEK_ON_DEVICE";
          errormsg = "The file pointer cannot be set on the specified device or file.";
          break;
        #endif
        #ifdef ERROR_IS_JOIN_TARGET
        case ERROR_IS_JOIN_TARGET:
          errorname = "ERROR_IS_JOIN_TARGET";
          errormsg = "A JOIN or SUBST command cannot be used for a drive that contains previously joined drives.";
          break;
        #endif
        #ifdef ERROR_IS_JOINED
        case ERROR_IS_JOINED:
          errorname = "ERROR_IS_JOINED";
          errormsg = "An attempt was made to use a JOIN or SUBST command on a drive that has already been joined.";
          break;
        #endif
        #ifdef ERROR_IS_SUBSTED
        case ERROR_IS_SUBSTED:
          errorname = "ERROR_IS_SUBSTED";
          errormsg = "An attempt was made to use a JOIN or SUBST command on a drive that has already been substituted.";
          break;
        #endif
        #ifdef ERROR_NOT_JOINED
        case ERROR_NOT_JOINED:
          errorname = "ERROR_NOT_JOINED";
          errormsg = "The system tried to delete the JOIN of a drive that is not joined.";
          break;
        #endif
        #ifdef ERROR_NOT_SUBSTED
        case ERROR_NOT_SUBSTED:
          errorname = "ERROR_NOT_SUBSTED";
          errormsg = "The system tried to delete the substitution of a drive that is not substituted.";
          break;
        #endif
        #ifdef ERROR_JOIN_TO_JOIN
        case ERROR_JOIN_TO_JOIN:
          errorname = "ERROR_JOIN_TO_JOIN";
          errormsg = "The system tried to join a drive to a directory on a joined drive.";
          break;
        #endif
        #ifdef ERROR_SUBST_TO_SUBST
        case ERROR_SUBST_TO_SUBST:
          errorname = "ERROR_SUBST_TO_SUBST";
          errormsg = "The system tried to substitute a drive to a directory on a substituted drive.";
          break;
        #endif
        #ifdef ERROR_JOIN_TO_SUBST
        case ERROR_JOIN_TO_SUBST:
          errorname = "ERROR_JOIN_TO_SUBST";
          errormsg = "The system tried to join a drive to a directory on a substituted drive.";
          break;
        #endif
        #ifdef ERROR_SUBST_TO_JOIN
        case ERROR_SUBST_TO_JOIN:
          errorname = "ERROR_SUBST_TO_JOIN";
          errormsg = "The system tried to SUBST a drive to a directory on a joined drive.";
          break;
        #endif
        #ifdef ERROR_BUSY_DRIVE
        case ERROR_BUSY_DRIVE:
          errorname = "ERROR_BUSY_DRIVE";
          errormsg = "The system cannot perform a JOIN or SUBST at this time.";
          break;
        #endif
        #ifdef ERROR_SAME_DRIVE
        case ERROR_SAME_DRIVE:
          errorname = "ERROR_SAME_DRIVE";
          errormsg = "The system cannot join or substitute a drive to or for a directory on the same drive.";
          break;
        #endif
        #ifdef ERROR_DIR_NOT_ROOT
        case ERROR_DIR_NOT_ROOT:
          errorname = "ERROR_DIR_NOT_ROOT";
          errormsg = "The directory is not a subdirectory of the root directory.";
          break;
        #endif
        #ifdef ERROR_DIR_NOT_EMPTY
        case ERROR_DIR_NOT_EMPTY:
          errorname = "ERROR_DIR_NOT_EMPTY";
          errormsg = "The directory is not empty.";
          break;
        #endif
        #ifdef ERROR_IS_SUBST_PATH
        case ERROR_IS_SUBST_PATH:
          errorname = "ERROR_IS_SUBST_PATH";
          errormsg = "The path specified is being used in a substitute.";
          break;
        #endif
        #ifdef ERROR_IS_JOIN_PATH
        case ERROR_IS_JOIN_PATH:
          errorname = "ERROR_IS_JOIN_PATH";
          errormsg = "Not enough resources are available to process this command.";
          break;
        #endif
        #ifdef ERROR_PATH_BUSY
        case ERROR_PATH_BUSY:
          errorname = "ERROR_PATH_BUSY";
          errormsg = "The path specified cannot be used at this time.";
          break;
        #endif
        #ifdef ERROR_IS_SUBST_TARGET
        case ERROR_IS_SUBST_TARGET:
          errorname = "ERROR_IS_SUBST_TARGET";
          errormsg = "An attempt was made to join or substitute a drive for which a directory on the drive is the target of a previous substitute.";
          break;
        #endif
        #ifdef ERROR_SYSTEM_TRACE
        case ERROR_SYSTEM_TRACE:
          errorname = "ERROR_SYSTEM_TRACE";
          errormsg = "System trace information was not specified in your CONFIG.SYS file, or tracing is disallowed.";
          break;
        #endif
        #ifdef ERROR_INVALID_EVENT_COUNT
        case ERROR_INVALID_EVENT_COUNT:
          errorname = "ERROR_INVALID_EVENT_COUNT";
          errormsg = "The number of specified semaphore events for DosMuxSemWait is not correct.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_MUXWAITERS
        case ERROR_TOO_MANY_MUXWAITERS:
          errorname = "ERROR_TOO_MANY_MUXWAITERS";
          errormsg = "DosMuxSemWait did not execute; too many semaphores are already set.";
          break;
        #endif
        #ifdef ERROR_INVALID_LIST_FORMAT
        case ERROR_INVALID_LIST_FORMAT:
          errorname = "ERROR_INVALID_LIST_FORMAT";
          errormsg = "The DosMuxSemWait list is not correct.";
          break;
        #endif
        #ifdef ERROR_LABEL_TOO_LONG
        case ERROR_LABEL_TOO_LONG:
          errorname = "ERROR_LABEL_TOO_LONG";
          errormsg = "The volume label you entered exceeds the label character limit of the target file system.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_TCBS
        case ERROR_TOO_MANY_TCBS:
          errorname = "ERROR_TOO_MANY_TCBS";
          errormsg = "Cannot create another thread.";
          break;
        #endif
        #ifdef ERROR_SIGNAL_REFUSED
        case ERROR_SIGNAL_REFUSED:
          errorname = "ERROR_SIGNAL_REFUSED";
          errormsg = "The recipient process has refused the signal.";
          break;
        #endif
        #ifdef ERROR_DISCARDED
        case ERROR_DISCARDED:
          errorname = "ERROR_DISCARDED";
          errormsg = "The segment is already discarded and cannot be locked.";
          break;
        #endif
        #ifdef ERROR_NOT_LOCKED
        case ERROR_NOT_LOCKED:
          errorname = "ERROR_NOT_LOCKED";
          errormsg = "The segment is already unlocked.";
          break;
        #endif
        #ifdef ERROR_BAD_THREADID_ADDR
        case ERROR_BAD_THREADID_ADDR:
          errorname = "ERROR_BAD_THREADID_ADDR";
          errormsg = "The address for the thread ID is not correct.";
          break;
        #endif
        #ifdef ERROR_BAD_ARGUMENTS
        case ERROR_BAD_ARGUMENTS:
          errorname = "ERROR_BAD_ARGUMENTS";
          errormsg = "The argument string passed to DosExecPgm is not correct.";
          break;
        #endif
        #ifdef ERROR_BAD_PATHNAME
        case ERROR_BAD_PATHNAME:
          errorname = "ERROR_BAD_PATHNAME";
          errormsg = "The specified path is invalid.";
          break;
        #endif
        #ifdef ERROR_SIGNAL_PENDING
        case ERROR_SIGNAL_PENDING:
          errorname = "ERROR_SIGNAL_PENDING";
          errormsg = "A signal is already pending.";
          break;
        #endif
        #ifdef ERROR_MAX_THRDS_REACHED
        case ERROR_MAX_THRDS_REACHED:
          errorname = "ERROR_MAX_THRDS_REACHED";
          errormsg = "No more threads can be created in the system.";
          break;
        #endif
        #ifdef ERROR_LOCK_FAILED
        case ERROR_LOCK_FAILED:
          errorname = "ERROR_LOCK_FAILED";
          errormsg = "Unable to lock a region of a file.";
          break;
        #endif
        #ifdef ERROR_BUSY
        case ERROR_BUSY:
          errorname = "ERROR_BUSY";
          errormsg = "The requested resource is in use.";
          break;
        #endif
        #ifdef ERROR_CANCEL_VIOLATION
        case ERROR_CANCEL_VIOLATION:
          errorname = "ERROR_CANCEL_VIOLATION";
          errormsg = "A lock request was not outstanding for the supplied cancel region.";
          break;
        #endif
        #ifdef ERROR_ATOMIC_LOCKS_NOT_SUPPORTED
        case ERROR_ATOMIC_LOCKS_NOT_SUPPORTED:
          errorname = "ERROR_ATOMIC_LOCKS_NOT_SUPPORTED";
          errormsg = "The file system does not support atomic changes to the lock type.";
          break;
        #endif
        #ifdef ERROR_INVALID_SEGMENT_NUMBER
        case ERROR_INVALID_SEGMENT_NUMBER:
          errorname = "ERROR_INVALID_SEGMENT_NUMBER";
          errormsg = "The system detected a segment number that was not correct.";
          break;
        #endif
        #ifdef ERROR_INVALID_ORDINAL
        case ERROR_INVALID_ORDINAL:
          errorname = "ERROR_INVALID_ORDINAL";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_ALREADY_EXISTS
        case ERROR_ALREADY_EXISTS:
          errorname = "ERROR_ALREADY_EXISTS";
          errormsg = "Cannot create a file when that file already exists.";
          break;
        #endif
        #ifdef ERROR_INVALID_FLAG_NUMBER
        case ERROR_INVALID_FLAG_NUMBER:
          errorname = "ERROR_INVALID_FLAG_NUMBER";
          errormsg = "The flag passed is not correct.";
          break;
        #endif
        #ifdef ERROR_SEM_NOT_FOUND
        case ERROR_SEM_NOT_FOUND:
          errorname = "ERROR_SEM_NOT_FOUND";
          errormsg = "The specified system semaphore name was not found.";
          break;
        #endif
        #ifdef ERROR_INVALID_STARTING_CODESEG
        case ERROR_INVALID_STARTING_CODESEG:
          errorname = "ERROR_INVALID_STARTING_CODESEG";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_INVALID_STACKSEG
        case ERROR_INVALID_STACKSEG:
          errorname = "ERROR_INVALID_STACKSEG";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_INVALID_MODULETYPE
        case ERROR_INVALID_MODULETYPE:
          errorname = "ERROR_INVALID_MODULETYPE";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_INVALID_EXE_SIGNATURE
        case ERROR_INVALID_EXE_SIGNATURE:
          errorname = "ERROR_INVALID_EXE_SIGNATURE";
          errormsg = "Cannot run %1 in Win32 mode.";
          break;
        #endif
        #ifdef ERROR_EXE_MARKED_INVALID
        case ERROR_EXE_MARKED_INVALID:
          errorname = "ERROR_EXE_MARKED_INVALID";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_BAD_EXE_FORMAT
        case ERROR_BAD_EXE_FORMAT:
          errorname = "ERROR_BAD_EXE_FORMAT";
          errormsg = "%1 is not a valid Win32 application.";
          break;
        #endif
        #ifdef ERROR_ITERATED_DATA_EXCEEDS_64k
        case ERROR_ITERATED_DATA_EXCEEDS_64k:
          errorname = "ERROR_ITERATED_DATA_EXCEEDS_64k";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_INVALID_MINALLOCSIZE
        case ERROR_INVALID_MINALLOCSIZE:
          errorname = "ERROR_INVALID_MINALLOCSIZE";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_DYNLINK_FROM_INVALID_RING
        case ERROR_DYNLINK_FROM_INVALID_RING:
          errorname = "ERROR_DYNLINK_FROM_INVALID_RING";
          errormsg = "The operating system cannot run this application program.";
          break;
        #endif
        #ifdef ERROR_IOPL_NOT_ENABLED
        case ERROR_IOPL_NOT_ENABLED:
          errorname = "ERROR_IOPL_NOT_ENABLED";
          errormsg = "The operating system is not presently configured to run this application.";
          break;
        #endif
        #ifdef ERROR_INVALID_SEGDPL
        case ERROR_INVALID_SEGDPL:
          errorname = "ERROR_INVALID_SEGDPL";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_AUTODATASEG_EXCEEDS_64k
        case ERROR_AUTODATASEG_EXCEEDS_64k:
          errorname = "ERROR_AUTODATASEG_EXCEEDS_64k";
          errormsg = "The operating system cannot run this application program.";
          break;
        #endif
        #ifdef ERROR_RING2SEG_MUST_BE_MOVABLE
        case ERROR_RING2SEG_MUST_BE_MOVABLE:
          errorname = "ERROR_RING2SEG_MUST_BE_MOVABLE";
          errormsg = "The code segment cannot be greater than or equal to 64KB.";
          break;
        #endif
        #ifdef ERROR_RELOC_CHAIN_XEEDS_SEGLIM
        case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:
          errorname = "ERROR_RELOC_CHAIN_XEEDS_SEGLIM";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_INFLOOP_IN_RELOC_CHAIN
        case ERROR_INFLOOP_IN_RELOC_CHAIN:
          errorname = "ERROR_INFLOOP_IN_RELOC_CHAIN";
          errormsg = "The operating system cannot run %1.";
          break;
        #endif
        #ifdef ERROR_ENVVAR_NOT_FOUND
        case ERROR_ENVVAR_NOT_FOUND:
          errorname = "ERROR_ENVVAR_NOT_FOUND";
          errormsg = "The system could not find the environment option that was entered.";
          break;
        #endif
        #ifdef ERROR_NO_SIGNAL_SENT
        case ERROR_NO_SIGNAL_SENT:
          errorname = "ERROR_NO_SIGNAL_SENT";
          errormsg = "No process in the command subtree has a signal handler.";
          break;
        #endif
        #ifdef ERROR_FILENAME_EXCED_RANGE
        case ERROR_FILENAME_EXCED_RANGE:
          errorname = "ERROR_FILENAME_EXCED_RANGE";
          errormsg = "The filename or extension is too long.";
          break;
        #endif
        #ifdef ERROR_RING2_STACK_IN_USE
        case ERROR_RING2_STACK_IN_USE:
          errorname = "ERROR_RING2_STACK_IN_USE";
          errormsg = "The ring 2 stack is in use.";
          break;
        #endif
        #ifdef ERROR_META_EXPANSION_TOO_LONG
        case ERROR_META_EXPANSION_TOO_LONG:
          errorname = "ERROR_META_EXPANSION_TOO_LONG";
          errormsg = "The global filename characters, * or ?, are entered incorrectly or too many global filename characters are specified.";
          break;
        #endif
        #ifdef ERROR_INVALID_SIGNAL_NUMBER
        case ERROR_INVALID_SIGNAL_NUMBER:
          errorname = "ERROR_INVALID_SIGNAL_NUMBER";
          errormsg = "The signal being posted is not correct.";
          break;
        #endif
        #ifdef ERROR_THREAD_1_INACTIVE
        case ERROR_THREAD_1_INACTIVE:
          errorname = "ERROR_THREAD_1_INACTIVE";
          errormsg = "The signal handler cannot be set.";
          break;
        #endif
        #ifdef ERROR_LOCKED
        case ERROR_LOCKED:
          errorname = "ERROR_LOCKED";
          errormsg = "The segment is locked and cannot be reallocated.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_MODULES
        case ERROR_TOO_MANY_MODULES:
          errorname = "ERROR_TOO_MANY_MODULES";
          errormsg = "Too many dynamic link modules are attached to this program or dynamic link module.";
          break;
        #endif
        #ifdef ERROR_NESTING_NOT_ALLOWED
        case ERROR_NESTING_NOT_ALLOWED:
          errorname = "ERROR_NESTING_NOT_ALLOWED";
          errormsg = "Cannot nest calls to LoadModule.";
          break;
        #endif
        #ifdef ERROR_BAD_PIPE
        case ERROR_BAD_PIPE:
          errorname = "ERROR_BAD_PIPE";
          errormsg = "The pipe state is invalid.";
          break;
        #endif
        #ifdef ERROR_PIPE_BUSY
        case ERROR_PIPE_BUSY:
          errorname = "ERROR_PIPE_BUSY";
          errormsg = "All pipe instances are busy.";
          break;
        #endif
        #ifdef ERROR_NO_DATA
        case ERROR_NO_DATA:
          errorname = "ERROR_NO_DATA";
          errormsg = "The pipe is being closed.";
          break;
        #endif
        #ifdef ERROR_PIPE_NOT_CONNECTED
        case ERROR_PIPE_NOT_CONNECTED:
          errorname = "ERROR_PIPE_NOT_CONNECTED";
          errormsg = "No process is on the other end of the pipe.";
          break;
        #endif
        #ifdef ERROR_MORE_DATA
        case ERROR_MORE_DATA:
          errorname = "ERROR_MORE_DATA";
          errormsg = "More data is available.";
          break;
        #endif
        #ifdef ERROR_VC_DISCONNECTED
        case ERROR_VC_DISCONNECTED:
          errorname = "ERROR_VC_DISCONNECTED";
          errormsg = "The session was cancelled.";
          break;
        #endif
        #ifdef ERROR_INVALID_EA_NAME
        case ERROR_INVALID_EA_NAME:
          errorname = "ERROR_INVALID_EA_NAME";
          errormsg = "The specified extended attribute name was invalid.";
          break;
        #endif
        #ifdef ERROR_EA_LIST_INCONSISTENT
        case ERROR_EA_LIST_INCONSISTENT:
          errorname = "ERROR_EA_LIST_INCONSISTENT";
          errormsg = "The extended attributes are inconsistent.";
          break;
        #endif
        #ifdef ERROR_NO_MORE_ITEMS
        case ERROR_NO_MORE_ITEMS:
          errorname = "ERROR_NO_MORE_ITEMS";
          errormsg = "No more data is available.";
          break;
        #endif
        #ifdef ERROR_CANNOT_COPY
        case ERROR_CANNOT_COPY:
          errorname = "ERROR_CANNOT_COPY";
          errormsg = "The Copy API cannot be used.";
          break;
        #endif
        #ifdef ERROR_DIRECTORY
        case ERROR_DIRECTORY:
          errorname = "ERROR_DIRECTORY";
          errormsg = "The directory name is invalid.";
          break;
        #endif
        #ifdef ERROR_EAS_DIDNT_FIT
        case ERROR_EAS_DIDNT_FIT:
          errorname = "ERROR_EAS_DIDNT_FIT";
          errormsg = "The extended attributes did not fit in the buffer.";
          break;
        #endif
        #ifdef ERROR_EA_FILE_CORRUPT
        case ERROR_EA_FILE_CORRUPT:
          errorname = "ERROR_EA_FILE_CORRUPT";
          errormsg = "The extended attribute file on the mounted file system is corrupt.";
          break;
        #endif
        #ifdef ERROR_EA_TABLE_FULL
        case ERROR_EA_TABLE_FULL:
          errorname = "ERROR_EA_TABLE_FULL";
          errormsg = "The extended attribute table file is full.";
          break;
        #endif
        #ifdef ERROR_INVALID_EA_HANDLE
        case ERROR_INVALID_EA_HANDLE:
          errorname = "ERROR_INVALID_EA_HANDLE";
          errormsg = "The specified extended attribute handle is invalid.";
          break;
        #endif
        #ifdef ERROR_EAS_NOT_SUPPORTED
        case ERROR_EAS_NOT_SUPPORTED:
          errorname = "ERROR_EAS_NOT_SUPPORTED";
          errormsg = "The mounted file system does not support extended attributes.";
          break;
        #endif
        #ifdef ERROR_NOT_OWNER
        case ERROR_NOT_OWNER:
          errorname = "ERROR_NOT_OWNER";
          errormsg = "Attempt to release mutex not owned by caller.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_POSTS
        case ERROR_TOO_MANY_POSTS:
          errorname = "ERROR_TOO_MANY_POSTS";
          errormsg = "Too many posts were made to a semaphore.";
          break;
        #endif
        #ifdef ERROR_PARTIAL_COPY
        case ERROR_PARTIAL_COPY:
          errorname = "ERROR_PARTIAL_COPY";
          errormsg = "Only part of a Read/WriteProcessMemory request was completed.";
          break;
        #endif
        #ifdef ERROR_MR_MID_NOT_FOUND
        case ERROR_MR_MID_NOT_FOUND:
          errorname = "ERROR_MR_MID_NOT_FOUND";
          errormsg = "The system cannot find message for message number 0x%1 in message file for %2.";
          break;
        #endif
        #ifdef ERROR_INVALID_ADDRESS
        case ERROR_INVALID_ADDRESS:
          errorname = "ERROR_INVALID_ADDRESS";
          errormsg = "Attempt to access invalid address.";
          break;
        #endif
        #ifdef ERROR_ARITHMETIC_OVERFLOW
        case ERROR_ARITHMETIC_OVERFLOW:
          errorname = "ERROR_ARITHMETIC_OVERFLOW";
          errormsg = "Arithmetic result exceeded 32 bits.";
          break;
        #endif
        #ifdef ERROR_PIPE_CONNECTED
        case ERROR_PIPE_CONNECTED:
          errorname = "ERROR_PIPE_CONNECTED";
          errormsg = "There is a process on other end of the pipe.";
          break;
        #endif
        #ifdef ERROR_PIPE_LISTENING
        case ERROR_PIPE_LISTENING:
          errorname = "ERROR_PIPE_LISTENING";
          errormsg = "Waiting for a process to open the other end of the pipe.";
          break;
        #endif
        #ifdef ERROR_EA_ACCESS_DENIED
        case ERROR_EA_ACCESS_DENIED:
          errorname = "ERROR_EA_ACCESS_DENIED";
          errormsg = "Access to the extended attribute was denied.";
          break;
        #endif
        #ifdef ERROR_OPERATION_ABORTED
        case ERROR_OPERATION_ABORTED:
          errorname = "ERROR_OPERATION_ABORTED";
          errormsg = "The I/O operation has been aborted because of either a thread exit or an application request.";
          break;
        #endif
        #ifdef ERROR_IO_INCOMPLETE
        case ERROR_IO_INCOMPLETE:
          errorname = "ERROR_IO_INCOMPLETE";
          errormsg = "Overlapped I/O event is not in a signalled state.";
          break;
        #endif
        #ifdef ERROR_IO_PENDING
        case ERROR_IO_PENDING:
          errorname = "ERROR_IO_PENDING";
          errormsg = "Overlapped I/O operation is in progress.";
          break;
        #endif
        #ifdef ERROR_NOACCESS
        case ERROR_NOACCESS:
          errorname = "ERROR_NOACCESS";
          errormsg = "Invalid access to memory location.";
          break;
        #endif
        #ifdef ERROR_SWAPERROR
        case ERROR_SWAPERROR:
          errorname = "ERROR_SWAPERROR";
          errormsg = "Error performing inpage operation.";
          break;
        #endif
        #ifdef ERROR_STACK_OVERFLOW
        case ERROR_STACK_OVERFLOW:
          errorname = "ERROR_STACK_OVERFLOW";
          errormsg = "Recursion too deep, stack overflowed.";
          break;
        #endif
        #ifdef ERROR_INVALID_MESSAGE
        case ERROR_INVALID_MESSAGE:
          errorname = "ERROR_INVALID_MESSAGE";
          errormsg = "The window cannot act on the sent message.";
          break;
        #endif
        #ifdef ERROR_CAN_NOT_COMPLETE
        case ERROR_CAN_NOT_COMPLETE:
          errorname = "ERROR_CAN_NOT_COMPLETE";
          errormsg = "Cannot complete this function.";
          break;
        #endif
        #ifdef ERROR_INVALID_FLAGS
        case ERROR_INVALID_FLAGS:
          errorname = "ERROR_INVALID_FLAGS";
          errormsg = "Invalid flags.";
          break;
        #endif
        #ifdef ERROR_UNRECOGNIZED_VOLUME
        case ERROR_UNRECOGNIZED_VOLUME:
          errorname = "ERROR_UNRECOGNIZED_VOLUME";
          errormsg = "The volume does not contain a recognized file system. Please make sure that all required file system drivers are loaded and that the volume is not corrupt.";
          break;
        #endif
        #ifdef ERROR_FILE_INVALID
        case ERROR_FILE_INVALID:
          errorname = "ERROR_FILE_INVALID";
          errormsg = "The volume for a file has been externally altered such that the opened file is no longer valid.";
          break;
        #endif
        #ifdef ERROR_FULLSCREEN_MODE
        case ERROR_FULLSCREEN_MODE:
          errorname = "ERROR_FULLSCREEN_MODE";
          errormsg = "The requested operation cannot be performed in full-screen mode.";
          break;
        #endif
        #ifdef ERROR_NO_TOKEN
        case ERROR_NO_TOKEN:
          errorname = "ERROR_NO_TOKEN";
          errormsg = "An attempt was made to reference a token that does not exist.";
          break;
        #endif
        #ifdef ERROR_BADDB
        case ERROR_BADDB:
          errorname = "ERROR_BADDB";
          errormsg = "The configuration registry database is corrupt.";
          break;
        #endif
        #ifdef ERROR_BADKEY
        case ERROR_BADKEY:
          errorname = "ERROR_BADKEY";
          errormsg = "The configuration registry key is invalid.";
          break;
        #endif
        #ifdef ERROR_CANTOPEN
        case ERROR_CANTOPEN:
          errorname = "ERROR_CANTOPEN";
          errormsg = "The configuration registry key could not be opened.";
          break;
        #endif
        #ifdef ERROR_CANTREAD
        case ERROR_CANTREAD:
          errorname = "ERROR_CANTREAD";
          errormsg = "The configuration registry key could not be read.";
          break;
        #endif
        #ifdef ERROR_CANTWRITE
        case ERROR_CANTWRITE:
          errorname = "ERROR_CANTWRITE";
          errormsg = "The configuration registry key could not be written.";
          break;
        #endif
        #ifdef ERROR_REGISTRY_RECOVERED
        case ERROR_REGISTRY_RECOVERED:
          errorname = "ERROR_REGISTRY_RECOVERED";
          errormsg = "One of the files in the Registry database had to be recovered by use of a log or alternate copy. The recovery was successful.";
          break;
        #endif
        #ifdef ERROR_REGISTRY_CORRUPT
        case ERROR_REGISTRY_CORRUPT:
          errorname = "ERROR_REGISTRY_CORRUPT";
          errormsg = "The Registry is corrupt. The structure of one of the files that contains Registry data is corrupt, or the system's image of the file in memory is corrupt, or the file could not be recovered because the alternate copy or log was absent or corrupt.";
          break;
        #endif
        #ifdef ERROR_REGISTRY_IO_FAILED
        case ERROR_REGISTRY_IO_FAILED:
          errorname = "ERROR_REGISTRY_IO_FAILED";
          errormsg = "An I/O operation initiated by the Registry failed unrecoverably. The Registry could not read in, or write out, or flush, one of the files that contain the system's image of the Registry.";
          break;
        #endif
        #ifdef ERROR_NOT_REGISTRY_FILE
        case ERROR_NOT_REGISTRY_FILE:
          errorname = "ERROR_NOT_REGISTRY_FILE";
          errormsg = "The system has attempted to load or restore a file into the Registry, but the specified file is not in a Registry file format.";
          break;
        #endif
        #ifdef ERROR_KEY_DELETED
        case ERROR_KEY_DELETED:
          errorname = "ERROR_KEY_DELETED";
          errormsg = "Illegal operation attempted on a Registry key which has been marked for deletion.";
          break;
        #endif
        #ifdef ERROR_NO_LOG_SPACE
        case ERROR_NO_LOG_SPACE:
          errorname = "ERROR_NO_LOG_SPACE";
          errormsg = "System could not allocate the required space in a Registry log.";
          break;
        #endif
        #ifdef ERROR_KEY_HAS_CHILDREN
        case ERROR_KEY_HAS_CHILDREN:
          errorname = "ERROR_KEY_HAS_CHILDREN";
          errormsg = "Cannot create a symbolic link in a Registry key that already has subkeys or values.";
          break;
        #endif
        #ifdef ERROR_CHILD_MUST_BE_VOLATILE
        case ERROR_CHILD_MUST_BE_VOLATILE:
          errorname = "ERROR_CHILD_MUST_BE_VOLATILE";
          errormsg = "Cannot create a stable subkey under a volatile parent key.";
          break;
        #endif
        #ifdef ERROR_NOTIFY_ENUM_DIR
        case ERROR_NOTIFY_ENUM_DIR:
          errorname = "ERROR_NOTIFY_ENUM_DIR";
          errormsg = "A notify change request is being completed and the information is not being returned in the caller's buffer. The caller now needs to enumerate the files to find the changes.";
          break;
        #endif
        #ifdef ERROR_DEPENDENT_SERVICES_RUNNING
        case ERROR_DEPENDENT_SERVICES_RUNNING:
          errorname = "ERROR_DEPENDENT_SERVICES_RUNNING";
          errormsg = "A stop control has been sent to a service which other running services are dependent on.";
          break;
        #endif
        #ifdef ERROR_INVALID_SERVICE_CONTROL
        case ERROR_INVALID_SERVICE_CONTROL:
          errorname = "ERROR_INVALID_SERVICE_CONTROL";
          errormsg = "The requested control is not valid for this service.";
          break;
        #endif
        #ifdef ERROR_SERVICE_REQUEST_TIMEOUT
        case ERROR_SERVICE_REQUEST_TIMEOUT:
          errorname = "ERROR_SERVICE_REQUEST_TIMEOUT";
          errormsg = "The service did not respond to the start or control request in a timely fashion.";
          break;
        #endif
        #ifdef ERROR_SERVICE_NO_THREAD
        case ERROR_SERVICE_NO_THREAD:
          errorname = "ERROR_SERVICE_NO_THREAD";
          errormsg = "A thread could not be created for the service.";
          break;
        #endif
        #ifdef ERROR_SERVICE_DATABASE_LOCKED
        case ERROR_SERVICE_DATABASE_LOCKED:
          errorname = "ERROR_SERVICE_DATABASE_LOCKED";
          errormsg = "The service database is locked.";
          break;
        #endif
        #ifdef ERROR_SERVICE_ALREADY_RUNNING
        case ERROR_SERVICE_ALREADY_RUNNING:
          errorname = "ERROR_SERVICE_ALREADY_RUNNING";
          errormsg = "An instance of the service is already running.";
          break;
        #endif
        #ifdef ERROR_INVALID_SERVICE_ACCOUNT
        case ERROR_INVALID_SERVICE_ACCOUNT:
          errorname = "ERROR_INVALID_SERVICE_ACCOUNT";
          errormsg = "The account name is invalid or does not exist.";
          break;
        #endif
        #ifdef ERROR_SERVICE_DISABLED
        case ERROR_SERVICE_DISABLED:
          errorname = "ERROR_SERVICE_DISABLED";
          errormsg = "The specified service is disabled and cannot be started.";
          break;
        #endif
        #ifdef ERROR_CIRCULAR_DEPENDENCY
        case ERROR_CIRCULAR_DEPENDENCY:
          errorname = "ERROR_CIRCULAR_DEPENDENCY";
          errormsg = "Circular service dependency was specified.";
          break;
        #endif
        #ifdef ERROR_SERVICE_DOES_NOT_EXIST
        case ERROR_SERVICE_DOES_NOT_EXIST:
          errorname = "ERROR_SERVICE_DOES_NOT_EXIST";
          errormsg = "The specified service does not exist as an installed service.";
          break;
        #endif
        #ifdef ERROR_SERVICE_CANNOT_ACCEPT_CTRL
        case ERROR_SERVICE_CANNOT_ACCEPT_CTRL:
          errorname = "ERROR_SERVICE_CANNOT_ACCEPT_CTRL";
          errormsg = "The service cannot accept control messages at this time.";
          break;
        #endif
        #ifdef ERROR_SERVICE_NOT_ACTIVE
        case ERROR_SERVICE_NOT_ACTIVE:
          errorname = "ERROR_SERVICE_NOT_ACTIVE";
          errormsg = "The service has not been started.";
          break;
        #endif
        #ifdef ERROR_FAILED_SERVICE_CONTROLLER_CONNECT
        case ERROR_FAILED_SERVICE_CONTROLLER_CONNECT:
          errorname = "ERROR_FAILED_SERVICE_CONTROLLER_CONNECT";
          errormsg = "The service process could not connect to the service controller.";
          break;
        #endif
        #ifdef ERROR_EXCEPTION_IN_SERVICE
        case ERROR_EXCEPTION_IN_SERVICE:
          errorname = "ERROR_EXCEPTION_IN_SERVICE";
          errormsg = "An exception occurred in the service when handling the control request.";
          break;
        #endif
        #ifdef ERROR_DATABASE_DOES_NOT_EXIST
        case ERROR_DATABASE_DOES_NOT_EXIST:
          errorname = "ERROR_DATABASE_DOES_NOT_EXIST";
          errormsg = "The database specified does not exist.";
          break;
        #endif
        #ifdef ERROR_SERVICE_SPECIFIC_ERROR
        case ERROR_SERVICE_SPECIFIC_ERROR:
          errorname = "ERROR_SERVICE_SPECIFIC_ERROR";
          errormsg = "The service has returned a service-specific error code.";
          break;
        #endif
        #ifdef ERROR_PROCESS_ABORTED
        case ERROR_PROCESS_ABORTED:
          errorname = "ERROR_PROCESS_ABORTED";
          errormsg = "The process terminated unexpectedly.";
          break;
        #endif
        #ifdef ERROR_SERVICE_DEPENDENCY_FAIL
        case ERROR_SERVICE_DEPENDENCY_FAIL:
          errorname = "ERROR_SERVICE_DEPENDENCY_FAIL";
          errormsg = "The dependency service or group failed to start.";
          break;
        #endif
        #ifdef ERROR_SERVICE_LOGON_FAILED
        case ERROR_SERVICE_LOGON_FAILED:
          errorname = "ERROR_SERVICE_LOGON_FAILED";
          errormsg = "The service did not start due to a logon failure.";
          break;
        #endif
        #ifdef ERROR_SERVICE_START_HANG
        case ERROR_SERVICE_START_HANG:
          errorname = "ERROR_SERVICE_START_HANG";
          errormsg = "After starting, the service hung in a start-pending state.";
          break;
        #endif
        #ifdef ERROR_INVALID_SERVICE_LOCK
        case ERROR_INVALID_SERVICE_LOCK:
          errorname = "ERROR_INVALID_SERVICE_LOCK";
          errormsg = "The specified service database lock is invalid.";
          break;
        #endif
        #ifdef ERROR_SERVICE_MARKED_FOR_DELETE
        case ERROR_SERVICE_MARKED_FOR_DELETE:
          errorname = "ERROR_SERVICE_MARKED_FOR_DELETE";
          errormsg = "The specified service has been marked for deletion.";
          break;
        #endif
        #ifdef ERROR_SERVICE_EXISTS
        case ERROR_SERVICE_EXISTS:
          errorname = "ERROR_SERVICE_EXISTS";
          errormsg = "The specified service already exists.";
          break;
        #endif
        #ifdef ERROR_ALREADY_RUNNING_LKG
        case ERROR_ALREADY_RUNNING_LKG:
          errorname = "ERROR_ALREADY_RUNNING_LKG";
          errormsg = "The system is currently running with the last-known-good configuration.";
          break;
        #endif
        #ifdef ERROR_SERVICE_DEPENDENCY_DELETED
        case ERROR_SERVICE_DEPENDENCY_DELETED:
          errorname = "ERROR_SERVICE_DEPENDENCY_DELETED";
          errormsg = "The dependency service does not exist or has been marked for deletion.";
          break;
        #endif
        #ifdef ERROR_BOOT_ALREADY_ACCEPTED
        case ERROR_BOOT_ALREADY_ACCEPTED:
          errorname = "ERROR_BOOT_ALREADY_ACCEPTED";
          errormsg = "The current boot has already been accepted for use as the last-known-good control set.";
          break;
        #endif
        #ifdef ERROR_SERVICE_NEVER_STARTED
        case ERROR_SERVICE_NEVER_STARTED:
          errorname = "ERROR_SERVICE_NEVER_STARTED";
          errormsg = "No attempts to start the service have been made since the last boot.";
          break;
        #endif
        #ifdef ERROR_DUPLICATE_SERVICE_NAME
        case ERROR_DUPLICATE_SERVICE_NAME:
          errorname = "ERROR_DUPLICATE_SERVICE_NAME";
          errormsg = "The name is already in use as either a service name or a service display name.";
          break;
        #endif
        #ifdef ERROR_END_OF_MEDIA
        case ERROR_END_OF_MEDIA:
          errorname = "ERROR_END_OF_MEDIA";
          errormsg = "The physical end of the tape has been reached.";
          break;
        #endif
        #ifdef ERROR_FILEMARK_DETECTED
        case ERROR_FILEMARK_DETECTED:
          errorname = "ERROR_FILEMARK_DETECTED";
          errormsg = "A tape access reached a filemark.";
          break;
        #endif
        #ifdef ERROR_BEGINNING_OF_MEDIA
        case ERROR_BEGINNING_OF_MEDIA:
          errorname = "ERROR_BEGINNING_OF_MEDIA";
          errormsg = "Beginning of tape or partition was encountered.";
          break;
        #endif
        #ifdef ERROR_SETMARK_DETECTED
        case ERROR_SETMARK_DETECTED:
          errorname = "ERROR_SETMARK_DETECTED";
          errormsg = "A tape access reached the end of a set of files.";
          break;
        #endif
        #ifdef ERROR_NO_DATA_DETECTED
        case ERROR_NO_DATA_DETECTED:
          errorname = "ERROR_NO_DATA_DETECTED";
          errormsg = "No more data is on the tape.";
          break;
        #endif
        #ifdef ERROR_PARTITION_FAILURE
        case ERROR_PARTITION_FAILURE:
          errorname = "ERROR_PARTITION_FAILURE";
          errormsg = "Tape could not be partitioned.";
          break;
        #endif
        #ifdef ERROR_INVALID_BLOCK_LENGTH
        case ERROR_INVALID_BLOCK_LENGTH:
          errorname = "ERROR_INVALID_BLOCK_LENGTH";
          errormsg = "When accessing a new tape of a multivolume partition, the current blocksize is incorrect.";
          break;
        #endif
        #ifdef ERROR_DEVICE_NOT_PARTITIONED
        case ERROR_DEVICE_NOT_PARTITIONED:
          errorname = "ERROR_DEVICE_NOT_PARTITIONED";
          errormsg = "Tape partition information could not be found when loading a tape.";
          break;
        #endif
        #ifdef ERROR_UNABLE_TO_LOCK_MEDIA
        case ERROR_UNABLE_TO_LOCK_MEDIA:
          errorname = "ERROR_UNABLE_TO_LOCK_MEDIA";
          errormsg = "Unable to lock the media eject mechanism.";
          break;
        #endif
        #ifdef ERROR_UNABLE_TO_UNLOAD_MEDIA
        case ERROR_UNABLE_TO_UNLOAD_MEDIA:
          errorname = "ERROR_UNABLE_TO_UNLOAD_MEDIA";
          errormsg = "Unable to unload the media.";
          break;
        #endif
        #ifdef ERROR_MEDIA_CHANGED
        case ERROR_MEDIA_CHANGED:
          errorname = "ERROR_MEDIA_CHANGED";
          errormsg = "Media in drive may have changed.";
          break;
        #endif
        #ifdef ERROR_BUS_RESET
        case ERROR_BUS_RESET:
          errorname = "ERROR_BUS_RESET";
          errormsg = "The I/O bus was reset.";
          break;
        #endif
        #ifdef ERROR_NO_MEDIA_IN_DRIVE
        case ERROR_NO_MEDIA_IN_DRIVE:
          errorname = "ERROR_NO_MEDIA_IN_DRIVE";
          errormsg = "No media in drive.";
          break;
        #endif
        #ifdef ERROR_NO_UNICODE_TRANSLATION
        case ERROR_NO_UNICODE_TRANSLATION:
          errorname = "ERROR_NO_UNICODE_TRANSLATION";
          errormsg = "No mapping for the Unicode character exists in the target multi-byte code page.";
          break;
        #endif
        #ifdef ERROR_DLL_INIT_FAILED
        case ERROR_DLL_INIT_FAILED:
          errorname = "ERROR_DLL_INIT_FAILED";
          errormsg = "A dynamic link library (DLL) initialization routine failed.";
          break;
        #endif
        #ifdef ERROR_SHUTDOWN_IN_PROGRESS
        case ERROR_SHUTDOWN_IN_PROGRESS:
          errorname = "ERROR_SHUTDOWN_IN_PROGRESS";
          errormsg = "A system shutdown is in progress.";
          break;
        #endif
        #ifdef ERROR_NO_SHUTDOWN_IN_PROGRESS
        case ERROR_NO_SHUTDOWN_IN_PROGRESS:
          errorname = "ERROR_NO_SHUTDOWN_IN_PROGRESS";
          errormsg = "Unable to abort the system shutdown because no shutdown was in progress.";
          break;
        #endif
        #ifdef ERROR_IO_DEVICE
        case ERROR_IO_DEVICE:
          errorname = "ERROR_IO_DEVICE";
          errormsg = "The request could not be performed because of an I/O device error.";
          break;
        #endif
        #ifdef ERROR_SERIAL_NO_DEVICE
        case ERROR_SERIAL_NO_DEVICE:
          errorname = "ERROR_SERIAL_NO_DEVICE";
          errormsg = "No serial device was successfully initialized. The serial driver will unload.";
          break;
        #endif
        #ifdef ERROR_IRQ_BUSY
        case ERROR_IRQ_BUSY:
          errorname = "ERROR_IRQ_BUSY";
          errormsg = "Unable to open a device that was sharing an interrupt request (IRQ) with other devices. At least one other device that uses that IRQ was already opened.";
          break;
        #endif
        #ifdef ERROR_MORE_WRITES
        case ERROR_MORE_WRITES:
          errorname = "ERROR_MORE_WRITES";
          errormsg = "A serial I/O operation was completed by another write to the serial port. (The IOCTL_SERIAL_XOFF_COUNTER reached zero.)";
          break;
        #endif
        #ifdef ERROR_COUNTER_TIMEOUT
        case ERROR_COUNTER_TIMEOUT:
          errorname = "ERROR_COUNTER_TIMEOUT";
          errormsg = "A serial I/O operation completed because the time-out period expired. (The IOCTL_SERIAL_XOFF_COUNTER did not reach zero.)";
          break;
        #endif
        #ifdef ERROR_FLOPPY_ID_MARK_NOT_FOUND
        case ERROR_FLOPPY_ID_MARK_NOT_FOUND:
          errorname = "ERROR_FLOPPY_ID_MARK_NOT_FOUND";
          errormsg = "No ID address mark was found on the floppy disk.";
          break;
        #endif
        #ifdef ERROR_FLOPPY_WRONG_CYLINDER
        case ERROR_FLOPPY_WRONG_CYLINDER:
          errorname = "ERROR_FLOPPY_WRONG_CYLINDER";
          errormsg = "Mismatch between the floppy disk sector ID field and the floppy disk controller track address.";
          break;
        #endif
        #ifdef ERROR_FLOPPY_UNKNOWN_ERROR
        case ERROR_FLOPPY_UNKNOWN_ERROR:
          errorname = "ERROR_FLOPPY_UNKNOWN_ERROR";
          errormsg = "The floppy disk controller reported an error that is not recognized by the floppy disk driver.";
          break;
        #endif
        #ifdef ERROR_FLOPPY_BAD_REGISTERS
        case ERROR_FLOPPY_BAD_REGISTERS:
          errorname = "ERROR_FLOPPY_BAD_REGISTERS";
          errormsg = "The floppy disk controller returned inconsistent results in its registers.";
          break;
        #endif
        #ifdef ERROR_DISK_RECALIBRATE_FAILED
        case ERROR_DISK_RECALIBRATE_FAILED:
          errorname = "ERROR_DISK_RECALIBRATE_FAILED";
          errormsg = "While accessing the hard disk, a recalibrate operation failed, even after retries.";
          break;
        #endif
        #ifdef ERROR_DISK_OPERATION_FAILED
        case ERROR_DISK_OPERATION_FAILED:
          errorname = "ERROR_DISK_OPERATION_FAILED";
          errormsg = "While accessing the hard disk, a disk operation failed even after retries.";
          break;
        #endif
        #ifdef ERROR_DISK_RESET_FAILED
        case ERROR_DISK_RESET_FAILED:
          errorname = "ERROR_DISK_RESET_FAILED";
          errormsg = "While accessing the hard disk, a disk controller reset was needed, but even that failed.";
          break;
        #endif
        #ifdef ERROR_EOM_OVERFLOW
        case ERROR_EOM_OVERFLOW:
          errorname = "ERROR_EOM_OVERFLOW";
          errormsg = "Physical end of tape encountered.";
          break;
        #endif
        #ifdef ERROR_NOT_ENOUGH_SERVER_MEMORY
        case ERROR_NOT_ENOUGH_SERVER_MEMORY:
          errorname = "ERROR_NOT_ENOUGH_SERVER_MEMORY";
          errormsg = "Not enough server storage is available to process this command.";
          break;
        #endif
        #ifdef ERROR_POSSIBLE_DEADLOCK
        case ERROR_POSSIBLE_DEADLOCK:
          errorname = "ERROR_POSSIBLE_DEADLOCK";
          errormsg = "A potential deadlock condition has been detected.";
          break;
        #endif
        #ifdef ERROR_MAPPED_ALIGNMENT
        case ERROR_MAPPED_ALIGNMENT:
          errorname = "ERROR_MAPPED_ALIGNMENT";
          errormsg = "The base address or the file offset specified does not have the proper alignment.";
          break;
        #endif
        #ifdef ERROR_SET_POWER_STATE_VETOED
        case ERROR_SET_POWER_STATE_VETOED:
          errorname = "ERROR_SET_POWER_STATE_VETOED";
          errormsg = "An attempt to change the system power state was vetoed by another application or driver.";
          break;
        #endif
        #ifdef ERROR_SET_POWER_STATE_FAILED
        case ERROR_SET_POWER_STATE_FAILED:
          errorname = "ERROR_SET_POWER_STATE_FAILED";
          errormsg = "The system BIOS failed an attempt to change the system power state.";
          break;
        #endif
        #ifdef ERROR_OLD_WIN_VERSION
        case ERROR_OLD_WIN_VERSION:
          errorname = "ERROR_OLD_WIN_VERSION";
          errormsg = "The specified program requires a newer version of Windows.";
          break;
        #endif
        #ifdef ERROR_APP_WRONG_OS
        case ERROR_APP_WRONG_OS:
          errorname = "ERROR_APP_WRONG_OS";
          errormsg = "The specified program is not a Windows or MS-DOS program.";
          break;
        #endif
        #ifdef ERROR_SINGLE_INSTANCE_APP
        case ERROR_SINGLE_INSTANCE_APP:
          errorname = "ERROR_SINGLE_INSTANCE_APP";
          errormsg = "Cannot start more than one instance of the specified program.";
          break;
        #endif
        #ifdef ERROR_RMODE_APP
        case ERROR_RMODE_APP:
          errorname = "ERROR_RMODE_APP";
          errormsg = "The specified program was written for an older version of Windows.";
          break;
        #endif
        #ifdef ERROR_INVALID_DLL
        case ERROR_INVALID_DLL:
          errorname = "ERROR_INVALID_DLL";
          errormsg = "One of the library files needed to run this application is damaged.";
          break;
        #endif
        #ifdef ERROR_NO_ASSOCIATION
        case ERROR_NO_ASSOCIATION:
          errorname = "ERROR_NO_ASSOCIATION";
          errormsg = "No application is associated with the specified file for this operation.";
          break;
        #endif
        #ifdef ERROR_DDE_FAIL
        case ERROR_DDE_FAIL:
          errorname = "ERROR_DDE_FAIL";
          errormsg = "An error occurred in sending the command to the application.";
          break;
        #endif
        #ifdef ERROR_DLL_NOT_FOUND
        case ERROR_DLL_NOT_FOUND:
          errorname = "ERROR_DLL_NOT_FOUND";
          errormsg = "One of the library files needed to run this application cannot be found.";
          break;
        #endif
        #ifdef ERROR_BAD_USERNAME
        case ERROR_BAD_USERNAME:
          errorname = "ERROR_BAD_USERNAME";
          errormsg = "The specified username is invalid.";
          break;
        #endif
        #ifdef ERROR_NOT_CONNECTED
        case ERROR_NOT_CONNECTED:
          errorname = "ERROR_NOT_CONNECTED";
          errormsg = "This network connection does not exist.";
          break;
        #endif
        #ifdef ERROR_OPEN_FILES
        case ERROR_OPEN_FILES:
          errorname = "ERROR_OPEN_FILES";
          errormsg = "This network connection has files open or requests pending.";
          break;
        #endif
        #ifdef ERROR_ACTIVE_CONNECTIONS
        case ERROR_ACTIVE_CONNECTIONS:
          errorname = "ERROR_ACTIVE_CONNECTIONS";
          errormsg = "Active connections still exist.";
          break;
        #endif
        #ifdef ERROR_DEVICE_IN_USE
        case ERROR_DEVICE_IN_USE:
          errorname = "ERROR_DEVICE_IN_USE";
          errormsg = "The device is in use by an active process and cannot be disconnected.";
          break;
        #endif
        #ifdef ERROR_BAD_DEVICE
        case ERROR_BAD_DEVICE:
          errorname = "ERROR_BAD_DEVICE";
          errormsg = "The specified device name is invalid.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_UNAVAIL
        case ERROR_CONNECTION_UNAVAIL:
          errorname = "ERROR_CONNECTION_UNAVAIL";
          errormsg = "The device is not currently connected but it is a remembered connection.";
          break;
        #endif
        #ifdef ERROR_DEVICE_ALREADY_REMEMBERED
        case ERROR_DEVICE_ALREADY_REMEMBERED:
          errorname = "ERROR_DEVICE_ALREADY_REMEMBERED";
          errormsg = "An attempt was made to remember a device that had previously been remembered.";
          break;
        #endif
        #ifdef ERROR_NO_NET_OR_BAD_PATH
        case ERROR_NO_NET_OR_BAD_PATH:
          errorname = "ERROR_NO_NET_OR_BAD_PATH";
          errormsg = "No network provider accepted the given network path.";
          break;
        #endif
        #ifdef ERROR_BAD_PROVIDER
        case ERROR_BAD_PROVIDER:
          errorname = "ERROR_BAD_PROVIDER";
          errormsg = "The specified network provider name is invalid.";
          break;
        #endif
        #ifdef ERROR_CANNOT_OPEN_PROFILE
        case ERROR_CANNOT_OPEN_PROFILE:
          errorname = "ERROR_CANNOT_OPEN_PROFILE";
          errormsg = "Unable to open the network connection profile.";
          break;
        #endif
        #ifdef ERROR_BAD_PROFILE
        case ERROR_BAD_PROFILE:
          errorname = "ERROR_BAD_PROFILE";
          errormsg = "The network connection profile is corrupt.";
          break;
        #endif
        #ifdef ERROR_NOT_CONTAINER
        case ERROR_NOT_CONTAINER:
          errorname = "ERROR_NOT_CONTAINER";
          errormsg = "Cannot enumerate a non-container.";
          break;
        #endif
        #ifdef ERROR_EXTENDED_ERROR
        case ERROR_EXTENDED_ERROR:
          errorname = "ERROR_EXTENDED_ERROR";
          errormsg = "An extended error has occurred.";
          break;
        #endif
        #ifdef ERROR_INVALID_GROUPNAME
        case ERROR_INVALID_GROUPNAME:
          errorname = "ERROR_INVALID_GROUPNAME";
          errormsg = "The format of the specified group name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_COMPUTERNAME
        case ERROR_INVALID_COMPUTERNAME:
          errorname = "ERROR_INVALID_COMPUTERNAME";
          errormsg = "The format of the specified computer name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_EVENTNAME
        case ERROR_INVALID_EVENTNAME:
          errorname = "ERROR_INVALID_EVENTNAME";
          errormsg = "The format of the specified event name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_DOMAINNAME
        case ERROR_INVALID_DOMAINNAME:
          errorname = "ERROR_INVALID_DOMAINNAME";
          errormsg = "The format of the specified domain name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_SERVICENAME
        case ERROR_INVALID_SERVICENAME:
          errorname = "ERROR_INVALID_SERVICENAME";
          errormsg = "The format of the specified service name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_NETNAME
        case ERROR_INVALID_NETNAME:
          errorname = "ERROR_INVALID_NETNAME";
          errormsg = "The format of the specified network name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_SHARENAME
        case ERROR_INVALID_SHARENAME:
          errorname = "ERROR_INVALID_SHARENAME";
          errormsg = "The format of the specified share name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_PASSWORDNAME
        case ERROR_INVALID_PASSWORDNAME:
          errorname = "ERROR_INVALID_PASSWORDNAME";
          errormsg = "The format of the specified password is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_MESSAGENAME
        case ERROR_INVALID_MESSAGENAME:
          errorname = "ERROR_INVALID_MESSAGENAME";
          errormsg = "The format of the specified message name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_MESSAGEDEST
        case ERROR_INVALID_MESSAGEDEST:
          errorname = "ERROR_INVALID_MESSAGEDEST";
          errormsg = "The format of the specified message destination is invalid.";
          break;
        #endif
        #ifdef ERROR_SESSION_CREDENTIAL_CONFLICT
        case ERROR_SESSION_CREDENTIAL_CONFLICT:
          errorname = "ERROR_SESSION_CREDENTIAL_CONFLICT";
          errormsg = "The credentials supplied conflict with an existing set of credentials.";
          break;
        #endif
        #ifdef ERROR_REMOTE_SESSION_LIMIT_EXCEEDED
        case ERROR_REMOTE_SESSION_LIMIT_EXCEEDED:
          errorname = "ERROR_REMOTE_SESSION_LIMIT_EXCEEDED";
          errormsg = "An attempt was made to establish a session to a network server, but there are already too many sessions established to that server.";
          break;
        #endif
        #ifdef ERROR_DUP_DOMAINNAME
        case ERROR_DUP_DOMAINNAME:
          errorname = "ERROR_DUP_DOMAINNAME";
          errormsg = "The workgroup or domain name is already in use by another computer on the network.";
          break;
        #endif
        #ifdef ERROR_NO_NETWORK
        case ERROR_NO_NETWORK:
          errorname = "ERROR_NO_NETWORK";
          errormsg = "The network is not present or not started.";
          break;
        #endif
        #ifdef ERROR_CANCELLED
        case ERROR_CANCELLED:
          errorname = "ERROR_CANCELLED";
          errormsg = "The operation was cancelled by the user.";
          break;
        #endif
        #ifdef ERROR_USER_MAPPED_FILE
        case ERROR_USER_MAPPED_FILE:
          errorname = "ERROR_USER_MAPPED_FILE";
          errormsg = "The requested operation cannot be performed on a file with a user mapped section open.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_REFUSED
        case ERROR_CONNECTION_REFUSED:
          errorname = "ERROR_CONNECTION_REFUSED";
          errormsg = "The remote system refused the network connection.";
          break;
        #endif
        #ifdef ERROR_GRACEFUL_DISCONNECT
        case ERROR_GRACEFUL_DISCONNECT:
          errorname = "ERROR_GRACEFUL_DISCONNECT";
          errormsg = "The network connection was gracefully closed.";
          break;
        #endif
        #ifdef ERROR_ADDRESS_ALREADY_ASSOCIATED
        case ERROR_ADDRESS_ALREADY_ASSOCIATED:
          errorname = "ERROR_ADDRESS_ALREADY_ASSOCIATED";
          errormsg = "The network transport endpoint already has an address associated with it.";
          break;
        #endif
        #ifdef ERROR_ADDRESS_NOT_ASSOCIATED
        case ERROR_ADDRESS_NOT_ASSOCIATED:
          errorname = "ERROR_ADDRESS_NOT_ASSOCIATED";
          errormsg = "An address has not yet been associated with the network endpoint.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_INVALID
        case ERROR_CONNECTION_INVALID:
          errorname = "ERROR_CONNECTION_INVALID";
          errormsg = "An operation was attempted on a non-existent network connection.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_ACTIVE
        case ERROR_CONNECTION_ACTIVE:
          errorname = "ERROR_CONNECTION_ACTIVE";
          errormsg = "An invalid operation was attempted on an active network connection.";
          break;
        #endif
        #ifdef ERROR_NETWORK_UNREACHABLE
        case ERROR_NETWORK_UNREACHABLE:
          errorname = "ERROR_NETWORK_UNREACHABLE";
          errormsg = "The remote network is not reachable by the transport.";
          break;
        #endif
        #ifdef ERROR_HOST_UNREACHABLE
        case ERROR_HOST_UNREACHABLE:
          errorname = "ERROR_HOST_UNREACHABLE";
          errormsg = "The remote system is not reachable by the transport.";
          break;
        #endif
        #ifdef ERROR_PROTOCOL_UNREACHABLE
        case ERROR_PROTOCOL_UNREACHABLE:
          errorname = "ERROR_PROTOCOL_UNREACHABLE";
          errormsg = "The remote system does not support the transport protocol.";
          break;
        #endif
        #ifdef ERROR_PORT_UNREACHABLE
        case ERROR_PORT_UNREACHABLE:
          errorname = "ERROR_PORT_UNREACHABLE";
          errormsg = "No service is operating at the destination network endpoint on the remote system.";
          break;
        #endif
        #ifdef ERROR_REQUEST_ABORTED
        case ERROR_REQUEST_ABORTED:
          errorname = "ERROR_REQUEST_ABORTED";
          errormsg = "The request was aborted.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_ABORTED
        case ERROR_CONNECTION_ABORTED:
          errorname = "ERROR_CONNECTION_ABORTED";
          errormsg = "The network connection was aborted by the local system.";
          break;
        #endif
        #ifdef ERROR_RETRY
        case ERROR_RETRY:
          errorname = "ERROR_RETRY";
          errormsg = "The operation could not be completed. A retry should be performed.";
          break;
        #endif
        #ifdef ERROR_CONNECTION_COUNT_LIMIT
        case ERROR_CONNECTION_COUNT_LIMIT:
          errorname = "ERROR_CONNECTION_COUNT_LIMIT";
          errormsg = "A connection to the server could not be made because the limit on the number of concurrent connections for this account has been reached.";
          break;
        #endif
        #ifdef ERROR_LOGIN_TIME_RESTRICTION
        case ERROR_LOGIN_TIME_RESTRICTION:
          errorname = "ERROR_LOGIN_TIME_RESTRICTION";
          errormsg = "Attempting to login during an unauthorized time of day for this account.";
          break;
        #endif
        #ifdef ERROR_LOGIN_WKSTA_RESTRICTION
        case ERROR_LOGIN_WKSTA_RESTRICTION:
          errorname = "ERROR_LOGIN_WKSTA_RESTRICTION";
          errormsg = "The account is not authorized to login from this station.";
          break;
        #endif
        #ifdef ERROR_INCORRECT_ADDRESS
        case ERROR_INCORRECT_ADDRESS:
          errorname = "ERROR_INCORRECT_ADDRESS";
          errormsg = "The network address could not be used for the operation requested.";
          break;
        #endif
        #ifdef ERROR_ALREADY_REGISTERED
        case ERROR_ALREADY_REGISTERED:
          errorname = "ERROR_ALREADY_REGISTERED";
          errormsg = "The service is already registered.";
          break;
        #endif
        #ifdef ERROR_SERVICE_NOT_FOUND
        case ERROR_SERVICE_NOT_FOUND:
          errorname = "ERROR_SERVICE_NOT_FOUND";
          errormsg = "The specified service does not exist.";
          break;
        #endif
        #ifdef ERROR_NOT_AUTHENTICATED
        case ERROR_NOT_AUTHENTICATED:
          errorname = "ERROR_NOT_AUTHENTICATED";
          errormsg = "The operation being requested was not performed because the user has not been authenticated.";
          break;
        #endif
        #ifdef ERROR_NOT_LOGGED_ON
        case ERROR_NOT_LOGGED_ON:
          errorname = "ERROR_NOT_LOGGED_ON";
          errormsg = "The operation being requested was not performed because the user has not logged on to the network. The specified service does not exist.";
          break;
        #endif
        #ifdef ERROR_CONTINUE
        case ERROR_CONTINUE:
          errorname = "ERROR_CONTINUE";
          errormsg = "Return that wants caller to continue with work in progress.";
          break;
        #endif
        #ifdef ERROR_ALREADY_INITIALIZED
        case ERROR_ALREADY_INITIALIZED:
          errorname = "ERROR_ALREADY_INITIALIZED";
          errormsg = "An attempt was made to perform an initialization operation when initialization has already been completed.";
          break;
        #endif
        #ifdef ERROR_NO_MORE_DEVICES
        case ERROR_NO_MORE_DEVICES:
          errorname = "ERROR_NO_MORE_DEVICES";
          errormsg = "No more local devices.";
          break;
        #endif
        #ifdef ERROR_NOT_ALL_ASSIGNED
        case ERROR_NOT_ALL_ASSIGNED:
          errorname = "ERROR_NOT_ALL_ASSIGNED";
          errormsg = "Not all privileges referenced are assigned to the caller.";
          break;
        #endif
        #ifdef ERROR_SOME_NOT_MAPPED
        case ERROR_SOME_NOT_MAPPED:
          errorname = "ERROR_SOME_NOT_MAPPED";
          errormsg = "Some mapping between account names and security IDs was not done.";
          break;
        #endif
        #ifdef ERROR_NO_QUOTAS_FOR_ACCOUNT
        case ERROR_NO_QUOTAS_FOR_ACCOUNT:
          errorname = "ERROR_NO_QUOTAS_FOR_ACCOUNT";
          errormsg = "No system quota limits are specifically set for this account.";
          break;
        #endif
        #ifdef ERROR_LOCAL_USER_SESSION_KEY
        case ERROR_LOCAL_USER_SESSION_KEY:
          errorname = "ERROR_LOCAL_USER_SESSION_KEY";
          errormsg = "No encryption key is available. A well-known encryption key was returned.";
          break;
        #endif
        #ifdef ERROR_NULL_LM_PASSWORD
        case ERROR_NULL_LM_PASSWORD:
          errorname = "ERROR_NULL_LM_PASSWORD";
          errormsg = "The NT password is too complex to be converted to a LAN Manager password. The LAN Manager password returned is a NULL string.";
          break;
        #endif
        #ifdef ERROR_UNKNOWN_REVISION
        case ERROR_UNKNOWN_REVISION:
          errorname = "ERROR_UNKNOWN_REVISION";
          errormsg = "The revision level is unknown.";
          break;
        #endif
        #ifdef ERROR_REVISION_MISMATCH
        case ERROR_REVISION_MISMATCH:
          errorname = "ERROR_REVISION_MISMATCH";
          errormsg = "Indicates two revision levels are incompatible.";
          break;
        #endif
        #ifdef ERROR_INVALID_OWNER
        case ERROR_INVALID_OWNER:
          errorname = "ERROR_INVALID_OWNER";
          errormsg = "This security ID may not be assigned as the owner of this object.";
          break;
        #endif
        #ifdef ERROR_INVALID_PRIMARY_GROUP
        case ERROR_INVALID_PRIMARY_GROUP:
          errorname = "ERROR_INVALID_PRIMARY_GROUP";
          errormsg = "This security ID may not be assigned as the primary group of an object.";
          break;
        #endif
        #ifdef ERROR_NO_IMPERSONATION_TOKEN
        case ERROR_NO_IMPERSONATION_TOKEN:
          errorname = "ERROR_NO_IMPERSONATION_TOKEN";
          errormsg = "An attempt has been made to operate on an impersonation token by a thread that is not currently impersonating a client.";
          break;
        #endif
        #ifdef ERROR_CANT_DISABLE_MANDATORY
        case ERROR_CANT_DISABLE_MANDATORY:
          errorname = "ERROR_CANT_DISABLE_MANDATORY";
          errormsg = "The group may not be disabled.";
          break;
        #endif
        #ifdef ERROR_NO_LOGON_SERVERS
        case ERROR_NO_LOGON_SERVERS:
          errorname = "ERROR_NO_LOGON_SERVERS";
          errormsg = "There are currently no logon servers available to service the logon request.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_LOGON_SESSION
        case ERROR_NO_SUCH_LOGON_SESSION:
          errorname = "ERROR_NO_SUCH_LOGON_SESSION";
          errormsg = "A specified logon session does not exist. It may already have been terminated.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_PRIVILEGE
        case ERROR_NO_SUCH_PRIVILEGE:
          errorname = "ERROR_NO_SUCH_PRIVILEGE";
          errormsg = "A specified privilege does not exist.";
          break;
        #endif
        #ifdef ERROR_PRIVILEGE_NOT_HELD
        case ERROR_PRIVILEGE_NOT_HELD:
          errorname = "ERROR_PRIVILEGE_NOT_HELD";
          errormsg = "A required privilege is not held by the client.";
          break;
        #endif
        #ifdef ERROR_INVALID_ACCOUNT_NAME
        case ERROR_INVALID_ACCOUNT_NAME:
          errorname = "ERROR_INVALID_ACCOUNT_NAME";
          errormsg = "The name provided is not a properly formed account name.";
          break;
        #endif
        #ifdef ERROR_USER_EXISTS
        case ERROR_USER_EXISTS:
          errorname = "ERROR_USER_EXISTS";
          errormsg = "The specified user already exists.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_USER
        case ERROR_NO_SUCH_USER:
          errorname = "ERROR_NO_SUCH_USER";
          errormsg = "The specified user does not exist.";
          break;
        #endif
        #ifdef ERROR_GROUP_EXISTS
        case ERROR_GROUP_EXISTS:
          errorname = "ERROR_GROUP_EXISTS";
          errormsg = "The specified group already exists.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_GROUP
        case ERROR_NO_SUCH_GROUP:
          errorname = "ERROR_NO_SUCH_GROUP";
          errormsg = "The specified group does not exist.";
          break;
        #endif
        #ifdef ERROR_MEMBER_IN_GROUP
        case ERROR_MEMBER_IN_GROUP:
          errorname = "ERROR_MEMBER_IN_GROUP";
          errormsg = "Either the specified user account is already a member of the specified group, or the specified group cannot be deleted because it contains a member.";
          break;
        #endif
        #ifdef ERROR_MEMBER_NOT_IN_GROUP
        case ERROR_MEMBER_NOT_IN_GROUP:
          errorname = "ERROR_MEMBER_NOT_IN_GROUP";
          errormsg = "The specified user account is not a member of the specified group account.";
          break;
        #endif
        #ifdef ERROR_LAST_ADMIN
        case ERROR_LAST_ADMIN:
          errorname = "ERROR_LAST_ADMIN";
          errormsg = "The last remaining administration account cannot be disabled or deleted.";
          break;
        #endif
        #ifdef ERROR_WRONG_PASSWORD
        case ERROR_WRONG_PASSWORD:
          errorname = "ERROR_WRONG_PASSWORD";
          errormsg = "Unable to update the password. The value provided as the current password is incorrect.";
          break;
        #endif
        #ifdef ERROR_ILL_FORMED_PASSWORD
        case ERROR_ILL_FORMED_PASSWORD:
          errorname = "ERROR_ILL_FORMED_PASSWORD";
          errormsg = "Unable to update the password. The value provided for the new password contains values that are not allowed in passwords.";
          break;
        #endif
        #ifdef ERROR_PASSWORD_RESTRICTION
        case ERROR_PASSWORD_RESTRICTION:
          errorname = "ERROR_PASSWORD_RESTRICTION";
          errormsg = "Unable to update the password because a password update rule has been violated.";
          break;
        #endif
        #ifdef ERROR_LOGON_FAILURE
        case ERROR_LOGON_FAILURE:
          errorname = "ERROR_LOGON_FAILURE";
          errormsg = "Logon failure: unknown user name or bad password.";
          break;
        #endif
        #ifdef ERROR_ACCOUNT_RESTRICTION
        case ERROR_ACCOUNT_RESTRICTION:
          errorname = "ERROR_ACCOUNT_RESTRICTION";
          errormsg = "Logon failure: user account restriction.";
          break;
        #endif
        #ifdef ERROR_INVALID_LOGON_HOURS
        case ERROR_INVALID_LOGON_HOURS:
          errorname = "ERROR_INVALID_LOGON_HOURS";
          errormsg = "Logon failure: account logon time restriction violation.";
          break;
        #endif
        #ifdef ERROR_INVALID_WORKSTATION
        case ERROR_INVALID_WORKSTATION:
          errorname = "ERROR_INVALID_WORKSTATION";
          errormsg = "Logon failure: user not allowed to log on to this computer.";
          break;
        #endif
        #ifdef ERROR_PASSWORD_EXPIRED
        case ERROR_PASSWORD_EXPIRED:
          errorname = "ERROR_PASSWORD_EXPIRED";
          errormsg = "Logon failure: the specified account password has expired.";
          break;
        #endif
        #ifdef ERROR_ACCOUNT_DISABLED
        case ERROR_ACCOUNT_DISABLED:
          errorname = "ERROR_ACCOUNT_DISABLED";
          errormsg = "Logon failure: account currently disabled.";
          break;
        #endif
        #ifdef ERROR_NONE_MAPPED
        case ERROR_NONE_MAPPED:
          errorname = "ERROR_NONE_MAPPED";
          errormsg = "No mapping between account names and security IDs was done.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_LUIDS_REQUESTED
        case ERROR_TOO_MANY_LUIDS_REQUESTED:
          errorname = "ERROR_TOO_MANY_LUIDS_REQUESTED";
          errormsg = "Too many local user identifiers (LUIDs) were requested at one time.";
          break;
        #endif
        #ifdef ERROR_LUIDS_EXHAUSTED
        case ERROR_LUIDS_EXHAUSTED:
          errorname = "ERROR_LUIDS_EXHAUSTED";
          errormsg = "No more local user identifiers (LUIDs) are available.";
          break;
        #endif
        #ifdef ERROR_INVALID_SUB_AUTHORITY
        case ERROR_INVALID_SUB_AUTHORITY:
          errorname = "ERROR_INVALID_SUB_AUTHORITY";
          errormsg = "The subauthority part of a security ID is invalid for this particular use.";
          break;
        #endif
        #ifdef ERROR_INVALID_ACL
        case ERROR_INVALID_ACL:
          errorname = "ERROR_INVALID_ACL";
          errormsg = "The access control list (ACL) structure is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_SID
        case ERROR_INVALID_SID:
          errorname = "ERROR_INVALID_SID";
          errormsg = "The security ID structure is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_SECURITY_DESCR
        case ERROR_INVALID_SECURITY_DESCR:
          errorname = "ERROR_INVALID_SECURITY_DESCR";
          errormsg = "The security descriptor structure is invalid.";
          break;
        #endif
        #ifdef ERROR_BAD_INHERITANCE_ACL
        case ERROR_BAD_INHERITANCE_ACL:
          errorname = "ERROR_BAD_INHERITANCE_ACL";
          errormsg = "The inherited access control list (ACL) or access control entry (ACE) could not be built.";
          break;
        #endif
        #ifdef ERROR_SERVER_DISABLED
        case ERROR_SERVER_DISABLED:
          errorname = "ERROR_SERVER_DISABLED";
          errormsg = "The server is currently disabled.";
          break;
        #endif
        #ifdef ERROR_SERVER_NOT_DISABLED
        case ERROR_SERVER_NOT_DISABLED:
          errorname = "ERROR_SERVER_NOT_DISABLED";
          errormsg = "The server is currently enabled.";
          break;
        #endif
        #ifdef ERROR_INVALID_ID_AUTHORITY
        case ERROR_INVALID_ID_AUTHORITY:
          errorname = "ERROR_INVALID_ID_AUTHORITY";
          errormsg = "The value provided was an invalid value for an identifier authority.";
          break;
        #endif
        #ifdef ERROR_ALLOTTED_SPACE_EXCEEDED
        case ERROR_ALLOTTED_SPACE_EXCEEDED:
          errorname = "ERROR_ALLOTTED_SPACE_EXCEEDED";
          errormsg = "No more memory is available for security information updates.";
          break;
        #endif
        #ifdef ERROR_INVALID_GROUP_ATTRIBUTES
        case ERROR_INVALID_GROUP_ATTRIBUTES:
          errorname = "ERROR_INVALID_GROUP_ATTRIBUTES";
          errormsg = "The specified attributes are invalid, or incompatible with the attributes for the group as a whole.";
          break;
        #endif
        #ifdef ERROR_BAD_IMPERSONATION_LEVEL
        case ERROR_BAD_IMPERSONATION_LEVEL:
          errorname = "ERROR_BAD_IMPERSONATION_LEVEL";
          errormsg = "Either a required impersonation level was not provided, or the provided impersonation level is invalid.";
          break;
        #endif
        #ifdef ERROR_CANT_OPEN_ANONYMOUS
        case ERROR_CANT_OPEN_ANONYMOUS:
          errorname = "ERROR_CANT_OPEN_ANONYMOUS";
          errormsg = "Cannot open an anonymous level security token.";
          break;
        #endif
        #ifdef ERROR_BAD_VALIDATION_CLASS
        case ERROR_BAD_VALIDATION_CLASS:
          errorname = "ERROR_BAD_VALIDATION_CLASS";
          errormsg = "The validation information class requested was invalid.";
          break;
        #endif
        #ifdef ERROR_BAD_TOKEN_TYPE
        case ERROR_BAD_TOKEN_TYPE:
          errorname = "ERROR_BAD_TOKEN_TYPE";
          errormsg = "The type of the token is inappropriate for its attempted use.";
          break;
        #endif
        #ifdef ERROR_NO_SECURITY_ON_OBJECT
        case ERROR_NO_SECURITY_ON_OBJECT:
          errorname = "ERROR_NO_SECURITY_ON_OBJECT";
          errormsg = "Unable to perform a security operation on an object which has no associated security.";
          break;
        #endif
        #ifdef ERROR_CANT_ACCESS_DOMAIN_INFO
        case ERROR_CANT_ACCESS_DOMAIN_INFO:
          errorname = "ERROR_CANT_ACCESS_DOMAIN_INFO";
          errormsg = "Indicates a Windows NT Server could not be contacted or that objects within the domain are protected such that necessary information could not be retrieved.";
          break;
        #endif
        #ifdef ERROR_INVALID_SERVER_STATE
        case ERROR_INVALID_SERVER_STATE:
          errorname = "ERROR_INVALID_SERVER_STATE";
          errormsg = "The security account manager (SAM) or local security authority (LSA) server was in the wrong state to perform the security operation.";
          break;
        #endif
        #ifdef ERROR_INVALID_DOMAIN_STATE
        case ERROR_INVALID_DOMAIN_STATE:
          errorname = "ERROR_INVALID_DOMAIN_STATE";
          errormsg = "The domain was in the wrong state to perform the security operation.";
          break;
        #endif
        #ifdef ERROR_INVALID_DOMAIN_ROLE
        case ERROR_INVALID_DOMAIN_ROLE:
          errorname = "ERROR_INVALID_DOMAIN_ROLE";
          errormsg = "This operation is only allowed for the Primary Domain Controller of the domain.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_DOMAIN
        case ERROR_NO_SUCH_DOMAIN:
          errorname = "ERROR_NO_SUCH_DOMAIN";
          errormsg = "The specified domain did not exist.";
          break;
        #endif
        #ifdef ERROR_DOMAIN_EXISTS
        case ERROR_DOMAIN_EXISTS:
          errorname = "ERROR_DOMAIN_EXISTS";
          errormsg = "The specified domain already exists.";
          break;
        #endif
        #ifdef ERROR_DOMAIN_LIMIT_EXCEEDED
        case ERROR_DOMAIN_LIMIT_EXCEEDED:
          errorname = "ERROR_DOMAIN_LIMIT_EXCEEDED";
          errormsg = "An attempt was made to exceed the limit on the number of domains per server.";
          break;
        #endif
        #ifdef ERROR_INTERNAL_DB_CORRUPTION
        case ERROR_INTERNAL_DB_CORRUPTION:
          errorname = "ERROR_INTERNAL_DB_CORRUPTION";
          errormsg = "Unable to complete the requested operation because of either a catastrophic media failure or a data structure corruption on the disk.";
          break;
        #endif
        #ifdef ERROR_INTERNAL_ERROR
        case ERROR_INTERNAL_ERROR:
          errorname = "ERROR_INTERNAL_ERROR";
          errormsg = "The security account database contains an internal inconsistency.";
          break;
        #endif
        #ifdef ERROR_GENERIC_NOT_MAPPED
        case ERROR_GENERIC_NOT_MAPPED:
          errorname = "ERROR_GENERIC_NOT_MAPPED";
          errormsg = "Generic access types were contained in an access mask which should already be mapped to non-generic types.";
          break;
        #endif
        #ifdef ERROR_BAD_DESCRIPTOR_FORMAT
        case ERROR_BAD_DESCRIPTOR_FORMAT:
          errorname = "ERROR_BAD_DESCRIPTOR_FORMAT";
          errormsg = "A security descriptor is not in the right format (absolute or self-relative).";
          break;
        #endif
        #ifdef ERROR_NOT_LOGON_PROCESS
        case ERROR_NOT_LOGON_PROCESS:
          errorname = "ERROR_NOT_LOGON_PROCESS";
          errormsg = "The requested action is restricted for use by logon processes only. The calling process has not registered as a logon process.";
          break;
        #endif
        #ifdef ERROR_LOGON_SESSION_EXISTS
        case ERROR_LOGON_SESSION_EXISTS:
          errorname = "ERROR_LOGON_SESSION_EXISTS";
          errormsg = "Cannot start a new logon session with an ID that is already in use.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_PACKAGE
        case ERROR_NO_SUCH_PACKAGE:
          errorname = "ERROR_NO_SUCH_PACKAGE";
          errormsg = "A specified authentication package is unknown.";
          break;
        #endif
        #ifdef ERROR_BAD_LOGON_SESSION_STATE
        case ERROR_BAD_LOGON_SESSION_STATE:
          errorname = "ERROR_BAD_LOGON_SESSION_STATE";
          errormsg = "The logon session is not in a state that is consistent with the requested operation.";
          break;
        #endif
        #ifdef ERROR_LOGON_SESSION_COLLISION
        case ERROR_LOGON_SESSION_COLLISION:
          errorname = "ERROR_LOGON_SESSION_COLLISION";
          errormsg = "The logon session ID is already in use.";
          break;
        #endif
        #ifdef ERROR_INVALID_LOGON_TYPE
        case ERROR_INVALID_LOGON_TYPE:
          errorname = "ERROR_INVALID_LOGON_TYPE";
          errormsg = "A logon request contained an invalid logon type value.";
          break;
        #endif
        #ifdef ERROR_CANNOT_IMPERSONATE
        case ERROR_CANNOT_IMPERSONATE:
          errorname = "ERROR_CANNOT_IMPERSONATE";
          errormsg = "Unable to impersonate via a named pipe until data has been read from that pipe.";
          break;
        #endif
        #ifdef ERROR_RXACT_INVALID_STATE
        case ERROR_RXACT_INVALID_STATE:
          errorname = "ERROR_RXACT_INVALID_STATE";
          errormsg = "The transaction state of a Registry subtree is incompatible with the requested operation.";
          break;
        #endif
        #ifdef ERROR_RXACT_COMMIT_FAILURE
        case ERROR_RXACT_COMMIT_FAILURE:
          errorname = "ERROR_RXACT_COMMIT_FAILURE";
          errormsg = "An internal security database corruption has been encountered.";
          break;
        #endif
        #ifdef ERROR_SPECIAL_ACCOUNT
        case ERROR_SPECIAL_ACCOUNT:
          errorname = "ERROR_SPECIAL_ACCOUNT";
          errormsg = "Cannot perform this operation on built-in accounts.";
          break;
        #endif
        #ifdef ERROR_SPECIAL_GROUP
        case ERROR_SPECIAL_GROUP:
          errorname = "ERROR_SPECIAL_GROUP";
          errormsg = "Cannot perform this operation on this built-in special group.";
          break;
        #endif
        #ifdef ERROR_SPECIAL_USER
        case ERROR_SPECIAL_USER:
          errorname = "ERROR_SPECIAL_USER";
          errormsg = "Cannot perform this operation on this built-in special user.";
          break;
        #endif
        #ifdef ERROR_MEMBERS_PRIMARY_GROUP
        case ERROR_MEMBERS_PRIMARY_GROUP:
          errorname = "ERROR_MEMBERS_PRIMARY_GROUP";
          errormsg = "The user cannot be removed from a group because the group is currently the user's primary group.";
          break;
        #endif
        #ifdef ERROR_TOKEN_ALREADY_IN_USE
        case ERROR_TOKEN_ALREADY_IN_USE:
          errorname = "ERROR_TOKEN_ALREADY_IN_USE";
          errormsg = "The token is already in use as a primary token.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_ALIAS
        case ERROR_NO_SUCH_ALIAS:
          errorname = "ERROR_NO_SUCH_ALIAS";
          errormsg = "The specified local group does not exist.";
          break;
        #endif
        #ifdef ERROR_MEMBER_NOT_IN_ALIAS
        case ERROR_MEMBER_NOT_IN_ALIAS:
          errorname = "ERROR_MEMBER_NOT_IN_ALIAS";
          errormsg = "The specified account name is not a member of the local group.";
          break;
        #endif
        #ifdef ERROR_MEMBER_IN_ALIAS
        case ERROR_MEMBER_IN_ALIAS:
          errorname = "ERROR_MEMBER_IN_ALIAS";
          errormsg = "The specified account name is already a member of the local group.";
          break;
        #endif
        #ifdef ERROR_ALIAS_EXISTS
        case ERROR_ALIAS_EXISTS:
          errorname = "ERROR_ALIAS_EXISTS";
          errormsg = "The specified local group already exists.";
          break;
        #endif
        #ifdef ERROR_LOGON_NOT_GRANTED
        case ERROR_LOGON_NOT_GRANTED:
          errorname = "ERROR_LOGON_NOT_GRANTED";
          errormsg = "Logon failure: the user has not been granted the requested logon type at this computer.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_SECRETS
        case ERROR_TOO_MANY_SECRETS:
          errorname = "ERROR_TOO_MANY_SECRETS";
          errormsg = "The maximum number of secrets that may be stored in a single system has been exceeded.";
          break;
        #endif
        #ifdef ERROR_SECRET_TOO_LONG
        case ERROR_SECRET_TOO_LONG:
          errorname = "ERROR_SECRET_TOO_LONG";
          errormsg = "The length of a secret exceeds the maximum length allowed.";
          break;
        #endif
        #ifdef ERROR_INTERNAL_DB_ERROR
        case ERROR_INTERNAL_DB_ERROR:
          errorname = "ERROR_INTERNAL_DB_ERROR";
          errormsg = "The local security authority database contains an internal inconsistency.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_CONTEXT_IDS
        case ERROR_TOO_MANY_CONTEXT_IDS:
          errorname = "ERROR_TOO_MANY_CONTEXT_IDS";
          errormsg = "During a logon attempt, the user's security context accumulated too many security IDs.";
          break;
        #endif
        #ifdef ERROR_LOGON_TYPE_NOT_GRANTED
        case ERROR_LOGON_TYPE_NOT_GRANTED:
          errorname = "ERROR_LOGON_TYPE_NOT_GRANTED";
          errormsg = "Logon failure: the user has not been granted the requested logon type at this computer.";
          break;
        #endif
        #ifdef ERROR_NT_CROSS_ENCRYPTION_REQUIRED
        case ERROR_NT_CROSS_ENCRYPTION_REQUIRED:
          errorname = "ERROR_NT_CROSS_ENCRYPTION_REQUIRED";
          errormsg = "A cross-encrypted password is necessary to change a user password.";
          break;
        #endif
        #ifdef ERROR_NO_SUCH_MEMBER
        case ERROR_NO_SUCH_MEMBER:
          errorname = "ERROR_NO_SUCH_MEMBER";
          errormsg = "A new member could not be added to a local group because the member does not exist.";
          break;
        #endif
        #ifdef ERROR_INVALID_MEMBER
        case ERROR_INVALID_MEMBER:
          errorname = "ERROR_INVALID_MEMBER";
          errormsg = "A new member could not be added to a local group because the member has the wrong account type.";
          break;
        #endif
        #ifdef ERROR_TOO_MANY_SIDS
        case ERROR_TOO_MANY_SIDS:
          errorname = "ERROR_TOO_MANY_SIDS";
          errormsg = "Too many security IDs have been specified.";
          break;
        #endif
        #ifdef ERROR_LM_CROSS_ENCRYPTION_REQUIRED
        case ERROR_LM_CROSS_ENCRYPTION_REQUIRED:
          errorname = "ERROR_LM_CROSS_ENCRYPTION_REQUIRED";
          errormsg = "A cross-encrypted password is necessary to change this user password.";
          break;
        #endif
        #ifdef ERROR_NO_INHERITANCE
        case ERROR_NO_INHERITANCE:
          errorname = "ERROR_NO_INHERITANCE";
          errormsg = "Indicates an ACL contains no inheritable components.";
          break;
        #endif
        #ifdef ERROR_FILE_CORRUPT
        case ERROR_FILE_CORRUPT:
          errorname = "ERROR_FILE_CORRUPT";
          errormsg = "The file or directory is corrupt and non-readable.";
          break;
        #endif
        #ifdef ERROR_DISK_CORRUPT
        case ERROR_DISK_CORRUPT:
          errorname = "ERROR_DISK_CORRUPT";
          errormsg = "The disk structure is corrupt and non-readable.";
          break;
        #endif
        #ifdef ERROR_NO_USER_SESSION_KEY
        case ERROR_NO_USER_SESSION_KEY:
          errorname = "ERROR_NO_USER_SESSION_KEY";
          errormsg = "There is no user session key for the specified logon session.";
          break;
        #endif
        #ifdef ERROR_LICENSE_QUOTA_EXCEEDED
        case ERROR_LICENSE_QUOTA_EXCEEDED:
          errorname = "ERROR_LICENSE_QUOTA_EXCEEDED";
          errormsg = "The service being accessed is licensed for a particular number of connections. No more connections can be made to the service at this time because there are already as many connections as the service can accept.";
          break;
        #endif
        #ifdef ERROR_INVALID_WINDOW_HANDLE
        case ERROR_INVALID_WINDOW_HANDLE:
          errorname = "ERROR_INVALID_WINDOW_HANDLE";
          errormsg = "Invalid window handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_MENU_HANDLE
        case ERROR_INVALID_MENU_HANDLE:
          errorname = "ERROR_INVALID_MENU_HANDLE";
          errormsg = "Invalid menu handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_CURSOR_HANDLE
        case ERROR_INVALID_CURSOR_HANDLE:
          errorname = "ERROR_INVALID_CURSOR_HANDLE";
          errormsg = "Invalid cursor handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_ACCEL_HANDLE
        case ERROR_INVALID_ACCEL_HANDLE:
          errorname = "ERROR_INVALID_ACCEL_HANDLE";
          errormsg = "Invalid accelerator table handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_HOOK_HANDLE
        case ERROR_INVALID_HOOK_HANDLE:
          errorname = "ERROR_INVALID_HOOK_HANDLE";
          errormsg = "Invalid hook handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_DWP_HANDLE
        case ERROR_INVALID_DWP_HANDLE:
          errorname = "ERROR_INVALID_DWP_HANDLE";
          errormsg = "Invalid handle to a multiple-window position structure.";
          break;
        #endif
        #ifdef ERROR_TLW_WITH_WSCHILD
        case ERROR_TLW_WITH_WSCHILD:
          errorname = "ERROR_TLW_WITH_WSCHILD";
          errormsg = "Cannot create a top-level child window.";
          break;
        #endif
        #ifdef ERROR_CANNOT_FIND_WND_CLASS
        case ERROR_CANNOT_FIND_WND_CLASS:
          errorname = "ERROR_CANNOT_FIND_WND_CLASS";
          errormsg = "Cannot find window class.";
          break;
        #endif
        #ifdef ERROR_WINDOW_OF_OTHER_THREAD
        case ERROR_WINDOW_OF_OTHER_THREAD:
          errorname = "ERROR_WINDOW_OF_OTHER_THREAD";
          errormsg = "Invalid window, belongs to other thread.";
          break;
        #endif
        #ifdef ERROR_HOTKEY_ALREADY_REGISTERED
        case ERROR_HOTKEY_ALREADY_REGISTERED:
          errorname = "ERROR_HOTKEY_ALREADY_REGISTERED";
          errormsg = "Hot key is already registered.";
          break;
        #endif
        #ifdef ERROR_CLASS_ALREADY_EXISTS
        case ERROR_CLASS_ALREADY_EXISTS:
          errorname = "ERROR_CLASS_ALREADY_EXISTS";
          errormsg = "Class already exists.";
          break;
        #endif
        #ifdef ERROR_CLASS_DOES_NOT_EXIST
        case ERROR_CLASS_DOES_NOT_EXIST:
          errorname = "ERROR_CLASS_DOES_NOT_EXIST";
          errormsg = "Class does not exist.";
          break;
        #endif
        #ifdef ERROR_CLASS_HAS_WINDOWS
        case ERROR_CLASS_HAS_WINDOWS:
          errorname = "ERROR_CLASS_HAS_WINDOWS";
          errormsg = "Class still has open windows.";
          break;
        #endif
        #ifdef ERROR_INVALID_INDEX
        case ERROR_INVALID_INDEX:
          errorname = "ERROR_INVALID_INDEX";
          errormsg = "Invalid index.";
          break;
        #endif
        #ifdef ERROR_INVALID_ICON_HANDLE
        case ERROR_INVALID_ICON_HANDLE:
          errorname = "ERROR_INVALID_ICON_HANDLE";
          errormsg = "Invalid icon handle.";
          break;
        #endif
        #ifdef ERROR_PRIVATE_DIALOG_INDEX
        case ERROR_PRIVATE_DIALOG_INDEX:
          errorname = "ERROR_PRIVATE_DIALOG_INDEX";
          errormsg = "Using private DIALOG window words.";
          break;
        #endif
        #ifdef ERROR_LISTBOX_ID_NOT_FOUND
        case ERROR_LISTBOX_ID_NOT_FOUND:
          errorname = "ERROR_LISTBOX_ID_NOT_FOUND";
          errormsg = "The listbox identifier was not found.";
          break;
        #endif
        #ifdef ERROR_NO_WILDCARD_CHARACTERS
        case ERROR_NO_WILDCARD_CHARACTERS:
          errorname = "ERROR_NO_WILDCARD_CHARACTERS";
          errormsg = "No wildcards were found.";
          break;
        #endif
        #ifdef ERROR_CLIPBOARD_NOT_OPEN
        case ERROR_CLIPBOARD_NOT_OPEN:
          errorname = "ERROR_CLIPBOARD_NOT_OPEN";
          errormsg = "Thread does not have a clipboard open.";
          break;
        #endif
        #ifdef ERROR_HOTKEY_NOT_REGISTERED
        case ERROR_HOTKEY_NOT_REGISTERED:
          errorname = "ERROR_HOTKEY_NOT_REGISTERED";
          errormsg = "Hot key is not registered.";
          break;
        #endif
        #ifdef ERROR_WINDOW_NOT_DIALOG
        case ERROR_WINDOW_NOT_DIALOG:
          errorname = "ERROR_WINDOW_NOT_DIALOG";
          errormsg = "The window is not a valid dialog window.";
          break;
        #endif
        #ifdef ERROR_CONTROL_ID_NOT_FOUND
        case ERROR_CONTROL_ID_NOT_FOUND:
          errorname = "ERROR_CONTROL_ID_NOT_FOUND";
          errormsg = "Control ID not found.";
          break;
        #endif
        #ifdef ERROR_INVALID_COMBOBOX_MESSAGE
        case ERROR_INVALID_COMBOBOX_MESSAGE:
          errorname = "ERROR_INVALID_COMBOBOX_MESSAGE";
          errormsg = "Invalid message for a combo box because it does not have an edit control.";
          break;
        #endif
        #ifdef ERROR_WINDOW_NOT_COMBOBOX
        case ERROR_WINDOW_NOT_COMBOBOX:
          errorname = "ERROR_WINDOW_NOT_COMBOBOX";
          errormsg = "The window is not a combo box.";
          break;
        #endif
        #ifdef ERROR_INVALID_EDIT_HEIGHT
        case ERROR_INVALID_EDIT_HEIGHT:
          errorname = "ERROR_INVALID_EDIT_HEIGHT";
          errormsg = "Height must be less than 256.";
          break;
        #endif
        #ifdef ERROR_DC_NOT_FOUND
        case ERROR_DC_NOT_FOUND:
          errorname = "ERROR_DC_NOT_FOUND";
          errormsg = "Invalid device context (DC) handle.";
          break;
        #endif
        #ifdef ERROR_INVALID_HOOK_FILTER
        case ERROR_INVALID_HOOK_FILTER:
          errorname = "ERROR_INVALID_HOOK_FILTER";
          errormsg = "Invalid hook procedure type.";
          break;
        #endif
        #ifdef ERROR_INVALID_FILTER_PROC
        case ERROR_INVALID_FILTER_PROC:
          errorname = "ERROR_INVALID_FILTER_PROC";
          errormsg = "Invalid hook procedure.";
          break;
        #endif
        #ifdef ERROR_HOOK_NEEDS_HMOD
        case ERROR_HOOK_NEEDS_HMOD:
          errorname = "ERROR_HOOK_NEEDS_HMOD";
          errormsg = "Cannot set non-local hook without a module handle.";
          break;
        #endif
        #ifdef ERROR_GLOBAL_ONLY_HOOK
        case ERROR_GLOBAL_ONLY_HOOK:
          errorname = "ERROR_GLOBAL_ONLY_HOOK";
          errormsg = "This hook procedure can only be set globally.";
          break;
        #endif
        #ifdef ERROR_JOURNAL_HOOK_SET
        case ERROR_JOURNAL_HOOK_SET:
          errorname = "ERROR_JOURNAL_HOOK_SET";
          errormsg = "The journal hook procedure is already installed.";
          break;
        #endif
        #ifdef ERROR_HOOK_NOT_INSTALLED
        case ERROR_HOOK_NOT_INSTALLED:
          errorname = "ERROR_HOOK_NOT_INSTALLED";
          errormsg = "The hook procedure is not installed.";
          break;
        #endif
        #ifdef ERROR_INVALID_LB_MESSAGE
        case ERROR_INVALID_LB_MESSAGE:
          errorname = "ERROR_INVALID_LB_MESSAGE";
          errormsg = "Invalid message for single-selection listbox.";
          break;
        #endif
        #ifdef ERROR_SETCOUNT_ON_BAD_LB
        case ERROR_SETCOUNT_ON_BAD_LB:
          errorname = "ERROR_SETCOUNT_ON_BAD_LB";
          errormsg = "LB_SETCOUNT sent to non-lazy listbox.";
          break;
        #endif
        #ifdef ERROR_LB_WITHOUT_TABSTOPS
        case ERROR_LB_WITHOUT_TABSTOPS:
          errorname = "ERROR_LB_WITHOUT_TABSTOPS";
          errormsg = "This list box does not support tab stops.";
          break;
        #endif
        #ifdef ERROR_DESTROY_OBJECT_OF_OTHER_THREAD
        case ERROR_DESTROY_OBJECT_OF_OTHER_THREAD:
          errorname = "ERROR_DESTROY_OBJECT_OF_OTHER_THREAD";
          errormsg = "Cannot destroy object created by another thread.";
          break;
        #endif
        #ifdef ERROR_CHILD_WINDOW_MENU
        case ERROR_CHILD_WINDOW_MENU:
          errorname = "ERROR_CHILD_WINDOW_MENU";
          errormsg = "Child windows cannot have menus.";
          break;
        #endif
        #ifdef ERROR_NO_SYSTEM_MENU
        case ERROR_NO_SYSTEM_MENU:
          errorname = "ERROR_NO_SYSTEM_MENU";
          errormsg = "The window does not have a system menu.";
          break;
        #endif
        #ifdef ERROR_INVALID_MSGBOX_STYLE
        case ERROR_INVALID_MSGBOX_STYLE:
          errorname = "ERROR_INVALID_MSGBOX_STYLE";
          errormsg = "Invalid message box style.";
          break;
        #endif
        #ifdef ERROR_INVALID_SPI_VALUE
        case ERROR_INVALID_SPI_VALUE:
          errorname = "ERROR_INVALID_SPI_VALUE";
          errormsg = "Invalid system-wide (SPI_*) parameter.";
          break;
        #endif
        #ifdef ERROR_SCREEN_ALREADY_LOCKED
        case ERROR_SCREEN_ALREADY_LOCKED:
          errorname = "ERROR_SCREEN_ALREADY_LOCKED";
          errormsg = "Screen already locked.";
          break;
        #endif
        #ifdef ERROR_HWNDS_HAVE_DIFF_PARENT
        case ERROR_HWNDS_HAVE_DIFF_PARENT:
          errorname = "ERROR_HWNDS_HAVE_DIFF_PARENT";
          errormsg = "All handles to windows in a multiple-window position structure must have the same parent.";
          break;
        #endif
        #ifdef ERROR_NOT_CHILD_WINDOW
        case ERROR_NOT_CHILD_WINDOW:
          errorname = "ERROR_NOT_CHILD_WINDOW";
          errormsg = "The window is not a child window.";
          break;
        #endif
        #ifdef ERROR_INVALID_GW_COMMAND
        case ERROR_INVALID_GW_COMMAND:
          errorname = "ERROR_INVALID_GW_COMMAND";
          errormsg = "Invalid GW_* command.";
          break;
        #endif
        #ifdef ERROR_INVALID_THREAD_ID
        case ERROR_INVALID_THREAD_ID:
          errorname = "ERROR_INVALID_THREAD_ID";
          errormsg = "Invalid thread identifier.";
          break;
        #endif
        #ifdef ERROR_NON_MDICHILD_WINDOW
        case ERROR_NON_MDICHILD_WINDOW:
          errorname = "ERROR_NON_MDICHILD_WINDOW";
          errormsg = "Cannot process a message from a window that is not a multiple document interface (MDI) window.";
          break;
        #endif
        #ifdef ERROR_POPUP_ALREADY_ACTIVE
        case ERROR_POPUP_ALREADY_ACTIVE:
          errorname = "ERROR_POPUP_ALREADY_ACTIVE";
          errormsg = "Popup menu already active.";
          break;
        #endif
        #ifdef ERROR_NO_SCROLLBARS
        case ERROR_NO_SCROLLBARS:
          errorname = "ERROR_NO_SCROLLBARS";
          errormsg = "The window does not have scroll bars.";
          break;
        #endif
        #ifdef ERROR_INVALID_SCROLLBAR_RANGE
        case ERROR_INVALID_SCROLLBAR_RANGE:
          errorname = "ERROR_INVALID_SCROLLBAR_RANGE";
          errormsg = "Scroll bar range cannot be greater than 0x7FFF.";
          break;
        #endif
        #ifdef ERROR_INVALID_SHOWWIN_COMMAND
        case ERROR_INVALID_SHOWWIN_COMMAND:
          errorname = "ERROR_INVALID_SHOWWIN_COMMAND";
          errormsg = "Cannot show or remove the window in the way specified.";
          break;
        #endif
        #ifdef ERROR_NO_SYSTEM_RESOURCES
        case ERROR_NO_SYSTEM_RESOURCES:
          errorname = "ERROR_NO_SYSTEM_RESOURCES";
          errormsg = "Insufficient system resources exist to complete the requested service.";
          break;
        #endif
        #ifdef ERROR_NONPAGED_SYSTEM_RESOURCES
        case ERROR_NONPAGED_SYSTEM_RESOURCES:
          errorname = "ERROR_NONPAGED_SYSTEM_RESOURCES";
          errormsg = "Insufficient system resources exist to complete the requested service.";
          break;
        #endif
        #ifdef ERROR_PAGED_SYSTEM_RESOURCES
        case ERROR_PAGED_SYSTEM_RESOURCES:
          errorname = "ERROR_PAGED_SYSTEM_RESOURCES";
          errormsg = "Insufficient system resources exist to complete the requested service.";
          break;
        #endif
        #ifdef ERROR_WORKING_SET_QUOTA
        case ERROR_WORKING_SET_QUOTA:
          errorname = "ERROR_WORKING_SET_QUOTA";
          errormsg = "Insufficient quota to complete the requested service.";
          break;
        #endif
        #ifdef ERROR_PAGEFILE_QUOTA
        case ERROR_PAGEFILE_QUOTA:
          errorname = "ERROR_PAGEFILE_QUOTA";
          errormsg = "Insufficient quota to complete the requested service.";
          break;
        #endif
        #ifdef ERROR_COMMITMENT_LIMIT
        case ERROR_COMMITMENT_LIMIT:
          errorname = "ERROR_COMMITMENT_LIMIT";
          errormsg = "The paging file is too small for this operation to complete.";
          break;
        #endif
        #ifdef ERROR_MENU_ITEM_NOT_FOUND
        case ERROR_MENU_ITEM_NOT_FOUND:
          errorname = "ERROR_MENU_ITEM_NOT_FOUND";
          errormsg = "A menu item was not found.";
          break;
        #endif
        #ifdef ERROR_EVENTLOG_FILE_CORRUPT
        case ERROR_EVENTLOG_FILE_CORRUPT:
          errorname = "ERROR_EVENTLOG_FILE_CORRUPT";
          errormsg = "The event log file is corrupt.";
          break;
        #endif
        #ifdef ERROR_EVENTLOG_CANT_START
        case ERROR_EVENTLOG_CANT_START:
          errorname = "ERROR_EVENTLOG_CANT_START";
          errormsg = "No event log file could be opened, so the event logging service did not start.";
          break;
        #endif
        #ifdef ERROR_LOG_FILE_FULL
        case ERROR_LOG_FILE_FULL:
          errorname = "ERROR_LOG_FILE_FULL";
          errormsg = "The event log file is full.";
          break;
        #endif
        #ifdef ERROR_EVENTLOG_FILE_CHANGED
        case ERROR_EVENTLOG_FILE_CHANGED:
          errorname = "ERROR_EVENTLOG_FILE_CHANGED";
          errormsg = "The event log file has changed between reads.";
          break;
        #endif
        #ifdef RPC_S_INVALID_STRING_BINDING
        case RPC_S_INVALID_STRING_BINDING:
          errorname = "RPC_S_INVALID_STRING_BINDING";
          errormsg = "The string binding is invalid.";
          break;
        #endif
        #ifdef RPC_S_WRONG_KIND_OF_BINDING
        case RPC_S_WRONG_KIND_OF_BINDING:
          errorname = "RPC_S_WRONG_KIND_OF_BINDING";
          errormsg = "The binding handle is not the correct type.";
          break;
        #endif
        #ifdef RPC_S_INVALID_BINDING
        case RPC_S_INVALID_BINDING:
          errorname = "RPC_S_INVALID_BINDING";
          errormsg = "The binding handle is invalid.";
          break;
        #endif
        #ifdef RPC_S_PROTSEQ_NOT_SUPPORTED
        case RPC_S_PROTSEQ_NOT_SUPPORTED:
          errorname = "RPC_S_PROTSEQ_NOT_SUPPORTED";
          errormsg = "The RPC protocol sequence is not supported.";
          break;
        #endif
        #ifdef RPC_S_INVALID_RPC_PROTSEQ
        case RPC_S_INVALID_RPC_PROTSEQ:
          errorname = "RPC_S_INVALID_RPC_PROTSEQ";
          errormsg = "The RPC protocol sequence is invalid.";
          break;
        #endif
        #ifdef RPC_S_INVALID_STRING_UUID
        case RPC_S_INVALID_STRING_UUID:
          errorname = "RPC_S_INVALID_STRING_UUID";
          errormsg = "The string universal unique identifier (UUID) is invalid.";
          break;
        #endif
        #ifdef RPC_S_INVALID_ENDPOINT_FORMAT
        case RPC_S_INVALID_ENDPOINT_FORMAT:
          errorname = "RPC_S_INVALID_ENDPOINT_FORMAT";
          errormsg = "The endpoint format is invalid.";
          break;
        #endif
        #ifdef RPC_S_INVALID_NET_ADDR
        case RPC_S_INVALID_NET_ADDR:
          errorname = "RPC_S_INVALID_NET_ADDR";
          errormsg = "The network address is invalid.";
          break;
        #endif
        #ifdef RPC_S_NO_ENDPOINT_FOUND
        case RPC_S_NO_ENDPOINT_FOUND:
          errorname = "RPC_S_NO_ENDPOINT_FOUND";
          errormsg = "No endpoint was found.";
          break;
        #endif
        #ifdef RPC_S_INVALID_TIMEOUT
        case RPC_S_INVALID_TIMEOUT:
          errorname = "RPC_S_INVALID_TIMEOUT";
          errormsg = "The timeout value is invalid.";
          break;
        #endif
        #ifdef RPC_S_OBJECT_NOT_FOUND
        case RPC_S_OBJECT_NOT_FOUND:
          errorname = "RPC_S_OBJECT_NOT_FOUND";
          errormsg = "The object universal unique identifier (UUID) was not found.";
          break;
        #endif
        #ifdef RPC_S_ALREADY_REGISTERED
        case RPC_S_ALREADY_REGISTERED:
          errorname = "RPC_S_ALREADY_REGISTERED";
          errormsg = "The object universal unique identifier (UUID) has already been registered.";
          break;
        #endif
        #ifdef RPC_S_TYPE_ALREADY_REGISTERED
        case RPC_S_TYPE_ALREADY_REGISTERED:
          errorname = "RPC_S_TYPE_ALREADY_REGISTERED";
          errormsg = "The type universal unique identifier (UUID) has already been registered.";
          break;
        #endif
        #ifdef RPC_S_ALREADY_LISTENING
        case RPC_S_ALREADY_LISTENING:
          errorname = "RPC_S_ALREADY_LISTENING";
          errormsg = "The RPC server is already listening.";
          break;
        #endif
        #ifdef RPC_S_NO_PROTSEQS_REGISTERED
        case RPC_S_NO_PROTSEQS_REGISTERED:
          errorname = "RPC_S_NO_PROTSEQS_REGISTERED";
          errormsg = "No protocol sequences have been registered.";
          break;
        #endif
        #ifdef RPC_S_NOT_LISTENING
        case RPC_S_NOT_LISTENING:
          errorname = "RPC_S_NOT_LISTENING";
          errormsg = "The RPC server is not listening.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_MGR_TYPE
        case RPC_S_UNKNOWN_MGR_TYPE:
          errorname = "RPC_S_UNKNOWN_MGR_TYPE";
          errormsg = "The manager type is unknown.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_IF
        case RPC_S_UNKNOWN_IF:
          errorname = "RPC_S_UNKNOWN_IF";
          errormsg = "The interface is unknown.";
          break;
        #endif
        #ifdef RPC_S_NO_BINDINGS
        case RPC_S_NO_BINDINGS:
          errorname = "RPC_S_NO_BINDINGS";
          errormsg = "There are no bindings.";
          break;
        #endif
        #ifdef RPC_S_NO_PROTSEQS
        case RPC_S_NO_PROTSEQS:
          errorname = "RPC_S_NO_PROTSEQS";
          errormsg = "There are no protocol sequences.";
          break;
        #endif
        #ifdef RPC_S_CANT_CREATE_ENDPOINT
        case RPC_S_CANT_CREATE_ENDPOINT:
          errorname = "RPC_S_CANT_CREATE_ENDPOINT";
          errormsg = "The endpoint cannot be created.";
          break;
        #endif
        #ifdef RPC_S_OUT_OF_RESOURCES
        case RPC_S_OUT_OF_RESOURCES:
          errorname = "RPC_S_OUT_OF_RESOURCES";
          errormsg = "Not enough resources are available to complete this operation.";
          break;
        #endif
        #ifdef RPC_S_SERVER_UNAVAILABLE
        case RPC_S_SERVER_UNAVAILABLE:
          errorname = "RPC_S_SERVER_UNAVAILABLE";
          errormsg = "The RPC server is unavailable.";
          break;
        #endif
        #ifdef RPC_S_SERVER_TOO_BUSY
        case RPC_S_SERVER_TOO_BUSY:
          errorname = "RPC_S_SERVER_TOO_BUSY";
          errormsg = "The RPC server is too busy to complete this operation.";
          break;
        #endif
        #ifdef RPC_S_INVALID_NETWORK_OPTIONS
        case RPC_S_INVALID_NETWORK_OPTIONS:
          errorname = "RPC_S_INVALID_NETWORK_OPTIONS";
          errormsg = "The network options are invalid.";
          break;
        #endif
        #ifdef RPC_S_NO_CALL_ACTIVE
        case RPC_S_NO_CALL_ACTIVE:
          errorname = "RPC_S_NO_CALL_ACTIVE";
          errormsg = "There is not a remote procedure call active in this thread.";
          break;
        #endif
        #ifdef RPC_S_CALL_FAILED
        case RPC_S_CALL_FAILED:
          errorname = "RPC_S_CALL_FAILED";
          errormsg = "The remote procedure call failed.";
          break;
        #endif
        #ifdef RPC_S_CALL_FAILED_DNE
        case RPC_S_CALL_FAILED_DNE:
          errorname = "RPC_S_CALL_FAILED_DNE";
          errormsg = "The remote procedure call failed and did not execute.";
          break;
        #endif
        #ifdef RPC_S_PROTOCOL_ERROR
        case RPC_S_PROTOCOL_ERROR:
          errorname = "RPC_S_PROTOCOL_ERROR";
          errormsg = "A remote procedure call (RPC) protocol error occurred.";
          break;
        #endif
        #ifdef RPC_S_UNSUPPORTED_TRANS_SYN
        case RPC_S_UNSUPPORTED_TRANS_SYN:
          errorname = "RPC_S_UNSUPPORTED_TRANS_SYN";
          errormsg = "The transfer syntax is not supported by the RPC server.";
          break;
        #endif
        #ifdef RPC_S_UNSUPPORTED_TYPE
        case RPC_S_UNSUPPORTED_TYPE:
          errorname = "RPC_S_UNSUPPORTED_TYPE";
          errormsg = "The universal unique identifier (UUID) type is not supported.";
          break;
        #endif
        #ifdef RPC_S_INVALID_TAG
        case RPC_S_INVALID_TAG:
          errorname = "RPC_S_INVALID_TAG";
          errormsg = "The tag is invalid.";
          break;
        #endif
        #ifdef RPC_S_INVALID_BOUND
        case RPC_S_INVALID_BOUND:
          errorname = "RPC_S_INVALID_BOUND";
          errormsg = "The array bounds are invalid.";
          break;
        #endif
        #ifdef RPC_S_NO_ENTRY_NAME
        case RPC_S_NO_ENTRY_NAME:
          errorname = "RPC_S_NO_ENTRY_NAME";
          errormsg = "The binding does not contain an entry name.";
          break;
        #endif
        #ifdef RPC_S_INVALID_NAME_SYNTAX
        case RPC_S_INVALID_NAME_SYNTAX:
          errorname = "RPC_S_INVALID_NAME_SYNTAX";
          errormsg = "The name syntax is invalid.";
          break;
        #endif
        #ifdef RPC_S_UNSUPPORTED_NAME_SYNTAX
        case RPC_S_UNSUPPORTED_NAME_SYNTAX:
          errorname = "RPC_S_UNSUPPORTED_NAME_SYNTAX";
          errormsg = "The name syntax is not supported.";
          break;
        #endif
        #ifdef RPC_S_UUID_NO_ADDRESS
        case RPC_S_UUID_NO_ADDRESS:
          errorname = "RPC_S_UUID_NO_ADDRESS";
          errormsg = "No network address is available to use to construct a universal unique identifier (UUID).";
          break;
        #endif
        #ifdef RPC_S_DUPLICATE_ENDPOINT
        case RPC_S_DUPLICATE_ENDPOINT:
          errorname = "RPC_S_DUPLICATE_ENDPOINT";
          errormsg = "The endpoint is a duplicate.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_AUTHN_TYPE
        case RPC_S_UNKNOWN_AUTHN_TYPE:
          errorname = "RPC_S_UNKNOWN_AUTHN_TYPE";
          errormsg = "The authentication type is unknown.";
          break;
        #endif
        #ifdef RPC_S_MAX_CALLS_TOO_SMALL
        case RPC_S_MAX_CALLS_TOO_SMALL:
          errorname = "RPC_S_MAX_CALLS_TOO_SMALL";
          errormsg = "The maximum number of calls is too small.";
          break;
        #endif
        #ifdef RPC_S_STRING_TOO_LONG
        case RPC_S_STRING_TOO_LONG:
          errorname = "RPC_S_STRING_TOO_LONG";
          errormsg = "The string is too long.";
          break;
        #endif
        #ifdef RPC_S_PROTSEQ_NOT_FOUND
        case RPC_S_PROTSEQ_NOT_FOUND:
          errorname = "RPC_S_PROTSEQ_NOT_FOUND";
          errormsg = "The RPC protocol sequence was not found.";
          break;
        #endif
        #ifdef RPC_S_PROCNUM_OUT_OF_RANGE
        case RPC_S_PROCNUM_OUT_OF_RANGE:
          errorname = "RPC_S_PROCNUM_OUT_OF_RANGE";
          errormsg = "The procedure number is out of range.";
          break;
        #endif
        #ifdef RPC_S_BINDING_HAS_NO_AUTH
        case RPC_S_BINDING_HAS_NO_AUTH:
          errorname = "RPC_S_BINDING_HAS_NO_AUTH";
          errormsg = "The binding does not contain any authentication information.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_AUTHN_SERVICE
        case RPC_S_UNKNOWN_AUTHN_SERVICE:
          errorname = "RPC_S_UNKNOWN_AUTHN_SERVICE";
          errormsg = "The authentication service is unknown.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_AUTHN_LEVEL
        case RPC_S_UNKNOWN_AUTHN_LEVEL:
          errorname = "RPC_S_UNKNOWN_AUTHN_LEVEL";
          errormsg = "The authentication level is unknown.";
          break;
        #endif
        #ifdef RPC_S_INVALID_AUTH_IDENTITY
        case RPC_S_INVALID_AUTH_IDENTITY:
          errorname = "RPC_S_INVALID_AUTH_IDENTITY";
          errormsg = "The security context is invalid.";
          break;
        #endif
        #ifdef RPC_S_UNKNOWN_AUTHZ_SERVICE
        case RPC_S_UNKNOWN_AUTHZ_SERVICE:
          errorname = "RPC_S_UNKNOWN_AUTHZ_SERVICE";
          errormsg = "The authorization service is unknown.";
          break;
        #endif
        #ifdef EPT_S_INVALID_ENTRY
        case EPT_S_INVALID_ENTRY:
          errorname = "EPT_S_INVALID_ENTRY";
          errormsg = "The entry is invalid.";
          break;
        #endif
        #ifdef EPT_S_CANT_PERFORM_OP
        case EPT_S_CANT_PERFORM_OP:
          errorname = "EPT_S_CANT_PERFORM_OP";
          errormsg = "The server endpoint cannot perform the operation.";
          break;
        #endif
        #ifdef EPT_S_NOT_REGISTERED
        case EPT_S_NOT_REGISTERED:
          errorname = "EPT_S_NOT_REGISTERED";
          errormsg = "There are no more endpoints available from the endpoint mapper.";
          break;
        #endif
        #ifdef RPC_S_NOTHING_TO_EXPORT
        case RPC_S_NOTHING_TO_EXPORT:
          errorname = "RPC_S_NOTHING_TO_EXPORT";
          errormsg = "No interfaces have been exported.";
          break;
        #endif
        #ifdef RPC_S_INCOMPLETE_NAME
        case RPC_S_INCOMPLETE_NAME:
          errorname = "RPC_S_INCOMPLETE_NAME";
          errormsg = "The entry name is incomplete.";
          break;
        #endif
        #ifdef RPC_S_INVALID_VERS_OPTION
        case RPC_S_INVALID_VERS_OPTION:
          errorname = "RPC_S_INVALID_VERS_OPTION";
          errormsg = "The version option is invalid.";
          break;
        #endif
        #ifdef RPC_S_NO_MORE_MEMBERS
        case RPC_S_NO_MORE_MEMBERS:
          errorname = "RPC_S_NO_MORE_MEMBERS";
          errormsg = "There are no more members.";
          break;
        #endif
        #ifdef RPC_S_NOT_ALL_OBJS_UNEXPORTED
        case RPC_S_NOT_ALL_OBJS_UNEXPORTED:
          errorname = "RPC_S_NOT_ALL_OBJS_UNEXPORTED";
          errormsg = "There is nothing to unexport.";
          break;
        #endif
        #ifdef RPC_S_INTERFACE_NOT_FOUND
        case RPC_S_INTERFACE_NOT_FOUND:
          errorname = "RPC_S_INTERFACE_NOT_FOUND";
          errormsg = "The interface was not found.";
          break;
        #endif
        #ifdef RPC_S_ENTRY_ALREADY_EXISTS
        case RPC_S_ENTRY_ALREADY_EXISTS:
          errorname = "RPC_S_ENTRY_ALREADY_EXISTS";
          errormsg = "The entry already exists.";
          break;
        #endif
        #ifdef RPC_S_ENTRY_NOT_FOUND
        case RPC_S_ENTRY_NOT_FOUND:
          errorname = "RPC_S_ENTRY_NOT_FOUND";
          errormsg = "The entry is not found.";
          break;
        #endif
        #ifdef RPC_S_NAME_SERVICE_UNAVAILABLE
        case RPC_S_NAME_SERVICE_UNAVAILABLE:
          errorname = "RPC_S_NAME_SERVICE_UNAVAILABLE";
          errormsg = "The name service is unavailable.";
          break;
        #endif
        #ifdef RPC_S_INVALID_NAF_ID
        case RPC_S_INVALID_NAF_ID:
          errorname = "RPC_S_INVALID_NAF_ID";
          errormsg = "The network address family is invalid.";
          break;
        #endif
        #ifdef RPC_S_CANNOT_SUPPORT
        case RPC_S_CANNOT_SUPPORT:
          errorname = "RPC_S_CANNOT_SUPPORT";
          errormsg = "The requested operation is not supported.";
          break;
        #endif
        #ifdef RPC_S_NO_CONTEXT_AVAILABLE
        case RPC_S_NO_CONTEXT_AVAILABLE:
          errorname = "RPC_S_NO_CONTEXT_AVAILABLE";
          errormsg = "No security context is available to allow impersonation.";
          break;
        #endif
        #ifdef RPC_S_INTERNAL_ERROR
        case RPC_S_INTERNAL_ERROR:
          errorname = "RPC_S_INTERNAL_ERROR";
          errormsg = "An internal error occurred in a remote procedure call (RPC).";
          break;
        #endif
        #ifdef RPC_S_ZERO_DIVIDE
        case RPC_S_ZERO_DIVIDE:
          errorname = "RPC_S_ZERO_DIVIDE";
          errormsg = "The RPC server attempted an integer division by zero.";
          break;
        #endif
        #ifdef RPC_S_ADDRESS_ERROR
        case RPC_S_ADDRESS_ERROR:
          errorname = "RPC_S_ADDRESS_ERROR";
          errormsg = "An addressing error occurred in the RPC server.";
          break;
        #endif
        #ifdef RPC_S_FP_DIV_ZERO
        case RPC_S_FP_DIV_ZERO:
          errorname = "RPC_S_FP_DIV_ZERO";
          errormsg = "A floating-point operation at the RPC server caused a division by zero.";
          break;
        #endif
        #ifdef RPC_S_FP_UNDERFLOW
        case RPC_S_FP_UNDERFLOW:
          errorname = "RPC_S_FP_UNDERFLOW";
          errormsg = "A floating-point underflow occurred at the RPC server.";
          break;
        #endif
        #ifdef RPC_S_FP_OVERFLOW
        case RPC_S_FP_OVERFLOW:
          errorname = "RPC_S_FP_OVERFLOW";
          errormsg = "A floating-point overflow occurred at the RPC server.";
          break;
        #endif
        #ifdef RPC_X_NO_MORE_ENTRIES
        case RPC_X_NO_MORE_ENTRIES:
          errorname = "RPC_X_NO_MORE_ENTRIES";
          errormsg = "The list of RPC servers available for the binding of auto handles has been exhausted.";
          break;
        #endif
        #ifdef RPC_X_SS_CHAR_TRANS_OPEN_FAIL
        case RPC_X_SS_CHAR_TRANS_OPEN_FAIL:
          errorname = "RPC_X_SS_CHAR_TRANS_OPEN_FAIL";
          errormsg = "Unable to open the character translation table file.";
          break;
        #endif
        #ifdef RPC_X_SS_CHAR_TRANS_SHORT_FILE
        case RPC_X_SS_CHAR_TRANS_SHORT_FILE:
          errorname = "RPC_X_SS_CHAR_TRANS_SHORT_FILE";
          errormsg = "The file containing the character translation table has fewer than 512 bytes.";
          break;
        #endif
        #ifdef RPC_X_SS_IN_NULL_CONTEXT
        case RPC_X_SS_IN_NULL_CONTEXT:
          errorname = "RPC_X_SS_IN_NULL_CONTEXT";
          errormsg = "A null context handle was passed from the client to the host during a remote procedure call.";
          break;
        #endif
        #ifdef RPC_X_SS_CONTEXT_DAMAGED
        case RPC_X_SS_CONTEXT_DAMAGED:
          errorname = "RPC_X_SS_CONTEXT_DAMAGED";
          errormsg = "The context handle changed during a remote procedure call.";
          break;
        #endif
        #ifdef RPC_X_SS_HANDLES_MISMATCH
        case RPC_X_SS_HANDLES_MISMATCH:
          errorname = "RPC_X_SS_HANDLES_MISMATCH";
          errormsg = "The binding handles passed to a remote procedure call do not match.";
          break;
        #endif
        #ifdef RPC_X_SS_CANNOT_GET_CALL_HANDLE
        case RPC_X_SS_CANNOT_GET_CALL_HANDLE:
          errorname = "RPC_X_SS_CANNOT_GET_CALL_HANDLE";
          errormsg = "The stub is unable to get the remote procedure call handle.";
          break;
        #endif
        #ifdef RPC_X_NULL_REF_POINTER
        case RPC_X_NULL_REF_POINTER:
          errorname = "RPC_X_NULL_REF_POINTER";
          errormsg = "A null reference pointer was passed to the stub.";
          break;
        #endif
        #ifdef RPC_X_ENUM_VALUE_OUT_OF_RANGE
        case RPC_X_ENUM_VALUE_OUT_OF_RANGE:
          errorname = "RPC_X_ENUM_VALUE_OUT_OF_RANGE";
          errormsg = "The enumeration value is out of range.";
          break;
        #endif
        #ifdef RPC_X_BYTE_COUNT_TOO_SMALL
        case RPC_X_BYTE_COUNT_TOO_SMALL:
          errorname = "RPC_X_BYTE_COUNT_TOO_SMALL";
          errormsg = "The byte count is too small.";
          break;
        #endif
        #ifdef RPC_X_BAD_STUB_DATA
        case RPC_X_BAD_STUB_DATA:
          errorname = "RPC_X_BAD_STUB_DATA";
          errormsg = "The stub received bad data.";
          break;
        #endif
        #ifdef ERROR_INVALID_USER_BUFFER
        case ERROR_INVALID_USER_BUFFER:
          errorname = "ERROR_INVALID_USER_BUFFER";
          errormsg = "The supplied user buffer is not valid for the requested operation.";
          break;
        #endif
        #ifdef ERROR_UNRECOGNIZED_MEDIA
        case ERROR_UNRECOGNIZED_MEDIA:
          errorname = "ERROR_UNRECOGNIZED_MEDIA";
          errormsg = "The disk media is not recognized. It may not be formatted.";
          break;
        #endif
        #ifdef ERROR_NO_TRUST_LSA_SECRET
        case ERROR_NO_TRUST_LSA_SECRET:
          errorname = "ERROR_NO_TRUST_LSA_SECRET";
          errormsg = "The workstation does not have a trust secret.";
          break;
        #endif
        #ifdef ERROR_NO_TRUST_SAM_ACCOUNT
        case ERROR_NO_TRUST_SAM_ACCOUNT:
          errorname = "ERROR_NO_TRUST_SAM_ACCOUNT";
          errormsg = "The SAM database on the Windows NT Server does not have a computer account for this workstation trust relationship.";
          break;
        #endif
        #ifdef ERROR_TRUSTED_DOMAIN_FAILURE
        case ERROR_TRUSTED_DOMAIN_FAILURE:
          errorname = "ERROR_TRUSTED_DOMAIN_FAILURE";
          errormsg = "The trust relationship between the primary domain and the trusted domain failed.";
          break;
        #endif
        #ifdef ERROR_TRUSTED_RELATIONSHIP_FAILURE
        case ERROR_TRUSTED_RELATIONSHIP_FAILURE:
          errorname = "ERROR_TRUSTED_RELATIONSHIP_FAILURE";
          errormsg = "The trust relationship between this workstation and the primary domain failed.";
          break;
        #endif
        #ifdef ERROR_TRUST_FAILURE
        case ERROR_TRUST_FAILURE:
          errorname = "ERROR_TRUST_FAILURE";
          errormsg = "The network logon failed.";
          break;
        #endif
        #ifdef RPC_S_CALL_IN_PROGRESS
        case RPC_S_CALL_IN_PROGRESS:
          errorname = "RPC_S_CALL_IN_PROGRESS";
          errormsg = "A remote procedure call is already in progress for this thread.";
          break;
        #endif
        #ifdef ERROR_NETLOGON_NOT_STARTED
        case ERROR_NETLOGON_NOT_STARTED:
          errorname = "ERROR_NETLOGON_NOT_STARTED";
          errormsg = "An attempt was made to logon, but the network logon service was not started.";
          break;
        #endif
        #ifdef ERROR_ACCOUNT_EXPIRED
        case ERROR_ACCOUNT_EXPIRED:
          errorname = "ERROR_ACCOUNT_EXPIRED";
          errormsg = "The user's account has expired.";
          break;
        #endif
        #ifdef ERROR_REDIRECTOR_HAS_OPEN_HANDLES
        case ERROR_REDIRECTOR_HAS_OPEN_HANDLES:
          errorname = "ERROR_REDIRECTOR_HAS_OPEN_HANDLES";
          errormsg = "The redirector is in use and cannot be unloaded.";
          break;
        #endif
        #ifdef ERROR_PRINTER_DRIVER_ALREADY_INSTALLED
        case ERROR_PRINTER_DRIVER_ALREADY_INSTALLED:
          errorname = "ERROR_PRINTER_DRIVER_ALREADY_INSTALLED";
          errormsg = "The specified printer driver is already installed.";
          break;
        #endif
        #ifdef ERROR_UNKNOWN_PORT
        case ERROR_UNKNOWN_PORT:
          errorname = "ERROR_UNKNOWN_PORT";
          errormsg = "The specified port is unknown.";
          break;
        #endif
        #ifdef ERROR_UNKNOWN_PRINTER_DRIVER
        case ERROR_UNKNOWN_PRINTER_DRIVER:
          errorname = "ERROR_UNKNOWN_PRINTER_DRIVER";
          errormsg = "The printer driver is unknown.";
          break;
        #endif
        #ifdef ERROR_UNKNOWN_PRINTPROCESSOR
        case ERROR_UNKNOWN_PRINTPROCESSOR:
          errorname = "ERROR_UNKNOWN_PRINTPROCESSOR";
          errormsg = "The print processor is unknown.";
          break;
        #endif
        #ifdef ERROR_INVALID_SEPARATOR_FILE
        case ERROR_INVALID_SEPARATOR_FILE:
          errorname = "ERROR_INVALID_SEPARATOR_FILE";
          errormsg = "The specified separator file is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_PRIORITY
        case ERROR_INVALID_PRIORITY:
          errorname = "ERROR_INVALID_PRIORITY";
          errormsg = "The specified priority is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_PRINTER_NAME
        case ERROR_INVALID_PRINTER_NAME:
          errorname = "ERROR_INVALID_PRINTER_NAME";
          errormsg = "The printer name is invalid.";
          break;
        #endif
        #ifdef ERROR_PRINTER_ALREADY_EXISTS
        case ERROR_PRINTER_ALREADY_EXISTS:
          errorname = "ERROR_PRINTER_ALREADY_EXISTS";
          errormsg = "The printer already exists.";
          break;
        #endif
        #ifdef ERROR_INVALID_PRINTER_COMMAND
        case ERROR_INVALID_PRINTER_COMMAND:
          errorname = "ERROR_INVALID_PRINTER_COMMAND";
          errormsg = "The printer command is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_DATATYPE
        case ERROR_INVALID_DATATYPE:
          errorname = "ERROR_INVALID_DATATYPE";
          errormsg = "The specified datatype is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_ENVIRONMENT
        case ERROR_INVALID_ENVIRONMENT:
          errorname = "ERROR_INVALID_ENVIRONMENT";
          errormsg = "The Environment specified is invalid.";
          break;
        #endif
        #ifdef RPC_S_NO_MORE_BINDINGS
        case RPC_S_NO_MORE_BINDINGS:
          errorname = "RPC_S_NO_MORE_BINDINGS";
          errormsg = "There are no more bindings.";
          break;
        #endif
        #ifdef ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT
        case ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT:
          errorname = "ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT";
          errormsg = "The account used is an interdomain trust account. Use your global user account or local user account to access this server.";
          break;
        #endif
        #ifdef ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT
        case ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT:
          errorname = "ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT";
          errormsg = "The account used is a Computer Account. Use your global user account or local user account to access this server.";
          break;
        #endif
        #ifdef ERROR_NOLOGON_SERVER_TRUST_ACCOUNT
        case ERROR_NOLOGON_SERVER_TRUST_ACCOUNT:
          errorname = "ERROR_NOLOGON_SERVER_TRUST_ACCOUNT";
          errormsg = "The account used is an server trust account. Use your global user account or local user account to access this server.";
          break;
        #endif
        #ifdef ERROR_DOMAIN_TRUST_INCONSISTENT
        case ERROR_DOMAIN_TRUST_INCONSISTENT:
          errorname = "ERROR_DOMAIN_TRUST_INCONSISTENT";
          errormsg = "The name or security ID (SID) of the domain specified is inconsistent with the trust information for that domain.";
          break;
        #endif
        #ifdef ERROR_SERVER_HAS_OPEN_HANDLES
        case ERROR_SERVER_HAS_OPEN_HANDLES:
          errorname = "ERROR_SERVER_HAS_OPEN_HANDLES";
          errormsg = "The server is in use and cannot be unloaded.";
          break;
        #endif
        #ifdef ERROR_RESOURCE_DATA_NOT_FOUND
        case ERROR_RESOURCE_DATA_NOT_FOUND:
          errorname = "ERROR_RESOURCE_DATA_NOT_FOUND";
          errormsg = "The specified image file did not contain a resource section.";
          break;
        #endif
        #ifdef ERROR_RESOURCE_TYPE_NOT_FOUND
        case ERROR_RESOURCE_TYPE_NOT_FOUND:
          errorname = "ERROR_RESOURCE_TYPE_NOT_FOUND";
          errormsg = "The specified resource type can not be found in the image file.";
          break;
        #endif
        #ifdef ERROR_RESOURCE_NAME_NOT_FOUND
        case ERROR_RESOURCE_NAME_NOT_FOUND:
          errorname = "ERROR_RESOURCE_NAME_NOT_FOUND";
          errormsg = "The specified resource name can not be found in the image file.";
          break;
        #endif
        #ifdef ERROR_RESOURCE_LANG_NOT_FOUND
        case ERROR_RESOURCE_LANG_NOT_FOUND:
          errorname = "ERROR_RESOURCE_LANG_NOT_FOUND";
          errormsg = "The specified resource language ID cannot be found in the image file.";
          break;
        #endif
        #ifdef ERROR_NOT_ENOUGH_QUOTA
        case ERROR_NOT_ENOUGH_QUOTA:
          errorname = "ERROR_NOT_ENOUGH_QUOTA";
          errormsg = "Not enough quota is available to process this command.";
          break;
        #endif
        #ifdef RPC_S_NO_INTERFACES
        case RPC_S_NO_INTERFACES:
          errorname = "RPC_S_NO_INTERFACES";
          errormsg = "No interfaces have been registered.";
          break;
        #endif
        #ifdef RPC_S_CALL_CANCELLED
        case RPC_S_CALL_CANCELLED:
          errorname = "RPC_S_CALL_CANCELLED";
          errormsg = "The server was altered while processing this call.";
          break;
        #endif
        #ifdef RPC_S_BINDING_INCOMPLETE
        case RPC_S_BINDING_INCOMPLETE:
          errorname = "RPC_S_BINDING_INCOMPLETE";
          errormsg = "The binding handle does not contain all required information.";
          break;
        #endif
        #ifdef RPC_S_COMM_FAILURE
        case RPC_S_COMM_FAILURE:
          errorname = "RPC_S_COMM_FAILURE";
          errormsg = "Communications failure.";
          break;
        #endif
        #ifdef RPC_S_UNSUPPORTED_AUTHN_LEVEL
        case RPC_S_UNSUPPORTED_AUTHN_LEVEL:
          errorname = "RPC_S_UNSUPPORTED_AUTHN_LEVEL";
          errormsg = "The requested authentication level is not supported.";
          break;
        #endif
        #ifdef RPC_S_NO_PRINC_NAME
        case RPC_S_NO_PRINC_NAME:
          errorname = "RPC_S_NO_PRINC_NAME";
          errormsg = "No principal name registered.";
          break;
        #endif
        #ifdef RPC_S_NOT_RPC_ERROR
        case RPC_S_NOT_RPC_ERROR:
          errorname = "RPC_S_NOT_RPC_ERROR";
          errormsg = "The error specified is not a valid Windows RPC error code.";
          break;
        #endif
        #ifdef RPC_S_UUID_LOCAL_ONLY
        case RPC_S_UUID_LOCAL_ONLY:
          errorname = "RPC_S_UUID_LOCAL_ONLY";
          errormsg = "A UUID that is valid only on this computer has been allocated.";
          break;
        #endif
        #ifdef RPC_S_SEC_PKG_ERROR
        case RPC_S_SEC_PKG_ERROR:
          errorname = "RPC_S_SEC_PKG_ERROR";
          errormsg = "A security package specific error occurred.";
          break;
        #endif
        #ifdef RPC_S_NOT_CANCELLED
        case RPC_S_NOT_CANCELLED:
          errorname = "RPC_S_NOT_CANCELLED";
          errormsg = "Thread is not cancelled.";
          break;
        #endif
        #ifdef RPC_X_INVALID_ES_ACTION
        case RPC_X_INVALID_ES_ACTION:
          errorname = "RPC_X_INVALID_ES_ACTION";
          errormsg = "Invalid operation on the encoding/decoding handle.";
          break;
        #endif
        #ifdef RPC_X_WRONG_ES_VERSION
        case RPC_X_WRONG_ES_VERSION:
          errorname = "RPC_X_WRONG_ES_VERSION";
          errormsg = "Incompatible version of the serializing package.";
          break;
        #endif
        #ifdef RPC_X_WRONG_STUB_VERSION
        case RPC_X_WRONG_STUB_VERSION:
          errorname = "RPC_X_WRONG_STUB_VERSION";
          errormsg = "Incompatible version of the RPC stub.";
          break;
        #endif
        #ifdef RPC_S_GROUP_MEMBER_NOT_FOUND
        case RPC_S_GROUP_MEMBER_NOT_FOUND:
          errorname = "RPC_S_GROUP_MEMBER_NOT_FOUND";
          errormsg = "The group member was not found.";
          break;
        #endif
        #ifdef EPT_S_CANT_CREATE
        case EPT_S_CANT_CREATE:
          errorname = "EPT_S_CANT_CREATE";
          errormsg = "The endpoint mapper database could not be created.";
          break;
        #endif
        #ifdef RPC_S_INVALID_OBJECT
        case RPC_S_INVALID_OBJECT:
          errorname = "RPC_S_INVALID_OBJECT";
          errormsg = "The object universal unique identifier (UUID) is the nil UUID.";
          break;
        #endif
        #ifdef ERROR_INVALID_TIME
        case ERROR_INVALID_TIME:
          errorname = "ERROR_INVALID_TIME";
          errormsg = "The specified time is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_FORM_NAME
        case ERROR_INVALID_FORM_NAME:
          errorname = "ERROR_INVALID_FORM_NAME";
          errormsg = "The specified Form name is invalid.";
          break;
        #endif
        #ifdef ERROR_INVALID_FORM_SIZE
        case ERROR_INVALID_FORM_SIZE:
          errorname = "ERROR_INVALID_FORM_SIZE";
          errormsg = "The specified Form size is invalid.";
          break;
        #endif
        #ifdef ERROR_ALREADY_WAITING
        case ERROR_ALREADY_WAITING:
          errorname = "ERROR_ALREADY_WAITING";
          errormsg = "The specified Printer handle is already being waited on.";
          break;
        #endif
        #ifdef ERROR_PRINTER_DELETED
        case ERROR_PRINTER_DELETED:
          errorname = "ERROR_PRINTER_DELETED";
          errormsg = "The specified Printer has been deleted.";
          break;
        #endif
        #ifdef ERROR_INVALID_PRINTER_STATE
        case ERROR_INVALID_PRINTER_STATE:
          errorname = "ERROR_INVALID_PRINTER_STATE";
          errormsg = "The state of the Printer is invalid.";
          break;
        #endif
        #ifdef ERROR_PASSWORD_MUST_CHANGE
        case ERROR_PASSWORD_MUST_CHANGE:
          errorname = "ERROR_PASSWORD_MUST_CHANGE";
          errormsg = "The user must change his password before he logs on the first time.";
          break;
        #endif
        #ifdef ERROR_DOMAIN_CONTROLLER_NOT_FOUND
        case ERROR_DOMAIN_CONTROLLER_NOT_FOUND:
          errorname = "ERROR_DOMAIN_CONTROLLER_NOT_FOUND";
          errormsg = "Could not find the domain controller for this domain.";
          break;
        #endif
        #ifdef ERROR_ACCOUNT_LOCKED_OUT
        case ERROR_ACCOUNT_LOCKED_OUT:
          errorname = "ERROR_ACCOUNT_LOCKED_OUT";
          errormsg = "The referenced account is currently locked out and may not be logged on to.";
          break;
        #endif
        #ifdef ERROR_NO_BROWSER_SERVERS_FOUND
        case ERROR_NO_BROWSER_SERVERS_FOUND:
          errorname = "ERROR_NO_BROWSER_SERVERS_FOUND";
          errormsg = "The list of servers for this workgroup is not currently available.";
          break;
        #endif
        default:
          {
            var char* buf = (char*)alloca(1000);
            begin_system_call();
            if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, errcode, 0, buf, 1000, NULL))
              errormsg = buf;
            end_system_call();
          }
          break;
      }
      func(errorname,errormsg);
    }

  # Behandlung von Win32-Fehlern
  # OS_error();
  # > GetLastError(): Fehlercode
    nonreturning_function(global, OS_error, (void));
    nonreturning_function(global, OS_file_error, (object pathname));
    local void OS_error_internal (DWORD errcode);
    local void OS_error_internal_body (const char* name, const char* msg)
    {
      if (!(name == NULL)) { # bekannter Name?
        write_errorasciz(" (");
        write_errorasciz(name);
        write_errorasciz(")");
      }
      if (!(msg == NULL)) { # Meldung vorhanden?
        write_errorasciz(": ");
        write_errorasciz(msg);
      }
    }
    local void OS_error_internal (DWORD errcode)
    {
      # Meldungbeginn ausgeben:
      write_errorstring(GETTEXT("Win32 error "));
      # Fehlernummer ausgeben:
      write_errorobject(UL_to_I(errcode));
      # nach Möglichkeit noch ausführlicher:
      get_OS_error_info(errcode,&OS_error_internal_body);
    }
    nonreturning_function(global, OS_error, (void))
    {
      var DWORD errcode;
      end_system_call(); # just in case
      begin_system_call();
      errcode = GetLastError();
      end_system_call();
      clr_break_sem_4(); # keine Win32-Operation mehr aktiv
      begin_error(); # Fehlermeldung anfangen
      if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
        STACK_3 = S(simple_os_error);
      OS_error_internal(errcode);
      end_error(args_end_pointer STACKop 7,true); # Fehlermeldung beenden
      NOTREACHED;
    }
    nonreturning_function(global, OS_file_error, (object pathname))
    {
      var DWORD errcode;
      begin_system_call();
      errcode = GetLastError();
      end_system_call();
      clr_break_sem_4(); # keine Win32-Operation mehr aktiv
      pushSTACK(pathname); # Wert von PATHNAME für FILE-ERROR
      begin_error(); # Fehlermeldung anfangen
      if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
        STACK_3 = S(simple_file_error);
      OS_error_internal(errcode);
      end_error(args_end_pointer STACKop 7,true); # Fehlermeldung beenden
      NOTREACHED;
    }

  # Behandlung von Winsock-Fehlern
  # SOCK_error();
  # > WSAGetLastError(): Fehlercode
    nonreturning_function(global, SOCK_error, (void))
    {
      var int errcode = WSAGetLastError();
      end_system_call();
      clr_break_sem_4(); # keine Win32-Operation mehr aktiv
      begin_error(); # Fehlermeldung anfangen
      if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
        STACK_3 = S(simple_os_error);
      # Meldungbeginn ausgeben:
      write_errorstring(GETTEXT("Winsock error "));
      # Fehlernummer ausgeben:
      write_errorobject(L_to_I(errcode));
      # nach Möglichkeit noch ausführlicher:
      var const char* errorname = NULL;
      var const char* errormsg = NULL;
      switch (errcode) {
        case WSAEINTR:
          errorname = "EINTR";
          errormsg = "Interrupted system call";
          break;
        case WSAEBADF:
          errorname = "EBADF";
          errormsg="Bad file number";
          break;
        case WSAEACCES:
          errorname = "EACCES";
          errormsg="Permission denied";
          break;
        case WSAEFAULT:
          errorname = "EFAULT";
          errormsg="Bad address";
          break;
        case WSAEINVAL:
          errorname = "EINVAL";
          errormsg="Invalid argument";
          break;
        case WSAEMFILE:
          errorname = "EMFILE";
          errormsg="Too many open files";
          break;

        case WSAEWOULDBLOCK:
          errorname = "EWOULDBLOCK";
          errormsg="Operation would block";
          break;
        case WSAEINPROGRESS:
          errorname = "EINPROGRESS";
          errormsg="Operation now in progress";
          break;
        case WSAEALREADY:
          errorname = "EALREADY";
          errormsg="Operation already in progress";
          break;
        case WSAENOTSOCK:
          errorname = "ENOTSOCK";
          errormsg="Socket operation on non-socket";
          break;
        case WSAEDESTADDRREQ:
          errorname = "EDESTADDRREQ";
          errormsg="Destination address required";
          break;
        case WSAEMSGSIZE:
          errorname = "EMSGSIZE";
          errormsg="Message too long";
          break;
        case WSAEPROTOTYPE:
          errorname = "EPROTOTYPE";
          errormsg="Protocol wrong type for socket";
          break;
        case WSAENOPROTOOPT:
          errorname = "ENOPROTOOPT";
          errormsg="Option not supported by protocol";
          break;
        case WSAEPROTONOSUPPORT:
          errorname = "EPROTONOSUPPORT";
          errormsg="Protocol not supported";
          break;
        case WSAESOCKTNOSUPPORT:
          errorname = "ESOCKTNOSUPPORT";
          errormsg="Socket type not supported";
          break;
        case WSAEOPNOTSUPP:
          errorname = "EOPNOTSUPP";
          errormsg="Operation not supported on socket";
          break;
        case WSAEPFNOSUPPORT:
          errorname = "EPFNOSUPPORT";
          errormsg="Protocol family not supported";
          break;
        case WSAEAFNOSUPPORT:
          errorname = "EAFNOSUPPORT";
          errormsg="Address family not supported by protocol family";
          break;
        case WSAEADDRINUSE:
          errorname = "EADDRINUSE";
          errormsg="Address already in use";
          break;
        case WSAEADDRNOTAVAIL:
          errorname = "EADDRNOTAVAIL";
          errormsg="Cannot assign requested address";
          break;
        case WSAENETDOWN:
          errorname = "ENETDOWN";
          errormsg="Network is down";
          break;
        case WSAENETUNREACH:
          errorname = "ENETUNREACH";
          errormsg="Network is unreachable";
          break;
        case WSAENETRESET:
          errorname = "ENETRESET";
          errormsg="Network dropped connection on reset";
          break;
        case WSAECONNABORTED:
          errorname = "ECONNABORTED";
          errormsg="Software caused connection abort";
          break;
        case WSAECONNRESET:
          errorname = "ECONNRESET";
          errormsg="Connection reset by peer";
          break;
        case WSAENOBUFS:
          errorname = "ENOBUFS";
          errormsg="No buffer space available";
          break;
        case WSAEISCONN:
          errorname = "EISCONN";
          errormsg="Socket is already connected";
          break;
        case WSAENOTCONN:
          errorname = "ENOTCONN";
          errormsg="Socket is not connected";
          break;
        case WSAESHUTDOWN:
          errorname = "ESHUTDOWN";
          errormsg="Cannot send after socket shutdown";
          break;
        case WSAETOOMANYREFS:
          errorname = "ETOOMANYREFS";
          errormsg="Too many references: cannot splice";
          break;
        case WSAETIMEDOUT:
          errorname = "ETIMEDOUT";
          errormsg="Connection timed out";
          break;
        case WSAECONNREFUSED:
          errorname = "ECONNREFUSED";
          errormsg="Connection refused";
          break;
        case WSAELOOP:
          errorname = "ELOOP";
          errormsg="Too many levels of symbolic links";
          break;
        case WSAENAMETOOLONG:
          errorname = "ENAMETOOLONG";
          errormsg="File name too long";
          break;
        case WSAEHOSTDOWN:
          errorname = "EHOSTDOWN";
          errormsg="Host is down";
          break;
        case WSAEHOSTUNREACH:
          errorname = "EHOSTUNREACH";
          errormsg="Host is unreachable";
          break;
        case WSAENOTEMPTY:
          errorname = "ENOTEMPTY";
          errormsg="Directory not empty";
          break;
        case WSAEPROCLIM:
          errorname = "EPROCLIM";
          errormsg="Too many processes";
          break;
        case WSAEUSERS:
          errorname = "EUSERS";
          errormsg="Too many users";
          break;
        case WSAEDQUOT:
          errorname = "EDQUOT";
          errormsg="Disk quota exceeded";
          break;
        case WSAESTALE:
          errorname = "ESTALE";
          errormsg="Stale NFS file handle";
          break;
        case WSAEREMOTE:
          errorname = "EREMOTE";
          errormsg="Too many levels of remote in path";
          break;

        case WSAEDISCON:
          errorname = "EDISCON";
          break;

        case WSASYSNOTREADY:
          errorname = "SYSNOTREADY";
          break;
        case WSAVERNOTSUPPORTED:
          errorname = "VERNOTSUPPORTED";
          break;
        case WSANOTINITIALISED:
          errorname = "NOTINITIALISED";
          break;

        default:
          break;
      }
      if (!(errorname == NULL)) { # bekannter Name?
        write_errorasciz(" (");
        write_errorasciz(errorname);
        write_errorasciz(")");
      }
      if (!(errormsg == NULL)) { # Meldung vorhanden?
        write_errorasciz(": ");
        write_errorasciz(errormsg);
      }
      end_error(args_end_pointer STACKop 7,true); # Fehlermeldung beenden
      NOTREACHED;
    }

/* print an error
 > DWORD errorcode: error code (errno)
 > FILE: Filename (with quotation marks) as constant ASCIZ-String
 > LINE: line number */
local void errno_out_body (const char* name, const char* msg) {
  if (name != NULL)
    fprintf(stderr," (%s)",name);
  if (msg != NULL)
    fprintf(stderr,": %s",msg);
  else
    fprintf(stderr,".");
}
global void errno_out_low (DWORD errorcode, const char* file, uintL line) {
  fprintf(stderr,"\n[%s:%d] GetLastError() = 0x%x",file,line,errorcode);
  get_OS_error_info(errorcode,&errno_out_body);
  fputs("\n",stderr);
}
