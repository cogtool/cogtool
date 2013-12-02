/* win32 shell tools */

/* shell_quote()
 surround dangerous strings with double quotes.
 escape quotes and backslashes.
 dest should be twice as large as source
  + 2 (for quotes) + 1 for zero byte + 1 for possible endslash */
int shell_quote (char * dest, const char * source) {
  const char * characters = " &<>|^\t";
  /* Chars other than command separators are actual only when command
     interpreter is used */
  BOOL ech, quote = !(*source); /* quote empty arguments */
  int escaped = 0;
  char * dcp = dest;
  *dcp++ = ' ';
  while (*source) {
    quote = quote || strchr(characters,*source);
    ech = *source == '\\';
    if (!escaped && *source == '"') *dcp++ = '\\';
    *dcp++ = *source++;
    escaped = !escaped && ech;
  }
  if (quote) {
    if (escaped) *dcp++ = '\\'; /* double ending slash */
    *dcp++ = '"'; *dest = '"'; }
  *dcp = 0;
  /* shift string left if no quote was inserted */
  if (!quote) for (dcp = dest;;dcp++) if (!(*dcp = dcp[1])) break;
  return dcp - dest;
}

/*========== shell shortcut resolution ==========*/
#include <shlobj.h>

/* extracts a filename field from windows shortcut
 > filename: name the shortcut file
 < resolved: buffer not less than MAX_PATH
 < result:   true if link was successfully resolved */
static BOOL resolve_shell_shortcut (LPCSTR filename, LPSTR resolved) {
  DWORD fileattr;
  HRESULT hres;
  IShellLink* psl;
  WIN32_FIND_DATA wfd;
  BOOL result = FALSE;
  IPersistFile* ppf;

  fileattr = GetFileAttributes(filename);
  if (fileattr == 0xFFFFFFFF) return FALSE;
  /* Get a pointer to the IShellLink interface. */
  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                          &IID_IShellLink, (LPVOID *) &psl);
  if (FAILED(hres)) return FALSE;
  /* Get a pointer to the IPersistFile interface. */
  hres = psl->lpVtbl->QueryInterface(psl, &IID_IPersistFile,(LPVOID *) &ppf);
  if (SUCCEEDED(hres)) {
    WCHAR wsz[MAX_PATH];
    /* Ensure that the string is Unicode. */
    MultiByteToWideChar(CP_ACP, 0, filename, -1, wsz,MAX_PATH);
    /* Load the shortcut. */
    hres = ppf->lpVtbl->Load(ppf, wsz, STGM_READ);
    if (SUCCEEDED(hres)) {
      /* Get the path to the link target. */
      hres = psl->lpVtbl->GetPath(psl, resolved, MAX_PATH,
                                  (WIN32_FIND_DATA *)&wfd,
                                  4 /* SLGP_RAWPATH */);
      if (SUCCEEDED(hres)) result = TRUE;
      if (!*resolved) {
        /* empty string. maybe broken link. try to get description
           as cygwin stores filenames there */
        hres = psl->lpVtbl->GetDescription(psl, resolved, MAX_PATH);
        if (FAILED(hres)) *resolved = 0;
      }
    }
    /* Release the pointer to the IPersistFile interface. */
    ppf->lpVtbl->Release(ppf);
  }
  /* Release the pointer to the IShellLink interface. */
  psl->lpVtbl->Release(psl);
  return result;
}

#if !defined(cpslashp)
# define cpslashp(c) ((c) == '\\' || (c) == '/')
#endif

/* Uses the base from filename to augment pathname.
   Return false when (pathname is relative AND filename doesn't
   contain the base), so pathname (contained in shortcut)
   cannot be referenced other than to current directory
   what is wrong. */
static BOOL augment_relative_pathname(LPCSTR filename, LPSTR pathname) {
  /* check if pathname is absolute */
  /* what to do with "/bar/foo" pathnames ?*/
  if (cpslashp(pathname[0])) return FALSE; /* let's panic */
  if (((pathname[0] >= 'a' && pathname[0] <= 'z')
    || (pathname[0] >= 'A' && pathname[0] <= 'Z'))
    && pathname[1] == ':' && cpslashp(pathname[2])) return TRUE;
  if (pathname[0] == '\\' && pathname[1] == '\\') return TRUE;
  {
    int fl = strlen(filename);
    int pl = strlen(pathname);
    const char * cp = filename + fl - 1;
    /* find the last slash */
    for (;!cpslashp(*cp) && cp > filename;cp--);
    if (!cpslashp(*cp)) return FALSE; /* no slash */
    memmove(pathname + (cp - filename + 1),pathname,pl + 1);
    memmove(pathname,filename,cp - filename + 1);
  }
  return TRUE;
}


typedef enum {
  shell_shortcut_notresolved = 0,
  shell_shortcut_notexists,
  shell_shortcut_file,
  shell_shortcut_directory
} shell_shortcut_target_t;

/* resolves shortcuts to shortcuts
 > filename : name of link file to resolve
 < resolved : buffer to receive resolved name
 < result : status of resolving and target file attributes */
static shell_shortcut_target_t
resolve_shell_shortcut_more (LPCSTR filename, LPSTR resolved)
{
  char pathname[_MAX_PATH];
  char pathname1[_MAX_PATH];
  int dirp = 0;
  int exists = 0;
  int try_counter = 33;
  int l, resolvedp = resolve_shell_shortcut(filename,pathname)
    && augment_relative_pathname(filename,pathname);
  /* handle links to links. cygwin can do such */
  while (resolvedp && try_counter--) {
    l=strlen(pathname);
    if (l >= 4 && stricmp(pathname+l-4,".lnk") == 0)
      resolvedp = resolve_shell_shortcut(pathname,pathname1)
              && augment_relative_pathname(pathname,pathname1)
              && strcpy(pathname,pathname1);
    else {
    /* not a link to shortcut but can be the symbolic filename */
      strcpy(pathname+l,".lnk");
      if (!resolve_shell_shortcut(pathname,pathname1)
        || !augment_relative_pathname(pathname,pathname1)) {
        pathname[l] = '\0'; break; }
      else strcpy(pathname,pathname1);
    }
  }
  if (resolvedp) { /* additional checks */
    DWORD fileattr = GetFileAttributes(pathname);
    exists = fileattr != 0xFFFFFFFF;
    dirp = exists && fileattr&FILE_ATTRIBUTE_DIRECTORY;
    if (resolved) {
      strcpy(resolved,pathname);
      if (dirp) strcat(resolved,"\\");
    }
    if (dirp) return shell_shortcut_directory;
    if (exists && !dirp) return shell_shortcut_file;
    return shell_shortcut_notexists;
  } else return shell_shortcut_notresolved;
}

/* see if a file is normal file or it is a "shell symlink"
 If directory+filename exists do nothing return false
 If it doesn't but direstory+filename+".lnk" exists then
 try to read it. On reading success return true with lnk
 filename value as resolved. See resolve_shell_shortcut also.
 > filename: resolving file name
 < resolved: buffer for resolved path and filename.
 < result: shell_shortcut_notresolved if file exists or link is invalid.
           otherwise - shortcut target status */
static shell_shortcut_target_t
resolve_shell_symlink (LPCSTR filename, LPSTR resolved)
{
  char pathname[_MAX_PATH];
  DWORD fileattr;

  strcpy(pathname,filename);
  fileattr = GetFileAttributes(pathname);
  if (fileattr != 0xFFFFFFFF) return shell_shortcut_notresolved;
  strcat(pathname,".lnk");
  fileattr = GetFileAttributes(pathname);
  if (fileattr == 0xFFFFFFFF) return shell_shortcut_notresolved;
  return resolve_shell_shortcut_more(pathname,resolved);
}

/* the ultimate shortcut megaresolver
   style inspired by directory_search_scandir
 > namein: filename pointing to file or directory
            wildcards (only asterisk) may appear only as filename
 < nameout: filename with directory and file shortcuts resolved
             on failure holds filename resolved so far
 < result:  true if resolving succeeded */
BOOL real_path (LPCSTR namein, LPSTR nameout) {
  WIN32_FIND_DATA wfd;
  HANDLE h = NULL;
  char * nametocheck;
  char * nametocheck_end;
  /* drive|dir1|dir2|name
           ^nametocheck
               ^nametocheck_end */
  char saved_char;
  BOOL next_name = 0;/* if we found an lnk and need to start over */
  int try_counter = 33;
  if (strlen(namein) >= MAX_PATH) return FALSE;
  strcpy(nameout,namein);
  do { /* whole file names */
    next_name = FALSE;
    if (!*nameout) return FALSE;
    /* skip drive or host or first slash */
    nametocheck = nameout;
    if (((*nametocheck >= 'a' && *nametocheck <= 'z')
         || (*nametocheck >= 'A' && *nametocheck <= 'Z'))
        && nametocheck[1] == ':' && cpslashp(nametocheck[2]))
      /* drive */
      nametocheck += 3;
    else if (nametocheck[0]=='\\' && nametocheck[1]=='\\') {
      int i;
      /* host */
      nametocheck+=2;
      for (i=0;i<2;i++) {/* skip host and sharename */
        while (*nametocheck && !cpslashp(*nametocheck))
          nametocheck++;
        if (*nametocheck) nametocheck++; else return FALSE;
      }
    } else if (cpslashp(*nametocheck)) nametocheck++;
    /* prefix skipped; start checking */
    do { /* each component after just skipped */
      int dots_only = 0;
      int have_stars = 0;
      /* separate a component */
      for (nametocheck_end = nametocheck;
           *nametocheck_end && !cpslashp(*nametocheck_end);
           nametocheck_end++);
      if (*nametocheck_end && nametocheck_end == nametocheck)
        return FALSE;/* two slashes one after another */
      /* save slash or zero */
      saved_char = *nametocheck_end;
      *nametocheck_end = 0;
      /* Is it . or .. ? FFF handles this strange way */
      { char * cp = nametocheck;
        for (;*cp=='.';cp++);
        dots_only = !(*cp) && cp > nametocheck; }
      /* Stars in the middle of filename: error
         Stars as pathname: success */
      { char * cp = nametocheck;
        for (;*cp && *cp!='*';cp++);
        have_stars = *cp == '*'; }
      if (have_stars && saved_char) return FALSE;
      if (!have_stars) {
        if (dots_only || !*nametocheck) {
          /* treat 'start/./end', 'drive/', 'host/' specially */
          /* search for ....\.\* */
          char saved[2];
          if (nametocheck_end - nameout + 2 > MAX_PATH) return FALSE;
          saved[0] = nametocheck_end[1]; saved[1] = nametocheck_end[2];
          /* !*nametocheck here means there was "something\" before */
          strcpy(nametocheck_end,*nametocheck?"\\*":"*");
          h = FindFirstFile(nameout,&wfd);
          nametocheck_end[1] = saved[0]; nametocheck_end[2] = saved[1];
          nametocheck_end[0] = 0;
          if (h != INVALID_HANDLE_VALUE) {
            FindClose(h); /* don't substitute */
          } else return FALSE; /* don't try lnk */
        } else {/* not only dots */
          h = FindFirstFile(nameout,&wfd);
          if (h != INVALID_HANDLE_VALUE) {
            /* make space for full (non 8.3) name component */
            int l = strlen(wfd.cFileName);
            FindClose(h);
            if (l != (nametocheck_end - nametocheck)) {
              int restlen =
                saved_char?(strlen(nametocheck_end+1)
                            +1/*saved_char*/+1/*zero byte*/)
                :0;
              if (nametocheck - nameout + restlen + l + 2 > MAX_PATH)
                return FALSE;
              if (restlen) memmove(nametocheck+l,nametocheck_end,restlen);
            }
            strncpy(nametocheck,wfd.cFileName,l);
            nametocheck_end = nametocheck + l;
          } else {/* try shortcut */
            char saved[4];
            char resolved[MAX_PATH];
            shell_shortcut_target_t rresult;
            if (nametocheck_end - nameout + 4 > MAX_PATH) return FALSE;
            strncpy(saved,nametocheck_end+1,4);
            strncpy(nametocheck_end,".lnk",5);
            rresult = resolve_shell_shortcut_more(nameout,resolved);
            strncpy(nametocheck_end+1,saved,4);
            *nametocheck_end = 0;
            /* use saved_char as directory indicator */
            if (rresult == shell_shortcut_notresolved
                || rresult == shell_shortcut_notexists
                || (saved_char ? rresult == shell_shortcut_file
                    : rresult == shell_shortcut_directory))
              return FALSE;
            if (saved_char) {
              /*need to subst nameout..nametocheck-1 with resolved path */
              int l1 = strlen(resolved);
              int l2 = strlen(nametocheck_end + 1);
              if (l1 + l2 + 2 > MAX_PATH) return FALSE;
              strncat(resolved,nametocheck_end + 1,l2+1);
            }
            strcpy(nameout,resolved);
            next_name = TRUE;
          }
        }
      }
      if (!next_name) {
        *nametocheck_end = saved_char;
        nametocheck = nametocheck_end;
        if (*nametocheck) nametocheck++;
      }
    } while (!next_name && *nametocheck);
    if (!(--try_counter)) return FALSE;
  } while (next_name);
  return TRUE;
}
