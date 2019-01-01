/* Utility and Unix shadow routines for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1994, 1995, 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         7-29-94
*/


#include <malloc.h>
#include <stddef.h> /* for offsetof */
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/utime.h>

/* must include CRT headers *before* config.h */
#include "config.h"
#undef access
#undef chdir
#undef chmod
#undef creat
#undef ctime
#undef fopen
#undef link
#undef mkdir
#undef mktemp
#undef open
#undef rename
#undef rmdir
#undef unlink

#undef close
#undef dup
#undef dup2
#undef pipe
#undef read
#undef write

#undef strerror

#include "lisp.h"

#include <pwd.h>

#include <windows.h>

#ifdef MEADOW
#include <tchar.h>
#include "mw32reg.h"
#include "mw32sync.h"
#endif
#include "charset.h"
#include "coding.h"
extern Lisp_Object Vfile_name_coding_system;

#include <shellapi.h>

#ifdef __GNUC__
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#endif

#ifdef HAVE_SOCKETS	/* TCP connection support, if kernel can do it */
#include <sys/socket.h>
#undef socket
#undef bind
#undef connect
#undef htons
#undef ntohs
#undef inet_addr
#undef gethostname
#undef gethostbyname
#undef getservbyname
#undef shutdown
#endif

#include "w32.h"
#include "ndir.h"
/* Meadow don't use w32heap.h */
#ifndef MEADOW
#include "w32heap.h"
#endif
#include "systime.h"

#undef min
#undef max
#define min(x, y) (((x) < (y)) ? (x) : (y))
#define max(x, y) (((x) > (y)) ? (x) : (y))

#ifndef CSTR_EQUAL
#define CSTR_EQUAL 2
#endif

extern Lisp_Object Vw32_downcase_file_names;
extern Lisp_Object Vw32_generate_fake_inodes;
extern Lisp_Object Vw32_get_true_file_attributes;
#ifdef MEADOW
extern Lisp_Object Vw32_get_true_file_link_count;
#endif

/* Array of W32 filename special characters */
static const int
w32_filename_illegal_characters[] = {'*', '?', '<', '>', '\"', 0};
static const int
w32_filename_special_characters[] = {'*', '?', '<', '>', '\"', '\\', '/', 0};

#ifndef MEADOW
extern Lisp_Object Vw32_num_mouse_buttons;
#endif

extern void w32_abort ();


/* Equivalent of strerror for W32 error codes.  */
char *
w32_strerror (int error_no)
{
  static char buf[500];

  if (error_no == 0)
    error_no = GetLastError ();

  buf[0] = '\0';
  if (!FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, NULL,
		      error_no,
		      0, /* choose most suitable language */
		      buf, sizeof (buf), NULL))
    sprintf (buf, "w32 error %u", error_no);
  return buf;
}

static char startup_dir[MAXPATHLEN];

/* Get the current working directory.  */
char *
getwd (char *dir)
{
  void dostounix_filename (char *);
#if 0
  if (GetCurrentDirectory (MAXPATHLEN, dir) > 0)
    return dir;
  return NULL;
#else
  /* Emacs doesn't actually change directory itself, and we want to
     force our real wd to be where emacs.exe is to avoid unnecessary
     conflicts when trying to rename or delete directories.  */
  strcpy (dir, startup_dir);
  dostounix_filename(dir);
  return dir;
#endif
}

#ifndef HAVE_SOCKETS
/* Emulate gethostname.  */
int
gethostname (char *buffer, int size)
{
  /* NT only allows small host names, so the buffer is 
     certainly large enough.  */
  return !GetComputerName (buffer, &size);
}
#endif /* HAVE_SOCKETS */

/* Emulate getloadavg.  */
int
getloadavg (double loadavg[], int nelem)
{
  int i;

  /* A faithful emulation is going to have to be saved for a rainy day.  */
  for (i = 0; i < nelem; i++) 
    {
      loadavg[i] = 0.0;
    }
  return i;
}

/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char the_passwd_name[PASSWD_FIELD_SIZE];
static char the_passwd_passwd[PASSWD_FIELD_SIZE];
static char the_passwd_gecos[PASSWD_FIELD_SIZE];
static char the_passwd_dir[PASSWD_FIELD_SIZE];
static char the_passwd_shell[PASSWD_FIELD_SIZE];

static struct passwd the_passwd = 
{
  the_passwd_name,
  the_passwd_passwd,
  0,
  0,
  0,
  the_passwd_gecos,
  the_passwd_dir,
  the_passwd_shell,
};

int 
getuid () 
{ 
  return the_passwd.pw_uid;
}

int 
geteuid () 
{ 
  /* I could imagine arguing for checking to see whether the user is
     in the Administrators group and returning a UID of 0 for that
     case, but I don't know how wise that would be in the long run.  */
  return getuid (); 
}

int 
getgid () 
{ 
  return the_passwd.pw_gid;
}

int 
getegid () 
{ 
  return getgid ();
}

struct passwd *
getpwuid (int uid)
{
  if (uid == the_passwd.pw_uid)
    return &the_passwd;
  return NULL;
}

struct passwd *
getpwnam (char *name)
{
  struct passwd *pw;
  
  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (stricmp (name, pw->pw_name))
    return NULL;

  return pw;
}

void
init_user_info ()
{
  /* Find the user's real name by opening the process token and
     looking up the name associated with the user-sid in that token.

     Use the relative portion of the identifier authority value from
     the user-sid as the user id value (same for group id using the
     primary group sid from the process token). */

  char            user_sid[256], name[256], domain[256];
  DWORD           length = sizeof (name), dlength = sizeof (domain), trash;
  HANDLE          token = NULL;
  SID_NAME_USE    user_type;

  if (OpenProcessToken (GetCurrentProcess (), TOKEN_QUERY, &token)
      && GetTokenInformation (token, TokenUser,
			      (PVOID) user_sid, sizeof (user_sid), &trash)
      && LookupAccountSid (NULL, *((PSID *) user_sid), name, &length,
			   domain, &dlength, &user_type))
    {
      strcpy (the_passwd.pw_name, name);
      /* Determine a reasonable uid value. */
      if (stricmp ("administrator", name) == 0)
	{
	  the_passwd.pw_uid = 0;
	  the_passwd.pw_gid = 0;
	}
      else
	{
	  SID_IDENTIFIER_AUTHORITY * pSIA;

	  pSIA = GetSidIdentifierAuthority (*((PSID *) user_sid));
	  /* I believe the relative portion is the last 4 bytes (of 6)
	     with msb first. */
	  the_passwd.pw_uid = ((pSIA->Value[2] << 24) +
			       (pSIA->Value[3] << 16) +
			       (pSIA->Value[4] << 8)  +
			       (pSIA->Value[5] << 0));
	  /* restrict to conventional uid range for normal users */
	  the_passwd.pw_uid = the_passwd.pw_uid % 60001;

	  /* Get group id */
	  if (GetTokenInformation (token, TokenPrimaryGroup,
				   (PVOID) user_sid, sizeof (user_sid), &trash))
	    {
	      SID_IDENTIFIER_AUTHORITY * pSIA;

	      pSIA = GetSidIdentifierAuthority (*((PSID *) user_sid));
	      the_passwd.pw_gid = ((pSIA->Value[2] << 24) +
				   (pSIA->Value[3] << 16) +
				   (pSIA->Value[4] << 8)  +
				   (pSIA->Value[5] << 0));
	      /* I don't know if this is necessary, but for safety... */
	      the_passwd.pw_gid = the_passwd.pw_gid % 60001;
	    }
	  else
	    the_passwd.pw_gid = the_passwd.pw_uid;
	}
    }
  /* If security calls are not supported (presumably because we
       are running under Windows 95), fallback to this. */
  else if (GetUserName (name, &length))
    {
      strcpy (the_passwd.pw_name, name);
      if (stricmp ("administrator", name) == 0)
	the_passwd.pw_uid = 0;
      else
	the_passwd.pw_uid = 123;
      the_passwd.pw_gid = the_passwd.pw_uid;
    }
  else
    {
      strcpy (the_passwd.pw_name, "unknown");
      the_passwd.pw_uid = 123;
      the_passwd.pw_gid = 123;
    }

  /* Ensure HOME and SHELL are defined. */
  if (getenv ("HOME") == NULL)
    abort ();
  if (getenv ("SHELL") == NULL)
    abort ();

  /* Set dir and shell from environment variables. */
  strcpy (the_passwd.pw_dir, getenv ("HOME"));
  strcpy (the_passwd.pw_shell, getenv ("SHELL"));

  if (token)
    CloseHandle (token);
}

int
random ()
{
  /* rand () on NT gives us 15 random bits...hack together 30 bits.  */
  return ((rand () << 15) | rand ());
}

void
srandom (int seed)
{
  srand (seed);
}

/* Check if str contains chars.
   chars must be an array of characters (not string)
   that is terminated by 0. */
LPTSTR is_string_contains_chars (LPCTSTR str, const int *chars)
{
  LPTSTR sp = (LPTSTR) str;
  int *cp;

  for (;*sp;sp = CharNext (sp))
    {
      for (cp = (int *) chars;*cp;cp++)
	{
	  if (*sp == *cp)
	    return sp;
	}
    }

  return NULL;
}

/* Normalize filename by converting all path separators to
   the specified separator.  Also conditionally convert upper
   case path name components to lower case.  */
static void
normalize_filename (LPTSTR fp, TCHAR path_sep)
{
  LPTSTR n_fp;
  TCHAR next_char;

  if (lstrlen (fp) > MAX_PATH) return;

  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so filenames can be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if ((*fp >= 'A' && *fp <= 'Z') 
      && (*(CharNext (fp)) == ':'))
    {
      next_char = tolower (*fp);
      *fp = next_char;
      fp = CharNext (fp);
      fp = CharNext (fp);
    }

  if (NILP (Vw32_downcase_file_names))
    {
      for (;*fp;fp = n_fp)
	{
	  n_fp = CharNext (fp);
	  if (IS_DIRECTORY_SEP (*fp))
	    *fp = path_sep;
	}
    }
  else
    {
      LPTSTR pelem, element_start;
      TCHAR path_element[MAX_PATH + 1];

      element_start = fp;
      for (pelem = path_element;*fp;)
	{
	  if (IS_DIRECTORY_SEP (*fp))
	    {
	      if (element_start && (pelem > path_element))
		memcpy (element_start, path_element,
			(pelem - path_element) * sizeof(TCHAR));

	      *fp = path_sep;
	      pelem = path_element;
	      element_start = fp;
	    }
	  else if (*fp == ':')
	    {
	      if (element_start && (pelem > path_element))
		memcpy (element_start, path_element,
			(pelem - path_element) * sizeof(TCHAR));

	      pelem = path_element;
	      element_start = fp;
	    }
	  else if (*fp >= 'A' && *fp <= 'Z')
	    {
	      *pelem = tolower (*fp);
	      pelem++, fp++;
	    }
	  else if (*fp >= 'a' && *fp <= 'z')
	    element_start = NULL;

	  n_fp = CharNext (fp);
	  while (n_fp > fp) *pelem++ = *fp++;
	}
    }

  return;
}

void
unixtodos_argument (p, ep, h2sp, qp, s2isp)
     register char *p;
     int ep, h2sp, qp, s2isp;
{
  int qf;
  char *o;

  qf = 0;
  o = p;
  while (-1)
    {
      if (*p == '\\' && 
	  ep)
	{
	  p++;
	  *o = *p;
	  if (!*p) break;
	  o++;
	  p++;
	  continue;
	}
      if (qf)
	{
	  if (*p == qf) 
	    {
	      qf = 0;
	      p++;
	      continue;
	    }
	  else *o = *p;
	}
      else
	{
	  switch (*p)
	    {
	    case '/':
	      if (!s2isp)
		*o = *p;
	      else
		*o = '\\';
	      break;
	    case '-':
	      if (!h2sp)
		*o = *p;
	      else
		*o = '/';
	      break;

	    case '\'':
	    case '"':
	      if (!qp)
		*o = *p;
	      else
		{
		  qf = *p;
		  p++;
		  continue;
		}
	      break;
	    default :
	      *o = *p;
	      break;
	    }
	}
      if (!*p)
	{
	  *o = *p;
	  break;
	}
      o++;
      p++;
    }
}


/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (p)
     register char *p;
{
  normalize_filename (p, '/');
}

/* Destructively turn slashes into backslashes.  */
void
unixtodos_filename (p)
     register char *p;
{
  normalize_filename (p, '\\');
}

/* Remove all CR's that are followed by a LF.
   (From msdos.c...probably should figure out a way to share it,
   although this code isn't going to ever change.)  */
int
crlf_to_lf (n, buf)
     register int n;
     register unsigned char *buf;
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

/* Parse the root part of file name, if present.  Return byte length and
    optionally store pointer to char after root.  */
static int
parse_root (LPCTSTR name, LPCTSTR *pPath)
{
  TCHAR firstchar;
  TCHAR secondchar;
  LPTSTR fp;

  LPCTSTR start = name;

  if (name == NULL)
    return 0;

  firstchar = *name;
  fp = CharNext (name);
  secondchar = *fp;
  fp = CharNext (fp);

  /* find the root name of the volume if given */
  if (IsCharAlpha (firstchar) && secondchar == ':')
    {
      if (IS_DIRECTORY_SEP (*fp))
	fp = CharNext (fp);

      name = fp;
    }
  else if (IS_DIRECTORY_SEP (firstchar) && IS_DIRECTORY_SEP (secondchar))
    {
      int slashes = 2;

      for (;*fp;fp = CharNext (fp))
        {
	  if (IS_DIRECTORY_SEP (*fp) && (slashes == 0))
	    break;
	  slashes--;
	}
      if (IS_DIRECTORY_SEP (*fp))
	fp = CharNext (fp);

      name = fp;
    }

  if (pPath)
    *pPath = name;

  return (name - start) / sizeof(TCHAR);
}

/* Get long base name for name; name is assumed to be absolute.  */
static int
get_long_basename (LPTSTR name, LPTSTR buf, int size)
{
  WIN32_FIND_DATA find_data;
  HANDLE dir_handle;
  int len = 0;

  /* must be valid filename, no wild cards or other illegal characters */
  if (is_string_contains_chars (name, w32_filename_illegal_characters))
    return 0;

  dir_handle = FindFirstFile (name, &find_data);
  if (dir_handle != INVALID_HANDLE_VALUE)
    {
      if ((len = lstrlen (find_data.cFileName)) < size)
	memcpy (buf, find_data.cFileName, sizeof(TCHAR) * (len + 1));
      else
	len = 0;
      FindClose (dir_handle);
    }
  return len;
}

/* Get long name for file, if possible (assumed to be absolute).  */
BOOL
w32_get_long_filename (LPTSTR name, LPTSTR buf, int size)
{
  char * o = buf;
  char * p;
  char * q;
  TCHAR full[MAX_PATH];
  int len;

  len = lstrlen (name);
  if (len >= MAX_PATH)
    return FALSE;

  /* Use local copy for destructive modification.  */
  memcpy (full, name, sizeof(TCHAR) * (len + 1));
  unixtodos_filename (full);

  /* Copy root part verbatim.  */
  len = parse_root (full, &p);
  memcpy (o, full, sizeof(TCHAR) * len);
  o += len;
  *o = '\0';
  size -= len;

  while (p != NULL && *p)
    {
      q = p;

      for(;;p = CharNext (p))
	{
	  if (!(*p) || IS_DIRECTORY_SEP(*p))
	    break;
	}

      if (*p) *p = '\0';
      else p = NULL;

      len = get_long_basename (full, o, size);
      if (len > 0)
	{
	  o += len;
	  size -= len;
	  if (p != NULL)
	    {
	      *p = '\\';
	      p = CharNext (p);

	      if (size < 2)
		return FALSE;

	      *o = '\\';
	      o = CharNext (o);

	      size--;
	      *o = '\0';
	    }
	}
      else
	return FALSE;
    }

  return TRUE;
}

int
is_unc_volume (LPCTSTR filename)
{
  LPCTSTR ptr = filename;
  TCHAR firstchar;
  TCHAR secondchar;

  firstchar = *ptr;
  ptr = CharNext (ptr);
  secondchar = *ptr;
  ptr = CharNext (ptr);
  
  if (!IS_DIRECTORY_SEP (firstchar) || !IS_DIRECTORY_SEP (secondchar) || !(*ptr))
    return 0;

  if (is_string_contains_chars (ptr, w32_filename_special_characters))
    return 0;

  return 1;
}

/* Routines that are no-ops on NT but are defined to get Emacs to compile.  */

int 
sigsetmask (int signal_mask) 
{ 
  return 0;
}

int 
sigmask (int sig) 
{ 
  return 0;
}

int 
sigblock (int sig) 
{ 
  return 0;
}

int 
sigunblock (int sig) 
{ 
  return 0;
}

int 
setpgrp (int pid, int gid) 
{ 
  return 0;
}

int 
alarm (int seconds) 
{ 
  return 0;
}

void 
unrequest_sigio (void) 
{ 
  return;
}

void
request_sigio (void) 
{ 
  return;
}

#ifndef MEADOW

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

LPBYTE 
w32_get_resource (key, lpdwtype)
    char *key;
    LPDWORD lpdwtype;
{
  LPBYTE lpvalue;
  HKEY hrootkey = NULL;
  DWORD cbData;
  BOOL ok = FALSE;
  
  /* Check both the current user and the local machine to see if 
     we have any resources.  */
  
  if (RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS 
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL 
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
	  return (lpvalue);
	}

      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;
	
      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
	  return (lpvalue);
	}
	
      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  return (NULL);
}
#else /* not MEADOW */
#endif

char *get_emacs_configuration (void);
extern Lisp_Object Vsystem_configuration;

void
init_time_zone_info ()
{
  char *tzstr;
  tzstr = getenv ("TZ");
  if (!tzstr)
    {
      TIME_ZONE_INFORMATION tzi;
      DWORD ret;
      char tmp[256];
      ret = GetTimeZoneInformation (&tzi);
      if (ret == 0xFFFFFFFF)
	{
	  putenv ("TZ=GMT+0");               /* can't help setting this. */
 	}
      else 
	{
	  int biashour, biasmin, dlhour, dlmin;

	  biashour = tzi.Bias / 60;
	  biasmin = (tzi.Bias >= 0) ? tzi.Bias : -tzi.Bias;
	  biasmin %= 60;
	  dlmin = tzi.Bias + tzi.DaylightBias;
	  dlhour = dlmin / 60;
	  dlmin = (dlmin >= 0) ? dlmin : -dlmin;
	  dlmin %= 60;

	  if (ret == TIME_ZONE_ID_UNKNOWN)
	    {
	      if (tzi.DaylightBias != 0)
		{
		  sprintf (tmp, "TZ=LMT%+d:%02dLDT%+d:%02d",
			   biashour, biasmin,
			   dlhour, dlmin);
		}
	      else
		{
		  sprintf (tmp, "TZ=LMT%+d:%02d",
			   biashour, biasmin);
		}
	      putenv (tmp);
	    }
	  else if (ret == TIME_ZONE_ID_STANDARD)
	    {
#if 0
	      sprintf (tmp, "TZ=%s%+d:%02d",
		       tzi.StandardName,
		       biashour, biasmin);
#else
	      sprintf (tmp, "TZ=LMT%+d:%02d",
		       biashour, biasmin);
#endif
	      putenv (tmp);
	    }
	  else if (ret == TIME_ZONE_ID_DAYLIGHT)
	    {
#if 0
	      sprintf (tmp, "TZ=%s%+d:%02d%s%+d:%02d",
		       tzi.StandardName,
		       biashour, biasmin,
		       tzi.DaylightName,
		       dlhour, dlmin);
#else
	      sprintf (tmp, "TZ=LMT%+d:%02dLDT%+d:%02d",
		       biashour, biasmin,
		       dlhour, dlmin);
#endif
	      putenv (tmp);
	    }
	}
    }
  _tzset ();
}

void
init_environment (char **argv)
{
  static const char * const tempdirs[] = {
    "$TMPDIR", "$TEMP", "$TMP", "c:/"
  };
  int i;
  const int imax = sizeof (tempdirs) / sizeof (tempdirs[0]);

  /* Make sure they have a usable $TMPDIR.  Many Emacs functions use
     temporary files and assume "/tmp" if $TMPDIR is unset, which
     will break on DOS/Windows.  Refuse to work if we cannot find
     a directory, not even "c:/", usable for that purpose.  */
  for (i = 0; i < imax ; i++)
    {
      const char *tmp = tempdirs[i];

      if (*tmp == '$')
	tmp = getenv (tmp + 1);
      /* Note that `access' can lie to us if the directory resides on a
	 read-only filesystem, like CD-ROM or a write-protected floppy.
	 The only way to be really sure is to actually create a file and
	 see if it succeeds.  But I think that's too much to ask.  */
      if (tmp && sys_access (tmp, D_OK) == 0)
	{
	  char * var = alloca (strlen (tmp) + 8);
	  sprintf (var, "TMPDIR=%s", tmp);
	  _putenv (strdup (var));
	  break;
	}
    }
  if (i >= imax)
    cmd_error_internal
      (Fcons (Qerror,
	      Fcons (build_string ("no usable temporary directories found!!"),
		     Qnil)),
       "While setting TMPDIR: ");

  /* Check for environment variables and use registry settings if they
     don't exist.  Fallback on default values where applicable.  */
  {
    int i;
    LPBYTE lpval;
    DWORD dwType;
    char locale_name[32];

    static struct env_entry
    {
      char * name;
      char * def_value;
    } env_vars[] = 
    {
      {"HOME", "C:/"},
      {"PRELOAD_WINSOCK", NULL},
      {"emacs_dir", "C:/emacs"},
      {"EMACSLOADPATH", "%emacs_dir%/site-lisp;%emacs_dir%/../site-lisp;%emacs_dir%/packages/lisp;%emacs_dir%/lisp;%emacs_dir%/leim"},
      {"SHELL", "%emacs_dir%/bin/cmdproxy.exe"},
      {"EMACSDATA", "%emacs_dir%/etc"},
      {"EMACSPATH", "%emacs_dir%/bin"},
      {"EMACSLOCKDIR", "%emacs_dir%/lock"},
      /* We no longer set INFOPATH because Info-default-directory-list
	 is then ignored.  */
      /*  {"INFOPATH", "%emacs_dir%/info"},  */
#ifdef MEADOW
      {"INFOPATH", "%emacs_dir%/info"},
      {"BITMAPPATH", "%emacs_dir%/bitmap"},
#endif
      {"EMACSDOC", "%emacs_dir%/etc"},
      {"TERM", "cmd"},
      {"LANG", NULL},
    };

  /* Get default locale info and use it for LANG.  */
  if (GetLocaleInfo (LOCALE_USER_DEFAULT,
                     LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
                     locale_name, sizeof (locale_name)))
    {
      for (i = 0; i < (sizeof (env_vars) / sizeof (env_vars[0])); i++)
        {
          if (strcmp (env_vars[i].name, "LANG") == 0)
            {
              env_vars[i].def_value = locale_name;
              break;
            }
        }
    }

#define SET_ENV_BUF_SIZE (4 * MAX_PATH)	/* to cover EMACSLOADPATH */

    /* Treat emacs_dir specially: set it unconditionally based on our
       location, if it appears that we are running from the bin subdir
       of a standard installation.  */
    {
      char *p;
      char modname[MAX_PATH];

      if (!GetModuleFileName (NULL, modname, MAX_PATH))
	abort ();
      if ((p = strrchr (modname, '\\')) == NULL)
	abort ();
      *p = 0;

      if ((p = strrchr (modname, '\\')) && stricmp (p, "\\bin") == 0)
	{
	  char buf[SET_ENV_BUF_SIZE];

	  *p = 0;
	  for (p = modname; *p; p++)
	    if (*p == '\\') *p = '/';
		  
	  _snprintf (buf, sizeof(buf)-1, "emacs_dir=%s", modname);
	  _putenv (strdup (buf));
	}
    }

    for (i = 0; i < (sizeof (env_vars) / sizeof (env_vars[0])); i++)
      {
	if (!getenv (env_vars[i].name))
	  {
	    int dont_free = 0;

#ifdef MEADOW
	    if ((lpval = mw32_get_env_resource (env_vars[i].name, &dwType)) == NULL)
#else
	    if ((lpval = w32_get_resource (env_vars[i].name, &dwType)) == NULL)
#endif
	      {
		lpval = env_vars[i].def_value;
		dwType = REG_EXPAND_SZ;
		dont_free = 1;
	      }

	    if (lpval)
	      {
		if (dwType == REG_EXPAND_SZ)
		  {
		    char buf1[SET_ENV_BUF_SIZE], buf2[SET_ENV_BUF_SIZE];

		    ExpandEnvironmentStrings ((LPSTR) lpval, buf1, sizeof(buf1));
		    _snprintf (buf2, sizeof(buf2)-1, "%s=%s", env_vars[i].name, buf1);
		    _putenv (strdup (buf2));
		  }
		else if (dwType == REG_SZ)
		  {
		    char buf[SET_ENV_BUF_SIZE];
		  
		    _snprintf (buf, sizeof(buf)-1, "%s=%s", env_vars[i].name, lpval);
		    _putenv (strdup (buf));
		  }

		if (!dont_free)
		  xfree (lpval);
	      }
	  }
      }
  }

  /* Rebuild system configuration to reflect invoking system.  */
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  /* Initialize time zone infomation */
  init_time_zone_info ();

  /* Another special case: on NT, the PATH variable is actually named
     "Path" although cmd.exe (perhaps NT itself) arranges for
     environment variable lookup and setting to be case insensitive.
     However, Emacs assumes a fully case sensitive environment, so we
     need to change "Path" to "PATH" to match the expectations of
     various elisp packages.  We do this by the sneaky method of
     modifying the string in the C runtime environ entry.

     The same applies to COMSPEC.  */
  {
    char ** envp;

    for (envp = environ; *envp; envp++)
      if (_strnicmp (*envp, "PATH=", 5) == 0)
	memcpy (*envp, "PATH=", 5);
      else if (_strnicmp (*envp, "COMSPEC=", 8) == 0)
	memcpy (*envp, "COMSPEC=", 8);
  }

  /* Remember the initial working directory for getwd, then make the
     real wd be the location of emacs.exe to avoid conflicts when
     renaming or deleting directories.  (We also don't call chdir when
     running subprocesses for the same reason.)  */
  if (!GetCurrentDirectory (MAXPATHLEN, startup_dir))
    abort ();

  {
    char *p, *q;
    static TCHAR modname[MAX_PATH];
    TCHAR curdir[MAX_PATH];

    if (!GetModuleFileName (NULL, modname, MAX_PATH))
      abort ();

    /* Find out the last directory separator */
    for (p = modname;*p;p = CharNext (p))
      if (IS_DIRECTORY_SEP (*p)) q = p;
    if (!q) abort ();

    memcpy (curdir, modname, (q - modname) * sizeof(TCHAR));
    curdir[q - modname] = '\0';

    SetCurrentDirectory (curdir);

    /* Ensure argv[0] has the full path to Emacs.  */
    argv[0] = modname; 
  }

#ifndef MEADOW
  /* Determine if there is a middle mouse button, to allow parse_button
     to decide whether right mouse events should be mouse-2 or
     mouse-3. */
  XSETINT (Vw32_num_mouse_buttons, GetSystemMetrics (SM_CMOUSEBUTTONS));
#endif

  init_user_info ();
}

char *
emacs_root_dir (void)
{
  static char root_dir[FILENAME_MAX];
  const char *p;

  p = getenv ("emacs_dir");
  if (p == NULL)
    abort ();
  strcpy (root_dir, p);
  root_dir[parse_root (root_dir, NULL)] = '\0';
  dostounix_filename (root_dir);
  return root_dir;
}

/* We don't have scripts to automatically determine the system configuration
   for Emacs before it's compiled, and we don't want to have to make the
   user enter it, so we define EMACS_CONFIGURATION to invoke this runtime
   routine.  */

char *
get_emacs_configuration (void)
{
  char *arch, *oem, *os;
  int build_num;
  static char configuration_buffer[32];
  OSVERSIONINFO osinfo;

  osinfo.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
  GetVersionEx (&osinfo);

  /* Determine the processor type.  */
  switch (get_processor_type ()) 
    {

#ifdef PROCESSOR_INTEL_386
    case PROCESSOR_INTEL_386:
    case PROCESSOR_INTEL_486:
    case PROCESSOR_INTEL_PENTIUM:
      arch = "i386";
      break;
#endif

#ifdef PROCESSOR_INTEL_860
    case PROCESSOR_INTEL_860:
      arch = "i860";
      break;
#endif

#ifdef PROCESSOR_MIPS_R2000
    case PROCESSOR_MIPS_R2000:
    case PROCESSOR_MIPS_R3000:
    case PROCESSOR_MIPS_R4000:
      arch = "mips";
      break;
#endif

#ifdef PROCESSOR_ALPHA_21064
    case PROCESSOR_ALPHA_21064:
      arch = "alpha";
      break;
#endif

    default:
      arch = "unknown";
      break;
    }

  /* Use the OEM field to reflect the compiler/library combination.  */
#ifdef _MSC_VER
#define COMPILER_NAME	"msvc"
#else
#ifdef __GNUC__
#define COMPILER_NAME	"mingw"
#else
#define COMPILER_NAME	"unknown"
#endif
#endif
  oem = COMPILER_NAME;

  switch (osinfo.dwPlatformId) {
  case VER_PLATFORM_WIN32_NT:
    os = "nt";
    build_num = osinfo.dwBuildNumber;
    break;
  case VER_PLATFORM_WIN32_WINDOWS:
    if (osinfo.dwMinorVersion == 0) {
      os = "windows95";
    } else {
      os = "windows98";
    }
    build_num = LOWORD (osinfo.dwBuildNumber);
    break;
  case VER_PLATFORM_WIN32s:
    /* Not supported, should not happen. */
    os = "windows32s";
    build_num = LOWORD (osinfo.dwBuildNumber);
    break;
  default:
    os = "unknown";
    build_num = 0;
    break;
  }

  if (osinfo.dwPlatformId == VER_PLATFORM_WIN32_NT) {
    sprintf (configuration_buffer, "%s-%s-%s%d.%d.%d", arch, oem, os,
	     get_w32_major_version (), get_w32_minor_version (), build_num);
  } else {
    sprintf (configuration_buffer, "%s-%s-%s.%d", arch, oem, os, build_num);
  }

  return configuration_buffer;
}

char *
get_emacs_configuration_options (void)
{
  static char options_buffer[256];

/* Work out the effective configure options for this build.  */
#ifdef _MSC_VER
#define COMPILER_VERSION	"--with-msvc (%d.%02d)", _MSC_VER / 100, _MSC_VER % 100
#else
#ifdef __GNUC__
#define COMPILER_VERSION	"--with-gcc (%d.%d)", __GNUC__, __GNUC_MINOR__
#else
#define COMPILER_VERSION	""
#endif
#endif

  sprintf (options_buffer, COMPILER_VERSION);
#ifdef EMACSDEBUG
  strcat (options_buffer, " --no-opt");
#endif
#ifdef USER_CFLAGS
  strcat (options_buffer, " --cflags");
  strcat (options_buffer, USER_CFLAGS);
#endif
#ifdef USER_LDFLAGS
  strcat (options_buffer, " --ldflags");
  strcat (options_buffer, USER_LDFLAGS);
#endif
  return options_buffer;
}


#include <sys/timeb.h>

/* Emulate gettimeofday (Ulrich Leodolter, 1/11/95).  */
void 
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  struct timeb tb;
  ftime (&tb);

  tv->tv_sec = tb.time;
  tv->tv_usec = tb.millitm * 1000L;
  if (tz) 
    {
      tz->tz_minuteswest = tb.timezone;	/* minutes west of Greenwich  */
      tz->tz_dsttime = tb.dstflag;	/* type of dst correction  */
    }
}

/* ------------------------------------------------------------------------- */
/* IO support and wrapper functions for W32 API. */
/* ------------------------------------------------------------------------- */

/* Place a wrapper around the MSVC version of ctime.  It returns NULL
   on network directories, so we handle that case here.  
   (Ulrich Leodolter, 1/11/95).  */
char *
sys_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : "Sun Jan 01 00:00:00 1970");
}

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
sys_sleep (int seconds)
{
  Sleep (seconds * 1000);
}

/* Internal MSVC functions for low-level descriptor munging */
extern int __cdecl _set_osfhnd (int fd, long h);
extern int __cdecl _free_osfhnd (int fd);

/* parallel array of private info on file handles */
filedesc fd_info [ MAXDESC ];

typedef struct volume_info_data {
  struct volume_info_data * next;

  /* time when info was obtained */
  DWORD     timestamp;

  /* actual volume info */
  char *    root_dir;
  DWORD     serialnum;
  DWORD     maxcomp;
  DWORD     flags;
  char *    name;
  char *    type;
} volume_info_data;

/* Global referenced by various functions.  */
static volume_info_data volume_info;

/* Vector to indicate which drives are local and fixed (for which cached
   data never expires).  */
static BOOL fixed_drives[26];

/* Consider cached volume information to be stale if older than 10s,
   at least for non-local drives.  Info for fixed drives is never stale.  */
#define DRIVE_INDEX( c ) ( (c) <= 'Z' ? (c) - 'A' : (c) - 'a' )
#define VOLINFO_STILL_VALID( root_dir, info )		\
  ( ( isalpha (root_dir[0]) &&				\
      fixed_drives[ DRIVE_INDEX (root_dir[0]) ] )	\
    || GetTickCount () - info->timestamp < 10000 )

/* Cache support functions.  */

/* Simple linked list with linear search is sufficient.  */
static volume_info_data *volume_cache = NULL;

static volume_info_data *
lookup_volume_info (char * root_dir)
{
  volume_info_data * info;

  for (info = volume_cache; info; info = info->next)
    if (stricmp (info->root_dir, root_dir) == 0)
      break;
  return info;
}

static void
add_volume_info (char * root_dir, volume_info_data * info)
{
  info->root_dir = xstrdup (root_dir);
  info->next = volume_cache;
  volume_cache = info;
}


/* Wrapper for GetVolumeInformation, which uses caching to avoid
   performance penalty (~2ms on 486 for local drives, 7.5ms for local
   cdrom drive, ~5-10ms or more for remote drives on LAN).  */
volume_info_data *
GetCachedVolumeInformation (char * root_dir)
{
  volume_info_data * info;
  char default_root[ MAX_PATH ];

  /* NULL for root_dir means use root from current directory.  */
  if (root_dir == NULL)
    {
      if (GetCurrentDirectory (MAX_PATH, default_root) == 0)
	return NULL;
      parse_root (default_root, &root_dir);
      *root_dir = 0;
      root_dir = default_root;
    }

  /* Local fixed drives can be cached permanently.  Removable drives
     cannot be cached permanently, since the volume name and serial
     number (if nothing else) can change.  Remote drives should be
     treated as if they are removable, since there is no sure way to
     tell whether they are or not.  Also, the UNC association of drive
     letters mapped to remote volumes can be changed at any time (even
     by other processes) without notice.
   
     As a compromise, so we can benefit from caching info for remote
     volumes, we use a simple expiry mechanism to invalidate cache
     entries that are more than ten seconds old.  */

#if 0
  /* No point doing this, because WNetGetConnection is even slower than
     GetVolumeInformation, consistently taking ~50ms on a 486 (FWIW,
     GetDriveType is about the only call of this type which does not
     involve network access, and so is extremely quick).  */

  /* Map drive letter to UNC if remote. */
  if ( isalpha( root_dir[0] ) && !fixed[ DRIVE_INDEX( root_dir[0] ) ] )
    {
      char remote_name[ 256 ];
      char drive[3] = { root_dir[0], ':' };

      if (WNetGetConnection (drive, remote_name, sizeof (remote_name))
	  == NO_ERROR)
	/* do something */ ;
    }
#endif

  info = lookup_volume_info (root_dir);

  if (info == NULL || ! VOLINFO_STILL_VALID (root_dir, info))
  {
    char  name[ 256 ];
    DWORD serialnum;
    DWORD maxcomp;
    DWORD flags;
    char  type[ 256 ];

    /* Info is not cached, or is stale. */
    if (!GetVolumeInformation (root_dir,
			       name, sizeof (name),
			       &serialnum,
			       &maxcomp,
			       &flags,
			       type, sizeof (type)))
      return NULL;

    /* Cache the volume information for future use, overwriting existing
       entry if present.  */
    if (info == NULL)
      {
	info = (volume_info_data *) xmalloc (sizeof (volume_info_data));
	add_volume_info (root_dir, info);
      }
    else
      {
	xfree (info->name);
	xfree (info->type);
      }

    info->name = xstrdup (name);
    info->serialnum = serialnum;
    info->maxcomp = maxcomp;
    info->flags = flags;
    info->type = xstrdup (type);
    info->timestamp = GetTickCount ();
  }

  return info;
}

/* Get information on the volume where name is held; set path pointer to
   start of pathname in name (past UNC header\volume header if present).  */
int
get_volume_info (LPCTSTR name, LPCTSTR *pPath)
{
  TCHAR temp[MAX_PATH];
  LPTSTR rootname = NULL;  /* default to current volume */
  LPTSTR fp;
  TCHAR firstchar;
  TCHAR secondchar;
  volume_info_data * info;

  if (name == NULL)
    return FALSE;

  firstchar = *name;
  fp = CharNext (name);
  secondchar = *fp;

  /* find the root name of the volume if given */
  if (IsCharAlpha (firstchar) && secondchar == ':')
    {
      rootname = temp;
      temp[0] = firstchar;
      temp[1] = secondchar;
      temp[2] = '\\';
      temp[3] = 0;
      name = CharNext (fp);
    }
  else if (IS_DIRECTORY_SEP (firstchar) && IS_DIRECTORY_SEP (secondchar))
    {
      LPTSTR str = temp;
      LPTSTR name_end = (LPTSTR) name;
      TCHAR currentchar;
      int len;
      int slashes = 4;
      rootname = temp;

      while(*name_end)
	{
	  if (IS_DIRECTORY_SEP (*name_end) && --slashes == 0)
	    break;
	  name_end = CharNext (name_end);
	}

      len = name_end - name;
      memcpy (str, name, len);
      str += len;

      *str++ = '\\';
      *str = '\0';
      name = name_end;
    }

  if (pPath)
    *pPath = name;
    
  info = GetCachedVolumeInformation (rootname);
  if (info != NULL)
    {
      /* Set global referenced by other functions.  */
      volume_info = *info;
      return TRUE;
    }
  return FALSE;
}

/* Determine if volume is FAT format (ie. only supports short 8.3
   names); also set path pointer to start of pathname in name.  */
int
is_fat_volume (const char * name, const char ** pPath)
{
  if (get_volume_info (name, pPath))
    return (volume_info.maxcomp == 12);
  return FALSE;
}

/* Map filename to a legal 8.3 name if necessary. */
char *
map_w32_filename (LPCTSTR name, LPCTSTR *pPath)
{
  static TCHAR shortname[MAX_PATH];
  LPTSTR str = shortname;
  TCHAR c;
  LPTSTR path, next_name;
  LPCTSTR save_name = name;

  if (lstrlen (name) >= MAX_PATH)
    {
      /* Return a filename which will cause callers to fail.  */
      strcpy (shortname, "?");
      return shortname;
    }

  if (is_fat_volume (name, (const char **)&path)) /* truncate to 8.3 */
    {
      register int left = 8;	/* maximum number of chars in part */
      register int extn = 0;	/* extension added? */
      register int dots = 2;	/* maximum number of dots allowed */

      while (name < path)
	*str++ = *name++;	/* skip past UNC header */

      while (c = *name)
        {
	  next_name = CharNext (name);
	  switch ( c )
	    {
	    case '\\':
	    case '/':
	      *str++ = '\\';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case ':':
	      *str++ = ':';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case '.':
	      if ( dots )
	        {
		  /* Convert path components of the form .xxx to _xxx,
		     but leave . and .. as they are.  This allows .emacs
		     to be read as _emacs, for example.  */

		  if (! *name ||
		      *name == '.' ||
		      IS_DIRECTORY_SEP (*name))
		    {
		      *str++ = '.';
		      dots--;
		    }
		  else
		    {
		      *str++ = '_';
		      left--;
		      dots = 0;
		    }
		}
	      else if ( !extn )
	        {
		  *str++ = '.';
		  extn = 1;		/* we've got an extension */
		  left = 3;		/* 3 chars in extension */
		}
	      else
	        {
		  /* any embedded dots after the first are converted to _ */
		  *str++ = '_';
		}
	      break;
	    case '~':
	    case '#':			/* don't lose these, they're important */
	      if ( ! left )
		str[-1] = c;		/* replace last character of part */
	      /* FALLTHRU */
	    default:
	      if (left >= next_name - name)
	        {
		  TCHAR ch;

		  while (name < next_name)
		    {
		      ch = *name++;
		      if (ch >= 'A' && ch <= 'Z')
			*str++ = tolower (ch);
		      else
			*str++ = ch;
		      left--;
		    }
		  dots = 0;		/* started a path component */
		}
	      break;
	    }
	  name = next_name;
	}
      *str = '\0';
    }
  else
    {
      lstrcpy (shortname, name);
      unixtodos_filename (shortname);
    }

  if (pPath)
    *pPath = shortname + (path - save_name);

  return shortname;
}

static int
is_exec (const char * name)
{
  char * p = strrchr (name, '.');
  return
    (p != NULL
     && (stricmp (p, ".exe") == 0 ||
	 stricmp (p, ".com") == 0 ||
	 stricmp (p, ".bat") == 0 ||
	 stricmp (p, ".cmd") == 0));
}

/* Emulate the Unix directory procedures opendir, closedir, 
   and readdir.  We can't use the procedures supplied in sysdep.c,
   so we provide them here.  */

struct direct dir_static;       /* simulated directory contents */
static HANDLE dir_find_handle = INVALID_HANDLE_VALUE;
static int    dir_is_fat;
static TCHAR  dir_pathname[MAXPATHLEN+1];
static WIN32_FIND_DATA dir_find_data;

/* Support shares on a network resource as subdirectories of a read-only
   root directory. */
static HANDLE wnet_enum_handle = INVALID_HANDLE_VALUE;
HANDLE open_unc_volume (const char *);
char  *read_unc_volume (HANDLE, char *, int);
void   close_unc_volume (HANDLE);

DIR *
opendir (char *filename)
{
  extern void* malloc ();

  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we defer the open until read time.  */

  if (dir_find_handle != INVALID_HANDLE_VALUE)
    return NULL;
  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    return NULL;

  if (is_unc_volume (filename))
    {
      wnet_enum_handle = open_unc_volume (filename);
      if (wnet_enum_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }

  if (!(dirp = (DIR *) malloc (sizeof (DIR))))
    return NULL;

  dirp->dd_fd = 0;
  dirp->dd_loc = 0;
  dirp->dd_size = 0;

  lstrcpyn (dir_pathname, map_w32_filename (filename, NULL), MAXPATHLEN);
  dir_pathname[MAXPATHLEN] = '\0';
  dir_is_fat = is_fat_volume (filename, NULL);

  return dirp;
}

void
closedir (DIR *dirp)
{
  /* If we have a find-handle open, close it.  */
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    {
      FindClose (dir_find_handle);
      dir_find_handle = INVALID_HANDLE_VALUE;
    }
  else if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      close_unc_volume (wnet_enum_handle);
      wnet_enum_handle = INVALID_HANDLE_VALUE;
    }
  xfree ((char *) dirp);
}

struct direct *
readdir (DIR *dirp)
{
  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      if (!read_unc_volume (wnet_enum_handle, 
			      dir_find_data.cFileName, 
			      MAX_PATH))
	return NULL;
    }
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  else if (dir_find_handle == INVALID_HANDLE_VALUE)
    {
      TCHAR filename[MAXNAMLEN + 3];
      LPTSTR lastp;
      int ln;

      lstrcpy (filename, dir_pathname);
      ln = lstrlen (filename);
      lastp = CharPrev (filename, &filename[ln]);
      if (!IS_DIRECTORY_SEP (*lastp))
	lstrcat (filename, "\\");
      lstrcat (filename, "*");

      dir_find_handle = FindFirstFile (filename, &dir_find_data);

      if (dir_find_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }
  else
    {
      if (!FindNextFile (dir_find_handle, &dir_find_data))
	return NULL;
    }
  
  /* Emacs never uses this value, so don't bother making it match
     value returned by stat().  */
  dir_static.d_ino = 1;
  
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;
  
  dir_static.d_namlen = strlen (dir_find_data.cFileName);
  lstrcpy (dir_static.d_name, dir_find_data.cFileName);
  if (dir_is_fat)
    _tcslwr (dir_static.d_name);
  else if (!NILP (Vw32_downcase_file_names))
    {
      register char *p;
      for (p = dir_static.d_name; *p; p++)
	if (*p >= 'a' && *p <= 'z')
	  break;
      if (!*p)
	_tcslwr (dir_static.d_name);
    }
  
  return &dir_static;
}

HANDLE
open_unc_volume (const char *path)
{
  NETRESOURCE nr; 
  HANDLE henum;
  int result;

  nr.dwScope = RESOURCE_GLOBALNET; 
  nr.dwType = RESOURCETYPE_DISK; 
  nr.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER; 
  nr.dwUsage = RESOURCEUSAGE_CONTAINER; 
  nr.lpLocalName = NULL; 
  nr.lpRemoteName = map_w32_filename (path, NULL);
  nr.lpComment = NULL; 
  nr.lpProvider = NULL;   

  result = WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK,  
			RESOURCEUSAGE_CONNECTABLE, &nr, &henum);

  if (result == NO_ERROR)
    return henum;
  else
    return INVALID_HANDLE_VALUE;
}

char *
read_unc_volume (HANDLE henum, char *readbuf, int size)
{
  DWORD count;
  int result;
  DWORD bufsize = 512;
  char *buffer;
  char *ptr;

  count = 1;
  buffer = alloca (bufsize);
  result = WNetEnumResource (wnet_enum_handle, &count, buffer, &bufsize);
  if (result != NO_ERROR)
    return NULL;

  /* WNetEnumResource returns \\resource\share...skip forward to "share". */
  ptr = ((LPNETRESOURCE) buffer)->lpRemoteName;
  ptr += 2;
  while (*ptr && !IS_DIRECTORY_SEP (*ptr)) ptr = CharNext (ptr);
  ptr = CharNext (ptr);

  lstrcpyn (readbuf, ptr, size);
  return readbuf;
}

void
close_unc_volume (HANDLE henum)
{
  if (henum != INVALID_HANDLE_VALUE)
    WNetCloseEnum (henum);
}

DWORD
unc_volume_file_attributes (const char *path)
{
  HANDLE henum;
  DWORD attrs;

  henum = open_unc_volume (path);
  if (henum == INVALID_HANDLE_VALUE)
    return -1;

  attrs = FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_DIRECTORY;

  close_unc_volume (henum);

  return attrs;
}


/* Shadow some MSVC runtime functions to map requests for long filenames
   to reasonable short names if necessary.  This was originally added to
   permit running Emacs on NT 3.1 on a FAT partition, which doesn't support 
   long file names.  */

int
sys_access (const char * path, int mode)
{
  DWORD attributes;

  /* MSVC implementation doesn't recognize D_OK.  */
  path = map_w32_filename (path, NULL);
  if (is_unc_volume (path))
    {
      attributes = unc_volume_file_attributes (path);
      if (attributes == -1) {
	errno = EACCES;
	return -1;
      }
    }
  else if ((attributes = GetFileAttributes (path)) == -1)
    {
      /* Should try mapping GetLastError to errno; for now just indicate
	 that path doesn't exist.  */
      errno = EACCES;
      return -1;
    }
  if ((mode & X_OK) != 0 && !is_exec (path))
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & W_OK) != 0 && (attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & D_OK) != 0 && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

int
sys_chdir (const char * path)
{
  return _chdir (map_w32_filename (path, NULL));
}

int
sys_chmod (const char * path, int mode)
{
  return _chmod (map_w32_filename (path, NULL), mode);
}

int
sys_creat (const char * path, int mode)
{
  return _creat (map_w32_filename (path, NULL), mode);
}

FILE *
sys_fopen(const char * path, const char * mode)
{
  int fd;
  int oflag;
  const char * mode_save = mode;

  /* Force all file handles to be non-inheritable.  This is necessary to
     ensure child processes don't unwittingly inherit handles that might
     prevent future file access. */

  if (mode[0] == 'r')
    oflag = O_RDONLY;
  else if (mode[0] == 'w' || mode[0] == 'a')
    oflag = O_WRONLY | O_CREAT | O_TRUNC;
  else
    return NULL;

  /* Only do simplistic option parsing. */
  while (*++mode)
    if (mode[0] == '+')
      {
	oflag &= ~(O_RDONLY | O_WRONLY);
	oflag |= O_RDWR;
      }
    else if (mode[0] == 'b')
      {
	oflag &= ~O_TEXT;
	oflag |= O_BINARY;
      }
    else if (mode[0] == 't')
      {
	oflag &= ~O_BINARY;
	oflag |= O_TEXT;
      }
    else break;

  fd = _open (map_w32_filename (path, NULL), oflag | _O_NOINHERIT, 0644);
  if (fd < 0)
    return NULL;

  return _fdopen (fd, mode_save);
}

/* This only works on NTFS volumes, but is useful to have.  */
int
sys_link (const char * old, const char * new)
{
  HANDLE fileh;
  int   result = -1;
  TCHAR oldname[MAX_PATH], newname[MAX_PATH];

  if (old == NULL || new == NULL)
    {
      errno = ENOENT;
      return -1;
    }

  lstrcpy (oldname, map_w32_filename (old, NULL));
  lstrcpy (newname, map_w32_filename (new, NULL));

  fileh = CreateFile (oldname, 0, 0, NULL, OPEN_EXISTING,
		      FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (fileh != INVALID_HANDLE_VALUE)
    {
      int wlen;

      /* Confusingly, the "alternate" stream name field does not apply
         when restoring a hard link, and instead contains the actual
         stream data for the link (ie. the name of the link to create).
         The WIN32_STREAM_ID structure before the cStreamName field is
         the stream header, which is then immediately followed by the
         stream data.  */

      struct {
	WIN32_STREAM_ID wid;
	WCHAR wbuffer[MAX_PATH];	/* extra space for link name */
      } data;

      wlen = MultiByteToWideChar (CP_ACP, MB_PRECOMPOSED, newname, -1,
				  data.wid.cStreamName, MAX_PATH);
      if (wlen > 0)
	{
	  LPVOID context = NULL;
	  DWORD wbytes = 0;

	  data.wid.dwStreamId = BACKUP_LINK;
	  data.wid.dwStreamAttributes = 0;
	  data.wid.Size.LowPart = wlen * sizeof(WCHAR);
	  data.wid.Size.HighPart = 0;
	  data.wid.dwStreamNameSize = 0;

	  if (BackupWrite (fileh, (LPBYTE)&data,
			   offsetof (WIN32_STREAM_ID, cStreamName)
			   + data.wid.Size.LowPart,
			   &wbytes, FALSE, FALSE, &context)
	      && BackupWrite (fileh, NULL, 0, &wbytes, TRUE, FALSE, &context))
	    {
	      /* succeeded */
	      result = 0;
	    }
	  else
	    {
	      /* Should try mapping GetLastError to errno; for now just
		 indicate a general error (eg. links not supported).  */
	      errno = EINVAL;  // perhaps EMLINK?
	    }
	}

      CloseHandle (fileh);
    }
  else
    errno = ENOENT;

  return result;
}

int
sys_mkdir (const char * path)
{
  return _mkdir (map_w32_filename (path, NULL));
}

/* Because of long name mapping issues, we need to implement this
   ourselves.  Also, MSVC's _mktemp returns NULL when it can't generate
   a unique name, instead of setting the input template to an empty
   string.

   Standard algorithm seems to be use pid or tid with a letter on the
   front (in place of the 6 X's) and cycle through the letters to find a
   unique name.  We extend that to allow any reasonable character as the
   first of the 6 X's.  */
char *
sys_mktemp (char * template)
{
  char * p;
  int i;
  unsigned uid = GetCurrentThreadId ();
  static char first_char[] = "abcdefghijklmnopqrstuvwyz0123456789!%-_@#";

  if (template == NULL)
    return NULL;
  p = template + strlen (template);
  i = 5;
  /* replace up to the last 5 X's with uid in decimal */
  while (--p >= template && p[0] == 'X' && --i >= 0)
    {
      p[0] = '0' + uid % 10;
      uid /= 10;
    }

  if (i < 0 && p[0] == 'X')
    {
      i = 0;
      do
	{
	  int save_errno = errno;
	  p[0] = first_char[i];
	  if (sys_access (template, 0) < 0)
	    {
	      errno = save_errno;
	      return template;
	    }
	}
      while (++i < sizeof (first_char));
    }

  /* Template is badly formed or else we can't generate a unique name,
     so return empty string */
  template[0] = 0;
  return template;
}

int
sys_open (const char * path, int oflag, int mode)
{
  const char* mpath = map_w32_filename (path, NULL);
  /* Try to open file without _O_CREAT, to be able to write to hidden
     and system files. Force all file handles to be
     non-inheritable. */
  int res = _open (mpath, (oflag & ~_O_CREAT) | _O_NOINHERIT, mode);
  if (res >= 0)
    return res;
  return _open (mpath, oflag | _O_NOINHERIT, mode);
}

int
sys_rename (const char * oldname, const char * newname)
{
  BOOL result;
  TCHAR temp[MAX_PATH];

  /* MoveFile on Windows 95 doesn't correctly change the short file name
     alias in a number of circumstances (it is not easy to predict when
     just by looking at oldname and newname, unfortunately).  In these
     cases, renaming through a temporary name avoids the problem.

     A second problem on Windows 95 is that renaming through a temp name when
     newname is uppercase fails (the final long name ends up in
     lowercase, although the short alias might be uppercase) UNLESS the
     long temp name is not 8.3.

     So, on Windows 95 we always rename through a temp name, and we make sure
     the temp name has a long extension to ensure correct renaming.  */

  lstrcpy (temp, map_w32_filename (oldname, NULL));

  if (os_subtype == OS_WIN95)
    {
      char * o;
      char * p;
      int    i = 0;

      oldname = map_w32_filename (oldname, NULL);
      if (o = strrchr (oldname, '\\'))
	o++;
      else
	o = (char *) oldname;

      if (p = strrchr (temp, '\\'))
	p++;
      else
	p = temp;

      do
	{
	  /* Force temp name to require a manufactured 8.3 alias - this
	     seems to make the second rename work properly.  */
	  sprintf (p, "_.%s.%u", o, i);
	  i++;
	  result = rename (oldname, temp);
	}
      /* This loop must surely terminate!  */
      while (result < 0 && errno == EEXIST);
      if (result < 0)
	return -1;
    }

  /* Emulate Unix behaviour - newname is deleted if it already exists
     (at least if it is a file; don't do this for directories).

     Since we mustn't do this if we are just changing the case of the
     file name (we would end up deleting the file we are trying to
     rename!), we let rename detect if the destination file already
     exists - that way we avoid the possible pitfalls of trying to
     determine ourselves whether two names really refer to the same
     file, which is not always possible in the general case.  (Consider
     all the permutations of shared or subst'd drives, etc.)  */

  newname = map_w32_filename (newname, NULL);
  result = rename (temp, newname);

  if (result < 0
      && errno == EEXIST
      && _chmod (newname, 0666) == 0
      && _unlink (newname) == 0)
    result = rename (temp, newname);

  return result;
}

int
sys_rmdir (const char * path)
{
  return _rmdir (map_w32_filename (path, NULL));
}

int
sys_unlink (const char * path)
{
  path = map_w32_filename (path, NULL);

  /* On Unix, unlink works without write permission. */
  _chmod (path, 0666);
  return _unlink (path);
}

static FILETIME utc_base_ft;
static long double utc_base;
static int init = 0;

static time_t
convert_time (FILETIME ft)
{
  long double ret;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  if (CompareFileTime (&ft, &utc_base_ft) < 0)
    return 0;

  ret = (long double) ft.dwHighDateTime * 4096 * 1024 * 1024 + ft.dwLowDateTime;
  ret -= utc_base;
  return (time_t) (ret * 1e-7);
}

void
convert_from_time_t (time_t time, FILETIME * pft)
{
  long double tmp;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp = (long double) time * 1e7 + utc_base;
  pft->dwHighDateTime = (DWORD) (tmp / (4096.0 * 1024 * 1024));
  pft->dwLowDateTime = (DWORD) (tmp - (4096.0 * 1024 * 1024) * pft->dwHighDateTime);
}

#if 0
/* No reason to keep this; faking inode values either by hashing or even
   using the file index from GetInformationByHandle, is not perfect and
   so by default Emacs doesn't use the inode values on Windows.
   Instead, we now determine file-truename correctly (except for
   possible drive aliasing etc).  */

/*  Modified version of "PJW" algorithm (see the "Dragon" compiler book). */
static unsigned
hashval (const unsigned char * str)
{
  unsigned h = 0;
  while (*str)
    {
      h = (h << 4) + *str++;
      h ^= (h >> 28);
    }
  return h;
}

/* Return the hash value of the canonical pathname, excluding the
   drive/UNC header, to get a hopefully unique inode number. */
static DWORD
generate_inode_val (LPCTSTR name)
{
  TCHAR fullname[MAX_PATH];
  LPTSTR p;
  unsigned hash;

  /* Get the truly canonical filename, if it exists.  (Note: this
     doesn't resolve aliasing due to subst commands, or recognise hard
     links.  */
  if (!w32_get_long_filename ((char *)name, fullname, MAX_PATH))
    abort ();

  parse_root (fullname, &p);
  /* Normal W32 filesystems are still case insensitive. */
  _tcslwr (p);
  return hashval (p);
}

#endif

/* MSVC stat function can't cope with UNC names and has other bugs, so
   replace it with our own.  This also allows us to calculate consistent
   inode values without hacks in the main Emacs code. */
int
stat (const char *path_0, struct stat * buf)
{
  LPCTSTR path = path_0;
  LPTSTR name, r, name1, name2, prevname;
  WIN32_FIND_DATA wfd;
  HANDLE fh;
  DWORD fake_inode;
  int permission;
  int len;
  int rootdir = FALSE;

  if (path == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  name = (char *) map_w32_filename (path, &path);
  /* must be valid filename, no wild cards or other illegal characters */
  if (is_string_contains_chars (name, w32_filename_illegal_characters))
    {
      errno = ENOENT;
      return -1;
    }

  name1 = CharNext (name);
  name2 = CharNext (name1);
  /* If name is "c:/.." or "/.." then stat "c:/" or "/".  */
  r = IS_DEVICE_SEP (*name1) ? name2 : name;
  if (IS_DIRECTORY_SEP (r[0]) && r[1] == '.' && r[2] == '.' && r[3] == '\0')
    {
      r[1] = r[2] = '\0';
    }

  /* Remove trailing directory separator, unless name is the root
     directory of a drive or UNC volume in which case ensure there
     is a trailing separator. */
  len = lstrlen (name);
  rootdir = (path >= name + len - 1
	     && (IS_DIRECTORY_SEP (*path) || *path == 0));
  name = memcpy (alloca ((len + 2) * sizeof(TCHAR)), name, (len + 1) * sizeof(TCHAR));

  prevname = CharPrev (name, &name[len]);

  if (is_unc_volume (name))
    {
      DWORD attrs = unc_volume_file_attributes (name);

      if (attrs == -1)
	return -1;

      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = attrs;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      lstrcpy (wfd.cFileName, name);
    }
  else if (rootdir)
    {
      if (!IS_DIRECTORY_SEP (*prevname))
	{
	  name[len] = '\\';
	  name[len + 1] = '\0';
	}
      if (GetDriveType (name) < 2)
	{
	  errno = ENOENT;
	  return -1;
	}
      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      lstrcpy (wfd.cFileName, name);
    }
  else
    {
      if (IS_DIRECTORY_SEP (*prevname))
	name[len - 1] = '\0';

      /* (This is hacky, but helps when doing file completions on
	 network drives.)  Optimize by using information available from
	 active readdir if possible.  */
      len = strlen (dir_pathname);
      if (IS_DIRECTORY_SEP (dir_pathname[len-1]))
	len--;
      if (dir_find_handle != INVALID_HANDLE_VALUE
	  && CompareString(LOCALE_SYSTEM_DEFAULT, 0,
			   name, len, dir_pathname, len) == CSTR_EQUAL
	  && IS_DIRECTORY_SEP (name[len])
	  && lstrcmpi (name + len + 1, dir_static.d_name) == 0)
	{
	  /* This was the last entry returned by readdir.  */
	  wfd = dir_find_data;
	}
      else
	{
	  fh = FindFirstFile (name, &wfd);
	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      errno = ENOENT;
	      return -1;
	    }
	  FindClose (fh);
	}
    }

  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
#ifndef MEADOW
  else if (!NILP (Vw32_get_true_file_attributes)
	   /* No access rights required to get info.  */
	   && (fh = CreateFile (name, 0, 0, NULL, OPEN_EXISTING, 0, NULL))
	      != INVALID_HANDLE_VALUE)
    {
      /* This is more accurate in terms of gettting the correct number
	 of links, but is quite slow (it is noticable when Emacs is
	 making a list of file name completions). */
      BY_HANDLE_FILE_INFORMATION info;

      if (GetFileInformationByHandle (fh, &info))
	{
	  buf->st_nlink = info.nNumberOfLinks;
	  /* Might as well use file index to fake inode values, but this
	     is not guaranteed to be unique unless we keep a handle open
	     all the time (even then there are situations where it is
	     not unique).  Reputedly, there are at most 48 bits of info
	     (on NTFS, presumably less on FAT). */
	  fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
	}
      else
	{
	  buf->st_nlink = 1;
	  fake_inode = 0;
	}

      switch (GetFileType (fh))
	{
	case FILE_TYPE_DISK:
	  buf->st_mode = _S_IFREG;
	  break;
	case FILE_TYPE_PIPE:
	  buf->st_mode = _S_IFIFO;
	  break;
	case FILE_TYPE_CHAR:
	case FILE_TYPE_UNKNOWN:
	default:
	  buf->st_mode = _S_IFCHR;
	}
      CloseHandle (fh);
    }
#endif /* not MEADOW */
  else
    {
      /* Don't bother to make this information more accurate.  */
      buf->st_mode = _S_IFREG;
      buf->st_nlink = 1;
      fake_inode = 0;
    }

#if 0
  /* Not sure if there is any point in this.  */
  if (!NILP (Vw32_generate_fake_inodes))
    fake_inode = generate_inode_val (name);
  else if (fake_inode == 0)
    {
      /* For want of something better, try to make everything unique.  */
      static DWORD gen_num = 0;
      fake_inode = ++gen_num;
    }
#endif

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = fake_inode ^ (fake_inode >> 16);
  else
    buf->st_ino = fake_inode;

  /* consider files to belong to current user */
  buf->st_uid = the_passwd.pw_uid;
  buf->st_gid = the_passwd.pw_gid;

  /* volume_info is set indirectly by map_w32_filename */
  buf->st_dev = volume_info.serialnum;
  buf->st_rdev = volume_info.serialnum;


  buf->st_size = wfd.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (wfd.ftLastWriteTime);
  buf->st_atime = convert_time (wfd.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (wfd.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;
  else if (is_exec (name))
    permission |= _S_IEXEC;

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* Provide fstat and utime as well as stat for consistent handling of
   file timestamps. */
int
fstat (int desc, struct stat * buf)
{
  HANDLE fh = (HANDLE) _get_osfhandle (desc);
  BY_HANDLE_FILE_INFORMATION info;
  DWORD fake_inode;
  int permission;

  switch (GetFileType (fh) & ~FILE_TYPE_REMOTE)
    {
    case FILE_TYPE_DISK:
      buf->st_mode = _S_IFREG;
      if (!GetFileInformationByHandle (fh, &info))
	{
	  errno = EACCES;
	  return -1;
	}
      break;
    case FILE_TYPE_PIPE:
      buf->st_mode = _S_IFIFO;
      goto non_disk;
    case FILE_TYPE_CHAR:
    case FILE_TYPE_UNKNOWN:
    default:
      buf->st_mode = _S_IFCHR;
    non_disk:
      memset (&info, 0, sizeof (info));
      info.dwFileAttributes = 0;
      info.ftCreationTime = utc_base_ft;
      info.ftLastAccessTime = utc_base_ft;
      info.ftLastWriteTime = utc_base_ft;
    }

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
  else
    {
      buf->st_nlink = info.nNumberOfLinks;
      /* Might as well use file index to fake inode values, but this
	 is not guaranteed to be unique unless we keep a handle open
	 all the time (even then there are situations where it is
	 not unique).  Reputedly, there are at most 48 bits of info
      (on NTFS, presumably less on FAT). */
      fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
    }

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = fake_inode ^ (fake_inode >> 16);
  else
    buf->st_ino = fake_inode;

  /* consider files to belong to current user */
  buf->st_uid = 0;
  buf->st_gid = 0;

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_rdev = info.dwVolumeSerialNumber;

  buf->st_size = info.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (info.ftLastWriteTime);
  buf->st_atime = convert_time (info.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (info.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;
  else
    {
#if 0 /* no way of knowing the filename */
      char * p = strrchr (name, '.');
      if (p != NULL &&
	  (stricmp (p, ".exe") == 0 ||
	   stricmp (p, ".com") == 0 ||
	   stricmp (p, ".bat") == 0 ||
	   stricmp (p, ".cmd") == 0))
	permission |= _S_IEXEC;
#endif
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

#ifdef MEADOW
static unsigned
hashval (const unsigned char * str)
{
  unsigned h = 0;
  while (*str)
    {
      h = (h << 4) + *str++;
      h ^= (h >> 28);
    }
  return h;
}

int
file_attributes_stat (const char *path, struct stat * buf)
{
  int num, len, ret, max;
  HANDLE hf;
  WIN32_FIND_DATA w32fd;
  TCHAR name[MAX_PATH];
  LPTSTR prevname;

  ret = stat (path, buf);

  if ((!NILP (Vw32_get_true_file_attributes))
      && (buf->st_ino == 0)
      && (((hf = CreateFile (path, 0,
			     (FILE_SHARE_READ | FILE_SHARE_WRITE),
			     NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL))
	   != INVALID_HANDLE_VALUE)
	  || ((hf = CreateFile (path, 0,
				(FILE_SHARE_READ | FILE_SHARE_WRITE),
				NULL, OPEN_EXISTING,
				FILE_FLAG_BACKUP_SEMANTICS, NULL))
	      != INVALID_HANDLE_VALUE)))
    {
      /* This is more accurate in terms of gettting the correct number
	 of links, but is quite slow (it is noticable when Emacs is
	 making a list of file name completions). */
      BY_HANDLE_FILE_INFORMATION info;

      if (GetFileInformationByHandle (hf, &info))
	{
	  int high_crc, low_crc, crc_numerator;
	  int fake_inode_bits = sizeof(buf->st_ino) * 8;
      
	  buf->st_nlink = info.nNumberOfLinks;
	  /* These expressions are for making 64bit
	     file index into fake_inode_bits order number.
	     A smaller order number is made by psude-CRC16....;-) */
	  crc_numerator = ((1 << (fake_inode_bits - 1)) +
			   (1 << (fake_inode_bits - 2)) +
			   (1 << 2) + 1);
	  high_crc = info.nFileIndexHigh % crc_numerator;
	  low_crc = info.nFileIndexLow % crc_numerator;
	  buf->st_ino = (high_crc + low_crc) % crc_numerator;
	}

      switch (GetFileType (hf))
	{
	case FILE_TYPE_DISK:
	  buf->st_mode |= _S_IFREG;
	  break;
	case FILE_TYPE_PIPE:
	  buf->st_mode |= _S_IFIFO;
	  break;
	case FILE_TYPE_CHAR:
	case FILE_TYPE_UNKNOWN:
	default:
	  buf->st_mode |= _S_IFCHR;
	}
      CloseHandle (hf);
    }
  else
    {
      int fake_inode;
      int fake_inode_bits = sizeof(buf->st_ino) * 8;
      int fake_inode_max = (1 << fake_inode_bits);
      fake_inode = hashval (path);
      buf->st_ino = fake_inode % fake_inode_max;
    }

  if ((!NILP (Vw32_get_true_file_link_count))
      && (buf->st_mode & _S_IFDIR))
    {
      len = lstrlen ((LPTSTR)path);
      if (len > (MAX_PATH - 3))
	return ret;
      memcpy (name, path, sizeof(TCHAR) * (len + 1));

      unixtodos_filename (name);

      prevname = CharPrev(name, &name[len]);

      if (IS_DIRECTORY_SEP(*prevname))
	{
	  name[len] = '*';
	  name[len + 1] = '\0';
	}
      else
	{
	  name[len] = '\\';
	  name[len + 1] = '*';
	  name[len + 2] = '\0';
	}

      if (NUMBERP (Vw32_get_true_file_link_count))
	{
	  max = XINT (Vw32_get_true_file_link_count);
	  if (max <= 0)
	    return ret;
	}
      else
	max = -1;

      num = 0;
      hf = FindFirstFile (name, &w32fd);

      if (hf == INVALID_HANDLE_VALUE)
	return ret;

      do {
	if (w32fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
	  num++;
	if (max > 0) max--;
	else if (max == 0) break;
      } while (FindNextFile (hf, &w32fd));

      FindClose (hf);

      buf->st_nlink = num;
  
    }

  return ret;
}
#endif

#if 0
int
utime (const char *name, struct utimbuf *times)
{
  struct utimbuf deftime;
  HANDLE fh;
  FILETIME mtime;
  FILETIME atime;

  if (times == NULL)
    {
      deftime.modtime = deftime.actime = time (NULL);
      times = &deftime;
    }

  /* Need write access to set times.  */
  fh = CreateFile (name, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
		   0, OPEN_EXISTING, 0, NULL);
  if (fh)
    {
      convert_from_time_t (times->actime, &atime);
      convert_from_time_t (times->modtime, &mtime);
      if (!SetFileTime (fh, NULL, &atime, &mtime))
	{
	  CloseHandle (fh);
	  errno = EACCES;
	  return -1;
	}
      CloseHandle (fh);
    }
  else
    {
      errno = EINVAL;
      return -1;
    }
  return 0;
}
#endif

#ifdef HAVE_SOCKETS

/* Wrappers for  winsock functions to map between our file descriptors
   and winsock's handles; also set h_errno for convenience.

   To allow Emacs to run on systems which don't have winsock support
   installed, we dynamically link to winsock on startup if present, and
   otherwise provide the minimum necessary functionality
   (eg. gethostname). */

/* function pointers for relevant socket functions */
int (PASCAL *pfn_WSAStartup) (WORD wVersionRequired, LPWSADATA lpWSAData);
void (PASCAL *pfn_WSASetLastError) (int iError);
int (PASCAL *pfn_WSAGetLastError) (void);
int (PASCAL *pfn_socket) (int af, int type, int protocol);
int (PASCAL *pfn_bind) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_connect) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_ioctlsocket) (SOCKET s, long cmd, u_long *argp);
int (PASCAL *pfn_recv) (SOCKET s, char * buf, int len, int flags);
int (PASCAL *pfn_send) (SOCKET s, const char * buf, int len, int flags);
int (PASCAL *pfn_closesocket) (SOCKET s);
int (PASCAL *pfn_shutdown) (SOCKET s, int how);
int (PASCAL *pfn_WSACleanup) (void);

u_short (PASCAL *pfn_htons) (u_short hostshort);
u_short (PASCAL *pfn_ntohs) (u_short netshort);
unsigned long (PASCAL *pfn_inet_addr) (const char * cp);
int (PASCAL *pfn_gethostname) (char * name, int namelen);
struct hostent * (PASCAL *pfn_gethostbyname) (const char * name);
struct servent * (PASCAL *pfn_getservbyname) (const char * name, const char * proto);
  
/* SetHandleInformation is only needed to make sockets non-inheritable. */
BOOL (WINAPI *pfn_SetHandleInformation) (HANDLE object, DWORD mask, DWORD flags);
#ifndef HANDLE_FLAG_INHERIT
#define HANDLE_FLAG_INHERIT	1
#endif

HANDLE winsock_lib;
static int winsock_inuse;

BOOL
term_winsock (void)
{
  if (winsock_lib != NULL && winsock_inuse == 0)
    {
      /* Not sure what would cause WSAENETDOWN, or even if it can happen
	 after WSAStartup returns successfully, but it seems reasonable
	 to allow unloading winsock anyway in that case. */
      if (pfn_WSACleanup () == 0 ||
	  pfn_WSAGetLastError () == WSAENETDOWN)
	{
	  if (FreeLibrary (winsock_lib))
	  winsock_lib = NULL;
	  return TRUE;
	}
    }
  return FALSE;
}

BOOL
init_winsock (int load_now)
{
  WSADATA  winsockData;

  if (winsock_lib != NULL)
    return TRUE;

  pfn_SetHandleInformation = NULL;
  pfn_SetHandleInformation
    = (void *) GetProcAddress (GetModuleHandle ("kernel32.dll"),
			       "SetHandleInformation");

  winsock_lib = LoadLibrary ("wsock32.dll");

  if (winsock_lib != NULL)
    {
      /* dynamically link to socket functions */

#define LOAD_PROC(fn) \
      if ((pfn_##fn = (void *) GetProcAddress (winsock_lib, #fn)) == NULL) \
        goto fail;

      LOAD_PROC( WSAStartup );
      LOAD_PROC( WSASetLastError );
      LOAD_PROC( WSAGetLastError );
      LOAD_PROC( socket );
      LOAD_PROC( bind );
      LOAD_PROC( connect );
      LOAD_PROC( ioctlsocket );
      LOAD_PROC( recv );
      LOAD_PROC( send );
      LOAD_PROC( closesocket );
      LOAD_PROC( shutdown );
      LOAD_PROC( htons );
      LOAD_PROC( ntohs );
      LOAD_PROC( inet_addr );
      LOAD_PROC( gethostname );
      LOAD_PROC( gethostbyname );
      LOAD_PROC( getservbyname );
      LOAD_PROC( WSACleanup );

#undef LOAD_PROC

      /* specify version 1.1 of winsock */
      if (pfn_WSAStartup (0x101, &winsockData) == 0)
        {
	  if (winsockData.wVersion != 0x101)
	    goto fail;

	  if (!load_now)
	    {
	      /* Report that winsock exists and is usable, but leave
		 socket functions disabled.  I am assuming that calling
		 WSAStartup does not require any network interaction,
		 and in particular does not cause or require a dial-up
		 connection to be established. */

	      pfn_WSACleanup ();
	      FreeLibrary (winsock_lib);
	      winsock_lib = NULL;
	    }
	  winsock_inuse = 0;
	  return TRUE;
	}

    fail:
      FreeLibrary (winsock_lib);
      winsock_lib = NULL;
    }

  return FALSE;
}


int h_errno = 0;

/* function to set h_errno for compatability; map winsock error codes to
   normal system codes where they overlap (non-overlapping definitions
   are already in <sys/socket.h> */
static void set_errno ()
{
  if (winsock_lib == NULL)
    h_errno = EINVAL;
  else
    h_errno = pfn_WSAGetLastError ();

  switch (h_errno)
    {
    case WSAEACCES:		h_errno = EACCES; break;
    case WSAEBADF: 		h_errno = EBADF; break;
    case WSAEFAULT:		h_errno = EFAULT; break;
    case WSAEINTR: 		h_errno = EINTR; break;
    case WSAEINVAL:		h_errno = EINVAL; break;
    case WSAEMFILE:		h_errno = EMFILE; break;
    case WSAENAMETOOLONG: 	h_errno = ENAMETOOLONG; break;
    case WSAENOTEMPTY:		h_errno = ENOTEMPTY; break;
    }
  errno = h_errno;
}

static void check_errno ()
{
  if (h_errno == 0 && winsock_lib != NULL)
    pfn_WSASetLastError (0);
}

/* Extend strerror to handle the winsock-specific error codes.  */
struct {
  int errnum;
  char * msg;
} _wsa_errlist[] = {
  WSAEINTR                , "Interrupted function call",
  WSAEBADF                , "Bad file descriptor",
  WSAEACCES               , "Permission denied",
  WSAEFAULT               , "Bad address",
  WSAEINVAL               , "Invalid argument",
  WSAEMFILE               , "Too many open files",
			
  WSAEWOULDBLOCK          , "Resource temporarily unavailable",
  WSAEINPROGRESS          , "Operation now in progress",
  WSAEALREADY             , "Operation already in progress",
  WSAENOTSOCK             , "Socket operation on non-socket",
  WSAEDESTADDRREQ         , "Destination address required",
  WSAEMSGSIZE             , "Message too long",
  WSAEPROTOTYPE           , "Protocol wrong type for socket",
  WSAENOPROTOOPT          , "Bad protocol option",
  WSAEPROTONOSUPPORT      , "Protocol not supported",
  WSAESOCKTNOSUPPORT      , "Socket type not supported",
  WSAEOPNOTSUPP           , "Operation not supported",
  WSAEPFNOSUPPORT         , "Protocol family not supported",
  WSAEAFNOSUPPORT         , "Address family not supported by protocol family",
  WSAEADDRINUSE           , "Address already in use",
  WSAEADDRNOTAVAIL        , "Cannot assign requested address",
  WSAENETDOWN             , "Network is down",
  WSAENETUNREACH          , "Network is unreachable",
  WSAENETRESET            , "Network dropped connection on reset",
  WSAECONNABORTED         , "Software caused connection abort",
  WSAECONNRESET           , "Connection reset by peer",
  WSAENOBUFS              , "No buffer space available",
  WSAEISCONN              , "Socket is already connected",
  WSAENOTCONN             , "Socket is not connected",
  WSAESHUTDOWN            , "Cannot send after socket shutdown",
  WSAETOOMANYREFS         , "Too many references",	    /* not sure */
  WSAETIMEDOUT            , "Connection timed out",
  WSAECONNREFUSED         , "Connection refused",
  WSAELOOP                , "Network loop",		    /* not sure */
  WSAENAMETOOLONG         , "Name is too long",
  WSAEHOSTDOWN            , "Host is down",
  WSAEHOSTUNREACH         , "No route to host",
  WSAENOTEMPTY            , "Buffer not empty",		    /* not sure */
  WSAEPROCLIM             , "Too many processes",
  WSAEUSERS               , "Too many users",		    /* not sure */
  WSAEDQUOT               , "Double quote in host name",    /* really not sure */
  WSAESTALE               , "Data is stale",		    /* not sure */
  WSAEREMOTE              , "Remote error",		    /* not sure */
			
  WSASYSNOTREADY          , "Network subsystem is unavailable",
  WSAVERNOTSUPPORTED      , "WINSOCK.DLL version out of range",
  WSANOTINITIALISED       , "Winsock not initialized successfully",
  WSAEDISCON              , "Graceful shutdown in progress",
#ifdef WSAENOMORE
  WSAENOMORE              , "No more operations allowed",   /* not sure */
  WSAECANCELLED           , "Operation cancelled",	    /* not sure */
  WSAEINVALIDPROCTABLE    , "Invalid procedure table from service provider",
  WSAEINVALIDPROVIDER     , "Invalid service provider version number",
  WSAEPROVIDERFAILEDINIT  , "Unable to initialize a service provider",
  WSASYSCALLFAILURE       , "System call failured",
  WSASERVICE_NOT_FOUND    , "Service not found",	    /* not sure */
  WSATYPE_NOT_FOUND       , "Class type not found",
  WSA_E_NO_MORE           , "No more resources available",  /* really not sure */
  WSA_E_CANCELLED         , "Operation already cancelled",  /* really not sure */
  WSAEREFUSED             , "Operation refused",	    /* not sure */
#endif
			
  WSAHOST_NOT_FOUND       , "Host not found",
  WSATRY_AGAIN            , "Authoritative host not found during name lookup",
  WSANO_RECOVERY          , "Non-recoverable error during name lookup",
  WSANO_DATA              , "Valid name, no data record of requested type",

  -1, NULL
};

char *
sys_strerror(int error_no)
{
  int i;
  static char unknown_msg[40];

  if (error_no >= 0 && error_no < sys_nerr)
    return sys_errlist[error_no];

  for (i = 0; _wsa_errlist[i].errnum >= 0; i++)
    if (_wsa_errlist[i].errnum == error_no)
      return _wsa_errlist[i].msg;

  sprintf(unknown_msg, "Unidentified error: %d", error_no);
  return unknown_msg;
}

/* [andrewi 3-May-96] I've had conflicting results using both methods,
   but I believe the method of keeping the socket handle separate (and
   insuring it is not inheritable) is the correct one. */

//#define SOCK_REPLACE_HANDLE

#ifdef SOCK_REPLACE_HANDLE
#define SOCK_HANDLE(fd) ((SOCKET) _get_osfhandle (fd))
#else
#define SOCK_HANDLE(fd) ((SOCKET) fd_info[fd].hnd)
#endif

int
sys_socket(int af, int type, int protocol)
{
  int fd;
  long s;
  child_process * cp;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return INVALID_SOCKET;
    }

  check_errno ();

  /* call the real socket function */
  s = (long) pfn_socket (af, type, protocol);
  
  if (s != INVALID_SOCKET)
    {
      /* Although under NT 3.5 _open_osfhandle will accept a socket
	 handle, if opened with SO_OPENTYPE == SO_SYNCHRONOUS_NONALERT,
	 that does not work under NT 3.1.  However, we can get the same
	 effect by using a backdoor function to replace an existing
	 descriptor handle with the one we want. */

      /* allocate a file descriptor (with appropriate flags) */
      fd = _open ("NUL:", _O_RDWR);
      if (fd >= 0)
        {
#ifdef SOCK_REPLACE_HANDLE
	  /* now replace handle to NUL with our socket handle */
	  CloseHandle ((HANDLE) _get_osfhandle (fd));
	  _free_osfhnd (fd);
	  _set_osfhnd (fd, s);
	  /* setmode (fd, _O_BINARY); */
#else
	  /* Make a non-inheritable copy of the socket handle.  Note
             that it is possible that sockets aren't actually kernel
             handles, which appears to be the case on Windows 9x when
             the MS Proxy winsock client is installed.  */
	  {
	    /* Apparently there is a bug in NT 3.51 with some service
	       packs, which prevents using DuplicateHandle to make a
	       socket handle non-inheritable (causes WSACleanup to
	       hang).  The work-around is to use SetHandleInformation
	       instead if it is available and implemented. */
	    if (pfn_SetHandleInformation)
	      {
		pfn_SetHandleInformation ((HANDLE) s, HANDLE_FLAG_INHERIT, 0);
	      }
	    else
	      {
		HANDLE parent = GetCurrentProcess ();
		HANDLE new_s = INVALID_HANDLE_VALUE;

		if (DuplicateHandle (parent,
				     (HANDLE) s,
				     parent,
				     &new_s,
				     0,
				     FALSE,
				     DUPLICATE_SAME_ACCESS))
		  {
		    /* It is possible that DuplicateHandle succeeds even
                       though the socket wasn't really a kernel handle,
                       because a real handle has the same value.  So
                       test whether the new handle really is a socket.  */
		    long nonblocking = 0;
		    if (pfn_ioctlsocket ((SOCKET) new_s, FIONBIO, &nonblocking) == 0)
		      {
			pfn_closesocket (s);
			s = (SOCKET) new_s;
		      }
		    else
		      {
			CloseHandle (new_s);
		      }
		  } 
	      }
	  }
	  fd_info[fd].hnd = (HANDLE) s;
#endif

	  /* set our own internal flags */
	  fd_info[fd].flags = FILE_SOCKET | FILE_BINARY | FILE_READ | FILE_WRITE;

	  cp = new_child ();
	  if (cp)
	    {
	      cp->fd = fd;
	      cp->status = STATUS_READ_ACKNOWLEDGED;

	      /* attach child_process to fd_info */
	      if (fd_info[ fd ].cp != NULL)
		{
		  DebPrint (("sys_socket: fd_info[%d] apparently in use!\n", fd));
		  abort ();
		}

	      fd_info[ fd ].cp = cp;

	      /* success! */
	      winsock_inuse++;	/* count open sockets */
	      return fd;
	    }

	  /* clean up */
	  _close (fd);
	}
      pfn_closesocket (s);
      h_errno = EMFILE;
    }
  set_errno ();

  return -1;
}


int
sys_bind (int s, const struct sockaddr * addr, int namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_bind (SOCK_HANDLE (s), addr, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}


int
sys_connect (int s, const struct sockaddr * name, int namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_connect (SOCK_HANDLE (s), name, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

u_short
sys_htons (u_short hostshort)
{
  return (winsock_lib != NULL) ?
    pfn_htons (hostshort) : hostshort;
}

u_short
sys_ntohs (u_short netshort)
{
  return (winsock_lib != NULL) ?
    pfn_ntohs (netshort) : netshort;
}

unsigned long
sys_inet_addr (const char * cp)
{
  return (winsock_lib != NULL) ?
    pfn_inet_addr (cp) : INADDR_NONE;
}

int
sys_gethostname (char * name, int namelen)
{
  if (winsock_lib != NULL)
    return pfn_gethostname (name, namelen);

  if (namelen > MAX_COMPUTERNAME_LENGTH)
    return !GetComputerName (name, (DWORD *)&namelen);

  h_errno = EFAULT;
  return SOCKET_ERROR;
}

struct hostent *
sys_gethostbyname(const char * name)
{
  struct hostent * host;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  host = pfn_gethostbyname (name);
  if (!host)
    set_errno ();
  return host;
}

struct servent *
sys_getservbyname(const char * name, const char * proto)
{
  struct servent * serv;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  serv = pfn_getservbyname (name, proto);
  if (!serv)
    set_errno ();
  return serv;
}

int
sys_shutdown (int s, int how)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_shutdown (SOCK_HANDLE (s), how);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

#endif /* HAVE_SOCKETS */


/* Shadow main io functions: we need to handle pipes and sockets more
   intelligently, and implement non-blocking mode as well. */

int
sys_close (int fd)
{
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  if (fd_info[fd].cp)
    {
      child_process * cp = fd_info[fd].cp;

      fd_info[fd].cp = NULL;

      if (CHILD_ACTIVE (cp))
        {
	  /* if last descriptor to active child_process then cleanup */
	  int i;
	  for (i = 0; i < MAXDESC; i++)
	    {
	      if (i == fd)
		continue;
	      if (fd_info[i].cp == cp)
		break;
	    }
	  if (i == MAXDESC)
	    {
#ifdef HAVE_SOCKETS
	      if (fd_info[fd].flags & FILE_SOCKET)
		{
#ifndef SOCK_REPLACE_HANDLE
		  if (winsock_lib == NULL) abort ();

		  pfn_shutdown (SOCK_HANDLE (fd), 2);
		  rc = pfn_closesocket (SOCK_HANDLE (fd));
#endif
		  winsock_inuse--; /* count open sockets */
		}
#endif
	      delete_child (cp);
	    }
	}
    }

  /* Note that sockets do not need special treatment here (at least on
     NT and Windows 95 using the standard tcp/ip stacks) - it appears that
     closesocket is equivalent to CloseHandle, which is to be expected
     because socket handles are fully fledged kernel handles. */
  rc = _close (fd);

  if (rc == 0)
    fd_info[fd].flags = 0;

  return rc;
}

int
sys_dup (int fd)
{
  int new_fd;

  new_fd = _dup (fd);
  if (new_fd >= 0)
    {
      /* duplicate our internal info as well */
      fd_info[new_fd] = fd_info[fd];
    }
  return new_fd;
}


int
sys_dup2 (int src, int dst)
{
  int rc;

  if (dst < 0 || dst >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  /* make sure we close the destination first if it's a pipe or socket */
  if (src != dst && fd_info[dst].flags != 0)
    sys_close (dst);
  
  rc = _dup2 (src, dst);
  if (rc == 0)
    {
      /* duplicate our internal info as well */
      fd_info[dst] = fd_info[src];
    }
  return rc;
}

/* Unix pipe() has only one arg */
int
sys_pipe (int * phandles)
{
  int rc;
  unsigned flags;

  /* make pipe handles non-inheritable; when we spawn a child, we
     replace the relevant handle with an inheritable one.  Also put
     pipes into binary mode; we will do text mode translation ourselves
     if required.  */
  rc = _pipe (phandles, 0, _O_NOINHERIT | _O_BINARY);

  if (rc == 0)
    {
      if (phandles[0] >= MAXDESC || phandles[1] >= MAXDESC)
	{
	  _close (phandles[0]);
	  _close (phandles[1]);
	  rc = -1;
	}
      else
	{
	  flags = FILE_PIPE | FILE_READ | FILE_BINARY;
	  fd_info[phandles[0]].flags = flags;

	  flags = FILE_PIPE | FILE_WRITE | FILE_BINARY;
	  fd_info[phandles[1]].flags = flags;
	}
    }

  return rc;
}

/* From ntproc.c */
extern Lisp_Object Vw32_pipe_read_delay;

/* Function to do blocking read of one byte, needed to implement
   select.  It is only allowed on sockets and pipes. */
int
_sys_read_ahead (int fd)
{
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  if ((fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET)) == 0
      || (fd_info[fd].flags & FILE_READ) == 0)
    {
      DebPrint (("_sys_read_ahead: internal error: fd %d is not a pipe or socket!\n", fd));
      abort ();
    }
  
  cp->status = STATUS_READ_IN_PROGRESS;
  
  if (fd_info[fd].flags & FILE_PIPE)
    {
#ifdef MEADOW
      int numbytes;
      rc = ReadFile((HANDLE)_get_osfhandle(fd), 
		    &cp->chr, sizeof(char), &numbytes, NULL);

      if (rc && (numbytes == sizeof(char))) rc = sizeof(char);
      else rc = 0;
#else
      rc = _read (fd, &cp->chr, sizeof (char));
#endif

      /* Give subprocess time to buffer some more output for us before
	 reporting that input is available; we need this because Windows 95
	 connects DOS programs to pipes by making the pipe appear to be
	 the normal console stdout - as a result most DOS programs will
	 write to stdout without buffering, ie.  one character at a
	 time.  Even some W32 programs do this - "dir" in a command
	 shell on NT is very slow if we don't do this. */
      if (rc > 0)
	{
	  int wait = XINT (Vw32_pipe_read_delay);

	  if (wait > 0)
	    Sleep (wait);
	  else if (wait < 0)
	    while (++wait <= 0)
	      /* Yield remainder of our time slice, effectively giving a
		 temporary priority boost to the child process. */
	      Sleep (0);
	}
    }
#ifdef HAVE_SOCKETS
  else if (fd_info[fd].flags & FILE_SOCKET)
    rc = pfn_recv (SOCK_HANDLE (fd), &cp->chr, sizeof (char), 0);
#endif
  
  if (rc == sizeof (char))
    cp->status = STATUS_READ_SUCCEEDED;
  else
    cp->status = STATUS_READ_FAILED;

  return cp->status;
}

int
sys_read (int fd, char * buffer, unsigned int count)
{
  int nchars;
  int to_read;
  DWORD waiting;
  char * orig_buffer = buffer;

  if (fd < 0 || fd >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  if (fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET))
    {
      child_process *cp = fd_info[fd].cp;

      if ((fd_info[fd].flags & FILE_READ) == 0)
        {
	  errno = EBADF;
	  return -1;
	}

      nchars = 0;

      /* re-read CR carried over from last read */
      if (fd_info[fd].flags & FILE_LAST_CR)
	{
	  if (fd_info[fd].flags & FILE_BINARY) abort ();
	  *buffer++ = 0x0d;
	  count--;
	  nchars++;
	  fd_info[fd].flags &= ~FILE_LAST_CR;
	}

      /* presence of a child_process structure means we are operating in
	 non-blocking mode - otherwise we just call _read directly.
	 Note that the child_process structure might be missing because
	 reap_subprocess has been called; in this case the pipe is
	 already broken, so calling _read on it is okay. */
      if (cp)
        {
	  int current_status = cp->status;

	  switch (current_status)
	    {
	    case STATUS_READ_FAILED:
	    case STATUS_READ_ERROR:
	      /* report normal EOF if nothing in buffer */
	      if (nchars <= 0)
		fd_info[fd].flags |= FILE_AT_EOF;
	      return nchars;

	    case STATUS_READ_READY:
	    case STATUS_READ_IN_PROGRESS:
	      DebPrint (("sys_read called when read is in progress\n"));
	      errno = EWOULDBLOCK;
	      return -1;

	    case STATUS_READ_SUCCEEDED:
	      /* consume read-ahead char */
	      *buffer++ = cp->chr;
	      count--;
	      nchars++;
	      cp->status = STATUS_READ_ACKNOWLEDGED;
	      ResetEvent (cp->char_avail);

	    case STATUS_READ_ACKNOWLEDGED:
	      break;

	    default:
	      DebPrint (("sys_read: bad status %d\n", current_status));
	      errno = EBADF;
	      return -1;
	    }

	  if (fd_info[fd].flags & FILE_PIPE)
	    {
	      PeekNamedPipe ((HANDLE) _get_osfhandle (fd), NULL, 0, NULL, &waiting, NULL);
	      to_read = min (waiting, (DWORD) count);

	      if (to_read > 0)
		nchars += _read (fd, buffer, to_read);
	    }
#ifdef HAVE_SOCKETS
	  else /* FILE_SOCKET */
	    {
	      if (winsock_lib == NULL) abort ();

	      /* do the equivalent of a non-blocking read */
	      pfn_ioctlsocket (SOCK_HANDLE (fd), FIONREAD, &waiting);
	      if (waiting == 0 && nchars == 0)
	        {
		  h_errno = errno = EWOULDBLOCK;
		  return -1;
		}

	      if (waiting)
	        {
		  /* always use binary mode for sockets */
		  int res = pfn_recv (SOCK_HANDLE (fd), buffer, count, 0);
		  if (res == SOCKET_ERROR)
		    {
		      DebPrint(("sys_read.recv failed with error %d on socket %ld\n",
				pfn_WSAGetLastError (), SOCK_HANDLE (fd)));
		      set_errno ();
		      return -1;
		    }
		  nchars += res;
		}
	    }
#endif
	}
      else
	{
	  int nread = _read (fd, buffer, count);
	  if (nread >= 0)
	    nchars += nread;
	  else if (nchars == 0)
	    nchars = nread;
	}

      if (nchars <= 0)
	fd_info[fd].flags |= FILE_AT_EOF;
#ifndef MEADOW
      /* Perform text mode translation if required.  */
      else if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  nchars = crlf_to_lf (nchars, orig_buffer);
	  /* If buffer contains only CR, return that.  To be absolutely
	     sure we should attempt to read the next char, but in
	     practice a CR to be followed by LF would not appear by
	     itself in the buffer.  */
	  if (nchars > 1 && orig_buffer[nchars - 1] == 0x0d)
	    {
	      fd_info[fd].flags |= FILE_LAST_CR;
	      nchars--;
	    }
	}
#endif
    }
  else
    nchars = _read (fd, buffer, count);

  return nchars;
}

/* For now, don't bother with a non-blocking mode */
int
sys_write (int fd, const void * buffer, unsigned int count)
{
  int nchars;

  if (fd < 0 || fd >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  if (fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET))
    {
      if ((fd_info[fd].flags & FILE_WRITE) == 0)
	{
	  errno = EBADF;
	  return -1;
	}

      /* Perform text mode translation if required.  */
      if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  char * tmpbuf = alloca (count * 2);
	  unsigned char * src = (void *)buffer;
	  unsigned char * dst = tmpbuf;
	  int nbytes = count;

	  while (1)
	    {
	      unsigned char *next;
	      /* copy next line or remaining bytes */
	      next = _memccpy (dst, src, '\n', nbytes);
	      if (next)
		{
		  /* copied one line ending with '\n' */
		  int copied = next - dst;
		  nbytes -= copied;
		  src += copied;
		  /* insert '\r' before '\n' */
		  next[-1] = '\r';
		  next[0] = '\n';
		  dst = next + 1;
		  count++;
		}	    
	      else
		/* copied remaining partial line -> now finished */
		break;
	    }
	  buffer = tmpbuf;
	}
    }

#ifdef HAVE_SOCKETS
  if (fd_info[fd].flags & FILE_SOCKET)
    {
      if (winsock_lib == NULL) abort ();
      nchars =  pfn_send (SOCK_HANDLE (fd), buffer, count, 0);
      if (nchars == SOCKET_ERROR)
        {
	  DebPrint(("sys_read.send failed with error %d on socket %ld\n",
		    pfn_WSAGetLastError (), SOCK_HANDLE (fd)));
	  set_errno ();
	}
    }
  else
#endif
    nchars = _write (fd, buffer, count);

  return nchars;
}

static void
check_windows_init_file ()
{
  extern int noninteractive, inhibit_window_system;

  /* A common indication that Emacs is not installed properly is when
     it cannot find the Windows installation file.  If this file does
     not exist in the expected place, tell the user.  */

  if (!noninteractive && !inhibit_window_system) 
    {
      extern Lisp_Object Vwindow_system, Vload_path, Qfile_exists_p;
      Lisp_Object objs[2];
      Lisp_Object full_load_path;
      Lisp_Object init_file;
      char *path;
      int fd;

#ifdef CANNOT_DUMP
      extern char cd_load_path[];
      objs[0] = Fcons(build_string(cd_load_path), Qnil);
#else
      objs[0] = Vload_path;
#endif
      path = getenv ("EMACSLOADPATH");
      if (!path)
	{
	  MessageBox (NULL,
		      "EMACSLOADPATH is not specified!\n"
		      "Cannot load startup lisp programs.",
		      "Emacs Abort Dialog",
		      MB_OK | MB_ICONEXCLAMATION | MB_TASKMODAL);
#undef abort
	  abort ();
	}
      objs[1] = decode_env_path (0, (getenv ("EMACSLOADPATH")));
      full_load_path = Fappend (2, objs);
      init_file = build_string ("term/w32-win");
      fd = openp (full_load_path, init_file, ".el:.elc", NULL, 0);
      if (fd < 0) 
	{
	  Lisp_Object load_path_print = Fprin1_to_string (full_load_path, Qnil);
	  char *init_file_name = XSTRING (init_file)->data;
	  char *load_path = XSTRING (load_path_print)->data;
	  char *buffer = alloca (1024);

	  sprintf (buffer, 
		   "The Emacs Windows initialization file \"%s.el\" "
		   "could not be found in your Emacs installation.  "
		   "Emacs checked the following directories for this file:\n"
		   "\n%s\n\n"
		   "When Emacs cannot find this file, it usually means that it "
		   "was not installed properly, or its distribution file was "
		   "not unpacked properly.\nSee the README.W32 file in the "
		   "top-level Emacs directory for more information.",
		   init_file_name, load_path);
	  MessageBox (NULL,
		      buffer,
		      "Emacs Abort Dialog",
		      MB_OK | MB_ICONEXCLAMATION | MB_TASKMODAL);
      /* Use the low-level Emacs abort. */
#undef abort
	  abort ();
	}
      else if (fd > 0)
	{
	  _close (fd);
	}
    }
}

void
term_ntproc ()
{
#ifdef HAVE_SOCKETS
  /* shutdown the socket interface if necessary */
  term_winsock ();
#endif
}

void
init_ntproc ()
{
#ifdef HAVE_SOCKETS
  /* Initialise the socket interface now if available and requested by
     the user by defining PRELOAD_WINSOCK; otherwise loading will be
     delayed until open-network-stream is called (w32-has-winsock can
     also be used to dynamically load or reload winsock).

     Conveniently, init_environment is called before us, so
     PRELOAD_WINSOCK can be set in the registry. */

  /* Always initialize this correctly. */
  winsock_lib = NULL;

  if (getenv ("PRELOAD_WINSOCK") != NULL)
    init_winsock (TRUE);
#endif

  /* Initial preparation for subprocess support: replace our standard
     handles with non-inheritable versions. */
  {
    HANDLE parent;
    HANDLE stdin_save =  INVALID_HANDLE_VALUE;
    HANDLE stdout_save = INVALID_HANDLE_VALUE;
    HANDLE stderr_save = INVALID_HANDLE_VALUE;

    parent = GetCurrentProcess ();

    /* ignore errors when duplicating and closing; typically the
       handles will be invalid when running as a gui program. */
    DuplicateHandle (parent, 
		     GetStdHandle (STD_INPUT_HANDLE), 
		     parent,
		     &stdin_save, 
		     0, 
		     FALSE, 
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_OUTPUT_HANDLE),
		     parent,
		     &stdout_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_ERROR_HANDLE),
		     parent,
		     &stderr_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    fclose (stdin);
    fclose (stdout);
    fclose (stderr);

    if (stdin_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdin_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_RDONLY);
    _fdopen (0, "r");

    if (stdout_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdout_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (1, "w");

    if (stderr_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stderr_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (2, "w");
  }

  /* unfortunately, atexit depends on implementation of malloc */
  /* atexit (term_ntproc); */
  signal (SIGABRT, term_ntproc);

  /* determine which drives are fixed, for GetCachedVolumeInformation */
  {
    /* GetDriveType must have trailing backslash. */
    char drive[] = "A:\\";

    /* Loop over all possible drive letters */
    while (*drive <= 'Z')
    {
      /* Record if this drive letter refers to a fixed drive. */
      fixed_drives[DRIVE_INDEX (*drive)] = 
	(GetDriveType (drive) == DRIVE_FIXED);

      (*drive)++;
    }

    /* Reset the volume info cache.  */
    volume_cache = NULL;
  }
  
  /* Check to see if Emacs has been installed correctly.  */
#ifndef PDUMP /* #### FIXME */
  check_windows_init_file ();
#endif
}

void init_w32system()
{
#if 0
  /* To encode and decode clipboard string properly, I don't make use
     of w32_mule_clipboard_format. */
  mw32_mule_clipboard_format = RegisterClipboardFormat (MULE_CLIPBOARD_FORMAT);
#else
  mw32_mule_clipboard_format = 0;
#endif

#ifdef MEADOW
  InitializeCriticalSection (&critsec_message);
  InitializeCriticalSection (&critsec_access_event);
  interrupt_handle = CreateEvent(0, TRUE, FALSE, NULL);
#endif

#ifdef CANNOT_DUMP
  {
    extern char cd_lock_path[];
    extern char cd_load_path[];
    char *p;
    if (p = egetenv("EMACSLOCKDIR"))
      strcpy(cd_lock_path, p);
    else if (p = egetenv("TMP"))
      strcpy(cd_lock_path, p);
    else if (p = egetenv("TEMP"))
      strcpy(cd_lock_path, p);
    else
      GetWindowsDirectory(cd_lock_path, MAX_PATH);

    printf("path:%s\n", cd_load_path);
  }
#endif
}

Lisp_Object Vexec_suffix_list;

void setup_exec_suffix_list()
{
  static char exec_suffix[] = EXEC_SUFFIXES;
  Lisp_Object slot;
  char *s, *e;
  int bytes;
  int len = strlen(exec_suffix);

  Vexec_suffix_list = Qnil;
  s = exec_suffix;
  do {
    e = strchr (s, ':');
    if (!e) e = &exec_suffix[len];

    bytes = e - s;
    if (bytes > 0)
      {
	slot = make_string (s, bytes);
	Vexec_suffix_list = Fcons (slot, Vexec_suffix_list);
      }
    s = e + 1;
  }while (*e);

  Vexec_suffix_list = Fnreverse (Vexec_suffix_list);

  return;
}


DEFUN ("dos-to-unix-filename", Fdos_to_unix_filename, Sdos_to_unix_filename,
       1, 1, 0,
       "Return filename in unix form. To put it concretely, make '\\' in a \n\
filename into '/'.(indestructively)")
     (filename)
     Lisp_Object filename;
{
  int size;
  Lisp_Object rfilename;
  char* filetmp;
  
  CHECK_STRING (filename, 0);
  size = STRING_BYTES (XSTRING (filename));
  filetmp = (char *) alloca (size + 1);
  if (!filetmp)
    {
      error ("Cannot allocate memory!.");
    }
  memcpy (filetmp, XSTRING (filename)->data, size + 1);

  dostounix_filename (filetmp);

  return make_string (filetmp, size);
}

DEFUN ("unix-to-dos-filename", Funix_to_dos_filename, Sunix_to_dos_filename,
       1, 1, 0,
       "Return filename in dos form. To put it concretely, make '/' in a \n\
filename into '\\'.(indestructively). ")
     (filename)
     Lisp_Object filename;
{
  int size;
  Lisp_Object rfilename;
  char *filetmp;
  
  CHECK_STRING (filename, 0);
  size = STRING_BYTES (XSTRING (filename));
  filetmp = (char *) alloca (size + 1);
  if (!filetmp)
    {
      error ("Cannot allocate memory!.");
    }
  memcpy (filetmp, XSTRING (filename)->data, size + 1);

  unixtodos_filename (filetmp);

  return make_string (filetmp, size);
}

DEFUN ("unix-to-dos-argument", Funix_to_dos_argument,
       Sunix_to_dos_argument,
       5, 5, 0,
       "Return argument in dos form.")
     (filename, ep, h2sp, qp, s2isp)
     Lisp_Object filename, ep, h2sp, qp, s2isp;
{
  char *filetmp;

  CHECK_STRING (filename, 0);

  filetmp = (char *) alloca (STRING_BYTES (XSTRING (filename)) + 1);
  if (!filetmp)
    {
      error ("Cannot allocate memory!.");
    }
  memcpy (filetmp, XSTRING (filename)->data,
	  STRING_BYTES (XSTRING (filename)) + 1);
  unixtodos_argument (filetmp, !NILP (ep), !NILP (h2sp),
		      !NILP (qp), !NILP (s2isp));
  
  return build_string (filetmp);

}

DEFUN ("w32-sfn-to-lfn", Fw32_sfn_to_lfn, Sw32_sfn_to_lfn, 1, 1, 0,
       "Convert short filename(8.3) to long filename.\n\
If system fails to retrieve a long filename, return nil.")
     (sfn)
     Lisp_Object sfn;
{
  HANDLE hff;
  WIN32_FIND_DATA w32FindData;
  Lisp_Object directory, fullpath;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (sfn, 0);
  GCPRO2 (directory, fullpath);

  fullpath = Fencode_coding_string (Fexpand_file_name (sfn, Qnil),
				    Vfile_name_coding_system, Qt);
  directory = Ffile_name_directory (fullpath);
  hff = FindFirstFile (XSTRING (fullpath)->data, &w32FindData); 
  if (hff == INVALID_HANDLE_VALUE){
    return Qnil;
  }
  fullpath =
    Fexpand_file_name (Fdecode_coding_string
		       (build_string (w32FindData.cFileName),
			Vfile_name_coding_system, Qt), 
		       directory);
  FindClose (hff);

  UNGCPRO;
  return fullpath;
}

DEFUN ("w32-lfn-to-sfn", Fw32_lfn_to_sfn, Sw32_lfn_to_sfn, 1, 1, 0,
       "Convert long filename to short filename(8.3).\n\
If system fails to retrieve a short filename, return nil.")
     (lfn)
     Lisp_Object lfn;
{
#ifdef WIN32_VER4
  char buf[MAX_PATH+1];
  int  len;
  Lisp_Object fullpath;

  CHECK_STRING (lfn, 0);
  fullpath = Fencode_coding_string (Fexpand_file_name (lfn, Qnil),
				    Vfile_name_coding_system, Qt);
  len = GetShortPathName (XSTRING (fullpath)->data, buf, MAX_PATH);
  if (!len) 
    return Qnil;
  return Fdecode_coding_string (Fexpand_file_name (make_string (buf, len),
						   Qnil).
			       Vfile_name_coding_system, Qt);
#else
  return Qnil;
#endif
}

DEFUN ("w32-get-fqdn", Fw32_get_fqdn,
       Sw32_get_fqdn, 0, 0, 0,
       "Get the FQDN of this host.")
     ()
{
#define MAX_HOSTNAME 4096
#define RETRY_MAX 5
#define IS_HOSTNAME_FQDN(hostname) (strchr (hostname, '.'))

  char hostname[MAX_HOSTNAME];
  char **aliases;
  struct hostent *hp;
  int i;

  init_winsock (TRUE);
  sys_gethostname (hostname, MAX_HOSTNAME);
  if (!memchr (hostname, '\0', MAX_HOSTNAME))
    return Qnil;

  if (IS_HOSTNAME_FQDN (hostname))
    return build_string (hostname);

  for (i = 0;i < RETRY_MAX;i++)
    {
      hp = sys_gethostbyname (hostname);
      if (hp) break;
      if (TRY_AGAIN != pfn_WSAGetLastError())
	return Qnil;
    }

  if (IS_HOSTNAME_FQDN (hp->h_name))
    return build_string (hp->h_name);

  for (aliases = hp->h_aliases;*aliases;aliases++)
    {
      if (IS_HOSTNAME_FQDN (*aliases))
	return build_string (*aliases);
    }

  return Qnil;
}

DEFUN ("w32-find-executable", Fw32_find_executable,
       Sw32_find_executable, 1, 2, 0,
       "Find executable path that treat the specified FILE.\n\
The optional argument DIRECTORY specify the default directory.\n\
If the object executable is not found, return 'notfound. ")
     (file, directory)
     Lisp_Object file, directory;
{
  unsigned char buf[MAX_PATH + 1];
  char *p;
  UINT ret;

  CHECK_STRING (file, 0);
  if (NILP (directory)) p = NULL;
  else
    {
      CHECK_STRING (directory, 0);
      directory = Fencode_coding_string (directory,
					 Vfile_name_coding_system,
					 Qt);
      p = XSTRING (directory)->data;
      unixtodos_filename (p);
    }

  file = Fencode_coding_string (file, Vfile_name_coding_system, Qt);

  ret = (UINT) FindExecutable (XSTRING (file)->data, p, buf);

  if (ret == ERROR_FILE_NOT_FOUND)
    error ("specified file:%s is not found", XSTRING (file)->data);
  else if (ret == ERROR_PATH_NOT_FOUND)
    error ("specified directory is not found", directory);
  else if (ret == ERROR_BAD_FORMAT)
    return intern ("badformat");
  else if (ret == 31)
    return intern ("notfound");
  else if (ret < 31)
    error ("unknown error");

  dostounix_filename (buf);

  return Fdecode_coding_string (build_string (buf),
				Vfile_name_coding_system, Qt);
}

void
syms_of_w32misc ()
{
  DEFVAR_LISP ("exec-suffix-list", &Vexec_suffix_list,
	       "*List of suffixes for executable.");
  setup_exec_suffix_list();

  defsubr (&Sdos_to_unix_filename);
  defsubr (&Sunix_to_dos_filename);
  defsubr (&Sunix_to_dos_argument);
  defsubr (&Sw32_sfn_to_lfn);
  defsubr (&Sw32_lfn_to_sfn);
  defsubr (&Sw32_get_fqdn);
  defsubr (&Sw32_find_executable);
}

/* end of nt.c */

