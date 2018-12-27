/* movemail foo bar -- move file foo to file bar,
   locking file foo the way /bin/mail respects.
   Copyright (C) 1986, 1992, 1993, 1994, 1996 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

/* Important notice: defining MAIL_USE_FLOCK or MAIL_USE_LOCKF *will
   cause loss of mail* if you do it on a system that does not normally
   use flock as its way of interlocking access to inbox files.  The
   setting of MAIL_USE_FLOCK and MAIL_USE_LOCKF *must agree* with the
   system's own conventions.  It is not a choice that is up to you.

   So, if your system uses lock files rather than flock, then the only way
   you can get proper operation is to enable movemail to write lockfiles there.
   This means you must either give that directory access modes
   that permit everyone to write lockfiles in it, or you must make movemail
   a setuid or setgid program.  */

/*
 * Modified January, 1986 by Michael R. Gretzinger (Project Athena)
 *
 * Added POP (Post Office Protocol) service.  When compiled -DMAIL_USE_POP
 * movemail will accept input filename arguments of the form
 * "po:username".  This will cause movemail to open a connection to
 * a pop server running on $MAILHOST (environment variable).  Movemail
 * must be setuid to root in order to work with POP.
 * 
 * New module: popmail.c
 * Modified routines:
 *	main - added code within #ifdef MAIL_USE_POP; added setuid (getuid ())
 *		after POP code. 
 * New routines in movemail.c:
 *	get_errmsg - return pointer to system error message
 *
 * Modified August, 1993 by Jonathan Kamens (OpenVision Technologies)
 *
 * Move all of the POP code into a separate file, "pop.c".
 * Use strerror instead of get_errmsg.
 *
 */

/*
 * Modified November, 1996 by YAMAGUCHI, Shuhei <yamagus@kw.netlaputa.or.jp>
 *
 * Added command line option for POP.
 * Default POP option behaviour is configuable with
 *   command name, win32-registry(win32 only) and environment variable.
 * All modified code is within #ifdef USE_POP_OPT.
 * Not tested under non x86-win32 systems.
 */

/*
 * Modified May, 1997 by TSUKAHARA, Hiroki <asuka@ba2.so-net.or.jp>
 *
 * Added progress dialog.
 * All modified code is within #ifdef PROGRESS_DIALOG
 * It works only under x86-win32 systems.
 * (It requires progdlg.c/progdlg.rc)
 *
 */

/*
 * Modified May, 1997 by YAMAGUCHI, Shuhei <yamagus@kw.netlaputa.or.jp>
 * Added command line option for Progress Dialog.
 * Fixed win32-registry code.
 *
 * I (and justly Mr. TSUKAHARA, too) repeat:
 *    There is no warranty, so do it as your own risk!
 */

/*
 * Modified Sep, 1997 by KOICHIRO, Ohba <koichiro@ca.mbn.or.jp>
 * Added including progdlg.h.
 * Fixed message handleing code.
 *
 */

/*
 * Modified Nov, 1998 by GOTO, Shun-ichi <gotoh@taiyo.co.jp>
 * Added code for APOP feature.
 *   New option, -A and -a  to use/unuse APOP. Default is unuse (-A)
 */

/*
 * Modified Aug 10, 1999 by GOTO, Shun-ichi <gotoh@taiyo.co.jp>
 *   Sync-up to Emacs 20.4, and modified to use GNU licensed md5.c and
 *   md5.h from glibc-2.1.1 instead of extracted code from RFC-1321.
 *   Add -d option to debug POP3 session.
 */

#define NO_SHORTNAMES   /* Tell config not to load remap.h */
#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <../src/syswait.h>
#include <getopt.h>
#ifdef MAIL_USE_POP
#include "pop.h"
#endif

#ifdef MSDOS
#undef access
#endif /* MSDOS */

#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif
#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif

#ifdef WINDOWSNT
#include "ntlib.h"
#include "io.h"
#define fork() 0
#define wait(var) (*(var) = 0)
/* Unfortunately, Samba doesn't seem to properly lock Unix files even
   though the locking call succeeds (and indeed blocks local access from
   other NT programs).  If you have direct file access using an NFS
   client or something other than Samba, the locking call might work
   properly - make sure it does before you enable this!

   [18-Feb-97 andrewi] I now believe my comment above to be incorrect,
   since it was based on a misunderstanding of how locking calls are
   implemented and used on Unix.  */
//#define DISABLE_DIRECT_ACCESS

/* Ensure all file i/o is in binary mode. */
#include <fcntl.h>
#ifndef __MINGW32__
int _fmode = _O_BINARY;
#endif
#endif /* WINDOWSNT */

/* Cancel substitutions made by config.h for Emacs.  */
#ifndef WINDOWSNT
#undef open
#undef read
#undef write
#undef close
#endif /* not WINDOWSNT */

#ifdef USG
#include <fcntl.h>
#include <unistd.h>
#ifndef F_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#endif
#endif /* USG */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#if defined (XENIX) || defined (WINDOWSNT)
#include <sys/locking.h>
#endif

#ifdef MAIL_USE_LOCKF
#define MAIL_USE_SYSTEM_LOCK
#endif

#ifdef MAIL_USE_FLOCK
#define MAIL_USE_SYSTEM_LOCK
#endif

#ifdef MAIL_USE_MMDF
extern int lk_open (), lk_close ();
#endif

#if !defined (MAIL_USE_SYSTEM_LOCK) && !defined (MAIL_USE_MMDF) && \
	defined (HAVE_LIBMAIL) && defined (HAVE_MAILLOCK_H)
#include <maillock.h>
/* We can't use maillock unless we know what directory system mail
   files appear in. */
#ifdef MAILDIR
#define MAIL_USE_MAILLOCK
static char *mail_spool_name ();
#endif
#endif

#ifndef errno
extern int errno;
#endif
char *strerror ();
extern char *rindex ();

void fatal ();
void error ();
void pfatal_with_name ();
void pfatal_and_delete ();
char *concat ();
long *xmalloc ();
int popmail ();
int pop_retr ();
int mbx_write ();
int mbx_delimit_begin ();
int mbx_delimit_end ();

/* Nonzero means this is name of a lock file to delete on fatal error.  */
char *delete_lockname;

#ifdef MEADOW
#define USE_POP_OPT
#endif /* MAIL_USE_POP */

#ifndef MAIL_USE_POP
#undef USE_POP_OPT
#endif /* MAIL_USE_POP */

#ifdef USE_POP_OPT
#include <string.h>

#ifdef WINDOWSNT
#include <stdlib.h>
#include <windows.h>
#ifdef PROGRESS_DIALOG
#include "progdlg.h"
#endif /* PROGRESS_DIALOG */
#define DIR_DELIM       '\\'
#else  /* not WINDOWSNT */
#include <sys/param.h>
#define DIR_DELIM       '/'
#endif /* not WINDOWSNT */

/* environment variable name of POP options */
#define POP_OPT_ENVNAME "MOVEMAIL"

/*
 * POP flags
 *   POP_UNREAD_ONLY and POP_FLUSH require "LAST", already obsoleted in RFC.
 */
#define POP_NO_OPTION   0       /* no options */
#define POP_CHECK_ONLY  (1 << 0) /* do only checking new mail */
#define POP_NO_DELE     (1 << 1) /* don't delete after retrieve */
#define POP_UNREAD_ONLY (1 << 2) /* retrieve only unread mail */
#define POP_FLUSH       (1 << 3) /* delete old mail before retrieve */
#ifdef PROGRESS_DIALOG
#define POP_PROGDLG     (1 << 4) /* display progress dialog */
#endif /* PROGRESS_DIALOG */
#define POP_APOP        (1 << 5) /* use APOP authentication */
#ifdef MAIL_USE_POP
#define POP_REVERSE     (1 << 6) /* reverse order */
#endif /* MAIL_USE_POP */

#ifdef WINDOWSNT
#define REG_STRMAX      256
#define MOVEMAIL_USER_KEY       "SOFTWARE\\GNU\\Mule\\Movemail"

#define REG_CHECK_ONLY  "CHECK_ONLY"
#define REG_NO_DELE     "NO_DELE"
#define REG_UNREAD_ONLY "UNREAD_ONLY"
#define REG_FLUSH       "FLUSH"
#define REG_PROGDLG     "PROGRESS_DIALOG"
#define REG_APOP        "APOP"
#define REG_REVERSE     "REVERSE"
#define REG_INNAME      "INNAME"
#define REG_OUTNAME     "OUTNAME"
#define REG_PASSWD      "PASSWORD"
#define REG_MAILHOST    "MAILHOST"
#endif /* WINDOWSNT */

/* Command name and default options */
typedef struct {
  char *name;
  int option;
} PopOpt;
static PopOpt prog_defaults[] = {
  { "retrmail", POP_NO_DELE|POP_UNREAD_ONLY, },
  { "msgchk", POP_CHECK_ONLY|POP_UNREAD_ONLY, },
};

long *xrealloc ();
static char *get_basename ();
void usage ();
void analyze_args ();
static int popoption;
#ifdef WINDOWSNT
void win32reg_opt ();
#define strcasecmp(a,b) stricmp((a), (b))
#endif /* WINDOWSNT */
#endif /* USE_POP_OPT */

int
main (argc, argv)
     int argc;
     char **argv;
{
  char *inname, *outname;
  int indesc, outdesc;
  int nread;
  WAITTYPE status;
  int preserve_mail = 0;

#ifndef MAIL_USE_SYSTEM_LOCK
  struct stat st;
  long now;
  int tem;
  char *lockname, *p;
  char *tempname;
  int desc;
#endif /* not MAIL_USE_SYSTEM_LOCK */

#ifdef MAIL_USE_MAILLOCK
  char *spool_name;
#endif

#ifdef MAIL_USE_POP
  int pop_reverse_order = 0;
# ifdef USE_POP_OPT
  char *env, *env_end;
  char **eargv;
  int eargc;
  char *argv0;
  char *passwd;
  int i;
#  define ARGSTR "rdcCkKuafFpPA"
# else /* ! USE_POP_OPT */
#  define ARGSTR "pr"
# endif /* USE_POP_OPT */
#else /* ! MAIL_USE_POP */
# define ARGSTR "p"
#endif /* MAIL_USE_POP */

#ifdef __MINGW32__
  _fmode = _O_BINARY;
#endif

  delete_lockname = 0;

#ifdef USE_POP_OPT
  popoption = POP_NO_OPTION;
  inname = outname = passwd = NULL;
  eargc = 0;
  eargv = NULL;
  argv0 = get_basename (argv[0]);
  argc--;
  argv++;

  /* decide default behaviour whith command name */
  for (i = 0; i < (sizeof(prog_defaults) / sizeof(prog_defaults[0])); i++)
    {
      if (!strcasecmp (argv0, prog_defaults[i].name))
        {
          popoption = prog_defaults[i].option;
          break;
        }
    }

#ifdef WINDOWSNT
  win32reg_opt (".Default", &popoption, &inname, &outname, &passwd);
  win32reg_opt (argv0, &popoption, &inname, &outname, &passwd);
#endif /* WINDOWSNT */

  /* analyze environment variable into words */
  if ((env = (char *) getenv (POP_OPT_ENVNAME)))
    {
      while (*env)
        {
          while (*env && isspace(*env))
            {
              env++;
            }
          if (!*env)
            {
              break;
            }
          eargc++;
          eargv = (char **) xrealloc (eargv, sizeof(char *) * eargc);
          env_end = env;
          while (*env_end && !isspace(*env_end))
            {
              env_end++;
            }
          eargv[eargc - 1] = (char *) xmalloc (env_end - env + 1);
          strncpy (eargv[eargc - 1], env, env_end - env);
          eargv[eargc - 1][env_end - env] = '\0';
          env = env_end;
        }
      analyze_args (eargc, eargv, &popoption, &inname, &outname, &passwd);
    }

  analyze_args (argc, argv, &popoption, &inname, &outname, &passwd);
  
  if ( popoption & POP_NO_DELE )
    preserve_mail++;

  if (popoption & POP_CHECK_ONLY)
    {
      if (inname == NULL)
        {
          usage ();
        }
    }
  else
    {
      if (inname == NULL || outname == NULL)
        {
          usage ();
        }
    }
#else  /* not USE_POP_OPT */
  if (argc < 3)
    {
      fprintf (stderr, "Usage: movemail inbox destfile [POP-password]\n");
      exit(1);
    }

  inname = argv[1];
  outname = argv[2];
#endif /* not USE_POP_OPT */

#ifdef MAIL_USE_MMDF
#ifdef USE_POP_OPT
  mmdf_init (argv0);
#else  /* not USE_POP_OPT */
  mmdf_init (argv[0]);
#endif /* not USE_POP_OPT */
#endif

#ifdef USE_POP_OPT
  if (!(popoption & POP_CHECK_ONLY))
    {
#endif /* USE_POP_OPT */
  if (*outname == 0)
    fatal ("Destination file name is empty", 0);

  /* Check access to output file.  */
  if (access (outname, F_OK) == 0 && access (outname, W_OK) != 0)
    pfatal_with_name (outname);

  /* Also check that outname's directory is writable to the real uid.  */
  {
    char *buf = (char *) xmalloc (strlen (outname) + 1);
    char *p;
    strcpy (buf, outname);
    p = buf + strlen (buf);
    while (p > buf && !IS_DIRECTORY_SEP (p[-1]))
      *--p = 0;
    if (p == buf)
      *p++ = '.';
    if (access (buf, W_OK) != 0)
      pfatal_with_name (buf);
    free (buf);
  }
#ifdef USE_POP_OPT
  }
#endif /* USE_POP_OPT */

#ifdef MAIL_USE_POP
  if (!strncmp (inname, "po:", 3))
    {
      int status;

#ifdef USE_POP_OPT
      status = popmail (inname + 3, outname, passwd, popoption);
#else  /* not USE_POP_OPT */
      status = popmail (inname + 3, outname, preserve_mail,
			(argc - optind == 3) ? argv[optind+2] : NULL,
			pop_reverse_order);
#endif /* not USE_POP_OPT */
      exit (status);
    }

  setuid (getuid ());
#endif /* MAIL_USE_POP */

#ifndef DISABLE_DIRECT_ACCESS

  /* Check access to input file.  */
  if (access (inname, R_OK | W_OK) != 0)
    pfatal_with_name (inname);

#ifndef MAIL_USE_MMDF
#ifndef MAIL_USE_SYSTEM_LOCK
#ifdef MAIL_USE_MAILLOCK
  spool_name = mail_spool_name (inname);
  if (! spool_name)
#endif
    {
      /* Use a lock file named after our first argument with .lock appended:
	 If it exists, the mail file is locked.  */
      /* Note: this locking mechanism is *required* by the mailer
	 (on systems which use it) to prevent loss of mail.

	 On systems that use a lock file, extracting the mail without locking
	 WILL occasionally cause loss of mail due to timing errors!

	 So, if creation of the lock file fails
	 due to access permission on the mail spool directory,
	 you simply MUST change the permission
	 and/or make movemail a setgid program
	 so it can create lock files properly.

	 You might also wish to verify that your system is one
	 which uses lock files for this purpose.  Some systems use other methods.

	 If your system uses the `flock' system call for mail locking,
	 define MAIL_USE_SYSTEM_LOCK in config.h or the s-*.h file
	 and recompile movemail.  If the s- file for your system
	 should define MAIL_USE_SYSTEM_LOCK but does not, send a bug report
	 to bug-gnu-emacs@prep.ai.mit.edu so we can fix it.  */

      lockname = concat (inname, ".lock", "");
      tempname = (char *) xmalloc (strlen (inname) + strlen ("EXXXXXX") + 1);
      strcpy (tempname, inname);
      p = tempname + strlen (tempname);
      while (p != tempname && !IS_DIRECTORY_SEP (p[-1]))
	p--;
      *p = 0;
      strcpy (p, "EXXXXXX");
      mktemp (tempname);
      unlink (tempname);

      while (1)
	{
	  /* Create the lock file, but not under the lock file name.  */
	  /* Give up if cannot do that.  */
	  desc = open (tempname, O_WRONLY | O_CREAT | O_EXCL, 0666);
	  if (desc < 0)
	    {
	      char *message = (char *) xmalloc (strlen (tempname) + 50);
	      sprintf (message, "%s--see source file lib-src/movemail.c",
		       tempname);
	      pfatal_with_name (message);
	    }
	  close (desc);

	  tem = link (tempname, lockname);
	  unlink (tempname);
	  if (tem >= 0)
	    break;
	  sleep (1);

	  /* If lock file is five minutes old, unlock it.
	     Five minutes should be good enough to cope with crashes
	     and wedgitude, and long enough to avoid being fooled
	     by time differences between machines.  */
	  if (stat (lockname, &st) >= 0)
	    {
	      now = time (0);
	      if (st.st_ctime < now - 300)
		unlink (lockname);
	    }
	}

      delete_lockname = lockname;
    }
#endif /* not MAIL_USE_SYSTEM_LOCK */
#endif /* not MAIL_USE_MMDF */

  if (fork () == 0)
    {
      int lockcount = 0;
      int status = 0;
#if defined (MAIL_USE_MAILLOCK) && defined (HAVE_TOUCHLOCK)
      long touched_lock, now;
#endif

      setuid (getuid ());

#ifndef MAIL_USE_MMDF
#ifdef MAIL_USE_SYSTEM_LOCK
      indesc = open (inname, O_RDWR);
#else  /* if not MAIL_USE_SYSTEM_LOCK */
      indesc = open (inname, O_RDONLY);
#endif /* not MAIL_USE_SYSTEM_LOCK */
#else  /* MAIL_USE_MMDF */
      indesc = lk_open (inname, O_RDONLY, 0, 0, 10);
#endif /* MAIL_USE_MMDF */

      if (indesc < 0)
	pfatal_with_name (inname);

#if defined (BSD_SYSTEM) || defined (XENIX)
      /* In case movemail is setuid to root, make sure the user can
	 read the output file.  */
      /* This is desirable for all systems
	 but I don't want to assume all have the umask system call */
      umask (umask (0) & 0333);
#endif /* BSD_SYSTEM || XENIX */
      outdesc = open (outname, O_WRONLY | O_CREAT | O_EXCL, 0666);
      if (outdesc < 0)
	pfatal_with_name (outname);

      /* This label exists so we can retry locking
	 after a delay, if it got EAGAIN or EBUSY.  */
    retry_lock:

      /* Try to lock it.  */
#ifdef MAIL_USE_MAILLOCK
      if (spool_name)
	{
	  /* The "0 - " is to make it a negative number if maillock returns
	     non-zero. */
	  status = 0 - maillock (spool_name, 1);
#ifdef HAVE_TOUCHLOCK
	  touched_lock = time (0);
#endif
	  lockcount = 5;
	}
      else
#endif /* MAIL_USE_MAILLOCK */
	{
#ifdef MAIL_USE_SYSTEM_LOCK
#ifdef MAIL_USE_LOCKF
	  status = lockf (indesc, F_LOCK, 0);
#else /* not MAIL_USE_LOCKF */
#ifdef XENIX
	  status = locking (indesc, LK_RLCK, 0L);
#else
#ifdef WINDOWSNT
	  status = locking (indesc, LK_RLCK, -1L);
#else
	  status = flock (indesc, LOCK_EX);
#endif
#endif
#endif /* not MAIL_USE_LOCKF */
#endif /* MAIL_USE_SYSTEM_LOCK */
	}

      /* If it fails, retry up to 5 times
	 for certain failure codes.  */
      if (status < 0)
	{
	  if (++lockcount <= 5)
	    {
#ifdef EAGAIN
	      if (errno == EAGAIN)
		{
		  sleep (1);
		  goto retry_lock;
		}
#endif
#ifdef EBUSY
	      if (errno == EBUSY)
		{
		  sleep (1);
		  goto retry_lock;
		}
#endif
	    }

	  pfatal_with_name (inname);
	}
  
      {
	char buf[1024];

	while (1)
	  {
	    nread = read (indesc, buf, sizeof buf);
	    if (nread != write (outdesc, buf, nread))
	      {
		int saved_errno = errno;
		unlink (outname);
		errno = saved_errno;
		pfatal_with_name (outname);
	      }
	    if (nread < sizeof buf)
	      break;
#if defined (MAIL_USE_MAILLOCK) && defined (HAVE_TOUCHLOCK)
	    if (spool_name)
	      {
		now = time (0);
		if (now - touched_lock > 60)
		  {
		    touchlock ();
		    touched_lock = now;
		  }
	      }
#endif /* MAIL_USE_MAILLOCK */
	  }
      }

#ifdef BSD_SYSTEM
      if (fsync (outdesc) < 0)
	pfatal_and_delete (outname);
#endif

      /* Check to make sure no errors before we zap the inbox.  */
      if (close (outdesc) != 0)
	pfatal_and_delete (outname);

#ifdef MAIL_USE_SYSTEM_LOCK
      if (! preserve_mail)
	{
#if defined (STRIDE) || defined (XENIX)
	  /* Stride, xenix have file locking, but no ftruncate.
	     This mess will do. */
	  close (open (inname, O_CREAT | O_TRUNC | O_RDWR, 0666));
#else
	  ftruncate (indesc, 0L);
#endif /* STRIDE or XENIX */
	}
#endif /* MAIL_USE_SYSTEM_LOCK */

#ifdef MAIL_USE_MMDF
      lk_close (indesc, 0, 0, 0);
#else
      close (indesc);
#endif

#ifndef MAIL_USE_SYSTEM_LOCK
      if (! preserve_mail)
	{
	  /* Delete the input file; if we can't, at least get rid of its
	     contents.  */
#ifdef MAIL_UNLINK_SPOOL
	  /* This is generally bad to do, because it destroys the permissions
	     that were set on the file.  Better to just empty the file.  */
	  if (unlink (inname) < 0 && errno != ENOENT)
#endif /* MAIL_UNLINK_SPOOL */
	    creat (inname, 0600);
	}
#endif /* not MAIL_USE_SYSTEM_LOCK */

#ifdef MAIL_USE_MAILLOCK
      /* This has to occur in the child, i.e., in the process that
         acquired the lock! */
      if (spool_name)
	mailunlock ();
#endif
      exit (0);
    }

  wait (&status);
  if (!WIFEXITED (status))
    exit (1);
  else if (WRETCODE (status) != 0)
    exit (WRETCODE (status));

#if !defined (MAIL_USE_MMDF) && !defined (MAIL_USE_SYSTEM_LOCK)
#ifdef MAIL_USE_MAILLOCK
  if (! spool_name)
#endif /* MAIL_USE_MAILLOCK */
    unlink (lockname);
#endif /* not MAIL_USE_MMDF and not MAIL_USE_SYSTEM_LOCK */

#endif /* ! DISABLE_DIRECT_ACCESS */

  return 0;
}

#ifdef MAIL_USE_MAILLOCK
/* This function uses stat to confirm that the mail directory is
   identical to the directory of the input file, rather than just
   string-comparing the two paths, because one or both of them might
   be symbolic links pointing to some other directory. */
static char *
mail_spool_name (inname)
     char *inname;
{
  struct stat stat1, stat2;
  char *indir, *fname;
  int status;

  if (! (fname = rindex (inname, '/')))
    return NULL;

  fname++;

  if (stat (MAILDIR, &stat1) < 0)
    return NULL;

  indir = (char *) xmalloc (fname - inname + 1);
  strncpy (indir, inname, fname - inname);
  indir[fname-inname] = '\0';


  status = stat (indir, &stat2);

  free (indir);

  if (status < 0)
    return NULL;

  if (stat1.st_dev == stat2.st_dev
      && stat1.st_ino == stat2.st_ino)
    return fname;

  return NULL;
}
#endif /* MAIL_USE_MAILLOCK */

/* Print error message and exit.  */

void
fatal (s1, s2)
     char *s1, *s2;
{
  if (delete_lockname)
    unlink (delete_lockname);
  error (s1, s2);
  exit (1);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

void
error (s1, s2, s3)
     char *s1, *s2, *s3;
{
  fprintf (stderr, "movemail: ");
  fprintf (stderr, s1, s2, s3);
  fprintf (stderr, "\n");
}

void
pfatal_with_name (name)
     char *name;
{
  char *s = concat ("", strerror (errno), " for %s");
  fatal (s, name);
}

void
pfatal_and_delete (name)
     char *name;
{
  char *s = concat ("", strerror (errno), " for %s");
  unlink (name);
  fatal (s, name);
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

long *
xmalloc (size)
     unsigned size;
{
  long *result = (long *) malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* This is the guts of the interface to the Post Office Protocol.  */

#ifdef MAIL_USE_POP

#ifndef WINDOWSNT
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#else
#undef _WINSOCKAPI_
#include <winsock.h>
#endif
#include <pwd.h>

#define NOTOK (-1)
#define OK 0
#define DONE 1

char *progname;
FILE *sfi;
FILE *sfo;
char ibuffer[BUFSIZ];
char obuffer[BUFSIZ];
char Errmsg[80];

#ifdef USE_POP_OPT
popmail (user, outfile, password, popoption)
     char *user;
     char *outfile;
     char *password;
     int popoption;
#else  /* not USE_POP_OPT */
popmail (user, outfile, preserve, password, reverse_order)
     char *user;
     int preserve;
     char *outfile;
     char *password;
     int reverse_order;
#endif /* not USE_POP_OPT */
{
  int nmsgs, nbytes;
  register int i;
  int mbfi;
  FILE *mbf;
  char *getenv ();
  popserver server;
  int start, end, increment;
#ifdef USE_POP_OPT
  int last_msg;
  int flags;
  int reverse_order = (popoption & POP_REVERSE);
  int preserve      = (popoption & POP_NO_DELE);
#endif /* USE_POP_OPT */

#ifdef USE_POP_OPT
  flags = 0;
  if (! (popoption & POP_CHECK_ONLY) )
    flags |= POP_NO_GETPASS;
  if (! (popoption & POP_APOP) )
    flags |= POP_NO_APOP;
  server = pop_open (0, user, password, flags);
#else  /* not USE_POP_OPT */
  server = pop_open (0, user, password, POP_NO_GETPASS);
#endif /* not USE_POP_OPT */
  if (! server)
    {
      error ("Error connecting to POP server: %s", pop_error);
      return (1);
    }

  if (pop_stat (server, &nmsgs, &nbytes))
    {
      error ("Error getting message count from POP server: %s", pop_error);
      return (1);
    }

#ifdef USE_POP_OPT
  if (popoption & POP_UNREAD_ONLY)
    {
      if ((last_msg = pop_last(server)) < 0)
        {
          error(pop_error);
          pop_close (server);
          return (1);
        }
    }
  else
    {
      last_msg = 0;
    }

  if (popoption & POP_CHECK_ONLY)
    {
      pop_close (server);
      if (last_msg > 0)
        {
          printf("%d messages in folder, %d new messages.\n",
                 nmsgs, nmsgs - last_msg);
          return (0);
        }
      if (nmsgs > 0)
        {
          printf("%d messages in folder.\n", nmsgs);
          return (0);
        }
      printf("You don't have any mail.\n");
      return (1);
    }

  if (popoption & POP_FLUSH)
    {
      for (i = 1; i <= last_msg; i++)
        {
          if (pop_delete(server, i))
            {
              error(pop_error);
              pop_close(server);
              return (1);
            }
        }
    }
#endif /* USE_POP_OPT */

#ifdef USE_POP_OPT
  if (!(nmsgs - last_msg))
#else  /* not USE_POP_OPT */
  if (!nmsgs)
#endif /* not USE_POP_OPT */
    {
      pop_close (server);
      return (0);
    }

  mbfi = open (outfile, O_WRONLY | O_CREAT | O_EXCL, 0666);
  if (mbfi < 0)
    {
      pop_close (server);
      error ("Error in open: %s, %s", strerror (errno), outfile);
      return (1);
    }
  fchown (mbfi, getuid (), -1);

  if ((mbf = fdopen (mbfi, "wb")) == NULL)
    {
      pop_close (server);
      error ("Error in fdopen: %s", strerror (errno));
      close (mbfi);
      unlink (outfile);
      return (1);
    }

#ifdef PROGRESS_DIALOG
  if (popoption & POP_PROGDLG)
    {
      progress_open(nmsgs - last_msg);
    }
#endif
  if (reverse_order)
    {
      start = nmsgs;
#ifdef USE_POP_OPT
      end = last_msg + 1;
#else /* not USE_POP_OPT */
      end = 1;
#endif /* not USE_POP_OPT */
      increment = -1;
    }
  else
    {
#ifdef USE_POP_OPT
      start = last_msg + 1;
#else /* not USE_POP_OPT */
      start = 1;
#endif /* not USE_POP_OPT */
      end = nmsgs;
      increment = 1;
    }

  for (i = start; i * increment <= end * increment; i += increment)
    {
      mbx_delimit_begin (mbf);
#ifdef PROGRESS_DIALOG
  if (popoption & POP_PROGDLG)
    {
      progress_stepit();
    }
#endif
      if (pop_retr (server, i, mbf) != OK)
	{
	  error ("%s", Errmsg, 0);
	  close (mbfi);
#ifdef PROGRESS_DIALOG
      if (popoption & POP_PROGDLG)
        {
          progress_close();
        }
#endif
	  return (1);
	}
      mbx_delimit_end (mbf);
      fflush (mbf);
      if (ferror (mbf))
	{
	  error ("Error in fflush: %s", strerror (errno));
	  pop_close (server);
	  close (mbfi);
#ifdef PROGRESS_DIALOG
      if (popoption & POP_PROGDLG)
        {
          progress_close();
        }
#endif
	  return (1);
	}
    }

#ifdef PROGRESS_DIALOG
  if (popoption & POP_PROGDLG)
    {
      progress_close();
    }
#endif
  /* On AFS, a call to write only modifies the file in the local
   *     workstation's AFS cache.  The changes are not written to the server
   *      until a call to fsync or close is made.  Users with AFS home
   *      directories have lost mail when over quota because these checks were
   *      not made in previous versions of movemail. */

#ifdef BSD_SYSTEM
  if (fsync (mbfi) < 0)
    {
      error ("Error in fsync: %s", strerror (errno), 0);
      return (1);
    }
#endif

  if (close (mbfi) == -1)
    {
      error ("Error in close: %s", strerror (errno));
      return (1);
    }

  if (! preserve)
#ifdef USE_POP_OPT
    for (i = last_msg + 1; i <= nmsgs; i++)
#else  /* not USE_POP_OPT */
    for (i = 1; i <= nmsgs; i++)
#endif /* not USE_POP_OPT */
      {
	if (pop_delete (server, i))
	  {
	    error ("Error from POP server: %s", pop_error);
	    pop_close (server);
	    return (1);
	  }
      }

  if (pop_quit (server))
    {
      error ("Error from POP server: %s", pop_error);
      return (1);
    }
    
  return (0);
}

int
pop_retr (server, msgno, arg)
     popserver server;
     FILE *arg;
{
  extern char *strerror ();
  char *line;
  int ret;

  if (pop_retrieve_first (server, msgno, &line))
    {
      char *error = concat ("Error from POP server: ", pop_error, "");
      strncpy (Errmsg, error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      free(error);
      return (NOTOK);
    }

  while ((ret = pop_retrieve_next (server, &line)) >= 0)
    {
      if (! line)
	break;

      if (mbx_write (line, ret, arg) != OK)
	{
	  strcpy (Errmsg, strerror (errno));
	  pop_close (server);
	  return (NOTOK);
	}
    }

  if (ret)
    {
      char *error = concat ("Error from POP server: ", pop_error, "");
      strncpy (Errmsg, error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      free(error);
      return (NOTOK);
    }

  return (OK);
}

#ifdef PROGRESS_DIALOG
void
do_events()
{
  if (popoption & POP_PROGDLG){
    progress_winmain();
  }
}
#endif /* PROGRESS_DIALOG */


/* Do this as a macro instead of using strcmp to save on execution time. */
#define IS_FROM_LINE(a) ((a[0] == 'F') \
			 && (a[1] == 'r') \
			 && (a[2] == 'o') \
			 && (a[3] == 'm') \
			 && (a[4] == ' '))

int
mbx_write (line, len, mbf)
     char *line;
     int len;
     FILE *mbf;
{
#ifdef MOVEMAIL_QUOTE_POP_FROM_LINES
  if (IS_FROM_LINE (line))
    {
      if (fputc ('>', mbf) == EOF)
	return (NOTOK);
    }
#endif
  if (line[0] == '\037')
    {
      if (fputs ("^_", mbf) == EOF)
	return (NOTOK);
      line++;
      len--;
    }
  if (fwrite (line, 1, len, mbf) != len) 
    return (NOTOK);
  if (fputc (0x0a, mbf) == EOF)
    return (NOTOK);
  return (OK);
}

int
mbx_delimit_begin (mbf)
     FILE *mbf;
{
  if (fputs ("\f\n0, unseen,,\n", mbf) == EOF)
    return (NOTOK);
  return (OK);
}

mbx_delimit_end (mbf)
     FILE *mbf;
{
  if (putc ('\037', mbf) == EOF)
    return (NOTOK);
  return (OK);
}

#endif /* MAIL_USE_POP */

#ifndef HAVE_STRERROR
char *
strerror (errnum)
     int errnum;
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}

#endif /* ! HAVE_STRERROR */

#ifdef USE_POP_OPT
long *
xrealloc (old, size)
     long *old;     
     unsigned size;
{
  long *result = (long *) realloc ((void *) old, size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

static char*
get_basename (path)
     char *path;
{
#ifdef WINDOWSNT
  static char base[_MAX_FNAME];
  char lpath[_MAX_PATH];
#else  /* not WINDOWSNT */
  /* static char base[MAXNAMELEN]; */
  static char base[MAXPATHLEN];
  char *fname;
  char *ext;
#endif /* not WINDOWSNT */

#ifdef WINDOWSNT
  GetModuleFileName (NULL, lpath, _MAX_PATH);
  _splitpath (lpath, NULL, NULL, base, NULL);
#else  /* not WINDOWSNT */
  if ((fname = strrchr (path, DIR_DELIM)) != NULL)
    {
      fname++;
    }
  else
    {
      fname = path;
    }

  if ((ext = strchr (fname, '.')) != NULL)
    {
      strncpy (base, fname, ext - fname);
      base[ext - fname] = '\0';
    }
  else
    {
      strcpy(base, fname);
    }
#endif /* not WINDOWSNT */

  return base;
}

void
usage ()
{
  fprintf (stderr,
           "Usage: movemail [-%s] inbox destfile [POP-password]\n",
	   ARGSTR);
  exit (1);
}


void
analyze_args (argc, argv, option, inname, outname, passwd)
     int argc;
     char *argv[];
     int *option;
     char **inname;
     char **outname;
     char **passwd;
{
  int i;
  extern int pop_debug;

  while (argc > 0 && (argv[0][0] == '-' || argv[0][0] == '/'))
    {
      if ((*argv)[1] == '\0')
        {
          --argc;
          ++argv;
          break;
        }
      for (i = 1; (*argv)[i] != '\0'; i++)
        {
          switch ((*argv)[i])
            {
              case 'c':         /* check only */
                *option |= POP_CHECK_ONLY;
                break;
              case 'C':         /* retrieve */
                *option &= ~POP_CHECK_ONLY;
                break;
              case 'k':         /* keep mail */
                *option |= POP_NO_DELE;
                break;
              case 'K':         /* kill mail */
                *option &= ~POP_NO_DELE;
                break;
              case 'u':         /* unread mail only */
                *option |= POP_UNREAD_ONLY;
                break;
              case 'a':         /* all mail */
                *option &= ~POP_UNREAD_ONLY;
                break;
              case 'f':         /* flush before */
                *option |= POP_FLUSH;
                break;
              case 'F':         /* don't flush before */
                *option &= ~POP_FLUSH;
                break;
#ifdef PROGRESS_DIALOG
              case 'p':           /* display progress dialog */
                *option |= POP_PROGDLG;
                break;
              case 'P':           /* don't display progress dialog */
                *option &= ~POP_PROGDLG;
                break;
#endif /* PROGRESS_DIALOG */
	      case 'A':		/* use APOP authentication */
		*option |= POP_APOP;
		break;
	      case 'r':		/* original -r option ... reverse */
		*option |= POP_REVERSE;
	      case 'd':		/* pop conversation debug */
	        pop_debug = 1;
		break;
              default:
                usage ();
                break;
            }
        }
      --argc;
      ++argv;
    }

  switch (argc)
    {
      case 3:
        *passwd = argv[2];
      case 2:
        *outname = argv[1];
      case 1:
        *inname = argv[0];
      case 0:
        break;
      default:
        usage ();
        break;
    }
}

#ifdef WINDOWSNT
void
win32reg_opt (base, option, inname, outname, passwd)
     char *base;
     int *option;
     char **inname;
     char **outname;
     char **passwd;
{
  HKEY hkey;
  DWORD type;
  DWORD length;
  char str[REG_STRMAX];
  DWORD dvalue;
  static char mailhost[REG_STRMAX + 9];
  char* env;
  long ret;

  /* open key */
  sprintf(str, "%s\\%s", MOVEMAIL_USER_KEY, base);
  ret = RegOpenKeyEx (HKEY_CURRENT_USER, str, 0, KEY_READ, &hkey);
  if (ret != ERROR_SUCCESS)
    {
      return;
    }

  /* REG_CHECK_ONLY */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_CHECK_ONLY, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_CHECK_ONLY;
            break;
          case 0:
            *option &= ~POP_CHECK_ONLY;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_NO_DELE */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_NO_DELE, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_NO_DELE;
            break;
          case 0:
            *option &= ~POP_NO_DELE;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_UNREAD_ONLY */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_UNREAD_ONLY, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_UNREAD_ONLY;
            break;
          case 0:
            *option &= ~POP_UNREAD_ONLY;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_FLUSH */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_FLUSH, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_FLUSH;
            break;
          case 0:
            *option &= ~POP_FLUSH;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_PROGDLG */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_PROGDLG, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_PROGDLG;
            break;
          case 0:
            *option &= ~POP_PROGDLG;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_APOP */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_APOP, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_APOP;
            break;
          case 0:
            *option &= ~POP_APOP;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_REVERSE */
  type = REG_DWORD;
  length = sizeof(dvalue);
  ret = RegQueryValueEx (hkey, REG_REVERSE, NULL,
                         &type, (LPBYTE) &dvalue, &length);
  if (ret == ERROR_SUCCESS)
    {
      switch (dvalue)
        {
          case 1:
            *option |= POP_REVERSE;
            break;
          case 0:
            *option &= ~POP_REVERSE;
            break;
          default:
            fatal ("Registry error!", 0);
            break;
        }
    }

  /* REG_INNAME */
  type = REG_SZ;
  length = sizeof(str);
  ret = RegQueryValueEx (hkey, REG_INNAME, NULL,
                         &type, (LPBYTE) str, &length);
  if (ret == ERROR_SUCCESS)
    {
      *inname = (char *) xmalloc (length);
      strcpy (*inname, str);
    }

  /* REG_OUTNAME */
  type = REG_SZ;
  length = sizeof(str);
  ret = RegQueryValueEx (hkey, REG_OUTNAME, NULL,
                         &type, (LPBYTE) str, &length);
  if (ret == ERROR_SUCCESS)
    {
      *outname = (char *) xmalloc (length);
      strcpy (*outname, str);
    }

  /* REG_PASSWD */
  type = REG_SZ;
  length = sizeof(str);
  ret = RegQueryValueEx (hkey, REG_PASSWD, NULL,
                         &type, (LPBYTE) str, &length);
  if (ret == ERROR_SUCCESS)
    {
      *passwd = (char *) xmalloc (length);
      strcpy (*passwd, str);
    }

  /* REG_MAILHOST */
  if (! (env = (char*) getenv ("MAILHOST")))
    {
      type = REG_SZ;
      length = sizeof(str - 9);
      ret = RegQueryValueEx (hkey, REG_MAILHOST, NULL,
                             &type, (LPBYTE) str, &length);
      if (ret == ERROR_SUCCESS)
        {
          sprintf (mailhost, "MAILHOST=%s", str);
          putenv (mailhost);
        }
	}

  RegCloseKey(hkey);
}
#endif /* WINDOWSNT */
#endif /* USE_POP_OPT */
