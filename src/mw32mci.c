/* MCI(Multimedia Control Interface) interaction layer.

This file is part of Meadow(Modified version of GNU Emacs).

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

/* 2003/02/23 created by MIYOSHI Masanori (miyoshi@boreas.dti.ne.jp) */

#include "config.h"
#include "lisp.h"
#include "charset.h"
#include "coding.h"
#include "mw32term.h"
#include <windows.h>
#include <mmsystem.h>
#include "mw32mci.h"

Lisp_Object Qmw32_mci;
Lisp_Object Qmw32_mci_notify;
Lisp_Object Qmw32_mci_notify_aborted;
Lisp_Object Qmw32_mci_notify_failure;
Lisp_Object Qmw32_mci_notify_successful;
Lisp_Object Qmw32_mci_notify_superseded;

#if !defined (BUFSIZE)
#define BUFSIZE 1024
#endif /* BUFFSIZE */

static MCISENDSTRINGPROC mciSendStringProc = NULL;
static MCIGETERRORSTRINGPROC mciGetErrorStringProc = NULL;


DEFUN ("mw32-mci-send-string", Fmw32_mci_send_string, Smw32_mci_send_string,
       1, 1, 0,
       "Send a command string to an MCI device. \
COMMAND is a string that specifies an MCI command.  Return a string \
that describes result information if successful or an error code \
otherwise.")
     (command)
     Lisp_Object command;
{
  TCHAR return_string[BUFSIZE];

  return_string[0] = '\0';
  if (!STRINGP (command))
    error ("Invalid command string");

  if (mciSendStringProc)
    {
      MCIERROR ret;
      ret = (*mciSendStringProc)(mw32_encode_lispy_string(Vw32_system_coding_system,
							  command, NULL),
				 return_string, BUFSIZE,
				 FRAME_MW32_WINDOW (XFRAME (selected_frame)));
      if (ret != 0)
	return make_number (ret);
      return mw32_decode_lispy_string (Vw32_system_coding_system,
				       return_string, 0);
    }
  else
    return Qnil;
}

DEFUN ("mw32-mci-get-error-string", Fmw32_mci_get_error_string,
       Smw32_mci_get_error_string,
       1, 1, 0,
       "Retrieves a string that describes the specified MCI ERROR-CODE.")
     (error_code)
     Lisp_Object error_code;
{
  TCHAR error_string[BUFSIZE];

 error_string[0] = '\0';
  if (!INTEGERP (error_code))
    error ("Invalid error-code");

  if (mciGetErrorStringProc)
    {
      BOOL ret = (*mciGetErrorStringProc) (XFASTINT (error_code),
					   error_string, BUFSIZE);
      if (!ret)
	error ("Unknown Error code: %d", error_code);
      return mw32_decode_lispy_string (Vw32_system_coding_system,
				       error_string, 0);
    }
  else
    return Qnil;
}

void
syms_of_mw32mci (void)
{
  defsubr (&Smw32_mci_send_string);
  defsubr (&Smw32_mci_get_error_string);

  Qmw32_mci = intern ("mw32-mci");
  staticpro (&Qmw32_mci);
  Qmw32_mci_notify = intern ("mw32-mci-notify");
  staticpro (&Qmw32_mci_notify);
  Qmw32_mci_notify_aborted = intern ("mw32-mci-notify-aborted");
  staticpro (&Qmw32_mci_notify_aborted);
  Qmw32_mci_notify_failure = intern ("mw32-mci-notify-failure");
  staticpro (&Qmw32_mci_notify_failure);
  Qmw32_mci_notify_successful = intern ("mw32-mci-notify-successful");
  staticpro (&Qmw32_mci_notify_successful);
  Qmw32_mci_notify_superseded = intern ("mw32-mci-notify-superseded");
  staticpro (&Qmw32_mci_notify_superseded);
}

void
init_mw32mci (void)
{
  HANDLE winmm_lib = LoadLibrary ("winmm.dll");

  if (winmm_lib != NULL)
    {
      mciSendStringProc = ((MCISENDSTRINGPROC)
			   GetProcAddress (winmm_lib, "mciSendStringA"));
      mciGetErrorStringProc = ((MCIGETERRORSTRINGPROC)
			       GetProcAddress (winmm_lib,
					       "mciGetErrorStringA"));
    }
}
