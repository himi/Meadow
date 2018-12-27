/* MCI(Multimedia Control Interface) interaction layer header file.

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

#if !defined (_MW32MCH_H_)
#define _MW32MCH_H_

#include <windows.h>
#include <mmsystem.h>

enum mw32_notification_reason {
  MW32_MCI_NOTIFY_ABORTED,
  MW32_MCI_NOTIFY_FAILURE,
  MW32_MCI_NOTIFY_SUCCESSFUL,
  MW32_MCI_NOTIFY_SUPERSEDED
};

extern Lisp_Object Qmw32_mci;
extern Lisp_Object Qmw32_mci_notify;
extern Lisp_Object Qmw32_mci_notify_aborted;
extern Lisp_Object Qmw32_mci_notify_failure;
extern Lisp_Object Qmw32_mci_notify_successful;
extern Lisp_Object Qmw32_mci_notify_superseded;

typedef MCIERROR (WINAPI *MCISENDSTRINGPROC) (LPCTSTR lpszCommand,  
					      LPTSTR lpszReturnString,  
					      UINT cchReturn,       
					      HANDLE hwndCallback);

typedef BOOL (WINAPI *MCIGETERRORSTRINGPROC) (DWORD fdwError,
					      LPTSTR lpszErrorText,
					      UINT cchErrorText);

void mw32_mci_entry_notify_callback (Lisp_Object func, Lisp_Object arg);
void mw32_mci_call_notify_callback (void);

void syms_of_mw32mci (void);
void init_mw32mci (void);


#endif /* _MW32MCH_H_ */
