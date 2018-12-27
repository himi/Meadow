/* movemail foo bar -- move file foo to file bar,
   locking file foo the way /bin/mail respects.
   Copyright (C) 1986, 1992, 1993, 1994, 1996, 1997 Free Software Foundation, Inc.

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

/*
 * Modified Feb, 1998 by OOBA, Koichiro <koichiro@ca.mbn.or.jp>
 *
 * Added progress dialog.
 * All modified code is within #ifdef PROGRESS_DIALOG
 * It works only under x86-win32 systems.
 * (It requires progdlg.c/progdlg.h/progdlg.rc)
 * Special thanks to TSUKAHARA, Hiroki.
 *
 */

#include <windows.h>
#include <commctrl.h>

#define MOVEMAIL_USER_KEY  "SOFTWARE\\GNU\\Mule\\Movemail"

#define REG_RECT_TOP       "Top"
#define REG_RECT_LEFT      "Left"


static HINSTANCE s_hInst = NULL;
static HWND s_hWnd = NULL;
static int s_max;
static int s_current;

void
resume_window_pos (HWND hWnd)
{
  HKEY hkey;
  long ret;
  DWORD type;
  DWORD length;
  DWORD left;
  DWORD top;

  if (hWnd == 0) return;

  ret = RegOpenKeyEx(HKEY_CURRENT_USER, MOVEMAIL_USER_KEY, 0,
		     KEY_READ, &hkey);
  if (ret != ERROR_SUCCESS) return;

  type = REG_DWORD;
  length = sizeof(left);
  ret = RegQueryValueEx(hkey, REG_RECT_LEFT, NULL, &type,
			(LPBYTE) &left, &length);
  if (ret != ERROR_SUCCESS) {
    RegCloseKey(hkey);
    return;
  }
  
  type = REG_DWORD;
  length = sizeof(top);
  ret = RegQueryValueEx(hkey, REG_RECT_TOP, NULL, &type,
			(LPBYTE) &top, &length);
  if (ret != ERROR_SUCCESS) {
    RegCloseKey(hkey);
    return;
  }
  
  SetWindowPos(hWnd, NULL, left, top, 0, 0, 
	       SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
  RegCloseKey(hkey);
}

void
save_window_pos (HWND hWnd)
{
  HKEY hkey;
  long ret;
  RECT rect;
  DWORD disposition;

  if (hWnd == 0) return;
  ret = GetWindowRect(hWnd, &rect);
  if (ret == 0) return;

  ret = RegCreateKeyEx(HKEY_CURRENT_USER, MOVEMAIL_USER_KEY, 0,
		       REG_NONE, REG_OPTION_NON_VOLATILE,
		       KEY_WRITE, NULL, &hkey,
		       &disposition);
  if (ret != ERROR_SUCCESS) return;

  ret = RegSetValueEx(hkey, REG_RECT_TOP, 0, REG_DWORD, 
		      (LPBYTE) &rect.top, sizeof(DWORD));
  ret = RegSetValueEx(hkey, REG_RECT_LEFT, 0, REG_DWORD,
		      (LPBYTE) &rect.left, sizeof(DWORD));
  RegCloseKey(hkey);
}

BOOL CALLBACK
DlgProc (HWND hWnd,
	 UINT uMsg,
	 WPARAM wParam,
	 LPARAM lParam)
{
  static char buf[32];
  switch (uMsg)
    {
    case WM_INITDIALOG:
      resume_window_pos(hWnd);
      wsprintf(buf, "%d/%d", s_current, s_max);
      SendMessage(GetDlgItem(hWnd, 2), WM_SETTEXT, 0, (LPARAM) buf);
      return TRUE;
    case WM_DESTROY:
      save_window_pos(hWnd);
      return TRUE;
    case WM_COMMAND:
      SendMessage(GetDlgItem(hWnd, 1), PBM_STEPIT, 0, 0);
      wsprintf(buf, "%d/%d", s_current, s_max);
      SendMessage(GetDlgItem(hWnd, 2), WM_SETTEXT, 0, (LPARAM) buf);
      return TRUE;
    }

  return FALSE;
}

void
progress_open (int max)
{
  HWND hwndProgress;

  if (s_hInst != NULL) return;
  InitCommonControls();

  s_max = max;
  s_current = 0;

  s_hInst = GetModuleHandle(NULL);
  s_hWnd = CreateDialog(s_hInst, MAKEINTRESOURCE(1), NULL, DlgProc);
  hwndProgress = GetDlgItem(s_hWnd, 1);

  SendMessage(hwndProgress, PBM_SETRANGE, 0, MAKELPARAM(0, max));
  SendMessage(hwndProgress, PBM_SETSTEP, 1, 0);
  SendMessage(hwndProgress, PBM_SETPOS, 0, 0);

  ShowWindow(s_hWnd, SW_SHOWNOACTIVATE);
  UpdateWindow(s_hWnd);
}

void
progress_stepit ()
{
  if (s_hWnd != 0) {
    s_current++;
    if (s_current <= s_max) {
      SendMessage(s_hWnd, WM_COMMAND, 0, 0);
    }
  }
}

void
progress_close ()
{
  if (s_hWnd != 0) {
    DestroyWindow(s_hWnd);
    s_hWnd = NULL;
    s_hInst = NULL;
  }
}

void
progress_winmain ()
{
  MSG msg;

  if( s_hWnd != 0 ){
    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
}
