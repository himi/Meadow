/**********
  Meadow Startup Module
  Created by H.Miyashita
**********/

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#ifdef SPECIAL_EXEC
#include "resource.h"
#endif

int win32_platform_id;
int invoke_meadowp;

void get_root(unsigned char *program, unsigned char *result)
{
  unsigned char dir_name[_MAX_DIR];
  unsigned char drive_name[_MAX_DRIVE];
  unsigned char file_name[_MAX_FNAME];
  unsigned char ext_name[_MAX_EXT];

  _splitpath(program, drive_name, dir_name, file_name, ext_name);

  sprintf(result, "%s%s", drive_name, dir_name);
}

#ifdef SPECIAL_EXEC
BOOL APIENTRY startdialog_wndproc(HWND hwnd,
				  UINT message,
				  UINT wParam,
				  LONG lParam)
{
  switch (message)
    {
    case WM_INITDIALOG:
      CheckRadioButton(hwnd,
		       IDC_RADIOMEADOW,
		       IDC_RADIOMULE,
		       IDC_RADIOMEADOW);
      return TRUE;

    case WM_COMMAND:
      // LOWORD added for portability
      invoke_meadowp = IsDlgButtonChecked(hwnd, IDC_RADIOMEADOW);
      if (LOWORD(wParam) == IDOK
	  || LOWORD(wParam) == IDCANCEL) {
	EndDialog(hwnd, TRUE);
	return TRUE;
      }
      break;
    }
  return FALSE;
}
#endif

int WINAPI
WinMain (HINSTANCE hinst, HINSTANCE hprevinst, LPSTR cmdline, int ncmdshow)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  PROCESS_INFORMATION child;
  unsigned char main_root[MAX_PATH];
  unsigned char this_prog[MAX_PATH];
  int version;
  int wait_for_child = FALSE;
  int no_window = FALSE;
  int ret_code = 0;
  unsigned char *runprog, *exec_name;

  version = GetVersion();

  if ( 0 == (version & 0x80000000) )
    win32_platform_id = 0;
  else if ((version & 0xC0000000) == 0xC0000000)
    win32_platform_id = 1;
  else {
    fprintf(stderr, "This Program can run only on Windows95/NT.\n");
    return 1;
  }

  GetModuleFileName(NULL, this_prog, MAX_PATH);
  get_root(this_prog, main_root);

#ifdef SPECIAL_EXEC
  {
    DialogBox(hinst, MAKEINTRESOURCE(IDD_STARTDIALOG),
	      GetDesktopWindow(), startdialog_wndproc);
    if (invoke_meadowp)
      {
	strcat(main_root, "Meadow\\" MEADOW "\\bin\\");
	exec_name = (unsigned char*) "MeadowCD.exe";
      }
    else
      {
	strcat(main_root, "MuleW32\\i386\\bin\\");
	exec_name = (unsigned char*) "mulecd.exe";
      }
    /* MessageBox(GetDesktopWindow(), main_root, exec_name, MB_OK); */
  }
  {
    unsigned char loadpath[MAX_PATH];
    char *p;
    strcpy(loadpath, main_root);
    p = strrchr(loadpath, '\\');
    if (p) *p = '\0';
    p = strrchr(loadpath, '\\');
    if (p) *p = '\0';
    strcat(loadpath, "\\lisp");

    strcpy(exec_pathname, main_root);

    SetEnvironmentVariable("EMACSPATH", exec_pathname);
    SetEnvironmentVariable("EMACSLOADPATH", loadpath);
    SetEnvironmentVariable("EMACS_NO_CONNECT", "1");
  }
#else
  exec_name = (unsigned char*) "Meadow.exe";
#endif

  runprog = (unsigned char*)alloca(strlen(main_root) + 
				   strlen(cmdline) +
				   MAX_PATH);

  if (strncmp (cmdline, "-raise", 6) == 0)
    {
      HWND hwnd;

      if (hwnd = FindWindow("Meadow", NULL))
	{
	  if (IsIconic(hwnd))
	    SendMessage(hwnd, WM_SYSCOMMAND, SC_RESTORE, 0);
	  SetForegroundWindow(hwnd);
	  return 0;
	}
      cmdline += 6;
    }
  /* Append original arguments if any; first look for -wait as first
     argument, and apply that ourselves.  */
  if (strncmp (cmdline, "-wait", 5) == 0)
    {
      wait_for_child = TRUE;
      cmdline += 5;
    }
  /* If with "-nw" option, assign console for child process */
  if (strncmp (cmdline, "-nw", 3) == 0)
    {
      no_window = TRUE;
    }

  sprintf(runprog, "%s%s %s", main_root, exec_name, cmdline);

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  if (no_window)
    {
      AllocConsole();
    }
  else
    {
      start.dwFlags = STARTF_USESHOWWINDOW;
      start.wShowWindow = SW_HIDE;
    }

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;

  if (CreateProcess (NULL, runprog, &sec_attrs, NULL, TRUE, 0,
		     NULL, NULL, &start, &child))
    {
      if (wait_for_child)
	{
	  WaitForSingleObject (child.hProcess, INFINITE);
	  GetExitCodeProcess (child.hProcess, (PDWORD) &ret_code);
	}
      CloseHandle (child.hThread);
      CloseHandle (child.hProcess);
    }
  else
    return 1;

  return (int) ret_code;

}
