/* IME specific staff for later W32 version4.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.

This file is part of Meadow.

Meadow is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Meadow is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Meadow; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* 97.10.13 written by himi */

#include <windows.h>
#include "config.h"
#include "lisp.h"
#include <imm.h>

#ifdef HAVE_NTGUI
#include "frame.h"
#include "window.h"
#include "charset.h"
#include "coding.h"
#include "mw32term.h"
#endif /* not HAVE_NTGUI */

#include <dde.h>

#ifndef HAVE_NTGUI
extern HWND hwndConsole;
#endif /* not HAVE_NTGUI */
extern HINSTANCE hinst;
extern HINSTANCE hprevinst;
extern LPSTR lpCmdLine;
extern int nCmdShow;

#define CHECK_IME_FACILITY \
  if (!fIME) error ("System have no IME facility.")

#define IMMCONTEXTCAR(imc) \
  (XFASTINT ((((unsigned long) (imc)) >> 16) & 0xffff))

#define IMMCONTEXTCDR(imc) \
  (XFASTINT (((unsigned long) (imc)) & 0xffff))

#ifdef IME_CONTROL

#ifndef IME_SETOPEN
#define IME_SETOPEN 4
#endif
#ifndef IME_GETOPEN
#define IME_GETOPEN 5
#endif
#ifndef IME_SETCONVERSIONFONTEX
#define IME_SETCONVERSIONFONTEX 25
#endif
#ifndef IME_SETCONVERSIONWINDOW
#define IME_SETCONVERSIONWINDOW 8
#endif
#ifndef MCW_WINDOW
#define MCW_WINDOW 2
#endif

#if 0
#define IMC_SETCOMPOSITIONWINDOW        0x000C
typedef struct COMPOSITIONFORM {
  DWORD dwStyle;
  POINT ptCurrentPos;
  RECT  rcArea;
} COMPOSITIONFORM;
#define CFS_FORCE_POSITION           0x20
#define CFS_POINT                    0x02
#endif

#define MAX_CONVAGENT 100

typedef struct conversion_agent {
  HIMC himc;
  HWND hwnd;
} conversion_agent;

static conversion_agent agent[MAX_CONVAGENT];
static int conversion_agent_num = -1;

BOOL fIME = FALSE;
typedef LRESULT (WINAPI *SENDIMEMESSAGEEXPROC)(HWND, LPARAM);
SENDIMEMESSAGEEXPROC SendIMEMessageExProc;

typedef BOOL (WINAPI *IMMGETOPENSTATUSPROC)(HIMC);
IMMGETOPENSTATUSPROC ImmGetOpenStatusProc;
typedef BOOL (WINAPI *IMMSETOPENSTATUSPROC)(HIMC, BOOL);
IMMSETOPENSTATUSPROC ImmSetOpenStatusProc;

typedef HWND (WINAPI *IMMGETDEFAULTIMEWNDPROC)(HWND);
IMMGETDEFAULTIMEWNDPROC ImmGetDefaultIMEWndProc;
typedef LONG (WINAPI *IMMGETCOMPOSITIONSTRINGPROC)
     (HIMC, DWORD, LPVOID, DWORD);
IMMGETCOMPOSITIONSTRINGPROC ImmGetCompositionStringProc;
typedef LONG (WINAPI *IMMSETCOMPOSITIONSTRINGPROC)
     (HIMC, DWORD, LPCVOID, DWORD, LPCVOID, DWORD);
IMMSETCOMPOSITIONSTRINGPROC ImmSetCompositionStringProc;
typedef BOOL (WINAPI *IMMSETCOMPOSITIONFONTPROC) (HIMC, LPLOGFONTA);
IMMSETCOMPOSITIONFONTPROC ImmSetCompositionFontProc;
typedef HIMC (WINAPI *IMMGETCONTEXTPROC)(HWND);
IMMGETCONTEXTPROC ImmGetContextProc;
typedef BOOL (WINAPI *IMMGETCONVERSIONSTATUSPROC)(HIMC, LPDWORD, LPDWORD);
IMMGETCONVERSIONSTATUSPROC ImmGetConversionStatusProc;
typedef BOOL (WINAPI *IMMSETCONVERSIONSTATUSPROC)(HIMC, DWORD, DWORD);
IMMSETCONVERSIONSTATUSPROC ImmSetConversionStatusProc;
typedef BOOL (WINAPI *IMMGETCONVERSIONLISTPROC)
     (HKL, HIMC, LPCTSTR, LPCANDIDATELIST, DWORD, UINT);
IMMGETCONVERSIONLISTPROC ImmGetConversionListProc;
typedef BOOL (WINAPI *IMMCONFIGUREIMEPROC)(HKL, HWND, DWORD, LPVOID);
IMMCONFIGUREIMEPROC ImmConfigureIMEProc;
typedef BOOL (WINAPI *IMMNOTIFYIMEPROC)(HIMC, DWORD, DWORD, DWORD);
IMMNOTIFYIMEPROC ImmNotifyIMEProc;
typedef BOOL (WINAPI *IMMRELEASECONTEXTPROC)(HWND, HIMC);
IMMRELEASECONTEXTPROC ImmReleaseContextProc;
typedef HIMC (WINAPI *IMMCREATECONTEXTPROC)(void);
IMMCREATECONTEXTPROC ImmCreateContextProc;
typedef BOOL (WINAPI *IMMDESTROYCONTEXTPROC)(HIMC);
IMMDESTROYCONTEXTPROC ImmDestroyContextProc;
typedef BOOL (WINAPI *IMMASSOCIATECONTEXTPROC) (HWND, HIMC);
IMMASSOCIATECONTEXTPROC ImmAssociateContextProc;
typedef BOOL (WINAPI *IMMGETCANDIDATELISTPROC)
(HIMC, DWORD, LPCANDIDATELIST, DWORD);
IMMGETCANDIDATELISTPROC ImmGetCandidateListProc;
typedef BOOL (WINAPI *IMMGETCANDIDATELISTCOUNTPROC) (HIMC, LPDWORD);
IMMGETCANDIDATELISTCOUNTPROC ImmGetCandidateListCountProc;
typedef BOOL (WINAPI *IMMGETHOTKEYPROC)(DWORD , LPUINT, LPUINT, LPHKL);
IMMGETHOTKEYPROC ImmGetHotKeyProc;

extern Lisp_Object Vime_control;

static WPARAM wIMEOpen;

Lisp_Object Vime_control;
Lisp_Object VIME_command_off_flag;
Lisp_Object Qim_info;


int IME_event_off_count;

void
mw32_set_ime_conv_window (HWND hwnd, int x, int y)
{
  if (fIME && !NILP (Vime_control))
    {
      HWND IMEhwnd;
      COMPOSITIONFORM compform;

      IMEhwnd = (ImmGetDefaultIMEWndProc) (hwnd);
      compform.dwStyle = CFS_FORCE_POSITION; /* CFS_POINT; */
      compform.ptCurrentPos.x = x;
      compform.ptCurrentPos.y = y;

      SendMessage (IMEhwnd, WM_IME_CONTROL, (WPARAM) IMC_SETCOMPOSITIONWINDOW,
		   (LPARAM) (&compform));
      return;
    }
}

void
mw32_set_ime_status (HWND hwnd, int openp)
{
  HIMC himc;

  himc = (ImmGetContextProc) (hwnd);
  (ImmSetOpenStatusProc) (himc, openp);
  (ImmReleaseContextProc) (hwnd, himc);

  return;
}

int
mw32_get_ime_status (HWND hwnd)
{
  HIMC himc;
  int ret;

  himc = (ImmGetContextProc) (hwnd);
  ret = (ImmGetOpenStatusProc) (himc);
  (ImmReleaseContextProc) (hwnd, himc);

  return ret;
}

int
mw32_set_ime_mode (HWND hwnd, int mode, int mask)
{
  HIMC himc;
  DWORD cmode, smode;

  himc = (ImmGetContextProc) (hwnd);
  if (!(ImmGetConversionStatusProc) (himc, &cmode, &smode)) return 0;

  cmode = (cmode & (~mask)) | (mode & mask);

  (ImmSetConversionStatusProc) (himc, cmode, smode);
  (ImmReleaseContextProc) (hwnd, himc);

  return 1;
}

int
mw32_get_ime_undetermined_string_length (HWND hwnd)
{
  long len;
  HIMC himc;

  himc = (ImmGetContextProc) (hwnd);
  if (!himc) return 0;
  len = (ImmGetCompositionStringProc) (himc, GCS_COMPSTR, NULL, 0);
  (ImmReleaseContextProc) (hwnd, himc);
  return len;
}

BOOL
mw32_get_ime_composition_string (HWND hwnd)
{
  HIMC hIMC;
  int size;
  HANDLE himestr;
  LPSTR lpstr;
  struct frame *f;
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO (hwnd);
  
  hIMC = (ImmGetContextProc) (hwnd);
  if (!hIMC) return FALSE;
  
  size = (ImmGetCompositionStringProc) (hIMC, GCS_RESULTSTR, NULL, 0);
  size += sizeof (TCHAR);
  himestr = GlobalAlloc (GHND, size);
  if (!himestr) abort ();
  lpstr = GlobalLock (himestr);
  if (!lpstr) abort ();
  (ImmGetCompositionStringProc) (hIMC, GCS_RESULTSTR, lpstr, size);
  (ImmReleaseContextProc) (hwnd, hIMC);
  GlobalUnlock (himestr);
  f = mw32_window_to_frame (dpyinfo, hwnd);
  PostMessage (NULL, WM_MULE_IME_REPORT, 
	       (WPARAM) himestr, (LPARAM) f);
  return TRUE;
}
	  
void
mw32_ime_control_init (void)
{
  HMODULE hImm32;
  HMODULE hUser32;
  hImm32 = GetModuleHandle ("IMM32.DLL");
  if (!hImm32) hImm32 = LoadLibrary ("IMM32.DLL");

  fIME = FALSE;
  Vime_control = Qnil;
  IME_event_off_count = 0;

  if (hImm32)
    {
      ImmGetOpenStatusProc =
	(IMMGETOPENSTATUSPROC)
	GetProcAddress (hImm32,
			"ImmGetOpenStatus");
      ImmSetOpenStatusProc =
	(IMMSETOPENSTATUSPROC)
	GetProcAddress (hImm32,
			"ImmSetOpenStatus");
      ImmGetDefaultIMEWndProc =
	(IMMGETDEFAULTIMEWNDPROC)
	GetProcAddress (hImm32,
			"ImmGetDefaultIMEWnd");
      ImmGetCompositionStringProc =
	(IMMGETCOMPOSITIONSTRINGPROC)
	GetProcAddress (hImm32, "ImmGetCompositionStringA");
      ImmSetCompositionStringProc =
	(IMMSETCOMPOSITIONSTRINGPROC)
	GetProcAddress (hImm32, "ImmSetCompositionStringA");
      ImmSetCompositionFontProc =
	(IMMSETCOMPOSITIONFONTPROC)
	GetProcAddress (hImm32, "ImmSetCompositionFontA");
      ImmGetContextProc =
	(IMMGETCONTEXTPROC) 
	GetProcAddress (hImm32,
			"ImmGetContext");
      ImmGetConversionStatusProc =
	(IMMGETCONVERSIONSTATUSPROC) 
	GetProcAddress (hImm32,
			"ImmGetConversionStatus");
      ImmSetConversionStatusProc =
	(IMMSETCONVERSIONSTATUSPROC) 
	GetProcAddress (hImm32,
			"ImmSetConversionStatus");
      ImmGetConversionListProc =
	(IMMGETCONVERSIONLISTPROC) 
	GetProcAddress (hImm32,
			"ImmGetConversionListA");
      ImmConfigureIMEProc =
	(IMMCONFIGUREIMEPROC) 
	GetProcAddress (hImm32,
			"ImmConfigureIMEA");
      ImmNotifyIMEProc =
	(IMMNOTIFYIMEPROC) 
	GetProcAddress (hImm32,
			"ImmNotifyIME");
      ImmReleaseContextProc =
	(IMMRELEASECONTEXTPROC)
	GetProcAddress (hImm32,
			"ImmReleaseContext");
      ImmCreateContextProc =
	(IMMCREATECONTEXTPROC)
	GetProcAddress (hImm32,
			"ImmCreateContext");
      ImmDestroyContextProc =
	(IMMDESTROYCONTEXTPROC)
	GetProcAddress (hImm32,
			"ImmDestroyContext");
      ImmAssociateContextProc =
	(IMMASSOCIATECONTEXTPROC)
	GetProcAddress (hImm32,
			"ImmAssociateContext");
      ImmGetCandidateListProc =
	(IMMGETCANDIDATELISTPROC)
	GetProcAddress (hImm32,
			"ImmGetCandidateListA");
      ImmGetCandidateListCountProc =
	(IMMGETCANDIDATELISTCOUNTPROC)
	GetProcAddress (hImm32,
			"ImmGetCandidateListCountA");
      ImmGetHotKeyProc =
	(IMMGETHOTKEYPROC)
	GetProcAddress (hImm32,
			"ImmGetHotKey");

      if (ImmGetOpenStatusProc &&
	  ImmSetOpenStatusProc &&
	  ImmGetDefaultIMEWndProc &&
	  ImmGetCompositionStringProc &&
	  ImmSetCompositionStringProc &&
	  ImmSetCompositionFontProc &&
	  ImmGetContextProc &&
	  ImmGetConversionStatusProc &&
	  ImmSetConversionStatusProc &&
	  ImmGetConversionListProc &&
	  ImmConfigureIMEProc &&
	  ImmNotifyIMEProc &&
	  ImmReleaseContextProc &&
	  ImmCreateContextProc &&
	  ImmDestroyContextProc &&
	  ImmAssociateContextProc &&
	  ImmGetCandidateListProc &&
	  ImmGetCandidateListCountProc &&
	  ImmGetHotKeyProc)
	{
	  fIME = TRUE;
	  Vime_control = Qt;
	}
    }
}

#ifdef HAVE_NTGUI
void
mw32_set_ime_font (HWND hwnd, LPLOGFONT psetlf)
{
  HIMC himc;

  if (fIME && psetlf && !NILP (Vime_control))
    {
      himc = (ImmGetContextProc) (hwnd);
      if (!himc) return;
      (ImmSetCompositionFontProc) (himc, psetlf);
      (ImmReleaseContextProc) (hwnd, himc);
    }
}
#endif  /* HAVE_NTGUI */

/* From here, communication programs to make IME a conversion machine. */

void
check_immcontext (Lisp_Object context)
{
  if (NUMBERP (context))
    {
      if (!((XFASTINT (context) >= 0) &&
	    (XFASTINT (context) < MAX_CONVAGENT)))
	error ("Wrong number of agent");
    }
  else
    {
      CHECK_LIST (context, 0);
      CHECK_NUMBER (XCONS (context)->car, 0);
      CHECK_NUMBER (XCONS (context)->cdr, 0);
    }
}

HIMC
immcontext (Lisp_Object context)
{
  if (NUMBERP (context))
    return agent[XFASTINT (context)].himc;
  else
    return ((((unsigned long) (XCONS (context)->car)) << 16) |
	    (((unsigned long) (XCONS (context)->cdr)) & 0xffff));
}

LRESULT CALLBACK
conversion_agent_wndproc (HWND hwnd, UINT message,
			  WPARAM wparam, LPARAM lparam)
{
  HIMC himc, holdimc;

  switch (message)
    {
    case WM_CREATE:
      himc = (ImmCreateContextProc) ();
      holdimc = (ImmAssociateContextProc) (hwnd, himc);
      SetWindowLong (hwnd, 0, himc);
      SetWindowLong (hwnd, 4, holdimc);
      break;

    case WM_DESTROY:
      holdimc = GetWindowLong (hwnd, 4);
      himc = (ImmAssociateContextProc) (hwnd, holdimc);
      (ImmDestroyContextProc) (himc);
      break;

    case WM_MULE_IMM_SET_STATUS:
      mw32_set_ime_status (hwnd, (int) wparam);
      break;

    case WM_MULE_IMM_GET_STATUS:
      return mw32_get_ime_status (hwnd);

    case WM_MULE_IMM_GET_UNDETERMINED_STRING_LENGTH:
      return mw32_get_ime_undetermined_string_length (hwnd);

    case WM_MULE_IMM_SET_MODE:
      return mw32_set_ime_mode (hwnd, (int) wparam, (int) lparam);

    case WM_MULE_IMM_SET_COMPOSITION_STRING:
#if 0
      return mw32_set_ime_composition_string (hwnd,
					      (int) wparam, (int) lparam);
#endif

    case WM_MULE_IMM_GET_COMPOSITION_STRING:
      return mw32_get_ime_composition_string (hwnd);

    case WM_MULE_IMM_NOTIFY:
#if 0
      return mw32_ime_notify (hwnd, (int) wparam, (int) lparam);
#endif

    default:
      return DefWindowProc (hwnd, message, wparam, lparam);
    }
  return 0;
}

int
initialize_conversion_agent ()
{
  int i;
  WNDCLASS wc;
  
  for (i = 0; i < MAX_CONVAGENT; i++)
    {
      agent[i].hwnd = 0;
      agent[i].himc = 0;
    }

  wc.style         = 0;
  wc.lpfnWndProc   = conversion_agent_wndproc;
  wc.cbClsExtra    = 0;
  wc.cbWndExtra    = sizeof (long) * 2;
  wc.hInstance     = hinst;
  wc.hIcon         = NULL;
  wc.hCursor       = NULL;
  wc.hbrBackground = NULL;
  wc.lpszMenuName  = NULL;
  wc.lpszClassName = CONVAGENT_CLASS;

  if (!RegisterClass (&wc))
    return 0;

  return 1;
}

#if 0
void
generate_ime_hot_key (HWND hwnd)
{
  HKL imehkl;
  UINT modifier;
  UINT vkey;
  (ImmGetHotKeyProc) (IME_JHOTKEY_CLOSE_OPEN,
		      &modifier, &vkey, &imehkl);
#endif

Lisp_Object
get_style_lisp_object (DWORD dwStyle)
{
  switch (dwStyle)
    {
    case IME_CAND_READ:
      return intern ("read");
    case IME_CAND_CODE:
      return intern ("code");
    case IME_CAND_MEANING:
      return intern ("meaning");
    case IME_CAND_RADICAL:
      return intern ("radical");
    case IME_CAND_STROKE:
      return intern ("stroke");
    case IME_CAND_UNKNOWN:
      return intern ("unknown");
    default:
      break;
    }

  return Qnil;
}

Lisp_Object
get_attribute_lisp_object (BYTE attr)
{
  switch (attr)
    {
    case ATTR_INPUT:
      return intern ("input");
    case ATTR_TARGET_CONVERTED:
      return intern ("target-converted");
    case ATTR_CONVERTED:
      return intern ("converted");
    case ATTR_TARGET_NOTCONVERTED:
      return intern ("target-not-converted");
    case ATTR_INPUT_ERROR:
      return intern ("input-error");
    default:
      break;
    }
  return Qnil;
}

BYTE lisp_object_to_attribute_data (Lisp_Object attr)
{
  if (EQ (attr, intern ("input")))
    return ATTR_INPUT;
  else if (EQ (attr, intern ("target-converted")))
    return ATTR_TARGET_CONVERTED;
  else if (EQ (attr, intern ("converted")))
    return ATTR_CONVERTED;
  else if (EQ (attr, intern ("target-not-converted")))
    return ATTR_TARGET_NOTCONVERTED;
  else if (EQ (attr, intern ("input-error")))
    return ATTR_INPUT_ERROR;
  else
    error ("Wrong attribute");

  return 0;
}



/*
  Emacs Lisp function entries
*/

DEFUN ("fep-force-on", Ffep_force_on, Sfep_force_on, 0, 1, 0,
       "Force status of IME open.")
  (eventp)
     Lisp_Object eventp;
{
  if (fIME && !NILP (Vime_control))
    {
      HIMC himc;
      HWND hwnd;

      if (!NILP (Ffep_get_mode ())) return Qnil;
#ifdef HAVE_NTGUI
      if (NILP (eventp))
	IME_event_off_count++;
      hwnd = FRAME_MW32_WINDOW (SELECTED_FRAME ());
#else
      hwnd = hwndConsole;
#endif
      SendMessage (hwnd, WM_MULE_IMM_SET_STATUS, 1, 0);
    }
  return Qnil;
}


DEFUN ("fep-force-off", Ffep_force_off, Sfep_force_off, 0, 1, 0,
       "Force status of IME close.")
  (eventp)
     Lisp_Object eventp;
{
  if (fIME && !NILP (Vime_control))
    {
      HIMC himc;
      HWND hwnd;

      if (NILP (Ffep_get_mode ())) return Qnil;
#ifdef HAVE_NTGUI
      if (NILP (eventp))
	IME_event_off_count++;
      hwnd = FRAME_MW32_WINDOW (SELECTED_FRAME ());
#else
      hwnd = hwndConsole;
#endif
      SendMessage (hwnd, WM_MULE_IMM_SET_STATUS, 0, 0);
    }
  return Qnil;
}


DEFUN ("fep-get-mode", Ffep_get_mode, Sfep_get_mode, 0, 0, "",
       "Get IME status.\n\
t means status of IME is open.  nil means it is close.")
  ()
{
  if (fIME && !NILP (Vime_control))
    {
      HWND hwnd;
      int result;

#ifdef HAVE_NTGUI
      hwnd = FRAME_MW32_WINDOW (SELECTED_FRAME ());
#else
      hwnd = hwndConsole;
#endif
      result = SendMessage (hwnd, WM_MULE_IMM_GET_STATUS, 0, 0);
      
      return result ? Qt : Qnil;
    }
  else
    return Qnil;
}

DEFUN ("w32-ime-undetermined-string-length",
       Fw32_ime_undetermined_string_length,
       Sw32_ime_undetermined_string_length, 0, 0, "",
       "Return length in byte of undetermined strings the current IME have.")
     ()
{

  if (fIME && !NILP (Vime_control))
    {
      HWND hwnd;
      long len;

#ifdef HAVE_NTGUI
      hwnd = FRAME_MW32_WINDOW (SELECTED_FRAME ());
#else
      hwnd = hwndConsole;
#endif
      len = SendMessage (hwnd, WM_MULE_IMM_GET_UNDETERMINED_STRING_LENGTH,
			 0, 0);

      return make_number (len);
    }
  else
    return Qnil;
}

DEFUN ("w32-set-ime-mode",
       Fw32_set_ime_mode,
       Sw32_set_ime_mode, 1, 2, 0,
       "Set IME mode to MODE. If FRAME is omitted, the selected frame is used.")
     (mode, frame)
     Lisp_Object mode, frame;
{
  FRAME_PTR f;

  if (NILP (frame))
    {
      f = SELECTED_FRAME ();
    }
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }
  if (fIME && !NILP (Vime_control))
    {
      HWND hwnd;
      int ret;
      int newmode, mask;

      newmode = 0;
      mask = 0;

      hwnd = FRAME_MW32_WINDOW (f);

      if (EQ (mode, intern ("katakana")))
	{
	  newmode |= IME_CMODE_KATAKANA;
	  mask |= IME_CMODE_KATAKANA;
	}
      else if (EQ (mode, intern ("hiragana")))
	{
	  newmode &= ~IME_CMODE_KATAKANA;
	  mask |= IME_CMODE_KATAKANA;
	}
      else if (EQ (mode, intern ("kanji")))
	{
	  newmode |= IME_CMODE_HANJACONVERT;
	  mask |= IME_CMODE_HANJACONVERT;
	}
      else if (EQ (mode, intern ("nokanji")))
	{
	  newmode &= ~IME_CMODE_HANJACONVERT;
	  mask |= IME_CMODE_HANJACONVERT;
	}
      else if (EQ (mode, intern ("code")))
	{
	  newmode |= IME_CMODE_CHARCODE;
	  mask |= IME_CMODE_CHARCODE;
	}
      else if (EQ (mode, intern ("nocode")))
	{
	  newmode &= ~IME_CMODE_CHARCODE;
	  mask |= IME_CMODE_CHARCODE;
	}
      else if (EQ (mode, intern ("noconvert")))
	{
	  newmode |= IME_CMODE_NOCONVERSION;
	  mask |= IME_CMODE_NOCONVERSION;
	}
      else if (EQ (mode, intern ("convert")))
	{
	  newmode &= ~IME_CMODE_NOCONVERSION;
	  mask |= IME_CMODE_NOCONVERSION;
	}
      else
	error ("unknown mode!!");

      ret = SendMessage (hwnd, WM_MULE_IMM_SET_MODE,
			 (WPARAM) newmode, (LPARAM) mask);

      if (!ret) return Qnil;
      return Qt;
    }
  return Qnil;
}

DEFUN ("w32-ime-register-word-dialog",
       Fw32_ime_register_word_dialog,
       Sw32_ime_register_word_dialog, 2, 2, 0,
       "Open IME regist word dialog.")
     (reading, word)
     Lisp_Object reading, word;
{
  HKL hkl;
  int len;
  REGISTERWORD regword;
  char *creading, *cword;

  CHECK_STRING (reading, 0);
  CHECK_STRING (word, 1);

  if (fIME && !NILP (Vime_control) && ImmConfigureIMEProc)
    {
      hkl = GetKeyboardLayout (0);
      MW32_ENCODE_TEXT (reading, Vw32_system_coding_system, &creading, &len);
      regword.lpReading = creading;
      MW32_ENCODE_TEXT (word, Vw32_system_coding_system, &cword, &len);
      regword.lpWord = cword;

      ShowCursor (TRUE);
      (ImmConfigureIMEProc) (hkl, FRAME_MW32_WINDOW (SELECTED_FRAME ()),
			     IME_CONFIG_REGISTERWORD, &regword);
      ShowCursor (FALSE);
    }
  return Qnil;
}

#if 0   /* incomplete */

DEFUN ("w32-ime-get-conversion-list",
       Fw32_ime_get_conversion_list,
       Sw32_ime_get_conversion_list, 2, 2, 0,
       "Get IME conversion list.\n\
OBJECT is converted by IME.  Return candidates.\n\
OPTION is as follows currently.\n\
1....'forward\n\
      convert normally.\n\
2....'backward\n\
      convert backward.\n\
\n\
OPTION will be revised in the future.\n\
\n\
Result have the form as follow.\n\
(STYLE CANDIDATE1 CANDIDATE2 ...)\n\
")
     (object, option)
     Lisp_Object object, option;
{
  Lisp_Object result, style;
  struct gcpro gcpro1, gcpro2;
  HKL hkl;
  HIMC himc;
  UINT flag;
  int i, len, nbytes;
  LPCANDIDATELIST lpcd;
  MEADOW_ENCODE_ALLOC_PREDEFINE;

  CHECK_STRING (object, 0);
  if (EQ (option, intern ("forward")))
    flag = GCL_CONVERSION;
  else if (EQ (option, intern ("backward")))
    flag = GCL_REVERSECONVERSION;
  else
    error ("Unknown option %s", option);

  hkl = GetKeyboardLayout (0);
  himc = (ImmCreateContextProc) ();

  MEADOW_ENCODE_ALLOC (LISPY_STRING_BYTES (object));
  MEADOW_ENCODE (XSTRING (object)->data, LISPY_STRING_BYTES (object));
  len = MEADOW_ENCODE_PRODUCED;
  MEADOW_ENCODE_BUF[len] = '\0';

  nbytes = (ImmGetConversionListProc) (hkl, himc, MEADOW_ENCODE_BUF,
				       NULL, 0, flag);

  lpcd = (LPCANDIDATELIST) alloca (nbytes);
  if (!lpcd) return Qnil;

  (ImmGetConversionListProc) (hkl, himc, MEADOW_ENCODE_BUF,
			      lpcd, nbytes, flag);

  (ImmDestroyContextProc) (himc);

  result = Qnil;

  style = get_style_lisp_object (lpcd->dwStyle);

  GCPRO2 (style, result);

  for (i = lpcd->dwCount - 1;i >= 0;i--)
    {
      result = Fcons (build_string (((unsigned char *) lpcd) +
				    lpcd->dwOffset[i]),
		      result);
    }
  
  UNGCPRO;

  return result;
}

#endif

#ifdef ENABLE_IMM_CONTEXT

DEFUN ("w32-ime-create-context",
       Fw32_ime_create_context,
       Sw32_ime_create_context, 0, 0, 0,
       "Create IME context.")
     ()
{
  HIMC himc;

  CHECK_IME_FACILITY;

  himc = (ImmCreateContextProc) ();

  return Fcons (IMMCONTEXTCAR (himc), IMMCONTEXTCDR (himc));
}

DEFUN ("w32-ime-destroy-context",
       Fw32_ime_destroy_context,
       Sw32_ime_destroy_context, 1, 1, 0,
       "Destroy IME context.")
     (context)
     Lisp_Object context;
{
  HIMC himc;

  CHECK_IME_FACILITY;

  check_immcontext (context);
  himc = immcontext (context);

  (ImmDestroyContextProc) (himc);
  return Qnil;
}

DEFUN ("w32-ime-associate-context",
       Fw32_ime_associate_context,
       Sw32_ime_associate_context, 1, 2, 0,
       "Associate IME CONTEXT to FRAME.\n\
Return an old context handle.")
     (context, frame)
     Lisp_Object context, frame;
{
  HWND hwnd;
  HIMC himc;

  CHECK_IME_FACILITY;

  check_immcontext (context);

  if (NILP (frame))
    hwnd = FRAME_MW32_WINDOW (selected_frame);
  else
    {
      CHECK_FRAME (frame, 0);
      hwnd = FRAME_MW32_WINDOW (XFRAME (frame));
    }

  himc = immcontext (context);
  himc = (ImmAssociateContextProc) (hwnd, himc);

  return Fcons (IMMCONTEXTCAR (himc), IMMCONTEXTCDR (himc));
}

#endif /* ENABLE_IMM_CONTEXT */

DEFUN ("w32-ime-create-conversion-agent",
       Fw32_ime_create_conversion_agent,
       Sw32_ime_create_conversion_agent, 0, 0, 0,
       "Create conversion agent.")
     ()
{
  int i;
  MSG msg;
  HWND hwnd;
  HIMC himc;

  CHECK_IME_FACILITY;

  if (conversion_agent_num == -1)
    {
      if (!initialize_conversion_agent ())
	return Qnil;
      conversion_agent_num = 0;
    }
  else if (conversion_agent_num == MAX_CONVAGENT)
    return Qnil;

  conversion_agent_num++;

  for (i = 0;i < MAX_CONVAGENT;i++)
    if (!agent[i].himc) break;

  SEND_MSGTHREAD_INFORM_MESSAGE (WM_MULE_IME_CREATE_AGENT,
				 (WPARAM) 0, (LPARAM) 0);
  WAIT_REPLY_MESSAGE (&msg, WM_MULE_IME_CREATE_AGENT_REPLY);
  hwnd = (HWND) msg.wParam;
  agent[i].hwnd = hwnd;
  agent[i].himc = GetWindowLong (hwnd, 0);

  /*  ShowWindow (hwnd, SW_SHOW); */

  SendMessage (hwnd, WM_MULE_IMM_SET_STATUS, 1, 0);

  return XFASTINT (i);
}

DEFUN ("w32-ime-destroy-conversion-agent",
       Fw32_ime_destroy_conversion_agent,
       Sw32_ime_destroy_conversion_agent, 1, 1, 0,
       "Destroy conversion agent.")
     (convagent)
     Lisp_Object convagent;
{
  int num;
  MSG msg;
  HWND hwnd;

  CHECK_IME_FACILITY;
  CHECK_NUMBER (convagent, 0);
  num = XINT (convagent);

  if ((conversion_agent_num == 0) ||
      (num < 0) ||
      (num > MAX_CONVAGENT) ||
      (agent[num].himc == 0))
    error ("Fail to destroy agent");

  conversion_agent_num--;

  (ImmSetOpenStatusProc) (agent[num].himc, FALSE);

  SEND_INFORM_MESSAGE (agent[num].hwnd, WM_MULE_IME_DESTROY_AGENT,
		       (WPARAM) 0, (LPARAM) 0);
  WAIT_REPLY_MESSAGE (&msg, WM_MULE_IME_DESTROY_AGENT_REPLY);
  agent[num].hwnd = 0;
  agent[num].himc = 0;
  
  return Qnil;
}

DEFUN ("w32-ime-set-composition-string",
       Fw32_ime_set_composition_string,
       Sw32_ime_set_composition_string, 3, 4, 0,
       "Set IME composition string.\n\
CONTEXT must be a valid context handle.\n\
COMPOSITION must be an alist consists of clause information.\n\
FIELD specifies the field of composition strings which must be\n\
one of the follows.\n\
  'clause\n\
  'string\n\
  'attribute\n\
When READINGP is non-nil, reading strings would be set.")
       (context, composition, field, readingp)
     Lisp_Object context, composition, field, readingp;
{
  HIMC himc;
  Lisp_Object curclause, curelem, str, attr;
  unsigned char *context_string, *cs;
  BYTE *attr_array, *aa;
  DWORD *clause_array, *ca;
  int clause_num, size, strsize, str_total_size;
  /*  int start_idx, end_idx; */
  struct coding_system coding;

  clause_num = size = 0;

  CHECK_IME_FACILITY;

  check_immcontext (context);
  CHECK_LIST (composition, 0);
  CHECK_SYMBOL (field, 0);
  himc = immcontext (context);

  for (curclause = composition; CONSP (curclause);
       curclause = XCONS (curclause)->cdr)
    {
      CHECK_LIST (curclause, 0);
      curelem = XCONS (curclause)->car;
      CHECK_LIST (curelem, 0);
      str = XCONS (curelem)->car;
      attr = XCONS (curelem)->cdr;
      CHECK_STRING (str, 0);
      CHECK_SYMBOL (attr, 0);
      size += LISPY_STRING_BYTES (str);
      clause_num++;
    }

  if (size == 0) return Qnil;

  setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system),
		       &coding);
  size = encoding_buffer_size (&coding, size);

  attr_array = (BYTE *) alloca (size * sizeof (BYTE));
  context_string = (unsigned char *) alloca (size * sizeof (char));
  clause_array = (DWORD *) alloca ((clause_num + 1) * sizeof (DWORD));
  if ((!attr_array) || (!context_string) || (!clause_array))
    error ("Can't allocate memory!");

  aa = attr_array;
  cs = context_string;
  ca = clause_array;
  *ca = 0;

  str_total_size = 0;
  for (curclause = composition; CONSP (curclause);
       curclause = XCONS (curclause)->cdr)
    {
      curelem = XCONS (curclause)->car;
      str = XCONS (curelem)->car;
      attr = XCONS (curelem)->cdr;
      encode_coding (&coding, XSTRING (str)->data, cs,
		     LISPY_STRING_BYTES (str), size);
      strsize = coding.produced;
      str_total_size += strsize;
      size -= strsize;
      cs += strsize;
      memset (aa,
	      lisp_object_to_attribute_data (attr),
	      strsize);
      aa += strsize;
      *(ca + 1) = *ca + strsize;
      ca++;
    }

  if (!NILP (readingp))
    {
      if (EQ (field, intern ("string")))
	(ImmSetCompositionStringProc) (himc, SCS_SETSTR, NULL, 0,
				       context_string, str_total_size);
      else if (EQ (field, intern ("clause")))
	(ImmSetCompositionStringProc) (himc, SCS_CHANGECLAUSE, NULL, 0,
				       clause_array,
				      (clause_num + 1) * sizeof (DWORD));
      else if (EQ (field, intern ("attribute")))
	(ImmSetCompositionStringProc) (himc, SCS_CHANGEATTR, NULL, 0,
				       attr_array, str_total_size);
      else
	error ("Unknown field:%s", XSYMBOL (field)->name);
    }
  else
    {
      if (EQ (field, intern ("string")))
	(ImmSetCompositionStringProc) (himc, SCS_SETSTR,
				       context_string, str_total_size,
				       NULL, 0);
      else if (EQ (field, intern ("clause")))
	(ImmSetCompositionStringProc) (himc, SCS_CHANGECLAUSE,
				       clause_array,
				       (clause_num + 1) * sizeof (DWORD),
				       NULL, 0);
      else if (EQ (field, intern ("attribute")))
	(ImmSetCompositionStringProc) (himc, SCS_CHANGEATTR,
				       attr_array, str_total_size,
				       NULL, 0);
      else
	error ("Unknown field:%s", XSYMBOL (field)->name);
    }

  return Qnil;

}       
 
DEFUN ("w32-ime-get-composition-string",
       Fw32_ime_get_composition_string,
       Sw32_ime_get_composition_string, 2, 2, 0,
       "Get IME composition strings.\n\
INFO means as follows.\n\
\n\
    'comp.........current composition information.\n\
    'compread.....current reading composition information.\n\
    'result.......resultant composition information.\n\
    'resultread...resultant reading composition information.")

     (context, info)
     Lisp_Object context, info;
{
  HIMC himc;
  DWORD clause_index, attr_index, str_index;

  DWORD *clause_array;
  BYTE *attr_array, *aa;
  unsigned char *compstr, *cs;
  long clause_size, attr_size, compstr_size;

  int start_idx, end_idx;

  int i, clause_num, size;

  Lisp_Object str;

  Lisp_Object result = Qnil;

  CHECK_IME_FACILITY;

  check_immcontext (context);
  himc = immcontext (context);

  if (EQ (info, intern ("comp")))
    {
      clause_index = GCS_COMPCLAUSE;
      attr_index = GCS_COMPATTR;
      str_index = GCS_COMPSTR;
     }
  else if (EQ (info, intern ("compread")))
    {
      clause_index = GCS_COMPREADCLAUSE;
      attr_index = GCS_COMPREADATTR;
      str_index = GCS_COMPREADSTR;
    }
  else if (EQ (info, intern ("result")))
    {
      clause_index = GCS_RESULTCLAUSE;
      attr_index = GCS_COMPATTR;
      str_index = GCS_RESULTSTR;
    }
  else if (EQ (info, intern ("resultread")))
    {
      clause_index = GCS_RESULTREADCLAUSE;
      attr_index = GCS_COMPREADATTR;
      str_index = GCS_RESULTREADSTR;
    }
  else 
    error ("Invalid option!");

  clause_size = (ImmGetCompositionStringProc) (himc, clause_index, NULL, 0);
  attr_size = (ImmGetCompositionStringProc) (himc, attr_index, NULL, 0);
  compstr_size = (ImmGetCompositionStringProc) (himc, str_index, NULL, 0);

  if ((clause_size < 0) ||
      (attr_size < 0) ||
      (compstr_size < 0))
    error ("IME internal error!");

  clause_array = (DWORD *) alloca (clause_size);
  attr_array = (BYTE *) alloca (attr_size);
  compstr = (unsigned char *) alloca (compstr_size);

  if ((!attr_array) || (!compstr) || (!clause_array))
    error ("Can't allocate memory!");

  if (((ImmGetCompositionStringProc)
       (himc, clause_index, clause_array, clause_size) < 0) ||
      ((ImmGetCompositionStringProc)
       (himc, attr_index, attr_array, attr_size) < 0) ||
      ((ImmGetCompositionStringProc)
       (himc, str_index, compstr, compstr_size) < 0))
    error ("IME internal error!");

  clause_num = clause_size / sizeof (DWORD) - 2;
  end_idx = 0;
  for (i = clause_num; i >= 0; i--)
    {
      cs = compstr + clause_array[i];
      aa = attr_array + clause_array[i];
      size = clause_array[i + 1] - clause_array[i];
      
      start_idx = end_idx;
      str = Fdecode_coding_string (make_string (cs, size),
				   Vw32_system_coding_system,
				   Qt);
      end_idx = start_idx + LISPY_STRING_BYTES (str);
      result = concat2 (result, str);

      Fput_text_property (start_idx, end_idx, Qim_info, 
			  get_attribute_lisp_object (*aa),
			  result);
    }

  return result;
}

DEFUN ("w32-ime-get-candidate-list",
       Fw32_ime_get_candidate_list,
       Sw32_ime_get_candidate_list, 2, 2, 0,
       "Get IME candidate list.")
     (context, index)
     Lisp_Object context, index;
{
  HIMC himc;
  DWORD counts;
  LPCANDIDATELIST lpcd;
  int size, i;
  Lisp_Object style, result = Qnil;

  CHECK_IME_FACILITY;
  check_immcontext (context);
  CHECK_NUMBER (index, 0);
  himc = immcontext (context);

  size = (ImmGetCandidateListCountProc) (himc, &counts);
  lpcd = (LPCANDIDATELIST) alloca (size);

  if (!lpcd)
    error ("Can't allocate memory!");

  if (!(ImmGetCandidateListProc) (himc, XFASTINT (index), lpcd, size))
    error ("Can't retrieve any candidate lists.");

  for (i = lpcd->dwCount - 1; i >= 0; i--)
    {
      result = 
	Fcons (Fdecode_coding_string (build_string (((unsigned char *) lpcd) +
						    lpcd->dwOffset[i]),
				      Vw32_system_coding_system, Qt),
	       result);
    }

  style = get_style_lisp_object (lpcd->dwStyle);

  result = Fcons (style, result);

  return result;
}

DEFUN ("w32-ime-select-candidate",
       Fw32_ime_select_candidate,
       Sw32_ime_select_candidate, 3, 3, 0,
       "Select a candidate.\n\
CONTEXT must be a valid context handle.\n\
CLAUSE must be a clause index where you want\n\
to change the current candidate.\n\
CANDIDATE must be a candidate index.")
     (context, clause, candidate)
     Lisp_Object context, clause, candidate;
{
  HIMC himc;

  CHECK_IME_FACILITY;
  check_immcontext (context);
  CHECK_NUMBER (clause, 0);
  CHECK_NUMBER (candidate, 0);

  himc = immcontext (context);
  if (!(ImmNotifyIMEProc) (himc, NI_SELECTCANDIDATESTR,
			   XFASTINT (clause), XFASTINT (candidate)))
    error ("Fail to select candidate!");

  return Qnil;
}

DEFUN ("w32-ime-change-candidate",
       Fw32_ime_change_candidate,
       Sw32_ime_change_candidate, 2, 2, 0,
       "change a candidate list.")
     (context, clause)
     Lisp_Object context, clause;
{
  HIMC himc;

  CHECK_IME_FACILITY;
  check_immcontext (context);
  CHECK_NUMBER (clause, 0);

  himc = immcontext (context);

  if (!(ImmNotifyIMEProc) (himc, NI_CHANGECANDIDATELIST,
			   XFASTINT (clause), 0));
    error ("Fail to change a candidate list!");

  return Qnil;
}

DEFUN ("w32-ime-open-candidate",
       Fw32_ime_open_candidate,
       Sw32_ime_open_candidate, 2, 2, 0,
       "Open a candidate list.")
     (context, clause)
     Lisp_Object context, clause;
{
  HIMC himc;

  CHECK_IME_FACILITY;
  check_immcontext (context);
  CHECK_NUMBER (clause, 0);

  himc = immcontext (context);

  if (!(ImmNotifyIMEProc) (himc, NI_OPENCANDIDATE,
			   XFASTINT (clause), 0));
    error ("Fail to open a candidate list!");

  return Qnil;
}
  
DEFUN ("w32-ime-close-candidate",
       Fw32_ime_close_candidate,
       Sw32_ime_close_candidate, 2, 2, 0,
       "Open a candidate list.")
     (context, clause)
     Lisp_Object context, clause;
{
  HIMC himc;

  CHECK_IME_FACILITY;
  check_immcontext (context);
  CHECK_NUMBER (clause, 0);

  himc = immcontext (context);

  if (!(ImmNotifyIMEProc) (himc, NI_CLOSECANDIDATE,
			   XFASTINT (clause), 0));
    error ("Fail to close a candidate list!");

  return Qnil;
}
  
DEFUN ("w32-ime-deal-with-context",
       Fw32_ime_deal_with_context,
       Sw32_ime_deal_with_context, 2, 2, 0,
       "Notify IME to deal with a context.\n\
You can command IME to change a context.\n\
OPERATION must be one of the followings.\n\
\n\
  'cancel .... clear the composition strings.\n\
  'complete .. make the composition strings result strings.\n\
  'convert ... convert the composition strings.\n\
  'revert .... revert the composition strings.")
     (context, operation)
     Lisp_Object context, operation;
{
  HIMC himc;
  DWORD op;

  CHECK_IME_FACILITY;

  check_immcontext (context);

  CHECK_SYMBOL (operation, 0);

  himc = immcontext (context);

  if (EQ (operation, intern ("cancel")))
    op = CPS_CANCEL;
  else if (EQ (operation, intern ("complete")))
    op = CPS_COMPLETE;
  else if (EQ (operation, intern ("convert")))
    op = CPS_CONVERT;
  else if (EQ (operation, intern ("revert")))
    op = CPS_REVERT;
  else
    error ("Unknown operation:%s", XSYMBOL (operation)->name);

  if (!(ImmNotifyIMEProc) (himc, NI_COMPOSITIONSTR,
			   op, 0))
    error ("Fail to deal with the context.");

  return Qnil;
}

#endif /* IME_CONTROL */

syms_of_mw32ime ()
{
#ifdef IME_CONTROL

  Qim_info = intern ("im_info");
  staticpro (&Qim_info);

  DEFVAR_LISP ("ime-control", &Vime_control, "IME control flag");

  defsubr (&Sfep_force_on);
  defsubr (&Sfep_force_off);
  defsubr (&Sfep_get_mode);
  defsubr (&Sw32_ime_undetermined_string_length);

  defsubr (&Sw32_set_ime_mode);

  defsubr (&Sw32_ime_register_word_dialog);

#ifdef ENABLE_IMM_CONTEXT
  defsubr (&Sw32_ime_create_context);
  defsubr (&Sw32_ime_destroy_context);
  defsubr (&Sw32_ime_associate_context);
#endif

  defsubr (&Sw32_ime_create_conversion_agent);
  defsubr (&Sw32_ime_destroy_conversion_agent);
  defsubr (&Sw32_ime_set_composition_string);
  defsubr (&Sw32_ime_get_composition_string);
  defsubr (&Sw32_ime_get_candidate_list);
  defsubr (&Sw32_ime_select_candidate);
  defsubr (&Sw32_ime_change_candidate);
  defsubr (&Sw32_ime_open_candidate);
  defsubr (&Sw32_ime_close_candidate);
  defsubr (&Sw32_ime_deal_with_context);
  /*  defsubr (&Sw32_ime_get_conversion_list); */

#endif /* IME_CONTROL */
}
