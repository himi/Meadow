/* MW32 version3 IME specific Lisp staff.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.

This file is part of Mule (MULtilingual Enhancement of GNU Emacs).

Mule is free software distributed in the form of patches to GNU Emacs.
You can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

Mule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* 93.6.1 written by M.Higashida <manabu@sigmath.osaka-u.ac.jp> */
/* 95.7.23 modified by himi for mule 2.2.2 console window */
/* 96.5.3 modified by himi for Mule2.3 Window Version 1.00 */
/* 96.10.1 modified by himi for Version4 W32 */
/* 97.10.13 spare mw32ime.c old version for WindowsNT 3.5/3.1 */

#include "config.h"

#ifdef NULL
#undef NULL
#endif
#include "lisp.h"

#ifdef HAVE_NTGUI
#include "frame.h"
#include "mw32term.h"
#include "coding.h"
#else
#include <windows.h>
#endif

#ifdef IME_CONTROL
/* #include <winnls32.h> */

#include <dde.h>
#endif /* IME_CONTROL */

#ifndef HAVE_NTGUI
extern HWND hwndConsole;
#endif

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

typedef struct _IMESTRUCT {
    UINT     fnc;
    WPARAM   wParam;
    UINT     wCount;
    UINT     dchSource;
    UINT     dchDest;
    LPARAM   lParam1;
    LPARAM   lParam2;
    LPARAM   lParam3;
} IMESTRUCT, *LPIMESTRUCT;

#if 0
#define IMC_SETCOMPOSITIONWINDOW        0x000C
typedef struct tagCOMPOSITIONFORM {
    DWORD dwStyle;
    POINT ptCurrentPos;
    RECT  rcArea;
} COMPOSITIONFORM;
#define CFS_FORCE_POSITION           0x20
#define CFS_POINT                    0x02
#endif

BOOL fIME = FALSE;
typedef LRESULT (WINAPI *SENDIMEMESSAGEEXPROC)(HWND, LPARAM);
SENDIMEMESSAGEEXPROC SendIMEMessageExProc;
typedef HWND (WINAPI *IMMGETDEFAULTIMEWNDPROC)(HWND);
IMMGETDEFAULTIMEWNDPROC ImmGetDefaultIMEWndProc;
typedef LONG (WINAPI *IMMGETCOMPOSITIONSTRINGPROC)
     (HIMC, DWORD, LPVOID, DWORD);
IMMGETCOMPOSITIONSTRINGPROC ImmGetCompositionStringProc;
typedef HIMC (WINAPI *IMMGETCONTEXTPROC)(HWND);
IMMGETCONTEXTPROC ImmGetContextProc;
typedef BOOL (WINAPI *IMMGETCONVERSIONSTATUSPROC)(HIMC, LPDWORD, LPDWORD);
IMMGETCONVERSIONSTATUSPROC ImmGetConversionStatusProc;
typedef BOOL (WINAPI *IMMSETCONVERSIONSTATUSPROC)(HIMC, DWORD, DWORD);
IMMSETCONVERSIONSTATUSPROC ImmSetConversionStatusProc;
typedef BOOL (WINAPI *IMMCONFIGUREIMEPROC)(HKL, HWND, DWORD, LPVOID);
IMMCONFIGUREIMEPROC ImmConfigureIMEProc;
typedef BOOL (WINAPI *IMMRELEASECONTEXTPROC)(HWND, HIMC);
IMMRELEASECONTEXTPROC ImmReleaseContextProc;

HANDLE hIME;
extern Lisp_Object Vime_control;

static WPARAM wIMEOpen;

Lisp_Object Vime_control;
Lisp_Object VIME_command_off_flag;
int IME_event_off_count;

#endif /* IME_CONTROL */


#ifdef IME_CONTROL

DEFUN ("fep-force-on", Ffep_force_on, Sfep_force_on, 0, 1, 0, "")
  (eventp)
     Lisp_Object eventp;
{
  if (fIME && !NILP (Vime_control))
    {
      LPIMESTRUCT lpIme;

      if (!NILP(Ffep_get_mode())) return Qnil;
      lpIme = (LPIMESTRUCT) GlobalLock (hIME);
      lpIme->fnc = IME_SETOPEN;
      lpIme->wParam = wIMEOpen = 1;
      GlobalUnlock (hIME);
#ifdef HAVE_NTGUI
      if (NILP(eventp))
	IME_event_off_count++;
      (SendIMEMessageExProc) (FRAME_MW32_WINDOW(selected_frame),
			      (LPARAM) hIME);
#else
      (SendIMEMessageExProc) (hwndConsole, (LPARAM) hIME);
#endif
    }
  return Qnil;
}


DEFUN ("fep-force-off", Ffep_force_off, Sfep_force_off, 0, 1, 0, "")
  (eventp)
     Lisp_Object eventp;
{
  if (fIME && !NILP (Vime_control))
    {
      LPIMESTRUCT lpIme;

      if (NILP(Ffep_get_mode())) return Qnil;

      lpIme = (LPIMESTRUCT) GlobalLock (hIME);
      lpIme->fnc = IME_SETOPEN;
      lpIme->wParam = wIMEOpen = 0;
      GlobalUnlock (hIME);
#ifdef HAVE_NTGUI
      if (NILP(eventp))
	IME_event_off_count++;
      (SendIMEMessageExProc) (FRAME_MW32_WINDOW(selected_frame),
			      (LPARAM) hIME);
#else
      (SendIMEMessageExProc) (hwndConsole, (LPARAM) hIME);
#endif
    }
  return Qnil;
}


DEFUN ("fep-get-mode", Ffep_get_mode, Sfep_get_mode, 0, 0, "", "")
  ()
{
  if (fIME && !NILP (Vime_control))
    {
      LPIMESTRUCT lpIme = (LPIMESTRUCT) GlobalLock (hIME);
      lpIme->fnc = IME_GETOPEN;
      GlobalUnlock (hIME);
#ifdef HAVE_NTGUI
      return (SendIMEMessageExProc) (FRAME_MW32_WINDOW(selected_frame),
			      (LPARAM) hIME) ? Qt : Qnil;
#else
      return (SendIMEMessageExProc) (hwndConsole, (LPARAM) hIME) ? Qt : Qnil;
#endif
    }
  else
    return Qnil;
}

DEFUN ("does-ime-have-undetermined-strings",
       Fdoes_ime_have_undetermined_strings,
       Sdoes_ime_have_undetermined_strings, 0, 0, "", "")
     ()
{
  if (fIME && !NILP (Vime_control))
    {
      Lisp_Object result;
      LPIMESTRUCT lpIme = (LPIMESTRUCT) GlobalLock (hIME);
      lpIme->fnc = IME_GETOPEN;
#ifdef HAVE_NTGUI
      if (((SendIMEMessageExProc) (FRAME_MW32_WINDOW(selected_frame),
				    (LPARAM) hIME)) &&
	  (lpIme -> wCount != 0))
#else
      if (((SendIMEMessageExProc) (hwndConsole, (LPARAM) hIME)) &&
	   (lpIme -> wCount != 0))
#endif
	result = Qt;
      else result = Qnil;

      /*      message ("unresolved string byte: %d", lpIme -> wCount); */
	  
      GlobalUnlock (hIME);
      return result;
    }
  else
    return Qnil;
}

#ifdef HAVE_NTGUI
void mw32_set_ime_font(HWND hwnd, LPLOGFONT psetlf)
{
  
  if (fIME && !NILP (Vime_control))
    {
      Lisp_Object result;
      LPIMESTRUCT lpIme = (LPIMESTRUCT) GlobalLock (hIME);
      HANDLE hplf;
      LPLOGFONT plf;
      
      lpIme->fnc = IME_SETCONVERSIONFONTEX;
      hplf = GlobalAlloc (GMEM_SHARE | GMEM_MOVEABLE, sizeof(LOGFONT));
      plf = (LPLOGFONT) GlobalLock(hplf);

      *plf = *psetlf;
      lpIme -> lParam1 = (LPARAM)hplf;

      ((SendIMEMessageExProc) (hwnd, (LPARAM) hIME));

      GlobalUnlock (hplf);
      GlobalUnlock (hIME);
      GlobalFree (hplf);
    }
}
#endif  

void mw32_set_ime_conv_window(struct frame *f)
{
  if (fIME && !NILP (Vime_control))
    {
      if (!ImmGetDefaultIMEWndProc){
	LPIMESTRUCT lpIme = (LPIMESTRUCT) GlobalLock (hIME);

	lpIme->fnc = IME_SETCONVERSIONWINDOW;
	lpIme->wParam = MCW_WINDOW;
	lpIme->lParam1 = 
	  MAKELRESULT(CHAR_TO_PIXEL_COL(f, FRAME_CURSOR_X(f)),
		      CHAR_TO_PIXEL_ROW(f, FRAME_CURSOR_Y(f)));
	GlobalUnlock (hIME);
	(SendIMEMessageExProc) (FRAME_MW32_WINDOW(f), (LPARAM) hIME);
      }else{
	HWND IMEhwnd;
	COMPOSITIONFORM compform;

	IMEhwnd = (ImmGetDefaultIMEWndProc)(FRAME_MW32_WINDOW(f));

	compform.dwStyle = CFS_FORCE_POSITION; /* CFS_POINT; */
	compform.ptCurrentPos.x = CHAR_TO_PIXEL_COL(f, FRAME_CURSOR_X(f));
	compform.ptCurrentPos.y = CHAR_TO_PIXEL_ROW(f, FRAME_CURSOR_Y(f));
  
	SendMessage(IMEhwnd, WM_IME_CONTROL, (WPARAM)IMC_SETCOMPOSITIONWINDOW,
		    (LPARAM)(&compform));
	return;
      }
    }
}
	  
void mw32_ime_control_init (void)
{
  HMODULE hImm32;
  HMODULE hUser32;
  hUser32 = GetModuleHandle ("USER32.DLL");
  if (!hUser32) hUser32 = LoadLibrary("USER32.DLL");
  hImm32 = GetModuleHandle ("IMM32.DLL");
  if (!hImm32) hImm32 = LoadLibrary("IMM32.DLL");

  fIME = FALSE;
  Vime_control = Qnil;
  IME_event_off_count = 0;
  if (hUser32)
    {
      SendIMEMessageExProc =
	(SENDIMEMESSAGEEXPROC)
	  GetProcAddress (hUser32, "SendIMEMessageExA");
      if (hImm32)
	{
	  ImmGetDefaultIMEWndProc =
	    (IMMGETDEFAULTIMEWNDPROC)
	      GetProcAddress (hImm32,
			      "ImmGetDefaultIMEWnd");
	  ImmGetCompositionStringProc =
	    (IMMGETCOMPOSITIONSTRINGPROC)
	      GetProcAddress (hImm32, "ImmGetCompositionStringA");
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
	  ImmConfigureIMEProc =
	    (IMMCONFIGUREIMEPROC) 
	      GetProcAddress (hImm32,
			      "ImmConfigureIMEA");
	  ImmReleaseContextProc =
	    (IMMRELEASECONTEXTPROC)
	      GetProcAddress (hImm32,
			      "ImmReleaseContext");
	}
      if (SendIMEMessageExProc)
	{
	  fIME = TRUE;
	  Vime_control = Qt;
	  hIME = GlobalAlloc (GMEM_SHARE | GMEM_MOVEABLE, sizeof(IMESTRUCT));
	}
    }
}

#endif /* IME_CONTROL */


syms_of_mw32ime()
{
#ifdef IME_CONTROL
  DEFVAR_LISP ("ime-control", &Vime_control, "IME control");

  defsubr (&Sfep_force_on);
  defsubr (&Sfep_force_off);
  defsubr (&Sfep_get_mode);
  defsubr (&Sdoes_ime_have_undetermined_strings);
#endif /* IME_CONTROL */
}
