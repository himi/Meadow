/* Dynamic Link routines header file on the Microsoft W32 system.
   Copyright (C) Miyashita Hisashi.

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

   Miyashita Hisashi (himi@bird.scphys.kyoto-u.ac.jp)
*/

/* 
   This file must be included to all the dynamic link library,
   and THIS FILE MUST BE INCLUDED AFTER macro definitions and `lisp.h'.
 */

/* 
   Users make dynamic link libiraries as follows.
   -----------------------------------------   
   #include <lisp.h>
   
   #define SYMS_OF_INITIALIZE <function_name>
   #define SYMS_OF_UNINITIALIZE <function_name>

   #include <mw32dl.h>
   -----------------------------------------   
*/

#ifndef MAX_DL
#define MAX_DL 256
#endif

#ifdef NO_DLL

#define UNDEFVAR_LISP(lname)
#define UNDEFVAR_LISP_NOPRO(lname)
#define UNDEFVAR_BOOL(lname)
#define UNDEFVAR_INT(lname)

#else /* not NO_DLL */
/* 
   DEFVAR_??? memorize the raw address of a variable.
   So we need not modify them.  
   But we should set document string dynamically. Redefine them.
*/

#define DEFVAR_LISP(lname, vname, doc) \
  defvar_lisp (lname, vname);          \
  set_variable_documentation (lname, doc)
#define DEFVAR_LISP_NOPRO(lname, vname, doc) \
  defvar_lisp_nopro (lname, vname, doc);     \
  set_variable_documentation (lname, doc)
#define DEFVAR_BOOL(lname, vname, doc) \
  defvar_bool (lname, vname);          \
  set_variable_documentation (lname, doc)
#define DEFVAR_INT(lname, vname, doc)  \
  defvar_int (lname, vname);           \
  set_variable_documentation (lname, doc)

#if 0 /* We can't support buffer local variables.  */
#define DEFVAR_PER_BUFFER(lname, vname, type, doc)  \
 defvar_per_buffer (lname, vname, type, 0)
#define DEFVAR_KBOARD(lname, vname, doc) \
     defvar_kboard (lname, \
		    (int)((char *)(&current_kboard->vname) \
			  - (char *)current_kboard))
#endif

/* UNDEFVAR_??? are needed by dynamic link libraries.
   DLLs must free their storage before they are released by emacs.  */

#define UNDEFVAR_LISP(lname) \
  undefvar (lname);          \
  free_variable_documentation (lname)
#define UNDEFVAR_LISP_NOPRO(lname) \
  undefvar (lname);                \
  free_variable_documentation (lname)
#define UNDEFVAR_BOOL(lname) \
  undefvar (lname);          \
  free_variable_documentation (lname)
#define UNDEFVAR_INT(lname) \
  undefvar (lname);         \
  free_variable_documentation (lname)

#if 0 /* We can't support buffer local variables.  */
#define UNDEFVAR_PER_BUFFER(lname) undefvar_per_buffer (lname)
#define UNDEFVAR_KBOARD(lname) undefvar_kboard (lname)
#endif

#ifndef NO_DLLENTRYPOINT_AUTOMATICALLY
BOOL DllEntryPoint (HINSTANCE hinst, DWORD dwreason, LPVOID reserved)
{
  if (dwreason == DLL_PROCESS_ATACH)
    {
      SYMS_OF_INITIALIZE;
    }
  else if (dwreason == DLL_PROCESS_DETACH)
    {
      SYMS_OF_UNINTIALIZE;
    }

  return TRUE;
}
#endif

#endif  /* not NO_DLL */

