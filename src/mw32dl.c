/* Dynamic Link routines on the Microsoft W32 system.

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

   MIYASHITA Hisashi (himi@meadowy.org)
*/

#include "config.h"

#include <stdlib.h>
#include <stdio.h>

#include "w32heap.h"
#include "lisp.h"

#define NO_DLL
#include "mw32dl.h"

extern Lisp_Object Qvariable_documentation;

static HANDLE handle_table[MAX_DL];
static int current_dls;


/* doc string is never changed until the dynamic link library is free.  */

int
set_function_documentation (name, doc)
     char *name, *doc;
{
  Lisp_Object sym, fun;

  if (!doc) return 0;

  sym = Fintern_soft (build_string(name), Qnil);
  if (NILP (sym)) return 0;
  fun = Findirect_function (sym);
  if (!SUBRP (fun)) return 0;

  XSUBR(fun)->doc = name;

  return 1;
}

int
set_variable_documentation (name, doc)
     char *name, *doc;
{
  Lisp_Object sym;

  if (!doc) return 0;

  sym = Fintern_soft (build_string(name), Qnil);
  if (NILP (sym)) return 0;

  Fput (sym, Qvariable_documentation, build_string (doc));

  return 1;
}

int
free_function_documentation (name)
     char *name;
{
  Lisp_Object sym, fun;

  sym = Fintern_soft (build_string(name), Qnil);
  if (NILP (sym)) return 0;
  fun = Findirect_function (sym);
  if (!SUBRP (fun)) return 0;

  XSUBR(fun)->doc = NULL;

  return 1;
}
  
int
free_variable_documentation (name)
     char *name;
{
  Lisp_Object sym;

  sym = Fintern_soft (build_string(name), Qnil);
  if (NILP (sym)) return 0;

  Fput (sym, Qvariable_documentation, Qnil);

  return 1;
}

void
undefsubr (sname)
     struct Lisp_Subr *sname;
{
  Lisp_Object sym;

  sym = intern (sname->symbol_name);
  XSETSUBR (XSYMBOL (sym)->function, Qunbound);
  free_function_documentation (sname);
}

void
undefvar (name)
     char *name;
{
  Lisp_Object sym;

  sym = intern (name);
  XSYMBOL (sym)->value = Qunbound;
  free_variable_documentation (name);
}

void
undefvar_per_buffer (name)
     char *name;
{
  Lisp_Object sym;

  sym = intern (name);
  XSYMBOL (sym)->function = Qunbound;
}


DEFUN ("dynamic-link", Fdynamic_link, Sdynamic_link, 1, 1, 0,
       "Link a dynamic link library named FILENAME.")
     (filename)
     Lisp_Object filename;
{
  int i;
  HANDLE hlib;
  Lisp_Object h;

  CHECK_STRING (filename, 0);

  hlib = LoadLibrary (XSTRING (filename)->data);

  current_dls++;

  if (!hlib)
    error ("fail to load a dynamic link library:%s.");

  for (i = 0;i < MAX_DL;i++)
    if (handle_table[i] == NULL)
      {
	handle_table[i] = hlib;
	break;
      }

  XSETFASTINT (h, i);
  return h;
}

DEFUN ("dynamic-unlink", Fdynamic_unlink, Sdynamic_unlink, 1, 1, 0,
       "Dynamic unlink the code.")
  (h)
     Lisp_Object h;
{
  HANDLE hlib;
  int i;

  CHECK_NUMBER (h, 0);
  i = XFASTINT (h);
  hlib = handle_table[i];

  if (!FreeLibrary (hlib))
    error ("Fail to unload a dynamic link library:%d", XFASTINT (h));

  current_dls--;
  handle_table[i] = NULL;
  return Qnil;
}

syms_of_mw32dl ()
{
  defsubr (&Sdynamic_link);
  defsubr (&Sdynamic_unlink);
  /*  defsubr (&Sget_dll_functioons);
  defsubr (&Sdll_function_call); */
}
