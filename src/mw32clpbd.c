/* MW32 clipboard module
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of Mule (MULtilingual Enhancement of GNU Emacs) 
Enhancement Advantage over Windows----Meadow.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* 95.5.2 created by himi */

#include "config.h"

#include <windows.h>
#include "lisp.h"
#include "charset.h"
#include "coding.h"
#include "buffer.h"
#include "blockinput.h"

#define WRITE_BUF_SIZE 1024

/* As to these definitions, we put no special meanings to size and
   textlen.  You must take consideration to null terminator or anything
   else by yourself. */
#define MW32_MULE_CB_REQSIZE(textlen) ((textlen) + sizeof(int))
#define MW32_MULE_CB_SIZE(ptr) (*((int*)(ptr)))
#define MW32_MULE_CB_CONTENTS(ptr) ((ptr) + sizeof(int))
#define MW32_MULE_SET_CB(ptr, text, size)	\
do {						\
  memcpy (((ptr) + sizeof(int)), text, size);	\
  (*((int*)(ptr))) = size;			\
}while(0)

Lisp_Object Vw32_clipboard_coding_system;
int mw32_mule_clipboard_format;

/* The last text we put into the clipboard.  This is used when the OS
   does not support sequence numbers (NT4, 95). It is undesirable to
   use data put on the clipboard by Emacs because the clipboard data
   could be MULEtilated by inappropriately chosen
   (next-)selection-coding-system.  For this reason, we must store the
   text *after* it was encoded/Unix-to-DOS-converted.  */
static unsigned char *last_clipboard_text = NULL;
static size_t clipboard_storage_size = 0;

/* From w32select.c(GNU Emacs) */

DEFUN ("w32-set-clipboard-data", Fw32_set_clipboard_data,
       Sw32_set_clipboard_data, 1, 2, 0,
       "This sets the clipboard data to the given text.\n\
Optional argument CODE specifies the coding-system object\n\
used in the clipboard, which defaults to w32-clipboard-coding-system.")
     (string, code)
     Lisp_Object string, code;
{
  BOOL ok = TRUE;
  HANDLE htext, hmuletext;
  struct coding_system coding;
  int htextsize, size;
  unsigned char *lptext, *lpmulecb;
  

  CHECK_STRING (string, 0);

  if (NILP (code))
    code = Vw32_clipboard_coding_system;
  setup_coding_system (Fcheck_coding_system (code), &coding);
  if (!coding.cmp_data)
    coding_allocate_composition_data (&coding, 0);  

  BLOCK_INPUT;

  htextsize = decoding_buffer_size(&coding, STRING_BYTES (XSTRING (string)) + 1);
  if ((htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE,
			    htextsize)) == NULL)
    goto error;
  if (mw32_mule_clipboard_format)
    {
      if ((hmuletext =
	   GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE,
			MW32_MULE_CB_REQSIZE (STRING_BYTES (XSTRING (string))
					      + 1)))
	  == NULL)
	goto error;
      if ((lpmulecb = (unsigned char *) GlobalLock (hmuletext)) == NULL)
	goto error;

      MW32_MULE_SET_CB (lpmulecb, XSTRING (string)->data,
			STRING_BYTES(XSTRING (string)) + 1);
      GlobalUnlock (hmuletext);
    }
  if ((lptext = (unsigned char *) GlobalLock (htext)) == NULL)
    goto error;
	    
  encode_coding (&coding, XSTRING (string)->data, lptext,
		 STRING_BYTES(XSTRING (string)), htextsize);
  size = coding.produced;
  lptext[size] = '\0';

  /* Stash away the data we are about to put into the
     clipboard, so we could later check inside
     Fw32_get_clipboard_data whether the clipboard still
     holds our data.  */
  if (clipboard_storage_size < size)
    {
      clipboard_storage_size = size + 100;
      last_clipboard_text = (char *) xrealloc (last_clipboard_text,
					       clipboard_storage_size);
    }
  if (last_clipboard_text)
    memcpy (last_clipboard_text, lptext, size);

  GlobalUnlock (htext);

  if (!OpenClipboard (NULL))
    goto error;

  ok = EmptyClipboard () && SetClipboardData (CF_TEXT, htext);

  if (mw32_mule_clipboard_format)
    ok = ok && SetClipboardData (mw32_mule_clipboard_format, hmuletext);

  CloseClipboard ();

  if (ok)
    goto done;

 error:
  ok = FALSE;
  if (htext)
    GlobalFree (htext);
  if (last_clipboard_text)
    *last_clipboard_text = '\0';

 done:
  UNBLOCK_INPUT;

  return (ok ? string : Qnil);
}


DEFUN ("w32-get-clipboard-data", Fw32_get_clipboard_data,
       Sw32_get_clipboard_data, 0, 1, 0,
       "This gets the clipboard data in text format.\n\
Optional argument CODE specifies the coding-system object\n\
used in the clipboard, which defaults to w32-clipboard-coding-system.")
     (code)
     Lisp_Object code;
{
  HANDLE htext, hmuletext;
  Lisp_Object ret = Qnil;
  struct coding_system coding;
  unsigned char *lptext, *buf;
  int nbytes, size, bufsize;

  BLOCK_INPUT;

  if (!OpenClipboard (NULL))
    goto done;
  
  if (NILP (code) && mw32_mule_clipboard_format &&
      (hmuletext = GetClipboardData (mw32_mule_clipboard_format)))
    {
      if ((lptext = (unsigned char *) GlobalLock (hmuletext)) == NULL)
	goto closeclip;
      /* Subtract 1 for the NULL terminator.  */
      ret = make_string (MW32_MULE_CB_CONTENTS (lptext),
			 MW32_MULE_CB_SIZE (lptext) - 1);
      GlobalUnlock (hmuletext);
      goto closeclip;
    }

  if ((htext = GetClipboardData (CF_TEXT)) == NULL)
    goto closeclip;
	    
  if ((lptext = (unsigned char *) GlobalLock (htext)) == NULL)
    goto closeclip;
	    
  nbytes = strlen (lptext);

  /* If the text in clipboard is identical to what we put there
     last time w32_set_clipboard_data was called, pretend there's no
     data in the clipboard.  This is so we don't pass our own text
     from the clipboard (which might be troublesome if the killed
     text includes null characters).  */
  if (last_clipboard_text
      && clipboard_storage_size >= nbytes
      && memcmp (last_clipboard_text, lptext, nbytes) == 0)
    goto closeclip;

  if (NILP (code))
    code = Vw32_clipboard_coding_system;
  setup_coding_system (Fcheck_coding_system (code), &coding);

  bufsize = encoding_buffer_size (&coding, nbytes);
  buf = (unsigned char*) xmalloc (bufsize);
  decode_coding (&coding, lptext, buf, nbytes, bufsize);
  size = coding.produced;
  ret = make_string (buf, size);
  xfree (buf);

  GlobalUnlock (htext);

 closeclip:
  CloseClipboard ();

 done:
  UNBLOCK_INPUT;

  return ret;
}

DEFUN ("w32-clipboard-data-exist-p", Fw32_clipboard_data_exist_p,
       Sw32_clipboard_data_exist_p, 0, 1, 0,
       "This inspects whether clipboard have any data.\n\
t means it has something. nil means it has nothing. Optional argument DUMMY\n\
is not used. This is to make compatiblity with x-selection-exists-p.")
     (dummy)
     Lisp_Object dummy;
{
  return IsClipboardFormatAvailable(CF_TEXT)? Qt:Qnil;
}


void
syms_of_mw32clipboard ()
{
  DEFVAR_LISP ("w32-clipboard-coding-system",
	       &Vw32_clipboard_coding_system,
	       "coding system in clipboard");
  Vw32_clipboard_coding_system = Qnil;
  defsubr (&Sw32_set_clipboard_data);
  defsubr (&Sw32_get_clipboard_data);
  defsubr (&Sw32_clipboard_data_exist_p);
}
