/* MW32 Functions for Windows.
   Copyright (C) 1989, 92, 93, 94, 95, 96, 1997, 1998, 1999, 2000, 2001
     Free Software Foundation.

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

/* MW32 implementation by MIYASHITA Hisashi <himi@meadowy.org> */

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>
#include <windows.h>
#include <shellapi.h>
#include <imm.h>
#include <commdlg.h>

#include "lisp.h"
#include "mw32term.h"
#include "mw32reg.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "intervals.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include "mw32sync.h"
#include <epaths.h>
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"
#include "atimer.h"

/* external interfaces */
void mw32_set_font P_ ((struct frame *, Lisp_Object, Lisp_Object));
#ifdef IME_CONTROL
void mw32_set_frame_ime_font P_ ((struct frame *, Lisp_Object, Lisp_Object));
#endif
/* frame.c */
void x_set_frame_parameters P_ ((FRAME_PTR f, Lisp_Object alist));
void x_report_frame_params P_ ((FRAME_PTR f, Lisp_Object *alistptr));
Lisp_Object x_get_focus_frame P_ ((struct frame *frame));
void x_sync P_ ((FRAME_PTR f));
int x_pixel_width P_ ((struct frame *f));
int x_pixel_height P_ ((struct frame *f));
int x_char_width P_ ((struct frame *f));
int x_char_height P_ ((struct frame *f));
int x_screen_planes P_ ((struct frame *f));
void x_sync P_ ((struct frame *f));

/* xfaces.c */
int x_defined_color P_ ((FRAME_PTR f, char *color_name, XColor *color_def, int alloc_p));
int x_bitmap_height P_ ((FRAME_PTR f, int id));
int x_bitmap_width P_ ((FRAME_PTR f, int id));
int x_create_bitmap_from_data P_ ((FRAME_PTR f, char *bits,
				   unsigned int width, unsigned int height));
int x_create_bitmap_from_file P_ ((FRAME_PTR f, Lisp_Object file));
void x_destroy_bitmap P_ ((FRAME_PTR f, int id));

/* window.c */
void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_tool_bar_lines P_ ((FRAME_PTR f, Lisp_Object value, Lisp_Object oldval));

/* xdisp.c (note that the name is replaced by "define". */
void mw32_implicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));

/* mw32term.c */
struct image_cache *make_image_cache ();
void mw32_reference_bitmap P_ ((FRAME_PTR f, int id));
enum text_cursor_kinds mw32_specified_cursor_type P_ ((Lisp_Object arg, int *width));

/* mw32menu.c */
extern void free_frame_menubar (FRAME_PTR);
extern void mw32_menu_display_help P_ ((HWND, HMENU, UINT, UINT));
extern void mw32_free_menu_strings P_ ((HWND));

/* internal functions */
static Lisp_Object unwind_create_frame P_ ((Lisp_Object));
static Lisp_Object unwind_create_tip_frame P_ ((Lisp_Object));
static void mw32_change_window_heights P_ ((Lisp_Object, int));
static void mw32_disable_image P_ ((struct frame *, struct image *));
static void mw32_set_foreground_color P_ ((struct frame *,
					   Lisp_Object, Lisp_Object));
static void mw32_set_background_color P_ ((struct frame *,
					   Lisp_Object, Lisp_Object));
static void mw32_set_line_spacing P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void mw32_set_mouse_color P_ ((struct frame *,
				      Lisp_Object, Lisp_Object));
static void mw32_set_cursor_color P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void mw32_set_border_color P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void mw32_set_cursor_type P_ ((struct frame *,
				      Lisp_Object, Lisp_Object));
static void mw32_set_cursor_height P_ ((struct frame *,
					Lisp_Object, Lisp_Object));
static void mw32_set_icon_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void mw32_set_icon_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void mw32_set_border_width P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void mw32_set_internal_border_width P_ ((struct frame *, Lisp_Object,
						Lisp_Object));
static void mw32_explicitly_set_name P_ ((struct frame *,
					  Lisp_Object, Lisp_Object));
static void mw32_set_autoraise P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void mw32_set_autolower P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void mw32_set_vertical_scroll_bars P_ ((struct frame *, Lisp_Object,
					       Lisp_Object));
static void mw32_set_visibility P_ ((struct frame *,
				     Lisp_Object, Lisp_Object));
static void mw32_set_scroll_bar_width P_ ((struct frame *,
					   Lisp_Object, Lisp_Object));
static void mw32_set_title P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void mw32_set_unsplittable P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void mw32_set_scroll_bar_foreground P_ ((struct frame *, Lisp_Object,
						Lisp_Object));
static void mw32_set_scroll_bar_background P_ ((struct frame *, Lisp_Object,
						Lisp_Object));
static Lisp_Object mw32_default_scroll_bar_color_parameter P_ ((struct frame *,
								Lisp_Object,
								Lisp_Object,
								char *, char *,
								int));
static void mw32_edge_detection P_ ((struct frame *, struct image *,
				     Lisp_Object, Lisp_Object));
static void mw32_laplace P_ ((struct frame *, struct image *));
static void mw32_emboss P_ ((struct frame *, struct image *));
static void mw32_detect_edges P_ ((struct frame *, struct image *,
				   int[9], int));

static struct mw32_display_info *mw32_display_info_for_name P_ ((Lisp_Object name));

/* TODO: Should be moved to mw32color.c */
static void mw32_set_screen_gamma P_ ((struct frame *,
				       Lisp_Object, Lisp_Object));
static void init_color_table P_ ((void));
static void free_color_table P_ ((void));
static unsigned long *colors_in_color_table P_ ((int *n));
static unsigned long lookup_rgb_color P_ ((struct frame *f,
					   int r, int g, int b));
static unsigned long lookup_pixel_color P_ ((struct frame *f,
					     unsigned long p));

/* variable declarations */

extern HINSTANCE hinst;
extern HINSTANCE hprevinst;
extern LPSTR lpCmdLine;
extern int nCmdShow;

LRESULT CALLBACK mw32_WndProc P_((HWND hwnd, UINT msg,
				  WPARAM wParam, LPARAM lParam));

#define EMACS_RESOURCE_CLASS "Emacs"

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline. */
Lisp_Object Vmw32_pointer_shape;
Lisp_Object Vmw32_nontext_pointer_shape;
Lisp_Object Vmw32_mode_pointer_shape;

/* The shape when over mouse-sensitive text.  */
Lisp_Object Vmw32_sensitive_text_pointer_shape;

/* Color of chars displayed in cursor box. */
Lisp_Object Vmw32_cursor_fore_pixel;

/* The colormap for converting color names to RGB values */
Lisp_Object Vmw32_color_map;

/* W32 Psudo(?) terminal connection flag.  */
static int mw32_open = 0;

/* The name we're using in resource queries.  Most often "emacs".  */

Lisp_Object Vx_resource_name;

/* The application class we're using in resource queries.
   Normally "Emacs".  */

Lisp_Object Vx_resource_class;

/* Non-zero means we're allowed to display an hourglass cursor.  */

int display_hourglass_p;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */

Lisp_Object Vmw32_hourglass_pointer_shape;

/* If non-nil, the pointer shape to indicate that windows can be
   dragged horizontally.  */

Lisp_Object Vmw32_window_horizontal_drag_shape;

/* Color of chars displayed in cursor box.  */

Lisp_Object Vmw32_cursor_fore_pixel;

/* Search path for bitmap files.  */

Lisp_Object Vmw32_bitmap_file_path;


Lisp_Object Qauto_raise;
Lisp_Object Qauto_lower;
Lisp_Object Qbar, Qcaret, Qcheckered_caret, Qhairline_caret;
Lisp_Object Qborder_color;
Lisp_Object Qborder_width;
Lisp_Object Qbox;
Lisp_Object Qcursor_color;
Lisp_Object Qcursor_type;
Lisp_Object Qcursor_height;
Lisp_Object Qgeometry;
Lisp_Object Qicon_left;
Lisp_Object Qicon_top;
Lisp_Object Qicon_type;
Lisp_Object Qicon_name;
Lisp_Object Qinternal_border_width;
Lisp_Object Qleft;
Lisp_Object Qright;
Lisp_Object Qmouse_color;
Lisp_Object Qnone;
Lisp_Object Qouter_window_id;
Lisp_Object Qparent_id;
Lisp_Object Qscroll_bar_width;
Lisp_Object Qsuppress_icon;
extern Lisp_Object Qtop;
Lisp_Object Qundefined_color;
Lisp_Object Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qwindow_id;
Lisp_Object Qx_frame_parameter;
Lisp_Object Qmw32_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Quser_position;
Lisp_Object Quser_size;
Lisp_Object Qime_font;
extern Lisp_Object Qdisplay;
Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
Lisp_Object Qscreen_gamma, Qline_spacing, Qcenter;
Lisp_Object Qcompound_text, Qcancel_timer;

/* The below are defined in frame.c.  */

extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;
extern Lisp_Object Qtool_bar_lines;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;

#if GLYPH_DEBUG
int image_cache_refcount, dpyinfo_refcount;
#endif

/* Error if we are not connected to X.  */

void
check_mw32 (void)
{
  if (! mw32_open)
    error ("Windows have not been initialized yet.");
}

/* Nonzero if we can use mouse menus.
   You should not call this unless HAVE_MENUS is defined.  */

int
have_menus_p (void)
{
  return mw32_open;
}

/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for X.  */

FRAME_PTR
check_mw32_frame (Lisp_Object frame)
{
  FRAME_PTR f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame, 0);
  f = XFRAME (frame);
  if (! FRAME_MW32_P (f))
    error ("Non-mw32 frame used");
  return f;
}

/* Let the user specify an X display with a frame.
   nil stands for the selected frame--or, if that is not an X frame,
   the first X display on the list.  */

static struct mw32_display_info *
check_mw32_display_info (Lisp_Object frame)
{
  struct mw32_display_info *dpyinfo = NULL;
  
  if (NILP (frame))
    {
      struct frame *sf = XFRAME (selected_frame);
      
      if (FRAME_MW32_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_MW32_DISPLAY_INFO (sf);
      else if (mw32_display_list != 0)
	dpyinfo = mw32_display_list;
      else
	error ("MW32 is not in use or not initialized");
    }
  else if (STRINGP (frame))
    dpyinfo = mw32_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
      if (! FRAME_MW32_P (f))
	error ("Non-X frame used");
      dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
    }

  return dpyinfo;
}


/* Return the Emacs frame-object corresponding to an X window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use GC_xxx type test macros.  */

struct frame *
mw32_window_to_frame (struct mw32_display_info *dpyinfo,
		      HWND hwnd)
{
  Lisp_Object tail, frame;
  struct frame *f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_MW32_P (f) || FRAME_MW32_DISPLAY_INFO (f) != dpyinfo)
	continue;
#if 0
      if ((f->output_data.x->edit_widget 
	   && XtWindow (f->output_data.x->edit_widget) == wdesc)
	  /* A tooltip frame?  */
	  || (!f->output_data.x->edit_widget
	      && FRAME_X_WINDOW (f) == wdesc)
          || f->output_data.x->icon_desc == wdesc)
        return f;
#endif
      if (FRAME_MW32_WINDOW (f) == hwnd)
        return f;
    }
  return 0;
}

/* Like x_window_to_frame but also compares the window with the widget's
   windows.  */

struct frame *
mw32_any_window_to_frame (struct mw32_display_info *dpyinfo,
			  HWND hwnd)
{
  do {
    if (hwnd == dpyinfo->root_window)
      return NULL;
    if ((WNDPROC) GetClassLong (hwnd, GCL_WNDPROC) == mw32_WndProc)
      return mw32_window_to_frame (dpyinfo, hwnd);
  } while (hwnd = GetParent (hwnd));

  return NULL;
}


/***********************************************************************
	    BITMAP handling (should be totally rewritten!)
 ***********************************************************************/

/* Code to deal with bitmaps.  Bitmaps are referenced by their bitmap
   id, which is just an int that this section returns.  Bitmaps are
   reference counted so they can be shared among frames.

   Bitmap indices are guaranteed to be > 0, so a negative number can
   be used to indicate no bitmap.

   If you use x_create_bitmap_from_data, then you must keep track of
   the bitmaps yourself.  That is, creating a bitmap from the same
   data more than once will not be caught.  */


/* Functions to access the contents of a bitmap, given an id.  */

int
x_bitmap_height (FRAME_PTR f, int id)
{
  return FRAME_MW32_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

int
x_bitmap_width (FRAME_PTR f, int id)
{
  return FRAME_MW32_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

#if 0
int
x_bitmap_pixmap (FRAME_PTR f, int id)
{
  return FRAME_MW32_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}
#endif

/* Allocate a new bitmap record.  Returns index of new record.  */

static int
mw32_allocate_bitmap_record (FRAME_PTR f)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->bitmaps == NULL)
    {
      dpyinfo->bitmaps_size = 10;
      dpyinfo->bitmaps
	= (struct mw32_bitmap_record *) xmalloc (dpyinfo->bitmaps_size
						 * sizeof (struct mw32_bitmap_record));
      dpyinfo->bitmaps_last = 1;
      return 1;
    }

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps_size *= 2;
  dpyinfo->bitmaps
    = (struct mw32_bitmap_record *) xrealloc (dpyinfo->bitmaps,
					      dpyinfo->bitmaps_size
					      * sizeof (struct mw32_bitmap_record));
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
mw32_reference_bitmap (FRAME_PTR f, int id)
{
  ++FRAME_MW32_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */
/* TODO:!!!!  Note that this function is used only for face stipple.*/

int
x_create_bitmap_from_data (FRAME_PTR f, char *bits,
			   unsigned int width, unsigned int height)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int id;
  HBITMAP hbmp;

#if 0
  bitmap = XCreateBitmapFromData (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				  bits, width, height);
#endif
  hbmp = 0;

  if (! hbmp)
    return -1;

  id = mw32_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].hbmp = hbmp;
  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;

  return id;
}

/* Create bitmap from file FILE for frame F.  */
/* TODO:!!!!  Note that this function is used only for face stipple.*/

int
x_create_bitmap_from_file (FRAME_PTR f, Lisp_Object file)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  unsigned int width, height;
  HBITMAP hbmp;
  int xhot, yhot, result, id;
  Lisp_Object found;
  int fd;
  char *filename;

  /* Look for an existing bitmap with the same name.  */
  for (id = 0; id < dpyinfo->bitmaps_last; ++id)
    {
      if (dpyinfo->bitmaps[id].refcount
	  && dpyinfo->bitmaps[id].file
	  && !strcmp (dpyinfo->bitmaps[id].file, (char *) SDATA (file)))
	{
	  ++dpyinfo->bitmaps[id].refcount;
	  return id + 1;
	}
    }

  /* Search bitmap-file-path for the file, if appropriate.  */
  fd = openp (Vmw32_bitmap_file_path, file, "", &found, 0);
  if (fd < 0)
    return -1;
  emacs_close (fd);

  filename = (char *) SDATA (found);

#if 0
  result = XReadBitmapFile (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    filename, &width, &height, &bitmap, &xhot, &yhot);
  if (result != BitmapSuccess)
    return -1;
#endif
  hbmp = 0;
  height = width = 0;
  if (!hbmp)
    return -1;

  id = mw32_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].hbmp = hbmp;
  dpyinfo->bitmaps[id - 1].file = (char *) xmalloc (SBYTES (file) + 1);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  strcpy (dpyinfo->bitmaps[id - 1].file, SDATA (file));

  return id;
}

/* Remove reference to bitmap with id number ID.  */

void
x_destroy_bitmap (FRAME_PTR f, int id)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);

  if (id > 0)
    {
      --dpyinfo->bitmaps[id - 1].refcount;
      if (dpyinfo->bitmaps[id - 1].refcount == 0)
	{
	  BLOCK_INPUT;
#if 0
	  XFreePixmap (FRAME_X_DISPLAY (f), dpyinfo->bitmaps[id - 1].pixmap);
#endif
	  if (dpyinfo->bitmaps[id - 1].file)
	    {
	      xfree (dpyinfo->bitmaps[id - 1].file);
	      dpyinfo->bitmaps[id - 1].file = NULL;
	    }
	  UNBLOCK_INPUT;
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

static void
x_destroy_all_bitmaps (struct mw32_display_info *dpyinfo)
{
  int i;
  for (i = 0; i < dpyinfo->bitmaps_last; i++)
    if (dpyinfo->bitmaps[i].refcount > 0)
      {
#if 0
	XFreePixmap (dpyinfo->display, dpyinfo->bitmaps[i].pixmap);
#endif
	if (dpyinfo->bitmaps[i].file)
	  xfree (dpyinfo->bitmaps[i].file);
      }
  dpyinfo->bitmaps_last = 0;
}


/***********************************************************************
	    Color table (should be moved into mw32color.c later)
 ***********************************************************************/

/* The default colors for the color map */
typedef struct ColorMap_t {
    char * name ;
    COLORREF colorref ;
} ColorMap_t ;

ColorMap_t mw32_color_map[] = {
#include "mw32rgb.h"
} ;

DEFUN ("mw32-default-color-map", Fmw32_default_color_map,
       Smw32_default_color_map, 0, 0, 0, "Return the default color map.")
  ()
{
    int i;
    ColorMap_t * pc = mw32_color_map;
    Lisp_Object cmap;

    cmap = Qnil;

    for (i = 0; i < sizeof (mw32_color_map) / sizeof (mw32_color_map[0]); pc++,i++)
      cmap = Fcons (Fcons (build_string (pc->name),
			   make_number (pc->colorref)),
		    cmap);
    return (cmap);
}

Lisp_Object
mw32_to_x_color (Lisp_Object rgb)
{
  Lisp_Object color;

  CHECK_NUMBER (rgb, 0);
  color = Frassq (rgb, Vmw32_color_map);
  if (!NILP (color))
    return (XCAR (color));
  else
    return Qnil;
}

static int
get_hex1 (int numstr)
{
  if (isalpha (numstr))
    {
      numstr = toupper (numstr);
      if (!(numstr >= 'A' && numstr <= 'F'))
	return -1;
      numstr -= ('A' - 10);
      return numstr;
    }
  if (isdigit (numstr))
    {
      numstr -= '0';
      return numstr;
    }
  return -1;
}

static int
color_radix_change (TCHAR *colstr, int size)
{
  int i;
  int ret, tmp;

  ret = 0;
  if ((size <= 0) || (size > 4)) return -1;
  for (i = 0;i < size;i++)
    {
      ret <<= 4;
      tmp = get_hex1 (*colstr);
      if (tmp == -1) 
	return -1;
      ret += tmp;
      colstr++;
    }
  return ret;
}

static COLORREF
x_old_rgb_names (TCHAR *colstr, int len)
{
  int red, green, blue;

  switch (len)
    {
    case 4:			/* #RGB */
      red = color_radix_change (colstr + 1, 1) << 4;
      green = color_radix_change (colstr + 2, 1) << 4;
      blue = color_radix_change (colstr + 3, 1) << 4;
      break;

    case 7:			/* #RRGGBB */
      red = color_radix_change (colstr + 1, 2);
      green = color_radix_change (colstr + 3, 2);
      blue = color_radix_change (colstr + 5, 2);
      break;
    case 10:			/* #RRRGGGBBB */
      red = color_radix_change (colstr + 1, 3) >> 4;
      green = color_radix_change (colstr + 4, 3) >> 4;
      blue = color_radix_change (colstr + 7, 3) >> 4;
      break;
    case 13:			/* #RRRRGGGGBBBB */
      red = color_radix_change (colstr + 1, 4) >> 8;
      green = color_radix_change (colstr + 5, 4) >> 8;
      blue = color_radix_change (colstr + 9, 4) >> 8;
      break;

    default :
      return CLR_INVALID;
    }
  if ((red == -1) || (green == -1) || (blue == -1))
    return CLR_INVALID;

  return RGB (red, green, blue);
}

static COLORREF
x_rgb_names (TCHAR *colstr, int len)
{
  TCHAR *colstrend;
  Lisp_Object ret;
  int red, green, blue;
  int redchars, greenchars, bluechars;

  colstr += 4;
  len -= 4;
  colstrend = memchr (colstr, '/', len);
  if (!colstrend)
    return Qnil;
  redchars = (int) (colstrend - colstr);
  red = color_radix_change (colstr, redchars);
  if (red == -1) return CLR_INVALID;

  len -= (int) (colstrend - colstr + 1);
  colstr = colstrend + 1;
  colstrend = memchr (colstr, '/', len);
  if (!colstrend)
    return CLR_INVALID;
  greenchars = (int) (colstrend - colstr);
  green = color_radix_change (colstr, greenchars);
  if (green == -1) return CLR_INVALID;

  len -= (int) (colstrend - colstr + 1);
  colstr = colstrend + 1;
  bluechars = len;
  blue = color_radix_change (colstr, bluechars);
  if (blue == -1) return CLR_INVALID;

  /* 
     We rescale color values in 4bit * <number of chars>,
     then normalize them in 8bit.
  */
  if (redchars > 2)
    red >>= -(8 - redchars * 4);
  else if (redchars < 2)
    red <<= (8 - redchars * 4);
  if (greenchars > 2)
    green >>= -(8 - greenchars * 4);
  else if (greenchars < 2)
    green <<= (8 - greenchars * 4);
  if (bluechars > 2)
    blue >>= -(8 - bluechars * 4);
  else if (bluechars < 2)
    blue <<= (8 - bluechars * 4);

  return RGB (red, green, blue);
}

static COLORREF
x_to_mw32_color (TCHAR *colorname)
{
  COLORREF ret;
  TCHAR *colstr;
  int colstrlen;
  Lisp_Object tail;

  colstr = colorname;
  colstrlen = strlen (colstr);
  if (*colstr == '#')
    {
      return x_old_rgb_names (colstr, colstrlen);
    }
    
  if (colstrlen > 4) {
    if (memcmp (colstr, "rgb:", 4) == 0)
      {
	return x_rgb_names (colstr, colstrlen);
      }
#if 0
    if (memcmp (colstr, "rgbi:", 5) == 0)
      {
	return x_rgbi_names (arg, colstr, colstrlen);
      }
#endif
  }
    
  ret = CLR_INVALID;
  for (tail = Vmw32_color_map; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem;

      elt = XCAR (tail);
      if (!CONSP (elt)) continue;

      tem = CAR (elt);

      if (colstrlen == LISPY_STRING_BYTES (tem))
	if (memicmp (colstr, SDATA (tem), colstrlen) == 0)
	  {
	    ret = XINT (CDR (elt));
	    break;
	  }
    }

  return ret;
}


/***********************************************************************
	    		Frame parameters
 ***********************************************************************/

/* Connect the frame-parameter names for X frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   that is an index in this table.  */

struct mw32_frame_parm_table
{
  char *name;
  void (*setter) P_ ((struct frame *, Lisp_Object, Lisp_Object));
};

static struct mw32_frame_parm_table mw32_frame_parms[] =
{
  "auto-raise",			mw32_set_autoraise,
  "auto-lower",			mw32_set_autolower,
  "background-color",		mw32_set_background_color,
  "border-color",		mw32_set_border_color,
  "border-width",		mw32_set_border_width,
  "cursor-color",		mw32_set_cursor_color,
  "cursor-type",		mw32_set_cursor_type,
  "cursor-height",		mw32_set_cursor_height,
  "font",			mw32_set_font,
  "foreground-color",		mw32_set_foreground_color,
#ifdef IME_CONTROL
  "ime-font",			mw32_set_frame_ime_font,
#endif
  "internal-border-width",	mw32_set_internal_border_width,
  "menu-bar-lines",		x_set_menu_bar_lines,
  "mouse-color",		mw32_set_mouse_color,
  "name",			mw32_explicitly_set_name,
  "scroll-bar-width",		mw32_set_scroll_bar_width,
  "title",			mw32_set_title,
  "unsplittable",		mw32_set_unsplittable,
  "vertical-scroll-bars",	mw32_set_vertical_scroll_bars,
  "visibility",			mw32_set_visibility,
  "tool-bar-lines",		x_set_tool_bar_lines,
  "scroll-bar-foreground",	mw32_set_scroll_bar_foreground,
  "scroll-bar-background",	mw32_set_scroll_bar_background,
  "screen-gamma",		mw32_set_screen_gamma,
  "line-spacing",		mw32_set_line_spacing,
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to X.  */

static void
init_mw32_parm_symbols (void)
{
  int i;

  for (i = 0; i < sizeof (mw32_frame_parms) / sizeof (mw32_frame_parms[0]); i++)
    Fput (intern (mw32_frame_parms[i].name), Qmw32_frame_parameter,
	  make_number (i));
}

/* Change the parameters of frame F as specified by ALIST.
   If a parameter is not specially recognized, do nothing special;
   otherwise call the `x_set_...' function for that parameter.
   Except for certain geometry properties, always call store_frame_param
   to store the new value in the parameter alist.  */

void
x_set_frame_parameters (FRAME_PTR f, Lisp_Object alist)
{
  Lisp_Object tail;

  /* If both of these parameters are present, it's more efficient to
     set them both at once.  So we wait until we've looked at the
     entire list before we set them.  */
  int width, height;

  /* Same here.  */
  Lisp_Object left, top;

  /* Same with these.  */
  Lisp_Object icon_left, icon_top;

  /* Record in these vectors all the parms specified.  */
  Lisp_Object *parms;
  Lisp_Object *values;
  int i, p;
  int icon_left_no_change = 0, icon_top_no_change = 0;

  struct gcpro gcpro1, gcpro2;

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    i++;

  parms = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));
  values = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt;

      elt = Fcar (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }
  /* TAIL and ALIST are not used again below here.  */
  alist = tail = Qnil;

  GCPRO2 (*parms, *values);
  gcpro1.nvars = i;
  gcpro2.nvars = i;

  /* There is no need to gcpro LEFT, TOP, ICON_LEFT, or ICON_TOP,
     because their values appear in VALUES and strings are not valid.  */
  top = left = Qunbound;
  icon_left = icon_top = Qunbound;

  /* Provide default values for HEIGHT and WIDTH.  */
  if (FRAME_NEW_WIDTH (f))
    width = FRAME_NEW_WIDTH (f);
  else
    width = FRAME_WIDTH (f);

  if (FRAME_NEW_HEIGHT (f))
    height = FRAME_NEW_HEIGHT (f);
  else
    height = FRAME_HEIGHT (f);

  /* Process foreground_color and background_color before anything else.
     They are independent of other properties, but other properties (e.g.,
     cursor_color) are dependent upon them.  */
  for (p = 0; p < i; p++) 
    {
      Lisp_Object prop, val;

      prop = parms[p];
      val = values[p];
      if (EQ (prop, Qforeground_color) || EQ (prop, Qbackground_color))
	{
	  register Lisp_Object param_index, old_value;

	  param_index = Fget (prop, Qmw32_frame_parameter);
	  old_value = get_frame_param (f, prop);
	  store_frame_param (f, prop, val);
 	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (mw32_frame_parms) / sizeof (mw32_frame_parms[0])))
	    (*mw32_frame_parms[XINT (param_index)].setter) (f, val, old_value);
	}
    }

  /* Now process them in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val = values[i];

      if (EQ (prop, Qwidth) && NUMBERP (val))
	width = XFASTINT (val);
      else if (EQ (prop, Qheight) && NUMBERP (val))
	height = XFASTINT (val);
      else if (EQ (prop, Qtop))
	top = val;
      else if (EQ (prop, Qleft))
	left = val;
      else if (EQ (prop, Qicon_top))
	icon_top = val;
      else if (EQ (prop, Qicon_left))
	icon_left = val;
      else if (EQ (prop, Qforeground_color) || EQ (prop, Qbackground_color))
	/* Processed above.  */
	continue;
      else
	{
	  register Lisp_Object param_index, old_value;

	  param_index = Fget (prop, Qmw32_frame_parameter);
	  old_value = get_frame_param (f, prop);
	  store_frame_param (f, prop, val);
 	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (mw32_frame_parms) / sizeof (mw32_frame_parms[0])))
	    (*mw32_frame_parms[XINT (param_index)].setter) (f, val, old_value);
	}
    }

  /* Don't die if just one of these was set.  */
  if (EQ (left, Qunbound))
    XSETINT (left, f->output_data.mw32->left_pos);
  if (EQ (top, Qunbound))
    XSETINT (top, f->output_data.mw32->top_pos);

  /* If one of the icon positions was not set, preserve or default it.  */
  if (EQ (icon_left, Qunbound) || ! INTEGERP (icon_left))
    {
      icon_left_no_change = 1;
      icon_left = Fcdr (Fassq (Qicon_left, f->param_alist));
      if (NILP (icon_left))
	XSETINT (icon_left, 0);
    }
  if (EQ (icon_top, Qunbound) || ! INTEGERP (icon_top))
    {
      icon_top_no_change = 1;
      icon_top = Fcdr (Fassq (Qicon_top, f->param_alist));
      if (NILP (icon_top))
	XSETINT (icon_top, 0);
    }

  /* Don't set these parameters unless they've been explicitly
     specified.  The window might be mapped or resized while we're in
     this function, and we don't want to override that unless the lisp
     code has asked for it.

     Don't set these parameters unless they actually differ from the
     window's current parameters; the window may not actually exist
     yet.  */
  {
    Lisp_Object frame;

    check_frame_size (f, &height, &width);

    XSETFRAME (frame, f);

    if (width != FRAME_WIDTH (f)
	|| height != FRAME_HEIGHT (f)
	|| FRAME_NEW_HEIGHT (f) || FRAME_NEW_WIDTH (f))
      Fset_frame_size (frame, make_number (width), make_number (height));

    if ((!NILP (left) || !NILP (top))
	&& ! (NUMBERP (left) && XINT (left) == f->output_data.mw32->left_pos
	      && NUMBERP (top) && XINT (top) == f->output_data.mw32->top_pos))
      {
	int leftpos = (NUMBERP (left) ? XINT (left) : 0);
	int toppos = (NUMBERP (top) ? XINT (top) : 0);

#if 0
	/* Record the signs.  */
	f->output_data.x->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->output_data.x->size_hint_flags |= XNegative;
	else if (INTEGERP (left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->output_data.x->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->output_data.x->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->output_data.x->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->output_data.x->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->output_data.x->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
	  }
	f->output_data.mw32->win_gravity = NorthWestGravity;
#endif

	/* Store the numeric value of the position.  */
	f->output_data.mw32->top_pos = toppos;
	f->output_data.mw32->left_pos = leftpos;


	/* Actually set that position, and convert to absolute.  */
	x_set_offset (f, leftpos, toppos, -1);
      }
  }

  UNGCPRO;
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

void
x_report_frame_params (FRAME_PTR f, Lisp_Object *alistptr)
{
  char buf[16];
  Lisp_Object tem;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->output_data.mw32->left_pos);
  if (f->output_data.mw32->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->output_data.mw32->top_pos);
  if (f->output_data.mw32->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
		  make_number (f->output_data.mw32->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
		  make_number (f->output_data.mw32->internal_border_width));
  sprintf (buf, "%ld", (long) GetWindowLong (FRAME_MW32_WINDOW (f),
					     GWL_ID));
  store_in_alist (alistptr, Qwindow_id,
		  build_string (buf));
#if 0
  sprintf (buf, "%ld", (long) FRAME_OUTER_WINDOW (f));
  store_in_alist (alistptr, Qouter_window_id,
		  build_string (buf));
#endif
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_MW32_DISPLAY_INFO (f)->name_list_element));

  if (f->output_data.mw32->parent_desc == FRAME_MW32_DISPLAY_INFO (f)->root_window)
    tem = Qnil;
  else
    tem = make_number (GetWindowLong (f->output_data.mw32->parent_desc,
				      GWL_ID));
  store_in_alist (alistptr, Qparent_id, tem);
}


/* Gamma-correct COLOR on frame F.  */

COLORREF
gamma_correct (FRAME_PTR f, COLORREF color)
{
  if (f->gamma)
    {
      int r, g, b;
      r = (int) (pow (GetRValue (color) / 256.0, f->gamma) * 256.0 + 0.5);
      g = (int) (pow (GetGValue (color) / 256.0, f->gamma) * 256.0 + 0.5);
      b = (int) (pow (GetBValue (color) / 256.0, f->gamma) * 256.0 + 0.5);
      return RGB (r, g, b);
    }
  return color;
}


/* Decide if color named COLOR_NAME is valid for use on frame F.  If
   so, return the RGB values in COLOR.  If ALLOC_P is non-zero,
   allocate the color.  Value is zero if COLOR_NAME is invalid, or
   no color could be allocated.  */

static int
mw32_defined_color (FRAME_PTR f, char *color_name,
		    COLORREF *color_def, int alloc_p)
{
  COLORREF col;
  col = x_to_mw32_color (color_name);

  if (col == CLR_INVALID) return 0;
  *color_def = col;

  return 1;
}

int
x_defined_color (FRAME_PTR f, char *color_name, XColor *color_def, int alloc_p)
{
  COLORREF col;

  col = x_to_mw32_color (color_name);
  if (col == CLR_INVALID) return 0;

  if (f)
    col = gamma_correct (f, col);

  memset (color_def, 0, sizeof (XColor));
  /* pixel value is used only for specifying GC or window attribute
     in terms of X semantics.  We don't have to emulate it on MW32. */
  color_def->pixel = col;
  color_def->red = GetRValue (col) << 8;
  color_def->green = GetGValue (col) << 8;
  color_def->blue = GetBValue (col) << 8;

  return 1;
}

/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG says.
   Signal an error if color can't be allocated.  */

static COLORREF
mw32_decode_color (FRAME_PTR f,
		   Lisp_Object color_name,
		   COLORREF mono_color)
{
  COLORREF ret;
  CHECK_STRING (color_name, 0);

#if 0 /* Don't do this.  It's wrong when we're not using the default
	 colormap, it makes freeing difficult, and it's probably not
	 an important optimization.  */
  if (strcmp (SDATA (color_name), "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (SDATA (color_name), "white") == 0)
    return WHITE_PIX_DEFAULT (f);
#endif

  /* Return MONO_COLOR for monochrome frames.  */
  if (FRAME_MW32_DISPLAY_INFO (f)->n_planes == 1)
    return mono_color;

  if (mw32_defined_color (f, SDATA (color_name), &ret, 1))
    return ret;

  Fsignal (Qerror, Fcons (build_string ("Undefined color"),
			  Fcons (color_name, Qnil)));
  return 0;
}



/***
    Frame parameter setting functions.
 ***/

/* Change the `line-spacing' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

static void
mw32_set_line_spacing (FRAME_PTR f, Lisp_Object new_value,
		       Lisp_Object old_value)
{
  if (NILP (new_value))
    f->extra_line_spacing = 0;
  else if (NATNUMP (new_value))
    f->extra_line_spacing = XFASTINT (new_value);
  else
    Fsignal (Qerror, Fcons (build_string ("Invalid line-spacing"),
			    Fcons (new_value, Qnil)));
  if (FRAME_VISIBLE_P (f))
    redraw_frame (f);
}


/* Change the `screen-gamma' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new
   value.  */

static void
mw32_set_screen_gamma (FRAME_PTR f, Lisp_Object new_value,
		       Lisp_Object old_value)
{
  if (NILP (new_value))
    f->gamma = 0;
  else if (NUMBERP (new_value) && XFLOATINT (new_value) > 0)
    /* The value 0.4545 is the normal viewing gamma.  */
    f->gamma = 1.0 / (0.4545 * XFLOATINT (new_value));
  else
    Fsignal (Qerror, Fcons (build_string ("Invalid screen-gamma"),
			    Fcons (new_value, Qnil)));

  clear_face_cache (0);
}


/* Functions called only from `x_set_frame_param'
   to set individual parameters.

   If FRAME_X_WINDOW (f) is 0,
   the frame is being created and its X-window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

void
mw32_set_foreground_color (FRAME_PTR f, Lisp_Object arg, Lisp_Object old_value)
{
  struct mw32_output *x = f->output_data.mw32;
  COLORREF fg, old_fg;

  fg = mw32_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = x->foreground_pixel;
  x->foreground_pixel = fg;

  if (FRAME_MW32_WINDOW (f) != 0)
    {
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }

#if 0 /* TODO: color */
  unload_color (f, old_fg);
#endif 
}

static void
mw32_set_background_color (FRAME_PTR f, Lisp_Object arg, Lisp_Object old_value)
{
  struct mw32_output *x = f->output_data.mw32;
  COLORREF bg;

  bg = mw32_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
#if 0 /* TODO: color */
  unload_color (f, x->background_pixel);
#endif 
  x->background_pixel = bg;

  if (FRAME_MW32_WINDOW (f) != 0)
    {
      update_face_from_frame_parameter (f, Qbackground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
mw32_set_mouse_color (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
#if 1

  COLORREF mask_color;
  if (!EQ (Qnil, arg))
    f->output_data.mw32->mouse_pixel = mw32_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  mask_color = f->output_data.mw32->background_pixel;
				/* No invisible pointers. */
  if (mask_color == f->output_data.mw32->mouse_pixel
	&& mask_color == f->output_data.mw32->background_pixel)
    f->output_data.mw32->mouse_pixel = f->output_data.mw32->foreground_pixel;

#else
  struct mw32_output *x = f->output_data.mw32;
  Cursor cursor, nontext_cursor, mode_cursor, cross_cursor;
  Cursor hourglass_cursor, horizontal_drag_cursor;
  int count;
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = x->background_pixel;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    {
      x_free_colors (f, &pixel, 1);
      pixel = x_copy_color (f, x->foreground_pixel);
    }

  unload_color (f, x->mouse_pixel);
  x->mouse_pixel = pixel;

  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  count = x_catch_errors (dpy);

  if (!NILP (Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape, 0);
      cursor = XCreateFontCursor (dpy, XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad text pointer cursor: %s");

  if (!NILP (Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape, 0);
      nontext_cursor
	= XCreateFontCursor (dpy, XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (dpy, XC_left_ptr);
  x_check_errors (dpy, "bad nontext pointer cursor: %s");

  if (!NILP (Vx_hourglass_pointer_shape))
    {
      CHECK_NUMBER (Vx_hourglass_pointer_shape, 0);
      hourglass_cursor
	= XCreateFontCursor (dpy, XINT (Vx_hourglass_pointer_shape));
    }
  else
    hourglass_cursor = XCreateFontCursor (dpy, XC_watch);
  x_check_errors (dpy, "bad hourglass pointer cursor: %s");
  
  x_check_errors (dpy, "bad nontext pointer cursor: %s");
  if (!NILP (Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape, 0);
      mode_cursor = XCreateFontCursor (dpy, XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad modeline pointer cursor: %s");

  if (!NILP (Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape, 0);
      cross_cursor
	= XCreateFontCursor (dpy, XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    cross_cursor = XCreateFontCursor (dpy, XC_crosshair);

  if (!NILP (Vx_window_horizontal_drag_shape))
    {
      CHECK_NUMBER (Vx_window_horizontal_drag_shape, 0);
      horizontal_drag_cursor
	= XCreateFontCursor (dpy, XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (dpy, XC_sb_h_double_arrow);

  /* Check and report errors with the above calls.  */
  x_check_errors (dpy, "can't set cursor shape: %s");
  x_uncatch_errors (dpy, count);

  {
    XColor fore_color, back_color;

    fore_color.pixel = x->mouse_pixel;
    x_query_color (f, &fore_color);
    back_color.pixel = mask_color;
    x_query_color (f, &back_color);
    
    XRecolorCursor (dpy, cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, nontext_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, mode_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, cross_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hourglass_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, horizontal_drag_cursor, &fore_color, &back_color);
  }

  if (FRAME_X_WINDOW (f) != 0)
    XDefineCursor (dpy, FRAME_X_WINDOW (f), cursor);

  if (cursor != x->text_cursor
      && x->text_cursor != 0)
    XFreeCursor (dpy, x->text_cursor);
  x->text_cursor = cursor;

  if (nontext_cursor != x->nontext_cursor
      && x->nontext_cursor != 0)
    XFreeCursor (dpy, x->nontext_cursor);
  x->nontext_cursor = nontext_cursor;

  if (hourglass_cursor != x->hourglass_cursor
      && x->hourglass_cursor != 0)
    XFreeCursor (dpy, x->hourglass_cursor);
  x->hourglass_cursor = hourglass_cursor;

  if (mode_cursor != x->modeline_cursor
      && x->modeline_cursor != 0)
    XFreeCursor (dpy, f->output_data.x->modeline_cursor);
  x->modeline_cursor = mode_cursor;
  
  if (cross_cursor != x->cross_cursor
      && x->cross_cursor != 0)
    XFreeCursor (dpy, x->cross_cursor);
  x->cross_cursor = cross_cursor;

  if (horizontal_drag_cursor != x->horizontal_drag_cursor
      && x->horizontal_drag_cursor != 0)
    XFreeCursor (dpy, x->horizontal_drag_cursor);
  x->horizontal_drag_cursor = horizontal_drag_cursor;

  XFlush (dpy);
  UNBLOCK_INPUT;
#endif
  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

void
mw32_set_cursor_color (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
#if 1
  unsigned long fore_pixel;

  if (!EQ (Vmw32_cursor_fore_pixel, Qnil))
    fore_pixel = mw32_decode_color (f, Vmw32_cursor_fore_pixel,
				    WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = f->output_data.mw32->background_pixel;
  f->output_data.mw32->cursor_pixel = mw32_decode_color (f, arg,
							 BLACK_PIX_DEFAULT (f));
  
  /* Make sure that the cursor color differs from the background color.  */
  if (f->output_data.mw32->cursor_pixel == f->output_data.mw32->background_pixel)
    {
      f->output_data.mw32->cursor_pixel = f->output_data.mw32->mouse_pixel;
      if (f->output_data.mw32->cursor_pixel == fore_pixel)
	fore_pixel = f->output_data.mw32->background_pixel;
    }
  f->output_data.mw32->cursor_foreground_pixel = fore_pixel;

  if (FRAME_MW32_WINDOW (f) != 0)
    {
      if (FRAME_VISIBLE_P (f))
	{
	  mw32_update_cursor (f, 0);
	  mw32_update_cursor (f, 1);
	}
    }
#else
  unsigned long fore_pixel, pixel;
  int fore_pixel_allocated_p = 0, pixel_allocated_p = 0;
  struct mw32_output *x = f->output_data.mw32;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      fore_pixel = x_decode_color (f, Vmw32_cursor_fore_pixel,
				   WHITE_PIX_DEFAULT (f));
      fore_pixel_allocated_p = 1;
    }
  else
    fore_pixel = x->background_pixel;
  
  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  pixel_allocated_p = 1;

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == x->background_pixel)
    {
      if (pixel_allocated_p)
	{
	  x_free_colors (f, &pixel, 1);
	  pixel_allocated_p = 0;
	}
      
      pixel = x->mouse_pixel;
      if (pixel == fore_pixel)
	{
	  if (fore_pixel_allocated_p)
	    {
	      x_free_colors (f, &fore_pixel, 1);
	      fore_pixel_allocated_p = 0;
	    }
	  fore_pixel = x->background_pixel;
	}
    }

  unload_color (f, x->cursor_foreground_pixel);
  if (!fore_pixel_allocated_p)
    fore_pixel = x_copy_color (f, fore_pixel);
  x->cursor_foreground_pixel = fore_pixel;

  unload_color (f, x->cursor_pixel);
  if (!pixel_allocated_p)
    pixel = x_copy_color (f, pixel);
  x->cursor_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      XSetBackground (FRAME_X_DISPLAY (f), x->cursor_gc, x->cursor_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), x->cursor_gc, fore_pixel);
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
	{
	  x_update_cursor (f, 0);
	  x_update_cursor (f, 1);
	}
    }
#endif

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the X server.
   Note that this does not fully take effect if done before
   F has an x-window; it must be redone when the window is created.

   Note: this is done in two routines because of the way X10 works.

   Note: under X11, this is normally the province of the window manager,
   and so emacs' border colors may be overridden.  */

void
mw32_set_border_color (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  COLORREF pix;

  CHECK_STRING (arg, 0);

  pix = mw32_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  f->output_data.mw32->border_pixel = pix;

  if ((FRAME_MW32_WINDOW (f) != 0)
      && (f->output_data.mw32->border_width > 0))
    {
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }

  update_face_from_frame_parameter (f, Qborder_color, arg);
}

/* Value is the internal representation of the specified cursor type
   ARG.  If type is BAR_CURSOR, return in *WIDTH the specified width
   of the bar cursor.  */

enum text_cursor_kinds
mw32_specified_cursor_type (Lisp_Object arg, int *width)
{
  enum text_cursor_kinds type;
  
  if (CONSP (arg)
      && (EQ (XCAR (arg), Qbar) || EQ (XCAR (arg), Qhairline_caret))
      && INTEGERP (XCDR (arg))
      && XINT (XCDR (arg)) >= 0)
    {
      arg = XCAR (arg);
      *width = XINT (XCDR (arg));
    }

  if (NILP (arg))
    {
      type = NO_CURSOR;
    }
  else if (EQ (arg, Qbar))
    {
      type = BAR_CURSOR;
      *width = 2;
    }
  else if (EQ (arg, Qhairline_caret))
    {
      type = HAIRLINE_CARET_CURSOR;
      *width = 1;
    }
  else if (EQ (arg, Qcaret))
    {
      type = CARET_CURSOR;
    }
  else if (EQ (arg, Qcheckered_caret))
    {
      type = CHECKERED_CARET_CURSOR;
    }
  else
    {
      type = FILLED_BOX_CURSOR;
    }

  return type;
}

void
mw32_set_cursor_type (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  int width = 1;
  
  FRAME_DESIRED_CURSOR (f) = mw32_specified_cursor_type (arg, &width);
}

void
mw32_set_cursor_height (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  int old= f->output_data.mw32->cursor_height;

  CHECK_NUMBER (arg, 0);

  if (MW32_FRAME_CARET_HEIGHT (f) == XINT (arg))
    return;

  MW32_FRAME_CARET_HEIGHT (f) = XINT (arg);
}


void
mw32_set_font (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  Lisp_Object result;
  Lisp_Object fontset_name;
  Lisp_Object frame;
  int old_fontset = f->output_data.mw32->fontset;

  CHECK_STRING (arg, 1);

  fontset_name = Fquery_fontset (arg, Qnil);

  result = (STRINGP (fontset_name)
	    ? mw32_new_fontset (f, SDATA (fontset_name))
	    : mw32_new_font (f, SDATA (arg)));
  
  if (EQ (result, Qnil))
    error ("Font `%s' is not defined", SDATA (arg));
  else if (EQ (result, Qt))
    error ("The characters of the given font have varying widths");
  else if (STRINGP (result))
    {
      if (STRINGP (fontset_name))
	{
	  /* Fontset names are built from ASCII font names, so the
	     names may be equal despite there was a change.  */
	  if (old_fontset == f->output_data.mw32->fontset)
	    return;
	}
      else if (!NILP (Fequal (result, oldval)))
	return;
      
      store_frame_param (f, Qfont, result);
      recompute_basic_faces (f);
    }
  else
    abort ();

  do_pending_window_change (0);

  /* Don't call `face-set-after-frame-default' when faces haven't been
     initialized yet.  This is the case when called from
     Fx_create_frame.  In that case, the X widget or window doesn't
     exist either, and we can end up in x_report_frame_params with a
     null widget which gives a segfault.  */
  if (FRAME_FACE_CACHE (f))
    {
      XSETFRAME (frame, f);
      call1 (Qface_set_after_frame_default, frame);
    }
}

#ifdef IME_CONTROL
void
mw32_set_frame_ime_font (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
#if 0				/* ??? */
  MSG msg;
#endif

  if (! NILP (arg))
    {
      CHECK_LIST (arg, 0);
      {
	extern Lisp_Object Qw32_logfont; /* mw32font.c */
	Lisp_Object tmpcar = CAR (arg);
	CHECK_SYMBOL (tmpcar, 1);
	if (!EQ (tmpcar, Qw32_logfont))
	  error ("ime-font: invalid logfont `%s'.",
		 SDATA (Fprin1_to_string (arg, Qt)));
      }
    }
  mw32font_set_frame_ime_font_by_llogfont (f, arg);

#if 0				/* ??? */
  WAIT_REPLY_MESSAGE (&msg, WM_MULE_IMM_SET_COMPOSITION_FONT_REPLY);
#endif
}
#endif

void
mw32_set_border_width (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  CHECK_NUMBER (arg, 0);

  if (XINT (arg) == f->output_data.mw32->border_width)
    return;

  if (FRAME_MW32_WINDOW (f) != 0)
    error ("Cannot change the border width of a window");

  f->output_data.mw32->border_width = XINT (arg);
}

void
mw32_set_internal_border_width (FRAME_PTR f, Lisp_Object arg,
				Lisp_Object oldval)
{
  int old = f->output_data.mw32->internal_border_width;

  CHECK_NUMBER (arg, 0);
  f->output_data.mw32->internal_border_width = XINT (arg);
  if (f->output_data.mw32->internal_border_width < 0)
    f->output_data.mw32->internal_border_width = 0;

  if (f->output_data.mw32->internal_border_width == old)
    return;

  if (FRAME_MW32_WINDOW (f) != 0)
    {
      x_set_window_size (f, 0, f->width, f->height);
      SET_FRAME_GARBAGED (f);
      do_pending_window_change (0);
    }
}

void
mw32_set_visibility (FRAME_PTR f, Lisp_Object value, Lisp_Object oldval)
{
  Lisp_Object frame;
  XSETFRAME (frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame, Qt);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
}


/* Change window heights in windows rooted in WINDOW by N lines.  */

static void
mw32_change_window_heights (Lisp_Object window, int n)
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);

  if (INTEGERP (w->orig_top))
    XSETFASTINT (w->orig_top, XFASTINT (w->orig_top) + n);
  if (INTEGERP (w->orig_height))
    XSETFASTINT (w->orig_height, XFASTINT (w->orig_height) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    mw32_change_window_heights (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      mw32_change_window_heights (window, n);
    }
}

void
x_set_menu_bar_lines (FRAME_PTR f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itslef, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 0)
	set_frame_menubar (f, 1, 0);
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
    }

  adjust_glyphs (f);
}


/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tool bar lines.  This function changes the
   height of all windows on frame F to match the new tool bar height.
   The frame's height doesn't change.  */

void
x_set_tool_bar_lines (FRAME_PTR f, Lisp_Object value, Lisp_Object oldval)
{
  int delta, nlines, root_height;
  Lisp_Object root_window;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an integer >= 0.  */
  if (INTEGERP (value) && XINT (value) >= 0)
    nlines = XFASTINT (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  ++windows_or_buffers_changed;

  delta = nlines - FRAME_TOOL_BAR_LINES (f);

  /* Don't resize the tool-bar to more than we have room for.  */
  root_window = FRAME_ROOT_WINDOW (f);
  root_height = XINT (XWINDOW (root_window)->height);
  if (root_height - delta < 1)
    {
      delta = root_height - 1;
      nlines = FRAME_TOOL_BAR_LINES (f) + delta;
    }

  FRAME_TOOL_BAR_LINES (f) = nlines;
  mw32_change_window_heights (root_window, delta);
  adjust_glyphs (f);
  
  /* We also have to make sure that the internal border at the top of
     the frame, below the menu bar or tool bar, is redrawn when the
     tool bar disappears.  This is so because the internal border is
     below the tool bar if one is displayed, but is below the menu bar
     if there isn't a tool bar.  The tool bar draws into the area
     below the menu bar.  */
  if (FRAME_MW32_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
    {
      updating_frame = f;
      clear_frame ();
      clear_current_matrices (f);
      updating_frame = NULL;
    }

  /* If the tool bar gets smaller, the internal border below it
     has to be cleared.  It was formerly part of the display
     of the larger tool bar, and updating windows won't clear it.  */

  /* The condition to clear the internal border below was changed,
     because it is not cleared when the tool bar gets larger.  */
  if (delta != 0)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = PIXEL_WIDTH (f);
      int y = nlines * CANON_Y_UNIT (f);

      if (updating_frame)
	f = updating_frame;
      else
	f = SELECTED_FRAME ();

      BLOCK_INPUT;
      mw32_clear_area (f, 0, y, width, y + height);
      UNBLOCK_INPUT;

      if (WINDOWP (f->tool_bar_window))
	clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);
    }
}


/* Set the foreground color for scroll bars on frame F to VALUE.
   VALUE should be a string, a color name.  If it isn't a string or
   isn't a valid color name, do nothing.  OLDVAL is the old value of
   the frame parameter.  */

static void
mw32_set_scroll_bar_foreground (FRAME_PTR f, Lisp_Object value,
				Lisp_Object oldval)
{
  COLORREF pixel;
  
  if (STRINGP (value))
    pixel = mw32_decode_color (f, value, BLACK_PIX_DEFAULT (f));
  else
    pixel = CLR_INVALID;

#if 0
  if (f->output_data.mw32->scroll_bar_foreground_pixel != CLR_INVALID)
    unload_color (f, f->output_data.mw32->scroll_bar_foreground_pixel);
#endif
  
  f->output_data.mw32->scroll_bar_foreground_pixel = pixel;
  if (FRAME_MW32_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (condemn_scroll_bars_hook)
	(*condemn_scroll_bars_hook) (f);
      if (judge_scroll_bars_hook)
	(*judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_foreground, value);
      redraw_frame (f);
    }
}


/* Set the background color for scroll bars on frame F to VALUE VALUE
   should be a string, a color name.  If it isn't a string or isn't a
   valid color name, do nothing.  OLDVAL is the old value of the frame
   parameter.  */

static void
mw32_set_scroll_bar_background (FRAME_PTR f, Lisp_Object value,
				Lisp_Object oldval)
{
  COLORREF pixel;

  if (STRINGP (value))
    pixel = mw32_decode_color (f, value, WHITE_PIX_DEFAULT (f));
  else
    pixel = CLR_INVALID;

#if 0  
  if (f->output_data.mw32->scroll_bar_background_pixel != CLR_INVALID)
    unload_color (f, f->output_data.mw32->scroll_bar_background_pixel);
#endif
  
  f->output_data.mw32->scroll_bar_background_pixel = pixel;
  if (FRAME_MW32_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (condemn_scroll_bars_hook)
	(*condemn_scroll_bars_hook) (f);
      if (judge_scroll_bars_hook)
	(*judge_scroll_bars_hook) (f);
      
      update_face_from_frame_parameter (f, Qscroll_bar_background, value);
      redraw_frame (f);
    }
}


/* Prepare to encode Lisp string as a text in a format appropriate for
   Windows.
   Not to recommend to use this function alone.  Use with the macro,
   MW32_ENCODE_TEXT().  */

int
mw32_encode_text_prepare (Lisp_Object coding_system,
			  struct coding_system *coding,
			  int bytes)
{
  setup_coding_system (coding_system, coding);
  coding->mode |= CODING_MODE_LAST_BLOCK;
  if (coding->type == coding_type_iso2022)
    coding->flags |= CODING_FLAG_ISO_SAFE;
  /* We suppress producing escape sequences for composition.  */
  coding->composing = COMPOSITION_DISABLED;
  return encoding_buffer_size (coding, bytes);
}

/* 
   Encode str in Lisp String to string for external systems,
   which include Windows APIs.
   This function returns a string in LPTSTR, and set the size
   in byte to *psize only if psize is not NULL.
*/
LPTSTR
mw32_encode_lispy_string (Lisp_Object coding_system,
			  Lisp_Object str,
			  int *psize)
{
  str = code_convert_string_norecord (str, coding_system, 1);
  if (psize) *psize = LISPY_STRING_BYTES (str);

  return (LPTSTR) SDATA (str);
}

/* 
   Decode tstr in LPTSTR to Lisp String for Emacs system.
   This function returns a Lisp String.  If size is not 0,
   it is regarded as size of tstr in byte.  If size is 0,
   this function call lstrlen to count the byte size of tstr.
*/
Lisp_Object
mw32_decode_lispy_string (Lisp_Object coding_system,
			  LPTSTR tstr, int size)
{
  Lisp_Object str;
  if (size == 0) {
    size = lstrlen (tstr);
  }
  str = make_string (tstr, size);
  return code_convert_string_norecord (str, coding_system, 0);
}


/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       x_id_name.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

static void
mw32_set_name (FRAME_PTR f, Lisp_Object name, int explicit)
{
  /* Make sure that requests from lisp code override requests from 
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 1;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the x_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_MW32_DISPLAY_INFO (f)->mw32_id_name, SDATA (f->name)))
	return;
      name = build_string (FRAME_MW32_DISPLAY_INFO (f)->mw32_id_name);
    }
  else
    CHECK_STRING (name, 0);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  f->name = name;

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  if (FRAME_MW32_WINDOW (f))
    {
      int size;
      char *ttext;
      MW32_ENCODE_TEXT (name, Vw32_system_coding_system, &ttext, &size);
      BLOCK_INPUT;
      SetWindowText (FRAME_MW32_WINDOW (f), ttext);
      UNBLOCK_INPUT;
    }
}

/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
void
mw32_explicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  mw32_set_name (f, arg, 1);
}

/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
mw32_implicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  mw32_set_name (f, arg, 0);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

static void
mw32_set_title (FRAME_PTR f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name, 0);

  if (FRAME_MW32_WINDOW (f))
    {
      int size;
      char *ttext;
      MW32_ENCODE_TEXT (name, Vw32_system_coding_system, &ttext, &size);
      BLOCK_INPUT;
      SetWindowText (FRAME_MW32_WINDOW (f), ttext);
      UNBLOCK_INPUT;
    }
}


static void
mw32_set_autoraise (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  f->auto_raise = !EQ (Qnil, arg);
}

static void
mw32_set_autolower (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->auto_lower = !EQ (Qnil, arg);
}

static void
mw32_set_unsplittable (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  f->no_split = !NILP (arg);
}

static void
mw32_set_vertical_scroll_bars (FRAME_PTR f, Lisp_Object arg,
			       Lisp_Object oldval)
{
  if ((EQ (arg, Qleft) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
      || (EQ (arg, Qright) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
      || (NILP (arg) && FRAME_HAS_VERTICAL_SCROLL_BARS (f))
      || (!NILP (arg) && ! FRAME_HAS_VERTICAL_SCROLL_BARS (f)))
    {
      FRAME_VERTICAL_SCROLL_BAR_TYPE (f)
	= (NILP (arg)
	   ? vertical_scroll_bar_none
	   : EQ (Qright, arg)
	   ? vertical_scroll_bar_right 
	   : vertical_scroll_bar_left);

      /* We set this parameter before creating the X window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_MW32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
}

static void
mw32_set_scroll_bar_width (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  int wid = FRAME_DEFAULT_FONT_WIDTH (f);

  if (NILP (arg))
    {
      /* A minimum width of 14 doesn't look good for toolkit scroll bars.  */
      int width = 16 + 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM;
      FRAME_SCROLL_BAR_COLS (f) = (width + wid - 1) / wid;
#if 1
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = width;
#else
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = FRAME_SCROLL_BAR_COLS (f) * wid;
#endif

      if (FRAME_MW32_WINDOW (f))
        x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
  else if (INTEGERP (arg) && XINT (arg) > 0
	   && XFASTINT (arg) != FRAME_SCROLL_BAR_PIXEL_WIDTH (f))
    {
      if (XFASTINT (arg) <= 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM)
	XSETINT (arg, 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM + 1);

      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = XFASTINT (arg);
      FRAME_SCROLL_BAR_COLS (f) = (XFASTINT (arg) + wid-1) / wid;
      if (FRAME_MW32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }

  change_frame_size (f, 0, FRAME_WIDTH (f), 0, 0, 0);
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.hpos = 0;
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.x = 0;
}



/***********************************************************************
	    		X resource emulation.
 ***********************************************************************/

/* Subroutines of creating an X frame.  */

/* Make sure that Vx_resource_name is set to a reasonable value.
   Fix it up, or set it to `emacs' if it is too hopeless.  */

static void
validate_x_resource_name (void)
{
  int len = 0;
  /* Number of valid characters in the resource name.  */
  int good_count = 0;
  /* Number of invalid characters in the resource name.  */
  int bad_count = 0;
  Lisp_Object new;
  int i;

  if (!STRINGP (Vx_resource_class))
    Vx_resource_class = build_string (EMACS_CLASS);

  if (STRINGP (Vx_resource_name))
    {
      unsigned char *p = SDATA (Vx_resource_name);
      int i;

      len = SBYTES (Vx_resource_name);

      /* Only letters, digits, - and _ are valid in resource names.
	 Count the valid characters and count the invalid ones.  */
      for (i = 0; i < len; i++)
	{
	  int c = p[i];
	  if (! ((c >= 'a' && c <= 'z')
		 || (c >= 'A' && c <= 'Z')
		 || (c >= '0' && c <= '9')
		 || c == '-' || c == '_'))
	    bad_count++;
	  else
	    good_count++;
	}
    }
  else
    /* Not a string => completely invalid.  */
    bad_count = 5, good_count = 0;

  /* If name is valid already, return.  */
  if (bad_count == 0)
    return;

  /* If name is entirely invalid, or nearly so, use `emacs'.  */
  if (good_count == 0
      || (good_count == 1 && bad_count > 0))
    {
      Vx_resource_name = build_string ("emacs");
      return;
    }

  /* Name is partly valid.  Copy it and replace the invalid characters
     with underscores.  */

  Vx_resource_name = new = Fcopy_sequence (Vx_resource_name);

  for (i = 0; i < len; i++)
    {
      int c = SREF (new, i);
      if (! ((c >= 'a' && c <= 'z')
	     || (c >= 'A' && c <= 'Z')
	     || (c >= '0' && c <= '9')
	     || c == '-' || c == '_'))
	SSET (new, i, '_');
    }
}


DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 2, 4, 0,
  "Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.\n\
This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the\n\
class, where INSTANCE is the name under which Emacs was invoked, or\n\
the name specified by the `-name' or `-rn' command-line arguments.\n\
\n\
The optional arguments COMPONENT and SUBCLASS add to the key and the\n\
class, respectively.  You must specify both of them or neither.\n\
If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'\n\
and the class is `Emacs.CLASS.SUBCLASS'.")
  (attribute, class, component, subclass)
     Lisp_Object attribute, class, component, subclass;
{
  register char *value;
  char *name_key;
  char *class_key;

  check_mw32 ();

  CHECK_STRING (attribute, 0);
  CHECK_STRING (class, 0);

  if (!NILP (component))
    CHECK_STRING (component, 1);
  if (!NILP (subclass))
    CHECK_STRING (subclass, 2);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (SBYTES (Vx_resource_name)
			      + (STRINGP (component)
				 ? SBYTES (component) : 0)
			      + SBYTES (attribute)
			      + 3);

  class_key = (char *) alloca (SBYTES (Vx_resource_class)
			       + SBYTES (class)
			       + (STRINGP (subclass)
				  ? SBYTES (subclass) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, SDATA (Vx_resource_name));
  strcpy (class_key, SDATA (Vx_resource_class));

  strcat (class_key, ".");
  strcat (class_key, SDATA (class));

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, SDATA (subclass));

      strcat (name_key, ".");
      strcat (name_key, SDATA (component));
    }

  strcat (name_key, ".");
  strcat (name_key, SDATA (attribute));

  value = x_get_string_resource (Qnil, name_key, class_key);

  if (value != (char *) 0)
    return build_string (value);
  else
    return Qnil;
}

/* Get an X resource, like Fx_get_resource, but for display DPYINFO.  */

Lisp_Object
display_x_get_resource (struct mw32_display_info *dpyinfo,
			Lisp_Object attribute, Lisp_Object class,
			Lisp_Object component, Lisp_Object subclass)
{
  register char *value;
  char *name_key;
  char *class_key;

  CHECK_STRING (attribute, 0);
  CHECK_STRING (class, 0);

  if (!NILP (component))
    CHECK_STRING (component, 1);
  if (!NILP (subclass))
    CHECK_STRING (subclass, 2);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (SBYTES (Vx_resource_name)
			      + (STRINGP (component)
				 ? SBYTES (component) : 0)
			      + SBYTES (attribute)
			      + 3);

  class_key = (char *) alloca (SBYTES (Vx_resource_class)
			       + SBYTES (class)
			       + (STRINGP (subclass)
				  ? SBYTES (subclass) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, SDATA (Vx_resource_name));
  strcpy (class_key, SDATA (Vx_resource_class));

  strcat (class_key, ".");
  strcat (class_key, SDATA (class));

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, SDATA (subclass));

      strcat (name_key, ".");
      strcat (name_key, SDATA (component));
    }

  strcat (name_key, ".");
  strcat (name_key, SDATA (attribute));

  value = x_get_string_resource (Qnil, name_key, class_key);

  if (value != (char *) 0)
    return build_string (value);
  else
    return Qnil;
}

/* Used when C code wants a resource value.  */
/* But actually not used for now.  */

static char *
x_get_resource_string (char *attribute, char *class)
{
  char *name_key;
  char *class_key;
  struct frame *sf = SELECTED_FRAME ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  */
  name_key = (char *) alloca (SBYTES (Vinvocation_name)
			      + strlen (attribute) + 2);
  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + strlen (class) + 2);

  sprintf (name_key, "%s.%s",
	   SDATA (Vinvocation_name),
	   attribute);
  sprintf (class_key, "%s.%s", EMACS_CLASS, class);

  return x_get_string_resource (Qnil, name_key, class_key);
}

/* Types we might convert a resource string into.  */
enum resource_types
{
  RES_TYPE_NUMBER,
  RES_TYPE_FLOAT,
  RES_TYPE_BOOLEAN,
  RES_TYPE_STRING,
  RES_TYPE_SYMBOL
};

/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   x_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

static Lisp_Object
mw32_get_arg (struct mw32_display_info *dpyinfo,
	      Lisp_Object alist, Lisp_Object param,
	      char *attribute,
	      char *class,
	      enum resource_types type)
{
  register Lisp_Object tem;

  tem = Fassq (param, alist);
  if (EQ (tem, Qnil))
    tem = Fassq (param, Vdefault_frame_alist);
  if (EQ (tem, Qnil))
    {

      if (attribute)
	{
	  tem = display_x_get_resource (dpyinfo,
					build_string (attribute),
					build_string (class),
					Qnil, Qnil);

	  if (NILP (tem))
	    return Qunbound;

	  switch (type)
	    {
	    case RES_TYPE_NUMBER:
	      return make_number (atoi (SDATA (tem)));

	    case RES_TYPE_FLOAT:
	      return make_float (atof (SDATA (tem)));

	    case RES_TYPE_BOOLEAN:
	      tem = Fdowncase (tem);
	      if (!strcmp (SDATA (tem), "on")
		  || !strcmp (SDATA (tem), "true"))
		return Qt;
	      else 
		return Qnil;

	    case RES_TYPE_STRING:
	      return tem;

	    case RES_TYPE_SYMBOL:
	      /* As a special case, we map the values `true' and `on'
		 to Qt, and `false' and `off' to Qnil.  */
	      {
		Lisp_Object lower;
		lower = Fdowncase (tem);
		if (!strcmp (SDATA (lower), "on")
		    || !strcmp (SDATA (lower), "true"))
		  return Qt;
		else if (!strcmp (SDATA (lower), "off")
		      || !strcmp (SDATA (lower), "false"))
		  return Qnil;
		else
		  return Fintern (tem, Qnil);
	      }

	    default:
	      abort ();
	    }
	}
      else
	return Qunbound;
    }
  return Fcdr (tem);
}

/* Like x_get_arg, but also record the value in f->param_alist.  */

static Lisp_Object
mw32_get_and_record_arg (FRAME_PTR f, Lisp_Object alist, Lisp_Object param,
			 char *attribute, char *class, enum resource_types type)
{
  Lisp_Object value;

  value = mw32_get_arg (FRAME_MW32_DISPLAY_INFO (f), alist, param,
			attribute, class, type);
  if (! NILP (value))
    store_frame_param (f, param, value);

  return value;
}


/***********************************************************************
    		    Default Frame parameter setting.
 ***********************************************************************/

/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).
   If no value is specified for PROP, look for an X default for XPROP
   on the frame named NAME.
   If that is not found either, use the value DEFLT.  */

static Lisp_Object
mw32_default_parameter (FRAME_PTR f, Lisp_Object alist,
			Lisp_Object prop, Lisp_Object deflt,
			char *xprop, char *xclass,
			enum resource_types type)
{
  Lisp_Object tem;

  tem = mw32_get_arg (FRAME_MW32_DISPLAY_INFO (f), alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}

/* Calculate the desired size and position of this window. */

#define DEFAULT_ROWS 40
#define DEFAULT_COLS 80

static int
mw32_figure_window_size (FRAME_PTR f, Lisp_Object parms)
{
  register Lisp_Object tem0, tem1, tem2;
  long window_prompting = 0;
  struct mw32_output *pmw32o = f->output_data.mw32;
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int default_rows, default_cols;

  default_rows = (int) ((double) (dpyinfo->height)
			/ pmw32o->line_height * 0.8);
  default_cols = (int) ((double) (dpyinfo->width)
			/ FONT_WIDTH (pmw32o->font) * 0.8);
  default_rows = min (default_rows, DEFAULT_ROWS);
  default_cols = min (default_cols, DEFAULT_COLS);

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_WIDTH (f, default_cols);
  f->height = default_rows;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  pmw32o->top_pos = 0;
  pmw32o->left_pos = 0;

  tem0 = mw32_get_arg (dpyinfo, parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = mw32_get_arg (dpyinfo, parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = mw32_get_arg (dpyinfo, parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (!EQ (tem0, Qunbound))
	{
	  CHECK_NUMBER (tem0, 0);
	  f->height = XINT (tem0);
	}
      if (!EQ (tem1, Qunbound))
	{
	  CHECK_NUMBER (tem1, 0);
	  SET_FRAME_WIDTH (f, XINT (tem1));
	}
    }

  pmw32o->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (pmw32o->font)));
  pmw32o->flags_areas_extra
    = FRAME_FLAGS_AREA_WIDTH (f);
  pmw32o->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  pmw32o->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = mw32_get_arg (dpyinfo, parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = mw32_get_arg (dpyinfo, parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = mw32_get_arg (dpyinfo, parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  pmw32o->top_pos = 0;
	}
      else if (EQ (tem0, Qunbound))
	pmw32o->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0, 0);
	  pmw32o->top_pos = XINT (tem0);
	}

      if (EQ (tem1, Qminus))
	{
	  pmw32o->left_pos = 0;
	}
      else if (EQ (tem1, Qunbound))
	pmw32o->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1, 0);
	  pmw32o->left_pos = XINT (tem1);
	}
    }

  return window_prompting;
}


/***********************************************************************
    		      Create Window of Frame
 ***********************************************************************/

void mw32m_create_frame_window (struct frame *f, LPSTR title)
{
  HWND hwnd;
  RECT rc;

  /* Prevent the title bar from overlapping with the task bar at the
     top of the screen on creating frame. */
  hwnd = FindWindow (TEXT ("Shell_TrayWnd"), NULL);
  if (hwnd
      && GetWindowRect (hwnd, &rc) && rc.top <= 0
      && rc.bottom > f->output_data.mw32->top_pos)
    f->output_data.mw32->top_pos = rc.bottom;

  hwnd = CreateWindowEx (f->output_data.mw32->dwStyleEx,
			 EMACS_CLASS,
			 (LPSTR) title,
			 f->output_data.mw32->dwStyle,
			 f->output_data.mw32->left_pos,
			 f->output_data.mw32->top_pos,
			 PIXEL_WIDTH (f),
			 PIXEL_HEIGHT (f),
			 NULL,
			 NULL,
			 hinst,
			 NULL);
  POST_THREAD_INFORM_MESSAGE (main_thread_id, WM_EMACS_CREATE_FRAME_REPLY,
			      (WPARAM) hwnd, (LPARAM) 0);
  DragAcceptFiles (hwnd, TRUE);

  if (mw32_frame_window == INVALID_HANDLE_VALUE)
    mw32_frame_window = hwnd;
}

static void
mw32m_create_tip_frame_window (struct frame *f)
{
  HWND hwnd;

  hwnd = CreateWindowEx (WS_EX_TOOLWINDOW, /* hide icon on task bar */
			 EMACS_CLASS,
			 f->namebuf,
			 f->output_data.mw32->dwStyle,
			 f->output_data.mw32->left_pos,
			 f->output_data.mw32->top_pos,
			 PIXEL_WIDTH (f),
			 PIXEL_HEIGHT (f),
			 NULL, /* root window is owner */
			 NULL,
			 hinst,
			 NULL);
  
  POST_THREAD_INFORM_MESSAGE (main_thread_id,
			      WM_EMACS_CREATE_TIP_FRAME_REPLY,
			      (WPARAM) hwnd, (LPARAM) 0);
}

static void
mw32m_create_scrollbar (HWND hwnd_parent,
			LPRECT lprect, HINSTANCE hinst)
{
  HWND hwnd;

  hwnd = CreateWindowEx (0L,
			 "SCROLLBAR",
			 (LPSTR) NULL,
			 WS_CHILD | WS_VISIBLE | SBS_VERT,
			 lprect->left,
			 lprect->top,
			 lprect->right,
			 lprect->bottom,
			 hwnd_parent,
			 (HMENU) NULL,
			 hinst, (LPVOID) NULL); 
  POST_THREAD_INFORM_MESSAGE (main_thread_id,
			      WM_EMACS_CREATE_SCROLLBAR_REPLY,
			      (WPARAM) hwnd, (LPARAM) 0);
  return;
}

static void
mw32m_destroy_frame (HWND hwnd)
{
  Lisp_Object tail, frame;
  HWND hnextwnd;
  struct frame *f;

  ReplyMessage (1);
  DestroyWindow (hwnd);

  /* When mw32_frame_window is destroyed,
     set the other window handle if possible. */
  if (hwnd == mw32_frame_window)
    {
      mw32_frame_window = INVALID_HANDLE_VALUE;
      for (tail = Vframe_list; CONSP (tail);
	   tail = XCONS (tail)->cdr)
	{
	  frame = XCONS (tail)->car;
	  if (!FRAMEP (frame)) continue;
	  f = XFRAME (frame);
	  if (f->output_data.nothing == 1)
	    break;
	  hnextwnd = FRAME_MW32_WINDOW (f);
	  if ((hnextwnd != hwnd)
	      && IsWindow (hnextwnd))
	    {
	      mw32_frame_window = hnextwnd;
	      break;
	    }
	}
    }

  POST_THREAD_INFORM_MESSAGE (main_thread_id, 
			      WM_EMACS_DESTROY_FRAME_REPLY,
			      (WPARAM) 0, (LPARAM) 0);
  return;
}

static void
mw32m_track_popup_menu (HWND parent, HANDLE hmenu, LPPOINT lppos)
{
  int flag;
  UINT track_flag;
  MSG msg2;

  track_flag = TPM_LEFTALIGN;
  if (GetAsyncKeyState (VK_LBUTTON) &0x8000) track_flag |= TPM_LEFTBUTTON;
  if (GetAsyncKeyState (VK_RBUTTON) &0x8000) track_flag |= TPM_RIGHTBUTTON;

  lock_mouse_cursor_visible (TRUE);

  flag = TrackPopupMenu (hmenu,
			 track_flag,
			 lppos->x, lppos->y,
			 0,
			 parent,
			 NULL);

  lock_mouse_cursor_visible (FALSE);

  if (!flag)
    POST_THREAD_INFORM_MESSAGE (main_thread_id,
				WM_EMACS_POPUP_MENU_REPLY,
				(WPARAM) 0, (LPARAM) 0);
  else
    {
      flag = PeekMessage (&msg2, parent, WM_COMMAND, WM_COMMAND, PM_REMOVE);
      if (flag
	  && ((msg2.message == WM_COMMAND)
	      && (HIWORD (msg2.wParam) == 0)))
	{
	  POST_THREAD_INFORM_MESSAGE (main_thread_id,
				      WM_EMACS_POPUP_MENU_REPLY,
				      (WPARAM) msg2.wParam, (LPARAM) 0);
	}
      else
	{
	  POST_THREAD_INFORM_MESSAGE (main_thread_id,
				      WM_EMACS_POPUP_MENU_REPLY,
				      (WPARAM) 0, (LPARAM) 0);
	}
    }
}

#ifdef IME_CONTROL
void mw32m_ime_create_agent ()
{
  HWND hwnd;
  hwnd = CreateWindowEx (0L,
			 CONVAGENT_CLASS, "Agent",
			 0,                          /* STYLE */
			 0, 0, 0, 0,
			 NULL,
			 NULL,
			 hinst,
			 NULL);
  POST_THREAD_INFORM_MESSAGE (main_thread_id, WM_MULE_IME_CREATE_AGENT_REPLY,
			      (WPARAM) hwnd, (LPARAM) 0);
}

static void
mw32m_ime_destroy_agent (HWND hwnd)
{
  DestroyWindow (hwnd);
  POST_THREAD_INFORM_MESSAGE (main_thread_id, 
			      WM_MULE_IME_DESTROY_AGENT_REPLY,
			      (WPARAM) 0, (LPARAM) 0);
}
#endif /* IME_CONTROL */

static void
mw32m_new_focus_frame (struct mw32_display_info *dpyinfo, FRAME_PTR f)
{
  HDC hdc1, hdc2;
  FRAME_PTR fold = dpyinfo->mw32_highlight_frame;

  if (f)
    {
      hdc1 = GetDC (FRAME_MW32_WINDOW (f));
      mw32_setup_default_hdc (hdc1);
      f->output_data.mw32->message_thread_hdc = hdc1;
    }
  else
    hdc1 = INVALID_HANDLE_VALUE;

  if (fold)
    {
      hdc2 = GetDC (FRAME_MW32_WINDOW (fold));
      mw32_setup_default_hdc (hdc2);
      fold->output_data.mw32->message_thread_hdc = hdc2;
    }
  else
    hdc2 = INVALID_HANDLE_VALUE;

  W32_BLOCK_INPUT;
  mw32_new_focus_frame (dpyinfo, f);
  W32_UNBLOCK_INPUT;

  if (hdc1 != INVALID_HANDLE_VALUE)
    {
      f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
      ReleaseDC (FRAME_MW32_WINDOW (f), hdc1);
    }
  if (hdc2 != INVALID_HANDLE_VALUE)
    {
      fold->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
      ReleaseDC (FRAME_MW32_WINDOW (fold), hdc2);
    }
}

#ifndef W32_VER4
static HANDLE
mw32_ime_string_handle (HANDLE hStr)
{
  HANDLE hw32_ir_string;
  LPTSTR lpStr;
  LPTSTR lpCode;

  if (!hStr) return 0;
  lpStr = GlobalLock (hStr);
  if (!lpStr) return 0;
  hw32_ir_string = 
    GlobalAlloc (GMEM_MOVEABLE | GMEM_SHARE, 
		 strlen (lpStr) + 1);
  if (!hw32_ir_string) abort ();
  lpCode = GlobalLock (hw32_ir_string);
  strcpy (lpCode, lpStr);
  GlobalUnlock (hw32_ir_string);
  GlobalUnlock (hStr);
  return hw32_ir_string;
}
#endif

/* Multi monitor APIs */
#if WINVER < 0x0500 && !defined (__MINGW32__)
typedef struct tagMONITORINFO
{
  DWORD   cbSize;
  RECT    rcMonitor;
  RECT    rcWork;
  DWORD   dwFlags;
} MONITORINFO, *LPMONITORINFO;
#endif /* WINVER < 0x0500 && !defined (__MINGW32__) */

#if !defined (MONITOR_DEFAULTTONEAREST)
#define MONITOR_DEFAULTTONEAREST    0x00000002
#endif /* MONITOR_DEFAULTTONEAREST */

#if !defined (HMONITOR_DECLARED) && WINVER < 0x0500
DECLARE_HANDLE (HMONITOR);
#endif /* not HMONITOR_DECLARED && WINVER < 0x0500 */

typedef HMONITOR (WINAPI *MONITORFROMWINDOWPROC) (HWND, DWORD);
static MONITORFROMWINDOWPROC MonitorFromWindowProc = NULL;

typedef BOOL (WINAPI *GETMONITORINFOPROC) (HMONITOR, LPMONITORINFO);
static GETMONITORINFOPROC GetMonitorInfoProc = NULL;

static int multi_monitor_api_valid_p = 0;

static void
initialze_multi_monitor_api (void)
{
  HMODULE hUser32 = LoadLibrary ("USER32.DLL");

  if (hUser32)
    {
      MonitorFromWindowProc = (MONITORFROMWINDOWPROC)
	GetProcAddress (hUser32, "MonitorFromWindow");

      GetMonitorInfoProc = (GETMONITORINFOPROC)
	GetProcAddress (hUser32, "GetMonitorInfoA");

      if (MonitorFromWindowProc && GetMonitorInfoProc)
	multi_monitor_api_valid_p = 1;
    }
}

static void
get_working_area_size (HWND hwnd, int *width, int *height)
{
  if (multi_monitor_api_valid_p)
    {
      /* If multi monitor APIs are implemented, get size of the
	 working area on the current display. */
      MONITORINFO info;
      HMONITOR hmon = (MonitorFromWindowProc) (hwnd, MONITOR_DEFAULTTONEAREST);

      bzero (&info, sizeof (MONITORINFO));
      info.cbSize = sizeof (MONITORINFO);
      (GetMonitorInfoProc) (hmon, &info);
      *height = info.rcWork.bottom - info.rcWork.top;
      *width = info.rcWork.right - info.rcWork.left;
    }
  else
    {
      /* If multi monitor APIs are *NOT* implemented, get size of the
	 working area on the primary display. */
      *width = GetSystemMetrics (SM_CXFULLSCREEN);
      *height = GetSystemMetrics (SM_CYFULLSCREEN);
    }
}

/* MW32 Window procedure.  */

LRESULT CALLBACK
mw32_WndProc (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO (hwnd);
#ifdef IME_CONTROL
  extern int IME_event_off_count;
#endif
  extern void mw32_scroll_bar_store_event (WPARAM wParam, LPARAM lParam);
  extern void mw32_menu_bar_store_activate_event (struct frame *f);

  struct frame *f;
  WPARAM SCParam;
  LRESULT ret = 1;

  f = mw32_window_to_frame (dpyinfo, hwnd);
  if (!f && !IS_EMACS_PRIVATE_MESSAGE (msg))
    return DefWindowProc (hwnd, msg, wParam, lParam);
  
  switch (msg) {
  case WM_ERASEBKGND:
    {
      RECT rect;

      GetClientRect (FRAME_MW32_WINDOW (f), &rect);
      f->output_data.mw32->message_thread_hdc = (HDC) wParam;
      mw32_clear_area (f, rect.left, rect.top, rect.right, rect.bottom);
      f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;

      return 0;
    }

  case WM_PAINT:
    {
      if (hwnd == tip_window) /* don't redraw tooltip */
	goto dflt;
      if (f->async_visible == 0)
	{
	  f->async_visible = IsWindowVisible (hwnd);
	  f->async_iconified = IsIconic (hwnd);
	  if (f->async_visible && !f->async_iconified)
	    SET_FRAME_GARBAGED (f);
	} 
      else
	{
	  mw32_expose_frame (f);
	  return 0;
	}
    }

    goto dflt;

  case WM_SETFOCUS:

    mw32m_new_focus_frame (dpyinfo, f);

    /* This code is for switching selected-frame.  In order to
       generate no events before Emacs is set up, check whether
       selected_frame is initial terminal frame.  This is because
       input_pending should be false at the startup to show startup
       message. */
    if (SELECTED_FRAME ()
	&& (SELECTED_FRAME () != XFRAME (Vterminal_frame))
	&& (SELECTED_FRAME () != f))
      PostMessage (hwnd, WM_EMACS_ACTIVATE, 
		   (WPARAM) 0, (LPARAM) 0);

    return 0;

  case WM_KILLFOCUS:

    if (MW32_FRAME_CARET_STATE (f) > NO_CARET)
      {
	DestroyCaret ();
	MW32_FRAME_CARET_STATE (f) = NO_CARET;
      }

    if (f == FRAME_MW32_DISPLAY_INFO (f)->mw32_focus_frame)
      mw32m_new_focus_frame (dpyinfo, NULL);

    /* reset mouse face and help echo.  */
    PostMessage (hwnd, WM_EMACS_CLEAR_MOUSE_FACE, 
		 (WPARAM) 1, (LPARAM) 0);
    return 0;

  case WM_MOVE:
	
    if (!f->async_iconified && f->async_visible)
      {
	RECT rect;

	GetWindowRect (hwnd, &rect);

	f->output_data.mw32->left_pos = rect.left;
	f->output_data.mw32->top_pos = rect.top;
      }

    break;

  case WM_SIZE:

    switch (wParam)
      {
      case SIZE_MINIMIZED:
	f->async_visible = 0;
	f->async_iconified = 1;
	break;
      case SIZE_MAXIMIZED:
      case SIZE_RESTORED:
	{
	  RECT rect;

	  GetWindowRect (hwnd, &rect);

	  f->async_visible = IsWindowVisible (hwnd);
	  f->async_iconified = IsIconic (hwnd);
	  f->output_data.mw32->left_pos = rect.left;
	  f->output_data.mw32->top_pos = rect.top;
	  SET_FRAME_GARBAGED (f);
	}
      }

    break;

  case WM_NCMOUSEMOVE:
    /* reset mouse face only.  */
    PostMessage (hwnd, WM_EMACS_CLEAR_MOUSE_FACE, 
		 (WPARAM) 0, (LPARAM) 0);
    goto dflt;

  case WM_WINDOWPOSCHANGED:
    if (!f->async_iconified && f->async_visible)
      {
	int call_defprocp = 1;
	RECT rect;
	LPWINDOWPOS lppos = (LPWINDOWPOS) lParam;

	if (!(lppos->flags & SWP_NOMOVE))
	  {
	    GetWindowRect (hwnd, &rect);
	    /* position change */
	    f->output_data.mw32->left_pos = rect.left;
	    f->output_data.mw32->top_pos = rect.top;
	    call_defprocp = 0;
	  }

	GetClientRect (hwnd, &rect);
	if ((!(lppos->flags & SWP_NOSIZE)) ||
	    (lppos->flags & SWP_DRAWFRAME))
	  {
	    /* size change */
	    int width, height;
	    int rows, cols;
	    int wdiff, hdiff;

	    height = rect.bottom - rect.top;
	    width = rect.right - rect.left;

	    if ((width < (2 * (FRAME_INTERNAL_BORDER_WIDTH (f)
			       + f->output_data.mw32->flags_areas_extra)
			  + f->output_data.mw32->vertical_scroll_bar_extra))
		|| (height < (2 * FRAME_INTERNAL_BORDER_WIDTH (f))))
	      {
		/* This case MUST not happen.  Because it means that the
		   window size is smaller than required size.  Normally,
		   this case is caused by iconification on Windows.  So
		   we MUST not regard it as a normal window size changing
		   message. (by himi)  Call DefWindowProc() to generate
		   WM_SIZE message.
		*/
		goto dflt;
	      }

	    wdiff = ((width
		      - 2 * (FRAME_INTERNAL_BORDER_WIDTH (f)
			     + f->output_data.mw32->flags_areas_extra)
		      - f->output_data.mw32->vertical_scroll_bar_extra)
		     % FRAME_DEFAULT_FONT_WIDTH (f));
	    hdiff = ((height
		      - 2 * FRAME_INTERNAL_BORDER_WIDTH (f))
		     % FRAME_LINE_HEIGHT (f));
	    if ((2 * wdiff) > FRAME_DEFAULT_FONT_WIDTH (f))
	      wdiff -= FRAME_DEFAULT_FONT_WIDTH (f);
	    if ((2 * hdiff) > FRAME_LINE_HEIGHT (f))
	      hdiff -= FRAME_LINE_HEIGHT (f);

	    width -= wdiff;
	    height -= hdiff;

	    {
	      /*
		Rearrange the frame size, which should be smaller than the
		display size.
	      */
	      HDC hdc = GetDC (hwnd);
	      RECT nrect;
	      int work_area_w, work_area_h;

	      nrect.left = nrect.top = 0;
	      nrect.right = width;
	      nrect.bottom = height;
	      AdjustWindowRect (&nrect, f->output_data.mw32->dwStyle,
				FRAME_EXTERNAL_MENU_BAR (f));

	      get_working_area_size (hwnd, &work_area_w, &work_area_h);
	      if (nrect.bottom - nrect.top > work_area_h)
		{
		  hdiff += FRAME_LINE_HEIGHT (f);
		  height -= FRAME_LINE_HEIGHT (f);
		}
	      if (nrect.right - nrect.left > work_area_w)
		{
		  wdiff += FRAME_DEFAULT_FONT_WIDTH (f);
		  width -= FRAME_DEFAULT_FONT_WIDTH (f);
		}
	      ReleaseDC (hwnd, hdc);
	    }

	    rows = PIXEL_TO_CHAR_HEIGHT (f, height);
	    cols = PIXEL_TO_CHAR_WIDTH (f, width);

	    if (cols != f->width
		|| rows != f->height
		|| width != PIXEL_WIDTH (f)
		|| height != PIXEL_HEIGHT (f))
	      {
		change_frame_size (f, rows, cols, 0, 1, 0);
		SET_FRAME_GARBAGED (f);
		cancel_mouse_face (f);
	      }

	    /* To adjust window correctly,
	       we must check size of the window twice
	       (strictly speaking number of dimention),
	       thus, width and height. */
	    if ((f->output_data.mw32->frame_change_state < 2)
		&& (wdiff || hdiff))
	      {
		f->output_data.mw32->frame_change_state++;
		SetWindowPos (hwnd, NULL, 0, 0,
			      lppos->cx - wdiff,
			      lppos->cy - hdiff,
			      SWP_NOZORDER | SWP_NOMOVE);
	      }
	    else
	      f->output_data.mw32->frame_change_state = 0;

	    call_defprocp = 0;
	  }
	else
	  f->output_data.mw32->frame_change_state = 0;

	if (!call_defprocp)
	  return 0;
      }

    goto dflt;

  case WM_CLOSE:
    PostMessage (hwnd, WM_EMACS_DESTROY, wParam, lParam);
    return 0;

  case WM_CREATE:
    return 0;

  case WM_INITMENU:
    {
      if (f->output_data.mw32->menubar_handle == (HMENU) wParam)
	mw32_menu_bar_store_activate_event (f);
      return 0;
    }
    
  case WM_MENUSELECT:
    /* Direct handling of help_echo in menus.  Should be safe now
       that we generate the help_echo by placing a help event in the
       keyboard buffer.  */
    {
      HMENU menu = (HMENU) lParam;
      UINT menu_item = (UINT) LOWORD (wParam);
      UINT flags = (UINT) HIWORD (wParam);

      mw32_menu_display_help (hwnd, menu, menu_item, flags);
    }
    return 0;

  case WM_MEASUREITEM:
    if (f)
      {
	MEASUREITEMSTRUCT * pMis = (MEASUREITEMSTRUCT *) lParam;

	if (pMis->CtlType == ODT_MENU)
	  {
	    /* Work out dimensions for popup menu titles. */
	    char * title = (char *) pMis->itemData;
	    HDC hdc = GetDC (hwnd);
	    HFONT menu_font = GetCurrentObject (hdc, OBJ_FONT);
	    LOGFONT menu_logfont;
	    HFONT old_font;
	    SIZE size;

	    GetObject (menu_font, sizeof (menu_logfont), &menu_logfont);
	    menu_logfont.lfWeight = FW_BOLD;
	    menu_font = CreateFontIndirect (&menu_logfont);
	    old_font = SelectObject (hdc, menu_font);

	    pMis->itemHeight = GetSystemMetrics (SM_CYMENUSIZE);
	    if (title)
	      {
		GetTextExtentPoint32 (hdc, title, strlen (title), &size);
		pMis->itemWidth = size.cx;
		if (pMis->itemHeight < size.cy)
		  pMis->itemHeight = size.cy;
	      }
	    else
	      pMis->itemWidth = 0;

	    SelectObject (hdc, old_font);
	    DeleteObject (menu_font);
	    ReleaseDC (hwnd, hdc);
	    return TRUE;
	  }
      }
    return 0;
      
  case WM_DRAWITEM:
    if (f)
      {
	DRAWITEMSTRUCT * pDis = (DRAWITEMSTRUCT *) lParam;

	if (pDis->CtlType == ODT_MENU)
	  {
	    /* Draw popup menu title. */
	    char * title = (char *) pDis->itemData;
	    if (title)
	      {
		HDC hdc = pDis->hDC;
		HFONT menu_font = GetCurrentObject (hdc, OBJ_FONT);
		LOGFONT menu_logfont;
		HFONT old_font;

		GetObject (menu_font, sizeof (menu_logfont), &menu_logfont);
		menu_logfont.lfWeight = FW_BOLD;
		menu_font = CreateFontIndirect (&menu_logfont);
		old_font = SelectObject (hdc, menu_font);

		/* Always draw title as if not selected.  */
		ExtTextOut (hdc,
			    pDis->rcItem.left
			    + GetSystemMetrics (SM_CXMENUCHECK),
			    pDis->rcItem.top,
			    ETO_OPAQUE, &pDis->rcItem,
			    title, strlen (title), NULL);

		SelectObject (hdc, old_font);
		DeleteObject (menu_font);
	      }
	    return TRUE;
	  }
      }
    return 0;
      
  case WM_EXITMENULOOP:
    {
      if (!wParam) /* not track popup menu */
	f->output_data.mw32->disable_reconstruct_menubar = 0;
      lock_mouse_cursor_visible (FALSE);
      return 0;
    }

  case WM_VSCROLL:
    {
      mw32_scroll_bar_store_event (wParam, lParam);
      return 0;
    }

  case WM_EMACS_FLASH_WINDOW:
    {
      FlashWindow (hwnd, TRUE);
      Sleep (100);
      FlashWindow (hwnd, FALSE);
      return 0;
    }
#if defined(MEADOW) && defined(IME_CONTROL)
#ifdef W32_VER4
  case WM_IME_NOTIFY:
    if (wParam == IMN_SETOPENSTATUS)
      {
	if (!IME_event_off_count)
	  PostMessage (hwnd, WM_MULE_IME_STATUS, 0, 0);
	else
	  IME_event_off_count--;
      }
    goto dflt;

  case WM_IME_STARTCOMPOSITION:
    {
      struct window *w = XWINDOW (f->selected_window);
      mw32_set_ime_conv_window (hwnd,
				WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x),
				WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y));
      f->output_data.mw32->ime_composition_state = 1;
      POST_THREAD_INFORM_MESSAGE (main_thread_id, WM_EMACS_GET_IME_FONT_PROP,
      				  (WPARAM) f, (LPARAM) 0);
    }
  goto dflt;

  case WM_EMACS_MODIFY_IME_FONT_PROP:
    {
      /* set logfont for IME */
      /* f is not current selected frame because running thread here
	 is main thread. Use selected_frame as target frame. */

      if (selected_frame) 
	{
	  struct frame *f = XFRAME (selected_frame);
	  LOGFONT lf = f->output_data.mw32->ime_logfont;
	  lf.lfHeight = (int) wParam;
	  mw32_set_ime_font (FRAME_MW32_WINDOW (f), &lf);
	}
    }
    goto dflt;

  case WM_IME_COMPOSITION:
    {
      if (lParam & GCS_RESULTSTR)
	{
	  extern BOOL mw32_get_ime_composion_string (HWND);
	  if (mw32_get_ime_composition_string (hwnd))
	    return 0;
	  else 
	    break;
	}
      goto dflt;
    }

  case WM_IME_ENDCOMPOSITION:
    /* To erase garbage image of system caret set update_mode_lines */
    /* I can not find smart solution for results of asynchronisity of
       IME events posted onto main thread. */
    if (CARET_CURSOR_P (XWINDOW (f->selected_window)->phys_cursor_type))
      update_mode_lines++;

    f->output_data.mw32->ime_composition_state = 0;

    goto dflt;

#else /* not W32_VER4 */

  case WM_IME_REPORT:
    switch (wParam)
      {
      case IR_STRING:
	{
	  HANDLE himestr;

	  himestr = mw32_ime_string_handle ((HANDLE) lParam);
	  if (!himestr) break;

	  PostMessage (NULL, WM_MULE_IME_REPORT, 
		       (WPARAM) himestr, (LPARAM) f);
	  return 1;
	}
      case IR_OPENCONVERT:
      case IR_CLOSECONVERT:
	if (!IME_event_off_count)
	  PostMessage (hwnd, WM_MULE_IME_STATUS, 0, 0);
	else
	  IME_event_off_count--;
	return 0;
      }
#endif /* not W32_VER4 */
  case WM_MULE_IMM_SET_COMPOSITION_FONT:
    f->output_data.mw32->ime_logfont = *((LPLOGFONT) lParam);
    mw32_set_ime_font (hwnd, (LPLOGFONT) lParam);
    POST_THREAD_INFORM_MESSAGE (main_thread_id,
				WM_MULE_IMM_SET_COMPOSITION_FONT_REPLY,
				(WPARAM) 0, (LPARAM) 0);
    break;
  case WM_MULE_IMM_SET_CONVERSION_WINDOW:
    mw32_set_ime_conv_window (hwnd, (int) wParam, (int) lParam);
    break;
#endif /* not MEADOW and IME_CONTROL */

    /* Emacs private message entries. */
  case WM_EMACS_CREATE_FRAME:
    mw32m_create_frame_window ((struct frame*) wParam, (LPSTR) lParam);
    break;

  case WM_EMACS_CREATE_TIP_FRAME:
    mw32m_create_tip_frame_window ((struct frame *) wParam);
    break;

  case WM_EMACS_CREATE_SCROLLBAR:
    mw32m_create_scrollbar (hwnd, (LPRECT) wParam, (HINSTANCE) lParam);
    break;

#ifdef IME_CONTROL
  case WM_MULE_IME_CREATE_AGENT:
    mw32m_ime_create_agent ();
    break;
  case WM_MULE_IME_DESTROY_AGENT:
    mw32m_ime_destroy_agent (hwnd);
    break;
#endif
  case WM_EMACS_DESTROY_FRAME:
    mw32m_destroy_frame (hwnd);
    break;

  case WM_EMACS_POPUP_MENU:
    /* Use menubar_active to indicate that WM_INITMENU is from
       TrackPopupMenu below, and should be ignored.  */
    f = mw32_window_to_frame (dpyinfo, hwnd);
    if (f)
      f->output_data.mw32->menubar_active = 1;

    mw32m_track_popup_menu (hwnd, (HANDLE) wParam, (LPPOINT) lParam);
    dpyinfo->grabbed = 0;
    break;

  case WM_EMACS_SETCARET:
    {
      static int last_phys_cursor_height;
      static int last_cursor_width;
      static int last_cursor_height;
      static HBITMAP last_bitmap;
      struct window *w = XWINDOW (f->selected_window);
      int count;
      int caret_spec_changed = 0;

      if (last_phys_cursor_height != w->phys_cursor_height
	  || last_cursor_width != MW32_FRAME_CARET_WIDTH (f)
	  || last_cursor_height != MW32_FRAME_CARET_HEIGHT (f)
	  || last_bitmap != MW32_FRAME_CARET_BITMAP (f))
	caret_spec_changed = 1;

      if (cursor_in_echo_area
	  && FRAME_HAS_MINIBUF_P (f)
	  && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
	w = XWINDOW (echo_area_window);

      if (f != FRAME_MW32_DISPLAY_INFO (f)->mw32_focus_frame ||
	  ! w->phys_cursor_on_p)
	wParam = HIDDEN_CARET;

      if (wParam == BLOCK_CARET)
	{
	  MW32_FRAME_CARET_BLOCKED (f) = 1;
	  if (MW32_FRAME_CARET_SHOWN (f)) HideCaret (hwnd);
	  break;
	}	
      
      if (wParam == UNBLOCK_CARET)
	{
	  MW32_FRAME_CARET_BLOCKED (f) = 0;
	  if (MW32_FRAME_CARET_SHOWN (f)) ShowCaret (hwnd);
	  break;
	}	
      
      if (wParam == SHOWN_CARET)
	{
	  int caret_height =
	    MW32_FRAME_CARET_HEIGHT (f) * w->phys_cursor_height / 4;
	  int caret_xpos =
	    WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
	  int caret_ypos =
	    WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
	    + w->phys_cursor_height - caret_height;

	  if (caret_spec_changed)
	    {
	      if (MW32_FRAME_CARET_STATE (f) > NO_CARET)
		{
		  DestroyCaret ();
		  MW32_FRAME_CARET_STATE (f) = NO_CARET;
		}	
	    }
	  
	  if (MW32_FRAME_CARET_STATE (f) == NO_CARET)
	    {
	      CreateCaret (hwnd, 
			   MW32_FRAME_CARET_BITMAP (f),
			   MW32_FRAME_CARET_WIDTH (f),
			   caret_height);
	      MW32_FRAME_CARET_STATE (f) = HIDDEN_CARET;
	      MW32_FRAME_CARET_BLOCKED (f) = 0;
	      last_phys_cursor_height = w->phys_cursor_height;
	      last_cursor_width = MW32_FRAME_CARET_WIDTH (f);
	      last_cursor_height = MW32_FRAME_CARET_HEIGHT (f);
	      last_bitmap = MW32_FRAME_CARET_BITMAP (f);
	    }

	  SetCaretPos (caret_xpos, caret_ypos);

	  if (MW32_FRAME_CARET_SHOWN (f)) break;
	  if (!MW32_FRAME_CARET_BLOCKED (f)) ShowCaret (hwnd);

	  MW32_FRAME_CARET_STATE (f) = SHOWN_CARET;
	}
      else
	{
	  if (! MW32_FRAME_CARET_SHOWN (f)) break;
	  if (!MW32_FRAME_CARET_BLOCKED (f)) HideCaret (hwnd);
	  MW32_FRAME_CARET_STATE (f) = HIDDEN_CARET;
	}
    }
    break;
  case WM_EMACS_SETFOREGROUND:
    {
      DWORD timeout;

      SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, &timeout, 0);
      SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, (LPVOID) 0, 0);

      SetForegroundWindow (hwnd);

      SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT,
			   0, (LPVOID) timeout, 0);

    }
    break;

    /* end of Emacs private message entries. */

  default:
#if defined(MEADOW) && defined(IME_CONTROL) && defined(W32_VER4)
    {
      extern LRESULT CALLBACK
	conversion_agent_wndproc (HWND hwnd, UINT message,
				  WPARAM wparam, LPARAM lparam);
      if (MESSAGE_IMM_COM_P (msg))
	return conversion_agent_wndproc (hwnd, msg, wParam, lParam);
    }
#endif

  dflt:
    return DefWindowProc (hwnd, msg, wParam, lParam);
  }

  return 1;
}

static BOOL
mw32_init_app (HINSTANCE hinst)
{
    WNDCLASS wc;

    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = (WNDPROC) mw32_WndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hinst;
    wc.hIcon = LoadIcon (hinst, EMACS_CLASS);
    wc.hCursor = LoadCursor (NULL, IDC_ARROW);
    wc.hbrBackground = GetStockObject (WHITE_BRUSH);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = EMACS_CLASS;

    return (RegisterClass (&wc));
}

/* Create a tooltip window. Unlike my_create_window, we do not do this
   indirectly via the Window thread, as we do not need to process Window
   messages for the tooltip.  Creating tooltips indirectly also creates
   deadlocks when tooltips are created for menu items.  */
static Window
mw32_create_tip_window (struct frame *f)
{
  MSG msg;
  Window hwnd;
  
  SEND_MSGTHREAD_INFORM_MESSAGE (WM_EMACS_CREATE_TIP_FRAME,
				 (WPARAM) f, (LPARAM) NULL);
  WAIT_REPLY_MESSAGE (&msg, WM_EMACS_CREATE_TIP_FRAME_REPLY);

  hwnd = (HWND) msg.wParam;

  if (hwnd == NULL)
    error ("Unable to create window");

  return hwnd;
}

/* Create and set up the X window for frame F.  */

static void
mw32_window (struct frame *f, int minibuffer_only)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  HWND hwnd;
  char *name;
  MSG msg;

  if (STRINGP (f->name))
    name = (char*) SDATA (f->name);
  else
    name = dpyinfo->mw32_id_name;

  SEND_MSGTHREAD_INFORM_MESSAGE (WM_EMACS_CREATE_FRAME,
				 (WPARAM) f, (LPARAM) name);
  WAIT_REPLY_MESSAGE (&msg, WM_EMACS_CREATE_FRAME_REPLY);

  hwnd = (HWND) msg.wParam;
  FRAME_MW32_WINDOW (f) = hwnd;

  f->output_data.mw32->mainthread_to_frame_handle = CreateEvent (0, TRUE, TRUE, NULL);

  validate_x_resource_name ();

  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    initialize_frame_menubar (f);

  mw32_calc_absolute_position (f);

  /* w32_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    mw32_set_name (f, name, explicit);
  }

  if (FRAME_MW32_WINDOW (f) == 0)
    error ("Unable to create window");
}


/* Handler for signals raised during x_create_frame and
   x_create_top_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is ``official'', nothing to do.  */
  if (!CONSP (Vframe_list) || !EQ (XCAR (Vframe_list), frame))
    {
#if GLYPH_DEBUG
      struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
#endif
      
      mw32_free_frame_resources (f);

      /* Check that reference counts are indeed correct.  */
      xassert (dpyinfo->reference_count == dpyinfo_refcount);
      xassert (dpyinfo->image_cache->refcount == image_cache_refcount);
      return Qt;
    }
  
  return Qnil;
}


DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
  "Make a new X window, which is called a \"frame\" in Emacs terms.\n\
Returns an Emacs frame object.\n\
ALIST is an alist of frame parameters.\n\
If the parameters specify that the frame should not have a minibuffer,\n\
and do not specify a specific minibuffer window to use,\n\
then `default-minibuffer-frame' must be a frame whose minibuffer can\n\
be shared by the new frame.\n\
\n\
This function is an internal primitive--use `make-frame' instead.")
  (parms)
     Lisp_Object parms;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  int width, height;
  int count = BINDING_STACK_SIZE ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  struct mw32_display_info *dpyinfo = NULL;
  Lisp_Object parent, display;
  struct kboard *kb;

  display = Qnil;
  check_mw32 ();

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  /* Get mw32 display info.  Curretly, arg is a dummy argument.  */
  dpyinfo = GET_MW32_DISPLAY_INFO (arg);
#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = mw32_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

#if 0
  /* See if parent window is specified.  */
  parent = mw32_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent, 0);
#else
  parent = Qnil;
#endif

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = mw32_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
		      RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (1);

  XSETFRAME (frame, f);

  /* Note that MW32 supports scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;
  /* Scroll bar locates on the right side.  */
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_right;

  f->output_method = output_mw32;
  f->output_data.mw32 = (struct mw32_output *) xmalloc (sizeof (struct mw32_output));
  bzero (f->output_data.mw32, sizeof (struct mw32_output));
  f->output_data.mw32->icon_bitmap = -1;
  f->output_data.mw32->fontset = -1;
  f->output_data.mw32->scroll_bar_foreground_pixel = -1;
  f->output_data.mw32->scroll_bar_background_pixel = -1;
  /* all handles must be set to INVALID_HANLE_VALUE.  */
  f->output_data.mw32->menubar_handle = INVALID_HANDLE_VALUE;
  f->output_data.mw32->hdc = INVALID_HANDLE_VALUE;
  f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
  {
    LOGFONT lf;

    mw32_initialize_default_logfont (&lf);
    f->output_data.mw32->ime_logfont = lf;
  }

  record_unwind_protect (unwind_create_frame, frame);

  f->icon_name
    = mw32_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
		    RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

  FRAME_MW32_DISPLAY_INFO (f) = dpyinfo;
#if GLYPH_DEBUG
  image_cache_refcount = FRAME_MW32_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

#if 0
  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;
    struct gcpro gcpro1;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    f->output_data.mw32->foreground_pixel = -1;
    f->output_data.mw32->background_pixel = -1;
    f->output_data.mw32->cursor_pixel = -1;
    f->output_data.mw32->cursor_foreground_pixel = -1;
    f->output_data.mw32->border_pixel = -1;
    f->output_data.mw32->mouse_pixel = -1;
    
    black = build_string ("black");
    GCPRO1 (black);
    f->output_data.mw32->foreground_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.mw32->background_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.mw32->cursor_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.mw32->cursor_foreground_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.mw32->border_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.mw32->mouse_pixel
      = mw32_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    UNGCPRO;
  }
#endif

  /* Specify the parent under which to make this X window.  */

  if (!NILP (parent))
    {
      f->output_data.mw32->explicit_parent = 1;
    }
  else
    {
      f->output_data.mw32->parent_desc = FRAME_MW32_DISPLAY_INFO (f)->root_window;
      f->output_data.mw32->explicit_parent = 0;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->mw32_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font, fontset;

    font = mw32_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);
    if (! STRINGP (font))
      font = build_string ("default");

    fontset = Fquery_fontset (font, Qnil);
    if (STRINGP (fontset))
      font = mw32_new_fontset (f, SDATA (fontset));
    else
      font = mw32_new_font (f, SDATA (font));

    if (!STRINGP (font))
      {
	/* Initial font cannot be created,
	   we should display detail informations
	   as far as possible. */
	if (SYMBOLP (font))
	  error ("Cannot select initial font:%s",
		 XSYMBOL (font)->name);
	else
	  error ("Cannot select initial font");
      }

    mw32_default_parameter (f, parms, Qfont, font, 
			    "font", "Font", RES_TYPE_STRING);
#ifdef IME_CONTROL
    mw32_default_parameter (f, parms, Qime_font, Qnil,
			    "ime-font", "IME-Font", RES_TYPE_STRING);
#endif
  }

  mw32_default_parameter (f, parms, Qborder_width, make_number (2),
			  "borderWidth", "BorderWidth", RES_TYPE_NUMBER);
  
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = mw32_get_arg (dpyinfo, parms, Qinternal_border_width,
			    "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  mw32_default_parameter (f, parms, Qinternal_border_width, make_number (1),
			  "internalBorderWidth", "internalBorderWidth",
			  RES_TYPE_NUMBER);
  mw32_default_parameter (f, parms, Qvertical_scroll_bars, Qleft,
			  "verticalScrollBars", "ScrollBars",
			  RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  mw32_default_parameter (f, parms, Qforeground_color, build_string ("black"),
			  "foreground", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qbackground_color, build_string ("white"),
			  "background", "Background", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qmouse_color, build_string ("black"),
			  "pointerColor", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qcursor_color, build_string ("black"),
			  "cursorColor", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qborder_color, build_string ("black"),
			  "borderColor", "BorderColor", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qscreen_gamma, Qnil,
			  "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  mw32_default_parameter (f, parms, Qline_spacing, Qnil,
			  "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);

#if 0
  mw32_default_parameter (f, parms, Qscroll_bar_foreground,
			  build_string ("LightBlue"),
			  "scrollBarForeground", "ScrollBarForeground",
			  RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qscroll_bar_background,
			  build_string ("black"),
			  "scrollBarForeground", "ScrollBarForeground",
			  RES_TYPE_STRING);
#endif

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);
  
  mw32_default_parameter (f, parms, Qmenu_bar_lines, make_number (1),
			  "menuBar", "MenuBar", RES_TYPE_NUMBER);
  mw32_default_parameter (f, parms, Qtool_bar_lines, make_number (1),
			  "toolBar", "ToolBar", RES_TYPE_NUMBER);
  mw32_default_parameter (f, parms, Qbuffer_predicate, Qnil,
			  "bufferPredicate", "BufferPredicate",
			  RES_TYPE_SYMBOL);
  mw32_default_parameter (f, parms, Qtitle, Qnil,
			  "title", "Title", RES_TYPE_STRING);

  f->output_data.mw32->dwStyle = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN;
  f->output_data.mw32->dwStyleEx = WS_EX_CLIENTEDGE;

  f->output_data.mw32->parent_desc = FRAME_MW32_DISPLAY_INFO (f)->root_window;

  /* Add the tool-bar height to the initial frame height so that the
     user gets a text display area of the size he specified with -g or
     via .Xdefaults.  Later changes of the tool-bar height don't
     change the frame size.  This is done so that users can create
     tall Emacs frames without having to guess how tall the tool-bar
     will get.  */
  if (FRAME_TOOL_BAR_LINES (f))
    {
      int margin, relief, bar_height;
      
      relief = (tool_bar_button_relief > 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);

      if (INTEGERP (Vtool_bar_button_margin)
	  && XINT (Vtool_bar_button_margin) > 0)
	margin = XFASTINT (Vtool_bar_button_margin);
      else if (CONSP (Vtool_bar_button_margin)
	       && INTEGERP (XCDR (Vtool_bar_button_margin))
	       && XINT (XCDR (Vtool_bar_button_margin)) > 0)
	margin = XFASTINT (XCDR (Vtool_bar_button_margin));
      else
	margin = 0;
	  
      bar_height = DEFAULT_TOOL_BAR_IMAGE_HEIGHT + 2 * margin + 2 * relief;
      f->height += (bar_height + CANON_Y_UNIT (f) - 1) / CANON_Y_UNIT (f);
    }

  /* Compute the size of the X window.  */
  window_prompting = mw32_figure_window_size (f, parms);

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.mw32->win_gravity = SouthEastGravity;
      else
	f->output_data.mw32->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.mw32->win_gravity = SouthWestGravity;
      else
	f->output_data.mw32->win_gravity = NorthWestGravity;
    }

  f->output_data.mw32->size_hint_flags = window_prompting;

  tem = mw32_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Create the X widget or window.  */
  mw32_window (f, minibuffer_only);

  /* Now consider the frame official.  */
  FRAME_MW32_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  mw32_default_parameter (f, parms, Qicon_type, Qnil,
			  "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);

  mw32_default_parameter (f, parms, Qauto_raise, Qnil,
			  "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  mw32_default_parameter (f, parms, Qauto_lower, Qnil,
			  "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  mw32_default_parameter (f, parms, Qcursor_type, Qbox,
			  "cursorType", "CursorType", RES_TYPE_SYMBOL);
  mw32_default_parameter (f, parms, Qcursor_height, make_number (4),
			  "cursorHeight", "CursorHeight", RES_TYPE_NUMBER);
  mw32_default_parameter (f, parms, Qscroll_bar_width, Qnil,
			  "scrollBarWidth", "ScrollBarWidth",
			  RES_TYPE_NUMBER);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height.  */
  width = f->width;
  height = f->height;
  
  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0, 0);

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.  If we
     don't do this, the `menu' face for instance won't have the right
     colors, and the menu bar won't appear in the specified colors for
     new frames.  */
  call1 (Qface_set_after_frame_default, frame);

#ifdef USE_X_TOOLKIT
  /* Create the menu bar.  */
  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    {
      /* If this signals an error, we haven't set size hints for the
	 frame and we didn't make it visible.  */
      initialize_frame_menubar (f);

      /* This is a no-op, except under Motif where it arranges the
	 main window for the widgets on it.  */
      lw_set_main_areas (f->output_data.x->column_widget,
			 f->output_data.x->menubar_widget,
			 f->output_data.x->edit_widget);
    }
#endif /* USE_X_TOOLKIT */

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (! f->output_data.mw32->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = mw32_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
				 RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;

      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else
	{
	  if (! NILP (visibility))
	    {
	      x_make_frame_visible (f);
	      mw32_new_focus_frame (dpyinfo, f);
	    }
	  SetForegroundWindow (FRAME_MW32_WINDOW (f));
	}
    }

  UNGCPRO;

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;
  
  return unbind_to (count, frame);
}


/***********************************************************************
    		           Lisp Functions.
 ***********************************************************************/

/* FRAME is used only to get a handle on the X display.  We don't pass the
   display info directly because we're called from frame.c, which doesn't
   know about that structure.  */

Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->mw32_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->mw32_focus_frame);
  return xfocus;
}


/* In certain situations, when the window manager follows a
   click-to-focus policy, there seems to be no way around calling
   XSetInputFocus to give another frame the input focus .

   In an ideal world, XSetInputFocus should generally be avoided so
   that applications don't interfere with the window manager's focus
   policy.  But I think it's okay to use when it's clearly done
   following a user-command.  */

DEFUN ("x-focus-frame", Fx_focus_frame, Sx_focus_frame, 1, 1, 0,
  "Set the input focus to FRAME.\n\
FRAME nil means use the selected frame.")
  (frame)
     Lisp_Object frame;
{
  struct frame *f = check_mw32_frame (frame);

  SetFocus (FRAME_MW32_WINDOW (f));
  
  return Qnil;
}


/***********************************************************************
           Color (later we move this part ot mw32color.c)
 ***********************************************************************/

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
  "Internal function called by `color-defined-p', which see.")
  (color, frame)
     Lisp_Object color, frame;
{
  COLORREF pixel;
  FRAME_PTR f = check_mw32_frame (frame);

  CHECK_STRING (color, 1);

  if (mw32_defined_color (f, SDATA (color), &pixel, 0))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
  "Internal function called by `color-values', which see.")
  (color, frame)
     Lisp_Object color, frame;
{
  XColor pixel;
  FRAME_PTR f = check_mw32_frame (frame);

  CHECK_STRING (color, 1);

  if (x_defined_color (f, SDATA (color), &pixel, 0))
    {
      Lisp_Object rgb[3];

      rgb[0] = make_number (pixel.red);
      rgb[1] = make_number (pixel.green);
      rgb[2] = make_number (pixel.blue);
      return Flist (3, rgb);
    }
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
  "Internal function called by `display-color-p', which see.")
  (display)
     Lisp_Object display;
{
  struct mw32_display_info *dpyinfo = check_mw32_display_info (display);

  if (dpyinfo->n_planes <= 2)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
  0, 1, 0,
  "Return t if the X display supports shades of gray.\n\
Note that color displays do support shades of gray.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct mw32_display_info *dpyinfo = check_mw32_display_info (display);

  if (dpyinfo->n_planes <= 1)
    return Qnil;

  return Qt;
}


/***********************************************************************
  external interface part mainly for frame.c
                                (Why are these functions here?;_;)
 ***********************************************************************/

int
x_pixel_width (f)
     register struct frame *f;
{
  return PIXEL_WIDTH (f);
}

int
x_pixel_height (f)
     register struct frame *f;
{
  return PIXEL_HEIGHT (f);
}

int
x_char_width (f)
     register struct frame *f;
{
  return FRAME_DEFAULT_FONT_WIDTH (f);
}

int
x_char_height (f)
     register struct frame *f;
{
  return FRAME_LINE_HEIGHT (f);
}

int
x_screen_planes (f)
     register struct frame *f;
{
  return FRAME_MW32_DISPLAY_INFO (f)->n_planes;
}

void
x_sync (FRAME_PTR f)
{
  /* do nothing. */
}



/***********************************************************************
              Open/Close connection (This style is from xterm.c). 
 ***********************************************************************/

/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */

static struct mw32_display_info *
mw32_display_info_for_name (Lisp_Object name)
{
  Lisp_Object names;
  struct mw32_display_info *dpyinfo;

  CHECK_STRING (name, 0);

  if (! EQ (Vwindow_system, intern ("w32")))
    error ("Not using MW32");

  if (mw32_display_list)
    return mw32_display_list;

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = mw32_term_init (name, (char *)0,
			    (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      fatal ("Cannot initialize MW32 window system %s.",
	     SDATA (name));
    }

  mw32_open = 1;

  XSETFASTINT (Vwindow_system_version, 1);

  return dpyinfo;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0, "Open a connection to an X server.\n\
DISPLAY is the name of the display to connect to.\n\
Optional second arg XRM-STRING is a string of resources in xrdb format.\n\
If the optional third arg MUST-SUCCEED is non-nil,\n\
terminate Emacs if we can't open the connection.")
  (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned char *xrm_option;
  struct mw32_display_info *dpyinfo;

  CHECK_STRING (display, 0);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string, 1);

  if (! EQ (Vwindow_system, intern ("w32")))
    error ("Not using MW32");

  mw32_init_app (hinst);
  Vmw32_color_map = Fmw32_default_color_map ();

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) SDATA (xrm_string);
  else
    xrm_option = (unsigned char *) 0;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = mw32_term_init (display, xrm_option,
			    (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      fatal ("Cannot initialize MW32 window system %s.",
	     SDATA (display));
    }

  mw32_open = 1;

  XSETFASTINT (Vwindow_system_version, 1);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
   "Close the connection to DISPLAY's X server.\n\
For DISPLAY, specify either a frame or a display name (a string).\n\
If DISPLAY is nil, that stands for the selected frame's display.")
  (display)
  Lisp_Object display;
{
  struct mw32_display_info *dpyinfo = check_mw32_display_info (display);
  int i;

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

#if 0
  /* Free the fonts in the font table.  */
  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name)
      {
	if (dpyinfo->font_table[i].name != dpyinfo->font_table[i].full_name)
	  xfree (dpyinfo->font_table[i].full_name);
	xfree (dpyinfo->font_table[i].name);
	XFreeFont (dpyinfo->display, dpyinfo->font_table[i].font);
      }

  x_destroy_all_bitmaps (dpyinfo);
  XSetCloseDownMode (dpyinfo->display, DestroyAll);
#endif

  mw32_delete_display (dpyinfo);

  POST_THREAD_INFORM_MESSAGE (msg_thread_id, 
			      WM_EMACS_CLOSE_CONNECTION, 
			      (WPARAM) 0, (LPARAM) 0);
  mw32_open = 0;

  return Qnil;
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
  "Return the list of display names that Emacs has connections to.")
  ()
{
  return Qnil;
}

/* Wait for responses to all X commands issued so far for frame F.  */


/***********************************************************************
			    Image types
 ***********************************************************************/

/* Value is the number of elements of vector VECTOR.  */

#define DIM(VECTOR)	(sizeof (VECTOR) / sizeof *(VECTOR))

/* List of supported image types.  Use define_image_type to add new
   types.  Use lookup_image_type to find a type for a given symbol.  */

static struct image_type *image_types;

/* The symbol `image' which is the car of the lists used to represent
   images in Lisp.  */

extern Lisp_Object Qimage;

/* The symbol `xbm' which is used as the type symbol for XBM images.  */

Lisp_Object Qxbm;

/* Keywords.  */

extern Lisp_Object QCwidth, QCheight, QCforeground, QCbackground, QCfile;
extern Lisp_Object QCdata;
Lisp_Object QCtype, QCascent, QCmargin, QCrelief;
Lisp_Object QCconversion, QCcolor_symbols, QCheuristic_mask;
Lisp_Object QCindex, QCmatrix, QCcolor_adjustment, QCmask;
Lisp_Object Qpostscript;

/* Other symbols.  */

Lisp_Object Qlaplace, Qemboss, Qedge_detection, Qheuristic;

/* Time in seconds after which images should be removed from the cache
   if not displayed.  */

Lisp_Object Vimage_cache_eviction_delay;

/* Function prototypes.  */

static void define_image_type P_ ((struct image_type *type));
static struct image_type *lookup_image_type P_ ((Lisp_Object symbol));
static void image_error P_ ((char *format, Lisp_Object, Lisp_Object));
static void x_laplace P_ ((struct frame *, struct image *));
static void x_emboss P_ ((struct frame *, struct image *));
static int x_build_heuristic_mask P_ ((struct frame *, struct image *,
				       Lisp_Object));


/* Define a new image type from TYPE.  This adds a copy of TYPE to
   image_types and adds the symbol *TYPE->type to Vimage_types.  */

static void
define_image_type (type)
     struct image_type *type;
{
  /* Make a copy of TYPE to avoid a bus error in a dumped Emacs.
     The initialized data segment is read-only.  */
  struct image_type *p = (struct image_type *) xmalloc (sizeof *p);
  bcopy (type, p, sizeof *p);
  p->next = image_types;
  image_types = p;
  Vimage_types = Fcons (*p->type, Vimage_types);
}


/* Look up image type SYMBOL, and return a pointer to its image_type
   structure.  Value is null if SYMBOL is not a known image type.  */

static INLINE struct image_type *
lookup_image_type (symbol)
     Lisp_Object symbol;
{
  struct image_type *type;

  for (type = image_types; type; type = type->next)
    if (EQ (symbol, *type->type))
      break;

  return type;
}


/* Value is non-zero if OBJECT is a valid Lisp image specification.  A
   valid image specification is a list whose car is the symbol
   `image', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported image type.  The rest of the property list depends on the
   image type.  */

int
valid_image_p (object)
     Lisp_Object object;
{
  int valid_p = 0;
  
  if (CONSP (object) && EQ (XCAR (object), Qimage))
    {
      Lisp_Object tem;

      for (tem = XCDR (object); CONSP (tem); tem = XCDR (tem))
	if (EQ (XCAR (tem), QCtype))
	  {
	    tem = XCDR (tem);
	    if (CONSP (tem) && SYMBOLP (XCAR (tem)))
	      {
		struct image_type *type;
		type = lookup_image_type (XCAR (tem));
		if (type)
		  valid_p = type->valid_p (object);
	      }
	    
	    break;
	  }
    }

  return valid_p;
}


/* Log error message with format string FORMAT and argument ARG.
   Signaling an error, e.g. when an image cannot be loaded, is not a
   good idea because this would interrupt redisplay, and the error
   message display would lead to another redisplay.  This function
   therefore simply displays a message.  */

static void
image_error (format, arg1, arg2)
     char *format;
     Lisp_Object arg1, arg2;
{
  add_to_log (format, arg1, arg2);
}



/***********************************************************************
			 Image specifications
 ***********************************************************************/

enum image_value_type
{
  IMAGE_DONT_CHECK_VALUE_TYPE,
  IMAGE_STRING_VALUE,
  IMAGE_STRING_OR_NIL_VALUE,
  IMAGE_SYMBOL_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE,
  IMAGE_ASCENT_VALUE,
  IMAGE_INTEGER_VALUE,
  IMAGE_FUNCTION_VALUE,
  IMAGE_NUMBER_VALUE,
  IMAGE_BOOL_VALUE
};

/* Structure used when parsing image specifications.  */

struct image_keyword
{
  /* Name of keyword.  */
  char *name;

  /* The type of value allowed.  */
  enum image_value_type type;

  /* Non-zero means key must be present.  */
  int mandatory_p;

  /* Used to recognize duplicate keywords in a property list.  */
  int count;

  /* The value that was found.  */
  Lisp_Object value;
};


static int parse_image_spec P_ ((Lisp_Object, struct image_keyword *,
				 int, Lisp_Object));
static Lisp_Object image_spec_value P_ ((Lisp_Object, Lisp_Object, int *));


/* Parse image spec SPEC according to KEYWORDS.  A valid image spec
   has the format (image KEYWORD VALUE ...).  One of the keyword/
   value pairs must be `:type TYPE'.  KEYWORDS is a vector of
   image_keywords structures of size NKEYWORDS describing other
   allowed keyword/value pairs.  Value is non-zero if SPEC is valid.  */

static int
parse_image_spec (spec, keywords, nkeywords, type)
     Lisp_Object spec;
     struct image_keyword *keywords;
     int nkeywords;
     Lisp_Object type;
{
  int i;
  Lisp_Object plist;

  if (!CONSP (spec) || !EQ (XCAR (spec), Qimage))
    return 0;

  plist = XCDR (spec);
  while (CONSP (plist))
    {
      Lisp_Object key, value;

      /* First element of a pair must be a symbol.  */
      key = XCAR (plist);
      plist = XCDR (plist);
      if (!SYMBOLP (key))
	return 0;

      /* There must follow a value.  */
      if (!CONSP (plist))
	return 0;
      value = XCAR (plist);
      plist = XCDR (plist);

      /* Find key in KEYWORDS.  Error if not found.  */
      for (i = 0; i < nkeywords; ++i)
	if (strcmp (keywords[i].name, XSYMBOL (key)->name->data) == 0)
	  break;

      if (i == nkeywords)
	continue;

      /* Record that we recognized the keyword.  If a keywords
	 was found more than once, it's an error.  */
      keywords[i].value = value;
      ++keywords[i].count;
      
      if (keywords[i].count > 1)
	return 0;

      /* Check type of value against allowed type.  */
      switch (keywords[i].type)
	{
	case IMAGE_STRING_VALUE:
	  if (!STRINGP (value))
	    return 0;
	  break;

	case IMAGE_STRING_OR_NIL_VALUE:
	  if (!STRINGP (value) && !NILP (value))
	    return 0;
	  break;

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) <= 0)
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR:
	  if (INTEGERP (value) && XINT (value) >= 0)
	    break;
	  if (CONSP (value)
	      && INTEGERP (XCAR (value)) && INTEGERP (XCDR (value))
	      && XINT (XCAR (value)) >= 0 && XINT (XCDR (value)) >= 0)
	    break;
	  return 0;

	case IMAGE_ASCENT_VALUE:
	  if (SYMBOLP (value) && EQ (value, Qcenter))
	    break;
	  else if (INTEGERP (value)
		   && XINT (value) >= 0
		   && XINT (value) <= 100)
	    break;
	  return 0;
	      
	case IMAGE_NON_NEGATIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) < 0)
	    return 0;
	  break;

	case IMAGE_DONT_CHECK_VALUE_TYPE:
	  break;

	case IMAGE_FUNCTION_VALUE:
	  value = indirect_function (value);
	  if (SUBRP (value) 
	      || COMPILEDP (value)
	      || (CONSP (value) && EQ (XCAR (value), Qlambda)))
	    break;
	  return 0;

	case IMAGE_NUMBER_VALUE:
	  if (!INTEGERP (value) && !FLOATP (value))
	    return 0;
	  break;

	case IMAGE_INTEGER_VALUE:
	  if (!INTEGERP (value))
	    return 0;
	  break;

	case IMAGE_BOOL_VALUE:
	  if (!NILP (value) && !EQ (value, Qt))
	    return 0;
	  break;

	default:
	  abort ();
	  break;
	}

      if (EQ (key, QCtype) && !EQ (type, value))
	return 0;
    }

  /* Check that all mandatory fields are present.  */
  for (i = 0; i < nkeywords; ++i)
    if (keywords[i].mandatory_p && keywords[i].count == 0)
      return 0;

  return NILP (plist);
}


/* Return the value of KEY in image specification SPEC.  Value is nil
   if KEY is not present in SPEC.  if FOUND is not null, set *FOUND
   to 1 if KEY was found in SPEC, set it to 0 otherwise.  */

static Lisp_Object
image_spec_value (spec, key, found)
     Lisp_Object spec, key;
     int *found;
{
  Lisp_Object tail;
  
  xassert (valid_image_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
	{
	  if (found)
	    *found = 1;
	  return XCAR (XCDR (tail));
	}
    }
  
  if (found)
    *found = 0;
  return Qnil;
}
     

DEFUN ("image-size", Fimage_size, Simage_size, 1, 3, 0,
  "Return the size of image SPEC as pair (WIDTH . HEIGHT).\n\
PIXELS non-nil means return the size in pixels, otherwise return the\n\
size in canonical character units.\n\
FRAME is the frame on which the image will be displayed.  FRAME nil\n\
or omitted means use the selected frame.")
  (spec, pixels, frame)
     Lisp_Object spec, pixels, frame;
{
  Lisp_Object size;

  size = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_mw32_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      int width = img->width + 2 * img->hmargin;
      int height = img->height + 2 * img->vmargin;
  
      if (NILP (pixels))
	size = Fcons (make_float ((double) width / CANON_X_UNIT (f)),
		      make_float ((double) height / CANON_Y_UNIT (f)));
      else
	size = Fcons (make_number (width), make_number (height));
    }
  else
    error ("Invalid image specification");

  return size;
}


DEFUN ("image-mask-p", Fimage_mask_p, Simage_mask_p, 1, 2, 0,
  "Return t if image SPEC has a mask bitmap.\n\
FRAME is the frame on which the image will be displayed.  FRAME nil\n\
or omitted means use the selected frame.")
  (spec, frame)
     Lisp_Object spec, frame;
{
  Lisp_Object mask;

  mask = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_mw32_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      if (MW32_IMAGE_HAS_MASK_P (img->mw32_img))
	mask = Qt;
    }
  else
    error ("Invalid image specification");

  return mask;
}



/***********************************************************************
		 Image type independent image structures
 ***********************************************************************/

static struct image *make_image P_ ((Lisp_Object spec, unsigned hash));
static void free_image P_ ((struct frame *f, struct image *img));


/* Allocate and return a new image structure for image specification
   SPEC.  SPEC has a hash value of HASH.  */

static struct image *
make_image (spec, hash)
     Lisp_Object spec;
     unsigned hash;
{
  struct image *img = (struct image *) xmalloc (sizeof *img);
  
  xassert (valid_image_p (spec));
  bzero (img, sizeof *img);
  img->type = lookup_image_type (image_spec_value (spec, QCtype, NULL));
  xassert (img->type != NULL);
  img->spec = spec;
  img->data.lisp_val = Qnil;
  img->ascent = DEFAULT_IMAGE_ASCENT;
  img->hash = hash;
  return img;
}


/* Free image IMG which was used on frame F, including its resources.  */

static void
free_image (f, img)
     struct frame *f;
     struct image *img;
{
  if (img)
    {
      struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);

      /* Remove IMG from the hash table of its cache.  */
      if (img->prev)
	img->prev->next = img->next;
      else
	c->buckets[img->hash % IMAGE_CACHE_BUCKETS_SIZE] = img->next;

      if (img->next)
	img->next->prev = img->prev;

      c->images[img->id] = NULL;

      /* Free resources, then free IMG.  */
      (img->type->free) (f, img);
      xfree (img);
    }
}


/* Prepare image IMG for display on frame F.  Must be called before
   drawing an image.  */

void
prepare_image_for_display (f, img)
     struct frame *f;
     struct image *img;
{
  EMACS_TIME t;

  /* We're about to display IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (t);
  img->timestamp = EMACS_SECS (t);

  /* If IMG doesn't have a pixmap yet, load it now, using the image
     type dependent loader function.  */
  if (!MW32_IMAGE_VALID_P (img->mw32_img) && !img->load_failed_p)
    img->load_failed_p = img->type->load (f, img) == 0;
}
     

/* Value is the number of pixels for the ascent of image IMG when
   drawn in face FACE.  */

int
image_ascent (img, face)
     struct image *img;
     struct face *face;
{
  int height = img->height + img->vmargin;
  int ascent;

  if (img->ascent == CENTERED_IMAGE_ASCENT)
    {
      if (face->font)
	/* This expression is arranged so that if the image can't be
	   exactly centered, it will be moved slightly up.  This is
	   because a typical font is `top-heavy' (due to the presence
	   uppercase letters), so the image placement should err towards
	   being top-heavy too.  It also just generally looks better.  */
	ascent = (height + face->font->ascent - face->font->descent + 1) / 2;
      else
	ascent = height / 2;
    }
  else
    ascent = (int) (height * img->ascent / 100.0);

  return ascent;
}



/***********************************************************************
		  Image management functions for MW32.
 ***********************************************************************/

/* Free resources of image IMG which is used on frame F.  */
static void
mw32_destruct_image (struct frame *f, struct image *img)
{
  /* MW32 implementation uses DIB section, so have only to free the
     allocated memory.  */
  if (img->mw32_img.pbmpinfo) xfree (img->mw32_img.pbmpinfo);
  if (img->mw32_img.pbmpinfo) xfree (img->mw32_img.pbmpdata);
  if (img->mw32_img.pbmpmask) xfree (img->mw32_img.pbmpmask);
}

/* Allocate color COLOR_NAME for image IMG on frame F.  If color
   cannot be allocated, use DFLT.  Add a newly allocated color to
   IMG->colors, so that it can be freed again.  Value is the pixel
   color.  */

static void
mw32_alloc_image_color (f, img, color_name, dflt)
{
  /* right now, do nothing. */
}

static void
mw32_create_image_simple (struct frame *f, struct image *img, int size)
{
  img->mw32_img.pbmpinfo = NULL;
  img->mw32_img.size = size;
  img->mw32_img.pbmpdata = (unsigned char*) xmalloc (sizeof (unsigned char)
						     * size);
}


/***********************************************************************
			     Image Cache
 ***********************************************************************/

static void cache_image P_ ((struct frame *f, struct image *img));
static void postprocess_image P_ ((struct frame *, struct image *));


/* Return a new, initialized image cache that is allocated from the
   heap.  Call free_image_cache to free an image cache.  */

struct image_cache *
make_image_cache ()
{
  struct image_cache *c = (struct image_cache *) xmalloc (sizeof *c);
  int size;
  
  bzero (c, sizeof *c);
  c->size = 50;
  c->images = (struct image **) xmalloc (c->size * sizeof *c->images);
  size = IMAGE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
  c->buckets = (struct image **) xmalloc (size);
  bzero (c->buckets, size);
  return c;
}


/* Free image cache of frame F.  Be aware that X frames share images
   caches.  */

void
free_image_cache (struct frame *f)
{
  struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);
  if (c)
    {
      int i;

      /* Cache should not be referenced by any frame when freed.  */
      xassert (c->refcount == 0);
      
      for (i = 0; i < c->used; ++i)
	free_image (f, c->images[i]);
      xfree (c->images);
      xfree (c->buckets);
      xfree (c);
      FRAME_MW32_IMAGE_CACHE (f) = NULL;
    }
}


/* Clear image cache of frame F.  FORCE_P non-zero means free all
   images.  FORCE_P zero means clear only images that haven't been
   displayed for some time.  Should be called from time to time to
   reduce the number of loaded images.  If image-eviction-seconds is
   non-nil, this frees images in the cache which weren't displayed for
   at least that many seconds.  */

void
clear_image_cache (struct frame *f, int force_p)
{
  struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);

  if (c && INTEGERP (Vimage_cache_eviction_delay))
    {
      EMACS_TIME t;
      unsigned long old;
      int i, nfreed;

      EMACS_GET_TIME (t);
      old = EMACS_SECS (t) - XFASTINT (Vimage_cache_eviction_delay);

      /* Block input so that we won't be interrupted by a SIGIO
	 while being in an inconsistent state.  */
      BLOCK_INPUT;
      
      for (i = nfreed = 0; i < c->used; ++i)
	{
	  struct image *img = c->images[i];
	  if (img != NULL
	      && (force_p || img->timestamp < old))
	    {
	      free_image (f, img);
	      ++nfreed;
	    }
	}

      /* We may be clearing the image cache because, for example,
	 Emacs was iconified for a longer period of time.  In that
	 case, current matrices may still contain references to
	 images freed above.  So, clear these matrices.  */
      if (nfreed)
	{
	  Lisp_Object tail, frame;
	  
	  FOR_EACH_FRAME (tail, frame)
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_MW32_P (f)
		  && FRAME_MW32_IMAGE_CACHE (f) == c)
		clear_current_matrices (f);
	    }

	  ++windows_or_buffers_changed;
	}

      UNBLOCK_INPUT;
    }
}


DEFUN ("clear-image-cache", Fclear_image_cache, Sclear_image_cache,
       0, 1, 0,
  "Clear the image cache of FRAME.\n\
FRAME nil or omitted means use the selected frame.\n\
FRAME t means clear the image caches of all frames.")
  (frame)
     Lisp_Object frame;
{
  if (EQ (frame, Qt))
    {
      Lisp_Object tail;
      
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_MW32_P (XFRAME (frame)))
	  clear_image_cache (XFRAME (frame), 1);
    }
  else
    clear_image_cache (check_mw32_frame (frame), 1);

  return Qnil;
}


/* Compute masks and transform image IMG on frame F, as specified
   by the image's specification,  */

static void
postprocess_image (struct frame *f, struct image *img)
{
  /* TODO: MW32 implementation.  */
  /* Manipulation of the image's mask.  */
  if (MW32_IMAGE_VALID_P (img->mw32_img))
    {
      Lisp_Object conversion, spec;
      Lisp_Object mask;

      spec = img->spec;
      
#if 0
      /* `:heuristic-mask t'
	 `:mask heuristic'
	 means build a mask heuristically.
	 `:heuristic-mask (R G B)'
	 `:mask (heuristic (R G B))'
	 means build a mask from color (R G B) in the
	 image.
	 `:mask nil'
	 means remove a mask, if any.  */

      /* These are implemented at imagemagick_load_image_data () */
#endif
	  
      /* Should we apply an image transformation algorithm?  */
      conversion = image_spec_value (spec, QCconversion, NULL);
      if (EQ (conversion, Qdisabled))
	mw32_disable_image (f, img);
      else if (EQ (conversion, Qlaplace))
	mw32_laplace (f, img);
      else if (EQ (conversion, Qemboss))
	mw32_emboss (f, img);
      else if (CONSP (conversion)
	       && EQ (XCAR (conversion), Qedge_detection))
	{
	  Lisp_Object tem;
	  tem = XCDR (conversion);
	  if (CONSP (tem))
	    mw32_edge_detection (f, img,
				 Fplist_get (tem, QCmatrix),
				 Fplist_get (tem, QCcolor_adjustment));
	}
    }
}


/* Return the id of image with Lisp specification SPEC on frame F.
   SPEC must be a valid Lisp image specification (see valid_image_p).  */

int
lookup_image (f, spec)
     struct frame *f;
     Lisp_Object spec;
{
  struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);
  struct image *img;
  int i;
  unsigned hash;
  struct gcpro gcpro1;
  EMACS_TIME now;

  /* F must be a window-system frame, and SPEC must be a valid image
     specification.  */
  xassert (FRAME_WINDOW_P (f));
  xassert (valid_image_p (spec));
  
  GCPRO1 (spec);

  /* Look up SPEC in the hash table of the image cache.  */
  hash = sxhash (spec, 0);
  i = hash % IMAGE_CACHE_BUCKETS_SIZE;

  for (img = c->buckets[i]; img; img = img->next)
    if (img->hash == hash && !NILP (Fequal (img->spec, spec)))
      break;

  /* If not found, create a new image and cache it.  */
  if (img == NULL)
    {
      extern Lisp_Object Qpostscript;
      
      BLOCK_INPUT;
      img = make_image (spec, hash);
      cache_image (f, img);
      img->load_failed_p = img->type->load (f, img) == 0;

      /* If we can't load the image, and we don't have a width and
	 height, use some arbitrary width and height so that we can
	 draw a rectangle for it.  */
      if (img->load_failed_p)
	{
	  Lisp_Object value;

	  value = image_spec_value (spec, QCwidth, NULL);
	  img->width = (INTEGERP (value)
			? XFASTINT (value) : DEFAULT_IMAGE_WIDTH);
	  value = image_spec_value (spec, QCheight, NULL);
	  img->height = (INTEGERP (value)
			 ? XFASTINT (value) : DEFAULT_IMAGE_HEIGHT);
	}
      else
	{
	  /* Handle image type independent image attributes
	     `:ascent ASCENT', `:margin MARGIN', `:relief RELIEF'.  */
	  Lisp_Object ascent, margin, relief;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (INTEGERP (ascent))
	    img->ascent = XFASTINT (ascent);
	  else if (EQ (ascent, Qcenter))
	    img->ascent = CENTERED_IMAGE_ASCENT;
	  
	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (INTEGERP (margin) && XINT (margin) >= 0)
	    img->vmargin = img->hmargin = XFASTINT (margin);
	  else if (CONSP (margin) && INTEGERP (XCAR (margin))
		   && INTEGERP (XCDR (margin)))
	    {
	      if (XINT (XCAR (margin)) > 0)
		img->hmargin = XFASTINT (XCAR (margin));
	      if (XINT (XCDR (margin)) > 0)
		img->vmargin = XFASTINT (XCDR (margin));
	    }
	  
	  relief = image_spec_value (spec, QCrelief, NULL);
	  if (INTEGERP (relief))
	    {
	      img->relief = XINT (relief);
	      img->hmargin += abs (img->relief);
	      img->vmargin += abs (img->relief);
	    }

	  /* Do image transformations and compute masks, unless we
	     don't have the image yet.  */
	  if (!EQ (*img->type->type, Qpostscript))
	    postprocess_image (f, img);
	}

      UNBLOCK_INPUT;
      xassert (!interrupt_input_blocked);
    }

  /* We're using IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (now);
  img->timestamp = EMACS_SECS (now);
  
  UNGCPRO;
  
  /* Value is the image id.  */
  return img->id;
}


/* Cache image IMG in the image cache of frame F.  */

static void
cache_image (f, img)
     struct frame *f;
     struct image *img;
{
  struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);
  int i;

  /* Find a free slot in c->images.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i] == NULL)
      break;

  /* If no free slot found, maybe enlarge c->images.  */
  if (i == c->used && c->used == c->size)
    {
      c->size *= 2;
      c->images = (struct image **) xrealloc (c->images,
					      c->size * sizeof *c->images);
    }

  /* Add IMG to c->images, and assign IMG an id.  */
  c->images[i] = img;
  img->id = i;
  if (i == c->used)
    ++c->used;

  /* Add IMG to the cache's hash table.  */
  i = img->hash % IMAGE_CACHE_BUCKETS_SIZE;
  img->next = c->buckets[i];
  if (img->next)
    img->next->prev = img;
  img->prev = NULL;
  c->buckets[i] = img;
}


/* Call FN on every image in the image cache of frame F.  Used to mark
   Lisp Objects in the image cache.  */

void
forall_images_in_image_cache (struct frame *f,
			      void (*fn) P_ ((struct image *img)))
{
  if (FRAME_LIVE_P (f) && FRAME_MW32_P (f))
    {
      struct image_cache *c = FRAME_MW32_IMAGE_CACHE (f);
      if (c)
	{
	  int i;
	  for (i = 0; i < c->used; ++i)
	    if (c->images[i])
	      fn (c->images[i]);
	}
    }
}



/***********************************************************************
	             image file handling.
 ***********************************************************************/

static char *mw32_map_image_file P_ ((char *, int *));

/* Find image file FILE.  Look in data-directory, then
   x-bitmap-file-path.  Value is the full name of the file found, or
   nil if not found.  */

static Lisp_Object
mw32_find_image_file (Lisp_Object file)
{
  Lisp_Object file_found, search_path;
  struct gcpro gcpro1, gcpro2;
  int fd;

  file_found = Qnil;
  search_path = Fcons (Vdata_directory, Vmw32_bitmap_file_path);
  GCPRO2 (file_found, search_path);

  /* Try to find FILE in data-directory, then mw32-bitmap-file-path.  */
  fd = openp (search_path, file, "", &file_found, 0);
  
  if (fd == -1)
    file_found = Qnil;
  else
    close (fd);

  UNGCPRO;
  return file_found;
}


/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  *SIZE is set to the size of the file.  */

static char *
mw32_map_image_file (char *file, int *size)
{
}

static char *
slurp_file (file, size)
     char *file;
     int *size;
{
  FILE *fp = NULL;
  char *buf = NULL;
  struct stat st;

  if (stat (file, &st) == 0
      && (fp = fopen (file, "r")) != NULL
      && (buf = (char *) xmalloc (st.st_size),
	  fread (buf, 1, st.st_size, fp) == st.st_size))
    {
      *size = st.st_size;
      fclose (fp);
    }
  else
    {
      if (fp)
	fclose (fp);
      if (buf)
	{
	  xfree (buf);
	  buf = NULL;
	}
    }
  
  return buf;
}



/***********************************************************************
	             BMP format handling (MW32 special feature)
 ***********************************************************************/

Lisp_Object Qbmp;
static int bmp_image_p (Lisp_Object object);
static int bmp_load (struct frame *f, struct image *img);

enum bmp_keyword_index
{
  BMP_TYPE,
  BMP_FILE,
  BMP_WIDTH,
  BMP_HEIGHT,
  BMP_DATA,
  BMP_FOREGROUND,
  BMP_BACKGROUND,
  BMP_ASCENT,
  BMP_MARGIN,
  BMP_RELIEF,
  BMP_ALGORITHM,
  BMP_HEURISTIC_MASK,
  BMP_MASK,
  BMP_LAST
};

static struct image_keyword bmp_format[BMP_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":width",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":height",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,   0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0}
};

static struct image_type bmp_type =
{
  &Qbmp,
  bmp_image_p,
  bmp_load,
  mw32_destruct_image,
  NULL
};

typedef struct bmp_info
{
  char type[2];      /* 0-1: 2-octets */
  int size;          /* 2-5: DWORD */
  int bmpoffset;     /* 10-13: DWORD */
  int infosize;      /* 14-17: DWORD */
  int width;         /* 18-21: LONG */
  int height;        /* 22-25: LONG */
  int plains;        /* 26-27: WORD */
  int bitcount;      /* 28-29: WORD */
  int compression;   /* 30-33: DWORD */
  int imagesize;     /* 34-37: DWORD */
  int xpermeter;     /* 38-41: LONG */
  int ypermeter;     /* 42-45: LONG */
  int color_num;     /* 46-49: DWORD */
  int m_color_num;   /* 50-53: DWORD */
}bmp_info;
#define BMP_FILE_HEADER_SIZE 14
#define BMP_HEADER_MIN_SIZE 54

#define BMP_READ_WORD(p, n) (p[n] | (p[n + 1] << 8))
#define BMP_READ_LONG(p, n) (p[n] | (p[n + 1] << 8) | (p[n + 2] << 16) | \
                             (((signed int) p[n + 3]) << 24))
#define BMP_READ_DWORD(p, n) (p[n] | (p[n + 1] << 8) | (p[n + 2] << 16) | (p[n + 3] << 24))

static int
bmp_get_bitmap_width_bytes (int width, int bitcount)
{
  /*
    DIB requires DWORD alignment.  The following expression
    is equivalent to these operations.

    bytes = (bi.width * bi.bitcount + 7) / 8;
    align = (sizeof(DWORD) - bytes % sizeof(DWORD)) % sizeof(DWORD);
    bi.imagesize = (bytes + align) * height;
  */
  return ((width * bitcount + 31) & ~31) / 8;
}

static bmp_info
bmp_read_bitmap_info (unsigned char *p)
{
  bmp_info bi;

  bi.type[0] = p[0]; bi.type[1] = p[1];
  bi.size = BMP_READ_DWORD (p, 2);
  bi.bmpoffset = BMP_READ_DWORD (p, 10);
  bi.infosize = BMP_READ_DWORD (p, 14);
  bi.width = BMP_READ_LONG (p, 18);
  bi.height = BMP_READ_LONG (p, 22);
  bi.plains = BMP_READ_WORD (p, 26);
  bi.bitcount = BMP_READ_WORD (p, 28);
  bi.compression = BMP_READ_DWORD (p, 30);
  bi.imagesize = BMP_READ_DWORD (p, 34);
  bi.xpermeter = BMP_READ_LONG (p, 38);
  bi.ypermeter = BMP_READ_LONG (p, 42);
  bi.color_num = BMP_READ_DWORD (p, 46);
  bi.m_color_num = BMP_READ_DWORD (p, 50);

  if (bi.color_num == 0)
    {
      if (bi.bitcount < 24)
	bi.color_num = 1 << bi.bitcount;
    }
  if (bi.imagesize == 0)
    {
      int bytes, height;

      height = bi.height;
      if (height < 0) height = - height;

      bytes = bmp_get_bitmap_width_bytes (bi.width, bi.bitcount);
      bi.imagesize = bytes * height;
    }

  return bi;
}

static RGBQUAD*
bmp_read_color_table (unsigned char *p, bmp_info *pbi)
{
  int i;
  RGBQUAD *prgb;

  if (pbi->color_num == 0) return NULL;
  prgb = (RGBQUAD*) xmalloc (sizeof (RGBQUAD) * pbi->color_num);
  p += BMP_FILE_HEADER_SIZE + pbi->infosize;
  for (i = 0; i < pbi->color_num; i++)
    {
      prgb[i].rgbBlue = *p++;
      prgb[i].rgbGreen = *p++;
      prgb[i].rgbRed = *p++;
      prgb[i].rgbReserved = 0;
      p++;
    }

  return prgb;
}

static int
bmp_setup_bitmapinfo (struct image *img,
		      RGBQUAD *prgb,
		      bmp_info *pbi)
{
  int n, size;
  BITMAPINFO *pbmpi;

  n = pbi->color_num;
  size = sizeof (BITMAPINFOHEADER) + n * sizeof (RGBQUAD);
  pbmpi = (BITMAPINFO*) xmalloc (size);
  memset (pbmpi, 0, size);

  pbmpi->bmiHeader.biSize = sizeof (BITMAPINFOHEADER);
  pbmpi->bmiHeader.biWidth = pbi->width;
  pbmpi->bmiHeader.biHeight = pbi->height;
  pbmpi->bmiHeader.biPlanes = pbi->plains;
  pbmpi->bmiHeader.biBitCount = pbi->bitcount;
  pbmpi->bmiHeader.biCompression = pbi->compression;
  pbmpi->bmiHeader.biSizeImage = pbi->imagesize;
  pbmpi->bmiHeader.biXPelsPerMeter = pbi->xpermeter;
  pbmpi->bmiHeader.biYPelsPerMeter = pbi->ypermeter;
  pbmpi->bmiHeader.biClrUsed = n;
  pbmpi->bmiHeader.biClrImportant = pbi->m_color_num;

  img->mw32_img.pbmpinfo = pbmpi;
  if (prgb)
    memcpy (pbmpi->bmiColors, prgb, n * sizeof (RGBQUAD));

  return 1;
}

static int
bmp_valid_data_p (unsigned char *p, int size)
{
  bmp_info bi;

  if (size < BMP_HEADER_MIN_SIZE) return 0;
  bi = bmp_read_bitmap_info (p);

  /* Many BMP file does not set valid size to bi.size.
     .. */
#if 0
  if (size < (bi.size
	      + bi.color_num * sizeof (RGBQUAD)
	      + BMP_FILE_HEADER_SIZE
	      + bi.imagesize))
    return 0;
#endif

  if (bi.type[0] != 'B') return 0;
  if (bi.type[1] != 'M') return 0;

  if (bi.compression != BI_RGB)
    return 0;

  if ((bi.bitcount != 1)
      && (bi.bitcount != 4)
      && (bi.bitcount != 8)
      && (bi.bitcount != 16)
      && (bi.bitcount != 24)
      && (bi.bitcount != 32))
    return 0;

  return 1;
}

static
int bmp_valid_object_p (Lisp_Object object,
			struct image_keyword *kw)
{
  /* TODO */
  if (!STRINGP (object))
    return 0;

  return bmp_valid_data_p (SDATA (object),
			   SBYTES (object));
}

static
int bmp_image_p (Lisp_Object object)
{
  struct image_keyword kw[BMP_LAST];
  bcopy (bmp_format, kw, sizeof kw);
  if (!parse_image_spec (object, kw, BMP_LAST, Qbmp))
    return 0;

  xassert (EQ (kw[BMP_TYPE].value, Qbmp));

  if (kw[BMP_FILE].count)
    {
      if (kw[BMP_WIDTH].count || kw[BMP_HEIGHT].count || kw[BMP_DATA].count)
	return 0;
    }
  else if (kw[BMP_DATA].count && bmp_valid_object_p (kw[BMP_DATA].value, kw))
    {
      /* in memory BMP data */
      if (kw[BMP_WIDTH].count || kw[BMP_HEIGHT].count || kw[BMP_FILE].count)
	return 0;
    }

  return 1;
}

static int
bmp_load_image_data (struct frame *f,
		     struct image *img,
		     char *p, char *end)
{
  bmp_info bi;
  RGBQUAD *prgb;

  bi = bmp_read_bitmap_info (p);
  img->width = bi.width;
  /* We should change image ascent by the sign of bi.height?
     Right now, ignore it...  */
  img->height = abs (bi.height);
    
  prgb = bmp_read_color_table (p, &bi);
  if (!prgb && (bi.color_num > 0)) return 0;
  mw32_create_image_simple (f, img, bi.imagesize);
  bmp_setup_bitmapinfo (img, prgb, &bi);
  img->mw32_img.size = bi.imagesize;
  bcopy (p + bi.bmpoffset, img->mw32_img.pbmpdata, bi.imagesize);
  if (prgb) xfree (prgb);

  return 1;
}

static int
bmp_load (struct frame *f, struct image *img)
{
  int success_p = 0;
  Lisp_Object file_name;

  xassert (bmp_image_p (img->spec));

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      char *contents;
      int size;
      struct gcpro gcpro1;
      
      file = mw32_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (SDATA (file), &size);
      if (contents == NULL)
	{
	  image_error ("Error loading BMP image `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      if (!bmp_valid_data_p (contents, size))
	{
	  xfree (contents);
	  image_error ("Error bad BMP file `%s'", img->spec, Qnil);
	  return 0;
	}

      success_p = bmp_load_image_data (f, img, contents, contents + size);
      xfree (contents);
      UNGCPRO;
    }
  else
    {
      struct image_keyword fmt[BMP_LAST];
      Lisp_Object data;
      int depth;
      int parsed_p;

      /* See if data looks like an in-memory BMP data.  */
      data = image_spec_value (img->spec, QCdata, NULL);

      /* Parse the image specification.  */
      bcopy (bmp_format, fmt, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, BMP_LAST, Qbmp);
      xassert (parsed_p);

      if (!bmp_valid_object_p (data, fmt))
	{
	  image_error ("Error bad BMP image data `%s'", data, Qnil);
	  return 0;
	}

      success_p = bmp_load_image_data (f, img, SDATA (data),
				       (SDATA (data)
					+ SBYTES (data)));
    }

  return success_p;
}



/***********************************************************************
	             ImageMagick Interface.
 ***********************************************************************/
#ifndef WIN32
#define WIN32
#endif
#ifdef WIN32_LEAN_AND_MEAN
#undef WIN32_LEAN_AND_MEAN
#endif
#include <magick/api.h>
#include <magick/nt-base.h>
#include <magick/blob-private.h>

#define IMAGE_MAGICK_DLL "CORE_RL_magick_.dll"
#define IMAGE_MAGICK_REQUIRE_VERSION 0x0624

Lisp_Object Qxbm, Qpbm, Qxpm, Qjpeg, Qtiff, Qgif, Qpng;

static int define_imagemagick_type (void);
static int resolve_imagemagick_api (void);
static int imagemagick_image_p (Lisp_Object);
static int imagemagick_load (struct frame *, struct image *);
static int imagemagick_get_bits_per_pixel (Image *);

/* ImageMagick function pointers.  */

typedef const char* (*pget_magick_version_t) (unsigned long *);
typedef void (*pinitialize_magick_t) (const char *);
typedef Image *(*pread_image_t) (const ImageInfo *, ExceptionInfo *);
typedef Image *(*pblob_to_image_t) (const ImageInfo *, const void *,
				    const size_t, ExceptionInfo *);
typedef ImageInfo *(*pclone_image_info_t) (const ImageInfo *);
typedef void (*pdestroy_image_t) (Image *);
typedef void (*pdestroy_image_info_t) (ImageInfo *);
typedef PixelPacket* (*pget_image_pixels_t) (Image *, const long, const long,
					     const unsigned long,
					     const unsigned long);
typedef void (*pget_exception_info_t) (ExceptionInfo *);
typedef Image *(*pping_blob_t) (const ImageInfo *, const void *,
				const size_t, ExceptionInfo *);
typedef Image *(*pping_image_t) (const ImageInfo *, ExceptionInfo *);
typedef Image *(*pdestroy_image_list_t) (Image *);
typedef const MagickInfo *(*pget_magick_info_t) (const char *,
						 ExceptionInfo *);
typedef char **(*pget_magick_list_t) (const char *, unsigned long *,
				      ExceptionInfo *);
typedef IndexPacket *(*pget_indexes_t) (const Image *);
typedef MagickBooleanType (*ptransform_rgb_image_t) (Image *,
						     const ColorspaceType);
typedef const PixelPacket *(*pacquire_image_pixels_t) (const Image *,
						       const long, const long,
						       const unsigned long,
						       const unsigned long,
						       ExceptionInfo *);
typedef MagickBooleanType (*pis_monochrome_image_t) (const Image *,
						     ExceptionInfo *);
typedef MagickBooleanType (*pset_image_type_t) (Image *, const ImageType);
typedef Image *(*pconstitute_image_t) (const unsigned long,
				       const unsigned long,
				       const char *, const StorageType,
				       const void *, ExceptionInfo *);
typedef Image *(*pallocate_image_t) (const ImageInfo *);
typedef PixelPacket* (*pset_image_pixels_t) (Image *, const long, const long,
					     const unsigned long,
					     const unsigned long);
typedef MagickBooleanType (*psync_image_pixels_t) (Image *);
typedef MagickBooleanType (*popen_blob_t) (const ImageInfo *, Image *,
					   const BlobMode, ExceptionInfo *);
typedef char *(*pread_blob_string_t) (Image *, char *);
typedef void (*pclose_blob_t) (Image *);
typedef MagickBooleanType (*pallocate_image_colormap_t) (Image *,
							 const unsigned long);
typedef MagickBooleanType (*pquery_color_database_t) (const char *,
						      PixelPacket *,
						      ExceptionInfo *);
typedef MagickInfo *(*pregister_magick_info_t) (MagickInfo *);
typedef MagickInfo *(*pset_magick_info_t) (const char *);
typedef void *(*prelinquish_magick_memory_t) (void *);
typedef void *(*pacquire_string_t) (const char *);
typedef const char *(*pget_image_magick_t) (const unsigned char *,
					    const size_t);

static HINSTANCE hmagick = NULL;
static ExceptionInfo magick_exception;

static pget_magick_version_t pget_magick_version;
static pinitialize_magick_t pinitialize_magick;
static pread_image_t pread_image;
static pblob_to_image_t pblob_to_image;
static pclone_image_info_t pclone_image_info;
static pdestroy_image_t pdestroy_image;
static pdestroy_image_info_t pdestroy_image_info;
static pget_image_pixels_t pget_image_pixels;
static pget_exception_info_t pget_exception_info;
static pping_blob_t pping_blob;
static pping_image_t pping_image;
static pdestroy_image_list_t pdestroy_image_list;
static pget_magick_info_t pget_magick_info;
static pget_magick_list_t pget_magick_list;
static pget_indexes_t pget_indexes;
static ptransform_rgb_image_t ptransform_rgb_image;
static pacquire_image_pixels_t pacquire_image_pixels;
static pis_monochrome_image_t pis_monochrome_image;
static pset_image_type_t pset_image_type;
static pconstitute_image_t pconstitute_image;
static pallocate_image_t pallocate_image;
static pset_image_pixels_t pset_image_pixels;
static psync_image_pixels_t psync_image_pixels;
static popen_blob_t popen_blob;
static pread_blob_string_t pread_blob_string;
static pclose_blob_t pclose_blob;
static pallocate_image_colormap_t pallocate_image_colormap;
static pquery_color_database_t pquery_color_database;
static pregister_magick_info_t pregister_magick_info;
static pset_magick_info_t pset_magick_info;
static prelinquish_magick_memory_t prelinquish_magick_memory;
static pacquire_string_t pacquire_string;
static pget_image_magick_t pget_image_magick;

/* ImageMagick function wrappers.  */

static const char *
magick_get_magick_version (unsigned long *version)
{
  if (hmagick == NULL || pget_magick_version == NULL)
    return NULL;
  return (*pget_magick_version) (version);
}

static void
magick_initialize_magick (const char *path)
{
  if (hmagick == NULL || pinitialize_magick == NULL)
    return;
  (*pinitialize_magick) (path);
}

static Image *
magick_read_image (const ImageInfo *image_info, ExceptionInfo *exception)
{
  if (hmagick == NULL || pread_image == NULL)
    return NULL;
  return (*pread_image) (image_info, exception);
}

static Image *
magick_blob_to_image (const ImageInfo *image_info,
		      const void *blob,
		      const size_t length,
		      ExceptionInfo *exception)
{
  if (hmagick == NULL || pblob_to_image == NULL)
    return NULL;
  return (*pblob_to_image) (image_info, blob, length, exception);
}

static ImageInfo *
magick_clone_image_info (const ImageInfo *image_info)
{
  if (hmagick == NULL || pclone_image_info == NULL)
    return NULL;
  return (*pclone_image_info) (image_info);
}

static void
magick_destroy_image (Image *image)
{
  if (hmagick == NULL || pdestroy_image == NULL)
    return;
  (*pdestroy_image) (image);
}

static void
magick_destroy_image_info (ImageInfo *image_info)
{
  if (hmagick == NULL || pdestroy_image_info == NULL)
    return;
  (*pdestroy_image_info) (image_info);
}

static PixelPacket *
magick_get_image_pixels (Image *image,
			 const long x,
			 const long y,
			 const unsigned long columns,
			 const unsigned long rows)
{
  if (hmagick == NULL || pget_image_pixels == NULL)
    return NULL;
  return (*pget_image_pixels) (image, x, y, columns, rows);
}

static void
magick_get_exception_info (ExceptionInfo *exception)
{
  if (hmagick == NULL || pget_exception_info == NULL)
    return;
  (*pget_exception_info) (exception);
}

static Image *
magick_ping_blob (const ImageInfo *image_info,
		  const void *blob,
		  const size_t length,
		  ExceptionInfo *exception)
{
  if (hmagick == NULL || pping_blob == NULL)
    return NULL;
  return (*pping_blob) (image_info, blob, length, exception);
}

static Image *
magick_ping_image (const ImageInfo *image_info,
		   ExceptionInfo *exception)
{
  if (hmagick == NULL || pping_image == NULL)
    return NULL;
  return (*pping_image) (image_info, exception);
}

static Image *
magick_destroy_image_list (Image *images)
{
  if (hmagick == NULL || pdestroy_image_list == NULL)
    return NULL;
  return (*pdestroy_image_list) (images);
}

static const MagickInfo *
magick_get_magick_info (const char *name,
			ExceptionInfo *exception)
{
  if (hmagick == NULL || pget_magick_info == NULL)
    return NULL;
  return (*pget_magick_info) (name, exception);
}

static char **
magick_get_magick_list (const char *pattern,
			unsigned long *number_formats,
			ExceptionInfo *exception)
{
  if (hmagick == NULL || pget_magick_list == NULL)
    return NULL;
  return (*pget_magick_list) (pattern, number_formats, exception);
}

static IndexPacket *
magick_get_indexes (const Image *image)
{
  if (hmagick == NULL || pget_indexes == NULL)
    return NULL;
  return (*pget_indexes) (image);
}

static MagickBooleanType
magick_transform_rgb_image (Image *image, const ColorspaceType colorspace)
{
  if (hmagick == NULL || ptransform_rgb_image == NULL)
    return MagickFalse;
  return (*ptransform_rgb_image) (image, colorspace);
}

static const PixelPacket *
magick_acquire_image_pixels (const Image *image,
			     const long x, const long y,
			     const unsigned long columns,
			     const unsigned long rows,
			     ExceptionInfo *exception)
{
  if (hmagick == NULL || pacquire_image_pixels == NULL)
    return NULL;
  return (*pacquire_image_pixels) (image, x, y, columns, rows, exception);
}

static MagickBooleanType
magick_is_monochrome_image (const Image *image, ExceptionInfo *exception)
{
  if (hmagick == NULL || pis_monochrome_image == NULL)
    return MagickFalse;
  return (*pis_monochrome_image) (image, exception);
}

static MagickBooleanType
magick_set_image_type (Image *image, const ImageType image_type)
{
  if (hmagick == NULL || pset_image_type == NULL)
    return MagickFalse;
  return (*pset_image_type) (image, image_type);
}

static Image *
magick_constitute_image (const unsigned long width, const unsigned long height,
			 const char *map, const StorageType type,
			 const void *pixels, ExceptionInfo *exception)
{
  if (hmagick == NULL || pconstitute_image == NULL)
    return NULL;
  return (*pconstitute_image) (width, height, map, type, pixels, exception);
}

static Image *
magick_allocate_image (const ImageInfo *image_info)
{
  if (hmagick == NULL || pallocate_image == NULL)
    return NULL;
  return (*pallocate_image) (image_info);
}


static PixelPacket *
magick_set_image_piexels (Image *image, const long x, const long y,
			  const unsigned long columns,
			  const unsigned long rows)
{
  if (hmagick == NULL || pset_image_pixels == NULL)
    return NULL;
  return (*pset_image_pixels) (image, x, y, columns, rows);
}

static MagickBooleanType
magick_sync_image_pixels (Image *image)
{
  if (hmagick == NULL || psync_image_pixels == NULL)
    return MagickFalse;
  return (*psync_image_pixels) (image);
}

static MagickBooleanType
magick_open_blob (const ImageInfo *image_info, Image *image,
		  const BlobMode mode, ExceptionInfo *exception)
{
  if (hmagick == NULL || popen_blob == NULL)
    return MagickFalse;
  return (*popen_blob) (image_info, image, mode, exception);
}

static char *
magick_read_blob_string (Image *image, char *string)
{
  if (hmagick == NULL || pread_blob_string == NULL)
    return NULL;
  return (*pread_blob_string) (image, string);
}

static void
magick_close_blob (Image *image)
{
  if (hmagick == NULL || pclose_blob == NULL)
    return;
  (*pclose_blob) (image);
}

static MagickBooleanType
magick_allocate_image_colormap (Image *image, const unsigned long colors)
{
  if (hmagick == NULL || pallocate_image_colormap == NULL)
    return MagickFalse;
  return (*pallocate_image_colormap) (image, colors);
}

static MagickBooleanType
magick_query_color_database (const char *name, PixelPacket *color,
			     ExceptionInfo *exception)
{
  if (hmagick == NULL || pquery_color_database == NULL)
    return MagickFalse;
  return (*pquery_color_database) (name, color, exception);
}

static MagickInfo *
magick_register_magick_info (MagickInfo *magick_info)
{
  if (hmagick == NULL || pregister_magick_info == NULL)
    return NULL;
  return (*pregister_magick_info) (magick_info);
}

static MagickInfo *
magick_set_magick_info (const char *name)
{
  if (hmagick == NULL || pset_magick_info == NULL)
    return NULL;
  return (*pset_magick_info) (name);
}

static void *
magick_relinquish_magick_memory (void *memory)
{
  if (hmagick == NULL || prelinquish_magick_memory == NULL)
    return NULL;
  return (*prelinquish_magick_memory) (memory);
}

static void *
magick_acquire_string (const char *source)
{
  if (hmagick == NULL || pacquire_string == NULL)
    return NULL;
  return (*pacquire_string) (source);
}

static const char *
magick_get_image_magick (const unsigned char *magick, const size_t length)
{
  if (hmagick == NULL || pget_image_magick == NULL)
    return NULL;
  return (*pget_image_magick) (magick, length);
}

static int
resolve_imagemagick_api (void)
{
  unsigned long version;

  hmagick = LoadLibrary (IMAGE_MAGICK_DLL);
  if (hmagick == NULL)
    return 0;

#define MAGICK_RESOLVE_FUNC(var, type, name)		\
do {							\
  (var) = (type) GetProcAddress (hmagick, (name));	\
  if (!(var)) goto error;				\
} while (0)

  /* first, check version number of ImageMagick. */
  MAGICK_RESOLVE_FUNC (pget_magick_version, pget_magick_version_t,
		       "GetMagickVersion");

  magick_get_magick_version (&version);
  if (version != IMAGE_MAGICK_REQUIRE_VERSION) /* XXX: strict check. */
	  goto error;

  /* second, load other function addresses. */
  MAGICK_RESOLVE_FUNC (pinitialize_magick, pinitialize_magick_t,
		       "InitializeMagick");
  MAGICK_RESOLVE_FUNC (pread_image, pread_image_t, "ReadImage");
  MAGICK_RESOLVE_FUNC (pblob_to_image, pblob_to_image_t, "BlobToImage");
  MAGICK_RESOLVE_FUNC (pclone_image_info, pclone_image_info_t,
		       "CloneImageInfo");
  MAGICK_RESOLVE_FUNC (pdestroy_image, pdestroy_image_t, "DestroyImage");
  MAGICK_RESOLVE_FUNC (pdestroy_image_info, pdestroy_image_info_t,
		       "DestroyImageInfo");
  MAGICK_RESOLVE_FUNC (pget_image_pixels, pget_image_pixels_t,
		       "GetImagePixels");
  MAGICK_RESOLVE_FUNC (pget_exception_info, pget_exception_info_t,
		       "GetExceptionInfo");
  MAGICK_RESOLVE_FUNC (pping_blob, pping_blob_t, "PingBlob");
  MAGICK_RESOLVE_FUNC (pping_image, pping_image_t, "PingImage");
  MAGICK_RESOLVE_FUNC (pdestroy_image_list, pdestroy_image_list_t,
		       "DestroyImageList");
  MAGICK_RESOLVE_FUNC (pget_magick_info, pget_magick_info_t,
		       "GetMagickInfo");
  MAGICK_RESOLVE_FUNC (pget_magick_list, pget_magick_list_t,
		       "GetMagickList");
  MAGICK_RESOLVE_FUNC (pget_indexes, pget_indexes_t, "GetIndexes");
  MAGICK_RESOLVE_FUNC (ptransform_rgb_image, ptransform_rgb_image_t,
		       "TransformRGBImage");
  MAGICK_RESOLVE_FUNC (pacquire_image_pixels, pacquire_image_pixels_t,
		       "AcquireImagePixels");
  MAGICK_RESOLVE_FUNC (pis_monochrome_image, pis_monochrome_image_t,
		       "IsMonochromeImage");
  MAGICK_RESOLVE_FUNC (pset_image_type, pset_image_type_t, "SetImageType");
  MAGICK_RESOLVE_FUNC (pconstitute_image, pconstitute_image_t,
		       "ConstituteImage");
  MAGICK_RESOLVE_FUNC (pallocate_image, pallocate_image_t, "AllocateImage");
  MAGICK_RESOLVE_FUNC (pset_image_pixels, pset_image_pixels_t,
		       "SetImagePixels");
  MAGICK_RESOLVE_FUNC (psync_image_pixels, psync_image_pixels_t,
		       "SyncImagePixels");
  MAGICK_RESOLVE_FUNC (popen_blob, popen_blob_t, "OpenBlob");
  MAGICK_RESOLVE_FUNC (pread_blob_string, pread_blob_string_t,
		       "ReadBlobString");
  MAGICK_RESOLVE_FUNC (pclose_blob, pclose_blob_t, "CloseBlob");
  MAGICK_RESOLVE_FUNC (pallocate_image_colormap, pallocate_image_colormap_t,
		       "AllocateImageColormap");
  MAGICK_RESOLVE_FUNC (pquery_color_database, pquery_color_database_t,
		       "QueryColorDatabase");
  MAGICK_RESOLVE_FUNC (pregister_magick_info, pregister_magick_info_t,
		       "RegisterMagickInfo");
  MAGICK_RESOLVE_FUNC (pset_magick_info, pset_magick_info_t, "SetMagickInfo");
  MAGICK_RESOLVE_FUNC (prelinquish_magick_memory, prelinquish_magick_memory_t,
		       "RelinquishMagickMemory");
  MAGICK_RESOLVE_FUNC (pacquire_string, pacquire_string_t, "AcquireString");
  MAGICK_RESOLVE_FUNC (pget_image_magick, pget_image_magick_t,
		       "GetImageMagick");

#undef MAGICK_RESOLVE_FUNC

  return 1;

 error:
  FreeLibrary (hmagick);
  hmagick = NULL;
  return 0;
}

static int
imagemagick_transparent_color_p (const PixelPacket *pixel)
{
  return pixel->opacity == MaxRGB;
}

static int
imagemagick_make_maskdata (struct image *img,
			   Image *images, int *mask_color)
{
  unsigned char *pmask = img->mw32_img.pbmpmask;
  PixelPacket *pixel = magick_get_image_pixels (images, 0, 0,
						images->columns, images->rows);
  IndexPacket *indexes = magick_get_indexes (images);
  int bytes = bmp_get_bitmap_width_bytes (images->columns, 1);
  long x, y;

  for (y = 0; y < (long) images->rows; y++) 
    {
      unsigned char *pwidth = pmask;
      int bit = 0;
      unsigned char byte = 0;

      for (x = 0; x < (long) images->columns; x++)
        {
          byte <<= 1;
	  switch (images->storage_class)
	    {
	    case DirectClass:
	      if (imagemagick_transparent_color_p (pixel)
		  || (mask_color != NULL
		      && mask_color[0] == ScaleQuantumToChar (pixel->red)
		      && mask_color[1] == ScaleQuantumToChar (pixel->green)
		      && mask_color[2] == ScaleQuantumToChar (pixel->blue)))
		byte |= 0x01;
	      break;
	    case PseudoClass:
	      {
		PixelPacket *p = &images->colormap[*indexes];
		if (mask_color != NULL
		    && mask_color[0] == ScaleQuantumToChar (p->red)
		    && mask_color[1] == ScaleQuantumToChar (p->green)
		    && mask_color[2] == ScaleQuantumToChar (p->blue))
		  byte |= 0x01;
	      }
	      break;
	    default:
	      return 0;
	    }
          bit++;

          if (bit == 8) 
            {
              *pwidth++ = byte;
              bit = 0;
              byte = 0;
            }
          pixel++;
	  indexes++;
        }
      if (bit != 0)
        *pwidth++ = byte << (8 - bit);
      pmask += bytes;
    }
  return 1;
}

static int
imagemagick_setup_bmpmask (struct image *img,
			   Image *images, int *mask_color) 
{
  if ((images->storage_class == DirectClass && images->matte)
      || mask_color != NULL)
    {
      int size = (bmp_get_bitmap_width_bytes (images->columns, 1)
		  * images->rows);
      unsigned char *pmask = xmalloc (size);
      if (pmask == NULL)
	return 0;
      memset (pmask, 0, size);

      img->mw32_img.mask_size = size;
      img->mw32_img.pbmpmask = pmask;

      imagemagick_make_maskdata (img, images, mask_color);
    }
  return 1;
}

static int
imagemagick_setup_fullcolor_bitmapinfo (struct image *img,
					RGBQUAD *prgb, Image *images)
{
  int size;
  BITMAPINFO *pbmpi;
  
  size = sizeof (BITMAPINFOHEADER);
  pbmpi = (BITMAPINFO*) xmalloc (size);
  memset (pbmpi, 0, size);

  pbmpi->bmiHeader.biSize = sizeof (BITMAPINFOHEADER);
  pbmpi->bmiHeader.biWidth = images->columns;
  pbmpi->bmiHeader.biHeight = (-1) * images->rows; /* flip */
  pbmpi->bmiHeader.biPlanes = 1;
  pbmpi->bmiHeader.biBitCount = imagemagick_get_bits_per_pixel (images);
  pbmpi->bmiHeader.biCompression = BI_RGB;
  pbmpi->bmiHeader.biSizeImage = 0;
  pbmpi->bmiHeader.biXPelsPerMeter = 0;
  pbmpi->bmiHeader.biYPelsPerMeter = 0;
  pbmpi->bmiHeader.biClrUsed = 0;
  pbmpi->bmiHeader.biClrImportant = 0;

  img->mw32_img.pbmpinfo = pbmpi;

  return 1;
}

static int
imagemagick_image_p (Lisp_Object object)
{
  /* TODO */
  return 1;
}

static int
imagimagick_get_fullcolor_bmpdata (struct image *img, Image *image)
{
  unsigned char *bmpbit = img->mw32_img.pbmpdata;
  PixelPacket *p = magick_get_image_pixels (image, 0, 0,
					    image->columns, image->rows);
  int bpp = imagemagick_get_bits_per_pixel (image);
  int bytes = bmp_get_bitmap_width_bytes (image->columns, bpp);
  long x, y;
  
  for (y = 0; y < (long) image->rows; y++)
    {
      unsigned char *pwidth = bmpbit;

      for (x = 0; x < (long) image->columns; x++)
        {
	  *pwidth++ = ScaleQuantumToChar (p->blue);
	  *pwidth++ = ScaleQuantumToChar (p->green);
	  *pwidth++ = ScaleQuantumToChar (p->red);
	  if (bpp == 32)
	    *pwidth++ = ScaleQuantumToChar (p->opacity);
          p++;
        }
      bmpbit += bytes;
    }
  
  return 1;
}

static RGBQUAD*
imagemagick_read_color_table (Image *image, bmp_info *pbi)
{
  int i;
  int n = 1 << imagemagick_get_bits_per_pixel (image);
  RGBQUAD *prgb;
  unsigned char *p;

  if (n > 256) return NULL;
  if (n < image->colors)
    n = image->colors;
  prgb = (RGBQUAD*) xmalloc (sizeof (RGBQUAD) * n);

  for (i = 0; i < image->colors; i++)
    {
      prgb[i].rgbBlue = ScaleQuantumToChar (image->colormap[i].blue);
      prgb[i].rgbGreen = ScaleQuantumToChar (image->colormap[i].green);
      prgb[i].rgbRed = ScaleQuantumToChar (image->colormap[i].red);
      prgb[i].rgbReserved = 0;
    }
  for ( ; i < n; i++)
    {
      prgb[i].rgbBlue = 0;
      prgb[i].rgbGreen = 0;
      prgb[i].rgbRed = 0;
      prgb[i].rgbReserved = 0;
    }

  return prgb;
}

static int
imagemagick_setup_palette_bitmapinfo (struct image *img,
				      RGBQUAD *prgb, Image *images)
{
  int n, size;
  BITMAPINFO *pbmpi;
  
  n = 1 << imagemagick_get_bits_per_pixel (images);
  size = sizeof (BITMAPINFOHEADER) + n * sizeof (RGBQUAD);
  pbmpi = (BITMAPINFO*) xmalloc (size);
  memset (pbmpi, 0, size);

  pbmpi->bmiHeader.biSize = sizeof (BITMAPINFOHEADER);
  pbmpi->bmiHeader.biWidth = images->columns;
  pbmpi->bmiHeader.biHeight = (-1) * images->rows; /* flip */
  pbmpi->bmiHeader.biPlanes = 1;
  pbmpi->bmiHeader.biBitCount = imagemagick_get_bits_per_pixel (images);
  pbmpi->bmiHeader.biCompression = BI_RGB;
  pbmpi->bmiHeader.biSizeImage = 0;
  pbmpi->bmiHeader.biXPelsPerMeter = 0;
  pbmpi->bmiHeader.biYPelsPerMeter = 0;
  pbmpi->bmiHeader.biClrUsed = 0;
  pbmpi->bmiHeader.biClrImportant = 0;

  img->mw32_img.pbmpinfo = pbmpi;
  if (prgb)
    memcpy (pbmpi->bmiColors, prgb, n * sizeof (RGBQUAD));

  return 1;
}

static int
imagimagick_get_mono_bmpdata (struct image *img, Image *image)
{
  unsigned char *bmpbit = img->mw32_img.pbmpdata;
  int bytes = bmp_get_bitmap_width_bytes (image->columns, 1);
  IndexPacket *indexes;
  long x, y;

  for (y = 0; y < (long) image->rows; y++) 
    {
      unsigned char *pwidth = bmpbit;
      int bit = 0;
      unsigned char byte = 0;
      const PixelPacket *p = magick_acquire_image_pixels (image, 0, y,
							  image->columns, 1,
							  &image->exception);
      if (p == (const PixelPacket *) NULL)
        break;
      indexes = magick_get_indexes (image);

      for (x = 0; x < (long) image->columns; x++)
        {
          byte <<= 1;
          byte |= indexes[x] ? 0x01 : 0x00;
          bit++;

          if (bit == 8)
            {
              *pwidth++ = byte;
              bit = 0;
              byte = 0;
            }
        }
      if (bit != 0)
        *pwidth++ = byte << (8 - bit);
      bmpbit += bytes;
    }

  return 1;
}

static int
imagimagick_get_256_bmpdata (struct image *img, Image *image)
{
  unsigned char *bmpbit = img->mw32_img.pbmpdata;
  int bytes = bmp_get_bitmap_width_bytes (image->columns, 8);
  IndexPacket *indexes;
  long x, y;

  for (y = 0; y < (long) image->rows; y++) 
    {
      unsigned char *pwidth = bmpbit;
      const PixelPacket *p = magick_acquire_image_pixels (image, 0, y,
							  image->columns, 1,
							  &image->exception);
      if (p == (const PixelPacket *) NULL)
        break;
      indexes = magick_get_indexes (image);

      for (x = 0; x < (long) image->columns; x++)
        {
          *pwidth++ = indexes[x];
        }
      bmpbit += bytes;
    }

  return 1;
}

static int
imagemagick_get_bits_per_pixel (Image *image)
{
  if (image->storage_class != DirectClass)
    {
      if (image->matte) 
        {
          magick_set_image_type (image, TrueColorMatteType);
          return 32;
        }

      if (magick_is_monochrome_image (image, &image->exception))
        return 1;
      else if (image->colors > (1 << 8)) 
        {
          magick_set_image_type (image, TrueColorType);
          return 24;
        }
      else
        return 8;
    }
  else {
    if (!image->matte)
      return 24;
    else
      return 32;
  }
}

static void
pixel_to_rgb (unsigned long pixel, RGBQUAD *prgb)
{
  prgb->rgbRed = pixel & 0xFF; pixel >>= 8;
  prgb->rgbGreen = pixel & 0xFF; pixel >>= 8;
  prgb->rgbBlue = pixel & 0xFF; pixel >>= 8;
  prgb->rgbReserved = 0;
}

static void
get_foreground_and_background_color (struct frame *f, struct image *img,
				     RGBQUAD *prgb)
{
  unsigned long foreground = f->output_data.mw32->foreground_pixel;
  unsigned long background = f->output_data.mw32->background_pixel;
  Lisp_Object value;
  RGBQUAD *pfg = prgb;
  RGBQUAD *pbg = prgb;

  if (prgb->rgbRed == 0 && prgb->rgbGreen == 0 && prgb->rgbBlue == 0)
    pbg++;
  else
    pfg++;

  /* Get foreground and background colors, maybe allocate colors.  */
  value = image_spec_value (img->spec, QCforeground, NULL);
  if (STRINGP (value))
    foreground = mw32_decode_color (f, value, WHITE_PIX_DEFAULT (f));
  pixel_to_rgb (foreground, pfg);

  value = image_spec_value (img->spec, QCbackground, NULL);
  if (STRINGP (value))
    background = mw32_decode_color (f, value, BLACK_PIX_DEFAULT (f));
  pixel_to_rgb (background, pbg);
}

static void
resolve_mask_color (Lisp_Object how, Image* image, int *rgb)
{
  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, try to determine the color
     heuristically. */
  int look_at_corners_p = 1;

  if (CONSP (how))
    {
      int i = 0;

      while (i < 3
	     && CONSP (how)
	     && NATNUMP (XCAR (how)))
	{
	  rgb[i++] = XFASTINT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
	  look_at_corners_p = 0;
	}
    }
  if (look_at_corners_p)
    {
      PixelPacket *corners[4];
      int i, best_count;

      /* Get the colors at the corners of image.  */
      corners[0] = magick_get_image_pixels (image, 0, 0, 1, 1);
      corners[1] = magick_get_image_pixels (image, image->columns - 1, 0,
					    1, 1);
      corners[2] = magick_get_image_pixels (image, image->columns - 1,
					    image->rows - 1, 1, 1);
      corners[3] = magick_get_image_pixels (image, 0,
					    image->rows - 1, 1, 1);

      /* Choose the most frequently found color as background.  */
      for (i = best_count = 0; i < 4; ++i)
	{
	  int j, n;
	  
	  for (j = n = 0; j < 4; ++j)
	    if (corners[i] == corners[j])
	      ++n;

	  if (n > best_count)
	    {
	      rgb[0] = ScaleQuantumToChar (corners[i]->red);
	      rgb[1] = ScaleQuantumToChar (corners[i]->green);
	      rgb[2] = ScaleQuantumToChar (corners[i]->blue);
	      best_count = n;
	    }
	}
    }
  return;
}

static int
imagemagick_load_image_data (struct frame *f,
                             struct image *img,
                             ImageInfo *image_info, Image *images)
{
  bmp_info bi;
  RGBQUAD *prgb = NULL;
  int imagesize, bpp;
  
  img->width = images->columns;
  img->height = images->rows;

  bpp = imagemagick_get_bits_per_pixel (images);
  imagesize = images->rows * bmp_get_bitmap_width_bytes (images->columns, bpp);

  switch (bpp)
    {
    case 1:
      prgb = imagemagick_read_color_table (images, &bi);
      if (!prgb) return 0;
      mw32_create_image_simple (f, img, imagesize);

      get_foreground_and_background_color (f, img, prgb);
      imagemagick_setup_palette_bitmapinfo (img, prgb, images);
      imagimagick_get_mono_bmpdata (img, images);
      break;
    case 8:
      prgb = imagemagick_read_color_table (images, &bi);
      if (!prgb) return 0;
      mw32_create_image_simple (f, img, imagesize);
      imagemagick_setup_palette_bitmapinfo (img, prgb, images);
      imagimagick_get_256_bmpdata (img, images);
      break;
    case 24:
    case 32:
      mw32_create_image_simple (f, img, imagesize);
      imagemagick_setup_fullcolor_bitmapinfo (img, prgb, images);
      imagimagick_get_fullcolor_bmpdata (img, images);
      break;
    }

  {
    /* `:heuristic-mask t'
       `:mask heuristic'
       means build a mask heuristically.
       `:heuristic-mask (R G B)'
       `:mask (heuristic (R G B))'
       means build a mask from color (R G B) in the
       image.
       `:mask nil'
       means remove a mask, if any.  */
	      
    int mask_color[3], *pmask_color = mask_color;
    int mask_p = 1;
    Lisp_Object mask = image_spec_value (img->spec, QCheuristic_mask, NULL);
    
    if (!NILP (mask))
      resolve_mask_color (Qt, images, mask_color);
    else
      {
	int found_p;
		    
	mask = image_spec_value (img->spec, QCmask, &found_p);
		  
	if (EQ (mask, Qheuristic))
	  resolve_mask_color (Qt, images, mask_color);
	else if (CONSP (mask)
		 && EQ (XCAR (mask), Qheuristic))
	  {
	    if (CONSP (XCDR (mask)))
	      resolve_mask_color (XCAR (XCDR (mask)), images, mask_color);
	    else
	      resolve_mask_color (XCDR (mask), images, mask_color);
	  }
	else if (NILP (mask) && found_p)
	  mask_p = 0;
	else
	  pmask_color = NULL;
      }
    if (mask_p)
      imagemagick_setup_bmpmask (img, images, pmask_color);
  }
    
  if (prgb) xfree (prgb);

  return 1;
}

static int
imagemagick_valid_data_p (const ImageInfo *image_info,
			  ExceptionInfo *exception)
{
  Image *image = magick_ping_image (image_info, exception);
  if (image == NULL)
    return 0;
  magick_destroy_image (image);
  return 1;
}

static int
imagemagick_valid_object_p (const ImageInfo *image_info, const void *blob,
			    const size_t length, ExceptionInfo *exception)
{
  /* Now, magick_ping_blob () of ImageMagick-5.5.3 works well.  */  
  Image *image = magick_ping_blob (image_info, blob, length, exception);
  
  if (image == NULL)
    return 0;
  magick_destroy_image (image);
  return 1;
}

static void
unpack_bitmap_data (unsigned char *src, /* packed data */
		    unsigned char *dst, /* unpacked data that is
					   aligned with byte boundary
					   per scanline */
		    int width, int height)
{
  int i, j;
  int m, n;
  int src_bits, dst_bits;

  m = n = 0;
  for (j = 0; j < height; j++)
    {
      dst_bits = 0x00;
      for (i = 0; i < width; i++)
	{
	  if ((j * width + i) % 8 == 0)
	    src_bits = src[m++];
	  if (i != 0 && i % 8 == 0)
	    {
	      dst[n++] = dst_bits;
	      dst_bits = 0x00;
	    }
	  dst_bits |= (src_bits & 0x01) ? (0x01 << (i % 8)) : 0x00;
	  src_bits >>= 1;
	}
      dst[n++] = dst_bits;
    }
}

static Image *
load_in_memory_image (struct image *img, Lisp_Object data,
		      const ImageInfo *image_info)
{
  Image *images = NULL;
  Lisp_Object width= image_spec_value (img->spec, QCwidth, NULL);
  Lisp_Object height= image_spec_value (img->spec, QCheight, NULL);
  int iw = NUMBERP (width) ? XFASTINT (width) : 0;
  int ih = NUMBERP (height) ? XFASTINT (height) : 0;
  unsigned char *bitmap;
  int i, j, k;

  if (iw <= 0 || ih <= 0)
    return NULL;
  
  if (STRINGP (data))
    {
      bitmap = SDATA (data);
    }
  else if (VECTORP (data))
    {
      int nbytes = (iw + BITS_PER_CHAR - 1) / BITS_PER_CHAR;
      unsigned char *p = (unsigned char *) alloca (sizeof (unsigned char)
						   * nbytes * ih);

      bitmap = p;
      for (i = 0; i < img->height; i++, p += nbytes)
	{
	  Lisp_Object line = XVECTOR (data)->contents[i];
	  if (STRINGP (line))
	    bcopy (SDATA (line), p, nbytes);
	  else
	    /* Raw data of bool-vector is packed, so I expand it. */
	    unpack_bitmap_data (XBOOL_VECTOR (line)->data, p, iw, 1);
	}
    }
  else if (BOOL_VECTOR_P (data))
    {
      int nbytes = (iw + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

      if (XBOOL_VECTOR (data)->size < iw * ih)
	return NULL;
      bitmap = (unsigned char *) alloca (sizeof (unsigned char) * nbytes * ih);
      /* Raw data of bool-vector is packed, so I expand it. */
      unpack_bitmap_data (XBOOL_VECTOR (data)->data, bitmap, iw, ih);
    }
  else
    {
      return NULL;
    }

  images = magick_allocate_image (image_info);

  if (images == NULL)
    return NULL;

  images->columns = iw;
  images->rows = ih;
  magick_set_image_type (images, BilevelType);

  for (i = 0, k = 0; i < ih; i++)
    {
      PixelPacket *q = magick_get_image_pixels (images, 0, i,
      						images->columns, 1);
      IndexPacket *indexes;
      unsigned char bits;
	
      if (q == (PixelPacket *) NULL)
	{
	  image_error ("Error bad ImageMagick image data `%s'", data, Qnil);
	  break;
	}
      indexes = magick_get_indexes (images);

      for (j = 0; j < iw; j++)
	{
	  if (j % 8 == 0)
	    {
	      bits = bitmap[k++];
	    }
	  *indexes++ = bits & 0x01 ? 0x00 : 0x01;
	  bits >>= 1;
	}
      if (!magick_sync_image_pixels (images))
	{
	  image_error ("Error bad ImageMagick image data `%s'", data, Qnil);
	  break;
	}
      
    }
  return images;
}

typedef struct
{
  char color[MaxTextExtent];
  char name[MaxTextExtent];
} XPM_color_symbols_elem;

typedef struct
{
  int n;
  XPM_color_symbols_elem *data;
} XPM_color_symbols;

static void *
parse_XPM_color_symbols (struct image *img)
{
  XPM_color_symbols *result = NULL;
  Lisp_Object color_symbols = image_spec_value (img->spec,
						QCcolor_symbols, NULL);
  if (!NILP (color_symbols))
    {
      int i = 0, nsymbols = 0;
      XPM_color_symbols_elem *elem;
      Lisp_Object tail;
	      
      for (tail = color_symbols; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object name = XCAR (XCAR (tail));
	  Lisp_Object color = XCDR (XCAR (tail));
	  if (STRINGP (name) && STRINGP (color))
	    nsymbols++;
	}
      elem = ((XPM_color_symbols_elem *)
	      xmalloc (sizeof (XPM_color_symbols_elem) * nsymbols));
      if (elem == NULL)
	return NULL;
      for (tail = color_symbols; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object name = XCAR (XCAR (tail));
	  Lisp_Object color = XCDR (XCAR (tail));

	  if (STRINGP (name) && STRINGP (color))
	    {
	      strcpy (elem[i].name, SDATA (name));
	      strcpy (elem[i].color, SDATA (color));
	      i++;
	    }
	}
      result = (XPM_color_symbols *) xmalloc (sizeof (XPM_color_symbols));
      if (result == NULL)
	{
	  xfree ((void *) elem);
	  return NULL;
	}
      result->n = nsymbols;
      result->data = elem;
    }
  return (void *) result;
}

static void
free_XPM_color_symbols (void *symbols)
{
  XPM_color_symbols *s = (XPM_color_symbols *) symbols;
  xfree ((void *) s->data);
  xfree ((void *) s);
}

static int
imagemagick_load (struct frame *f, struct image *img)
{
  int success_p = 0;
  Lisp_Object file_name;
  ImageInfo *image_info = NULL;
  Image *image = NULL;
  struct gcpro gcpro1;

  xassert (imagemagick_image_p (img->spec));

  image_info = magick_clone_image_info (NULL);

  image_info->client_data = parse_XPM_color_symbols (img);

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      
      file = mw32_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
          magick_destroy_image_info (image_info);
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}
      strcpy (image_info->filename, SDATA (file));

      if (imagemagick_valid_data_p (image_info, &magick_exception))
	{
	  image = magick_read_image (image_info, &magick_exception);
	  if (image == NULL)
	    image_error ("Error loading ImageMagick image `%s'",
			 img->spec, Qnil);
	}
      else
	{
	  image_error ("Error bad ImageMagick file `%s'", img->spec, Qnil);
	}
    }
  else
    {
      Lisp_Object data;
      
      data = image_spec_value (img->spec, QCdata, NULL);
      GCPRO1 (data);

      if (STRINGP (data)
	  && imagemagick_valid_object_p (image_info, SDATA (data),
					 (size_t) SBYTES (data),
					 &magick_exception))
	{
	  image = magick_blob_to_image (image_info, SDATA (data),
					(size_t) SBYTES (data),
					&magick_exception);
	  if (image == NULL)
	    image_error ("Error loading ImageMagick image `%s'",
			 img->spec, Qnil);
	}
      else
	{
	  image = load_in_memory_image (img, data, image_info);
	  if (image == NULL)
	    image_error ("Error loading ImageMagick image `%s'",
			 img->spec, Qnil);
	}
    }
  if (image != NULL)
    {
      Lisp_Object index_prop = image_spec_value (img->spec, QCindex, NULL);

      if (INTEGERP (index_prop))
	{
	  int index = XINT (index_prop);
	  int i;

	  for (i = 0; i < index; i++)
	    if (image->next)
	      image = image->next;
	    else
	      break;
	}

      success_p = imagemagick_load_image_data (f, img, image_info, image);
      magick_destroy_image_list (image);
    }

  if (image_info->client_data != NULL)
    {
      free_XPM_color_symbols (image_info->client_data);
      image_info->client_data = NULL;
    }
  

  magick_destroy_image_info (image_info);
  UNGCPRO;
  
  return success_p;
}

static Lisp_Object*
imagemagick_image_type_lisp_symbol (const char *name)
{
  int i, namesize;
  char *p, *q;
  Lisp_Object str, lowerstr, sym, *psym;

  /* deal with the name exception */
  if (strcmp (name, "PS") == 0)
    {
      str = build_string ("postscript");
    }
  else
    {
      str = build_string ((char*) name);
    }
  lowerstr = Fdowncase (str);
  sym = Fintern_soft (lowerstr, Qnil);

  if (!NILP (sym)) 
    {
      if (EQ (sym, Qxbm))
	return &Qxbm;
      else if (EQ (sym, Qxpm))
	return &Qxpm;
      else if (EQ (sym, Qpostscript))
	return &Qpostscript;
      else if (EQ (sym, Qpbm))
	return &Qpbm;
      else if (EQ (sym, Qjpeg))
	return &Qjpeg;
      else if (EQ (sym, Qtiff))
	return &Qtiff;
      else if (EQ (sym, Qgif))
	return &Qgif;
      else if (EQ (sym, Qpng))
	return &Qpng;
    }

  /* This image type seems not match prescribed type.  */
  psym = (Lisp_Object *) xmalloc (sizeof (Lisp_Object));
  *psym = intern ((char *) name);
  staticpro (psym);

  return psym;
}

static int 
define_imagemagick_type (void)
{
  int i;
  unsigned long nformats;
  struct image_type img_type;
  char **formats = magick_get_magick_list ("*", &nformats, &magick_exception);

  img_type.valid_p = imagemagick_image_p;
  img_type.load = imagemagick_load;
  img_type.free = mw32_destruct_image;
  img_type.next = NULL;

  if (formats == NULL)
    return 0;
      
  for (i = 0; i < nformats; i++)
    {
      /* Ignore Encapsulated PostScript related formats.  */
      if (!_stricmp (formats[i], "EPI")
	  || !_stricmp (formats[i], "EPS")
	  || !_stricmp (formats[i], "EPSF")
	  || !_stricmp (formats[i], "EPSI")
	  || !_stricmp (formats[i], "EPT")
	  || !_stricmp (formats[i], "EPT2")
	  || !_stricmp (formats[i], "EPT3"))
	continue;
      img_type.type = imagemagick_image_type_lisp_symbol (formats[i]);
      magick_relinquish_magick_memory ((void **) formats[i]);
      if (img_type.type)
	define_image_type (&img_type);
    }
  magick_relinquish_magick_memory ((void **) formats);

  return 1;
}

typedef struct
{
  char key[MaxTextExtent];
  char color[MaxTextExtent];
  char symbol[MaxTextExtent];
} XPM_palette_info;

static int
XPM_strip_line (char *buffer)
{
  int i, n;
  int length = strlen (buffer);

  n = 0;
  for (i = 0; i < length; i++)
    {
      if (buffer[i] == '"')
	{
	  buffer[i] = ' ';
	  n++;
	}
    }
  return n == 2;
}

static char *
XPM_find_next_line (char *p)
{
  while (*p != '\n' && *p != '\0')
    {
      p++;
    }
  if (*p == '\n')
    p++;
  return p;
}

static char *
XPM_extract_line (char *buffer, int size, char *src)
{
  int length;
  char *p = XPM_find_next_line (src);
  
  if (p == src)
    return NULL;

  length = p - src;
  if (length > size)
    length = size;
  strncpy (buffer, src, length);
  buffer[length] = '\0';
  return p;
}

static char *
XPM_read_blob_to_string (Image *image)
{
  unsigned long length = MaxTextExtent;
  char *buffer = (char *) xmalloc (sizeof (char) * length);
  char *p = buffer;

  bzero (p, sizeof (char) * length);
  if (buffer != NULL)
    /* magick_read_blob_string () returns NULL, even if reading blob
       successfully has finished. So I also check *p. */
    while (magick_read_blob_string (image, p) != NULL || *p != '\0')
      {
	p += strlen (p) + 1; /* one for '\n' */
	*(p - 1) = '\n'; /* replace '\0' to '\n' */
	if (p - buffer + MaxTextExtent + 1 >= length)
	  {
	    length <<= 1;
	    buffer = xrealloc ((void *) buffer, length);
	    if (buffer == NULL)
	      break;
	    p = buffer + strlen (buffer);
	    bzero (p, sizeof (char) * (length - strlen (buffer)));
	  }
      }
  if (buffer == NULL)
    {
      image_error ("Memory allocation failed", Qnil, Qnil);
      return NULL;
    }
  return buffer;
}

static char *
XPM_read_values_section (char *xpm_buffer, Image *image, unsigned long *width)
{
  char line_buffer[MaxTextExtent];
  char *p = XPM_extract_line (line_buffer, MaxTextExtent - 1, xpm_buffer);
  int count;
  
  while (p != NULL)
    {
      if (XPM_strip_line (line_buffer))
	{
	  count = sscanf (line_buffer, "%lu %lu %lu %lu",
			  &image->columns, &image->rows,
			  &image->colors, width);
	  if (count == 4)
	    break;
	}
      p = XPM_extract_line (line_buffer, MaxTextExtent - 1, p);
    }

  if (count != 4 || *width > 2 || image->columns == 0 ||
      image->rows == 0 || image->colors == 0)
    {
      image_error ("Not a XPM image file", Qnil, Qnil);
      return NULL;
    }
  if (*width > MaxTextExtent - 1)
    {
      image_error ("Too large width", Qnil, Qnil);
      return NULL;
    }
  return p;
}

#define XPM_NUM_KEYS 6

static char *
XPM_read_colors_section (char *p, Image *image, unsigned long width,
			 XPM_palette_info **palette_info, long *none,
			 ExceptionInfo *exception)
{
  static char *valid_keys[XPM_NUM_KEYS] = { "c", "g", "g4", "m", "b", "s"};
  char line_buffer[MaxTextExtent];
  int i = 0;
  *palette_info = (XPM_palette_info *) xmalloc (sizeof (XPM_palette_info)
						* image->colors);
  if (*palette_info == NULL)
    {
      image_error ("Memory allocation failed", Qnil, Qnil);
      return NULL;
    }

  while (i < image->colors)
    {
      p = XPM_extract_line (line_buffer, MaxTextExtent - 1, p);
      if (p == NULL)
	break;
      
      if (XPM_strip_line (line_buffer))
	{
	  char* q;
      
	  (*palette_info)[i].key[0] = '\0';
	  strcpy ((*palette_info)[i].color, "gray");
	  (*palette_info)[i].symbol[0] = '\0';
      
	  /* one for double quote */
	  strncpy ((*palette_info)[i].key, line_buffer + 1, width);
	  (*palette_info)[i].key[width] = '\0';
	  
	  q = strtok (line_buffer + 1 + width, " \t");
	  while (q != NULL)
	    {
	      char *result = NULL;
	      int j;

	      /* Read a key */
	      for (j = 0; j < XPM_NUM_KEYS; j++)
		{
		  if (strcmp (q, valid_keys[j]) == 0)
		    break;
		}
	      if (j == XPM_NUM_KEYS)
		{
		  image_error ("invalid XPM key `%s'", build_string (q), Qnil);
		  return NULL;
		}
	      if (strcmp (q, "s") == 0)
		result = (*palette_info)[i].symbol;
	      else if (strcmp (q, "c") == 0)
		result = (*palette_info)[i].color;

	      /* Read a value */
	      q = strtok (NULL, " \t");
	      if (q == NULL)
		{
		  image_error ("invalid colormap", Qnil, Qnil);
		  return NULL;
		}
	      if (result != NULL)
		{
		  if (strlen (q) > MaxTextExtent - 1)
		    {
		      image_error ("Too large colormap data",
				   build_string (q), Qnil);
		      return NULL;
		    }
		  strcpy (result, q);
		}
	      /* check if next token is a valid key. */
	      while (1)
		{
		  q = strtok (NULL, " \t\n,");
		  if (q == NULL)
		    break;
		  for (j = 0; j < XPM_NUM_KEYS; j++)
		    if (strcmp (q, valid_keys[j]) == 0)
		      break;
		  if (j != XPM_NUM_KEYS)
		    break;
		  if (result != NULL)
		    {
		      if (strlen (result) + strlen (q) + 1 > MaxTextExtent - 1)
			{
			  image_error ("Too long colormap name `%s'",
				       build_string (result), Qnil);
			  return NULL;
			}
		      strcat (result, " ");
		      strcat (result, q);
		    }
		}
	    }
	  if (strcmp ((*palette_info)[i].color, "none") == 0
	      || strcmp ((*palette_info)[i].color, "None") == 0)
	    {
	      image->storage_class = DirectClass;
	      image->matte = 1;
	      *none = i;
	      strcpy ((*palette_info)[i].color, "black");
	    }
	  if (!magick_query_color_database ((*palette_info)[i].color,
					    &image->colormap[i], exception))
	    {
	      image_error ("Unknown color name `%s'",
			   build_string ((*palette_info)[i].color), Qnil);
	      break;
	    }
	  i++;
	}
    }
  if (i != image->colors)
    {
      image_error ("Corrupt XPM image file", Qnil, Qnil);
      return NULL;
    }
  return p;
}

static void
XPM_correct_colormap_with_symbols (const ImageInfo *image_info, Image *image,
				   XPM_palette_info *palette_info,
				   long *none, ExceptionInfo *exception)
{
  if (image_info->client_data != NULL)
    {
      int m, n;
      XPM_color_symbols *symbols
	= (XPM_color_symbols *) image_info->client_data;
      for (m = 0; m < symbols->n; m++)
	{
	  for (n = 0; n < image->colors; n++)
	    {
	      if (strcmp (palette_info[n].symbol,
			  symbols->data[m].name) == 0)
		{
		  strcpy (palette_info[n].color,
			  symbols->data[m].color);
		  if (strcmp (palette_info[n].color, "none") == 0
		      || strcmp (palette_info[n].color, "None") == 0)
		    {
		      image->storage_class = DirectClass;
		      image->matte = 1;
		      *none = n;
		      strcpy (palette_info[n].color, "black");
		    }
		  if (!magick_query_color_database (palette_info[n].color,
						    &image->colormap[n],
						    exception))
		    {
		      image_error ("Unknown color name `%s'",
				   build_string (palette_info[n].color), Qnil);
		      break;
		    }
		}
	    }
	}
    }
}

static char *
XPM_read_pixels_section (char *p, const ImageInfo *image_info, Image *image,
			 unsigned long width, long none,
			 XPM_palette_info *palette_info)
{
  int i = 0;
  int x;
  char line_buffer[MaxTextExtent];
  char key[MaxTextExtent];
  char *q;
  IndexPacket *indexes;
  PixelPacket *r;
 
  if (!image_info->ping)
    {
      int y = 0;
      while  (y < image->rows)
	{
	  p = XPM_extract_line (line_buffer, MaxTextExtent - 1, p);
	  if (p == NULL)
	    break;
	  if (XPM_strip_line (line_buffer))
	    {
	      r = magick_set_image_piexels (image, 0, y, image->columns, 1);
	      if (r == NULL)
		break;
	      indexes = magick_get_indexes (image);

	      q = line_buffer + 1;
	      for (x = 0; x < image->columns; x++)
		{
		  strncpy (key, q, width);
		  key[width] = '\0';
		  if (strcmp (key, palette_info[i].key) != 0)
		    for (i = 0; i < max (image->colors - 1, 1); i++)
		      if (strcmp (key, palette_info[i].key) == 0)
			break;
		  if (image->storage_class == PseudoClass)
		    indexes[x] = (IndexPacket) i;
		  *r = image->colormap[i];
		  r->opacity = (Quantum) (i == none ? TransparentOpacity
					  : OpaqueOpacity);
		  r++;
		  q += width;
		}
	      if (!magick_sync_image_pixels (image))
		break;
	      y++;
	    }
	}
      if (y < image->rows)
	{
	  image_error ("Not enough pixel data", Qnil, Qnil);
	  return NULL;
	}
    }
  return p;
}

static Image *
XPM_read_image (const ImageInfo *image_info, ExceptionInfo *exception)
{
  char *xpm_buffer;
  XPM_palette_info *palette_info;
  Image *image;
  long none;
  char *p;
  unsigned int status;
  unsigned long width;

  /* Open image file. */
  xassert (image_info != (const ImageInfo *) NULL);
  xassert (image_info->signature == MagickSignature);
  xassert (exception != (ExceptionInfo *) NULL);
  xassert (exception->signature == MagickSignature);

  image = magick_allocate_image (image_info);
  status = magick_open_blob (image_info, image, ReadBinaryBlobMode, exception);
  if (status == 0)
    {
      image_error ("Unable to open file `%s'",
		   build_string ((char *) image_info->filename), Qnil);
      magick_destroy_image (image);
      return NULL;
    }

  /* Read blob to string. */
  xpm_buffer = XPM_read_blob_to_string (image);
  if (xpm_buffer == NULL)
    {
      magick_close_blob (image);
      magick_destroy_image (image);
      return NULL;
    }

  /* Read <Values> section. */
  p = XPM_read_values_section (xpm_buffer, image, &width);
  if (p == NULL)
    {
      xfree ((void *) xpm_buffer);
      magick_close_blob (image);
      magick_destroy_image (image);
      return NULL;
    }
  
  /* Initialize image structure. */
  image->depth = QuantumDepth;
  if (!magick_allocate_image_colormap (image, image->colors))
    {
      image_error ("Colormap allocation failed", Qnil, Qnil);
      xfree ((void *) xpm_buffer);
      magick_close_blob (image);
      magick_destroy_image (image);
      return NULL;
    }
  
  /* Read <Colors> section. */
  p = XPM_read_colors_section (p, image, width, &palette_info,
			       &none, exception);
  if (p == NULL)
    {
      if (palette_info != NULL)
	xfree ((void *) palette_info);
      xfree ((void *) xpm_buffer);
      magick_close_blob (image);
      magick_destroy_image (image);
      return NULL;
    }
  
  /* Colormap correction */
  XPM_correct_colormap_with_symbols (image_info, image,
				     palette_info, &none, exception);

  /* Read <Pixels> section. */
  p = XPM_read_pixels_section (p, image_info, image, width,
			       none, palette_info);
  if (p == NULL)
    {
      xfree ((void *) palette_info);
      xfree ((void *) xpm_buffer);
      magick_close_blob (image);
      magick_destroy_image (image);
      return NULL;
    }
  
  xfree ((void *) palette_info);
  xfree ((void *) xpm_buffer);
  magick_close_blob (image);

  return image;
}

static int
initialize_imagemagick (void)
{
  int ret;
  
  if (!resolve_imagemagick_api ())
    return 0;

  magick_initialize_magick (NULL);
  magick_get_exception_info (&magick_exception);

  /* Caution: Call this function here before the following block where
     a new MagickInfo (encoder, decoder and so forth) is entried.
     Because this function implicitly resets MagickInfos .
   */
  ret = define_imagemagick_type ();
  
  {
    const MagickInfo *info = magick_get_magick_info ("XPM", &magick_exception);
    if (info)
      {
	/* CAUTION: This code may cause a crash. */
	/* With ImageMagick-5.5.3, this code works well.  */  
	MagickInfo *info_new = magick_set_magick_info (info->name);

	info_new->decoder = XPM_read_image;
	info_new->encoder = info->encoder;
	info_new->magick = info->magick;
	info_new->description = magick_acquire_string (info->description);
 	info_new->module = magick_acquire_string (info->module);
	magick_register_magick_info (info_new);
      }
  }
  return ret;
}

DEFUN ("mw32-get-image-magick", Fmw32_get_image_magick,
       Smw32_get_image_magick, 1, 1, 0,
  "Searches for an image format that matches the specified magick string.")
     (string)
     Lisp_Object string;
{
  const char *format = magick_get_image_magick (SDATA (string),
						SBYTES (string));

  if (format == NULL)
    return Qnil;
  else
    return *imagemagick_image_type_lisp_symbol (format);
}

DEFUN ("mw32-get-image-magick-extensions", Fmw32_get_image_magick_extensions,
       Smw32_get_image_magick_extensions, 0, 0, 0,
  "Return a list of all image extensions which ImageMagick can handle.")
     ()
{
  Lisp_Object tail = Vimage_types;
  Lisp_Object result = Qnil;
  Lisp_Object type;
  
  while (tail != Qnil)
    {
      type = XCAR (tail);

      if (SYMBOLP (type))
	{
	  char *format = XSYMBOL (type)->name->data;
	  const MagickInfo *info = magick_get_magick_info (format,
							   &magick_exception);

	  if (info && info->magick)
	    {
	      type = Fdowncase (Fsymbol_name (type));
	      if (NILP (Fmember (type, result)))
		result = Fcons (type, result);  
	    }
	}
      tail = XCDR (tail);
    }
  return result;
}


/***********************************************************************
			      Algorithms
 ***********************************************************************/
/* Non-zero means draw a cross on images having `:conversion
   disabled'.  */

int cross_disabled_images;

/* Value is the intensity of the color whose red/green/blue values
   are R, G, and B.  */

#define COLOR_INTENSITY(R, G, B) ((2 * (R) + 3 * (G) + (B)) / 6)

/* Edge detection matrices for different edge-detection
   strategies.  */

static int emboss_matrix[9] = {
   /* x - 1	x	x + 1  */
        2,     -1,  	  0,		/* y - 1 */
       -1,      0,        1,		/* y     */
        0,      1,       -2		/* y + 1 */
};

static int laplace_matrix[9] = {
   /* x - 1	x	x + 1  */
        1,      0,  	  0,		/* y - 1 */
        0,      0,        0,		/* y     */
        0,      0,       -1		/* y + 1 */
};

static int
mw32_image_to_fullcolor (struct frame *f, struct image *img)
{
  HDC hCompatDC;
  HDC hdc = FRAME_HDC (f);
  HBITMAP hbmp;
  mw32_image* pmimg = &(img->mw32_img);
  BITMAPINFO *pbi, *pbmpinfo = pmimg->pbmpinfo;
  unsigned char *pbmpdata, *pdata;
  int imagesize = 0, lines = 0;
  
  if (pbmpinfo->bmiHeader.biBitCount == 32)
    return 1;

  hCompatDC = CreateCompatibleDC (hdc);
  hbmp = CreateDIBSection (hCompatDC, pbmpinfo, DIB_RGB_COLORS,
			   (void **)&pdata, NULL, 0);
  if (!hbmp)
    {
      DeleteDC (hCompatDC);
      return 0;
    }

  memcpy (pdata, pmimg->pbmpdata, pmimg->size);
  imagesize = img->height * bmp_get_bitmap_width_bytes (img->width, 32);
  pbmpdata = (unsigned char*) xmalloc (sizeof (unsigned char) * imagesize);

  pbi = (BITMAPINFO *) xmalloc (sizeof (BITMAPINFOHEADER));
  pbi->bmiHeader.biSize = sizeof (BITMAPINFOHEADER);
  pbi->bmiHeader.biWidth = pbmpinfo->bmiHeader.biWidth;
  pbi->bmiHeader.biHeight = pbmpinfo->bmiHeader.biHeight;
  pbi->bmiHeader.biPlanes = pbmpinfo->bmiHeader.biPlanes;
  pbi->bmiHeader.biBitCount = 32;
  pbi->bmiHeader.biCompression = BI_RGB;
  pbi->bmiHeader.biSizeImage = imagesize;
  pbi->bmiHeader.biXPelsPerMeter = pbmpinfo->bmiHeader.biXPelsPerMeter;
  pbi->bmiHeader.biYPelsPerMeter = pbmpinfo->bmiHeader.biYPelsPerMeter;
  pbi->bmiHeader.biClrUsed = 0;
  pbi->bmiHeader.biClrImportant = 0;

  lines = GetDIBits (hCompatDC, hbmp, 0, img->height,
		     pbmpdata, pbi, DIB_RGB_COLORS);
  DeleteObject (hbmp);
  DeleteDC (hCompatDC);
  if (!lines)
    {
      xfree (pbi);
      xfree (pbmpdata);
      return 0;
    }

  xfree (pmimg->pbmpdata);
  xfree (pmimg->pbmpinfo);

  pmimg->pbmpinfo = pbi;
  pmimg->size = imagesize;
  pmimg->pbmpdata = pbmpdata;

  return 1;
}

static void
mw32_cross_disabled_image (f, img)
     struct frame *f;
     struct image *img;
{
  HDC hdc = FRAME_HDC (f);
  HDC hCompatDC;
  HBITMAP hbmp, hbmpmask;
  mw32_image* pmimg = &(img->mw32_img);
  BITMAPINFO *pbmpinfo = pmimg->pbmpinfo;
  unsigned char *pdata;
  HGDIOBJ hold1, hold2;

  if (!cross_disabled_images) return;

  hCompatDC = CreateCompatibleDC (hdc);

  hbmp = CreateDIBSection (hCompatDC, pbmpinfo, DIB_RGB_COLORS,
			   (void **)&pdata, NULL, 0);
  if (!hbmp)
    {
      DeleteDC (hCompatDC);
      return;
    }
  memcpy (pdata, pmimg->pbmpdata, pmimg->size);

  hold1 = SelectObject (hCompatDC, hbmp);
  hold2 = SelectObject (hCompatDC, GetStockObject (BLACK_BRUSH));
  MoveToEx (hCompatDC, 0, 0, NULL);
  LineTo (hCompatDC, img->width, img->height);
  MoveToEx (hCompatDC, img->width, 0, NULL);
  LineTo (hCompatDC, 0, img->height);

  memcpy (pmimg->pbmpdata, pdata, pmimg->size);
  SelectObject (hCompatDC, hold2);
  SelectObject (hCompatDC, hold1);
  DeleteObject (hbmp);

  if (pmimg->pbmpmask)
    {
      struct {
	BITMAPINFOHEADER h;
	RGBQUAD c[2];
      } maskinfo;

      memset (&maskinfo, 0, sizeof (maskinfo));
      maskinfo.h.biSize = sizeof (BITMAPINFOHEADER);
      maskinfo.h.biWidth = pmimg->pbmpinfo->bmiHeader.biWidth;
      maskinfo.h.biHeight = pmimg->pbmpinfo->bmiHeader.biHeight;
      maskinfo.h.biPlanes = 1;
      maskinfo.h.biBitCount = 1;
      maskinfo.h.biCompression = BI_RGB;
      maskinfo.c[1].rgbRed = maskinfo.c[1].rgbGreen
	= maskinfo.c[1].rgbBlue = 255;

      hbmp = CreateDIBSection (hCompatDC, (LPBITMAPINFO) &maskinfo,
			       DIB_RGB_COLORS, (void **) &pdata,
			       NULL, 0);
      if (!(hbmp))
	{
	  DeleteDC (hCompatDC);
	  return;
	}
      memcpy (pdata, pmimg->pbmpmask, pmimg->mask_size);

      hold1 = SelectObject (hCompatDC, hbmp);
      hold2 = SelectObject (hCompatDC, GetStockObject (WHITE_BRUSH));
      MoveToEx (hCompatDC, 0, 0, NULL);
      LineTo (hCompatDC, img->width, img->height);
      MoveToEx (hCompatDC, img->width, 0, NULL);
      LineTo (hCompatDC, 0, img->height);
      memcpy (pmimg->pbmpmask, pdata, pmimg->mask_size);

      SelectObject (hCompatDC, hold2);
      SelectObject (hCompatDC, hold1);
      DeleteObject (hbmp);
    }

  DeleteDC (hCompatDC);
  return;
}

static void
mw32_disable_image (struct frame *f, struct image *img)
{
  const int h = 58;
  const int l = 117;
  RGBQUAD *bmpbit, *end, *p;
  int depth, size;

  if (!mw32_image_to_fullcolor (f, img))
    return;

  depth = img->mw32_img.pbmpinfo->bmiHeader.biBitCount;
  if (depth != 32)
    return;

  bmpbit = (RGBQUAD *) img->mw32_img.pbmpdata;
  size = img->height * img->width;
  end = bmpbit + size;

  for (p = bmpbit; p < end; p++)
    {
      int i = COLOR_INTENSITY (p->rgbBlue, p->rgbGreen, p->rgbRed);
      int i2 = (0xff - h - l) * i / 0xff + l;
      p->rgbBlue = p->rgbGreen = p->rgbRed = i2;
    }

  mw32_cross_disabled_image (f, img);
}

static void
mw32_detect_edges (struct frame *f, struct image *img,
		   int matrix[9], int color_adjust)
{
  int depth;
  RGBQUAD *pbmpdata;
  RGBQUAD *new, *p;
  int x, y, i, sum;

  if (!mw32_image_to_fullcolor (f, img))
    return;

  depth = img->mw32_img.pbmpinfo->bmiHeader.biBitCount;
  if (depth != 32)
    return;

  for (i = sum = 0; i < 9; ++i)
    sum += abs (matrix[i]);

  if (sum == 0)
    return;

  pbmpdata = (RGBQUAD *) img->mw32_img.pbmpdata;
  new = (RGBQUAD *) xmalloc (img->mw32_img.size);

#define COLOR(A, X, Y) ((A) + (Y) * img->width + (X))

  for (y = 0; y < img->height; ++y)
    {
      p = COLOR (new, 0, y);
      p->rgbRed = p->rgbGreen = p->rgbBlue = 0xff/2;
      p = COLOR (new, img->width - 1, y);
      p->rgbRed = p->rgbGreen = p->rgbBlue = 0xff/2;
    }

  for (x = 1; x < img->width - 1; ++x)
    {
      p = COLOR (new, x, 0);
      p->rgbRed = p->rgbGreen = p->rgbBlue = 0xff/2;
      p = COLOR (new, x, img->height - 1);
      p->rgbRed = p->rgbGreen = p->rgbBlue = 0xff/2;
    }

  for (y = 1; y < img->height - 1; ++y)
    {
      p = COLOR (new, 1, y);
      
      for (x = 1; x < img->width - 1; ++x, ++p)
	{
	  int r, g, b, y1, x1;

	  r = g = b = i = 0;
	  for (y1 = y - 1; y1 < y + 2; ++y1)
	    for (x1 = x - 1; x1 < x + 2; ++x1, ++i)
	      if (matrix[i])
	        {
	          RGBQUAD *t = COLOR (pbmpdata, x1, y1);
		  r += matrix[i] * t->rgbRed;
		  g += matrix[i] * t->rgbGreen;
		  b += matrix[i] * t->rgbBlue;
		}

	  r = (r / sum + (color_adjust / 256)) & 0xff;
	  g = (g / sum + (color_adjust / 256)) & 0xff;
	  b = (b / sum + (color_adjust / 256)) & 0xff;
	  p->rgbRed = p->rgbGreen = p->rgbBlue = COLOR_INTENSITY (r, g, b);
	}
    }

  xfree (pbmpdata);
  img->mw32_img.pbmpdata = (unsigned char *) new;

#undef COLOR

}

static void
mw32_laplace (struct frame *f, struct image *img)
{
  mw32_detect_edges (f, img, laplace_matrix, 45000);
}

static void
mw32_emboss (struct frame *f, struct image *img)
{
  mw32_detect_edges (f, img, emboss_matrix, 0xffff / 2);
}

static void
mw32_edge_detection (struct frame *f, struct image *img,
		     Lisp_Object matrix, Lisp_Object color_adjust)
{
  int i = 0;
  int trans[9];
  
  if (CONSP (matrix))
    {
      for (i = 0;
	   i < 9 && CONSP (matrix) && NUMBERP (XCAR (matrix));
	   ++i, matrix = XCDR (matrix))
	trans[i] = (int) XFLOATINT (XCAR (matrix));
    }
  else if (VECTORP (matrix) && ASIZE (matrix) >= 9)
    {
      for (i = 0; i < 9 && NUMBERP (AREF (matrix, i)); ++i)
	trans[i] = (int) XFLOATINT (AREF (matrix, i));
    }

  if (NILP (color_adjust))
    color_adjust = make_number (0xffff / 2);

  if (i == 9 && NUMBERP (color_adjust))
    mw32_detect_edges (f, img, trans, (int) XFLOATINT (color_adjust));
}


/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* If non-null, an asynchronous timer that, when it expires, displays
   an hourglass cursor on all frames.  */

static struct atimer *hourglass_atimer;

/* Non-zero means an hourglass cursor is currently shown.  */

static int hourglass_shown_p;

/* Number of seconds to wait before displaying an hourglass cursor.  */

static Lisp_Object Vhourglass_delay;

/* Default number of seconds to wait before displaying an hourglass
   cursor.  */

#define DEFAULT_HOURGLASS_DELAY 1

/* Function prototypes.  */

static void show_hourglass P_ ((struct atimer *));
static void hide_hourglass P_ ((void));


/* Cancel a currently active hourglass timer, and start a new one.  */

void
start_hourglass (void)
{
  EMACS_TIME delay;
  int secs, usecs = 0;
  
  cancel_hourglass ();

  if (INTEGERP (Vhourglass_delay)
      && XINT (Vhourglass_delay) > 0)
    secs = XFASTINT (Vhourglass_delay);
  else if (FLOATP (Vhourglass_delay)
	   && XFLOAT_DATA (Vhourglass_delay) > 0)
    {
      Lisp_Object tem;
      tem = Ftruncate (Vhourglass_delay, Qnil);
      secs = XFASTINT (tem);
      usecs = (int) ((XFLOAT_DATA (Vhourglass_delay) - secs) * 1000000);
    }
  else
    secs = DEFAULT_HOURGLASS_DELAY;
  
  EMACS_SET_SECS_USECS (delay, secs, usecs);
  hourglass_atimer = start_atimer (ATIMER_RELATIVE, delay,
				   show_hourglass, NULL);
}


/* Cancel the hourglass cursor timer if active, hide a busy cursor if
   shown.  */

void
cancel_hourglass (void)
{
  if (hourglass_atimer)
    {
      cancel_atimer (hourglass_atimer);
      hourglass_atimer = NULL;
    }
  
  if (hourglass_shown_p)
    hide_hourglass ();
}


/* Timer function of hourglass_atimer.  TIMER is equal to
   hourglass_atimer.

   Display an hourglass pointer on all frames by mapping the frames'
   hourglass_window.  Set the hourglass_p flag in the frames'
   output_data.mw32 structure to indicate that an hourglass cursor is
   shown on the frames.  */

static void
show_hourglass (struct atimer *timer)
{
  /* The timer implementation will cancel this timer automatically
     after this function has run.  Set hourglass_atimer to null
     so that we know the timer doesn't have to be canceled.  */
  hourglass_atimer = NULL;

  if (!hourglass_shown_p)
    {
      Lisp_Object rest, frame;
  
      BLOCK_INPUT;
  
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);
	  
	  f->output_data.mw32->hourglass_p = 1;
	  if (FRAME_LIVE_P (f) && FRAME_MW32_P (f))
	    {
	      if (FRAME_OUTER_WINDOW (f))
		{
		  /* TODO: SHOW hourglass mouse cursor.  */
		}
	    }
	}

      hourglass_shown_p = 1;
      UNBLOCK_INPUT;
    }
}


/* Hide the hourglass pointer on all frames, if it is currently
   shown.  */

static void
hide_hourglass (void)
{
  if (hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);
      
	  if (FRAME_MW32_P (f)
	      /* Watch out for newly created frames.  */
	      && f->output_data.mw32->hourglass_p)
	    {
	      /* hide hourglass mouse cursor.  */
	      f->output_data.mw32->hourglass_p = 0;
	    }
	}

      hourglass_shown_p = 0;
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object mw32_create_tip_frame P_ ((struct mw32_display_info *,
					      Lisp_Object, Lisp_Object));
static void compute_tip_xy P_ ((struct frame *, Lisp_Object, Lisp_Object,
				Lisp_Object, int, int, int *, int *));

/* The frame of a currently visible tooltip.  */

Lisp_Object tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

Lisp_Object tip_timer;
Window tip_window;

/* If non-nil, a vector of 3 elements containing the last args
   with which x-show-tip was called.  See there.  */

Lisp_Object last_show_tip_args;

/* Maximum size for tooltips; a cons (COLUMNS . ROWS).  */

Lisp_Object Vmw32_max_tooltip_size;


static Lisp_Object
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = NULL;
      tip_frame = Qnil;
    }

  return deleted;
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  TEXT is the string to
   display in the tip frame.  Value is the frame.

   Note that functions called here, esp. x_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
mw32_create_tip_frame (struct mw32_display_info *dpyinfo,
		       Lisp_Object parms, Lisp_Object text)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  long window_prompting = 0;
  int width, height;
  int count = BINDING_STACK_SIZE ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct kboard *kb;
  int face_change_count_before = face_change_count;
  Lisp_Object buffer;
  struct buffer *old_buffer;

  check_mw32 ();

  /* Use this general default value to start with until we know if
     this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  /* Get the name of the frame to use for resource lookup.  */
  name = mw32_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");
  Vx_resource_name = name;

  frame = Qnil;
  GCPRO3 (parms, name, frame);
  /* Make a frame without minibuffer nor mode-line.  */
  f = make_frame (0);
  f->wants_modeline = 0;
  XSETFRAME (frame, f);

  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  current_buffer->truncate_lines = Qnil;
  Ferase_buffer ();
  Finsert (1, &text);
  set_buffer_internal_1 (old_buffer);

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  record_unwind_protect (unwind_create_tip_frame, frame);

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_mw32;
  f->output_data.mw32 =
    (struct mw32_output *) xmalloc (sizeof (struct mw32_output));
  bzero (f->output_data.mw32, sizeof (struct mw32_output));

  f->output_data.mw32->icon_bitmap = -1;
  f->output_data.mw32->fontset = -1;
  f->output_data.mw32->scroll_bar_foreground_pixel = -1;
  f->output_data.mw32->scroll_bar_background_pixel = -1;
  /* all handles must be set to INVALID_HANLE_VALUE.  */
  f->output_data.mw32->menubar_handle = INVALID_HANDLE_VALUE;
  f->output_data.mw32->hdc = INVALID_HANDLE_VALUE;
  f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;

  FRAME_MW32_DISPLAY_INFO (f) = dpyinfo;
  f->icon_name = Qnil;

#if 0 /* GLYPH_DEBUG TODO: image support.  */
  image_cache_refcount = FRAME_X_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

  f->output_data.mw32->parent_desc = FRAME_MW32_DISPLAY_INFO (f)->root_window;
  f->output_data.mw32->explicit_parent = 0;
  f->output_data.mw32->dwStyle = WS_BORDER | WS_POPUP | WS_DISABLED;

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->mw32_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font, fontset;
    int use_default_font_p = 1;

    tem = Qnil;
      
    font = mw32_get_arg (dpyinfo, parms, Qfont,
			 "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
	fontset = Fquery_fontset (font, Qnil);
	if (STRINGP (fontset))
	  tem = mw32_new_fontset (f, SDATA (fontset));
	else
	  tem = mw32_new_font (f, SDATA (font));

	if (STRINGP (tem))
	  use_default_font_p = 0;
	else
	  message ("Cannot select tooltip font: `%s'", SDATA (font));
      }

    /* Next, try default font.  */
    if (!STRINGP (tem))
      {
	font = build_string ("default");
	fontset = Fquery_fontset (font, Qnil);
	if (STRINGP (fontset))
	  tem = mw32_new_fontset (f, SDATA (fontset));
	else
	  tem = mw32_new_font (f, SDATA (font));
      }
    UNBLOCK_INPUT;
    /* In case of default font, second argument must be nil.  */
    mw32_default_parameter (f, use_default_font_p ? Qnil : parms,
			    Qfont, tem, "font", "Font", RES_TYPE_STRING);
  }
  
  mw32_default_parameter (f, parms, Qborder_width, make_number (2),
			  "borderWidth", "BorderWidth", RES_TYPE_NUMBER);
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = mw32_get_arg (dpyinfo, parms, Qinternal_border_width,
			    "internalBorder", "internalBorder",
			    RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  mw32_default_parameter (f, parms, Qinternal_border_width, make_number (1),
			  "internalBorderWidth", "internalBorderWidth",
			  RES_TYPE_NUMBER);

  /* Also do the stuff which must be set before the window exists.  */
  mw32_default_parameter (f, parms, Qforeground_color, build_string ("black"),
			  "foreground", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qbackground_color, build_string ("white"),
			  "background", "Background", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qmouse_color, build_string ("black"),
			  "pointerColor", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qcursor_color, build_string ("black"),
			  "cursorColor", "Foreground", RES_TYPE_STRING);
  mw32_default_parameter (f, parms, Qborder_color, build_string ("black"),
			  "borderColor", "BorderColor", RES_TYPE_STRING);

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);

  window_prompting = mw32_figure_window_size (f, parms);

#if 0 /* necessary? */
  /* No fringes on tip frame.  */
  f->output_data.mw32->fringes_extra = 0;
  f->output_data.mw32->fringe_cols = 0;
  f->output_data.mw32->left_fringe_width = 0;
  f->output_data.mw32->right_fringe_width = 0;
#endif

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.mw32->win_gravity = SouthEastGravity;
      else
	f->output_data.mw32->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.mw32->win_gravity = SouthWestGravity;
      else
	f->output_data.mw32->win_gravity = NorthWestGravity;
    }

  f->output_data.mw32->size_hint_flags = window_prompting;

  BLOCK_INPUT;
  tip_window = FRAME_MW32_WINDOW (f) = mw32_create_tip_window (f);
  UNBLOCK_INPUT;

  mw32_default_parameter (f, parms, Qauto_raise, Qnil,
			  "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  mw32_default_parameter (f, parms, Qauto_lower, Qnil,
			  "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height.  */
  width = f->width;
  height = f->height;
  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0, 0);

  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, intern ("tooltip"))))
    Fmodify_frame_parameters (frame, Fcons (Fcons (intern ("tooltip"), Qt),
					    Qnil));

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame get's set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    /* Set tip_frame here, so that */
    tip_frame = frame;
    call1 (Qface_set_after_frame_default, frame);

    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qbackground_color, bg),
					      Qnil));
  }

  f->no_split = 1;

  UNGCPRO;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);

  /* Now that the frame is official, it counts as a reference to
     its display.  */
  FRAME_MW32_DISPLAY_INFO (f)->reference_count++;

  /* Setting attributes of faces of the tooltip frame from resources
     and similar will increment face_change_count, which leads to the
     clearing of all current matrices.  Since this isn't necessary
     here, avoid it by resetting face_change_count to the value it
     had before we created the tip frame.  */
  face_change_count = face_change_count_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}


/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx,
		Lisp_Object dy, int width, int height,
		int *root_x, int *root_y)
{
  Lisp_Object left, top;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if (!INTEGERP (left) || !INTEGERP (top))
    {
      POINT pt;

      BLOCK_INPUT;
      GetCursorPos (&pt);
      *root_x = pt.x;
      *root_y = pt.y;
      UNBLOCK_INPUT;
    }

  if (INTEGERP (top))
    *root_y = XINT (top);
  else if (*root_y + XINT (dy) - height < 0)
    *root_y -= XINT (dy);
  else
    {
      *root_y -= height;
      *root_y += XINT (dy);
    }

  if (INTEGERP (left))
    *root_x = XINT (left);
  else if (*root_x + XINT (dx) + width <= FRAME_MW32_DISPLAY_INFO (f)->width)
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = 0;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a \"tooltip\" window on frame FRAME.
A tooltip window is a small window displaying a string.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be
used to change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARAMS contains a `left' parameter,
the tooltip is displayed at that x-position.  Otherwise it is
displayed at the mouse position, with offset DX added (default is 5 if
DX isn't specified).  Likewise for the y-position; if a `top' frame
parameter is specified, it determines the y-position of the tooltip
window, otherwise it is displayed at the mouse position, with offset
DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
  (string, frame, parms, timeout, dx, dy)
     Lisp_Object string, frame, parms, timeout, dx, dy;
{
  struct frame *f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int i, width, height;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  int count = BINDING_STACK_SIZE ();

  specbind (Qinhibit_redisplay, Qt);

  GCPRO4 (string, parms, frame, timeout);

  CHECK_STRING (string, 0);
  f = check_mw32_frame (frame);
  if (NILP (timeout))
    timeout = make_number (5);
  else
    CHECK_NATNUM (timeout, 0);

  if (NILP (dx))
    dx = make_number (5);
  else
    CHECK_NUMBER (dx, 0);

  if (NILP (dy))
    dy = make_number (-10);
  else
    CHECK_NUMBER (dy, 0);

  if (NILP (last_show_tip_args))
    last_show_tip_args = Fmake_vector (make_number (3), Qnil);

  if (!NILP (tip_frame))
    {
      Lisp_Object last_string = AREF (last_show_tip_args, 0);
      Lisp_Object last_frame = AREF (last_show_tip_args, 1);
      Lisp_Object last_parms = AREF (last_show_tip_args, 2);

      if (EQ (frame, last_frame)
	  && !NILP (Fequal (last_string, string))
	  && !NILP (Fequal (last_parms, parms)))
	{
	  struct frame *f = XFRAME (tip_frame);

	  /* Only DX and DY have changed.  */
	  if (!NILP (tip_timer))
	    {
	      Lisp_Object timer = tip_timer;
	      tip_timer = Qnil;
	      call1 (Qcancel_timer, timer);
	    }

	  BLOCK_INPUT;
	  compute_tip_xy (f, parms, dx, dy, PIXEL_WIDTH (f),
			  PIXEL_HEIGHT (f), &root_x, &root_y);

	  /* Put tooltip in topmost group and in position.  */
	  SetWindowPos (FRAME_MW32_WINDOW (f), HWND_TOPMOST,
			root_x, root_y, 0, 0,
			SWP_NOSIZE | SWP_NOACTIVATE);
	  UNBLOCK_INPUT;
	  goto start_timer;
	}
    }

  /* Hide a previous tip, if any.  */
  Fx_hide_tip ();

  ASET (last_show_tip_args, 0, string);
  ASET (last_show_tip_args, 1, frame);
  ASET (last_show_tip_args, 2, parms);

  /* Add default values to frame parameters.  */
  if (NILP (Fassq (Qname, parms)))
    parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
  if (NILP (Fassq (Qinternal_border_width, parms)))
    parms = Fcons (Fcons (Qinternal_border_width, make_number (3)), parms);
  if (NILP (Fassq (Qborder_width, parms)))
    parms = Fcons (Fcons (Qborder_width, make_number (1)), parms);
  if (NILP (Fassq (Qborder_color, parms)))
    parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
  if (NILP (Fassq (Qbackground_color, parms)))
    parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		   parms);

  /* Block input until the tip has been fully drawn, to avoid crashes
     when drawing tips in menus.  */
  BLOCK_INPUT;

  /* Create a frame for the tooltip, and record it in the global
     variable tip_frame.  */
  frame = mw32_create_tip_frame (FRAME_MW32_DISPLAY_INFO (f), parms, string);
  f = XFRAME (frame);

  /* Set up the frame's root window.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left = w->top = make_number (0);

  if (CONSP (Vmw32_max_tooltip_size)
      && INTEGERP (XCAR (Vmw32_max_tooltip_size))
      && XINT (XCAR (Vmw32_max_tooltip_size)) > 0
      && INTEGERP (XCDR (Vmw32_max_tooltip_size))
      && XINT (XCDR (Vmw32_max_tooltip_size)) > 0)
    {
      w->width = XCAR (Vmw32_max_tooltip_size);
      w->height = XCDR (Vmw32_max_tooltip_size);
    }
  else
    {
      w->width = make_number (80);
      w->height = make_number (40);
    }

  f->window_width = XINT (w->width);
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (XWINDOW (FRAME_ROOT_WINDOW (f))->buffer));

  current_buffer->truncate_lines = Qnil;
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (FRAME_ROOT_WINDOW (f), pos);

  /* Compute width and height of the tooltip.  */
  width = height = 0;
  for (i = 0; i < w->desired_matrix->nrows; ++i)
    {
      struct glyph_row *row = &w->desired_matrix->rows[i];
      struct glyph *last;
      int row_width;

      /* Stop at the first empty row at the end.  */
      if (!row->enabled_p || !row->displays_text_p)
	break;

      /* Let the row go over the full width of the frame.  */
      row->full_width_p = 1;

      row_width = row->pixel_width;
      
      /* There's a glyph at the end of rows that is use to place
	 the cursor there.  Don't include the width of this glyph.  */
      if (row->used[TEXT_AREA] && i != w->desired_matrix->nrows - 1)
	{
	  /* Check if the next row is empty.  */
	  struct glyph_row *row_next = &w->desired_matrix->rows[i + 1];
	  if (!row_next->enabled_p || !row_next->displays_text_p)
	    {
	      last = &row->glyphs[TEXT_AREA][row->used[TEXT_AREA] - 1];
	      row_width -= last->pixel_width;
	    }
	}

      /* TODO: find why tips do not draw along baseline as instructed.  */
      height += row->height;
      width = max (width, row_width);
    }

  /* Add the frame's internal border to the width and height the X
     window should have.  */
  height += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);
  width += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);

  {
    /* Adjust Window size to take border into account.  */
    RECT rect;
    rect.left = rect.top = 0;
    rect.right = width;
    rect.bottom = height;
    AdjustWindowRect (&rect, f->output_data.mw32->dwStyle,
		      FRAME_EXTERNAL_MENU_BAR (f));

    /* Position and size tooltip, and put it in the topmost group.  */
    SetWindowPos (FRAME_MW32_WINDOW (f), HWND_TOPMOST,
		  root_x, root_y, rect.right - rect.left,
		  rect.bottom - rect.top, SWP_NOACTIVATE);

    /* Let redisplay know that we have made the frame visible already.  */
    /* f->async_visible = 1; /* CAUTION: Don't make this flag on here */
    
    ShowWindow (FRAME_MW32_WINDOW (f), SW_SHOWNOACTIVATE);
  }

  /* Draw into the window.  */
  w->must_be_updated_p = 1;
  update_single_window (w, 1);

  UNBLOCK_INPUT;

  /* Restore original current buffer.  */
  set_buffer_internal_1 (old_buffer);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  UNGCPRO;
  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
  ()
{
  int count;
  Lisp_Object deleted, frame, timer;
  struct gcpro gcpro1, gcpro2;

  /* Return quickly if nothing to do.  */
  if (NILP (tip_timer) && NILP (tip_frame))
    return Qnil;

  frame = tip_frame;
  timer = tip_timer;
  GCPRO2 (frame, timer);
  tip_frame = tip_timer = deleted = Qnil;

  count = BINDING_STACK_SIZE ();
  specbind (Qinhibit_redisplay, Qt);
  specbind (Qinhibit_quit, Qt);

  if (!NILP (timer))
    call1 (Qcancel_timer, timer);

  if (FRAMEP (frame))
    {
      Fdelete_frame (frame, Qnil);
      deleted = Qt;
    }

  UNGCPRO;
  return unbind_to (count, deleted);
}

/***********************************************************************
			File selection dialog
 ***********************************************************************/
#include <dlgs.h>
#define FILE_NAME_TEXT_FIELD edt1

extern Lisp_Object Qfile_name_history;

/* Callback for altering the behaviour of the Open File dialog.
   Makes the Filename text field contain "Current Directory" and be
   read-only when "Directories" is selected in the filter.  This
   allows us to work around the fact that the standard Open File
   dialog does not support directories.  */
static UINT CALLBACK
file_dialog_callback (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  if (msg == WM_NOTIFY)
    {
      OFNOTIFY * notify = (OFNOTIFY *) lParam;
      /* Detect when the Filter dropdown is changed.  */
      if (notify->hdr.code == CDN_TYPECHANGE 
	  || notify->hdr.code == CDN_INITDONE)
	{
	  HWND dialog = GetParent (hwnd);
	  HWND edit_control = GetDlgItem (dialog, FILE_NAME_TEXT_FIELD);

	  /* Directories is in index 2.  */
	  if (notify->lpOFN->nFilterIndex == 2)
	    {
	      CommDlg_OpenSave_SetControlText (dialog, FILE_NAME_TEXT_FIELD,
					       "Current Directory");
	      EnableWindow (edit_control, FALSE);
	    }
	  else
	    {
	      CommDlg_OpenSave_SetControlText (dialog, FILE_NAME_TEXT_FIELD,
					       "");
	      EnableWindow (edit_control, TRUE);
	    }
	}
    }
  return 0;
}

DEFUN ("mw32-file-dialog", Fmw32_file_dialog, Smw32_file_dialog, 2, 4, 0,
  "Read file name, prompting with PROMPT in directory DIR.\n\
Use a file selection dialog.\n\
Select DEFAULT-FILENAME in the dialog's file selection box, if\n\
specified.  Don't let the user enter a file name in the file\n\
selection dialog's entry field, if MUSTMATCH is non-nil.")
  (prompt, dir, default_filename, mustmatch)
     Lisp_Object prompt, dir, default_filename, mustmatch;
{
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  LPTSTR init_dir;
  TCHAR filename[MAX_PATH + 1];
  int filter_index = 1;
  OPENFILENAME file_details;

  GCPRO5 (prompt, dir, default_filename, mustmatch, file);
  CHECK_STRING (prompt, 0);
  CHECK_STRING (dir, 1);

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  init_dir = mw32_encode_lispy_string (Vw32_system_coding_system,
				       Funix_to_dos_filename (dir), NULL);

  if (STRINGP (default_filename))
    {
      file = Ffile_name_nondirectory (Funix_to_dos_filename (default_filename));
      if (LISPY_STRING_BYTES (file) > 0)
	{
	  int size;
	  LPTSTR filename_tmp;

	  filename_tmp = mw32_encode_lispy_string (Vw32_system_coding_system, file, &size);
	  memcpy (filename, filename_tmp, size);
	  filename[size / sizeof (filename[0])] = '\0';
	}
      else
	{
	  /* If default_file_name is a directory, filter for directory
	     is selected. */
	  filename[0] = '\0';
	  filter_index = 2;	/* 2 means file is a directory */
	}
    }
  else
    filename[0] = '\0';

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  BLOCK_INPUT;

  bzero (&file_details, sizeof (file_details));
  file_details.lStructSize = sizeof (file_details);
  file_details.hwndOwner = FRAME_MW32_WINDOW (f);
  /* Undocumented Bug in Common File Dialog:
     If a filter is not specified, shell links are not resolved.  */
  file_details.lpstrFilter = "All Files (*.*)\0*.*\0Directories\0*|*\0\0";
  file_details.nFilterIndex = filter_index;
  file_details.lpstrFile = filename;
  file_details.nMaxFile = sizeof (filename) / sizeof (filename[0]);
  file_details.lpstrInitialDir = init_dir;
  file_details.lpstrTitle
    = mw32_encode_lispy_string (Vw32_system_coding_system,
				prompt, NULL);
  file_details.Flags = (OFN_HIDEREADONLY | OFN_NOCHANGEDIR
			| OFN_EXPLORER | OFN_ENABLEHOOK);

  if (!NILP (mustmatch))
    file_details.Flags |= OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;

  file_details.lpfnHook = (LPOFNHOOKPROC) file_dialog_callback;

  if (GetOpenFileName (&file_details))
    {
      file = mw32_decode_lispy_string (Vw32_system_coding_system,
				       filename, 0);
      file = Fdos_to_unix_filename (file);
      if (file_details.nFilterIndex == 2)
	{
	  /* "Folder Only" selected - strip dummy file name.  */
	  file = Ffile_name_directory (file);
	}
    }
  else
    file = Qnil;

  UNBLOCK_INPUT;
  file = unbind_to (count, file);

  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, file);
}

/***********************************************************************
                         w32 specialized functions
 ***********************************************************************/
static Lisp_Object Qwm_syscommand;
static Lisp_Object Qsc_keymenu;
static Lisp_Object Qsc_monitorpower;
static Lisp_Object Qsc_tasklist;
static Lisp_Object Qsc_maximize;
static Lisp_Object Qsc_restore;
static Lisp_Object Qlow_power;
static Lisp_Object Qshut_off;

DEFUN ("w32-access-windows-intrinsic-facility",
       Fw32_access_windows_intrinsic_facility,
       Sw32_access_windows_intrinsic_facility, 2, 4, 0,
       "This function is internal use only.  Don't call it directly.\n\
\n\
When CATEGORY is WM-SYSCOMMAND, you can choose\n\
   SC-KEYMENU,\n\
   SC-MONITORPOWER,\n\
   SC-TASKLIST,\n\
   SC-MAXIMIZE,\n\
   SC-RESTORE,\n\
as <CATEGORY-DEPENDENT-ARG>.")
  (category, category_dependent_args, optional_args, frame)
     Lisp_Object category, category_dependent_args, optional_args, frame;
{
  FRAME_PTR f = check_mw32_frame (NILP (frame) ? selected_frame : frame);

  if (EQ (category, Qwm_syscommand))
    {
      WPARAM command;
      LPARAM arg = 0;

      if (EQ (category_dependent_args, Qsc_keymenu))
	command = SC_KEYMENU;
      else if (EQ (category_dependent_args, Qsc_monitorpower))
	{
	  command = SC_MONITORPOWER;
	  
	  if (SYMBOLP (optional_args))
	    {
	      if (EQ (optional_args, Qlow_power))
		arg = 1;
	      else if (EQ (optional_args, Qshut_off))
		arg = 2;
	    }
	  if (arg == 0)
	    error ("Invalid optional argument for sc-monitorpower: %s",
		   XSYMBOL (optional_args)->name->data);
	}
      else if (EQ (category_dependent_args, Qsc_tasklist))
	command = SC_TASKLIST;
      else if (EQ (category_dependent_args, Qsc_maximize))
	command = SC_MAXIMIZE;
      else if (EQ (category_dependent_args, Qsc_restore))
	command = SC_RESTORE;
      else 
	error ("Invalid command for wm-syscommand: `%s'",
	       XSYMBOL (category_dependent_args)->name->data);

      PostMessage (FRAME_MW32_WINDOW (f), WM_SYSCOMMAND, command, arg);
    }
  else
    error ("Invalid category: `%s'", XSYMBOL (category)->name->data);

  return Qnil;
}

DEFUN ("file-system-info", Ffile_system_info, Sfile_system_info, 1, 1, 0,
  "Return storage information about the file system FILENAME is on.\n\
Value is a list of floats (TOTAL FREE AVAIL), where TOTAL is the total\n\
storage of the file system, FREE is the free storage, and AVAIL is the\n\
storage available to a non-superuser.  All 3 numbers are in bytes.\n\
If the underlying system call fails, value is nil.")
  (filename)
  Lisp_Object filename;
{
  Lisp_Object encoded, value;

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);
  encoded = ENCODE_FILE (filename);

  value = Qnil;

  /* Determining the required information on Windows turns out, sadly,
     to be more involved than one would hope.  The original Win32 api
     call for this will return bogus information on some systems, but we
     must dynamically probe for the replacement api, since that was
     added rather late on.  */
  {
    HMODULE hKernel = GetModuleHandle ("kernel32");
    BOOL (*pfn_GetDiskFreeSpaceEx)
      (char *, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER)
      = (void *) GetProcAddress (hKernel, "GetDiskFreeSpaceEx");

    /* On Windows, we may need to specify the root directory of the
       volume holding FILENAME.  */
    char rootname[MAX_PATH];
    char *name = SDATA (encoded);

    /* find the root name of the volume if given */
    if (isalpha (name[0]) && name[1] == ':')
      {
	rootname[0] = name[0];
	rootname[1] = name[1];
	rootname[2] = '\\';
	rootname[3] = 0;
      }
    else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
      {
	char *str = rootname;
	int slashes = 4;
	do
	  {
	    if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	      break;
	    *str++ = *name++;
	  }
	while ( *name );

	*str++ = '\\';
	*str = 0;
      }

    if (pfn_GetDiskFreeSpaceEx)
      {
	LARGE_INTEGER availbytes;
	LARGE_INTEGER freebytes;
	LARGE_INTEGER totalbytes;

	if (pfn_GetDiskFreeSpaceEx (rootname,
				    (ULARGE_INTEGER *) &availbytes,
				    (ULARGE_INTEGER *) &totalbytes,
				    (ULARGE_INTEGER *) &freebytes))
	  value = list3 (make_float ((double) totalbytes.QuadPart),
			 make_float ((double) freebytes.QuadPart),
			 make_float ((double) availbytes.QuadPart));
      }
    else
      {
	DWORD sectors_per_cluster;
	DWORD bytes_per_sector;
	DWORD free_clusters;
	DWORD total_clusters;

	if (GetDiskFreeSpace (rootname,
			      &sectors_per_cluster,
			      &bytes_per_sector,
			      &free_clusters,
			      &total_clusters))
	  value = list3 (make_float ((double) total_clusters
				     * sectors_per_cluster * bytes_per_sector),
			 make_float ((double) free_clusters
				     * sectors_per_cluster * bytes_per_sector),
			 make_float ((double) free_clusters
				     * sectors_per_cluster
				     * bytes_per_sector));
      }
  }

  return value;
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

void
reinit_syms_of_mw32fns (void)
{
  /* This is zero if not using X windows.  */
  mw32_open = 0;
}

void
syms_of_mw32fns (void)
{
  reinit_syms_of_mw32fns ();

  /* The section below is built by the lisp expression at the top of the file,
     just above where these variables are declared.  */
  /*&&& init symbols here &&&*/
  Qauto_raise = intern ("auto-raise");
  staticpro (&Qauto_raise);
  Qauto_lower = intern ("auto-lower");
  staticpro (&Qauto_lower);
  Qbar = intern ("bar");
  staticpro (&Qbar);
  Qcaret = intern ("caret");
  staticpro (&Qcaret);
  Qcheckered_caret = intern ("checkered-caret");
  staticpro (&Qcheckered_caret);
  Qhairline_caret = intern ("hairline-caret");
  staticpro (&Qhairline_caret);
  Qborder_color = intern ("border-color");
  staticpro (&Qborder_color);
  Qborder_width = intern ("border-width");
  staticpro (&Qborder_width);
  Qbox = intern ("box");
  staticpro (&Qbox);
  Qcursor_color = intern ("cursor-color");
  staticpro (&Qcursor_color);
  Qcursor_type = intern ("cursor-type");
  staticpro (&Qcursor_type);
  Qcursor_height = intern ("cursor-height");
  staticpro (&Qcursor_height);
  Qgeometry = intern ("geometry");
  staticpro (&Qgeometry);
  Qicon_left = intern ("icon-left");
  staticpro (&Qicon_left);
  Qicon_top = intern ("icon-top");
  staticpro (&Qicon_top);
  Qicon_type = intern ("icon-type");
  staticpro (&Qicon_type);
  Qicon_name = intern ("icon-name");
  staticpro (&Qicon_name);
  Qinternal_border_width = intern ("internal-border-width");
  staticpro (&Qinternal_border_width);
  Qleft = intern ("left");
  staticpro (&Qleft);
  Qright = intern ("right");
  staticpro (&Qright);
  Qmouse_color = intern ("mouse-color");
  staticpro (&Qmouse_color);
  Qnone = intern ("none");
  staticpro (&Qnone);
  Qparent_id = intern ("parent-id");
  staticpro (&Qparent_id);
  Qscroll_bar_width = intern ("scroll-bar-width");
  staticpro (&Qscroll_bar_width);
  Qsuppress_icon = intern ("suppress-icon");
  staticpro (&Qsuppress_icon);
  Qundefined_color = intern ("undefined-color");
  staticpro (&Qundefined_color);
  Qvertical_scroll_bars = intern ("vertical-scroll-bars");
  staticpro (&Qvertical_scroll_bars);
  Qvisibility = intern ("visibility");
  staticpro (&Qvisibility);
  Qwindow_id = intern ("window-id");
  staticpro (&Qwindow_id);
  Qouter_window_id = intern ("outer-window-id");
  staticpro (&Qouter_window_id);
  Qmw32_frame_parameter = intern ("mw32-frame-parameter");
  staticpro (&Qmw32_frame_parameter);
  Qx_resource_name = intern ("x-resource-name");
  staticpro (&Qx_resource_name);
  Quser_position = intern ("user-position");
  staticpro (&Quser_position);
  Quser_size = intern ("user-size");
  staticpro (&Quser_size);
  Qime_font = intern ("ime-font");
  staticpro (&Qime_font);
  Qscroll_bar_foreground = intern ("scroll-bar-foreground");
  staticpro (&Qscroll_bar_foreground);
  Qscroll_bar_background = intern ("scroll-bar-background");
  staticpro (&Qscroll_bar_background);
  Qscreen_gamma = intern ("screen-gamma");
  staticpro (&Qscreen_gamma);
  Qline_spacing = intern ("line-spacing");
  staticpro (&Qline_spacing);
  Qcenter = intern ("center");
  staticpro (&Qcenter);
  Qcompound_text = intern ("compound-text");
  staticpro (&Qcompound_text);
  Qcancel_timer = intern ("cancel-timer");
  staticpro (&Qcancel_timer);
  /* This is the end of symbol initialization.  */

  /* Text property `display' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qdisplay, Qt), Vtext_property_default_nonsticky);


  Qlaplace = intern ("laplace");
  staticpro (&Qlaplace);
  Qemboss = intern ("emboss");
  staticpro (&Qemboss);
  Qedge_detection = intern ("edge-detection");
  staticpro (&Qedge_detection);
  Qheuristic = intern ("heuristic");
  staticpro (&Qheuristic);
  QCmatrix = intern (":matrix");
  staticpro (&QCmatrix);
  QCcolor_adjustment = intern (":color-adjustment");
  staticpro (&QCcolor_adjustment);
  QCmask = intern (":mask");
  staticpro (&QCmask);
 
  Qface_set_after_frame_default = intern ("face-set-after-frame-default");
  staticpro (&Qface_set_after_frame_default);

  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  DEFVAR_LISP ("w32-color-map", &Vmw32_color_map,
	       "A array of color name mappings for windows.");
  Vmw32_color_map = Qnil;

  init_mw32_parm_symbols ();

  DEFVAR_BOOL ("cross-disabled-images", &cross_disabled_images,
    "Non-nil means always draw a cross over disabled images.\n\
Disabled images are those having an `:conversion disabled' property.\n\
A cross is always drawn on black & white displays.");
  cross_disabled_images = 0;

  DEFVAR_LISP ("mw32-bitmap-file-path", &Vmw32_bitmap_file_path,
	       "List of directories to search for bitmap files for MW32.");
  Vmw32_bitmap_file_path = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    "The name Emacs uses to look up X resources.\n\
`x-get-resource' uses this as the first component of the instance name\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-name' to the name under which Emacs\n\
was invoked, or to the value specified with the `-name' or `-rn'\n\
switches, if present.\n\
\n\
It may be useful to bind this variable locally around a call\n\
to `x-get-resource'.  See also the variable `x-resource-class'.");
  Vx_resource_name = Qnil;

  DEFVAR_LISP ("x-resource-class", &Vx_resource_class,
    "The class Emacs uses to look up X resources.\n\
`x-get-resource' uses this as the first component of the instance class\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-class' to \"Emacs\".\n\
\n\
Setting this variable permanently is not a reasonable thing to do,\n\
but binding this variable locally around a call to `x-get-resource'\n\
is a reasonable practice.  See also the variable `x-resource-name'.");
  Vx_resource_class = build_string (EMACS_CLASS);

  DEFVAR_LISP ("mw32-hourglass-pointer-shape", &Vmw32_hourglass_pointer_shape,
    "The shape of the pointer when Emacs is busy.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vmw32_hourglass_pointer_shape = Qnil;

  DEFVAR_BOOL ("display-hourglass", &display_hourglass_p,
    "Non-zero means Emacs displays an hourglass pointer on window systems.");
  display_hourglass_p = 1;
  
  DEFVAR_LISP ("hourglass-delay", &Vhourglass_delay,
     "*Seconds to wait before displaying an hourglass pointer.\n\
Value must be an integer or float.");
  Vhourglass_delay = make_number (DEFAULT_HOURGLASS_DELAY);

  DEFVAR_LISP ("mw32-sensitive-text-pointer-shape",
	      &Vmw32_sensitive_text_pointer_shape,
	      "The shape of the pointer when over mouse-sensitive text.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vmw32_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("mw32-window-horizontal-drag-cursor",
	      &Vmw32_window_horizontal_drag_shape,
  "Pointer shape to use for indicating a window can be dragged horizontally.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vmw32_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("mw32-cursor-fore-pixel", &Vmw32_cursor_fore_pixel,
	       "A string indicating the foreground color of the cursor box.");
  Vmw32_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("mw32-max-tooltip-size", &Vmw32_max_tooltip_size,
   "Maximum size for tooltips.  Value is a pair (COLUMNS . ROWS).\n\
Text larger than this is clipped.");
  Vmw32_max_tooltip_size = Fcons (make_number (80), make_number (40));

  DEFVAR_LISP ("image-cache-eviction-delay", &Vimage_cache_eviction_delay,
     "Time after which cached images are removed from the cache.\n\
When an image has not been displayed this many seconds, remove it\n\
from the image cache.  Value must be an integer or nil with nil\n\
meaning don't clear the cache.");
  Vimage_cache_eviction_delay = make_number (30 * 60);

  defsubr (&Sx_get_resource);

  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);

  defsubr (&Sx_display_list);

  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_focus_frame);

#if 0
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_save_under);
#endif

  /* Images.  */
  Qbmp = intern ("bmp");
  staticpro (&Qbmp);
  QCtype = intern (":type");
  staticpro (&QCtype);
  QCconversion = intern (":conversion");
  staticpro (&QCconversion);
  QCheuristic_mask = intern (":heuristic-mask");
  staticpro (&QCheuristic_mask);
  QCcolor_symbols = intern (":color-symbols");
  staticpro (&QCcolor_symbols);
  QCascent = intern (":ascent");
  staticpro (&QCascent);
  QCmargin = intern (":margin");
  staticpro (&QCmargin);
  QCrelief = intern (":relief");
  staticpro (&QCrelief);
  QCindex = intern (":index");
  staticpro (&QCindex);

  Qpostscript = intern ("postscript");
  staticpro (&Qpostscript);

#if 0
  QCloader = intern (":loader");
  staticpro (&QCloader);
  QCbounding_box = intern (":bounding-box");
  staticpro (&QCbounding_box);
  QCpt_width = intern (":pt-width");
  staticpro (&QCpt_width);
  QCpt_height = intern (":pt-height");
  staticpro (&QCpt_height);
#endif
  Qxbm = intern ("xbm");
  staticpro (&Qxbm);
  Qpbm = intern ("pbm");
  staticpro (&Qpbm);
  Qxpm = intern ("xpm");
  staticpro (&Qxpm);
  Qjpeg = intern ("jpeg");
  staticpro (&Qjpeg);
  Qtiff = intern ("tiff");
  staticpro (&Qtiff);
  Qgif = intern ("gif");
  staticpro (&Qgif);
  Qpng = intern ("png");
  staticpro (&Qpng);

  defsubr (&Sw32_access_windows_intrinsic_facility);
  Qwm_syscommand = intern ("WM-SYSCOMMAND");
  staticpro (&Qwm_syscommand);
  Qsc_keymenu = intern ("SC-KEYMENU");
  staticpro (&Qsc_keymenu);
  Qsc_monitorpower = intern ("SC-MONITORPOWER");
  staticpro (&Qsc_monitorpower);
  Qsc_tasklist = intern ("SC-TASKLIST");
  staticpro (&Qsc_tasklist);
  Qsc_maximize = intern ("SC-MAXIMIZE");
  staticpro (&Qsc_maximize);
  Qsc_restore = intern ("SC-RESTORE");
  staticpro (&Qsc_restore);
  Qlow_power = intern ("low-power");
  staticpro (&Qlow_power);
  Qshut_off = intern ("shut-off");
  staticpro (&Qshut_off);
  
  defsubr (&Sfile_system_info);

  defsubr (&Sclear_image_cache);
  defsubr (&Simage_size);
  defsubr (&Simage_mask_p);

  hourglass_atimer = NULL;
  hourglass_shown_p = 0;

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  
  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);

  last_show_tip_args = Qnil;
  staticpro (&last_show_tip_args);

  defsubr (&Smw32_file_dialog);

  defsubr (&Smw32_get_image_magick);
  defsubr (&Smw32_get_image_magick_extensions);
}


void
init_mw32fns (void)
{
  image_types = NULL;
  Vimage_types = Qnil;
  
  Vmw32_bitmap_file_path = decode_env_path ("BITMAPPATH", PATH_BITMAPS);

  define_image_type (&bmp_type);

  /* When dumping, don't call it. */
  if (initialized)
    {
      initialize_imagemagick ();
      initialze_multi_monitor_api ();
    }
}

#undef abort

void 
w32_abort (void)
{
  int button;
  button = MessageBox (NULL,
		       "A fatal error has occurred!\n\n"
		       "Select Abort to exit, Retry to debug, Ignore to continue",
		       "Emacs Abort Dialog",
		       MB_ICONEXCLAMATION | MB_TASKMODAL
		       | MB_SETFOREGROUND | MB_ABORTRETRYIGNORE);
  switch (button)
    {
    case IDRETRY:
      DebugBreak ();
      break;
    case IDIGNORE:
      break;
    case IDABORT:
    default:
      abort ();
      break;
    }
#if defined (__MINGW32__)
  exit (1);
#endif /* __MINGW32__ */
}
