/* MW32 Communication module for Windows.
   Copyright (C) 1989, 93, 94, 95, 96, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.

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

/* New display code by Gerd Moellmann <gerd@gnu.org>.  */
/* Xt features made by Fred Pierresteguy.  */
/* MW32 implementation by MIYASHITA Hisashi <himi@meadowy.org> */
/* 
   TODO list:
   ----------
   mw32color
   mouse cursor.
   stipple op.
   mw32_scroll_bar_report_motion
*/

#include <config.h>

/* On 4.3 these lose if they come after xterm.h.  */
/* Putting these at the beginning seems to be standard for other .c files.  */
#include <signal.h>

#include <stdio.h>

#include "lisp.h"
#include "blockinput.h"
#include "mw32sync.h"
#include <shellapi.h>
#include "mw32term.h"
#include "mw32mci.h"

#include "systty.h"
#include "systime.h"

#include <ctype.h>
#include <errno.h>

#include "charset.h"
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "gnu.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "process.h"
#include "atimer.h"
#if defined (__MINGW32__)
#include <limits.h>
#endif /* __MINGW32__ */
#include <mmsystem.h>

#define mw32_any_window_to_frame mw32_any_window_to_frame
#define mw32_top_window_to_frame mw32_window_to_frame

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

#define BETWEEN(X, LOWER, UPPER)  ((X) >= (LOWER) && (X) < (UPPER))


/* Bitmaps for truncated lines.  */

enum bitmap_type
{
  NO_BITMAP = -1,
  LEFT_TRUNCATION_BITMAP = 0,
  RIGHT_TRUNCATION_BITMAP = 1,
  OVERLAY_ARROW_BITMAP = 2,
  CONTINUED_LINE_BITMAP = 3,
  CONTINUATION_LINE_BITMAP = 4,
  ZV_LINE_BITMAP = 5,
  BITMAP_TYPE_MAX = 6
};

enum bitmap_location
{
  LEFT_FLAGS_AREA,
  RIGHT_FLAGS_AREA,
};

struct internal_bitmap
{
  int width;
  int height;
  enum bitmap_location location;
  unsigned char *pbitmap;
  HBITMAP hBMP;
};

/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */

struct internal_bitmap internal_bitmap_array[] = 
{
  /* LEFT_TRUNCATION_BITMAP */
  {8, 8, LEFT_FLAGS_AREA, 
   "\x18\x00\x30\x00\x60\x00\xfc\x00\xfc\x00\x60\x00\x30\x00\x18\x00",
   INVALID_HANDLE_VALUE},
  /* RIGHT_TRUNCATION_BITMAP */
  {8, 8, RIGHT_FLAGS_AREA,
   "\x18\x00\x0c\x00\x06\x00\x3f\x00\x3f\x00\x06\x00\x0c\x00\x18\x00",
   INVALID_HANDLE_VALUE},
  /* OVERLAY_ARROW_BITMAP */
  {8, 8, LEFT_FLAGS_AREA,
   "\xc0\x00\xf0\x00\xf8\x00\xfc\x00\xfc\x00\xf8\x00\xf0\x00\xc0\x00",
   INVALID_HANDLE_VALUE},
  /* CONTINUED_LINE_BITMAP */
  {8, 8, RIGHT_FLAGS_AREA,
   "\x3c\x00\x3e\x00\x03\x00\x27\x00\x3f\x00\x3e\x00\x3c\x00\x3e\x00",
   INVALID_HANDLE_VALUE},
  /* CONTINUATION_LINE_BITMAP */
  {8, 8, LEFT_FLAGS_AREA,
   "\x3c\x00\x7c\x00\xc0\x00\xe4\x00\xfc\x00\x7c\x00\x3c\x00\x7c\x00",
   INVALID_HANDLE_VALUE},
  /* ZV_LINE_BITMAP */
  {8, 8, LEFT_FLAGS_AREA,
   "\x00\x00\x00\x00\x78\x00\x78\x00\x78\x00\x78\x00\x00\x00\x00\x00",
   INVALID_HANDLE_VALUE},
};

extern Lisp_Object Qhelp_echo;


/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* If a string, XTread_socket generates an event to display that string.
   (The display is done in read_char.)  */
   
static Lisp_Object help_echo;
static Lisp_Object help_echo_window;
static Lisp_Object help_echo_object;
static int help_echo_pos;

/* Temporary variable for XTread_socket.  */

static Lisp_Object previous_help_echo;
static int last_mousemove_x = 0;
static int last_mousemove_y = 0;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Non-zero means draw block and hollow cursor as wide as the glyph
   under it.  For example, if a block cursor is over a tab, it will be
   drawn as wide as that tab on the display.  */

int mw32_stretch_cursor_p;

/* This is a chain of structures for all the X displays currently in
   use.  */

struct mw32_display_info *mw32_display_list;

/* This is a list of cons cells, each of the form (NAME
   . FONT-LIST-CACHE), one for each element of x_display_list and in
   the same order.  NAME is the name of the frame.  FONT-LIST-CACHE
   records previous values returned by x-list-fonts.  */

Lisp_Object x_display_name_list;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the XT functions.
   It is zero while not inside an update.  In that case, the XT
   functions assume that `selected_frame' is the frame to apply to.  */

extern struct frame *updating_frame;

extern int waiting_for_input;

/* This is a frame waiting to be auto-raised, within XTread_socket.  */

struct frame *pending_autoraise_frame;

/* Nominal cursor position -- where to draw output.  
   HPOS and VPOS are window relative glyph matrix coordinates.
   X and Y are window relative pixel coordinates.  */

struct cursor_pos output_cursor;

/* Mouse movement.

   Formerly, we used PointerMotionHintMask (in standard_event_mask)
   so that we would have to call XQueryPointer after each MotionNotify
   event to ask for another such event.  However, this made mouse tracking
   slow, and there was a bug that made it eventually stop.

   Simply asking for MotionNotify all the time seems to work better.

   In order to avoid asking for motion events and then throwing most
   of them away or busy-polling the server for mouse positions, we ask
   the server for pointer motion hints.  This means that we get only
   one event per group of mouse movements.  "Groups" are delimited by
   other kinds of events (focus changes and button clicks, for
   example), or by XQueryPointer calls; when one of these happens, we
   get another MotionNotify event the next time the mouse moves.  This
   is at least as efficient as getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.  */

/* Where the mouse was last time we reported a mouse event.  */

FRAME_PTR last_mouse_frame;
static RECT last_mouse_glyph;
static Lisp_Object last_mouse_press_frame;



/* wheel message. */
#ifdef W32_INTELLIMOUSE
static UINT mw32_wheel_message = WM_MOUSEWHEEL;
#endif

/* to prevent thread from operating messages.  */
CRITICAL_SECTION critsec_message;

/* to prevent thread from accessing events.  */
CRITICAL_SECTION critsec_access_event;

/* make thread waiting for gobbling input.  */
HANDLE next_message_block_event;

/* flag if mw32term is blocked in message loop. */
int message_loop_blocked_p;

/* Message Handling thread & its id.  */
HANDLE msg_thread;
DWORD msg_thread_id;

/* Main thread & its id. */
HANDLE main_thread;
DWORD main_thread_id;

/* To handle C-g quickly, this event handle is
   pulsed when C-g is detected. */
HANDLE interrupt_handle;

/* thread id that owns block_input critical section.  */
DWORD block_input_ownthread;

/* If the system has already created frame, this variable is set to
   its window handle.  This variable is used for sending message to
   window procedure. */
HWND mw32_frame_window;


/* The scroll bar in which the last X motion event occurred.

   If the last X motion event occurred in a scroll bar, we set this so
   XTmouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last X motion event didn't occur in a scroll bar, we set
   this to Qnil, to tell XTmouse_position to return an ordinary motion
   event.  */

static Lisp_Object last_mouse_scroll_bar;

/* This is a hack.  We would really prefer that MW32_mouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the time-stamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */

static Time last_mouse_movement_time;

/* For synchronous note_mouse_movement.  */
/* last_mouse_motion_frame maintains the frame where a mouse moves last time.
   Its value is set to NULL when a mouse is not moved.  */
static Lisp_Object last_mouse_motion_frame;

/* last_mouse_motion_message describes the mouse movement last time.  */
static MSG last_mouse_motion_message;

/* Incremented by XTread_socket whenever it really tries to read
   events.  */

#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

/* Initial values of argv and argc.  */

extern char **initial_argv;
extern int initial_argc;

extern Lisp_Object Vcommand_line_args, Vsystem_name;

/* Tells if a window manager is present or not.  */

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* Enumeration for overriding/changing the face to use for drawing
   glyphs in x_draw_glyphs.  */

enum draw_glyphs_face
{
  DRAW_NORMAL_TEXT,
  DRAW_INVERSE_VIDEO,
  DRAW_CURSOR,
  DRAW_MOUSE_FACE,
  DRAW_IMAGE_RAISED,
  DRAW_IMAGE_SUNKEN
};

/* MW32 xterm compatibility functions. */
void x_set_window_size P_ ((struct frame *, int, int, int));
void x_make_frame_visible P_ ((struct frame *));
void x_set_offset P_ ((struct frame *, int, int, int));
void x_iconify_frame P_ ((struct frame *));
void x_set_mouse_position P_ ((struct frame *f, int x, int y));
void x_set_mouse_pixel_position P_ ((struct frame *f, int pix_x, int pix_y));
void x_destroy_window P_ ((FRAME_PTR f));
void pixel_to_glyph_coords P_ ((FRAME_PTR f, int pix_x, int pix_y,
				int *px, int *py, RECT *bounds, int noclip));

/* for mw32fns module interfaces. */
HDC mw32_get_frame_hdc P_ ((struct frame *f));
void mw32_destroy_frame_hdc P_ ((struct frame *f));
static void mw32_lower_frame P_ ((struct frame *f));
void mw32_scroll_bar_clear P_ ((struct frame *));
static void mw32_raise_frame P_ ((struct frame *));
void mw32_initialize P_ ((void));
int mw32_text_icon P_ ((struct frame *, char *));
int mw32_bitmap_icon P_ ((struct frame *, Lisp_Object));
/* int mw32_alloc_nearest_color P_ ((struct frame *, Colormap, XColor *)); */
void mw32_update_cursor P_ ((FRAME_PTR f, int on_p));
void mw32_display_and_set_cursor P_ ((struct window *, int, int, int, int, int));
void mw32_menu_bar_store_activate_event P_ ((struct frame *f));

void mw32_expose_frame P_ ((struct frame *f));
void mw32_new_focus_frame P_ ((struct mw32_display_info *, struct frame *));
void mw32_free_frame_resources P_((struct frame *f));
Lisp_Object mw32_new_font P_((struct frame *f, char *fontname));
Lisp_Object mw32_new_fontset P_((struct frame *f, char *fontsetname));
void mw32_delete_display P_ ((struct mw32_display_info *));
struct mw32_display_info *mw32_term_init P_ ((Lisp_Object display_name,
					      char *xrm_option,
					      char *resource_name));

/* MW32 redisplay IF functions */
static void mw32i_write_glyphs P_ ((struct glyph *, int));
static void mw32i_produce_glyphs P_ ((struct it *));
static void mw32i_insert_glyphs P_ ((struct glyph *, int));
static void mw32i_clear_end_of_line P_ ((int));
static void mw32i_scroll_run P_ ((struct window *, struct run *));
static void mw32i_after_update_window_line P_ ((struct glyph_row *));
static void mw32i_update_window_begin P_ ((struct window *));
static void mw32i_update_window_end P_ ((struct window *, int, int));
static void mw32i_flush P_ ((struct frame *f));
static void mw32i_clear_mouse_face P_ ((struct window *));

/* MW32 Terminal Hook or redisplay IF functions */
static void MW32_frame_up_to_date P_ ((struct frame *));
static void MW32_reassert_line_highlight P_ ((int, int));
static void MW32_change_line_highlight P_ ((int, int, int, int));
static void MW32_set_terminal_modes P_ ((void));
static void MW32_reset_terminal_modes P_ ((void));
static void MW32_cursor_to P_ ((int, int, int, int));
static void MW32_clear_frame P_ ((void));
static void MW32_delete_glyphs P_ ((int));
static void MW32_ring_bell P_ ((void));
static void MW32_update_begin P_ ((struct frame *));
static void MW32_update_end P_ ((struct frame *));
static int MW32_read_socket P_ ((int, struct input_event *, int, int));
static void MW32_frame_up_to_date P_ ((struct frame *));
static void MW32_frame_rehighlight P_ ((struct frame *));
static int MW32_estimate_mode_line_height P_ ((struct frame *f,
					       enum face_id face_id));
/* MW32 internal services */
static int cursor_in_mouse_face_p P_ ((struct window *));
static int clear_mouse_face P_ ((struct mw32_display_info *));
static void frame_to_window_pixel_xy P_ ((struct window *, int *, int *));
static int fast_find_position P_ ((struct window *, int, int *, int *,
				   int *, int *, Lisp_Object));
static int fast_find_string_pos P_ ((struct window *, int, Lisp_Object,
				     int *, int *, int *, int *, int));
static void set_output_cursor P_ ((struct cursor_pos *));
static struct glyph *x_y_to_hpos_vpos P_ ((struct window *, int, int,
					   int *, int *, int *, int));
static void note_mode_line_highlight P_ ((struct window *, int, int));
static void note_mouse_highlight P_ ((struct frame *, int, int));
static void note_tool_bar_highlight P_ ((struct frame *f, int, int));
static void anticipate_overwrite_caret P_ ((struct window *));
static void restore_overwritten_caret P_ ((struct frame *f));
static void notice_overwritten_cursor P_ ((struct window *, int, int));
static void show_mouse_face P_ ((struct mw32_display_info *,
				 enum draw_glyphs_face));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void mw32_frame_rehighlight_1 P_ ((struct mw32_display_info *dpyinfo));
static int mw32_get_keymodifier_state P_(());
#define MW32GETMODIFIER(dpyinfo) mw32_get_keymodifier_state()
#define MW32GETMOUSEMODIFIER(dpyinfo, mouse) \
 (MW32GETMODIFIER (dpyinfo) | (mouse ? up_modifier : down_modifier))


/* static int mw32_alloc_nearest_color_1 P_ ((Display *, Colormap, XColor *)); */
static void mw32_set_window_size_1 P_ ((struct frame *, int, int, int));
static void mw32_font_min_bounds P_ ((MW32LogicalFont *, int *, int *));
static int mw32_compute_min_glyph_bounds P_ ((struct frame *));
static void mw32_clear_cursor P_ ((struct window *));
static void mw32_display_cursor P_ ((struct window *, int, int, int, int, int));

static void mw32_draw_phys_cursor_glyph P_ ((struct window *,
					     struct glyph_row *,
					     enum draw_glyphs_face));

static void mw32_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void mw32_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int));
static int mw32_intersect_rectangles P_ ((RECT *, RECT *, RECT *));

static void mw32_update_cursor_in_window_tree P_ ((struct window *, int));
static void mw32_update_window_cursor P_ ((struct window *, int));
static void mw32_erase_phys_cursor P_ ((struct window *));
static void mw32_draw_bitmap P_ ((struct window *, struct glyph_row *, 
				  enum bitmap_type));
static void mw32_clip_to_row P_ ((HDC, struct window *, struct glyph_row *, int));
static int mw32_phys_cursor_in_rect_p P_ ((struct window *, RECT *));
static void mw32_draw_row_bitmaps P_ ((struct window *, struct glyph_row *));
static void mw32_draw_window_fringes (struct window *w);
static void mw32_draw_vertical_border P_ ((struct window *));
static INLINE void take_vertical_position_into_account P_ ((struct it *));
static void mw32_produce_stretch_glyph P_ ((struct it *));
static struct scroll_bar *mw32_window_to_scroll_bar P_ ((Window));

/* Window that is tracking the mouse.  */
static HWND track_mouse_window = NULL;

typedef BOOL (WINAPI *TrackMouseEvent_Proc) (LPTRACKMOUSEEVENT);

TrackMouseEvent_Proc track_mouse_event_fn = NULL;

/* Flush message queue of frame F, or of all frames if F is null.  */

static void
mw32i_flush (struct frame *f)
{
#if 0
  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
	mw32i_flush (XFRAME (frame));
    }
  else if (FRAME_MW32_P (f))
    {
      /* wait until the message queue is flushed. */
    }
#else
      GdiFlush ();
#endif
}


/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* This is a function useful for recording debugging information about
   the sequence of occurrences in this file.  */

struct record 
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

record_event (locus, type)
     char *locus;
     int type;
{
  if (event_record_index == sizeof (event_record) / sizeof (struct record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif /* 0 */



/* Return the struct mw32_display_info corresponding to DPY.  */

struct mw32_display_info *
mw32_display_info_for_display (DWORD id)
{
  return mw32_display_list;
}

/* Allocate & Return Frame's device contest.  */

void
mw32_setup_default_hdc (HDC hdc)
{
  SetTextAlign (hdc, TA_BASELINE);
  SetMapMode (hdc, MM_TEXT);
  SetBkMode (hdc, TRANSPARENT);
}

HDC
mw32_get_frame_hdc (struct frame *f)
{
  int i;
  HDC *phdc, hdc;
  struct frame **pf;
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);

  phdc = dpyinfo->alloced_DCs;
  pf = dpyinfo->alloced_DC_frames;
  for (i = 0; i < MAX_DC_NUM; i++)
    {
      if (*phdc == INVALID_HANDLE_VALUE)
	{
	  hdc = GetDC (FRAME_MW32_WINDOW (f));
	  f->output_data.mw32->hdc = *phdc = hdc;
	  *pf = f;
	  break;
	}
      phdc++;
      pf++;
    }
  if (i == MAX_DC_NUM)
    {
      phdc = dpyinfo->alloced_DCs;
      pf = dpyinfo->alloced_DC_frames;

      RestoreDC (*phdc, -1);
      ReleaseDC (FRAME_MW32_WINDOW (*pf), *phdc);
      pf[0]->output_data.mw32->hdc = INVALID_HANDLE_VALUE;
      memmove (phdc, phdc + 1, sizeof (HDC) * (MAX_DC_NUM - 1));
      memmove (pf, pf + 1, sizeof (struct frame*) * (MAX_DC_NUM - 1));
      hdc = GetDC (FRAME_MW32_WINDOW (f));
      f->output_data.mw32->hdc = phdc[MAX_DC_NUM - 1] = hdc;
      pf[MAX_DC_NUM - 1] = f;
    }
  
  SaveDC (hdc);
  mw32_setup_default_hdc (hdc);

  return hdc;
}

/* Destroy Frame's dc.  */

void
mw32_destroy_frame_hdc (struct frame *f)
{
  HDC hdc;
  hdc = f->output_data.mw32->hdc;

  if (hdc != INVALID_HANDLE_VALUE)
    {
      int i;
      struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
      HDC *phdc = dpyinfo->alloced_DCs;
      struct frame **pf = dpyinfo->alloced_DC_frames;

      for (i = 0; i < MAX_DC_NUM; i++)
	{
	  if (*phdc == hdc)
	    {
	      xassert (*pf == f);
	      f->output_data.mw32->hdc = *phdc = INVALID_HANDLE_VALUE;
	      *pf = NULL;
	      break;
	    }
	  phdc++;
	  pf++;
	}
      xassert (i < MAX_DC_NUM);
      RestoreDC (hdc, -1);
      ReleaseDC (FRAME_MW32_WINDOW (f), hdc);
    }
}

void
mw32_fill_area_pix (struct frame *f, PIX_TYPE pix,
		    int x0, int y0, int x1, int y1)
{
  RECT r;
  HBRUSH hb;
  HANDLE oldobj;
  HDC hdc = FRAME_HDC (f);

  r.left = x0;
  r.top = y0;
  r.right = x1;
  r.bottom = y1;
  hb = CreateSolidBrush (pix);
  oldobj = SelectObject (hdc, hb);
  FillRect (hdc, &r, hb);
  SelectObject (hdc, oldobj);
  DeleteObject (hb);
}

void
mw32_fill_area (struct frame *f, int x0, int y0, int x1, int y1)
{
  mw32_fill_area_pix (f, f->output_data.mw32->foreground_pixel,
		      x0, y0, x1, y1);
}

void
mw32_clear_area (struct frame *f, int x0, int y0, int x1, int y1)
{
  mw32_fill_area_pix (f, f->output_data.mw32->background_pixel,
		      x0, y0, x1, y1);
}

void
mw32_set_clip_area (struct frame *f, int x0, int y0, int x1, int y1)
{
  HDC hdc;
  HRGN region;
  hdc = FRAME_HDC (f);
  region = CreateRectRgn (x0, y0, x1, y1);
  SelectClipRgn (hdc, region);
  DeleteObject (region);
}

void
mw32_unset_clip_area (struct frame *f)
{
  SelectClipRgn (FRAME_HDC (f), NULL);
}

void
mw32_set_caret (struct frame *f, int state)
{
  if (W32_SELF_INPUT_BLOCKED_P)
    {
      W32_UNBLOCK_INPUT;
      SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW (f),
			   WM_EMACS_SETCARET,
			   state, 0);
      W32_BLOCK_INPUT;
    }
  else
    {
      SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW (f),
			   WM_EMACS_SETCARET,
			   state, 0);
    }
}


/***********************************************************************
		    Starting and ending an update
 ***********************************************************************/
									
/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to x_update_window_begin for
   each window being updated.  Currently, there is nothing to do here
   because all interesting stuff is done on a window basis.  */

static void
MW32_update_begin (struct frame *f)
{
}


/* Start update of window W.  Set the global variable updated_window
   to the window being updated and set output_cursor to the cursor
   position of W.  */

static void
mw32i_update_window_begin (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct mw32_display_info *display_info = FRAME_MW32_DISPLAY_INFO (f);

  updated_window = w;
  set_output_cursor (&w->cursor);

  BLOCK_INPUT;

  if (f == FRAME_MW32_DISPLAY_INFO (f)->mw32_focus_frame
      && f == XFRAME (selected_frame)
      && CARET_CURSOR_P (FRAME_DESIRED_CURSOR (f)))
    mw32_set_caret (f, BLOCK_CARET);

  if (f == display_info->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      display_info->mouse_face_defer = 1;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	display_info->mouse_face_window = Qnil;

#if 0 /* Rows in a current matrix containing glyphs in mouse-face have
	 their mouse_face_p flag set, which means that they are always
	 unequal to rows in a desired matrix which never have that
	 flag set.  So, rows containing mouse-face glyphs are never
	 scrolled, and we don't have to switch the mouse highlight off
	 here to prevent it from being scrolled.  */
      
      /* Can we tell that this update does not affect the window
	 where the mouse highlight is?  If so, no need to turn off.
	 Likewise, don't do anything if the frame is garbaged;
	 in that case, the frame's current matrix that we would use
	 is all wrong, and we will redisplay that line anyway.  */
      if (!NILP (display_info->mouse_face_window)
	  && w == XWINDOW (display_info->mouse_face_window))
	{
	  int i;

	  for (i = 0; i < w->desired_matrix->nrows; ++i)
	    if (MATRIX_ROW_ENABLED_P (w->desired_matrix, i))
	      break;

	  if (i < w->desired_matrix->nrows)
	    clear_mouse_face (display_info);

	  /* If we should require GdiFlush(),
	     insert here.  */
	}
#endif /* 0 */
    }

  UNBLOCK_INPUT;
}


/* Draw a vertical window border to the right of window W if W doesn't
   have vertical scroll bars.  */

static void
mw32_draw_vertical_border (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  
  /* Redraw borders between horizontally adjacent windows.  Don't
     do it for frames with vertical scroll bars because either the
     right scroll bar of a window, or the left scroll bar of its
     neighbor will suffice as a border.  */
  if (!WINDOW_RIGHTMOST_P (w)
      && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      int x0, x1, y0, y1;

      window_box_edges (w, -1, &x0, &y0, &x1, &y1);
      x0 = x1 + FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH (f);
      x1 = x0 + 1;
      /* y1 -= 1; needless? */
      
      mw32_fill_area (f, x0, y0, x1, y1);
    }
}
   
   
/* End update of window W (which is equal to updated_window).

   Draw vertical borders between horizontally adjacent windows, and
   display W's cursor if CURSOR_ON_P is non-zero.

   MOUSE_FACE_OVERWRITTEN_P non-zero means that some row containing
   glyphs in mouse-face were overwritten.  In that case we have to
   make sure that the mouse-highlight is properly redrawn.

   W may be a menu bar pseudo-window in case we don't have X toolkit
   support.  Such windows don't have a cursor, so don't display it
   here.  */

static void
mw32i_update_window_end (struct window *w,
			 int cursor_on_p,
			 int mouse_face_overwritten_p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct mw32_display_info
    *dpyinfo = FRAME_MW32_DISPLAY_INFO (XFRAME (w->frame));
      
  if (!w->pseudo_window_p)
    {
      if (cursor_on_p)
	mw32_display_and_set_cursor (w, 1, output_cursor.hpos,
				     output_cursor.vpos,
				     output_cursor.x, output_cursor.y);
      
      mw32_draw_vertical_border (w);
      /* If we should require GdiFlush(),
	 insert here.  */
      mw32_draw_window_fringes (w);
    }
  
  /* If a row with mouse-face was overwritten, arrange for
     MW32_frame_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }

  if (f == FRAME_MW32_DISPLAY_INFO (f)->mw32_focus_frame
      && f == XFRAME (selected_frame)
      && CARET_CURSOR_P (FRAME_DESIRED_CURSOR (f)))
    mw32_set_caret (f, UNBLOCK_CARET);
  updated_window = NULL;
}


/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
MW32_update_end (struct frame *f)
{
  /* Mouse highlight may be displayed again.  */
  FRAME_MW32_DISPLAY_INFO (f)->mouse_face_defer = 0;
}


/* This function is called from various places in xdisp.c whenever a
   complete update has been performed.  The global variable
   updated_window is not available here.  */

static void
MW32_frame_up_to_date (struct frame *f)
{
  if (FRAME_MW32_P (f))
    {
      struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);

      if (dpyinfo->mouse_face_deferred_gc
	  || f == dpyinfo->mouse_face_mouse_frame)
	{
	  BLOCK_INPUT;
	  W32_BLOCK_INPUT;
	  if (dpyinfo->mouse_face_mouse_frame)
	    note_mouse_highlight (dpyinfo->mouse_face_mouse_frame,
				  dpyinfo->mouse_face_mouse_x,
				  dpyinfo->mouse_face_mouse_y);
	  dpyinfo->mouse_face_deferred_gc = 0;
	  W32_UNBLOCK_INPUT;
	  UNBLOCK_INPUT;
	}
    }
}


/* Draw truncation mark bitmaps, continuation mark bitmaps, overlay
   arrow bitmaps, or clear the areas where they would be displayed
   before DESIRED_ROW is made current.  The window being updated is
   found in updated_window.  This function It is called from
   update_window_line only if it is known that there are differences
   between bitmaps to be drawn between current row and DESIRED_ROW.  */

static void
mw32i_after_update_window_line (struct glyph_row *desired_row)
{
  struct window *w = updated_window;
  
  xassert (w);
  
  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    {
      struct frame *f;
      int width;
      
      BLOCK_INPUT;

      /* When a window has disappeared, make sure that no rest of
	 full-width rows stays visible in the internal border.  */
      if (windows_or_buffers_changed
	  && (f = XFRAME (w->frame),
	      width = FRAME_INTERNAL_BORDER_WIDTH (f),
	      width != 0))
	{
	  int height = desired_row->visible_height;
	  int x = (window_box_right (w, -1)
		   + FRAME_X_RIGHT_FLAGS_AREA_WIDTH (f));
	  int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));

	  mw32_clear_area (f, x, y, x + width, y + height);
	}
      /* If we should require GdiFlush(),
	 insert here.  */
      UNBLOCK_INPUT;
    }
}


/* Draw the bitmap WHICH in one of the areas to the left or right of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.  */

static void
mw32_draw_bitmap (struct window *w,
		  struct glyph_row *row,
		  enum bitmap_type which)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HDC hCompatDC;
  HDC hdc = FRAME_HDC (f);
  HBITMAP hBMP;
  HANDLE horgobj;
  struct internal_bitmap *pib;
  enum bitmap_location loc;
  int x, y, wd, h, dy;
  struct face *face;

  SaveDC (hdc);
  /* Must clip because of partially visible lines.  */
  mw32_clip_to_row (hdc, w, row, 1);

  if ((which < 0) || (which >= BITMAP_TYPE_MAX))
    abort ();
  pib = &internal_bitmap_array[which];
  wd = pib->width;
  h = pib->height;
  loc = pib->location;
  if (loc == LEFT_FLAGS_AREA)
    {
      x = (WINDOW_TO_FRAME_PIXEL_X (w, 0)
	   - (FRAME_X_LEFT_FLAGS_AREA_WIDTH (f) - wd) / 2
	   - wd);
    }
  else
    {
      x = (window_box_right (w, -1)
	   + (FRAME_X_RIGHT_FLAGS_AREA_WIDTH (f) - wd) / 2);
    }

  /* Convert to frame coordinates.  Set dy to the offset in the row to
     start drawing the bitmap.  */
  y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  dy = (row->height - h) / 2;

  /* Draw the bitmap.  I believe these small pixmaps can be cached
     by the server.  */
  face = FACE_FROM_ID (f, BITMAP_AREA_FACE_ID);

  hBMP = pib->hBMP;
  if (hBMP == INVALID_HANDLE_VALUE)
    hBMP = pib->hBMP = CreateBitmap (wd, h, 1, 1, pib->pbitmap);
  if (!hBMP) return;

  SetTextColor (hdc, face->background);
  SetBkColor (hdc, face->foreground);

  hCompatDC = CreateCompatibleDC (hdc);
  if (!hCompatDC) return;

  horgobj = SelectObject (hCompatDC, hBMP);
  BitBlt (hdc, x, y + dy, wd, h, hCompatDC, 0, 0, SRCCOPY);
  SelectObject (hCompatDC, horgobj);

  DeleteDC (hCompatDC);
  RestoreDC (hdc, -1);
}


/* Draw flags bitmaps for glyph row ROW on window W.  Call this
   function with input blocked.  */

static void
mw32_draw_row_bitmaps (struct window *w,
		       struct glyph_row *row)
{
  struct frame *f = XFRAME (w->frame);
  enum bitmap_type bitmap;
  struct face *face;
  int x0, y0;
  int header_line_height = -1;

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

  face = FACE_FROM_ID (f, BITMAP_AREA_FACE_ID);
  PREPARE_FACE_FOR_DISPLAY (f, face);

  /* Decide which bitmap to draw at the left side.  */
  if (row->overlay_arrow_p)
    bitmap = OVERLAY_ARROW_BITMAP;
  else if (row->truncated_on_left_p)
    bitmap = LEFT_TRUNCATION_BITMAP;
  else if (MATRIX_ROW_CONTINUATION_LINE_P (row))
    bitmap = CONTINUATION_LINE_BITMAP;
  else if (row->indicate_empty_line_p)
    bitmap = ZV_LINE_BITMAP;
  else
    bitmap = NO_BITMAP;

  /* Clear flags area if no bitmap to draw or if bitmap doesn't fill
     the flags area.  */
  if (bitmap == NO_BITMAP
      || FRAME_FLAGS_BITMAP_WIDTH (f) < FRAME_MW32_LEFT_FLAGS_AREA_WIDTH (f)
      || row->height > FRAME_FLAGS_BITMAP_HEIGHT (f))
    {
      /* If W has a vertical border to its left, don't draw over it.  */
      int border = ((XFASTINT (w->left) > 0
		     && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
		    ? 1 : 0);
      int left = window_box_left (w, -1);

      if (header_line_height < 0)
	header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      
      /* In case the same realized face is used for bitmap areas and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      x0 = (left
	    - FRAME_X_LEFT_FLAGS_AREA_WIDTH (f)
	    + border);
      y0 = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height, row->y));
      if (face->stipple)
	{
	  /* TODO: stipple support */
	}
      else
	{
	  mw32_fill_area_pix (f, face->background,
			      x0, y0,
			      x0 + FRAME_MW32_LEFT_FLAGS_AREA_WIDTH (f) - border,
			      y0 + row->visible_height);
	}
    }

  /* Draw the left bitmap.  */
  if (bitmap != NO_BITMAP)
    mw32_draw_bitmap (w, row, bitmap);

  /* Decide which bitmap to draw at the right side.  */
  if (row->truncated_on_right_p)
    bitmap = RIGHT_TRUNCATION_BITMAP;
  else if (row->continued_p)
    bitmap = CONTINUED_LINE_BITMAP;
  else
    bitmap = NO_BITMAP;

  /* Clear flags area if no bitmap to draw of if bitmap doesn't fill
     the flags area.  */
  if (bitmap == NO_BITMAP
      || FRAME_FLAGS_BITMAP_WIDTH (f) < FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH (f)
      || row->height > FRAME_FLAGS_BITMAP_HEIGHT (f))
    {
      int right = window_box_right (w, -1);

      if (header_line_height < 0)
	header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);

      /* In case the same realized face is used for bitmap areas and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */

      x0 = right;
      y0 = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height, row->y));
      if (face->stipple)
	{
	  /* TODO: stipple support */
	}
      else
	{
	  mw32_fill_area_pix (f, face->background,
			      x0, y0,
			      x0 + FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH (f),
			      y0 + row->visible_height);
	}
    }

  /* Draw the right bitmap.  */
  if (bitmap != NO_BITMAP)
    mw32_draw_bitmap (w, row, bitmap);
}


/* Draw the fringes of window W.  Only fringes for rows marked for
   update in redraw_fringe_bitmaps_p are drawn.  */

static void
mw32_draw_window_fringes (struct window *w)
{
  struct glyph_row *row;
  int yb = window_text_bottom_y (w);
  int nrows = w->current_matrix->nrows;
  int y = 0, rn;

  if (w->pseudo_window_p)
    return;

  for (y = 0, rn = 0, row = w->current_matrix->rows;
       y < yb && rn < nrows;
       y += row->height, ++row, ++rn)
    {
#if 0
      if (!row->redraw_fringe_bitmaps_p)
	continue;
#endif
      mw32_draw_row_bitmaps (w, row);
    }
}


/***********************************************************************
			  Line Highlighting
 ***********************************************************************/

/* External interface to control of standout mode.  Not used for X
   frames.  Aborts when called.  */

static void
MW32_reassert_line_highlight (int new, int vpos)
{
  abort ();
}


/* Call this when about to modify line at position VPOS and change
   whether it is highlighted.  Not used for X frames.  Aborts when
   called.  */

static void
MW32_change_line_highlight (int new_highlight, int vpos,
			    int y, int first_unused_hpos)
{
  abort ();
}


/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no X window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
MW32_set_terminal_modes ()
{
}

/* This is called when exiting or suspending Emacs.  Exiting will make
   the X-windows go away, and suspending requires no action.  */

static void
MW32_reset_terminal_modes ()
{
}



/***********************************************************************
			    Output Cursor
 ***********************************************************************/

/* Set the global variable output_cursor to CURSOR.  All cursor
   positions are relative to updated_window.  */

static void
set_output_cursor (struct cursor_pos *cursor)
{
  output_cursor.hpos = cursor->hpos;
  output_cursor.vpos = cursor->vpos;
  output_cursor.x = cursor->x;
  output_cursor.y = cursor->y;
}


/* Set a nominal cursor position.

   HPOS and VPOS are column/row positions in a window glyph matrix.  X
   and Y are window text area relative pixel positions.
   
   If this is done during an update, updated_window will contain the
   window that is being updated and the position is the future output
   cursor position for that window.  If updated_window is null, use
   selected_window and display the cursor at the given position.  */

static void
MW32_cursor_to (int vpos, int hpos, int y, int x)
{
  struct window *w;

  /* If updated_window is not set, work on selected_window.  */
  if (updated_window)
    w = updated_window;
  else
    w = XWINDOW (selected_window);

  /* Set the output cursor.  */
  output_cursor.hpos = hpos;
  output_cursor.vpos = vpos;
  output_cursor.x = x;
  output_cursor.y = y;

  /* If not called as part of an update, really display the cursor.
     This will also set the cursor position of W.  */
  if (updated_window == NULL)
    mw32_display_cursor (w, 1, hpos, vpos, x, y);
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static struct face *mw32_get_glyph_face_and_encoding P_ ((struct frame *,
							  struct glyph *,
							  FontCp *,
							  int *));
static struct face *mw32_get_char_face_and_encoding P_ ((struct frame *, int,
							 int, FontCp *, int));
static MW32CharMetric* *mw32_per_char_metric P_ ((MW32LogicalFont *,
						  struct frame *,
						  FontCp));
static FontCp mw32_encode_char P_ ((int, struct font_info *));
static void mw32_append_glyph P_ ((struct it *));
static void mw32_append_composite_glyph P_ ((struct it *));
static void mw32_append_stretch_glyph P_ ((struct it *it, Lisp_Object,
					   int, int, double));
static void mw32_produce_image_glyph P_ ((struct it *it));


/* Encode CHAR2B using encoding information from FONT_INFO.  CHAR2B is
   the two-byte form of C.  Encoding is returned in *CHAR2B.  */

static FontCp
mw32_encode_char (int c, struct font_info *font_info)
{
  int len = 0;
  int charset = CHAR_CHARSET (c);
  int c1, c2;
  FontCp result = 0;
  MW32LogicalFont *plf = MW32_FONT_FROM_FONT_INFO (font_info);
  int charset_dimension;

  SPLIT_CHAR (c, charset, c1, c2);
  charset_dimension = CHARSET_DIMENSION (charset);

  /* We have to change code points in the following cases.  */
  if (font_info->font_encoder)
    {
      /* This font requires CCL program to calculate code
	 point of characters.  */
      struct ccl_program *ccl = font_info->font_encoder;
      int i;

      len = plf->encoding.font_unit_byte;

      if (charset_dimension == 1)
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = c1;
	  ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
	  for (i = 1; i <= len; i++)
	    result = (ccl->reg[i] | (result << 8));
	}
      else
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = c1, ccl->reg[2] = c2;
	  ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
	  for (i = 1; i <= len; i++)
	    result = (ccl->reg[i] | (result << 8));
	}
    }
  else
    {
      switch (plf->encoding.type)
	{
	case ENCODING_DIMENSION:
	  if (charset_dimension == 1)
	    return MAKEFONTCP (0, c1);
	  else
	    return MAKEFONTCP (c1, c2);
	  
	case ENCODING_BYTE1MSB1:
	  return MAKEFONTCP (0, (c1 | 0x80));

	case ENCODING_BYTE2MSB11:
	  return MAKEFONTCP ((c1 | 0x80), (c2 | 0x80));

	case ENCODING_SHIFTJIS:
	  if (charset_dimension == 2)
	    {
	      int s1, s2;
	      ENCODE_SJIS (c1, c2, s1, s2);
	      return MAKEFONTCP (s1, s2);
	    }
	  else
	    {
	      return MAKEFONTCP (0, (c1 | 0x80));
	    }

	case ENCODING_UNICODE:
	  if (charset_dimension == 2)
	    {
	      result = ((c1 - 32) * CHARSET_CHARS (charset)
			+ (c2 - 32));
	      return result;
	    }
	  else
	    {
	      return MAKEFONTCP (0, c1 - 32);
	    }

	default:
	  abort ();
	}
    }
  return result;
}


/* Get face and two-byte form of character C in face FACE_ID on frame
   F.  The encoding of C is returned in *CHAR2B.  MULTIBYTE_P non-zero
   means we want to display multibyte text.  Value is a pointer to a
   realized face that is ready for display.  */

static INLINE struct face *
mw32_get_char_face_and_encoding (struct frame *f,
				 int c, int face_id,
				 FontCp *fontcp,
				 int multibyte_p)
{
  struct face *face = FACE_FROM_ID (f, face_id);

  if (!multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      *fontcp = MAKEFONTCP (0, c);
      face_id = FACE_FOR_CHAR (f, face, c);
      face = FACE_FROM_ID (f, face_id);
    }
  else if (c < 128 && face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      *fontcp = MAKEFONTCP (0, c);
    }
  else
    {
      if (face->font != NULL)
	{
	  /* encode the character by font_info*/
	  struct font_info *font_info
	    = MW32_FONT_INFO_FROM_FONT (face->font);
	  if (font_info)
	    *fontcp =  mw32_encode_char (c, font_info);
	}
      else
	{
	  int c1, c2, charset;
      
	  /* Split characters into bytes.  If c2 is -1 afterwards, C is
	     really a one-byte character so that byte1 is zero.  */
	  SPLIT_CHAR (c, charset, c1, c2);
	  if (c2 > 0)
	    *fontcp = MAKEFONTCP (c1, c2);
	  else
	    *fontcp = MAKEFONTCP (0, c1);
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  
  return face;
}


/* Get face and two-byte form of character glyph GLYPH on frame F.
   The encoding of GLYPH->u.ch is returned in *CHAR2B.  Value is
   a pointer to a realized face that is ready for display.  */

static INLINE struct face *
mw32_get_glyph_face_and_encoding (struct frame *f,
				  struct glyph *glyph,
				  FontCp *fontcp,
				  int *two_byte_p)
{
  struct face *face;

  xassert (glyph->type == CHAR_GLYPH);
  face = FACE_FROM_ID (f, glyph->face_id);

  if (two_byte_p)
    *two_byte_p = 0;

  if (!glyph->multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      *fontcp = MAKEFONTCP (0, glyph->u.ch);
    }
  else if (glyph->u.ch < 128
	   && glyph->face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      *fontcp = MAKEFONTCP (0, glyph->u.ch);
    }
  else
    {
      int c1, c2, charset;

      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (glyph->u.ch, charset, c1, c2);
      if (charset == CHARSET_ASCII)
	{
	  *fontcp = MAKEFONTCP (0, c1);
	}
      else
	{
	  struct font_info *font_info;
	  if (face->font)
	    {
	      font_info = MW32_FONT_INFO_FROM_FONT (face->font);
	      *fontcp = mw32_encode_char (glyph->u.ch, font_info);
	      if ((two_byte_p) && (*fontcp > 0xFF))
		*two_byte_p = 1;
	    }
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  return face;
}


/* Store one glyph for IT->char_to_display in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
mw32_append_glyph (struct it *it)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  xassert (it->char_to_display != '\n' && it->char_to_display != '\t');
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = CHAR_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = it->glyph_not_available_p;
      glyph->face_id = it->face_id;
      glyph->u.ch = it->char_to_display;
      ++it->glyph_row->used[area];
    }
}

/* Store one glyph for the composition IT->cmp_id in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
mw32_append_composite_glyph (struct it *it)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = COMPOSITE_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.cmp_id = it->cmp_id;
      ++it->glyph_row->used[area];
    }
}


/* Change IT->ascent and IT->height according to the setting of
   IT->voffset.  */

static INLINE void
take_vertical_position_into_account (it)
     struct it *it;
{
  if (it->voffset)
    {
      if (it->voffset < 0)
	/* Increase the ascent so that we can display the text higher
	   in the line.  */
	it->ascent += abs (it->voffset);
      else
	/* Increase the descent so that we can display the text lower
	   in the line.  */
	it->descent += it->voffset;
    }
}


/* Produce glyphs/get display metrics for the image IT is loaded with.
   See the description of struct display_iterator in dispextern.h for
   an overview of struct display_iterator.  */

static void
mw32_produce_image_glyph (struct it *it)
{
  struct image *img;
  struct face *face;

  xassert (it->what == IT_IMAGE);

  face = FACE_FROM_ID (it->f, it->face_id);
  img = IMAGE_FROM_ID (it->f, it->image_id);
  xassert (img);

  /* Make sure X resources of the face and image are loaded.  */
  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  prepare_image_for_display (it->f, img);

  it->ascent = it->phys_ascent = image_ascent (img, face);
  it->descent = it->phys_descent = img->height + 2 * img->vmargin - it->ascent;
  it->pixel_width = img->width + 2 * img->hmargin;

  it->nglyphs = 1;
  
  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }

  take_vertical_position_into_account (it);
  
  if (it->glyph_row)
    {
      struct glyph *glyph;
      enum glyph_row_area area = it->area;
      
      glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
      if (glyph < it->glyph_row->glyphs[area + 1])
	{
	  glyph->charpos = CHARPOS (it->position);
	  glyph->object = it->object;
	  glyph->pixel_width = it->pixel_width;
	  glyph->voffset = it->voffset;
	  glyph->type = IMAGE_GLYPH;
	  glyph->multibyte_p = it->multibyte_p;
	  glyph->left_box_line_p = it->start_of_box_run_p;
	  glyph->right_box_line_p = it->end_of_box_run_p;
	  glyph->overlaps_vertically_p = 0;
          glyph->padding_p = 0;
	  glyph->glyph_not_available_p = 0;
	  glyph->face_id = it->face_id;
	  glyph->u.img_id = img->id;
	  ++it->glyph_row->used[area];
	}
    }
}


/* Append a stretch glyph to IT->glyph_row.  OBJECT is the source
   of the glyph, WIDTH and HEIGHT are the width and height of the 
   stretch.  ASCENT is the percentage/100 of HEIGHT to use for the 
   ascent of the glyph (0 <= ASCENT <= 1).  */
  
static void
mw32_append_stretch_glyph (struct it *it, Lisp_Object object,
			   int width, int height, double ascent)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (ascent >= 0 && ascent <= 1);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = object;
      glyph->pixel_width = width;
      glyph->voffset = it->voffset;
      glyph->type = STRETCH_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = 0;
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.stretch.ascent = (unsigned int)(height * ascent);
      glyph->u.stretch.height = height;
      ++it->glyph_row->used[area];
    }
}


/* Produce a stretch glyph for iterator IT.  IT->object is the value
   of the glyph property displayed.  The value must be a list
   `(space KEYWORD VALUE ...)' with the following KEYWORD/VALUE pairs
   being recognized:

   1. `:width WIDTH' specifies that the space should be WIDTH *
   canonical char width wide.  WIDTH may be an integer or floating 
   point number.

   2. `:relative-width FACTOR' specifies that the width of the stretch
   should be computed from the width of the first character having the
   `glyph' property, and should be FACTOR times that width.

   3. `:align-to HPOS' specifies that the space should be wide enough
   to reach HPOS, a value in canonical character units.

   Exactly one of the above pairs must be present.  

   4. `:height HEIGHT' specifies that the height of the stretch produced
   should be HEIGHT, measured in canonical character units.

   5. `:relative-height FACTOR' specifies that the height of the the
   stretch should be FACTOR times the height of the characters having
   the glyph property.

   Either none or exactly one of 4 or 5 must be present.

   6. `:ascent ASCENT'  specifies that ASCENT percent of the height
   of the stretch should be used for the ascent of the stretch.
   ASCENT must be in the range 0 <= ASCENT <= 100.  */

#define NUMVAL(X)				\
     ((INTEGERP (X) || FLOATP (X))		\
      ? XFLOATINT (X)				\
      : - 1)


static void
mw32_produce_stretch_glyph (struct it *it)
{
  /* (space :width WIDTH :height HEIGHT.  */
#if GLYPH_DEBUG
  extern Lisp_Object Qspace;
#endif
  extern Lisp_Object QCwidth, QCheight, QCascent;
  extern Lisp_Object QCrelative_width, QCrelative_height;
  extern Lisp_Object QCalign_to;
  Lisp_Object prop, plist;
  double width = 0, height = 0, ascent = 0;
  struct face *face = FACE_FROM_ID (it->f, it->face_id);
  MW32LogicalFont *font = face->font ? face->font : FRAME_FONT (it->f);

  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  
  /* List should start with `space'.  */
  xassert (CONSP (it->object) && EQ (XCAR (it->object), Qspace));
  plist = XCDR (it->object);

  /* Compute the width of the stretch.  */
  if (prop = Fplist_get (plist, QCwidth),
      NUMVAL (prop) > 0)
    /* Absolute width `:width WIDTH' specified and valid.  */
    width = NUMVAL (prop) * CANON_X_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_width),
	   NUMVAL (prop) > 0)
    {
      /* Relative width `:relative-width FACTOR' specified and valid.
	 Compute the width of the characters having the `glyph'
	 property.  */
      struct it it2;
      unsigned char *p = BYTE_POS_ADDR (IT_BYTEPOS (*it));
      
      it2 = *it;
      if (it->multibyte_p)
	{
	  int maxlen = ((IT_BYTEPOS (*it) >= GPT ? ZV : GPT)
			- IT_BYTEPOS (*it));
	  it2.c = STRING_CHAR_AND_LENGTH (p, maxlen, it2.len);
	}
      else
	it2.c = *p, it2.len = 1;

      it2.glyph_row = NULL;
      it2.what = IT_CHARACTER;
      mw32i_produce_glyphs (&it2);
      width = NUMVAL (prop) * it2.pixel_width;
    }
  else if (prop = Fplist_get (plist, QCalign_to),
	   NUMVAL (prop) > 0)
    width = NUMVAL (prop) * CANON_X_UNIT (it->f) - it->current_x;
  else
    /* Nothing specified -> width defaults to canonical char width.  */
    width = CANON_X_UNIT (it->f);
  
  /* Compute height.  */
  if (prop = Fplist_get (plist, QCheight),
      NUMVAL (prop) > 0)
    height = NUMVAL (prop) * CANON_Y_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_height),
	   NUMVAL (prop) > 0)
    height = FONT_HEIGHT (font) * NUMVAL (prop);
  else
    height = FONT_HEIGHT (font);

  /* Compute percentage of height used for ascent.  If 
     `:ascent ASCENT' is present and valid, use that.  Otherwise,
     derive the ascent from the font in use.  */
  if (prop = Fplist_get (plist, QCascent),
      NUMVAL (prop) > 0 && NUMVAL (prop) <= 100)
    ascent = NUMVAL (prop) / 100.0;
  else
    ascent = (double) font->ascent / FONT_HEIGHT (font);

  if (width <= 0)
    width = 1;
  if (height <= 0)
    height = 1;

  if (it->glyph_row)
    {
      Lisp_Object object = it->stack[it->sp - 1].string;
      if (!STRINGP (object))
	object = it->w->buffer;
      mw32_append_stretch_glyph (it, object,
				 (int) width, (int) height, ascent);
    }

  it->pixel_width = (int) width;
  it->ascent = it->phys_ascent = (int)(height * ascent);
  it->descent = it->phys_descent = (int)(height - it->ascent);
  it->nglyphs = 1;

  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }
  
  take_vertical_position_into_account (it);
}

/* Return proper value to be used as baseline offset of font that has
   ASCENT and DESCENT to draw characters by the font at the vertical
   center of the line of frame F.

   Here, out task is to find the value of BOFF in the following figure;

	-------------------------+-----------+-
	 -+-+---------+-+        |           |
	  | |         | |        |           |
	  | |         | |        F_ASCENT    F_HEIGHT
	  | |         | ASCENT   |           |
     HEIGHT |         | |        |           |
	  | |         |-|-+------+-----------|------- baseline
	  | |         | | BOFF   |           |
	  | |---------|-+-+      |           |
	  | |         | DESCENT  |           |
	 -+-+---------+-+        F_DESCENT   |
	-------------------------+-----------+-

	-BOFF + DESCENT + (F_HEIGHT - HEIGHT) / 2 = F_DESCENT
	BOFF = DESCENT +  (F_HEIGHT - HEIGHT) / 2 - F_DESCENT
	DESCENT = FONT->descent
	HEIGHT = FONT_HEIGHT (FONT)
	F_DESCENT = (F->output_data.mw32->font->descent
		     - F->output_data.mw32->baseline_offset)
	F_HEIGHT = FRAME_LINE_HEIGHT (F)
*/

#define VCENTER_BASELINE_OFFSET(FONT, F)			\
  ((FONT)->descent						\
   + (FRAME_LINE_HEIGHT ((F)) - FONT_HEIGHT ((FONT))		\
      + (FRAME_LINE_HEIGHT ((F)) > FONT_HEIGHT ((FONT)))) / 2	\
   - ((F)->output_data.mw32->font->descent - (F)->output_data.mw32->baseline_offset))

/* Produce glyphs/get display metrics for the display element IT is
   loaded with.  See the description of struct display_iterator in
   dispextern.h for an overview of struct display_iterator.  */

static void
mw32i_produce_glyphs (struct it *it)
{
  it->glyph_not_available_p = 0;

  if (it->what == IT_CHARACTER)
    {
      FontCp fontcp;
      MW32LogicalFont *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      MW32CharMetric cm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;			/* baseline offset */
      /* We may change it->multibyte_p upon unibyte<->multibyte
	 conversion.  So, save the current value now and restore it
	 later.

	 Note: It seems that we don't have to record multibyte_p in
	 struct glyph because the character code itself tells if or
	 not the character is multibyte.  Thus, in the future, we must
	 consider eliminating the field `multibyte_p' in the struct
	 glyph.  */
      int saved_multibyte_p = it->multibyte_p;

      /* Maybe translate single-byte characters to multibyte, or the
	 other way.  */
      it->char_to_display = it->c;
      if (!ASCII_BYTE_P (it->c))
	{
	  if (unibyte_display_via_language_environment
	      && SINGLE_BYTE_CHAR_P (it->c)
	      && (it->c >= 0240
		  || !NILP (Vnonascii_translation_table)))
	    {
	      it->char_to_display = unibyte_char_to_multibyte (it->c);
	      it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
	  else if (!SINGLE_BYTE_CHAR_P (it->c)
		   && !it->multibyte_p)
	    {
	      it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
	}
      
      /* Get font to use.  Encode IT->char_to_display.  */
      mw32_get_char_face_and_encoding (it->f, it->char_to_display,
				       it->face_id, &fontcp,
				       it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
	{
	  font = FRAME_FONT (it->f);
	  boff = it->f->output_data.mw32->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = MW32_FONT_INFO_FROM_FONT (font);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      if (it->char_to_display >= ' '
	  && (!it->multibyte_p || it->char_to_display < 128))
	{
	  /* Either unibyte or ASCII.  */
	  int stretched_p;

	  it->nglyphs = 1;


	  cm = MW32_INVOKE_METRICPROC (font, FRAME_HDC (it->f), fontcp);
	  it->ascent = font->ascent + boff;
	  it->descent = font->descent - boff;

	  if (MW32_CHARMETRIC_VALID_P (cm))
	    {
	      it->phys_ascent = cm.ascent + boff;
	      it->phys_descent = cm.descent - boff;
	      it->pixel_width = cm.width;
	    }
	  else
	    {
	      it->glyph_not_available_p = 1;
	      it->phys_ascent = font->ascent + boff;
	      it->phys_descent = font->descent - boff;
	      it->pixel_width = FONT_WIDTH (font);
	    }

	  /* If this is a space inside a region of text with
	     `space-width' property, change its width.  */
	  stretched_p = it->char_to_display == ' ' && !NILP (it->space_width);
	  if (stretched_p)
	    it->pixel_width = (int)(it->pixel_width
				    * XFLOATINT (it->space_width));

	  /* If face has a box, add the box thickness to the character
	     height.  If character has a box line to the left and/or
	     right, add the box line width to the character's width.  */
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;
	      
	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = -thick;

	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }

	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  /* If we have to actually produce glyphs, do it.  */
	  if (it->glyph_row)
	    {
	      if (stretched_p)
		{
		  /* Translate a space with a `space-width' property
		     into a stretch glyph.  */
		  double ascent = (double) font->ascent / FONT_HEIGHT (font);
		  mw32_append_stretch_glyph (it, it->object, it->pixel_width, 
					     it->ascent + it->descent, ascent);
		}
	      else
		mw32_append_glyph (it);

	      /* If characters with lbearing or rbearing are displayed
		 in this line, record that fact in a flag of the
		 glyph row.  This is used to optimize X output code.  */
	      if (MW32_CHARMETRIC_VALID_P (cm)
		  && ((cm.overhang > 0) || cm.roverhang > 0))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	}
      else if (it->char_to_display == '\n')
	{
	  /* A newline has no width but we need the height of the line.  */
	  it->pixel_width = 0;
	  it->nglyphs = 0;
	  it->ascent = it->phys_ascent = font->ascent + boff;
	  it->descent = it->phys_descent = font->descent - boff;
      
	  if (face->box != FACE_NO_BOX
	      && face->box_line_width > 0)
	    {
	      it->ascent += face->box_line_width;
	      it->descent += face->box_line_width;
	    }
	}
      else if (it->char_to_display == '\t')
	{
	  int tab_width = it->tab_width * CANON_X_UNIT (it->f);
	  int x = it->current_x + it->continuation_lines_width;
	  int next_tab_x = ((1 + x + tab_width - 1) / tab_width) * tab_width;

	  /* If the distance from the current position to the next tab
	     stop is less than a canonical character width, use the
	     tab stop after that.  */
	  if (next_tab_x - x < CANON_X_UNIT (it->f))
	    next_tab_x += tab_width;
      
	  it->pixel_width = next_tab_x - x;
	  it->nglyphs = 1;
	  it->ascent = it->phys_ascent = font->ascent + boff;
	  it->descent = it->phys_descent = font->descent - boff;
	  
	  if (it->glyph_row)
	    {
	      double ascent = (double) it->ascent / (it->ascent + it->descent);
	      mw32_append_stretch_glyph (it, it->object, it->pixel_width, 
					 it->ascent + it->descent, ascent);
	    }
	}
      else 
	{
	  /* A multi-byte character.  Assume that the display width of the
	     character is the width of the character multiplied by the
	     width of the font.  */

	  /* If we found a font, this font should give us the right
	     metrics.  If we didn't find a font, use the frame's
	     default font and calculate the width of the character
	     from the charset width; this is what old redisplay code
	     did.  */
	  cm = MW32_INVOKE_METRICPROC (font, FRAME_HDC (it->f), fontcp);
	  if (font_not_found_p || !MW32_CHARMETRIC_VALID_P (cm))
	    {
	      int charset = CHAR_CHARSET (it->char_to_display);

	      it->glyph_not_available_p = 1;
	      it->pixel_width = (FONT_WIDTH (FRAME_FONT (it->f))
				 * CHARSET_WIDTH (charset));
	      it->phys_ascent = font->ascent + boff;
	      it->phys_descent = font->descent - boff;
	    }
	  else
	    {
	      it->pixel_width = cm.width;
	      it->phys_ascent = cm.ascent + boff;
	      it->phys_descent = cm.descent - boff;
	      if (it->glyph_row
		  && ((cm.overhang > 0) || (cm.roverhang > 0)))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	  it->nglyphs = 1;
	  it->ascent = font->ascent + boff;
	  it->descent = font->descent - boff;
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;

	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = - thick;
	  
	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }
  
	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  if (it->glyph_row)
	    mw32_append_glyph (it);
	}
      it->multibyte_p = saved_multibyte_p;
    }
  else if (it->what == IT_COMPOSITION)
    {
      /* Note: A composition is represented as one glyph in the
	 glyph matrix.  There are no padding glyphs.  */
      FontCp fontcp;
      MW32LogicalFont *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      MW32CharMetric cm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;			/* baseline offset */
      struct composition *cmp = composition_table[it->cmp_id];

      /* Maybe translate single-byte characters to multibyte.  */
      it->char_to_display = it->c;
      if (unibyte_display_via_language_environment
	  && SINGLE_BYTE_CHAR_P (it->c)
	  && (it->c >= 0240
	      || (it->c >= 0200
		  && !NILP (Vnonascii_translation_table))))
	{
	  it->char_to_display = unibyte_char_to_multibyte (it->c);
	}
      
      /* Get face and font to use.  Encode IT->char_to_display.  */
      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
      face = FACE_FROM_ID (it->f, it->face_id);
      mw32_get_char_face_and_encoding (it->f, it->char_to_display,
				       it->face_id, &fontcp, it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
	{
	  font = FRAME_FONT (it->f);
	  boff = it->f->output_data.mw32->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = MW32_FONT_INFO_FROM_FONT (font);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      /* There are no padding glyphs, so there is only one glyph to
	 produce for the composition.  Important is that pixel_width,
	 ascent and descent are the values of what is drawn by
	 draw_glyphs (i.e. the values of the overall glyphs composed).  */
      it->nglyphs = 1;

      /* If we have not yet calculated pixel size data of glyphs of
	 the composition for the current face font, calculate them
	 now.  Theoretically, we have to check all fonts for the
	 glyphs, but that requires much time and memory space.  So,
	 here we check only the font of the first glyph.  This leads
	 to incorrect display very rarely, and C-l (recenter) can
	 correct the display anyway.  */
      if (cmp->font != (void *) font)
	{
	  /* Ascent and descent of the font of the first character of
	     this composition (adjusted by baseline offset).  Ascent
	     and descent of overall glyphs should not be less than
	     them respectively.  */
	  int font_ascent = font->ascent + boff;
	  int font_descent = font->descent - boff;
	  /* Bounding box of the overall glyphs.  */
	  int leftmost, rightmost, lowest, highest;
	  int i, width, ascent, descent;

	  cmp->font = (void *) font;

	  /* Initialize the bounding box.  */
	  cm = MW32_INVOKE_METRICPROC (font, FRAME_HDC (it->f), fontcp);
	  if (font_info && MW32_CHARMETRIC_VALID_P (cm))
	    {
	      width = cm.width;
	      ascent = cm.ascent;
	      descent = cm.descent;
	    }
	  else
	    {
	      width = FONT_WIDTH (font);
	      ascent = font->ascent;
	      descent = font->descent;
	    }
	  
	  rightmost = width;
	  lowest = - descent + boff;
	  highest = ascent + boff;
	  leftmost = 0;
	  
	  if (font_info
	      && font_info->default_ascent
	      && CHAR_TABLE_P (Vuse_default_ascent)
	      && !NILP (Faref (Vuse_default_ascent,
			       make_number (it->char_to_display))))
	    highest = font_info->default_ascent + boff;

	  /* Draw the first glyph at the normal position.  It may be
	     shifted to right later if some other glyphs are drawn at
	     the left.  */
	  cmp->offsets[0] = 0;
	  cmp->offsets[1] = boff;

	  /* Set cmp->offsets for the remaining glyphs.  */
	  for (i = 1; i < cmp->glyph_len; i++)
	    {
	      int left, right, btm, top;
	      int ch = COMPOSITION_GLYPH (cmp, i);
	      int face_id = FACE_FOR_CHAR (it->f, face, ch);
	      
	      face = FACE_FROM_ID (it->f, face_id);
	      mw32_get_char_face_and_encoding (it->f, ch, face->id, &fontcp,
					       it->multibyte_p);
	      font = face->font;
	      if (font == NULL)
		{
		  font = FRAME_FONT (it->f);
		  boff = it->f->output_data.mw32->baseline_offset;
		  font_info = NULL;
		}
	      else
		{
		  font_info
		    = MW32_FONT_INFO_FROM_FONT (font);
		  boff = font_info->baseline_offset;
		  if (font_info->vertical_centering)
		    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
		}

	      cm = MW32_INVOKE_METRICPROC (font, FRAME_HDC (it->f), fontcp);
	      if (font_info && MW32_CHARMETRIC_VALID_P (cm))
		{
		  width = cm.width;
		  ascent = cm.ascent;
		  descent = cm.descent;
		}
	      else
		{
		  width = FONT_WIDTH (font);
		  ascent = 1;
		  descent = 0;
		}

	      if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
		{
		  /* Relative composition with or without
		     alternate chars.  */
		  left = (leftmost + rightmost - width) / 2;
		  btm = - descent + boff;
		  if (font_info && font_info->relative_compose
		      && (! CHAR_TABLE_P (Vignore_relative_composition)
			  || NILP (Faref (Vignore_relative_composition,
					  make_number (ch)))))
		    {

		      if (- descent >= font_info->relative_compose)
			/* One extra pixel between two glyphs.  */
			btm = highest + 1;
		      else if (ascent <= 0)
			/* One extra pixel between two glyphs.  */
			btm = lowest - 1 - ascent - descent;
		    }
		}
	      else
		{
		  /* A composition rule is specified by an integer
		     value that encodes global and new reference
		     points (GREF and NREF).  GREF and NREF are
		     specified by numbers as below:

			0---1---2 -- ascent
			|       |
			|       |
			|       |
			9--10--11 -- center
			|       |
		     ---3---4---5--- baseline
			|       |
			6---7---8 -- descent
		  */
		  int rule = COMPOSITION_RULE (cmp, i);
		  int gref, nref, grefx, grefy, nrefx, nrefy;

		  COMPOSITION_DECODE_RULE (rule, gref, nref);
		  grefx = gref % 3, nrefx = nref % 3;
		  grefy = gref / 3, nrefy = nref / 3;

		  left = (leftmost
			  + grefx * (rightmost - leftmost) / 2
			  - nrefx * width / 2);
		  btm = ((grefy == 0 ? highest
			  : grefy == 1 ? 0
			  : grefy == 2 ? lowest
			  : (highest + lowest) / 2)
			 - (nrefy == 0 ? ascent + descent
			    : nrefy == 1 ? descent - boff
			    : nrefy == 2 ? 0
			    : (ascent + descent) / 2));
		}

	      cmp->offsets[i * 2] = left;
	      cmp->offsets[i * 2 + 1] = btm + descent;

	      /* Update the bounding box of the overall glyphs. */
	      right = left + width;
	      top = btm + descent + ascent;
	      if (left < leftmost)
		leftmost = left;
	      if (right > rightmost)
		rightmost = right;
	      if (top > highest)
		highest = top;
	      if (btm < lowest)
		lowest = btm;
	    }

	  /* If there are glyphs whose x-offsets are negative,
	     shift all glyphs to the right and make all x-offsets
	     non-negative.  */
	  if (leftmost < 0)
	    {
	      for (i = 0; i < cmp->glyph_len; i++)
		cmp->offsets[i * 2] -= leftmost;
	      rightmost -= leftmost;
	    }

	  cmp->pixel_width = rightmost;
	  cmp->ascent = highest;
	  cmp->descent = - lowest;
	  if (cmp->ascent < font_ascent)
	    cmp->ascent = font_ascent;
	  if (cmp->descent < font_descent)
	    cmp->descent = font_descent;
	}

      it->pixel_width = cmp->pixel_width;
      it->ascent = it->phys_ascent = cmp->ascent;
      it->descent = it->phys_descent = cmp->descent;

      if (face->box != FACE_NO_BOX)
	{
	  int thick = face->box_line_width;

	  if (thick > 0)
	    {
	      it->ascent += thick;
	      it->descent += thick;
	    }
	  else
	    thick = - thick;
	  
	  if (it->start_of_box_run_p)
	    it->pixel_width += thick;
	  if (it->end_of_box_run_p)
	    it->pixel_width += thick;
	}
  
      /* If face has an overline, add the height of the overline
	 (1 pixel) and a 1 pixel margin to the character height.  */
      if (face->overline_p)
	it->ascent += 2;

      take_vertical_position_into_account (it);
  
      if (it->glyph_row)
	mw32_append_composite_glyph (it);
    }
  else if (it->what == IT_IMAGE)
    mw32_produce_image_glyph (it);
  else if (it->what == IT_STRETCH)
    mw32_produce_stretch_glyph (it);

  /* Accumulate dimensions.  Note: can't assume that it->descent > 0
     because this isn't true for images with `:ascent 100'.  */
#if 0 /* we should reconsider this assertion! */
  xassert (it->ascent >= 0 && it->descent >= 0);
#endif
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;
  
  it->descent += it->extra_line_spacing;
  
  it->max_ascent = max (it->max_ascent, it->ascent);
  it->max_descent = max (it->max_descent, it->descent);
  it->max_phys_ascent = max (it->max_phys_ascent, it->phys_ascent);
  it->max_phys_descent = max (it->max_phys_descent, it->phys_descent);
}


/* Estimate the pixel height of the mode or top line on frame F.
   FACE_ID specifies what line's height to estimate.  */

int
MW32_estimate_mode_line_height (struct frame *f, enum face_id face_id)
{
  int height = FONT_HEIGHT (FRAME_FONT (f));

  /* This function is called so early when Emacs starts that the face
     cache and mode line face are not yet initialized.  */
  if (FRAME_FACE_CACHE (f))
      {
	struct face *face = FACE_FROM_ID (f, face_id);
	if (face)
	  {
	    if (face->font)
	      height = FONT_HEIGHT (face->font);
	    if (face->box_line_width > 0)
	      height += 2 * face->box_line_width;
	  }
      }
  
  return height;
}


/***********************************************************************
			    Glyph display
 ***********************************************************************/

/* A sequence of glyphs to be drawn in the same face.

   This data structure is not really completely X specific, so it
   could possibly, at least partially, be useful for other systems.  It
   is currently not part of the external redisplay interface because
   it's not clear what other systems will need.  */

struct glyph_string
{
  /* X-origin of the string.  */
  int x;

  /* Y-origin and y-position of the base line of this string.  */
  int y, ybase;

  /* The width of the string, not including a face extension.  */
  int width;

  /* The width of the string, including a face extension.  */
  int background_width;

  /* The height of this string.  This is the height of the line this
     string is drawn in, and can be different from the height of the
     font the string is drawn in.  */
  int height;

  /* Number of pixels this string overwrites in front of its x-origin.
     This number is zero if the string has an lbearing >= 0; it is
     -lbearing, if the string has an lbearing < 0.  */
  int left_overhang;

  /* Number of pixels this string overwrites past its right-most
     nominal x-position, i.e. x + width.  Zero if the string's
     rbearing is <= its nominal width, rbearing - width otherwise.  */
  int right_overhang;

  /* The frame on which the glyph string is drawn.  */
  struct frame *f;

  /* The window on which the glyph string is drawn.  */
  struct window *w;

  /* window for convenience.  */
  /* Display *display; */
  HWND window;

  /* Frame's HDC. */
  HDC hdc;

  /* The glyph row for which this string was built.  It determines the
     y-origin and height of the string.  */
  struct glyph_row *row;

  /* The area within row.  */
  enum glyph_row_area area;

  /* Characters to be drawn, and number of characters.  */
  FontCp *pfcp;
  unsigned char *pstr;
  int nchars;
  int nbytes;
  /* Glyph Layout Information */
  int *pdx;

  /* A face-override for drawing cursors, mouse face and similar.  */
  enum draw_glyphs_face hl;

  /* Face in which this string is to be drawn.  */
  struct face *face;

  /* Font in which this string is to be drawn.  */
  MW32LogicalFont *font;

  /* Font info for this string.  */
  struct font_info *font_info;

  /* Non-null means this string describes (part of) a composition.
     All characters from char2b are drawn composed.  */
  struct composition *cmp;

  /* Index of this glyph string's first character in the glyph
     definition of CMP.  If this is zero, this glyph string describes
     the first character of a composition.  */
  int gidx;

  /* 1 means this glyph strings face has to be drawn to the right end
     of the window's drawing area.  */
  unsigned extends_to_end_of_line_p : 1;

  /* 1 means the background of this string has been drawn.  */
  unsigned background_filled_p : 1;

  /* 1 means glyph string must be drawn with 16-bit functions.  */
  unsigned two_byte_p : 1;

  /* 1 means that the original font determined for drawing this glyph
     string could not be loaded.  The member `font' has been set to
     the frame's default font in this case.  */
  unsigned font_not_found_p : 1;

  /* 1 means that the face in which this glyph string is drawn has a
     stipple pattern.  */
  unsigned stippled_p : 1;

  /* 1 means only the foreground of this glyph string must be drawn,
     and we should use the physical height of the line this glyph
     string appears in as clip rect.  */
  unsigned for_overlaps_p : 1;

  /* A pointer to the first glyph in the string.  This glyph
     corresponds to char2b[0].  Needed to draw rectangles if
     font_not_found_p is 1.  */
  struct glyph *first_glyph;

  /* Image, if any.  */
  struct image *img;

  struct glyph_string *next, *prev;
};


#if 1

static void
mw32_dump_glyph_string (s)
     struct glyph_string *s;
{
  fprintf (stderr, "glyph string\n");
  fprintf (stderr, "  x, y, w, h = %d, %d, %d, %d\n",
	   s->x, s->y, s->width, s->height);
  fprintf (stderr, "  ybase = %d\n", s->ybase);
  fprintf (stderr, "  hl = %d\n", s->hl);
  fprintf (stderr, "  left overhang = %d, right = %d\n",
	   s->left_overhang, s->right_overhang);
  fprintf (stderr, "  nchars = %d\n", s->nchars);
  fprintf (stderr, "  extends to end of line = %d\n",
	   s->extends_to_end_of_line_p);
  fprintf (stderr, "  font height = %d\n", FONT_HEIGHT (s->font));
  fprintf (stderr, "  bg width = %d\n", s->background_width);
}

#endif /* GLYPH_DEBUG */



static void mw32_append_glyph_string_lists P_ ((struct glyph_string **,
						struct glyph_string **,
						struct glyph_string *,
						struct glyph_string *));
static void mw32_prepend_glyph_string_lists P_ ((struct glyph_string **,
						 struct glyph_string **,
						 struct glyph_string *,
						 struct glyph_string *));
static void mw32_append_glyph_string P_ ((struct glyph_string **,
					  struct glyph_string **,
					  struct glyph_string *));
static int mw32_left_overwritten P_ ((struct glyph_string *));
static int mw32_left_overwriting P_ ((struct glyph_string *));
static int mw32_right_overwritten P_ ((struct glyph_string *));
static int mw32_right_overwriting P_ ((struct glyph_string *));
static int mw32_fill_glyph_string P_ ((struct glyph_string *, int, int, int,
				       int));
static void mw32_init_glyph_string P_ ((struct glyph_string *,
					FontCp *, unsigned char *, int *,
					struct window *,
					struct glyph_row *,
					enum glyph_row_area, int, 
					enum draw_glyphs_face));
static int mw32_draw_glyphs P_ ((struct window *, int , struct glyph_row *,
				 enum glyph_row_area, int, int,
				 enum draw_glyphs_face, int *, int *, int));
static void mw32_set_glyph_string_clipping P_ ((struct glyph_string *));
static void mw32_set_glyph_string_gc P_ ((struct glyph_string *));
static void mw32_draw_glyph_string_background P_ ((struct glyph_string *,
						int));
static void mw32_draw_glyph_string_foreground P_ ((struct glyph_string *));
static void mw32_draw_composite_glyph_string_foreground P_ ((struct glyph_string *));
static void mw32_draw_glyph_string_box P_ ((struct glyph_string *));
static void mw32_draw_glyph_string  P_ ((struct glyph_string *));
static void mw32_compute_glyph_string_overhangs P_ ((struct glyph_string *));
static void mw32_set_cursor_gc P_ ((struct glyph_string *));
static void mw32_set_mode_line_face_gc P_ ((struct glyph_string *));
static void mw32_set_mouse_face_gc P_ ((struct glyph_string *));
static void mw32i_get_glyph_overhangs P_ ((struct glyph *, struct frame *,
					  int *, int *));
static void mw32_compute_overhangs_and_x P_ ((struct glyph_string *, int, int));
static PIX_TYPE mw32_get_lighter_color P_ ((struct frame *f, PIX_TYPE base,
					    double factor, int delta));
static void mw32_setup_relief_color P_ ((struct frame *, struct relief *,
					 double, int, unsigned long));
static void mw32_setup_relief_colors P_ ((struct glyph_string *));
static void mw32_draw_image_glyph_string P_ ((struct glyph_string *));
static void mw32_draw_image_relief P_ ((struct glyph_string *));
static void mw32_draw_image_foreground P_ ((struct glyph_string *));
static void mw32_fill_image_glyph_string P_ ((struct glyph_string *));
static void mw32_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					      int, int, int));
static void mw32_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				       int, int, int, int, HRGN));
static void mw32_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				    int, int, int, HRGN));
static void mw32_fix_overlapping_area P_ ((struct window *, struct glyph_row *,
					   enum glyph_row_area));
static int mw32_fill_stretch_glyph_string P_ ((struct glyph_string *,
					       struct glyph_row *,
					       enum glyph_row_area, int, int));

#if GLYPH_DEBUG
static void mw32_check_font P_ ((struct frame *, MW32LogicalFont *));
#endif


/* Append the list of glyph strings with head H and tail T to the list
   with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the result.  */

static INLINE void
mw32_append_glyph_string_lists (struct glyph_string **head,
				struct glyph_string **tail,
				struct glyph_string *h,
				struct glyph_string *t)
{
  if (h)
    {
      if (*head)
	(*tail)->next = h;
      else
	*head = h;
      h->prev = *tail;
      *tail = t;
    }
}


/* Prepend the list of glyph strings with head H and tail T to the
   list with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the
   result.  */

static INLINE void
mw32_prepend_glyph_string_lists (struct glyph_string **head,
				 struct glyph_string **tail,
				 struct glyph_string *h,
				 struct glyph_string *t)
{
  if (h)
    {
      if (*head)
	(*head)->prev = t;
      else
	*tail = t;
      t->next = *head;
      *head = h;
    }
}


/* Append glyph string S to the list with head *HEAD and tail *TAIL.
   Set *HEAD and *TAIL to the resulting list.  */

static INLINE void
mw32_append_glyph_string (struct glyph_string **head,
			  struct glyph_string **tail,
			  struct glyph_string *s)
{
  s->next = s->prev = NULL;
  mw32_append_glyph_string_lists (head, tail, s, s);
}


static void
mw32_generic_setup_face_hdc (struct glyph_string *s)
{
  SetTextColor (s->hdc, s->face->foreground);
  SetBkColor (s->hdc, s->face->background);
}

/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
mw32_setup_cursor_hdc (struct glyph_string *s)
{
  COLORREF fg, bg;
  bg = s->f->output_data.mw32->cursor_pixel;
  fg = s->face->background;
  if (fg == bg)
    fg = s->face->foreground;
  if (fg == bg)
    fg = s->f->output_data.mw32->cursor_foreground_pixel;
  if (fg == bg)
    fg = s->face->foreground;
  /* Make sure the cursor is distinct from text in this face.  */
  if (bg == s->face->background
      && fg == s->face->foreground)
    {
      bg = s->face->foreground;
      fg = s->face->background;
    }

  SetTextColor (s->hdc, fg);
  SetBkColor (s->hdc, bg);
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */
   
static void
mw32_setup_mouse_face_hdc (struct glyph_string *s)
{     
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = FRAME_MW32_DISPLAY_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
  
  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0);
  s->face = FACE_FROM_ID (s->f, face_id);
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  mw32_generic_setup_face_hdc (s);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static INLINE void
mw32_setup_mode_line_face_hdc (struct glyph_string *s)
{     
  mw32_generic_setup_face_hdc (s);
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static INLINE void
mw32_setup_glyph_string_hdc (struct glyph_string *s)
{
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);
  
  if (s->hl == DRAW_NORMAL_TEXT)
    {
      mw32_generic_setup_face_hdc (s);
      s->stippled_p = (s->face->stipple != 0);
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      mw32_setup_mode_line_face_hdc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      mw32_setup_cursor_hdc (s);
      s->stippled_p = 0;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      mw32_setup_mouse_face_hdc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      mw32_generic_setup_face_hdc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else
    {
      s->stippled_p = s->face->stipple != 0;
    }
}


/* Return in *R the clipping rectangle for glyph string S.  */

static void
mw32_get_glyph_string_clip_rect (struct glyph_string *s, LPRECT lpr)
{
  int l, w, t, h;
  if (s->row->full_width_p)
    {
      /* Draw full-width.  X coordinates are relative to S->w->left.  */
      int canon_x = CANON_X_UNIT (s->f);
      
      l = WINDOW_LEFT_MARGIN (s->w) * canon_x;
      w = XFASTINT (s->w->width) * canon_x;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (s->f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (s->f) * canon_x;
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (s->f))
	    l -= width;
	}
      
      l += FRAME_INTERNAL_BORDER_WIDTH (s->f);
      
      /* Unless displaying a mode or menu bar line, which are always
	 fully visible, clip to the visible part of the row.  */
      if (s->w->pseudo_window_p)
	h = s->row->visible_height;
      else
	h = s->height;
    }
  else
    {
      /* This is a text line that may be partially visible.  */
      l = WINDOW_AREA_TO_FRAME_PIXEL_X (s->w, s->area, 0);
      w = window_box_width (s->w, s->area);
      h = s->row->visible_height;
    }

  /* If S draws overlapping rows, it's sufficient to use the top and
     bottom of the window for clipping because this glyph string
     intentionally draws over other lines.  */
  if (s->for_overlaps_p)
    {
      t = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      h = window_text_bottom_y (s->w) - t;
    }
  else
    {
      /* Don't use S->y for clipping because it doesn't take partially
	 visible lines into account.  For example, it can be negative for
	 partially visible lines at the top of a window.  */
      if (!s->row->full_width_p
	  && MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (s->w, s->row))
	t = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      else
	t = max (0, s->row->y);

      /* If drawing a tool-bar window, draw it over the internal border
	 at the top of the window.  */
      if (s->w == XWINDOW (s->f->tool_bar_window))
	t -= s->f->output_data.mw32->internal_border_width;
    }
      
  t = WINDOW_TO_FRAME_PIXEL_Y (s->w, t);

  lpr->left = l;
  lpr->top = t;
  lpr->right = l + w;
  lpr->bottom = t + h;
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static INLINE void
mw32_set_glyph_string_clipping (struct glyph_string *s)
{
  RECT r;
  HRGN region;
  mw32_get_glyph_string_clip_rect (s, &r);
  region = CreateRectRgnIndirect (&r);
  SelectClipRgn (s->hdc, region);
  DeleteObject (region);
}


/* Compute overhangs and x-positions for glyph string S and its
   predecessors, or successors.  X is the starting x-position for S.
   BACKWARD_P non-zero means process predecessors.  */
   
static void
mw32_compute_glyph_string_x (struct glyph_string *s,
			     int x, int backward_p)
{
  if (backward_p)
    {
      while (s)
	{
	  x -= s->width;
	  s->x = x;
	  s = s->prev;
	}
    }
  else
    {
      while (s)
	{
	  s->x = x;
	  x += s->width;
	  s = s->next;
	}
    }
}


/* Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
   frame F.  Overhangs of glyphs other than type CHAR_GLYPH are
   assumed to be zero.  */

static void
mw32i_get_glyph_overhangs (struct glyph *glyph,
			   struct frame *f,
			   int *left, int *right)
{
  *left = *right = 0;
  
  if (glyph->type == CHAR_GLYPH)
    {
      MW32LogicalFont *font;
      struct face *face;
      struct font_info *font_info;
      FontCp fontcp;
      MW32CharMetric cm;

      face = mw32_get_glyph_face_and_encoding (f, glyph, &fontcp, NULL);
      font = face->font;
      if (font)
	{
	  font_info = MW32_FONT_INFO_FROM_FONT (font);
	  cm = MW32_INVOKE_METRICPROC (font, FRAME_HDC (f), fontcp);
	  if (MW32_CHARMETRIC_VALID_P (cm))
	    {
	      *right = cm.overhang;
	      *left = cm.roverhang;
	    }
	}
    }
}


/* Return the index of the first glyph preceding glyph string S that
   is overwritten by S because of S's left overhang.  Value is -1
   if no glyphs are overwritten.  */

static int
mw32_left_overwritten (struct glyph_string *s)
{
  int k;
    
  if (s->left_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = s->first_glyph - glyphs;

      for (i = first - 1; i >= 0 && x > -s->left_overhang; --i)
	x -= glyphs[i].pixel_width;

      k = i + 1;
    }
  else
    k = -1;

  return k;
}


/* Return the index of the first glyph preceding glyph string S that
   is overwriting S because of its right overhang.  Value is -1 if no
   glyph in front of S overwrites S.  */

static int
mw32_left_overwriting (struct glyph_string *s)
{
  int i, k, x;
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = s->first_glyph - glyphs;

  k = -1;
  x = 0;
  for (i = first - 1; i >= 0; --i)
    {
      int left, right;
      mw32i_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x + right > 0)
	k = i;
      x -= glyphs[i].pixel_width;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that is
   not overwritten by S because of S's right overhang.  Value is -1 if
   no such glyph is found.  */

static int
mw32_right_overwritten (struct glyph_string *s)
{
  int k = -1;

  if (s->right_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);
      int end = s->row->used[s->area];
      
      for (i = first; i < end && s->right_overhang > x; ++i)
	x += glyphs[i].pixel_width;

      k = i;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that
   overwrites S because of its left overhang.  Value is negative
   if no such glyph is found.  */

static int
mw32_right_overwriting (struct glyph_string *s)
{
  int i, k, x;
  int end = s->row->used[s->area];
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);

  k = -1;
  x = 0;
  for (i = first; i < end; ++i)
    {
      int left, right;
      mw32i_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x - left < 0)
	k = i;
      x += glyphs[i].pixel_width;
    }

  return k;
}


/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static INLINE void
mw32_clear_glyph_string_rect (struct glyph_string *s,
			      int x, int y, int w, int h)
{
  PIX_TYPE pix;
  pix = GetBkColor (s->hdc);
  mw32_fill_area_pix (s->f, pix, x, y, x + w, y + h);
}


/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
mw32_draw_glyph_string_background (struct glyph_string *s,
				   int force_p)
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_line_width, 0);

#if 0
      if (s->stippled_p)
	{
	  /* TODO: stipple support */
	  s->background_filled_p = 1;
	}
#endif
      if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	  || s->font_not_found_p
	  || s->extends_to_end_of_line_p
	  || force_p)
	{
	  mw32_fill_area_pix (s->f, s->face->background,
			      s->x, s->y + box_line_width,
			      s->x + s->background_width,
			      s->y + s->height - box_line_width);
	  s->background_filled_p = 1;
	}
    }
}


/* Draw the foreground of glyph string S.  */

static void
mw32_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  Rectangle (s->hdc, x, s->y,
		     x + g->pixel_width - 1, s->y + s->height - 1);
	  x += g->pixel_width;
	}
    }
  else
    {
      RECT r;
      int boff = s->font_info->baseline_offset;

      if (s->font_info->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (s->font, s->f) - boff;

      r.left = s->x;
      r.top = s->y;
      r.right = s->x + s->width;
      r.bottom = s->y + s->height;

      /* Draw text.  If background has already been filled, don't fill
	 background.  But when drawing the cursor, always fill background
	 bacause there is no chance that characters under a box cursor
	 are invisible.  */
      if (s->for_overlaps_p
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	{
	  MW32_INVOKE_OUTPUTPROC (s->font, s->hdc, s->pstr,
				  x, s->ybase - boff,
				  s->nbytes, s->pdx, &r, 0);
	}
      else
	{
	  MW32_INVOKE_OUTPUTPROC (s->font, s->hdc, s->pstr,
				  x, s->ybase - boff,
				  s->nbytes, s->pdx, &r, ETO_OPAQUE);
	}
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
mw32_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->gidx is the index of
     the first character drawn for glyphs of this composition.
     S->gidx == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->gidx == 0)
	Rectangle (s->hdc, x, s->y, x + s->width - 1, s->y + s->height - 1);
    }
  else
    {
      unsigned char str[MAXBYTES1FCP];
      unsigned char *pstr;
      RECT r;
      int dx = 0;
      
      r.left = s->x;
      r.top = s->y;
      r.right = s->x + s->width;
      r.bottom = s->y + s->height;
      for (i = 0; i < s->nchars; i++, ++s->gidx)
	{
	  pstr = str;
	  SERIALIZE_FONTCP (pstr, (s->pfcp)[i]);


	  MW32_INVOKE_OUTPUTPROC (s->font, s->hdc, str,
				  x + s->cmp->offsets[s->gidx * 2],
				  s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
				  pstr - str, &dx, &r, 0);
	}
    }
}

#if 0 /* We should create color management system and move it to
	 mw32color.c later. */

/* Value is an array of XColor structures for the contents of the
   color map of display DPY.  Set *NCELLS to the size of the array.
   Note that this probably shouldn't be called for large color maps,
   say a 24-bit TrueColor map.  */

static const XColor *
x_color_cells (dpy, ncells)
     Display *dpy;
     int *ncells;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);

  if (dpyinfo->color_cells == NULL)
    {
      Screen *screen = dpyinfo->screen;
      int i;
      
      dpyinfo->ncolor_cells
	= XDisplayCells (dpy, XScreenNumberOfScreen (screen));
      dpyinfo->color_cells
	= (XColor *) xmalloc (dpyinfo->ncolor_cells
			      * sizeof *dpyinfo->color_cells);
      
      for (i = 0; i < dpyinfo->ncolor_cells; ++i)
	dpyinfo->color_cells[i].pixel = i;
      
      XQueryColors (dpy, dpyinfo->cmap,
		    dpyinfo->color_cells, dpyinfo->ncolor_cells);
    }

  *ncells = dpyinfo->ncolor_cells;
  return dpyinfo->color_cells;
}


/* On frame F, translate pixel colors to RGB values for the NCOLORS
   colors in COLORS.  Use cached information, if available.  */

void
x_query_colors (f, colors, ncolors)
     struct frame *f;
     XColor *colors;
     int ncolors;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (dpyinfo->color_cells)
    {
      int i;
      for (i = 0; i < ncolors; ++i)
	{
	  unsigned long pixel = colors[i].pixel;
	  xassert (pixel < dpyinfo->ncolor_cells);
	  xassert (dpyinfo->color_cells[pixel].pixel == pixel);
	  colors[i] = dpyinfo->color_cells[pixel];
	}
    }
  else
    XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors, ncolors);
}


/* On frame F, translate pixel color to RGB values for the color in
   COLOR.  Use cached information, if available.  */

void
x_query_color (f, color)
     struct frame *f;
     XColor *color;
{
  x_query_colors (f, color, 1);
}
     

/* Allocate the color COLOR->pixel on DISPLAY, colormap CMAP.  If an
   exact match can't be allocated, try the nearest color available.
   Value is non-zero if successful.  Set *COLOR to the color
   allocated.  */

static int
x_alloc_nearest_color_1 (dpy, cmap, color)
     Display *dpy;
     Colormap cmap;
     XColor *color;
{
  int rc;

  rc = XAllocColor (dpy, cmap, color);
  if (rc == 0)
    {
      /* If we got to this point, the colormap is full, so we're going
	 to try to get the next closest color.  The algorithm used is
	 a least-squares matching, which is what X uses for closest
	 color matching with StaticColor visuals.  */
      int nearest, i;
      unsigned long nearest_delta = ~0;
      int ncells;
      const XColor *cells = x_color_cells (dpy, &ncells);

      for (nearest = i = 0; i < ncells; ++i)
	{
	  long dred   = (color->red   >> 8) - (cells[i].red   >> 8);
	  long dgreen = (color->green >> 8) - (cells[i].green >> 8);
	  long dblue  = (color->blue  >> 8) - (cells[i].blue  >> 8);
	  unsigned long delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (delta < nearest_delta)
	    {
	      nearest = i;
	      nearest_delta = delta;
	    }
	}
      
      color->red   = cells[nearest].red;
      color->green = cells[nearest].green;
      color->blue  = cells[nearest].blue;
      rc = XAllocColor (dpy, cmap, color);
    }
  else
    {
      /* If allocation succeeded, and the allocated pixel color is not
         equal to a cached pixel color recorded earlier, there was a
         change in the colormap, so clear the color cache.  */
      struct x_display_info *dpyinfo = x_display_info_for_display (dpy);
      XColor *cached_color;
      
      if (dpyinfo->color_cells
	  && (cached_color = &dpyinfo->color_cells[color->pixel],
	      (cached_color->red != color->red
	       || cached_color->blue != color->blue
	       || cached_color->green != color->green)))
	{
	  xfree (dpyinfo->color_cells);
	  dpyinfo->color_cells = NULL;
	  dpyinfo->ncolor_cells = 0;
	}
    }

#ifdef DEBUG_X_COLORS
  if (rc)
    register_color (color->pixel);
#endif /* DEBUG_X_COLORS */
  
  return rc;
}


/* Allocate the color COLOR->pixel on frame F, colormap CMAP.  If an
   exact match can't be allocated, try the nearest color available.
   Value is non-zero if successful.  Set *COLOR to the color
   allocated.  */

int
x_alloc_nearest_color (f, cmap, color)
     struct frame *f;
     Colormap cmap;
     XColor *color;
{
  gamma_correct (f, color);
  return x_alloc_nearest_color_1 (FRAME_X_DISPLAY (f), cmap, color);
}


/* Allocate color PIXEL on frame F.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  x_query_color (f, &color);
  XAllocColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}


/* Allocate color PIXEL on display DPY.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_dpy_color (dpy, cmap, pixel)
     Display *dpy;
     Colormap cmap;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  XQueryColor (dpy, cmap, &color);
  XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}

#endif /* #if 0 */


/* Brightness beyond which a color won't have its highlight brightness
   boosted.

   Nominally, highlight colors for `3d' faces are calculated by
   brightening an object's color by a constant scale factor, but this
   doesn't yield good results for dark colors, so for colors who's
   brightness is less than this value (on a scale of 0-65535) have an
   use an additional additive factor.

   The value here is set so that the default menu-bar/mode-line color
   (grey75) will not have its highlights changed at all.  */
#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 190


/* Obtain a color which is lighter or darker than *PIXEL by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as PIXEL, try a color where all RGB
   values have DELTA added.  Return the allocated color in *PIXEL.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static PIX_TYPE
mw32_get_lighter_color (struct frame *f, PIX_TYPE base,
			double factor, int delta)
{
  PIX_TYPE new;
  int red, green, blue;
  long bright;
  int success_p;

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  red = GetRValue (base);
  green = GetGValue (base);
  blue = GetBValue (base);

  xassert (factor >= 0);
  red = min (0xff, (int)(factor * red));
  green = min (0xff, (int)(factor * green));
  blue = min (0xff, (int)(factor * blue));

  /* Calculate brightness of COLOR.  */
  bright = (2 * red + 3 * green + blue) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double)bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = (int)(delta * dimness * factor / 2.0);

      if (factor < 1)
	{
	  red =   max (0, red -   min_delta);
	  green = max (0, green - min_delta);
	  blue =  max (0, blue -  min_delta);
	}
      else
	{
	  red =   min (0xff, min_delta + red);
	  green = min (0xff, min_delta + green);
	  blue =  min (0xff, min_delta + blue);
	}
    }
  new = RGB (red, green, blue);
  new = GetNearestColor (FRAME_HDC(f), new);
  if (new == CLR_INVALID)
    {
      red = min (0xff, delta + red);
      green = min (0xff, delta + green);
      blue = min (0xff, delta + blue);
      new = RGB (red, green, blue);
      new = GetNearestColor (FRAME_HDC (f), new);
    }

  return new;
}


/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.mw32->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */
   
static void
mw32_setup_relief_color (struct frame *f,
			 struct relief *relief,
			 double factor,
			 int delta,
			 PIX_TYPE default_pixel)
{
  struct mw32_output *di = f->output_data.mw32;
  PIX_TYPE background = di->relief_background;
  PIX_TYPE pixel;

  /* Free previously allocated color.  The color cell will be reused
     when it has been freed as many times as it was allocated, so this
     doesn't affect faces using the same colors.  */
  if (relief->allocated_p)
    {
      /* TODO: Do nothing because we have not
	 supported color pallete yet. */
      relief->allocated_p = 0;
    }

  /* Get ligher color.  */
  pixel = background;
  if (pixel = mw32_get_lighter_color (f, pixel, factor, delta))
    relief->foreground = pixel;
  else
    relief->foreground = default_pixel;

  relief->allocated_p = 1;
}


/* Set up colors for the relief lines around glyph string S.  */

static void
mw32_setup_relief_colors (struct glyph_string *s)
{
  struct mw32_output *di = s->f->output_data.mw32;
  PIX_TYPE color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else
    color = GetBkColor (s->hdc);

  if (!di->white_relief.allocated_p
      || color != di->relief_background)
    {
      di->relief_background = color;
      mw32_setup_relief_color (s->f, &di->white_relief, 1.2, 0x80,
			       WHITE_PIX_DEFAULT (s->f));
      mw32_setup_relief_color (s->f, &di->black_relief, 0.6, 0x40,
			       BLACK_PIX_DEFAULT (s->f));
    }
}


/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the relief
   to draw, it must be >= 0.  RAISED_P non-zero means draw a raised
   relief.  LEFT_P non-zero means draw a relief on the left side of
   the rectangle.  RIGHT_P non-zero means draw a relief on the right
   side of the rectangle.  CLIP_RECT is the clipping rectangle to use
   when drawing.  */

/* MW32 implementation.

(left_x, top_y)               width
  +-----------------------------^-----------------------------+
  | +---------------------------v----------------------------/|
  | |   Call Polygon() for upper-left edge and              | |
  < >  width                                          width < >
  | |                      lower-right edge.                | |
  |/ -------------------------------------------------------+ |
  +-----------------------------------------------------------+
                                                   (right_x, bottom_y)
*/

static void
mw32_draw_relief_rect (struct frame *f,
		       int left_x, int top_y, int right_x, int bottom_y,
		       int width, int raised_p, int left_p, int right_p,
		       HRGN clip_rgn)
{
  int i;
  HDC hdc = FRAME_HDC (f);
  struct mw32_output *di = f->output_data.mw32;
  LOGBRUSH logpenbrush;
  HPEN hp1, hp2, hptmp;
  HGDIOBJ hold1, hold2;

  logpenbrush.lbStyle = BS_SOLID;
  logpenbrush.lbHatch = 0;

  SaveDC (hdc);
  if (clip_rgn != INVALID_HANDLE_VALUE)
    SelectClipRgn (hdc, clip_rgn);

  logpenbrush.lbColor = di->white_relief.foreground;
  hp1 = ExtCreatePen (PS_COSMETIC | PS_SOLID, 1, &logpenbrush,
		      0, NULL);
  logpenbrush.lbColor = di->black_relief.foreground;
  hp2 = ExtCreatePen( PS_COSMETIC | PS_SOLID, 1, &logpenbrush,
		      0, NULL);

  /* Note that W32 GDI excludes bottom-right line.  */
  bottom_y--;
  right_x--;
  if (width == 1)
    {
      if (!raised_p)
	{
	  hptmp = hp1, hp1 = hp2, hp2 = hptmp;
	}

      hold1 = SelectObject (hdc, hp1);
      if (left_p)
	{
	  MoveToEx (hdc, left_x, bottom_y, NULL);
	  LineTo (hdc, left_x, top_y);
	}
      else
	MoveToEx (hdc, left_x, top_y, NULL);
      /* To draw the point (right_x, top_y), add 1 to the end.  */
      LineTo (hdc, right_x + 1, top_y);

      SelectObject (hdc, hp2);
      MoveToEx (hdc, right_x, top_y + 1, NULL);
      if (right_p)
	LineTo (hdc, right_x, bottom_y);
      else
	MoveToEx (hdc, right_x, bottom_y, NULL);
      /* To draw the point (left_x, bottom_y), sub 1 to the end.  */
      LineTo (hdc, left_x - 1, bottom_y);
      SelectObject (hdc, hold1);
    }
  else if (width > 1)
    {
      HBRUSH hb1, hb2;
      POINT points[4];

      if (raised_p)
	{
	  hb1 = CreateSolidBrush (di->white_relief.foreground);
	  hb2 = CreateSolidBrush (di->black_relief.foreground);
	}
      else
	{
	  hptmp = hp1, hp1 = hp2, hp2 = hptmp;
	  hb2 = CreateSolidBrush (di->white_relief.foreground);
	  hb1 = CreateSolidBrush (di->black_relief.foreground);
	}

      /* Top.  */
      hold1 = SelectObject (hdc, hp1);
      hold2 = SelectObject (hdc, hb1);

      points[0].x = left_x, points[0].y = top_y;
      points[1].x = right_x, points[1].y = top_y;
      points[2].x = right_x - (width - 1) * right_p,
	points[2].y = top_y + width - 1;
      points[3].x = left_x + (width - 1) * left_p,
	points[3].y = top_y + width - 1;
      Polygon (hdc, points, 4);

      /* Left.  */
      if (left_p)
	{
	  points[0].x = left_x, points[0].y = top_y;
	  points[1].x = left_x, points[1].y = bottom_y - 1;
	  points[2].x = left_x + width - 1, points[2].y = bottom_y - width;
	  points[3].x = left_x + width - 1, points[3].y = top_y + width - 1;
	  Polygon (hdc, points, 4);
	}

      /* Bottom.  */
      SelectObject (hdc, hp2);
      SelectObject (hdc, hb2);

      points[0].x = left_x, points[0].y = bottom_y;
      points[1].x = right_x, points[1].y = bottom_y;
      points[2].x = right_x - (width - 1) * right_p,
	points[2].y = bottom_y - width + 1;
      points[3].x = left_x + (width - 1) * left_p,
	points[3].y = bottom_y - width + 1;
      Polygon (hdc, points, 4);

      /* Right.  */
      if (right_p)
	{
	  points[0].x = right_x, points[0].y = top_y + 1;
	  points[1].x = right_x, points[1].y = bottom_y - 1;
	  points[2].x = right_x - width + 1, points[2].y = bottom_y - width;
	  points[3].x = right_x - width + 1; points[3].y = top_y + width;
	  Polygon (hdc, points, 4);
	}

      SelectObject (hdc, hold2);
      SelectObject (hdc, hold1);
      DeleteObject (hb1);
      DeleteObject (hb2);
    }
  RestoreDC (hdc, -1);
  DeleteObject (hp1);
  DeleteObject (hp2);
}

/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P non-zero means draw a line on the
   left side of the rectangle.  RIGHT_P non-zero means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
mw32_draw_box_rect (struct glyph_string *s, int left_x, int top_y,
		    int right_x, int bottom_y, int width,
		    int left_p, int right_p, HRGN clip_rgn)
{
  HDC hdc = s->hdc;
  RECT r;
  HRGN rr_rgn;
  HBRUSH hb;
  HGDIOBJ hold;

  SaveDC (hdc);
  hb = CreateSolidBrush (s->face->box_color);
  hold = SelectObject (hdc, hb);
  if (clip_rgn != INVALID_HANDLE_VALUE)
    SelectClipRgn (hdc, clip_rgn);

  if (left_p && right_p)
    rr_rgn = CreateRectRgn (left_x + width, top_y + width,
			    right_x - width, bottom_y - width);
  else if (left_p)
    rr_rgn = CreateRectRgn (left_x + width, top_y + width,
			    right_x, bottom_y - width);
  else if (right_p)
    rr_rgn = CreateRectRgn (left_x, top_y + width,
			    right_x - width, bottom_y - width);
  else
    rr_rgn = CreateRectRgn (left_x, top_y + width,
			    right_x, bottom_y - width);
  ExtSelectClipRgn (hdc, rr_rgn, RGN_DIFF);

  r.left = left_x;
  r.top = top_y;
  r.right = right_x;
  r.bottom = bottom_y;
  FillRect (hdc, &r, hb);

  SelectObject (hdc, hold);
  RestoreDC (hdc, -1);
  DeleteObject (hb);
  DeleteObject (rr_rgn);
}


/* Draw a box around glyph string S.  */

static void
mw32_draw_glyph_string_box (struct glyph_string *s)
{
  int width, left_x, right_x, top_y, bottom_y, last_x, raised_p;
  int left_p, right_p;
  struct glyph *last_glyph;
  RECT clip_rect;
  HRGN hcrr;

  last_x = window_box_right (s->w, s->area);
  if (s->row->full_width_p
      && !s->w->pseudo_window_p)
    {
      last_x += FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH (s->f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (s->f))
	last_x += FRAME_SCROLL_BAR_WIDTH (s->f) * CANON_X_UNIT (s->f);
    }
  
  /* The glyph that may have a right box line.  */
  last_glyph = (s->cmp || s->img
		? s->first_glyph
		: s->first_glyph + s->nchars - 1);

  width = abs (s->face->box_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = (s->row->full_width_p && s->extends_to_end_of_line_p
	     ? last_x
	     : min (last_x, s->x + s->background_width));
  top_y = s->y;
  bottom_y = top_y + s->height;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));
  
  mw32_get_glyph_string_clip_rect (s, &clip_rect);
  hcrr = CreateRectRgnIndirect (&clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    mw32_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
			left_p, right_p, hcrr);
  else
    {
      mw32_setup_relief_colors (s);
      mw32_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
			     width, raised_p, left_p, right_p, hcrr);
    }
  DeleteObject (hcrr);
}


static int
mw32_setup_image_bitmap_handle (HDC hdc, struct image *img,
				HBITMAP *phbmp, HBITMAP *phbmpmask)
{
  unsigned char *pdata;
  mw32_image* pmimg = &(img->mw32_img);

  *phbmpmask = NULL;
  *phbmp = CreateDIBSection (hdc, pmimg->pbmpinfo, DIB_RGB_COLORS,
			     (void **) &pdata, NULL, 0);
  if (!(*phbmp)) return 0;
  memcpy (pdata, pmimg->pbmpdata, pmimg->size);

  /* prepare bitmap mask. */
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
      maskinfo.c[1].rgbRed = maskinfo.c[1].rgbGreen = maskinfo.c[1].rgbBlue = 255;

      *phbmpmask = CreateDIBSection (hdc, (LPBITMAPINFO)&maskinfo,
				     DIB_RGB_COLORS, (void **) &pdata,
				     NULL, 0);
      if (!(*phbmpmask))
	{
	  DeleteObject (*phbmp);
	  *phbmp = 0;
	  return 0;
	}
      memcpy (pdata, pmimg->pbmpmask, pmimg->mask_size);
    }

  return 1;
}

/* Draw foreground of image glyph string S.  */

static void
mw32_draw_image_foreground (struct glyph_string *s)
{
  HBITMAP hbmp, hbmpmask;
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;

  if (MW32_IMAGE_VALID_P (s->img->mw32_img)
      && mw32_setup_image_bitmap_handle (s->hdc, s->img, &hbmp, &hbmpmask))
    {
      RECT clip_rect, image_rect, r;
      HDC hdcimg = CreateCompatibleDC (s->hdc);
      if (!hdcimg) return;

      mw32_get_glyph_string_clip_rect (s, &clip_rect);
      image_rect.left = x;
      image_rect.top = y;
      image_rect.right = x + s->img->width - 1;
      image_rect.bottom = y + s->img->height - 1;

      if (mw32_intersect_rectangles (&clip_rect, &image_rect, &r))
	{
	  HGDIOBJ holdbmp = SelectObject (hdcimg, hbmp);
	  /* we ignore s->img->mask.  Use hbmpmask instead.  */
	  if (hbmpmask)
	    {
	      HDC hdc_fg = CreateCompatibleDC (s->hdc);
	      HDC hdc_bg = CreateCompatibleDC (s->hdc);
	      if (hdc_fg && hdc_bg)
		{
		  int w = s->img->width;
		  int h = s->img->height;
		  HBITMAP hfg = CreateCompatibleBitmap (s->hdc, w, h);
		  HBITMAP hbg = CreateCompatibleBitmap (s->hdc, w, h);
		  if (hfg && hbg)
		    {
		      HGDIOBJ hfg_old = SelectObject (hdc_fg, hfg);
		      HGDIOBJ hbg_old = SelectObject (hdc_bg, hbg);
	      
		      /* Build up background */
		      BitBlt (hdc_bg, 0, 0, w, h, s->hdc, x, y, SRCCOPY);
		      holdbmp = SelectObject (hdcimg, hbmpmask);
		      BitBlt (hdc_bg, 0, 0, w, h, hdcimg, 0, 0, SRCAND);
		      /* Build up foreground */
		      BitBlt (hdc_fg, 0, 0, w, h, hdcimg, 0, 0, NOTSRCCOPY);
		      SelectObject (hdcimg, hbmp);
		      BitBlt (hdc_fg, 0, 0, w, h, hdcimg, 0, 0, SRCAND);
		      /* Merge background and foreground */
		      BitBlt (hdc_fg, 0, 0, w, h, hdc_bg, 0, 0, SRCPAINT);
		      /* Copy the result to HDC */
		      BitBlt (s->hdc, x, y, w, h, hdc_fg, 0, 0, SRCCOPY);
		      SelectObject (hdc_fg, hfg_old);
		      SelectObject (hdc_bg, hbg_old);
		    }
		  if (hfg) DeleteObject (hfg);
		  if (hbg) DeleteObject (hbg);
		}
	      if (hdc_fg) DeleteDC (hdc_fg);
	      if (hdc_bg) DeleteDC (hdc_bg);
	    }
	  else
	    {
	      BitBlt (s->hdc, x, y, s->img->width, s->img->height,
		      hdcimg, 0, 0, SRCCOPY);
	    }
	  SelectObject (hdcimg, holdbmp);
	}
      DeleteDC (hdcimg);
      DeleteObject (hbmp);
      DeleteObject (hbmpmask);

      /* When the image has a mask, we can expect that at
	 least part of a mouse highlight or a block cursor will
	 be visible.  If the image doesn't have a mask, make
	 a block cursor visible by drawing a rectangle around
	 the image.  I believe it's looking better if we do
	 nothing here for mouse-face.  */
      if (s->hl == DRAW_CURSOR)
	{
	  HANDLE hold;
	  HBRUSH hb = GetStockObject (NULL_BRUSH);
	  hold = SelectObject (s->hdc, hb);
	  Rectangle (s->hdc, x, y,
		     x + s->img->width, y + s->img->height);
	  SelectObject (s->hdc, hold);
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    Rectangle (s->hdc, x, y,
	       x + s->img->width, y + s->img->height);
}


/* Draw a relief around the image glyph string S.  */

static void
mw32_draw_image_relief (struct glyph_string *s)
{
  int x0, y0, x1, y1, thick, raised_p;
  RECT r;
  HRGN hr;
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);
  
  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;
  
  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;
  
  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      thick = tool_bar_button_relief > 0 ? tool_bar_button_relief : 3;
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = abs (s->img->relief);
      raised_p = s->img->relief > 0;
    }
  
  x0 = x - thick;
  y0 = y - thick;
  x1 = x + s->img->width + thick;
  y1 = y + s->img->height + thick;
  
  mw32_setup_relief_colors (s);
  mw32_get_glyph_string_clip_rect (s, &r);
  hr = CreateRectRgnIndirect (&r);
  mw32_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p, 1, 1, hr);
  DeleteObject (hr);
}

/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
mw32_draw_glyph_string_bg_rect (struct glyph_string *s,
				int x, int y, int w, int h)
{
  if (s->stippled_p)
    {
      /* TODO: stipple support */
      mw32_clear_glyph_string_rect (s, x, y, w, h);
    }
  else
    mw32_clear_glyph_string_rect (s, x, y, w, h);
}


/* Draw image glyph string S.  

            s->y
   s->x      +-------------------------
	     |   s->face->box
	     |
	     |     +-------------------------
	     |     |  s->img->margin
	     |     |
	     |     |       +-------------------
	     |     |       |  the image

 */

static void
mw32_draw_image_glyph_string (struct glyph_string *s)
{
  int x, y;
  int box_line_hwidth = abs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;

  height = s->height - 2 * box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->img->height
      || s->img->hmargin
      || s->img->vmargin
      || MW32_IMAGE_HAS_MASK_P (s->img->mw32_img)
      || !MW32_IMAGE_VALID_P (s->img->mw32_img)
      || s->width != s->background_width)
    {
      if (box_line_hwidth && s->first_glyph->left_box_line_p)
	x = s->x + box_line_hwidth;
      else
	x = s->x;
      
      y = s->y + box_line_vwidth;
      
      mw32_draw_glyph_string_bg_rect (s, x, y, s->background_width, height);
      s->background_filled_p = 1;
    }

  mw32_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    mw32_draw_image_relief (s);
}


/* Draw stretch glyph string S.  */

static void
mw32_draw_stretch_glyph_string (struct glyph_string *s)
{
  xassert (s->first_glyph->type == STRETCH_GLYPH);
  s->stippled_p = s->face->stipple != 0;

  if (s->hl == DRAW_CURSOR
      && !mw32_stretch_cursor_p)
    {
      /* If `x-stretch-block-cursor' is nil, don't draw a block cursor
	 as wide as the stretch glyph.  */
      int width = min (CANON_X_UNIT (s->f), s->background_width);

      /* Draw cursor.  */
      mw32_draw_glyph_string_bg_rect (s, s->x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < s->background_width)
	{
	  int x = s->x + width, y = s->y;
	  int w = s->background_width - width, h = s->height;
	  RECT r;
	  HRGN hr;

	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    mw32_setup_mouse_face_hdc (s);
  
	  mw32_get_glyph_string_clip_rect (s, &r);
	  hr = CreateRectRgnIndirect (&r);
	  SaveDC (s->hdc);
	  SelectClipRgn (s->hdc, hr);
	  DeleteObject (hr);
	  
	  if (s->face->stipple)
	    {
	      /* TODO: stipple support */
	      /* Fill background with a stipple pattern.  */
	    }
	  else
	    {
	      mw32_fill_area_pix (s->f, GetTextColor (s->hdc),
				  x, y, x + w, y + h);
	    }
	  RestoreDC (s->hdc, -1);
	}
    }
  else if (!s->background_filled_p)
    mw32_draw_glyph_string_bg_rect (s, s->x, s->y, s->background_width,
				    s->height);
  
  s->background_filled_p = 1;
}


/* Draw glyph string S.  */

static void
mw32_draw_glyph_string (struct glyph_string *s)
{
  int relief_drawn_p = 0;

  /* If S draws into the background of its successor, draw the
     background of the successor first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps_p)
    {
      xassert (s->next->img == NULL);
      mw32_set_glyph_string_clipping (s->next);
      mw32_draw_glyph_string_background (s->next, 1);
    }

  /* Set up S->gc, set clipping and draw S.  */
  mw32_setup_glyph_string_hdc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps_p
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      mw32_set_glyph_string_clipping (s);
      mw32_draw_glyph_string_background (s, 1);
      mw32_draw_glyph_string_box (s);
      relief_drawn_p = 1;
    }
  else
    mw32_set_glyph_string_clipping (s);


  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      mw32_draw_image_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      mw32_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps_p)
	s->background_filled_p = 1;
      else
	mw32_draw_glyph_string_background (s, 0);
      mw32_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps_p || s->gidx > 0)
	s->background_filled_p = 1;
      else
	mw32_draw_glyph_string_background (s, 1);
      mw32_draw_composite_glyph_string_foreground (s);
      break;

    default:
      abort ();
    }

  if (!s->for_overlaps_p)
    {
      /* Draw underline.  */
      if (s->face->underline_p)
	{
	  unsigned long tem, h;
	  int y;

	  /* Always underline thickness is 1 on mw32 implementation. */
	  h = 1;

	  /* Get the underline position.  This is the recommended
	     vertical offset in pixels from the baseline to the top of
	     the underline.  This is a signed value according to the
	     specs, and its default is

	     ROUND ((maximum descent) / 2), with
	     ROUND(x) = floor (x + 0.5)  */

	  /* To make underline straight, the vertical offset has been
	     changed, that should be derived according to the
	     proportion of glyph_row descent. And its default is
	     ROUND ((descent of row) / 2), with
	     descent of row = height of row - ascent of row. */
	  
	  if (s->row)
	    y = s->ybase + (s->row->height - s->row->ascent + 1) / 2;
	  else
	    y = s->y + s->height - h;
      
	  if (s->face->underline_defaulted_p)
	    mw32_fill_area_pix (s->f, s->face->foreground,
				s->x, y, s->x + s->width, y + h);
	  else
	    mw32_fill_area_pix (s->f, s->face->underline_color,
				s->x, y, s->x + s->width, y + h);
	}

      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    mw32_fill_area_pix (s->f, s->face->foreground,
				s->x, s->y + dy, s->x + s->width,
				s->y + dy + h);
	  else
	    mw32_fill_area_pix (s->f, s->face->overline_color,
				s->x, s->y + dy, s->x + s->width,
				s->y + dy + h);
	}
  
      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  unsigned long h = 1;
	  unsigned long dy = (s->height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    mw32_fill_area_pix (s->f, s->face->foreground,
				s->x, s->y + dy, s->x + s->width,
				s->y + dy + h);
	  else
	    mw32_fill_area_pix (s->f, s->face->strike_through_color,
				s->x, s->y + dy, s->x + s->width,
				s->y + dy + h);
	}
  
      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
	mw32_draw_glyph_string_box (s);
    }
  
  /* As for mw32 implementation, we don't have to
     reset clipping because all drawing functions restore
     DC state before exiting whenever setting clip region.  */
}


static int mw32_fill_composite_glyph_string P_ ((struct glyph_string *,
						 struct face **, int));


/* Fill glyph string S with composition components specified by S->cmp.
   
   FACES is an array of faces for all components of this composition.
   S->gidx is the index of the first component for S.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of a component not in S.  */

static int
mw32_fill_composite_glyph_string (struct glyph_string *s,
				  struct face **faces,
				  int overlaps_p)
{
  int i;

  xassert (s);

  s->for_overlaps_p = overlaps_p;
  
  s->face = faces[s->gidx];
  s->font = s->face->font;
  if (s->font)
    s->font_info = MW32_FONT_INFO_FROM_FONT (s->font);

  /* For all glyphs of this composition, starting at the offset
     S->gidx, until we reach the end of the definition or encounter a
     glyph that requires the different face, add it to S.  */
  ++s->nchars;
  for (i = s->gidx + 1; i < s->cmp->glyph_len && faces[i] == s->face; ++i)
    ++s->nchars;

  /* All glyph strings for the same composition has the same width,
     i.e. the width set for the first component of the composition.  */

  s->width = s->first_glyph->pixel_width;

  /* If the specified font could not be loaded, use the frame's
     default font, but record the fact that we couldn't load it in
     the glyph string so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
  
  xassert (s->face);

  /* This glyph string must always be drawn with 16-bit functions.  */
  s->two_byte_p = 1;

  return s->gidx + s->nchars;
}

/* Fill glyph string S from a sequence of character glyphs.
   
   FACE_ID is the face id of the string.  START is the index of the
   first glyph to consider, END is the index of the last + 1.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of the first glyph not in S.  */

/* Note for MW32 implementation.
   This function also sets metrics of each character, which will
   be used by textout method(ExtTextOut()) and glyph overhang
   operations.  */

static int
mw32_fill_glyph_string (struct glyph_string *s,
			int face_id,
			int start, int end,
			int overlaps_p)
{
  int i;
  unsigned char *pstr;
  struct glyph *glyph, *last;
  int voffset;
  int glyph_not_available_p;
  
  xassert (s->f == XFRAME (s->w->frame));
  xassert (s->nchars == 0);
  xassert (start >= 0 && end > start);

  s->for_overlaps_p = overlaps_p,
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  voffset = glyph->voffset;
  
  glyph_not_available_p = glyph->glyph_not_available_p;

  pstr = s->pstr;
  while (glyph < last
	 && glyph->type == CHAR_GLYPH
	 && glyph->voffset == voffset
	 /* Same face id implies same font, nowadays.  */
	 && glyph->face_id == face_id
	 && glyph->glyph_not_available_p == glyph_not_available_p)
    {
      int two_byte_p;
      FontCp fontcp;

      s->face = mw32_get_glyph_face_and_encoding (s->f, glyph,
						  &fontcp,
						  &two_byte_p);
      s->pfcp[s->nchars] = fontcp;
      i = pstr - s->pstr;
      s->pdx[i] = glyph->pixel_width;

      /* Strip off extra width of a box line */
      if (s->face->box != FACE_NO_BOX
	  && s->first_glyph->left_box_line_p
	  && i == 0)
	s->pdx[i] -= abs (s->face->box_line_width);

      SERIALIZE_FONTCP (pstr, fontcp);
      s->two_byte_p = two_byte_p;
      ++s->nchars;
      xassert (s->nchars <= end - start);
      s->width += glyph->pixel_width;
      ++glyph;
    }
  s->nbytes = pstr - s->pstr;

  s->font = s->face->font;
  if (s->font)
    s->font_info = MW32_FONT_INFO_FROM_FONT (s->font);
  
  /* If the specified font could not be loaded, use the frame's font,
     but record the fact that we couldn't load it in
     S->font_not_found_p so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL || glyph_not_available_p)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }
  /* MW32: setup layout info. */
  MW32_INVOKE_LAYOUTPROC (s->font, s->hdc, s->pstr, s->nbytes, s->width,
			  s->pdx, &s->left_overhang, &s->right_overhang);

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  xassert (s->face);
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from image glyph S->first_glyph.  */

static void
mw32_fill_image_glyph_string (struct glyph_string *s)
{
  xassert (s->first_glyph->type == IMAGE_GLYPH);
  s->img = IMAGE_FROM_ID (s->f, s->first_glyph->u.img_id);
  xassert (s->img);
  s->face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
  s->font = s->face->font;
  s->width = s->first_glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
}


/* Fill glyph string S from a sequence of stretch glyphs.

   ROW is the glyph row in which the glyphs are found, AREA is the
   area within the row.  START is the index of the first glyph to
   consider, END is the index of the last + 1.

   Value is the index of the first glyph not in S.  */

static int
mw32_fill_stretch_glyph_string (struct glyph_string *s,
				struct glyph_row *row,
				enum glyph_row_area area,
				int start, int end)
{
  struct glyph *glyph, *last;
  int voffset, face_id;
  
  xassert (s->first_glyph->type == STRETCH_GLYPH);
  
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  face_id = glyph->face_id;
  s->face = FACE_FROM_ID (s->f, face_id);
  s->font = s->face->font;
  if (s->font)
    s->font_info = MW32_FONT_INFO_FROM_FONT (s->font);
  s->width = glyph->pixel_width;
  voffset = glyph->voffset;

  for (++glyph;
       (glyph < last
	&& glyph->type == STRETCH_GLYPH
	&& glyph->voffset == voffset
	&& glyph->face_id == face_id);
       ++glyph)
    s->width += glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  xassert (s->face);
  return glyph - s->row->glyphs[s->area];
}


/* Initialize glyph string S.  CHAR2B is a suitably allocated vector
   of FontCp for S; it can't be allocated in
   x_init_glyph_string because it must be allocated via `alloca'.  W
   is the window on which S is drawn.  ROW and AREA are the glyph row
   and area within the row from which S is constructed.  START is the
   index of the first glyph structure covered by S.  HL is a
   face-override for drawing S.  */
   
static void
mw32_init_glyph_string (struct glyph_string *s,
			FontCp *pfcp,
			unsigned char *pstr,
			int *pdx,
			struct window *w,
			struct glyph_row *row,
			enum glyph_row_area area,
			int start,
			enum draw_glyphs_face hl)
{
  bzero (s, sizeof *s);
  s->w = w;
  s->f = XFRAME (w->frame);
  s->window = FRAME_MW32_WINDOW (s->f);
  s->hdc = FRAME_HDC (s->f);
  s->pfcp = pfcp;
  s->pstr = pstr;
  s->pdx = pdx;
  s->hl = hl;
  s->row = row;
  s->area = area;
  s->first_glyph = row->glyphs[area] + start;
  s->height = row->height;
  s->y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  /* Display the internal border below the tool-bar window.  */
  if (s->w == XWINDOW (s->f->tool_bar_window))
    s->y -= s->f->output_data.mw32->internal_border_width;
  
  s->ybase = s->y + row->ascent;
}


/* Set background width of glyph string S.  START is the index of the
   first glyph following S.  LAST_X is the right-most x-position + 1
   in the drawing area.  */

static INLINE void
mw32_set_glyph_string_background_width (struct glyph_string *s,
					int start,
					int last_x)
{
  /* If the face of this glyph string has to be drawn to the end of
     the drawing area, set S->extends_to_end_of_line_p.  */
  struct face *default_face = FACE_FROM_ID (s->f, DEFAULT_FACE_ID);
  
  if (start == s->row->used[s->area]
      && s->area == TEXT_AREA
      && ((s->hl == DRAW_NORMAL_TEXT
	   && (s->row->fill_line_p
	       || s->face->background != default_face->background
	       || s->face->stipple != default_face->stipple
	       || s->row->mouse_face_p))
	  || s->hl == DRAW_MOUSE_FACE))
      s->extends_to_end_of_line_p = 1;
  
  /* If S extends its face to the end of the line, set its
     background_width to the distance to the right edge of the drawing
     area.  */
  if (s->extends_to_end_of_line_p)
    s->background_width = last_x - s->x + 1;
  else
    s->background_width = s->width;
}


/* Add a glyph string for a stretch glyph to the list of strings
   between HEAD and TAIL.  START is the index of the stretch glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

/* SunOS 4 bundled cc, barfed on continuations in the arg lists here
   and below -- keep them on one line.  */
#define BUILD_STRETCH_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									    \
       {								    \
	 s = (struct glyph_string *) alloca (sizeof *s);		    \
	 mw32_init_glyph_string (s, NULL, NULL, NULL, W, ROW, AREA, START, HL); \
	 START = mw32_fill_stretch_glyph_string (s, ROW, AREA, START, END); \
	 mw32_append_glyph_string (&HEAD, &TAIL, s);			    \
         s->x = (X);							    \
       }								    \
     while (0)


/* Add a glyph string for an image glyph to the list of strings
   between HEAD and TAIL.  START is the index of the image glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

#define BUILD_IMAGE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									\
       {								\
	 s = (struct glyph_string *) alloca (sizeof *s);		\
	 mw32_init_glyph_string (s, NULL, NULL, NULL, W, ROW, AREA, START, HL); \
	 mw32_fill_image_glyph_string (s);				\
	 mw32_append_glyph_string (&HEAD, &TAIL, s);			\
	 ++START;							\
         s->x = (X);							\
       }								\
     while (0)


/* Add a glyph string for a sequence of character glyphs to the list
   of strings between HEAD and TAIL.  START is the index of the first
   glyph in row area AREA of glyph row ROW that is part of the new
   glyph string.  END is the index of the last glyph in that glyph row
   area.  X is the current output position assigned to the new glyph
   string constructed.  HL overrides that face of the glyph; e.g. it
   is DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the
   right-most x-position of the drawing area.  */

#define BUILD_CHAR_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 int c, face_id;						   \
	 FontCp *pfcp;							   \
	 unsigned char *pstr;						   \
	 int *pdx;							   \
									   \
	 c = (ROW)->glyphs[AREA][START].u.ch;				   \
	 face_id = (ROW)->glyphs[AREA][START].face_id;			   \
									   \
	 s = (struct glyph_string *) alloca (sizeof *s);		   \
	 pfcp = (FontCp *) alloca ((END - START) * (sizeof *pfcp));	   \
	 pstr = (unsigned char*) alloca ((END - START) * MAXBYTES1FCP);    \
	 pdx = (int*) alloca ((END - START) * sizeof (int) * MAXBYTES1FCP);\
	 memset (pdx, 0, ((END - START) * sizeof (int) * MAXBYTES1FCP));   \
	 mw32_init_glyph_string (s, pfcp, pstr, pdx, W, ROW, AREA, START, HL); \
	 mw32_append_glyph_string (&HEAD, &TAIL, s);			   \
	 s->x = (X);							   \
	 START = mw32_fill_glyph_string (s, face_id, START, END,	   \
                                         OVERLAPS_P);			   \
       }								   \
     while (0)
     

/* Add a glyph string for a composite sequence to the list of strings
   between HEAD and TAIL.  START is the index of the first glyph in
   row area AREA of glyph row ROW that is part of the new glyph
   string.  END is the index of the last glyph in that glyph row area.
   X is the current output position assigned to the new glyph string
   constructed.  HL overrides that face of the glyph; e.g. it is
   DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the right-most
   x-position of the drawing area.  */

#define BUILD_COMPOSITE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)	  \
  do {									  \
    int cmp_id = (ROW)->glyphs[AREA][START].u.cmp_id;			  \
    int face_id = (ROW)->glyphs[AREA][START].face_id;			  \
    struct face *base_face = FACE_FROM_ID (XFRAME (w->frame), face_id);	  \
    struct composition *cmp = composition_table[cmp_id];		  \
    int glyph_len = cmp->glyph_len;					  \
    FontCp *pfcp;							  \
    struct face **faces;						  \
    struct glyph_string *first_s = NULL;				  \
    int n;								  \
    									  \
    base_face = base_face->ascii_face;					  \
    pfcp = (FontCp *) alloca ((sizeof *pfcp) * glyph_len);		  \
    faces = (struct face **) alloca ((sizeof *faces) * glyph_len);	  \
    /* At first, fill in `char2b' and `faces'.  */			  \
    for (n = 0; n < glyph_len; n++)					  \
      {									  \
	int c = COMPOSITION_GLYPH (cmp, n);				  \
	int this_face_id = FACE_FOR_CHAR (XFRAME (w->frame), base_face, c); \
	faces[n] = FACE_FROM_ID (XFRAME (w->frame), this_face_id);	  \
	mw32_get_char_face_and_encoding (XFRAME (w->frame), c,		  \
				         this_face_id, pfcp + n, 1);	  \
      }									  \
    									  \
    /* Make glyph_strings for each glyph sequence that is drawable by	  \
       the same face, and append them to HEAD/TAIL.  */			  \
    for (n = 0; n < cmp->glyph_len;)					  \
      {									  \
	s = (struct glyph_string *) alloca (sizeof *s);			  \
	mw32_init_glyph_string (s, pfcp + n, NULL, NULL, W, ROW, AREA, START, HL); \
	mw32_append_glyph_string (&(HEAD), &(TAIL), s);			  \
 	s->cmp = cmp;							  \
	s->gidx = n;							  \
	s->x = (X);							  \
									  \
	if (n == 0)							  \
	  first_s = s;							  \
									  \
	n = mw32_fill_composite_glyph_string (s, faces, OVERLAPS_P);	  \
      }									  \
    									  \
    ++START;								  \
    s = first_s;							  \
  } while (0)
    

/* Build a list of glyph strings between HEAD and TAIL for the glyphs
   of AREA of glyph row ROW on window W between indices START and END.
   HL overrides the face for drawing glyph strings, e.g. it is
   DRAW_CURSOR to draw a cursor.  X and LAST_X are start and end
   x-positions of the drawing area.

   This is an ugly monster macro construct because we must use alloca
   to allocate glyph strings (because x_draw_glyphs can be called
   asynchronously).  */

#define BUILD_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 HEAD = TAIL = NULL;						   \
	 while (START < END)						   \
	   {								   \
             struct glyph *first_glyph = (ROW)->glyphs[AREA] + START;	   \
             switch (first_glyph->type)					   \
	       {							   \
	       case CHAR_GLYPH:						   \
                 BUILD_CHAR_GLYPH_STRINGS (W, ROW, AREA, START, END, HEAD, \
		                           TAIL, HL, X, LAST_X,		   \
                                           OVERLAPS_P);	                   \
		 break;							   \
									   \
	       case COMPOSITE_GLYPH:					   \
                 BUILD_COMPOSITE_GLYPH_STRING (W, ROW, AREA, START, END,   \
						 HEAD, TAIL, HL, X, LAST_X,\
						 OVERLAPS_P);		   \
		 break;							   \
									   \
	       case STRETCH_GLYPH:					   \
		 BUILD_STRETCH_GLYPH_STRING (W, ROW, AREA, START, END,	   \
					     HEAD, TAIL, HL, X, LAST_X);   \
		 break;							   \
									   \
	       case IMAGE_GLYPH:					   \
		 BUILD_IMAGE_GLYPH_STRING (W, ROW, AREA, START, END, HEAD, \
					   TAIL, HL, X, LAST_X);	   \
		 break;							   \
									   \
	       default:							   \
		 abort ();						   \
	       }							   \
									   \
             mw32_set_glyph_string_background_width (s, START, LAST_X);	   \
	     (X) += s->width;						   \
            }								   \
       }								   \
     while (0)


/* Draw glyphs between START and END in AREA of ROW on window W,
   starting at x-position X.  X is relative to AREA in W.  HL is a
   face-override with the following meaning:

   DRAW_NORMAL_TEXT	draw normally
   DRAW_CURSOR		draw in cursor face
   DRAW_MOUSE_FACE	draw in mouse face.
   DRAW_INVERSE_VIDEO	draw in mode line face
   DRAW_IMAGE_SUNKEN	draw an image with a sunken relief around it
   DRAW_IMAGE_RAISED	draw an image with a raised relief around it

   If REAL_START is non-null, return in *REAL_START the real starting
   position for display.  This can be different from START in case
   overlapping glyphs must be displayed.  If REAL_END is non-null,
   return in *REAL_END the real end position for display.  This can be
   different from END in case overlapping glyphs must be displayed.

   If OVERLAPS_P is non-zero, draw only the foreground of characters
   and clip to the physical height of ROW.

   Value is the x-position reached, relative to AREA of W.  */
     
static int
mw32_draw_glyphs (struct window *w, int x, struct glyph_row *row,
		  enum glyph_row_area area, int start, int end,
		  enum draw_glyphs_face hl, int *real_start, int *real_end,
		  int overlaps_p)
{
  struct glyph_string *head, *tail;
  struct glyph_string *s;
  int last_x, area_width;
  int x_reached;
  int i, j;

  /* Let's rather be paranoid than getting a SEGV.  */
  end = min (end, row->used[area]);
  start = max (0, start);
  start = min (end, start);
  if (real_start)
    *real_start = start;
  if (real_end)
    *real_end = end;

  /* Translate X to frame coordinates.  Set last_x to the right
     end of the drawing area.  */
  if (row->full_width_p)
    {
      /* X is relative to the left edge of W, without scroll bars
	 or flag areas.  */
      struct frame *f = XFRAME (w->frame);
      /* int width = FRAME_FLAGS_AREA_WIDTH (f);  */
      int window_left_x = WINDOW_LEFT_MARGIN (w) * CANON_X_UNIT (f);

      x += window_left_x;
      area_width = XFASTINT (w->width) * CANON_X_UNIT (f);
      last_x = window_left_x + area_width;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f);
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
	    last_x += width;
	  else
	    x -= width;
	}

      x += FRAME_INTERNAL_BORDER_WIDTH (f);
      last_x += FRAME_INTERNAL_BORDER_WIDTH (f);
    }
  else
    {
      x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, x);
      area_width = window_box_width (w, area);
      last_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, area_width);
    }

  /* Build a doubly-linked list of glyph_string structures between
     head and tail from what we have to draw.  Note that the macro
     BUILD_GLYPH_STRINGS will modify its start parameter.  That's
     the reason we use a separate variable `i'.  */
  i = start;
  BUILD_GLYPH_STRINGS (w, row, area, i, end, head, tail, hl, x, last_x,
		       overlaps_p);
  if (tail)
    x_reached = tail->x + tail->background_width;
  else
    x_reached = x;

  /* If there are any glyphs with lbearing < 0 or rbearing > width in
     the row, redraw some glyphs in front or following the glyph
     strings built above.  */
  if (head && !overlaps_p && row->contains_overlapping_glyphs_p)
    {
      int dummy_x = 0;
      struct glyph_string *h, *t;

      /* On mw32 environment, we've done caliculated overhang metrics. */

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that are overwritten because of the first glyph
	 string's left overhang.  The background of all strings
	 prepended must be drawn because the first glyph string 
	 draws over it.  */
      i = mw32_left_overwritten (head);
      if (i >= 0)
	{
	  j = i;
	  BUILD_GLYPH_STRINGS (w, row, area, j, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
			       overlaps_p);
	  start = i;
	  if (real_start)
	    *real_start = start;
	  mw32_compute_glyph_string_x (t, head->x, 1);
	  mw32_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that overwrite that glyph string because of their
	 right overhang.  For these strings, only the foreground must
	 be drawn, because it draws over the glyph string at `head'.
	 The background must not be drawn because this would overwrite
	 right overhangs of preceding glyphs for which no glyph
	 strings exist.  */
      i = mw32_left_overwriting (head);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, i, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
			       overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  if (real_start)
	    *real_start = i;
	  mw32_compute_glyph_string_x (t, head->x, 1);
	  mw32_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyphs strings for glyphs following the last glyph
	 string tail that are overwritten by tail.  The background of
	 these strings has to be drawn because tail's foreground draws
	 over it.  */
      i = mw32_right_overwritten (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
			       overlaps_p);
	  mw32_compute_glyph_string_x (h, tail->x + tail->width, 0);
	  mw32_append_glyph_string_lists (&head, &tail, h, t);
	  if (real_end)
	    *real_end = i;
	}

      /* Append glyph strings for glyphs following the last glyph
	 string tail that overwrite tail.  The foreground of such
	 glyphs has to be drawn because it writes into the background
	 of tail.  The background must not be drawn because it could
	 paint over the foreground of following glyphs.  */
      i = mw32_right_overwriting (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
			       overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  mw32_compute_glyph_string_x (h, tail->x + tail->width, 0);
	  mw32_append_glyph_string_lists (&head, &tail, h, t);
	  if (real_end)
	    *real_end = i;
	}
    }

  if (head)
    {
      HDC hdc = head->hdc;

      SaveDC (hdc);
      /* Draw all strings.  */
      for (s = head; s; s = s->next)
	mw32_draw_glyph_string (s);
      RestoreDC (hdc, -1);
    }
  
  /* Value is the x-position up to which drawn, relative to AREA of W.
     This doesn't include parts drawn because of overhangs.  */
  x_reached = FRAME_TO_WINDOW_PIXEL_X (w, x_reached);
  if (!row->full_width_p)
    {
      if (area > LEFT_MARGIN_AREA)
	x_reached -= window_box_width (w, LEFT_MARGIN_AREA);
      if (area > TEXT_AREA)
	x_reached -= window_box_width (w, TEXT_AREA);
    }
  
  return x_reached;
}


/* Fix the display of area AREA of overlapping row ROW in window W.  */

static void
mw32_fix_overlapping_area (struct window *w,
			   struct glyph_row *row,
			   enum glyph_row_area area)
{
  int i, x;
  
  BLOCK_INPUT;
  
  if (area == LEFT_MARGIN_AREA)
    x = 0;
  else if (area == TEXT_AREA)
    x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
  else
    x = (window_box_width (w, LEFT_MARGIN_AREA)
	 + window_box_width (w, TEXT_AREA));

  for (i = 0; i < row->used[area];)
    {
      if (row->glyphs[area][i].overlaps_vertically_p)
	{
	  int start = i, start_x = x;

	  do
	    {
	      x += row->glyphs[area][i].pixel_width;
	      ++i;
	    }
	  while (i < row->used[area]
		 && row->glyphs[area][i].overlaps_vertically_p);

	  mw32_draw_glyphs (w, start_x, row, area, start, i,
			    (row->inverse_p
			     ? DRAW_INVERSE_VIDEO : DRAW_NORMAL_TEXT),
			    NULL, NULL, 1);
	}
      else
	{
	  x += row->glyphs[area][i].pixel_width;
	  ++i;
	}
    }
  
  UNBLOCK_INPUT;
}


/* Output LEN glyphs starting at START at the nominal cursor position.
   Advance the nominal cursor over the text.  The global variable
   updated_window contains the window being updated, updated_row is
   the glyph row being updated, and updated_area is the area of that
   row being updated.  */

static void
mw32i_write_glyphs (struct glyph *start, int len)
{
  int x, hpos, real_start, real_end;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;

  anticipate_overwrite_caret (updated_window);
  
  /* Write glyphs.  */

  hpos = start - updated_row->glyphs[updated_area];
  x = mw32_draw_glyphs (updated_window, output_cursor.x,
			updated_row, updated_area,
			hpos, hpos + len,
			(updated_row->inverse_p
			 ? DRAW_INVERSE_VIDEO : DRAW_NORMAL_TEXT),
			&real_start, &real_end, 0);

  /* If we drew over the cursor, note that it is not visible any more.  */
  notice_overwritten_cursor (updated_window, real_start,
			     real_end - real_start);
  restore_overwritten_caret (XFRAME (WINDOW_FRAME (updated_window)));

  /* Invalidate old phys cursor if the glyph at its hpos is redrawn.  */
  if (updated_area == TEXT_AREA
      && updated_window->phys_cursor_on_p
      && updated_window->phys_cursor.vpos == output_cursor.vpos
      && updated_window->phys_cursor.hpos >= hpos
      && updated_window->phys_cursor.hpos < hpos + len)
    updated_window->phys_cursor_on_p = 0;

  /* If we should require GdiFlush(),
     insert here.  */
  UNBLOCK_INPUT;
  
  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x = x;
}


/* Insert LEN glyphs from START at the nominal cursor position.   */

static void
mw32i_insert_glyphs (struct glyph *start, register int len)
{
  HDC hdc;
  struct frame *f;
  struct window *w;
  int line_height, shift_by_width, shifted_region_width;
  struct glyph_row *row;
  struct glyph *glyph;
  int frame_x, frame_y, hpos, real_start, real_end;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  w = updated_window;
  f = XFRAME (WINDOW_FRAME (w));

  anticipate_overwrite_caret (w);

  /* Get the height of the line we are in.  */
  row = updated_row;
  line_height = row->height;

  /* Get the width of the glyphs to insert.  */
  shift_by_width = 0;
  for (glyph = start; glyph < start + len; ++glyph)
    shift_by_width += glyph->pixel_width;

  /* Get the width of the region to shift right.  */
  shifted_region_width = (window_box_width (w, updated_area)
			  - output_cursor.x
			  - shift_by_width);

  /* Shift right.  */
  frame_x = window_box_left (w, updated_area) + output_cursor.x;
  frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, output_cursor.y);
  hdc = FRAME_HDC (f);
  BitBlt (hdc,
	  frame_x + shift_by_width, frame_y,
	  shifted_region_width, line_height,
	  hdc, frame_x, frame_y, SRCCOPY);

  /* Write the glyphs.  */
  hpos = start - row->glyphs[updated_area];
  mw32_draw_glyphs (w, output_cursor.x, row, updated_area, hpos, hpos + len,
		    DRAW_NORMAL_TEXT, &real_start, &real_end, 0);
  notice_overwritten_cursor (w, real_start, real_end - real_start);
  restore_overwritten_caret (f);
  
  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x += shift_by_width;
  /* If we should require GdiFlush(),
     insert here.  */
  UNBLOCK_INPUT;
}


/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
MW32_delete_glyphs (int n)
{
  abort ();
}


/* Erase the current text line from the nominal cursor position
   (inclusive) to pixel column TO_X (exclusive).  The idea is that
   everything from TO_X onward is already erased.

   TO_X is a pixel position relative to updated_area of
   updated_window.  TO_X == -1 means clear to the end of this area.  */

static void
mw32i_clear_end_of_line (int to_x)
{
  struct frame *f;
  struct window *w = updated_window;
  int max_x, min_y, max_y;
  int from_x, from_y, to_y;
  
  xassert (updated_window && updated_row);
  f = XFRAME (w->frame);
  
  if (updated_row->full_width_p)
    {
      max_x = XFASTINT (w->width) * CANON_X_UNIT (f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
	  && !w->pseudo_window_p)
	  max_x += FRAME_SCROLL_BAR_PIXEL_WIDTH (f);
    }
  else
    max_x = window_box_width (w, updated_area);
  max_y = window_text_bottom_y (w);

  /* TO_X == 0 means don't do anything.  TO_X < 0 means clear to end
     of window.  For TO_X > 0, truncate to end of drawing area.  */
  if (to_x == 0)
    return;
  else if (to_x < 0)
    to_x = max_x;
  else
    to_x = min (to_x, max_x);

  to_y = min (max_y, output_cursor.y + updated_row->height);
  
  /* Notice if the cursor will be cleared by this operation.  */
  if (!updated_row->full_width_p)
    {
      anticipate_overwrite_caret (w);
      notice_overwritten_cursor (w, output_cursor.hpos, -1);
    }

  from_x = output_cursor.x;
     
  /* Translate to frame coordinates.  */
  if (updated_row->full_width_p)
    {
      from_x = WINDOW_TO_FRAME_PIXEL_X (w, from_x);
      to_x = WINDOW_TO_FRAME_PIXEL_X (w, to_x);
    }
  else
    {
      from_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, from_x);
      to_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, to_x);
    }
  
  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, max (min_y, output_cursor.y));
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, to_y);
  
  /* Prevent inadvertently clearing to end of the X window.  */
  if (to_x > from_x && to_y > from_y)
    {
      BLOCK_INPUT;
      mw32_clear_area (f, from_x, from_y, to_x, to_y);
      /* If we should require GdiFlush(),
	 insert here.  */
      UNBLOCK_INPUT;
    }

  restore_overwritten_caret (f);
}


/* Clear entire frame.  If updating_frame is non-null, clear that
   frame.  Otherwise clear the selected frame.  */

static void
MW32_clear_frame ()
{
  struct frame *f;

  if (updating_frame)
    f = updating_frame;
  else
    f = SELECTED_FRAME ();

  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  /* We don't set the output cursor here because there will always
     follow an explicit cursor_to.  */
  BLOCK_INPUT;

  {
    RECT rect;
    GetClientRect (FRAME_MW32_WINDOW (f), &rect);
    mw32_clear_area (f, rect.left, rect.top, rect.right, rect.bottom);
  }

#if 0
  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  mw32_scroll_bar_clear (f);
#endif

  UNBLOCK_INPUT;
}




/* Make audible bell.  */

void
MW32_ring_bell (void)
{
  extern void w32_sys_ring_bell (void);

  if (visible_bell)
    {
      struct frame *f = SELECTED_FRAME ();
      POST_INFORM_MESSAGE (FRAME_MW32_WINDOW (f),
			   WM_EMACS_FLASH_WINDOW, 0, 0);
    }
  else
    w32_sys_ring_bell ();
}


/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to x_update_begin and x_update_end.  */

static void
MW32_set_terminal_window (int n)
{
  /* This function intentionally left blank.  */
}



/***********************************************************************
			      Line Dance
 ***********************************************************************/

/* Perform an insert-lines or delete-lines operation, inserting N
   lines or deleting -N lines at vertical position VPOS.  */

static void
MW32_ins_del_lines (int vpos, int n)
{
  abort ();
}


/* Scroll part of the display as described by RUN.  */

static void
mw32i_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;
  HWND hwnd = FRAME_MW32_WINDOW (f);
  HRGN expect_dirty;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the flags areas to the
     left and right of W.  */
  window_box (w, -1, &x, &y, &width, &height);
  width += FRAME_MW32_FLAGS_AREA_WIDTH (f);
  x -= FRAME_MW32_LEFT_FLAGS_AREA_WIDTH (f);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y + height, x + width, bottom_y);
    }
  else
    {
      /* Scolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y, x + width, to_y);
    }

  BLOCK_INPUT;
  
  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  updated_window = w;
  mw32_clear_cursor (w);

  {
    RECT from;
    RECT to;
    HRGN dirty = CreateRectRgn (0, 0, 0, 0);
    HRGN combined = CreateRectRgn (0, 0, 0, 0);

    from.left = to.left = x;
    from.right = to.right = x + width;
    from.top = from_y;
    from.bottom = from_y + height;
    to.top = y;
    to.bottom = bottom_y;

    ScrollWindowEx (hwnd, 0, to_y - from_y, &from, &to, dirty,
		    NULL, SW_INVALIDATE);

    /* Combine this with what we expect to be dirty. This covers the
       case where not all of the region we expect is actually dirty.  */
    CombineRgn (combined, dirty, expect_dirty, RGN_OR);

    /* If the dirty region is not what we expected, redraw the entire frame.  */
#if 0
    /* This causes unexpected "End of buffer".*/
    if (!EqualRgn (combined, expect_dirty))
      SET_FRAME_GARBAGED (f);
#endif
    DeleteObject (dirty);
    DeleteObject (combined);
  }
  DeleteObject (expect_dirty);
  
  UNBLOCK_INPUT;
}



/***********************************************************************
	    Expose operation (for WM_PAINT message)
 ***********************************************************************/

/* function declarations */
static int expose_window_tree P_ ((struct window *, RECT *));
static int expose_window P_ ((struct window *, RECT *));
static void expose_area P_ ((struct window *, struct glyph_row *,
			     RECT *, enum glyph_row_area));
static int expose_line P_ ((struct window *, struct glyph_row *,
			    RECT *));
									
/* Redisplay an exposed area of frame F, which will be obtained
   by BeginPaint(). */

void
mw32_expose_frame (struct frame *f)
{
  RECT r;
  HDC hdc;
  HWND hwnd;
  PAINTSTRUCT ps;
  int mouse_face_overwritten_p = 0;

  TRACE ((stderr, "expose_frame "));

  /* No need to redraw if frame will be redrawn soon.  */
  if (FRAME_GARBAGED_P (f))
    {
      TRACE ((stderr, " garbaged\n"));
      return;
    }

  /* If basic faces haven't been realized yet, there is no point in
     trying to redraw anything.  This can happen when we get an expose
     event while Emacs is starting, e.g. by moving another window.  */
  if (FRAME_FACE_CACHE (f) == NULL
      || FRAME_FACE_CACHE (f)->used < BASIC_FACE_ID_SENTINEL)
    {
      TRACE ((stderr, " no faces\n"));
      return;
    }

  /* If FRAME_DESIRED_CURSOR(f) of selected frame f is caret cursor,
     It may be needed to hide system caret before drawing window to
     get rid of garbage image of cursor of just deleted window. */
  
  if (f == XFRAME (selected_frame)
      && CARET_CURSOR_P (FRAME_DESIRED_CURSOR (f)))
    mw32_set_caret (f, HIDDEN_CARET);
  
  hwnd = FRAME_MW32_WINDOW (f);
  hdc = BeginPaint (hwnd, &ps);

  mw32_setup_default_hdc (hdc);

  W32_BLOCK_INPUT;
  f->output_data.mw32->message_thread_hdc = hdc;

  r = ps.rcPaint;
  TRACE ((stderr, "(%d, %d, %d, %d)\n", r.left, r.top, r.right, r.bottom));

  if (ps.fErase)
    mw32_clear_area (f, r.left, r.top, r.right, r.bottom);
  /* Main thread maybe has already destroyed root_window. */
  if (WINDOWP (f->root_window))
    mouse_face_overwritten_p = expose_window_tree (XWINDOW (f->root_window), &r);

  if (WINDOWP (f->tool_bar_window))
     mouse_face_overwritten_p
       |= expose_window (XWINDOW (f->tool_bar_window), &r);

  /* Also on MW32, we redo the highlight if expose_window*()
     directs to rehighlight mouse face.  Original comments
     in xterm.c are the following. */
  /* Some window managers support a focus-follows-mouse style with
     delayed raising of frames.  Imagine a partially obscured frame,
     and moving the mouse into partially obscured mouse-face on that
     frame.  The visible part of the mouse-face will be highlighted,
     then the WM raises the obscured frame.  With at least one WM, KDE
     2.1, Emacs is not getting any event for the raising of the frame
     (even tried with SubstructureRedirectMask), only Expose events.
     These expose events will draw text normally, i.e. not
     highlighted.  Which means we must redo the highlight here.
     Subsume it under ``we love X''.  --gerd 2001-08-15  */
  if (mouse_face_overwritten_p && !FRAME_GARBAGED_P (f))
    {
      struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
      if (f == dpyinfo->mouse_face_mouse_frame)
	{
	  int x = dpyinfo->mouse_face_mouse_x;
	  int y = dpyinfo->mouse_face_mouse_y;
	  clear_mouse_face (dpyinfo);
	  note_mouse_highlight (f, x, y);
	}
    }

  f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
  W32_UNBLOCK_INPUT;
  EndPaint (hwnd, &ps);
}


/* Redraw (parts) of all windows in the window tree rooted at W that
   intersect R.  R contains frame pixel coordinates.  */

static int
expose_window_tree (struct window *w, RECT *r)
{
  struct frame *f = XFRAME (w->frame);
  int mouse_face_overwritten_p = 0;

  while (w && !FRAME_GARBAGED_P (f))
    {
      if (!NILP (w->hchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->hchild), r);
      else if (!NILP (w->vchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->vchild), r);
      else
	mouse_face_overwritten_p |= expose_window (w, r);
      
      w = NILP (w->next) ? NULL : XWINDOW (w->next);
    }

  return mouse_face_overwritten_p;
}


/* Redraw the part of glyph row area AREA of glyph row ROW on window W
   which intersects rectangle R.  R is in window-relative coordinates.  */

static void
expose_area (struct window *w,
	     struct glyph_row *row,
	     RECT *r,
	     enum glyph_row_area area)
{
  struct glyph *first = row->glyphs[area];
  struct glyph *end = row->glyphs[area] + row->used[area];
  struct glyph *last;
  int first_x, start_x, x;

  if (area == TEXT_AREA && row->fill_line_p)
    /* If row extends face to end of line write the whole line.  */
    mw32_draw_glyphs (w, 0, row, area,
		      0, row->used[area],
		      row->inverse_p ? DRAW_INVERSE_VIDEO : DRAW_NORMAL_TEXT,
		      NULL, NULL, 0);
  else
    {
      /* Set START_X to the window-relative start position for drawing glyphs of
	 AREA.  The first glyph of the text area can be partially visible.
	 The first glyphs of other areas cannot.  */
      if (area == LEFT_MARGIN_AREA)
	start_x = 0;
      else if (area == TEXT_AREA)
	start_x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
      else
	start_x = (window_box_width (w, LEFT_MARGIN_AREA)
		   + window_box_width (w, TEXT_AREA));
      x = start_x;

      /* Find the first glyph that must be redrawn.  */
      while (first < end
	     && x + first->pixel_width < r->left)
	{
	  x += first->pixel_width;
	  ++first;
	}
      
      /* Find the last one.  */
      last = first;
      first_x = x;
      while (last < end && x < r->right)
	{
	  x += last->pixel_width;
	  ++last;
	}
      
      /* Repaint.  */
      if (last > first)
	mw32_draw_glyphs (w, first_x - start_x, row, area,
			  first - row->glyphs[area],
			  last - row->glyphs[area],
			  row->inverse_p ? DRAW_INVERSE_VIDEO : DRAW_NORMAL_TEXT,
			  NULL, NULL, 0);
    }
}
      

/* Redraw the parts of the glyph row ROW on window W intersecting
   rectangle R.  R is in window-relative coordinates.  Value is
   non-zero if mouse-face was overwritten.  */

static int
expose_line (struct window *w,
	     struct glyph_row *row,
	     RECT *r)
{
  xassert (row->enabled_p);
  
  if (row->mode_line_p || w->pseudo_window_p)
    mw32_draw_glyphs (w, 0, row, TEXT_AREA, 0, row->used[TEXT_AREA],
		      row->inverse_p ? DRAW_INVERSE_VIDEO : DRAW_NORMAL_TEXT,
		      NULL, NULL, 0);
  else
    {
      if (row->used[LEFT_MARGIN_AREA])
	expose_area (w, row, r, LEFT_MARGIN_AREA);
      if (row->used[TEXT_AREA])
	expose_area (w, row, r, TEXT_AREA);
      if (row->used[RIGHT_MARGIN_AREA])
	expose_area (w, row, r, RIGHT_MARGIN_AREA);
      mw32_draw_row_bitmaps (w, row);
    }

  return row->mouse_face_p;
}


/* Return non-zero if W's cursor intersects rectangle R.  */

static int
mw32_phys_cursor_in_rect_p (struct window *w, RECT *r)
{
  RECT cr, result;
  struct glyph *cursor_glyph;

  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph)
    {
      cr.left = w->phys_cursor.x;
      cr.top = w->phys_cursor.y;
      cr.right = cr.left + cursor_glyph->pixel_width;
      cr.bottom = cr.top + w->phys_cursor_height;
      return mw32_intersect_rectangles (&cr, r, &result);
    }
  else
    return 0;
}


/* Redraw the part of window W intersection rectangle FR.  Pixel
   coordinates in FR are frame-relative.  Call this function with
   input blocked.  Value is non-zero if the exposure overwrites
   mouse-face.  */

static int
expose_window (struct window *w, RECT *fr)
{
  struct frame *f = XFRAME (w->frame);
  RECT wr, r;
  int mouse_face_overwritten_p = 0;

  /* If window is not yet fully initialized, do nothing.  This can
     happen when toolkit scroll bars are used and a window is split.
     Reconfiguring the scroll bar will generate an expose for a newly
     created window.  */
  if (w->current_matrix == NULL)
    return 0;

  /* When we're currently updating the window, display and current
     matrix usually don't agree.  Arrange for a thorough display
     later.  */
  if (w == updated_window)
    {
      SET_FRAME_GARBAGED (f);
      return 0;
    }

  /* Frame-relative pixel rectangle of W.  */
  wr.left = XFASTINT (w->left) * CANON_X_UNIT (f);
  wr.top = XFASTINT (w->top) * CANON_Y_UNIT (f);
  wr.right = wr.left + XFASTINT (w->width) * CANON_X_UNIT (f);
  wr.bottom = wr.top + XFASTINT (w->height) * CANON_Y_UNIT (f);

  if (mw32_intersect_rectangles (fr, &wr, &r))
    {
      int yb = window_text_bottom_y (w);
      struct glyph_row *row;
      int cursor_cleared_p;
  
      TRACE ((stderr, "expose_window (%d, %d, %d, %d)\n",
	      r.left, r.top, r.right, r.bottom));

      /* Convert to window coordinates.  */
      r.left = FRAME_TO_WINDOW_PIXEL_X (w, r.left);
      r.top = FRAME_TO_WINDOW_PIXEL_Y (w, r.top);

      /* Turn off the cursor.  */
      if (!w->pseudo_window_p
	  && mw32_phys_cursor_in_rect_p (w, &r))
	{
	  mw32_clear_cursor (w);
	  cursor_cleared_p = 1;
	}
      else
	cursor_cleared_p = 0;

      /* Find the first row intersecting the rectangle R.  */
      for (row = w->current_matrix->rows;
	   row->enabled_p;
	   ++row)
	{
	  int y0 = row->y;
	  int y1 = MATRIX_ROW_BOTTOM_Y (row);

	  if ((y0 >= r.top && y0 < r.bottom)
	      || (y1 > r.top && y1 < r.bottom)
	      || (r.top >= y0 && r.top < y1)
	      || (r.bottom > y0 && r.bottom < y1))
	    {
	      if (expose_line (w, row, &r))
		mouse_face_overwritten_p = 1;
	    }

	  if (y1 >= yb)
	    break;
	}

      /* Display the mode line if there is one.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && (row = MATRIX_MODE_LINE_ROW (w->current_matrix),
	      row->enabled_p)
	  && row->y < r.bottom)
	{
	  if (expose_line (w, row, &r))
	    mouse_face_overwritten_p = 1;
	}

      if (!w->pseudo_window_p)
	{
	  /* Draw border between windows.  */
	  mw32_draw_vertical_border (w);
      
	  /* Turn the cursor on again.  */
	  if (cursor_cleared_p)
	    mw32_update_window_cursor (w, 1);
	}
    }
  
  return mouse_face_overwritten_p;
}


/* Determine the intersection of two rectangles R1 and R2.  Return
   the intersection in *RESULT.  Value is non-zero if RESULT is not
   empty.  */

static int
mw32_intersect_rectangles (RECT *pr1, RECT *pr2, RECT *presult)
{
  RECT r;
  int intersection_p = 0;

  r.left = max (pr1->left, pr2->left);
  r.top = max (pr1->top, pr2->top);
  r.right = min (pr1->right, pr2->right);
  r.bottom = min (pr1->bottom, pr2->bottom);
  if (((r.right - r.left) >= 0)
      && ((r.bottom - r.top) >= 0))
    {
      *presult = r;
      return 1;
    }
  return 0;
}


static void
frame_highlight (f)
     struct frame *f;
{
  /* Do nothing but update the cursor.  */
  mw32_update_cursor (f, 1);
}

static void
frame_unhighlight (f)
     struct frame *f;
{
  /* Do nothing but update the cursor.  */
  mw32_update_cursor (f, 1);
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

/* On MW32 implementation, also set focus_message_frame.  */

void
mw32_new_focus_frame (struct mw32_display_info *dpyinfo,
		      struct frame *frame)
{
  struct frame *old_focus = dpyinfo->mw32_focus_frame;

  dpyinfo->mw32_focus_message_frame = frame;

  if (frame != dpyinfo->mw32_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->mw32_focus_frame = frame;

#if 0  /* We shouldn't concern on lowering and raising the frame
	  because the system automatically operate them. */
      if (old_focus && old_focus->auto_lower)
	mw32_lower_frame (old_focus);
#endif

#if 0
      selected_frame = frame;
      XSETFRAME (XWINDOW (selected_frame->selected_window)->frame,
		 selected_frame);
      Fselect_window (selected_frame->selected_window);
      choose_minibuf_frame ();
#endif /* ! 0 */

      /* Only registering.  */
      if (dpyinfo->mw32_focus_frame && dpyinfo->mw32_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->mw32_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  mw32_frame_rehighlight_1 (dpyinfo);
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
mw32_mouse_leave (struct mw32_display_info *dpyinfo)
{
  mw32_new_focus_frame (dpyinfo, dpyinfo->mw32_focus_message_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
MW32_frame_rehighlight (struct frame *frame)
{
  mw32_frame_rehighlight_1 (FRAME_MW32_DISPLAY_INFO (frame));
}

static void
mw32_frame_rehighlight_1 (struct mw32_display_info *dpyinfo)
{
  struct frame *old_highlight = dpyinfo->mw32_highlight_frame;

  if (dpyinfo->mw32_focus_frame)
    {
      dpyinfo->mw32_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->mw32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->mw32_focus_frame))
	   : dpyinfo->mw32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->mw32_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->mw32_focus_frame) = Qnil;
	  dpyinfo->mw32_highlight_frame = dpyinfo->mw32_focus_frame;
	}
    }
  else
    dpyinfo->mw32_highlight_frame = 0;

  if (dpyinfo->mw32_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->mw32_highlight_frame)
	frame_highlight (dpyinfo->mw32_highlight_frame);
    }
}



/***********************************************************************
		  Glyph-Pixel Coordinate translation
 ***********************************************************************/

/* Given a pixel position (PIX_X, PIX_Y) on frame F, return glyph
   co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle that the
   glyph at X, Y occupies, if BOUNDS != 0.  If NOCLIP is non-zero, do
   not force the value into range.  */

void
pixel_to_glyph_coords (FRAME_PTR f, int pix_x, int pix_y,
		       int *px, int *py, RECT *bounds, int noclip)
{
  /* Arrange for the division in PIXEL_TO_CHAR_COL etc.  to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH ((f)->output_data.mw32->font) - 1;
  if (pix_y < 0)
    pix_y -= (f)->output_data.mw32->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->left = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->top = CHAR_TO_PIXEL_ROW (f, pix_y);
      bounds->right = bounds->left + FONT_WIDTH  (f->output_data.mw32->font);
      bounds->bottom = bounds->bottom + f->output_data.mw32->line_height;
    }

  if (!noclip)
    {
      if (pix_x < 0)
	pix_x = 0;
      else if (pix_x > FRAME_WINDOW_WIDTH (f))
	pix_x = FRAME_WINDOW_WIDTH (f);

      if (pix_y < 0)
	pix_y = 0;
      else if (pix_y > f->height)
	pix_y = f->height;
    }

  *px = pix_x;
  *py = pix_y;
}


/* Given HPOS/VPOS in the current matrix of W, return corresponding
   frame-relative pixel positions in *FRAME_X and *FRAME_Y.  If we
   can't tell the positions because W's display is not up to date,
   return 0.  */

int
glyph_to_pixel_coords (struct window *w,
		       int hpos, int vpos,
		       int *frame_x, int *frame_y)
{
  int success_p;

  xassert (hpos >= 0 && hpos < w->current_matrix->matrix_w);
  xassert (vpos >= 0 && vpos < w->current_matrix->matrix_h);

  if (display_completed)
    {
      struct glyph_row *row = MATRIX_ROW (w->current_matrix, vpos);
      struct glyph *glyph = row->glyphs[TEXT_AREA];
      struct glyph *end = glyph + min (hpos, row->used[TEXT_AREA]);

      *frame_y = row->y;
      *frame_x = row->x;
      while (glyph < end)
	{
	  *frame_x += glyph->pixel_width;
	  ++glyph;
	}

      success_p = 1;
    }
  else
    {
      *frame_y = *frame_x = 0;
      success_p = 0;
    }

  *frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, *frame_y);
  *frame_x = WINDOW_TO_FRAME_PIXEL_X (w, *frame_x);
  return success_p;
}



/************************************************************************
			Mouse Pointer Tracking
 ************************************************************************/

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static void
note_mouse_movement (FRAME_PTR frame, MSG *msg)
{
  if (msg->hwnd != FRAME_MW32_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (LOWORD (msg->lParam) < last_mouse_glyph.left
	   || LOWORD (msg->lParam) > last_mouse_glyph.right
	   || HIWORD (msg->lParam) < last_mouse_glyph.top
	   || HIWORD (msg->lParam) > last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, LOWORD (msg->lParam), HIWORD (msg->lParam));
    }
}

/* This is used for debugging, to turn off note_mouse_highlight.  */

 int disable_mouse_highlight;



/************************************************************************
			      Mouse Face
 ************************************************************************/

/* Find the glyph under window-relative coordinates X/Y in window W.
   Consider only glyphs from buffer text, i.e. no glyphs from overlay
   strings.  Return in *HPOS and *VPOS the row and column number of
   the glyph found.  Return in *AREA the glyph area containing X.
   Value is a pointer to the glyph found or null if X/Y is not on
   text, or we can't tell because W's current matrix is not up to
   date.  */

static struct glyph *
x_y_to_hpos_vpos (struct window *w,
		  int x, int y,
		  int *hpos, int *vpos, int *area,
		  int buffer_only_p)
{
  struct glyph *glyph, *end;
  struct glyph_row *row = NULL;
  int x0, i, left_area_width;

  /* Find row containing Y.  Give up if some row is not enabled.  */
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      row = MATRIX_ROW (w->current_matrix, i);
      if (!row->enabled_p)
	return NULL;
      if (y >= row->y && y < MATRIX_ROW_BOTTOM_Y (row))
	break;
    }

  *vpos = i;
  *hpos = 0;

  /* Give up if Y is not in the window.  */
  if (i == w->current_matrix->nrows)
    return NULL;

  /* Get the glyph area containing X.  */
  if (w->pseudo_window_p)
    {
      *area = TEXT_AREA;
      x0 = 0;
    }
  else
    {
      left_area_width = window_box_width (w, LEFT_MARGIN_AREA);
      if (x < left_area_width)
	{
	  *area = LEFT_MARGIN_AREA;
	  x0 = 0;
	}
      else if (x < left_area_width + window_box_width (w, TEXT_AREA))
	{
	  *area = TEXT_AREA;
	  x0 = row->x + left_area_width;
	}
      else
	{
	  *area = RIGHT_MARGIN_AREA;
	  x0 = left_area_width + window_box_width (w, TEXT_AREA);
	}
    }

  /* Find glyph containing X.  */
  glyph = row->glyphs[*area];
  end = glyph + row->used[*area];
  while (glyph < end)
    {
      if (x < x0 + glyph->pixel_width)
	{
	  if (w->pseudo_window_p)
	    break;
	  else if (!buffer_only_p || BUFFERP (glyph->object))
	    break;
	}
      
      x0 += glyph->pixel_width;
      ++glyph;
    }

  if (glyph == end)
    return NULL;

  *hpos = glyph - row->glyphs[*area];
  return glyph;
}


/* Convert frame-relative x/y to coordinates relative to window W.
   Takes pseudo-windows into account.  */

static void
frame_to_window_pixel_xy (struct window *w,
			  int *x, int *y)
{
  if (w->pseudo_window_p)
    {
      /* A pseudo-window is always full-width, and starts at the
	 left edge of the frame, plus a frame border.  */
      struct frame *f = XFRAME (w->frame);
      *x -= FRAME_INTERNAL_BORDER_WIDTH_SAFE (f);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
  else
    {
      *x = FRAME_TO_WINDOW_PIXEL_X (w, *x);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
}


/* Take proper action when mouse has moved to the mode or header line of
   window W, x-position X.  MODE_LINE_P non-zero means mouse is on the
   mode line.  X is relative to the start of the text display area of
   W, so the width of bitmap areas and scroll bars must be subtracted
   to get a position relative to the start of the mode line.  */

static void
note_mode_line_highlight (struct window *w,
			  int x, int mode_line_p)
{
  struct frame *f = XFRAME (w->frame);
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  struct glyph_row *row;
  HCURSOR cursor;

  if (mode_line_p)
    row = MATRIX_MODE_LINE_ROW (w->current_matrix);
  else
    row = MATRIX_HEADER_LINE_ROW (w->current_matrix);

  if (row->enabled_p)
    {
      struct glyph *glyph, *end;
      Lisp_Object help, map;
      int x0;
      
      /* Find the glyph under X.  */
      glyph = row->glyphs[TEXT_AREA];
      end = glyph + row->used[TEXT_AREA];
      x0 = - (FRAME_LEFT_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f)
	      + FRAME_MW32_LEFT_FLAGS_AREA_WIDTH (f));
      
      while (glyph < end
	     && x >= x0 + glyph->pixel_width)
	{
	  x0 += glyph->pixel_width;
	  ++glyph;
	}

      if (glyph < end
	  && STRINGP (glyph->object)
	  && XSTRING (glyph->object)->intervals
	  && glyph->charpos >= 0
	  && glyph->charpos < XSTRING (glyph->object)->size)
	{
	  /* If we're on a string with `help-echo' text property,
	     arrange for the help to be displayed.  This is done by
	     setting the global variable help_echo to the help string.  */
	  help = Fget_text_property (make_number (glyph->charpos),
				     Qhelp_echo, glyph->object);
	  if (!NILP (help))
	    {
	      help_echo = help;
	      XSETWINDOW (help_echo_window, w);
	      help_echo_object = glyph->object;
	      help_echo_pos = glyph->charpos;
	    }
	  /* Change the mouse pointer according to what is under X/Y.  */
	  map = Fget_text_property (make_number (glyph->charpos),
				    Qlocal_map, glyph->object);
	  if (KEYMAPP (map))
	    cursor = f->output_data.mw32->nontext_cursor;
	  else
	    {
	      map = Fget_text_property (make_number (glyph->charpos),
					Qkeymap, glyph->object);
	      if (KEYMAPP (map))
		cursor = f->output_data.mw32->nontext_cursor;
	    }
	}
    }

  /*
    TODO: mouse cursor
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
  */
}

/* Take proper action when the mouse has moved to position X, Y on
   frame F as regards highlighting characters that have mouse-face
   properties.  Also de-highlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

static Lisp_Object
note_mouse_highlight_handler (ignore)
     Lisp_Object ignore;
{
  return Qnil;
}

static void
note_mouse_highlight (struct frame *f, int x, int y)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int portion;
  Lisp_Object window;
  struct window *w;
  struct buffer *b;
  HCURSOR cursor = INVALID_HANDLE_VALUE;

  /* When a menu is active, don't highlight because this looks odd.  */
  if (popup_activated ())
    return;

  if (disable_mouse_highlight
      || !f->glyphs_initialized_p)
    return;

  dpyinfo->mouse_face_mouse_x = x;
  dpyinfo->mouse_face_mouse_y = y;
  dpyinfo->mouse_face_mouse_frame = f;

  if (dpyinfo->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      dpyinfo->mouse_face_deferred_gc = 1;
      return;
    }

  if (dpyinfo->mouse_cursor_stat < 0)
    {
      clear_mouse_face (dpyinfo);
      return;
    }

  /* Which window is that in?  */
  window = window_from_coordinates (f, x, y, &portion, 1);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);

  /* Not on a window -> return.  */
  if (!WINDOWP (window))
    return;

  /* Reset help_echo. It will get recomputed below.  */
  help_echo = Qnil;

  /* Convert to window-relative pixel coordinates.  */
  w = XWINDOW (window);
  frame_to_window_pixel_xy (w, &x, &y);

  /* Handle tool-bar window differently since it doesn't display a
     buffer.  */
  if (EQ (window, f->tool_bar_window))
    {
      note_tool_bar_highlight (f, x, y);
      return;
    }

  /* Mouse is on the mode or header line?  */
  if (portion == 1 || portion == 3)
    {
      note_mode_line_highlight (w, x, portion == 1);
      return;
    }

  if (portion == 2)
    cursor = f->output_data.mw32->horizontal_drag_cursor;
  else
    cursor = f->output_data.mw32->text_cursor;

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  b = XBUFFER (w->buffer);
  if (/* Within text portion of the window.  */
      portion == 0
      && EQ (w->window_end_valid, w->buffer)
      && XFASTINT (w->last_modified) == BUF_MODIFF (b)
      && XFASTINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
    {
      int hpos, vpos, pos, i, area;
      struct glyph *glyph;
      Lisp_Object object;
      Lisp_Object mouse_face = Qnil, overlay = Qnil, position;
      Lisp_Object *overlay_vec = NULL;
      int len, noverlays;
      struct buffer *obuf;
      int obegv, ozv, same_region;

      /* Find the glyph under X/Y.  */
      glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, &area, 0);

      /* Clear mouse face if X/Y not over text.  */
      if (glyph == NULL
	  || area != TEXT_AREA
	  || !MATRIX_ROW (w->current_matrix, vpos)->displays_text_p)
	{
	  if (clear_mouse_face (dpyinfo))
	    cursor = INVALID_HANDLE_VALUE;
	  goto set_cursor;
	}

      pos = glyph->charpos;
      object = glyph->object;
      if (!STRINGP (object) && !BUFFERP (object))
	goto set_cursor;

      /* If we get an out-of-range value, return now; avoid an error.  */
      if (BUFFERP (object) && pos > BUF_Z (b))
	goto set_cursor;

      /* Make the window's buffer temporarily current for
	 overlays_at and compute_char_face.  */
      obuf = current_buffer;
      current_buffer = b;
      obegv = BEGV;
      ozv = ZV;
      BEGV = BEG;
      ZV = Z;

      /* Is this char mouse-active or does it have help-echo?  */
      position = make_number (pos);

      if (BUFFERP (object))
	{
	  /* Put all the overlays we want in a vector in overlay_vec.
	     Store the length in len.  If there are more than 10, make
	     enough space for all, and try again.  */
	  len = 10;
	  overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	  noverlays = overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL, 0);
	  if (noverlays > len)
	    {
	      len = noverlays;
	      overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	      noverlays = overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL,0);
	    }

	  /* Sort overlays into increasing priority order.  */
	  noverlays = sort_overlays (overlay_vec, noverlays, w);
	}
      else
	noverlays = 0;

      same_region = (EQ (window, dpyinfo->mouse_face_window)
		     && vpos >= dpyinfo->mouse_face_beg_row
		     && vpos <= dpyinfo->mouse_face_end_row
		     && (vpos > dpyinfo->mouse_face_beg_row
			 || hpos >= dpyinfo->mouse_face_beg_col)
		     && (vpos < dpyinfo->mouse_face_end_row
			 || hpos < dpyinfo->mouse_face_end_col
			 || dpyinfo->mouse_face_past_end));

      if (same_region)
	cursor = INVALID_HANDLE_VALUE;

      /* Check mouse-face highlighting.  */
      if (! same_region
	  /* If there exists an overlay with mouse-face overlapping
	     the one we are currently highlighting, we have to
	     check if we enter the overlapping overlay, and then
	     highlight only that.  */
	  || (OVERLAYP (dpyinfo->mouse_face_overlay)
	      && mouse_face_overlay_overlaps (dpyinfo->mouse_face_overlay)))
	{
	  /* Find the highest priority overlay that has a mouse-face
	     property.  */
	  overlay = Qnil;
	  for (i = noverlays - 1; i >= 0 && NILP (overlay); --i)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		overlay = overlay_vec[i];
	    }
	  
	  /* If we're actually highlighting the same overlay as
	     before, there's no need to do that again.  */
	  if (!NILP (overlay)
	      && EQ (overlay, dpyinfo->mouse_face_overlay))
	    goto check_help_echo;
	    
	  dpyinfo->mouse_face_overlay = overlay;

	  /* Clear the display of the old active region, if any.  */
	  if (clear_mouse_face (dpyinfo))
	    cursor = INVALID_HANDLE_VALUE;

	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    {
	      Lisp_Object args[4];

	      args[0] = intern ("get-text-property");
	      args[1] = position;
	      args[2] = Qmouse_face;
	      args[3] = object;

	      mouse_face =
		internal_condition_case_2 (Ffuncall,
					   4,
					   args,
					   Qerror,
					   note_mouse_highlight_handler);
	    }						     

	  /* Handle the overlay case.  */
	  if (!NILP (overlay))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after;
	      int ignore;

	      before = Foverlay_start (overlay);
	      after = Foverlay_end (overlay);
	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);

	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;
	      dpyinfo->mouse_face_face_id
		= face_at_buffer_position (w, pos, 0, 0,
					   &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = INVALID_HANDLE_VALUE;
	    }
	  /* Handle the text property case.  */
	  else if (!NILP (mouse_face) && BUFFERP (object))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after, beginning, end;
	      int ignore;

	      beginning = Fmarker_position (w->start);
	      end = make_number (BUF_Z (XBUFFER (object))
				 - XFASTINT (w->window_end_pos));
	      before
		= Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, beginning);
	      after
		= Fnext_single_property_change (position, Qmouse_face,
						object, end);
		
	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);
	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;

	      if (BUFFERP (object))
		dpyinfo->mouse_face_face_id
		  = face_at_buffer_position (w, pos, 0, 0,
					     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = INVALID_HANDLE_VALUE;
	    }
	  else if (!NILP (mouse_face) && STRINGP (object))
	    {
	      Lisp_Object b, e;
	      int ignore;
		
	      b = Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, Qnil);
	      e = Fnext_single_property_change (position, Qmouse_face,
						object, Qnil);
	      if (NILP (b))
		b = make_number (0);
	      if (NILP (e))
		e = make_number (XSTRING (object)->size - 1);
	      fast_find_string_pos (w, XINT (b), object,
				    &dpyinfo->mouse_face_beg_col,
				    &dpyinfo->mouse_face_beg_row,
				    &dpyinfo->mouse_face_beg_x,
				    &dpyinfo->mouse_face_beg_y, 0);
	      fast_find_string_pos (w, XINT (e), object,
				    &dpyinfo->mouse_face_end_col,
				    &dpyinfo->mouse_face_end_row,
				    &dpyinfo->mouse_face_end_x,
				    &dpyinfo->mouse_face_end_y, 1);
	      dpyinfo->mouse_face_past_end = 0;
	      dpyinfo->mouse_face_window = window;
	      dpyinfo->mouse_face_face_id
		= face_at_string_position (w, object, pos, 0, 0, 0, &ignore,
					   glyph->face_id, 1);
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = INVALID_HANDLE_VALUE;
	    }
	  else if (STRINGP (object) && NILP (mouse_face))
	    {
	      /* A string which doesn't have mouse-face, but
		 the text ``under'' it might have.  */
	      struct glyph_row *r = MATRIX_ROW (w->current_matrix, vpos);
	      int start = MATRIX_ROW_START_CHARPOS (r);
	      
	      pos = string_buffer_position (w, object, start);
	      if (pos > 0)
		mouse_face = get_char_property_and_overlay (make_number (pos),
							    Qmouse_face,
							    w->buffer,
							    &overlay);
	      if (!NILP (mouse_face) && !NILP (overlay))
		{
		  Lisp_Object before = Foverlay_start (overlay);
		  Lisp_Object after = Foverlay_end (overlay);
		  int ignore;

		  /* Note that we might not be able to find position
		     BEFORE in the glyph matrix if the overlay is
		     entirely covered by a `display' property.  In
		     this case, we overshoot.  So let's stop in
		     the glyph matrix before glyphs for OBJECT.  */
		  fast_find_position (w, XFASTINT (before),
				      &dpyinfo->mouse_face_beg_col,
				      &dpyinfo->mouse_face_beg_row,
				      &dpyinfo->mouse_face_beg_x,
				      &dpyinfo->mouse_face_beg_y,
				      object);
		       
		  dpyinfo->mouse_face_past_end
		    = !fast_find_position (w, XFASTINT (after),
					   &dpyinfo->mouse_face_end_col,
					   &dpyinfo->mouse_face_end_row,
					   &dpyinfo->mouse_face_end_x,
					   &dpyinfo->mouse_face_end_y,
					   Qnil);
		  dpyinfo->mouse_face_window = window;
		  dpyinfo->mouse_face_face_id
		    = face_at_buffer_position (w, pos, 0, 0,
					       &ignore, pos + 1, 1);

		  /* Display it as active.  */
		  show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
		  cursor = INVALID_HANDLE_VALUE;
		}
	    }
	}

    check_help_echo:

      /* Look for a `help-echo' property.  */
      {
	Lisp_Object help, overlay;

	/* Check overlays first.  */
	help = overlay = Qnil;
	for (i = noverlays - 1; i >= 0 && NILP (help); --i)
	  {
	    overlay = overlay_vec[i];
	    help = Foverlay_get (overlay, Qhelp_echo);
	  }

	if (!NILP (help))
	  {
	    help_echo = help;
	    help_echo_window = window;
	    help_echo_object = overlay;
	    help_echo_pos = pos;
	  }
	else
	  {
	    Lisp_Object object = glyph->object;
	    int charpos = glyph->charpos;
	      
	    /* Try text properties.  */
	    if (STRINGP (object)
		&& charpos >= 0
		&& charpos < XSTRING (object)->size)
	      {
		help = Fget_text_property (make_number (charpos),
					   Qhelp_echo, object);
		if (NILP (help))
		  {
		    /* If the string itself doesn't specify a help-echo,
		       see if the buffer text ``under'' it does.  */
		    struct glyph_row *r
		      = MATRIX_ROW (w->current_matrix, vpos);
		    int start = MATRIX_ROW_START_CHARPOS (r);
		    int pos = string_buffer_position (w, object, start);
		    if (pos > 0)
		      {
			help = Fget_char_property (make_number (pos),
						   Qhelp_echo, w->buffer);
			if (!NILP (help))
			  {
			    charpos = pos;
			    object = w->buffer;
			  }
		      }
		  }
	      }
	    else if (BUFFERP (object)
		     && charpos >= BEGV
		     && charpos < ZV)
	      help = Fget_text_property (make_number (charpos), Qhelp_echo,
					 object);
	    
	    if (!NILP (help))
	      {
		help_echo = help;
		help_echo_window = window;
		help_echo_object = object;
		help_echo_pos = charpos;
	      }
	  }
      }
	  
      BEGV = obegv;
      ZV = ozv;
      current_buffer = obuf;
    }

 set_cursor:

#if 0
  if (cursor != INVALID_HANDLE_VALUE)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
#endif
  return;
}

static void
redo_mouse_highlight ()
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  LOWORD (last_mouse_motion_message.lParam),
			  HIWORD (last_mouse_motion_message.wParam));
}



/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* Tool-bar item index of the item on which a mouse button was pressed
   or -1.  */

static int last_tool_bar_item;


/* Get information about the tool-bar item at position X/Y on frame F.
   Return in *GLYPH a pointer to the glyph of the tool-bar item in
   the current matrix of the tool-bar window of F, or NULL if not
   on a tool-bar item.  Return in *PROP_IDX the index of the tool-bar
   item in F->tool_bar_items.  Value is

   -1	if X/Y is not on a tool-bar item
   0	if X/Y is on the same item that was highlighted before.
   1	otherwise.  */

static int
mw32_tool_bar_item (struct frame *f,
		    int x, int y,
		    struct glyph **glyph,
		    int *hpos, int *vpos,
		    int *prop_idx)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int area;

  /* Find the glyph under X/Y.  */
  *glyph = x_y_to_hpos_vpos (w, x, y, hpos, vpos, &area, 0);
  if (*glyph == NULL)
    return -1;

  /* Get the start of this tool-bar item's properties in
     f->tool_bar_items.  */
  if (!tool_bar_item_info (f, *glyph, prop_idx))
    return -1;

  /* Is mouse on the highlighted item?  */
  if (EQ (f->tool_bar_window, dpyinfo->mouse_face_window)
      && *vpos >= dpyinfo->mouse_face_beg_row
      && *vpos <= dpyinfo->mouse_face_end_row
      && (*vpos > dpyinfo->mouse_face_beg_row
	  || *hpos >= dpyinfo->mouse_face_beg_col)
      && (*vpos < dpyinfo->mouse_face_end_row
	  || *hpos < dpyinfo->mouse_face_end_col
	  || dpyinfo->mouse_face_past_end))
    return 0;
  
  return 1;
}


/* Possibly highlight a tool-bar item on frame F when mouse moves to
   tool-bar window-relative coordinates X/Y.  Called from
   note_mouse_highlight.  */

static void
note_tool_bar_highlight (struct frame *f, int x, int y)
{
  Lisp_Object window = f->tool_bar_window;
  struct window *w = XWINDOW (window);
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int hpos, vpos;
  struct glyph *glyph;
  struct glyph_row *row;
  int i;
  Lisp_Object enabled_p;
  int prop_idx;
  enum draw_glyphs_face draw;
  int mouse_down_p, rc;

  /* Function note_mouse_highlight is called with negative x(y
     values when mouse moves outside of the frame.  */
  if (x <= 0 || y <= 0)
    {
      clear_mouse_face (dpyinfo);
      return;
    }

  rc = mw32_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx);
  if (rc < 0)
    {
      /* Not on tool-bar item.  */
      clear_mouse_face (dpyinfo);
      return;
    }
  else if (rc == 0)
    goto set_help_echo;

  clear_mouse_face (dpyinfo);
  
  /* Mouse is down, but on different tool-bar item?  */
  mouse_down_p = (dpyinfo->grabbed
		  && f == last_mouse_frame
		  && FRAME_LIVE_P (f));
  if (mouse_down_p
      && last_tool_bar_item != prop_idx)
    return;

  dpyinfo->mouse_face_image_state = DRAW_NORMAL_TEXT;
  draw = mouse_down_p ? DRAW_IMAGE_SUNKEN : DRAW_IMAGE_RAISED;
  
  /* If tool-bar item is not enabled, don't highlight it.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (!NILP (enabled_p))
    {
      /* Compute the x-position of the glyph.  In front and past the
	 image is a space.  We include this is the highlighted area.  */
      row = MATRIX_ROW (w->current_matrix, vpos);
      for (i = x = 0; i < hpos; ++i)
	x += row->glyphs[TEXT_AREA][i].pixel_width;
      
      /* Record this as the current active region.  */
      dpyinfo->mouse_face_beg_col = hpos;
      dpyinfo->mouse_face_beg_row = vpos;
      dpyinfo->mouse_face_beg_x = x;
      dpyinfo->mouse_face_beg_y = row->y;
      dpyinfo->mouse_face_past_end = 0;
      
      dpyinfo->mouse_face_end_col = hpos + 1;
      dpyinfo->mouse_face_end_row = vpos;
      dpyinfo->mouse_face_end_x = x + glyph->pixel_width;
      dpyinfo->mouse_face_end_y = row->y;
      dpyinfo->mouse_face_window = window;
      dpyinfo->mouse_face_face_id = TOOL_BAR_FACE_ID;
      
      /* Display it as active.  */
      show_mouse_face (dpyinfo, draw);
      dpyinfo->mouse_face_image_state = draw;
    }
      
 set_help_echo:
  
  /* Set help_echo to a help string.to display for this tool-bar item.
     XTread_socket does the rest.  */
  help_echo_object = help_echo_window = Qnil;
  help_echo_pos = -1;
  help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_HELP);
  if (NILP (help_echo))
    help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_CAPTION);
}



/* Find the glyph matrix position of buffer position POS in window W.
   *HPOS, *VPOS, *X, and *Y are set to the positions found.  W's
   current glyphs must be up to date.  If POS is above window start
   return (0, 0, 0, 0).  If POS is after end of W, return end of
   last line in W.  */

#if 0 /* This is a version of fast_find_position that's more correct
	 in the presence of hscrolling, for example.  I didn't install
	 it right away because the problem fixed is minor, it failed
	 in 20.x as well, and I think it's too risky to install 
	 so near the release of 21.1.  2001-09-25 gerd.  */

static int
fast_find_position (w, charpos, hpos, vpos, x, y, stop)
     struct window *w;
     int charpos;
     int *hpos, *vpos, *x, *y;
     Lisp_Object stop;
{
  struct glyph_row *row, *first;
  struct glyph *glyph, *end;
  int i, past_end = 0;

  first = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row = row_containing_pos (w, charpos, first, NULL);
  if (row == NULL)
    {
      if (charpos < MATRIX_ROW_START_CHARPOS (first))
	{
	  *x = *y = *hpos = *vpos = 0;
	  return 0;
	}
      else
	{
	  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
	  past_end = 1;
	}
    }

  *x = row->x;
  *y = row->y;
  *vpos = MATRIX_ROW_VPOS (row, w->current_matrix);
  
  glyph = row->glyphs[TEXT_AREA];
  end = glyph + row->used[TEXT_AREA];
  
  /* Skip over glyphs not having an object at the start of the row.
     These are special glyphs like truncation marks on terminal
     frames.  */
  if (row->displays_text_p)
    while (glyph < end
	   && INTEGERP (glyph->object)
	   && !EQ (stop, glyph->object)
	   && glyph->charpos < 0)
      {
	*x += glyph->pixel_width;
	++glyph;
      }

  while (glyph < end
	 && !INTEGERP (glyph->object)
	 && !EQ (stop, glyph->object)
	 && (!BUFFERP (glyph->object)
	     || glyph->charpos < charpos))
    {
      *x += glyph->pixel_width;
      ++glyph;
    }

  *hpos = glyph - row->glyphs[TEXT_AREA];
  return past_end;
}

#else /* not 0 */

static int
fast_find_position (struct window *w, int pos,
		    int *hpos, int *vpos, int *x, int *y,
		    Lisp_Object stop)
{
  int i;
  int lastcol;
  int maybe_next_line_p = 0;
  int line_start_position;
  int yb = window_text_bottom_y (w);
  struct glyph_row *row, *best_row;
  int row_vpos, best_row_vpos;
  int current_x;

  row = best_row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row_vpos = best_row_vpos = MATRIX_ROW_VPOS (row, w->current_matrix);

  while (row->y < yb)
    {
      if (row->used[TEXT_AREA])
	line_start_position = row->glyphs[TEXT_AREA]->charpos;
      else
	line_start_position = 0;

      if (line_start_position > pos)
	break;
      /* If the position sought is the end of the buffer,
	 don't include the blank lines at the bottom of the window.  */
      else if (line_start_position == pos
	       && pos == BUF_ZV (XBUFFER (w->buffer)))
	{
	  maybe_next_line_p = 1;
	  break;
	}
      else if (line_start_position > 0)
	{
	  best_row = row;
	  best_row_vpos = row_vpos;
	}

      if (row->y + row->height >= yb)
	break;
      
      ++row;
      ++row_vpos;
    }
  
  /* Find the right column within BEST_ROW.  */
  lastcol = 0;
  current_x = best_row->x;
  for (i = 0; i < best_row->used[TEXT_AREA]; i++)
    {
      struct glyph *glyph = best_row->glyphs[TEXT_AREA] + i;
      int charpos = glyph->charpos;

      if (BUFFERP (glyph->object))
	{
	  if (charpos == pos)
	    {
	      *hpos = i;
	      *vpos = best_row_vpos;
	      *x = current_x;
	      *y = best_row->y;
	      return 1;
	    }
	  else if (charpos > pos)
	    break;
	}
      else if (EQ (glyph->object, stop))
	break;

      if (charpos > 0)
	lastcol = i;
      current_x += glyph->pixel_width;
    }

  /* If we're looking for the end of the buffer,
     and we didn't find it in the line we scanned,
     use the start of the following line.  */
  if (maybe_next_line_p)
    {
      ++best_row;
      ++best_row_vpos;
      lastcol = 0;
      current_x = best_row->x;
    }

  *vpos = best_row_vpos;
  *hpos = lastcol + 1;
  *x = current_x;
  *y = best_row->y;
  return 0;
}

#endif /* not 0 */


/* Find the position of the the glyph for position POS in OBJECT in
   window W's current matrix, and return in *X/*Y the pixel
   coordinates, and return in *HPOS/*VPOS the column/row of the glyph.

   RIGHT_P non-zero means return the position of the right edge of the
   glyph, RIGHT_P zero means return the left edge position.

   If no glyph for POS exists in the matrix, return the position of
   the glyph with the next smaller position that is in the matrix, if
   RIGHT_P is zero.  If RIGHT_P is non-zero, and no glyph for POS
   exists in the matrix, return the position of the glyph with the
   next larger position in OBJECT.

   Value is non-zero if a glyph was found.  */

static int
fast_find_string_pos (struct window *w, int pos, Lisp_Object object,
		      int *hpos, int *vpos, int *x, int *y, int right_p)
{
  int yb = window_text_bottom_y (w);
  struct glyph_row *r;
  struct glyph *best_glyph = NULL;
  struct glyph_row *best_row = NULL;
  int best_x = 0;

  for (r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       r->enabled_p && r->y < yb;
       ++r)
    {
      struct glyph *g = r->glyphs[TEXT_AREA];
      struct glyph *e = g + r->used[TEXT_AREA];
      int gx;

      for (gx = r->x; g < e; gx += g->pixel_width, ++g)
	if (EQ (g->object, object))
	  {
	    if (g->charpos == pos)
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
		goto found;
	      }
	    else if (best_glyph == NULL
		     || ((abs (g->charpos - pos)
			 < abs (best_glyph->charpos - pos))
			 && (right_p
			     ? g->charpos < pos
			     : g->charpos > pos)))
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
	      }
	  }
    }

 found:

  if (best_glyph)
    {
      *x = best_x;
      *hpos = best_glyph - best_row->glyphs[TEXT_AREA];

      if (right_p)
	{
	  *x += best_glyph->pixel_width;
	  ++*hpos;
	}
      
      *y = best_row->y;
      *vpos = best_row - w->current_matrix->rows;
    }

  return best_glyph != NULL;
}


/* Display the active region described by mouse_face_*
   in its mouse-face if HL > 0, in its normal face if HL = 0.  */

static void
show_mouse_face (struct mw32_display_info *dpyinfo,
		 enum draw_glyphs_face draw)
{
  struct window *w = XWINDOW (dpyinfo->mouse_face_window);
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  int i;
  int cursor_off_p = 0;
  struct cursor_pos saved_cursor;
  HDC mhdc = INVALID_HANDLE_VALUE;

  /* If the current thread is not main thread,
     prepare HDC */
  if (!MW32_MAIN_THREAD_P ())
    {
      mhdc = GetDC (FRAME_MW32_WINDOW (f));
      mw32_setup_default_hdc (mhdc);
      f->output_data.mw32->message_thread_hdc = mhdc;
    }

  saved_cursor = output_cursor;
  
  /* If window is in the process of being destroyed, don't bother
     to do anything.  */
  if (w->current_matrix == NULL)
    goto set_x_cursor;

  /* Recognize when we are called to operate on rows that don't exist
     anymore.  This can happen when a window is split.  */
  if (dpyinfo->mouse_face_end_row >= w->current_matrix->nrows)
    goto set_x_cursor;

  set_output_cursor (&w->phys_cursor);

  /* Note that mouse_face_beg_row etc. are window relative.  */
  for (i = dpyinfo->mouse_face_beg_row;
       i <= dpyinfo->mouse_face_end_row;
       i++)
    {
      int start_hpos, end_hpos, start_x;
      struct glyph_row *row = MATRIX_ROW (w->current_matrix, i);

      /* Don't do anything if row doesn't have valid contents.  */
      if (!row->enabled_p)
	continue;

      /* For all but the first row, the highlight starts at column 0.  */
      if (i == dpyinfo->mouse_face_beg_row)
	{
	  start_hpos = dpyinfo->mouse_face_beg_col;
	  start_x = dpyinfo->mouse_face_beg_x;
	}
      else
	{
	  start_hpos = 0;
	  start_x = 0;
	}

      if (i == dpyinfo->mouse_face_end_row)
	end_hpos = dpyinfo->mouse_face_end_col;
      else
	end_hpos = row->used[TEXT_AREA];

      /* If the cursor's in the text we are about to rewrite, turn the
	 cursor off.  */
      if (!w->pseudo_window_p
	  && i == output_cursor.vpos
	  && output_cursor.hpos >= start_hpos - 1
	  && output_cursor.hpos <= end_hpos)
	{
	  mw32_update_window_cursor (w, 0);
	  cursor_off_p = 1;
	}

      if (end_hpos > start_hpos)
	{
	  mw32_draw_glyphs (w, start_x, row, TEXT_AREA, 
			    start_hpos, end_hpos, draw, NULL, NULL, 0);
	  row->mouse_face_p = draw == DRAW_MOUSE_FACE || DRAW_IMAGE_RAISED;
	}
    }

  /* If we turned the cursor off, turn it back on.  */
  if (cursor_off_p)
    mw32_display_cursor (w, 1,
			 output_cursor.hpos, output_cursor.vpos,
			 output_cursor.x, output_cursor.y);

  output_cursor = saved_cursor;

 set_x_cursor:
#if 0 /* TODO: mouse cursor
  /* Change the mouse cursor.  */
  if (draw == DRAW_NORMAL_TEXT)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->text_cursor);
  else if (draw == DRAW_MOUSE_FACE)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->cross_cursor);
  else
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->nontext_cursor);
#endif


  if (!MW32_MAIN_THREAD_P ()
      && mhdc != INVALID_HANDLE_VALUE)
    {
      ReleaseDC (FRAME_MW32_WINDOW (f), mhdc);
      f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
    }

  return;
}

/* Clear out the mouse-highlighted active region.
   Redraw it un-highlighted first.  Value is non-zero if mouse
   face was actually drawn unhighlighted.  */

static int
clear_mouse_face (struct mw32_display_info *dpyinfo)
{
  int cleared = 0;
  
  if (!NILP (dpyinfo->mouse_face_window))
    {
      show_mouse_face (dpyinfo, DRAW_NORMAL_TEXT);
      cleared = 1;
    }

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  return cleared;
}


/* Clear any mouse-face on window W.  This function is part of the
   redisplay interface, and is called from try_window_id and similar
   functions to ensure the mouse-highlight is off.  */

static void
mw32i_clear_mouse_face (struct window *w)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (XFRAME (w->frame));
  Lisp_Object window;

  BLOCK_INPUT;
  XSETWINDOW (window, w);
  if (EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);
  /* If we should require GdiFlush(),
     insert here.  */
  UNBLOCK_INPUT;
}


/* Just discard the mouse face information for frame F, if any.
   This is used when the size of F is changed.  */

void
cancel_mouse_face (FRAME_PTR f)
{
  Lisp_Object window;
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);

  window = dpyinfo->mouse_face_window;
  if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }
}



/* Try to determine frame pixel position and size of the glyph under
   frame pixel coordinates X/Y on frame F .  Return the position and
   size in *RECT.  Value is non-zero if we could compute these
   values.  */

static int
glyph_rect (struct frame *f, int x, int y, RECT *rect)
{
  Lisp_Object window;
  int part, found = 0;

  window = window_from_coordinates (f, x, y, &part, 0);
  if (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      struct glyph_row *r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
      struct glyph_row *end = r + w->current_matrix->nrows - 1;
      int area;

      frame_to_window_pixel_xy (w, &x, &y);
      
      for (; !found && r < end && r->enabled_p; ++r)
	if (r->y >= y)
	  {
	    struct glyph *g = r->glyphs[TEXT_AREA];
	    struct glyph *end = g + r->used[TEXT_AREA];
	    int gx;
	      
	    for (gx = r->x; !found && g < end; gx += g->pixel_width, ++g)
	      if (gx >= x)
		{
		  rect->left = WINDOW_TO_FRAME_PIXEL_X (w, gx);
		  rect->top = WINDOW_TO_FRAME_PIXEL_Y (w, r->y);
		  rect->right = rect->left + g->pixel_width;
		  rect->bottom = rect->top + r->height;
		  found = 1;
		}
	  }
    }

  return found;
}


/* Return the current position of the mouse.
   *FP should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *FP, *BAR_WINDOW,
   and *PART to the frame, window, and scroll bar part that the mouse
   is over.  Set *X and *Y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *FP to the frame the
   mouse is on, *BAR_WINDOW to nil, and *X and *Y to the character cell
   the mouse is over.

   Set *TIME to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
MW32_mouse_position (FRAME_PTR *fp,
		     int insist,
		     Lisp_Object *bar_window,
		     enum scroll_bar_part *part,
		     Lisp_Object *x, Lisp_Object *y,
		     unsigned long *time)
{
  FRAME_PTR f1;

  BLOCK_INPUT;

#if 0
  if (! NILP (last_mouse_scroll_bar))
    mw32_scroll_bar_report_motion (f, bar_window, part, x, y, time);
  else
#endif
    {
      POINT pt;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_MW32_DISPLAY (XFRAME (frame)) == FRAME_MW32_DISPLAY (*fp))
	  XFRAME (frame)->mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;
      
      GetCursorPos (&pt);
      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	if (FRAME_MW32_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    /* Is the window one of our frames?  */
	    f1 = mw32_any_window_to_frame (FRAME_MW32_DISPLAY_INFO (*fp),
					   WindowFromPoint (pt));
	  }
      
#if 0
	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar = w32_window_to_scroll_bar (win);

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
#if 0
		win_x = parent_x;
		win_y = parent_y;
#endif
	      }
	  }

#endif
	if (f1 == 0 && insist > 0)
	  f1 = SELECTED_FRAME ();

	if (f1)
	  {
	    /* Ok, we found a frame.  Store all the values.
	       last_mouse_glyph is a rectangle used to reduce the
	       generation of mouse events.  To not miss any motion
	       events, we must divide the frame into rectangles of the
	       size of the smallest character that could be displayed
	       on it, i.e. into the same rectangles that matrices on
	       the frame are divided into.  */
	    int width, height, gx, gy;
	    RECT rect;

	    ScreenToClient (FRAME_MW32_WINDOW (f1), &pt);
	    
	    if (glyph_rect (f1, pt.x, pt.y, &rect))
	      last_mouse_glyph = rect;
	    else
	      {
		width = FRAME_SMALLEST_CHAR_WIDTH (f1);
		height = FRAME_SMALLEST_FONT_HEIGHT (f1);
		gx = pt.x;
		gy = pt.y;
	      
		/* Arrange for the division in PIXEL_TO_CHAR_COL etc. to
		   round down even for negative values.  */
		if (gx < 0)
		  gx -= width - 1;
		if (gy < 0)
		  gy -= height - 1;
		gx = (gx + width - 1) / width * width;
		gy = (gy + height - 1) / height * height;
	    
		last_mouse_glyph.left = gx;
		last_mouse_glyph.top = gy;
		last_mouse_glyph.right = gx + width;
		last_mouse_glyph.bottom = gy + height;
	      }

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, pt.x);
	    XSETINT (*y, pt.y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}


/************************************************************************
		     Windows Control Scroll bar.
 ************************************************************************/

/* Given a window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */
struct scroll_bar *
mw32_window_to_scroll_bar (HWND hwnd)
{
  Lisp_Object tail, frame;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object frame, bar, condemned;

      frame = XCAR (tail);
      /* All elements of Vframe_list should be frames.  */
      if (! GC_FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
         right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
              condemned scroll bar lists with one loop.  */
	   ! GC_NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! GC_NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (SCROLL_BAR_MW32_WINDOW (XSCROLL_BAR (bar)) == hwnd)
	  return XSCROLL_BAR (bar);
    }
  return 0;
}

/* Open a new window to serve as a scroll bar, and return the
   scroll bar vector for it.  */
static struct scroll_bar *
mw32_scroll_bar_create (struct window *window,
			int top, int left,
			int width, int height)
{
  FRAME_PTR frame = XFRAME (WINDOW_FRAME (window));
  struct scroll_bar *bar =
    XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

  {
    extern HINSTANCE hinst;
    MSG msg;
    RECT rect;
    HWND hwndScroll;
    SCROLLINFO scinfo;

    rect.top = top;
    rect.left = left;
    rect.bottom = height;
    rect.right = width;
    SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW (frame),
			 WM_EMACS_CREATE_SCROLLBAR,
			 (WPARAM) &rect, (LPARAM) hinst);
    WAIT_REPLY_MESSAGE (&msg, WM_EMACS_CREATE_SCROLLBAR_REPLY);

    scinfo.cbSize = sizeof (SCROLLINFO);
    scinfo.fMask = SIF_ALL;
    scinfo.nMin = 0;
    scinfo.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (frame, height);
    scinfo.nPage = 0;
    scinfo.nPos = 0;
    SetScrollInfo ((HWND) msg.wParam, SB_CTL, &scinfo, TRUE);

    SET_SCROLL_BAR_MW32_WINDOW (bar, (HWND) msg.wParam);

  }

  XSETWINDOW (bar->window, window);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (frame);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (frame), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  return bar;
}

/* Draw BAR's handle in the proper position.
   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)  

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll bar
   handle, we want to let them drag it down all the way, so that the
   bar's top is as far down as it goes; otherwise, there's no way to
   move to the very end of the buffer.  */
static void
mw32_scroll_bar_set_handle (struct scroll_bar *bar,
			    int start, int end,
			    int rebuild)
{
  int dragging;
  HWND hwndScroll;
  SCROLLINFO scinfo;

  hwndScroll = SCROLL_BAR_MW32_WINDOW (bar);

  dragging = ! NILP (bar->dragging);

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  scinfo.cbSize = sizeof (SCROLLINFO);
  scinfo.fMask = SIF_PAGE | SIF_POS;
  scinfo.nPage = end - start;
  scinfo.nPos = start;
  SetScrollInfo (hwndScroll, SB_CTL, &scinfo, TRUE);

  /* Store the adjusted setting in the scroll bar.  */
  XSETINT (bar->start, start);
  XSETINT (bar->end, end);
}

/* Move a scroll bar around on the screen, to accommodate changing
   window configurations.  */
static void
mw32_scroll_bar_move (struct scroll_bar *bar,
		      int top, int left, int width, int height)
{
  HWND hwndScroll;

  hwndScroll = SCROLL_BAR_MW32_WINDOW (bar);

  MoveWindow (hwndScroll, left, top, width, height, TRUE);

  XSETINT (bar->left,   left);
  XSETINT (bar->top,    top);
  XSETINT (bar->width,  width);
  XSETINT (bar->height, height);
}

/* Destroy the window for BAR, and set its Emacs window's scroll bar
   to nil.  */
static void
mw32_scroll_bar_remove (struct scroll_bar *bar)
{
  MSG msg;

  /* Destroy the window.  */
  SEND_INFORM_MESSAGE (SCROLL_BAR_MW32_WINDOW (bar), WM_CLOSE, 0, 0);

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
MW32_set_vertical_scroll_bar (struct window *w,
			      int portion, int whole, int position)
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  struct scroll_bar *bar;
  int top, height, left, sb_left, width, sb_width;
  int window_x, window_y, window_width, window_height;

  /* Get window dimensions.  */
  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);
  top = window_y;
  width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    left = XINT (w->left) + XINT (w->width) - FRAME_SCROLL_BAR_COLS (f);
  else
    left = XFASTINT (w->left);
  left *= CANON_X_UNIT (f);
  left += FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0)
    sb_width = FRAME_SCROLL_BAR_PIXEL_WIDTH (f);
  else
    sb_width = width;

#if 0
  /* Compute the left edge of the scroll bar.  */
  /*    width
     |<-------->|
     | sb_width |
     |  <---->  | 
     |  +----+  |
     |  | /\ |  |
     |  |/  \|  |
     |  |----|  |
  */
  sb_left = left + (width - sb_width) / 2;
#else
  /* scroll bar is attached to the left boundary.  */
  sb_left = left;
#endif

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    bar = mw32_scroll_bar_create (w, top, sb_left, sb_width, height);
  else
    {
      /* It may just need to be moved and resized.  */
      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      mw32_scroll_bar_move (bar, top, sb_left, sb_width, height);
    }

  /* Since toolkit scroll bars are smaller than the space reserved
     for them on the frame, we have to clear "under" them.  */
  if (width > 0 && height > 0)
    mw32_clear_area (f, left, top, left + width, top + height);

  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (NILP (bar->dragging))
    {
      int top_range =
	VERTICAL_SCROLL_BAR_TOP_RANGE (f, height);

      if (whole == 0)
	mw32_scroll_bar_set_handle (bar, 0, top_range, 0);
      else
	{
	  int start = (int)(((double) position * top_range) / whole);
	  int end = (int)(((double) (position + portion) * top_range) / whole);

	  mw32_scroll_bar_set_handle (bar, start, end, 0);
	}
    }

  XSETVECTOR (w->vertical_scroll_bar, bar);
}

/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

static void 
MW32_condemn_scroll_bars (FRAME_PTR frame)
{
  /* Transfer all the scroll bars to FRAME_CONDEMNED_SCROLL_BARS.  */
  while (! NILP (FRAME_SCROLL_BARS (frame)))
    {
      Lisp_Object bar;
      bar = FRAME_SCROLL_BARS (frame);
      FRAME_SCROLL_BARS (frame) = XSCROLL_BAR (bar)->next;
      XSCROLL_BAR (bar)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
      XSCROLL_BAR (bar)->prev = Qnil;
      if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = bar;
      FRAME_CONDEMNED_SCROLL_BARS (frame) = bar;
    }
}

/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
MW32_redeem_scroll_bar (struct window *window)
{
  struct scroll_bar *bar;
  FRAME_PTR f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  f = XFRAME (WINDOW_FRAME (window));
  if (NILP (bar->prev))
    {
      /* If the prev pointer is nil, it must be the first in one of
	 the lists.  */
      if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
	/* It's not condemned.  Everything's fine.  */
	return;
      else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		   window->vertical_scroll_bar))
	FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
      else
	/* If its prev pointer is nil, it must be at the front of
	   one or the other!  */
	abort ();
    }
  else
    XSCROLL_BAR (bar->prev)->next = bar->next;

  if (! NILP (bar->next))
    XSCROLL_BAR (bar->next)->prev = bar->prev;

  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */
static void
MW32_judge_scroll_bars (FRAME_PTR f)
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      mw32_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}

/* Handle an Expose or GraphicsExpose event on a scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */
void
mw32_scroll_bar_expose (FRAME_PTR f)
{
#if 0 /* Needless for Windows control */
  Lisp_Object bar;

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    {
      HWND hwndScroll = SCROLL_BAR_MW32_WINDOW (XSCROLL_BAR (bar));
      UpdateWindow (hwndScroll);
    }
#endif
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */
static int
mw32_scroll_bar_handle_click (struct scroll_bar *bar, MSG *msg,
			      struct input_event *emacs_event)
{
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO (msg->hwnd);
  if (!GC_WINDOWP (bar->window))
    abort ();

  EVENT_INIT (*emacs_event);
  MW32_INIT_EMACS_EVENT (*emacs_event);
  emacs_event->kind = w32_scroll_bar_click;
  emacs_event->code = 0;
  emacs_event->modifiers = MW32GETMODIFIER (dpyinfo);
  emacs_event->frame_or_window = bar->window;
  emacs_event->timestamp = msg->time;
  {
    int y;
    int top_range =
      VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

    y = HIWORD (msg -> wParam);
   /* copy from emacs 19.34 ....(himi) */
    switch (LOWORD (msg -> wParam))
      {
      case SB_THUMBTRACK:
	emacs_event->part = scroll_bar_handle;
#if 0
	printf ("Scroll Bar Tracking...%d\n", y);
	fflush (stdout);
#endif
	break;
      case SB_LINEDOWN:
	emacs_event->part = scroll_bar_down_arrow;
	break;
      case SB_LINEUP:
	emacs_event->part = scroll_bar_up_arrow;
	break;
      case SB_PAGEUP:
	emacs_event->part = scroll_bar_above_handle;
	break;
      case SB_PAGEDOWN:
	emacs_event->part = scroll_bar_below_handle;
	break;
      case SB_TOP:
	emacs_event->part = scroll_bar_handle;
	y = 0;
	break;
      case SB_BOTTOM:
	emacs_event->part = scroll_bar_handle;
	y = top_range;
	break;
      case SB_THUMBPOSITION:
	emacs_event->part = scroll_bar_handle;
#if 0
	printf ("Scroll Bar Tracking...%d\n", y);
	fflush (stdout);
#endif
	break;
      case SB_ENDSCROLL:
      default:
	return 0;
	break;
      }
    XSETINT (emacs_event->x, y);

    XSETINT (emacs_event->y, top_range);
  }
  return 1;
}

void
mw32_scroll_bar_store_event (WPARAM wParam, LPARAM lParam)
{
  struct input_event event;
  MSG msg;
  struct scroll_bar *bar;

  bar = mw32_window_to_scroll_bar ((HWND) lParam);
  msg.time = GetTickCount ();
  msg.wParam = wParam;
  msg.lParam = lParam;
  if (bar && mw32_scroll_bar_handle_click (bar, &msg, &event))
    {
      kbd_buffer_store_event (&event);
      SetEvent (keyboard_handle);
    }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

static void
mw32_scroll_bar_report_motion (FRAME_PTR *fp,
			       Lisp_Object *bar_window,
			       enum scroll_bar_part *part,
			       Lisp_Object *x, Lisp_Object *y,
			       unsigned long *time)
{
#if 0  /* TODO: not implemented yet on MW32 */
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  Window w = SCROLL_BAR_X_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int win_x, win_y;
  Window dummy_window;
  int dummy_coord;
  unsigned int dummy_mask;

  BLOCK_INPUT;

  /* Get the mouse's position relative to the scroll bar window, and
     report that.  */
  if (! XQueryPointer (FRAME_X_DISPLAY (f), w,

		       /* Root, child, root x and root y.  */
		       &dummy_window, &dummy_window,
		       &dummy_coord, &dummy_coord,

		       /* Position relative to scroll bar.  */
		       &win_x, &win_y,

		       /* Mouse buttons and modifier keys.  */
		       &dummy_mask))
    ;
  else
    {
#if 0
      int inside_height
	= VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, XINT (bar->height));
#endif
      int top_range
	= VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

      win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

      if (! NILP (bar->dragging))
	win_y -= XINT (bar->dragging);

      if (win_y < 0)
	win_y = 0;
      if (win_y > top_range)
	win_y = top_range;

      *fp = f;
      *bar_window = bar->window;

      if (! NILP (bar->dragging))
	*part = scroll_bar_handle;
      else if (win_y < XINT (bar->start))
	*part = scroll_bar_above_handle;
      else if (win_y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_below_handle;

      XSETINT (*x, win_y);
      XSETINT (*y, top_range);

      f->mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;
    }

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
#endif
}


/************************************************************************
            Message processing  Section 1 (Keyboard, Mouse, DnD, Menu)
 ************************************************************************/

/* internal variables */

int mw32_rbutton_to_emacs_button;
int mw32_mbutton_to_emacs_button;
int mw32_lbutton_to_emacs_button;
int mw32_hide_mouse_timeout;
int mw32_hide_mouse_on_key;

static void
mw32_mouse_button_cc (struct mw32_display_info *dpyinfo,
		      UINT mouse_event,
		      int *button,
		      int *up,
		      int *modifier)
{
  Lisp_Object modp = Qnil;

  switch (mouse_event) {
  case WM_LBUTTONUP:
    *button = mw32_lbutton_to_emacs_button;
    *up = 1;
    break;
  case WM_LBUTTONDOWN:
    *button = mw32_lbutton_to_emacs_button;
    *up = 0;
    break;
  case WM_MBUTTONUP:
    *button = mw32_mbutton_to_emacs_button;
    *up = 1;
    break;
  case WM_MBUTTONDOWN:
    *button = mw32_mbutton_to_emacs_button;
    *up = 0;
    break;
  case WM_RBUTTONUP:
    *button = mw32_rbutton_to_emacs_button;
    *up = 1;
    break;
  case WM_RBUTTONDOWN:
    *button = mw32_rbutton_to_emacs_button;
    *up = 0;
    break;
  }

  *modifier = MW32GETMOUSEMODIFIER (dpyinfo, *up);
}

/* Keyboard processing - modifier keys, etc. */
/* Convert a keysym to its name.  */
char *
x_get_keysym_name (int keysym)
{
  /* Make static so we can always return it */
  static char value[100];

  GetKeyNameText (keysym, value, 100);
  return value;
}

/* Modifier Key list.  ....
   0 is a normal key, 1 is a neglected key, 2 is meta modifier,
   4 is ctrl modifier, 8 is shift modifier, 16 is alt modifier,
   32 is super modifier. 64 is hyper modifier.  
   128 is a special key that is translated by windows. 
   (ToAscii translate it to normal key).
   It is noted to prevent it.  */
/*0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f   */
static BYTE keymodifier[256] =
{
  0,  0,  0,  0,  0,  0,  0,  0,128,128,  0,  0,  0,128,  0,  0,  /* 0x00-0x0f */
  8,  4,  2,  0,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  0,  0,  /* 0x10-0x1f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,128,  0,  /* 0x20-0x2f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x30-0x3f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x40-0x4f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x50-0x5f */
128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,  /* 0x60-0x6f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x70-0x7f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x80-0x8f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0x90-0x9f */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xa0-0xaf */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xb0-0xbf */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xc0-0xcf */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xd0-0xdf */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xe0-0xef */
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  /* 0xf0-0xff */
};

static int
mw32_get_keymodifier_state ()
{
  BYTE keystate[256];
  int i;
  int mod_result = 0;
  PBYTE pks, pkm;
  BYTE tmp;

  pks = keystate;
  pkm = keymodifier;
  GetKeyboardState (keystate);
  for (i = 0; i < 256; i++, pks++, pkm++)
    {
      if ((*pkm == 0) || (*pkm == 128)) continue;    /* for speed.  */
      tmp = *pks >> 7;
      if (tmp & (*pkm >> 1)) mod_result |= meta_modifier;
      else if (tmp & (*pkm >> 2)) mod_result |= ctrl_modifier;
      else if (tmp & (*pkm >> 3)) mod_result |= shift_modifier;
      else if (tmp & (*pkm >> 4)) mod_result |= alt_modifier;
      else if (tmp & (*pkm >> 5)) mod_result |= super_modifier;
      else if (tmp & (*pkm >> 6)) mod_result |= hyper_modifier;
    }
  return mod_result;
}

static int
mw32_emacs_translate_message (struct mw32_display_info *dpyinfo,
			      UINT type, UINT virtkey,
			      UINT *keycode, int *modifier)
{
  BYTE keystate[256];
  static BYTE ansi_code[4];
  static int isdead = 0;
  extern char *lispy_function_keys[];

  static int cur_mod;

  if (!type) {
    if (isdead) {
      *modifier = cur_mod;
      isdead = 0;
      *keycode = ansi_code[2];
      return 1;
    }
    return 0;
  }

  cur_mod = MW32GETMODIFIER (dpyinfo);
  *modifier = cur_mod;

  if (keymodifier[virtkey] == 128) {
    *keycode = virtkey;
    return 2;
  }
  if (keymodifier[virtkey] == 1) return 3;
  if (keymodifier[virtkey]) return 0;

  GetKeyboardState (keystate);
  if (cur_mod)
    keystate[VK_KANA] = 0;
  keystate[VK_CONTROL] = 0;
  keystate[VK_MENU] = 0;
  keystate[VK_RCONTROL] = 0;
  keystate[VK_LCONTROL] = 0;
  keystate[VK_RMENU] = 0;
  keystate[VK_LMENU] = 0;
  if (keymodifier[VK_CAPITAL])
    keystate[VK_CAPITAL] = 0;
  if (cur_mod & shift_modifier)
    keystate[VK_SHIFT] = 0x80;
  else
    keystate[VK_SHIFT] = 0;

  /* Use ToAsciiEx() only! */
  isdead = ToAsciiEx (virtkey, 0, keystate, (LPWORD) ansi_code,
		      0, GetKeyboardLayout (0));
  if (isdead < 0) return 0;
  if (isdead >= 1) {
    isdead--;
    *keycode = ansi_code[0];
    return 1;
  }
  if (lispy_function_keys[virtkey]) {
    *keycode = virtkey;
    return 2;
  }
  return 3;
}

/* Drop file Support */

int 
mw32_drop_file_handler (FRAME_PTR frame,
			MSG* msg,
			struct input_event* emacs_event)
{
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO (msg->hwnd);
  HDROP hDrop;
  POINT pt;

  hDrop = (HANDLE) msg->wParam;
  DragQueryPoint (hDrop, &pt);
  /* DragQueryPoint returns position based on window coordination */
  EVENT_INIT (*emacs_event);
  MW32_INIT_EMACS_EVENT (*emacs_event);
  emacs_event->kind = drag_n_drop;
  emacs_event->code = (int) hDrop;
  emacs_event->modifiers = MW32GETMODIFIER (dpyinfo);
  XSETINT (emacs_event->x, pt.x);
  XSETINT (emacs_event->y, pt.y);
  XSETFRAME (emacs_event->frame_or_window, frame);
  emacs_event->timestamp = msg->time;

  return 1;
}

/* IntelliMouse Support */

#ifdef W32_INTELLIMOUSE
int 
mw32_mouse_wheel_handler (FRAME_PTR frame,
			  MSG* msg,
			  struct input_event* emacs_event)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (frame);
  POINT pt;

  /* Sony VAIO Jog Dial Utility sends WM_MOUSEWHEEL with the posotion
     guessed by active window. Here replace it by last mouse position
     stored as last_mouse_motion_message. There is no side effect,
     maybe.. 2001/03/16 K. Horiguchi */

  pt.x = (signed short) LOWORD (last_mouse_motion_message.lParam);
  pt.y = (signed short) HIWORD (last_mouse_motion_message.lParam);

/*
  pt.x = (signed short) LOWORD (msg->lParam);
  pt.y = (signed short) HIWORD (msg->lParam);
  ScreenToClient (msg->hwnd, &pt);
*/

  emacs_event->kind = w32_mouse_wheel;
  emacs_event->code = (signed short) HIWORD (msg->wParam);
  emacs_event->modifiers = MW32GETMODIFIER (dpyinfo);
  XSETINT (emacs_event->x, pt.x);
  XSETINT (emacs_event->y, pt.y);
  XSETFRAME (emacs_event->frame_or_window, frame);
  emacs_event->timestamp = msg->time;

  return 1;
}
#endif

/* We must construct menu structure, but only main thread
   can do it.  We store event menu_bar_activate_event to
   request main thread to reconstruct menu sturucture.
   Then, this thread must wait for the finish, but
   if main thread is busy, it cannot deal with the request.
   Nevertheless, we caputre any event mainly in order
   to store keyboard quit.
   Thus, I set timeout to 500 millisecond. */
void
mw32_menu_bar_store_activate_event (struct frame *f)
{
  extern Lisp_Object Vmenu_updating_frame;
  int result;

  struct input_event emacs_event;
  HANDLE ev = f->output_data.mw32->mainthread_to_frame_handle;

  EVENT_INIT (emacs_event);
  MW32_INIT_EMACS_EVENT (emacs_event);
  emacs_event.kind = menu_bar_activate_event;
  XSETFRAME (emacs_event.frame_or_window, f);
  emacs_event.timestamp = GetTickCount ();

  ResetEvent (ev);
  kbd_buffer_store_event (&emacs_event);
  SetEvent (keyboard_handle);

  message_loop_blocked_p = 1;
  result = WaitForSingleObject (ev, 500);
  /* main thread have already entered updating phase.
     Freeze until the finish! */
  if (!NILP (Vmenu_updating_frame))
    result = WaitForSingleObject (ev, 1000);
  message_loop_blocked_p = 0;

#if 0
  if (result == WAIT_OBJECT_0)
    fprintf (stderr, "Success!!\n");
  else if (result == WAIT_TIMEOUT)
    fprintf (stderr, "Timeout!!\n");
  else
    fprintf (stderr, "Unknown:%d!!\n", result);
#endif

  lock_mouse_cursor_visible (TRUE);
  DrawMenuBar (FRAME_MW32_WINDOW (f));

  f->output_data.mw32->disable_reconstruct_menubar = 1;

  return;
}

/************************************************************************
            Message processing  Section 2 (message loop)
 ************************************************************************/

/* Deal with mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  EVENT_TYPE is either ButtionPress
   or ButtonRelase.  */

static int
mw32_process_tool_bar_click (FRAME_PTR f, MSG *pmsg, int modifiers)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int hpos, vpos, prop_idx;
  struct glyph *glyph;
  Lisp_Object enabled_p;
  int x = GET_X_LPARAM (pmsg->lParam);
  int y = GET_Y_LPARAM (pmsg->lParam);
  
  /* If not on the highlighted tool-bar item, return.  */
  frame_to_window_pixel_xy (w, &x, &y);
  if (mw32_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx) != 0)
    return 0;

  /* If item is disabled, do nothing.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (NILP (enabled_p))
    return 0;
  
  if (pmsg->message == WM_EMACS_TOOL_BAR_DOWN)
    {
      /* Show item in pressed state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_SUNKEN);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_SUNKEN;
      last_tool_bar_item = prop_idx;
      return 0;
    }
  else /* WM_EMACS_TOOL_BAR_UP */
    {
      struct input_event buf;
      Lisp_Object key, frame;

      /* Show item in released state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_RAISED);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_RAISED;

      key = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_KEY);
      XSETFRAME (frame, f);

      EVENT_INIT (buf);
      MW32_INIT_EMACS_EVENT (buf);
      buf.kind = TOOL_BAR_EVENT;
      buf.frame_or_window = frame;
      buf.arg = frame;
      kbd_buffer_store_event (&buf);

      buf.kind = TOOL_BAR_EVENT;
      buf.frame_or_window = frame;
      buf.arg = key;
      buf.modifiers = modifiers;
      kbd_buffer_store_event (&buf);
      last_tool_bar_item = -1;

      return 1;
    }

  return 0;
}

static Lisp_Object
get_face_height (Lisp_Object face)
{
  Lisp_Object height = Qnil;
  extern Lisp_Object QCheight;

  if (!NILP (face) && SYMBOLP (face))
    {
      height = Finternal_get_lisp_face_attribute (face, QCheight, Qnil);
      if (!INTEGERP (height) && !FLOATP (height))
	height = Qnil;
    }
  else if (CONSP (face))
    {
      height = get_face_height (XCAR (face));
      if (NILP (height))
	height = get_face_height (XCDR (face));
    }
  return height;
}

static void
mw32_get_ime_font_property (FRAME_PTR f)
{
  /* set logfont for IME */
  LOGFONT lf = f->output_data.mw32->ime_logfont;
  
  if (lf.lfHeight == 0)
    {
      extern Lisp_Object Qface, QCheight, Qdefault;
      Lisp_Object p, face, height;

      if (!NILP (Feolp ()))
	XSETFASTINT (p, max (Fline_beginning_position (Qnil), PT - 1));
      else
	XSETFASTINT (p, PT);
      face = Fget_text_property (p, Qface, Qnil);

      height = get_face_height (face);

      if (NILP (height))
	height = Finternal_get_lisp_face_attribute (Qdefault, QCheight, Qnil);
      if (INTEGERP (height))
	lf.lfHeight = (int) (- XINT (height)
			     * FRAME_MW32_DISPLAY_INFO (f)->resy / 720);
      else if (FLOATP (height))
	lf.lfHeight = (int) (XFLOATINT (height) * FRAME_LINE_HEIGHT (f));
    }
  if (W32_SELF_INPUT_BLOCKED_P)
    {
      W32_UNBLOCK_INPUT;
      SEND_MSGTHREAD_INFORM_MESSAGE (WM_EMACS_MODIFY_IME_FONT_PROP,
				     (WPARAM) lf.lfHeight , 0);
      W32_BLOCK_INPUT;
    }
  else
    {
      SEND_MSGTHREAD_INFORM_MESSAGE (WM_EMACS_MODIFY_IME_FONT_PROP,
				     (WPARAM) lf.lfHeight , 0);
    }
}

struct mw32_tool_bar_parm
{
  struct frame *f;
  int modifier;
};

int
mw32_process_main_thread_message (MSG *pwait_msg)
{
  MSG msg;

  for (;;)
    {
      if (pwait_msg)
	GetMessage (&msg, NULL, 0, 0);
      else
	if (!PeekMessage (&msg, NULL, 0, 0, PM_REMOVE)) break;

      switch (msg.message)
	{
#if 0
	case WM_EMACS_NOTE_MOUSE_MOVEMENT:
	  break;
#endif
	case WM_EMACS_TOOL_BAR_DOWN:
	case WM_EMACS_TOOL_BAR_UP:
	  {
	    struct mw32_tool_bar_parm *tbparm
	      = (struct mw32_tool_bar_parm *) msg.wParam;
	    mw32_process_tool_bar_click (tbparm->f, &msg, tbparm->modifier);
	    xfree (tbparm);
	    break;
	  }
	case WM_EMACS_GET_IME_FONT_PROP:
	  {
	    FRAME_PTR f = (FRAME_PTR) msg.wParam;
	    mw32_get_ime_font_property (f);
	    break;
	  }
	}
      if (pwait_msg && (pwait_msg->message == msg.message))
	{
	  *pwait_msg = msg;
	  break;
	}
    }

  return 1;
}

/* If you want to add synchronized operation for event,
   you may do in this.
   NOTICE THAT this function may be merged with MW32_read_socket()
   when blocking problem is solved!  */
void
note_sync_event (void)
{
  mw32_process_main_thread_message (NULL);

  if (!(inhibit_window_system || noninteractive)
      && !NILP (last_mouse_motion_frame))
    {
      struct frame *f = XFRAME (last_mouse_motion_frame);

      note_mouse_movement (f, &last_mouse_motion_message);

      /* If the contents of the global variable help_echo
	 has changed, generate a HELP_EVENT.  */
      if (help_echo != previous_help_echo ||
	  (!NILP (help_echo) && !STRINGP (help_echo) && f->mouse_moved))
	{
	  struct input_event buf[2];

	  if (NILP (help_echo))
	    {
	      help_echo_object = help_echo_window = Qnil;
	      help_echo_pos = -1;
	    }
	      
	  any_help_event_p = 1;
	  gen_help_event (buf, 2, help_echo, last_mouse_motion_frame,
			  help_echo_window, help_echo_object,
			  help_echo_pos);
	  kbd_buffer_store_event (&buf[0]);
	  kbd_buffer_store_event (&buf[1]);
	}
      last_mouse_motion_frame = Qnil;
    }
  return;
}

/* Interface for read_avail_input@keyboard.c.
   This function only waits until the message thread
   store events if expected is true. */
int
MW32_read_socket (int sd, struct input_event *bufp,
		  int numchars, int expected)
{
  if (expected)
    {
      ResetEvent (keyboard_handle);
      WaitForSingleObject (keyboard_handle, INFINITE);
    }
  return 0;
}

static void
update_mouse_cursor (struct mw32_display_info *dpyinfo, MSG msg)
{
  if (last_mouse_motion_message.lParam != msg.lParam)
    {
      last_mouse_motion_message = msg;
      last_mouse_movement_time = msg.time;
      if (dpyinfo->mouse_cursor_stat < 0)
	{
	  ShowCursor (TRUE);
	  dpyinfo->mouse_cursor_stat = 0;
	}
    }
  else
    {
      if (XFASTINT (mw32_hide_mouse_timeout) > 0 &&
	  msg.time - last_mouse_motion_message.time
	  > XFASTINT (mw32_hide_mouse_timeout))
	if (dpyinfo->mouse_cursor_stat == 0)
	  {
	    ShowCursor (FALSE);
	    dpyinfo->mouse_cursor_stat = -1;
	  }
    }
}


/* The main MW32 message loop - mw32_message_loop.  */
/* Read messages from the Windows, and process them.
   This routine is executed by the special thread, called message
   thread.  We return as soon as there are no more events
   to be read.

   Emacs Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   EXPECTED is nonzero if the caller knows incoming message
   is available.  */

static int
mw32_message_loop (int sd, struct input_event *bufp,
		   int numchars, int expected, int *leftover)
{
  int count = 0;
  MSG msg;
  struct frame *f;
  static int lastmsgp = 0;
  static MSG lastmsg = {INVALID_HANDLE_VALUE, 0, 0, 0, 0, {0, 0}};
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO (sd);

  *leftover = 0;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
	
  xassert (numchars > 0);

  if (lastmsgp)
    {
      msg = lastmsg;
    }
  while (1)
    {
      int button;
      int up;
      int modifier;

      if (lastmsgp) lastmsgp = 0;
      else if (expected)
	{
	  GetMessage (&msg, NULL, 0, 0);
	  expected = 0;
	}
      else if (!PeekMessage (&msg, NULL, 0, 0, PM_REMOVE)) break;

      SetEvent (keyboard_handle);

      if (msg.hwnd)
	{
	  f = mw32_window_to_frame (dpyinfo, msg.hwnd);
	  if ((!f) && (!IS_EMACS_PRIVATE_MESSAGE (msg.message)))
	    {
	      TranslateMessage (&msg);
	      DispatchMessage (&msg);
	      continue;
	    }
	}
      else
	f = NULL;

      switch (msg.message)
	{
	case WM_KEYDOWN:
	  if (mw32_hide_mouse_on_key
	      && dpyinfo->mouse_cursor_stat == 0)
	    {
	      dpyinfo->mouse_cursor_stat = -1;
	      ShowCursor (FALSE);
	      clear_mouse_face (dpyinfo);
	    }

	case WM_SYSKEYDOWN:

	  if (f && !f->iconified && f->visible)
	    {
	      UINT keycode;
	      int keymod;
	      int keyflag;
	      int ocount = count;

	      keyflag = mw32_emacs_translate_message (dpyinfo, 1, msg.wParam, 
						      &keycode, &keymod);
	      EVENT_INIT (*bufp);
	      MW32_INIT_EMACS_EVENT (*bufp);
	      while (1)
		{
		  switch (keyflag)
		    {
		    case 0:
		      goto dflt_no_translate;
		    case 1:
		      bufp->kind = ascii_keystroke;
		      break;
		    case 2:
		      bufp->kind = non_ascii_keystroke;
		      break;
		    case 3:
		      goto dflt;
		    }
		  bufp->code = keycode;
		  bufp->modifiers = keymod;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp->timestamp = msg.time;
		  bufp++;
		  numchars--;
		  count++;

		  keyflag = mw32_emacs_translate_message (dpyinfo, 0, 0,
							  &keycode, &keymod);
		  if (!keyflag) break;

		  if (numchars == 0)
		    {
		      count = ocount;
		      *leftover = 1; /* no space, quit! */
		      break;
		    }
		}
	    }
	  break;

	case WM_KEYUP:
	case WM_SYSKEYUP:
	  f = mw32_window_to_frame (dpyinfo, msg.hwnd);
	  if (!f) goto dflt;

	    {
	      int keyflag;
	      UINT keycode;
	      int keymod;
	      keyflag = mw32_emacs_translate_message (dpyinfo, 1, msg.wParam,
						      &keycode, &keymod);
	      if (keyflag == 0) goto dflt_no_translate;
	      goto dflt;
	    }
	    break;

	case WM_MULE_IME_STATUS:
	  if (f && !f->iconified && f->visible && numchars > 0)
	    {
	      EVENT_INIT (*bufp);
	      MW32_INIT_EMACS_EVENT (*bufp);
	      bufp->kind = non_ascii_keystroke;
	      bufp->code = VK_KANJI;
	      bufp->modifiers = 0;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->timestamp = msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;
	case WM_MULE_IME_REPORT:
	  {
	    LPTSTR lpStr;
	    struct input_event buf;
	    HANDLE hw32_ime_string = (HANDLE) msg.wParam;

	    if (count != 0)
	      {
		*leftover = 1;
		break;
	      }
	    f = (struct frame *) msg.lParam;
	    if (f && !f->iconified && f->visible)
	      {
		lpStr = GlobalLock (hw32_ime_string);
		while (1)
		  {
		    EVENT_INIT (buf);
		    MW32_INIT_EMACS_EVENT (buf);
		    XSETFRAME (buf.frame_or_window, f);
		    buf.timestamp = msg.time;
		    buf.modifiers = 0;
		    if (*lpStr)
		      {
			buf.kind = ascii_keystroke;
			buf.code = *lpStr;
			kbd_buffer_store_event (&buf);
			lpStr++;
		      }
		    else
		      {
			buf.kind = non_ascii_keystroke;
			buf.code = VK_COMPEND;
			kbd_buffer_store_event (&buf);
			break;
		      }
		  }
		GlobalUnlock (hw32_ime_string);
		GlobalFree (hw32_ime_string);
	      }
	  }
	  break;

	case WM_MOUSEMOVE:
	  /* If the mouse has just moved into the frame, start tracking
	     it, so we will be notified when it leaves the frame.  Mouse
	     tracking only works under W98 and NT4 and later. On earlier
	     versions, there is no way of telling when the mouse leaves the
	     frame, so we just have to put up with help-echo and mouse
	     highlighting remaining while the frame is not active.  */
	  if (track_mouse_event_fn && !track_mouse_window)
	    {
	      TRACKMOUSEEVENT tme;
	      tme.cbSize = sizeof (tme);
	      tme.dwFlags = TME_LEAVE;
	      tme.hwndTrack = msg.hwnd;

	      track_mouse_event_fn (&tme);
	      track_mouse_window = msg.hwnd;
	    }

	  /* Ignore non-movement.  */
	  {
	    int x = LOWORD (msg.lParam);
	    int y = HIWORD (msg.lParam);
	    if (x == last_mousemove_x && y == last_mousemove_y)
	      break;
	    last_mousemove_x = x;
	    last_mousemove_y = y;
	  }

	  previous_help_echo = help_echo;

	  if (last_mouse_frame
	      && FRAME_LIVE_P (last_mouse_frame)
	      && (dpyinfo->grabbed || !f))
	    f = last_mouse_frame;

	  update_mouse_cursor (dpyinfo, msg);
	  if (f)
	    {
	      XSETFRAME (last_mouse_motion_frame, f);
	      dpyinfo->mouse_face_mouse_frame = f;
	    }
	  else
	    clear_mouse_face (dpyinfo);

	  goto dflt;
	  
	case WM_NCMOUSEMOVE:
	  clear_mouse_face (dpyinfo);
	  dpyinfo->mouse_face_mouse_frame = NULL;
	  last_mouse_motion_frame = Qnil;

	  /* Generate a nil HELP_EVENT to cancel a help-echo.
	     Do it only if there's something to cancel.
	     Otherwise, the startup message is cleared when
	     the mouse leaves the frame.  */
	  if (msg.wParam && any_help_event_p)
	    {
	      Lisp_Object frame;
	      int n;

	      XSETFRAME (frame, f);
	      help_echo = Qnil;
	      n = gen_help_event (bufp, numchars,
				  Qnil, frame, Qnil, Qnil, 0);
	      bufp += n, count += n, numchars -= n;
	      any_help_event_p = 0;
	    }
	  update_mouse_cursor (dpyinfo, msg);
	  goto dflt;

	case WM_MOUSELEAVE:
	  if (f)
	    {
	      if (f == dpyinfo->mouse_face_mouse_frame)
		{
		  /* If we move outside the frame, then we're
		     certainly no longer on any text in the frame.  */
		  clear_mouse_face (dpyinfo);
		  dpyinfo->mouse_face_mouse_frame = 0;
		  last_mouse_motion_frame = Qnil;
		}

	      /* Generate a nil HELP_EVENT to cancel a help-echo.
		 Do it only if there's something to cancel.
		 Otherwise, the startup message is cleared when
		 the mouse leaves the frame.  */
	      if (any_help_event_p)
		{
		  Lisp_Object frame;
		  int n;

		  XSETFRAME (frame, f);
		  help_echo = Qnil;
		  n = gen_help_event (bufp, numchars,
				      Qnil, frame, Qnil, Qnil, 0);
		  bufp += n, count += n, numchars -= n;
		  any_help_event_p = 0;
		}
	    }
	  track_mouse_window = NULL;
	  goto dflt;

	case WM_LBUTTONDOWN:
	case WM_MBUTTONDOWN:
	case WM_RBUTTONDOWN:
	  if (dpyinfo->mouse_cursor_stat < 0)
	    ShowCursor (TRUE);
	  dpyinfo->mouse_cursor_stat = 2;

	  /* fall through */
	case WM_LBUTTONUP:
	case WM_MBUTTONUP:
	case WM_RBUTTONUP:
	  if (dpyinfo->mouse_cursor_stat > 0)
	    dpyinfo->mouse_cursor_stat--; /* 2 -> 1,  1 -> 0 */

	  mw32_mouse_button_cc (dpyinfo, msg.message, &button, &up, &modifier);
	  {
	    /* If we decide we want to generate an event to be seen
	       by the rest of Emacs, we put it here.  */
	    int tool_bar_p = 0;

	    f = mw32_window_to_frame (dpyinfo, msg.hwnd);

	    if (f && WINDOWP (f->tool_bar_window)
		&& XFASTINT (XWINDOW (f->tool_bar_window)->height))
	      {
		Lisp_Object window;
		int p;
		window = window_from_coordinates (f, LOWORD (msg.lParam),
						  HIWORD (msg.lParam), &p, 1);
		if (EQ (window, f->tool_bar_window))
		  {
		    struct mw32_tool_bar_parm *tbparm
		      = ((struct mw32_tool_bar_parm *)
			 xmalloc (sizeof (struct mw32_tool_bar_parm)));

		    tbparm->f = f;
		    tbparm->modifier = MW32GETMODIFIER (dpyinfo);
		    tool_bar_p = 1;
		    if (msg.message == WM_LBUTTONUP)
		      {
			POST_THREAD_INFORM_MESSAGE (main_thread_id,
						    WM_EMACS_TOOL_BAR_UP,
						    (WPARAM) tbparm,
						    msg.lParam);
		      }
		    else if (msg.message == WM_LBUTTONDOWN)
		      {
			POST_THREAD_INFORM_MESSAGE (main_thread_id,
						    WM_EMACS_TOOL_BAR_DOWN,
						    (WPARAM) tbparm,
						    msg.lParam);
		      }
		  }
	      }
	    if (!tool_bar_p && f == dpyinfo->mw32_focus_frame)
	      {
		if (numchars >= 1)
		  {
		    EVENT_INIT (*bufp);
		    MW32_INIT_EMACS_EVENT (*bufp);
		    bufp->kind = mouse_click;
		    bufp->code = button;
		    bufp->timestamp = msg.time;
		    bufp->modifiers = modifier;
		    XSETINT (bufp->x, LOWORD (msg.lParam));
		    XSETINT (bufp->y, HIWORD (msg.lParam));
		    XSETFRAME (bufp->frame_or_window, f);
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
		
	    if (up == 1)
	      {
		dpyinfo->grabbed &= ~(1 << button);
		if (!dpyinfo->grabbed) {
		  ReleaseCapture ();
		}
	      }
	    else if (up == 0)
	      {
		dpyinfo->grabbed |= (1 << button);
		last_mouse_frame = f;
		SetCapture (msg.hwnd);
	      }
	    else
	      break;
	  }
	  goto dflt;

	case WM_DROPFILES:
         {
           if (f && !f->iconified && f->visible &&
	       (mw32_drop_file_handler (f, &msg, bufp)))
	     {
	       bufp++;
	       count++;
	       numchars--;
	     }
         }
	 break;

#ifdef W32_INTELLIMOUSE
	case WM_MOUSEWHEEL:
          {
            if (f && !f->iconified && f->visible &&
	        (mw32_mouse_wheel_handler (f, &msg, bufp)))
	      {
		bufp++;
		count++;
		numchars--;
	      }
          }
          break;
#endif
	case MM_MCINOTIFY:
	  bufp->kind = mw32_mci_event;
	  XSETFRAME (bufp->frame_or_window, f);
	  switch (msg.wParam)
	    {
	    case MCI_NOTIFY_ABORTED:
	      bufp->code = MW32_MCI_NOTIFY_ABORTED;
	      break;
	    case MCI_NOTIFY_FAILURE:
	      bufp->code = MW32_MCI_NOTIFY_FAILURE;
	      break;
	    case MCI_NOTIFY_SUCCESSFUL:
	      bufp->code = MW32_MCI_NOTIFY_SUCCESSFUL;
	      break;
	    case MCI_NOTIFY_SUPERSEDED:
	      bufp->code = MW32_MCI_NOTIFY_SUPERSEDED;
	      break;
	    default:
	      abort ();
	    }
	  bufp->arg = make_number (msg.lParam);
	  bufp->timestamp = msg.time;
	  bufp++;
	  count++;
	  numchars--;
	  break;

	  /*
	    WM_EMACS_CLEAR_MOUSE_FACE message is used for clearing
	    mouse face and help echo.  But when the WPARAM is zero,
	    don't clear help echo.  This feature is used by WM_NCMOUSEMOVE,
	    because non-client area controls should not interfere
	    the help echo message.
	  */
	case WM_EMACS_CLEAR_MOUSE_FACE:
	  clear_mouse_face (dpyinfo);
	  /* Generate a nil HELP_EVENT to cancel a help-echo.
	     Do it only if there's something to cancel.
	     Otherwise, the startup message is cleared when
	     the mouse leaves the frame.  */
	  if (msg.wParam && any_help_event_p)
	    {
	      Lisp_Object frame;
	      int n;

	      XSETFRAME (frame, f);
	      help_echo = Qnil;
	      n = gen_help_event (bufp, numchars,
				  Qnil, frame, Qnil, Qnil, 0);
	      bufp += n, count += n, numchars -= n;
	      any_help_event_p = 0;
	    }
	  break;
 	  
	case WM_EMACS_DESTROY:
	  if (numchars <= 0)
	    abort ();

	  EVENT_INIT (*bufp);
	  MW32_INIT_EMACS_EVENT (*bufp);
	  bufp->kind = delete_window_event;
	  XSETFRAME (bufp->frame_or_window, f);
	  bufp++;
	  count++;
	  numchars--;
	  break;

	case WM_EMACS_CLOSE_CONNECTION:
	  ExitThread (1);

	case WM_EMACS_CREATE_FRAME:
	  mw32m_create_frame_window ((struct frame*)msg.wParam,
				     (LPSTR)msg.lParam);
	  break;
#ifdef IME_CONTROL
	case WM_MULE_IME_CREATE_AGENT:
	  mw32m_ime_create_agent ();
	  break;
#endif

	case WM_EMACS_ACTIVATE:
	  if (f && numchars > 0)
	    {
	      EVENT_INIT (*bufp);
	      MW32_INIT_EMACS_EVENT (*bufp);
	      bufp->kind = meadow_private_event;
	      bufp->code = 0;
	      bufp->modifiers = 0;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->timestamp = msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;

	case WM_COMMAND:
	  f = mw32_window_to_frame (dpyinfo, msg.hwnd);
	  if ((msg.lParam == 0) && (HIWORD (msg.wParam) == 0))
	    {
	      /* Came from window menu */

	      if (count != 0)
		{
		  *leftover = 1;
		  break;
		}
	      menubar_selection_callback (msg.hwnd,
					  LOWORD (msg.wParam));
	    }
	  break;

	default:
	dflt:

#ifdef W32_INTELLIMOUSE
	    if (msg.message == mw32_wheel_message)
	      {
		WORD wp;

		wp = LOWORD (msg.wParam);
		msg.wParam = MAKELONG (0, wp);
		if (f && !f->iconified && f->visible &&
		    (mw32_mouse_wheel_handler (f, &msg, bufp))){
		  bufp++;
		  count++;
		  numchars--;
		}
		break;
	      }
#endif

	  TranslateMessage (&msg);
	  DispatchMessage (&msg);

	dflt_no_translate:	  
	  continue;
	}
      if (*leftover)
	{
	  lastmsg = msg;
	  lastmsgp = 1;
	  break;
	}
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  if (pending_autoraise_frame)
    {
      mw32_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  return count;
}

/* entry routine for the message thread. */
#ifndef KBD_BUFFER_SIZE
#define KBD_BUFFER_SIZE 8192
#endif
static DWORD WINAPI
mw32_async_handle_message (void *params)
{
  extern int quit_char;
  struct input_event buf[KBD_BUFFER_SIZE];
  int i, nread, leftover = 0;

  while (1)
    {
      if (!leftover)
	WaitMessage ();
      nread = mw32_message_loop (0, buf, KBD_BUFFER_SIZE, 0, &leftover);
      /* Scan the chars for C-g and store them in kbd_buffer.  */
      for (i = 0; i < nread; i++)
	{
	  kbd_buffer_store_event (&buf[i]);
	  /* Don't look at input that follows a C-g too closely.
	     This reduces lossage due to autorepeat on C-g.  */
	  if (buf[i].kind == ascii_keystroke
	      && buf[i].code == quit_char)
	    {
	      break;
	    }
	}
      SetEvent (keyboard_handle);
    }

  return 1;
}


/***********************************************************************
			     Text Cursor
 ***********************************************************************/

static void
anticipate_overwrite_caret (struct window *w)
{
  struct frame *f = XFRAME (w->frame);

  if (updated_area == TEXT_AREA
      && output_cursor.vpos == w->phys_cursor.vpos
      && output_cursor.x <= w->phys_cursor.x
      && CARET_CURSOR_P (w->phys_cursor_type)
      && MW32_FRAME_CARET_SHOWN (f))
    mw32_set_caret (f, HIDDEN_CARET);
}

static void
restore_overwritten_caret (struct frame *f)
{
  if (CARET_CURSOR_P (XWINDOW (f->selected_window)->phys_cursor_type) &&
      ! MW32_FRAME_CARET_SHOWN (f))
    mw32_set_caret (f, SHOWN_CARET);
}


/* Notice if the text cursor of window W has been overwritten by a
   drawing operation that outputs N glyphs starting at HPOS in the
   line given by output_cursor.vpos.

   N < 0 means all the rest of the line after HPOS has been
   written.  */

static void
notice_overwritten_cursor (struct window *w, int hpos, int n)
{
  if (updated_area == TEXT_AREA
      && output_cursor.vpos == w->phys_cursor.vpos
      && output_cursor.x <= w->phys_cursor.x
      && w->phys_cursor_on_p)
    {
      if (n < 0)
	w->phys_cursor_on_p = 0;
      else
	{
	  /* It depends on the width of the N glyphs written at HPOS
	     if the cursor has been overwritten or not.  */
	  struct glyph *glyph = &updated_row->glyphs[TEXT_AREA][hpos];
	  struct glyph *end = glyph + n;
	  int width = 0;

	  for (; glyph < end; ++glyph)
	    width += glyph->pixel_width;

	  if (output_cursor.x + width > w->phys_cursor.x)
	    w->phys_cursor_on_p = 0;
	}
    }
}


/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.
   WHOLE_LINE_P non-zero means include the areas used for truncation
   mark display and alike in the clipping rectangle.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
mw32_clip_to_row (HDC hdc, struct window *w,
		  struct glyph_row *row,
		  int whole_line_p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  int window_x, window_y, window_width, window_height;
  RECT clip_rect;
  HRGN hcr;

  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);

  clip_rect.left = WINDOW_TO_FRAME_PIXEL_X (w, 0);
  clip_rect.top = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  clip_rect.top = max (clip_rect.top, window_y);
  clip_rect.right = clip_rect.left + window_width;
  clip_rect.bottom = clip_rect.top + row->visible_height;

  /* If clipping to the whole line, including trunc marks, extend
     the rectangle to the left and increase its width.  */
  if (whole_line_p)
    {
      clip_rect.left -= FRAME_MW32_LEFT_FLAGS_AREA_WIDTH (f);
      clip_rect.right += FRAME_MW32_FLAGS_AREA_WIDTH (f);
    }
  hcr = CreateRectRgnIndirect (&clip_rect);
  SelectClipRgn (hdc, hcr);
  DeleteObject (hcr);
}

static int
mw32_compute_cursor_width (struct frame *f,
			   struct glyph *cursor_glyph)
{
  int wd;

  /* Compute the width of the rectangle to draw.  If on a stretch
     glyph, and `x-stretch-block-cursor' is nil, don't draw a
     rectangle as wide as the glyph, but use a canonical character
     width instead.  */
  wd = cursor_glyph->pixel_width;
  if (cursor_glyph->type == STRETCH_GLYPH
      && !mw32_stretch_cursor_p)
    wd = min (CANON_X_UNIT (f), wd);

  return wd;
}

/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
mw32_draw_hollow_cursor (struct window *w,
			 struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int x, y, wd, h;
  struct glyph *cursor_glyph;
  PIX_TYPE fg;

  /* Compute frame-relative coordinates from window-relative
     coordinates.  */
  x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
  y = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
       + row->ascent - w->phys_cursor_ascent);
  h = row->height - 1;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;
  wd = mw32_compute_cursor_width (f, cursor_glyph);
  
  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  fg = f->output_data.mw32->cursor_pixel;

  /* Set clipping, draw the rectangle, and reset clipping again.  */
  {
    HPEN hp;
    HBRUSH hb;
    LOGBRUSH logpenbrush;
    HDC hdc = FRAME_HDC (f);

    logpenbrush.lbStyle = BS_SOLID;
    logpenbrush.lbColor = fg;
    hp = ExtCreatePen (PS_COSMETIC | PS_SOLID, 1, &logpenbrush,
		       0, NULL);
    hb = GetStockObject (NULL_BRUSH);
    SaveDC (hdc);
    SelectObject (hdc, hp);
    SelectObject (hdc, hb);
    mw32_clip_to_row (hdc, w, row, 0);
    Rectangle (hdc, x, y, x + wd, y + h);
    RestoreDC (hdc, -1);
    DeleteObject (hp);
  }
}

static void
mw32_draw_caret_cursor (struct frame *f,
			struct window *w,
			struct glyph_row *row)
{
  if (w->phys_cursor_type == CHECKERED_CARET_CURSOR)
    MW32_FRAME_CARET_BITMAP (f) = (HBITMAP) 1;
  else
    MW32_FRAME_CARET_BITMAP (f) = (HBITMAP) 0;

  if (w->phys_cursor_type == HAIRLINE_CARET_CURSOR)
    MW32_FRAME_CARET_WIDTH (f) = 1;
  else
    {
      struct glyph *cursor_glyph = get_phys_cursor_glyph (w);

      if (cursor_glyph == NULL)
	return;

      MW32_FRAME_CARET_WIDTH (f) = mw32_compute_cursor_width (f, cursor_glyph);
    }
  w->cursor_off_p = 0;

  mw32_set_caret (f, SHOWN_CARET);
}

/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
mw32_draw_bar_cursor (struct window *w,
		      struct glyph_row *row,
		      int width)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;
  unsigned long mask;
      
  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *row;
      row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      mw32_draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
    }
  else
    {
      int x, y;
      HPEN hp;
      LOGBRUSH logpenbrush;
      HDC hdc = FRAME_HDC (f);
      PIX_TYPE pix = f->output_data.mw32->cursor_pixel;

      SaveDC (hdc);

      if (width < 0)
	width = MW32_FRAME_CARET_WIDTH (f);
      width = min (cursor_glyph->pixel_width, width);
      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
      y = WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y);
      x += width / 2;

      logpenbrush.lbStyle = BS_SOLID;
      logpenbrush.lbColor = pix;
      hp = ExtCreatePen (PS_COSMETIC | PS_SOLID | PS_ENDCAP_FLAT, width,
			 &logpenbrush, 0, NULL);

      mw32_clip_to_row (hdc, w, row, 0);
      MoveToEx (hdc, x, y, NULL);
      LineTo (hdc, x, y + row->height);

      RestoreDC (hdc, -1);
      DeleteObject (hp);
    }
}


/* Clear the cursor of window W to background color, and mark the
   cursor as not shown.  This is used when the text where the cursor
   is is about to be rewritten.  */

static void
mw32_clear_cursor (struct window *w)
{
  if (FRAME_VISIBLE_P (XFRAME (w->frame)) && w->phys_cursor_on_p)
    mw32_update_window_cursor (w, 0);
}


/* Draw the cursor glyph of window W in glyph row ROW.  See the
   comment of mw32_draw_glyphs for the meaning of HL.  */

static void
mw32_draw_phys_cursor_glyph (struct window *w,
			     struct glyph_row *row,
			     enum draw_glyphs_face hl)
{
  /* If cursor hpos is out of bounds, don't draw garbage.  This can
     happen in mini-buffer windows when switching between echo area
     glyphs and mini-buffer.  */
  if (w->phys_cursor.hpos < row->used[TEXT_AREA])
    {
      int on_p = w->phys_cursor_on_p;
      
      if (CARET_CURSOR_P (w->phys_cursor_type))
	{
	  struct frame *f = XFRAME (w->frame);
	  mw32_draw_caret_cursor (f, w, row);
	}
      else
	{
	  mw32_draw_glyphs (w, w->phys_cursor.x, row, TEXT_AREA,
			    w->phys_cursor.hpos, w->phys_cursor.hpos + 1,
			    hl, 0, 0, 0);
	}
      w->phys_cursor_on_p = on_p;

      /* When we erase the cursor, and ROW is overlapped by other
	 rows, make sure that these overlapping parts of other rows
	 are redrawn.  */
      if (hl == DRAW_NORMAL_TEXT && row->overlapped_p)
	{
	  if (row > w->current_matrix->rows
	      && MATRIX_ROW_OVERLAPS_SUCC_P (row - 1))
	    mw32_fix_overlapping_area (w, row - 1, TEXT_AREA);

	  if (MATRIX_ROW_BOTTOM_Y (row) < window_text_bottom_y (w)
	      && MATRIX_ROW_OVERLAPS_PRED_P (row + 1))
	    mw32_fix_overlapping_area (w, row + 1, TEXT_AREA);
	}
    }
}


/* Erase the image of a cursor of window W from the screen.  */

static void
mw32_erase_phys_cursor (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int hpos = w->phys_cursor.hpos;
  int vpos = w->phys_cursor.vpos;
  int mouse_face_here_p = 0;
  struct glyph_matrix *active_glyphs = w->current_matrix;
  struct glyph_row *cursor_row;
  struct glyph *cursor_glyph;
  enum draw_glyphs_face hl;

  /* No cursor displayed or row invalidated => nothing to do on the
     screen.  */
  if (w->phys_cursor_type == NO_CURSOR)
    goto mark_cursor_off;

  /* If cursor is caret, hide it. */
  if (CARET_CURSOR_P (w->phys_cursor_type))
    {
      mw32_set_caret (f, HIDDEN_CARET);
      goto mark_cursor_off;
    }

  /* VPOS >= active_glyphs->nrows means that window has been resized.
     Don't bother to erase the cursor.  */
  if (vpos >= active_glyphs->nrows)
    goto mark_cursor_off;

  /* If row containing cursor is marked invalid, there is nothing we
     can do.  */
  cursor_row = MATRIX_ROW (active_glyphs, vpos);
  if (!cursor_row->enabled_p)
    goto mark_cursor_off;
  
  /* If row is completely invisible, don't attempt to delete a cursor which
     isn't there.  This may happen if cursor is at top of window, and
     we switch to a buffer with a header line in that window.  */
  if (cursor_row->visible_height <= 0)
    goto mark_cursor_off;

  /* This can happen when the new row is shorter than the old one.
     In this case, either x_draw_glyphs or clear_end_of_line
     should have cleared the cursor.  Note that we wouldn't be
     able to erase the cursor in this case because we don't have a
     cursor glyph at hand.  */
  if (w->phys_cursor.hpos >= cursor_row->used[TEXT_AREA])
    goto mark_cursor_off;
	 
  /* If the cursor is in the mouse face area, redisplay that when
     we clear the cursor.  */
  if (! NILP (dpyinfo->mouse_face_window)
      && w == XWINDOW (dpyinfo->mouse_face_window)
      && (vpos > dpyinfo->mouse_face_beg_row
	  || (vpos == dpyinfo->mouse_face_beg_row
	      && hpos >= dpyinfo->mouse_face_beg_col))
      && (vpos < dpyinfo->mouse_face_end_row
	  || (vpos == dpyinfo->mouse_face_end_row
	      && hpos < dpyinfo->mouse_face_end_col))
      /* Don't redraw the cursor's spot in mouse face if it is at the
	 end of a line (on a newline).  The cursor appears there, but
	 mouse highlighting does not.  */
      && cursor_row->used[TEXT_AREA] > hpos)
    mouse_face_here_p = 1;

  /* Maybe clear the display under the cursor.  */
  if (w->phys_cursor_type == HOLLOW_BOX_CURSOR)
    {
      int x, y;
      int header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);

      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	goto mark_cursor_off;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
      y = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
					   cursor_row->y));
      
      mw32_clear_area (f, x, y,
		       x + cursor_glyph->pixel_width,
		       y + cursor_row->visible_height);
    }
  
  /* Erase the cursor by redrawing the character underneath it.  */
  if (mouse_face_here_p)
    hl = DRAW_MOUSE_FACE;
  else if (cursor_row->inverse_p)
    hl = DRAW_INVERSE_VIDEO;
  else
    hl = DRAW_NORMAL_TEXT;
  mw32_draw_phys_cursor_glyph (w, cursor_row, hl);

 mark_cursor_off:
  w->phys_cursor_on_p = 0;
  w->phys_cursor_type = NO_CURSOR;
}


/* Non-zero if physical cursor of window W is within mouse face.  */

static int
cursor_in_mouse_face_p (struct window *w)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (XFRAME (w->frame));
  int in_mouse_face = 0;
  
  if (WINDOWP (dpyinfo->mouse_face_window)
      && XWINDOW (dpyinfo->mouse_face_window) == w)
    {
      int hpos = w->phys_cursor.hpos;
      int vpos = w->phys_cursor.vpos;

      if (vpos >= dpyinfo->mouse_face_beg_row
	  && vpos <= dpyinfo->mouse_face_end_row
	  && (vpos > dpyinfo->mouse_face_beg_row
	      || hpos >= dpyinfo->mouse_face_beg_col)
	  && (vpos < dpyinfo->mouse_face_end_row
	      || hpos < dpyinfo->mouse_face_end_col
	      || dpyinfo->mouse_face_past_end))
	in_mouse_face = 1;
    }

  return in_mouse_face;
}


/* Display or clear cursor of window W.  If ON is zero, clear the
   cursor.  If it is non-zero, display the cursor.  If ON is nonzero,
   where to put the cursor is specified by HPOS, VPOS, X and Y.  */

void
mw32_display_and_set_cursor (struct window *w,
			     int on, int hpos, int vpos,
			     int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  int new_cursor_type;
  int new_cursor_width;
  struct glyph_matrix *current_glyphs;
  struct glyph_row *glyph_row;
  struct glyph *glyph;

  /* This is pointless on invisible frames, and dangerous on garbaged
     windows and frames; in the latter case, the frame or window may
     be in the midst of changing its size, and x and y may be off the
     window.  */
  if (! FRAME_VISIBLE_P (f)
      || FRAME_GARBAGED_P (f)
      || vpos >= w->current_matrix->nrows
      || hpos >= w->current_matrix->matrix_w)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && !w->phys_cursor_on_p)
    return;

  current_glyphs = w->current_matrix;
  glyph_row = MATRIX_ROW (current_glyphs, vpos);
  glyph = glyph_row->glyphs[TEXT_AREA] + hpos;
  
  /* If cursor row is not enabled, we don't really know where to 
     display the cursor.  */
  if (!glyph_row->enabled_p)
    {
      w->phys_cursor_on_p = 0;
      return;
    }

  //xassert (interrupt_input_blocked);

  /* Set new_cursor_type to the cursor we want to be displayed.  In a
     mini-buffer window, we want the cursor only to appear if we are
     reading input from this window.  For the selected window, we want
     the cursor type given by the frame parameter.  If explicitly
     marked off, draw no cursor.  In all other cases, we want a hollow
     box cursor.  */
  new_cursor_width = -1;
  if (cursor_in_echo_area
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
    {
      if (w == XWINDOW (echo_area_window))
	new_cursor_type = FRAME_DESIRED_CURSOR (f);
      else
	new_cursor_type = HOLLOW_BOX_CURSOR;
    }
  else
    {
      if (f != FRAME_MW32_DISPLAY_INFO (f)->mw32_focus_frame
	  || w != XWINDOW (f->selected_window))
	{
	  extern int cursor_in_non_selected_windows;
	  
	  if (MINI_WINDOW_P (w)
	      || !cursor_in_non_selected_windows
	      || NILP (XBUFFER (w->buffer)->cursor_type))
	    new_cursor_type = NO_CURSOR;
	  else
	    new_cursor_type = HOLLOW_BOX_CURSOR;
	}
      else if (w->cursor_off_p && ! CARET_CURSOR_P (w->phys_cursor_type))
	new_cursor_type = NO_CURSOR;
      else
	{
	  struct buffer *b = XBUFFER (w->buffer);

	  if (EQ (b->cursor_type, Qt))
	    new_cursor_type = FRAME_DESIRED_CURSOR (f);
	  else
	    new_cursor_type = mw32_specified_cursor_type (b->cursor_type, 
							  &new_cursor_width);
	}
    }

  /* Disable system caret cursor when switch from system caret to
     non-system one. */
  if (CARET_CURSOR_P (w->phys_cursor_type)
      && !CARET_CURSOR_P (new_cursor_type))
    mw32_set_caret (f, HIDDEN_CARET);

  /* If cursor is currently being shown and we don't want it to be or
     it is in the wrong place, or the cursor type is not what we want,
     erase it.  */
  if (w->phys_cursor_on_p
      && (!on
	  || w->phys_cursor.x != x
	  || w->phys_cursor.y != y
	  || new_cursor_type != w->phys_cursor_type))
    mw32_erase_phys_cursor (w);

  /* Don't check phys_cursor_on_p here because that flag is only set
     to zero in some cases where we know that the cursor has been
     completely erased, to avoid the extra work of erasing the cursor
     twice.  In other words, phys_cursor_on_p can be 1 and the cursor
     still not be visible, or it has only been partly erased.  */
  if (on && FRAME_HDC_VALID_P (XFRAME (WINDOW_FRAME (w))))
    {
      w->phys_cursor_ascent = glyph_row->ascent;
      w->phys_cursor_height = glyph_row->height;
      
      /* Set phys_cursor_.* before x_draw_.* is called because some
	 of them may need the information.  */
      w->phys_cursor.x = x;
      w->phys_cursor.y = glyph_row->y;
      w->phys_cursor.hpos = hpos;
      w->phys_cursor.vpos = vpos;
      w->phys_cursor_type = new_cursor_type;
      w->phys_cursor_on_p = 1;

      switch (new_cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  mw32_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  mw32_draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  mw32_draw_bar_cursor (w, glyph_row, new_cursor_width);
	  break;

	case NO_CURSOR:
	  break;

	case HAIRLINE_CARET_CURSOR:
	case CHECKERED_CARET_CURSOR:
	case CARET_CURSOR:
	  mw32_draw_caret_cursor (f, w, glyph_row);
	  break;

	default:
	  abort ();
	}
    }

#ifdef IME_CONTROL
  if (on
      && f->output_data.mw32->ime_composition_state
      && (XWINDOW (selected_window) == w)
      && (FRAME_MW32_DISPLAY_INFO (f)->mw32_highlight_frame == f))
    {
      /* Maybe, we're in critsec, so use POST_INFORM_MESSAGE.  */
      PostMessage (FRAME_MW32_WINDOW (f),
		   WM_MULE_IMM_SET_CONVERSION_WINDOW,
		   (WPARAM) WINDOW_TEXT_TO_FRAME_PIXEL_X (w, x),
		   (LPARAM) WINDOW_TO_FRAME_PIXEL_Y (w, y));
    }
#endif
}


/* Display the cursor on window W, or clear it.  X and Y are window
   relative pixel coordinates.  HPOS and VPOS are glyph matrix
   positions.  If W is not the selected window, display a hollow
   cursor.  ON non-zero means display the cursor at X, Y which
   correspond to HPOS, VPOS, otherwise it is cleared.  */

static void
mw32_display_cursor (struct window *w,
		     int on, int hpos, int vpos, int x, int y)
{
  BLOCK_INPUT;
  mw32_display_and_set_cursor (w, on, hpos, vpos, x, y);
  UNBLOCK_INPUT;
}


/* Display the cursor on window W, or clear it, according to ON_P.
   Don't change the cursor's position.  */

void
mw32_update_cursor (FRAME_PTR f, int on_p)
{
  /* Main thread maybe has already destroyed root_window. */
  if (WINDOWP (f->root_window))
    mw32_update_cursor_in_window_tree (XWINDOW (f->root_window), on_p);
}


/* Call x_update_window_cursor with parameter ON_P on all leaf windows
   in the window tree rooted at W.  */

static void
mw32_update_cursor_in_window_tree (struct window *w, int on_p)
{
  while (w)
    {
      if (!NILP (w->hchild))
	mw32_update_cursor_in_window_tree (XWINDOW (w->hchild), on_p);
      else if (!NILP (w->vchild))
	mw32_update_cursor_in_window_tree (XWINDOW (w->vchild), on_p);
      else
	mw32_update_window_cursor (w, on_p);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Switch the display of W's cursor on or off, according to the value
   of ON.  */

static void
mw32_update_window_cursor (struct window *w,
			   int on)
{
  /* Don't update cursor in windows whose frame is in the process
     of being deleted.  */
  if (w->current_matrix)
    {
      BLOCK_INPUT;
      mw32_display_and_set_cursor (w, on, w->phys_cursor.hpos, w->phys_cursor.vpos,
				   w->phys_cursor.x, w->phys_cursor.y);
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
			    Frame/Window control
 ***********************************************************************/

/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

void
mw32_calc_absolute_position (struct frame *f)
{
#if 0
  Window child;
  int win_x = 0, win_y = 0;
  int flags = f->output_data.x->size_hint_flags;
  int this_window;

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (! ((flags & XNegative) || (flags & YNegative)))
    return;

#ifdef USE_X_TOOLKIT
  this_window = XtWindow (f->output_data.x->widget);
#else
  this_window = FRAME_X_WINDOW (f);
#endif

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.
     But do this only if we will need the results.  */
  if (f->output_data.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
    {
      int count;

      BLOCK_INPUT;
      count = x_catch_errors (FRAME_X_DISPLAY (f));
      while (1)
	{
	  x_clear_errors (FRAME_X_DISPLAY (f));
	  XTranslateCoordinates (FRAME_X_DISPLAY (f),

				 /* From-window, to-window.  */
				 this_window,
				 f->output_data.x->parent_desc,

				 /* From-position, to-position.  */
				 0, 0, &win_x, &win_y,

				 /* Child of win.  */
				 &child);
	  if (x_had_errors_p (FRAME_X_DISPLAY (f)))
	    {
	      Window newroot, newparent = 0xdeadbeef;
	      Window *newchildren;
	      unsigned int nchildren;

	      if (! XQueryTree (FRAME_X_DISPLAY (f), this_window, &newroot,
				&newparent, &newchildren, &nchildren))
		break;

	      XFree ((char *) newchildren);

	      f->output_data.x->parent_desc = newparent;
	    }
	  else
	    break;
	}

      x_uncatch_errors (FRAME_X_DISPLAY (f), count);
      UNBLOCK_INPUT;
    }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.x->left_pos = (FRAME_X_DISPLAY_INFO (f)->width
				  - 2 * f->output_data.x->border_width - win_x
				  - PIXEL_WIDTH (f)
				  + f->output_data.x->left_pos);

  {
    int height = PIXEL_HEIGHT (f);

#if defined USE_X_TOOLKIT && defined USE_MOTIF
    /* Something is fishy here.  When using Motif, starting Emacs with
       `-g -0-0', the frame appears too low by a few pixels.

       This seems to be so because initially, while Emacs is starting,
       the column widget's height and the frame's pixel height are
       different.  The column widget's height is the right one.  In
       later invocations, when Emacs is up, the frame's pixel height
       is right, though.

       It's not obvious where the initial small difference comes from.
       2000-12-01, gerd.  */
    
    XtVaGetValues (f->output_data.x->column_widget, XtNheight, &height, NULL);
#endif

  if (flags & YNegative)
    f->output_data.x->top_pos = (FRAME_X_DISPLAY_INFO (f)->height
				 - 2 * f->output_data.x->border_width
				 - win_y
				 - height
				 + f->output_data.x->top_pos);
  }
  
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.x->size_hint_flags &= ~ (XNegative | YNegative);
#endif
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

void
x_set_offset (struct frame *f,
	      int xoff, int yoff,
	      int change_gravity)
{
  if (change_gravity)
    {
      f->output_data.mw32->top_pos = yoff;
      f->output_data.mw32->left_pos = xoff;
#if 0
      f->output_data.mw32->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.mw32->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.mw32->size_hint_flags |= YNegative;
      f->output_data.mw32->win_gravity = NorthWestGravity;
#endif
    }
  mw32_calc_absolute_position (f);

  BLOCK_INPUT;

  SetWindowPos (FRAME_MW32_WINDOW (f),
		HWND_NOTOPMOST,
		f->output_data.mw32->left_pos, f->output_data.mw32->top_pos,
		0,0,
		SWP_NOZORDER | SWP_NOSIZE);
  UNBLOCK_INPUT;
}

/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

void
x_set_window_size (struct frame *f,
		   int change_gravity,
		   int cols, int rows)
{
  int pixelwidth, pixelheight;
  RECT rect;

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);

  f->output_data.mw32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FRAME_DEFAULT_FONT_WIDTH (f)));
  f->output_data.mw32->flags_areas_extra = FRAME_FLAGS_AREA_WIDTH (f);

  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);

  rect.left = rect.top = 0;
  rect.right = pixelwidth;
  rect.bottom = pixelheight;

  AdjustWindowRectEx (&rect, f->output_data.mw32->dwStyle,
		      FRAME_EXTERNAL_MENU_BAR (f),
		      f->output_data.mw32->dwStyleEx);

  /* All windows have an extra pixel */

  SetWindowPos (FRAME_MW32_WINDOW (f),
		HWND_NOTOPMOST,
		0, 0,
		rect.right - rect.left,
		rect.bottom - rect.top,
		SWP_NOZORDER | SWP_NOMOVE);

  if (IsIconic (FRAME_MW32_WINDOW (f)) || IsZoomed (FRAME_MW32_WINDOW (f)))
    {
      WINDOWPLACEMENT placement;

      placement.length = sizeof placement;

      GetWindowPlacement (FRAME_MW32_WINDOW (f), &placement);

      placement.rcNormalPosition.right = (placement.rcNormalPosition.left +
					  (rect.right - rect.left));
      placement.rcNormalPosition.bottom = (placement.rcNormalPosition.top +
					   (rect.bottom - rect.top));

      SetWindowPlacement (FRAME_MW32_WINDOW (f), &placement);
    }

  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.

     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.  */
  change_frame_size (f, rows, cols, 0, 1, 0);
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size. 
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  UNBLOCK_INPUT;
}

/* Raise frame F.  */

static void
mw32_raise_frame (struct frame *f)
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW (f),
			   WM_EMACS_SETFOREGROUND,
			   0, 0);
      UNBLOCK_INPUT;
    }
}

/* Lower frame F.  */

static void
mw32_lower_frame (struct frame *f)
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      SetWindowPos (FRAME_MW32_WINDOW (f),
		    HWND_BOTTOM,
		    0,0,0,0,
		    SWP_NOSIZE | SWP_NOMOVE);
      UNBLOCK_INPUT;
    }
}

static void
MW32_frame_raise_lower (FRAME_PTR f, int raise_flag)
{
  if (raise_flag)
    mw32_raise_frame (f);
  else
    mw32_lower_frame (f);
}


/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

void
x_make_frame_visible (struct frame *f)
{
  int mask;

  BLOCK_INPUT;

  if (! FRAME_VISIBLE_P (f))
    {
      if (! FRAME_ICONIFIED_P (f))
	x_set_offset (f, f->output_data.mw32->left_pos,
		      f->output_data.mw32->top_pos, 0);
#if 1
      ShowWindow (FRAME_MW32_WINDOW (f), SW_RESTORE);
      ShowWindow (FRAME_MW32_WINDOW (f), SW_SHOW);
#else
      PostMessage (FRAME_MW32_WINDOW (f), WM_EMACS_SHOWWINDOW,
		   (WPARAM) SW_SHOWNORMAL, 0);
#endif
    }

  UNBLOCK_INPUT;

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    XSETFRAME (frame, f);
#if 0
    while (! f->async_visible)
      {
	/* Machines that do polling rather than SIGIO have been observed
	   to go into a busy-wait here.  So we'll fake an alarm signal
	   to let the handler know that there's something to be read.
	   We used to raise a real alarm, but it seems that the handler
	   isn't always enabled here.  This is probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while processing
	       the fake one.  Turn it off and let the handler reset it.  */
	    alarm (0);
	    input_poll_signal ();
	  }
      }
#endif
    FRAME_SAMPLE_VISIBILITY (f);
  }
}

/* Change from mapped state to withdrawn state.  */

/* Make the frame visible (mapped and not iconified).  */

void
x_make_frame_invisible (f)
     struct frame *f;
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_MW32_DISPLAY_INFO (f)->mw32_highlight_frame == f)
    FRAME_MW32_DISPLAY_INFO (f)->mw32_highlight_frame = 0;

  BLOCK_INPUT;

#if 1
  ShowWindow (FRAME_MW32_WINDOW (f), SW_HIDE);
#else
  PostMessage (FRAME_MW32_WINDOW (f),
	       WM_EMACS_SHOWWINDOW, (WPARAM) SW_HIDE, 0);
#endif

  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;

  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified.  */

void
x_iconify_frame (f)
     struct frame *f;
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_MW32_DISPLAY_INFO (f)->mw32_highlight_frame == f)
    FRAME_MW32_DISPLAY_INFO (f)->mw32_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f) && !FRAME_ICONIFIED_P (f))
    x_set_offset (f, f->output_data.mw32->left_pos,
		  f->output_data.mw32->top_pos, 0);

#if 0
  ShowWindow (FRAME_MW32_WINDOW (f), SW_SHOWMINIMIZED);
#else
  PostMessage (FRAME_MW32_WINDOW (f), WM_SYSCOMMAND, SC_ICON, 0);
#endif

  cancel_mouse_face (f);
  f->async_iconified = 1;
  f->async_visible = 0;

  UNBLOCK_INPUT;

  return;
}


/***********************************************************************
		         Locate Mouse Pointer
 ***********************************************************************/

void
x_set_mouse_position (struct frame *f, int x, int y)
{
  POINT pt;

  pt.x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (FRAME_FONT (f)) / 2;
  pt.y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.mw32->line_height / 2;

  if (pt.x < 0) pt.x = 0;
  if (pt.x > PIXEL_WIDTH (f)) pt.x = PIXEL_WIDTH (f);

  if (pt.y < 0) pt.y = 0;
  if (pt.y > PIXEL_HEIGHT (f)) pt.y = PIXEL_HEIGHT (f);

  BLOCK_INPUT;

  ClientToScreen (FRAME_MW32_WINDOW (f), &pt);
  SetCursorPos (pt.x, pt.y);
  UNBLOCK_INPUT;
}

/* Move the mouse to position pixel PIX_X, PIX_Y relative to frame F.  */

void
x_set_mouse_pixel_position (struct frame *f,
			    int pix_x, int pix_y)
{
  POINT org;
 
  BLOCK_INPUT;
  org.x = org.y = 0;
  ClientToScreen (FRAME_MW32_WINDOW (f), &org);
  SetCursorPos (pix_x + org.x, pix_y + org.y);
  UNBLOCK_INPUT;
}


/***********************************************************************
			          Icon
 ***********************************************************************/
/* currently nothing in this section*/


/***********************************************************************
				Fonts
 ***********************************************************************/

/* Changing the font of the frame.  */
/* Give frame F the font named FONTNAME as its default font, and
   return the full name of that font.  FONTNAME may be a wildcard
   pattern; in that case, we choose some font that fits the pattern.
   The return value shows which font we chose.  */
Lisp_Object
mw32_new_font (struct frame *f, char *fontname)
{
  struct font_info *fontp
    = FS_LOAD_FONT (f, 0, fontname, -1);

  if (!fontp)
    return Qnil;

  FRAME_FONT (f) = (MW32LogicalFont *) (fontp->font);
  f->output_data.mw32->baseline_offset = fontp->baseline_offset;
  f->output_data.mw32->fontset = -1;
  
  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FRAME_DEFAULT_FONT_WIDTH (f);
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    {
      int wid = FRAME_DEFAULT_FONT_WIDTH (f);
      f->scroll_bar_cols = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_MW32_WINDOW (f) != 0)
    {
      frame_update_line_height (f);

      /* Don't change the size of a tip frame; there's no point in
	 doing it because it's done in Fx_show_tip, and it leads to
	 problems because the tip frame has no widget.  */
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
	x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    FRAME_LINE_HEIGHT (f) = FONT_HEIGHT (FRAME_FONT (f));

  return build_string (fontp->full_name);
}

/* Give frame F the fontset named FONTSETNAME as its default font, and
   return the full name of that fontset.  FONTSETNAME may be a wildcard
   pattern; in that case, we choose some fontset that fits the pattern.
   The return value shows which fontset we chose.  */

Lisp_Object
mw32_new_fontset (struct frame *f, char *fontsetname)
{
  int fontset = fs_query_fontset (build_string (fontsetname), 0);
  Lisp_Object result;

  if (fontset < 0)
    return Qnil;

  if (f->output_data.mw32->fontset == fontset)
    /* This fontset is already set in frame F.  There's nothing more
       to do.  */
    return fontset_name (fontset);

  result = mw32_new_font (f, (XSTRING (fontset_ascii (fontset))->data));

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  FRAME_FONTSET (f) = fontset;

  return build_string (fontsetname);
}


/***********************************************************************
	                Frame Resource Manager
 ***********************************************************************/

/* Free resources of frame F.  */
void
mw32_free_frame_resources (struct frame *f)
{
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);

  BLOCK_INPUT;

  free_frame_menubar (f);

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);
  
  mw32_destroy_frame_hdc (f);
  CloseHandle (f->output_data.mw32->mainthread_to_frame_handle);
  xfree (f->output_data.mw32);
  f->output_data.mw32 = NULL;
  
  if (f == dpyinfo->mw32_focus_frame)
    dpyinfo->mw32_focus_frame = 0;
  if (f == dpyinfo->mw32_focus_message_frame)
    dpyinfo->mw32_focus_message_frame = 0;
  if (f == dpyinfo->mw32_highlight_frame)
    dpyinfo->mw32_highlight_frame = 0;

  if (f == dpyinfo->mouse_face_mouse_frame)
    {
      dpyinfo->mouse_face_beg_row
	= dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row
	= dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
      dpyinfo->mouse_face_mouse_frame = 0;
    }

  UNBLOCK_INPUT;
}


/* Destroy the X window of frame F.  */

void
x_destroy_window (FRAME_PTR f)
{
  MSG msg;
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  HWND hwnd = FRAME_MW32_WINDOW (f);

  mw32_free_frame_resources (f);
  last_mouse_motion_frame = Qnil;

  SEND_INFORM_MESSAGE (hwnd, WM_EMACS_DESTROY_FRAME, 0, 0);
  WAIT_REPLY_MESSAGE (&msg, WM_EMACS_DESTROY_FRAME_REPLY);

  dpyinfo->reference_count--;
}


/***********************************************************************
			    Message Thread
 ***********************************************************************/


/***********************************************************************
			    Initialization
 ***********************************************************************/

static int mw32_initialized;

struct mw32_display_info *
mw32_term_init (Lisp_Object display_name,
		char *xrm_option, char *resource_name)
{
  int connection;
  struct mw32_display_info *dpyinfo;

  BLOCK_INPUT;

  if (!mw32_initialized)
    {
      mw32_initialize ();
      mw32_initialized = 1;
    }

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = (struct mw32_display_info *) xmalloc (sizeof (struct mw32_display_info));
  bzero (dpyinfo, sizeof *dpyinfo);

#ifdef MULTI_KBOARD
  {
    struct x_display_info *share;
    Lisp_Object tail;

    for (share = x_display_list, tail = x_display_name_list; share;
	 share = share->next, tail = XCDR (tail))
      if (same_x_server (XSTRING (XCAR (XCAR (tail)))->data,
			 XSTRING (display_name)->data))
	break;
    if (share)
      dpyinfo->kboard = share->kboard;
    else
      {
	dpyinfo->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
	init_kboard (dpyinfo->kboard);
	if (!EQ (XSYMBOL (Qvendor_specific_keysyms)->function, Qunbound))
	  {
	    char *vendor = ServerVendor (dpy);
	    UNBLOCK_INPUT;
	    dpyinfo->kboard->Vsystem_key_alist
	      = call1 (Qvendor_specific_keysyms,
		       build_string (vendor ? vendor : ""));
	    BLOCK_INPUT;
	  }

	dpyinfo->kboard->next_kboard = all_kboards;
	all_kboards = dpyinfo->kboard;
	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the mini-buffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = dpyinfo->kboard;
      }
    dpyinfo->kboard->reference_count++;
  }
#endif

  /* Put this display on the chain.  */
  dpyinfo->next = mw32_display_list;
  mw32_display_list = dpyinfo;

  /* Put it on x_display_name_list as well, to keep them parallel.  */ 
  x_display_name_list = Fcons (Fcons (display_name, Qnil),
			       x_display_name_list);
  dpyinfo->name_list_element = XCAR (x_display_name_list);

  dpyinfo->mw32_id_name
    = (char *) xmalloc (STRING_BYTES (XSTRING (Vinvocation_name))
			+ STRING_BYTES (XSTRING (Vsystem_name))
			+ 2);
  sprintf (dpyinfo->mw32_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

#if 0
  /* Get the scroll bar cursor.  */
  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);
#endif

  {
    RECT r;
    HDC hdc;
    double h, w;
    dpyinfo->root_window = GetDesktopWindow ();
    GetWindowRect (dpyinfo->root_window, &r);
    dpyinfo->height = r.bottom - r.top;
    dpyinfo->width = r.right - r.left;

    hdc = GetDC (dpyinfo->root_window);
    dpyinfo->n_planes = (GetDeviceCaps (hdc, BITSPIXEL)
			 * GetDeviceCaps (hdc, PLANES));
    w = GetDeviceCaps (hdc, HORZSIZE);
    h = GetDeviceCaps (hdc, VERTSIZE);
    dpyinfo->pixel_width = w / dpyinfo->width;
    dpyinfo->pixel_height = h / dpyinfo->height;
    dpyinfo->resx = GetDeviceCaps (hdc, LOGPIXELSX);
    dpyinfo->resy = GetDeviceCaps (hdc, LOGPIXELSY);

    ReleaseDC (dpyinfo->root_window, hdc);
  }

  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->font_table = NULL;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->mw32_focus_frame = 0;
  dpyinfo->mw32_focus_message_frame = 0;
  dpyinfo->mw32_highlight_frame = 0;
  dpyinfo->image_cache = make_image_cache ();

#if 0
  connection = ConnectionNumber (dpyinfo->display);
  dpyinfo->connection = connection;
#endif

  /* Initialize HDC cache.  */
  {
    int i;
    for (i = 0;i < MAX_DC_NUM;i++)
      dpyinfo->alloced_DCs[i] = INVALID_HANDLE_VALUE;
  }

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone,
   and without sending any more commands to the X server.  */

void
mw32_delete_display (dpyinfo)
     struct mw32_display_info *dpyinfo;
{
  if (mw32_display_list == dpyinfo)
    mw32_display_list = dpyinfo->next;
  else
    {
      struct mw32_display_info *tail;

      for (tail = mw32_display_list; tail; tail = tail->next)
	if (tail->next == dpyinfo)
	  tail->next = tail->next->next;
    }

  /* Release DCs */
  {
    int i;
    HDC *phdc = dpyinfo->alloced_DCs;
    struct frame **pf = dpyinfo->alloced_DC_frames;
    for (i = 0;i < MAX_DC_NUM;i++)
      {
	if (*phdc != INVALID_HANDLE_VALUE)
	  {
	    abort ();
	    RestoreDC (*phdc, -1);
	    ReleaseDC (FRAME_MW32_WINDOW (pf[i]), *phdc);
	  }
	phdc++;
      }
  }

  if (dpyinfo->font_table)
    xfree (dpyinfo->font_table);
  xfree (dpyinfo->mw32_id_name);
  /* xfree (dpyinfo->color_cells); */
  xfree (dpyinfo);
}


/* Set up use of X before we make the first connection.  */

static struct redisplay_interface mw32_redisplay_interface =
{
  mw32i_produce_glyphs,
  mw32i_write_glyphs,
  mw32i_insert_glyphs,
  mw32i_clear_end_of_line,
  mw32i_scroll_run,
  mw32i_after_update_window_line,
  mw32i_update_window_begin,
  mw32i_update_window_end,
  MW32_cursor_to,
  mw32i_flush,
  mw32i_clear_mouse_face,
  mw32i_get_glyph_overhangs,
  mw32_fix_overlapping_area
};

void
mw32_initialize ()
{
  Lisp_Object frame;
  char *defaultvalue;
  int argc = 0;
  char** argv = 0;
  
  rif = &mw32_redisplay_interface;

  clear_frame_hook = MW32_clear_frame;
  ins_del_lines_hook = MW32_ins_del_lines;
  change_line_highlight_hook = MW32_change_line_highlight;
  delete_glyphs_hook = MW32_delete_glyphs;
  ring_bell_hook = MW32_ring_bell;
  reset_terminal_modes_hook = MW32_reset_terminal_modes;
  set_terminal_modes_hook = MW32_set_terminal_modes;
  update_begin_hook = MW32_update_begin;
  update_end_hook = MW32_update_end;
  set_terminal_window_hook = MW32_set_terminal_window;
  read_socket_hook = MW32_read_socket;
  frame_up_to_date_hook = MW32_frame_up_to_date;
  reassert_line_highlight_hook = MW32_reassert_line_highlight;
  mouse_position_hook = MW32_mouse_position;
  frame_rehighlight_hook = MW32_frame_rehighlight;
  frame_raise_lower_hook = MW32_frame_raise_lower;
  set_vertical_scroll_bar_hook = MW32_set_vertical_scroll_bar;
  condemn_scroll_bars_hook = MW32_condemn_scroll_bars;
  redeem_scroll_bar_hook = MW32_redeem_scroll_bar;
  judge_scroll_bars_hook = MW32_judge_scroll_bars;
  estimate_mode_line_height_hook = MW32_estimate_mode_line_height;

  scroll_region_ok = 1;		/* we'll scroll partial frames */
  char_ins_del_ok = 1;
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_frame = 0;	/* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  last_tool_bar_item = -1;
  any_help_event_p = 0;
  
  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

#ifdef IME_CONTROL
  mw32_ime_control_init ();
#endif

#ifdef W32_INTELLIMOUSE
  mw32_wheel_message = RegisterWindowMessage ("MSWHEEL_ROLLMSG");
#endif

  next_message_block_event = CreateEvent (0, TRUE, TRUE, NULL);
  keyboard_handle = CreateEvent (0, TRUE, TRUE, NULL);

  if (!DuplicateHandle (GetCurrentProcess (),
			GetCurrentThread (),
			GetCurrentProcess (),
			&main_thread,
			0, FALSE,
			DUPLICATE_SAME_ACCESS))
    abort ();

  main_thread_id = GetCurrentThreadId ();

  /* Create message thread.  */
  /* Caution!!!!! inherited thread can't make Lisp Object directly 
     in stack.  */
  /* message thread might consume lots of stack
     because of mw32_draw_glyphs().  We prepare 4MB.  */
  msg_thread = CreateThread (NULL, 4 * 1024 * 1024,
			     mw32_async_handle_message,
			     0, 0, &msg_thread_id);
  // SetThreadPriority(msg_thread, THREAD_PRIORITY_ABOVE_NORMAL);
  message_loop_blocked_p = 0;
  mw32_frame_window = INVALID_HANDLE_VALUE;

  mw32_display_list = NULL;

  track_mouse_window = NULL;
  
  {
    HMODULE user32_lib = GetModuleHandle ("user32.dll");
    /*
      TrackMouseEvent not available in all versions of Windows, so must load
      it dynamically.  Do it once, here, instead of every time it is used.
    */
    track_mouse_event_fn
      = (TrackMouseEvent_Proc) GetProcAddress (user32_lib, "TrackMouseEvent");
  }
}



/***********************************************************************
	      Emacs Lisp Interfaces / Symbol Registration
 ***********************************************************************/

DEFUN ("w32-get-system-metrics", Fw32_get_system_metrics,
       Sw32_get_system_metrics,
       1, 1, 0, 
"Retrieve system metrics. This function only calls GetSystemMetrics.")
  (Lisp_Object index)
{
  Lisp_Object ret;
  CHECK_NUMBER (index, 0);
  XSETINT (ret, GetSystemMetrics (XFASTINT (index)));
  return ret;
}

DEFUN ("w32-set-modifier-key", Fw32_set_modifier_key,
       Sw32_set_modifier_key, 2, 2, 0,
       "Set modifier key. \n\
KEY must be a virtual key code or string that describe a key.\n\
MODIFIER is one of these.\n\
'nil....normal key. 'none...neglected key. \n\
'meta...meta modifier. 'ctrl...ctrl modifier. \n\
'shift...shift modifier(it works modifier key only.)\n\
'alt...alt modifier. 'super...super modifier.\n\
'hyper...hyper modifier.")
     (Lisp_Object key, Lisp_Object modifier)
{
  int virtkey;

  if (NUMBERP (key))
    {
      virtkey = XFASTINT (key);
      if (!((key <= 255) && (key >= 0)))
	error ("invalid key %d", virtkey);
    }
  else if (STRINGP (key))
    {
      extern char *lispy_function_keys[];
      int i;

      virtkey = -1;
      for (i = 0;i < 256;i++)
	{
	  if (lispy_function_keys[i] &&
	      (strcmp (lispy_function_keys[i],
		       XSTRING (key)->data) == 0))
	    {
	      virtkey = i;
	      break;
	    }
	}

      if (virtkey == -1)
	error ("Can't find the key:%s", XSTRING (key)->data);
    }
  else
    error ("KEY must be a string or number.");
      

  if (EQ (modifier, Qnil))
    {
      keymodifier[virtkey] &= 0x80;
    }
  else if (EQ (modifier, intern ("none")))
    {
      keymodifier[virtkey] |= 0x01;
    }
  else if (EQ (modifier, intern ("meta")))
    {
      keymodifier[virtkey] |= 0x02;
    }
  else if (EQ (modifier, intern ("ctrl")))
    {
      keymodifier[virtkey] |= 0x04;
    }
  else if (EQ (modifier, intern ("shift")))
    {
      keymodifier[virtkey] |= 0x08;
    }
  else if (EQ (modifier, intern ("alt")))
    {
      keymodifier[virtkey] |= 0x10;
    }
  else if (EQ (modifier, intern ("super")))
    {
      keymodifier[virtkey] |= 0x20;
    }
  else if (EQ (modifier, intern ("hyper")))
    {
      keymodifier[virtkey] |= 0x40;
    }
  else
    {
      error ("unknown modifier type!!");
    }
  return Qnil;
}

DEFUN ("w32-get-key-state", Fw32_get_key_state, Sw32_get_key_state,
       1, 1, 0, 
"Retrieve a key state when the previous message was received;\n\
not the current state. KEY is a virtual key code to get a state.")
  (Lisp_Object key)
{
  int state;
  Lisp_Object ret;
  CHECK_NUMBER (key, 0);
  state = GetKeyState (XFASTINT (key));
  if (state & 0x8000) ret = Qt;
  else ret = Qnil;
  return ret;
}

DEFUN ("w32-get-mouse-wheel-scroll-lines", 
       Fw32_get_mouse_wheel_scroll_lines,
       Sw32_get_mouse_wheel_scroll_lines,
       1, 1, 0, 
       "Retrieve a number of scroll lines from delta number of mouse wheel.")
  (Lisp_Object delta)
{
#ifdef W32_INTELLIMOUSE
  UINT lines;
  int dt;

  CHECK_NUMBER (delta, 0);

  dt = XINT (delta);
  if (!SystemParametersInfo (SPI_GETWHEELSCROLLLINES, 0, &lines, 0))
    return Qnil;

  if (lines == WHEEL_PAGESCROLL)
    if (dt > 0) return intern ("above-handle");
    else if (dt < 0) return intern ("below-handle");
    else dt = 0;

  return make_number (-((signed int) (lines) * dt / WHEEL_DELTA));
#else
  return make_number (0);
#endif
}

DEFUN ("w32-keyboard-type", 
       Fw32_keyboard_type,
       Sw32_keyboard_type,
       0, 0, 0, 
       "Retrieve w32 keyboard type with string.")
     ()
{
  int keyboardtypenum, oemkeyboardtype, functionkeys;
  char keyboardlayout[KL_NAMELENGTH];
  char buf[KL_NAMELENGTH + 20 + 20 + 20 +1];
  char *oemtype;
  static char* keyboardtype[] =
    {
      "unknown",
      "PC/XT#83",
      "Olivetti/ICO#102",
      "PC/AT#84",
      "PC/AT#101",
      "Nokia1050",
      "Nokia9140",
      "Japanese",
    };
  static char* japanoemname[] =
    {
      "Microsoft",
      "AX", 0, 0,
      "EPSON",
      "Fujitsu", 0,
      "IBM@Japan", 0, 0,
      "Matsushita", 0, 0,
      "NEC", 0, 0, 0, 0,
      "Toshiba",
    };

  keyboardtypenum = GetKeyboardType (0);
  functionkeys    = GetKeyboardType (2);
  oemkeyboardtype = GetKeyboardType (1);
  if (!GetKeyboardLayoutName (keyboardlayout)) return Qnil;
#if 0
  if (!((functionkeytype >= 1) && (functionkeytype <= 6)))
    functionkeytype = 0;
#endif
  if (!((keyboardtypenum >= 1) && (keyboardtypenum <= 7)))
    keyboardtypenum = 0;

  if (keyboardtypenum == 7)
    {
      if (!((oemkeyboardtype >= 0) && (oemkeyboardtype <= 18)))
	oemtype = 0;
      else
	oemtype = japanoemname[oemkeyboardtype];
    }
  else
    oemtype = 0;

  if (oemtype)
    sprintf (buf, "%s-%s-%d-%s",
	     keyboardtype[keyboardtypenum],
	     oemtype,
	     functionkeys,
	     keyboardlayout);
  else
    sprintf (buf, "%s-OEMNo.0x%08x-%d-%s",
	     keyboardtype[keyboardtypenum],
	     oemkeyboardtype,
	     functionkeys,
	     keyboardlayout);

  return build_string (buf);
}

DEFUN ("mw32-get-device-capability",
       Fmw32_get_device_capability,
       Smw32_get_device_capability,
       1, 2, 0, 
"Retrieve system metrics. This function only calls GetSystemMetrics.")
  (Lisp_Object item, Lisp_Object target)
{
  HDC hdc;
  Lisp_Object ret;

  if (EQ (target, Qnil))
    {
      hdc = FRAME_HDC (SELECTED_FRAME ());
    }
  else if (FRAMEP (target))
    {
      hdc = FRAME_HDC (XFRAME (target));
    }
  else
    {
      Fsignal (Qwrong_type_argument,
	       Fcons (build_string ("Currently, target must be frame or nil."),
		      Fcons (item, Qnil)));
    }

  if (EQ (item, intern ("width-in-mm")))
    {
      int val = GetDeviceCaps (hdc, HORZSIZE);
      XSETINT (ret, val);
    }
  else if (EQ (item, intern ("height-in-mm")))
    {
      int val = GetDeviceCaps (hdc, VERTSIZE);
      XSETINT (ret, val);
    }
  else if (EQ (item, intern ("width")))
    {
      int val = GetDeviceCaps (hdc, HORZRES);
      XSETINT (ret, val);
    }
  else if (EQ (item, intern ("height")))
    {
      int val = GetDeviceCaps (hdc, VERTRES);
      XSETINT (ret, val);
    }
  else if (EQ (item, intern ("pixel-width-in-mm")))
    {
      int val = GetDeviceCaps (hdc, LOGPIXELSX);
      ret = make_float (25.4 / val);
    }
  else if (EQ (item, intern ("pixel-height-in-mm")))
    {
      int val = GetDeviceCaps (hdc, LOGPIXELSY);
      ret = make_float (25.4 / val);
    }
  else if (EQ (item, intern ("color-bits")))
    {
      int val = (GetDeviceCaps (hdc, BITSPIXEL)
		 * GetDeviceCaps (hdc, PLANES));
      XSETINT (ret, val);
    }
  else if (EQ (item, intern ("colors")))
    {
      int val = GetDeviceCaps (hdc, NUMCOLORS);
      if (val == -1)
	ret = intern ("full");
      else
	XSETINT (ret, val);
    }
  else if (EQ (item, intern ("clipping")))
    {
      int val = GetDeviceCaps (hdc, CLIPCAPS);
      if (val)
	ret = Qt;
      else
	ret = Qnil;
    }
  else if (EQ (item, intern ("text-capabilities")))
    {
      int val = GetDeviceCaps (hdc, TEXTCAPS);

      ret = Qnil;
      if (val & TC_OP_CHARACTER)
	ret = Fcons (intern ("character"), ret);
      if (val & TC_OP_STROKE)
	ret = Fcons (intern ("stroke"), ret);
      if (val & TC_CP_STROKE)
	ret = Fcons (intern ("clipping"), ret);
      if (val & TC_CR_90)
	ret = Fcons (intern ("rotate-90"), ret);
      if (val & TC_CR_ANY)
	ret = Fcons (intern ("rotate"), ret);
      if (val & TC_SA_DOUBLE)
	ret = Fcons (intern ("scale"), ret);
      if (val & TC_SA_INTEGER)
	ret = Fcons (intern ("scale-in-integer"), ret);

      if (val & TC_IA_ABLE)
	ret = Fcons (intern ("italic"), ret);
      if (val & TC_UA_ABLE)
	ret = Fcons (intern ("underline"), ret);
      if (val & TC_SO_ABLE)
	ret = Fcons (intern ("strikeout"), ret);

      if (val & TC_RA_ABLE)
	ret = Fcons (intern ("raster"), ret);
      if (val & TC_VA_ABLE)
	ret = Fcons (intern ("vector"), ret);
    }
  else if (NUMBERP (item))
    {
      int val = GetDeviceCaps (hdc, XINT (item));
      XSETINT (ret, val);
    }
  else
    {
      Fsignal (Qwrong_type_argument,
	       Fcons (build_string ("Invalid item."),
		      Fcons (item, Qnil)));
    }

  return ret;
}

DEFUN ("Meadow-version",
       FMeadow_version,
       SMeadow_version,
       0, 1, 0, 
       "return the Meadow's version in string. \n\
The optional argument DUMMY is not currently used.")
     (dummy)
{
  return (build_string (MEADOW_VERSION_STRING));
}

void
syms_of_mw32term ()
{
  staticpro (&x_display_name_list);
  x_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&last_mouse_press_frame);
  last_mouse_press_frame = Qnil;
  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;

  help_echo = Qnil;
  staticpro (&help_echo);
  help_echo_object = Qnil;
  staticpro (&help_echo_object);
  help_echo_window = Qnil;
  staticpro (&help_echo_window);
  previous_help_echo = Qnil;
  staticpro (&previous_help_echo);
  help_echo_pos = -1;

  DEFVAR_BOOL ("mw32-stretch-cursor", &mw32_stretch_cursor_p,
    "*Non-nil means draw block cursor as wide as the glyph under it.\n\
For example, if a block cursor is over a tab, it will be drawn as\n\
wide as that tab on the display.");
  mw32_stretch_cursor_p = 0;


  DEFVAR_INT ("w32-lbutton-to-emacs-button", &mw32_lbutton_to_emacs_button,
    "Position of a mouse button sent to emacs, when the w32 left button \n\
is changed.");
  DEFVAR_INT ("w32-mbutton-to-emacs-button", &mw32_mbutton_to_emacs_button,
    "Position of a mouse button sent to emacs, when the w32 middle button \n\
is changed.");
  DEFVAR_INT ("w32-rbutton-to-emacs-button", &mw32_rbutton_to_emacs_button,
    "Position of a mouse button sent to emacs, when the w32 right button \n\
is changed.");
  DEFVAR_INT ("w32-hide-mouse-timeout", &mw32_hide_mouse_timeout,
    "Mouse cursor will hide after some rest. (in milliseconds) \n\
Cursor will not hide if 0. (default) \n\
This works correctly on Windows 98, 2000 or later.");
  DEFVAR_BOOL ("w32-hide-mouse-on-key", &mw32_hide_mouse_on_key,
    "Non nil means mouse will hide on key input.");

  mw32_hide_mouse_timeout = 0;    /* infinite */
  mw32_hide_mouse_on_key = 0;
  mw32_lbutton_to_emacs_button = 0;
  mw32_mbutton_to_emacs_button = 2;
  mw32_rbutton_to_emacs_button = 1;

  defsubr (&Sw32_get_system_metrics);
  defsubr (&Sw32_set_modifier_key);
  defsubr (&Sw32_get_key_state);
  defsubr (&Sw32_get_mouse_wheel_scroll_lines);
  defsubr (&Sw32_keyboard_type);
  defsubr (&Smw32_get_device_capability);
  defsubr (&SMeadow_version);
}
