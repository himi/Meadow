/* Definitions and headers for communication with MW32 display engine.
   Copyright (C) 1989, 1993, 1994, 1998, 1999, 2000, 2001, 2001
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

#ifndef _MW32TERM_H_
#define _MW32TERM_H_

#include <windows.h>
#include <windowsx.h>
#include "frame.h"
#include "mw32font.h"

/* The class of this application.  */
#define EMACS_CLASS "MEADOW"

#define PIX_TYPE COLORREF
#define BLACK_PIX_DEFAULT(f) PALETTERGB(0,0,0)
#define WHITE_PIX_DEFAULT(f) PALETTERGB(255,255,255)

enum text_cursor_kinds
{
  NO_CURSOR = -1,
  FILLED_BOX_CURSOR,
  HOLLOW_BOX_CURSOR,
  BAR_CURSOR,
  CARET_CURSOR = 0x100,
  CHECKERED_CARET_CURSOR,
  HAIRLINE_CARET_CURSOR
};

#define CARET_CURSOR_P(c) ((c) >= CARET_CURSOR)

enum frame_caret_state
{
  NO_CARET,             /* system caret is not created */
  BLOCK_CARET,		/* (NOT STATUS) lock caret to be hidden */
  HIDDEN_CARET,		/* system caret is not shown */
  SHOWN_CARET,		/* system caret is sohwn */
  UNBLOCK_CARET		/* (NOT STATUS) unlock blocked state of caret  */
};


#define MW32_FRAME_CARET_STATE(f) ((f)->output_data.mw32->caret_state)
#define MW32_FRAME_CARET_BLOCKED(f) ((f)->output_data.mw32->caret_blocked)
#define MW32_FRAME_CARET_SHOWN(f) (MW32_FRAME_CARET_STATE(f) > HIDDEN_CARET)
#define MW32_FRAME_CARET_WIDTH(f) ((f)->output_data.mw32->cursor_width)
#define MW32_FRAME_CARET_HEIGHT(f) ((f)->output_data.mw32->cursor_height)
#define MW32_FRAME_CARET_BITMAP(f) ((f)->output_data.mw32->caret_bitmap)

/* Structure recording MW32 image and reference count.
   If REFCOUNT is 0 then this record is free to be reused.  */

struct mw32_bitmap_record
{
  HBITMAP hbmp;
  char *file;
  int refcount;
  /* Record some info about this pixmap.  */
  int height, width, depth;
};



/* 
   On Windows 95/98, we can allocate 5 DCs per thread.
 */
#define MAX_DC_NUM 3

/* For each X display, we have a structure that records
   information about it.  */

typedef struct mw32_device_cap
{
  int dummy;
} mw32_device_cap;

struct mw32_display_info
{
  /* Chain of all x_display_info structures.  */
  struct mw32_display_info *next;
  
  /* Connection number (normally a file descriptor number).  */
  int connection;

#if 0  
  /* This says how to access this display on Windows.  */
  DWORD vchandle;
#endif
  
  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).
     The same cons cell also appears in mw32_display_name_list.  */
  Lisp_Object name_list_element;
  
  /* Number of frames that are on this display.  */
  int reference_count;

  /* Display's device capability */
  mw32_device_cap dispcap;
  
  /* Number of panes on this screen.  */
  int n_planes;
  
  /* Dimensions of this screen in pixel.  */
  int height, width;

  /* Pixel size in millimeter. */
  double pixel_width, pixel_height;

  /* Dots per inch of the screen.  */
  double resx, resy;
  
  /* Mask of things that cause the mouse to be grabbed.  */
  int grabbed;

  /* Status of mouse cursor. */
  /* 0 means mouse cursor is visible, -1 is invisible,
     1 means locked visible. */
  int mouse_cursor_stat;
  
  /* The root window of this screen.  */
  HWND root_window;
  
  /* A table of all the fonts we have already loaded.  */
  struct font_info *font_table;

  /* The current capacity of x_font_table.  */
  int font_table_size;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /* These variables describe the range of text currently shown in its
     mouse-face, together with the window they apply to.  As long as
     the mouse stays within this range, we need not redraw anything on
     its account.  Rows and columns are glyph matrix positions in
     MOUSE_FACE_WINDOW.  */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_beg_x, mouse_face_beg_y;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_end_x, mouse_face_end_y;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;
  Lisp_Object mouse_face_overlay;

  /* 1 if a mouse motion event came and we didn't handle it right away because
     gc was in progress.  */
  int mouse_face_deferred_gc;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero means defer mouse-motion highlighting.  */
  int mouse_face_defer;

  int mouse_face_image_state;

  char *mw32_id_name;

  /* The number of fonts actually stored in x_font_table.
     font_table[n] is used and valid iff 0 <= n < n_fonts.  0 <=
     n_fonts <= font_table_size and font_table[i].name != 0.  */
  int n_fonts;

  /* Pointer to bitmap records.  */
  struct mw32_bitmap_record *bitmaps;

  /* Allocated size of bitmaps field.  */
  int bitmaps_size;

  /* Last used bitmap index.  */
  int bitmaps_last;

#if 0
  /* Which modifier keys are on which modifier bits?

     With each keystroke, X returns eight bits indicating which modifier
     keys were held down when the key was pressed.  The interpretation
     of the top five modifier bits depends on what keys are attached
     to them.  If the Meta_L and Meta_R keysyms are on mod5, then mod5
     is the meta bit.

     meta_mod_mask is a mask containing the bits used for the meta key.
     It may have more than one bit set, if more than one modifier bit
     has meta keys on it.  Basically, if EVENT is a KeyPress event,
     the meta key is pressed if (EVENT.state & meta_mod_mask) != 0.

     shift_lock_mask is LockMask if the XK_Shift_Lock keysym is on the
     lock modifier bit, or zero otherwise.  Non-alphabetic keys should
     only be affected by the lock modifier bit if XK_Shift_Lock is in
     use; XK_Caps_Lock should only affect alphabetic keys.  With this
     arrangement, the lock modifier should shift the character if
     (EVENT.state & shift_lock_mask) != 0.  */
  int meta_mod_mask, shift_lock_mask;

  /* These are like meta_mod_mask, but for different modifiers.  */
  int alt_mod_mask, super_mod_mask, hyper_mod_mask;

  /* Communication with window managers.  */
  Atom Xatom_wm_protocols;
  
  /* Kinds of protocol things we may receive.  */
  Atom Xatom_wm_take_focus;
  Atom Xatom_wm_save_yourself;
  Atom Xatom_wm_delete_window;
  
  /* Atom for indicating window state to the window manager.  */
  Atom Xatom_wm_change_state;
  
  /* Other WM communication */
  Atom Xatom_wm_configure_denied; /* When our config request is denied */
  Atom Xatom_wm_window_moved;     /* When the WM moves us.  */
  
  /* EditRes protocol */
  Atom Xatom_editres;

  /* More atoms, which are selection types.  */
  Atom Xatom_CLIPBOARD, Xatom_TIMESTAMP, Xatom_TEXT, Xatom_DELETE,
  Xatom_COMPOUND_TEXT,
  Xatom_MULTIPLE, Xatom_INCR, Xatom_EMACS_TMP, Xatom_TARGETS, Xatom_NULL,
  Xatom_ATOM_PAIR;

  /* More atoms for font properties.  The last three are private
     properties, see the comments in src/fontset.h.  */
  Atom Xatom_PIXEL_SIZE,
  Xatom_MULE_BASELINE_OFFSET, Xatom_MULE_RELATIVE_COMPOSE,
  Xatom_MULE_DEFAULT_ASCENT;

  /* More atoms for Ghostscript support.  */
  Atom Xatom_DONE, Xatom_PAGE;

  /* Atom used in toolkit scroll bar client messages.  */
  Atom Xatom_Scrollbar;
#endif

#ifdef MULTI_KBOARD
  struct kboard *kboard;
#endif

  /* The frame (if any) which has the window that has keyboard focus.
     Zero if none.  This is examined by Ffocus_frame in mw32fns.c.  Note
     that a mere EnterNotify event can set this; if you need to know the
     last frame specified in a FocusIn or FocusOut event, use
     mw32_focus_message_frame.  */
  struct frame *mw32_focus_frame;

  /* The last frame mentioned in a FocusIn or FocusOut event.  This is
     separate from mw32_focus_frame, because whether or not LeaveNotify
     events cause us to lose focus depends on whether or not we have
     received a FocusIn event for it.  */
  struct frame *mw32_focus_message_frame;

  /* The frame which currently has the visual highlight, and should get
     keyboard input (other sorts of input have the frame encoded in the
     event).  It points to the X focus frame's selected window's
     frame.  It differs from mw32_focus_frame when we're using a global
     minibuffer.  */
  struct frame *mw32_highlight_frame;

  /* allocated DCs */
  HDC alloced_DCs[MAX_DC_NUM];
  struct frame *alloced_DC_frames[MAX_DC_NUM];

  /* Cache of images.  */
  struct image_cache *image_cache;

#if 0
  /* The null pixel used for filling a character background with
     background color of a gc.  */
  Pixmap null_pixel;

  /* The gray pixmap.  */
  Pixmap gray;

  /* If non-null, a cache of the colors in the color map.  Don't
     use this directly, call x_color_cells instead.  */
  XColor *color_cells;
  int ncolor_cells;
#endif
};

/* This checks to make sure we have a display.  */
extern void check_x P_ ((void));

/* This is a chain of structures for all the MW32 display_infos
   currently in use.  */
extern struct mw32_display_info *mw32_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of x_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
extern Lisp_Object mw32_display_name_list;

/* A flag to control how to display unibyte 8-bit character.  */
extern int unibyte_display_via_language_environment;

#define GET_MW32_DISPLAY_INFO(arg) (mw32_display_list)


/* Put some things in x_output for compatibility.
   mainly for frame.c...
   NTEMACS_TODO: Move all common things here to eliminate unneccesary
   diffs between X and w32 code.  */
struct x_output
{
  PIX_TYPE background_pixel;
  PIX_TYPE foreground_pixel;
};

/* Each X frame object points to its own struct x_output object
   in the output_data.x field.  The x_output structure contains
   the information that is specific to X windows.  */

struct mw32_output
{
  /* Position of the window (x and y offsets in root window).  */
  int left_pos;
  int top_pos;

  /* Border width of the window as known by the X window system.  */
  int border_width;

  /* Size of the window in pixels.  */
  int pixel_height, pixel_width;

  /* Height of a line, in pixels.  */
  int line_height;

  /* caret bitmap handle */
  HBITMAP caret_bitmap;

  /* current cursor/caret width.  */
  int cursor_width;

  /* current cursor/caret height. unit is 4th of whole line height */
  int cursor_height;

  /* It holds the current caret state.
     See frame_caret_state for detail. */
  int caret_state;

  /* Ture if caret is blocked. */
  int caret_blocked;

  /* If this flag is set, menubar is never reconstructed.
     This is used mainly during menubar activation.*/
  int disable_reconstruct_menubar;

#if 0
  /* The tiled border used when the mouse is out of the frame.  */
  Pixmap border_tile;
#endif

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  When the frame is selected,
     a highlighting is displayed inside the internal border.  */
  int internal_border_width;

  /* The window ID used for this frame.
     May be zero while the frame object is being created
     and the X window has not yet been created.  */
  HWND window_desc;

  /* window's DC.  If it has not been alloced,
     we have to set it to INVALID_HANDLE_VALUE.  */
  HDC hdc;

  /* Window's DC that is used for message thread. */
  HDC message_thread_hdc;

  /* window style flags */
  DWORD dwStyle;
  DWORD dwStyleEx;

#if 0
  /* The X window used for the bitmap icon;
     or 0 if we don't have a bitmap icon.  */
  HWND icon_desc;
#endif

  /* The window that is the parent of this window.
     Usually this is a window that was made by the window manager,
     but it can be the root window, and it can be explicitly specified
     (see the explicit_parent field, below).  */
  HWND parent_desc;

  /* frame menubar handle */
  HANDLE menubar_handle;

  /* Event handle to inform message loop thread that the mainthread has
     done something on this frame. */
  HANDLE mainthread_to_frame_handle;

  /* If >=0, a bitmap index.  The indicated bitmap is used for the
     icon. */
  int icon_bitmap;

  /* Default ASCII font of this frame.  */
  MW32LogicalFont *font;

  /* The baseline offset of the default ASCII font.  */
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset;

  /* Pixel values used for various purposes.
     border_pixel may be -1 meaning use a gray tile.  */
  PIX_TYPE background_pixel;
  PIX_TYPE foreground_pixel;
  PIX_TYPE cursor_pixel;
  PIX_TYPE border_pixel;
  PIX_TYPE mouse_pixel;
  PIX_TYPE cursor_foreground_pixel;
  PIX_TYPE scroll_bar_foreground_pixel;
  PIX_TYPE scroll_bar_background_pixel;

  /* Descriptor for the cursor in use for this window.  */
  HCURSOR text_cursor;
  HCURSOR nontext_cursor;
  HCURSOR modeline_cursor;
  HCURSOR cross_cursor;
  HCURSOR hourglass_cursor;
  HCURSOR horizontal_drag_cursor;

  /* Non-zero means hourglass cursor is currently displayed.  */
  unsigned hourglass_p : 1;

  /* Flag to set when the X window needs to be completely repainted.  */
  int needs_exposure;

  /* What kind of text cursor should we draw in the future?
     This should always be filled_box_cursor or bar_cursor.  */
  enum text_cursor_kinds desired_cursor;

  /* The size of the extra width currently allotted for vertical
     scroll bars, in pixels.  */
  int vertical_scroll_bar_extra;

  /* The extra width currently allotted for the areas in which
     truncation marks, continuation marks, and overlay arrows are
     displayed.  */
  int flags_areas_extra;

  /* This is the gravity value for the specified window position.  */
  int win_gravity;

  /* The geometry flags for this window.  */
  int size_hint_flags;

  /* This is the Emacs structure for the X display this frame is on.  */
  struct mw32_display_info *display_info;

  /* Nonzero means our parent is another application's window
     and was explicitly specified.  */
  char explicit_parent;

  /* Nonzero means tried already to make this frame visible.  */
  char asked_for_visible;

  /* Nonzero if this frame was ever previously visible.  */
  char has_been_visible;

  /* Nonzero means menubar is currently active.  */
  char menubar_active;

  /* Nonzero means a menu command is being processed.  */
  char menu_command_in_progress;

  /* Relief GCs, colors etc.  */
  struct relief
  {
    PIX_TYPE foreground;
    PIX_TYPE background;
    int allocated_p;
  }
  black_relief, white_relief;

  /* The background for which the above relief GCs were set up.
     They are changed only when a different background is involved.  */
  PIX_TYPE relief_background;

  /* 1 means frame size is about to change.
     2 means frame size has been already changed and adjusted.  */
  char frame_change_state;

  /* It holds the state of IME.  Currently it only maintain the status
     of composition string.  If it is non-zero, the composition window is
     now opened.  Otherwise, it is closed.  */
  char ime_composition_state;

  /* logfont for IME  */
  LOGFONT ime_logfont;  
};

/* Return the X window used for displaying data in frame F.  */
#define FRAME_MW32_WINDOW(f) ((f)->output_data.mw32->window_desc)

/* Return the outermost window associated with the frame F.  */
#define FRAME_OUTER_WINDOW(f) (FRAME_MW32_WINDOW (f))

/* Return frame's DC. */
#define FRAME_HDC(f)					\
  (MW32_MAIN_THREAD_P() ?				\
   ((f->output_data.mw32->hdc != INVALID_HANDLE_VALUE)	\
    ? f->output_data.mw32->hdc : mw32_get_frame_hdc(f))	\
   : ((f->output_data.mw32->message_thread_hdc		\
       != INVALID_HANDLE_VALUE)				\
      ? f->output_data.mw32->message_thread_hdc		\
      : (abort(), INVALID_HANDLE_VALUE)))

/* Return the validity of frame's DC. */
#define FRAME_HDC_VALID_P(f)				\
  (MW32_MAIN_THREAD_P() ?				\
   1							\
   : ((f->output_data.mw32->message_thread_hdc		\
       != INVALID_HANDLE_VALUE)				\
      ? 1 : 0))

#define FRAME_FONT(f) ((f)->output_data.mw32->font)
#define FRAME_FONTSET(f) ((f)->output_data.mw32->fontset)
#define FRAME_INTERNAL_BORDER_WIDTH(f) ((f)->output_data.mw32->internal_border_width)
#define FRAME_MENUBAR_HEIGHT(f) ((f)->output_data.mw32->menubar_height)
#define FRAME_LINE_HEIGHT(f) ((f)->output_data.mw32->line_height)

/* Width of the default font of frame F.  Must be defined by each
   terminal specific header.  */
#define FRAME_DEFAULT_FONT_WIDTH(F) 	FONT_WIDTH (FRAME_FONT (F))

/* This gives the mw32_display_info structure for the display F is on.  */
#define FRAME_MW32_DISPLAY_INFO(f) ((f)->output_data.mw32->display_info)
/* For compatibility with xterm.h, fontset.h uses this definition.
   Don't use this definition in mw32 modules.  */
#define FRAME_X_DISPLAY_INFO(f) FRAME_MW32_DISPLAY_INFO(f)

/* This is the `Display *' which frame F is on.  */
/* #define FRAME_MW32_DISPLAY(f) (FRAME_MW32_DISPLAY_INFO (f)->display) */
/* Right now, on MW32, always return 1.  */
#define FRAME_MW32_DISPLAY(f) (1)

/* CAUTION!!!!!!!!!!!!!!!!
   These macros selects output_data.x (x_output) to access
   foreground_pixel and background_pixel.  So you CANNOT use
   these macros for any frames that uses MW32 output.
   Refer make_terminal_frame()@frame.c for the detail.  These
   are only for a terminal frame!  */
#define FRAME_FOREGROUND_PIXEL(f) ((f)->output_data.x->foreground_pixel)
#define FRAME_BACKGROUND_PIXEL(f) ((f)->output_data.x->background_pixel)

/* These two really ought to be called FRAME_PIXEL_{WIDTH,HEIGHT}.  */
#define PIXEL_WIDTH(f) ((f)->output_data.mw32->pixel_width)
#define PIXEL_HEIGHT(f) ((f)->output_data.mw32->pixel_height)

#define FRAME_DESIRED_CURSOR(f) ((f)->output_data.mw32->desired_cursor)

/* Value is the smallest width of any character in any font on frame F.  */

#define FRAME_SMALLEST_CHAR_WIDTH(F) \
     FRAME_MW32_DISPLAY_INFO(F)->smallest_char_width

/* Value is the smallest height of any font on frame F.  */

#define FRAME_SMALLEST_FONT_HEIGHT(F) \
     FRAME_MW32_DISPLAY_INFO(F)->smallest_font_height

/* True if mw32_img is properly initialized.  */
#define MW32_IMAGE_VALID_P(I) ((I).pbmpdata)
#define MW32_IMAGE_HAS_MASK_P(I) ((I).pbmpmask)

/* Return a pointer to the image cache of frame F.  */
#define FRAME_MW32_IMAGE_CACHE(F) (FRAME_MW32_DISPLAY_INFO ((F))->image_cache)
/* This is for compatibility with xterm.h, fontset.h.
   Don't use this definition in mw32 modules.  */
#define FRAME_X_IMAGE_CACHE(F) FRAME_MW32_IMAGE_CACHE(F)


/* Pixel width of the bitmaps drawn to indicate truncation,
   continuation etc.  */

#define FRAME_FLAGS_BITMAP_WIDTH(f)	8
#define FRAME_FLAGS_BITMAP_HEIGHT(f)	8

/* Total width of a areas reserved for drawing truncation bitmaps,
   continuation bitmaps and alike.  The width is in canonical char
   units of the frame.  This must currently be the case because window
   sizes aren't pixel values.  If it weren't the case, we wouldn't be
   able to split windows horizontally nicely.  */

#define FRAME_MW32_FLAGS_AREA_COLS(F)					\
     ((2 * FRAME_FLAGS_BITMAP_WIDTH ((F)) + CANON_X_UNIT ((F)) - 1)	\
      / CANON_X_UNIT ((F)))

/* Total width of flags areas in pixels.  */

#define FRAME_MW32_FLAGS_AREA_WIDTH(F) \
     (FRAME_MW32_FLAGS_AREA_COLS ((F)) * CANON_X_UNIT ((F)))

/* Pixel-width of the left flags area.  */

#define FRAME_MW32_LEFT_FLAGS_AREA_WIDTH(F) \
     (FRAME_MW32_FLAGS_AREA_WIDTH (F) / 2)

/* Pixel-width of the right flags area.  Note that we are doing
   integer arithmetic here, so don't loose a pixel if the total
   width is an odd number.  */

#define FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH(F) 	\
     (FRAME_MW32_FLAGS_AREA_WIDTH (F) - FRAME_MW32_FLAGS_AREA_WIDTH (F) / 2)

/* These macro definitions are for compatibility with frame.h,
   Don't use these in mw32 modules.  */
#define FRAME_X_FLAGS_AREA_COLS FRAME_MW32_FLAGS_AREA_COLS
#define FRAME_X_FLAGS_AREA_WIDTH FRAME_MW32_FLAGS_AREA_WIDTH
#define FRAME_X_LEFT_FLAGS_AREA_WIDTH FRAME_MW32_LEFT_FLAGS_AREA_WIDTH
#define FRAME_X_RIGHT_FLAGS_AREA_WIDTH FRAME_MW32_RIGHT_FLAGS_AREA_WIDTH



/* X-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar
{
  /* These fields are shared by all vectors.  */
  EMACS_INT size_from_Lisp_Vector_struct;
  struct Lisp_Vector *next_from_Lisp_Vector_struct;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* The X window representing this scroll bar.  Since this is a full
     32-bit quantity, we store it split into two 32-bit values.  */
  Lisp_Object mw32_window_low, mw32_window_high;

  /* The position and size of the scroll bar in pixels, relative to the
     frame.  */
  Lisp_Object top, left, width, height;

  /* The starting and ending positions of the handle, relative to the
     handle area (i.e. zero is the top position, not
     SCROLL_BAR_TOP_BORDER).  If they're equal, that means the handle
     hasn't been drawn yet.

     These are not actually the locations where the beginning and end
     are drawn; in order to keep handles from becoming invisible when
     editing large files, we establish a minimum height by always
     drawing handle bottoms VERTICAL_SCROLL_BAR_MIN_HANDLE pixels below
     where they would be normally; the bottom and top are in a
     different co-ordinate system.  */
  Lisp_Object start, end;

  /* If the scroll bar handle is currently being dragged by the user,
     this is the number of pixels from the top of the handle to the
     place where the user grabbed it.  If the handle isn't currently
     being dragged, this is Qnil.  */
  Lisp_Object dragging;
};

/* The number of elements a vector holding a struct scroll_bar needs.  */
#define SCROLL_BAR_VEC_SIZE					\
  ((sizeof (struct scroll_bar)					\
    - sizeof (EMACS_INT) - sizeof (struct Lisp_Vector *))	\
   / sizeof (Lisp_Object))

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))


/* Building a 32-bit C integer from two 16-bit lisp integers.  */
#define SCROLL_BAR_PACK(low, high) (XINT (high) << 16 | XINT (low))

/* Setting two lisp integers to the low and high words of a 32-bit C int.  */
#define SCROLL_BAR_UNPACK(low, high, int32) \
  (XSETINT ((low),   (int32)        & 0xffff), \
   XSETINT ((high), ((int32) >> 16) & 0xffff))


/* Extract the MW32 window id of the scroll bar from a struct scroll_bar.  */
#define SCROLL_BAR_MW32_WINDOW(ptr) \
  ((HWND) SCROLL_BAR_PACK ((ptr)->mw32_window_low, (ptr)->mw32_window_high))

/* Store a window id in a struct scroll_bar.  */
#define SET_SCROLL_BAR_MW32_WINDOW(ptr, id) \
  (SCROLL_BAR_UNPACK ((ptr)->mw32_window_low, (ptr)->mw32_window_high, (int) id))

/* Return the inside width of a vertical scroll bar, given the outside
   width.  */
#define VERTICAL_SCROLL_BAR_INSIDE_WIDTH(f, width) \
  ((width) \
   - VERTICAL_SCROLL_BAR_LEFT_BORDER \
   - VERTICAL_SCROLL_BAR_RIGHT_BORDER \
   - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2)

/* Return the length of the rectangle within which the top of the
   handle must stay.  This isn't equivalent to the inside height,
   because the scroll bar handle has a minimum height.  

   This is the real range of motion for the scroll bar, so when we're
   scaling buffer positions to scroll bar positions, we use this, not
   VERTICAL_SCROLL_BAR_INSIDE_HEIGHT.  */
#if 0 // MW32 implementation fix scroll bar range.
#define VERTICAL_SCROLL_BAR_TOP_RANGE(f, height) \
  (VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, height) - VERTICAL_SCROLL_BAR_MIN_HANDLE)
#else
#define VERTICAL_SCROLL_BAR_TOP_RANGE(f, height) 0xffff
#endif


/* Return the inside height of vertical scroll bar, given the outside
   height.  See VERTICAL_SCROLL_BAR_TOP_RANGE too.  */
#define VERTICAL_SCROLL_BAR_INSIDE_HEIGHT(f, height) \
  ((height) - VERTICAL_SCROLL_BAR_TOP_BORDER - VERTICAL_SCROLL_BAR_BOTTOM_BORDER)


/* Border widths for scroll bars.

   Scroll bar windows don't have any X borders; their border width is
   set to zero, and we redraw borders ourselves.  This makes the code
   a bit cleaner, since we don't have to convert between outside width
   (used when relating to the rest of the screen) and inside width
   (used when sizing and drawing the scroll bar window itself).

   The handle moves up and down/back and forth in a rectangle inset
   from the edges of the scroll bar.  These are widths by which we
   inset the handle boundaries from the scroll bar edges.  */
#define VERTICAL_SCROLL_BAR_LEFT_BORDER (2)
#define VERTICAL_SCROLL_BAR_RIGHT_BORDER (2)
#define VERTICAL_SCROLL_BAR_TOP_BORDER (2)
#define VERTICAL_SCROLL_BAR_BOTTOM_BORDER (2)

/* Minimum lengths for scroll bar handles, in pixels.  */
#define VERTICAL_SCROLL_BAR_MIN_HANDLE (5)

/* Trimming off a few pixels from each side prevents
   text from glomming up against the scroll bar */
#define VERTICAL_SCROLL_BAR_WIDTH_TRIM (0)


/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

   Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */
#define CHAR_TO_PIXEL_ROW(f, row) \
  ((f)->output_data.mw32->internal_border_width \
   + (row) * (f)->output_data.mw32->line_height)
#define CHAR_TO_PIXEL_COL(f, col) \
  ((f)->output_data.mw32->internal_border_width \
   + (col) * FONT_WIDTH ((f)->output_data.mw32->font))

/* Return the pixel width/height of frame F if it has
   WIDTH columns/HEIGHT rows.  */
#define CHAR_TO_PIXEL_WIDTH(f, width) \
  (CHAR_TO_PIXEL_COL (f, width) \
   + (f)->output_data.mw32->vertical_scroll_bar_extra \
   + (f)->output_data.mw32->flags_areas_extra \
   + (f)->output_data.mw32->internal_border_width)
#define CHAR_TO_PIXEL_HEIGHT(f, height) \
  (CHAR_TO_PIXEL_ROW (f, height) \
   + (f)->output_data.mw32->internal_border_width)


/* Return the row/column (zero-based) of the character cell containing 
   the pixel on FRAME at ROW/COL.  */
#define PIXEL_TO_CHAR_ROW(f, row) \
  (((row) - (f)->output_data.mw32->internal_border_width) \
   / (f)->output_data.mw32->line_height)
#define PIXEL_TO_CHAR_COL(f, col) \
  (((col) - (f)->output_data.mw32->internal_border_width) \
   / FONT_WIDTH ((f)->output_data.mw32->font))

/* How many columns/rows of text can we fit in WIDTH/HEIGHT pixels on
   frame F?  */
#define PIXEL_TO_CHAR_WIDTH(f, width) \
  (PIXEL_TO_CHAR_COL (f, ((width) \
			  - (f)->output_data.mw32->internal_border_width \
			  - (f)->output_data.mw32->flags_areas_extra \
			  - (f)->output_data.mw32->vertical_scroll_bar_extra)))
#define PIXEL_TO_CHAR_HEIGHT(f, height) \
  (PIXEL_TO_CHAR_ROW (f, ((height) \
			  - (f)->output_data.mw32->internal_border_width)))


/* Message definitions */

#define WM_EMACS_PRIVATE_START (WM_USER + 2000)
#define WM_EMACS_PRIVATE_END (WM_USER + 3000)

#define IS_EMACS_PRIVATE_MESSAGE(message)    \
  (((message) >= WM_EMACS_PRIVATE_START) &&  \
   ((message) < WM_EMACS_PRIVATE_END))

#define WM_EMACS_SIZE                              (WM_USER+2000)
#define WM_EMACS_MOVE                              (WM_USER+2001)
#define WM_EMACS_DESTROY                           (WM_USER+2002)
#define WM_EMACS_ACTIVATE                          (WM_USER+2003)
#define WM_EMACS_CLOSE_CONNECTION                  (WM_USER+2004)
#define WM_EMACS_CREATE_FRAME                      (WM_USER+2005) 
#define WM_EMACS_POPUP_MENU                        (WM_USER+2006)
#define WM_EMACS_DESTROY_FRAME                     (WM_USER+2007)
#define WM_EMACS_FLASH_WINDOW                      (WM_USER+2008)
#define WM_EMACS_SETCARET                          (WM_USER+2009)
#define WM_EMACS_CLEAR_MOUSE_FACE                  (WM_USER+2010)
#define WM_EMACS_CREATE_TIP_FRAME                  (WM_USER+2011) 
#define WM_EMACS_SETFOREGROUND                     (WM_USER+2012)
/* to report "switch-buffer" event by himi */
#define WM_IME_REPORT     0x0280
#define IR_STRING         0x140
#define IR_OPENCONVERT    0x120
#define IR_CLOSECONVERT   0x122

#define IDM_UNDO 100
#ifdef W32_SCROLLBAR
#define WM_EMACS_VSCROLL                           (WM_USER+2020)
#define WM_EMACS_CREATE_SCROLLBAR                  (WM_USER+2021)
#define WM_EMACS_DESTROY_SCROLLBAR                 (WM_USER+2022)
#endif
#define WM_EMACS_NOTE_MOUSE_MOVEMENT               (WM_USER+2023)
#define WM_EMACS_TOOL_BAR_UP                       (WM_USER+2024)
#define WM_EMACS_TOOL_BAR_DOWN                     (WM_USER+2025)
#define WM_EMACS_GET_IME_FONT_PROP                 (WM_USER+2026)
#define WM_EMACS_MODIFY_IME_FONT_PROP              (WM_USER+2027)

#define WM_EMACS_CREATE_FRAME_REPLY                (WM_USER+2050)
#define WM_EMACS_CREATE_SCROLLBAR_REPLY            (WM_USER+2051)
#define WM_EMACS_DESTROY_FRAME_REPLY               (WM_USER+2052)
#define WM_EMACS_POPUP_MENU_REPLY                  (WM_USER+2053)
#define WM_EMACS_SETLOCALE                         (WM_USER+2054)
#define WM_EMACS_SETKEYBOARDLAYOUT                 (WM_USER+2055)
#define WM_EMACS_REGISTER_HOT_KEY                  (WM_USER+2056)
#define WM_EMACS_UNREGISTER_HOT_KEY                (WM_USER+2057)
#define WM_EMACS_CREATE_TIP_FRAME_REPLY            (WM_USER+2058)


#if 0 /* To be removed. */
#define WND_FONTWIDTH_INDEX    (0) 
#define WND_LINEHEIGHT_INDEX   (4) 
#define WND_BORDER_INDEX       (8) 
#define WND_SCROLLBAR_INDEX    (12) 
#define WND_BACKGROUND_INDEX   (16) 
#define WND_LAST_INDEX         (20)

#define WND_EXTRA_BYTES     (WND_LAST_INDEX)
#endif

/* This method guarantee message reachability.
   But you must deal with any messages sent by this method in
   window procedure.  You cannot deal with them in the thread
   message loop(W32read_socket). */
#define SEND_INFORM_MESSAGE(window, message, wparam, lparam)     \
  (SendMessage ((window), (message), (wparam), (lparam)))

/*
  You should avoid using this method to send message to the
  message thread if possible.  Although this method guarantee
  message reachablity even when message thread has no window,
  you must deal with any messages sent by this method
  both in the thread message loop(W32read_socket) and
  in the window procedure(normally w32_WndProc).
*/
#define SEND_MSGTHREAD_INFORM_MESSAGE(message, wparam, lparam)       \
  do {                                                               \
    if (mw32_frame_window != INVALID_HANDLE_VALUE)                    \
      SendMessage(mw32_frame_window, (message), (wparam), (lparam));  \
    else                                                             \
      while (!PostThreadMessage (msg_thread_id, (message),           \
                                 (wparam), (lparam)))                \
        sleep(1);                                                    \
  }while(0)

#ifdef IME_CONTROL
extern void w32_ime_create_agent();
#endif

extern int mw32_process_main_thread_message(MSG *pwait_msg);

/* This method does NOT guarantee message reachability
   if its destination is the message thread. */
#define POST_THREAD_INFORM_MESSAGE(thread, message, wparam, lparam)    \
  while (!PostThreadMessage ((thread), (message), (wparam), (lparam))) \
    sleep(1)

/* This method does NOT guarantee message reachability in any case. */
#define POST_INFORM_MESSAGE(window, message, wparam, lparam)      \
  while (!PostMessage ((window), (message), (wparam), (lparam)))  \
    sleep(1)

#define WAIT_REPLY_MESSAGE(ret, msgno)                           \
  (((ret)->message = msgno), mw32_process_main_thread_message(ret))

#ifdef W32_INTELLIMOUSE
#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL 0x20A
#endif
#ifndef WHEEL_DELTA
#define WHEEL_DELTA 120
#endif
#ifndef SPI_GETWHEELSCROLLLINES
#define SPI_GETWHEELSCROLLLINES 104
#endif
#ifndef WHEEL_PAGESCROLL
#define WHEEL_PAGESCROLL (0xffffffff)
#endif
#endif

#ifndef VK_KANJI
#define VK_KANJI 0x19
#endif

#if defined(__MINGW32__) && defined(VK_KANA) && VK_KANA != 0x15
/* Cygwin header file defines a wrong virtual key code as VK_KANA!  */
#undef VK_KANA
#endif

#ifndef VK_KANA
#define VK_KANA  0x15
#endif
#define VK_COMPEND 0x1A
#define WM_MEADOW_CREATE_WINDOW    (WM_USER+2100)
#define WM_MEADOW_CREATE_SCROLLBAR (WM_USER+2101)

/* For internal communications
   from window procedure to event loop. */
#define WM_MULE_IME_REPORT         (WM_USER+2200)
#define WM_MULE_IME_STATUS         (WM_USER+2201)

#ifdef IME_CONTROL
/* For internal communications
   from main thread to window procedure. */
#define WM_MULE_IMM_MESSAGE_START             (WM_USER+2300)
#define WM_MULE_IMM_SET_STATUS                (WM_USER+2300)
#define WM_MULE_IMM_GET_STATUS                (WM_USER+2301)
#define WM_MULE_IMM_DEAL_WITH_CONTEXT         (WM_USER+2302)
#define WM_MULE_IMM_SET_COMPOSITION_STRING    (WM_USER+2303)
#define WM_MULE_IMM_GET_COMPOSITION_STRING    (WM_USER+2304)
#define WM_MULE_IMM_SET_MODE                  (WM_USER+2305)
#define WM_MULE_IMM_NOTIFY                    (WM_USER+2310)
#define WM_MULE_IMM_GET_UNDETERMINED_STRING_LENGTH (WM_USER+2320)
#define WM_MULE_IMM_MESSAGE_END               (WM_USER+2399)
#define MESSAGE_IMM_COM_P(message)              \
  (((message) >= WM_MULE_IMM_MESSAGE_START) &&  \
   ((message) <= WM_MULE_IMM_MESSAGE_END))

/* For synchronization
   to create conversion agent
   between main thread and event loop. */
#define WM_MULE_IME_CREATE_AGENT        (WM_USER+2400)
#define WM_MULE_IME_CREATE_AGENT_REPLY  (WM_USER+2401)
#define WM_MULE_IME_DESTROY_AGENT       (WM_USER+2402)
#define WM_MULE_IME_DESTROY_AGENT_REPLY (WM_USER+2403)
#define CONVAGENT_CLASS "ConvAgent"

#define WM_MULE_IMM_SET_COMPOSITION_FONT       (WM_USER+2404)
#define WM_MULE_IMM_SET_COMPOSITION_FONT_REPLY (WM_USER+2405)

#define WM_MULE_IMM_SET_CONVERSION_WINDOW       (WM_USER+2406)
#define WM_MULE_IMM_SET_CONVERSION_WINDOW_REPLY (WM_USER+2407)
#endif

/* Message Handling thread & its id.  */
extern HANDLE msg_thread;
extern DWORD msg_thread_id;

/* Main thread & its id. */
extern HANDLE main_thread;
extern DWORD main_thread_id;

/* flag if mw32term is blocked in message loop. */
extern int message_loop_blocked_p;

/* Return true if the current thread is main thread.  */
#define MW32_MAIN_THREAD_P() (main_thread_id == GetCurrentThreadId())

/* If the system has already created frame, this variable is set to
   its window handle.  This variable is used for sending message to
   window procedure. */
extern HWND mw32_frame_window;
extern Lisp_Object Vw32_system_coding_system;

struct coding_system;

extern int mw32_encode_text_prepare (Lisp_Object coding_system,
				     struct coding_system *coding,
				     int bytes);

#define MW32_ENCODE_TEXT(string, lcs, to_text, size)		\
do {								\
  struct coding_system coding;					\
  int bufsize;							\
  int bytes = LISPY_STRING_BYTES (string);			\
  bufsize = mw32_encode_text_prepare((lcs), &coding, bytes);	\
  *(to_text) = alloca (bufsize);				\
  encode_coding (&coding, XSTRING (string)->data,		\
                 *(to_text), bytes, bufsize);			\
  *(size) = coding.produced;					\
  (*(to_text))[coding.produced] = '\0';				\
} while (0)

#define MW32_ENCODE_CSTR(cstr, lcs, tostr, size)		\
do {								\
  struct coding_system coding;					\
  int bufsize;							\
  int bytes = strlen(cstr);					\
  bufsize = mw32_encode_text_prepare((lcs), &coding, bytes);	\
  *(tostr) = alloca (bufsize + 1);				\
  encode_coding (&coding, (cstr),				\
                 *(tostr), bytes, bufsize);			\
  *(size) = coding.produced;					\
  (*(tostr))[coding.produced] = '\0';				\
} while (0)

#define LISPY_STRING_BYTES(str) (STRING_BYTES(XSTRING(str)))
#define LISPY_STRING_CHARS(str) (XSTRING(str)->size)

extern LPTSTR mw32_encode_lispy_string (Lisp_Object coding_system,
					Lisp_Object str,
					int *psize);
extern Lisp_Object mw32_decode_lispy_string (Lisp_Object coding_system,
					     LPTSTR cstr, int size);

/* Macro for Emacs event initialization */
/* 
   `frame_or_window' and `arg' members in an emacs event will be
   protected from GC by kbd_buffer_store_event().  So uninit value
   may cause GC fault.  This macro assures initiazliation of emacs event.
 */
#define MW32_INIT_EMACS_EVENT(ee) ((ee).frame_or_window = (ee).arg = Qnil)


/* misc. functions */


/* to keep data type compatibility (mainly for xfaces.h). */

#ifndef HAVE_X_WINDOWS
typedef struct {
  unsigned long pixel;
  unsigned short red, green, blue;
  char flags;
  char pad;
} XColor;
#endif


struct window;
struct glyph_matrix;
struct frame;
struct input_event;
struct face;
struct image;

/* From mw32fns.c.  */

Lisp_Object display_mw32_get_resource P_ ((struct mw32_display_info *,
					   Lisp_Object, Lisp_Object,
					   Lisp_Object, Lisp_Object));
struct frame *check_mw32_frame P_ ((Lisp_Object));
EXFUN (Fx_display_color_p, 1);
EXFUN (Fx_display_grayscale_p, 1);
int image_ascent P_ ((struct image *, struct face *));

struct x_display_info;

/**********************
  Defined in mw32term.c
***********************/
/* MW32 xterm compatibility functions. */
extern void x_delete_display P_ ((struct x_display_info *));
extern void x_set_window_size P_ ((struct frame *, int, int, int));
extern void x_make_frame_visible P_ ((struct frame *));
extern void x_set_offset P_ ((struct frame *, int, int, int));
extern void x_iconify_frame P_ ((struct frame *));
extern void x_set_mouse_position P_ ((struct frame *f, int x, int y));
extern void x_set_mouse_pixel_position P_ ((struct frame *f, int pix_x, int pix_y));
extern void x_destroy_window P_ ((FRAME_PTR f));
extern void pixel_to_glyph_coords P_ ((FRAME_PTR f, int pix_x, int pix_y,
				       int *px, int *py, RECT *bounds, int noclip));
/* for mw32fns module interfaces. */
extern void mw32_setup_default_hdc P_ ((HDC hdc));
extern HDC mw32_get_frame_hdc P_ ((struct frame *f));
extern void mw32_destroy_frame_hdc P_ ((struct frame *f));
/* extern void mw32_lower_frame P_ ((struct frame *f)); */
extern void mw32_scroll_bar_clear P_ ((struct frame *));
/* extern void mw32_raise_frame P_ ((struct frame *)); */
extern void mw32_initialize P_ ((void));
extern int mw32_text_icon P_ ((struct frame *, char *));
extern int mw32_bitmap_icon P_ ((struct frame *, Lisp_Object));
/* extern int mw32_alloc_nearest_color P_ ((struct frame *, Colormap, XColor *)); */
extern void mw32_update_cursor P_ ((FRAME_PTR f, int on_p));
extern void mw32_display_and_set_cursor P_ ((struct window *, int, int, int, int, int));
extern void mw32_menu_bar_store_activate_event P_ ((struct frame *f));
extern void mw32_expose_frame P_ ((struct frame *f));
extern void mw32_new_focus_frame P_ ((struct mw32_display_info *, struct frame *));
extern void mw32_free_frame_resources P_((struct frame *f));
extern Lisp_Object mw32_new_font P_((struct frame *f, char *fontname));
extern Lisp_Object mw32_new_fontset P_((struct frame *f, char *fontsetname));
extern void mw32_delete_display P_ ((struct mw32_display_info *));
extern struct mw32_display_info *mw32_term_init P_ ((Lisp_Object display_name,
						     char *xrm_option,
						     char *resource_name));

/**********************
  Defined in mw32fns.c
***********************/
extern void check_mw32 P_ ((void));
extern void mw32_set_font P_ ((struct frame *f,
			       Lisp_Object arg,
			       Lisp_Object oldval));

/* for frame.c */
extern struct frame *mw32_window_to_frame P_ ((struct mw32_display_info *, HWND));
extern struct frame *mw32_any_window_to_frame P_ ((struct mw32_display_info *, HWND));
extern void x_set_frame_parameters P_ ((FRAME_PTR f, Lisp_Object alist));
extern void x_report_frame_params P_ ((FRAME_PTR f, Lisp_Object *alistptr));
extern Lisp_Object x_get_focus_frame P_ ((struct frame *frame));
extern void x_sync P_ ((FRAME_PTR f));
extern int x_pixel_width P_ ((struct frame *f));
extern int x_pixel_height P_ ((struct frame *f));
extern int x_char_width P_ ((struct frame *f));
extern int x_char_height P_ ((struct frame *f));
extern int x_screen_planes P_ ((struct frame *f));
extern void x_sync P_ ((struct frame *f));
/* for xfaces.c */
extern int x_defined_color P_ ((FRAME_PTR f, char *color_name,
				XColor *color_def, int alloc_p));
extern int x_bitmap_height P_ ((FRAME_PTR f, int id));
extern int x_bitmap_width P_ ((FRAME_PTR f, int id));
extern int x_create_bitmap_from_data P_ ((FRAME_PTR f, char *bits,
					  unsigned int width, unsigned int height));
extern int x_create_bitmap_from_file P_ ((FRAME_PTR f, Lisp_Object file));
extern void x_destroy_bitmap P_ ((FRAME_PTR f, int id));
/* for window.c */
extern void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
extern void x_set_tool_bar_lines P_ ((FRAME_PTR f, Lisp_Object value, Lisp_Object oldval));
/* xdisp.c (note that the name is replaced by "define". */
#define x_implicitly_set_name mw32_implicitly_set_name
void mw32_implicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
/* mw32term.c */
extern struct image_cache *make_image_cache ();
extern void mw32_reference_bitmap P_ ((FRAME_PTR f, int id));
extern enum text_cursor_kinds mw32_specified_cursor_type P_ ((Lisp_Object arg, int *width));

/**********************
  Defined in xfaces.c
***********************/
extern int frame_update_line_height P_ ((struct frame *));
extern int compute_glyph_face P_ ((struct frame *, int, int));
extern int compute_glyph_face_1 P_ ((struct frame *, Lisp_Object, int));
extern int defined_color P_ ((struct frame *, char *, XColor *, int));

/**********************
  Defined in mw32menu.c
***********************/
extern void free_frame_menubar ();
extern void x_activate_menubar P_ ((struct frame *));
extern int popup_activated P_ ((void));
extern void initialize_frame_menubar P_ ((struct frame *));

/**********************
  Defined in mw32ime.c
***********************/
extern void mw32_ime_control_init P_ ((void));
extern void mw32_set_ime_conv_window P_ ((HWND hwnd, int x, int y));

/***********************************
  TODO!!!
**********************************/

#define lock_mouse_cursor_visible(flag) ((void) 0)

/* When compiling on Windows 9x/ME and NT 3.x, the following are not defined
   (even though they are supported on 98 and ME.  */
#ifndef WM_MOUSELEAVE
#define WM_MOUSELEAVE 0x02A3
#endif

#ifndef TME_LEAVE
#define TME_LEAVE 0x00000002;

typedef struct tagTRACKMOUSEEVENT
{
  DWORD cbSize;
  DWORD dwFlags;
  HWND hwndTrack;
  DWORD dwHoverTime;
} TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT;
#endif

#ifndef SPI_GETFOREGROUNDLOCKTIMEOUT
#define SPI_GETFOREGROUNDLOCKTIMEOUT        0x2000
#endif

#ifndef SPI_SETFOREGROUNDLOCKTIMEOUT
#define SPI_SETFOREGROUNDLOCKTIMEOUT        0x2001
#endif

#endif /* not _MW32TERM_H_ */
