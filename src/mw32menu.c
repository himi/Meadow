/* W32 Communication module for menu.
   Copyright (C) 1986, 1988, 1993, 1994, 1996 Free Software Foundation, Inc.

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

/* X pop-up deck-of-cards menu facility for gnuemacs.
 *
 * Written by Jon Arnold and Roman Budzianowski
 * Mods and rewrite by Robert Krawitz
 *
 */

/* Modified by Fred Pierresteguy on December 93
   to make the popup menus and menubar use the Xt.  */

/* Rewritten for clarity and GC protection by rms in Feb 94.  */

/* made by modifying xmenu.c  by H.Miyashita. */

#include <signal.h>
#include <config.h>

#include <stdio.h>
#include "lisp.h"
#include "termhooks.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "blockinput.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"

#include "dispextern.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "mw32term.h"

/* Local memory management */
#define local_heap (GetProcessHeap ())
#define local_alloc(n) (HeapAlloc (local_heap, HEAP_ZERO_MEMORY, (n)))
#define local_free(p) (HeapFree (local_heap, 0, ((LPVOID) (p))))

#define malloc_widget_value() ((widget_value *) local_alloc (sizeof (widget_value)))
#define free_widget_value(wv) (local_free ((wv)))

/******************************************************************/
#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

static HMENU current_popup_menu;

typedef BOOL (WINAPI * GetMenuItemInfoA_Proc) (
    IN HMENU,
    IN UINT,
    IN BOOL,
    IN OUT LPMENUITEMINFOA
    );
typedef BOOL (WINAPI * SetMenuItemInfoA_Proc) (
    IN HMENU,
    IN UINT,
    IN BOOL,
    IN LPCMENUITEMINFOA
    );

GetMenuItemInfoA_Proc get_menu_item_info = NULL;
SetMenuItemInfoA_Proc set_menu_item_info = NULL;

Lisp_Object Vmenu_updating_frame;

Lisp_Object Qdebug_on_next_call;

extern Lisp_Object Qmenu_bar;
extern Lisp_Object Qmouse_click, Qevent_kind;

extern Lisp_Object QCtoggle, QCradio;

extern Lisp_Object Voverriding_local_map;
extern Lisp_Object Voverriding_local_map_menu_flag;

extern Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;

extern Lisp_Object Qmenu_bar_update_hook;

#ifdef USE_X_TOOLKIT
extern void set_frame_menubar ();
extern void process_expose_from_menu ();
extern XtAppContext Xt_app_con;

static Lisp_Object xdialog_show ();
void popup_get_selection ();
#else
#ifdef HAVE_NTGUI
extern void set_frame_menubar ();
#endif
#endif

static Lisp_Object xmenu_show ();
static void keymap_panes ();
static void single_keymap_panes ();
static void single_menu_item ();
static void list_of_panes ();
static void list_of_items ();
static Lisp_Object mw32menu_show ();
static void mw32_free_menu_strings (HWND);

/* This holds a Lisp vector that holds the results of decoding
   the keymaps or alist-of-alists that specify a menu.

   It describes the panes and items within the panes.

   Each pane is described by 3 elements in the vector:
   t, the pane name, the pane's prefix key.
   Then follow the pane's items, with 5 elements per item:
   the item string, the enable flag, the item's value,
   the definition, and the equivalent keyboard key's description string.

   In some cases, multiple levels of menus may be described.
   A single vector slot containing nil indicates the start of a submenu.
   A single vector slot containing lambda indicates the end of a submenu.
   The submenu follows a menu item which is the way to reach the submenu.

   A single vector slot containing quote indicates that the
   following items should appear on the right of a dialog box.

   Using a Lisp vector to hold this information while we decode it
   takes care of protecting all the data from GC.  */

#define MENU_ITEMS_PANE_NAME 1
#define MENU_ITEMS_PANE_PREFIX 2
#define MENU_ITEMS_PANE_LENGTH 3

#define MENU_ITEMS_ITEM_NAME 0
#define MENU_ITEMS_ITEM_ENABLE 1
#define MENU_ITEMS_ITEM_VALUE 2
#define MENU_ITEMS_ITEM_EQUIV_KEY 3
#define MENU_ITEMS_ITEM_DEFINITION 4
#define MENU_ITEMS_ITEM_TYPE 5
#define MENU_ITEMS_ITEM_SELECTED 6
#define MENU_ITEMS_ITEM_HELP 7
#define MENU_ITEMS_ITEM_LENGTH 8

static Lisp_Object menu_items;

/* Number of slots currently allocated in menu_items.  */
static int menu_items_allocated;

/* This is the index in menu_items of the first empty slot.  */
static int menu_items_used;

/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
static int menu_items_n_panes;

/* Current depth within submenus.  */
static int menu_items_submenu_depth;

/* Flag which when set indicates a dialog or menu has been posted.
   On MW32, this flag is always 0 right now.  */
static int popup_activated_flag;

#ifdef USE_X_TOOLKIT
static int next_menubar_widget_id;
#endif

/* This is set nonzero after the user activates the menu bar, and set
   to zero again after the menu bars are redisplayed by prepare_menu_bar.
   While it is nonzero, all calls to set_frame_menubar go deep.

   I don't understand why this is needed, but it does seem to be
   needed on Motif, according to Marcus Daniels <marcus@sysc.pdx.edu>.  */

int pending_menu_activation;

#ifdef USE_X_TOOLKIT

/* Return the frame whose ->output_data.x->id equals ID, or 0 if none.  */

static struct frame *
menubar_id_to_frame (id)
     LWLIB_ID id;
{
  Lisp_Object tail, frame;
  FRAME_PTR f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (f->output_data.nothing == 1)
	continue;
      if (f->output_data.x->id == id)
	return f;
    }
  return 0;
}

#endif

/* Initialize the menu_items structure if we haven't already done so.
   Also mark it as currently empty.  */

static void
init_menu_items ()
{
  if (NILP (menu_items))
    {
      menu_items_allocated = 60;
      menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
    }

  menu_items_used = 0;
  menu_items_n_panes = 0;
  menu_items_submenu_depth = 0;
}

/* Call at the end of generating the data in menu_items.
   This fills in the number of items in the last pane.  */

static void
finish_menu_items ()
{
}

/* Call when finished using the data for the current menu
   in menu_items.  */

static void
discard_menu_items ()
{
  /* Free the structure if it is especially large.
     Otherwise, hold on to it, to save time.  */
  if (menu_items_allocated > 200)
    {
      menu_items = Qnil;
      menu_items_allocated = 0;
    }
}

/* Make the menu_items vector twice as large.  */

static void
grow_menu_items ()
{
  Lisp_Object old;
  int old_size = menu_items_allocated;
  old = menu_items;

  menu_items_allocated *= 2;
  menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
  bcopy (XVECTOR (old)->contents, XVECTOR (menu_items)->contents,
	 old_size * sizeof (Lisp_Object));
}

/* Begin a submenu.  */

static void
push_submenu_start ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qnil;
  menu_items_submenu_depth++;
}

/* End a submenu.  */

static void
push_submenu_end ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qlambda;
  menu_items_submenu_depth--;
}

/* Indicate boundary between left and right.  */

static void
push_left_right_boundary ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qquote;
}

/* Start a new menu pane in menu_items..
   NAME is the pane name.  PREFIX_VEC is a prefix key for this pane.  */

static void
push_menu_pane (name, prefix_vec)
     Lisp_Object name, prefix_vec;
{
  if (menu_items_used + MENU_ITEMS_PANE_LENGTH > menu_items_allocated)
    grow_menu_items ();

  if (menu_items_submenu_depth == 0)
    menu_items_n_panes++;
  XVECTOR (menu_items)->contents[menu_items_used++] = Qt;
  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = prefix_vec;
}

/* Push one menu item into the current pane.
   NAME is the string to display.  ENABLE if non-nil means
   this item can be selected.  KEY is the key generated by
   choosing this item, or nil if this item doesn't really have a definition.
   DEF is the definition of this item.
   EQUIV is the textual description of the keyboard equivalent for
   this item (or nil if none).  */

static void
push_menu_item (name, enable, key, def, equiv, type, selected, help)
     Lisp_Object name, enable, key, def, equiv, type, selected, help;
{
  if (menu_items_used + MENU_ITEMS_ITEM_LENGTH > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = enable;
  XVECTOR (menu_items)->contents[menu_items_used++] = key;
  XVECTOR (menu_items)->contents[menu_items_used++] = equiv;
  XVECTOR (menu_items)->contents[menu_items_used++] = def;
  XVECTOR (menu_items)->contents[menu_items_used++] = type;
  XVECTOR (menu_items)->contents[menu_items_used++] = selected;
  XVECTOR (menu_items)->contents[menu_items_used++] = help;
}


static BOOL appendmenu_encode(HMENU hmenu, UINT fuflags, UINT idnewitem,
			      LPSTR name)
{
  int size;
  char *ttext;

  MW32_ENCODE_CSTR(name, Vw32_system_coding_system, &ttext, &size);
  return AppendMenu(hmenu, fuflags, idnewitem, ttext);
}

static int modifymenu_encode(HMENU hmenu, UINT uitem, UINT fuflags,
			     UINT idnewitem, LPSTR name)
{
  char *ttext;
  LPTSTR menustr;
  int size, len, menustrsize, menunum;

  MW32_ENCODE_CSTR(name, Vw32_system_coding_system, &ttext, &size);
  menustr = (LPTSTR) alloca (sizeof(TCHAR) * (size + 1));

  menustrsize = GetMenuString (hmenu, uitem, menustr, size + 1, MF_BYPOSITION);
  if (menustrsize
      && (memcmp (menustr, ttext,
		  sizeof(TCHAR) * menustrsize) == 0))
    return 1;

  menunum = GetMenuItemCount (hmenu);

  if (menunum > uitem)
    {
      DeleteMenu (hmenu, uitem, MF_BYPOSITION);
      if ((menunum - 1) == uitem)
	AppendMenu (hmenu, fuflags, idnewitem, ttext);
      else
	InsertMenu (hmenu, uitem, MF_BYPOSITION | fuflags,
		    idnewitem, ttext);
    }
  else
    AppendMenu (hmenu, fuflags, idnewitem, ttext);

  return 0;
}

static BOOL insertmenu_encode(HMENU hmenu, UINT uitem, UINT fuflags,
			      UINT idnewitem, LPSTR name)
{
  int size;
  char *ttext;

  MW32_ENCODE_CSTR(name, Vw32_system_coding_system, &ttext, &size);
  return InsertMenu(hmenu, uitem, fuflags, idnewitem, ttext);
}

/* Is this item a separator? */
static int
name_is_separator (char *name)
{
  char *start = name;

  /* Check if name string consists of only dashes ('-').  */
  while (*name == '-') name++;
  /* Separators can also be of the form "--:TripleSuperMegaEtched"
     or "--deep-shadow".  We don't implement them yet, se we just treat
     them like normal separators.  */
  return (*name == '\0' || start + 2 == name);
}

/* Appned one menu item into the menu_handle.
   NAME is the string to display.  ENABLE if non-nil means
   this item can be selected.  EQUIV is the textual description
   of the keyboard equivalent for this item (or nil if none).  */
static void 
add_menu_item (hmenu, name, menu_id, enable, description, help)
     HMENU hmenu;
     Lisp_Object name;
     UINT menu_id;
     UINT enable;
     Lisp_Object description;
     Lisp_Object help;
{
  if (NILP(name) || name_is_separator (XSTRING (name)->data))
    AppendMenu (hmenu, MF_SEPARATOR, menu_id, NULL);
  else
    {
      TCHAR *menu_str;
      int size, desclen;
#ifdef MEADOW /* 96.10.12 modified by himi */
      struct coding_system coding;
      extern Lisp_Object Vw32_system_coding_system;
      int decodebufsize;
#endif
      if (STRINGP(description)) 
	desclen = strlen(XSTRING(description) -> data);
      /* Never description set to a size entry... so use 'strlen' */
      else desclen = 0;
	
#ifdef MEADOW
      setup_coding_system(Fcheck_coding_system(Vw32_system_coding_system), &coding);
      decodebufsize = encoding_buffer_size(&coding, LISPY_STRING_BYTES(name) + 1);
      menu_str = alloca(decodebufsize + desclen);
      encode_coding(&coding, XSTRING(name)->data, menu_str,
		    LISPY_STRING_BYTES(name), decodebufsize);
      size = coding.produced;
#else
      size = LISPY_STRING_BYTES(XSTRING(name));
      menu_str = alloca(size + desclen + 1);
      memcpy(menu_str, XSTRING(name) -> data, size);
#endif
      if (desclen)
	memcpy(&menu_str[size], XSTRING(description) -> data, desclen);
      menu_str[size + desclen] = '\0';

#if 0
      printf("%s-%d(Menu_Create) \n", menu_str, menu_id);
#endif

      if (enable)
	AppendMenu (hmenu, MF_STRING, menu_id, menu_str);
      else
	AppendMenu (hmenu, (MF_STRING | MF_GRAYED), menu_id, menu_str);
    }

  /* This must be done after the menu item is created.  */
  if (set_menu_item_info)
    {
      MENUITEMINFO info;
      bzero (&info, sizeof (info));
      info.cbSize = sizeof (info);
      info.fMask = MIIM_DATA;
      
      /* Set help string for menu item.  Leave it as a Lisp_Object
	 until it is ready to be displayed, since GC can happen while
	 menus are active.  */
      if (help)
	info.dwItemData = (DWORD) help;

      set_menu_item_info (hmenu, menu_id, FALSE, &info);
    }
}


/* Look through KEYMAPS, a vector of keymaps that is NMAPS long,
   and generate menu panes for them in menu_items.
   If NOTREAL is nonzero,
   don't bother really computing whether an item is enabled.  */

static void
keymap_panes (keymaps, nmaps, notreal)
     Lisp_Object *keymaps;
     int nmaps;
     int notreal;
{
  int mapno;

  init_menu_items ();

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.
     P is the number of panes we have made so far.  */
  for (mapno = 0; mapno < nmaps; mapno++)
    single_keymap_panes (keymaps[mapno], Qnil, Qnil, notreal, 10);

  finish_menu_items ();
}

/* This is a recursive subroutine of keymap_panes.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.
   If NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate expressions in menu items and don't make any menu.

   If we encounter submenus deeper than MAXDEPTH levels, ignore them.  */

static void
single_keymap_panes (keymap, pane_name, prefix, notreal, maxdepth)
     Lisp_Object keymap;
     Lisp_Object pane_name;
     Lisp_Object prefix;
     int notreal;
     int maxdepth;
{
  Lisp_Object pending_maps = Qnil;
  Lisp_Object tail, item;
  struct gcpro gcpro1, gcpro2;
  int notbuttons = 0;

  if (maxdepth <= 0)
    return;

  push_menu_pane (pane_name, prefix);

#ifndef HAVE_BOXES
  /* Remember index for first item in this pane so we can go back and
     add a prefix when (if) we see the first button.  After that, notbuttons
     is set to 0, to mark that we have seen a button and all non button
     items need a prefix.  */
  notbuttons = menu_items_used;
#endif

  for (tail = keymap; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      GCPRO2 (keymap, pending_maps);
      /* Look at each key binding, and if it is a menu item add it
	 to this menu.  */
      item = XCONS (tail)->car;
      if (CONSP (item))
	single_menu_item (XCONS (item)->car, XCONS (item)->cdr,
			  &pending_maps, notreal, maxdepth, &notbuttons);
      else if (VECTORP (item))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = XVECTOR (item)->size;
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      single_menu_item (character, XVECTOR (item)->contents[c],
				&pending_maps, notreal, maxdepth, &notbuttons);
	    }
	}
      UNGCPRO;
    }

  /* Process now any submenus which want to be panes at this level.  */
  while (!NILP (pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = Fcar (pending_maps);
      eltcdr = XCONS (elt)->cdr;
      string = XCONS (eltcdr)->car;
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in xmenu_show.  */
      single_keymap_panes (Fcar (elt), string,
			   XCONS (eltcdr)->cdr, notreal, maxdepth - 1);
      pending_maps = Fcdr (pending_maps);
    }
}

/* This is a subroutine of single_keymap_panes that handles one
   keymap entry.
   KEY is a key in a keymap and ITEM is its binding. 
   PENDING_MAPS_PTR points to a list of keymaps waiting to be made into
   separate panes.
   If NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate expressions in menu items and don't make any menu.
   If we encounter submenus deeper than MAXDEPTH levels, ignore them.
   NOTBUTTONS_PTR is only used when simulating toggle boxes and radio
   buttons.  It points to variable notbuttons in single_keymap_panes,
   which keeps track of if we have seen a button in this menu or not.  */

static void
single_menu_item (key, item, pending_maps_ptr, notreal, maxdepth,
		  notbuttons_ptr)
     Lisp_Object key, item;
     Lisp_Object *pending_maps_ptr;
     int maxdepth, notreal;
     int *notbuttons_ptr;
{
  Lisp_Object def, map, item_string, enabled;
  struct gcpro gcpro1, gcpro2;
  int res;
  
  /* Parse the menu item and leave the result in item_properties.  */
  GCPRO2 (key, item);
  res = parse_menu_item (item, notreal, 0);
  UNGCPRO;
  if (!res)
    return;			/* Not a menu item.  */

  map = XVECTOR (item_properties)->contents[ITEM_PROPERTY_MAP];
  
  if (notreal)
    {
      /* We don't want to make a menu, just traverse the keymaps to
	 precompute equivalent key bindings.  */
      if (!NILP (map))
	single_keymap_panes (map, Qnil, key, 1, maxdepth - 1);
      return;
    }

  enabled = XVECTOR (item_properties)->contents[ITEM_PROPERTY_ENABLE];
  item_string = XVECTOR (item_properties)->contents[ITEM_PROPERTY_NAME]; 

  if (!NILP (map) && XSTRING (item_string)->data[0] == '@')
    {
      if (!NILP (enabled))
	/* An enabled separate pane. Remember this to handle it later.  */
	*pending_maps_ptr = Fcons (Fcons (map, Fcons (item_string, key)),
				   *pending_maps_ptr);
      return;
    }

#ifndef HAVE_BOXES
  /* Simulate radio buttons and toggle boxes by putting a prefix in
     front of them.  */
  {
    Lisp_Object prefix = Qnil;
    Lisp_Object type = XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE];
    if (!NILP (type))
      {
	Lisp_Object selected
	  = XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED];

	if (*notbuttons_ptr)
	  /* The first button. Line up previous items in this menu.  */
	  {
	    int index = *notbuttons_ptr; /* Index for first item this menu.  */
	    int submenu = 0;
	    Lisp_Object tem;
	    while (index < menu_items_used)
	      {
		tem
		  = XVECTOR (menu_items)->contents[index + MENU_ITEMS_ITEM_NAME];
		if (NILP (tem))
		  {
		    index++;
		    submenu++;		/* Skip sub menu.  */
		  }
		else if (EQ (tem, Qlambda))
		  {
		    index++;
		    submenu--;		/* End sub menu.  */
		  }
		else if (EQ (tem, Qt))
		  {
		    int i = (index + MENU_ITEMS_ITEM_NAME
			     + MENU_ITEMS_PANE_NAME);
		    tem = AREF (menu_items, i);
		    if (SREF (tem, 0) != '\0')
		      ASET (menu_items, i,
			    concat2 (build_string ("    "), tem));
		    index += MENU_ITEMS_PANE_LENGTH; /* Skip new pane marker. */
		  }
		else if (EQ (tem, Qquote))
		  index++;		/* Skip a left, right divider. */
		else
		  {
		    if (!submenu && XSTRING (tem)->data[0] != '\0'
			&& XSTRING (tem)->data[0] != '-')
		      XVECTOR (menu_items)->contents[index + MENU_ITEMS_ITEM_NAME]
			= concat2 (build_string ("    "), tem);
		    index += MENU_ITEMS_ITEM_LENGTH;
		  }
	      }
	    *notbuttons_ptr = 0;
	  }

	/* Calculate prefix, if any, for this item.  */
	if (EQ (type, QCtoggle))
	  prefix = build_string (NILP (selected) ? "[ ] " : "[X] ");
	else if (EQ (type, QCradio))
	  prefix = build_string (NILP (selected) ? "( ) " : "(*) ");
      }
    /* Not a button. If we have earlier buttons, then we need a prefix.  */
    else if (!*notbuttons_ptr && XSTRING (item_string)->data[0] != '\0'
	     && XSTRING (item_string)->data[0] != '-')
      prefix = build_string ("    ");

    if (!NILP (prefix))
      item_string = concat2 (prefix, item_string);
  }
#endif /* not HAVE_BOXES */
 
/* On W32, menu control automatically shows ">" on parental menu. */
#if !defined(USE_X_TOOLKIT) && !defined(HAVE_NTGUI)
  if (!NILP(map))
    /* Indicate visually that this is a submenu.  */
    item_string = concat2 (item_string, build_string (" >"));
#endif

#ifdef HAVE_NTGUI
  /* Display a submenu using the toolkit.  */
  if (! (NILP (map) || NILP (enabled)))
    {
      push_submenu_start ();
      /* item_string specify a name of theroot menu. */
      single_keymap_panes (map, item_string, key, 0, maxdepth - 1);
      push_submenu_end ();
    }
  else
    {
      push_menu_item (item_string, enabled, key,
		      XVECTOR (item_properties)->contents[ITEM_PROPERTY_DEF],
		      XVECTOR (item_properties)->contents[ITEM_PROPERTY_KEYEQ],
		      XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE],
		      XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED],
		      XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP]);
    }
#endif
}

/* Push all the panes and items of a menu described by the
   alist-of-alists MENU.
   This handles old-fashioned calls to x-popup-menu.  */

static void
list_of_panes (menu)
     Lisp_Object menu;
{
  Lisp_Object tail;

  init_menu_items ();

  for (tail = menu; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt, pane_name, pane_data;
      elt = Fcar (tail);
      pane_name = Fcar (elt);
      CHECK_STRING (pane_name, 0);
      push_menu_pane (pane_name, Qnil);
      pane_data = Fcdr (elt);
      CHECK_CONS (pane_data, 0);
#ifdef HAVE_NTGUI
      push_submenu_start ();
#endif
      list_of_items (pane_data);
#ifdef HAVE_NTGUI
      push_submenu_end ();
#endif
    }

  finish_menu_items ();
}

/* Push the items in a single pane defined by the alist PANE.  */

static void
list_of_items (pane)
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;

  for (tail = pane; !NILP (tail); tail = Fcdr (tail))
    {
      item = Fcar (tail);
      if (STRINGP (item))
	push_menu_item (item, Qnil, Qnil, Qt, Qnil, Qnil, Qnil, Qnil);
      else if (NILP (item))
	push_left_right_boundary ();
      else
	{
	  CHECK_CONS (item, 0);
	  item1 = Fcar (item);
	  CHECK_STRING (item1, 1);
	  push_menu_item (item1, Qt, Fcdr (item), Qt, Qnil, Qnil, Qnil, Qnil);
	}
    }
}

DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
  "Pop up a deck-of-cards menu and return user's selection.\n\
POSITION is a position specification.  This is either a mouse button event\n\
or a list ((XOFFSET YOFFSET) WINDOW)\n\
where XOFFSET and YOFFSET are positions in pixels from the top left\n\
corner of WINDOW's frame.  (WINDOW may be a frame object instead of a window.)\n\
This controls the position of the center of the first line\n\
in the first pane of the menu, not the top left of the menu as a whole.\n\
If POSITION is t, it means to use the current mouse position.\n\
\n\
MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.\n\
The menu items come from key bindings that have a menu string as well as\n\
a definition; actually, the \"definition\" in such a key binding looks like\n\
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into\n\
the keymap as a top-level element.\n\n\
If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.\n\
Otherwise, REAL-DEFINITION should be a valid key binding definition.\n\
\n\
You can also use a list of keymaps as MENU.\n\
  Then each keymap makes a separate pane.\n\
When MENU is a keymap or a list of keymaps, the return value\n\
is a list of events.\n\n\
\n\
Alternatively, you can specify a menu of multiple panes\n\
  with a list of the form (TITLE PANE1 PANE2...),\n\
where each pane is a list of form (TITLE ITEM1 ITEM2...).\n\
Each ITEM is normally a cons cell (STRING . VALUE);\n\
but a string can appear as an item--that makes a nonselectable line\n\
in the menu.\n\
With this form of menu, the return value is VALUE from the chosen item.\n\
\n\
If POSITION is nil, don't display the menu at all, just precalculate the\n\
cached information about equivalent key sequences.")
  (position, menu)
     Lisp_Object position, menu;
{
  int number_of_panes, panes;
  Lisp_Object keymap, tem;
  int xpos, ypos;
  Lisp_Object title;
  char *error_name;
  Lisp_Object selection;
  int i, j;
  FRAME_PTR f;
  Lisp_Object x, y, window;
  int keymaps = 0;
  int for_click = 0;
  struct gcpro gcpro1;
  HMENU hmenu;

#ifdef HAVE_MENUS
  if (! NILP (position))
    {
      check_mw32 ();

      /* Decode the first argument: find the window and the coordinates.  */
      if (EQ (position, Qt)
	  || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                                   || EQ (XCAR (position), Qtool_bar))))
	{
	  /* Use the mouse's current position.  */
	  FRAME_PTR new_f = SELECTED_FRAME ();
	  Lisp_Object bar_window;
	  enum scroll_bar_part part;
	  unsigned long time;

	  if (mouse_position_hook)
	    (*mouse_position_hook) (&new_f, 1, &bar_window,
				    &part, &x, &y, &time);
	  if (new_f != 0)
	    XSETFRAME (window, new_f);
	  else
	    {
	      window = selected_window;
	      XSETFASTINT (x, 0);
	      XSETFASTINT (y, 0);
	    }
	}
      else
	{
	  tem = Fcar (position);
	  if (CONSP (tem))
	    {
	      window = Fcar (Fcdr (position));
	      x = Fcar (tem);
	      y = Fcar (Fcdr (tem));
	    }
	  else
	    {
	      for_click = 1;
	      tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	      window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	      tem = Fcar (Fcdr (Fcdr (tem))); /* POSN_WINDOW_POSN (tem) */
	      x = Fcar (tem);
	      y = Fcdr (tem);
	    }
	}

      CHECK_NUMBER (x, 0);
      CHECK_NUMBER (y, 0);

      /* Decode where to put the menu.  */

      if (FRAMEP (window))
	{
	  f = XFRAME (window);
	  xpos = 0;
	  ypos = 0;
	}
      else if (WINDOWP (window))
	{
	  CHECK_LIVE_WINDOW (window, 0);
	  f = XFRAME (WINDOW_FRAME (XWINDOW (window)));

	  xpos = (FRAME_DEFAULT_FONT_WIDTH(f) *
		  XFASTINT(XWINDOW (window)->left));
	  ypos = (FRAME_LINE_HEIGHT(f) *
		  XFASTINT(XWINDOW (window)->top));
	}
      else
	/* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
	   but I don't want to make one now.  */
	CHECK_WINDOW (window, 0);

      xpos += XINT (x);
      ypos += XINT (y);

      XSETFRAME (Vmenu_updating_frame, f);
    }
  Vmenu_updating_frame = Qnil;
#endif /* HAVE_MENUS */

  title = Qnil;
  GCPRO1 (title);

  /* Decode the menu items from what was specified.  */

  keymap = get_keymap (menu, 0, 0);
  if (CONSP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1, NILP (position));

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = map_prompt (keymap);
      if (NILP (title) && !NILP (prompt))
	title = prompt;

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = prompt;

      keymaps = 1;
    }
  else if (CONSP (menu) && KEYMAPP (XCAR (menu)))
    {
      /* We were given a list of keymaps.  */
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;

      title = Qnil;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; CONSP (tem); tem = Fcdr (tem))
	{
	  Lisp_Object prompt;

	  maps[i++] = keymap = get_keymap (Fcar (tem), 1, 0);

	  prompt = map_prompt (keymap);
	  if (NILP (title) && !NILP (prompt))
	    title = prompt;
	}

      /* Extract the detailed info to make one pane.  */
      keymap_panes (maps, nmaps, NILP (position));

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = title;

      keymaps = 1;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title, 1);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }
  
  if (NILP (position))
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

#ifdef HAVE_MENUS
  /* Display them in a menu.  */
  BLOCK_INPUT;

  selection = mw32menu_show (f, xpos, ypos, for_click,
			    keymaps, title, &error_name);
  UNBLOCK_INPUT;
  f->output_data.mw32->menubar_active = 0;
  f->output_data.mw32->menu_command_in_progress = 0;
  mw32_free_menu_strings (FRAME_MW32_WINDOW (f));
  current_popup_menu = NULL;

  discard_menu_items ();

  UNGCPRO;
#endif /* HAVE_MENUS */

  if (error_name) error (error_name);
  return selection;
}


DEFUN ("x-popup-dialog", Fx_popup_dialog, Sx_popup_dialog, 2, 2, 0,
  "Pop up a dialog box and return user's selection.\n\
POSITION specifies which frame to use.\n\
This is normally a mouse button event or a window or frame.\n\
If POSITION is t, it means to use the frame the mouse is on.\n\
The dialog box appears in the middle of the specified frame.\n\
\n\
CONTENTS specifies the alternatives to display in the dialog box.\n\
It is a list of the form (TITLE ITEM1 ITEM2...).\n\
Each ITEM is a cons cell (STRING . VALUE).\n\
The return value is VALUE from the chosen item.\n\n\
An ITEM may also be just a string--that makes a nonselectable item.\n\
An ITEM may also be nil--that means to put all preceding items\n\
on the left of the dialog box and all following items on the right.\n\
\(By default, approximately half appear on each side.)")
  (position, contents)
     Lisp_Object position, contents;
{
  FRAME_PTR f;
  Lisp_Object window;

  check_mw32 ();
  
  /* Decode the first argument: find the window or frame to use.  */
  if (EQ (position, Qt)
      || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                               || EQ (XCAR (position), Qtool_bar))))
    {
#if 0 /* Using the frame the mouse is on may not be right.  */
      /* Use the mouse's current position.  */
      FRAME_PTR new_f = selected_frame;
      Lisp_Object bar_window;
      int part;
      unsigned long time;
      Lisp_Object x, y;

      (*mouse_position_hook) (&new_f, 1, &bar_window, &part, &x, &y, &time);

      if (new_f != 0)
	XSETFRAME (window, new_f);
      else
	window = selected_window;
#endif
      window = selected_window;
    }
  else if (CONSP (position))
    {
      Lisp_Object tem;
      tem = Fcar (position);
      if (CONSP (tem))
	window = Fcar (Fcdr (position));
      else
	{
	  tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	  window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	}
    }
  else if (WINDOWP (position) || FRAMEP (position))
    window = position;
  else
    window = Qnil;

  /* Decode where to put the menu.  */

  if (FRAMEP (window))
    f = XFRAME (window);
  else if (WINDOWP (window))
    {
      CHECK_LIVE_WINDOW (window, 0);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
    }
  else
    /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
       but I don't want to make one now.  */
    CHECK_WINDOW (window, 0);

#ifndef HAVE_DIALOGS
  /* Display a menu with these alternatives
     in the middle of frame F.  */
  {
    Lisp_Object x, y, frame, newpos;
    XSETFRAME (frame, f);
    XSETINT (x, x_pixel_width (f) / 2);
    XSETINT (y, x_pixel_height (f) / 2);
    newpos = Fcons (Fcons (x, Fcons (y, Qnil)), Fcons (frame, Qnil));

    return Fx_popup_menu (newpos,
			  Fcons (Fcar (contents), Fcons (contents, Qnil)));
  }
#else /* HAVE_DIALOGS */
  {
    Lisp_Object title;
    char *error_name;
    Lisp_Object selection;

    /* Decode the dialog items from what was specified.  */
    title = Fcar (contents);
    CHECK_STRING (title, 1);

    list_of_panes (Fcons (contents, Qnil));

    /* Display them in a dialog box.  */
    BLOCK_INPUT;
    selection = xdialog_show (f, 0, title, &error_name);
    UNBLOCK_INPUT;

    discard_menu_items ();

    if (error_name) error (error_name);
    return selection;
  }
#endif
}


/* Reserved space for W32 specific functions.  */

/* Activate the menu bar of frame F.
   This is called from keyboard.c when it gets the
   menu_bar_activate_event out of the Emacs event queue.

   To activate the menu bar, we use the X button-press event
   that was saved in saved_menu_event.
   That makes the toolkit do its thing.

   But first we recompute the menu bar contents (the whole tree).

   The reason for saving the button event until here, instead of
   passing it to the toolkit right away, is that we can safely
   execute Lisp code.  */

void   
x_activate_menubar (f)
     FRAME_PTR f;
{
#if 0
  if (!f->output_data.x->saved_menu_event->type)
    return;

  set_frame_menubar (f, 0, 1);
  BLOCK_INPUT;
  XtDispatchEvent ((XEvent *) f->output_data.x->saved_menu_event);
  UNBLOCK_INPUT;
#ifdef USE_MOTIF
  if (f->output_data.x->saved_menu_event->type == ButtonRelease)
    pending_menu_activation = 1;
#endif
  
  /* Ignore this if we get it a second time.  */
  f->output_data.x->saved_menu_event->type = 0;
#else
  pending_menu_activation = 1;
  set_frame_menubar (f, 0, 1);
  pending_menu_activation = 0;

  return;
#endif
}

/* Detect if a dialog or menu has been posted.  */

int
popup_activated ()
{
  return popup_activated_flag;
}

/* Display help string for currently pointed to menu item. Not
   supported on NT 3.51 and earlier, as GetMenuItemInfo is not
   available. */
void
mw32_menu_display_help (HWND owner, HMENU menu, UINT item, UINT flags)
{
  if (get_menu_item_info)
    {
      struct frame *f = mw32_window_to_frame (GET_MW32_DISPLAY_INFO(hwnd),
					      owner);
      Lisp_Object frame, help;

      // No help echo on owner-draw menu items.
      if (flags & MF_OWNERDRAW || flags & MF_POPUP)
	help = Qnil;
      else
	{
	  MENUITEMINFO info;

	  bzero (&info, sizeof (info));
	  info.cbSize = sizeof (info);
	  info.fMask = MIIM_DATA;
	  get_menu_item_info (menu, item, FALSE, &info);

	  help = info.dwItemData ? (Lisp_Object) info.dwItemData : Qnil;
	}

      /* Store the help echo in the keyboard buffer as the X toolkit
	 version does, rather than directly showing it. This seems to
	 solve the GC problems that were present when we based the
	 Windows code on the non-toolkit version.  */
      if (f)
	{
	  XSETFRAME (frame, f);
	  kbd_buffer_store_help_event (frame, help);
	}
      else
	/* X version has a loop through frames here, which doesn't
	   appear to do anything, unless it has some side effect.  */
	show_help_echo (help, Qnil, Qnil, Qnil, 1);
    }
}

/* Free memory used by owner-drawn strings.  */
static void
mw32_free_submenu_strings (HMENU menu)
{
  int i, num = GetMenuItemCount (menu);
  for (i = 0; i < num; i++)
    {
      MENUITEMINFO info;
      bzero (&info, sizeof (info));
      info.cbSize = sizeof (info);
      info.fMask = MIIM_DATA | MIIM_TYPE | MIIM_SUBMENU;

      get_menu_item_info (menu, i, TRUE, &info);

      /* Owner-drawn names are held in dwItemData.  */
      if ((info.fType & MF_OWNERDRAW) && info.dwItemData)
	{
#ifdef MENU_DEBUG
	  DebPrint ("Menu: freeing %ld for owner-draw", info.dwItemData);
#endif
	  local_free (info.dwItemData);
	}

      /* Recurse down submenus.  */
      if (info.hSubMenu)
	mw32_free_submenu_strings (info.hSubMenu);
    }
}

void
mw32_free_menu_strings (HWND hwnd)
{
  HMENU menu = current_popup_menu;

  if (get_menu_item_info)
    {
      /* If there is no popup menu active, free the strings from the frame's
	 menubar.  */
      if (!menu)
	menu = GetMenu (hwnd);

      if (menu)
	mw32_free_submenu_strings (menu);
    }

  current_popup_menu = NULL;
}

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */

void
menubar_selection_callback (hwnd, client_data)
     HWND hwnd;
     int client_data;
{
  Lisp_Object prefix, entry;
  struct mw32_display_info *dpyinfo = GET_MW32_DISPLAY_INFO(hwnd);
  FRAME_PTR f = mw32_window_to_frame (dpyinfo, hwnd);
  Lisp_Object vector;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  if (!f)
    return;
  subprefix_stack = (Lisp_Object *) alloca (f->menu_bar_items_used * sizeof (Lisp_Object));
  vector = f->menu_bar_vector;
  prefix = Qnil;
  i = 0;
  while (i < f->menu_bar_items_used)
    {
      if (EQ (XVECTOR (vector)->contents[i], Qnil))
	{
	  subprefix_stack[submenu_depth++] = prefix;
	  prefix = entry;
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qlambda))
	{
	  prefix = subprefix_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qt))
	{
	  prefix = XVECTOR (vector)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  entry = XVECTOR (vector)->contents[i + MENU_ITEMS_ITEM_VALUE];
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((int) (EMACS_INT) client_data == i)
	    {
	      int j;
	      struct input_event buf;
	      Lisp_Object frame;

	      XSETFRAME (frame, f);
	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = frame;
	      kbd_buffer_store_event (&buf);

	      for (j = 0; j < submenu_depth; j++)
		if (!NILP (subprefix_stack[j]))
		  {
		    buf.kind = MENU_BAR_EVENT;
		    buf.frame_or_window = frame;
		    buf.arg = subprefix_stack[j];
		    kbd_buffer_store_event (&buf);
		  }

	      if (!NILP (prefix))
		{
		  buf.kind = MENU_BAR_EVENT;
		  buf.frame_or_window = frame;
		  buf.arg = prefix;
		  kbd_buffer_store_event (&buf);
		}

	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = entry;
	      kbd_buffer_store_event (&buf);

	      /* Free memory used by owner-drawn and help-echo strings. */
	      mw32_free_menu_strings (FRAME_MW32_WINDOW (f));
	      f->output_data.mw32->menu_command_in_progress = 0;
	      f->output_data.mw32->menubar_active = 0;
	      return;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
  /* Free memory used by owner-drawn and help-echo strings. */
  mw32_free_menu_strings (FRAME_MW32_WINDOW (f));
  f->output_data.mw32->menu_command_in_progress = 0;
  f->output_data.mw32->menubar_active = 0;
}


/* Return a menu handle that is set to structures for a menu bar item
   whose event type is ITEM_KEY (with string ITEM_NAME)
   and whose contents come from the list of keymaps MAPS.  */

static HANDLE
single_submenu (item_key, item_name, maps, one_item_handle)
     Lisp_Object item_key, item_name, maps;
     HANDLE one_item_handle;
{
#if 0
  /*  widget_value *wv, *prev_wv, *save_wv, *first_wv; */
#endif
  HANDLE handle, current_handle, up_handle, first_handle;
  int item_number = 0;
  int one_item_id;

  int i;
  int submenu_depth = 0;
  Lisp_Object length;
  int len;
  Lisp_Object *mapvec;
  HANDLE *submenu_stack;
  int mapno;
  int previous_items = menu_items_used;
  int top_level_items = 0;

  Lisp_Object enable, descrip, def, help;

  length = Flength (maps);
  len = XINT (length);

  /* Convert the list MAPS into a vector MAPVEC.  */
  mapvec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
  for (i = 0; i < len; i++)
    {
      mapvec[i] = Fcar (maps);
      maps = Fcdr (maps);
    }

  menu_items_n_panes = 0;

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.  */
  for (i = 0; i < len; i++)
    {
      if (SYMBOLP (mapvec[i])
	  || (CONSP (mapvec[i])
	      && NILP (Fkeymapp (mapvec[i]))))
	{
	  /* Here we have a command at top level in the menu bar
	     as opposed to a submenu.  */
	  top_level_items = 1;
	  push_menu_pane (Qnil, Qnil);
	  push_menu_item (item_name, Qt, item_key, mapvec[i], Qnil,
			  Qnil, Qnil, Qnil);
	}
      else
	single_keymap_panes (mapvec[i], item_name, item_key, 0, 10);
    }

  /* Create a tree of menu handle objects
     representing the panes and their items.  */

  submenu_stack
    = (HANDLE *) alloca (menu_items_used * sizeof (HANDLE));
  first_handle = CreatePopupMenu ();
  current_handle = up_handle = INVALID_HANDLE_VALUE;

  /* Loop over all panes and items made during this call
     and construct a tree of widget_value objects.
     Ignore the panes and items made by previous calls to
     single_submenu, even though those are also in menu_items.  */
  i = previous_items;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = up_handle;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  current_handle = up_handle;
	  up_handle = submenu_stack[--submenu_depth];
	  i++;
	}
#if 0
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
#endif
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;
	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_SYSTEM (pane_name);
	      XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME]
		= pane_name;	      
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);

	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
#if 0
	  if (menu_items_n_panes == 1)
#else
	  if ((menu_items_n_panes == 1) && (submenu_depth == 0))
#endif
	    pane_string = "";

	  if (current_handle == INVALID_HANDLE_VALUE)
	    up_handle = first_handle;
	  else
	    up_handle = current_handle;

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (strcmp (pane_string, ""))
	    {
	      handle = CreatePopupMenu ();
	      /* Ignore the @ that means "separate pane".
		 This is a kludge, but this isn't worth more time.  */
	      if (!NILP (prefix) && pane_string[0] == '@')
		pane_string++;

	      appendmenu_encode (up_handle, MF_POPUP | MF_STRING,
				 (UINT) handle, pane_string);
	      current_handle = handle;
	    }
	  else
	    current_handle = up_handle;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  def = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_DEFINITION];
	  help = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_HELP];

#if 1
	  if (current_handle == INVALID_HANDLE_VALUE)
	    current_handle = first_handle;

	  one_item_id = (!NILP (def) ? (EMACS_INT) i : 0),
	  add_menu_item (current_handle,
			 item_name,
			 one_item_id,
			 !NILP (enable),
			 descrip,
			 help);
	  item_number++;
#else
	  wv = xmalloc_widget_value ();
	  if (prev_wv) 
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;

	  wv->name = (char *) XSTRING (item_name)->data;
	  if (!NILP (descrip))
	    wv->key = (char *) XSTRING (descrip)->data;
	  wv->value = 0;
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  wv->call_data = (!NILP (def) ? (void *) (EMACS_INT) i : 0);
	  wv->enabled = !NILP (enable);
	  prev_wv = wv;
#endif

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* If we have just one "menu item"
     that was originally a button, return it by itself.  */
#if 1
  /* item_string, */
  if (top_level_items && item_number == 1)
    {
      DestroyMenu (first_handle);
      if (one_item_handle != INVALID_HANDLE_VALUE)
	add_menu_item (one_item_handle,
		       item_name,
		       one_item_id,
		       !NILP (enable),
		       descrip,
		       help);
      return INVALID_HANDLE_VALUE;
    }
      
#else
  if (top_level_items && first_wv->contents && first_wv->contents->next == 0)
    {
      wv = first_wv->contents;
      free_widget_value (first_wv);
      return wv;
    }
#endif

  return first_handle;
}

/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (f, first_time, deep_p)
     FRAME_PTR f;
     int first_time;
     int deep_p;
{
  int i;
  Lisp_Object tail, items, frame;
#if 0
  widget_value *wv, *first_wv, *prev_wv = 0;
#else
  HANDLE handle, current_handle, first_handle;
#endif
  HANDLE menubar_handle = f->output_data.mw32->menubar_handle;

  XSETFRAME (Vmenu_updating_frame, f);

  /* We must not change the menubar when actually in use.  */
  if (f->output_data.mw32->disable_reconstruct_menubar)
    goto exit_setup;

  /*
    In the future, I will implement 
     the lazy preparation for menu bar.
     Partially impremented, use deep_p flag.
  */
  if (menubar_handle == INVALID_HANDLE_VALUE)
    {
      deep_p = 1;
      first_handle = CreateMenu ();
    }
  else
    {
      if (pending_menu_activation)
	{
	  int num;
	  first_handle = menubar_handle;
	  /* clean up menu */
	  num = GetMenuItemCount (menubar_handle);
	  for (i = 0;i < num;i++)
	    DeleteMenu (menubar_handle, 0, MF_BYPOSITION);
	}
      else
	{
	  if (deep_p)
	    first_handle = CreateMenu ();
	  else
	    first_handle = menubar_handle;
	}
    }

  if (deep_p)
    {
      /* Make a widget-value tree representing the entire menu trees.  */

      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      int specpdl_count = specpdl_ptr - specpdl;
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= (Lisp_Object *) alloca (previous_menu_items_used
				  * sizeof (Lisp_Object));

      /* If we are making a new widget, its contents are empty,
	 do always reinitialize them.  */
      if (menubar_handle == INVALID_HANDLE_VALUE)
	previous_menu_items_used = 0;

      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer;
      specbind (Qinhibit_quit, Qt);
      /* Don't let the debugger step into this code
	 because it is not reentrant.  */
      specbind (Qdebug_on_next_call, Qnil);

      record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));
      if (NILP (Voverriding_local_map_menu_flag))
	{
	  specbind (Qoverriding_terminal_local_map, Qnil);
	  specbind (Qoverriding_local_map, Qnil);
	}

      set_buffer_internal_1 (XBUFFER (buffer));

      /* Run the Lucid hook.  */
      call1 (Vrun_hooks, Qactivate_menubar_hook);
      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag))
	call0 (Qrecompute_lucid_menubar);
      safe_run_hooks (Qmenu_bar_update_hook);
      FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));

      items = FRAME_MENU_BAR_ITEMS (f);

      inhibit_garbage_collection ();

      /* Save the frame's previous menu bar contents data.  */
      bcopy (XVECTOR (f->menu_bar_vector)->contents, previous_items,
	     previous_menu_items_used * sizeof (Lisp_Object));

      /* Fill in the current menu bar contents.  */
      menu_items = f->menu_bar_vector;
      menu_items_allocated = XVECTOR (menu_items)->size;
      init_menu_items ();
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object key, string, maps;

	  key = XVECTOR (items)->contents[i];
	  string = XVECTOR (items)->contents[i + 1];
	  maps = XVECTOR (items)->contents[i + 2];
	  if (NILP (string))
	    break;

	  handle = single_submenu (key, string, maps, first_handle);
	  if (handle != INVALID_HANDLE_VALUE)
	    appendmenu_encode (first_handle, MF_POPUP | MF_STRING, 
			       (UINT) handle, XSTRING(string)->data);
	}

      finish_menu_items ();

      set_buffer_internal_1 (prev);
      unbind_to (specpdl_count, Qnil);

      /* If there has been no change in the Lisp-level contents
	 of the menu bar, skip redisplaying it.  Just exit.  */

      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], XVECTOR (menu_items)->contents[i])))
	  break;
      if (i == menu_items_used && i == previous_menu_items_used && i != 0)
	{
	  /* No Lisp-level change!.
	     Cleanup and Exit!! */
	  if (first_handle != menubar_handle)
	    DestroyMenu (first_handle);
	  menu_items = Qnil;
	  goto exit_setup;
	}

      f->menu_bar_vector = menu_items;
      f->menu_bar_items_used = menu_items_used;
      menu_items = Qnil;
    }
  else
    {
      /* Make a widget-value tree containing
	 just the top level menu bar strings.  */

      int nochange = 1;
      int idx = 0;
      int num;

      items = FRAME_MENU_BAR_ITEMS (f);
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object string;

	  string = XVECTOR (items)->contents[i + 1];
	  if (NILP (string))
	    break;

	  nochange = (modifymenu_encode (first_handle, idx,
					 MF_STRING, 0,
					 XSTRING(string)->data)
		      && nochange);
	  idx++;
	}
      num = GetMenuItemCount (first_handle);
      if (num > idx)
	{
	  nochange = 0;
	  for (;num > idx;num--)
	    DeleteMenu (first_handle, idx, MF_BYPOSITION);
	}
      if (nochange)
	goto exit_setup;

      /* Forget what we thought we knew about what is in the
	 detailed contents of the menu bar menus.
	 Changing the top level always destroys the contents.  */
      f->menu_bar_items_used = 0;
    }

  /* Own created first_handle if it is newly created.  */
  if (first_handle != menubar_handle)
    {
      BLOCK_INPUT;
      SetMenu (FRAME_MW32_WINDOW (f), first_handle);
      if (menubar_handle != INVALID_HANDLE_VALUE)
	DestroyMenu (menubar_handle);
      UNBLOCK_INPUT;

      f->output_data.mw32->menubar_handle = first_handle;
    }

#if 0
  {
    int menubar_size 
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
	    + f->output_data.x->menubar_widget->core.border_width)
	 : 0);

#if 0 /* Experimentally, we now get the right results
	 for -geometry -0-0 without this.  24 Aug 96, rms.  */
#ifdef USE_LUCID
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (f->output_data.x->column_widget,
		       XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }
#endif /* USE_LUCID */
#endif /* 0 */

    f->output_data.x->menubar_height = menubar_size;
  }
#endif /* 0 */

  /* inform other threads that reconstruction have finished. */
  if (pending_menu_activation)
    SetEvent (f->output_data.mw32->mainthread_to_frame_handle);
  Vmenu_updating_frame = Qnil;
  if (!pending_menu_activation)
    {
      BLOCK_INPUT;
      DrawMenuBar (FRAME_MW32_WINDOW (f));
      UNBLOCK_INPUT;
    }
  return;

 exit_setup:
  /* inform other threads that reconstruction have finished. */
  if (pending_menu_activation)
    SetEvent (f->output_data.mw32->mainthread_to_frame_handle);
  Vmenu_updating_frame = Qnil;
  return;
}


/* Called from Fw32_create_frame to create the inital menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible.  */

void initialize_frame_menubar (f)
    FRAME_PTR f;
{
  /* This function is called before the first chance to redisplay
     the frame.  It has to be, so the frame will have the right size.  */
  FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));
  set_frame_menubar (f, 1, 1);
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void free_frame_menubar (f)
    FRAME_PTR f;
{
  RECT rect;
  HWND hwnd = FRAME_MW32_WINDOW (f);
  HMENU old = GetMenu (hwnd) ;

  BLOCK_INPUT;
  /* To preserve window area of the frame as correctly as possible,
     call SetWindowPos() with its before value. */
  GetWindowRect (hwnd, &rect);
  SetMenu (hwnd, NULL);
  SetWindowPos (hwnd, HWND_NOTOPMOST,
		rect.left, rect.top,
		rect.right - rect.left, rect.bottom - rect.top,
		SWP_NOZORDER);
  DestroyMenu (old);
  UNBLOCK_INPUT;

  f->output_data.mw32->menubar_handle = INVALID_HANDLE_VALUE;
}


/* w32menu_show actually displays a menu using the panes and items in menu_items
   and returns the value selected from it.
   There are two versions of w32menu_show, one for Xt and one for Xlib.
   Both assume input is blocked by the caller.  */

/* F is the frame the menu is for.
   X and Y are the frame-relative specified position,
   relative to the inside upper left corner of the frame F.
   MENUBARP is 1 if the click that asked for this menu came from the menu bar.
   KEYMAPS is 1 if this menu was specified with keymaps;
   in that case, we return a list containing the chosen item's value
   and perhaps also the pane's prefix.
   TITLE is the specified menu title.
   ERROR is a place to store an error message string in case of failure.
   (We return nil on failure, but the value doesn't actually matter.)  */


static Lisp_Object
mw32menu_show (f, x, y, for_click, keymaps, title, error)
     FRAME_PTR f;
     int x;
     int y;
     int for_click;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  int i, menu_item_selection = 0;
  UINT track_flag;
  POINT pos ;
  MSG msg;
  HANDLE handle, current_handle, up_handle, first_handle;

#if 0
  LWLIB_ID menu_id;
  Widget menu;
  Arg av[2];
  int ac = 0;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
#endif
  HANDLE *submenu_stack
    = (HANDLE *) alloca (menu_items_used * sizeof (HANDLE));

  Lisp_Object *subprefix_stack
    = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object));
  int submenu_depth = 0;

  int first_pane;
  int next_release_must_exit = 0;

  *error = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error = "Empty menu";
      return Qnil;
    }

  /* Create a handle that belongs to a tree of 
     the panes and their items.  */
  current_popup_menu = first_handle = CreatePopupMenu ();
  current_handle = up_handle = INVALID_HANDLE_VALUE;
  first_pane = 1;
 
  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = up_handle;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  current_handle = up_handle;
	  up_handle = submenu_stack[--submenu_depth];
	  i++;
	}
#if 0
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
#endif
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;
	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_SYSTEM (pane_name);
	      XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME]
		= pane_name;	      
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
#if 0
	  if (menu_items_n_panes == 1)
#else
	  if ((menu_items_n_panes == 1) && (submenu_depth == 0))
#endif
	    pane_string = "";

	  if (current_handle == INVALID_HANDLE_VALUE)
	    up_handle = first_handle;
	  else
	    up_handle = current_handle;

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (strcmp (pane_string, ""))
	    {
	      handle = CreatePopupMenu ();
	      /* Ignore the @ that means "separate pane".
		 This is a kludge, but this isn't worth more time.  */
	      if (!NILP (prefix) && pane_string[0] == '@')
		pane_string++;

	      appendmenu_encode (up_handle, MF_POPUP | MF_STRING,
				 (UINT) handle, pane_string);
	      current_handle = handle;
	    }
	  else
	    current_handle = up_handle;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected, help;
	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  def = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_DEFINITION];
	  type = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_TYPE];
	  selected
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_SELECTED];
	  help = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_HELP];

	  if (current_handle == INVALID_HANDLE_VALUE)
	    current_handle = first_handle;

	  add_menu_item (current_handle,
			 item_name,
			 (!NILP (def) ? (void *) (EMACS_INT) i : 0),
			 !NILP (enable),
			 descrip,
			 help);

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* Deal with the title, if it is non-nil.  */
  if (!NILP (title))
    {
      InsertMenu (first_handle, 0,
		  MF_BYPOSITION | MF_SEPARATOR, 0, 0);
      insertmenu_encode (first_handle, 0,
			 MF_OWNERDRAW, 0,
			 XSTRING (title)->data);
    }

#if 0
  /* Actually create the menu.  */
  menu_id = widget_id_tick++;
  menu = lw_create_widget ("popup", first_wv->name, menu_id, first_wv,
			   f->output_data.x->widget, 1, 0,
			   popup_selection_callback,
			   popup_deactivate_callback);
#endif

  pos.x = x ;
  pos.y = y ;
    
  /* Offset the coordinates to root-relative.  */
  ClientToScreen (FRAME_MW32_WINDOW(f),&pos) ;

  /* Display the menu.  */
  SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW(f), WM_EMACS_POPUP_MENU, 
		       (WPARAM)first_handle, (LPARAM) &pos);
  WAIT_REPLY_MESSAGE (&msg, WM_EMACS_POPUP_MENU_REPLY);

  menu_item_selection = msg.wParam;
  DestroyMenu (first_handle);
      
  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = entry = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	    {
	      subprefix_stack[submenu_depth++] = prefix;
	      prefix = entry;
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	    {
	      prefix = subprefix_stack[--submenu_depth];
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	    {
	      prefix
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  /* Ignore a nil in the item list.
	     It's meaningful only for dialog boxes.  */
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	    i += 1;
	  else
	    {
	      entry
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
	      if (menu_item_selection == i)
		{
		  if (keymaps != 0)
		    {
		      int j;

		      entry = Fcons (entry, Qnil);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }

  return Qnil;
}


void
reinit_syms_of_mw32menu ()
{
  menu_items = Qnil;
}

void
syms_of_mw32menu ()
{
  staticpro_nopdump (&menu_items);
  reinit_syms_of_mw32menu ();

  current_popup_menu = NULL;
  
  Qdebug_on_next_call = intern ("debug-on-next-call");
  staticpro (&Qdebug_on_next_call);

  DEFVAR_LISP ("menu-updating-frame", &Vmenu_updating_frame,
    "Frame for which we are updating a menu.\n\
The enable predicate for a menu command should check this variable.");
  Vmenu_updating_frame = Qnil;

  defsubr (&Sx_popup_menu);
  defsubr (&Sx_popup_dialog);
}


/* globals_of_w32menu is used to initialize those global variables
   that must always be initialized on startup even when the global
   variable initialized is non zero (see the function main in
   emacs.c).  globals_of_w32menu is called from syms_of_w32menu when
   the global variable initialized is 0 and directly from main when
   initialized is non zero. */
void
globals_of_w32menu (void)
{
  /* See if Get/SetMenuItemInfo functions are available.  */
  HMODULE user32 = GetModuleHandle ("user32.dll");
  get_menu_item_info
    = (GetMenuItemInfoA_Proc) GetProcAddress (user32, "GetMenuItemInfoA");
  set_menu_item_info
    = (SetMenuItemInfoA_Proc) GetProcAddress (user32, "SetMenuItemInfoA");
}
