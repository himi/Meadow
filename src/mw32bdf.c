/* MW32 bdf font specific staff.
   Copyright (C) 1998-2002 Free Software Foundation, Inc.

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

/* implemented by MIYASHITA Hisashi <himi@meadowy.org> */

/*****************************************

  This module provides the following functions.

        bdffont *mw32_init_bdf_font(char *filename);

	void mw32_free_bdf_font(bdffont *fontp);

	Lisp_Object mw32_get_bdf_font_info(Lisp_Object(string) fontfile);

	int mw32_get_bdf_glyph(bdffont *fontp, int index, int size,
	                  glyph_struct *glyph);

	int mw32_BDF_TextOut(bdffont *fontp, HDC hdc, int left, int top,
			     unsigned char *text, int dim, int bytelen,
			     int fixed_pitch_size, character_spacing);

******************************************/


#include <windows.h>
#include "config.h"
#include "lisp.h"
#include "mw32bdf.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* 10 planes */
#define BDF_CODEPOINT_HEAP_INITIAL_SIZE (96 * 10)
/* about 96 characters */
#define BDF_BITMAP_HEAP_INITIAL_SIZE    (64 * 96)

HANDLE hbdf_cp_heap = INVALID_HANDLE_VALUE;
HANDLE hbdf_bmp_heap = INVALID_HANDLE_VALUE;

void mw32_free_bdf_font (bdffont *fontp);
bdffont *mw32_init_bdf_font (char *filename);

cache_bitmap cached_bitmap_slots[BDF_FONT_CACHE_SIZE];
cache_bitmap *pcached_bitmap_latest = cached_bitmap_slots;

#define FONT_CACHE_SLOT_OVER_P(p) ((p) >= cached_bitmap_slots + BDF_FONT_CACHE_SIZE)

static int 
search_file_line (char *key, unsigned char *start, int len,
		  unsigned char **val, unsigned char **next)
{
  int linelen;
  unsigned char *p, *q;

  p = memchr (start, '\n', len);
  if (!p) return -1;
  for (;start < p;start++)
    {
      if ((*start != ' ') && (*start != '\t')) break;
    }
  linelen = p - start + 1;
  *next = p + 1;
  if (strncmp (start, key, min (strlen (key), linelen)) == 0)
    {
      *val = start + strlen (key);
      return 1;
    }
  
  return 0;
}

static int
proceed_file_line (char *key, unsigned char *start, int *len,
		   unsigned char **val, unsigned char **next)
{
  int flag = 0;

  do {
    flag = search_file_line (key, start, *len, val, next);
    *len -= (int) (*next - start);
    start = *next;
  }while (flag == 0);

  if (flag == -1) return 0;
  return 1;
}
   
static char*
get_quoted_string (char *start, char *end)
{
  char *p, *q, *result;

  p = memchr (start, '\"', end - start);
  if (!p) return NULL;
  p++;
  q = memchr (p, '\"', end - p);
  if (!q) return NULL;

  result = (char*) xmalloc (q - p + 1);

  memcpy (result, p, q - p);
  result[q - p] = '\0';

  return result;
}

static int
set_bdf_font_info (bdffont *fontp)
{
  unsigned char *start, *p, *q;
  int len, flag;
  int bbw, bbh, bbx, bby;
  int val1;

  len = fontp->size;
  start = fontp->font;

  fontp->yoffset = 0;
  fontp->relative_compose = 0;
  fontp->default_ascent = 0;

  fontp->registry = NULL;
  fontp->encoding = NULL;
  fontp->slant = NULL;
  fontp->width = NULL;

  flag = proceed_file_line ("FONTBOUNDINGBOX", start, &len, &p, &q);
  if (!flag) return 0;
  bbw = strtol (p, (char **) &start, 10);
  p = start;
  bbh = strtol (p, (char **) &start, 10);
  p = start;
  bbx = strtol (p, (char **) &start, 10);
  p = start;
  bby = strtol (p, (char **) &start, 10);

  fontp->llx = bbx;
  fontp->lly = bby;
  fontp->urx = bbw + bbx;
  fontp->ury = bbh + bby;

  start = q;
  flag = proceed_file_line ("STARTPROPERTIES", start, &len, &p, &q);
  if (!flag) return 1;

  flag = 0;

  do {
    start = q;
    if (search_file_line ("PIXEL_SIZE", start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
      }
    else if (search_file_line ("FONT_ASCENT", start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
	fontp->ury = val1;
      }
    else if (search_file_line ("FONT_DESCENT", start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
	fontp->lly = -val1;
      }
    else if (search_file_line ("_MULE_BASELINE_OFFSET",
			       start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
	fontp->yoffset = -val1;
      }
    else if (search_file_line ("_MULE_RELATIVE_COMPOSE",
			       start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
	fontp->relative_compose = val1;
      }
    else if (search_file_line ("_MULE_DEFAULT_ASCENT",
			       start, len, &p, &q) == 1)
      {
	val1 = atoi (p);
	fontp->default_ascent = val1;
      }
    else if (search_file_line ("CHARSET_REGISTRY", start, len, &p, &q) == 1)
      {
        fontp->registry = get_quoted_string (p, q);
      }
    else if (search_file_line ("CHARSET_ENCODING", start, len, &p, &q) == 1)
      {
        fontp->encoding = get_quoted_string (p, q);
      }
    else if (search_file_line ("SLANT", start, len, &p, &q) == 1)
      {
        fontp->slant = get_quoted_string (p, q);
      }
    else if (search_file_line ("SETWIDTH_NAME", start, len, &p, &q) == 1)
      {
        fontp->width = get_quoted_string (p, q);
      }
    else
      {
	flag = search_file_line ("ENDPROPERTIES", start, len, &p, &q);
      }
    if (flag == -1) return 0;
    len -= (q - start);
  } while (flag == 0);
  start = q;
  flag = proceed_file_line ("CHARS", start, &len, &p, &q);
  if (!flag) return 0;
  fontp->seeked = q;

  return 1;
}

bdffont*
mw32_init_bdf_font (char *filename)
{
  HANDLE hfile, hfilemap;
  bdffont *bdffontp;
  unsigned char *font;
  BY_HANDLE_FILE_INFORMATION fileinfo;
  int i;

  if (hbdf_cp_heap == INVALID_HANDLE_VALUE)
    hbdf_cp_heap = HeapCreate (0, BDF_CODEPOINT_HEAP_INITIAL_SIZE, 0);
  if (hbdf_bmp_heap == INVALID_HANDLE_VALUE)
    hbdf_bmp_heap = HeapCreate (0, BDF_BITMAP_HEAP_INITIAL_SIZE, 0);

  if (!hbdf_cp_heap || !hbdf_bmp_heap)
    error ("Fail to create heap for BDF.");

  hfile = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hfile == INVALID_HANDLE_VALUE) return NULL;
  if (!GetFileInformationByHandle (hfile, &fileinfo) ||
      (fileinfo.nFileSizeHigh != 0) ||
      (fileinfo.nFileSizeLow > BDF_FILE_SIZE_MAX))
    {
      CloseHandle (hfile);
      error ("Fail to open BDF file.");
    }
  hfilemap = CreateFileMapping (hfile, NULL, PAGE_READONLY, 0, 0, NULL);
  if (hfilemap == INVALID_HANDLE_VALUE)
    {
      CloseHandle (hfile);
      error ("Can't map font.");
    }


  font = MapViewOfFile (hfilemap, FILE_MAP_READ, 0, 0, 0);

  if (!font)
    {
      CloseHandle (hfile);
      CloseHandle (hfilemap);
      error ("Can't view font.");
    }

  bdffontp = (bdffont *) xmalloc (sizeof (bdffont));

  for (i = 0; i < BDF_FIRST_OFFSET_TABLE; i++)
    bdffontp->chtbl[i] = NULL;
  bdffontp->size = fileinfo.nFileSizeLow;
  bdffontp->font = font;
  bdffontp->hfile = hfile;
  bdffontp->hfilemap = hfilemap;
  bdffontp->filename = (char*) xmalloc (strlen (filename) + 1);
  strcpy (bdffontp->filename, filename);
  
  if (!set_bdf_font_info (bdffontp))
    {
      mw32_free_bdf_font (bdffontp);
      error ("Invalid BDF font!");
    }
  return bdffontp;
}

void
mw32_free_bdf_font (bdffont *fontp)
{
  int i, j;
  font_char *pch;
  cache_bitmap *pcb;

  UnmapViewOfFile (fontp->font);
  CloseHandle (fontp->hfilemap);
  CloseHandle (fontp->hfile);

  if (fontp->registry) xfree (fontp->registry);
  if (fontp->encoding) xfree (fontp->encoding);
  if (fontp->slant) xfree (fontp->slant);
  if (fontp->width) xfree (fontp->width);

  xfree (fontp->filename);
  for (i = 0; i < BDF_FIRST_OFFSET_TABLE; i++)
    {
      pch = fontp->chtbl[i];
      if (pch)
	{
	  for (j = 0; j < BDF_SECOND_OFFSET_TABLE; j++)
	    {
	      pcb = pch[j].pcbmp;
 	      if (pcb)
 		{
		  if (pcb->pbmp)
		    {
		      HeapFree (hbdf_bmp_heap, 0, pcb->pbmp);
		      pcb->pbmp = NULL;
		    }
 		  pcb->psrc = NULL;
 		}
	    }
	  HeapFree (hbdf_cp_heap, 0, pch);
	}
    }
  xfree (fontp);
}

Lisp_Object
mw32_get_bdf_font_info (Lisp_Object fontfile)
{
  bdffont *bdffontp;
  Lisp_Object ret = Qnil;

  bdffontp = mw32_init_bdf_font (XSTRING (fontfile)->data);

  if (!bdffontp) return Qnil;
  
  store_in_alist (&ret, intern ("width"),
		  make_number (bdffontp->urx - bdffontp->llx));
  store_in_alist (&ret, intern ("height"),
		  make_number (bdffontp->ury - bdffontp->lly));
  store_in_alist (&ret, intern ("base"),
		  make_number (bdffontp->ury));
  store_in_alist (&ret, intern ("overhang"), XFASTINT(0));
  store_in_alist (&ret, intern ("relative-compose"),
		  make_number (bdffontp->relative_compose));
  store_in_alist (&ret, intern ("default-ascent"),
		  make_number (bdffontp->default_ascent));

  if (bdffontp->registry)
    store_in_alist (&ret, intern ("charset-registry"),
		    build_string (bdffontp->registry));
  if (bdffontp->encoding)
    store_in_alist (&ret, intern ("charset-encoding"),
		    build_string (bdffontp->encoding));
  if (bdffontp->slant)
    store_in_alist (&ret, intern ("slant"),
		    build_string (bdffontp->slant));
  if (bdffontp->width)
    store_in_alist (&ret, intern ("setwidth-name"),
		    build_string (bdffontp->width));

  mw32_free_bdf_font (bdffontp);

  return ret;
}

static font_char*
get_cached_font_char (bdffont *fontp, int index)
{
  font_char *pch, *result;
  int i;

  if (!BDF_CODEPOINT_RANGE_COVER_P (index))
    return NULL;

  pch = fontp->chtbl[BDF_FIRST_OFFSET (index)];
  if (!pch)
    return NULL;

  result = &pch[BDF_SECOND_OFFSET (index)];

  if (!result->offset) return NULL;

  return result;
}

static font_char*
cache_char_offset (bdffont *fontp, int index, unsigned char *offset)
{
  font_char *pch, *result;
  int i;

  if (!BDF_CODEPOINT_RANGE_COVER_P (index))
    return NULL;

  pch = fontp->chtbl[BDF_FIRST_OFFSET (index)];
  if (!pch)
    {
      pch = fontp->chtbl[BDF_FIRST_OFFSET (index)] =
	(font_char*) HeapAlloc (hbdf_cp_heap,
				HEAP_ZERO_MEMORY, 
				sizeof (font_char) *
				BDF_SECOND_OFFSET_TABLE);
      if (!pch) return NULL;
      /* memset (pch, 0, sizeof (font_char) * BDF_SECOND_OFFSET_TABLE); */
    }

  result = &pch[BDF_SECOND_OFFSET (index)];
  result->offset = offset;

  return result;
}

static font_char*
seek_char (bdffont *fontp, int index)
{
  font_char *result;
  int len, flag, font_index;
  unsigned char *start, *p, *q;

  if (!fontp->seeked) return NULL;

  start = fontp->seeked;
  len = fontp->size - (start - fontp->font);

  do {
    flag = proceed_file_line ("ENCODING", start, &len, &p, &q);
    if (!flag)
      {
	fontp->seeked = NULL;
	return NULL;
      }
    font_index = atoi (p);
    result = cache_char_offset (fontp, font_index, q);
    if (!result) return NULL;

    start = result->offset;
  } while (font_index != index);
  fontp->seeked = start;

  return result;
}

void
mw32_init_cached_bitmap_slots ()
{
  int i;
  /* cache_bitmap cached_bitmap_slots[BDF_FONT_CACHE_SIZE];*/
  for (i = 0; i < BDF_FONT_CACHE_SIZE; i++)
    {
      cached_bitmap_slots[i].psrc = NULL;
      cached_bitmap_slots[i].row_byte_size = 0;
      cached_bitmap_slots[i].bitmap_size = 0;
      cached_bitmap_slots[i].pbmp = NULL;
    }
}

static void
clear_cached_bitmap_slots ()
{
  int i;
  cache_bitmap *p;

  p = pcached_bitmap_latest;
  for (i = 0; i < BDF_FONT_CLEAR_SIZE; i++)
    {
      if (p->psrc)
	{
	  if (p->pbmp)
	    {
	      HeapFree (hbdf_bmp_heap, 0, p->pbmp);
	      p->pbmp = NULL;
	    }
	  p->psrc->pcbmp = NULL;
	  p->psrc = NULL;
	}
      p++;
      if (FONT_CACHE_SLOT_OVER_P (p))
	p = cached_bitmap_slots;
    }
}

#define GET_HEX_VAL(x) ((isdigit(x)) ? ((x) - '0') : \
			(((x) >= 'A') && ((x) <= 'F')) ? ((x) - 'A' + 10) : \
			(((x) >= 'a') && ((x) <= 'f')) ? ((x) - 'a' + 10) : \
			(-1))

int
mw32_get_bdf_glyph (bdffont *fontp, int index, int size, glyph_struct *glyph)
{
  font_char *pch;
  unsigned char *start, *p, *q, *bitmapp;
  unsigned char val, val1, val2;
  int i, j, len, flag, consumed;
  int align, rowbytes;

  pch = get_cached_font_char (fontp, index);
  if (!pch)
    {
      pch = seek_char (fontp, index);
      if (!pch)
	return 0;
    }

  start = pch->offset;

  if ((size == 0) && pch->pcbmp)
    {
      glyph->metric = pch->pcbmp->metric;
      return 1;
    }

  len = fontp->size - (start - fontp->font);

  flag = proceed_file_line ("DWIDTH", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->metric.dwidth = atoi (p);

  start = q;
  flag = proceed_file_line ("BBX", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->metric.bbw = strtol (p, (char **) &start, 10);
  p = start;
  glyph->metric.bbh = strtol (p, (char **) &start, 10);
  p = start;
  glyph->metric.bbox = strtol (p, (char **) &start, 10);
  p = start;
  glyph->metric.bboy = strtol (p, (char **) &start, 10);

  if (size == 0) return 1;

  start = q;
  flag = proceed_file_line ("BITMAP", start, &len, &p, &q);
  if (!flag)
    return 0;

  consumed = 0;
  flag = 0;
  p = q;
  bitmapp = glyph->bitmap;
  rowbytes = (glyph->metric.bbw + 7) / 8;
  /* DIB requires DWORD alignment.  */
  align = sizeof (DWORD) - rowbytes % sizeof (DWORD);
  consumed = glyph->metric.bbh * (rowbytes + align);
  glyph->bitmap_size = consumed;
  glyph->row_byte_size = rowbytes;
  if (size < consumed) return 0;

  for(i = 0; i < glyph->metric.bbh; i++)
    {
      q = memchr (p, '\n', len);
      if (!q) return 0;
      for (j = 0; ((q > p) && (j < rowbytes)); j++)
	{
	  int ival = GET_HEX_VAL (*p);
	  if (ival == -1) return 0;
	  val1 = ival;
	  p++;
	  ival = GET_HEX_VAL (*p);
	  if (ival == -1) return 0;
	  val2 = ival;
	  p++;
	  val = (unsigned char) ((val1 << 4) | val2);
	  if (val) flag = 1;
	  *bitmapp++ = val;
	}
      for (j = 0; j < align; j++)
	*bitmapp++ = 0x00;
      p = q + 1;
    }

  /* If this glyph is white space, return -1. */
  if (flag == 0) return -1;

  return consumed;
}

static cache_bitmap*
get_bitmap_with_cache (bdffont *fontp, int index)
{
  int bitmap_size, bitmap_real_size;
  font_char *pch;
  cache_bitmap* pcb;
  unsigned char *pbmp;
  glyph_struct glyph;

  pch = get_cached_font_char (fontp, index);
  if (pch)
    {
      pcb = pch->pcbmp;
      if (pcb) return pcb;
    }

  bitmap_size = (((fontp->urx - fontp->llx) / 8 + 3)
		 * (fontp->ury - fontp->lly) + 256);
  glyph.bitmap = (unsigned char*) alloca (sizeof (unsigned char)
					  * bitmap_size);

  bitmap_real_size = mw32_get_bdf_glyph (fontp, index, bitmap_size, &glyph);

  if (bitmap_real_size == 0)
    return NULL;

  pch = get_cached_font_char (fontp, index);
  if (!pch) return NULL;

  if (bitmap_real_size > 0)
    {
       pbmp = (unsigned char*) HeapAlloc (hbdf_bmp_heap, 0,
					  bitmap_real_size);
       if (!pbmp) return NULL;
       memcpy (pbmp, glyph.bitmap, bitmap_real_size);
    }
  else
    pbmp = NULL; /* white space character */

  pcb = pcached_bitmap_latest;
  if (pcb->psrc)
    clear_cached_bitmap_slots ();

  pcb->psrc = pch;
  pcb->metric = glyph.metric;
  pcb->pbmp = pbmp;
  pcb->bitmap_size = glyph.bitmap_size;
  pcb->row_byte_size = glyph.row_byte_size;

  pch->pcbmp = pcb;
  
  pcached_bitmap_latest++;
  if (FONT_CACHE_SLOT_OVER_P (pcached_bitmap_latest))
    pcached_bitmap_latest = cached_bitmap_slots;

  return pcb;
}

static HBITMAP
create_offscreen_bitmap (HDC hdc, int width, int height, unsigned char **bitsp)
{
  HBITMAP hBMP;
  struct {
    BITMAPINFOHEADER h;
    RGBQUAD c[2];
  } info;

  memset (&info, 0, sizeof (info));
  info.h.biSize = sizeof (BITMAPINFOHEADER);
  info.h.biWidth = width;
  info.h.biHeight = -height;
  info.h.biPlanes = 1;
  info.h.biBitCount = 1;
  info.h.biCompression = BI_RGB;
  info.c[1].rgbRed = info.c[1].rgbGreen = info.c[1].rgbBlue = 255;

  return CreateDIBSection (hdc, (LPBITMAPINFO) &info,
			   DIB_RGB_COLORS, (void **) bitsp, NULL, 0);
}

static int
draw_for_invalid_index (HDC hdc,
			int left, int top,
			int width, int height)
{
  /* If we cannot find out any griphs,
     maybe we should display DEFAULT_CHAR
     in the properties.  But we don't do so
     because BDF specification does not specify it.
     We display a circle with slash, (which means
     empty). */
  int bd_top, bd_left, bd_right, bd_bottom;
  int wmargin, hmargin;
  LOGBRUSH logpenbrush;
  HPEN hpen, horgpen;

  logpenbrush.lbStyle = BS_SOLID;
  logpenbrush.lbColor = GetTextColor (hdc);
  hpen = ExtCreatePen (PS_COSMETIC | PS_SOLID, 1, &logpenbrush,
		       0, NULL);
  horgpen = SelectObject (hdc, hpen);

  wmargin = (int) (width * 0.1F);
  bd_left = left + wmargin;
  bd_right = left + width - wmargin;

  hmargin = (int) (height * 0.1F);
  bd_top = top + hmargin;
  bd_bottom = top + height - hmargin;

  Arc (hdc, bd_left, bd_top, bd_right, bd_bottom,
       bd_left, bd_top, bd_left, bd_top);
  MoveToEx (hdc, bd_right, bd_top, NULL);
  LineTo (hdc, bd_left, bd_bottom);

  SelectObject (hdc, horgpen);
  DeleteObject (hpen);

  return 1;
}

int
mw32_BDF_TextOut (bdffont *fontp, HDC hdc, int left,
		  int top, unsigned char *text, int dim, int bytelen,
		  int fixed_pitch_size, int character_spacing)
{
  int index, btop;
  unsigned char *textp;
  cache_bitmap *pcb;
  HBRUSH hFgBrush, hOrgBrush;
  HANDLE horgobj;
  UINT textalign;
  int width, height;
  HDC hCompatDC;
  int ret = 1;
  static HBITMAP hBMP = 0;
  static HDC DIBsection_hdc = 0;
  static int DIBsection_width, DIBsection_height;
  static unsigned char *bits;

  hCompatDC = CreateCompatibleDC (hdc);
  if (!hCompatDC)
    return 0;

  textalign = GetTextAlign (hdc);

  hFgBrush = CreateSolidBrush (GetTextColor (hdc));
  hOrgBrush = SelectObject (hdc, hFgBrush);

  textp = text;

  while (bytelen > 0)
    {
      if (dim == 1)
	{
	  index = *textp++;
	  bytelen--;
	}
      else
	{
	  bytelen -= 2;
	  if (bytelen < 0) break;
	  index = MAKELENDSHORT (textp[1], textp[0]);
	  textp += 2;
	}
      pcb = get_bitmap_with_cache (fontp, index);
      if (!pcb)
	{
	  if (fixed_pitch_size)
	    width = fixed_pitch_size;
	  else
	    width = (fontp->urx - fontp->llx);
	  height = (fontp->ury - fontp->lly);

	  if (textalign & TA_BASELINE)
	    btop = top - fontp->ury;
	  else if (textalign & TA_BOTTOM)
	    btop = top - height;
	  else
	    btop = top;

	  draw_for_invalid_index (hdc, left, btop,
				  width, height);
	  left += width;
	}
      else if (pcb->pbmp)
	{
	  width = pcb->metric.bbw;
	  height = pcb->metric.bbh;
	  
	  if (!(hBMP
		&& (DIBsection_hdc == hdc)
		&& (DIBsection_width == width)
		&& (DIBsection_height == height)))
	    {
	      if (hBMP) DeleteObject (hBMP);
	      hBMP = create_offscreen_bitmap (hdc, width, height, &bits);
	      DIBsection_hdc = hdc;
	      DIBsection_width = width;
	      DIBsection_height = height;
	      if (!hBMP)
		{
		  DeleteDC (hCompatDC);
		  SelectObject (hdc, hOrgBrush);
		  DeleteObject (hFgBrush);
		  return 0;
		}
	    }

	  memcpy (bits, pcb->pbmp, pcb->bitmap_size);

	  if (textalign & TA_BASELINE)
	    btop = top - (pcb->metric.bbh + pcb->metric.bboy);
	  else if (textalign & TA_BOTTOM)
	    btop = top - pcb->metric.bbh;
	  else
	    btop = top;
      
	  horgobj = SelectObject (hCompatDC, hBMP);
	  BitBlt (hdc, left, btop, width, height, hCompatDC, 0, 0, 0xE20746);
	  SelectObject (hCompatDC, horgobj);
	  if (fixed_pitch_size)
	    left += fixed_pitch_size;
	  else
	    left += pcb->metric.dwidth;
	}
      else
	{
	  if (fixed_pitch_size)
	    left += fixed_pitch_size;
	  else
	    left += pcb->metric.dwidth;
	}
      left += character_spacing;
    }

  DeleteDC (hCompatDC);

  SelectObject (hdc, hOrgBrush);
  DeleteObject (hFgBrush);

  return ret;
}
