/* MW32 font specific staff.
   Copyright (C) 2002 Free Software Foundation, Inc.

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

/* MW32 implementation by MIYASHITA Hisashi <himi@meadowy.org> */

#include "config.h"
#include "lisp.h"
#include "frame.h"
#include "charset.h"
#include "coding.h"
#include "mw32term.h"
#include "mw32font.h"
#include "mw32bdf.h"
#include "dispextern.h"
#include "fontset.h"
#include <commdlg.h>

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

extern Lisp_Object Qwidth, Qheight, Qfont, Qfunction;
extern Lisp_Object QCwidth, QCheight, QCfamily, QCweight, QCslant;

Lisp_Object Qany, Qspec, Qstrict;
Lisp_Object Qoverhang, Qrelative_compose, Qdefault_ascent;
Lisp_Object Qencoding, Qfont_unit_byte;
Lisp_Object Q1_byte_set_msb, Q2_byte_set_msb, Qunicode, Qshift_jis;
Lisp_Object Qspacing, Qcentering;

Lisp_Object Qbase, Qweight, Qfixed, Qitalic;
Lisp_Object Qfamily, Qraster, Qscalable;

Lisp_Object Vw32_system_coding_system;
Lisp_Object Vw32_default_font_request_alist;

Lisp_Object Qw32_logfont, Qbdf_font;

Lisp_Object face_unspecified_attrs[LFACE_VECTOR_SIZE];

static const unsigned char *default_fr_name = "default";

static Lisp_Object Vmw32_charset_windows_font_info_alist;
static Lisp_Object Vmw32_font_request_table;
static MW32FontRequest *mw32_fr_table = 0;
static int mw32_fr_num = 0;

#define LF_HASH_SIZE 71
static MW32LogicalFont *lfhashtbl[LF_HASH_SIZE];

static MW32LogicalFont **mw32_plf_table = 0;
static int mw32_lf_num = 0;
static int mw32_plf_alloced_num = 0;

int mw32_lf_loader_num = 0;
LOADLFPROC mw32_lf_loaders[10];

static Lisp_Object mw32_get_font_request_parameter (Lisp_Object, Lisp_Object);

static int mw32_valid_encoding_p (Lisp_Object encoding);
static void mw32_set_encoding_to_lf (MW32LogicalFont *plf, Lisp_Object encoding);
static void mw32_set_encoding_byte_from_charset (MW32LogicalFont *plf, int c);


/***********************************************************************
		   Macros of xfaces.c and fontset.c defs.;_;
 ***********************************************************************/

extern Lisp_Object Qunspecified;
#define LFACE_FONT(LFACE)	    AREF ((LFACE), LFACE_FONT_INDEX)
#define UNSPECIFIEDP(ATTR) EQ ((ATTR), Qunspecified)

/* for fontset.c. */

extern Lisp_Object Vfontset_table;
extern Lisp_Object fontset_ref_via_base (Lisp_Object, int *);
extern void fontset_set (Lisp_Object, int, Lisp_Object);
#define FONTSET_ID(fontset)		XCHAR_TABLE (fontset)->extras[0]
#define FONTSET_NAME(fontset)		XCHAR_TABLE (fontset)->extras[1]
#define FONTSET_FRAME(fontset)		XCHAR_TABLE (fontset)->extras[2]
#define FONTSET_ASCII(fontset)		XCHAR_TABLE (fontset)->contents[0]
#define FONTSET_BASE(fontset)		XCHAR_TABLE (fontset)->parent
#define BASE_FONTSET_P(fontset)		NILP (FONTSET_BASE (fontset))
#define FONTSET_FROM_ID(id) AREF (Vfontset_table, id)
#define FONTSET_REF_VIA_BASE(fontset, c) fontset_ref_via_base (fontset, &c)
#define FONTSET_SET(fontset, c, newelt) fontset_set (fontset, c, newelt)


/***********************************************************************
			   Methods for Windows font.
 ***********************************************************************/

typedef struct mw32_windows_font
{
  LOGFONT logfont;
  HFONT pfont;
  int ttfp;
  MW32CharMetric cur_cm;
  MW32CharMetric cmcache[256];
} mw32_windows_font;

static void
mw32_windows_set_logical_font_from_char (MW32LogicalFont *plf, int c)
{
  Lisp_Object charset_sym, slot;

  if (c < 0)
    {
      charset_sym = CHARSET_SYMBOL (CHARSET_ASCII);
    }
  else
    {
      charset_sym = CHARSET_SYMBOL (CHAR_CHARSET (c));
    }

  slot = assq_no_quit (charset_sym, Vmw32_charset_windows_font_info_alist);

  if (NILP (slot) || !CONSP (slot)) return;
  slot = XCDR (slot);
  if (NILP (slot) || !CONSP (slot)) return;
  slot = XCDR (slot);
  if (NILP (slot) || !CONSP (slot)) return;
  slot = XCAR (slot);

  if (!mw32_valid_encoding_p (slot)) return;
  mw32_set_encoding_to_lf (plf, slot);
}

static void
mw32_textout (MW32LogicalFont *plf, HDC hdc, unsigned char *text,
	      int x, int ybase, int bytes, int* xoffs, RECT *pr,
	      int mode)
{
  HANDLE hold;
  mw32_windows_font *pwf = (mw32_windows_font*) plf->pphys;
  if (pwf->pfont == INVALID_HANDLE_VALUE)
    pwf->pfont = CreateFontIndirect (&pwf->logfont);
  hold = SelectObject (hdc, pwf->pfont);
  ExtTextOut (hdc, x, ybase, mode, pr, text, bytes, xoffs);
  SelectObject (hdc, hold);
}

static int
getDeviceWidth (HDC hdc, int width)
{
  POINT p;
  
  p.x = width;
  p.y = 0;
  LPtoDP (hdc, &p, 1);
  return p.x;
}

static MW32CharMetric
mw32_glyph_metric (MW32LogicalFont *plf, HDC hdc, FontCp cp)
{
  mw32_windows_font *pwf = (mw32_windows_font*) plf->pphys;

#if 0
  if (plf->fixed_pitch)
    {
      if (MW32_CHARMETRIC_VALID_P (pwf->cur_cm))
	return &pwf->cur_cm;

      pwf->cur_cm.width = plf->width;
      pwf->cur_cm.ascent = plf->ascent;
      pwf->cur_cm.descent = plf->descent;
      pwf->cur_cm.overhang = plf->overhang;
      pwf->cur_cm.roverhang = 0;

      return &pwf->cur_cm;
    }
#endif
  if (plf->encoding.font_unit_byte == 1)
    {
      if (cp > 255)
	{
	  MW32CharMetric cm;
	  MW32_CHARMETRIC_INVALIDATE (cm);
	  return cm;
	}
      if (MW32_CHARMETRIC_VALID_P (pwf->cmcache[cp]))
	return pwf->cmcache[cp];
    }

  {
    HANDLE hold;

    if (pwf->pfont == INVALID_HANDLE_VALUE)
      pwf->pfont = CreateFontIndirect (&pwf->logfont);
    hold = SelectObject (hdc, pwf->pfont);

    if (pwf->ttfp)
      {
	ABC abc;
	int ret = GetCharABCWidths (hdc, cp, cp, &abc);

	if (ret)
	  {
	    if (abc.abcA < 0)
	      pwf->cur_cm.roverhang = -abc.abcA;
	    else
	      pwf->cur_cm.roverhang = 0;
	    pwf->cur_cm.width = abc.abcA + abc.abcB + abc.abcC;
	    if (abc.abcC < 0)
	      pwf->cur_cm.overhang = -abc.abcC;
	    else
	      pwf->cur_cm.overhang = 0;
	  }
	else
	  {
	    MW32CharMetric cm;
	    MW32_CHARMETRIC_INVALIDATE (cm);
	    SelectObject (hdc, hold);
	    return cm;
	  }
      }
    else
      {
	unsigned char str[MAXBYTES1FCP];
	unsigned char *p;
	SIZE sz;

	p = str;
	SERIALIZE_FONTCP (p, cp);
	if (GetTextExtentPoint32 (hdc, str, p - str, &sz))
	  {
	    pwf->cur_cm.width = sz.cx;
	  }
	else
	  {
	    int w;
	    GetCharWidth (hdc, cp, cp, &w);
	    pwf->cur_cm.width = w;
	  }
	pwf->cur_cm.roverhang = 0;
	pwf->cur_cm.overhang = plf->overhang;
      }
    pwf->cur_cm.ascent = plf->ascent;
    pwf->cur_cm.descent = plf->descent;
    pwf->cur_cm.width += getDeviceWidth (hdc, plf->character_spacing);

    SelectObject (hdc, hold);
  }
  if (plf->encoding.font_unit_byte == 1)
    pwf->cmcache[cp] = pwf->cur_cm;
  
  return pwf->cur_cm;
}

static int
mw32_set_layout (MW32LogicalFont *plf, HDC hdc, unsigned char *pstr,
		 int nbytes, int maxextent, int *xoffs,
		 int *pleft_ov, int *pright_ov)
{
#if 0 /* No layout information is used currently. */
  int result;
  HANDLE hold;
  GCP_RESULTS gcp;
  mw32_windows_font *pwf = (mw32_windows_font*) plf->pphys;
  if (pwf->pfont == INVALID_HANDLE_VALUE)
    pwf->pfont = CreateFontIndirect (&pwf->logfont);

  if ((maxextent == 0) && (nbytes == 1))
    {
      *xoffs = 0;
      *pleft_ov = 0;
      *pright_ov = 0;
      return 1;
    }

  hold = SelectObject (hdc, pwf->pfont);

  memset (&gcp, 0, sizeof (gcp));
  gcp.lStructSize = sizeof (GCP_RESULTS);
  gcp.lpOutString = (LPTSTR) alloca (nbytes);
  gcp.lpOrder = (UINT*) alloca (sizeof (UINT) * nbytes);
  gcp.lpDx = (int*) alloca (sizeof (int) * nbytes);
  gcp.lpCaretPos = (int*) alloca (sizeof (int) * nbytes);
  gcp.lpGlyphs = (LPWSTR) alloca (sizeof (WCHAR) * nbytes * 10);
  gcp.nGlyphs = nbytes * 10;
  result = GetCharacterPlacement (hdc, pstr, nbytes, maxextent, &gcp,
				  GCP_USEKERNING | GCP_MAXEXTENT | GCP_JUSTIFY);
  *pleft_ov = 0;
  *pright_ov = plf->overhang;
  if (!result)
    {
      LPVOID mbuf;

      FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER
		     | FORMAT_MESSAGE_FROM_SYSTEM
		     | FORMAT_MESSAGE_IGNORE_INSERTS,
		     NULL, GetLastError (),
		     MAKELANGID (LANG_NEUTRAL, SUBLANG_NEUTRAL),
		     (LPTSTR) &mbuf, 0 , NULL);
      /* fprintf (stderr, mbuf); /* Currently meaningless */
      LocalFree (mbuf);
      return 0;
    }
  // memcpy (xoffs, gcp.lpDx, sizeof (int) * nbytes);

  SelectObject (hdc, hold);

  return 1;
#else
  return 0;
#endif
}

static void mw32_logfont_free (MW32LogicalFont *plf)
{
  mw32_windows_font *pwf = (mw32_windows_font*) plf->pphys;

  if (pwf->pfont != INVALID_HANDLE_VALUE)
    DeleteObject (pwf->pfont);
  xfree (pwf);
}

static int mw32_lf_equal (MW32LogicalFont *plf1, MW32LogicalFont *plf2);

static int
mw32_logfont_equal (MW32LogicalFont *self, MW32LogicalFont *other)
{
#define LF_EQUAL_ELEM(elem) (plf1->elem == plf2->elem)
  if (self->pphys_type == other->pphys_type)
    {
      LOGFONT *plf1 = &((mw32_windows_font*) self->pphys)->logfont;
      LOGFONT *plf2 = &((mw32_windows_font*) other->pphys)->logfont;

      return (mw32_lf_equal (self, other)
	      && LF_EQUAL_ELEM (lfCharSet)
	      && LF_EQUAL_ELEM (lfWeight)
	      && LF_EQUAL_ELEM (lfItalic)
	      && LF_EQUAL_ELEM (lfUnderline)
	      && LF_EQUAL_ELEM (lfStrikeOut)
	      && strcmp (plf1->lfFaceName, plf2->lfFaceName) == 0);
      /*
	The following variables are ignored.
	lfQuality, lfOutPrecision, lfPitchAndFamily, lfEscapement,
	lfClipPrecision
      */
    }
  return 0;
#undef LF_EQUAL_ELEM
}

static int
mw32_set_windows_logical_font (struct frame *f, MW32LogicalFont *plf,
			       LOGFONT *plogf, int c)
{
  mw32_windows_font *pwf;
  HFONT hf;

  pwf = (mw32_windows_font*) xmalloc (sizeof (mw32_windows_font));

  /* set attributes. */
  if (hf = CreateFontIndirect (plogf))
    {
      HWND hwnd;
      HDC hdc;
      HANDLE oldobj;
      TEXTMETRIC tm;
      int flag;
      HDC mhdc = INVALID_HANDLE_VALUE;

      if (f->output_data.mw32->message_thread_hdc == INVALID_HANDLE_VALUE)
	{
	  mhdc = GetDC (FRAME_MW32_WINDOW (f));
	  mw32_setup_default_hdc (mhdc);
	  f->output_data.mw32->message_thread_hdc = mhdc;
	}

      hwnd = FRAME_MW32_WINDOW (f);
      if (hwnd)
	hdc = FRAME_HDC (f);
      else
	hdc = GetDC (GetDesktopWindow ());

      oldobj = SelectObject (hdc, hf);
      flag = GetTextMetrics (hdc, &tm);
      SelectObject (hdc, oldobj);
      if (!hwnd)
	ReleaseDC (GetDesktopWindow (), hdc);
      else if (mhdc != INVALID_HANDLE_VALUE)
	{
	  ReleaseDC (FRAME_MW32_WINDOW (f), mhdc);
	  f->output_data.mw32->message_thread_hdc = INVALID_HANDLE_VALUE;
	}
      
      if (!flag)
	{
	  xfree (pwf);
	  return 0;
	}
      /* If the font width is not desired, adjust `lfWidth' and
	 recreate it later. */
      if (plogf->lfWidth != 0 && plogf->lfWidth != tm.tmAveCharWidth)
	{
	  int target_width = plogf->lfWidth;
	  plogf->lfWidth = plogf->lfWidth * plogf->lfWidth / tm.tmAveCharWidth;
	  tm.tmAveCharWidth = target_width;
	  DeleteObject (hf);
	  hf = INVALID_HANDLE_VALUE;
	}
      
      pwf->pfont = hf;

      plf->width = tm.tmAveCharWidth;
      plf->ascent = tm.tmAscent;
      plf->descent = tm.tmHeight - tm.tmAscent;
      plf->overhang = tm.tmOverhang;
      plf->fixed_pitch = !(tm.tmPitchAndFamily & TMPF_FIXED_PITCH);
      mw32_windows_set_logical_font_from_char (plf, c);
      plf->relative_compose = 0;
      plf->default_ascent = 0;
      plf->character_spacing = 0;
      plf->centering = 0;

      pwf->ttfp = (tm.tmPitchAndFamily & TMPF_TRUETYPE);
    }
  else
    {
      xfree (pwf);
      return 0;
    }

  /* set methods */
  plf->textout = mw32_textout;
  plf->glyph_metric = mw32_glyph_metric;
  plf->set_layout = mw32_set_layout;
  plf->free = mw32_logfont_free;
  plf->equal = mw32_logfont_equal;

  /* set PF object.  */
  plf->pphys_type = MW32_FID_WINDOWS_FONT;
  plf->pphys = pwf;
  pwf->logfont = *plogf;
  {
    int i;
    MW32_CHARMETRIC_INVALIDATE (pwf->cur_cm);
    for (i = 0; i < (sizeof (pwf->cmcache) / sizeof (pwf->cmcache[0])); i++)
      MW32_CHARMETRIC_INVALIDATE (pwf->cmcache[i]);
  }

  return 1;
}


/***********************************************************************
			   Methods for BDF font.
 ***********************************************************************/
static void
mw32_bdffont_textout (MW32LogicalFont *plf, HDC hdc, unsigned char *text,
		      int x, int ybase, int bytes, int* xoffs, RECT *pr,
		      int mode)
{
  bdffont *pbf = (bdffont*) plf->pphys;
  if (mode == ETO_OPAQUE)
    {
      HBRUSH bk = CreateSolidBrush (GetBkColor (hdc));
      FillRect (hdc, pr, bk);
      DeleteObject (bk);
    }
  mw32_BDF_TextOut (pbf, hdc, x, ybase, text,
		    plf->encoding.font_unit_byte, bytes, 0,
		    getDeviceWidth (hdc, plf->character_spacing));
}

static MW32CharMetric
mw32_bdffont_glyph_metric (MW32LogicalFont *plf, HDC hdc, FontCp cp)
{
  MW32CharMetric cm;
  glyph_struct glyph;
  bdffont *pbf = (bdffont*) plf->pphys;

  if (mw32_get_bdf_glyph (pbf, cp, 0, &glyph))
    {
      cm.width = glyph.metric.dwidth;
      cm.ascent = glyph.metric.bbh + glyph.metric.bboy;
      cm.descent = -glyph.metric.bboy;
      cm.overhang = 0;
      cm.roverhang = 0;
    }
  else
    {
      cm.width = plf->width;
      cm.ascent = plf->ascent;
      cm.descent = plf->descent;
      cm.overhang = plf->overhang;
      cm.roverhang = 0;
    }
  cm.width += getDeviceWidth (hdc, plf->character_spacing);

  return cm;
}

static int
mw32_bdffont_set_layout (MW32LogicalFont *plf, HDC hdc, unsigned char *pstr,
			 int nbytes, int maxextent, int *xoffs,
			 int *pleft_ov, int *pright_ov)
{
  /* TODO */
  return 1;
}

static void mw32_bdffont_free (MW32LogicalFont *plf)
{
  bdffont* pbf = (bdffont*) plf->pphys;

  if (pbf != NULL)
    {
      mw32_free_bdf_font (pbf);
    }
}

static int
mw32_bdffont_equal (MW32LogicalFont *self, MW32LogicalFont *other)
{
  if (self->pphys_type == other->pphys_type)
    {
      bdffont *pbf1 = (bdffont*) self->pphys;
      bdffont *pbf2 = (bdffont*) other->pphys;
      
      return mw32_lf_equal (self, other)
	&& strcmp (pbf1->filename, pbf2->filename) == 0;
    }
  return 0;
}

static int
mw32_set_bdf_font (struct frame *f,
		   MW32LogicalFont *plf,
		   unsigned char* filename)
{
  bdffont* pbf;

  if (pbf = mw32_init_bdf_font (filename))
    {
      plf->width = pbf->urx - pbf->llx;
      plf->ascent = pbf->ury;
      plf->descent = -pbf->lly;
      plf->overhang = 0;
      plf->relative_compose = pbf->relative_compose;
      plf->default_ascent = pbf->default_ascent;
      plf->character_spacing = 0;
      plf->fixed_pitch = TRUE;
      plf->centering = 0;
    }
  else
    {
      return 0;
    }

  /* set methods */
  plf->textout = mw32_bdffont_textout;
  plf->glyph_metric = mw32_bdffont_glyph_metric;
  plf->set_layout = mw32_bdffont_set_layout;
  plf->free = mw32_bdffont_free;
  plf->equal = mw32_bdffont_equal;

  plf->pphys_type = MW32_FID_BDF_FONT;
  plf->pphys = pbf;

  return 1;
}

/***********************************************************************
			   font management part.
 ***********************************************************************/

int
mw32_get_font_request (const unsigned char *name)
{
  int i;
  MW32FontRequest *pfr;

  pfr = mw32_fr_table;
  for (i = 0; i < mw32_fr_num; i++, pfr++)
    {
      if (strcmp (pfr->name, name) == 0)
	return i;
    }
  return -1;
}

static int
set_ccl_method (ccl_id_type cclid, struct ccl_program *encoder)
{
  extern Lisp_Object Vccl_program_table;
  extern Lisp_Object Qccl_program_idx;
  Lisp_Object encoding, ccl_id, ccl_prog;

  if (!cclid.data) return 0;
  encoding = Fintern_soft (make_string (cclid.data, cclid.size), Qnil);

#if 0
  if (Fcoding_system_p (encoding))
    {
      struct coding_system* lpmccode;

      lpmccode = (struct coding_system*) xmalloc (sizeof (struct coding_system));

      setup_coding_system (encoding, lpmccode);
      if ((lpmccode->type) == coding_type_ccl)
	{
	  xfree (lpmccode);
	  return 0;
	}
      encoder->size = 0;
      encoder->prog = (Lisp_Object *) lpmccode;
    }
  else
    {
      setup_ccl_program (encoder, encoding);
    }
#else
  if (SYMBOLP (encoding) &&
      (SYMBOLP (encoding = Fget (encoding, Qccl_program))) &&
      (setup_ccl_program (encoder, encoding) >= 0))
    return 1;
#endif

  return 0;
}

static void
set_font_info (MW32LogicalFont *plf, int c)
{
  int size;
  struct font_info *fontp;
  extern Lisp_Object Vvertical_centering_font_regexp; /* fontset.c */

  if (!plf->fontip)
    {
      plf->fontip = (struct font_info *) xmalloc (sizeof (struct font_info));
      bzero (plf->fontip, sizeof (struct font_info));
    }
  fontp = plf->fontip;

  fontp->font = plf;
  size = strlen (plf->pfr->name) + 1;
  fontp->font_idx = plf->idx;

  if (fontp->name == 0 || strcmp (fontp->name, plf->pfr->name) != 0)
    {
      if (fontp->name)
	xfree (fontp->name);
      fontp->name = (char *) xmalloc (size);
      memcpy (fontp->name, plf->pfr->name, size);
    }

  if (fontp->full_name == 0 || strcmp (fontp->full_name, plf->pfr->name) != 0)
    {
      if (fontp->full_name)
	xfree (fontp->full_name);
      fontp->full_name = (char *) xmalloc (size);
      memcpy (fontp->full_name, plf->pfr->name, size);
    }

  fontp->size = FONT_WIDTH (plf);
  fontp->height = FONT_HEIGHT (plf);
  fontp->baseline_offset = 0;
  fontp->relative_compose = plf->relative_compose;
  fontp->default_ascent = plf->default_ascent;

  if (c >= 0)
    {
      int i;

      fontp->charset = CHAR_CHARSET (c);

      fontp->font_encoder = NULL;
      if (plf->encoding.type == ENCODING_EXTERNAL)
	{
	  struct ccl_program ccl_prog;
	  if (set_ccl_method (plf->encoding.e.ccl_method, &ccl_prog))
	    {
	      fontp->font_encoder
		= (struct ccl_program *) xmalloc (sizeof (struct ccl_program));
	      *(fontp->font_encoder) = ccl_prog;
	    }
	}
      fontp->encoding[0] = fontp->encoding[1] = plf->encoding.type;
      for (i = MIN_CHARSET_OFFICIAL_DIMENSION1; i <= MAX_CHARSET; i++)
	fontp->encoding[i] = plf->encoding.type;
    }
  else
    {
      fontp->charset = CHARSET_ASCII;
      fontp->encoding[1] = plf->encoding.type;
    }
  if (plf->centering
      || (STRINGP (Vvertical_centering_font_regexp)
	  && (fast_c_string_match_ignore_case
	      (Vvertical_centering_font_regexp, fontp->full_name) >= 0)))
    fontp->vertical_centering = 1;
}

/* this function load MW32LogicalFont, and returns font_info*.
   But by this function, FR layer cannot use face-related data to
   select LF.  So you should avoid using this function to select
   font for face.  This function is only for the interface of
   fontset.c */
struct font_info*
mw32_load_font (struct frame *f,
		char *xname, int size)
{
  unsigned char *name = (unsigned char*) xname;
  int idx;
  MW32FontRequest *pfr;
  MW32LogicalFont *plf;
  
  idx = mw32_get_font_request (name);
  if (idx < 0) return (struct font_info *) NULL;
  pfr = &mw32_fr_table[idx];
  plf = mw32_load_lf (pfr, f, NULL, -1);
  if (!plf) return NULL;
  if (plf->fontip) return plf->fontip;

  set_font_info (plf, -1);

  return plf->fontip;
}

#if 0
void
w32_unload_font (struct frame *f, FONT_TYPE *font)
{
#if 0
  HFONT hf;
  hf = MWF_GET_FONT (font->fs_font, font->property_index);

  if (hf)
    {
      DeleteObject (*phf);
      *phf = NULL;
    }
#endif
  xfree (font);

  return;
}
#endif

static void
mw32_find_ccl_program (struct font_info *fontp)
{
  MW32LogicalFont *plf;

  plf = (MW32LogicalFont*) fontp->font;
  if (!plf) return;

  fontp->font_encoder = NULL;
  if (plf->encoding.type == ENCODING_EXTERNAL)
    {
      struct ccl_program ccl_prog;
      if (set_ccl_method (plf->encoding.e.ccl_method, &ccl_prog))
	{
	  fontp->font_encoder
	    = (struct ccl_program *) xmalloc (sizeof (struct ccl_program));
	  *(fontp->font_encoder) = ccl_prog;
	}
    }

  return;
}

static Lisp_Object
mw32_get_font_request_parameter (Lisp_Object param, Lisp_Object alist)
{
  Lisp_Object elem;

  elem = assq_no_quit (param, alist);

  if (NILP (elem))
    {
      elem = assq_no_quit (param, Vw32_default_font_request_alist);
      if (NILP (elem)) return Qnil;
    }

  return Fcdr (elem);
}

static void 
mw32_internal_add_font (unsigned char *name, Lisp_Object alist)
{
  int i, idx, size;
  MW32FontRequest *pfr;

  idx = mw32_fr_num;
  size = ASIZE (Vmw32_font_request_table);
  if ((idx + 1) >= size)
    {
      Lisp_Object tem;
      int newsize = size + 64;

      tem = Fmake_vector (make_number (newsize), Qnil);
      for (i = 0; i < size; i++)
	AREF (tem, i) = AREF (Vmw32_font_request_table, i);
      Vmw32_font_request_table = tem;

      mw32_fr_table = (MW32FontRequest*) xrealloc (mw32_fr_table,
						   sizeof (MW32FontRequest)
						   * newsize);
    }
  AREF (Vmw32_font_request_table, idx) = alist;
  pfr = &mw32_fr_table[idx];
  pfr->idx = idx;
  pfr->loaded_LF_num = 0;
  pfr->num_pLFs = 4;
  pfr->pLFs = (MW32LogicalFont**) xmalloc (sizeof (MW32LogicalFont *)
					   * pfr->num_pLFs);
  bzero (pfr->pLFs, sizeof (MW32LogicalFont *) * pfr->num_pLFs);
  pfr->name = xstrdup (name);

  mw32_fr_num++;
}

static Lisp_Object
logfont_to_lisp_object (LOGFONT *lf)
{
  Lisp_Object logfont[13];
  /* [0] is identifier.
     [1] is name.
     [2] is width. 
     [3] is height. 
     [4] is weight.
     [5] is orientation.
     [6] is italic.
     [7] is underline.
     [8] is strikeout.
     [9] is charset.
     [10] is quality. 
     [11] is OutPrecision 
     [12] is PitchAndFamily */

  unsigned char *buf;
  struct coding_system coding;
  int nbytes, size, bufsize;

  nbytes = strlen (lf->lfFaceName);
  setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system), &coding);
  bufsize = decoding_buffer_size (&coding, nbytes); 
  buf = (unsigned char *) alloca (bufsize);
  decode_coding (&coding, lf->lfFaceName, buf, nbytes, bufsize);
  size = coding.produced;
  logfont[0] = Qw32_logfont;
  logfont[1] = make_string (buf, size);

  logfont[2] = make_number (lf->lfWidth);
  logfont[3] = make_number (lf->lfHeight);
  XSETFASTINT (logfont[4], lf->lfWeight);
  XSETFASTINT (logfont[5], lf->lfOrientation);
  logfont[6] = (lf->lfItalic) ? Qt:Qnil;
  logfont[7] = (lf->lfUnderline) ? Qt:Qnil;
  logfont[8] = (lf->lfStrikeOut) ? Qt:Qnil;
  XSETFASTINT (logfont[9], lf->lfCharSet);
  XSETFASTINT (logfont[10], lf->lfQuality);
  XSETFASTINT (logfont[11], lf->lfOutPrecision);
  XSETFASTINT (logfont[12], lf->lfPitchAndFamily);

  return Flist (XFASTINT (13), logfont);
}

static int
encode_logfont_name (Lisp_Object src, char *dest)
{
  int bufsize, size;
  struct coding_system coding;
  char *buf;

  setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system), &coding);
  bufsize = encoding_buffer_size (&coding, LISPY_STRING_BYTES (src) + 1);

  buf = alloca (bufsize);
  /* LOGFONT is NEVER accessed by lisp interpreter. */
  encode_coding (&coding, XSTRING (src)->data, buf,
		 LISPY_STRING_BYTES (src), bufsize);
  size = coding.produced;
  if (size >= LOGFONT_FACENAME_MAX)
    error ("The name is too long. Max is %d", LOGFONT_FACENAME_MAX);

  buf[size] = '\0';
  if (dest)
    strcpy (dest, buf);

  return size;
}

static LOGFONT
lisp_object_to_logfont (Lisp_Object font)
{
  LOGFONT lf;
  Lisp_Object tmpcdr, tmpcar;

  tmpcdr = Fcdr (font);
  tmpcar = Fcar (tmpcdr);

  encode_logfont_name (tmpcar, lf.lfFaceName);

  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfWidth = XINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfHeight = XINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfWeight = XFASTINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfOrientation = XFASTINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfItalic = !NILP (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfUnderline = !NILP (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfStrikeOut = !NILP (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfCharSet = XFASTINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfQuality = XFASTINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfOutPrecision = XFASTINT (tmpcar);
  tmpcdr = Fcdr (tmpcdr);
  tmpcar = Fcar (tmpcdr);
  lf.lfPitchAndFamily = XFASTINT (tmpcar);

/* Set the default value... */
  lf.lfEscapement = lf.lfOrientation;
  lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;

  return lf;
}

static int
check_lisp_object_logfont (Lisp_Object llf, int noerror)
{
  Lisp_Object tmpcar, tmpcdr, type;

  if (noerror && !CONSP (llf)) return 0;
  else CHECK_LIST (llf, 0);

  tmpcar = CAR (llf);
  tmpcdr = CDR (llf);
  if (noerror && !SYMBOLP (tmpcar)) return 0;
  else CHECK_SYMBOL (tmpcar, 1); /* w32-logfont or bdf-font */

  type = tmpcar;
  tmpcar = CAR (tmpcdr);
  tmpcdr = CDR (tmpcdr);
  if (noerror && !STRINGP (tmpcar)) return 0;
  else CHECK_STRING (tmpcar, 1); /* name  or path */

  if (type == Qw32_logfont)
    {
      encode_logfont_name (tmpcar, NULL);
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 2); /* width */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 3); /* height */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 4); /* weight */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 5); /* orientation */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (!((EQ (tmpcar, Qnil)) || (EQ (tmpcar, Qt)))) /* italic */
	{
	  if (noerror) return 0;
	  error ("italic cell must be nil or t");
	}
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (!((EQ (tmpcar, Qnil)) || (EQ (tmpcar, Qt)))) /* underline */
	{
	  if (noerror) return 0;
	  error ("underline cell must be nil or t");
	}
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (!((EQ (tmpcar, Qnil)) || (EQ (tmpcar, Qt)))) /* strikeout */
	{
	  if (noerror) return 0;
	  error ("strikeout cell must be nil or t");
	}
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 6); /* charset */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 7); /* quality */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 8); /* OutPrecision */
      tmpcar = CAR (tmpcdr);
      tmpcdr = CDR (tmpcdr);
      if (noerror && !INTEGERP (tmpcar)) return 0;
      else CHECK_NUMBER (tmpcar, 9); /* PitchAndFamily */
    }
  else if (type != Qbdf_font)
    {
      if (noerror) return 0;
      else error ("undefined type of logfont %s", XSYMBOL (type)->name);
    }

  if (!NILP (tmpcdr))
    {
      if (noerror) return 0;
      else error ("Invalid logfont. Its length is too long.");
    }

  return 1;
}

/* encoding in Meadow font system has one of the following value.
   o ... nil (default)
   o ... 1-byte-set-msb
   o ... 2-byte-set-msb
   o ... shift_jis
   o ... unicode
   o ... <encoder> registered by (w32-regist-font-encoder).
 */
static int
mw32_valid_encoding_p (Lisp_Object encoding)
{
  if (NILP (encoding)) return 1;
  if (EQ (encoding, Q1_byte_set_msb)) return 1;
  if (EQ (encoding, Q2_byte_set_msb)) return 1;
  if (EQ (encoding, Qshift_jis)) return 1;
  if (EQ (encoding, Qunicode)) return 1;
  if (!SYMBOLP (encoding)) return 0;
  if (ENCODING_METHOD_P (encoding)) return 1;

  return 0;
}

static void
mw32_set_encoding_to_lf (MW32LogicalFont *plf, Lisp_Object encoding)
{
  if (NILP (encoding))
    {
      /* Actually, need not set
	 because all members must have been cleared.  */
      plf->encoding.type = ENCODING_DIMENSION;
    }
  else if (EQ (encoding, Q1_byte_set_msb))
    {
      plf->encoding.type = ENCODING_BYTE1MSB1;
      plf->encoding.font_unit_byte = 1;
    }
  else if (EQ (encoding, Q2_byte_set_msb))
    {
      plf->encoding.type = ENCODING_BYTE2MSB11;
      plf->encoding.font_unit_byte = 2;
    }
  else if (EQ (encoding, Qshift_jis))
    {
      plf->encoding.type = ENCODING_SHIFTJIS;
      plf->encoding.font_unit_byte = 2;
    }
  else if (EQ (encoding, Qunicode))
    {
      plf->encoding.type = ENCODING_UNICODE;
      plf->encoding.font_unit_byte = 2;
    }
  else if (ENCODING_METHOD_P (encoding))
    {
      Lisp_Object symname, byte;
      char *data;
      int size;

      plf->encoding.type = ENCODING_EXTERNAL;
      if (plf->encoding.e.ccl_method.data)
	xfree (plf->encoding.e.ccl_method.data);

      symname = Fsymbol_name (encoding);
      size = LISPY_STRING_BYTES (symname);
      data = (char*) xmalloc (size);
      plf->encoding.e.ccl_method.size = size;
      plf->encoding.e.ccl_method.data = data;
      memcpy (data, XSTRING (symname)->data, size);

      byte = Fget (encoding, Qfont_unit_byte);
      if (NUMBERP (byte) && (XINT (byte) > 0))
	{
	  plf->encoding.font_unit_byte = XFASTINT (byte);
	}
    }
  else
    {
      abort ();
    }
}

static void
mw32_set_encoding_byte_from_charset (MW32LogicalFont *plf, int c)
{
  if (plf->encoding.font_unit_byte == 0)
    {
      if (c < 0)
	{
	  plf->encoding.font_unit_byte = 1;
	}
      else
	{
	  Lisp_Object charset = CHAR_CHARSET (c);
	  if (NILP (charset))
	    plf->encoding.font_unit_byte = 1;
	  else
	    plf->encoding.font_unit_byte = CHARSET_DIMENSION (charset);
	}
    }
}

struct font_info*
mw32_get_font_info (FRAME_PTR f, int font_idx)
{
  MW32LogicalFont *plf;

  if (font_idx >= mw32_lf_num) return NULL;
  plf = mw32_plf_table[font_idx];
  if (!plf) return NULL;
  return plf->fontip;
}

struct font_info*
mw32_query_font (struct frame *f, char *fontname)
{
  MW32FontRequest *pfr;
  int idx = mw32_get_font_request (fontname);
  if (idx < 0) return NULL;
  pfr = &mw32_fr_table[idx];
  if (pfr->loaded_LF_num > 0)
    return pfr->pLFs[0]->fontip;
  return NULL;
}

int
w32_score_logfont (logfont_candidates *plc, Lisp_Object request)
{
  int score;
  Lisp_Object key, val, tem;
  char *str;

  CHECK_LIST (request, 0);
  key = XCONS (request)->car;
  val = XCONS (request)->cdr;
  if (CONSP (key))
    {
      CHECK_NUMBER (val, 0);
      score = XINT (val);
      tem = key;
      key = XCONS (tem)->car;
      val = XCONS (tem)->cdr;
    }
  else
    score = 1;

  if (EQ (key, Qwidth))
    {
      CHECK_NUMBER (val, 0);
      if ((plc->type == 0)
	  && ((XINT (val) != plc->width)))
	score = 0;
    }
  else if (EQ (key, Qheight))
    {
      CHECK_NUMBER (val, 0);
      if ((plc->type == 0)
	  && ((XINT (val) != plc->height)))
	score = 0;
    }
  else if (EQ (key, Qweight))
    {
      CHECK_NUMBER (val, 0);
      if ((plc->type == 0)
	  && ((XINT (val) != plc->lf.lfWeight)))
	score = 0;
    }
  else if (EQ (key, Qbase))
    {
      CHECK_NUMBER (val, 0);
      if (XINT (val) != plc->base)
	score = 0;
    }
  else if (EQ (key, Qitalic))
    {
      if (plc->type == 0
	  && ((NILP (val) && plc->lf.lfItalic)
	      || (!NILP (val) && !plc->lf.lfItalic)))
      	score = 0;
    }
  else if (EQ (key, Qfixed))
    {
      int fixedp = (plc->lf.lfPitchAndFamily & FIXED_PITCH);
      if ((NILP (val) && fixedp)
	  || (!NILP (val) && !fixedp))
	score = 0;
    }
  else if (EQ (key, Qfamily))
    {
      if (NILP (Fstring_equal (val, plc->family)))
	score = 0;
    }

  return score;
}

static Lisp_Object
xlfd_pattern_regexp (Lisp_Object pattern)
{
  if (!index (XSTRING (pattern)->data, '*')
      && !index (XSTRING (pattern)->data, '?'))
    /* PATTERN does not contain any wild cards.  */
    return pattern;

  {
    /* We must at first update the cached data.  */
    char *regex = (char *) alloca (XSTRING (pattern)->size * 2 + 3);
    char *p0, *p1 = regex;

    /* Convert "*" to ".*", "?" to ".".  */
    *p1++ = '^';
    for (p0 = (char *) XSTRING (pattern)->data; *p0; p0++)
      {
	if (*p0 == '*')
	  {
	    *p1++ = '.';
	    *p1++ = '*';
	  }
	else if (*p0 == '?')
	  *p1++ = '.';
	else
	  *p1++ = *p0;
      }
    *p1++ = '$';
    *p1++ = 0;

    return build_string (regex);
  }
}

/* Emulate x_list_fonts (). */
/* The specification of the original version of x_list_fonts () is the
   following.

   Return a list of names of available fonts matching PATTERN on frame F.

   If SIZE is > 0, it is the size (maximum bounds width) of fonts
   to be listed.

   SIZE < 0 means include scalable fonts.

   Frame F null means we have not yet created any frame on X, and
   consult the first display in x_display_list.  MAXNAMES sets a limit
   on how many fonts to match.  */
/* On MW32, we ignore SIZE argument because FR layer should ignore actual
   font attributes.  */

Lisp_Object
mw32_list_fonts (struct frame *f,
		 Lisp_Object pattern,
		 int size,
		 int maxnames)
{
  int i, num;
  char *name;
  Lisp_Object list = Qnil, patterns, regexp;

  patterns = Fassoc (pattern, Valternate_fontname_alist);
  if (NILP (patterns))
    patterns = Fcons (pattern, Qnil);

  num = 0;
  for (; CONSP (patterns); patterns = XCDR (patterns))
    {
      pattern = XCAR (patterns);
      if (!STRINGP (pattern))
	continue;

      regexp = xlfd_pattern_regexp (pattern);
      for (i = 0;i < mw32_fr_num;i++)
	{
	  name = mw32_fr_table[i].name;
	  if (fast_c_string_match_ignore_case (regexp, name) >= 0)
	    {
	      list = Fcons (build_string (name), list);
	      num++;
	      if (num >= maxnames) return list;
	    }
	}
     }
  return list;
}


/***********************************************************************
			   LF hash table
 ***********************************************************************/

#define HASHMAXVAL ((1 << 19) - 1) /* 7th Mersenne Prime number:-) */

static int
mw32_hash_ccl_method (struct ccl_id_type cclm)
{
  char *p;
  int i, hashval;
  p = cclm.data;
  hashval = 0;
  for (i = 0; i < cclm.size; i++)
    {
      hashval = (hashval << 8) | *p;
      if (hashval > HASHMAXVAL) hashval %= HASHMAXVAL;
    }
  return hashval;
}

unsigned int
mw32_default_hash_function (MW32LogicalFont *plf)
{
  unsigned int hashval;

  hashval = plf->width;
  hashval = (hashval << 3) ^ plf->ascent;
  hashval = (hashval << 3) ^ plf->descent;
  hashval = (hashval << 2) ^ plf->overhang;
  /* TODO */
  hashval = (hashval << 2) ^ plf->encoding.type;
  hashval = (hashval << 2) ^ plf->relative_compose;
  hashval = (hashval << 2) ^ plf->character_spacing;
  hashval = (hashval << 2) ^ plf->default_ascent;
  hashval = (hashval << 1) ^ plf->fixed_pitch;
  hashval = (hashval << 1) ^ plf->centering;
  if (hashval > HASHMAXVAL) hashval %= HASHMAXVAL;
  hashval = hashval ^ ((int) plf->hash);
  hashval = hashval ^ ((int) plf->textout);
  hashval = hashval ^ ((int) plf->glyph_metric);
  hashval = hashval ^ ((int) plf->set_layout);
  hashval = hashval ^ ((int) plf->free);

  return hashval;
}

static int
mw32_ccl_method_equal (struct ccl_id_type cclm1,
		       struct ccl_id_type cclm2)
{
  return ((cclm1.size == cclm2.size)
	  && ((cclm1.size == 0)
	      || (memcmp (cclm1.data, cclm2.data,
			  cclm1.size * sizeof (char)) == 0)));
}

static int
mw32_lf_equal (MW32LogicalFont *plf1, MW32LogicalFont *plf2)
{
#define LF_EQUAL_ELEM(elem) (plf1->elem == plf2->elem)
  return (LF_EQUAL_ELEM (width)
	  && LF_EQUAL_ELEM (ascent)
	  && LF_EQUAL_ELEM (descent)
	  && LF_EQUAL_ELEM (overhang)
	  /* TODO */
	  && LF_EQUAL_ELEM (encoding.type)
	  && LF_EQUAL_ELEM (relative_compose)
	  && LF_EQUAL_ELEM (character_spacing)
	  && LF_EQUAL_ELEM (default_ascent)
	  && LF_EQUAL_ELEM (fixed_pitch)
	  && LF_EQUAL_ELEM (centering)
	  && LF_EQUAL_ELEM (hash)
	  && LF_EQUAL_ELEM (textout)
	  && LF_EQUAL_ELEM (glyph_metric)
	  && LF_EQUAL_ELEM (set_layout)
	  && LF_EQUAL_ELEM (free)
	  && LF_EQUAL_ELEM (pfr));
#undef LF_EQUAL_ELEM
}

static MW32LogicalFont*
mw32_register_lf_hash (MW32LogicalFont *plf)
{
  unsigned int hashval;
  int idx;
  MW32LogicalFont *pclf, *pclfb;

  hashval = MW32_INVOKE_HASHPROC (plf);
  idx = hashval % LF_HASH_SIZE;
  for (pclf = lfhashtbl[idx], pclfb = NULL; pclf;
       pclfb = pclf, pclf = pclf->pnext)
    if (MW32_INVOKE_EQUALPROC (plf, pclf)) return pclf;

  if (pclfb)
    pclfb->pnext = plf;
  else
    lfhashtbl[idx] = plf;

  return NULL;
}

static void
mw32_unregister_lf_hash (MW32LogicalFont *plf)
{
  unsigned int hashval;
  int idx;
  MW32LogicalFont *pclf, *pclfb;

  hashval = MW32_INVOKE_HASHPROC (plf);
  idx = hashval % LF_HASH_SIZE;
  for (pclf = lfhashtbl[idx], pclfb = NULL; pclf;
       pclfb = pclf, pclf = pclf->pnext)
    if (plf == pclf)
      {
	if (pclfb)
	  pclfb->pnext = pclf->pnext;
	else
	  lfhashtbl[idx] = NULL;
      }

  return; 
}


/***********************************************************************
			   LF loaders
 ***********************************************************************/

int
mw32_add_lf_loader (LOADLFPROC pllfproc)
{
  if ((sizeof (mw32_lf_loaders) / sizeof (mw32_lf_loaders[0]))
      <= mw32_lf_loader_num)
    abort ();
  mw32_lf_loaders[mw32_lf_loader_num++] = pllfproc;
  return mw32_lf_loader_num;
}

/* this function may cause GC!  */
MW32LogicalFont*
mw32_load_lf (MW32FontRequest *pfr, struct frame *f,
	      struct face *face, int c)
{
  int i;
  LOADLFPROC proc;
  MW32LogicalFont *plf;
  for (i = 0; i < mw32_lf_loader_num; i++)
    {
      proc = mw32_lf_loaders[i];
      if (proc)
	{
	  plf = (*proc) (pfr, f, face, c);
	  if (plf)
	    {
	      mw32_set_encoding_byte_from_charset (plf, c);
	      return plf;
	    }
	}
    }

  return NULL;
}

int
mw32_compute_min_glyph_bounds (FRAME_PTR f)
{
  int i, h, w;
  MW32LogicalFont **pplf;
  struct mw32_display_info *dpyinfo = FRAME_MW32_DISPLAY_INFO (f);
  int old_minwidth, old_minheight;
  int minwidth, minheight;

  old_minwidth = dpyinfo->smallest_char_width;
  old_minheight = dpyinfo->smallest_font_height;
  minwidth = minheight = 100000;

  pplf = mw32_plf_table;
  for (i = 0; i < mw32_lf_num; pplf++)
    {
      if (!*pplf || MW32_LF_EMPTY_SLOT_P (*pplf))
	continue;
      w = FONT_WIDTH (*pplf);
      h = FONT_HEIGHT (*pplf);

      if (w < minwidth) minwidth = w;
      if (h < minheight) minheight = h;
      i++;
    }
#if 1
  /* Divide them by 2 as the safety factor. */
  dpyinfo->smallest_char_width = max (minwidth / 2, 1);
  dpyinfo->smallest_font_height = max (minheight / 2, 1);
#else
  fprintf (stderr, "MW:%d, MH:%d\n", minwidth, minheight);
  dpyinfo->smallest_char_width = 1;
  dpyinfo->smallest_font_height = 1;
#endif

  return (minwidth < old_minwidth
	  || minheight < old_minheight);
}

static MW32LogicalFont*
mw32_add_logical_font (MW32FontRequest *pfr)
{
  int i;
  MW32LogicalFont *plf;

  if (mw32_lf_num >= mw32_plf_alloced_num)
    {
      if (mw32_plf_alloced_num == 0)
	{
	  mw32_plf_alloced_num = 50;
	  mw32_plf_table = (MW32LogicalFont**) xmalloc (sizeof (MW32LogicalFont*)
						      * mw32_plf_alloced_num);
	  memset (mw32_plf_table, 0,
		  sizeof (MW32LogicalFont*) * mw32_plf_alloced_num);
	}
      else
	{
	  mw32_plf_table = (MW32LogicalFont**) xrealloc (mw32_plf_table,
							 sizeof (MW32LogicalFont*)
							 * mw32_plf_alloced_num * 2);
	  memset (mw32_plf_table + mw32_plf_alloced_num, 0,
		  sizeof (MW32LogicalFont*) * mw32_plf_alloced_num);
	  mw32_plf_alloced_num *= 2;
	}
      plf = (MW32LogicalFont*) xmalloc (sizeof (MW32LogicalFont));
      mw32_plf_table[mw32_lf_num] = plf;
      i = mw32_lf_num;
    }
  else
    {
      MW32LogicalFont **pplf;
      pplf = mw32_plf_table;
      for (i = 0; i <= mw32_lf_num; i++)
	{
	  if (!*pplf)
	    {
 	      *pplf = plf = (MW32LogicalFont*) xmalloc (sizeof (MW32LogicalFont));
	      break;
	    }
	  pplf++;
	}
      if (i >= mw32_plf_alloced_num) abort ();
    }

  memset (plf, 0, sizeof (MW32LogicalFont));
  plf->pfr = pfr;
  plf->idx = i;
  plf->hash = mw32_default_hash_function;

  if (pfr->num_pLFs < pfr->loaded_LF_num + 1)
    {
      pfr->num_pLFs *= 2;
      pfr->pLFs = (MW32LogicalFont**) xrealloc (pfr->pLFs,
						sizeof (MW32LogicalFont*)
						* (pfr->num_pLFs));
    }
  pfr->pLFs[pfr->loaded_LF_num++] = plf;

  mw32_lf_num++;

  return plf;
}

MW32LogicalFont*
mw32_register_logical_font (FRAME_PTR f, MW32LogicalFont *plfcand)
{
  MW32LogicalFont *plf_result;

  plf_result = mw32_register_lf_hash (plfcand);
  if (!plf_result)
    {
      mw32_compute_min_glyph_bounds (f);
      return plfcand;
    }
  mw32_delete_logical_font (plfcand);

  return plf_result;
}

void
mw32_delete_logical_font (MW32LogicalFont *plf)
{
  int i;
  MW32FontRequest *pfr;

  mw32_unregister_lf_hash (plf);

  if (plf->free)
    MW32_INVOKE_FREEPROC (plf);
  pfr = plf->pfr;

  if (plf->encoding.e.ccl_method.data)
    xfree (plf->encoding.e.ccl_method.data);

  for (i = 0; i < pfr->loaded_LF_num; i++)
    {
      if ((pfr->pLFs)[i] == plf)
	{
	  if ((pfr->loaded_LF_num - i) > 1)
	    memmove (&(pfr->pLFs[i]), &(pfr->pLFs[i + 1]),
		    (pfr->loaded_LF_num - i - 1) * sizeof (MW32LogicalFont*));
	  break;
	}
    }
  pfr->loaded_LF_num--;

  mw32_plf_table[plf->idx] = NULL;
  xfree (plf);
  mw32_lf_num--;
}

static void
mw32_set_option_alist_to_logical_font (MW32LogicalFont *plf,
				       Lisp_Object option)
{
  Lisp_Object val;

  val = mw32_get_font_request_parameter (Qencoding, option);
  if (!NILP (val)) mw32_set_encoding_to_lf (plf, val);
  val = mw32_get_font_request_parameter (Qrelative_compose, option);
  if (INTEGERP (val)) plf->relative_compose = XINT (val);
  val = mw32_get_font_request_parameter (Qdefault_ascent, option);
  if (INTEGERP (val)) plf->default_ascent = XINT (val);
  val = mw32_get_font_request_parameter (Qspacing, option);
  if (INTEGERP (val)) plf->character_spacing = XINT (val);
  val = mw32_get_font_request_parameter (Qcentering, option);
  if (!NILP (val)) plf->centering = 1;
}

static MW32LogicalFont*
mw32_load_lf_from_lisp_object (struct frame *f,
			       MW32FontRequest *pfr,
			       Lisp_Object llf,
			       Lisp_Object option,
			       int c)
{
  MW32LogicalFont *plf;

  if (!check_lisp_object_logfont (llf, 1)) return NULL;
  if (EQ (XCAR (llf), Qbdf_font))
    {
      Lisp_Object tem;
      unsigned char* filename;
      tem = XCDR (llf);
      filename = XSTRING (CAR (tem))->data;
      plf = mw32_add_logical_font (pfr);
      if (!mw32_set_bdf_font (f, plf, filename))
	{
	  mw32_delete_logical_font (plf);
	  return NULL;
	}
    }
  else
    {
      LOGFONT lf;
      lf = lisp_object_to_logfont (llf);
      plf = mw32_add_logical_font (pfr);
      if (!mw32_set_windows_logical_font (f, plf, &lf, c))
	{
	  mw32_delete_logical_font (plf);
	  return NULL;
	}
    }
  mw32_set_option_alist_to_logical_font (plf, option);

  return mw32_register_logical_font (f, plf);
}

static int
mw32_face_family_unspecified_p (Lisp_Object family_attr)
{
  unsigned char *name;

  if (UNSPECIFIEDP (family_attr)) return 1;
  if (!STRINGP (family_attr)) return 1;

  if (LISPY_STRING_BYTES (family_attr) != 1) return 0;

  name = XSTRING (family_attr)->data;
  if (name[0] == '*') return 1;

  return 0;
}

static char mw32_enumerated_face_name[LF_FACESIZE];

static int CALLBACK
enumfont_callback (ENUMLOGFONTEX *elogfont,
		   NEWTEXTMETRICEX *metric,
		   DWORD font_type,
		   LPARAM lparam)
{
  if (metric->ntmTm.tmPitchAndFamily & TMPF_TRUETYPE
      && !(metric->ntmTm.tmPitchAndFamily & TMPF_FIXED_PITCH)
      && elogfont->elfLogFont.lfFaceName[0] != '@')
    {
      memcpy (mw32_enumerated_face_name,
	      elogfont->elfLogFont.lfFaceName, LF_FACESIZE);
      return 0;
    }
  return 1;
}

static int
mw32_convert_lface_to_LOGFONT (struct frame *f,
			       Lisp_Object *attrs, LOGFONT *plogf,
			       int c)
{
  if (c >= 0)
    {
      int cnum;
      Lisp_Object slot, lcnum;
      slot = assq_no_quit (CHARSET_SYMBOL (CHAR_CHARSET (c)),
			   Vmw32_charset_windows_font_info_alist);
      if (NILP (slot) || !CONSP (slot)) return 0;
      slot = XCDR (slot);
      if (NILP (slot) || !CONSP (slot)) return 0;
      lcnum = XCAR (slot);
      if (!NUMBERP (lcnum)) return 0;
      cnum = XINT (lcnum);
      if ((cnum < 0) || (cnum > 255)) return 0;
      plogf->lfCharSet = cnum;
    }
  else
    {
      plogf->lfCharSet = ANSI_CHARSET;
    }

  if (!UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX]))
    {
      Lisp_Object elt = attrs[LFACE_HEIGHT_INDEX];
      int pt;
      if (INTEGERP (elt))
	pt = XINT (elt);
      else if (FLOATP (elt))
	pt = (int) XFLOATINT (elt);
      /* I want this value to be a cell height that includes internal
	 leading, so this should be positive. */
      plogf->lfHeight = (int)(pt * FRAME_MW32_DISPLAY_INFO (f)->resy / 720);
    }

  if (!UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX]))
    {
      extern Lisp_Object Qbold, Qnormal, Qultra_bold, Qextra_bold, Qsemi_bold;
      extern Lisp_Object Qlight, Qsemi_light, Qultra_light, Qextra_light;
      Lisp_Object elt = attrs[LFACE_WEIGHT_INDEX];

      if (EQ (Qnormal, elt))
	plogf->lfWeight = FW_DONTCARE;
      else if (EQ (Qbold, elt))
	plogf->lfWeight = FW_BOLD;
      else if (EQ (Qultra_bold, elt))
	plogf->lfWeight = FW_ULTRABOLD;
      else if (EQ (Qsemi_bold, elt))
	plogf->lfWeight = FW_SEMIBOLD;
      else if (EQ (Qextra_bold, elt))
	plogf->lfWeight = FW_EXTRABOLD;
      else if (EQ (Qlight, elt))
	plogf->lfWeight = FW_LIGHT;
      else if (EQ (Qsemi_light, elt))
	plogf->lfWeight = FW_EXTRALIGHT;
      else if (EQ (Qultra_light, elt))
	plogf->lfWeight = FW_THIN;
      else if (EQ (Qextra_light, elt))
	plogf->lfWeight = FW_THIN;
    }

  if (!UNSPECIFIEDP (attrs[LFACE_AVGWIDTH_INDEX])
      && INTEGERP (attrs[LFACE_AVGWIDTH_INDEX]))
    plogf->lfWidth = XINT (attrs[LFACE_AVGWIDTH_INDEX]) / 10;

  if (!UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX])
      && SYMBOLP (attrs[LFACE_SLANT_INDEX]))
    {
      extern Lisp_Object Qnormal, Qitalic, Qreverse_italic;
      extern Lisp_Object Qoblique, Qreverse_oblique;
      Lisp_Object elt = attrs[LFACE_SLANT_INDEX];

      if (EQ (Qnormal, elt))
	plogf->lfItalic = 0;
      else if (EQ (Qitalic, elt))
	plogf->lfItalic = 1;
      else if (EQ (Qreverse_italic, elt))
	plogf->lfItalic = 0;  /* TODO */
      else if (EQ (Qoblique, elt))
	plogf->lfItalic = 1;
      else if (EQ (Qreverse_oblique, elt))
	plogf->lfItalic = 0;  /* TODO */
    }

  if (!mw32_face_family_unspecified_p (attrs[LFACE_FAMILY_INDEX]))
    encode_logfont_name (attrs[LFACE_FAMILY_INDEX], plogf->lfFaceName);
  else
    {
      HWND hwnd = GetDesktopWindow ();
      HDC hdc = GetDC (hwnd);
      mw32_enumerated_face_name[0] = '\0';
      EnumFontFamiliesEx (hdc, plogf, (FONTENUMPROC) enumfont_callback, 0, 0);
      ReleaseDC (hwnd, hdc);

      if (mw32_enumerated_face_name[0] != '\0')
	memcpy (plogf->lfFaceName, mw32_enumerated_face_name, LF_FACESIZE);
      else
	{
	  plogf->lfHeight = plogf->lfWidth = 0;
	  strncpy (plogf->lfFaceName, "FixedSys", LF_FACESIZE);
	}
    }

  return 1;
}

void
mw32_initialize_default_logfont (LOGFONT* lf)
{
  static LOGFONT default_logfont =
    {
      /* The height is explicitly set here to avoid the annoying
	 variable font width. */
      16, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE, DEFAULT_CHARSET,
      OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
      FF_DONTCARE | FIXED_PITCH, ""
    };

  *lf = default_logfont;
}

static MW32LogicalFont*
mw32_default_logical_font_loader (MW32FontRequest *pfr,
				  struct frame *f,
				  struct face *face,
				  int c)
{
  MW32LogicalFont *plf;
  LOGFONT lf;
  Lisp_Object* attrs;

  if (face)
    attrs = face->lface;
  else
    attrs = face_unspecified_attrs;

  plf = mw32_add_logical_font (pfr);

  mw32_initialize_default_logfont (&lf);
  if ((!mw32_convert_lface_to_LOGFONT (f, attrs, &lf, c))
      || (!mw32_set_windows_logical_font (f, plf, &lf, c)))
    {
      mw32_delete_logical_font (plf);
      return NULL;
    }

  return mw32_register_logical_font (f, plf);
}

/* spec is 6 length vector.
   [ <char or char-table> <avgwidth> <height>
     <family> <weight> <slant> ]
*/
static int
mw32_fr_spec_match_p (Lisp_Object spec, Lisp_Object *attrs, int c)
{
  /* <char or char-table> */
  {
    Lisp_Object chspec;
    int c1, c2, charset, dim;
    int sc1, sc2, scharset, sdim;

    chspec = AREF (spec, 0);
    if (c < 0)
      {
	charset = CHARSET_ASCII;
	c1 = c2 = 0;
      }
    else
      SPLIT_CHAR (c, charset, c1, c2);
    if (INTEGERP (chspec))
      {
	SPLIT_CHAR (XINT (chspec), scharset, sc1, sc2);
	sdim = CHARSET_DIMENSION (scharset);
	if (((sdim == 1) && (sc1 > 0))
	    || ((sdim == 2) && (sc2 > 0)))
	  {
	    if (c != XINT (chspec)) return 0;
	  }
	else
	  {
	    /* chspec is generic character. */
	    if (charset != scharset) return 0;
	    if ((sdim == 2) && (sc1 > 0) && (sc1 != c1)) return 0;
	  }
      }
    else if (CHAR_TABLE_P (chspec))
      {
	if (NILP (Faref (chspec, make_number (c))))
	  return 0;
	else
	  return 0;
      }
  }
  /* <avgwidth> */
  {
    Lisp_Object awspec;

    awspec = AREF (spec, 1);
    if (!EQ (awspec, Qany)
	&& !UNSPECIFIEDP (attrs[LFACE_AVGWIDTH_INDEX]))
      {
	if (!EQ (awspec, attrs[LFACE_AVGWIDTH_INDEX]))
	  return 0;
      }
  }
  /* <height> */
  {
    Lisp_Object hspec;

    hspec = AREF (spec, 2);
    if (!EQ (hspec, Qany)
	&& !UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX]))
      {
	if (!NUMBERP (hspec)) return 0;
	if (XFLOATINT (hspec) != XFLOATINT (attrs[LFACE_HEIGHT_INDEX]))
	  return 0;
      }
  }
  /* <family> */
  {
    Lisp_Object fspec;

    fspec = AREF (spec, 3);
    if (!EQ (fspec, Qany)
	&& !UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX]))
      {
	int r;
	if (!STRINGP (fspec)) return 0;
	if (fast_string_match (fspec, attrs[LFACE_FAMILY_INDEX]) < 0)
	  return 0;
      }
  }
  /* <weight> */
  {
    Lisp_Object wspec;

    wspec = AREF (spec, 4);
    if (!EQ (wspec, Qany)
	&& !UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX]))
      {
	if (!EQ (wspec, attrs[LFACE_WEIGHT_INDEX]))
	  return 0;
      }
  }
  /* <slant> */
  {
    Lisp_Object sspec;

    sspec = AREF (spec, 5);
    if (!EQ (sspec, Qany)
	&& !UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX]))
      {
	if (!EQ (sspec, attrs[LFACE_SLANT_INDEX]))
	  return 0;
      }
  }
  return 1;
}

/* val is (<logfont> [<option-alist>]) */
static MW32LogicalFont*
mw32_load_strict_spec (Lisp_Object val,
		       MW32FontRequest *pfr,
		       struct frame *f,
		       int c)
{
  Lisp_Object logfont, option;

  if (!CONSP (val)) return NULL;

  logfont = XCAR (val);
  val = XCDR (val);

  if (CONSP (val) && CONSP (XCAR (val)))
    option = XCAR (val);
  else
    option = Qnil;

  return  mw32_load_lf_from_lisp_object (f, pfr, logfont, option, c);
}

/* val is (<func>) */
static MW32LogicalFont*
mw32_load_function_spec (Lisp_Object val,
			 MW32FontRequest *pfr,
			 struct frame *f,
			 int c,
			 Lisp_Object face_attrs)
{
  Lisp_Object func, result, logfont, option;
  Lisp_Object args[4];

  if (!MW32_MAIN_THREAD_P ()) return NULL;
  if (!CONSP (val)) return NULL;

  func = XCAR (val);
  if (!FUNCTIONP (func)) return NULL;
  val = XCDR (val);

  args[0] = func;
  args[1] = make_number (c);
  args[2] = face_attrs;
  XSETFRAME (args[3], f);

  result = safe_call (4, args);
  if (!CONSP (result)) return NULL;

  option = Qnil;
  logfont = CAR (result);
  if (!CONSP (logfont))
    {
      logfont = result;
      if (CONSP (val)) option = XCAR (val);
    }
  else
    {
      option = CAR (CDR (result));
    }
  if ((!NILP (option)) && (!CONSP (option))) return NULL;

  return mw32_load_lf_from_lisp_object (f, pfr, logfont, option, c);
}

static Lisp_Object
mw32_convert_face_attrs (Lisp_Object *attrs)
{
  int i = 0;
  Lisp_Object args[5 * 2];
  args[i++] = QCwidth, args[i++] = attrs[LFACE_AVGWIDTH_INDEX];
  args[i++] = QCheight, args[i++] = attrs[LFACE_HEIGHT_INDEX];
  args[i++] = QCfamily, args[i++] = attrs[LFACE_FAMILY_INDEX];
  args[i++] = QCweight, args[i++] = attrs[LFACE_WEIGHT_INDEX];
  args[i++] = QCslant, args[i++] = attrs[LFACE_SLANT_INDEX];
  return Flist (i, args);
}

/*
  (spec
    (<specvec> strict <logfont> [<option-alist>])
                      or
    (<specvec> function <func> [<option-alist>])
    ...)
*/

static MW32LogicalFont*
mw32_spec_base_font_loader (MW32FontRequest *pfr,
			    struct frame *f,
			    struct face *face,
			    int c)
{
  struct gcpro gcpro1, gcpro2;
  MW32LogicalFont *plf;
  Lisp_Object *attrs;
  Lisp_Object alist, specs, tem, spec, type, face_attrs;

  if (face)
    attrs = face->lface;
  else
    attrs = face_unspecified_attrs;

  face_attrs = mw32_convert_face_attrs (attrs);

  alist = AREF (Vmw32_font_request_table, pfr->idx);
  specs = assq_no_quit (Qspec, alist);
  if (!CONSP (specs)) return NULL;
  specs = XCDR (specs);
  GCPRO2 (specs, face_attrs);

  plf = NULL;
  for (; CONSP (specs); specs = XCDR (specs))
    {
      tem = XCAR (specs);
      if (!CONSP (tem)) continue;
      spec = XCAR (tem);
      if (!VECTORP (spec)) continue;
      if (!mw32_fr_spec_match_p (spec, attrs, c)) continue;

      tem = XCDR (tem);
      if (!CONSP (tem)) continue;
      type = XCAR (tem);
      tem = XCDR (tem);

      if (EQ (type, Qstrict))
	plf = mw32_load_strict_spec (tem, pfr, f, c);
      else if (EQ (type, Qfunction))
	plf = mw32_load_function_spec (tem, pfr, f, c, face_attrs);

      if (plf) break;
    }

  UNGCPRO;
  return plf;
}


/***********************************************************************
			   Interfaces for xfaces.c
 ***********************************************************************/

/* Load font of face FACE which is used on frame F to display
   character C.  The name of the font to load is determined by lface
   and fontset of FACE.  */

void
load_face_font (struct frame *f, struct face *face, int c)
{
  int fontset_id;
  Lisp_Object fontset = Qnil;
  MW32FontRequest *pfr;
  MW32LogicalFont *plf = NULL;

  /* First, check the fontset database.  */
  fontset_id = face->fontset;
  if (fontset_id >= 0)
    {
      fontset = FONTSET_FROM_ID (fontset_id);
      if (!NILP (fontset)
	  && !BASE_FONTSET_P (fontset))
	{
	  Lisp_Object elt = FONTSET_REF_VIA_BASE (fontset, c);
	  if (!NILP (elt))
	    {
	      /* A suitable face for C is already recorded, which means
		 that a proper LF is already loaded.  */
	      int face_id = XINT (elt);

	      xassert (face_id == face->id);
	      face = FACE_FROM_ID (f, face_id);
	      plf = mw32_plf_table[face->font_info_id];
	      pfr = plf->pfr;
	    }
	}
    }

  /* If we don't look up plf from the database, try to
     create LF with FR.  */
  if (!plf)
    {
      Lisp_Object lfont_name;
      Lisp_Object *attrs = face->lface;

      /* We try to extract FR name from the face.  */
      lfont_name = attrs[LFACE_FONT_INDEX];
      if (!STRINGP (lfont_name))
	{
	  /* We try to extract FR name from the frame parameter.  */
	  lfont_name = get_frame_param (f, Qfont);
	}
      if (STRINGP (lfont_name))
	{
	  int idx;
	  idx = mw32_get_font_request (XSTRING (lfont_name)->data);
	  if (idx >= 0) pfr = &mw32_fr_table[idx];
	  else pfr = NULL;
	}
      else
	{
	  /* We cound not use the default FR.  */
	  pfr = mw32_fr_table;
	}

      if (pfr)
	{
	  /* try to load LogicalFont.  */
	  plf = mw32_load_lf (pfr, f, face, c);

	  /* Register the loaded LF to the fontset database.  */
	  if (plf
	      && !NILP (fontset)
	      && !BASE_FONTSET_P (fontset))
	    FONTSET_SET (fontset, c, make_number (face->id));

	  if (!plf)
	    add_to_log ("Unable to load font %s",
			build_string (pfr->name), Qnil);
	}
    }

  /* set the LogicalFont if it is found.  */
  if (plf)
    {
      face->font_info_id = plf->idx;
      face->font = plf;
      face->font_name = pfr->name;
      set_font_info (plf, c);
    }
}

/* Set the `font' frame parameter of FRAME determined from `default'
   face attributes LFACE.  If a face or fontset name is explicitely
   specfied in LFACE, use it as is.  Otherwise, determine a font name
   from the other font-related atrributes of LFACE.  In that case, if
   there's no matching font, signals an error.  */
/* For MW32 implementation, we don't call choose_face_font () to obtain
   the default font name.  */

void set_font_frame_param (frame, lface)
{
  struct frame *f = XFRAME (frame);

  if (FRAME_WINDOW_P (f))
    {
      Lisp_Object font_name;
      char *font;
      
      if (STRINGP (LFACE_FONT (lface)))
	{
	  font_name = LFACE_FONT (lface);
	  Fmodify_frame_parameters (frame, Fcons (Fcons (Qfont, font_name), Qnil));
	}
    }
}


/***********************************************************************
		    IME font support for mw32fns.c
 ***********************************************************************/

void
mw32font_set_frame_ime_font_by_llogfont (FRAME_PTR f, Lisp_Object llf)
{
#ifdef IME_CONTROL
  LOGFONT lf;

  if (NILP (llf))
    mw32_initialize_default_logfont (&lf);
  else
    lf = lisp_object_to_logfont (llf);
  f->output_data.mw32->ime_logfont = lf;
  SEND_INFORM_MESSAGE (FRAME_MW32_WINDOW (f),
		       WM_MULE_IMM_SET_COMPOSITION_FONT,
		       (WPARAM) f,
		       (LPARAM) &lf);
#endif /* IME_CONTROL */
}


/***********************************************************************
			   Lisp functions
 ***********************************************************************/
  
DEFUN ("w32-check-logfont", Fw32_check_logfont, Sw32_check_logfont, 1, 1, 0,
       "Check validity of LOGFONT. \n\
A correct LOGFONT must be a list that have 2 or 12 elements.\n\
(bdf-font path) or \n\
(w32-logfont name width height weight orientation italic underline\n\
strikeout charset quality OutPrecision PitchAndFamily)")
     (logfont)
     Lisp_Object logfont;
/* The w32-logfont is to specify type of logfont.
The width must be a string that describes the name of its font.\n\
The height must be a number that describes the width of its font.\n\
The weight must be a number that describes the weight.(1-1000)\n\
The orientation must be a number that describes the orientation.(angle)\n\
If the italic is nil, it is not italic font.\n\
If the strikeout is nil, it doesn't have a strikeout line.\n\
The charset must be a number that describes the charset that it uses.\n\
(128 is SJIS, 0 is ANSI)\n\
The quality must be a number that describes its quality.(0-2) */
{
  if (!check_lisp_object_logfont (logfont, 0))
    return Qnil;
  return Qt;
}

static void
mw32_check_font_option (Lisp_Object option)
{
  Lisp_Object encoding, relative_compose, default_ascent;
  Lisp_Object centering, spacing;

  encoding = mw32_get_font_request_parameter (Qencoding, option);
  if (!NILP (encoding) && (!mw32_valid_encoding_p (encoding)))
    Fsignal (Qerror, Fcons (build_string ("Invalid encoding"),
			    Fcons (encoding, Qnil)));

  relative_compose = mw32_get_font_request_parameter (Qrelative_compose,
						      option);
  if (!NILP (relative_compose)) CHECK_NUMBER (relative_compose, 0);

  default_ascent = mw32_get_font_request_parameter (Qdefault_ascent,
						    option);
  if (!NILP (default_ascent)) CHECK_NUMBER (default_ascent, 1);

  spacing = mw32_get_font_request_parameter (Qspacing, option);
  if (!NILP (spacing)) CHECK_NUMBER (spacing, 2);

  centering = mw32_get_font_request_parameter (Qcentering, option);
  /* needless to check centering */
}

static void
mw32_check_font_request_alist (Lisp_Object alist)
{
  Lisp_Object fr, spec, type, logfont, func, tem;

  /* check spec */
  fr = assq_no_quit (Qspec, alist);
  for (fr = XCDR (fr); CONSP (fr); fr = XCDR (fr))
    {
      tem = XCAR (fr);
      if (!CONSP (tem))
	Fsignal (Qerror, Fcons (build_string ("Invalid font request"),
				Fcons (alist, Qnil)));

      /* check spec */
      spec = XCAR (tem);
      if (!VECTORP (spec))
	Fsignal (Qerror, Fcons (build_string ("Invalid font spec"),
				Fcons (spec, Qnil)));

      /* check spec value */
      tem = XCDR (tem);
      if (!CONSP (tem))
	Fsignal (Qerror, Fcons (build_string ("Invalid font spec value"),
				Fcons (tem, Qnil)));

      type = XCAR (tem);
      tem = XCDR (tem);
      if (EQ (type, Qstrict))
	{
	  if (!CONSP (tem))
	    Fsignal (Qerror, Fcons (build_string ("Invalid font strict spec value"),
				    Fcons (tem, Qnil)));
	  logfont = XCAR (tem);
	  if (!check_lisp_object_logfont (logfont, 1))
	    {
	      Fsignal (Qerror, Fcons (build_string ("Invalid logfont"),
				      Fcons (logfont, Qnil)));
	    }
	  tem = XCDR (tem);
	  if (CONSP (tem))
	    mw32_check_font_option (XCAR (tem));
	}
      else if (EQ (type, Qfunction))
	{
	  if (!CONSP (tem))
	    Fsignal (Qerror, Fcons (build_string ("Invalid font function spec value"),
				    Fcons (tem, Qnil)));
	  func = XCAR (tem);
	  if (!FUNCTIONP (func))
	    Fsignal (Qerror, Fcons (build_string ("Invalid function"),
				    Fcons (func, Qnil)));
	  tem = XCDR (tem);
	  if (CONSP (tem))
	    mw32_check_font_option (XCAR (tem));
	}
      else
	{
	  Fsignal (Qerror, Fcons (build_string ("Invalid font spec type"),
				  Fcons (type, Qnil)));
	}
    }
}

DEFUN ("w32-add-font-internal", Fw32_add_font_internal,
       Sw32_add_font_internal, 2, 2, 0,
       "Add a font. NAME is a name of the font.\n\
ALIST is parameters to use this font.")
     (name, alist)
{
  CHECK_STRING (name, 0);
  CHECK_LIST (alist, 1);

  if (mw32_get_font_request (XSTRING (name)->data) >= 0)
    error ("Already registerd %s font.", XSTRING (name)->data);

  mw32_check_font_request_alist (alist);

  mw32_internal_add_font (XSTRING (name)->data, alist);

  return Qnil;
}

DEFUN ("w32-change-font-attribute-internal",
       Fw32_change_font_attribute_internal,
       Sw32_change_font_attribute_internal, 2, 2, 0,
       "Change the font attribute.")
     (name, alist)
     Lisp_Object name, alist;
{
  int idx;
  MW32FontRequest *pfr;

  CHECK_STRING (name, 0);
  CHECK_LIST (alist, 1);

  idx = mw32_get_font_request (XSTRING (name)->data);
  if (idx < 0)
    error ("Font request %s is not registered.", XSTRING (name)->data);

  mw32_check_font_request_alist (alist);

  AREF (Vmw32_font_request_table, idx) = alist;
  pfr = &mw32_fr_table[idx];
  pfr->loaded_LF_num = 0;
  pfr->idx = idx;
  pfr->name = xstrdup (XSTRING (name)->data);

  return Qnil;
}

DEFUN ("w32-font-list", Fw32_font_list, Sw32_font_list, 0, 0, 0,
       "Return a list of all w32 font. ")
     ()
{
  int i;
  Lisp_Object val = Qnil;

  for (i = 0;i < mw32_fr_num;i++)
    val = Fcons (build_string (mw32_fr_table[i].name), val);

  return val;
}

DEFUN ("w32-get-font-info", Fw32_get_font_info,
       Sw32_get_font_info, 1, 1, 0,
       "Get the font information you specified.\n\
NAME is a name of the font.")
     (name)
{
  int idx;
  Lisp_Object ret = Qnil;

  CHECK_STRING (name, 0);

  idx = mw32_get_font_request (XSTRING (name)->data);
  if (idx < 0)
    error ("Font request %s is not registered.", XSTRING (name)->data);

  return AREF (Vmw32_font_request_table, idx);
}

DEFUN ("w32-get-logfont-info", Fw32_get_logfont_info, 
       Sw32_get_logfont_info, 1, 2, 0,
       "Get the metric of the logfont you specified.\n\
Return an a-list that consists of width, height, base, overhang,\n\
charset-num, and max-width elements.")
     (logfont, frame)
     Lisp_Object logfont, frame;
{
  LOGFONT lf;
  HFONT hf;
  Lisp_Object ret = Qnil;

  Fw32_check_logfont (logfont);

  if (EQ (XCONS (logfont)->car, Qbdf_font))
    {
      return mw32_get_bdf_font_info (XCONS (XCONS (logfont)->cdr)->car);
    }
  else
    {

      lf = lisp_object_to_logfont (logfont);
      
      if (hf = CreateFontIndirect (&lf))
	{
	  HDC hdc;
	  HANDLE oldobj;
	  TEXTMETRIC tm;
	  HWND hwnd;
	  int flag;

	  if (FRAMEP (frame))
	    hwnd = FRAME_MW32_WINDOW (XFRAME (frame));
	  else 
	    hwnd = GetDesktopWindow ();

	  hdc = GetDC (hwnd);
	  oldobj = SelectObject (hdc, hf);
	  flag = GetTextMetrics (hdc, &tm);
	  SelectObject (hdc, oldobj);
	  DeleteObject (hf);
	  ReleaseDC (hwnd, hdc);
	  if (!flag)
	    error ("Fail to get metrics!!, FaceName:%s",
		   lf.lfFaceName);

	  store_in_alist (&ret, Qwidth,
			 make_number ((int) tm.tmAveCharWidth));
	  store_in_alist (&ret, Qheight,
			 make_number ((int) tm.tmHeight));
	  store_in_alist (&ret, Qbase,
			 make_number ((int) tm.tmAscent));
	  store_in_alist (&ret, intern ("overhang"),
			 make_number ((int) tm.tmOverhang));
	  store_in_alist (&ret, intern ("charset-num"),
			 make_number ((int) tm.tmCharSet));
	  store_in_alist (&ret, intern ("max-width"),
			 make_number ((int) tm.tmMaxCharWidth));
	  return ret;
	}
    }
  error ("unable to create font!");

  return Qnil;
}

int CALLBACK
mw32_enumfontfamilyproc (ENUMLOGFONT *lpelf,
			 NEWTEXTMETRIC *lpntm,
			 int fonttype,
			 Lisp_Object *result)
{
  Lisp_Object rlist[4];
  Lisp_Object rf;

  int bufsize, size;
  int fullname_size, style_size;
  unsigned char *buf;
  struct coding_system coding;

  fullname_size = strlen (lpelf->elfFullName);
  style_size = strlen (lpelf->elfStyle);

  setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system), &coding);
  bufsize = decoding_buffer_size (&coding, max (fullname_size, style_size));
  buf = (unsigned char *) alloca (bufsize);

  /* This procedure may cause GC, so we should make it first. */
  rlist[3] = logfont_to_lisp_object (&(lpelf->elfLogFont));

  decode_coding (&coding, lpelf->elfFullName, buf,
		 fullname_size, bufsize);
  size = coding.produced;
  rlist[0] = make_string (buf, size);

  decode_coding (&coding, lpelf->elfStyle, buf, 
		 style_size, bufsize);
  size = coding.produced;
  rlist[1] = make_string (buf, size);

  if (fonttype & RASTER_FONTTYPE)
    rlist[2] = Qraster;
  else
    rlist[2] = Qscalable;

  /* To protect data from GC, I connect these expressions. */
  /* result is protected from GC, so we can refer it by pointer. */
  *result = Fcons (Flist (XFASTINT (4), rlist), *result);

  return 1;

}

DEFUN ("w32-enum-logfont", Fw32_enum_logfont,
       Sw32_enum_logfont, 0, 2, 0,
       "Enumerate logfonts in the system.\n\
FAMILY is a family name of enumerated logfonts.\n\
If this argument is nil, enumerate all families.\n\
If DEVICE is nil, the common device context is\n\
used for enumeration.\n\
This function returns results, as follow.\n\
((FULLNAME STYLENAME FONTTYPE LOGFONT) \n\
 (FULLNAME STYLENAME FONTTYPE LOGFONT) \n\
 ....)")
(family, device)
     Lisp_Object family, device;
{
  struct gcpro gcpro1;
  int device_type;
  HWND hwnd;
  HDC hdc;
  Lisp_Object result = Qnil;

  if (NILP (device)) device_type = 0;
  else if (EQ (device, intern ("printer")))
    error ("Not yet supported device: %s", device);
  else if (EQ (device, intern ("display"))) device_type = 0;
  else
    error ("Unknown device: %s", device);

  if (device_type == 0)
    {
      hwnd = GetDesktopWindow ();
      hdc = GetDC (hwnd);
    }
  
  GCPRO1 (result);
  if (NILP (family))
    EnumFontFamilies (hdc, NULL,
		      (FONTENUMPROC) mw32_enumfontfamilyproc,
		      (LPARAM) &result);
  else
    {
      struct coding_system coding;
      int size, bufsize;
      unsigned char *buf;

      CHECK_STRING (family, 0);
      setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system),
			   &coding);
      bufsize = encoding_buffer_size (&coding, LISPY_STRING_BYTES (family) + 1);
      buf = alloca (bufsize);
      encode_coding (&coding, XSTRING (family)->data, buf,
		     LISPY_STRING_BYTES (family), bufsize);
      size = coding.produced;
      buf[size] = '\0';
      EnumFontFamilies (hdc, buf,
			(FONTENUMPROC) mw32_enumfontfamilyproc,
			(LPARAM) &result);
    }

  UNGCPRO;
  if (device_type == 0)
    {
      ReleaseDC (hwnd, hdc);
    }

  return result;
}

DEFUN ("w32-query-get-logfont", Fw32_query_get_logfont,
       Sw32_query_get_logfont, 0, 0, 0,
       "Return the font that you selected with Font Select Dialog Box.")
     ()
{
  CHOOSEFONT cf;
  LOGFONT lf;
  unsigned char buf[100];
  int ret;

  cf.lStructSize = sizeof (CHOOSEFONT);
  cf.hwndOwner = FRAME_MW32_WINDOW (SELECTED_FRAME ());
  cf.hDC = (HDC) NULL;
  cf.lpLogFont = &lf;
  cf.Flags = CF_SCREENFONTS | CF_FIXEDPITCHONLY | 
    CF_FORCEFONTEXIST | CF_EFFECTS;
  cf.rgbColors = RGB (0,0,0);
  cf.lpfnHook = (LPCFHOOKPROC) NULL;
  cf.lpTemplateName = (LPSTR) NULL;
  cf.hInstance = (HINSTANCE) NULL;
  cf.lpszStyle = (LPSTR) NULL;
  cf.nFontType = SCREEN_FONTTYPE;

  ShowCursor (TRUE);
  ret = ChooseFont (&cf);
  ShowCursor (FALSE);


  if (ret)
    return logfont_to_lisp_object (&lf);
  else
    return Qnil;
}

DEFUN ("w32-score-logfont-candidates", Fw32_score_logfont_candidates,
       Sw32_score_logfont_candidates, 2, 2, 0,
       "Score logfont candidates from requests.\n\
The first argument REQUESTS is a list that consists of either\n\
  ((KEY . VALUE) . POINT)\n\
or\n\
  (KEY . VALUE).\n\
In the latter form, its POINT is regarded as 1.\n\
\n\
The second argument CANDIDATES is a list that consists of\n\
  (FULLNAME STYLENAME FONTTYPE LOGFONT)\n\
type cell, and its format is equivalent to the result format of\n\
(w32-enum-logfont).\n\
\n\
On each element of CANDIDATES and each element of REQUESTS,\n\
this function add POINT to a score of the element of CANDIDATES\n\
if it conforms the elements of REQUEST.\n\
\n\
The meaningful KEY is one of the followings.\n\
   family (The VALUE is a string.)\n\
   width  (The VALUE is a number.)\n\
   height (The VALUE is a number.)\n\
   base   (The VALUE is a number.)\n\
   width  (The VALUE is a number.)\n\
   weight (The VALUE is a number.)\n\
   italic (If The VALUE is non-nil, italic fonts conform, vice-virsa.)\n\
   fixed  (If The VALUE is non-nil, fixed-pitch fonts conform, vice-virsa.)\n\
\n\
This function returns a list that consists of the score of the element of CANDIDATES,\n\
and the order is the same with CANDIDATES.")
     (requests, candidates)
     Lisp_Object requests, candidates;
{
  logfont_candidates *plc, *plc2;
  Lisp_Object curcand, tem, tem2, curreq;
  Lisp_Object result = Qnil;
  HFONT hf;
  int *psc;
  int *pscore;
  int num;
  int i;

  num = Flength (candidates);
  plc = (logfont_candidates*) xmalloc (sizeof (logfont_candidates) * num);
  pscore = (int *) xmalloc (sizeof (int) * num);
  memset (pscore, 0, sizeof (int) * num);

  plc2 = plc;
  for (i = 0; i < num; i++)
    {
      CHECK_LIST (candidates, 0);
      curcand = XCONS (candidates)->car;
      CHECK_LIST (curcand, 0);
      plc2->family = XCONS (curcand)->car;
      CHECK_STRING (plc2->family, 0);
      tem = XCONS (curcand)->cdr;
      CHECK_LIST (tem, 1);
      tem = XCONS (tem)->cdr;
      CHECK_LIST (tem, 1);
      tem2 = XCONS (tem)->car;
      if (EQ (tem2, Qscalable))
	plc2->type = 1;
      else
	plc2->type = 0;
      tem = XCONS (tem)->cdr;
      CHECK_LIST (tem, 0);
      tem2 = XCONS (tem)->car;
      Fw32_check_logfont (tem2);
      plc2->lf = lisp_object_to_logfont (tem2);

      plc2->width = plc2->height
	= plc2->base = plc2->overhang = plc2->charset = -1;
      hf = CreateFontIndirect (&plc2->lf);
      if (hf)
	{
	  HDC hdc;
	  HANDLE oldobj;
	  TEXTMETRIC tm;
	  HWND hwnd;
	  int flag;

	  hwnd = GetDesktopWindow ();
	  hdc = GetDC (hwnd);
	  oldobj = SelectObject (hdc, hf);
	  flag = GetTextMetrics (hdc, &tm);
	  SelectObject (hdc, oldobj);
	  DeleteObject (hf);
	  ReleaseDC (hwnd, hdc);
	  if (flag)
	    {
	      plc2->width = tm.tmAveCharWidth;
	      plc2->height = tm.tmHeight;
	      plc2->base = tm.tmAscent;
	      plc2->overhang = tm.tmOverhang;
	      plc2->charset = tm.tmCharSet;
	    }
	}
      plc2++;
      candidates = XCONS (candidates)->cdr;
    }

  for (; !NILP (requests); requests = XCONS (requests)->cdr)
    {
      CHECK_LIST (requests, 0);
      curreq = XCONS (requests)->car;
      plc2 = plc;
      psc = pscore;
      for (i = 0; i < num; i++)
	{
	  *psc += w32_score_logfont (plc2, curreq);
	  plc2++;
	  psc++;
	}
    }

  for (i = (num - 1); i >= 0; i--)
    result = Fcons (make_number (pscore[i]), result);
  
  return result;
}

/* syms */
void
reinit_syms_of_mw32font ()
{
  /* Setting callback functions for fontset handler.  */
  get_font_info_func = mw32_get_font_info;
#if 0
  list_fonts_func = mw32_list_fonts;
#endif
  load_font_func = mw32_load_font;
  query_font_func = mw32_query_font;
  set_frame_fontset_func = mw32_set_font;
  find_ccl_program_func = mw32_find_ccl_program;
  check_window_system_func = check_mw32;

  {
    int i;
    for (i = 0; i < (sizeof (face_unspecified_attrs) / sizeof (Lisp_Object)); i++)
      face_unspecified_attrs[i] = Qunspecified;
  }

  mw32_init_cached_bitmap_slots ();

  mw32_add_lf_loader (mw32_spec_base_font_loader);
  mw32_add_lf_loader (mw32_default_logical_font_loader);
}

syms_of_mw32font ()
{
  Qany = intern ("any");
  staticpro (&Qany);
  Qspec = intern ("spec");
  staticpro (&Qspec);
  Qstrict = intern ("strict");
  staticpro (&Qstrict);
  Qw32_logfont = intern ("w32-logfont");
  staticpro (&Qw32_logfont);
  Qbdf_font = intern ("bdf-font");
  staticpro (&Qbdf_font);

  Qweight = intern ("weight");
  staticpro (&Qweight);
  Qbase = intern ("base");
  staticpro (&Qbase);
  Qoverhang = intern ("overhang");
  staticpro (&Qoverhang);
  Qfixed = intern ("fixed");
  staticpro (&Qfixed);
  Qitalic = intern ("italic");
  staticpro (&Qitalic);
  Qfamily = intern ("family");
  staticpro (&Qfamily);
  Qraster = intern ("raster");
  staticpro (&Qraster);
  Qscalable = intern ("scalable");
  staticpro (&Qscalable);

  Qencoding = intern ("encoding");
  staticpro (&Qencoding);
  Qfont_unit_byte = intern ("font-unit-byte");
  staticpro (&Qfont_unit_byte);

  Q1_byte_set_msb = intern ("1-byte-set-msb");
  staticpro (&Q1_byte_set_msb);
  Q2_byte_set_msb = intern ("2-byte-set-msb");
  staticpro (&Q2_byte_set_msb);
  Qunicode = intern ("unicode");
  staticpro (&Qunicode);
  Qshift_jis = intern ("shift_jis");
  staticpro (&Qshift_jis);

  Qrelative_compose = intern ("relative-compose");
  staticpro (&Qrelative_compose);
  Qdefault_ascent = intern ("default-ascent");
  staticpro (&Qdefault_ascent);
  Qspacing = intern ("spacing");
  staticpro (&Qspacing);
  Qcentering = intern ("centering");
  staticpro (&Qcentering);

  DEFVAR_LISP ("w32-system-coding-system",
	       &Vw32_system_coding_system,
	       "coding system in windows system");
  Vw32_system_coding_system = Qnil;

  DEFVAR_LISP ("w32-default-font-request-alist",
	       &Vw32_default_font_request_alist,
	       "* Default parameter alist to request fonts.");
  Vw32_default_font_request_alist = Qnil;

  DEFVAR_LISP ("mw32-charset-windows-font-info-alist",
	       &Vmw32_charset_windows_font_info_alist,
	       "It holds correspondences between Mule's charset and Windows logfont info.\n\
Each slot consists of <charset>, <charset-num>, <encoding>,\n\
and <option-alist>.");
  Vmw32_charset_windows_font_info_alist = Qunbound;

  Vmw32_font_request_table = Fmake_vector (make_number (0), Qnil);
  staticpro (&Vmw32_font_request_table);

  defsubr (&Sw32_check_logfont);
  defsubr (&Sw32_add_font_internal);
  defsubr (&Sw32_change_font_attribute_internal);
  defsubr (&Sw32_font_list);
  defsubr (&Sw32_get_font_info);
  defsubr (&Sw32_get_logfont_info);
  defsubr (&Sw32_query_get_logfont);
  defsubr (&Sw32_enum_logfont);
  defsubr (&Sw32_score_logfont_candidates);


  /*
    obsolte functions:
    defsubr (&Sw32_change_font_logfont);
    defsubr (&Sw32_get_font_logfont);
  */

  /*
    defsubr (&Sw32_delete_fontset);
    defsubr (&Sw32_delete_font);
    defsubr (&Sw32_get_font_info);
  */

  reinit_syms_of_mw32font ();
}
