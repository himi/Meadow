/* MW32 font specific staff header file.
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

#ifndef __MW32_FONT__
#define __MW32_FONT__
#include <windows.h>

#ifndef SHIFTJIS_CHARSET
#define SHIFTJIS_CHARSET 128
#endif

#define LOGFONT_FACENAME_MAX 32

typedef struct ccl_id_type
{
  char *data; /* NOT ASCIZ */
  int size;
} ccl_id_type;
#define ENCODING_METHOD_P(obj) (!NILP(Fget((obj), Qccl_program)))

typedef struct MW32CharMetric
{
  int ascent;
  int descent;
  int width;
  int overhang;
  int roverhang;
} MW32CharMetric;

#define MW32_CHARMETRIC_VALID_P(cm) ((cm).width > 0)
#define MW32_CHARMETRIC_INVALIDATE(cm) ((cm).width = -1)

typedef int FontCp;
typedef struct MW32Encoding MW32Encoding;
typedef struct MW32FontRequest MW32FontRequest;
typedef struct MW32LogicalFont MW32LogicalFont;
struct face;

typedef MW32LogicalFont* (*LOADLFPROC)(MW32FontRequest*, struct frame *,
				       struct face*, int);

typedef unsigned int (*HASHPROC)(MW32LogicalFont*);
typedef void (*OUTPUTPROC)(MW32LogicalFont*, HDC, unsigned char*,
			   int, int, int, int*, RECT*, int);
typedef MW32CharMetric (*METRICPROC)(MW32LogicalFont*, HDC, FontCp);
typedef int (*LAYOUTPROC)(MW32LogicalFont*, HDC, unsigned char*,
			  int, int, int*, int*, int*);
typedef void (*FREEPROC)(MW32LogicalFont*);
typedef int (*EQUALPROC)(MW32LogicalFont*, MW32LogicalFont*);

struct MW32FontRequest
{
  int idx;
  unsigned char *name;

  int loaded_LF_num;
  MW32LogicalFont **pLFs;
  int num_pLFs;
};

typedef enum
  {
    MW32_FID_WINDOWS_FONT,
    MW32_FID_BDF_FONT
  } font_id;

typedef enum
  {
    ENCODING_DIMENSION = 0,
    ENCODING_BYTE1MSB1 = 1,
    ENCODING_BYTE2MSB11 = 2,
    ENCODING_SHIFTJIS = 4,
    ENCODING_UNICODE = 16,
    ENCODING_EXTERNAL = 32
  } MW32EncodingType;

struct MW32Encoding
{
  MW32EncodingType type;
  int font_unit_byte;
  union 
  {
    ccl_id_type ccl_method;
  } e;
};

struct MW32LogicalFont
{
  MW32FontRequest *pfr;
  int idx;

  /* font informations */
  int width;
  int ascent;
  int descent;
  int overhang;
  int relative_compose;
  int default_ascent;
  int fixed_pitch;
  int character_spacing;
  int centering;
  struct font_info *fontip;

  MW32Encoding encoding;

  /* LF methods */
  HASHPROC hash;
  OUTPUTPROC textout;
  METRICPROC glyph_metric;
  LAYOUTPROC set_layout;
  FREEPROC free;
  EQUALPROC equal;

  /* for hash */
  MW32LogicalFont *pnext;

  /* Physical font layer related data */
  font_id pphys_type;
  void *pphys;
};

#define FONT_WIDTH(font) ((font)->width)
#define FONT_HEIGHT(font) ((font)->ascent + (font)->descent)
#define FONT_BASE(font) ((font)->ascent)
#define FONT_NAME(font) ((font)->pfr->name)

#define MW32_LF_EMPTY_SLOT_P(font) (!((font)->pfr))

#define MW32_FONT_INFO_FROM_FONT(font) ((font)->fontip)
#define MW32_FONT_FROM_FONT_INFO(font_info) ((MW32LogicalFont*)(font_info)->font)

#define MW32_INVOKE_HASHPROC(font) ((*(font)->hash)(font))
#define MW32_INVOKE_OUTPUTPROC(font, hdc, text, x, ybase, bytes, xoffs, prect, mode) \
  ((*(font)->textout)(font, hdc, text, x, ybase, bytes, xoffs, prect, mode))
#define MW32_INVOKE_METRICPROC(font, hdc, cp) \
  ((*(font)->glyph_metric)(font, hdc, cp))
#define MW32_INVOKE_LAYOUTPROC(font, hdc, text, bytes, max, xoffs, ploverhang, proverhang) \
  ((*(font)->set_layout)(font, hdc, text, bytes, max, xoffs, ploverhang, proverhang))
#define MW32_INVOKE_FREEPROC(font) \
  ((*(font)->free)(font))
#define MW32_INVOKE_EQUALPROC(font1, font2) ((*(font1)->equal)(font1, font2))

#define MAKEFONTCP(b1, b2) (((b1) << 8) | (b2))
#define SERIALIZE_FONTCP(pstr, fcp)		\
do{						\
  if ((fcp) > 0xFF) *(pstr)++ = ((fcp) >> 8); 	\
  *(pstr)++ = ((fcp) & 0xFF);			\
} while (0)
#define MAXBYTES1FCP 2

typedef struct logfont_candidates
{
  int type;
  int width;
  int height;
  int base;
  int overhang;
  int charset;
  Lisp_Object family;
  LOGFONT lf;
} logfont_candidates;

extern struct font_info *font_info_table;
extern Lisp_Object Qccl_program;

/* export functions. */
extern ccl_id_type mw32_get_ccl_id P_ ((Lisp_Object encode));
struct font_info* mw32_get_font_info P_ ((struct frame *, int));
extern void load_face_font P_ ((struct frame *, struct face *, int));
extern void set_font_frame_param P_ ((Lisp_Object, Lisp_Object));
extern int mw32_add_lf_loader P_ ((LOADLFPROC pllfproc));
extern MW32LogicalFont* mw32_load_lf P_ ((MW32FontRequest *pfr,
					  struct frame *f,
					  struct face *face, int c));
extern void mw32_delete_logical_font P_ ((MW32LogicalFont *plf));
extern void mw32_initialize_default_logfont P_ ((LOGFONT*));

#endif  /* __MW32_FONT__ */
