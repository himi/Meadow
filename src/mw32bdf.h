/*****************************************
  Meadow BDF font manager
         Written by H.Miyashita.
******************************************/
#define BDF_FIRST_OFFSET_TABLE 0x200
#define BDF_SECOND_OFFSET_TABLE 0x80
#define BDF_SECOND_OFFSET(x) ((x) & 0x7f)
#define BDF_FIRST_OFFSET(x) (((x) >> 8) | (((x) & 0x80) << 1))
#define BDF_CODEPOINT_MAX (BDF_FIRST_OFFSET_TABLE * BDF_SECOND_OFFSET_TABLE)
#define BDF_CODEPOINT_RANGE_COVER_P(x) (((x) >= 0) && ((x) <= BDF_CODEPOINT_MAX))

#define BDF_FONT_CACHE_SIZE 3000
#define BDF_FONT_CLEAR_SIZE 600

/* 
   GLYPH METRIC (# ... character's reference point)
   ^
 y |              (urx, ury)
   |  ^ +----------------+
 a | b| |character       | <- font bounding Box
 x | b| |                |
 i | h| | #(bbox, bboy)  |
 s |  v +----------------+
   |   (llx, lly)
   |    <---------------->
   |           bbw
   +----------------------->
   origin     x axis
 */



/* Structure of glyph information of one character.  */
typedef struct
{
  int dwidth;			/* width in pixels */
  int bbw, bbh, bbox, bboy;	/* bounding box in pixels */
}glyph_metric;

typedef struct
{
  glyph_metric metric;
  int row_byte_size;            /* size in byte occupied by one row of the bitmap */
  int bitmap_size;		/* size in byte of the following slots */
  unsigned char *bitmap;	/*  */
} glyph_struct;

typedef struct fchar *pfont_char;

typedef struct
{
  glyph_metric metric;
  pfont_char psrc;
  int row_byte_size;
  int bitmap_size;
  unsigned char *pbmp;
}cache_bitmap;

typedef struct fchar
{
  unsigned char *offset;
  cache_bitmap *pcbmp;
}font_char;

typedef struct
{
  char *filename;
  HANDLE hfile;
  HANDLE hfilemap;
  unsigned char *font;
  unsigned char *seeked;
  DWORD size;
  font_char *chtbl[BDF_FIRST_OFFSET_TABLE];
  int llx, lly, urx, ury;	/* Font bounding box */

  int yoffset;
  int relative_compose;
  int default_ascent;

  unsigned char *registry;
  unsigned char *encoding;
  unsigned char *slant;
  unsigned char *width;

}bdffont;

#define BDF_FILE_SIZE_MAX 256*1024*1024 /* 256Mb */
#define BDF_FONT_FILE(font) (((bdffont*)(font))->filename)
#define MAKELENDSHORT(c1, c2) (unsigned short)((c1) | ((c2) << 8))

extern bdffont *mw32_init_bdf_font(char *filename);
extern void mw32_free_bdf_font(bdffont *fontp);
extern Lisp_Object mw32_get_bdf_font_info(Lisp_Object fontfile);
extern int mw32_get_bdf_glyph(bdffont *fontp, int index, int size,
			      glyph_struct *glyph);
extern int mw32_BDF_TextOut(bdffont *fontp, HDC hdc, int left,
			    int top, unsigned char *text, int dim, int bytelen,
			    int fixed_pitch_size, int character_spacing);
extern void mw32_init_cached_bitmap_slots();
