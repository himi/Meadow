#ifndef EMACS_W32_H
#define EMACS_W32_H

/* Support routines for the NT version of Emacs.
   Copyright (C) 1994 Free Software Foundation, Inc.

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


/* File descriptor set emulation.  */

/* MSVC runtime library has limit of 64 descriptors by default */
#define FD_SETSIZE  64
typedef struct {
  unsigned int bits[FD_SETSIZE / 32];
} fd_set;

/* standard access macros */
#define FD_SET(n, p) \
  do { \
    if ((n) < FD_SETSIZE) { \
      (p)->bits[(n)/32] |= (1 << (n)%32); \
    } \
  } while (0)
#define FD_CLR(n, p) \
  do { \
    if ((n) < FD_SETSIZE) { \
      (p)->bits[(n)/32] &= ~(1 << (n)%32); \
    } \
  } while (0)
#define FD_ISSET(n, p) ((n) < FD_SETSIZE ? ((p)->bits[(n)/32] & (1 << (n)%32)) : 0)
#define FD_ZERO(p) memset((p), 0, sizeof(fd_set))

#define SELECT_TYPE fd_set

/* ------------------------------------------------------------------------- */

/* child_process.status values */
enum {
  STATUS_READ_ERROR = -1,
  STATUS_READ_READY,
  STATUS_READ_IN_PROGRESS,
  STATUS_READ_FAILED,
  STATUS_READ_SUCCEEDED,
  STATUS_READ_ACKNOWLEDGED
};

/* This structure is used for both pipes and sockets; for
   a socket, the process handle in pi is NULL. */
typedef struct _child_process
{
  int                   fd;
  int                   pid;
  HANDLE                char_avail;
  HANDLE                char_consumed;
  HANDLE                thrd;
  HWND                  hwnd;
  PROCESS_INFORMATION   procinfo;
  volatile int          status;
  char                  chr;
} child_process;

#define MAXDESC FD_SETSIZE
#define MAX_CHILDREN  MAXDESC/2
#define CHILD_ACTIVE(cp) ((cp)->char_avail != NULL)

/* parallel array of private info on file handles */
typedef struct
{
  unsigned         flags;
  HANDLE           hnd;
  child_process *  cp;
} filedesc;

extern filedesc fd_info [ MAXDESC ];

/* fd_info flag definitions */
#define FILE_READ               0x0001
#define FILE_WRITE              0x0002
#define FILE_BINARY             0x0010
#define FILE_LAST_CR            0x0020
#define FILE_AT_EOF             0x0040
#define FILE_SEND_SIGCHLD       0x0080
#define FILE_PIPE               0x0100
#define FILE_SOCKET             0x0200

extern child_process * new_child (void);
extern void delete_child (child_process *cp);

/* ------------------------------------------------------------------------- */

/* Equivalent of strerror for W32 error codes.  */
extern char * w32_strerror (int error_no);

/* Get long (aka "true") form of file name, if it exists.  */
extern BOOL w32_get_long_filename (char * name, char * buf, int size);

/* Prepare our standard handles for proper inheritance by child processes.  */
extern void prepare_standard_handles (int in, int out, 
				      int err, HANDLE handles[4]);

/* Reset our standard handles to their original state.  */
extern void reset_standard_handles (int in, int out, 
				    int err, HANDLE handles[4]);

/* Return the string resource associated with KEY of type TYPE.  */
extern LPBYTE w32_get_resource (char * key, LPDWORD type);

/* W32 clipboard format for MULE text. */
extern UINT mw32_mule_clipboard_format;

/* The old version of Meadow registers "Mule_TEXT" as emacs-mule
   clipboard format, which is only emacs-mule text.  "Mule_TEXT_V2" is
   a simple format to store size of emacs-mule text.  Its header is an
   interger that specifies size of emacs-mule text, and the rest is
   the text itself.  */
#define MULE_CLIPBOARD_FORMAT "Mule_TEXT_V2"

extern void init_ntproc ();
extern void term_ntproc ();

#ifdef MEADOW
/* mw32mem.c functions.  */

enum MW32_OSTYPE {
  OS_WIN95 = 1,
  OS_NT = 2
};

extern int os_subtype;
extern int get_processor_type ();
extern int get_w32_major_version ();
extern int get_w32_minor_version ();

typedef struct file_data {
    char          *name;
    unsigned long  size;
    HANDLE         file;
    HANDLE         file_mapping;
    unsigned char *file_base;
} file_data;

extern int open_input_file (file_data *p_file, char *name);
extern int open_output_file (file_data *p_file, char *name, unsigned long size);
extern void close_file_data (file_data *p_file);

#define OFFSET_TO_RVA(var,section) \
	  (section->VirtualAddress + ((DWORD)(var) - section->PointerToRawData))

#define RVA_TO_OFFSET(var,section) \
	  (section->PointerToRawData + ((DWORD)(var) - section->VirtualAddress))

#define RVA_TO_PTR(var,section,filedata) \
	  ((void *)(RVA_TO_OFFSET(var,section) + (filedata).file_base))

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER * find_section (char * name, IMAGE_NT_HEADERS * nt_header);

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER * rva_to_section32 (DWORD rva, IMAGE_NT_HEADERS32 * nt_header);

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER * rva_to_section64 (DWORD rva, IMAGE_NT_HEADERS64 * nt_header);

#endif /* MEADOW */

#endif /* EMACS_W32_H */
