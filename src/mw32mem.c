/* MW32 memory manager
   Copyright (C) 2002 Free Software Foundation, Inc.
   Copyright (C) 2002 MIYASHITA, Hisashi.

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

#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include "config.h"
#include "lisp.h"
#include "w32.h"

int os_subtype;

HINSTANCE hinst = NULL;
HINSTANCE hprevinst = NULL;
LPSTR lpCmdLine = "";
int nCmdShow = 0;

static int w32_major_version;
static int w32_minor_version;
static int page_size, page_mask;
static unsigned char* lisp_heap_start;
static unsigned char* lisp_heap_current;
static unsigned char* lisp_heap_alloced;
static unsigned char* lisp_heap_end;
static int allocation_unit;
static int processor_type;

/* These are defined to get Emacs to compile, but are not used.  */
int edata;
int etext;
void _start ()
{
}

int get_processor_type ()
{
  return processor_type;
}

int get_w32_major_version ()
{
  return w32_major_version;
}

int get_w32_minor_version ()
{
  return w32_minor_version;
}

static void
mw32_initialize_system_info ()
{
  OSVERSIONINFO sysver;
  SYSTEM_INFO sysinfo;

  sysver.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
  GetVersionEx (&sysver);
  w32_major_version = sysver.dwMajorVersion;
  w32_minor_version = sysver.dwMinorVersion;
  if (sysver.dwPlatformId == VER_PLATFORM_WIN32_NT)
    os_subtype = OS_NT;
  else
    os_subtype = OS_WIN95;

  GetSystemInfo (&sysinfo);

  page_size = sysinfo.dwPageSize;
  page_mask = page_size - 1;
  allocation_unit = sysinfo.dwAllocationGranularity;
  processor_type = sysinfo.dwProcessorType;
}


/* startup entry */

static void mw32_initialize_lisp_heap ();
static void mw32_initialize_buffer_heap ();

void
mw32_start (void)
{
#ifndef __BORLANDC__ /* 97/01/13 by Y.Matsushima */
  extern void mainCRTStartup (void);
#endif
  /* w32console.c */
  extern BOOL ctrl_c_handler (unsigned long type);

  mw32_initialize_system_info ();
  mw32_initialize_lisp_heap ();
  mw32_initialize_buffer_heap ();
  /* mw32_initialize_system_heap (); */

  /* The default behavior is to treat files as binary and patch up
     text files appropriately, in accordance with the MSDOS code.  */
  _fmode = O_BINARY;

#if defined (__MINGW32__)
#if __MINGW32_MAJOR_VERSION >= 3 || __MINGW32_MAJOR_VERSION == 2 && __MINGW32_MINOR_VERSION >= 3
 {
   extern int _CRT_fmode;
   _CRT_fmode = _O_BINARY;
 }
#endif /* __MINGW32_MAJOR_VERSION >= 3 || __MINGW32_MAJOR_VERSION == 2 && __MINGW32_MINOR_VERSION >= 3 */
#endif /* __MINGW32__ */

  /* This prevents ctrl-c's in shells running while we're suspended from
     having us exit.  */
  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);

  /* Invoke the NT CRT startup routine now that our housecleaning
     is finished.  */
  /* determine WinMain args like crt0.c does */
  hinst = GetModuleHandle (NULL);
  lpCmdLine = GetCommandLine ();
  nCmdShow = SW_SHOWDEFAULT;
  
#ifndef __BORLANDC__ /* 97/01/13 by Y.Matsushima */
  mainCRTStartup ();
#endif
}

/***********************************************************************
			gmalloc interface (lisp heap).
 ***********************************************************************/

static void
mw32_initialize_lisp_heap ()
{
  /* The base address for our GNU malloc heap is chosen in conjuction
     with the link settings for temacs.exe which control the stack size,
     the initial default process heap size and the executable image base
     address.  The link settings and the malloc heap base below must all
     correspond; the relationship between these values depends on how NT
     and Windows 95 arrange the virtual address space for a process (and on
     the size of the code and data segments in temacs.exe).

     The most important thing is to make base address for the executable
     image high enough to leave enough room between it and the 4MB floor
     of the process address space on Windows 95 for the primary thread stack,
     the process default heap, and other assorted odds and ends
     (eg. environment strings, private system dll memory etc) that are
     allocated before temacs has a chance to grab its malloc arena.  The
     malloc heap base can then be set several MB higher than the
     executable image base, leaving enough room for the code and data
     segments.

     Because some parts of Emacs can use rather a lot of stack space
     (for instance, the regular expression routines can potentially
     allocate several MB of stack space) we allow 8MB for the stack.

     Allowing 1MB for the default process heap, and 1MB for odds and
     ends, we can base the executable at 16MB and still have a generous
     safety margin.  At the moment, the executable has about 810KB of
     code (for x86) and about 550KB of data - on RISC platforms the code
     size could be roughly double, so if we allow 4MB for the executable
     we will have plenty of room for expansion.

     Thus we would like to set the malloc heap base to 20MB.  However,
     Windows 95 refuses to allocate the heap starting at this address, so we
     set the base to 27MB to make it happy.  Since Emacs now leaves
     28 bits available for pointers, this lets us use the remainder of
     the region below the 256MB line for our malloc arena - 229MB is
     still a pretty decent arena to play in!  */

#ifdef _M_X64
  unsigned char *base = ((unsigned char*) (1LL << 31));  /* 256MB */
  unsigned char *end  = ((unsigned char*) (1LL << 36));  /* 256MB + 8GB */
#else
  unsigned char *base = ((char*) 0) + 0x01B00000;     /*  27MB */
  unsigned char *end  = ((char*) 0) + (((EMACS_INT) 1) << VALBITS); /* 256MB */
#endif
  void *ptr = NULL;

  while (!ptr && (base < end))
    {
      ptr = VirtualAlloc (base, end - base, MEM_RESERVE, PAGE_NOACCESS);
      base += 0x00100000;  /* 1MB increment */
    }

  if (!ptr)
    {
#ifdef _M_X64
      fprintf (stderr, "Error: could not allocate Lisp heap (%llX - %llX)\n", base, end);
#else
      fprintf (stderr, "Error: could not allocate Lisp heap (%lX - %lX)\n", base, end);
#endif
      exit (1);
    }

  lisp_heap_start = base;
  lisp_heap_current = base;
  lisp_heap_alloced = base;
  lisp_heap_end = end;
}

PDUMP_PINT
get_reserved_heap_size ()
{
  return lisp_heap_end - lisp_heap_start;
}

/* The start of the data segment.  */
unsigned char *
get_data_start (void)
{
  return lisp_heap_start;
}

/* The end of the data segment.  */
unsigned char *
get_data_end (void)
{
  return lisp_heap_end;
}

int
getpagesize (void)
{
  return page_size;
}

void*
sbrk (long increment)
{
  void *result;
  PDUMP_PINT size = (PDUMP_PINT) increment;
  
  result = lisp_heap_current;

#if 0
  fprintf(stderr, "sbrk %ld: %llx %llx %llx\n", increment, lisp_heap_start, lisp_heap_current, lisp_heap_alloced);
#endif

  /* If size is negative, shrink the heap by decommitting pages.  */
  if (size < 0) 
    {
      PDUMP_PINT dealloc_size;
      unsigned char *new_current;

      size = -size;

      /* Sanity checks.  */
      if ((lisp_heap_current - size) < lisp_heap_start)
	return NULL;

      dealloc_size = size + (lisp_heap_alloced - lisp_heap_current);
      /* round down dealloc_size to page boundary.  */
      dealloc_size = (dealloc_size & ~page_mask);
      new_current = lisp_heap_alloced - dealloc_size;

      if (dealloc_size > 0) 
	{
	  /* Decommit size bytes from the end of the heap.  */
	  if (!VirtualFree (new_current, dealloc_size, MEM_DECOMMIT))
	    return NULL;
 	}

      lisp_heap_alloced = new_current;
      lisp_heap_current -= size;
    } 
  /* If size is positive, grow the heap by committing reserved pages.  */
  else if (size > 0) 
    {
      PDUMP_PINT req_size;
      PDUMP_PINT alloced_size;
      /* Sanity checks.  */
      if ((lisp_heap_current + size) >= lisp_heap_end)
	return NULL;

      /* If already committed, record it and return. */
      req_size = size - (lisp_heap_alloced - lisp_heap_current);
      if (req_size <= 0)
	{
	  lisp_heap_current += size;
	  return result;
	}

      /* Commit more of our heap. */
      if (VirtualAlloc (lisp_heap_alloced, req_size, MEM_COMMIT,
			PAGE_READWRITE) == NULL)
	return NULL;
      lisp_heap_current += size;
      /* round up size to caliculate reallly alloced size.  */
      alloced_size = ((size + page_mask) & ~page_mask);
      lisp_heap_alloced += alloced_size;
    }
  
  return result;
}

/***********************************************************************
			heap manager for buffer
 ***********************************************************************/

static HANDLE buffer_heap;

static void
mw32_initialize_buffer_heap ()
{
  buffer_heap = HeapCreate (HEAP_NO_SERIALIZE, 1024 * 1024, 0);
  if (!buffer_heap)
    {
      fprintf (stderr, "Error: could not create buffer heap\n");
      exit (1);
    }
}

void*
mw32_allocate_buffer_heap (int size)
{
  return  HeapAlloc (buffer_heap, HEAP_NO_SERIALIZE, size);
}

void*
mw32_reallocate_buffer_heap (void* ptr, int size)
{
  return  HeapReAlloc (buffer_heap, HEAP_NO_SERIALIZE, ptr, size);
}

void
mw32_free_buffer_heap (void* ptr)
{
  HeapFree (buffer_heap, HEAP_NO_SERIALIZE, ptr);
}

/***********************************************************************
			Memory Mapped File
 ***********************************************************************/

int
open_input_file (file_data *p_file, char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_READONLY, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0) 
    return FALSE;

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

int
open_output_file (file_data *p_file, char *filename, unsigned long size)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;

  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  file_mapping = CreateFileMapping (file, NULL, PAGE_READWRITE, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;
  
  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == 0) 
    return FALSE;
  
  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

/* Close the system structures associated with the given file.  */
void
close_file_data (file_data *p_file)
{
    UnmapViewOfFile (p_file->file_base);
    CloseHandle (p_file->file_mapping);
    CloseHandle (p_file->file);
}

/***********************************************************************
			Portable Dumper I/F
 ***********************************************************************/

/* TODO */

/***********************************************************************
			PE Image utility.
 ***********************************************************************/

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER *
find_section (char * name, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (strcmp (section->Name, name) == 0)
	return section;
      section++;
    }
  return NULL;
}

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER *
rva_to_section32 (DWORD rva, IMAGE_NT_HEADERS32 * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + section->SizeOfRawData)
	return section;
      section++;
    }
  return NULL;
}

IMAGE_SECTION_HEADER *
rva_to_section64 (DWORD rva, IMAGE_NT_HEADERS64 * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + section->SizeOfRawData)
	return section;
      section++;
    }
  return NULL;
}
