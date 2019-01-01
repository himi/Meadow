/*
 * File Inspect and Broking End Resolver  -- fiber --
 *
 *       Copyright (C) 1997-2004  Shuichi Kitaguchi <kit@meadowy.org>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either versions 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with fiber, see the file COPYING.  If not, write to the Free
 * Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

/*
 * HISTORY
 * -------
 *
 *    01 Feb, 1998 : Version 1.0.0 - first release.
 *    13 Jun, 1998 : Version 1.1.0 - if registry key is not found, write init values.
 *                                   add "-s" optoin.
 *                                   WriteRegistry bug fix.
 *                                   add "-o" option.
 *    23 Jun, 1998 : Version 1.1.1 - show options with "-l".
 *                                   remove copied file when "-s" mode.
 *                                   do not delete "foo.ext" when execute "fiber foo"
 *                                   and "foo.ext" exists.
 *    27 Jun, 1998 : Version 1.1.2 - add SEE_MASK_FLAG_DDEWAIT flag in
 *                                   ShellExecuteOpenSync.
 *    29 Jun, 1998 : Version 1.1.3 - use ShellExecuteEx if not sync mode.
 *    19 Jul, 1999 : Version 1.1.4 - add "-p" and "-n" option.
 *                                   "ExecuteUnknownExt=yes" is default.
 *    07 Aug, 1999 : Version 1.2.0 - add "-b" and "-d" option.
 *    05 Oct, 1999 : Version 1.2.1 - brush up English text.
 *    12 Jan, 2004 : Version 1.2.2 - convert path name to full path name before
 *                                   executing files.
 *                                 - eliminate "file:" scheme.
 *    18 Jan, 2004 : Version 1.2.3 - DO NOT eliminate "file:" scheme.
 */


#include <windows.h>
#include <shellapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <mbstring.h>


#define FIBER_VERSION   "1.2.3"


/* constants */
#define FIBER_EXTNUM     64	/* default extension numbers */
#define FIBER_EXTLEN     8	/* extension length */
#define FIBER_OPTIONLEN  64	/* option length */
#define FIBER_IDENTLEN   32	/* ident length */
#define BUFLEN           4096	/* buffer */
#define FIBER_EXTLINELEN (FIBER_EXTLEN+FIBER_IDENTLEN+3) /* line len */

#define STATE_ERROR     0
#define STATE_INPUTFILE 1
#define STATE_ADDEXT    2

/* registry keys */
#define FIBER_SUBKEY                   "SOFTWARE\\GNU\\Fiber"
#define FIBER_SUBKEY_EXTNUM            "ExtNum"
#define FIBER_SUBKEY_EXECUTEUNKNOWNEXT "ExecuteUnknownExt"
#define FIBER_SUBKEY_EXECUTEURL        "ExecuteURL"
#define FIBER_SUBKEY_TRUSTEXT          "TrustExt"
#define FIBER_SUBKEY_OVERRIDEEXT       "OverrideExt"
#define FIBER_SUBKEY_EXTENSION         "Extension"


/* structures */
typedef struct tagEXTFILE {
  char  szExt[FIBER_EXTLEN+1];
  char  szIdent[FIBER_IDENTLEN+1];
  DWORD dwIdentLength;
  DWORD dwOffset;
} EXTFILE;


/* variables */
EXTFILE  *ef=NULL;
int      iExtNum=0;
int      iEfs=0;
BOOL     fExecuteUnknownExt;
BOOL     fExecuteURL;
BOOL     fTrustExt;
BOOL     fOverrideExt;

/* default ext values */
EXTFILE efInit[] = {
  {  "au", ".snd",4,0},
  { "wav", "WAVE",4,8},
  { "mid", "MThd",4,0},
  { "rcp", "RCM-PC98V2.0",12,0},
  { "mov", "moov",4,4},
  { "avi", "AVI",3,8},
  { "mpg", "\x00\x00\x01\xb3",4,0},
  { "jpg", "JFIF",4,134},
  { "jpg", "JFIF",4,6},
  { "bmp", "BM",2,0},
  { "gif", "GIF8",4,0},
  { "xpm", "XPM",3,3},
  { "tif", "MM",2,0},
  { "tif", "II",2,0},
  { "mki", "MAKI01",6,0},
  { "mag", "MAKI02",6,0},
  { "pic", "PIC",3,0},
  {  "pi", "Pi",2,0},
  {  "p2", "P2",2,0},
  { "png", "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a",8,0},
  { "pdf", "%PDF-",5,0},
  {  "ps", "%!",2,0},
  { "dvi", "\xf7\x02",2,0},
  { "zip", "PK",2,0},
  {  "gz", "\x1f\x8b",2,0},
  { "lzh", "SFX",3,0x48},
  { "lzh", "SFX",3,0x2e},
  { "lzh", "SFX",3,0x2a},
  { "lzh", "-lhd-",4,2},
  { "lzh", "-lh0-",4,2},
  { "lzh", "-lh1-",4,2},
  { "lzh", "-lh2-",4,2},
  { "lzh", "-lh3-",4,2},
  { "lzh", "-lh4-",4,2},
  { "lzh", "-lh5-",4,2},
  { "lzh", "-lh6-",4,2},
  { "lzh", "-lh7-",4,2},
  { "zoo", "ZOO",3,0},
  {    "",    "",0,0}
};



/* check file existing */
BOOL CheckFile( LPCSTR lpszFileName )
{
  HANDLE   hFile;

  hFile = CreateFile( lpszFileName,
		      GENERIC_READ,
		      FILE_SHARE_READ,
		      NULL,
		      OPEN_EXISTING,
		      0,
		      NULL );
  if ( hFile == INVALID_HANDLE_VALUE ){
    return ( FALSE );
  } else {
    CloseHandle( hFile );
    return ( TRUE );
  }
}


/* '/' -> '\' */
BOOL RevConvertPathSeparator( LPSTR szPathName )
{
  char *pt;

  while ( 1 ){
    pt = _mbsrchr( szPathName, '/' );
    if ( pt == NULL ) break;
    *pt = '\\';
  } /* while ( 1 ){ */

  return ( TRUE );
}


BOOL GetFileNameFromFullPath( LPCSTR szFullPath,
			      LPSTR  lpBuf,
			      DWORD  dwLength )
{
  char  *pt;
  DWORD dwLen;

  pt = _mbsrchr(szFullPath,'\\');
  if ( pt ){
    dwLen = strlen(pt) + 1;
    pt++;
  } else {
    dwLen = strlen(szFullPath) + 1;
    pt = (char*)szFullPath;
  } /* if ( pt ){ */
  if ( dwLen <= dwLength ){
    memcpy(lpBuf,pt,dwLen);
    return ( TRUE );
  } else {
    return ( FALSE );
  } /* if ( dwLen <= dwLength ){ */
}


BOOL BinaryToHexString( LPCSTR szString,
			DWORD  dwLen,
			LPSTR  lpBuf,
			DWORD  dwBufLen )
{
  DWORD i;
  char  szBuf[8];

  if ( dwBufLen < dwLen*4 ) return ( FALSE );
  lpBuf[0]=0;
  for ( i=0; i<dwLen; i++ ){
    sprintf(szBuf,"0x%02x",(BYTE)szString[i]);
    strcat(lpBuf,szBuf);
  }
  return ( TRUE );
}


/* string -> value */
BOOL CStringToValue( LPCSTR  szSrc,
		     LPDWORD lpdwDst )
{
  BOOL  ret;
  LPSTR pt;

  if ( szSrc[0] == '0' ){
    switch ( szSrc[1] ){
    case 'x':			/* hex */
      *lpdwDst = 0;
      pt = (LPSTR)&szSrc[2];
      while ( *pt ){
	if ( (*pt>='0') && (*pt<='9') ){
	  *lpdwDst = *lpdwDst*16 + *pt-'0';
	} else if ( (*pt>='A') && (*pt<='F') ){
	  *lpdwDst = *lpdwDst*16 + *pt-'A'+10;
	} else {
	  *lpdwDst = *lpdwDst*16 + *pt-'a'+10;
	}
	pt++;
      } /* while ( *pt ){ */
      ret = TRUE;
      break;
    case 'b':			/* bin */
      *lpdwDst = 0;
      pt = (LPSTR)&szSrc[2];
      while ( *pt ){
	*lpdwDst = *lpdwDst*2 + *pt-'0';
	pt++;
      }
      ret = TRUE;
      break;
    default:
      if ( (szSrc[1]>='0') && (szSrc[1]<='9') ){ /* oct */
	*lpdwDst = 0;
	pt = (LPSTR)&szSrc[1];
	while ( *pt ){
	  *lpdwDst = *lpdwDst*8 + *pt-'0';
	  pt++;
	}
	ret = TRUE;
      } else {			/* error */
	ret = FALSE;
      }
      break;
    }
  } else {
    /* dec */
    *lpdwDst = atol(szSrc);
    if ( *lpdwDst == 0 ){
      ret = FALSE;
    } else {
      ret = TRUE;
    }
  } /* if ( szSrc[0] == '0' ){ */
  return ( ret );
}

/* 0x**0x**... */
DWORD HexStringToBinary( LPCSTR szSrc,
			 LPSTR  szDst,
			 DWORD  dwLen )
{
  char  szBuf[FIBER_IDENTLEN+1];
  char  *spt,*dpt;
  DWORD dwRet;
  DWORD dwV;

  if ( strlen(szSrc) > FIBER_IDENTLEN ) return ( 0 );

  dwRet = 0;
  spt   = (LPSTR)szSrc;
  dpt   = szDst;
  while ( *spt ){
    memcpy(szBuf,spt,4);	/* xxx */
    szBuf[4] = 0;
    CStringToValue(szBuf,&dwV);
    *dpt++ = (char)dwV;
    dwRet++;
    spt += 4;
    if ( dwRet > dwLen ) break;
  } /* while ( *spt ){ */
  
  return ( dwRet );
}


BOOL SetEf( EXTFILE *ef, LPCSTR lpLine )
{
  char *pt,*ppt;
  BOOL ret=FALSE;

  if ( (pt=_mbschr(lpLine,'=')) != NULL ){
    *pt++ = (char)0;
    if ( strlen(lpLine) <= FIBER_EXTLINELEN ){
      strcpy(ef->szExt,lpLine);
      if ( (ppt=strchr(pt,',')) != NULL ){
	*ppt++ = (char)0;
	if ( ! CStringToValue(ppt,&ef->dwOffset) ){
	  printf("Error: illegal offset value.\n");
	  goto Exit;
	}
      } else {
	ef->dwOffset = 0;
      }	/* if ( (ppt=strchr(pt,',')) != NULL ){ */
      if ( strlen(pt) <= FIBER_IDENTLEN ){
	if ( *pt == '0' ){
	  ef->dwIdentLength =
	    HexStringToBinary(pt,ef->szIdent,sizeof(ef->szIdent));
	  } else {
	    strcpy(ef->szIdent,pt);
	    ef->dwIdentLength = strlen(ef->szIdent);
	  }
	ret = TRUE;
      } else {
	printf("Error: ident [%s] is too long.\n",pt);
      }	/* if ( strlen(pt) <= FIBER_IDENTLEN ){ */
    } else {
      printf("Error: ext [%s] is too long.\n",pt);
    } /* if ( strlen(lpLine) <= FIBER_EXTLINELEN ){ */
  } /* if ( (pt=_mbschr(lpLine,'=')) != NULL ){ */
 Exit:
  if ( ! ret )
    ef->szExt[0] = 0;
  return ( ret );
}


BOOL WriteRegistry( VOID )
{
  DWORD ret;
  HKEY  hKey;
  char  szBuf[256];
  char  szIdent[FIBER_IDENTLEN*4+1];
  char  szSubKey[256];
  BOOL  fRet = TRUE;
  int   i,c;
  DWORD dwDisposition;
  char  *pt;

  ret = RegCreateKeyEx( HKEY_LOCAL_MACHINE,
			FIBER_SUBKEY,
			0,
			"",
			0,
			KEY_READ|KEY_WRITE,
			NULL,
			&hKey,
			&dwDisposition );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot open registry.\n");
    return ( FALSE );
  }

  c=0;
  for ( i=0; i<iEfs; i++ ){
    if ( ef[i].szExt[0] != 0 ){
      sprintf(szSubKey,"%s%d",FIBER_SUBKEY_EXTENSION,c);
      if ( (ef[i].szIdent[0]>=0x20) && (ef[i].szIdent[0]<0x80) ){
	memcpy( szIdent, ef[i].szIdent, ef[i].dwIdentLength );
	pt = szIdent + ef[i].dwIdentLength;
      } else {
	BinaryToHexString(ef[i].szIdent,ef[i].dwIdentLength,szIdent,sizeof(szIdent));
	pt = szIdent + ef[i].dwIdentLength*4;
      }
      *pt = '\0';
      if ( ef[i].dwOffset == 0 ){
	sprintf(szBuf,"%s=%s",ef[i].szExt,szIdent);
      } else {
	sprintf(szBuf,"%s=%s,%ld",ef[i].szExt,szIdent,ef[i].dwOffset);
      }
      ret = RegSetValueEx( hKey, szSubKey, 0, REG_SZ, szBuf, strlen(szBuf)+1 );
      if ( ret != ERROR_SUCCESS ){
	fRet = FALSE;
	break;
      }
      c++;
    } /* if ( ef[i].szExt[0] != 0 ){ */
  } /* for ( i=0; i<iEfs; i++ ){ */
  if ( fRet )
    for ( i=c; i<iExtNum; i++ ){
      sprintf(szSubKey,"%s%d",FIBER_SUBKEY_EXTENSION,i);
      RegDeleteValue( hKey, szSubKey );
    }

  RegCloseKey( hKey );

  return ( fRet );
}


BOOL InitializeRegistry( VOID )
{
  DWORD ret;
  HKEY  hKey;
  BOOL  fRet = TRUE;
  DWORD dwDisposition;
  int   i;
  char  szBuf[64];

  ret = RegCreateKeyEx( HKEY_LOCAL_MACHINE,
			FIBER_SUBKEY,
			0,
			"",
			0,
			KEY_READ|KEY_WRITE,
			NULL,
			&hKey,
			&dwDisposition );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot create registry.\n");
    return ( FALSE );
  }

  szBuf[0] = FIBER_EXTNUM;
  ret = RegSetValueEx( hKey, FIBER_SUBKEY_EXTNUM, 0,
		       REG_BINARY, szBuf, 1 );
  if ( ret != ERROR_SUCCESS ){
    fRet = FALSE;
    goto Exit;
  }

  ret = RegSetValueEx( hKey, FIBER_SUBKEY_EXECUTEUNKNOWNEXT, 0,
		       REG_BINARY, "\x1", 1 );
  if ( ret != ERROR_SUCCESS ){
    fRet = FALSE;
    goto Exit;
  }
  ret = RegSetValueEx( hKey, FIBER_SUBKEY_EXECUTEURL, 0,
		       REG_BINARY, "\x1", 1 );
  if ( ret != ERROR_SUCCESS ){
    fRet = FALSE;
    goto Exit;
  }
  ret = RegSetValueEx( hKey, FIBER_SUBKEY_TRUSTEXT, 0,
		       REG_BINARY, "\x1", 1 );
  if ( ret != ERROR_SUCCESS ){
    fRet = FALSE;
    goto Exit;
  }
  ret = RegSetValueEx( hKey, FIBER_SUBKEY_OVERRIDEEXT, 0,
		       REG_BINARY, "\x1", 1 );
  if ( ret != ERROR_SUCCESS ){
    fRet = FALSE;
    goto Exit;
  }
  RegCloseKey( hKey );

  /* init ef */
  iExtNum = FIBER_EXTNUM;
  ef = efInit;
  i=0;
  while ( ef[i].dwIdentLength != 0 ) i++;
  iEfs = ++i;

  /* write ef */
  WriteRegistry();

 Exit:
  return ( fRet );
}


BOOL ReadRegistry( VOID )
{
  DWORD ret;
  HKEY  hKey;
  char  szBuf[256];
  DWORD dwBufLen;
  char  szSubKey[256];
  BOOL  fRet = TRUE;
  int   i;

  ret = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
		      FIBER_SUBKEY,
		      0,
		      KEY_EXECUTE,
		      &hKey );
  if ( ret != ERROR_SUCCESS ){
    if ( InitializeRegistry() ){
      ret = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
			  FIBER_SUBKEY,
			  0,
			  KEY_EXECUTE,
			  &hKey );
      if ( ret != ERROR_SUCCESS ) return ( FALSE );
    } else {
      printf("Error: cannot open registry.\n");
      return ( FALSE );
    }
  }

  dwBufLen = sizeof(szBuf);
  ret = RegQueryValueEx( hKey,
			 FIBER_SUBKEY_EXTNUM,
			 NULL,
			 NULL,
			 szBuf,
			 &dwBufLen );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot query [%s] from registry.\n",FIBER_SUBKEY_EXTNUM);
    fRet = FALSE;
    goto Exit;
  }
  iExtNum = szBuf[0];

  dwBufLen = sizeof(szBuf);
  ret = RegQueryValueEx( hKey,
			 FIBER_SUBKEY_EXECUTEUNKNOWNEXT,
			 NULL,
			 NULL,
			 szBuf,
			 &dwBufLen );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot query [%s] from registry.\n",FIBER_SUBKEY_EXECUTEUNKNOWNEXT);
    fRet = FALSE;
    goto Exit;
  }
  fExecuteUnknownExt = ( szBuf[0] == 0 ) ? FALSE : TRUE;
  
  dwBufLen = sizeof(szBuf);
  ret = RegQueryValueEx( hKey,
			 FIBER_SUBKEY_EXECUTEURL,
			 NULL,
			 NULL,
			 szBuf,
			 &dwBufLen );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot query [%s] from registry.\n",FIBER_SUBKEY_EXECUTEURL);
    fRet = FALSE;
    goto Exit;
  }
  fExecuteURL = ( szBuf[0] == 0 ) ? FALSE : TRUE;
  
  dwBufLen = sizeof(szBuf);
  ret = RegQueryValueEx( hKey,
			 FIBER_SUBKEY_TRUSTEXT,
			 NULL,
			 NULL,
			 szBuf,
			 &dwBufLen );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot query [%s] from registry.\n",FIBER_SUBKEY_TRUSTEXT);
    fRet = FALSE;
    goto Exit;
  }
  fTrustExt = ( szBuf[0] == 0 ) ? FALSE : TRUE;
  
  dwBufLen = sizeof(szBuf);
  ret = RegQueryValueEx( hKey,
			 FIBER_SUBKEY_OVERRIDEEXT,
			 NULL,
			 NULL,
			 szBuf,
			 &dwBufLen );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot query [%s] from registry.\n",FIBER_SUBKEY_OVERRIDEEXT);
    fRet = FALSE;
    goto Exit;
  }
  fOverrideExt = ( szBuf[0] == 0 ) ? FALSE : TRUE;

  ef = (EXTFILE*)malloc(sizeof(EXTFILE)*iExtNum);
  if ( ef == NULL ){
    printf("Error: Not enough memory.\n");
    fRet = FALSE;
    goto Exit;
  }

  for ( i=0; i<iExtNum; i++ )
    ef[i].szExt[0] = 0;

  for ( iEfs=0; iEfs<iExtNum; iEfs++ ){
    dwBufLen = sizeof(szBuf);
    sprintf(szSubKey,"%s%d",FIBER_SUBKEY_EXTENSION,iEfs);
    ret = RegQueryValueEx( hKey, szSubKey, NULL, NULL, szBuf, &dwBufLen );
    if ( ret != ERROR_SUCCESS ) break;
    if ( ! SetEf( &ef[iEfs], szBuf ) ) break;
  } /* for ( iEfs=0; iEfs<iExtNum; iEfs++ ){ */
 Exit:
  RegCloseKey( hKey );

  return ( fRet );
}


int CheckFileType( LPSTR szInputFile )
{
  int    i;
  HANDLE hFile;
  char   szBuf[FIBER_IDENTLEN+1];
  DWORD  dwLen;

  for ( i=0; i<iEfs; i++ ){
    hFile = CreateFile(szInputFile,
		       GENERIC_READ,
		       FILE_SHARE_READ,
		       NULL,
		       OPEN_EXISTING,
		       0,
		       NULL);
    if ( hFile == INVALID_HANDLE_VALUE ) continue;
    if ( SetFilePointer(hFile,ef[i].dwOffset,NULL,FILE_BEGIN) != 0xFFFFFFFF ){
      ReadFile(hFile,szBuf,ef[i].dwIdentLength,&dwLen,NULL);
      if ( memcmp(szBuf,ef[i].szIdent,ef[i].dwIdentLength) == 0 ){
	CloseHandle( hFile );
	break;
      }
    } else {
      printf("Error: SetFilePointer fail.\n");
    } /* if ( SetFilePointer(hFile,ef[i].dwOffset,NULL,FILE_BEGIN) != 0xFFFFFFFF ){ */
    CloseHandle( hFile );
  } /* for ( i=0; i<iEfs; i++ ){ */
  if ( i == iEfs )
    return ( INT_MAX );
  else
    return ( i );
}


int SearchExtension( char *ext )
{
  int i;

  for ( i=0; i<iEfs; i++ )
    if ( strcmpi(ext,ef[i].szExt) == 0 ) break;

  if ( i == iEfs )
    return ( INT_MAX );
  else
    return ( i );
}


int GetExtension( char *lpszInputFile )
{
  char *pt;
  int  n;
  char szFileName[MAX_PATH];

  GetFileNameFromFullPath(lpszInputFile,szFileName,sizeof(szFileName));
  pt = strrchr(szFileName,'.');
  if ( pt == NULL ){
    n = INT_MAX;		/* ext is not found */
  } else {
    n = SearchExtension(++pt);
    if ( n == INT_MAX ) n = INT_MAX-1;
  }
  return ( n );
}


VOID PrintYesOrNo( BOOL flag )
{
  if ( flag )
    printf("yes\n");
  else
    printf("no\n");
}


VOID PrintOptions( VOID )
{
  printf("\n");
  printf(" OPTION            = VALUE\n");
  printf("---------------------------\n");

  printf(" ExtNum            = %d\n",iExtNum);
  printf(" ExecuteUnknownExt = ");
  PrintYesOrNo(fExecuteUnknownExt);
  printf(" ExecuteURL        = ");
  PrintYesOrNo(fExecuteURL);
  printf(" TrustExt          = ");
  PrintYesOrNo(fTrustExt);
  printf(" OverrideExt       = ");
  PrintYesOrNo(fOverrideExt);
}


VOID PrintExts( VOID )
{
  int i;
  char szBuf[256];

  printf("\n");
  printf(" ID : EXT=OFFSET\n");
  printf("-----------------\n");

  for ( i=0; i<iEfs; i++ ){
    if ( ef[i].szExt[0] == 0 ) break;
    if ( (ef[i].szIdent[0]>=0x20)&&(ef[i].szIdent[0]<0x80) ){
      strcpy(szBuf,ef[i].szIdent);
    } else {
      BinaryToHexString(ef[i].szIdent,ef[i].dwIdentLength,szBuf,sizeof(szBuf));
    }
    printf("%3d : %s=%s,%ld\n",i,ef[i].szExt,szBuf,ef[i].dwOffset);
  } /* for ( i=0; i<iEfs; i++ ){ */
}


BOOL SetOption( LPSTR szOption )
{
  LPSTR pt;
  int   num;
  char  opt;
  DWORD ret;
  HKEY  hKey;
  DWORD dwDisposition;
  BOOL  fRet=TRUE;

  pt = _mbschr( szOption, '=' );
  if ( pt == NULL ){
    printf("Error: illegal option format.\n");
    return ( FALSE );
  }
  *pt++ = '\0';

  ret = RegCreateKeyEx( HKEY_LOCAL_MACHINE,
			FIBER_SUBKEY,
			0,
			"",
			0,
			KEY_READ|KEY_WRITE,
			NULL,
			&hKey,
			&dwDisposition );
  if ( ret != ERROR_SUCCESS ){
    printf("Error: cannot open registry.\n");
    return ( FALSE );
  }

  if ( ! strcmp( FIBER_SUBKEY_EXTNUM, szOption ) ){
    /* ExtNum */
    num = atoi( pt );
    if ( num >= 256 ){
      printf("Error: too large number.\n");
      fRet = FALSE;
    } else if ( num > 0 ){
      opt = (char)num;
      ret = RegSetValueEx( hKey, FIBER_SUBKEY_EXTNUM, 0, REG_BINARY, &opt, 1 );
    } /* if ( num >= 256 ){ */
  } else if ( ! strcmp( FIBER_SUBKEY_EXECUTEUNKNOWNEXT, szOption ) ||
	      ! strcmp( FIBER_SUBKEY_EXECUTEURL, szOption ) ||
	      ! strcmp( FIBER_SUBKEY_TRUSTEXT, szOption ) ||
	      ! strcmp( FIBER_SUBKEY_OVERRIDEEXT, szOption ) ){
    if ( ! strcmp( pt, "no" ) )
      opt = 0;
    else if ( ! strcmp( pt, "yes" ) )
      opt = 1;
    else
      opt = 2;
    if ( opt != 2 ){
      ret = RegSetValueEx( hKey, szOption, 0, REG_BINARY, &opt, 1 );
    } else {
      printf("Error: please set OPTIONAME={yes,no}.\n");
      fRet = FALSE;
    } /* if ( opt != 2 ){ */
  } else {
    printf("Error: unknown option.\n");
    fRet = FALSE;
  }
  RegCloseKey( hKey );

  return ( fRet );
}


VOID ShellExecuteOpenSync( SHELLEXECUTEINFO *lpsei,
			   BOOL    fSynchronous,
			   BOOL    fDelete )
{
  BOOL             fRet;
  DWORD            dwRet;

  fRet = ShellExecuteEx( lpsei );
#ifdef _DEBUG
  printf("fRet=[%08x]\n", fRet);
  printf("hProcess=[%08x]\n", lpsei->hProcess);
#endif
  if ( fRet ){
    if ( fSynchronous && lpsei->hProcess != NULL ){
      dwRet = WaitForSingleObject( lpsei->hProcess, INFINITE );
      if ( dwRet == WAIT_FAILED )
	printf("Error: WaitForSingleObject fail.(%ld)\n",GetLastError());
      if ( fDelete )
	DeleteFile( lpsei->lpFile );
    } /* if ( fSynchronous ){ */
  } else {
    printf("Error: ShellExecuteEx fail.(%ld)\n",GetLastError());
  } /* if ( fRet ){ */
}

VOID PrintUsage( VOID )
{
  printf("File Inspect and Broking End Resolver  Version %s\n",FIBER_VERSION);
  printf("        Copyright (C) 1997-2004  Shuichi Kitaguchi\n");
  printf("\n");
  printf("Usage : fiber [option] <filename>\n");
  printf("Option:     -e ext                 assume that the extention is \"ext\"\n");
  printf("            -s                     synchronous mode\n");
  printf("            -a ext=ident,offset    add ext to database\n");
  printf("            -l                     list options and database\n");
  printf("            -r num                 remove ext indentified by num from database\n");
  printf("            -o Option={num,yes/no} set option\n");
  printf("            -i                     initialize database\n");
  printf("            -p parameters          set parameters\n");
  printf("            -n show                ShowWindow parameter \"nCmdShow\"\n");
  printf("            -b verb                name of verb(action)\n");
  printf("            -d directory           working directory\n");
}

/*
 * before ShellExecuteEx() "home.txt" or "www.txt",
 * internet browser is launched. when executing full
 * path name, browser is not launched. why?
 */
VOID
ConvertToFullPathName( LPSTR lpInputFile )
{
  char  szFullName[MAX_PATH];
  LPSTR lpFullName;
  DWORD dwLen;
  
  dwLen = GetFullPathName( lpInputFile, sizeof(szFullName),
			   szFullName, &lpFullName );
  if ( dwLen < sizeof(szFullName )){
    strcpy(lpInputFile, szFullName);
  } else if ( dwLen >= sizeof(szFullName) ){
    printf("Error: conversion buffer is small for [%s]\n",lpInputFile);
  } else {
    printf("Error: GetFullPathName fail.(%ld)\n",GetLastError());
  }
}

int
main( int  argc, char *argv[] )
{
  int  i,ri;
  char szInputFile[MAX_PATH];
  char szOpenFile[MAX_PATH];
  char *pt;
  int  state;
  char szExt[FIBER_EXTLEN];
  char szAddExt[FIBER_EXTLINELEN];
  char szOption[FIBER_OPTIONLEN];
  BOOL fExt         = FALSE;
  BOOL fError       = FALSE;
  BOOL fList        = FALSE;
  BOOL fAdd         = FALSE;
  BOOL fRemove      = FALSE;
  BOOL fInitialize  = FALSE;
  BOOL fOption      = FALSE;
  BOOL fSynchronous = FALSE;
  int  iRemoveNum;
  SHELLEXECUTEINFO sei;
#ifdef _DEBUG
  FILE *fp;
#endif

  szInputFile[0] = 0;
  szOpenFile[0]  = 0;

  memset( &sei, 0, sizeof(sei) );
  sei.cbSize       = sizeof(sei);
  sei.fMask        = SEE_MASK_NOCLOSEPROCESS|SEE_MASK_FLAG_DDEWAIT;
  sei.nShow        = SW_SHOWNORMAL;

  /* parse arguments */
  i=1;
  while ( i < argc ){
    if ( ! strcmp(argv[i],"-e") ){
      /* -e */
      fExt=TRUE;
      if ( (i+1) < argc ){
	if ( strlen(argv[i+1]) >= sizeof(szExt) ){
	  printf("Error: ext [%s] is too long.\n",argv[i+1]);
	  exit( -1 );
	}
	strcpy(szExt,argv[i+1]);
	i++;
      } else {
	printf("Error: ext is required with \'-e\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-a") ){
      /* -a */
      fAdd = TRUE;
      if ( (i+1) < argc ){
	if ( strlen(argv[i+1]) >= sizeof(szAddExt) ){
	  printf("Error: ext [%s] is too long.\n",argv[i+1]);
	  exit( -1 );
	}
	strcpy(szAddExt,argv[i+1]);
	i++;
      } else {
	printf("Error: ext is required with \'-a\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-r") ){
      /* -r */
      fRemove = TRUE;
      if ( (i+1) < argc ){
	iRemoveNum = atoi(argv[i+1]);
	i++;
      } else {
	printf("Error: number is required with \'-r\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-o") ){
      /* -o */
      fOption = TRUE;
      if ( (i+1) < argc ){
	if ( strlen(argv[i+1]) >= sizeof(szOption) ){
	  printf("Error: ext [%s] is too long.\n",argv[i+1]);
	  exit( -1 );
	}
	strcpy(szOption,argv[i+1]);
	i++;
      } else {
	printf("Error: Option is required with \'-o\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-l") ){
      /* -l */
      fList = TRUE;
    } else if ( ! strcmp(argv[i],"-i") ){
      /* -i */
      fInitialize = TRUE;
    } else if ( ! strcmp(argv[i],"-s") ){
      fSynchronous = TRUE;
    } else if ( ! strcmp(argv[i],"-p") ){
      /* -p */
      if ( (i+1) < argc ){
	sei.lpParameters = argv[i+1];
	i++;
      } else {
	printf("Error: parameter is required with \'-p\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-n") ){
      /* -n */
      if ( (i+1) < argc ){
	sei.nShow = atoi(argv[i+1]);
	if ( sei.nShow == 0 ) sei.nShow = SW_SHOWNORMAL;
	i++;
      } else {
	printf("Error: show is required with \'-n\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-b") ){
      /* -b */
      if ( (i+1) < argc ){
	sei.lpVerb = argv[i+1];
	i++;
      } else {
	printf("Error: verb(action) is required with \'-b\' option.\n");
	fError=TRUE;
      }
    } else if ( ! strcmp(argv[i],"-d") ){
      /* -d */
      if ( (i+1) < argc ){
	sei.lpDirectory = argv[i+1];
	i++;
      } else {
	printf("Error: directory name is required with \'-b\' option.\n");
	fError=TRUE;
      }
    } else {
      /* filename */
      if ( strlen(argv[i]) > sizeof(szInputFile) ){
	printf("Error: specified filename [%s] is too long.\n",argv[i]);
	fError=TRUE;
      } else {
	strcpy(szInputFile,argv[i]);
      }
      break;
    }
    i++;
  }

  /* show usage */
  if ( (argc==1) || (fError==TRUE) ){
    PrintUsage();
    exit( 0 );
  }

  /* read registry database */
  if ( ! ReadRegistry() ) exit( -1 );

  /* initialize */
  if ( fInitialize ){
    if ( InitializeRegistry() )
      exit( 0 );
    else
      exit( -1 );
  } /* if ( fInitialize ){ */

  /* set option */
  if ( fOption ){
    if ( SetOption(szOption) )
      exit( 0 );
    else
      exit( -1 );
  } /* if ( fOption ){ */
    
  /* add */
  if ( fAdd ){
    if ( (iEfs+1) > iExtNum ){
      printf("Error: cannot add ext (too many exts).\n");
      exit( -1 );
    }
    if ( ! SetEf(&ef[iEfs],szAddExt) ) exit( -1 );
    iEfs++;
    ef[iEfs].szExt[0] = 0;
    if ( ! WriteRegistry() ) exit( -1 );
    printf("ext [%s] is added.\n",ef[iEfs-1].szExt);
    exit( 0 );
  } /* if ( fAdd ){ */

  /* remove */
  if ( fRemove ){
    if ( iRemoveNum > iExtNum ){
      printf("Error: num is too large.\n");
      exit( -1 );
    }
    ef[iRemoveNum].szExt[0] = 0;
    if ( ! WriteRegistry() ) exit( -1 );
    printf("Id [%d] is removed.\n",iRemoveNum);
    exit( 0 );
  }

  /* list */
  if ( fList ){
    PrintOptions();
    PrintExts();
    exit( 0 );
  } /* if ( fList ){ */

  
#if 0	/* Windows' "file:" scheme is very funny... */
  /* eliminate "file:" scheme. */
  if( (strlen(szInputFile) >= 5) &&
      strncmp("file:", szInputFile, 5) == 0 ){
    memmove( szInputFile, &szInputFile[5], strlen(szInputFile)-5+1 );
  }
#endif

  /* set ext and execute */
  if ( fExt ){
    sprintf(szOpenFile,"%s.%s",szInputFile,szExt);
    ConvertToFullPathName(szOpenFile);
    if ( CheckFile( szOpenFile ) ){
      sei.lpFile = szOpenFile;
      ShellExecuteOpenSync( &sei, fSynchronous, FALSE );
    } else {
      CopyFile(szInputFile,szOpenFile,FALSE);
      sei.lpFile = szOpenFile;
      ShellExecuteOpenSync( &sei, fSynchronous, TRUE );
    }
    exit( 0 );
  } /* if ( fExt ){ */

  /* execute URL */
  if ( fExecuteURL ){
    pt = strchr(szInputFile,':');
    if ( pt ){
      if ( pt > (szInputFile+1) ){ /* "http:","ftp:",... */
	sei.lpFile = szInputFile;
	ShellExecuteOpenSync( &sei, fSynchronous, FALSE );
	exit( 0 );
      }
    } /* if ( pt ){ */
  } /* if ( fExecuteURL ){ */

  ConvertToFullPathName(szInputFile);
  RevConvertPathSeparator(szInputFile);

  i = CheckFileType(szInputFile);
  ri = GetExtension(szInputFile);
  /*
   * INT_MAX   - no exts.
   * INT_MAX-1 - unknown exts.
   *
   * state =  0 - error
   *          1 - szInputFile
   *          2 - add ext
   */

  if ( i == INT_MAX ){
    switch ( ri ){
    case INT_MAX:
      state = STATE_ERROR;
      break;
    case INT_MAX-1:
      state = ( fExecuteUnknownExt ) ? STATE_INPUTFILE : STATE_ERROR;
      break;
    default:
      state = ( fExecuteUnknownExt ) ? STATE_INPUTFILE : STATE_ERROR;
      break;
    }    
  } else {
    switch ( ri ){
    case INT_MAX:
      state = STATE_ADDEXT;
      break;
    case INT_MAX-1:
      state = ( fOverrideExt ) ? STATE_ADDEXT : STATE_INPUTFILE;
      break;
    default:
      state = ( fTrustExt ) ? STATE_INPUTFILE : STATE_ADDEXT;
      break;
    }
  } /* if ( i == INT_MAX ){ */
#ifdef _DEBUG
  fp = fopen("C:\\TEMP\\FIBER.LOG","a+");
  fprintf(fp,"InputFile=[%s],i=[%d]/ri=[%d],state=[%d]\n",szInputFile,i,ri,state);
  fclose(fp);
  printf("InputFile=[%s],i=[%d]/ri=[%d],state=[%d]\n",szInputFile,i,ri,state);
#endif
  /* go! */
  switch ( state ){
  case STATE_INPUTFILE:
    sei.lpFile = szInputFile;
    ShellExecuteOpenSync( &sei, fSynchronous, FALSE );
    break;
  case STATE_ADDEXT:
    sprintf(szOpenFile,"%s.%s",szInputFile,ef[i].szExt);
    if ( CheckFile( szOpenFile ) ){
      sei.lpFile = szOpenFile;
      ShellExecuteOpenSync( &sei, fSynchronous, FALSE );
    } else {
      CopyFile(szInputFile,szOpenFile,FALSE);
      sei.lpFile = szOpenFile;
      ShellExecuteOpenSync( &sei, fSynchronous, TRUE );
    }
    break;
  default:
    printf("Error: file not found or not supported.\n");
    break;
  } /* switch ( state ){ */
  exit ( 0 );
}
