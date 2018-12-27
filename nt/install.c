/* Meadow Install Program */
#include <windows.h>
#include <ddeml.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <io.h>
#include <direct.h>
#include <mbstring.h>

#include "../src/mw32reg.h"

/* alias old names */
#if _MSC_VER >= 1300
#define stricmp _stricmp
#define access _access
#define chdir _chdir
#endif

LPCTSTR meadow_version_string = MEADOW;
LPCTSTR meadow_version_root = REG_VERSION_ROOT;
LPCTSTR meadow_version_env_root = REG_VERSION_ENV_ROOT;

int w32_platform_id = -1;	/* 0=WinNT/2000/XP, 1=Win9x, -1=(unknown) */
int codepage;
int verbose_flag = 0;		/* 0...normal, 1...verbose */

typedef struct slmessage
{
  int cp;
  unsigned char *message;
} slmessage;

/* US CP is 437,.... -1 must be last entry. It is default...*/

#define ENGLISH -1
#define NIHONGO 932

slmessage access_denied[] =
{
#ifndef __MINGW32__
  {NIHONGO, "アクセスが拒否されました。\n"
   "Administrator権限で実行してください。\n"},
#endif
  {ENGLISH, "Access denied.\n"
   "Run this program with administrator privilege."}
};
slmessage undetect_error[] =
{
#ifndef __MINGW32__
  {NIHONGO, "原因不明のシステムエラー。\n"},
#endif
  {ENGLISH, "Undetected system error.\n"},
};
slmessage usage[] =
{
  {ENGLISH, "Usage: install [-s] [-r] [-h dir] [-v]\n"
   "        -s          do not add shortcut to Start Menu.\n"
   "        -r          remove information from Registry Database.\n"
   "        -h dir      specify home directory name.\n"
   "        -v          verbose mode (for debug).\n"},
};
slmessage lt_check_error[] =
{
#ifndef __MINGW32__
  {NIHONGO, "アーカイブの展開に失敗しています。\n"
   "INSTALL.Meadow のWinZipの注意点を良く読んでください。\n"},
#endif
  {ENGLISH, "Some files not extracted correctly from the archive.\n"
   "Read the description about WinZip in INSTALL.Meadow.\n"},
};
slmessage lfn_check_error[] =
{
#ifndef __MINGW32__
  {NIHONGO, "アーカイブの展開に失敗しています。\n"
   "Long File Nameの使えるtar & gzip を使って下さい。\n"},
#endif
  {ENGLISH, "Some files not extracted correctly from the archive.\n"
   "Use tar & gzip which can handle Long File Names properly.\n"},
};
slmessage pf_error[] =
{
#ifndef __MINGW32__
  {NIHONGO, "このプログラムはWindows 95/98/Me/NT/2000でのみ動作します。\n"},
#endif
  {ENGLISH, "This program can run only on Windows 95/98/Me/NT/2000.\n"},
};
slmessage illegal_comspec[] =
{
#ifndef __MINGW32__
  {NIHONGO, "システムがWindows NT系であるのに、環境変数COMSPECにCOMMAND.COMが指定されている可能性があります。AUTOEXEC.BAT等でCOMSPECが指定してある場合、その記述を削除してください。\n"},
#endif
  {ENGLISH, "This system is Windows NT, but environment variable COMSPEC may point to COMMAND.COM.  If COMSPEC is specified in AUTOEXEC.BAT and etc, please remove it.\n"},
};
slmessage no_comspec[] =
{
#ifndef __MINGW32__
  {NIHONGO, "環境変数COMSPECが定義されていません。\n"},
#endif
  {ENGLISH, "Environment variable COMSPEC is not defined.\n"},
};
slmessage nd_homedrive[] =
{
#ifndef __MINGW32__
  {NIHONGO, "HOMEDRIVEが定義されていません。\n"},
#endif
  {ENGLISH, "HOMEDRIVE is not defined. \n"},
};
slmessage nd_homepath[] =
{
#ifndef __MINGW32__
  {NIHONGO, "HOMEPATHが定義されていません。\n"},
#endif
  {ENGLISH, "HOMEPATH is not defined. \n"},
};
slmessage nd_homedir[] =
{
#ifndef __MINGW32__
  {NIHONGO, "環境変数HOMEが設定できません。\n"},
#endif
  {ENGLISH, "Cannot set an environment variable HOME. \n"},
};
slmessage home_prompt[] =
{
#ifndef __MINGW32__
  {NIHONGO, "Meadowが.emacsファイルを読み込むディレクトリを指定してください。\n"
   "何も設定しない場合は、Meadowは以下のディレクトリに存在する.emacsファイルを読み込みます。\n"},
#endif
  {ENGLISH, "Enter the directory which Meadow read .emacs file from.\n"
   "If you do not specify, Meadow reads .emacs file in following directory.\n"},
};
void slmessage_fprintf(int lang, FILE *stream, slmessage* slfmt, ...)
{
  va_list va;

  va_start(va, slfmt);
  for (;!(slfmt->cp == lang || slfmt->cp == -1);slfmt++);
  vfprintf(stream, slfmt->message, va);
  va_end(va);
}

HDDEDATA CALLBACK DdeCallback (UINT uType, UINT uFmt, HCONV hconv,
			       HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
			       DWORD dwData1, DWORD dwData2)
{
  return NULL;
}

#define DdeCommand(str) 	\
	DdeClientTransaction (str, strlen (str)+1, HConversation, (HSZ)NULL, \
		              CF_TEXT, XTYP_EXECUTE, 30000, NULL)

#define MULE_CLASS ""

void error_op(int errorflag)
{
  if (errorflag != ERROR_SUCCESS){
    switch(errorflag) {
    case ERROR_ACCESS_DENIED:
      slmessage_fprintf(codepage, stderr, access_denied);
      break;
    default:
      slmessage_fprintf(codepage, stderr, undetect_error);
      break;
    }
    exit(100);
  }
  return;
}

void error_usage(void)
{
  fprintf(stderr, "installer for %s\n", MEADOW_VERSION_STRING);
  slmessage_fprintf(codepage, stderr, usage);
  exit(1);
}

void init_current_environment()
{
  int version;

  codepage = GetConsoleCP();

  version = GetVersion();
  if ( 0 == (version & 0x80000000) )
    w32_platform_id = 0;
  else if ((version & 0xC0000000) == 0xC0000000)
    w32_platform_id = 1;
  else {
    slmessage_fprintf(codepage, stderr, pf_error);
    exit(1);
  }
}

HKEY create_key(HKEY hrootkey, LPCTSTR entry)
{
  HKEY hkey;
  DWORD result;
  int errorflag;

  errorflag = RegCreateKeyEx(hrootkey, entry,
			     0, MULE_CLASS, REG_OPTION_NON_VOLATILE,
			     KEY_WRITE, NULL, &hkey, &result);

  error_op(errorflag);

  return hkey;
}

void check_files(unsigned char *mule_root)
{
  unsigned char buf[MAX_PATH];
  FILE *handle;
  int data, flag;

  sprintf(buf, "%slisp\\mw32scroll.elc", mule_root);
  flag = access(buf, 0);
  if (flag != 0) {
    if (verbose_flag)
      perror(buf);
    slmessage_fprintf(codepage, stderr, lfn_check_error);
    exit(100);
  }
  sprintf(buf, "%slisp\\loaddefs.el", mule_root);
  handle = fopen(buf, "rb");
  while ((data = fgetc(handle)) != EOF) {
    if (data == '\n') break;
    if (data == '\r') {
      slmessage_fprintf(codepage, stderr, lt_check_error);
      exit(100);
    }
  }
  return;
}

void set_value(HKEY hkey, unsigned char *name, unsigned char *value)
{
  int errorflag;

  errorflag = RegSetValueEx (hkey, name, 0, REG_SZ,
			     value, lstrlen (value) + 1);
  error_op(errorflag);
}

void set_value_path(HKEY hkey, 
		    unsigned char *name,
		    unsigned char *dir_name,
		    unsigned char *subdir)
{
  unsigned char full_name[MAX_PATH];
  strcpy(full_name, dir_name);
  strcat(full_name, subdir);
  set_value(hkey, name, full_name);
}

void get_root(unsigned char *program, unsigned char *result)
{
  unsigned char dir_name[_MAX_DIR];
  unsigned char drive_name[_MAX_DRIVE];
  unsigned char file_name[_MAX_FNAME];
  unsigned char ext_name[_MAX_EXT];

  _splitpath(program, drive_name, dir_name, file_name, ext_name);

  sprintf(result, "%s%s", drive_name, dir_name);
}

void get_shell_name(unsigned char *result)
{
  unsigned char dir_name[_MAX_DIR];
  unsigned char drive_name[_MAX_DRIVE];
  unsigned char file_name[_MAX_FNAME];
  unsigned char ext_name[_MAX_EXT];
  unsigned char *shell_name;

#if 0
  shell_name = getenv("COMSPEC");
  if (!shell_name) {
    if (w32_platform_id == 1)
      strcpy(result, "cmdproxy");
    else
      strcpy(result, "cmdproxy");
  } else {
    _splitpath(shell_name, drive_name, dir_name, file_name, ext_name);
    sprintf(result, "%s%s", file_name, ext_name);
  }
#else
  shell_name = getenv("COMSPEC");
  if (shell_name){
    /* if COMSPEC is specified, system is NT and shell_name has "command.com" */
    if (w32_platform_id == 0){
      _splitpath(shell_name, drive_name, dir_name, file_name, ext_name);
      if (stricmp(ext_name, ".COM") == 0){
	slmessage_fprintf(codepage, stdout, illegal_comspec);
	exit(100);
      }
    }
  } else {
    slmessage_fprintf(codepage, stdout, no_comspec);
    exit(100);
  }
  strcpy(result, "cmdproxy");
#endif
}

void set_registry_data(char *main_root,
		       char *shell_name,
		       char *home_dir)
{
  HKEY hkey;
  TCHAR tmpstr[MAX_PATH];
  unsigned char buffer[2048];
  unsigned char main_top_root[MAX_PATH];
  int i;

  sprintf(tmpstr, meadow_version_env_root, meadow_version_string);

  i = strlen(main_root) - 2;
  for (;i >= 0;i--) if (main_root[i] == '\\') break;
  
  if (i > 0)
    {
      memcpy(main_top_root, main_root, i + 1);
      main_top_root[i + 1] = '\0';
    }
  else
    strcpy(main_top_root, main_root);

  hkey = create_key(HKEY_LOCAL_MACHINE, tmpstr);
  sprintf(buffer, "%ssite-lisp;%ssite-lisp;%spackages\\lisp;%slisp;%sleim",
	  main_root, main_top_root, main_root, main_root, main_root);
  set_value(hkey,      "EMACSLOADPATH", buffer);
  set_value_path(hkey, "EMACSDATA",     main_root, "etc");
  set_value_path(hkey, "EMACSPATH",     main_root, "bin");
  set_value_path(hkey, "EMACSLOCKDIR",  main_root, "lock");
  set_value_path(hkey, "INFOPATH",      main_root, "info");
  set_value_path(hkey, "BITMAPPATH",    main_root, "bitmap");
  set_value_path(hkey, "EMACSDOC",      main_root, "etc");
  set_value(hkey,      "TERM",          "CMD");
  set_value(hkey,      "SHELL",         shell_name);
  set_value(hkey,      "HOME",          home_dir);

  RegCloseKey(hkey);
}

void remove_registry_data(void)
{
  RegDeleteKey(HKEY_LOCAL_MACHINE, meadow_version_root);
}

void register_to_shell(char *main_root)
{
  DWORD idDde;
  HCONV HConversation;
  HSZ program_manager;
  char additem[MAX_PATH*2 + 100];

  idDde = 0;
  DdeInitialize (&idDde, (PFNCALLBACK)DdeCallback, APPCMD_CLIENTONLY, 0);

  program_manager = DdeCreateStringHandle (idDde, "PROGMAN", CP_WINANSI);

  if (HConversation = DdeConnect (idDde, program_manager,
				  program_manager, NULL))
    {
      DdeCommand ("[CreateGroup (Meadow)]");
      DdeCommand ("[ReplaceItem (Meadow2)]");
      sprintf (additem, "[AddItem (%s\\bin\\RunMW32.exe,Meadow2,"
	       "%s\\bin\\meadow.ico)]",
	       main_root, main_root);
      DdeCommand (additem);

      DdeDisconnect (HConversation);
    }

  DdeFreeStringHandle (idDde, program_manager);

  DdeUninitialize (idDde);
}

int check_opt(char *arg, char *opt)
{
  if ((arg[0] != '/')
      && (arg[0] != '-')) return 0;

  if (strcmp(&arg[1], opt) == 0) return 1;
  return 0;
}

/* convert '/' -> '\' */
void r_convert_path_separator(unsigned char *pathname)
{
  char *pt;

  while ( 1 ){
    pt = strrchr(pathname, '/');
    if (pt == NULL) break;
    if (pt == (char *) pathname){
      *pt = '\\';
      break;
    }
    if (_ismbblead(*(pt-1)) == 0) *pt = '\\'; /* replace */
  }
}

int main (int argc, char **argv)
{
  unsigned char main_root[MAX_PATH];
  unsigned char this_prog[MAX_PATH];
  unsigned char home_dir[MAX_PATH];
  unsigned char pre_home_dir[MAX_PATH];
  unsigned char shell_name[MAX_PATH];
  unsigned char *p;
  int remove_flag = 0;
  int register_flag = 1;

  init_current_environment();

  GetModuleFileName(NULL, this_prog, MAX_PATH);
  get_root(this_prog, main_root);

  check_files(main_root);

  {
    int i;
    int set_home_dir = 0;

    for (i = 1;i < argc;i++)
      {
	if (check_opt(argv[i], "r"))
	  {
	    remove_flag = 1;
	    set_home_dir = 1;
	  }
	else if (check_opt(argv[i], "s"))
	  {
	    register_flag = 0;
	  }
	else if (check_opt(argv[i], "v"))
	  {
	    verbose_flag = 1;
	  }
	else if (check_opt(argv[i], "h"))
	  {
	    if ((i+1) < argc)
	      {
		strcpy(home_dir, argv[i+1]);
		set_home_dir = 1;
		i++;
	      }
	    else
	      {
		error_usage();
	      }
	  }
	else
	  {
	    error_usage();
	  }
      }
    if (set_home_dir < 0)
      error_usage();
    else if (set_home_dir == 0)
      {
	slmessage_fprintf(codepage, stdout, home_prompt);
	p = getenv("HOME");
	if (p)
	  {
	    strcpy(pre_home_dir, p);
	    fprintf(stdout, "[%s] (HOME directory)\n", pre_home_dir);
	  }
	else
	  {
	    strcpy(pre_home_dir, main_root);
	    p = strrchr(pre_home_dir, '\\');
	    if (p) *p = '\0';
	    fprintf(stdout, "[%s]\n", pre_home_dir);
	  }
	fgets(home_dir, MAX_PATH, stdin);
	p = strchr(home_dir, '\n');
	if (p) *p = '\0';
	if (home_dir[0] == '\0') strcpy(home_dir, pre_home_dir);
	r_convert_path_separator(home_dir);
      }
  }

  get_shell_name(shell_name);

  if (remove_flag)
    {
      remove_registry_data();
    }
  else
    {
      set_registry_data(main_root, shell_name, home_dir);
      sprintf(pre_home_dir, "HOME=%s", home_dir);
      if (_putenv(pre_home_dir) != 0)
	{
          if (verbose_flag)
            perror(pre_home_dir);
	  slmessage_fprintf(codepage, stdout, nd_homedir);
	  exit(1);
	}
      if (register_flag)
	register_to_shell(main_root);
    }

  return 0;
}
