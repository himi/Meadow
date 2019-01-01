/******************************************
      MW32 Registry Manager
      Written by himi
******************************************/

#include "config.h"
#include "lisp.h"
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include "mw32reg.h"
#include "charset.h"
#include "coding.h"
#include "mw32term.h"

#ifdef MEADOW
LPCTSTR meadow_version_string = MEADOW;
LPCTSTR meadow_common_root = REG_COMMON_ROOT;
LPCTSTR meadow_common_env_root = REG_COMMON_ENV_ROOT;
LPCTSTR meadow_version_root = REG_VERSION_ROOT;
LPCTSTR meadow_version_env_root = REG_VERSION_ENV_ROOT;
#endif

static LPBYTE
mw32_get_resource_with_class (LPCTSTR name,
			      LPCTSTR key,
			      HKEY hkey,
			      LPDWORD lpdwtype)
{
  HKEY hrkey;
  LPBYTE lpret;
  DWORD dwType;
  DWORD cbData;

  if (RegOpenKeyEx (hkey, key, 0, KEY_READ, &hrkey)
      != ERROR_SUCCESS) return (LPBYTE) NULL;

  if (RegQueryValueEx (hrkey, name, NULL, &dwType, NULL, &cbData)
      != ERROR_SUCCESS) goto error; 
      
  if (dwType != REG_SZ) goto error;
  lpret = (LPBYTE) xmalloc (cbData);
  if (RegQueryValueEx (hrkey, name, NULL, &dwType, lpret, &cbData) !=
      ERROR_SUCCESS)  goto error;

  if (lpdwtype) *lpdwtype = dwType;

  RegCloseKey (hrkey);
  return lpret;

 error:
  RegCloseKey (hrkey);
  return (LPBYTE) NULL;
  
}
      
LPBYTE mw32_get_string_resource (LPCTSTR name, LPCTSTR key)
{
  LPBYTE lpret = NULL;

  if (!name) return NULL;
  lpret = mw32_get_resource_with_class (name, key,
					HKEY_CURRENT_USER, (LPDWORD) 0);
  if (!lpret)
    lpret = mw32_get_resource_with_class (name, key,
					  HKEY_LOCAL_MACHINE, (LPDWORD) 0);
  return lpret;

}

LPBYTE 
mw32_get_env_resource (char *name, LPDWORD lpdwtype)
{
  LPBYTE lpret = NULL;
  TCHAR tmpstr[MAX_PATH];

  sprintf (tmpstr, meadow_version_env_root, meadow_version_string);

  if (!name) return NULL;

  lpret = mw32_get_resource_with_class (name, tmpstr,
					HKEY_CURRENT_USER, lpdwtype);
  if (lpret) return lpret;
  lpret = mw32_get_resource_with_class (name, tmpstr,
					HKEY_LOCAL_MACHINE, lpdwtype);
  if (lpret) return lpret;


  lpret = mw32_get_resource_with_class (name, meadow_common_env_root,
					HKEY_CURRENT_USER, lpdwtype);
  if (lpret) return lpret;
  lpret = mw32_get_resource_with_class (name, meadow_common_env_root,
					HKEY_LOCAL_MACHINE, lpdwtype);

  return lpret;
}

char *
x_get_string_resource (int rdb, char *name, char *class)
{
  char *lpret;
  TCHAR tmpstr[MAX_PATH];

  sprintf (tmpstr, meadow_version_root, meadow_version_string);

  lpret = (char*) mw32_get_string_resource (name, tmpstr);
  if (lpret) return lpret;
  lpret = (char*) mw32_get_string_resource (name, meadow_common_root);
  if (lpret) return lpret;

  lpret = (char*) mw32_get_string_resource (class, tmpstr);
  if (lpret) return lpret;
  lpret = (char*) mw32_get_string_resource (class, meadow_common_root);

  return lpret;

}


/***********************************************************************
                      Registry opration functions
 ***********************************************************************/

Lisp_Object Qregistry_binary;
Lisp_Object Qregistry_dword;
Lisp_Object Qregistry_dword_little_endian;
Lisp_Object Qregistry_dword_big_endian;
Lisp_Object Qregistry_expand_sz;
Lisp_Object Qregistry_link;
Lisp_Object Qregistry_multi_sz;
Lisp_Object Qregistry_none;
Lisp_Object Qregistry_qword;
Lisp_Object Qregistry_qword_little_endian;
Lisp_Object Qregistry_resource_list;
Lisp_Object Qregistry_sz;
Lisp_Object Qregistry_full_resource_descriptor;
Lisp_Object Qregistry_resource_requirements_list;

Lisp_Object registry_integer_overflow_data;

struct root_key
{
  char *name;
  HKEY hkey;
};

static struct root_key root_keys[] =
{
  {"HKEY_CLASSES_ROOT", HKEY_CLASSES_ROOT},
  {"HKEY_CURRENT_CONFIG", HKEY_CURRENT_CONFIG},
  {"HKEY_CURRENT_USER", HKEY_CURRENT_USER},
  {"HKEY_LOCAL_MACHINE", HKEY_LOCAL_MACHINE},
  {"HKEY_USERS", HKEY_USERS},
  {"HKEY_PERFORMANCE_DATA", HKEY_PERFORMANCE_DATA},
  {"HKEY_DYN_DATA", HKEY_DYN_DATA},
  {NULL, NULL}
};

typedef struct registry_key
{
  HKEY hkey;
  LPTSTR rootkey;
  LPTSTR subkey;
  LPTSTR name;
  DWORD type;
  void (*to_registry_data) (LPBYTE pdata, DWORD size, Lisp_Object data);
  DWORD (*to_registry_data_size) (Lisp_Object data);
  Lisp_Object (*to_lisp_object) (LPBYTE data, DWORD size);
} registry_key;

/* REG_QWORD is defined in MSVC 7 or later */
#ifndef REG_QWORD
#define REG_QWORD 11
#endif

static Lisp_Object
registry_type_to_symbol (DWORD type)
{
  switch (type)
    {
    case REG_NONE:
      return Qregistry_none;
    case REG_SZ:
      return Qregistry_sz;
    case REG_EXPAND_SZ:
      return Qregistry_expand_sz;
    case REG_BINARY:
      return Qregistry_binary;
    case REG_DWORD:		/* is REG_DWORD_LITTLE_ENDIAN */
      return Qregistry_dword;
    case REG_DWORD_BIG_ENDIAN:
      return Qregistry_dword_big_endian;
    case REG_LINK:
      return Qregistry_link;
    case REG_MULTI_SZ:
      return Qregistry_multi_sz;
    case REG_RESOURCE_LIST:
      return Qregistry_resource_list;
    case REG_FULL_RESOURCE_DESCRIPTOR:
      return Qregistry_full_resource_descriptor;
    case REG_RESOURCE_REQUIREMENTS_LIST:
      return Qregistry_resource_requirements_list;
    case REG_QWORD:
      return Qregistry_qword;
    default:
      return Qnil;		/* unknown */
    }
}

static DWORD
registry_symbol_to_type (Lisp_Object type)
{
  if (EQ (Qregistry_none, type))
    return REG_NONE;
  else if (EQ (Qregistry_sz, type))
    return REG_SZ;
  else if (EQ (Qregistry_expand_sz, type))
    return REG_EXPAND_SZ;
  else if (EQ (Qregistry_binary, type))
    return REG_BINARY;
  else if (EQ (Qregistry_dword, type))
    return REG_DWORD;
  else if (EQ (Qregistry_dword_big_endian, type))
    return REG_DWORD_BIG_ENDIAN;
  else if (EQ (Qregistry_link, type))
    return REG_LINK;
  else if (EQ (Qregistry_multi_sz, type))
    return REG_MULTI_SZ;
  else if (EQ (Qregistry_resource_list, type))
    return REG_RESOURCE_LIST;
  else if (EQ (Qregistry_full_resource_descriptor, type))
    return REG_FULL_RESOURCE_DESCRIPTOR;
  else if (EQ (Qregistry_resource_requirements_list, type))
    return REG_RESOURCE_REQUIREMENTS_LIST;
  else if (EQ (Qregistry_qword, type))
    return REG_QWORD;
  else
    return 0;
}

static DWORD
registry_to_reg_dword_size(Lisp_Object data)
{
  return sizeof(DWORD);
}

static DWORD
registry_to_reg_qword_size(Lisp_Object data)
{
  return sizeof(DWORD) * 2;
}

static DWORD
registry_to_reg_sz_size(Lisp_Object data)
{
  char *text;
  DWORD size;

  MW32_ENCODE_TEXT (data, Vw32_system_coding_system, &text, &size);
  return size + 1;
}

static void
registry_to_reg_sz(LPBYTE pdata, DWORD size, Lisp_Object data)
{
  char *text;
  DWORD l;

  MW32_ENCODE_TEXT (data, Vw32_system_coding_system, &text, &l);
  bcopy (text, pdata, size);
}

static DWORD
registry_to_reg_multi_sz_size(Lisp_Object data)
{
  DWORD size = 0;
  Lisp_Object tail, string;
 
  for (tail = data; CONSP (tail); tail = XCDR (tail))
    {
      string = XCAR (tail);
      size += registry_to_reg_sz_size (string);
    }

  return size + 1;
}

static void
registry_to_reg_multi_sz(LPBYTE pdata, DWORD size, Lisp_Object data)
{
  Lisp_Object tail, string;
  LPBYTE p = pdata;
  DWORD l = 0;
 
  for (tail = data; CONSP (tail); tail = XCDR (tail))
    {
      string = XCAR (tail);
      l = registry_to_reg_sz_size (string);
      registry_to_reg_sz (p, l, string);
      p += l;
    }

  *p = '\0';
}

static Lisp_Object
registry_to_string(LPBYTE pdata, DWORD size)
{
  return build_string (pdata);
}

#define REGSTRY_BUFFER_MAX ((DWORD) 32768U)

static Lisp_Object
registry_to_expand_string(LPBYTE pdata, DWORD size)
{
  LPTSTR pstr;
  Lisp_Object result;

  pstr = alloca(REGSTRY_BUFFER_MAX);
  if (ExpandEnvironmentStrings (pdata, pstr, REGSTRY_BUFFER_MAX) == 0L) 
    return Qnil;
  result = build_string (pstr);

  return result;
}

#undef REGSTRY_BUFFER_MAX

static Lisp_Object
registry_to_string_list(LPBYTE pdata, DWORD size)
{
  char *ptr = pdata;
  Lisp_Object result = Qnil;

  while (*ptr != '\0')
    {
      result = Fcons (build_string (ptr), result);
      ptr += strlen (ptr) + 1;
    }
  result = Fnreverse (result);

  return result;
}

static Lisp_Object
registry_to_raw_number(LPBYTE pdata, DWORD size)
{
  /* Returns data as cons of higher 16bit and lower 16bit. */
  /* Do not convert endian here.  This function returns data
     AS-IS. */
  DWORD data;

  data = *(LPDWORD) pdata;
  return Fcons (make_number(data >> 16), make_number(data & 0xFFFF));
}

static Lisp_Object
registry_to_raw_qword_number(LPBYTE pdata, DWORD size)
{
  int i;
  Lisp_Object result = Qnil;

  /* return as 4 element list ordered from highest 16bit to
     lowest 16 bit. */
  for (i=0; i<4; i++)
    result = Fcons (make_number (((LPWORD)pdata)[i]), result);

  return result;
}

static Lisp_Object
registry_to_unibyte_string(LPBYTE pdata, DWORD size)
{
  return make_unibyte_string (pdata, size);
}

static void
registry_key_function_setup(registry_key *regkey)
{
  switch (regkey->type)
    {
    case REG_SZ:
      regkey->to_lisp_object = registry_to_string;
      regkey->to_registry_data = registry_to_reg_sz;
      regkey->to_registry_data_size = registry_to_reg_sz_size;
      break;

    case REG_EXPAND_SZ:
      regkey->to_lisp_object = registry_to_expand_string;
      regkey->to_registry_data = registry_to_reg_sz;
      regkey->to_registry_data_size = registry_to_reg_sz_size;
      break;
      
    case REG_MULTI_SZ:
      regkey->to_lisp_object = registry_to_string_list;
      regkey->to_registry_data = registry_to_reg_multi_sz;
      regkey->to_registry_data_size = registry_to_reg_multi_sz_size;
      break;
      
    case REG_DWORD:
    case REG_DWORD_BIG_ENDIAN:
      regkey->to_lisp_object = registry_to_raw_number;
      regkey->to_registry_data_size = registry_to_reg_dword_size;
      break;

    case REG_QWORD:
      regkey->to_lisp_object = registry_to_raw_qword_number;
      regkey->to_registry_data_size = registry_to_reg_qword_size;
      break;
      
    case REG_BINARY:
    case REG_NONE:
    case REG_LINK:
    case REG_RESOURCE_LIST:
    case REG_FULL_RESOURCE_DESCRIPTOR:
    case REG_RESOURCE_REQUIREMENTS_LIST:
    default:
      regkey->to_lisp_object = registry_to_unibyte_string;
      break;
    }
}

static Lisp_Object
registry_get (registry_key *regkey)
{
  HKEY hrkey;
  LPBYTE pdata = NULL;
  DWORD type;
  DWORD size;
  Lisp_Object obj = Qnil;

  if (regkey->subkey)
    {
      if (RegOpenKeyEx (regkey->hkey, regkey->subkey, 0, KEY_READ, &hrkey) != ERROR_SUCCESS)
        {
          Fsignal (Qerror, Fcons (build_string ("Cannot open registry key"),
                                  Fcons (build_string (regkey->rootkey),
                                         build_string (regkey->subkey))));
          return Qnil;
        }
    }
  else
    hrkey = regkey->hkey;

  if (RegQueryValueEx (hrkey, regkey->name, NULL, &type, NULL, &size)
      != ERROR_SUCCESS) goto quit;
  pdata = (LPBYTE) alloca (++size);
  if (RegQueryValueEx (hrkey, regkey->name, NULL, &type, pdata, &size)
      != ERROR_SUCCESS) goto quit;

  regkey->type = type;
  registry_key_function_setup (regkey);
  if (!regkey->to_lisp_object)
    goto quit;
  obj = (*regkey->to_lisp_object) (pdata, size);

 quit:
  if (regkey->hkey != hrkey)
    RegCloseKey (hrkey);
  if (NILP(obj))
    return Qnil;

  return Fcons (obj, registry_type_to_symbol (type));
}

static Lisp_Object
registry_delete_key (registry_key *regkey)
{
  if (RegDeleteKey(regkey->hkey, regkey->subkey) != ERROR_SUCCESS)
    {
      Fsignal (Qerror, Fcons (build_string ("Invalid registry key"),
                              Fcons(build_string (regkey->rootkey), build_string (regkey->subkey))));
      return Qnil;
    }

  return Qt;
}

static Lisp_Object
registry_delete_value (registry_key *regkey)
{
  HKEY hrkey;
  LONG err;

  if (regkey->subkey)
    {
      if (RegOpenKeyEx (regkey->hkey, regkey->subkey, 0, KEY_WRITE, &hrkey) != ERROR_SUCCESS)
        {
          Fsignal (Qerror, Fcons (build_string ("Cannot open registry key"),
                                  Fcons (build_string (regkey->rootkey),
                                         build_string (regkey->subkey))));
          return Qnil;
        }
    }
  else
    hrkey = regkey->hkey;

  err = RegDeleteValue(hrkey, regkey->name);
  if (regkey->hkey != hrkey)
    RegCloseKey (hrkey);

  if (err != ERROR_SUCCESS)
    {
      Fsignal (Qerror, Fcons (build_string ("Invalid registry name"),
                              Fcons(build_string (regkey->rootkey), 
                                    Fcons(build_string (regkey->subkey), build_string (regkey->name)))));
      return Qnil;
    }

  return Qt;
}

static Lisp_Object
registry_create_key (registry_key *regkey)
{
  HKEY hrkey;
  LONG err;

  err = RegCreateKeyEx(regkey->hkey, regkey->subkey, 0, NULL, 
                       REG_OPTION_NON_VOLATILE,
                       KEY_WRITE, NULL, &hrkey,
                       NULL);

  if (err != ERROR_SUCCESS)
    {
      Fsignal (Qerror, Fcons (build_string ("Invalid registry key"),
                              Fcons(build_string (regkey->rootkey), build_string (regkey->subkey))));
      return Qnil;
    }

  RegCloseKey(hrkey);

  return Qt;
}

static HKEY
registry_root_key_to_hkey (char *root_key)
{
  int i;

  for (i = 0; root_keys[i].name != NULL; i++)
    if (stricmp (root_key, root_keys[i].name) == 0)
      break;

  if (root_keys[i].name == NULL)
    return 0;

  return root_keys[i].hkey;
}


static int
registry_prepare (char *key, char **prootkey, char **psubkey)
{
  char *ptr;
  
  if (strlen (key) == 0)
    return 0;			/* fail */

  /* split long key into root key and sub key,
     First entry until fist separator or EOL is used as root key name,
     and rest are used as subkey. */
  ptr = strchr (key, '\\');
  if (ptr == key)
    return 0;			/* no root key! */
  if (ptr == NULL)
    {
      /* no subkey */
      *prootkey = xstrdup (key);
      *psubkey = NULL;
    }
  else
    {
      int len = ptr-key;
      char *rootkey = xmalloc (len+1);
      memcpy (rootkey, key, len);
      rootkey[len] = '\0';
      *prootkey = rootkey;
      *psubkey = xstrdup (ptr+1);
    }
  return 1;
}

static int
registry_key_setup(registry_key *regkey, char *key_data, char *name_data)
{
  bzero (regkey, sizeof (registry_key));

  if (!registry_prepare (key_data, &regkey->rootkey, &regkey->subkey))
    return 0;

  regkey->hkey = registry_root_key_to_hkey (regkey->rootkey);
  if (regkey->hkey == 0)
    return 0;

  regkey->name = name_data;

  return 1;
}

static void
registry_key_destory(registry_key *regkey)
{
  if (regkey->rootkey) xfree (regkey->rootkey);
  if (regkey->subkey) xfree (regkey->subkey);
}

static int
registry_data_valid_p(Lisp_Object data)
{
  Lisp_Object type_sym;
  DWORD type;

  type_sym = XCDR (data);
  if (!SYMBOLP (type_sym))
    {
      wrong_type_argument (Qsymbolp, type_sym);
      return 0;
    }
  type = registry_symbol_to_type (type_sym);

  switch (type)
    {
    case REG_SZ:
    case REG_EXPAND_SZ:
      if (!STRINGP (XCAR (data)))
        {
          wrong_type_argument (Qstringp, (XCAR (data)));
          return 0;
        }
      break;

    case REG_MULTI_SZ:
      if (!CONSP (XCAR (data)) && !NILP (XCAR (data)))
        {
          wrong_type_argument (Qlistp, (XCAR (data)));
          return 0;
        }
      else 
        {
          Lisp_Object tail, string;

          for (tail = XCAR (data); CONSP (tail); tail = XCDR (tail))
            {
              string = XCAR (tail);
              if (!STRINGP (string))
                {
                  wrong_type_argument (Qstringp, string);
                  return 0;
                }
            }
        }
      break;

    case REG_DWORD:
    case REG_DWORD_BIG_ENDIAN:
      return 0;
      /* TODO
      if (!CONSP (XCAR (data)))
        return 0;
      */
      break;

    case REG_QWORD:
      return 0;
      /* TODO
      if (!CONSP (XCAR (data)) && !NILP (XCAR (data)))
        return 0;
      */
      break;

    case REG_BINARY:
    case REG_NONE:
    case REG_LINK:
    case REG_RESOURCE_LIST:
    case REG_FULL_RESOURCE_DESCRIPTOR:
    case REG_RESOURCE_REQUIREMENTS_LIST:
    default:
      return 0;
      /* TODO
      if (!STRINGP (XCAR (data)))
        return 0;
      */
      break;
    }

  return 1;
}

static Lisp_Object
registry_set (registry_key *regkey, Lisp_Object data)
{
  HKEY hrkey;
  DWORD size;
  LPBYTE pdata = NULL;
  Lisp_Object result = Qnil;

  if (regkey->subkey)
    {
      if (RegOpenKeyEx (regkey->hkey, regkey->subkey, 0, KEY_WRITE, &hrkey) != ERROR_SUCCESS)
        {
          Fsignal (Qerror, Fcons (build_string ("Cannot open registry key"),
                                  Fcons (build_string (regkey->rootkey),
                                         build_string (regkey->subkey))));
          return Qnil;
        }
    }
  else
    hrkey = regkey->hkey;

  regkey->type = registry_symbol_to_type (XCDR (data));
  if (!regkey->type) goto quit;

  registry_key_function_setup (regkey);
  if (!regkey->to_registry_data
      || !regkey->to_registry_data_size)
    goto quit;
  
  size = (*regkey->to_registry_data_size) (XCAR (data));
  pdata = (LPBYTE) xmalloc (size);
  (*regkey->to_registry_data) (pdata, size, XCAR (data));

  if (RegSetValueEx (hrkey, regkey->name, 0, regkey->type, pdata, size) == ERROR_SUCCESS)
    result = Qt;
  else
    result = Qnil;

  xfree (pdata);

 quit:
  if (regkey->hkey != hrkey)
    RegCloseKey (hrkey);

  return result;
}

static Lisp_Object
registry_enum_all_keys(HKEY hkey, DWORD max_subkey_len, int with_data)
{
  registry_key regkey;
  LPTSTR name_buf;
  DWORD name_buf_siz;
  Lisp_Object obj = Qnil;
  int idx = 0;
  struct gcpro gcpro1;

  bzero (&regkey, sizeof(registry_key));
  name_buf_siz = max_subkey_len+1;
  name_buf = alloca (name_buf_siz);
  GCPRO1 (obj);
  while (RegEnumKeyEx (hkey, idx++, name_buf, &name_buf_siz,
		       NULL, NULL, NULL, NULL) == ERROR_SUCCESS)
    {
      /*  if with_data, item is (NAME . (DATA . TYPE))
	  if not with_data, item is NAME */
      Lisp_Object name_obj = DECODE_SYSTEM (build_string (name_buf));
      if (with_data)
        {
          regkey.hkey = hkey;
          regkey.subkey = name_buf;
          regkey.name = NULL;
          obj = Fcons (Fcons (name_obj,
                              registry_get (&regkey)),
                       obj);
        }
      else
	obj = Fcons (name_obj, obj);
      name_buf_siz = max_subkey_len+1;
    }
  obj = Fnreverse (obj);
  UNGCPRO;

  return obj;
}

static Lisp_Object
registry_enum_all_values(HKEY hkey, DWORD max_value_len, int with_data)
{
  registry_key regkey;
  LPTSTR name_buf;
  DWORD name_buf_siz;
  Lisp_Object obj = Qnil;
  int idx = 0;
  struct gcpro gcpro1;

  bzero (&regkey, sizeof(registry_key));
  name_buf_siz = max_value_len+1;
  name_buf = alloca (name_buf_siz);
  GCPRO1 (obj);
  while (RegEnumValue (hkey, idx++, name_buf, &name_buf_siz,
		       NULL, NULL, NULL, NULL) == ERROR_SUCCESS)
    {
      /*  if with_data, item is (NAME . (DATA . TYPE))
	  if not with_data, item is NAME */
      Lisp_Object name_obj = DECODE_SYSTEM (build_string (name_buf));
      if (with_data)
        {
          regkey.hkey = hkey;
          regkey.subkey = NULL;
          regkey.name = name_buf;
          obj = Fcons (Fcons (name_obj, 
                              registry_get (&regkey)),
                       obj);
        }
      else
	obj = Fcons (name_obj, obj);
      name_buf_siz = 1000; //max_value_len+1;
    }
  obj = Fnreverse (obj);
  UNGCPRO;

  return obj;
}

DEFUN ("mw32-registry-list-keys", Fmw32_registry_list_keys,
       Smw32_registry_list_keys, 1, 2, 0,
       "Make string list of sub key names under specified KEY.\n\
If optional argument WITH-DATA is non-nil, returns list of cons of\n\
name string and value. The format of value is same to\n\
`mw32-registry-get'.")
       (key, with_data)
       Lisp_Object key, with_data;
{
  registry_key regkey;
  DWORD n_subkeys, max_subkey_len;
  LPTSTR key_data;
  DWORD key_len;
  HKEY hkey = 0;
  Lisp_Object result;
  LPSTR errmsg = NULL;
  long err;

  CHECK_STRING (key, 0);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);
  if (!registry_key_setup(&regkey, key_data, NULL))
    {
      errmsg = "Cannot open KEY, %s";
      goto quit;
    }

  err = RegOpenKeyEx (regkey.hkey, regkey.subkey, 0, KEY_READ, &hkey);
  if (err != ERROR_SUCCESS)
    {
      errmsg = "Cannot open KEY, %s";
      goto quit;		/* open failed */
    }
  
  /* first, determine maximum size of value name */
  if (RegQueryInfoKey (hkey, NULL, NULL, NULL, &n_subkeys, &max_subkey_len,
		       NULL, NULL, NULL, NULL, NULL, NULL) != ERROR_SUCCESS)
    {
      errmsg = "Cannot get information of KEY, %s";
      goto quit;		/* failed */
    }
  
  result = registry_enum_all_keys(hkey, max_subkey_len, !NILP (with_data));
  
 quit:
  if (hkey) RegCloseKey (hkey);
  registry_key_destory (&regkey);
  
  if (errmsg) {
    int nargs = 2;
    Lisp_Object args[2];
    args[0] = build_string (errmsg);
    args[1] = key;
    Fsignal (Qerror, Fcons (Fformat (nargs, args), Qnil));
  }
  return result;
}


DEFUN ("mw32-registry-list-values", Fmw32_registry_list_values,
       Smw32_registry_list_values, 1, 2, 0,
       "Make string list of value names under specified KEY.\n\
If optional argument WITH-DATA is non-nil, returns list of cons of\n\
name string and value. The format of value is same to\n\
`mw32-registry-get'.")
       (key, with_data)
       Lisp_Object key, with_data;
{
  registry_key regkey;
  DWORD n_values, max_value_len;
  LPTSTR key_data;
  DWORD key_len;
  HKEY hkey = 0;
  LPSTR errmsg = NULL;
  long err;
  Lisp_Object result;

  CHECK_STRING (key, 0);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);
  if (!registry_key_setup(&regkey, key_data, NULL))
    {
      errmsg = "Cannot open KEY, %s";
      goto quit;
    }

  err = RegOpenKeyEx (regkey.hkey, regkey.subkey, 0, KEY_READ, &hkey);
  if (err != ERROR_SUCCESS)
    {
      errmsg = "Cannot open KEY, %s";
      goto quit;		/* open failed */
    }
  
  /* first, determine maximum size of value name */
  if (RegQueryInfoKey (hkey, NULL, NULL, NULL, NULL, NULL, NULL,
		       &n_values, &max_value_len,
		       NULL, NULL, NULL) != ERROR_SUCCESS)
    {
      errmsg = "Cannot get information of KEY, %s";
      goto quit;		/* failed */
    }

  result = registry_enum_all_values(hkey, max_value_len, !NILP (with_data));

 quit:
  if (hkey) RegCloseKey (hkey);
  registry_key_destory (&regkey);
  if (errmsg) {
    int nargs = 2;
    Lisp_Object args[2];
    args[0] = build_string (errmsg);
    args[1] = key;
    Fsignal (Qerror, Fcons (Fformat (nargs, args), Qnil));
  }
  return result;
}


DEFUN ("mw32-registry-get", Fmw32_rigistry_get, Smw32_registry_get, 1, 2, 0,
       "Get registry data from KEY or NAME in KEY if specified.\n\
Result is cons of value data and registry data type.\n\
This is example of REG_SZ value \"SystemRoot\" in key\n\
\"HKEY_LOCAL_MACHINE\\\\SOFTWARE\\\\Microsoft\\\\Windows NT\\\\CurrentVersion\"\n\
  (\"c:\\\\WINDOWS\" . registry-sz)\n\
\n\
Car of returned cons is:\n\
 cons for 'registry-dword or 'registry-dword-big-endian,\n\
 or list for 'registry-qword ordered from highest 16bit word to lowest.\n\
 or list of string without decoding for 'registry-multi-sz,\n\
 or string for all other types.\n\
\n\
Available data types and corresponding REG_xxx are shown bellow:\n\
 registry-binary                       REG_BINARY\n\
 registry-dword                        REG_DWORD\n\
 registry-dword-little-endian          (same to REG_DWORD)\n\
 registry-dword-big-endian             REG_DWORD_BIG_ENDIAN\n\
 registry-expand-sz                    REG_EXPAND_SZ\n\
 registry-link                         REG_LINK\n\
 registry-multi-sz                     REG_MULTI_SZ\n\
 registry-none                         REG_NONE\n\
 registry-qword                        REG_QWORD\n\
 registry-qword-little-endian          (same to REG_QWORD)\n\
 registry-resource-list                REG_RESOURCE_LIST\n\
 registry-sz                           REG_SZ\n\
 registry-full-resource-descriptor     REG_FULL_RESOURCE_DESCRIPTOR\n\
 registry-resource-requirements-list   REG_RESOURCE_REQUIREMENTS_LIST\n\
")
     (key, name)
     Lisp_Object key, name;
{
  registry_key regkey;
  Lisp_Object result;
  int key_len, name_len;
  char *key_data = NULL, *name_data = NULL; /* alloca'ed data */

  CHECK_STRING (key, 0);
  if (!NILP (name))
    CHECK_STRING (name, 0);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);
  
  if (STRINGP (name))
    {
      MW32_ENCODE_TEXT (name, Vw32_system_coding_system,
			&name_data, &name_len);
    }
  else
    name_data = NULL;

  if (!registry_key_setup(&regkey, key_data, name_data))
    {
      registry_key_destory (&regkey);
      return Qnil;
    }

  result = registry_get (&regkey);

  registry_key_destory (&regkey);

  return result;
}


DEFUN ("mw32-registry-words-to-integer",
       Fmw32_registry_words_to_integer, Smw32_registry_words_to_integer,
       2, 4, 0,
       "Make emacs integer from big integer specified as some 16bit words. \n\
At least two words, LWORD and HWORD, should be specified for 32 bit\n\
integer data. And with optional HHWORD and HHHWORD, four words for\n\
64 bit integer data. Check that higher bits are all correct as sign\n\
expansion of lower bits as signed integer (VALBITS width).\n\
If it does not fit for emacs integer, cause overflow error.")
       (lword, hword, hhword, hhhword)
       Lisp_Object lword, hword, hhword, hhhword;
{
  EMACS_INT dw, hdw;
  EMACS_INT mask = ~((EMACS_INT)0) << (VALBITS-1);
  
  CHECK_NUMBER (lword, 0);
  CHECK_NUMBER (hword, 0);
  dw = (XFASTINT (hword) << 16) + XFASTINT (lword);
  
  if (!NILP(hhword))
    {
      CHECK_NUMBER (hhword, 0);
      CHECK_NUMBER (hhhword, 0);
      hdw = (XFASTINT (hhhword) << 16) + XFASTINT (hhword);
    }
  
  // TODO for 64bit env.
  if (((dw & mask) == 0 &&	/* non negative number */
       (NILP (hhword) || hdw == 0)) ||
      ((dw & mask) == mask &&	/* negative number */
       (NILP (hhword) || hdw == ~((DWORD)0)))) { 
    return make_number (dw);
  } else {
    /* not fit for emacs integer, cause overflow error */
    Fsignal (Qoverflow_error, registry_integer_overflow_data);
  }
  
  /* never come here */
  return Qnil;
}

DEFUN ("mw32-registry-delete-key", Fmw32_registry_delete_key, Smw32_registry_delete_key, 1, 1, 0,
       ""
)
     (key)
     Lisp_Object key;
{
  registry_key regkey;
  Lisp_Object result;
  int key_len;
  char *key_data = NULL; /* alloca'ed data */

  CHECK_STRING (key, 0);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);

  if (!registry_key_setup(&regkey, key_data, NULL))
    {
      registry_key_destory (&regkey);
      return Qnil;
    }
  
  result = registry_delete_key(&regkey);
 
  registry_key_destory (&regkey);

  return result;
}

DEFUN ("mw32-registry-delete-value", Fmw32_registry_delete_value, Smw32_registry_delete_value, 2, 2, 0,
       ""
)
     (key, name)
     Lisp_Object key, name;
{
  registry_key regkey;
  Lisp_Object result;
  int key_len, name_len;
  char *key_data = NULL, *name_data = NULL; /* alloca'ed data */

  CHECK_STRING (key, 0);
  CHECK_STRING (name, 1);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);
  MW32_ENCODE_TEXT (name, Vw32_system_coding_system, &name_data, &name_len);

  if (!registry_key_setup(&regkey, key_data, name_data))
    {
      registry_key_destory (&regkey);
      return Qnil;
    }

  result = registry_delete_value(&regkey);

  registry_key_destory (&regkey);

  return result;
}

DEFUN ("mw32-registry-create-key", Fmw32_registry_create_key, Smw32_registry_create_key, 1, 1, 0,
       ""
)
     (key)
     Lisp_Object key;
{
  registry_key regkey;
  Lisp_Object result;
  int key_len;
  char *key_data = NULL; /* alloca'ed data */

  CHECK_STRING (key, 0);

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);

  if (!registry_key_setup(&regkey, key_data, NULL))
    {
      registry_key_destory (&regkey);
      return Qnil;
    }

  result = registry_create_key(&regkey);
 
  registry_key_destory (&regkey);

  return result;
}

DEFUN ("mw32-registry-set", Fmw32_registry_set, Smw32_registry_set, 3, 3, 0,
       ""
)
     (Lisp_Object key, Lisp_Object name, Lisp_Object data)
{
  registry_key regkey;
  Lisp_Object result;
  int key_len, name_len;
  char *key_data = NULL, *name_data = NULL; /* alloca'ed data */

  CHECK_STRING (key, 0);
  CHECK_STRING (name, 1);
  CHECK_LIST (data, 2);

  if (!registry_data_valid_p (data))
    return Qnil;

  MW32_ENCODE_TEXT (key, Vw32_system_coding_system, &key_data, &key_len);

  if (STRINGP (name))
    {
      MW32_ENCODE_TEXT (name, Vw32_system_coding_system,
			&name_data, &name_len);
    }
  else
    name_data = NULL;

  if (!registry_key_setup(&regkey, key_data, name_data))
    {
      registry_key_destory (&regkey);
      return Qnil;
    }

  result = registry_set (&regkey, data);

  registry_key_destory (&regkey);

  return result;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
reinit_syms_of_mw32reg ()
{
}

void
syms_of_mw32reg ()
{ 
  Qregistry_binary = intern ("registry-binary");
  staticpro (&Qregistry_binary);
  Qregistry_dword = intern ("registry-dword");
  staticpro (&Qregistry_dword);
  Qregistry_dword_little_endian = intern ("registry-dword-little-endian");
  staticpro (&Qregistry_dword_little_endian);
  Qregistry_dword_big_endian = intern ("registry-dword-big-endian");
  staticpro (&Qregistry_dword_big_endian);
  Qregistry_expand_sz = intern ("registry-expand-sz");
  staticpro (&Qregistry_expand_sz);
  Qregistry_link = intern ("registry-link");
  staticpro (&Qregistry_link);
  Qregistry_multi_sz = intern ("registry-multi-sz");
  staticpro (&Qregistry_multi_sz);
  Qregistry_none = intern ("registry-none");
  staticpro (&Qregistry_none);
  Qregistry_qword = intern ("registry-qword");
  staticpro (&Qregistry_qword);
  Qregistry_qword_little_endian = intern ("registry-qword-little-endian");
  staticpro (&Qregistry_qword_little_endian);
  Qregistry_resource_list = intern ("registry-resource-list");
  staticpro (&Qregistry_resource_list);
  Qregistry_sz = intern ("registry-sz");
  staticpro (&Qregistry_sz);
  Qregistry_full_resource_descriptor =
    intern ("registry-full-resource-descriptor");
  staticpro (&Qregistry_full_resource_descriptor);
  Qregistry_resource_requirements_list =
    intern ("registry-resource-requirements-list");
  staticpro (&Qregistry_resource_requirements_list);

  registry_integer_overflow_data
    = Fcons (Qoverflow_error,
	     Fcons (build_string ("data is not fit for emacs integer"),
		    Qnil));
  staticpro (&registry_integer_overflow_data);
  
  defsubr (&Smw32_registry_get);
  defsubr (&Smw32_registry_words_to_integer);
  defsubr (&Smw32_registry_list_keys);
  defsubr (&Smw32_registry_list_values);
  defsubr (&Smw32_registry_delete_key);
  defsubr (&Smw32_registry_delete_value);
  defsubr (&Smw32_registry_create_key);
  defsubr (&Smw32_registry_set);
}

void
init_mw32reg ()
{
}
