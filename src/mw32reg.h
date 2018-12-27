/* MW32 registry access module header file */

#ifdef MEADOW
#define REG_MEADOW_ROOT "Software\\GNU\\Meadow"
#define REG_COMMON_ROOT "Software\\GNU\\Meadow\\Common"
#define REG_COMMON_ENV_ROOT "Software\\GNU\\Meadow\\Common\\Environment"
#define REG_VERSION_ROOT "Software\\GNU\\Meadow\\%s"
#define REG_VERSION_ENV_ROOT "Software\\GNU\\Meadow\\%s\\Environment"
#else
#define REG_ROOT "Software\\GNU\\Emacs\\"
#endif

extern LPBYTE mw32_get_string_resource (LPCTSTR name, LPCTSTR class);
extern LPBYTE mw32_get_env_resource (char *name, LPDWORD lpdwtype);
extern LPBYTE mw32_get_resource(char *key, LPDWORD lpdwtype);
extern char *x_get_string_resource (int rdb, char *name, char *class);


