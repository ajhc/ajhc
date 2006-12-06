/* HsFFI.h for jhc */

#include <inttypes.h>

typedef int HsInt;
typedef int32_t HsChar;
typedef int8_t HsInt8;
typedef int16_t HsInt16;
typedef int32_t HsInt32;
typedef int64_t HsInt64;
typedef uint8_t HsWord8;
typedef uint16_t HsWord16;
typedef uint32_t HsWord32;
typedef uint64_t HsWord64;
typedef int HsBool;
typedef double HsDouble;
typedef float HsFloat;
typedef void *HsPtr;
typedef void (*HsFunPtr)(void);
typedef void *HsStablePtr;

#define HS_BOOL_FALSE 0
#define HS_BOOL_TRUE 1

void hs_init (int *argc, char **argv[]);
void hs_exit (void);
void hs_set_argv(int argc, char *argv[]);
void hs_perform_gc(void);
void hs_free_stable_ptr(HsStablePtr sp);
void hs_free_fun_ptr(HsFunPtr fp);


