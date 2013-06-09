#include <assert.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>
#include <setjmp.h>
#ifndef __WIN32__
#ifdef __ARM_EABI__
#include <malloc.h>
#else
#include <sys/select.h>
#include <sys/utsname.h>
#endif
#include <sys/times.h>
#include <sys/types.h>
#include <sys/param.h>
#else
#include <malloc.h>
#endif

#include "HsFFI.h"
#include "sys/wsize.h"
#include "rts/cdefs.h"

#ifndef _JHC_DEBUG
#ifdef NDEBUG
#define _JHC_DEBUG 0
#else
#define _JHC_DEBUG 1
#endif
#endif

#ifndef _JHC_STANDALONE
#define _JHC_STANDALONE 1
#endif

#ifndef JHC_STATUS
#define JHC_STATUS 0
#endif

#ifdef __WIN32__
#define JHC_isWindows   1
#define JHC_isBigEndian 0
#else
#define JHC_isWindows 0
#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)
#endif

#define JHC_isPosix (!JHC_isWindows && !defined(__ARM_EABI__))

#include "rts/profile.h"
#include "rts/rts_support.h"
#include "rts/gc.h"
#include "rts/jhc_rts.h"
#include "lib/lib_cbits.h"

// the program will provide the following
void _amain(void);
#if _JHC_GC == _JHC_GC_JGC
void jhc_hs_init(gc_t gc,arena_t arena);
#else
void jhc_hs_init();
#endif
extern const void * const nh_stuff[];
