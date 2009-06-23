
// jhc_rts_header.h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <assert.h>
#include <float.h>
#ifndef __WIN32__
#include <sys/select.h>
#include <sys/times.h>
#include <endian.h>
#include <sys/utsname.h>
#endif
#include <setjmp.h>


// #define our options

#define _JHC_GC_NONE  0
#define _JHC_JGC      1
#define _JHC_GC_BOEHM 2


#ifndef _JHC_GC
#define _JHC_GC _JHC_GC_NONE
#endif

#ifndef _JHC_PROFILE
#define _JHC_PROFILE 0
#endif

#ifndef _JHC_DEBUG
#ifdef NDEBUG
#define _JHC_DEBUG 0
#else
#define _JHC_DEBUG 1
#endif
#endif


// GNU attributes

#ifdef __GNUC__
#define A_ALIGNED  __attribute__ ((aligned))
#define A_CONST    __attribute__ ((const))
#define A_MALLOC   __attribute__ ((malloc))
#define A_MAYALIAS __attribute__ ((__may_alias__))
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE     __attribute__ ((pure))
#define A_UNUSED   __attribute__ ((unused))
#ifdef __i386__
#define A_REGPARM __attribute__ ((fastcall))
#else
#define A_REGPARM
#endif
#define A_STD    A_REGPARM

#else
#define A_ALIGNED
#define A_CONST
#define A_MALLOC
#define A_MAYALIAS
#define A_NORETURN
#define A_PURE
#define A_UNUSED
#define A_STD
#endif

// these should be enabled with newer versions of gcc
#define A_HOT
#define A_COLD
#define A_FALIGNED

#define STR(s) #s
#define XSTR(s) STR(s)
#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))


#ifdef __WIN32__
#define JHC_isWindows   1
#define JHC_isBigEndian 0
#else
#define JHC_isWindows 0
#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)
#endif

#define JHC_isPosix (!JHC_isWindows)

