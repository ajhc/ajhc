
// jhc_rts_header.h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <float.h>
#include <sys/times.h>
#include <setjmp.h>


// #define our options

#ifndef _JHC_BOEHM_GC
#define _JHC_BOEHM_GC 0
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
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE __attribute__ ((pure))
#define A_CONST __attribute__ ((const))
#define A_UNUSED __attribute__ ((unused))
#define A_MALLOC __attribute__ ((malloc))
#define A_MAYALIAS __attribute__ ((__may_alias__))
#ifdef __i386__
#define A_REGPARM __attribute__ ((fastcall))
#else
#define A_REGPARM
#endif
#define A_STD    A_REGPARM

#else
#define A_MAYALIAS
#define A_NORETURN
#define A_PURE
#define A_CONST
#define A_UNUSED
#define A_MALLOC
#define A_STD
#endif


#define STR(s) #s
#define XSTR(s) STR(s)
#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))



