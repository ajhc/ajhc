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
#ifndef __WIN32__
#include <sys/select.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/utsname.h>
#endif
#include <setjmp.h>

// #define our options

#define _JHC_GC_NONE   0
#define _JHC_GC_JGC    1
#define _JHC_GC_BOEHM  2
#define _JHC_GC_REGION 3

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

#ifndef _JHC_STANDALONE
#define _JHC_STANDALONE 1
#endif

#ifndef JHC_STATUS
#define JHC_STATUS 0
#endif

// GNU attributes
#ifdef __GNUC__
#  define __predict_true(exp)     __builtin_expect(!!(exp), 1)
#  define __predict_false(exp)    __builtin_expect(!!(exp), 0)
#else
#  define __predict_true(exp)     (exp)
#  define __predict_false(exp)    (exp)
#endif

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

#define M_ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))

#ifdef __WIN32__
#define JHC_isWindows   1
#define JHC_isBigEndian 0
#else
#define JHC_isWindows 0
#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)
#endif

#define JHC_isPosix (!JHC_isWindows)

static void _amain(void);

static void jhc_alloc_init(void);
static void jhc_alloc_fini(void);
static void jhc_alloc_print_stats(void);
static void jhc_print_profile(void);

static int jhc_argc;
static char **jhc_argv;
static char *jhc_progname;

#if JHC_STATUS > 1
#define debugf(...) fprintf(stderr,__VA_ARGS__)
#else
#define debugf(...) do { } while (0)
#endif
