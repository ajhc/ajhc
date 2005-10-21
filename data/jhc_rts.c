#include <stdlib.h>
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#define _GNU_SOURCE
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <float.h>

#ifdef USE_BOEHM_GC
#include <gc/gc.h>
#define malloc GC_malloc
#define free GC_free
#endif


#ifdef __GNUC__
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE __attribute__ ((pure))
#define A_CONST __attribute__ ((const))
#define A_UNUSED __attribute__ ((unused))
#else
#define A_NORETURN
#define A_PURE
#define A_CONST
#define A_UNUSED
#endif

#define STR(s) #s
#define XSTR(s) STR(s)

static void XAmain();
static int jhc_argc;
static char **jhc_argv;
static char *jhc_progname;

static int jhc_stdrnd[2] A_UNUSED = { 1 , 1 };

#ifdef _JHC_PROFILE
static uintmax_t prof_memory_allocated;
static uintmax_t prof_function_calls;
static uintmax_t prof_case_statements;
static uintmax_t prof_updates;

#define malloc(n) ( prof_memory_allocated += (n), malloc( (n) ) )
#define update_inc() prof_updates++
#define function_inc() prof_function_calls++
#define case_inc() prof_case_statements++
#else
#define update_inc()  do { } while(0)
#define function_inc()  do { } while(0)
#define case_inc()  do { } while(0)
#endif



static void
jhc_print_profile(void) {
#ifdef _JHC_PROFILE
        wprintf(L"Memory Allocated: %llu\n", (long long)prof_memory_allocated);
        wprintf(L"Function Calls:   %llu\n", (long long)prof_function_calls);
        wprintf(L"Case Statements:  %llu\n", (long long)prof_case_statements);
        wprintf(L"Updates:          %llu\n", (long long)prof_updates);
#endif
}

int
main(int argc, char *argv[])
{
        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
        setlocale(LC_ALL,"");
        XAmain();
        jhc_print_profile();
        return 0;
}

static void A_NORETURN A_UNUSED
jhc_exit(int n) {
        jhc_print_profile();
        exit(n);
}

static void  A_NORETURN A_UNUSED
jhc_error(char *s) {
        fputs(s,stderr);
        fputs("\n",stderr);
        jhc_print_profile();
        exit(255);
}

static void  A_NORETURN A_UNUSED
jhc_case_fell_off(int n) {
        fflush(stdout);
        fprintf(stderr, "\n%s:%i: case fell off\n", __FILE__, n);
        abort();
}


typedef union node node_t;

