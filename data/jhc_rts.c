#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <float.h>
#include <setjmp.h>

#ifdef USE_BOEHM_GC
#include <gc/gc.h>
#define jhc_malloc GC_malloc
#define jhc_malloc_atomic GC_malloc_atomic
#define jhc_free GC_free
#endif


#ifdef __GNUC__
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE __attribute__ ((pure))
#define A_CONST __attribute__ ((const))
#define A_UNUSED __attribute__ ((unused))
#define A_MALLOC __attribute__ ((malloc))
#else
#define A_NORETURN
#define A_PURE
#define A_CONST
#define A_UNUSED
#define A_MALLOC
#endif

#if defined(__GNUC__) && defined(__i386__)
#define A_REGPARM __attribute__ ((regparm(2)))
#else
#define A_REGPARM
#endif

#define STR(s) #s
#define XSTR(s) STR(s)

static void _amain(void);
static int jhc_argc;
static char **jhc_argv;
static char *jhc_progname;
static jmp_buf jhc_uncaught;

static int jhc_stdrnd[2] A_UNUSED = { 1 , 1 };

#ifdef _JHC_PROFILE
static uintmax_t prof_function_calls;
static uintmax_t prof_case_statements;
static uintmax_t prof_updates;
#ifndef USE_BOEHM_GC
static void *prof_memstart;
#endif

#define update_inc() prof_updates++
#define function_inc() prof_function_calls++
#define case_inc() prof_case_statements++
#else
#define update_inc()  do { } while(0)
#define function_inc()  do { } while(0)
#define case_inc()  do { } while(0)
#endif


#ifndef USE_BOEHM_GC
static void *jhc_mem = NULL;

static inline void *jhc_malloc(size_t n) {
        void *ret = jhc_mem;
        jhc_mem += n;
        return ret;
}

#define jhc_malloc_atomic(x) jhc_malloc(x)

#endif

static void
jhc_print_profile(void) {
#ifdef _JHC_PROFILE
        wprintf(L"Command: %s\n", jhc_command);
#ifndef USE_BOEHM_GC
        wprintf(L"Memory Allocated: %llu\n", (long long)(jhc_mem - prof_memstart));
#endif
        wprintf(L"Function Calls:   %llu\n", (long long)prof_function_calls);
        wprintf(L"Case Statements:  %llu\n", (long long)prof_case_statements);
        wprintf(L"Updates:          %llu\n", (long long)prof_updates);
#endif
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

#define jhc_setjmp(jb) sigsetjmp(*(jmp_buf *)jb,0)
#define jhc_longjmp(jb) siglongjmp(*(jmp_buf *)jb,1)

struct jhc_continuation {
    void *argument;
    jmp_buf jump_buf;
};



int
main(int argc, char *argv[])
{
#ifndef USE_BOEHM_GC
        size_t mem_size = 1000000000;
        while(!jhc_mem) {
                jhc_mem = malloc(mem_size);
                mem_size *= 0.80;
        }
#ifdef _JHC_PROFILE
        prof_memstart = jhc_mem;
#endif
#else
        GC_INIT()
#endif

        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
        setlocale(LC_ALL,"");
        if (sigsetjmp(jhc_uncaught,0))
                jhc_error("Uncaught Exception");
        else
                _amain();
        jhc_print_profile();
        return 0;
}




