
static void _amain(void);
static void jhc_arch_assert(void);
static int jhc_argc;
static char **jhc_argv;
static char *jhc_progname;
static jmp_buf jhc_uncaught;

static HsInt jhc_stdrnd[2] A_UNUSED = { 1 , 1 };

#if _JHC_PROFILE

static uintmax_t jhc_prof_function_calls;
static uintmax_t jhc_prof_case_statements;
static uintmax_t jhc_prof_updates;

#define jhc_update_inc()   jhc_prof_updates++
#define jhc_function_inc() jhc_prof_function_calls++
#define jhc_case_inc()     jhc_prof_case_statements++

#else

#define jhc_update_inc()    do { } while(0)
#define jhc_function_inc()  do { } while(0)
#define jhc_case_inc()      do { } while(0)

#endif

static void
jhc_print_profile(void) {
        struct tms tm;
        times(&tm);
        if(!(_JHC_PROFILE || getenv("JHC_RTS_PROFILE"))) return;

        fwprintf(stderr, L"\n-----------------\n");
        fwprintf(stderr, L"Profiling: %s\n", jhc_progname);
        fwprintf(stderr, L"Command: %s\n", jhc_command);
        fwprintf(stderr, L"Complie: %s\n", jhc_c_compile);
        fwprintf(stderr, L"Version: %s\n\n", jhc_version);
#if !_JHC_BOEHM_GC
        fwprintf(stderr, L"Memory Allocated: %llu bytes\n", (unsigned long long)(jhc_mem - jhc_memstart));
#endif
        float cpt = (float)sysconf(_SC_CLK_TCK);
        fwprintf(stderr, L"User Time:   %.2fs\n", (float)tm.tms_utime/cpt);
        fwprintf(stderr, L"System Time: %.2fs\n", (float)tm.tms_stime/cpt);
        fwprintf(stderr, L"Total Time:  %.2fs\n", (float)(tm.tms_stime + tm.tms_utime)/cpt);

#if _JHC_PROFILE
        fwprintf(stderr, L"\nFunction Calls:   %llu\n", (unsigned long long)jhc_prof_function_calls);
        fwprintf(stderr, L"Case Statements:  %llu\n", (unsigned long long)jhc_prof_case_statements);
        fwprintf(stderr, L"Updates:          %llu\n", (unsigned long long)jhc_prof_updates);
#endif
        fwprintf(stderr, L"-----------------\n");
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

#define prim_umaxbound(t) ((t)~((t)0))
#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*8 - 1))))
#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*8 - 1))))


int
main(int argc, char *argv[])
{
        jhc_arch_assert();
        jhc_malloc_init();
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



