
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

        fprintf(stderr, "\n-----------------\n");
        fprintf(stderr, "Profiling: %s\n", jhc_progname);
        fprintf(stderr, "Command: %s\n", jhc_command);
        fprintf(stderr, "Complie: %s\n", jhc_c_compile);
        fprintf(stderr, "Version: %s\n\n", jhc_version);
#if _JHC_GC == _JHC_GC_NONE
        fprintf(stderr, "Memory Allocated: %llu bytes\n", (unsigned long long)(jhc_mem - jhc_memstart));
#endif
        float cpt = (float)sysconf(_SC_CLK_TCK);
        fprintf(stderr, "User Time:   %.2fs\n", (float)tm.tms_utime/cpt);
        fprintf(stderr, "System Time: %.2fs\n", (float)tm.tms_stime/cpt);
        fprintf(stderr, "Total Time:  %.2fs\n", (float)(tm.tms_stime + tm.tms_utime)/cpt);

#if _JHC_PROFILE
        fprintf(stderr, "\nFunction Calls:   %llu\n", (unsigned long long)jhc_prof_function_calls);
        fprintf(stderr, "Case Statements:  %llu\n", (unsigned long long)jhc_prof_case_statements);
        fprintf(stderr, "Updates:          %llu\n", (unsigned long long)jhc_prof_updates);
#endif
        fprintf(stderr, "-----------------\n");
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


inline static int A_UNUSED
jhc_utf8_getchar(void)
{
    return getchar_unlocked();
}

inline static int A_UNUSED
jhc_utf8_getc(FILE *f)
{
    return getc_unlocked(f);
}

inline static int A_UNUSED
jhc_utf8_putchar(int ch)
{
    return putchar_unlocked(ch);
}

inline static int A_UNUSED
jhc_utf8_putc(int ch, FILE *f)
{
    return putc_unlocked(ch,f);
}


int
main(int argc, char *argv[])
{
        /* A few random assertions about the architecture that the compiler
         * assumes. should be true of any but the oddest of beasts.
         */

        assert(sizeof(HsPtr) == sizeof(HsFunPtr));
        assert(sizeof(HsPtr) == sizeof(intptr_t));
        assert(sizeof(HsPtr) == sizeof(uintptr_t));
        assert(CHAR_BIT == 8);
        assert(EOF == -1);

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




