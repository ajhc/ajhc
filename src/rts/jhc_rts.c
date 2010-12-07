static jmp_buf jhc_uncaught;

#ifdef __WIN32__
A_UNUSED static char *jhc_options_os =  "mingw32";
A_UNUSED static char *jhc_options_arch = "i386";
#else
A_UNUSED struct utsname jhc_utsname;
A_UNUSED static char *jhc_options_os = "(unknown os)";
A_UNUSED static char *jhc_options_arch = "(unknown arch)";
#endif


static void A_NORETURN A_UNUSED A_COLD
jhc_exit(int n) {
        fflush(stdout);
        jhc_print_profile();
        exit(n);
}

static void  A_NORETURN A_UNUSED  A_COLD
jhc_error(char *s) {
        fflush(stdout);
        fputs(s,stderr);
        fputs("\n",stderr);
        jhc_exit(1);
}

static void  A_NORETURN A_UNUSED  A_COLD
jhc_case_fell_off(int n) {
        fflush(stdout);
        fprintf(stderr, "\n%s:%i: case fell off\n", __FILE__, n);
        abort();
}

#define jhc_setjmp(jb) setjmp(*(jb))
#define jhc_longjmp(jb) longjmp(*(jb),1)

struct jhc_continuation {
    void *argument;
    jmp_buf jump_buf;
};

#define prim_umaxbound(t) ((t)~((t)0))
#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*8 - 1))))
#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*8 - 1))))

void
hs_set_argv(int argc, char *argv[])
{
        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
}

static int hs_init_count;

static void jhc_hs_init(void);

void
hs_init(int *argc, char **argv[])
{

        if(!hs_init_count++) {
                /* A few random assertions about the architecture that the compiler
                 * assumes. should be true of any but the oddest of beasts. */
                assert(sizeof(HsPtr) == sizeof(HsFunPtr));
                assert(sizeof(HsPtr) == sizeof(intptr_t));
                assert(sizeof(HsPtr) == sizeof(uintptr_t));
                assert(CHAR_BIT == 8);
                assert(EOF == -1);

                jhc_alloc_init();
                jhc_hs_init();
                hs_set_argv(*argc,*argv);
#if JHC_isPosix
                if(!uname(&jhc_utsname)) {
                        jhc_options_arch = jhc_utsname.machine;
                        jhc_options_os   = jhc_utsname.sysname;
                }
#endif
                setlocale(LC_ALL,"");
        }
}

void
hs_exit(void)
{
        if(!hs_init_count) {
                fprintf(stderr, "hs_exit() called before hs_init()\n");
                abort();
        }
        if(!--hs_init_count) {
                jhc_alloc_fini();
                jhc_exit(0);
        }
}

#if _JHC_STANDALONE
int
main(int argc, char *argv[])
{
        hs_init(&argc,&argv);
        if (jhc_setjmp(&jhc_uncaught))
                jhc_error("Uncaught Exception");
        else
                _amain();
        hs_exit();
        return 0;
}
#endif

