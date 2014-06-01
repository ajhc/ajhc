#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

#include "HsFFI.h"
#include "rts/gc.h"
#include "rts/profile.h"
#include "rts/rts_support.h"

jmp_buf jhc_uncaught;
int jhc_argc;
char **jhc_argv;
char *jhc_progname;

#ifdef __WIN32__
A_UNUSED char *jhc_options_os =  "mingw32";
A_UNUSED char *jhc_options_arch = "i386";
#elif defined(__ARM_EABI__)
A_UNUSED char *jhc_options_os =  "nds";
A_UNUSED char *jhc_options_arch = "ARM";
#else
A_UNUSED char *jhc_options_os = "(unknown os)";
A_UNUSED char *jhc_options_arch = "(unknown arch)";
#endif

void
hs_set_argv(int argc, char *argv[])
{
        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
}

void A_NORETURN A_UNUSED A_COLD
jhc_exit(int n) {
        fflush(stdout);
        jhc_print_profile();
        exit(n);
}

void  A_NORETURN A_UNUSED  A_COLD
jhc_error(char *s) {
        fflush(stdout);
        fputs(s,stderr);
        fputs("\n",stderr);
        jhc_exit(1);
}

void  A_NORETURN A_UNUSED  A_COLD
jhc_case_fell_off(int n) {
        fflush(stdout);
        fprintf(stderr, "\n%s:%i: case fell off\n", __FILE__, n);
        abort();
}

void jhc_hs_init(void);

static int hs_init_count;
void
hs_init(int *argc, char **argv[])
{

        if(!hs_init_count++) {
                jhc_alloc_init();
                jhc_hs_init();
                hs_set_argv(*argc,*argv);
#if JHC_isPosix
                struct utsname jhc_utsname;
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
