static void A_UNUSED
profile_print_header(FILE *file, char *value_unit)
{
        fprintf(file, "JOB \"%s", jhc_progname);
        for(int i = 0; i < jhc_argc; i++)
                fprintf(file, " %s", jhc_argv[i]);
        fprintf(file, "\"\n");
        fprintf(file, "DATE \"%s\"\n", ctime(NULL));
        fprintf(file, "SAMPLE_UNIT \"seconds\"\n");
        fprintf(file, "VALUE_UNIT \"%s\"\n", value_unit ? value_unit : "bytes");
}

#if _JHC_PROFILE

struct profile_stack {
    struct tms tm_total;
    struct tms tm_pushed;
};

struct profile_stack gc_alloc_time;
struct profile_stack gc_gc_time;


void
profile_push(struct profile_stack *ps)
{
        times(&ps->tm_pushed);
}

void
profile_pop(struct profile_stack *ps)
{
    struct tms tm;
    times(&tm);
    ps->tm_total.tms_utime += tm.tms_utime - ps->tm_pushed.tms_utime;
    ps->tm_total.tms_stime += tm.tms_stime - ps->tm_pushed.tms_stime;
}


#else

#define profile_push(x) do { } while(0)
#define profile_pop(x) do { } while(0)

#endif

#ifndef __WIN32__
#ifndef __ARM_EABI__
void print_times(struct tms *tm) {
    float cpt = (float)sysconf(_SC_CLK_TCK);
    fprintf(stderr, "User Time:   %.2fs\n", (float)tm->tms_utime/cpt);
    fprintf(stderr, "System Time: %.2fs\n", (float)tm->tms_stime/cpt);
    fprintf(stderr, "Total Time:  %.2fs\n", (float)(tm->tms_stime + tm->tms_utime)/cpt);
}
#endif
#endif

static void A_COLD
jhc_print_profile(void) {
#ifndef __WIN32__
        struct tms tm;
        times(&tm);
#endif
        if(!(_JHC_PROFILE || getenv("JHC_RTS_PROFILE"))) return;

        fprintf(stderr, "\n-----------------\n");
        fprintf(stderr, "Profiling: %s\n", jhc_progname);
        fprintf(stderr, "Command: %s\n", jhc_command);
        fprintf(stderr, "Complie: %s\n", jhc_c_compile);
        fprintf(stderr, "Version: %s\n\n", jhc_version);
        jhc_alloc_print_stats();
#ifndef __WIN32__
#ifndef __ARM_EABI__
        print_times(&tm);
#if _JHC_PROFILE
        print_times(&gc_gc_time.tm_total);
        print_times(&gc_alloc_time.tm_total);
#endif
#endif
#endif
        fprintf(stderr, "-----------------\n");
}
