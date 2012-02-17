// profiling and debugging code.

#if defined(__WIN32__)
#define HAVE_TIMES 0
#else
#define HAVE_TIMES 1
#endif

#include <stdio.h>
#include <stdlib.h>
#if HAVE_TIMES
#include <sys/times.h>
#include <time.h>
#endif
#include <unistd.h>

#include "jhc_rts_header.h"

void A_UNUSED
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

#if HAVE_TIMES
struct profile_stack {
    struct tms tm_total;
    struct tms tm_pushed;
};

struct profile_stack gc_alloc_time;
struct profile_stack gc_gc_time;

void
jhc_profile_push(struct profile_stack *ps)
{
        times(&ps->tm_pushed);
}

void
jhc_profile_pop(struct profile_stack *ps)
{
    struct tms tm;
    times(&tm);
    ps->tm_total.tms_utime += tm.tms_utime - ps->tm_pushed.tms_utime;
    ps->tm_total.tms_stime += tm.tms_stime - ps->tm_pushed.tms_stime;
}

void print_times(struct tms *tm) {
#if  !defined(__WIN32__) && !defined(__ARM_EABI__)
    float cpt = (float)sysconf(_SC_CLK_TCK);
    fprintf(stderr, "User Time:   %.2fs\n", (float)tm->tms_utime/cpt);
    fprintf(stderr, "System Time: %.2fs\n", (float)tm->tms_stime/cpt);
    fprintf(stderr, "Total Time:  %.2fs\n", (float)(tm->tms_stime + tm->tms_utime)/cpt);
#endif
    return;
}
#else

struct profile_stack;
void jhc_profile_push(struct profile_stack *ps) {}
void jhc_profile_pop(struct profile_stack *ps) {}

#endif

void A_COLD
jhc_print_profile(void) {
        if(!(_JHC_PROFILE || getenv("JHC_RTS_PROFILE"))) return;
        fprintf(stderr, "\n-----------------\n");
        fprintf(stderr, "Profiling: %s\n", jhc_progname);
        fprintf(stderr, "Command: %s\n", jhc_command);
        fprintf(stderr, "Complie: %s\n", jhc_c_compile);
        fprintf(stderr, "Version: %s\n\n", jhc_version);
        jhc_alloc_print_stats();
#if HAVE_TIMES
        struct tms tm;
        times(&tm);
        print_times(&tm);
#endif
#if _JHC_PROFILE
        print_times(&gc_gc_time.tm_total);
        print_times(&gc_alloc_time.tm_total);
#endif
        fprintf(stderr, "-----------------\n");
}

#if _JHC_PROFILE

#define BUCKETS 7
static unsigned alloced[BUCKETS];
static unsigned alloced_atomic[BUCKETS];

static void
alloc_count(int n,int atomic)
{
        n = n ? ((n - 1)/sizeof(void *)) + 1 : 0;
        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;
        (atomic ? alloced_atomic : alloced)[n]++;
}

static void
print_alloc_size_stats(void) {
        char fmt[] = "%10s %10s %10s %10s %10s\n";
        char fmt2[] = "%10u %10u %10u %10u %10u\n";
        fprintf(stderr,fmt,"Size","Normal","Atomic","Total","Accum");
        fprintf(stderr,fmt,"----","------","------","-----","-----");
        unsigned accum = 0;
        for(int i = 0; i < BUCKETS; i++) {
                accum += alloced[i] + alloced_atomic[i];
                fprintf(stderr,fmt2,i,alloced[i],alloced_atomic[i],alloced_atomic[i] + alloced[i], accum);
        }
}
#endif

#if JHC_MEM_ANNOTATE && _JHC_GC == _JHC_GC_JGC
#include <Judy.h>

static Pvoid_t mem_annotate = NULL;

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define gc_alloc(gc,sc,c,nptrs) \
    gc_alloc_annot(gc,sc,c,nptrs,(__FILE__ ":" STR(__LINE__)))

A_UNUSED static void *
gc_alloc_annot(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs, char *str)
{
        void *ret = (gc_alloc)(gc,sc,count,nptrs);
        PWord_t pval;
        JLI(pval,mem_annotate,(Word_t)ret);
        *pval = (Word_t)str;
        return ret;
}

char *
gc_lookup(void *ptr)
{
        PWord_t pval;
        JLG(pval,mem_annotate,(Word_t)ptr & ~(Word_t)3);
        return pval ? (char *)*pval : "(none)";
}

#endif

#if _JHC_DEBUG  && _JHC_GC == _JHC_GC_JGC

// these ensure the type synonyms are available to the debugger
uintptr_t _dummy1;
node_t *_dummy2;
dnode_t *_dummy3;
sptr_t *_dummy4;
fptr_t *_dummy5;
wptr_t *_dummy6;

bool A_UNUSED
jhc_valid_whnf(wptr_t s)
{
        return ((GET_PTYPE(s) == P_VALUE) || ((GET_PTYPE(s) == P_WHNF) && jhc_malloc_sanity(s,P_WHNF)));
}

bool A_UNUSED
jhc_valid_lazy(sptr_t s)
{
        if(jhc_valid_whnf((wptr_t)s))
                return true;
        assert(GET_PTYPE(s) == P_LAZY);
        node_t *ds = (node_t *)FROM_SPTR(s);
        assert(jhc_malloc_sanity(ds,P_LAZY));
        if(IS_LAZY(ds->head)) {
                if(ds->head == BLACK_HOLE) return true;
                assert(GET_PTYPE(ds->head) == P_FUNC);
                return true;
        } else
                return jhc_valid_whnf((wptr_t)ds->head);
}

#endif

#if _JHC_DEBUG
wptr_t A_STD
promote(sptr_t s)
{
        assert(!IS_LAZY(s));
        assert(jhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}

sptr_t A_STD
demote(wptr_t s)
{
        assert(!IS_LAZY(s));
        assert(jhc_valid_whnf(s));
        return (sptr_t)s;
}

void A_STD
update(void * thunk, wptr_t new)
{
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!IS_LAZY(new));
        GETHEAD(thunk) = (fptr_t)new;
}

#endif
