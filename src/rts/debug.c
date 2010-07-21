#if JHC_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_MEM_UNDEFINED(x,y) \
    do { } while (0)
#define VALGRIND_MAKE_MEM_DEFINED(x,y) \
    do { } while (0)
#define VALGRIND_MAKE_MEM_NOACCESS(x,y) \
    do { } while (0)
#define VALGRIND_PRINTF(...) \
    do { } while (0)
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
