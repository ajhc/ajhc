
#if   _JHC_BOEHM_GC

#include <gc/gc.h>
#define jhc_malloc GC_malloc
#define jhc_malloc_whnf GC_malloc
#define jhc_malloc_suspension GC_malloc
#define jhc_malloc_atomic GC_malloc_atomic
#define jhc_malloc_atomic_whnf GC_malloc_atomic
#define jhc_free GC_free

static inline void
jhc_malloc_init(void)
{
        GC_INIT();
}

#else

static void *jhc_mem = NULL;
static void *jhc_memstart;


static inline void
jhc_malloc_init(void)
{
        size_t mem_size = 1000000000;
        while(!jhc_mem) {
                jhc_mem = malloc(mem_size);
                mem_size *= 0.80;
        }
        jhc_memstart = jhc_mem;
}


#if _JHC_DEBUG

struct jhcm_header {
        unsigned short line_number;
};


extern void _start;
extern void _end;

static int
jhc_malloc_sanity(void *p,int tag)
{
        assert((p >= jhc_memstart && p < jhc_mem) || (p >= &_start && p < &_end));
        return 1;
}


#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__)

static void * A_MALLOC
jhc_malloc_debug(size_t n,int line)
{
        void *ret = jhc_mem;
        jhc_mem += ALIGN(__alignof__(void *),sizeof(uintptr_t) + n);
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

static inline void * A_MALLOC
jhc_malloc(size_t n)
{
        void *ret = jhc_mem;
        jhc_mem += ALIGN(__alignof__(void *),n);
        return ret;
}

#define jhc_malloc_sanity(p,t) do {} while(0)

#endif

#define jhc_malloc_atomic(x)      jhc_malloc(x)
#define jhc_malloc_atomic_whnf(x) jhc_malloc(x)
#define jhc_malloc_whnf(x)        jhc_malloc(x)
#define jhc_malloc_suspension(x)  jhc_malloc(x)


#endif // USE_BOEHM_GC



