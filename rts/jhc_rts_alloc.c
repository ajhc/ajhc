#define jhc_malloc_whnf jhc_malloc
#define jhc_malloc_suspension jhc_malloc
#define jhc_malloc_atomic jhc_malloc
#define jhc_malloc_atomic_whnf jhc_malloc_atomic
#define jhc_malloc_sanity(p,t) (1)

#ifdef JHC_ALLOC_NEEDS_STUBS
void hs_perform_gc(void) {}
void hs_free_stable_ptr(HsStablePtr sp) {}
void hs_free_fun_ptr(HsFunPtr fp) {}
#endif

#if _JHC_GC == _JHC_GC_BOEHM

#include <gc/gc.h>

#define jhc_malloc GC_malloc
#undef  jhc_malloc_atomic
#define jhc_malloc_atomic GC_malloc_atomic
#define jhc_free GC_free

static void jhc_alloc_init(void) { GC_INIT(); }
static void jhc_alloc_fini(void) { }
static void jhc_alloc_print_stats(void) { }

#elif _JHC_GC == _JHC_GC_NONE

// memory allocated in 1MB chunks.
#define JHC_MEM_CHUNK_SIZE (1 << 20)

static char initial_chunk[JHC_MEM_CHUNK_SIZE];

static void *jhc_current_chunk = initial_chunk;
static unsigned mem_chunks,mem_offset;

static void jhc_alloc_init(void) {}
static void jhc_alloc_fini(void) {}

static void
jhc_alloc_print_stats(void) {
        fprintf(stderr, "Memory Allocated: %u bytes\n", (JHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);
        print_alloc_size_stats();
}

static void
jhc_malloc_grow(void) {
        void *c = malloc(JHC_MEM_CHUNK_SIZE);
        if(!c) {
                fputs("Out of memory!\n",stderr);
                abort();
        }
        mem_chunks++;
        jhc_current_chunk = c;
        mem_offset = 0;
}

static inline void * A_MALLOC
jhc_malloc_basic(size_t n) {
        n = M_ALIGN(sizeof(void *),n);
        if (n > (JHC_MEM_CHUNK_SIZE - mem_offset))
                jhc_malloc_grow();
        void *ret = jhc_current_chunk + mem_offset;
        mem_offset += n;
        return ret;
}

#if _JHC_DEBUG

#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__,0)
#undef jhc_malloc_atomic
#define jhc_malloc_atomic(n) jhc_malloc_debug(n,__LINE__,1)

static void * A_MALLOC
jhc_malloc_debug(size_t n,int line,int atomic) {
        alloc_count(n,atomic);
        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

static inline void * A_MALLOC
jhc_malloc(size_t n) {
        alloc_count(n,0);
        return jhc_malloc_basic(n);
}

#undef jhc_malloc_atomic
static inline void * A_MALLOC
jhc_malloc_atomic(size_t n) {
        alloc_count(n,1);
        return jhc_malloc_basic(n);
}

#endif

#endif
