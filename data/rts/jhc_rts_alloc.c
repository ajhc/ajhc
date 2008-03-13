
// some default definitions

#define jhc_malloc_whnf jhc_malloc
#define jhc_malloc_suspension jhc_malloc
#define jhc_malloc_atomic jhc_malloc
#define jhc_malloc_atomic_whnf jhc_malloc_atomic
#define jhc_malloc_sanity(p,t) (1)

extern void _start,_end;


#if _JHC_GC == _JHC_GC_BOEHM

#include <gc/gc.h>

#define jhc_malloc GC_malloc
#undef  jhc_malloc_atomic
#define jhc_malloc_atomic GC_malloc_atomic
#define jhc_free GC_free

static inline void jhc_malloc_init(void) { GC_INIT(); }
static inline void jhc_alloc_print_stats(void) { GC_dump(); }

#elif _JHC_GC == _JHC_GC_NONE

static void *jhc_current_chunk;
static unsigned mem_chunks,mem_offset;

// memory allocated in 1MB chunks.
#define JHC_MEM_CHUNK_SIZE (1 << 20)

static inline void
jhc_malloc_init(void) { return; }

static void
jhc_alloc_print_stats(void) {
        fprintf(stderr, "Memory Allocated: %llu bytes\n", (JHC_MEM_CHUNK_SIZE*((unsigned long long)mem_chunks - 1)) + mem_offset);

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
        n = ALIGN(sizeof(void *),n);
        if (n > (JHC_MEM_CHUNK_SIZE - mem_offset))
                jhc_malloc_grow();
        void *ret = jhc_current_chunk + mem_offset;
        mem_offset += n;
        return ret;
}


#if _JHC_DEBUG

#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__)

static void * A_MALLOC
jhc_malloc_debug(size_t n,int line) {
        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

static inline void * A_MALLOC
jhc_malloc(size_t n) { return jhc_malloc_basic(n); }


#endif

#elif _JHC_GC == _JHC_GC_JGC

#error "jgc not supported yet."

#endif




