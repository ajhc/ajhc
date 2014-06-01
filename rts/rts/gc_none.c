#include <stdio.h>
#include <stdlib.h>

#include "rts/gc.h"
#include "rts/profile.h"

#if _JHC_GC == _JHC_GC_BOEHM

void hs_perform_gc(void) {
        GC_gcollect();
}

void jhc_alloc_init(void) { GC_INIT(); }
void jhc_alloc_fini(void) { }

#elif _JHC_GC == _JHC_GC_NONE

// memory allocated in 1MB chunks.
#define JHC_MEM_CHUNK_SIZE (1 << 20)

static char initial_chunk[JHC_MEM_CHUNK_SIZE];

static void *jhc_current_chunk = initial_chunk;
static unsigned mem_chunks,mem_offset;

void jhc_alloc_init(void) {}

void
jhc_alloc_fini(void) {
        if(_JHC_PROFILE) {
                fprintf(stderr, "Memory Allocated: %u bytes\n", (JHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);
                print_alloc_size_stats();
        }
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

#define M_ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))

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

void * A_MALLOC
jhc_malloc_debug(size_t n,int line,int atomic) {
        alloc_count(n,atomic);
        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

void * A_MALLOC
jhc_malloc(size_t n) {
        alloc_count(n,0);
        return jhc_malloc_basic(n);
}

#undef jhc_malloc_atomic
void * A_MALLOC
jhc_malloc_atomic(size_t n) {
        alloc_count(n,1);
        return jhc_malloc_basic(n);
}

#endif

#endif
