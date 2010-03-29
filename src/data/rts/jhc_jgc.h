
#ifdef JHC_RTS_INCLUDE
#undef JHC_RTS_INCLUDE
#include "jhc_jgc.h"
#define JHC_RTS_INCLUDE
#else

#if _JHC_GC == _JHC_GC_JGC

#define JGC_STATUS 0


#ifdef JHC_JGC_STACK

struct frame {
        struct frame *prev;
        unsigned nptrs;
        void *ptrs[0];
};

typedef struct frame *gc_t;

#else

typedef void* *gc_t;

#endif

static gc_t saved_gc;

#ifndef JHC_JGC_STACK
static gc_t gc_stack_base;
#endif


#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))

typedef struct {
        uint8_t count;
        uint8_t nptrs;
        uint16_t tag;
} entry_header_t;



// round all allocations up to this many blocks.
// the underlying malloc implementation has some
// minimum size and this allows memory blocks to
// be reused more often.
#define GC_MINIMUM_SIZE 3
#define GC_BASE sizeof(void *)
#define GC_ALIGNMENT (2*sizeof(void *))

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)



static void gc_perform_gc(gc_t gc);
static void *gc_alloc_tag(gc_t gc,unsigned count, unsigned nptrs, int tag);



#ifdef NDEBUG
#define JUDYERROR_NOTEST 1
#endif

#include <Judy.h>


#if JGC_STATUS > 1
#define debugf(...) fprintf(stderr,__VA_ARGS__)
#else
#define debugf(...) do { } while (0)
#endif



#endif
#endif
