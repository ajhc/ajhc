#ifndef JHC_GC_JGC_H
#define JHC_GC_JGC_H

#include <stdbool.h>
#include <stdint.h>
#include "sys/queue.h"
#include "HsFFI.h"

struct sptr;
struct s_arena;
struct s_cache;
typedef struct s_arena *arena_t;
typedef void* *gc_t;
typedef void* heap_t;  // a pointer into the GCed heap.

#if defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT)
#if (_JHC_JGC_BLOCK_SHIFT) >= (_JHC_JGC_MEGABLOCK_SHIFT)
#error "_JHC_JGC_MEGABLOCK_SHIFT should be larger than _JHC_JGC_BLOCK_SHIFT."
#endif
#elif defined(_JHC_JGC_BLOCK_SHIFT) || defined(_JHC_JGC_MEGABLOCK_SHIFT)
#error "Should define both _JHC_JGC_BLOCK_SHIFT and _JHC_JGC_MEGABLOCK_SHIFT."
#else
#define _JHC_JGC_BLOCK_SHIFT     12
#define _JHC_JGC_MEGABLOCK_SHIFT 20
#endif /* defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT) */

#if !defined(_JHC_JGC_GC_STACK_SHIFT)
#define _JHC_JGC_GC_STACK_SHIFT  18
#endif /* !defined(_JHC_JGC_BLOCK_SHIFT) */

#define BLOCK_SIZE     (1UL << (_JHC_JGC_BLOCK_SHIFT))
#define MEGABLOCK_SIZE (1UL << (_JHC_JGC_MEGABLOCK_SHIFT))
#define GC_STACK_SIZE  (1UL << (_JHC_JGC_GC_STACK_SHIFT))
#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~(BLOCK_SIZE - 1)))
#define TO_BLOCKS(x) (((x) + sizeof(uintptr_t) - 1)/sizeof(uintptr_t))

extern arena_t saved_arena;
extern gc_t saved_gc;

void print_cache(struct s_cache *sc);
struct s_cache *new_cache(arena_t arena, unsigned short size,
                          unsigned short num_ptrs);
arena_t new_arena(void);
struct s_cache *find_cache(struct s_cache **rsc, arena_t arena,
                           unsigned short size, unsigned short num_ptrs);
void alloc_public_caches(arena_t arena, size_t size);
struct s_caches_pub *public_caches(arena_t arena);
void gc_add_root(gc_t gc, arena_t arena, void * root);
void A_STD gc_perform_gc(gc_t gc, arena_t arena);
uint32_t get_heap_flags(void* sp);

heap_t s_alloc(gc_t gc, arena_t arena, struct s_cache *sc) A_STD;
heap_t (gc_alloc)(gc_t gc, arena_t arena,struct s_cache **sc, unsigned count, unsigned nptrs) A_STD;
heap_t gc_array_alloc(gc_t gc, arena_t arena, unsigned count) A_STD;
heap_t gc_array_alloc_atomic(gc_t gc, arena_t arena, unsigned count, unsigned slab_flags) A_STD;
/* foreignptr, saved_gc must be set properly. */
heap_t gc_malloc_foreignptr(gc_t gc, arena_t arena, unsigned alignment, unsigned size, bool finalizer) A_STD;
heap_t gc_new_foreignptr(gc_t gc, arena_t arena, HsPtr ptr) A_STD;
bool gc_add_foreignptr_finalizer(struct sptr* fp, HsFunPtr finalizer) A_STD;

#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; \
        for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; \
        gc_t sgc = gc;  gc_t gc = sgc + n;
#define gc_frame1(gc,p1) gc[0] = (sptr_t)p1; gc_t sgc = gc;  gc_t gc = sgc + 1;
#define gc_frame2(gc,p1,p2) gc[0] = (sptr_t)p1; gc[1] = (sptr_t)p2; \
                                    gc_t sgc = gc;  gc_t gc = sgc + 2;

struct StablePtr {
    LIST_ENTRY(StablePtr) link;
    struct sptr* contents;
};

extern LIST_HEAD(StablePtr_list, StablePtr) root_StablePtrs;

#endif
