#ifndef JHC_GC_JGC_H
#define JHC_GC_JGC_H

#include <stdbool.h>
#include "sys/queue.h"

struct sptr;
struct s_arena;
struct s_cache;
typedef void* *gc_t;

#define BLOCK_SIZE     (1UL << 12)
#define MEGABLOCK_SIZE (1UL << 20)
#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~(BLOCK_SIZE - 1)))
#define GC_BASE sizeof(void *)
#define TO_BLOCKS(x) ((x) <= GC_BASE ? 1 : (((x) - 1)/GC_BASE) + 1)

extern struct s_arena *arena;
extern gc_t saved_gc;

void *s_alloc(gc_t gc, struct s_cache *sc) A_STD;
void print_cache(struct s_cache *sc);
struct s_cache *new_cache(struct s_arena *arena, unsigned short size,
                          unsigned short num_ptrs);
struct s_arena *new_arena(void);
struct s_cache *find_cache(struct s_cache **rsc, struct s_arena *arena,
                           unsigned short size, unsigned short num_ptrs);
void *(gc_alloc)(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs);
void gc_add_root(gc_t gc, void * root);
void *gc_array_alloc(gc_t gc, unsigned count);

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
