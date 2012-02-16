#ifndef JHC_GC_JGC_H
#define JHC_GC_JGC_H

#include <stdbool.h>

struct s_arena;
struct s_cache;
typedef void* *gc_t;

#define BLOCK_SIZE     (1UL << 12)
#define MEGABLOCK_SIZE (1UL << 20)
#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~ (BLOCK_SIZE - 1)))
#define GC_BASE sizeof(void *)
#define TO_BLOCKS(x) ((x) <= GC_BASE ? 1 : (((x) - 1)/GC_BASE) + 1)

extern struct s_arena *arena;
extern gc_t saved_gc;

void *s_alloc(gc_t gc, struct s_cache *sc) A_STD;
struct s_cache *find_cache(struct s_cache **rsc, struct s_arena *arena,
                           unsigned short size, unsigned short num_ptrs);
void gc_add_root(gc_t gc, void * root);

#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; \
        for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; gc_t sgc = gc;  gc_t gc = sgc + n;

#endif
