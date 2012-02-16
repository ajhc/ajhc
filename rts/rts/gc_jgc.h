#ifndef JHC_GC_JGC_H
#define JHC_GC_JGC_H

#include <stdbool.h>

struct s_arena;
struct s_cache;

#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~ (BLOCK_SIZE - 1)))
#define BLOCK_SIZE     (1UL << 12)
#define MEGABLOCK_SIZE (1UL << 20)

#ifdef JHC_JGC_STACK
struct frame;
typedef struct frame *gc_t;
#else
typedef void* *gc_t;
#endif

extern gc_t saved_gc;

#define GC_BASE sizeof(void *)
#define TO_BLOCKS(x) ((x) <= GC_BASE ? 1 : (((x) - 1)/GC_BASE) + 1)

#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; gc_t sgc = gc;  gc_t gc = sgc + n;

void *s_alloc(gc_t gc, struct s_cache *sc);
struct s_cache *find_cache(struct s_cache **rsc, struct s_arena *arena, unsigned short size, unsigned short num_ptrs);

#endif
