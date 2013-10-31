#include "jhc_rts_header.h"
#include "sys/queue.h"
#include "sys/bitarray.h"
#include "rts/cdefs.h"
#include "rts/constants.h"
#include "rts/gc_jgc_internal.h"
#include "rts/conc.h"

#if _JHC_GC == _JHC_GC_JGC

#if defined(_JHC_JGC_LIMITED_NUM_MEGABLOCK)
static char aligned_megablock[(MEGABLOCK_SIZE)*(_JHC_JGC_LIMITED_NUM_MEGABLOCK)] __attribute__ ((aligned(BLOCK_SIZE)));
#endif
#if defined(_JHC_JGC_LIMITED_NUM_GC_STACK)
static char gc_stack_base_area[(GC_STACK_SIZE)*sizeof(gc_t)*(_JHC_JGC_LIMITED_NUM_GC_STACK)];
#endif
SLIST_HEAD(,s_arena) used_arenas;
SLIST_HEAD(,s_arena) free_arenas;
SLIST_HEAD(,s_megablock) free_megablocks;
SLIST_HEAD(,s_block) free_monolithic_blocks;

#define TO_GCPTR(x) (entry_t *)(FROM_SPTR(x))

void gc_perform_gc(gc_t gc, arena_t arena) A_STD;
static bool s_set_used_bit(void *val) A_UNUSED;
static void clear_used_bits(arena_t arena) A_UNUSED;
static void s_cleanup_blocks(arena_t arena);
static struct s_block *get_free_block(gc_t gc, arena_t arena, bool retry);
static void *jhc_aligned_alloc(unsigned size);

static const void *nh_start, *nh_end;

static bool
gc_check_heap(entry_t *s)
{
        return (s < (entry_t *)nh_start || s > (entry_t *)nh_end);
}

static void
stack_grow(struct stack *s, unsigned grow)
{
        s->size += grow;
        s->stack = realloc(s->stack, sizeof(s->stack[0])*s->size);
        assert(s->stack);
        debugf("stack:");
        for(unsigned i = 0; i < s->ptr; i++) {
                debugf(" %p", (void *)s->stack[i]);
        }
        debugf("\n");
}

inline static void
stack_check(struct stack *s, unsigned n) {
        if(__predict_false(s->size - s->ptr < n)) {
#ifndef _JHC_JGC_STACKGROW
#define _JHC_JGC_STACKGROW (1024)
#endif
                stack_grow(s,n + (_JHC_JGC_STACKGROW));
        }
}

void gc_add_root(gc_t gc, arena_t arena, void *root)
{
        if(IS_PTR(root)) {
                entry_t *nroot = TO_GCPTR(root);
                if(gc_check_heap(nroot)) {
                        stack_check(&arena->root_stack,1);
                        arena->root_stack.stack[arena->root_stack.ptr++] = nroot;
                }
        }
}

static void
gc_add_grey(struct stack *stack, entry_t *s)
{
        VALGRIND_MAKE_MEM_DEFINED(s,(S_BLOCK(s))->u.pi.size * sizeof(uintptr_t));
        if(gc_check_heap(s) && s_set_used_bit(s))
                stack->stack[stack->ptr++] = s;
}

static void
gc_mark_deeper(struct stack *stack, unsigned *number_redirects)
{
        while(stack->ptr) {
                entry_t *e = stack->stack[--stack->ptr];
                struct s_block *pg = S_BLOCK(e);
                if(!(pg->flags & SLAB_MONOLITH))
                        VALGRIND_MAKE_MEM_DEFINED(e,pg->u.pi.size * sizeof(uintptr_t));
                debugf("Processing Grey: %p\n",e);
                unsigned num_ptrs = pg->flags & SLAB_MONOLITH ? pg->u.m.num_ptrs : pg->u.pi.num_ptrs;
                stack_check(stack, num_ptrs);
                for(unsigned i = 0; i < num_ptrs; i++) {
                        if(1 && (P_LAZY == GET_PTYPE(e->ptrs[i]))) {
                                VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(e->ptrs[i]), sizeof(uintptr_t));
                                if(!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {
                                        *number_redirects++;
                                        debugf(" *");
                                        e->ptrs[i] = (sptr_t)GETHEAD(FROM_SPTR(e->ptrs[i]));
                                }
                        }
                        if(IS_PTR(e->ptrs[i])) {
                                entry_t * ptr = TO_GCPTR(e->ptrs[i]);
                                debugf("Following: %p %p\n",e->ptrs[i], ptr);
                                gc_add_grey(stack, ptr);
                        }
                }
        }
}

#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)
#define DO_GC_MARK_DEEPER(S,N)  gc_mark_deeper((S),(N))
#else
#define DO_GC_MARK_DEEPER(S,N)  do { } while (/* CONSTCOND */ 0)
#endif

void A_STD
gc_perform_gc(gc_t gc, arena_t arena)
{
        profile_push(&gc_gc_time);
        arena->number_gcs++;

        unsigned number_redirects = 0;
        unsigned number_stack = 0;
        unsigned number_ptr = 0;
        struct stack stack = EMPTY_STACK;

        clear_used_bits(arena);

        debugf("Setting Roots:");
        stack_check(&stack, arena->root_stack.ptr);
        for(unsigned i = 0; i < arena->root_stack.ptr; i++) {
                gc_add_grey(&stack, arena->root_stack.stack[i]);
                debugf(" %p", arena->root_stack.stack[i]);
                DO_GC_MARK_DEEPER(&stack, &number_redirects);
        }
        debugf(" # ");
        struct StablePtr *sp;

        jhc_rts_lock();
        LIST_FOREACH(sp, &root_StablePtrs, link) {
            gc_add_grey(&stack, (entry_t *)sp);
            debugf(" %p", sp);
            DO_GC_MARK_DEEPER(&stack, &number_redirects);
        }
        jhc_rts_unlock();

        debugf("\n");
        debugf("Trace:");
#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)
        stack_check(&stack, 1); // Just alloc
#else
        stack_check(&stack, gc - arena->gc_stack_base);
#endif
        number_stack = gc - arena->gc_stack_base;
        for(unsigned i = 0; i < number_stack; i++) {
                debugf(" |");
                // TODO - short circuit redirects on stack
                sptr_t ptr = arena->gc_stack_base[i];
                if(1 && (IS_LAZY(ptr))) {
                        assert(GET_PTYPE(ptr) == P_LAZY);
                        VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(ptr), sizeof(uintptr_t));
                        if(!IS_LAZY(GETHEAD(FROM_SPTR(ptr)))) {
                                void *gptr = TO_GCPTR(ptr);
                                if(gc_check_heap(gptr))
                                        s_set_used_bit(gptr);
                                number_redirects++;
                                debugf(" *");
                                ptr = (sptr_t)GETHEAD(FROM_SPTR(ptr));
                        }
                }
                if(__predict_false(!IS_PTR(ptr))) {
                        debugf(" -");
                        continue;
                }
                number_ptr++;
                entry_t *e = TO_GCPTR(ptr);
                debugf(" %p",(void *)e);
                gc_add_grey(&stack, e);
                DO_GC_MARK_DEEPER(&stack, &number_redirects);
        }
        debugf("\n");

        gc_mark_deeper(&stack, &number_redirects); // Final marking
        free(stack.stack);
        s_cleanup_blocks(arena);
        if (JHC_STATUS) {
                jhc_printf_stderr("%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\n",
                        arena->number_gcs,
                        arena->number_allocs,
                        (unsigned)arena->block_used,
                        (unsigned)arena->block_threshold,
                        number_stack,
                        number_ptr,
                        number_redirects,
                        (unsigned)arena->root_stack.ptr
                       );
                arena->number_allocs = 0;
        }
        profile_pop(&gc_gc_time);
}

/* Enter with rts_lock. */
static gc_t
new_gc_stack(arena_t arena) {
        if (!arena->gc_stack_base) {
#if defined(_JHC_JGC_LIMITED_NUM_GC_STACK)
                static int count = 0;
                if (count >= _JHC_JGC_LIMITED_NUM_GC_STACK) {
                        abort();
                }
                arena->gc_stack_base = (void *) (gc_stack_base_area +
                    (GC_STACK_SIZE) * sizeof(gc_t) * count);
                count++;
#else
                arena->gc_stack_base = malloc((GC_STACK_SIZE)*sizeof(arena->gc_stack_base[0]));
#endif
        }
        return arena->gc_stack_base;
}

static int jhc_alloc_init_count = 0;
void
jhc_alloc_init(gc_t *gc_p,arena_t *arena_p) {
        VALGRIND_PRINTF("Jhc-Valgrind mode active.\n");

        jhc_rts_lock();
        if (!jhc_alloc_init_count++) {
                SLIST_INIT(&used_arenas);
                SLIST_INIT(&free_arenas);
                SLIST_INIT(&free_megablocks);
                SLIST_INIT(&free_monolithic_blocks);
                if(nh_stuff[0]) {
                        nh_end = nh_start = nh_stuff[0];
                        for(int i = 1; nh_stuff[i]; i++) {
                                if(nh_stuff[i] < nh_start)
                                        nh_start = nh_stuff[i];
                                if(nh_stuff[i] > nh_end)
                                        nh_end = nh_stuff[i];
                        }
                }
        }
        *arena_p = new_arena();
        SLIST_INSERT_HEAD(&used_arenas, *arena_p, link);
        *gc_p = new_gc_stack(*arena_p);
        jhc_rts_unlock();
}

void
jhc_alloc_fini(gc_t gc,arena_t arena) {
        struct s_block *pg;
        struct s_megablock *mb;
        struct s_cache *sc;

        if(_JHC_PROFILE || JHC_STATUS) {
                jhc_printf_stderr("arena: %p\n", arena);
                jhc_printf_stderr("  block_used: %i\n", arena->block_used);
                jhc_printf_stderr("  block_threshold: %i\n", arena->block_threshold);
                struct s_cache *sc;
                SLIST_FOREACH(sc,&arena->caches,next)
                        print_cache(sc);
        }

        jhc_rts_lock();
        SLIST_FOREACH(pg, &arena->monolithic_blocks, link) {
                SLIST_INSERT_HEAD(&free_monolithic_blocks, pg, link);
        }
        SLIST_FOREACH(mb, &arena->megablocks, next) {
                SLIST_INSERT_HEAD(&free_megablocks, mb, next);
        }
        if(arena->current_megablock) {
                SLIST_INSERT_HEAD(&free_megablocks, arena->current_megablock, next);
        }

        SLIST_FOREACH(sc, &arena->caches, next) {
                SLIST_INIT(&sc->blocks);
                SLIST_INIT(&sc->full_blocks);
#if _JHC_PROFILE
                sc->allocations = 0;
#endif
        }

        SLIST_REMOVE(&used_arenas, arena, s_arena, link);
        SLIST_INSERT_HEAD(&free_arenas, arena, link);
        jhc_rts_unlock();
}

heap_t A_STD
(gc_alloc)(gc_t gc, arena_t arena, struct s_cache **sc, unsigned count, unsigned nptrs)
{
        assert(nptrs <= count);
        entry_t *e = s_alloc(gc, arena, find_cache(sc, arena, count, nptrs));
        VALGRIND_MAKE_MEM_UNDEFINED(e,sizeof(uintptr_t)*count);
        debugf("gc_alloc: %p %i %i\n",(void *)e, count, nptrs);
        return (void *)e;
}

static heap_t A_STD
s_monoblock(arena_t arena, unsigned size, unsigned nptrs, unsigned flags) {
        jhc_rts_lock();
        struct s_block *b = SLIST_FIRST(&free_monolithic_blocks);
        if (b) {
                SLIST_REMOVE(&free_monolithic_blocks, b, s_block, link);
        } else {
                b = jhc_aligned_alloc(size * sizeof(uintptr_t));
        }
        jhc_rts_unlock();

        b->flags = flags | SLAB_MONOLITH;
        b->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(1) +
                    sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
        b->u.m.num_ptrs = nptrs;
        SLIST_INSERT_HEAD(&arena->monolithic_blocks, b, link);
        b->used[0] = 1;
        return (void *)b + b->color*sizeof(uintptr_t);
}

// Allocate an array of count garbage collectable locations in the garbage
// collected heap.
heap_t A_STD
gc_array_alloc(gc_t gc, arena_t arena, unsigned count)
{
        if (!count)
               return NULL;
        if (count <= GC_STATIC_ARRAY_NUM)
                return (wptr_t)s_alloc(gc, arena, arena->array_caches[count - 1]);
        if (count < GC_MAX_BLOCK_ENTRIES)
                return s_alloc(gc, arena, find_cache(NULL, arena, count, count));
        return s_monoblock(arena, count, count, 0);
        abort();
}

// Allocate an array of count non-garbage collectable locations in the garbage
// collected heap.
heap_t A_STD
gc_array_alloc_atomic(gc_t gc, arena_t arena, unsigned count, unsigned flags)
{
        if (!count)
               return NULL;
        if (count <= GC_STATIC_ARRAY_NUM && !flags)
                return (wptr_t)s_alloc(gc, arena, arena->array_caches_atomic[count - 1]);
        if (count < GC_MAX_BLOCK_ENTRIES && !flags)
                return s_alloc(gc, arena, find_cache(NULL, arena, count, 0));
        return s_monoblock(arena, count, count, flags);
        abort();
}

/* This finds a bit that isn't set, sets it, then returns its index.  It
 * assumes that a bit is available to be found, otherwise it goes into an
 * infinite loop. */

static unsigned
bitset_find_free(unsigned *next_free,int n,bitarray_t ba[static n]) {
        assert(*next_free < (unsigned)n);
        unsigned i = *next_free;
        do {
                int o = __builtin_ffsl(~ba[i]);
                if(__predict_true(o)) {
                        ba[i] |= (1UL << (o - 1));
                        *next_free = i;
                        return (i*BITS_PER_UNIT + (o - 1));
                }
                i = (i + 1) % n;
                assert(i != *next_free);
        } while (1);
}

static void *
jhc_aligned_alloc(unsigned size) {
        void *base;
#if defined(__WIN32__)
        base = _aligned_malloc(MEGABLOCK_SIZE, BLOCK_SIZE);
        int ret = !base;
#elif defined(__ARM_EABI__)
        base = memalign(BLOCK_SIZE, MEGABLOCK_SIZE);
        int ret = !base;
#elif (defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ <  1060)
        assert(sysconf(_SC_PAGESIZE) == BLOCK_SIZE);
        base = valloc(MEGABLOCK_SIZE);
        int ret = !base;
#else
        int ret = posix_memalign(&base,BLOCK_SIZE,MEGABLOCK_SIZE);
#endif
        if(ret != 0) {
                jhc_printf_stderr("Unable to allocate memory for aligned alloc: %u\n", size);
                abort();
        }
        return base;
}

struct s_megablock *
s_new_megablock(arena_t arena)
{
        jhc_rts_lock();
        struct s_megablock *mb = SLIST_FIRST(&free_megablocks);
        if (mb) {
                SLIST_REMOVE(&free_megablocks, mb, s_megablock, next);
        } else {
                mb = malloc(sizeof(*mb));
#ifdef _JHC_JGC_LIMITED_NUM_MEGABLOCK
                static int count = 0;
                if (count >= _JHC_JGC_LIMITED_NUM_MEGABLOCK) {
                        abort();
                }
                mb->base = aligned_megablock + (MEGABLOCK_SIZE) * count;
                count++;
#else
                mb->base = jhc_aligned_alloc(MEGABLOCK_SIZE);
#endif
        }
        jhc_rts_unlock();

        VALGRIND_MAKE_MEM_NOACCESS(mb->base,MEGABLOCK_SIZE);
        mb->next_free = 0;
        return mb;
}

/* block allocator */

static struct s_block *
get_free_block(gc_t gc, arena_t arena, bool retry) {
        arena->block_used++;
        if(__predict_true(SLIST_FIRST(&arena->free_blocks))) {
                struct s_block *pg = SLIST_FIRST(&arena->free_blocks);
                SLIST_REMOVE_HEAD(&arena->free_blocks,link);
                return pg;
        } else {
#ifdef _JHC_JGC_NAIVEGC
                if(retry == false) {
                        gc_perform_gc(gc, arena);
                        return NULL;
                }
#else
                if((arena->block_used >= arena->block_threshold)) {
                        gc_perform_gc(gc, arena);
                        // if we are still using 80% of the heap after a gc, raise the threshold.
                        if(__predict_false((unsigned)arena->block_used * 10 >= arena->block_threshold * 9)) {
                                arena->block_threshold *= 2;
                        }
                }
#endif
                if(__predict_false(!arena->current_megablock))
                        arena->current_megablock = s_new_megablock(arena);
                struct s_megablock *mb = arena->current_megablock;
                struct s_block *pg = mb->base + BLOCK_SIZE*mb->next_free;
                mb->next_free++;
                if(mb->next_free == MEGABLOCK_SIZE / BLOCK_SIZE) {
                        SLIST_INSERT_HEAD(&arena->megablocks,mb, next);
                        arena->current_megablock = NULL;
                }
                VALGRIND_MAKE_MEM_UNDEFINED(pg,sizeof(struct s_block));
                pg->u.pi.num_free = 0;
                return pg;
        }
}

typedef void (*finalizer_ptr)(HsPtr arg);
typedef void (*finalizer_env_ptr)(HsPtr env, HsPtr arg);

void hs_foreignptr_env_helper(HsPtr env, HsPtr arg) {
        ((finalizer_ptr)env)(arg);
}

static void
s_cleanup_blocks(arena_t arena) {
        struct s_block *pg = SLIST_FIRST(&arena->monolithic_blocks);
        SLIST_INIT(&arena->monolithic_blocks);
        while (pg) {
                if (pg->used[0]) {
                        SLIST_INSERT_HEAD(&arena->monolithic_blocks, pg, link);
                        pg = SLIST_NEXT(pg,link);
                } else {
                        if (pg->flags & SLAB_FLAG_FINALIZER) {
                                HsPtr *ptr = (HsPtr *)pg;
                                if(ptr[pg->color + 1]) {
                                        finalizer_ptr *fp = ptr[pg->color + 1];
                                        do {
                                                fp[0](ptr[pg->color]);
                                        } while(*++fp);
                                }
                        }
                        void *ptr = pg;
                        pg = SLIST_NEXT(pg,link);
                        free(ptr);
                }
        }
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                // 'best' keeps track of the block with the fewest free spots
                // and percolates it to the front, effectively a single pass
                // of a bubblesort to help combat fragmentation. It does
                // not increase the complexity of the cleanup algorithm as
                // we had to scan every block anyway, but over many passes
                // of the GC it will eventually result in a more sorted list
                // than would occur by chance.

                struct s_block *best = NULL;
                int free_best = 4096;
                pg = SLIST_FIRST(&sc->blocks);
                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);
                SLIST_INIT(&sc->blocks);
                SLIST_INIT(&sc->full_blocks);
                if(!pg) {
                        pg = fpg;
                        fpg = NULL;
                }
                while(pg) {
                        struct s_block *npg = SLIST_NEXT(pg,link);
                        if(__predict_false(pg->u.pi.num_free == 0)) {
                                // Add full blockes to the cache's full block list.
                                SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);
                        } else if(__predict_true(pg->u.pi.num_free == sc->num_entries)) {
                                // Return completely free block to arena free block list.
                                arena->block_used--;
                                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + sizeof(struct s_block),
                                                           BLOCK_SIZE - sizeof(struct s_block));
                                SLIST_INSERT_HEAD(&arena->free_blocks,pg,link);
                        } else {
                                if(!best) {
                                        free_best = pg->u.pi.num_free;
                                        best = pg;
                                } else {
                                        if(pg->u.pi.num_free < free_best) {
                                                struct s_block *tmp = best;
                                                best = pg; pg = tmp;
                                                free_best = pg->u.pi.num_free;
                                        }
                                        SLIST_INSERT_HEAD(&sc->blocks,pg,link);
                                }
                        }
                        if(!npg && fpg) {
                                pg = fpg;
                                fpg = NULL;
                        } else
                                pg = npg;
                }
                if(best)
                        SLIST_INSERT_HEAD(&sc->blocks,best,link);
        }
}

inline static void
clear_block_used_bits(unsigned num_entries, struct s_block *pg)
{
        pg->u.pi.num_free = num_entries;
        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));
        int excess = num_entries % BITS_PER_UNIT;
        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);
#if JHC_VALGRIND
                unsigned header =  sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(num_entries);
                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + header, BLOCK_SIZE - header);
#endif
}

/*
 * allocators
 */

heap_t A_STD
s_alloc(gc_t gc, arena_t arena, struct s_cache *sc)
{
#if _JHC_PROFILE
       sc->allocations++;
       sc->arena->number_allocs++;
#endif
        bool retry = false;
        struct s_block *pg;
        if (__predict_false(arena->force_gc_next_s_alloc)) {
                arena->force_gc_next_s_alloc = 0;
                gc_perform_gc(gc, arena);
        }
retry_s_alloc:
        pg = SLIST_FIRST(&sc->blocks);
        if(__predict_false(!pg)) {
                pg = get_free_block(gc, sc->arena, retry);
                if(__predict_false(!pg)) {
                        retry = true;
                        goto retry_s_alloc;
                }
                VALGRIND_MAKE_MEM_NOACCESS(pg, BLOCK_SIZE);
                VALGRIND_MAKE_MEM_DEFINED(pg, sizeof(struct s_block));
                if(sc->num_entries != pg->u.pi.num_free)
                        VALGRIND_MAKE_MEM_UNDEFINED((char *)pg->used,
                                                    BITARRAY_SIZE_IN_BYTES(sc->num_entries));
                else
                        VALGRIND_MAKE_MEM_DEFINED((char *)pg->used,
                                                  BITARRAY_SIZE_IN_BYTES(sc->num_entries));
                assert(pg);
                pg->flags = sc->flags;
                pg->color = sc->color;
                pg->u.pi.num_ptrs = sc->num_ptrs;
                pg->u.pi.size = sc->size;
                pg->u.pi.next_free = 0;
                SLIST_INSERT_HEAD(&sc->blocks,pg,link);
                if(sc->num_entries != pg->u.pi.num_free)
                        clear_block_used_bits(sc->num_entries, pg);
                pg->used[0] = 1; //set the first bit
                pg->u.pi.num_free = sc->num_entries - 1;
                return (uintptr_t *)pg + pg->color;
        } else {
                __builtin_prefetch(pg->used,1);
                pg->u.pi.num_free--;
                unsigned next_free = pg->u.pi.next_free;
                unsigned found = bitset_find_free(&next_free,BITARRAY_SIZE(sc->num_entries),pg->used);
                pg->u.pi.next_free = next_free;
                void *val = (uintptr_t *)pg + pg->color + found*pg->u.pi.size;
                if(__predict_false(0 == pg->u.pi.num_free)) {
                        assert(pg == SLIST_FIRST(&sc->blocks));
                        SLIST_REMOVE_HEAD(&sc->blocks,link);
                        SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);
                }
                assert(S_BLOCK(val) == pg);
                //jhc_printf_stderr("s_alloc: val: %p s_block: %p size: %i color: %i found: %i num_free: %i\n", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);
                return val;
        }
}

struct s_cache *
new_cache(arena_t arena, unsigned short size, unsigned short num_ptrs)
{
        struct s_cache *sc = malloc(sizeof(*sc));
        memset(sc,0,sizeof(*sc));
        sc->arena = arena;
        sc->size = size;
        sc->num_ptrs = num_ptrs;
        sc->flags = 0;
        size_t excess = BLOCK_SIZE - sizeof(struct s_block);
        sc->num_entries = (8*excess) / (8*sizeof(uintptr_t)*size + 1) - 1;
        sc->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) +
                        sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
        SLIST_INIT(&sc->blocks);
        SLIST_INIT(&sc->full_blocks);
        SLIST_INSERT_HEAD(&arena->caches,sc,next);
        return sc;
}

// clear all used bits, must be followed by a marking phase.
static void
clear_used_bits(arena_t arena)
{
        struct s_block *pg;
        SLIST_FOREACH(pg, &arena->monolithic_blocks, link)
            pg->used[0] = 0;
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                SLIST_FOREACH(pg, &sc->blocks, link)
                    clear_block_used_bits(sc->num_entries,pg);
                SLIST_FOREACH(pg, &sc->full_blocks, link)
                    clear_block_used_bits(sc->num_entries,pg);
        }
}

// Set a used bit. returns true if the tagged node should be scanned by the GC.
// this happens when the used bit was not previously set and the node contains
// internal pointers.

static bool
s_set_used_bit(void *val)
{
        assert(val);
        struct s_block *pg = S_BLOCK(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->color;
        if(__predict_true(BIT_IS_UNSET(pg->used,offset/pg->u.pi.size))) {
                if (pg->flags & SLAB_MONOLITH) {
                        pg->used[0] = 1;
                        return (bool)pg->u.m.num_ptrs;

                } else {
                        BIT_SET(pg->used,offset/pg->u.pi.size);
                        pg->u.pi.num_free--;
                        return (bool)pg->u.pi.num_ptrs;
                }
        }
        return false;
}

struct s_cache *
find_cache(struct s_cache **rsc, arena_t arena,
           unsigned short size, unsigned short num_ptrs)
{
        if(__predict_true(rsc && *rsc))
                return *rsc;
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                if(sc->size == size && sc->num_ptrs == num_ptrs)
                        goto found;
        }
        sc = new_cache(arena,size,num_ptrs);
found:
        if(rsc)
                *rsc = sc;
        return sc;
}

void
alloc_public_caches(arena_t arena, size_t size) {
        if (arena->public_caches_p == NULL) {
                arena->public_caches_p = malloc(size);
                memset(arena->public_caches_p, 0, size);
        }
}

struct s_caches_pub *
public_caches(arena_t arena) {
        return arena->public_caches_p;
}

/* Enter with rts_lock. */
arena_t
new_arena(void) {
        arena_t arena = SLIST_FIRST(&free_arenas);
        if (arena) {
                SLIST_REMOVE(&free_arenas, arena, s_arena, link);
        } else {
                arena = malloc(sizeof(struct s_arena));
                memset(arena, 0, sizeof(*arena));
                // Following menbers isn't clear at jhc_alloc_fini for keeping caches.
                SLIST_INIT(&arena->caches);
                arena->public_caches_p = NULL;
        }
        SLIST_INIT(&arena->free_blocks);
        SLIST_INIT(&arena->megablocks);
        SLIST_INIT(&arena->monolithic_blocks);
        arena->block_used = 0;
        arena->block_threshold = 8;
        arena->current_megablock = NULL;
        memset(&arena->root_stack, 0, sizeof(arena->root_stack));

        for (int i = 0; i < GC_STATIC_ARRAY_NUM; i++) {
                find_cache(&arena->array_caches[i], arena, i + 1, i + 1);
                find_cache(&arena->array_caches_atomic[i], arena, i + 1, 0);
        }
        return arena;
}

uint32_t
get_heap_flags(void * sp) {
        uint32_t ret = 0;
        switch (GET_PTYPE(sp)) {
        case P_VALUE: return SLAB_VIRTUAL_VALUE;
        case P_FUNC: return SLAB_VIRTUAL_FUNC;
        case P_LAZY:
                     ret |= SLAB_VIRTUAL_LAZY;
        case P_WHNF:
                     if (S_BLOCK(sp) == NULL)
                             return (ret | SLAB_VIRTUAL_SPECIAL);
                     if ((void *)sp >= nh_start && (void *)sp <= nh_end)
                             return (ret | SLAB_VIRTUAL_CONSTANT);
                     return ret |= S_BLOCK(sp)->flags;
        }
        return ret;
}

heap_t A_STD
gc_malloc_foreignptr(gc_t gc, arena_t arena, unsigned alignment, unsigned size, bool finalizer) {
        // we don't allow higher alignments yet.
        assert (alignment <= sizeof(uintptr_t));
        // no finalizers yet
        assert (!finalizer);
        unsigned spacing = 1 + finalizer;
        wptr_t *res = gc_array_alloc_atomic(gc, arena, spacing + TO_BLOCKS(size),
                                             finalizer ? SLAB_FLAG_FINALIZER : SLAB_FLAG_NONE);
        res[0] = (wptr_t)(res + spacing);
        if (finalizer)
                res[1] = NULL;
        return TO_SPTR(P_WHNF, res);
}

heap_t A_STD
gc_new_foreignptr(gc_t gc, arena_t arena, HsPtr ptr) {
        HsPtr *res = gc_array_alloc_atomic(gc, arena, 2, SLAB_FLAG_FINALIZER);
        res[0] = ptr;
        res[1] = NULL;
        return TO_SPTR(P_WHNF, res);
}

bool A_STD
gc_add_foreignptr_finalizer(wptr_t fp, HsFunPtr finalizer) {
        if (!(SLAB_FLAG_FINALIZER & get_heap_flags(fp)))
                return false;
        HsFunPtr **res = (HsFunPtr**)FROM_SPTR(fp);
        unsigned len = 0;
        if (res[1])
                while(res[1][len++]);
        else
                len = 1;
        res[1] = realloc(res[1], (len + 1) * sizeof(HsFunPtr));
        HsFunPtr *ptrs = res[1];
        ptrs[len - 1] = finalizer;
        ptrs[len] = NULL;
        return true;
}

void
print_cache(struct s_cache *sc) {
        jhc_printf_stderr("num_entries: %i with %lu bytes of header\n",
                (int)sc->num_entries, sizeof(struct s_block) +
                BITARRAY_SIZE_IN_BYTES(sc->num_entries));
        jhc_printf_stderr("  size: %i words %i ptrs\n",
                (int)sc->size,(int)sc->num_ptrs);
#if _JHC_PROFILE
        jhc_printf_stderr("  allocations: %lu\n", (unsigned long)sc->allocations);
#endif
        if(SLIST_EMPTY(&sc->blocks) && SLIST_EMPTY(&sc->full_blocks))
                return;
        jhc_printf_stderr("  blocks:\n");
        jhc_printf_stderr("%20s %9s %9s %s\n", "block", "num_free", "next_free", "status");
        struct s_block *pg;
        SLIST_FOREACH(pg,&sc->blocks,link)
            jhc_printf_stderr("%20p %9i %9i %c\n", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'P');
        SLIST_FOREACH(pg,&sc->full_blocks,link)
            jhc_printf_stderr("%20p %9i %9i %c\n", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'F');
}

void hs_perform_gc(void) {
        arena_t arena;
        jhc_rts_lock();
        SLIST_FOREACH(arena, &used_arenas, link) {
                arena->force_gc_next_s_alloc = 1;
        }
        jhc_rts_unlock();
}

#endif
