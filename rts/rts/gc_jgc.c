#include "jhc_rts_header.h"
#include "sys/queue.h"
#include "sys/bitarray.h"

#if _JHC_GC == _JHC_GC_JGC

struct s_arena {
        struct s_megablock *current_megablock;
        SLIST_HEAD(,s_block) free_blocks;
        unsigned block_used;
        unsigned block_threshold;
        SLIST_HEAD(,s_cache) caches;
        SLIST_HEAD(,s_megablock) megablocks;
};

struct s_megablock {
        void *base;
        unsigned next_free;
        SLIST_ENTRY(s_megablock) next;
};

struct s_block_info {
        unsigned char color;
        unsigned char size;
        unsigned char num_ptrs;
        unsigned char flags;
};

struct s_block {
        SLIST_ENTRY(s_block) link;
        struct s_block_info pi;
        unsigned short num_free;
        unsigned short next_free;
        bitarray_t used[];
};

struct s_cache {
        SLIST_ENTRY(s_cache) next;
        SLIST_HEAD(,s_block) blocks;
        SLIST_HEAD(,s_block) full_blocks;
        struct s_block_info pi;
        unsigned short num_entries;
        struct s_arena *arena;
};

gc_t saved_gc;
static struct s_arena *arena;
static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection
static gc_t gc_stack_base;

#ifdef JHC_JGC_STACK
struct frame {
        struct frame *prev;
        unsigned nptrs;
        void *ptrs[0];
};
#define gc_frame0(gc,n,...) struct { struct frame *prev; unsigned nptrs;void *ptrs[n]; } l \
          = { gc, n, { __VA_ARGS__ } }; gc_t gc = (gc_t)(void *)&l;
#else
#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; gc_t sgc = gc;  gc_t gc = sgc + n;
#define gc_frame1(gc,p1) gc[0] = (sptr_t)p1; gc_t sgc = gc;  gc_t gc = sgc + 1;
#define gc_frame2(gc,p1,p2) gc[0] = (sptr_t)p1; gc[1] = (sptr_t)p2; gc_t sgc = gc;  gc_t gc = sgc + 2;
#endif

static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection

#define TO_GCPTR(x) (entry_t *)(FROM_SPTR(x))

static void gc_perform_gc(gc_t gc);
static bool s_set_used_bit(void *val) A_UNUSED;
static void clear_used_bits(struct s_arena *arena) A_UNUSED;
static struct s_arena *new_arena(void);
static void s_cleanup_blocks(struct s_arena *arena);
static void print_cache(struct s_cache *sc);

typedef struct {
        sptr_t ptrs[0];
} entry_t;

static const void *nh_start, *nh_end;

static bool
gc_check_heap(entry_t *s)
{
        return (s < (entry_t *)nh_start || s > (entry_t *)nh_end);
}

struct stack {
        unsigned size;
        unsigned ptr;
        entry_t * *stack;
};

#define EMPTY_STACK { 0, 0, NULL }

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
                stack_grow(s,n + 1024);
        }
}

static struct stack root_stack = EMPTY_STACK;

A_UNUSED static void
gc_add_root(gc_t gc, sptr_t root)
{
        if(IS_PTR(root)) {
                entry_t *nroot = TO_GCPTR(root);
                if(gc_check_heap(nroot)) {
                        stack_check(&root_stack,1);
                        root_stack.stack[root_stack.ptr++] = nroot;
                }
        }
}

static void
gc_add_grey(struct stack *stack, entry_t *s)
{
        VALGRIND_MAKE_MEM_DEFINED(s,(S_BLOCK(s))->pi.size * sizeof(uintptr_t));
        if(gc_check_heap(s) && s_set_used_bit(s))
                stack->stack[stack->ptr++] = s;
}

static void
gc_perform_gc(gc_t gc)
{
        profile_push(&gc_gc_time);
        number_gcs++;

        unsigned number_redirects = 0;
        unsigned number_stack = 0;
        unsigned number_ptr = 0;
        struct stack stack = EMPTY_STACK;

        clear_used_bits(arena);

        debugf("Setting Roots:");
        stack_check(&stack, root_stack.ptr);
        for(unsigned i = 0; i < root_stack.ptr; i++) {
                gc_add_grey(&stack, root_stack.stack[i]);
                debugf(" %p", root_stack.stack[i]);
        }
        debugf("\n");
        debugf("Trace:");
#ifdef JHC_JGC_STACK
        for(;gc;gc = gc->prev) {
                debugf(" |");
                stack_check(&stack, gc->nptrs);
                for(unsigned i = 0;i < gc->nptrs; i++) {
                        number_stack++;
                        // TODO - short circuit redirects on stack
                        sptr_t ptr = gc->ptrs[i];
                        if(P_LAZY == GET_PTYPE(ptr)) {
                                if(!IS_LAZY(GETHEAD(FROM_SPTR(ptr)))) {
                                        J1U(r,gc_allocated,((uintptr_t)FROM_SPTR(ptr) - sizeof(entry_t))/GC_ALIGNMENT);
                                        if(r)
                                                J1S(r,gc_grey,((uintptr_t)FROM_SPTR(ptr) - sizeof(entry_t))/GC_ALIGNMENT);
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
                        entry_t *e = (entry_t *)FROM_SPTR(ptr) - 1;
                        debugf(" %p",(void *)e);
                        gc_add_grey(&gc_grey, &stack, (uintptr_t)e / GC_ALIGNMENT);
                }
        }
#else
        stack_check(&stack, gc - gc_stack_base);
        number_stack = gc - gc_stack_base;
        for(unsigned i = 0; i < number_stack; i++) {
                debugf(" |");
                // TODO - short circuit redirects on stack
                sptr_t ptr = gc_stack_base[i];
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
        }
#endif
        debugf("\n");

        while(stack.ptr) {
                entry_t *e = stack.stack[--stack.ptr];
                struct s_block *pg = S_BLOCK(e);
                VALGRIND_MAKE_MEM_DEFINED(e,pg->pi.size * sizeof(uintptr_t));
                debugf("Processing Grey: %p\n",e);

                stack_check(&stack, pg->pi.num_ptrs);
                for(int i = 0; i < pg->pi.num_ptrs; i++) {
                        if(1 && (P_LAZY == GET_PTYPE(e->ptrs[i]))) {
                                VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(e->ptrs[i]), sizeof(uintptr_t));
                                if(!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {
                                        number_redirects++;
                                        debugf(" *");
                                        e->ptrs[i] = (sptr_t)GETHEAD(FROM_SPTR(e->ptrs[i]));
                                }
                        }
                        if(IS_PTR(e->ptrs[i])) {
                                entry_t * ptr = TO_GCPTR(e->ptrs[i]);
                                debugf("Following: %p %p\n",e->ptrs[i], ptr);
                                gc_add_grey( &stack, ptr);
                        }
                }
        }
        free(stack.stack);
        s_cleanup_blocks(arena);
        if(JHC_STATUS) {
#ifdef JHC_JGC_STACK
                void * gc_stack_base = &gc_stack_base;
                Word_t n_roots;
                J1C(n_roots,gc_roots,0,-1);
#endif

                fprintf(stderr, "%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\n",
                        number_gcs,
                        number_allocs,
                        (unsigned)arena->block_used,
                        (unsigned)arena->block_threshold,
                        number_stack,
                        number_ptr,
                        number_redirects,
                        (unsigned)root_stack.ptr
                       );
                number_allocs = 0;
        }
        profile_pop(&gc_gc_time);
}

void jhc_alloc_print_stats(void) { }

void
jhc_alloc_init(void) {
        VALGRIND_PRINTF("Jhc-Valgrind mode active.\n");
#ifndef JHC_JGC_STACK
        saved_gc = gc_stack_base = malloc((1UL << 18)*sizeof(gc_stack_base[0]));
#endif
        arena = new_arena();
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

void
jhc_alloc_fini(void) {
        if(JHC_STATUS) {
                fprintf(stderr, "arena: %p\n", arena);
                fprintf(stderr, "  block_used: %i\n", arena->block_used);
                fprintf(stderr, "  block_threshold: %i\n", arena->block_threshold);
                struct s_cache *sc = SLIST_FIRST(&arena->caches);
                for(;sc;sc = SLIST_NEXT(sc,next)) {
                        print_cache(sc);
                }
        }
}

A_UNUSED static void *
(gc_alloc)(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs)
{
        profile_push(&gc_alloc_time);
        if (JHC_STATUS)
                number_allocs++;
        assert(nptrs <= count);
        entry_t *e = s_alloc(gc, find_cache(sc, arena, count, nptrs));
        VALGRIND_MAKE_MEM_UNDEFINED(e,sizeof(uintptr_t)*count);
        debugf("allocated: %p %i %i\n",(void *)e, count, nptrs);
        profile_pop(&gc_alloc_time);
        return (void *)e;
}

#include "sys/bitarray.h"
#include "sys/queue.h"

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

struct s_megablock *
s_new_megablock(struct s_arena *arena)
{
        struct s_megablock *mb = malloc(sizeof(*mb));
#if defined(__WIN32__)
        mb->base = _aligned_malloc(MEGABLOCK_SIZE, BLOCK_SIZE);
        int ret = !mb->base;
#elif defined(__ARM_EABI__)
        mb->base = memalign(BLOCK_SIZE,MEGABLOCK_SIZE);
        int ret = !mb->base;
#elif (defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ <  1060)
        assert(sysconf(_SC_PAGESIZE) == BLOCK_SIZE);
        mb->base = valloc(MEGABLOCK_SIZE);
        int ret = !mb->base;
#else
        int ret = posix_memalign(&mb->base,BLOCK_SIZE,MEGABLOCK_SIZE);
#endif
        if(ret != 0) {
                fprintf(stderr,"Unable to allocate memory for megablock\n");
                abort();
        }
        VALGRIND_MAKE_MEM_NOACCESS(mb->base,MEGABLOCK_SIZE);
        //VALGRIND_FREELIKE_BLOCK(mb->base,0);
        mb->next_free = 0;
        return mb;
}

/* block allocator */

static struct s_block *
get_free_block(gc_t gc, struct s_arena *arena) {
        arena->block_used++;
        if(__predict_true(SLIST_FIRST(&arena->free_blocks))) {
                struct s_block *pg = SLIST_FIRST(&arena->free_blocks);
                SLIST_REMOVE_HEAD(&arena->free_blocks,link);
                return pg;
        } else {
                if((arena->block_used >= arena->block_threshold)) {
                        gc_perform_gc(gc);
                        // if we are still using 80% of the heap after a gc, raise the threshold.
                        if(__predict_false((unsigned)arena->block_used * 10 >= arena->block_threshold * 9)) {
                                arena->block_threshold *= 2;
                        }
                }
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
                pg->num_free = 0;
                return pg;
        }
}

static void
s_cleanup_blocks(struct s_arena *arena) {
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
                struct s_block *pg = SLIST_FIRST(&sc->blocks);
                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);
                SLIST_INIT(&sc->blocks);
                SLIST_INIT(&sc->full_blocks);
                if(!pg) {
                        pg = fpg;
                        fpg = NULL;
                }
                while(pg) {
                        struct s_block *npg = SLIST_NEXT(pg,link);
                        if(__predict_false(pg->num_free == 0)) {
                                SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);
                        } else if(__predict_true(pg->num_free == sc->num_entries)) {
                                arena->block_used--;
                                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + sizeof(struct s_block), BLOCK_SIZE - sizeof(struct s_block));
                                SLIST_INSERT_HEAD(&arena->free_blocks,pg,link);
                        } else {
                                if(!best) {
                                        free_best = pg->num_free;
                                        best = pg;
                                } else {
                                        if(pg->num_free < free_best) {
                                                struct s_block *tmp = best;
                                                best = pg; pg = tmp;
                                                free_best = pg->num_free;
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
        pg->num_free = num_entries;
        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));
        int excess = num_entries % BITS_PER_UNIT;
        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);
#if JHC_VALGRIND
                unsigned header =  sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(num_entries);
                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + header, BLOCK_SIZE - header);
#endif
}

void *
s_alloc(gc_t gc, struct s_cache *sc)
{
        struct s_block *pg = SLIST_FIRST(&sc->blocks);
        if(__predict_false(!pg)) {
                pg = get_free_block(gc, sc->arena);
                VALGRIND_MAKE_MEM_NOACCESS(pg, BLOCK_SIZE);
                VALGRIND_MAKE_MEM_DEFINED(pg, sizeof(struct s_block));
                if(sc->num_entries != pg->num_free)
                        VALGRIND_MAKE_MEM_UNDEFINED((char *)pg->used,BITARRAY_SIZE_IN_BYTES(sc->num_entries));
                else
                        VALGRIND_MAKE_MEM_DEFINED((char *)pg->used,BITARRAY_SIZE_IN_BYTES(sc->num_entries));
                assert(pg);
                pg->pi = sc->pi;
                pg->next_free = 0;
                SLIST_INSERT_HEAD(&sc->blocks,pg,link);
                if(sc->num_entries != pg->num_free)
                        clear_block_used_bits(sc->num_entries, pg);
                pg->used[0] = 1; //set the first bit
                pg->num_free = sc->num_entries - 1;
                return (uintptr_t *)pg + pg->pi.color;
        } else {
                __builtin_prefetch(pg->used,1);
                pg->num_free--;
                unsigned next_free = pg->next_free;
                unsigned found = bitset_find_free(&next_free,BITARRAY_SIZE(sc->num_entries),pg->used);
                pg->next_free = next_free;
                void *val = (uintptr_t *)pg + pg->pi.color + found*pg->pi.size;
                if(__predict_false(0 == pg->num_free)) {
                        assert(pg == SLIST_FIRST(&sc->blocks));
                        SLIST_REMOVE_HEAD(&sc->blocks,link);
                        SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);
                }
                assert(S_BLOCK(val) == pg);
                //printf("s_alloc: val: %p s_block: %p size: %i color: %i found: %i num_free: %i\n", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);
                return val;
        }
}

/*
static void
s_free(void *val)
{
        assert(val);
        struct s_block *pg = s_block(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;
//        printf("s_free:  val: %p s_block: %p size: %i color: %i num_free: %i offset: %i bit: %i\n", val, pg, pg->pi.size, pg->pi.color, pg->num_free, offset, offset/pg->pi.size);
        assert(BIT_VALUE(pg->used,offset/(pg->pi.size)));
        BIT_UNSET(pg->used,offset/(pg->pi.size));
        pg->num_free++;
}
*/

static struct s_cache *
new_cache(struct s_arena *arena, unsigned short size, unsigned short num_ptrs)
{
        struct s_cache *sc = malloc(sizeof(*sc));
        sc->arena = arena;
        sc->pi.size = size;
        sc->pi.num_ptrs = num_ptrs;
        sc->pi.flags = 0;
        size_t excess = BLOCK_SIZE - sizeof(struct s_block);
        sc->num_entries = (8*excess) / (8*sizeof(uintptr_t)*size + 1) - 1;
        //sc->num_entries = (8*excess) / (8*size*sizeof(uintptr_t) + 1);
        sc->pi.color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
        SLIST_INIT(&sc->blocks);
        SLIST_INIT(&sc->full_blocks);
        SLIST_INSERT_HEAD(&arena->caches,sc,next);
        //print_cache(sc);
        return sc;
}

// clear all used bits, must be followed by a marking phase.
static void
clear_used_bits(struct s_arena *arena)
{
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                struct s_block *pg = SLIST_FIRST(&sc->blocks);
                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);
                do {
                        for(;pg;pg = SLIST_NEXT(pg,link))
                                clear_block_used_bits(sc->num_entries,pg);
                        pg = fpg;
                        fpg = NULL;
                }  while(pg);
        }
}

// set a used bit. returns true if the
// tagged node should be scanned by the GC.
// this happens when the used bit was not previously set
// and the node contains internal pointers.

static bool
s_set_used_bit(void *val)
{
        assert(val);
        struct s_block *pg = S_BLOCK(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;
        if(__predict_true(BIT_IS_UNSET(pg->used,offset/pg->pi.size))) {
                BIT_SET(pg->used,offset/pg->pi.size);
                pg->num_free--;
                return (bool)pg->pi.num_ptrs;
        }
        return false;
}

struct s_cache *
find_cache(struct s_cache **rsc, struct s_arena *arena, unsigned short size, unsigned short num_ptrs)
{
        if(__predict_true(rsc && *rsc))
                return *rsc;
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                if(sc->pi.size == size && sc->pi.num_ptrs == num_ptrs)
                        goto found;
        }
        sc = new_cache(arena,size,num_ptrs);
found:
        if(rsc)
                *rsc = sc;
        return sc;
}

struct s_arena *
new_arena(void) {
        struct s_arena *arena = malloc(sizeof(struct s_arena));
        SLIST_INIT(&arena->caches);
        SLIST_INIT(&arena->free_blocks);
        SLIST_INIT(&arena->megablocks);
        arena->block_used = 0;
        arena->block_threshold = 8;
        arena->current_megablock = NULL;
        return arena;
}

void
print_cache(struct s_cache *sc) {
        fprintf(stderr, "num_entries: %i\n",(int)sc->num_entries);
//        printf("  entries: %i words\n",(int)(sc->num_entries*sc->pi.size));
        fprintf(stderr, "  header: %lu bytes\n", sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries));
        fprintf(stderr, "  size: %i words\n",(int)sc->pi.size);
//        printf("  color: %i words\n",(int)sc->pi.color);
        fprintf(stderr, "  nptrs: %i words\n",(int)sc->pi.num_ptrs);
//        printf("  end: %i bytes\n",(int)(sc->pi.color+ sc->num_entries*sc->pi.size)*sizeof(uintptr_t));
        fprintf(stderr, "%20s %9s %9s %s\n", "block", "num_free", "next_free", "status");
        struct s_block *pg;
        SLIST_FOREACH(pg,&sc->blocks,link) {
            fprintf(stderr, "%20p %9i %9i %c\n", pg, pg->num_free, pg->next_free, 'P');
        }
        fprintf(stderr, "  full_blocks:\n");
        SLIST_FOREACH(pg,&sc->full_blocks,link) {
            fprintf(stderr, "%20p %9i %9i %c\n", pg, pg->num_free, pg->next_free, 'F');
        }
}

#endif
