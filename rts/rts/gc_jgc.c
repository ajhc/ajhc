#if _JHC_GC == _JHC_GC_JGC
#ifdef JHC_HEADER

#ifdef JHC_JGC_STACK
struct frame;
typedef struct frame *gc_t;
#else
typedef void* *gc_t;
static gc_t gc_stack_base;
static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection
#endif

static gc_t saved_gc;

#define GC_BASE sizeof(void *)
#define TO_BLOCKS(x) ((x) <= GC_BASE ? 1 : (((x) - 1)/GC_BASE) + 1)

static void gc_perform_gc(gc_t gc);

#else

#include "sys/queue.h"

//static gc_t saved_gc;
static struct s_arena *arena;

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
static const void * const nh_stuff[];

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

#endif
#endif
