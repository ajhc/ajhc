#ifdef JHC_RTS_INCLUDE
#undef JHC_RTS_INCLUDE
#include "jhc_jgc.c"
#define JHC_RTS_INCLUDE
#else
#if _JHC_GC == _JHC_GC_JGC

static struct s_arena *arena;

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)

#ifdef JHC_JGC_STACK
#define gc_frame0(gc,n,...) struct { struct frame *prev; unsigned nptrs;void *ptrs[n]; } l \
          = { gc, n, { __VA_ARGS__ } }; gc_t gc = (gc_t)(void *)&l;
#else
#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; gc_t sgc = gc;  gc_t gc = sgc + n;
#endif


static Pvoid_t  gc_roots       = NULL;  // extra roots in addition to the stack
static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection

#define SHOULD_FOLLOW(w)  IS_PTR(w)
#define TO_GCPTR(x) (void *)((uintptr_t)FROM_SPTR(x) - sizeof(entry_t))

typedef struct {
        void * ptrs[0];
} entry_t;


static bool
gc_check_heap(void *s)
{
        int r; J1T(r,gc_inheap,(uintptr_t)s / PAGESIZE);
        return r;
}

static void
gc_add_root(gc_t gc, void *root)
{
        if(SHOULD_FOLLOW(root)) {
                void * nroot = TO_GCPTR(root);
                if(gc_check_heap((void *)nroot)) {
                        int r; J1S(r,gc_roots,(Word_t)nroot);
                }
        }
}

struct stack {
        unsigned size;
        unsigned ptr;
        uintptr_t *stack;
};

#define EMPTY_STACK { 0, 0, NULL }

static void
stack_check(struct stack *s, unsigned n) {
        if(__predict_false(s->size - s->ptr < n)) {
                s->size += 1024 + n;
                s->stack = realloc(s->stack, sizeof(uintptr_t)*s->size);
                assert(s->stack);
                debugf("stack:");
                for(unsigned i = 0; i < s->ptr; i++) {
                        debugf(" %p", (void *)s->stack[i]);
                }
                debugf("\n");
        }
}

static void
gc_add_grey(struct stack *stack, void *s)
{
        if(gc_check_heap(s) && s_set_used_bit(s))
                stack->stack[stack->ptr++] = (uintptr_t)s;
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

        Word_t ix;
        debugf("Setting Roots:");
        Word_t n_roots;
        J1C(n_roots,gc_roots,0,-1);
        stack_check(&stack, n_roots);
        int r; for(ix = 0,(J1F(r,gc_roots,ix)); r; (J1N(r,gc_roots,ix))) {
                debugf(" %p", (void *)ix);
                gc_add_grey(&stack, (void *)ix);
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
                        if(__predict_false(!SHOULD_FOLLOW(ptr))) {
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
                if(P_LAZY == GET_PTYPE(ptr)) {
                        if(!IS_LAZY(GETHEAD(FROM_SPTR(ptr)))) {
                                void *gptr = TO_GCPTR(ptr);
                                if(gc_check_heap(gptr))
                                        s_set_used_bit(gptr);
                                number_redirects++;
                                debugf(" *");
                                ptr = (sptr_t)GETHEAD(FROM_SPTR(ptr));
                        }
                }
                if(__predict_false(!SHOULD_FOLLOW(ptr))) {
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
                entry_t *e = (entry_t *)stack.stack[--stack.ptr];
                struct s_page *pg = S_PAGE(e);
                debugf("Processing Grey: %p\n",e);

                int offset = pg->pi.tag ? 1 : 0;
                stack_check(&stack, pg->pi.num_ptrs);
                for(int i = offset; i < pg->pi.num_ptrs + offset; i++) {
                        if(P_LAZY == GET_PTYPE(e->ptrs[i])) {
                                if(!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {
                                        number_redirects++;
                                        debugf(" *");
                                        e->ptrs[i] = GETHEAD(FROM_SPTR(e->ptrs[i]));
                                }
                        }
                        if(__predict_true(SHOULD_FOLLOW(e->ptrs[i]))) {
                                entry_t * ptr = TO_GCPTR(e->ptrs[i]);
                                debugf("Following: %p %p\n",e->ptrs[i], ptr);
                                gc_add_grey( &stack, ptr);
                        }
                }
        }
        free(stack.stack);
        s_cleanup_pages(arena);
        if(JGC_STATUS) {
#ifdef JHC_JGC_STACK
                void * gc_stack_base = &gc_stack_base;
                Word_t n_roots;
                J1C(n_roots,gc_roots,0,-1);
#endif

                fprintf(stdout, "%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\n",
                        number_gcs,
                        number_allocs,
                        (unsigned)arena->num_used,
                        page_threshold,
                        number_stack,
                        number_ptr,
                        number_redirects,
                        (unsigned)n_roots
                       );
                //               jhc_malloc_fini();
        }
        number_allocs = 0;
        profile_pop(&gc_gc_time);
}

static void *
gc_alloc_tag(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs, int tag)
{
        profile_push(&gc_alloc_time);
        number_allocs++;
        assert(nptrs <= count);
        entry_t *e = s_alloc(gc, find_cache(sc, arena, count, nptrs, tag));
        debugf("allocated: %p %i %i %i\n",(void *)e, count, nptrs, tag);
        profile_pop(&gc_alloc_time);
        return (void *)e;
}

static void jhc_alloc_print_stats(void) { }

#ifdef JHC_JGC_STACK
static void jhc_malloc_init(void) { }
#else
static void
jhc_malloc_init(void) {
        saved_gc = gc_stack_base = malloc(8*8192*sizeof(gc_stack_base[0]));
        arena = new_arena();
}
#endif

static void
jhc_malloc_fini(void) {
        if(0) {
                printf("arena: %p\n", arena);
                printf("  base: %p\n", arena->base);
                printf("  next_free: %i\n", arena->next_free);
                printf("  num_used: %i\n", arena->num_used);
                struct s_cache *sc = SLIST_FIRST(&arena->caches);
                for(;sc;sc = SLIST_NEXT(sc,next)) {
                        print_cache(sc);
                }
        }
}

#endif
#endif
