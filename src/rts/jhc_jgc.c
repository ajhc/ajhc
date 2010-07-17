#if _JHC_GC == _JHC_GC_JGC
static struct s_arena *arena;

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)

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
#endif

static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection

#define TO_GCPTR(x) (entry_t *)(FROM_SPTR(x))

typedef struct {
        sptr_t ptrs[0];
} entry_t;


static bool
gc_check_heap(entry_t *s)
{
        return (s < &_start || s > &_end);
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
        s->stack = realloc(s->stack, sizeof(uintptr_t)*s->size);
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

static void
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
        memcpy(stack.stack + stack.ptr,root_stack.stack, root_stack.ptr * sizeof(root_stack.stack[0]));
        stack.ptr += root_stack.ptr;
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
                debugf("Processing Grey: %p\n",e);

                stack_check(&stack, pg->pi.num_ptrs);
                for(int i = 0; i < pg->pi.num_ptrs; i++) {
                        if(1 && (P_LAZY == GET_PTYPE(e->ptrs[i]))) {
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

                fprintf(stdout, "%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\n",
                        number_gcs,
                        number_allocs,
                        (unsigned)arena->block_used,
                        block_threshold,
                        number_stack,
                        number_ptr,
                        number_redirects,
                        (unsigned)root_stack.ptr
                       );
        }
        number_allocs = 0;
        profile_pop(&gc_gc_time);
}

static void *
gc_alloc(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs)
{
        profile_push(&gc_alloc_time);
        number_allocs++;
        assert(nptrs <= count);
        entry_t *e = s_alloc(gc, find_cache(sc, arena, count, nptrs));
        debugf("allocated: %p %i %i\n",(void *)e, count, nptrs);
        profile_pop(&gc_alloc_time);
        return (void *)e;
}

static void jhc_alloc_print_stats(void) { }

#ifdef JHC_JGC_STACK
static void jhc_alloc_init(void) { }
#else
static void
jhc_alloc_init(void) {
        saved_gc = gc_stack_base = malloc((1UL << 18)*sizeof(gc_stack_base[0]));
        arena = new_arena();
}
#endif

static void
jhc_alloc_fini(void) {
        if(JHC_STATUS) {
                printf("arena: %p\n", arena);
                printf("  block_used: %i\n", arena->block_used);
                printf("  block_threshold: %i\n", arena->block_threshold);
                struct s_cache *sc = SLIST_FIRST(&arena->caches);
                for(;sc;sc = SLIST_NEXT(sc,next)) {
                        print_cache(sc);
                }
        }
}
#endif
