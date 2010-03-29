

#ifndef JGC_H
#define JGC_H

// #if __GNUC_PREREQ__(2, 96)
#if 1
#  define __predict_true(exp)     __builtin_expect(!!(exp), 1)
#  define __predict_false(exp)    __builtin_expect(!!(exp), 0)
#else
#  define __predict_true(exp)     (exp)
#  define __predict_false(exp)    (exp)
#endif

#define JGC_STATUS 0

#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))

typedef struct {
        uint8_t count;
        uint8_t nptrs;
        uint16_t tag;
} entry_header_t;

struct frame {
        struct frame *prev;
        unsigned nptrs;
        void *ptrs[0];
};


// round all allocations up to this many blocks.
// the underlying malloc implementation has some
// minimum size and this allows memory blocks to
// be reused more often.
#define GC_MINIMUM_SIZE 3
#define GC_BASE sizeof(void *)
#define GC_ALIGNMENT (2*sizeof(void *))

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)

#define INITIAL_GC NULL

#ifdef JHC_JGC_STACK
#define gc_frame0(gc,n,...) struct { struct frame *prev; unsigned nptrs;void *ptrs[n]; } l \
          = { gc, n, { __VA_ARGS__ } }; gc_t gc = (gc_t)(void *)&l;
#else
#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; gc_t sgc = gc;  gc_t gc = sgc + n;
#endif

static void gc_perform_gc(gc_t gc);
static void *gc_alloc_tag(gc_t gc,unsigned count, unsigned nptrs, int tag);

static inline void *
gc_alloc_bytes(gc_t gc,size_t count) {
        return gc_alloc_tag(gc, TO_BLOCKS(count), 0, 0);
}


#endif


#ifdef NDEBUG
#define JUDYERROR_NOTEST 1
#endif

#include <Judy.h>
#include <assert.h>
#include <stdio.h>


#if JGC_STATUS > 1
#define debugf(...) fprintf(stderr,__VA_ARGS__)
#else
#define debugf(...) do { } while (0)
#endif


static Pvoid_t  gc_roots       = NULL;  // extra roots in addition to the stack
static Pvoid_t  gc_allocated   = NULL;  // black set of currently allocated memory
static size_t   heap_threshold = 2048;  // threshold at which we want to run a gc rather than malloc more memory
static size_t   mem_inuse;              // amount of memory in use by gc'ed memory
static unsigned number_gcs;             // number of garbage collections
static unsigned number_allocs;          // number of allocations since last garbage collection

#define SHOULD_FOLLOW(w)  IS_PTR(w)

typedef struct {
        union {
                entry_header_t v;
                void * _dummy;
        } u;
        void * ptrs[0];
} entry_t;


static bool
gc_add_root(gc_t gc, void *root)
{
        if(SHOULD_FOLLOW(root)) {
                int r; J1S(r,gc_roots,(((Word_t)root - sizeof(entry_t)) / GC_ALIGNMENT));
                return (bool)r;
        } else
                return false;
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
gc_add_grey(Pvoid_t gc_grey[1], struct stack *stack, uintptr_t ix)
{
        int r;
        J1U(r, gc_allocated, ix);
        if(r) {
                J1S(r, *gc_grey, ix);
                if(r)
                        stack->stack[stack->ptr++] = (uintptr_t)ix * GC_ALIGNMENT;
        }
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

        Pvoid_t gc_grey = NULL;
        Word_t ix;
        debugf("Setting Roots:");
        Word_t n_roots;
        J1C(n_roots,gc_roots,0,-1);
        stack_check(&stack, n_roots);
        int r; for(ix = 0,(J1F(r,gc_roots,ix)); r; (J1N(r,gc_roots,ix))) {
                debugf(" %p", (void *)(ix * GC_ALIGNMENT));
                gc_add_grey(&gc_grey, &stack, ix);
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
        for(int i = 0; i < number_stack; i++) {
                debugf(" |");
                // TODO - short circuit redirects on stack
                sptr_t ptr = gc_stack_base[i];
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
                ix = (Word_t)e / GC_ALIGNMENT;
                gc_add_grey(&gc_grey, &stack, ix);
        }
#endif
        debugf("\n");

        while(stack.ptr) {
                uintptr_t ix = stack.stack[--stack.ptr] / GC_ALIGNMENT;
                debugf("Processing Grey: %p\n",(void *)(ix * GC_ALIGNMENT));

                entry_t *e = (entry_t *)(ix * GC_ALIGNMENT);
                int offset = e->u.v.tag ? 1 : 0;
                stack_check(&stack, e->u.v.nptrs);
                for(int i = offset; i < e->u.v.nptrs + offset; i++) {
                        if(P_LAZY == GET_PTYPE(e->ptrs[i])) {
                                if(!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {
                                        number_redirects++;
                                        debugf(" *");
                                        e->ptrs[i] = GETHEAD(FROM_SPTR(e->ptrs[i]));
                                }
                        }
                        if(__predict_true(SHOULD_FOLLOW(e->ptrs[i]))) {
                                entry_t * ptr = (entry_t *)(FROM_SPTR(e->ptrs[i])) - 1;
                                debugf("Following: %p %p\n",e->ptrs[i], (void *)ptr);
                                gc_add_grey(&gc_grey, &stack, (uintptr_t)ptr / GC_ALIGNMENT);
                        }
                }
        }
        free(stack.stack);
        for(ix = 0, (J1F(r,gc_allocated,ix)); r; (J1N(r,gc_allocated,ix))) {
                entry_t *e = (entry_t *)(ix * GC_ALIGNMENT);
                mem_inuse -= (e->u.v.count + 1)*GC_BASE;
                free(e);
        }
        J1FA(r,gc_allocated);
        gc_allocated = gc_grey;
        if(JGC_STATUS) {
                fprintf(stderr, "%3u - Ss: %5u Ps: %5u Rs: %5u As: %6u ", number_gcs, number_stack, number_ptr, number_redirects, number_allocs);
                Word_t n_allocated,n_roots;
                J1C(n_allocated,gc_allocated,0,-1);
                J1C(n_roots,gc_roots,0,-1);
#ifdef JHC_JGC_STACK
                void * gc_stack_base = &gc_stack_base;
#endif
                fprintf(stderr,"live: %5lu root: %3lu inuse: %6lu threshold: %6lu %p %p\n",n_allocated,n_roots,(long unsigned)mem_inuse,(long unsigned)heap_threshold, gc_stack_base, gc);
        }
        number_allocs = 0;
        profile_pop(&gc_gc_time);
}

static void *
gc_alloc_tag(gc_t gc,unsigned count, unsigned nptrs, int tag)
{
        profile_push(&gc_alloc_time);
        number_allocs++;
        assert(nptrs <= count);
        if(__predict_false(mem_inuse > heap_threshold)) {
                gc_perform_gc(gc);
                if(__predict_false(mem_inuse > ((heap_threshold * 6) / 10))) {
                        heap_threshold *= 2;
                        if(JGC_STATUS)
                                fprintf(stderr, "Increasing heap threshold to %u bytes because mem usage is %u.\n", (unsigned) heap_threshold, (unsigned)mem_inuse);
                }
        }
        entry_t *e = malloc((count + 1)*GC_BASE);
        mem_inuse += (count + 1)*GC_BASE;
        e->u.v.count = count;
        e->u.v.nptrs = nptrs;
        e->u.v.tag = tag;
        debugf("allocated: %p %i %i %i\n",(void *)e, count, nptrs, tag);
        int r; J1S(r,gc_allocated,(Word_t)e / GC_ALIGNMENT);
        profile_pop(&gc_alloc_time);
        return (void *)(e + 1);
}

