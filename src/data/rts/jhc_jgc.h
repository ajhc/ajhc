

#ifndef JGC_H
#define JGC_H

#include <stddef.h>
#include <stdbool.h>
#include <inttypes.h>

#define JGC_STATUS 1

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
#define GC_MINIMUM_SIZE 1
#define GC_BASE sizeof(void *)

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)

#define INITIAL_GC NULL
//typedef struct frame *gc_t;

#define gc_frame0(gc,n,...) struct { struct frame *prev; unsigned nptrs;void *ptrs[n]; } l \
          = { gc, n, { __VA_ARGS__ } }; gc_t gc = (gc_t)(void *)&l;

#define gc_count(ty)  (TO_BLOCKS(sizeof(ty)))
#define gc_mk_alloc_tag(ty, np, tag) static inline ty *gc_alloc_ ## ty(gc_t gc) { ty *x = gc_alloc(gc,gc_count(ty),np); gc_tag(x) = tag; return x; }
#define gc_mk_alloc(ty, np) gc_mk_alloc_tag(ty,np,0)
#define gc_mk_alloc_tag_s(ty, np, tag) static inline ty *gc_alloc_ ## ty ## _s(gc_t gc, ty v) { ty *x = gc_alloc(gc,gc_count(ty),np); gc_tag(x) = tag; *x = v; return x; }
#define gc_tag(p) (((entry_header_t *)((void *)p - sizeof(void *)))->tag)

#define FOOF 0xF00DF00FACEBAFFUL

void gc_print_stats(gc_t gc);
void gc_perform_gc(gc_t gc);
void *gc_alloc_tag(gc_t gc,unsigned count, unsigned nptrs, int tag);

// static inline void *
// gc_alloc_tag(gc_t gc,size_t count, unsigned nptrs, int tag)
// {
//         void *ptr = gc_alloc(gc, TO_BLOCKS(count), 0);
//         return ptr;
// }

static inline void *
gc_alloc_bytes(gc_t gc,size_t count)
{
        return gc_alloc_tag(gc, TO_BLOCKS(count), 0, 0);
}

bool gc_add_root(gc_t gc, void *root);
bool gc_del_root(gc_t gc, void *root);

#endif


#include <Judy.h>
#include <assert.h>
#include <stdio.h>


#if 0
#define debugf(...) fprintf(stderr,__VA_ARGS__)
#else
#define debugf(...)
#endif


static Pvoid_t gc_roots = NULL;
static Pvoid_t gc_allocated = NULL;
static Pvoid_t gc_free = NULL;

// we allow new malloced memory until this threshold is reached
static size_t heap_threshold = 256;

// how much memory is currently in use
static size_t mem_inuse;

unsigned number_gcs;


// #define SHOULD_FOLLOW(w)  (w && !((uintptr_t)w & 0x3))
// #define SHOULD_FOLLOW(w)  (IS_PTR(w) && ((w) < &_start || (w) >= &_end))
#define SHOULD_FOLLOW(w)  IS_PTR(w)

typedef struct {
        union {
        entry_header_t v;
        void * _dummy;
        } u;
        void * ptrs[0];
} entry_t;


bool
gc_add_root(gc_t gc, void *root)
{
        if(SHOULD_FOLLOW(root)) {
                int r;
                J1S(r,gc_roots,((Word_t)root / GC_BASE) - 1 );
                return (bool)r;
        } else {
                return false;
        }
}

bool
gc_del_root(gc_t gc, void *root)
{
        if(SHOULD_FOLLOW(root)) {
                int r;
                J1U(r,gc_roots,((Word_t)root / GC_BASE) - 1);
                return (bool)r;
        } else {
                return false;
        }
}

void
gc_print_stats(gc_t gc)
{
        Word_t n_allocated,n_roots,n_free;
        J1C(n_allocated,gc_allocated,0,-1);
        J1C(n_roots,gc_roots,0,-1);
        J1C(n_free,gc_free,0,-1);
        fprintf(stderr,"allocated: %lu roots: %lu free: %lu mem_inuse: %lu heap_threshold: %lu gcs: %u\n",n_allocated,n_roots,n_free,(long unsigned)mem_inuse,(long unsigned)heap_threshold,number_gcs);
}

void
gc_perform_gc(gc_t gc)
{
        profile_push(&gc_gc_time);
        number_gcs++;
        Pvoid_t gc_grey = NULL;
        Pvoid_t gc_black = NULL;
        Word_t ix;
        int r;
        // initialize the grey set with the roots
        debugf("Setting Roots:");
        for(ix = 0,(J1F(r,gc_roots,ix)); r; (J1N(r,gc_roots,ix))) {
                debugf(" %p",(void *)(ix * GC_BASE));
                int d; J1S(d,gc_grey,ix);
        }
        debugf("\n");
        debugf("Trace:");
        for(;gc;gc = gc->prev) {
                debugf(" |");
                for(unsigned i = 0;i < gc->nptrs; i++) {
                        if(!SHOULD_FOLLOW(gc->ptrs[i])) {
                                debugf(" -");
                                continue;
                        }
                        entry_t *e = (entry_t *)FROM_SPTR(gc->ptrs[i]) - 1;
                        debugf(" %p",(void *)e);
                        int d; J1S(d,gc_grey,(Word_t)e / GC_BASE);
                }
        }
        debugf("\n");
        // trace the grey
        while(ix = 0,(J1F(r,gc_grey,ix)),r) {
                debugf("Processing Grey: %p ",(void *)(ix * GC_BASE));
                J1U(r,gc_grey,ix);
                J1U(r,gc_allocated,ix);
                if(r == 0) {
                        debugf("Skipping.\n");
                        continue;
                }
                debugf("Blackening\n");
                J1S(r,gc_black,ix);

                entry_t *e = (entry_t *)(ix * GC_BASE);
                int offset = e->u.v.tag ? 1 : 0;
                for(int i = 0 + offset;i < e->u.v.nptrs + offset; i++) {
                        entry_t * ptr = e->ptrs[i];
                        if(SHOULD_FOLLOW(ptr)) {
                                ptr = FROM_SPTR(ptr);
                                debugf("Following: %p %p\n",e->ptrs[i], (void *)(ptr - 1));
                                Word_t p = (Word_t)(ptr - 1) / GC_BASE;
                                int r;
                                J1T(r,gc_black,p);
                                if(!r)
                                        J1S(r,gc_grey,p);
                        }

                }
        }

        ix = 0;
        Word_t w;
        // add any contents of the old list free list to our new one
        for((J1F(r,gc_free,ix)); r; (J1N(r,gc_free,ix))) {
                int d; J1S(d,gc_allocated,ix);
        }
        J1FA(w,gc_free);
        gc_free = gc_allocated;
        assert(gc_grey == NULL);
        gc_allocated = gc_black;
#if JGC_STATUS
        gc_print_stats(gc);
#endif
#if 0
        for((J1F(r,gc_free,ix)); r; (J1N(r,gc_free,ix))) {
                entry_t *e = (entry_t *)(ix * GC_BASE);
                mem_inuse -= (e->u.v.count + 1)*GC_BASE;
                J1U(r,gc_free,ix);
                free(e);
        }
#endif
        profile_pop(&gc_gc_time);
}


void *
gc_alloc_tag(gc_t gc,unsigned count, unsigned nptrs, int tag)
{
        profile_push(&gc_alloc_time);
        assert(nptrs <= count);
        int r;
        Word_t ix = 0;
        bool retried = false;
        size_t initial_mem = mem_inuse;
retry:
        for((J1F(r,gc_free,ix)); r; (J1N(r,gc_free,ix))) {
                entry_t *e = (entry_t *)(ix * GC_BASE);
                if(e->u.v.count == count) {
                        debugf("Reusing space: %p %i %i %i\n",(void *)e,count,nptrs,tag);
                        J1S(r,gc_allocated,ix);
                        J1U(r,gc_free,ix);
                        e->u.v.nptrs = nptrs;
                        e->u.v.tag = tag;
                        memset(e + 1,0,count*GC_BASE);
                        profile_pop(&gc_alloc_time);
                        return (void *)(e + 1);
                } else {
                        mem_inuse -= (e->u.v.count + 1)*GC_BASE;
                        J1U(r,gc_free,ix);
                        free(e);
                }
        }
        // if we didn't free up
        if(retried) {
        if(mem_inuse > ((heap_threshold * 7) / 10)) {
                heap_threshold *= 2;
#if JGC_STATUS
                fprintf(stderr, "Increasing heap threshold to %u bytes.\n", (unsigned) heap_threshold);
        } else {
                fprintf(stderr, "Freed %u bytes.\n", (unsigned) (initial_mem - mem_inuse));
#endif
        }
        }
        entry_t *e;
        if(mem_inuse < heap_threshold && (e = malloc((count + 1)*GC_BASE))) {
                mem_inuse += (count + 1)*GC_BASE;
                e->u.v.count = count;
                e->u.v.nptrs = nptrs;
                e->u.v.tag = tag;
                int r;
                debugf("allocated: %p %i %i %i\n",(void *)e, count, nptrs, tag);
                J1S(r,gc_allocated,(Word_t)e / GC_BASE);
                profile_pop(&gc_alloc_time);
                return (void *)(e + 1);
        } else {
                gc_perform_gc(gc);
                retried = true;
                goto retry;
        }
}


