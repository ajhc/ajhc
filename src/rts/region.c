
#elif _JHC_GC == _JHC_GC_REGION

#undef _JHC_GC_CONTEXT
#define _JHC_GC_CONTEXT 1


typedef unsigned jhc_gc_context_t;

#include <sys/queue.h>

static inline void jhc_malloc_init(void) { return; }

// Region chunk size
#define JHC_MEM_CHUNK_SIZE (1 << 12)


struct region {
        SLIST_HEAD(,region_page) pages;
        unsigned offset;
};

struct region_page {
        SLIST_ENTRY(region_page) next;
        uintptr_t data[];
};


#define JHC_REGION_STACK_SIZE  1024
static struct region region_stack[JHC_REGION_STACK_SIZE];

SLIST_HEAD(,region_page) free_pages;


static struct region_page *
new_region_page(unsigned current_region) {
        if(SLIST_EMPTY(&free_pages)) {

                unsigned cr = current_region;
                while(cr < JHC_REGION_STACK_SIZE && !SLIST_EMPTY(&region_stack[cr].pages)) {
                        struct region_page *next = SLIST_FIRST(&region_stack[cr].pages);
                        SLIST_FIRST(&region_stack[cr].pages) = NULL;
                        while(next) {
                                struct region_page *nnext = SLIST_NEXT(next,next);
                                SLIST_INSERT_HEAD(&free_pages, next, next);
                                next = nnext;
                        }
                        cr++;
                }
                if(cr == current_region) {
                        return malloc(JHC_MEM_CHUNK_SIZE);
                }
        }
        struct region_page *r = SLIST_FIRST(&free_pages);
        SLIST_REMOVE_HEAD(&free_pages,next);
        return r;
}

static struct region *
new_region(unsigned current_region) {
        struct region *r = &region_stack[current_region];
        if(SLIST_EMPTY(&r->pages)) {
                r->offset = JHC_MEM_CHUNK_SIZE;
                return r;
        } else {
                // if the region already has some pages allocated, then
                // we reuse one of them and return the rest to the free list.
                struct region_page *next = SLIST_NEXT(SLIST_FIRST(&r->pages),next);
                if(next) {
                        SLIST_NEXT(SLIST_FIRST(&r->pages), next) = NULL;
                        while(next) {
                                struct region_page *nnext = SLIST_NEXT(next,next);
                                SLIST_INSERT_HEAD(&free_pages, next, next);
                                next = nnext;
                        }
                }
                r->offset = sizeof(struct region_page);
                return r;
        }
}



static inline void * A_MALLOC
jhc_malloc_region(unsigned current_region, struct region *r, size_t n) {
        n = ALIGN(sizeof(void *),n);
        struct region_page *rp;
        if (n > (JHC_MEM_CHUNK_SIZE - r->offset)) {
                rp = new_region_page(current_region);
                SLIST_INSERT_HEAD(&r->pages, rp, next);
                r->offset = sizeof(struct region_page);
        } else {
                rp = SLIST_FIRST(&r->pages);
        }


        void *ret = (void *)rp + r->offset;
        r->offset += n;
        return ret;
}





#endif
