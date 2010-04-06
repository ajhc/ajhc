
#include <sys/queue.h>
#include <sys/param.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include "bitarray.h"
// #include "slub.h"

#define PAGESIZE 4096
#define ARENASIZE 65536

#define S_PAGE(val) (struct s_page *)((uintptr_t)(val) & ~ (PAGESIZE - 1))


static Pvoid_t  gc_inheap; // whether the page is a heap page

typedef uint16_t page_num_t;

struct s_arena {
        void *base;
        page_num_t next_free,num_used;
        bitarray_t used[BITARRAY_SIZE(ARENASIZE)];
        SLIST_HEAD(,s_cache) caches;
};

struct s_page {
        TAILQ_ENTRY(s_page) tailq;
        unsigned short num_free;
        unsigned short color;
        unsigned short size;
        unsigned short next_free;
        bitarray_t used[];
};

struct s_cache {
        SLIST_ENTRY(s_cache) next;
        struct s_arena *arena;
        TAILQ_HEAD(,s_page) pages;
        unsigned short num_entries;
        unsigned short size;
        unsigned short num_ptrs;
        unsigned short color;
};


void print_cache(struct s_cache *sc);
/* this finds a bit that isn't set, sets it, and returns it */

static int
bitset_find_free(int *next_free,int n,bitarray_t ba[static n]) {
        assert(*next_free < n);
        int i = *next_free;
        for(; i < n; i++) {
                if(~ba[i]) goto found;
        }
        for(i = 0; i < *next_free; i++) {
                if(~ba[i]) goto found;
        }
        return -1;
found:
        {
        int o = __builtin_ffsl(~ba[i]);
        assert(o);
        ba[i] |= (1UL << (o - 1));
        *next_free = i;
        return (i*BITS_PER_UNIT + (o - 1));
        }
}

/* page allocator */

static unsigned page_threshold = 4;

static struct s_page *
get_free_page(gc_t gc, struct s_arena *arena) {
        if(arena->num_used >= page_threshold) {
                gc_perform_gc(gc);
                // if we are stil using 80% of the heap after a gc, raise the threshold.
                if(arena->num_used * 10 >= page_threshold * 8) {
                        page_threshold *= 2;
                }
        }
        int next_free = arena->next_free;
        int found = bitset_find_free(&next_free, BITARRAY_SIZE(ARENASIZE), arena->used);
        arena->next_free = next_free;
        if(found == -1)
                return NULL;
        int r;
        J1S(r, gc_inheap, (uintptr_t)arena->base/PAGESIZE + found);
        arena->num_used++;
        return (struct s_page *)(arena->base + PAGESIZE*found);
}

static void
s_cleanup_pages(struct s_arena *arena) {
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                struct s_page *pg;
                TAILQ_FOREACH(pg,&sc->pages,tailq) {
                        if(pg->num_free == sc->num_entries) {
                                TAILQ_REMOVE(&sc->pages,pg,tailq);
                                BIT_UNSET(arena->used,((uintptr_t)pg - (uintptr_t)arena->base) / PAGESIZE);
                                arena->num_used--;
                        }
                }
        }
}



static void *
s_alloc(gc_t gc, struct s_cache *sc)
{
        struct s_page *pg;
        assert(sc);
        TAILQ_FOREACH(pg,&sc->pages,tailq)
                if(pg->num_free > 0)
                        break;
        if(__predict_false(!pg)) {
        pg = get_free_page(gc, sc->arena);
        assert(pg);
        pg->num_free = sc->num_entries;
        pg->color = sc->color;
        pg->size = sc->size;
        pg->next_free = 0;
        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(sc->num_entries));
        for(int i = BITARRAY_SIZE(sc->num_entries)*BITS_PER_UNIT - 1; i >= sc->num_entries; i--)
                BIT_SET(pg->used,i);
        TAILQ_INSERT_HEAD(&sc->pages,pg,tailq);
        }

        int next_free = pg->next_free;
        int found = bitset_find_free(&next_free,BITARRAY_SIZE(sc->num_entries),pg->used);
        pg->next_free = next_free;
        assert(found != -1);
        uintptr_t *pgp = (uintptr_t *)pg + pg->color;
        void *val = &pgp[found * (pg->size/sizeof(uintptr_t))];
        //        printf("s_alloc: val: %p s_page: %p size: %i color: %i found: %i num_free: %i\n", val, pg, pg->size, pg->color, found, pg->num_free);
        pg->num_free--;
        if(__predict_false(0 == pg->num_free)) {
                TAILQ_REMOVE(&sc->pages,pg,tailq);
                TAILQ_INSERT_TAIL(&sc->pages,pg,tailq);
        }
        assert(S_PAGE(val) == pg);
        return val;
}


static void
s_free(void *val)
{
        assert(val);
        struct s_page *pg = S_PAGE(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->color;
//        printf("s_free:  val: %p s_page: %p size: %i color: %i num_free: %i offset: %i bit: %i\n", val, pg, pg->size, pg->color, pg->num_free, offset, offset/pg->size);
        assert(BIT_VALUE(pg->used,offset/(pg->size/sizeof(uintptr_t))));
        BIT_UNSET(pg->used,offset/(pg->size/sizeof(uintptr_t)));
        pg->num_free++;
}


static struct s_cache *
new_cache(struct s_arena *arena, unsigned short size, unsigned short num_ptrs)
{
        struct s_cache *sc = malloc(sizeof(struct s_cache));
        sc->arena = arena;
        sc->size = size;
        size_t excess = PAGESIZE - sizeof(struct s_page);
        sc->num_entries = (8*excess) / (8*size + 1) - 1;
        //sc->num_entries = (8*excess) / (8*size*sizeof(uintptr_t) + 1);
        sc->color = (sizeof(struct s_page) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
        sc->num_ptrs = num_ptrs;
        TAILQ_INIT(&sc->pages);
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
                struct s_page *pg;
                TAILQ_FOREACH(pg,&sc->pages,tailq) {
                        pg->num_free = sc->num_entries;
                        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(sc->num_entries));
                        for(int i = BITARRAY_SIZE(sc->num_entries)*BITS_PER_UNIT - 1; i >= sc->num_entries; i--)
                                BIT_SET(pg->used,i);
                }
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
        // TODO - check if there are internal pointers
        struct s_page *pg = S_PAGE(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->color;
        if(BIT_IS_UNSET(pg->used,offset/(pg->size/sizeof(uintptr_t)))) {
                pg->num_free--;
                BIT_SET(pg->used,offset/(pg->size/sizeof(uintptr_t)));
                return true;
        }
        return false;
}

static struct s_cache *
find_cache(struct s_cache **rsc, struct s_arena *arena, unsigned short size, unsigned short num_ptrs)
{
        if(__predict_true(rsc && *rsc)) {
      //          printf("s_cached: %p\n", rsc);
                return *rsc;
        }
       // printf("s_new: %p\n", rsc);
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

struct s_arena *
new_arena(void) {
        struct s_arena *arena = malloc(sizeof(struct s_arena));
        SLIST_INIT(&arena->caches);
        int ret = posix_memalign(&arena->base,PAGESIZE,ARENASIZE*PAGESIZE);
        if(ret != 0) {
                fprintf(stderr,"Unable to allocate memory with posix_memalign\n");
                exit(1);
        }
        arena->next_free = 0;
        arena->num_used = 0;
        memset(arena->used,0,sizeof(arena->used));
        return arena;
}


void
print_cache(struct s_cache *sc) {
        printf("num_entries: %i\n",(int)sc->num_entries);
        printf("  entries: %i bytes\n",(int)(sc->num_entries*sc->size));
        printf("  header: %lu bytes\n", sizeof(struct s_page) + BITARRAY_SIZE_IN_BYTES(sc->num_entries));
     //   printf("excess: %i\n", PAGESIZE - sizeof(struct s_page) - sizeof(bitarray_t));
        printf("  size: %i bytes\n",(int)sc->size);
        printf("  color: %i words\n",(int)sc->color);
        printf("  color_off: %i bytes\n",(int)(sc->color*sizeof(uintptr_t)));
        printf("  end: %i bytes\n",(int)(sc->color*sizeof(uintptr_t) + sc->num_entries*sc->size));
        printf("%20s %9s %9s %9s %9s\n", "page", "num_free", "color", "size", "next_free");
        struct s_page *pg;
        TAILQ_FOREACH(pg,&sc->pages,tailq) {
            printf("%20p %9i %9i %9i %9i\n", pg, pg->num_free, pg->color, pg->size, pg->next_free);
        }
}

#ifdef SLAB_TEST

#define NUM_CACHES 15
#define FACTOR (1 << 16)

void
stress_test(int n) {
        struct s_arena *arena = new_arena();
        struct s_cache *caches[NUM_CACHES];

        void *ptrs[n];
        memset(ptrs,0,n*sizeof(void *));
        for(int i = 0; i < NUM_CACHES; i++)
                caches[i] = new_cache(arena,sizeof(void *)*(i + 1), 0);
        for(int i = 0; i < FACTOR * n; i++) {
                int wp = rand() % n;
                if (ptrs[wp]) {
                        s_free(ptrs[wp]);
                        //free(ptrs[wp]);
                        ptrs[wp] = NULL;
                } else {
                        ptrs[wp] = s_alloc(caches[rand() % NUM_CACHES]);
                        //ptrs[wp] = malloc((rand() % NUM_CACHES) * sizeof(uintptr_t));
                }
        }
}


int
main(int argc, char *argv[])
{

        setbuf(stdout,NULL);
        stress_test(1 << 8);
        struct s_arena *arena = new_arena();
        for(int i = 0;i < 10; i++) {
        struct s_cache *sc = new_cache(arena,i,0);
        print_cache(sc);
        }
        struct s_cache *sc1 = new_cache(arena,7,4);
        struct s_cache *sc2 = new_cache(arena,1,3);

        printf("Alloc1: %p\n", s_alloc(sc1));
        printf("Alloc1: %p\n", s_alloc(sc1));
        printf("Alloc1: %p\n", s_alloc(sc1));
        printf("Alloc2: %p\n", s_alloc(sc2));
        printf("Alloc2: %p\n", s_alloc(sc2));
        printf("Alloc2: %p\n", s_alloc(sc2));

        print_cache(sc1);
        print_cache(sc2);


        return 0;
}

#endif
