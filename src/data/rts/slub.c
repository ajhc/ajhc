#ifdef JHC_RTS_INCLUDE
#undef JHC_RTS_INCLUDE
#include "slub.c"
#define JHC_RTS_INCLUDE
#else
#if _JHC_GC == _JHC_GC_JGC

#define PAGESIZE  4096
#define ARENASIZE 65536

#define S_PAGE(val) (struct s_page *)((uintptr_t)(val) & ~ (PAGESIZE - 1))

static Pvoid_t  gc_inheap; // whether the page is a heap page

typedef uint16_t page_num_t;

struct s_arena {
        void *base;
        SLIST_HEAD(,s_cache) caches;
        page_num_t next_free,num_used;
        SLIST_HEAD(,s_page) free_pages;
        bitarray_t used[BITARRAY_SIZE(ARENASIZE)];
};


struct s_page_info {
        unsigned char color;
        unsigned char size;
        unsigned char tag;
        unsigned char num_ptrs;
};

struct s_page {
        SLIST_ENTRY(s_page) link;
        struct s_page_info pi;
        unsigned short num_free;
        unsigned short next_free;
        bitarray_t used[];
};

struct s_cache {
        SLIST_ENTRY(s_cache) next;
        SLIST_HEAD(,s_page) pages;
        SLIST_HEAD(,s_page) full_pages;
        struct s_page_info pi;
        unsigned short num_entries;
        unsigned short num_ptrs;
        struct s_arena *arena;
};


/* This finds a bit that isn't set, sets it, then returns its index.  It
 * assumes that a bit is available to be found, otherwise it goes into an
 * infinite loop. */

static unsigned
bitset_find_free(unsigned *next_free,int n,bitarray_t ba[static n]) {
        assert(*next_free < n);
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

/* page allocator */

static unsigned page_threshold = 8;

static struct s_page *
get_free_page(gc_t gc, struct s_arena *arena) {
        if(__predict_true(SLIST_FIRST(&arena->free_pages))) {
                struct s_page *pg = SLIST_FIRST(&arena->free_pages);
                SLIST_REMOVE_HEAD(&arena->free_pages,link);
                arena->num_used++;
                return pg;
        } else {
                if((arena->num_used >= page_threshold)) {
                        gc_perform_gc(gc);
                        // if we are still using 80% of the heap after a gc, raise the threshold.
                        if(__predict_false((unsigned)arena->num_used * 10 >= page_threshold * 9)) {
                                page_threshold *= 2;
                        }
                }
                unsigned next_free = arena->next_free;
                unsigned found = bitset_find_free(&next_free, BITARRAY_SIZE(ARENASIZE), arena->used);
                arena->next_free = next_free;
                int r; J1S(r, gc_inheap, (uintptr_t)arena->base/PAGESIZE + found);
                arena->num_used++;
                struct s_page *pg = (struct s_page *)(arena->base + PAGESIZE*found);
                pg->num_free = 0;
                return pg;
        }
}

static void
s_cleanup_pages(struct s_arena *arena) {
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {

                // 'best' keeps track of the page with the fewest free spots
                // and percolates it to the front, effectively a single pass
                // of a bubblesort to help combat fragmentation. It does
                // not increase the complexity of the cleanup algorithm as
                // we had to scan every page anyway, but over many passes
                // of the GC it will eventually result in a more sorted list
                // than would occur by chance.

                struct s_page *best = NULL;
                int free_best = 4096;
                struct s_page *pg = SLIST_FIRST(&sc->pages);
                struct s_page *fpg = SLIST_FIRST(&sc->full_pages);
                SLIST_INIT(&sc->pages);
                SLIST_INIT(&sc->full_pages);
                if(!pg) {
                        pg = fpg;
                        fpg = NULL;
                }
                while(pg) {
                        struct s_page *npg = SLIST_NEXT(pg,link);
                        if(__predict_false(pg->num_free == 0)) {
                                SLIST_INSERT_HEAD(&sc->full_pages,pg,link);
                        } else if(__predict_true(pg->num_free == sc->num_entries)) {
                                arena->num_used--;
                                SLIST_INSERT_HEAD(&arena->free_pages,pg,link);
                        } else {
                                if(!best) {
                                        free_best = pg->num_free;
                                        best = pg;
                                } else {
                                        if(pg->num_free < free_best) {
                                                struct s_page *tmp = best;
                                                best = pg; pg = tmp;
                                                free_best = pg->num_free;
                                        }
                                        SLIST_INSERT_HEAD(&sc->pages,pg,link);
                                }
                        }
                        if(!npg && fpg) {
                                pg = fpg;
                                fpg = NULL;
                        } else
                                pg = npg;
                }
                if(best)
                        SLIST_INSERT_HEAD(&sc->pages,best,link);
        }
}

inline static void
clear_page_used_bits(unsigned num_entries, struct s_page *pg)
{
        pg->num_free = num_entries;
        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));
        int excess = num_entries % BITS_PER_UNIT;
        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);
}

static void *
s_alloc(gc_t gc, struct s_cache *sc)
{
        struct s_page *pg = SLIST_FIRST(&sc->pages);
        if(__predict_false(!pg)) {
                pg = get_free_page(gc, sc->arena);
                assert(pg);
                pg->pi = sc->pi;
                pg->next_free = 0;
                SLIST_INSERT_HEAD(&sc->pages,pg,link);
                if(sc->num_entries != pg->num_free)
                        clear_page_used_bits(sc->num_entries, pg);
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
                        assert(pg == SLIST_FIRST(&sc->pages));
                        SLIST_REMOVE_HEAD(&sc->pages,link);
                        SLIST_INSERT_HEAD(&sc->full_pages,pg,link);
                }
                assert(S_PAGE(val) == pg);
                //printf("s_alloc: val: %p s_page: %p size: %i color: %i found: %i num_free: %i\n", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);
                return val;
        }
}


/*
static void
s_free(void *val)
{
        assert(val);
        struct s_page *pg = S_PAGE(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;
//        printf("s_free:  val: %p s_page: %p size: %i color: %i num_free: %i offset: %i bit: %i\n", val, pg, pg->pi.size, pg->pi.color, pg->num_free, offset, offset/pg->pi.size);
        assert(BIT_VALUE(pg->used,offset/(pg->pi.size)));
        BIT_UNSET(pg->used,offset/(pg->pi.size));
        pg->num_free++;
}
*/


static struct s_cache *
new_cache(struct s_arena *arena, unsigned short size, unsigned short num_ptrs, unsigned char tag)
{
        struct s_cache *sc = malloc(sizeof(struct s_cache));
        sc->arena = arena;
        sc->pi.size = size;
        sc->pi.tag = tag;
        sc->pi.num_ptrs = num_ptrs;
        size_t excess = PAGESIZE - sizeof(struct s_page);
        sc->num_entries = (8*excess) / (8*sizeof(uintptr_t)*size + 1) - 1;
        //sc->num_entries = (8*excess) / (8*size*sizeof(uintptr_t) + 1);
        sc->pi.color = (sizeof(struct s_page) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
        SLIST_INIT(&sc->pages);
        SLIST_INIT(&sc->full_pages);
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
                struct s_page *pg = SLIST_FIRST(&sc->pages);
                struct s_page *fpg = SLIST_FIRST(&sc->full_pages);
                do {
                        for(;pg;pg = SLIST_NEXT(pg,link))
                                clear_page_used_bits(sc->num_entries,pg);
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
        struct s_page *pg = S_PAGE(val);
        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;
        if(__predict_true(BIT_IS_UNSET(pg->used,offset/pg->pi.size))) {
                BIT_SET(pg->used,offset/pg->pi.size);
                pg->num_free--;
                return (bool)pg->pi.num_ptrs;
        }
        return false;
}

static struct s_cache *
find_cache(struct s_cache **rsc, struct s_arena *arena, unsigned short size, unsigned short num_ptrs, unsigned char tag)
{
        if(__predict_true(rsc && *rsc))
                return *rsc;
        struct s_cache *sc = SLIST_FIRST(&arena->caches);
        for(;sc;sc = SLIST_NEXT(sc,next)) {
                if(sc->pi.size == size && sc->pi.num_ptrs == num_ptrs && sc->pi.tag == tag)
                        goto found;
        }
        sc = new_cache(arena,size,num_ptrs, tag);
found:
        if(rsc)
                *rsc = sc;
        return sc;
}

struct s_arena *
new_arena(void) {
        struct s_arena *arena = malloc(sizeof(struct s_arena));
        SLIST_INIT(&arena->caches);
        SLIST_INIT(&arena->free_pages);
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
//        printf("  entries: %i words\n",(int)(sc->num_entries*sc->pi.size));
        printf("  header: %lu bytes\n", sizeof(struct s_page) + BITARRAY_SIZE_IN_BYTES(sc->num_entries));
     //   printf("excess: %i\n", PAGESIZE - sizeof(struct s_page) - sizeof(bitarray_t));
        printf("  size: %i words\n",(int)sc->pi.size);
//        printf("  color: %i words\n",(int)sc->pi.color);
        printf("  nptrs: %i words\n",(int)sc->pi.num_ptrs);
        printf("  tag: %i words\n",(int)sc->pi.tag);
//        printf("  color_off: %i bytes\n",(int)(sc->pi.color*sizeof(uintptr_t)));
//        printf("  end: %i bytes\n",(int)(sc->pi.color+ sc->num_entries*sc->pi.size)*sizeof(uintptr_t));
        printf("%20s %9s %9s\n", "page", "num_free", "next_free");
        struct s_page *pg;
        SLIST_FOREACH(pg,&sc->pages,link) {
            printf("%20p %9i %9i\n", pg, pg->num_free, pg->next_free);
        }
        printf("  full_pages:\n");
        SLIST_FOREACH(pg,&sc->full_pages,link) {
            printf("%20p %9i %9i\n", pg, pg->num_free, pg->next_free);
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
#endif
#endif
