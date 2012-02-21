#include "jhc_rts_header.h"

#define NUM_CACHES 15
#define FACTOR (1 << 16)

void
stress_test(int n) {
        jhc_alloc_init();
        struct s_arena *arena = new_arena();
        struct s_cache *caches[NUM_CACHES];

        void *ptrs[n];
        memset(ptrs,0,n*sizeof(void *));
        for(int i = 0; i < NUM_CACHES; i++)
                caches[i] = new_cache(arena,sizeof(void *)*(i + 1), 0);
        for(int i = 0; i < FACTOR * n; i++) {
                int wp = rand() % n;
                if (ptrs[wp]) {
                        //s_free(ptrs[wp]);
                        //free(ptrs[wp]);
                        ptrs[wp] = NULL;
                } else {
                        ptrs[wp] = s_alloc(saved_gc, caches[rand() % NUM_CACHES]);
                        //ptrs[wp] = malloc((rand() % NUM_CACHES) * sizeof(uintptr_t));
                }
        }
}

int
main(int argc, char *argv[])
{

        setbuf(stdout,NULL);
        stress_test(1 << 2);
        struct s_arena *arena = new_arena();
        for(int i = 0;i < 10; i++) {
        struct s_cache *sc = new_cache(arena,i,0);
        print_cache(sc);
        }
        struct s_cache *sc1 = new_cache(arena,7,4);
        struct s_cache *sc2 = new_cache(arena,1,3);

        printf("Alloc1: %p\n", s_alloc(saved_gc, sc1));
        printf("Alloc1: %p\n", s_alloc(saved_gc, sc1));
        printf("Alloc1: %p\n", s_alloc(saved_gc, sc1));
        printf("Alloc2: %p\n", s_alloc(saved_gc, sc2));
        printf("Alloc2: %p\n", s_alloc(saved_gc, sc2));
        printf("Alloc2: %p\n", s_alloc(saved_gc, sc2));

        print_cache(sc1);
        print_cache(sc2);

        return 0;
}
