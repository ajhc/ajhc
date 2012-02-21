#include "jhc_rts_header.h"
#include "rts/gc_jgc_internal.h"
#include "rts/constants.h"
#include "seatest.h"

bool
block_aligned(void *p) {
        return (S_BLOCK(p) == p);
}

void
block_sanity(struct s_arena *arena, struct s_cache *sc, struct s_block *b) {
        assert_true (!sc == !!(b->flags & SLAB_MONOLITH));
        assert_true (block_aligned(b));
}

void
cache_sanity(struct s_arena *arena, struct s_cache *sc) {
        struct s_block *b;
        assert_true(sc->arena == arena);
        SLIST_FOREACH(b, &sc->blocks, link)
            block_sanity(arena, sc, b);
}

void
arena_sanity(struct s_arena *arena) {
        assert_true(!!arena);
        struct s_cache *sc;
        struct s_block *b;
        SLIST_FOREACH(sc, &arena->caches, next)
            cache_sanity(arena, sc);
        SLIST_FOREACH(b, &arena->monolithic_blocks, link)
            block_sanity(arena, NULL, b);

}

#define PTR1 (HsPtr)0xDEADBEEF
#define PTR2 (HsPtr)0xB00B1E5

void foreignptr_test(void) {
        gc_t gc = saved_gc;
        HsPtr **ptr = gc_new_foreignptr(PTR1);
        assert_ptr_equal(PTR1, ptr[0]);
        assert_ptr_equal(NULL, ptr[1]);
        assert_true(gc_add_foreignptr_finalizer((sptr_t)ptr, PTR2));
        assert_bit_mask_matches(get_heap_flags(ptr), SLAB_FLAG_FINALIZER);
        assert_ptr_equal(PTR1, ptr[0]);
        assert_true(!!ptr[1]);
        assert_ptr_equal(ptr[1][0], PTR2);
        arena_sanity(arena);
}

void basic_test(void) {
        arena_sanity(arena);
        gc_t gc = saved_gc;
        heap_t e = gc_alloc(gc, NULL, 2, 2);
        ((void **)e)[0] = e;
        ((void **)e)[1] = e;
        gc[0] = e;
        arena_sanity(arena);
        gc_perform_gc(gc + 1);
        arena_sanity(arena);
}

int main(int argc, char *argv[])
{
        hs_init(&argc, &argv);
        test_fixture_start();
        run_test(basic_test);
        run_test(foreignptr_test);
        test_fixture_end();
        hs_exit();
        return 0;
}
