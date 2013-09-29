#include "jhc_rts_header.h"


struct my_entry {
  int n;
};

int
main(int argc, char *argv[])
{
  gc_t gc;
  arena_t arena;
  jhc_alloc_init(&gc, &arena);
  struct s_cache *sc = new_cache(arena,sizeof(struct my_entry),1);
  wptr_t ptr = s_alloc(gc, arena, sc);
  ((struct my_entry*)ptr)->n = 0x200;
  gc_add_root(gc, arena, ptr);

  gc_perform_gc(gc, arena);
  return 0;
}
