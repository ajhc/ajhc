#if _JHC_GC == _JHC_GC_JGC

extern void _start, _end;

#ifdef JHC_JGC_STACK
struct frame;
typedef struct frame *gc_t;
#else
typedef void* *gc_t;
static gc_t gc_stack_base;
#endif

static gc_t saved_gc;

#define GC_MINIMUM_SIZE 1
#define GC_BASE sizeof(void *)

#define TO_BLOCKS(x) ((x) <= GC_MINIMUM_SIZE*GC_BASE ? GC_MINIMUM_SIZE : (((x) - 1)/GC_BASE) + 1)

struct s_cache;
static void gc_perform_gc(gc_t gc);
static void *gc_alloc(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs);

#endif
