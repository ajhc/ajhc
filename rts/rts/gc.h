#ifndef JHC_GC_H
#define JHC_GC_H

#define _JHC_GC_NONE   0
#define _JHC_GC_JGC    1
#define _JHC_GC_BOEHM  2
#define _JHC_GC_REGION 3

#ifndef _JHC_GC
#define _JHC_GC _JHC_GC_NONE
#endif

#include "rts/gc_none.h"
#include "rts/gc_jgc.h"

#if _JHC_GC == _JHC_GC_JGC
void jhc_alloc_init(gc_t *gc_p,arena_t *arena_p);
void jhc_alloc_fini(gc_t gc,arena_t arena);
#else
void jhc_alloc_init(void);
void jhc_alloc_fini(void);
#endif

#endif
