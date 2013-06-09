// dummy values for things the rts expects the compiled program to provide.

#include "jhc_rts_header.h"

char jhc_c_compile[] = "(compile)";
char jhc_command[] = "(command)";
char jhc_version[] = "(version)";

#if _JHC_GC == _JHC_GC_JGC
void jhc_hs_init(gc_t gc,arena_t arena) {}
#else
void jhc_hs_init(void) {}
#endif
const void * const nh_stuff[] = { NULL };
