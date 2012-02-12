#ifndef RTS_CONSTANTS_H
#define RTS_CONSTANTS_H
/* these constants are shared between jhc-prim and the rts. */

// Normal memory block.
#define SLAB_FLAG_NONE      0

// Each element has a finalizer-list as its second word.
#define SLAB_FLAG_FINALIZER 1

// In addition to whatever other finalization is done, 'free' should be called
// on the first word of each entry.
#define SLAB_FLAG_FREE      2

// Finalizers should be delayed until entire slab is freed up and individually
// freed members need not be kept track of.
#define SLAB_FLAG_DELAY     4

#endif
