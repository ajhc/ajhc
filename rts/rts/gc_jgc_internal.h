#ifndef GC_JGC_INTERNAL_H
#define GC_JGC_INTERNAL_H

#include "rts/gc_jgc.h"
#include "sys/bitarray.h"
#include "sys/queue.h"

#if _JHC_GC == _JHC_GC_JGC

typedef struct {
        sptr_t ptrs[0];
} entry_t;

struct stack {
        unsigned size;
        unsigned ptr;
        entry_t * *stack;
};

#define EMPTY_STACK { 0, 0, NULL }

struct s_caches_pub;

struct s_arena {
        SLIST_ENTRY(s_arena) link;
        struct s_megablock *current_megablock;
        SLIST_HEAD(,s_block) free_blocks;
        unsigned block_used;
        unsigned block_threshold;
        SLIST_HEAD(,s_cache) caches;
        SLIST_HEAD(,s_block) monolithic_blocks;
        SLIST_HEAD(,s_megablock) megablocks;
        unsigned number_gcs;    // number of garbage collections
        unsigned number_allocs; // number of allocations since last garbage collection
        gc_t gc_stack_base;
        struct stack root_stack;
// 7 to share caches with the first 7 tuples
#define GC_STATIC_ARRAY_NUM 7
#define GC_MAX_BLOCK_ENTRIES 150
        struct s_cache *array_caches[GC_STATIC_ARRAY_NUM];
        struct s_cache *array_caches_atomic[GC_STATIC_ARRAY_NUM];
        struct s_caches_pub *public_caches_p; // access from main_code.c
        int force_gc_next_s_alloc;
};

struct s_megablock {
        void *base;
        unsigned next_free;
        SLIST_ENTRY(s_megablock) next;
};

struct s_block {
        SLIST_ENTRY(s_block) link;
        unsigned char flags;  // defined in rts/constants.h
        unsigned char color;  // offset in words to first entry.
        union {
                // A normal block.
                struct {
                        unsigned char num_ptrs;
                        unsigned char size;
                        unsigned short num_free;
                        unsigned short next_free;
                } pi;
                // A monolithic block.
                struct {
                        unsigned num_ptrs;
                } m;
        } u;
        bitarray_t used[];
};

struct s_cache {
        SLIST_ENTRY(s_cache) next;
        SLIST_HEAD(,s_block) blocks;
        SLIST_HEAD(,s_block) full_blocks;
        unsigned char color;
        unsigned char size;
        unsigned char num_ptrs;
        unsigned char flags;
        unsigned short num_entries;
        struct s_arena *arena;
#if _JHC_PROFILE
        unsigned allocations;
#endif
};
#endif
#endif
