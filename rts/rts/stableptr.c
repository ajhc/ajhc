#include "sys/queue.h"

struct StablePtr {
    LIST_ENTRY(StablePtr) link;
    sptr_t contents;
};

LIST_HEAD(, StablePtr) root_StablePtrs = LIST_HEAD_INITIALIZER();

wptr_t c_newStablePtr(sptr_t c) {
    struct StablePtr* sp = malloc(sizeof(struct StablePtr));
    sp->contents = c;
    LIST_INSERT_HEAD(&root_StablePtrs, sp, link);
    assert(GET_PTYPE(sp) == 0);
    return (wptr_t)TO_SPTR(P_VALUE,(wptr_t)sp);
}

void c_freeStablePtr(wptr_t wp) {
    struct StablePtr *sp = FROM_SPTR((HsPtr)wp);
    LIST_REMOVE(sp, link);
    free(sp);
}

sptr_t c_derefStablePtr(wptr_t wp) {
    struct StablePtr *sp = FROM_SPTR((HsPtr)wp);
    return sp->contents;
}
