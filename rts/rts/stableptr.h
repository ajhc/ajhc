#ifndef STABLEPTR_H
#define STABLEPTR_H

#include "rts/jhc_rts.h"

wptr_t c_newStablePtr(sptr_t c);
void c_freeStablePtr(wptr_t wp);
sptr_t c_derefStablePtr(wptr_t wp);
void hs_free_stable_ptr(HsStablePtr sp);
void hs_free_fun_ptr(HsFunPtr fp);

#endif /* STABLEPTR_H */
