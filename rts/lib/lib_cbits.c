/* this file contains C only needed to help support the
 * standard libraries */

#include <stdio.h>

#include "HsFFI.h"
#include "rts/cdefs.h"

HsInt jhc_stdrnd[2] A_UNUSED = { 1 , 1 };
HsInt jhc_data_unique A_UNUSED;

HsBool A_UNUSED
jhc_wait_for_input(FILE *f,HsInt timeout) {
#if JHC_isPosix
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fileno(f),&fds);
        struct timeval to = {  0, timeout * 1000 };
        int retval = select(1,&fds,NULL,&fds,&to);
        if(retval)
                return HS_BOOL_TRUE;
        else
                return HS_BOOL_FALSE;
#else
        return HS_BOOL_FALSE;
#endif
}

uint32_t
jhc_hash32(uint32_t key)
{
  int c2=0x27d4eb2d; // a prime or an odd constant
  key = (key ^ 61) ^ (key >> 16);
  key = key + (key << 3);
  key = key ^ (key >> 4);
  key = key * c2;
  key = key ^ (key >> 15);
  return key;
}

uint64_t jhc_hash64(uint64_t key)
{
  key = (~key) + (key << 21); // key = (key << 21) - key - 1;
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); // key * 265
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); // key * 21
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}

uintptr_t
jhc_hashptr(uintptr_t key)
{
    if (sizeof(uintptr_t) == sizeof(uint32_t)) {
        return (uintptr_t)jhc_hash32((uint32_t)key);
    } else {
        return (uintptr_t)jhc_hash64((uint64_t)key);
    }
}
