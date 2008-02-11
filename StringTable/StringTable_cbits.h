#ifndef ST_CBITS_H
#define ST_CBITS_H

#include <_stdint.h>

#define MAX_ENTRY_SIZE 256
#define VALID_BITMASK 0x1

// always shift, then mask
#define ATOM_LEN_MASK 0xff
#define ATOM_LEN_SHIFT 1

typedef uint32_t atom_t;


atom_t stringtable_lookup(unsigned char *cs, int len);
void stringtable_stats(void);
int stringtable_find(atom_t cl, unsigned char **res);
unsigned char *stringtable_ptr(atom_t cl);


#endif
