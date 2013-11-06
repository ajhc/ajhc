#include <stdio.h>
#include <stdlib.h>
// return costant value
int get_int(void) {  return 42; }

// return constant value
const char* get_str(void) {
  return "So Long, and Thanks for All the Fish";
}

void* make_entry(void) {
  /* allocate some pointer */
  return (void*)0xdeadbeaf;
}

void dump(void* p) {
  if(p != 0xdeadbeaf) {
    printf("wow! someone overwrite my pointer!(%x)\n", p);
    abort();
  }
}
