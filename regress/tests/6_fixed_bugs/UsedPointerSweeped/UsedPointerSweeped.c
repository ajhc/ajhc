#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static int count = 0;

// return costant value
int get_int(void) {  return 42; }

// return constant value
const char *get_str(void) {
	return "So Long, and Thanks for All the Fish";
}

void *make_entry(void) {
	/* allocate some pointer */
	return (void *)0xdeadbeaf;
}

void dump(void* p) {
	uint32_t p1 = (uint32_t) make_entry();
	uint32_t p2 = (uint32_t) p;

	if (count++ > 1000) {
		exit(0);
	}
	if (p1 != p2) {
		printf("wow! someone overwrite my pointer!(0x%x != 0x%x)\n", p1, p2);
		abort();
	}
}
