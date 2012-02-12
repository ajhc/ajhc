# tests of the rts

CC=gcc
CFLAGS=-I.. -I../.. -std=gnu99 -g -O2

all: slab_test stableptr_test

slab_test: slab_test.c

clean:
	rm -f slab_test

test: all
	./slab_test
	./stableptr_test

stableptr_test: stableptr_test.c seatest.c
