# tests of the rts

CC=gcc
CFLAGS=-I.. -I../.. -std=gnu99 -g -O2  \
       -D_JHC_GC=_JHC_GC_JGC  -DJHC_UNIT -D_JHC_STANDALONE=0 \
       -DJHC_VALGRIND=1

TESTS=slab_test stableptr_test jgc_test
all: $(TESTS)

RTSFILES=hs_fake.c ../rts/profile.c ../rts/jhc_rts.c ../rts/gc_jgc.c \
	 ../rts/stableptr.c ../rts/gc_none.c ../rts/rts_support.c

clean:
	rm -f $(TESTS)

test: all
	./slab_test
	./stableptr_test
	./jgc_test

stableptr_test: stableptr_test.c seatest.c  $(RTSFILES)
slab_test: slab_test.c $(RTSFILES)
jgc_test:  jgc_test.c seatest.c $(RTSFILES)
