char jhc_c_compile[] = "(compile)";
char jhc_command[] = "(command)";
char jhc_version[] = "(version)";

#define JHC_VALGRIND 1

#define _JHC_STANDALONE 0
#define _JHC_GC _JHC_GC_JGC

#include "jhc_rts_header.h"
#include "jhc_jgc.h"
#include "debug.c"
#include "jhc_rts_alloc.c"
#include "jhc_rts.c"
#include "profile.c"
#include "jhc_rts2.c"
#include "slub.c"
#include "jhc_jgc.c"
#include "rts/stableptr.c"

#include "seatest.h"

static void jhc_hs_init(void) {}
static const void * const nh_stuff[] = { NULL };

#define SAMPLE_SPTR (sptr_t)TO_SPTR(P_VALUE,0xF0D0);

int num_stableptrs(void) {
        int count = 0;
        struct StablePtr *sp;
        LIST_FOREACH(sp, &root_StablePtrs, link)
            count++;
        return count;
}

bool in_stableptr_list(struct StablePtr *spi) {
        int count = 0;
        struct StablePtr *sp;
        LIST_FOREACH(sp, &root_StablePtrs, link)
            if (sp == spi)
                    return true;
        return false;
}

void stableptr_test(void) {
        assert_int_equal(0, num_stableptrs());
        sptr_t sptr = SAMPLE_SPTR;
        wptr_t wptr = c_newStablePtr(sptr);
        assert_int_equal(1, num_stableptrs());
        assert_true(c_derefStablePtr(wptr) == sptr);
        c_freeStablePtr(wptr);
        assert_int_equal(0, num_stableptrs());
}

int main(int argc, const char *argv[])
{
        test_fixture_start();
        run_test(stableptr_test);
        test_fixture_end();
        return 0;
}
