#include "jhc_rts_header.h"
#include "sys/queue.h"

#include "seatest.h"

#define SAMPLE_SPTR (sptr_t)TO_SPTR(P_VALUE,0xF0D0);

int num_stableptrs(void)
{
        int count = 0;
        struct StablePtr *sp;
        LIST_FOREACH(sp, &root_StablePtrs, link)
        count++;
        return count;
}

bool in_stableptr_list(struct StablePtr *spi)
{
        int count = 0;
        struct StablePtr *sp;
        LIST_FOREACH(sp, &root_StablePtrs, link)
        if (sp == spi)
                return true;
        return false;
}

void stableptr_test(void)
{
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
