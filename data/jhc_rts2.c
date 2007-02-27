
#include <assert.h>

#define ISLAZY(x)   (((uintptr_t)(x)) & 0x1)
#define DETAG(x)    ((uintptr_t)(x) & ~0x3)

#define GETHEAD(x)  (NODEP(x)->tag)
#define NODEP(x)    ((node_t *)(x))
#define EVALTAG(fn) (assert(((uintptr_t)(fn) & 0x3) == 0),(tag_t)((uintptr_t)(fn) | P_LAZY))

#define P_VALUE 0x2
#define P_WHNF  0x0
#define P_LAZY  0x1

#define BLACK_HOLE 0xDEADBEEF


/* a value may be one of the following and is represented by a sptr_t
 *
 * -------------------------
 * |    raw value      | 10|
 * -------------------------
 *
 * -------------------------
 * |    whnf location  | 00|
 * -------------------------
 *
 * -------------------------
 * |   lazy location   | u1|
 * -------------------------
 *
 * whnf and raw value field formats are completly determined by the data type
 * and need not conform to any particular format.
 *
 *  a lazy location is always at least a single word long which is always one
 *  of the following
 *
 * -------------------------
 * |    raw value      | 10|
 * -------------------------
 *
 * -------------------------
 * |    whnf location  | 00|
 * -------------------------
 *
 *  which are interpreted exactly as above or
 *
 * -------------------------
 * |    code pointer   | u1|
 * -------------------------
 * |     data ...          |
 *
 * something to evaluate, code pointer is a pointer to a function that takes
 * the memory location as its only argument, the called function is in charge
 * of updating the location if needed.
 *
 * note that it is invalid to have a lazy location point to another lazy
 * location. there is only ever one level of indirection allowed, and only from
 * lazy locations
 *
 * note that a partial application is just like any other value in WHNF as far
 * as the above is concered. It happens to possibly contain a code pointer.
 *
 * u bits are undefined.
 *
 */


/*
 * type names
 *
 * sptr_t - a tagged smart pointer, may be a value, may be a pointer to a whnf or lazy location
 * wptr_t - a value guarenteed to be in whnf
 * node_t - definitely a pointer to a lazy location
 * tag_t - the first value in a lazy location, has a tag indicating what it is
 *
 */

typedef struct node enode_t;
typedef struct node * sptr_t;
typedef struct node * wptr_t;
typedef uintptr_t tag_t;

typedef struct node {
        tag_t tag;
        sptr_t rest[];
} node_t;


typedef sptr_t (*eval_fn)(node_t *node) A_STD;

// fetch is like a cast, an 'eval' where you know the target is in WHNF
static wptr_t A_STD A_UNUSED
fetch(sptr_t s)
{
        assert(!ISLAZY(s));
        return s;
}

static wptr_t A_STD A_UNUSED
eval(sptr_t s)
{
        if(ISLAZY(s)) {
                void *ds = (void *)DETAG(s);
                sptr_t h = (sptr_t)(GETHEAD(ds));
                assert(h != BLACK_HOLE);
                if(ISLAZY(h)) {
                        eval_fn fn = (eval_fn)DETAG(h);
#ifndef NDEBUG
                        GETHEAD(ds) = BLACK_HOLE;
#endif
                        wptr_t r = (*fn)(NODEP(ds));
#ifndef NDEBUG
                        assert(GETHEAD(ds) != BLACK_HOLE);
#endif
                        return r;
                }
                return h;
        }
        return s;
}


static void A_STD A_UNUSED
update(sptr_t thunk, wptr_t new)
{
        update_inc();
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!ISLAZY(new));
        GETHEAD(thunk) = (tag_t)new;
}



