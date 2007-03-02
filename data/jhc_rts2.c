
#include <assert.h>

#define ISLAZY(x)   (((uintptr_t)(x)) & 0x1)
#define DETAG(x)    ((uintptr_t)(x) & ~0x1)

#define GETHEAD(x)  (NODEP(x)->head)
#define GETWHAT(x)  (DNODEP(x)->what)
#define NODEP(x)    ((node_t *)(x))
#define DNODEP(x)   ((dnode_t *)(x))
#define EVALTAG(fn) (assert(((uintptr_t)(fn) & 0x3) == 0),(sptr_t)((uintptr_t)(fn) | P_LAZY))
#define VALUE(n)    ((wptr_t)(((uintptr_t)(n) << 2) | P_VALUE))
#define ISVALUE(n)  (assert(!ISLAZY(n)), ((uintptr_t)(n) & 0x2))

#define P_VALUE 0x2
#define P_WHNF  0x0
#define P_LAZY  0x1

#define BLACK_HOLE ((fptr_t)0xDEADBEEF)


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
 * |   lazy location   | 01|
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
 * |    code pointer   | 01|
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
 * fptr_t - a pointer to a whnf or a function pointer to something to evaluate, first value in a lazy location.
 * what_t  - the discriminator of a discriminated union
 *
 */

typedef struct node *  sptr_t;
typedef struct dnode * wptr_t;
typedef void *         fptr_t;
typedef unsigned       what_t;


typedef struct node {
        fptr_t head;
        sptr_t rest[];
} A_MAYALIAS node_t;

typedef struct dnode {
        what_t what;
        sptr_t rest[];
} A_MAYALIAS dnode_t;

#if _JHC_DEBUG
// these ensure the type synonyms are available to the debugger
uintptr_t _dummy1;
node_t *_dummy2;
dnode_t *_dummy3;
sptr_t *_dummy4;
fptr_t *_dummy5;
wptr_t *_dummy6;
#endif

typedef wptr_t (*eval_fn)(node_t *node) A_STD;

// fetch is like a cast, an 'eval' where you know the target is in WHNF
static inline wptr_t A_STD A_UNUSED
fetch(sptr_t s)
{
        assert(!ISLAZY(s));
        return (wptr_t)s;
}

// like eval but you know the target is in WHNF or is a already evaluated indirection
static inline wptr_t A_STD A_UNUSED
follow(sptr_t s)
{
        if(ISLAZY(s)) {
                sptr_t h = (sptr_t)(GETHEAD(DETAG(s)));
                assert(!ISLAZY(h));
                return (wptr_t)h;
        }
        return (wptr_t)s;
}

static inline wptr_t A_STD A_UNUSED
eval(sptr_t s)
{
        if(ISLAZY(s)) {
                void *ds = (void *)DETAG(s);
                sptr_t h = (sptr_t)(GETHEAD(ds));
                assert(h != BLACK_HOLE);
                if(ISLAZY(h)) {
                        eval_fn fn = (eval_fn)DETAG(h);
#if _JHC_DEBUG
                        GETHEAD(ds) = BLACK_HOLE;
#endif
                        wptr_t r = (*fn)(NODEP(ds));
#if _JHC_DEBUG
                        assert(GETHEAD(ds) != BLACK_HOLE);
#endif
                        return r;
                }
                return (wptr_t)h;
        }
        return (wptr_t)s;
}


static inline void A_STD A_UNUSED
update(sptr_t thunk, wptr_t new)
{
        jhc_update_inc();
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!ISLAZY(new));
        GETHEAD(thunk) = (fptr_t)new;
}




