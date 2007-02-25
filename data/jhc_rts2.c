
#include <assert.h>

#define PTAG(x)   (((uintptr_t)(x)) & 0x3)
#define TAGP(x,t) ((x) | (t))
#define NODEP(x)  ((node_t *)(x))
#define EVALTAG(fn) ((tag_t)((uintptr_t)(fn) | P_EVAL))

#define P_VALUE    0x3
#define P_INDIRECT 0x0

#define P_EVAL 0x1
#define P_WHNF 0x2



/* a pointer may be one of the following
 *
 * -------------------------
 * |    raw value      | 11|
 * -------------------------
 *
 *
 * -------------------------
 * |    heap location  | 00|
 * -------------------------
 *
 *  a heap location is always at least a single word long, its header word may
 *  be the following,
 *
 * -------------------------
 * |    raw value      | 11|
 * -------------------------
 *
 *
 * -------------------------
 * |    heap indirect  | 00|
 * -------------------------
 *
 * note that these two values have the exact same format as a pointer, so you
 * may always overwrite the first word of a heap location with a pointer.
 *
 * -------------------------
 * |    code pointer   | 01|
 * -------------------------
 * |     data ...          |
 *
 * something to evaluate, code pointer is a pointer to a function that takes
 * the memory location as its only argument, the called function is in charge
 * of updating the location if needed and it is assumed enough space is available
 * for whatever the location is updated with
 *
 *
 * -------------------------
 * |    data ...       | 10|
 * -------------------------
 * |     ...               |
 *
 * an object in WHNF. format is type specific
 *
 *
 * raw values are never pointers to the heap. data is filled in in a type
 * specific manner.
 *
 * note that a partial application is just like any other value but it happens
 * to contain a code pointer, and may be represented by a 01 or a 11 if in
 * WHNF and depending on whether it has data attached.
 *
 */


/*
 * type names
 *
 * sptr_t - a tagged smart pointer, may be a value, may be a pointer to a node
 * node_t - definitely a pointer to a node
 * tag_t - the first value in a node, has a tag indicating what it is
 *
 * there are several things a sptr_t can represent, often, it can be determined
 * statically what the possibilities are at any given point in the program
 *
 *
 * a literal value in WHNF
 * a pointer to a whnf node
 * a pointer to a thunk
 *
 * a thunk is always either one of
 * an unevaluated closure, a pointer to a function and its arguments
 *
 * or if evaluated, the first word is one of:
 * a pointer to something in WHNF
 * a literal value in WHNF
 *
 */

typedef struct node enode_t;
typedef struct node * sptr_t;
typedef uintptr_t tag_t;

typedef struct node {
        tag_t tag;
        sptr_t rest[];
} node_t;


typedef sptr_t (*eval_fn)(node_t *node);

// fetch is like a cast, an 'eval' where you know the target is in WHNF
static sptr_t A_UNUSED
fetch(sptr_t s)
{
    assert(PTAG(s) == P_VALUE ||  PTAG((sptr_t)(NODEP(s)->tag)) == P_WHNF);
    return s;
}

static sptr_t A_UNUSED
eval(sptr_t s)
{
        if(PTAG(s) == P_INDIRECT) {
                sptr_t h = (sptr_t)(NODEP(s)->tag);
                switch(PTAG(h)) {
                case P_INDIRECT:
                        return eval(h);
                case P_EVAL:
                        return ((eval_fn)NODEP((uintptr_t)h & ~0x3))(NODEP(s));
                case P_VALUE:
                        return h;
                default: // P_WHNF
                        return s;
                }
        } else {
                assert(PTAG(s) == P_VALUE);
                return s;
        }
}


static void A_UNUSED
update(sptr_t thunk, sptr_t new)
{
        assert(PTAG(thunk) == P_INDIRECT);
        assert(PTAG(NODEP(thunk)->tag) == P_EVAL);
        NODEP(thunk)->tag = (tag_t)new;
}



