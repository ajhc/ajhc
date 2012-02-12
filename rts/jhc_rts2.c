/*@Internals

# The Run Time System

Jhc is very minimalist in that it does not have a precompiled run time system,
but rather generates what is needed as part of the compilation process.
However, back ends do have specific run-time representations of data, which can
be affected by things like the choice of garbage collector. The following
describes the general layout for the C based back-ends, but compiler options
such as garbage collection method or whether we do full program analysis, will
affect which features are used and whether certain optimized layouts are
possible.

Unboxed values directly translate to values in the target language, an unboxed
Int will translate directly into an 'int' as an argument and an unboxed pointer
will be a raw pointer. Unboxed values have no special interpretation and are
_not_ followed by the garbage collector. If the target language does not
support a feature such as multiple return values, it will have to be simulated.
It would not be wrong to think of Grin code that only deals with unboxed values
to be isomorphic to C-- or C augmented with multiple return values.

Boxed values have a standard representation and can be followed. Unlike some
other implementation, being boxed does not imply the object is located on the
heap. It may be on the stack, heap, or even embedded within the smart pointer
itself. Being boxed only means that the object may be represented by a smart
pointer, which may or may not actually be a pointer in the traditional sense.

A boxed value in jhc is represented by a 'smart pointer' of c type sptr_t. a
smart pointer is the size of a native pointer, but can take on different roles
depending on a pair of tag bits, called the ptype.

smart pointers take on a general form as follows:

    -------------------------
    |    payload        | GL|
    -------------------------

      G - if set, then the garbage collector should not treat value as a pointer to be followed
      L - lazy, this bit being set means the value is potentially not in WHNF

A sptr_t on its own in the wild can only take on one of the following forms:

    -------------------------
    |    whnf raw value | 10|
    -------------------------

    -------------------------
    |    whnf location  | 00|
    -------------------------

WHNF stands for 'Weak Head Normal Form' and means that the value is not a
suspended function and hence not a pointer to a thunk. It may be directly
examined and need not be evaluated. wptr_t is an alias for sptr_t that is
guarenteed to be of one of the above forms. It is used to improve safety for
when we can statically know that a value is WHNF and hence we can skip the
expensive 'eval'.

The difference between the raw value and the whnf location is that the first
contains uninterpreted bits, while the second is a pointer to a location on the
heap or stack and hence the garbage collector should follow it. The format of
the memory pointed to by the whnf location is unspecified and dependent on the
actual type being represented.

Partial (unsaturated) applications are normal WHNF values. Saturated
applications which may be 'eval'ed and updated are called thunks and must not
be pointed to by WHNF pointers. Their representation follows.

    -------------------------
    |   lazy location   | 01|
    -------------------------

A lazy location points to either a thunk, or a redirection to a WHNF value. A
lazy location is always a pointer to an allocated block of memory which always
begins with a restricted smart pointer. This restricted smart pointer is represented by
the C type alias 'fptr_t'. fptr_t's only occur as the first entry in a lazy
location, they never are passed around as objects in their own right.

A fptr_t may be a whnf value or a code pointer. If a fptr_t is a whnf value (of one of
the two forms given above) then it is called a redirection, the lazy location should be
treated exactly as if it were the whnf given. This is used to redirect an evaluated
thunk to its computed value.

A fptr_t may also be a 'code pointer' in which case the lazy location is called
a thunk. A code pointer is a pointer to executable machine code that evaluates
a closure and returns a wptr_t, the returned wptr_t is then generally written
over the code pointer, turning the thunk into a redirection. It is the
responsibility of the code pointed to to perform this redirection.

    -------------------------
    |    code pointer   | 11|
    -------------------------
    |     data ...          |

When debugging, the special code pointer BLACK_HOLE is also sometimes stored in
a fptr_t to detect certain run-time errors.

Note that unlike other implementations, a fptr_t may _not_ be another lazy
location. you can not have chained redirections, a redirection is always a
redirection to a whnf value.

    sptr_t - a tagged smart pointer, may contain a whnf value or a lazy location.
    wptr_t - a tagged smart pointer that contains a whnf value (either raw or a location)
    fptr_t - a tagged smart pointer, may contain a whnf value indicating a redirection, or a code pointer indicating a thunk.

*/

#define P_WHNF  0x0
#define P_LAZY  0x1
#define P_VALUE 0x2
#define P_FUNC  0x3

#define IS_LAZY(x)     (bool)(((uintptr_t)(x)) & 0x1)
#define IS_PTR(x)      (bool)(!(((uintptr_t)(x)) & 0x2))

#define FROM_SPTR(x)   (typeof (x))((uintptr_t)(x) & ~0x3)  // remove a ptype from a smart pointer
#define GET_PTYPE(x)   ((uintptr_t)(x) & 0x3)               // return the ptype associated with a smart pointer
#define TO_SPTR(t,x)   (typeof (x))((uintptr_t)(x) | (t))   // attach a ptype to a smart pointer
#define TO_SPTR_C(t,x) (typeof (x))((uintptr_t)(x) + (t))   // attach a ptype to a smart pointer, suitable for use by constant initialializers

#define GETHEAD(x)   (NODEP(x)->head)
#define NODEP(x)     ((node_t *)(x))
#define DNODEP(x)    ((dnode_t *)(x))

#define MKLAZY(fn)    TO_SPTR(P_LAZY,(sptr_t)fn)
#define MKLAZY_C(fn)  TO_SPTR_C(P_LAZY,(sptr_t)fn)
#define TO_FPTR(fn)   TO_SPTR_C(P_FUNC,(fptr_t)fn)

#define RAW_SET_F(n)   ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))
#define RAW_SET_UF(n)  ((wptr_t)(((uintptr_t)(n) << 2) | P_VALUE))
#define RAW_GET_F(n)   ((intptr_t)(n) >> 2)
#define RAW_GET_UF(n)  ((uintptr_t)(n) >> 2)

#define RAW_SET_16(w)  (wptr_t)(((uintptr_t)(w) << 16) | P_VALUE)
#define RAW_GET_16(n)  ((intptr_t)(n) >> 16)
#define RAW_GET_U16(n) ((uintptr_t)(n) >> 16)

// demote is always safe, we must only promote when we know the argument is a WHNF
#define PROMOTE(n)   ((wptr_t)(n))
#define DEMOTE(n)    ((sptr_t)(n))

#define FETCH_TAG(x)      RAW_GET_U16(IS_PTR(x) ? FETCH_MEM_TAG(x) : (what_t)(x))
#define FETCH_RAW_TAG(x)  RAW_GET_U16(x)
#define SET_RAW_TAG(x)    RAW_SET_16(x)
#define FETCH_MEM_TAG(x)  (DNODEP(x)->what)
#define SET_MEM_TAG(x,v)  (DNODEP(x)->what = (what_t)RAW_SET_16(v))

#define BLACK_HOLE TO_FPTR(0xDEADBEE0)

struct sptr {};
struct wptr {};
struct fptr {};

// we use dummy structs here so the compiler will catch any attempt
// to use one type in anothers place
typedef struct sptr * sptr_t;
typedef struct sptr * wptr_t;
typedef struct fptr * fptr_t;
typedef uintptr_t     what_t;

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

static bool A_UNUSED
jhc_valid_whnf(wptr_t s)
{
        return ((GET_PTYPE(s) == P_VALUE) || ((GET_PTYPE(s) == P_WHNF) && jhc_malloc_sanity(s,P_WHNF)));
}

static bool A_UNUSED
jhc_valid_lazy(sptr_t s)
{
        if(jhc_valid_whnf((wptr_t)s))
                return true;
        assert(GET_PTYPE(s) == P_LAZY);
        node_t *ds = (node_t *)FROM_SPTR(s);
        assert(jhc_malloc_sanity(ds,P_LAZY));
        if(IS_LAZY(ds->head)) {
                if(ds->head == BLACK_HOLE) return true;
                assert(GET_PTYPE(ds->head) == P_FUNC);
                return true;
        } else
                return jhc_valid_whnf((wptr_t)ds->head);
}

#else

#define jhc_valid_whnf(x) true
#define jhc_valid_lazy(x) true

#endif

#if _JHC_GC == _JHC_GC_JGC
typedef wptr_t (*eval_fn)(gc_t gc,node_t *node) A_STD;
#else
typedef wptr_t (*eval_fn)(node_t *node) A_STD;
#endif

// both promote and demote evaluate to nothing when debugging is not enabled
// otherwise, they check that their arguments are in the correct form.

static inline wptr_t A_STD A_UNUSED  A_HOT
promote(sptr_t s)
{
        assert(!IS_LAZY(s));
        assert(jhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}

static inline sptr_t A_STD A_UNUSED A_HOT
demote(wptr_t s)
{
        assert(!IS_LAZY(s));
        assert(jhc_valid_whnf(s));
        return (sptr_t)s;
}

// like eval but you know the target is in WHNF or is a already evaluated indirection
static inline wptr_t A_STD A_UNUSED  A_HOT
follow(sptr_t s)
{
        assert(jhc_valid_lazy(s));
        if(IS_LAZY(s)) {
                sptr_t h = (sptr_t)(GETHEAD(FROM_SPTR(s)));
                assert(!IS_LAZY(h));
                return (wptr_t)h;
        }
        return (wptr_t)s;
}

static wptr_t A_STD A_UNUSED  A_HOT
#if _JHC_GC == _JHC_GC_JGC
eval(gc_t gc,sptr_t s)
#else
eval(sptr_t s)
#endif
{
        assert(jhc_valid_lazy(s));
        if(IS_LAZY(s)) {
                assert(GET_PTYPE(s) == P_LAZY);
                void *ds = FROM_SPTR(s);
                sptr_t h = (sptr_t)(GETHEAD(ds));
                assert((fptr_t)h != BLACK_HOLE);
                if(IS_LAZY(h)) {
                        eval_fn fn = (eval_fn)FROM_SPTR(h);
                        assert(GET_PTYPE(h) == P_FUNC);
#if _JHC_DEBUG
                        GETHEAD(ds) = BLACK_HOLE;
#endif
#if _JHC_GC == _JHC_GC_JGC
                        wptr_t r = (*fn)(gc,NODEP(ds));
#else
                        wptr_t r = (*fn)(NODEP(ds));
#endif
#if _JHC_DEBUG
                        assert(GETHEAD(ds) != BLACK_HOLE);
#endif
                        return r;
                }
                return (wptr_t)h;
        }
        assert(jhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}

static void A_STD A_UNUSED A_HOT
update(void * thunk, wptr_t new)
{
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!IS_LAZY(new));
        GETHEAD(thunk) = (fptr_t)new;
}

#include "rts/slub.c"
#include "rts/gc_jgc.c"
