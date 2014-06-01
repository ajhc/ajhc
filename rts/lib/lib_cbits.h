#ifndef LIB_CBITS_H
#define LIB_CBITS_H

#include "HsFFI.h"
struct FILE;

extern HsInt jhc_stdrnd[2];
extern HsInt jhc_data_unique;
HsBool jhc_wait_for_input(FILE *f, HsInt timeout);

#ifdef __WIN32__
#define getchar_unlocked() getchar()
#define putchar_unlocked(x) putchar(x)
#define getc_unlocked(x) getc(x)
#define putc_unlocked(x,y) putc(x,y)
#endif

inline static int A_UNUSED
jhc_utf8_getchar(void)
{
        return getchar_unlocked();
}

inline static int A_UNUSED
jhc_utf8_getc(FILE *f)
{
        return getc_unlocked(f);
}

inline static int A_UNUSED
jhc_utf8_putchar(int ch)
{
        return putchar_unlocked(ch);
}

inline static int A_UNUSED
jhc_utf8_putc(int ch, FILE *f)
{
        return putc_unlocked(ch, f);
}

#endif
