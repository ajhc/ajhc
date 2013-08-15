#ifndef LIB_CBITS_H
#define LIB_CBITS_H

#include "HsFFI.h"
struct FILE;

extern HsInt jhc_stdrnd[2];
extern HsInt jhc_data_unique;
HsBool jhc_wait_for_input(FILE *f,HsInt timeout);

#ifdef _JHC_USE_OWN_STDIO
/* Implement us! */
int jhc_utf8_getchar(void);
int jhc_utf8_getc(FILE *f);
int jhc_utf8_putchar(int ch);
int jhc_utf8_putc(int ch, FILE *f);
#else
inline static int A_UNUSED
jhc_utf8_getchar(void)
{
    return getchar();
}

inline static int A_UNUSED
jhc_utf8_getc(FILE *f)
{
    return getc(f);
}

inline static int A_UNUSED
jhc_utf8_putchar(int ch)
{
    return putchar(ch);
}

inline static int A_UNUSED
jhc_utf8_putc(int ch, FILE *f)
{
    return putc(ch,f);
}
#endif /* _JHC_USE_OWN_STDIO */

#endif
