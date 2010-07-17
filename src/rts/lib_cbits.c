/* this file contains C only needed to help support the 
 * standard libraries */

static HsInt jhc_stdrnd[2] A_UNUSED = { 1 , 1 };
static HsInt jhc_data_unique A_UNUSED;

static HsBool A_UNUSED
jhc_wait_for_input(FILE *f,HsInt timeout) {
#if JHC_isPosix
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fileno(f),&fds);
        struct timeval to = {  0, timeout * 1000 };
        int retval = select(1,&fds,NULL,&fds,&to);
        if(retval)
                return HS_BOOL_TRUE;
        else
                return HS_BOOL_FALSE;
#else
        return HS_BOOL_FALSE;
#endif
}

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
    return putc_unlocked(ch,f);
}
