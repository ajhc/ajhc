#include "rts/conc.h"

#if _JHC_CONC == _JHC_CONC_NONE
jhc_threadid_t
forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err)
{
        (*wrapper)(entry);
        return 0; /* xxx */
}

#elif _JHC_CONC == _JHC_CONC_PTHREAD
jhc_threadid_t
forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err)
{
        pthread_t tid;
        *err = pthread_create(&tid, NULL, wrapper, entry);
        if (*err) {
                pthread_detach(tid);
        }
        return tid;
}

#elif _JHC_CONC == _JHC_CONC_CUSTOM
/* You should impl me at your side. */
#else
#error "You should choose _JHC_CONC."
#endif /* _JHC_CONC == ??? */
