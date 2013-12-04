#include "rts/conc.h"

static jhc_mutex_t jhc_rts_mutex;

void
jhc_conc_init(void)
{
        jhc_mutex_init(&jhc_rts_mutex);
}

void
jhc_rts_lock(void)
{
        jhc_mutex_lock(&jhc_rts_mutex);
}

void
jhc_rts_unlock(void)
{
        jhc_mutex_unlock(&jhc_rts_mutex);
}

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
