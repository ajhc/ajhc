#ifndef CONC_H
#define CONC_H

#define _JHC_CONC_NONE    0
#define _JHC_CONC_PTHREAD 1
#define _JHC_CONC_CUSTOM  2

#ifndef _JHC_CONC
#define _JHC_CONC _JHC_CONC_NONE
#endif

#if _JHC_CONC == _JHC_CONC_NONE
#define jhc_threadid_t          int
#define jhc_mutex_t             int
#define jhc_mutex_init(M)	(*(M) = 0)
#define jhc_mutex_lock(M)	do { } while (/* CONSTCOND */ 0)
#define jhc_mutex_unlock(M)	do { } while (/* CONSTCOND */ 0)

#elif _JHC_CONC == _JHC_CONC_PTHREAD
#include <pthread.h>
#define jhc_threadid_t          pthread_t
#define jhc_mutex_t             pthread_mutex_t
#define jhc_mutex_init(M)	(void) pthread_mutex_init((M), NULL)
#define jhc_mutex_lock(M)	pthread_mutex_lock((M))
#define jhc_mutex_unlock(M)	pthread_mutex_unlock((M))

#elif _JHC_CONC == _JHC_CONC_CUSTOM
/* You should impl "jhc_threadid_t" and "jhc_mutex_t". */
void jhc_mutex_init(jhc_mutex_t *mutex)
void jhc_mutex_lock(jhc_mutex_t *mutex)
void jhc_mutex_unlock(jhc_mutex_t *mutex)

#else
#error "You should choose _JHC_CONC."

#endif /* _JHC_CONC == ??? */

/* Common functions */
jhc_threadid_t forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err);
void jhc_conc_init(void);
void jhc_rts_lock(void);
void jhc_rts_unlock(void);

#endif /* CONC_H */
