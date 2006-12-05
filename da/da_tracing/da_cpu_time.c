#include <sys/times.h>

#if defined(LOWERCASE) 
#if defined(UNDERSCORE)
void gen_cputime_(
#else
#if defined(DOUBLEUNDERSCORE)
void gen_cputime__(
#else
void gen_cputime(
#endif
#endif
#endif

#if defined(UPPERCASE) 
#if defined(UNDERSCORE)
void GEN_CPUTIME_(
#else
#if defined(DOUBLEUNDERSCORE)
void GEN_CPUTIME__(
#else
void GEN_CPUTIME(
#endif
#endif
#endif

  pr_CPUTime,
  pi_rc)

#if defined (CRAY) || defined(FRL8)
  double *pr_CPUTime;
#else
  float *pr_CPUTime;
#endif
  int   *pi_rc;
{
  struct tms time;
  clock_t rc;
  
  rc = times(&time);

  if (rc == -1) {
    *pi_rc = 1;
  } else {
    *pi_rc = 0;
  }

  *pr_CPUTime = (float)time.tms_utime;
}

/* END-OF-FILE */
