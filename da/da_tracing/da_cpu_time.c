#include <sys/times.h>

#ifdef NOUNDERSCORE
void gen_cputime(
#else
void gen_cputime_(
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
