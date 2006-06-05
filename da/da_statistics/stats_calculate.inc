SUBROUTINE Stats_Calculate( n, k, qc_flag, x, nn, &
                            minimum, maximum, &
                            average, rms_err )

   INTEGER, INTENT(IN)    :: n             ! Number of obs.
   INTEGER, INTENT(IN)    :: k             ! Level.
   INTEGER, INTENT(IN)    :: qc_flag       ! QC flag.
   REAL, INTENT(IN)       :: x             ! Value.
   INTEGER, INTENT(INOUT) :: nn            ! Number of ok obs.
   TYPE (maxmin_type), INTENT(INOUT) :: minimum, maximum
   REAL, INTENT(INOUT)    :: average, rms_err

   IF ( qc_flag >= obs_qc_pointer ) THEN
      nn = nn + 1

      IF ( x < minimum%value) THEN
         minimum%value = x
         minimum%n     = n
         minimum%l     = k
      END IF

      IF ( x > maximum%value) THEN
         maximum%value = x
         maximum%n     = n
         maximum%l     = k
      END IF

      average = average + x
      rms_err = rms_err + x * x
   END IF

END SUBROUTINE Stats_Calculate


