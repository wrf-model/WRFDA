subroutine da_oa_stats_ssmt2( stats_unit, iv, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: iv            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! O-A

   TYPE (maxmin_type)               :: minimum
   TYPE (maxmin_type)               :: maximum
   INTEGER                          :: nrh
   INTEGER                          :: n, k
   real                             :: average, rms_err

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nrh = 0
   num_ssmt2_tot = 0
   
   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type( 1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   IF ( iv%num_ssmt2 > 0 ) THEN

      DO n=1, iv%num_ssmt2

         if ( iv%ssmt2(n)%loc%proc_domain ) then

            do k=1, iv%ssmt2(n)%info%levels

               CALL Stats_Calculate( n, k, iv%ssmt2(n)%rh(k)%qc, & 
                                     re%ssmt2(n)%rh(k), nrh, &
                                     minimum, maximum, &
                                     average, rms_err)

            end do
         end if    ! end if( iv%ssmt2(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nrh )
   num_ssmt2_tot = nrh
      
   call proc_stats_combine(average, rms_err, &
                           minimum%value, maximum%value, &
                           minimum%n, maximum%n, &
                           minimum%l, maximum%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nrh /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-A for SSMT2'
   
      call DA_Print_Stats_ssmt2( stats_unit, nrh, minimum, maximum, &
                                 average, rms_err )
   
    END IF
   
   END IF

END subroutine da_oa_stats_ssmt2
