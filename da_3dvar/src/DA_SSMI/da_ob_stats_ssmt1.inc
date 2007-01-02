subroutine da_ob_stats_ssmt1 ( stats_unit, iv )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: iv            ! O-B

   TYPE (maxmin_type)               :: minimum
   TYPE (maxmin_type)               :: maximum
   INTEGER                          :: nt
   INTEGER                          :: n, k
   real                             :: average, rms_err

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nt = 0
   
   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type( 1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   IF ( iv%num_ssmt1 > 0 ) THEN

      DO n=1, iv%num_ssmt1

         if ( iv%ssmt1(n)%loc%proc_domain ) then

            do k=1, iv%ssmt1(n)%info%levels
    
               CALL Stats_Calculate( n, k, iv%ssmt1(n)%t(k)%qc, &
                                     iv%ssmt1(n)%t(k)%inv, nt, &
                                     minimum, maximum, &
                                     average, rms_err)
            end do
         end if    ! end if( iv%ssmt1(n)%loc%proc_domain )
      END DO
      
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nt )
   
   call proc_stats_combine(average, rms_err, &
                           minimum%value, maximum%value, &
                           minimum%n, maximum%n, &
                           minimum%l, maximum%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
     IF ( nt /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-B for SSMT1'
   
      CALL DA_Print_Stats_ssmt1( stats_unit, nt, minimum, maximum, &
                                 average, rms_err )
   
     END IF
   
   END IF

end subroutine da_ob_stats_ssmt1
