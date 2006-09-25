subroutine da_ao_stats_ssmt1( stats_unit, iv, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: iv            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! O-A

   TYPE (maxmin_type)               :: minimum
   TYPE (maxmin_type)               :: maximum
   INTEGER                          :: nt
   INTEGER                          :: n, k
   real                             :: average, rms_err

   nt = 0
   num_ssmt1_tot = 0

   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type( 1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   IF ( iv%num_ssmt1 > 0 ) THEN

      DO n=1, iv%num_ssmt1

         if ( iv%ssmt1(n)%loc%proc_domain ) then

            do k=1, iv%ssmt1(n)%info%levels

               CALL da_stats_calculate( n, k, iv%ssmt1(n)%t(k)%qc, & 
                                     re%ssmt1(n)%t(k), nt, &
                                     minimum, maximum, &
                                     average, rms_err)

            end do
         end if    ! end if( iv%ssmt1(n)%loc%proc_domain )
      END DO
   END IF

! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( nt )
   num_ssmt1_tot = nt

   call da_proc_stats_combine(average, rms_err, &
                           minimum%value, maximum%value, &
                           minimum%n, maximum%n, &
                           minimum%l, maximum%l )
   
   IF ( rootproc ) THEN
   
    IF ( nt /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-A for SSMT1'
   
      call da_print_stats_ssmt1( stats_unit, nt, minimum, maximum, &
                                 average, rms_err )
   
    END IF
   
   END IF

END subroutine da_ao_stats_ssmt1
