subroutine da_ao_stats_ssmt1( stats_unit, iv, re )

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: iv            ! OI
   type ( y_type), intent (in)      :: re            ! O-A

   type (maxmin_type)               :: minimum
   type (maxmin_type)               :: maximum
   integer                          :: nt
   integer                          :: n, k
   real                             :: average, rms_err

   nt = 0
   num_ssmt1_tot = 0

   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type( 1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   if ( iv%num_ssmt1 > 0 ) then

      do n=1, iv%num_ssmt1

         if ( iv%ssmt1(n)%loc%proc_domain ) then

            do k=1, iv%ssmt1(n)%info%levels

               call da_stats_calculate( n, k, iv%ssmt1(n)%t(k)%qc, & 
                                     re%ssmt1(n)%t(k), nt, &
                                     minimum, maximum, &
                                     average, rms_err)

            end do
         end if    ! end if( iv%ssmt1(n)%loc%proc_domain )
      end do
   end if

! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( nt )
   num_ssmt1_tot = nt

   call da_proc_stats_combine(average, rms_err, &
                           minimum%value, maximum%value, &
                           minimum%n, maximum%n, &
                           minimum%l, maximum%l )
   
   if ( rootproc ) then
   
    if ( nt /= 0 ) then
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-A for SSMT1'
   
      call da_print_stats_ssmt1( stats_unit, nt, minimum, maximum, &
                                 average, rms_err )
   
    end if
   
   end if

end subroutine da_ao_stats_ssmt1


