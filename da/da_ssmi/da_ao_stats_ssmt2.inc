subroutine da_ao_stats_ssmt2 (stats_unit, iv, re)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: iv            ! OI
   type  (y_type), intent (in)      :: re            ! O-A

   type (maxmin_type)               :: minimum
   type (maxmin_type)               :: maximum
   integer                          :: nrh
   integer                          :: n, k
   real                             :: average, rms_err

   nrh = 0
   num_ssmt2_tot = 0
   
   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type (1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   if (iv%num_ssmt2 > 0) then
      do n=1, iv%num_ssmt2
         if (iv%ssmt2(n)%loc%proc_domain) then
            do k=1, iv%ssmt2(n)%info%levels
               call da_stats_calculate (n, k, iv%ssmt2(n)%rh(k)%qc, & 
                                     re%ssmt2(n)%rh(k), nrh, &
                                     minimum, maximum, &
                                     average, rms_err)

            end do
         end if    ! end if (iv%ssmt2(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nrh)
   num_ssmt2_tot = nrh
      
   call da_proc_stats_combine(average, rms_err, &
                           minimum%value, maximum%value, &
                           minimum%n, maximum%n, &
                           minimum%l, maximum%l)
   
   if (rootproc) then 
      if (nrh /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-A for SSMT2'
         call da_print_stats_ssmt2 (stats_unit, nrh, minimum, maximum, &
                                 average, rms_err)
      end if
   end if

end subroutine da_ao_stats_ssmt2


