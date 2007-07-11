subroutine da_ao_stats_satem (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_satem_type)          :: satem
   integer                          :: nthickness
   integer                          :: n, k

   nthickness = 0
   num_satem_tot = 0

   satem%maximum%thickness = maxmin_type (missing_r, 0, 0)
   satem%minimum%thickness = maxmin_type(-missing_r, 0, 0)
   satem%average = residual_satem1_type(0.0)
   satem%rms_err = satem%average

   if (oi%num_satem > 0) then
      nthickness = 0

      satem%maximum%thickness = maxmin_type(0.0, 0, 0)

      satem%minimum = satem%maximum
      satem%average = residual_satem1_type(0.0)
      satem%rms_err = satem%average

      do n=1, oi%num_satem
         if (oi%satem(n)%loc%proc_domain) then
            do k=1, oi%satem(n)%info%levels
               call da_stats_calculate (n, k, oi%satem(n)%thickness(k)%qc, & 
                          re%satem(n)%thickness(k), nthickness, &
                          satem%minimum%thickness, satem%maximum%thickness, &
                          satem%average%thickness, satem%rms_err%thickness)
            end do
         end if    ! end if (oi%satem(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nthickness)
   num_satem_tot = nthickness

   call da_proc_stats_combine(satem%average%thickness, satem%rms_err%thickness, &
             satem%minimum%thickness%value, satem%maximum%thickness%value, &
             satem%minimum%thickness%n, satem%maximum%thickness%n, &
             satem%minimum%thickness%l, satem%maximum%thickness%l)

   if (rootproc) then
      if (nthickness /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for satem'
         call da_print_stats_satem(stats_unit, nthickness, satem)
      end if
   end if

end subroutine da_ao_stats_satem


