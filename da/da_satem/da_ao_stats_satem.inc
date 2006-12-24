subroutine da_ao_stats_satem (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_satem_type)          :: Satem
   integer                          :: nthickness
   integer                          :: n, k

   nthickness = 0
   num_satem_tot = 0

   Satem%maximum%thickness = maxmin_type (missing_r, 0, 0)
   Satem%minimum%thickness = maxmin_type(-missing_r, 0, 0)
   Satem%average = residual_satem1_type(0.0)
   Satem%rms_err = Satem%average

   if (oi%num_satem > 0) then
      nthickness = 0

      Satem%maximum%thickness = maxmin_type(0.0, 0, 0)

      Satem%minimum = Satem%maximum
      Satem%average = residual_satem1_type(0.0)
      Satem%rms_err = Satem%average

      do n=1, oi%num_Satem
         if (oi%satem(n)%loc%proc_domain) then
            do k=1, oi%satem(n)%info%levels
               call da_stats_calculate (n, k, oi%Satem(n)%thickness(k)%qc, & 
                          re%satem(n)%thickness(k), nthickness, &
                          Satem%minimum%thickness, Satem%maximum%thickness, &
                          Satem%average%thickness, Satem%rms_err%thickness)
            end do
         end if    ! end if (oi%satem(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nthickness)
   num_satem_tot = nthickness

   call da_proc_stats_combine(Satem%average%thickness, Satem%rms_err%thickness, &
             Satem%minimum%thickness%value, Satem%maximum%thickness%value, &
             Satem%minimum%thickness%n, Satem%maximum%thickness%n, &
             Satem%minimum%thickness%l, Satem%maximum%thickness%l)

   if (rootproc) then
      if (nthickness /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Satem'
         call da_print_stats_satem(stats_unit, nthickness, Satem)
      end if
   end if

end subroutine da_ao_stats_satem


