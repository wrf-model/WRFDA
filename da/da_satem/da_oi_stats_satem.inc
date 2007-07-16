subroutine da_oi_stats_satem (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_satem_type)          :: satem
   integer                          :: nthickness
   integer                          :: n, k

   nthickness = 0

   satem%maximum%thickness = maxmin_type(missing_r, 0, 0)
   satem%minimum%thickness = maxmin_type(-missing_r, 0, 0)
   satem%average = residual_satem1_type(0.0)
   satem%rms_err = satem%average

   if (oi%num_satem > 0) then
      do n=1, oi%num_satem
         if (oi%satem(n)%loc%proc_domain) then
            do k=1, oi%satem(n)%info%levels
               call da_stats_calculate(oi%satem(n)%loc%obs_global_index, &
                          k, oi%satem(n)%thickness(k)%qc, &
                          oi%satem(n)%thickness(k)%inv, nthickness, &
                          satem%minimum%thickness, satem%maximum%thickness, &
                          satem%average%thickness, satem%rms_err%thickness)
            end do
         end if    ! end if (oi%satem(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nthickness)

   call da_proc_stats_combine(satem%average%thickness, satem%rms_err%thickness, &
             satem%minimum%thickness%value, satem%maximum%thickness%value, &
             satem%minimum%thickness%n, satem%maximum%thickness%n, &
             satem%minimum%thickness%l, satem%maximum%thickness%l)

   if (rootproc) then
      if (nthickness /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for satem'
         call da_print_stats_satem(stats_unit, nthickness, satem)
      end if
   end if

end subroutine da_oi_stats_satem


