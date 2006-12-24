subroutine da_oi_stats_gpspw (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_gpspw_type)                :: Gpspw
   integer                          :: ntpw
   integer                          :: n

   ntpw = 0

   Gpspw%maximum%tpw = maxmin_type(missing_r, 0, 0)
   Gpspw%minimum%tpw = maxmin_type(-missing_r, 0, 0)
   Gpspw%average = residual_gpspw1_type(0.0)
   Gpspw%rms_err = Gpspw%average

   if (oi%num_Gpspw > 0) then
      do n=1, oi%num_Gpspw
         if (oi%gpspw(n)%loc%proc_domain) then
            call da_stats_calculate(oi%Gpspw(n)%loc%obs_global_index, &
                               0, oi%gpspw(n)%tpw%qc, &
                              oi%gpspw(n)%tpw%inv, ntpw, &
                              gpspw%minimum%tpw  , gpspw%maximum%tpw, &
                              gpspw%average%tpw  , gpspw%rms_err%tpw)

         end if    ! end if (oi%gpspw(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.

   call da_proc_sum_int(ntpw)

   call da_proc_stats_combine(Gpspw%average%tpw, Gpspw%rms_err%tpw, &
                           Gpspw%minimum%tpw%value, Gpspw%maximum%tpw%value, &
                           Gpspw%minimum%tpw%n, Gpspw%maximum%tpw%n, &
                           Gpspw%minimum%tpw%l, Gpspw%maximum%tpw%l)

   if (rootproc) then
      if (ntpw /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Gpspw'
         call da_print_stats_gpspw(stats_unit, ntpw, Gpspw)
      end if
   end if

end subroutine da_oi_stats_gpspw


