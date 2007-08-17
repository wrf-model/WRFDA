subroutine da_oi_stats_polaramv(stats_unit, oi)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates (Obs - Analysis) statistics for Polar AMVs
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_polaramv_type)       :: polaramv
   integer                          :: nu, nv
   integer                          :: n, k

   nu = 0
   nv = 0
   
   polaramv%maximum%u = maxmin_type(missing_r, 0, 0)
   polaramv%maximum%v = maxmin_type(missing_r, 0, 0)
   polaramv%minimum%u = maxmin_type(-missing_r, 0, 0)
   polaramv%minimum%v = maxmin_type(-missing_r, 0, 0)
   polaramv%average = residual_polaramv1_type(0.0, 0.0)
   polaramv%rms_err = polaramv%average

   if (oi%num_polaramv > 0) then
      do n=1, oi%num_polaramv
         if (oi%polaramv(n)%loc%proc_domain) then
            do k=1, oi%polaramv(n)%info%levels
               call da_stats_calculate(oi%polaramv(n)%loc%obs_global_index, &
                                     0, oi%polaramv(n)%u(k)%qc, &
                                     oi%polaramv(n)%u(k)%inv, nu, &
                                     polaramv%minimum%u, polaramv%maximum%u, &
                                     polaramv%average%u, polaramv%rms_err%u)

               call da_stats_calculate(oi%polaramv(n)%loc%obs_global_index, &
                                     0, oi%polaramv(n)%v(k)%qc, &
                                     oi%polaramv(n)%v(k)%inv, nv, &
                                     polaramv%minimum%v, polaramv%maximum%v, &
                                     polaramv%average%v, polaramv%rms_err%v)
            end do
         end if    ! end if (oi%polaramv(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_stats_combine(polaramv%average%u, polaramv%rms_err%u, &
                           polaramv%minimum%u%value, polaramv%maximum%u%value, &
                           polaramv%minimum%u%n, polaramv%maximum%u%n, &
                           polaramv%minimum%u%l, polaramv%maximum%u%l)
   call da_proc_stats_combine(polaramv%average%v, polaramv%rms_err%v, &
                           polaramv%minimum%v%value, polaramv%maximum%v%value, &
                           polaramv%minimum%v%n, polaramv%maximum%v%n, &
                           polaramv%minimum%v%l, polaramv%maximum%v%l)
      
   if (rootproc) then
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for polaramv'
         call da_print_stats_polaramv(stats_unit, nu, nv, polaramv)
      end if
   end if

end subroutine da_oi_stats_polaramv


