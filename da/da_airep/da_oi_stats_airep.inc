subroutine da_oi_stats_airep (stats_unit, oi)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_airep_type)                :: Airep
   integer                          :: nu, nv, nt
   integer                          :: n, k

   nu = 0
   nv = 0
   nt = 0

   Airep%maximum%u = maxmin_type(missing_r, 0, 0)
   Airep%maximum%v = maxmin_type(missing_r, 0, 0)
   Airep%maximum%t = maxmin_type(missing_r, 0, 0)
   Airep%minimum%u = maxmin_type(-missing_r, 0, 0)
   Airep%minimum%v = maxmin_type(-missing_r, 0, 0)
   Airep%minimum%t = maxmin_type(-missing_r, 0, 0)
   Airep%average = residual_airep1_type(0.0, 0.0, 0.0)
   Airep%rms_err = Airep%average

   if (oi%num_Airep > 0) then
      do n=1, oi%num_Airep
         if (oi%airep(n)%loc%proc_domain) then
            do k=1, oi%Airep(n)%info%levels
               call da_stats_calculate(oi%airep(n)%loc%obs_global_index, &
                               k, oi%airep(n)%u(k)%qc, &
                               oi%airep(n)%u(k)%inv, nu, &
                               airep%minimum%u, airep%maximum%u, &
                               airep%average%u, airep%rms_err%u)
               call da_stats_calculate(oi%airep(n)%loc%obs_global_index, &
                               k, oi%airep(n)%v(k)%qc, &
                               oi%airep(n)%v(k)%inv, nv, &
                               airep%minimum%v, airep%maximum%v, &
                               airep%average%v, airep%rms_err%v)
               call da_stats_calculate(oi%airep(n)%loc%obs_global_index, &
                               k, oi%Airep(n)%t(k)%qc, &
                               oi%Airep(n)%t(k)%inv, nt, &
                               Airep%minimum%t, Airep%maximum%t, &
                               Airep%average%t, Airep%rms_err%t)
            end do
         end if    ! end if (oi%airep(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)

   call da_proc_stats_combine(Airep%average%u, Airep%rms_err%u, &
                           Airep%minimum%u%value, Airep%maximum%u%value, &
                           Airep%minimum%u%n, Airep%maximum%u%n, &
                           Airep%minimum%u%l, Airep%maximum%u%l)
   call da_proc_stats_combine(Airep%average%v, Airep%rms_err%v, &
                           Airep%minimum%v%value, Airep%maximum%v%value, &
                           Airep%minimum%v%n, Airep%maximum%v%n, &
                           Airep%minimum%v%l, Airep%maximum%v%l)
   call da_proc_stats_combine(Airep%average%t, Airep%rms_err%t, &
                           Airep%minimum%t%value, Airep%maximum%t%value, &
                           Airep%minimum%t%n, Airep%maximum%t%n, &
                           Airep%minimum%t%l, Airep%maximum%t%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Airep'
         call da_print_stats_airep(stats_unit, nu, nv, nt, Airep)
      end if
   end if

 end subroutine da_oi_stats_airep


