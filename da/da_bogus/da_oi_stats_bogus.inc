subroutine da_oi_stats_bogus (stats_unit, oi)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_bogus_type)          :: Bogus
   integer                          :: nu, nv, nt, nq, nslp
   integer                          :: n, k

   nslp = 0
   nu = 0
   nv = 0
   nt = 0
   nq = 0

   Bogus%maximum%u = maxmin_type(missing_r, 0, 0)
   Bogus%maximum%v = maxmin_type(missing_r, 0, 0)
   Bogus%maximum%t = maxmin_type(missing_r, 0, 0)
   Bogus%maximum%q = maxmin_type(missing_r, 0, 0)
   Bogus%maximum%slp = maxmin_type(missing_r, 0, 0)
   Bogus%minimum%u = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%v = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%t = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%q = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%slp = maxmin_type(-missing_r, 0, 0)
   Bogus%average = residual_bogus1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   Bogus%rms_err = Bogus%average

   if (oi%num_Bogus > 0) then
      do n=1, oi%num_Bogus
         if (oi%bogus(n)%loc%proc_domain) then
            do k=1, oi%bogus(n)%info%levels
               call da_stats_calculate(oi%bogus(n)%loc%obs_global_index, &
                               k, oi%bogus(n)%u(k)%qc, &
                               oi%bogus(n)%u(k)%inv, nu, &
                               bogus%minimum%u, Bogus%maximum%u, &
                               bogus%average%u, Bogus%rms_err%u)
               call da_stats_calculate(oi%bogus(n)%loc%obs_global_index, &
                               k, oi%bogus(n)%v(k)%qc, &
                               oi%bogus(n)%v(k)%inv, nv, &
                               bogus%minimum%v, Bogus%maximum%v, &
                               bogus%average%v, Bogus%rms_err%v)
               call da_stats_calculate(oi%bogus(n)%loc%obs_global_index, &
                               k, oi%bogus(n)%t(k)%qc, &
                               oi%bogus(n)%t(k)%inv, nt, &
                               bogus%minimum%t, Bogus%maximum%t, &
                               bogus%average%t, Bogus%rms_err%t)
               call da_stats_calculate(oi%bogus(n)%loc%obs_global_index, &
                               k, oi%bogus(n)%q(k)%qc, &
                               oi%bogus(n)%q(k)%inv, nq, &
                               bogus%minimum%q, Bogus%maximum%q, &
                               bogus%average%q, Bogus%rms_err%q)
            end do
            call da_stats_calculate( oi%bogus(n)%loc%obs_global_index, &
                            0, oi%bogus(n)%slp%qc, &
                            oi%Bogus(n)%slp%inv, nslp,  &
                            Bogus%minimum%slp, Bogus%maximum%slp, &
                            Bogus%average%slp, Bogus%rms_err%slp)
         end if
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)
   call da_proc_sum_int(nq)
   call da_proc_sum_int(nslp)

   call da_proc_stats_combine(Bogus%average%u, Bogus%rms_err%u, &
                           Bogus%minimum%u%value, Bogus%maximum%u%value, &
                           Bogus%minimum%u%n, Bogus%maximum%u%n, &
                           Bogus%minimum%u%l, Bogus%maximum%u%l)
   call da_proc_stats_combine(Bogus%average%v, Bogus%rms_err%v, &
                           Bogus%minimum%v%value, Bogus%maximum%v%value, &
                           Bogus%minimum%v%n, Bogus%maximum%v%n, &
                           Bogus%minimum%v%l, Bogus%maximum%v%l)
   call da_proc_stats_combine(Bogus%average%t, Bogus%rms_err%t, &
                           Bogus%minimum%t%value, Bogus%maximum%t%value, &
                           Bogus%minimum%t%n, Bogus%maximum%t%n, &
                           Bogus%minimum%t%l, Bogus%maximum%t%l)
   call da_proc_stats_combine(Bogus%average%q, Bogus%rms_err%q, &
                           Bogus%minimum%q%value, Bogus%maximum%q%value, &
                           Bogus%minimum%q%n, Bogus%maximum%q%n, &
                           Bogus%minimum%q%l, Bogus%maximum%q%l)
   call da_proc_stats_combine(Bogus%average%slp, Bogus%rms_err%slp, &
                           Bogus%minimum%slp%value, Bogus%maximum%slp%value, &
                           Bogus%minimum%slp%n, Bogus%maximum%slp%n, &
                           Bogus%minimum%slp%l, Bogus%maximum%slp%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. nq /= 0 .or. nslp /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Bogus'
         call da_print_stats_bogus(stats_unit, nu, nv, nt, nq, nslp, Bogus)
      end if
   end if

end subroutine da_oi_stats_bogus


