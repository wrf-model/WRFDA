subroutine da_oi_stats_sonde_sfc (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_sonde_sfc_type)      :: sonde_sfc
   integer                          :: nu, nv, nt, np, nq
   integer                          :: n

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0

   sonde_sfc%maximum%u = maxmin_type(missing_r, 0, 0)
   sonde_sfc%maximum%v = maxmin_type(missing_r, 0, 0)
   sonde_sfc%maximum%t = maxmin_type(missing_r, 0, 0)
   sonde_sfc%maximum%p = maxmin_type(missing_r, 0, 0)
   sonde_sfc%maximum%q = maxmin_type(missing_r, 0, 0)
   sonde_sfc%minimum%u = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%v = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%t = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%p = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%q = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%average = residual_sonde_sfc1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   sonde_sfc%rms_err = sonde_sfc%average

   !  if (oi%num_sound < 1) return

   do n=1, oi%num_sound
      if (oi%sonde_sfc(n)%loc%proc_domain) then
         call da_stats_calculate(oi%sonde_sfc(n)%loc%obs_global_index, &
                               0, oi%sonde_sfc(n)%u%qc, &
                               oi%sonde_sfc(n)%u%inv, nu, &
                               sonde_sfc%minimum%u, sonde_sfc%maximum%u, &
                               sonde_sfc%average%u, sonde_sfc%rms_err%u)
         call da_stats_calculate(oi%sonde_sfc(n)%loc%obs_global_index, &
                               0, oi%sonde_sfc(n)%v%qc, &
                               oi%sonde_sfc(n)%v%inv, nv, &
                               sonde_sfc%minimum%v, sonde_sfc%maximum%v, &
                               sonde_sfc%average%v, sonde_sfc%rms_err%v)
         call da_stats_calculate(oi%sonde_sfc(n)%loc%obs_global_index, &
                               0, oi%sonde_sfc(n)%t%qc, &
                               oi%sonde_sfc(n)%t%inv, nt, &
                               sonde_sfc%minimum%t, sonde_sfc%maximum%t, &
                               sonde_sfc%average%t, sonde_sfc%rms_err%t)
         call da_stats_calculate(oi%sonde_sfc(n)%loc%obs_global_index, &
                               0, oi%sonde_sfc(n)%p%qc, &
                               oi%sonde_sfc(n)%p%inv, np, &
                               sonde_sfc%minimum%p, sonde_sfc%maximum%p, &
                               sonde_sfc%average%p, sonde_sfc%rms_err%p)

         call da_stats_calculate(oi%sonde_sfc(n)%loc%obs_global_index, &
                               0, oi%sonde_sfc(n)%q%qc, &
                               oi%sonde_sfc(n)%q%inv, nq, &
                               sonde_sfc%minimum%q, sonde_sfc%maximum%q, &
                               sonde_sfc%average%q, sonde_sfc%rms_err%q)
      end if    ! end if (oi%sonde_sfc(n)%loc%proc_domain)
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)
   call da_proc_sum_int(np)
   call da_proc_sum_int(nq)

   call da_proc_stats_combine(Sonde_sfc%average%u, Sonde_sfc%rms_err%u, &
                           Sonde_sfc%minimum%u%value, Sonde_sfc%maximum%u%value, &
                           Sonde_sfc%minimum%u%n, Sonde_sfc%maximum%u%n, &
                           Sonde_sfc%minimum%u%l, Sonde_sfc%maximum%u%l)

   call da_proc_stats_combine(Sonde_sfc%average%v, Sonde_sfc%rms_err%v, &
                           Sonde_sfc%minimum%v%value, Sonde_sfc%maximum%v%value, &
                           Sonde_sfc%minimum%v%n, Sonde_sfc%maximum%v%n, &
                           Sonde_sfc%minimum%v%l, Sonde_sfc%maximum%v%l)

   call da_proc_stats_combine(Sonde_sfc%average%t, Sonde_sfc%rms_err%t, &
                           Sonde_sfc%minimum%t%value, Sonde_sfc%maximum%t%value, &
                           Sonde_sfc%minimum%t%n, Sonde_sfc%maximum%t%n, &
                           Sonde_sfc%minimum%t%l, Sonde_sfc%maximum%t%l)

   call da_proc_stats_combine(Sonde_sfc%average%p, Sonde_sfc%rms_err%p, &
                           Sonde_sfc%minimum%p%value, Sonde_sfc%maximum%p%value, &
                           Sonde_sfc%minimum%p%n, Sonde_sfc%maximum%p%n, &
                           Sonde_sfc%minimum%p%l, Sonde_sfc%maximum%p%l)

   call da_proc_stats_combine(Sonde_sfc%average%q, Sonde_sfc%rms_err%q, &
                           Sonde_sfc%minimum%q%value, Sonde_sfc%maximum%q%value, &
                           Sonde_sfc%minimum%q%n, Sonde_sfc%maximum%q%n, &
                           Sonde_sfc%minimum%q%l, Sonde_sfc%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for sonde_sfc'
         call da_print_stats_sonde_sfc(stats_unit, nu, nv, nt, np, nq, sonde_sfc)
      end if
   end if

 end subroutine da_oi_stats_sonde_sfc


