subroutine da_oi_stats_ships (stats_unit, oi)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_ships_type)          :: ships
   integer                          :: nu, nv, nt, np, nq
   integer                          :: n

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0

   ships%maximum%u = maxmin_type(missing_r, 0, 0)
   ships%maximum%v = maxmin_type(missing_r, 0, 0)
   ships%maximum%t = maxmin_type(missing_r, 0, 0)
   ships%maximum%p = maxmin_type(missing_r, 0, 0)
   ships%maximum%q = maxmin_type(missing_r, 0, 0)
   ships%minimum%u = maxmin_type(-missing_r, 0, 0)
   ships%minimum%v = maxmin_type(-missing_r, 0, 0)
   ships%minimum%t = maxmin_type(-missing_r, 0, 0)
   ships%minimum%p = maxmin_type(-missing_r, 0, 0)
   ships%minimum%q = maxmin_type(-missing_r, 0, 0)
   ships%average = residual_ships1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   ships%rms_err = ships%average

   if (oi%num_ships > 0) then
      do n=1, oi%num_ships
         if (oi%ships(n)%loc%proc_domain) then
            call da_stats_calculate(oi%ships(n)%loc%obs_global_index, &
                               0, oi%ships(n)%u%qc, &
                               oi%ships(n)%u%inv, nu, &
                               ships%minimum%u, ships%maximum%u, &
                               ships%average%u, ships%rms_err%u)
            call da_stats_calculate(oi%ships(n)%loc%obs_global_index, &
                               0, oi%ships(n)%v%qc, &
                               oi%ships(n)%v%inv, nv, &
                               ships%minimum%v, ships%maximum%v, &
                               ships%average%v, ships%rms_err%v)
            call da_stats_calculate(oi%ships(n)%loc%obs_global_index, &
                               0, oi%ships(n)%t%qc, &
                               oi%ships(n)%t%inv, nt, &
                               ships%minimum%t, ships%maximum%t, &
                               ships%average%t, ships%rms_err%t)
            call da_stats_calculate(oi%ships(n)%loc%obs_global_index, &
                               0, oi%ships(n)%p%qc, &
                               oi%ships(n)%p%inv, np, &
                               ships%minimum%p, ships%maximum%p, &
                               ships%average%p, ships%rms_err%p)
            call da_stats_calculate(oi%ships(n)%loc%obs_global_index, &
                               0, oi%ships(n)%q%qc, &
                               oi%ships(n)%q%inv, nq, &
                               ships%minimum%q, ships%maximum%q, &
                               ships%average%q, ships%rms_err%q)

         end if    ! end if (oi%ships(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)
   call da_proc_sum_int(np)
   call da_proc_sum_int(nq)

   call da_proc_stats_combine(Ships%average%u, Ships%rms_err%u, &
                           Ships%minimum%u%value, Ships%maximum%u%value, &
                           Ships%minimum%u%n, Ships%maximum%u%n, &
                           Ships%minimum%u%l, Ships%maximum%u%l)
   call da_proc_stats_combine(Ships%average%v, Ships%rms_err%v, &
                           Ships%minimum%v%value, Ships%maximum%v%value, &
                           Ships%minimum%v%n, Ships%maximum%v%n, &
                           Ships%minimum%v%l, Ships%maximum%v%l)
   call da_proc_stats_combine(Ships%average%t, Ships%rms_err%t, &
                           Ships%minimum%t%value, Ships%maximum%t%value, &
                           Ships%minimum%t%n, Ships%maximum%t%n, &
                           Ships%minimum%t%l, Ships%maximum%t%l)
   call da_proc_stats_combine(Ships%average%p, Ships%rms_err%p, &
                           Ships%minimum%p%value, Ships%maximum%p%value, &
                           Ships%minimum%p%n, Ships%maximum%p%n, &
                           Ships%minimum%p%l, Ships%maximum%p%l)
   call da_proc_stats_combine(Ships%average%q, Ships%rms_err%q, &
                           Ships%minimum%q%value, Ships%maximum%q%value, &
                           Ships%minimum%q%n, Ships%maximum%q%n, &
                           Ships%minimum%q%l, Ships%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ships'
         call da_print_stats_ships(stats_unit, nu, nv, nt, np, nq, ships)
      end if
   end if

end subroutine da_oi_stats_ships


