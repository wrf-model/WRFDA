subroutine da_maxmin_in_field(field, max, min)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                    intent(in)  :: field(its:ite,jts:jte,kts:kte)
   type(maxmin_field_type), intent(out) :: max(kts:kte)
   type(maxmin_field_type), intent(out) :: min(kts:kte)

   if (trace_use_dull) call da_trace_entry("da_maxmin_in_field")

   do k = kts, kte
      max(k)%value = -1.0e20
      min(k)%value =  1.0e20
      do j = jts, jte
         do i = its, ite
            if (field(i,j,k) > max(k)%value) then
               max(k)%value = field(i,j,k)
               max(k)%i     = i
               max(k)%j     = j
            end if
            if (field(i,j,k) < min(k)%value) then
               min(k)%value = field(i,j,k)
               min(k)%i     = i
               min(k)%j     = j
            end if
         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_maxmin_in_field")

end subroutine da_maxmin_in_field


s%minimum%p, ships%minimum%q, &
      ' Maximum(n,k): ', ships%maximum%u, ships%maximum%v, ships%maximum%t, &
                         ships%maximum%p, ships%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', ships%average%u/real(nu), ships%average%v/real(nv), &
                         ships%average%t/real(nt), ships%average%p/real(np), &
                         ships%average%q/real(nq), &
      '    RMSE     : ', sqrt(ships%rms_err%u/real(nu)), &
                         sqrt(ships%rms_err%v/real(nv)), &
                         sqrt(ships%rms_err%t/real(nt)), &
                         sqrt(ships%rms_err%p/real(np)), &
                         sqrt(ships%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_ships")

end subroutine da_print_stats_ships


           stats%average%t, stats%rms_err%t)  
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%p%qc,  & 
            re%sonde_sfc(n)%p, np, & 
            stats%minimum%p, stats%maximum%p, &
            stats%average%p, stats%rms_err%p)
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%q%qc,  & 
            re%sonde_sfc(n)%q, nq, & 
            stats%minimum%q, stats%maximum%q, &
            stats%average%q, stats%rms_err%q)
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   call da_proc_sum_int (nt)
   call da_proc_sum_int (np)
   call da_proc_sum_int (nq)
   iv%nstats(sound) = iv%nstats(sound) + nu + nv + nt + np + nq

   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)
   call da_proc_stats_combine(stats%average%t, stats%rms_err%t, &
      stats%minimum%t%value, stats%maximum%t%value, &
      stats%minimum%t%n, stats%maximum%t%n, &
      stats%minimum%t%l, stats%maximum%t%l)
   call da_proc_stats_combine(stats%average%p, stats%rms_err%p, &
      stats%minimum%p%value, stats%maximum%p%value, &
      stats%minimum%p%n, stats%maximum%p%n, &
      stats%minimum%p%l, stats%maximum%p%l)
   call da_proc_stats_combine(stats%average%q, stats%rms_err%q, &
      stats%minimum%q%value, stats%maximum%q%value, &
      stats%minimum%q%n, stats%maximum%q%n, &
      stats%minimum%q%l, stats%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for sonde_sfc'
         call da_print_stats_sonde_sfc(stats_unit, nu, nv, nt, np, nq, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_sonde_sfc")

end subroutine da_ao_stats_sonde_sfc


