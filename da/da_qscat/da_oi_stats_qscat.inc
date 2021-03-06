subroutine da_oi_stats_qscat (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_qscat_type) :: stats
   integer                 :: nu, nv
   integer                 :: n

   if (trace_use_dull) call da_trace_entry("da_oi_stats_qscat")

   nu = 0
   nv = 0
   
   stats%maximum%u = maxmin_type(-1.0E+20, 0, 0)
   stats%maximum%v = maxmin_type(-1.0E+20, 0, 0)
   stats%minimum%u = maxmin_type(1.0E+20, 0, 0)
   stats%minimum%v = maxmin_type(1.0E+20, 0, 0)
   stats%average = residual_qscat1_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(qscat)%nlocal
      if (iv%info(qscat)%proc_domain(1,n)) then
         call da_stats_calculate(n, 0, iv%qscat(n)%u%qc, iv%qscat(n)%u%inv, nu, &
            stats%minimum%u, stats%maximum%u, stats%average%u, stats%rms_err%u)
         call da_stats_calculate(n, 0, iv%qscat(n)%v%qc, iv%qscat(n)%v%inv, nv, &
            stats%minimum%v, stats%maximum%v, stats%average%v, stats%rms_err%v)
      end if    ! end if (iv%info(qscat)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   
   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)
   
   if (rootproc) then 
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for qscat'
         call da_print_stats_qscat(stats_unit, nu, nv, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_qscat")

end subroutine da_oi_stats_qscat


