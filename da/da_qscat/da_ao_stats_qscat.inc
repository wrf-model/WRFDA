subroutine da_ao_stats_qscat (stats_unit, oi, re)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_qscat_type)          :: qscat
   integer                          :: nu, nv
   integer                          :: n

   nu = 0
   nv = 0
   num_qscat_tot = 0

   qscat%maximum%u = maxmin_type(-1.0E+20, 0, 0)
   qscat%maximum%v = maxmin_type(-1.0E+20, 0, 0)
   qscat%minimum%u = maxmin_type (1.0E+20, 0, 0)
   qscat%minimum%v = maxmin_type (1.0E+20, 0, 0)
   qscat%average = residual_qscat1_type(0.0, 0.0)
   qscat%rms_err = qscat%average

   if (oi%num_qscat > 0) then
      do n=1, oi%num_qscat
         if (oi%qscat(n)%loc%proc_domain) then
            call da_stats_calculate (n, 0, oi%qscat(n)%u%qc, & 
                               re%qscat(n)%u, nu, & 
                               qscat%minimum%u, qscat%maximum%u, &
                               qscat%average%u, qscat%rms_err%u)
            call da_stats_calculate (n, 0, oi%qscat(n)%v%qc, & 
                               re%qscat(n)%v, nv, & 
                               qscat%minimum%v, qscat%maximum%v, &
                               qscat%average%v, qscat%rms_err%v)
         end if    ! end if (oi%qscat(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   num_qscat_tot = nu + nv
   
   call da_proc_stats_combine(qscat%average%u, qscat%rms_err%u, &
                           qscat%minimum%u%value, qscat%maximum%u%value, &
                           qscat%minimum%u%n, qscat%maximum%u%n, &
                           qscat%minimum%u%l, qscat%maximum%u%l)
   call da_proc_stats_combine(qscat%average%v, qscat%rms_err%v, &
                           qscat%minimum%v%value, qscat%maximum%v%value, &
                           qscat%minimum%v%n, qscat%maximum%v%n, &
                           qscat%minimum%v%l, qscat%maximum%v%l)
   
   if (rootproc) then  
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for qscat'
         call da_print_stats_qscat(stats_unit, nu, nv, qscat)
      end if
   end if

end subroutine da_ao_stats_qscat


