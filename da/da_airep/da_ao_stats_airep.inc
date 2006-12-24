subroutine da_ao_stats_airep (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_airep_type)          :: Airep
   integer                          :: nu, nv, nt
   integer                          :: n, k

   num_airep_tot = 0

   nu = 0
   nv = 0
   nt = 0

   Airep%maximum%u = maxmin_type (missing_r, 0, 0)
   Airep%maximum%v = maxmin_type (missing_r, 0, 0)
   Airep%maximum%t = maxmin_type (missing_r, 0, 0)
   Airep%minimum%u = maxmin_type(-missing_r, 0, 0)
   Airep%minimum%v = maxmin_type(-missing_r, 0, 0)
   Airep%minimum%t = maxmin_type(-missing_r, 0, 0)
   Airep%average = residual_airep1_type(0.0, 0.0, 0.0)
   Airep%rms_err = Airep%average

   if (oi%num_Airep > 0) then
      do n=1, oi%num_Airep
         if (oi%airep(n)%loc%proc_domain) then
            do k=1, oi%Airep(n)%info%levels
               call da_stats_calculate (n, k, oi%Airep(n)%u(k)%qc,  & 
                                 re%Airep(n)%u(k), nu, &
                                 airep%minimum%u, Airep%maximum%u, &
                                 airep%average%u, Airep%rms_err%u)
               call da_stats_calculate (n, k, oi%Airep(n)%v(k)%qc,  & 
                                 re%Airep(n)%v(k), nv, &
                                 airep%minimum%v, Airep%maximum%v, &
                                 airep%average%v, Airep%rms_err%v)
               call da_stats_calculate (n, k, oi%Airep(n)%t(k)%qc,  & 
                                 re%Airep(n)%t(k), nt, &
                                  Airep%minimum%t, Airep%maximum%t, &
                                  Airep%average%t, Airep%rms_err%t)
           end do
         end if    ! end if (oi%airep(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   call da_proc_sum_int (nt)
   num_airep_tot = nu + nv + nt

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
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Airep'
         call da_print_stats_airep(stats_unit, nu, nv, nt, Airep)
      end if
   end if

 end subroutine da_ao_stats_airep


