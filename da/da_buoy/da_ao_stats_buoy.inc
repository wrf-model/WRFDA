subroutine da_ao_stats_buoy (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_buoy_type)           :: Buoy
   integer                          :: nu, nv, nt, np, nq
   integer                          :: n

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   num_Buoy_tot = 0

   Buoy%maximum%u = maxmin_type (missing_r, 0, 0)
   Buoy%maximum%v = maxmin_type (missing_r, 0, 0)
   Buoy%maximum%t = maxmin_type (missing_r, 0, 0)
   Buoy%maximum%p = maxmin_type (missing_r, 0, 0)
   Buoy%maximum%q = maxmin_type (missing_r, 0, 0)
   Buoy%minimum%u = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%v = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%t = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%p = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%q = maxmin_type(-missing_r, 0, 0)
   Buoy%average = residual_buoy1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   Buoy%rms_err = Buoy%average

   if (oi%num_Buoy > 0) then
      do n=1, oi%num_Buoy
         if (oi%Buoy(n)%loc%proc_domain) then
            call da_stats_calculate (n, 0, oi%Buoy(n)%u%qc, & 
                               re%Buoy(n)%u, nu, & 
                               buoy%minimum%u, Buoy%maximum%u, &
                               buoy%average%u, Buoy%rms_err%u)
            call da_stats_calculate (n, 0, oi%Buoy(n)%v%qc, & 
                               re%Buoy(n)%v, nv, & 
                               buoy%minimum%v, Buoy%maximum%v, &
                               buoy%average%v, Buoy%rms_err%v)
            call da_stats_calculate (n, 0, oi%Buoy(n)%t%qc, & 
                               re%Buoy(n)%t, nt, & 
                               buoy%minimum%t, Buoy%maximum%t, &
                               buoy%average%t, Buoy%rms_err%t)
            call da_stats_calculate (n, 0, oi%Buoy(n)%p%qc, & 
                               re%Buoy(n)%p, np, & 
                               buoy%minimum%p, Buoy%maximum%p, &
                               buoy%average%p, Buoy%rms_err%p)
            call da_stats_calculate (n, 0, oi%Buoy(n)%q%qc, & 
                               re%Buoy(n)%q, nq, & 
                               Buoy%minimum%q, Buoy%maximum%q, &
                               Buoy%average%q, Buoy%rms_err%q)

          end if    ! end if (oi%Buoy(n)%loc%proc_domain)
       end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   call da_proc_sum_int (nt)
   call da_proc_sum_int (np)
   call da_proc_sum_int (nq)
   num_Buoy_tot = nu + nv + nt + np + nq

   call da_proc_stats_combine(Buoy%average%u, Buoy%rms_err%u, &
                           Buoy%minimum%u%value, Buoy%maximum%u%value, &
                           Buoy%minimum%u%n, Buoy%maximum%u%n, &
                           Buoy%minimum%u%l, Buoy%maximum%u%l)
   call da_proc_stats_combine(Buoy%average%v, Buoy%rms_err%v, &
                           Buoy%minimum%v%value, Buoy%maximum%v%value, &
                           Buoy%minimum%v%n, Buoy%maximum%v%n, &
                           Buoy%minimum%v%l, Buoy%maximum%v%l)
   call da_proc_stats_combine(Buoy%average%t, Buoy%rms_err%t, &
                           Buoy%minimum%t%value, Buoy%maximum%t%value, &
                           Buoy%minimum%t%n, Buoy%maximum%t%n, &
                           Buoy%minimum%t%l, Buoy%maximum%t%l)
   call da_proc_stats_combine(Buoy%average%p, Buoy%rms_err%p, &
                           Buoy%minimum%p%value, Buoy%maximum%p%value, &
                           Buoy%minimum%p%n, Buoy%maximum%p%n, &
                           Buoy%minimum%p%l, Buoy%maximum%p%l)
   call da_proc_stats_combine(Buoy%average%q, Buoy%rms_err%q, &
                           Buoy%minimum%q%value, Buoy%maximum%q%value, &
                           Buoy%minimum%q%n, Buoy%maximum%q%n, &
                           Buoy%minimum%q%l, Buoy%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Buoy'
         call da_print_stats_buoy(stats_unit, nu, nv, nt, np, nq, Buoy)
      end if
   end if

 end subroutine da_ao_stats_buoy


