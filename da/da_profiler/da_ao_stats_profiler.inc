subroutine da_ao_stats_profiler( stats_unit, oi, re )

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type ( y_type), intent (in)      :: re            ! A - O

   type (stats_profiler_type)       :: Profiler
   integer                          :: nu, nv
   integer                          :: n, k

   nu = 0
   nv = 0
   num_profiler_tot = 0

   Profiler%maximum%u = maxmin_type( missing_r, 0, 0)
   Profiler%maximum%v = maxmin_type( missing_r, 0, 0)
   Profiler%minimum%u = maxmin_type(-missing_r, 0, 0)
   Profiler%minimum%v = maxmin_type(-missing_r, 0, 0)
   Profiler%average = residual_profiler1_type(0.0, 0.0)
   Profiler%rms_err = Profiler%average

   if ( oi%num_Profiler > 0 ) then

      do n=1, oi%num_Profiler

      if( oi%profiler(n)%loc%proc_domain ) then

       do k=1, oi%Profiler(n)%info%levels
         call da_stats_calculate( n, k, oi%Profiler(n)%u(k)%qc, & 
                               re%Profiler(n)%u(k), nu, & 
                               profiler%minimum%u, Profiler%maximum%u, &
                               profiler%average%u, Profiler%rms_err%u )
         call da_stats_calculate( n, k, oi%Profiler(n)%v(k)%qc, & 
                               re%Profiler(n)%v(k), nv, & 
                               Profiler%minimum%v, Profiler%maximum%v, &
                               Profiler%average%v, Profiler%rms_err%v )
       end do
      end if    ! end if( oi%profiler(n)%loc%proc_domain )
      end do
      
   end if

! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( nu )
   call da_proc_sum_int( nv )
   num_profiler_tot = nu + nv
   
   call da_proc_stats_combine(Profiler%average%u, Profiler%rms_err%u, &
                           Profiler%minimum%u%value, Profiler%maximum%u%value, &
                           Profiler%minimum%u%n, Profiler%maximum%u%n, &
                           Profiler%minimum%u%l, Profiler%maximum%u%l )
   call da_proc_stats_combine(Profiler%average%v, Profiler%rms_err%v, &
                           Profiler%minimum%v%value, Profiler%maximum%v%value, &
                           Profiler%minimum%v%n, Profiler%maximum%v%n, &
                           Profiler%minimum%v%l, Profiler%maximum%v%l )
   
   if ( rootproc ) then
   
    if ( nu /= 0 .or. nv /= 0 ) then
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Profiler'
   
      call da_print_stats_profiler(stats_unit, nu, nv, Profiler)
   
    end if
   
   end if

end subroutine da_ao_stats_profiler


