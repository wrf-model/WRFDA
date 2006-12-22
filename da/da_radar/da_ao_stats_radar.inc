subroutine da_ao_stats_radar( stats_unit, oi, re )

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type ( y_type), intent (in)      :: re            ! A - O

   type (stats_Radar_type)          :: Radar
   integer                          :: nrv, nrf
   integer                          :: n, k

   nrv = 0
   nrf = 0
   num_Radar_tot = 0.

   Radar%maximum%rv = maxmin_type( missing_r, 0, 0)
   Radar%maximum%rf = maxmin_type( missing_r, 0, 0)
   Radar%minimum%rv = maxmin_type(-missing_r, 0, 0)
   Radar%minimum%rf = maxmin_type(-missing_r, 0, 0)
   Radar%average = residual_Radar1_type(0.0, 0.0)
   Radar%rms_err = Radar%average

   if (oi%num_Radar > 0) then
      do n=1, oi%num_Radar
         if (oi%Radar(n)%loc%proc_domain) then
            do k=1, oi%Radar(n)%info%levels
               if ( use_Radar_rv ) then
                  call da_stats_calculate( n, k, oi%Radar(n)%rv(k)%qc, & 
                                     re%Radar(n)%rv(k), nrv, & 
                                     Radar%minimum%rv, Radar%maximum%rv, &
                                     Radar%average%rv, Radar%rms_err%rv )
               end if

               if ( use_Radar_rf ) then
                  call da_stats_calculate( n, k, oi%Radar(n)%rf(k)%qc, & 
                                     re%Radar(n)%rf(k), nrf, & 
                                     Radar%minimum%rf, Radar%maximum%rf, &
                                     Radar%average%rf, Radar%rms_err%rf )
               end if
            end do
         end if     ! end if( oi%pilot(n)%loc%proc_domain )
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   if (use_Radar_rv) then
      call da_proc_sum_int( nrv )
      call da_proc_stats_combine(Radar%average%rv, Radar%rms_err%rv, &
                             Radar%minimum%rv%value, Radar%maximum%rv%value, &
                             Radar%minimum%rv%n, Radar%maximum%rv%n, &
                             Radar%minimum%rv%l, Radar%maximum%rv%l )
   end if

   if (use_Radar_rf) then
      call da_proc_sum_int( nrf )
      call da_proc_stats_combine(Radar%average%rf, Radar%rms_err%rf, &
                             Radar%minimum%rf%value, Radar%maximum%rf%value, &
                             Radar%minimum%rf%n, Radar%maximum%rf%n, &
                             Radar%minimum%rf%l, Radar%maximum%rf%l )
   end if
   num_Radar_tot = nrv + nrf

   if (rootproc) then

      if (nrv /= 0 .or. nrf /= 0) then 
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Radar'
         call da_print_stats_radar(stats_unit, nrv, nrf, Radar)
      end if
   end if

end subroutine da_ao_stats_radar


