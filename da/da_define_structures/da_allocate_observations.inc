subroutine da_allocate_observations  (ob)

   !---------------------------------------------------------------------------
   ! PURPOSE: Allocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(inout)        :: ob     ! Observation structure.

   if (trace_use) call da_trace_entry("da_allocate_observations")

   if (ob%num_sound > 0) then
      allocate(ob%sound(1:ob%num_sound))

      allocate(ob%sonde_sfc(1:ob%num_sound))
   end if

   if (ob%num_synop > 0) &
      allocate(ob%synop(1:ob%num_synop))

   if (ob%num_airep > 0) &
      allocate(ob%airep(1:ob%num_airep))

   if (ob%num_geoamv> 0) &
      allocate(ob%geoamv(1:ob%num_geoamv))

   if (ob%num_polaramv> 0) &
      allocate(ob%polaramv(1:ob%num_polaramv))

   if (ob%num_satem > 0) &
      allocate(ob%satem(1:ob%num_satem))

   if (ob%num_metar > 0) &
      allocate(ob%metar(1:ob%num_metar))

   if (ob%num_ships > 0) &
      allocate(ob%ships(1:ob%num_ships))

   if (ob%num_pilot > 0) &
      allocate(ob%pilot(1:ob%num_pilot))

   if (ob%num_gpspw > 0) &
      allocate(ob%gpspw(1:ob%num_gpspw))

   if (ob%num_gpsref > 0) &
      allocate(ob%gpsref(1:ob%num_gpsref))

   if (ob%num_ssmi_tb > 0) &
      allocate(ob%ssmi_tb(1:ob%num_ssmi_tb))

   if (ob%num_ssmi_retrieval > 0) &
      allocate(ob%ssmi_retrieval(1:ob%num_ssmi_retrieval))

   if (ob%num_ssmt1 > 0) &
      allocate(ob%ssmt1(1:ob%num_ssmt1))

   if (ob%num_ssmt2 > 0) &
      allocate(ob%ssmt2(1:ob%num_ssmt2))

   if (ob%num_qscat > 0) &
      allocate(ob%qscat(1:ob%num_qscat))

   if (ob%num_profiler > 0) &
      allocate(ob%profiler(1:ob%num_profiler))

   if (ob%num_buoy     > 0) &
      allocate(ob%buoy(1:ob%num_buoy))

   if (ob%num_Radar     > 0) &
      allocate(ob%Radar(1:ob%num_Radar))

   if (ob%num_Bogus     > 0) &
      allocate(ob%Bogus(1:ob%num_Bogus))

   if (ob%num_airsr     > 0) &
      allocate(ob%airsr(1:ob%num_airsr))

   if (trace_use) call da_trace_exit("da_allocate_observations")

end subroutine da_allocate_observations


