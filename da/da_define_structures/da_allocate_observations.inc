subroutine da_allocate_observations  (ob)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(inout) :: ob     ! Observation structure.

   if (trace_use) call da_trace_entry("da_allocate_observations")

   if (ob%nlocal(sound) > 0) then
      allocate(ob%sound     (1:ob%nlocal(sound)))
      allocate(ob%sonde_sfc (1:ob%nlocal(sound)))
   end if

   if (ob%nlocal(synop)     > 0) allocate(ob%synop    (1:ob%nlocal(synop)))
   if (ob%nlocal(airep)     > 0) allocate(ob%airep    (1:ob%nlocal(airep)))
   if (ob%nlocal(geoamv)    > 0) allocate(ob%geoamv   (1:ob%nlocal(geoamv)))
   if (ob%nlocal(polaramv)  > 0) allocate(ob%polaramv (1:ob%nlocal(polaramv)))
   if (ob%nlocal(satem)     > 0) allocate(ob%satem    (1:ob%nlocal(satem)))
   if (ob%nlocal(metar)     > 0) allocate(ob%metar    (1:ob%nlocal(metar)))
   if (ob%nlocal(ships)     > 0) allocate(ob%ships    (1:ob%nlocal(ships)))
   if (ob%nlocal(pilot)     > 0) allocate(ob%pilot    (1:ob%nlocal(pilot)))
   if (ob%nlocal(gpspw)     > 0) allocate(ob%gpspw    (1:ob%nlocal(gpspw)))
   if (ob%nlocal(gpsref)    > 0) allocate(ob%gpsref   (1:ob%nlocal(gpsref)))
   if (ob%nlocal(ssmi_tb)   > 0) allocate(ob%ssmi_tb  (1:ob%nlocal(ssmi_tb)))
   if (ob%nlocal(ssmi_rv)   > 0) allocate(ob%ssmi_rv  (1:ob%nlocal(ssmi_rv)))
   if (ob%nlocal(ssmt1)     > 0) allocate(ob%ssmt1    (1:ob%nlocal(ssmt1)))
   if (ob%nlocal(ssmt2)     > 0) allocate(ob%ssmt2    (1:ob%nlocal(ssmt2)))
   if (ob%nlocal(qscat)     > 0) allocate(ob%qscat    (1:ob%nlocal(qscat)))
   if (ob%nlocal(profiler)  > 0) allocate(ob%profiler (1:ob%nlocal(profiler)))
   if (ob%nlocal(buoy)      > 0) allocate(ob%buoy     (1:ob%nlocal(buoy)))
   if (ob%nlocal(radar)     > 0) allocate(ob%radar    (1:ob%nlocal(radar)))
   if (ob%nlocal(bogus)     > 0) allocate(ob%bogus    (1:ob%nlocal(bogus)))
   if (ob%nlocal(airsr)     > 0) allocate(ob%airsr    (1:ob%nlocal(airsr)))

   if (trace_use) call da_trace_exit("da_allocate_observations")

end subroutine da_allocate_observations


