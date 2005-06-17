SUBROUTINE DA_Allocate_Observations ( ob )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate components of observation structure.
!
!  METHOD:  Allocate components in turn.
!
!  HISTORY:
!           01/06/2000 - Creation of F90 version.           Dale Barker
!           07/08/2003 - Updated for Profiler Data          S. R. H. Rizvi
!           07/08/2003 - Updated for Buoy     Data          S. R. H. Rizvi
!           05/27/2004 - GPS refractivity                   Y.-R. Guo
!
!  PARENT_MODULE: DA_Define_Structures
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (ob_type), intent(inout)        :: ob     ! Observation structure.

!------------------------------------------------------------------------------

   if(ob%num_sound > 0) then
      ALLOCATE(ob%sound(1:ob%num_sound))

      ALLOCATE(ob%sonde_sfc(1:ob%num_sound))
   endif

   if(ob%num_synop > 0) &
      ALLOCATE(ob%synop(1:ob%num_synop))

   if(ob%num_airep > 0) &
      ALLOCATE(ob%airep(1:ob%num_airep))

   if(ob%num_geoamv> 0) &
      ALLOCATE(ob%geoamv(1:ob%num_geoamv))

   if(ob%num_polaramv> 0) &
      ALLOCATE(ob%polaramv(1:ob%num_polaramv))

   if(ob%num_satem > 0) &
      ALLOCATE(ob%satem(1:ob%num_satem))

   if(ob%num_metar > 0) &
      ALLOCATE(ob%metar(1:ob%num_metar))

   if(ob%num_ships > 0) &
      ALLOCATE(ob%ships(1:ob%num_ships))

   if(ob%num_pilot > 0) &
      ALLOCATE(ob%pilot(1:ob%num_pilot))

   if(ob%num_gpspw > 0) &
      ALLOCATE(ob%gpspw(1:ob%num_gpspw))

   if(ob%num_gpsref > 0) &
      ALLOCATE(ob%gpsref(1:ob%num_gpsref))

   if(ob%num_ssmi_tb > 0) &
      ALLOCATE(ob%ssmi_tb(1:ob%num_ssmi_tb))

   if(ob%num_ssmi_retrieval > 0) &
      ALLOCATE(ob%ssmi_retrieval(1:ob%num_ssmi_retrieval))

   if(ob%num_ssmt1 > 0) &
      ALLOCATE(ob%ssmt1(1:ob%num_ssmt1))

   if(ob%num_ssmt2 > 0) &
      ALLOCATE(ob%ssmt2(1:ob%num_ssmt2))

   if(ob%num_qscat > 0) &
      ALLOCATE(ob%qscat(1:ob%num_qscat))

  if(ob%num_profiler > 0) &
      ALLOCATE(ob%profiler(1:ob%num_profiler))

   if(ob%num_buoy     > 0) &
      ALLOCATE(ob%buoy(1:ob%num_buoy))

   if(ob%num_Radar     > 0) &
      ALLOCATE(ob%Radar(1:ob%num_Radar))

   if(ob%num_Bogus     > 0) &
      ALLOCATE(ob%Bogus(1:ob%num_Bogus))

END SUBROUTINE DA_Allocate_Observations
