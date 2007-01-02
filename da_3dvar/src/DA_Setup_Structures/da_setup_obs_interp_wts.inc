SUBROUTINE DA_Setup_Obs_Interp_Wts ( xp, iv )
!-------------------------------------------------------------------------
!  HISTORY:
!
!       Modified:         07/08/2003                        Syed RH Rizvi
!       Included the Profiler and Buoy obs
!
!           03/04/2005 - AMV's from Geostationary and       Syed RH Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN)  :: xp         ! Domain decomposition vars.
   TYPE (ob_type), INTENT(INOUT)  :: iv         ! Innovation vector (O-B).

   INTEGER                        :: n          ! Loop counter.

   do n=1, iv % num_sound
     CALL DA_Store_Obs_Grid_Info( xp, iv%sound(n)%loc )

     CALL DA_Store_Obs_Grid_Info( xp, iv%sonde_sfc(n)%loc )
   enddo

   do n=1, iv % num_synop
     CALL DA_Store_Obs_Grid_Info( xp, iv%synop(n)%loc )
   enddo


   do n=1, iv % num_geoamv
     CALL DA_Store_Obs_Grid_Info( xp, iv%geoamv(n)%loc )
   enddo

   do n=1, iv % num_polaramv
     CALL DA_Store_Obs_Grid_Info( xp, iv%polaramv(n)%loc )
   enddo

   do n=1, iv % num_airep
     CALL DA_Store_Obs_Grid_Info( xp, iv%airep(n)%loc )
   enddo

   do n=1, iv % num_metar
     CALL DA_Store_Obs_Grid_Info( xp, iv%metar(n)%loc )
   enddo

   do n=1, iv % num_ships
     CALL DA_Store_Obs_Grid_Info( xp, iv%ships(n)%loc )
   enddo

   do n=1, iv % num_gpspw
     CALL DA_Store_Obs_Grid_Info( xp, iv%gpspw(n)%loc )
   enddo

   do n=1, iv % num_gpsref
     CALL DA_Store_Obs_Grid_Info( xp, iv%gpsref(n)%loc )
   enddo

   do n=1, iv % num_ssmi_tb
     CALL DA_Store_Obs_Grid_Info( xp, iv%ssmi_tb(n)%loc )
   enddo

   do n=1, iv % num_ssmi_retrieval
     CALL DA_Store_Obs_Grid_Info( xp, iv%ssmi_retrieval(n)%loc )
   enddo

   do n=1, iv % num_pilot
     CALL DA_Store_Obs_Grid_Info( xp, iv%pilot(n)%loc )
   enddo

   do n=1, iv % num_Radar
     CALL DA_Store_Obs_Grid_Info( xp, iv%Radar(n)%loc )
   enddo

   do n=1, iv % num_pseudo
     CALL DA_Store_Obs_Grid_Info( xp, iv%pseudo(n)%loc )
   enddo

   do n=1, iv % num_satem
     CALL DA_Store_Obs_Grid_Info( xp, iv%satem(n)%loc )
   enddo

   do n=1, iv % num_ssmt1
     CALL DA_Store_Obs_Grid_Info( xp, iv%ssmt1(n)%loc )
   enddo

   do n=1, iv % num_ssmt2
     CALL DA_Store_Obs_Grid_Info( xp, iv%ssmt2(n)%loc )
   enddo
   
   do n=1, iv % num_qscat
     CALL DA_Store_Obs_Grid_Info( xp, iv%qscat(n)%loc )
   enddo

   do n=1, iv % num_profiler
     CALL DA_Store_Obs_Grid_Info( xp, iv%profiler(n)%loc )
   enddo

   do n=1, iv % num_buoy
     CALL DA_Store_Obs_Grid_Info( xp, iv%buoy(n)%loc )
   enddo

   do n=1, iv % num_bogus
     CALL DA_Store_Obs_Grid_Info( xp, iv%bogus(n)%loc )
   enddo

END SUBROUTINE DA_Setup_Obs_Interp_Wts

