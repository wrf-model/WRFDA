SUBROUTINE da_setup_obs_structures( xp, ob, iv )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate and read in components of observation structure.
!
!  METHOD:  Allocate and read in components of observation structure.
!
!  HISTORY: 10/05/2001 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!           02/12/2002 - Compute interplation weights.      Al Bourgeois
!              07/2005 - Add radiance part                  Zhiquan Liu
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), INTENT(IN):: xp          ! Domain decomposition vars. 
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   write (6,'(A)') ' ------------------------------'
   write (6,'(A)') ' [4.0] Set up observations (ob)'
   write (6,'(A)') ' ------------------------------'
   write (6,*)

!------------------------------------------------------------------------------      
!  [1.0] Setup and read in fields from first guess:
!------------------------------------------------------------------------------      

   iv%missing = missing
!  iv%ptop    = xb%ptop

   iv%total_obs         = 0
   iv%total_rad_pixel   = 0
   iv%total_rad_channel = 0
   
   iv%num_sound=0
   iv%num_synop=0
   iv%num_geoamv=0
   iv%num_polaramv=0
   iv%num_airep=0
   iv%num_metar=0
   iv%num_ships=0
   iv%num_pilot=0
   iv%num_satem=0
   iv%num_gpspw=0
   iv%num_gpsref=0
   iv%num_qscat=0
   iv%num_ssmt1=0
   iv%num_ssmt2=0
   iv%num_ssmi_tb=0
   iv%num_ssmi_retrieval=0
   iv%num_buoy=0
   iv%num_profiler=0
   iv%num_radar=0
   iv%num_bogus=0
   iv%num_inst=0

   iv%num_sound_glo=0
   iv%num_synop_glo=0
   iv%num_geoamv_glo=0
   iv%num_polaramv_glo=0
   iv%num_airep_glo=0
   iv%num_metar_glo=0
   iv%num_ships_glo=0
   iv%num_pilot_glo=0
   iv%num_satem_glo=0
   iv%num_gpspw_glo=0
   iv%num_gpsref_glo=0
   iv%num_qscat_glo=0
   iv%num_ssmt1_glo=0
   iv%num_ssmt2_glo=0
   iv%num_ssmi_tb_glo=0
   iv%num_ssmi_retrieval_glo=0
   iv%num_buoy_glo=0
   iv%num_profiler_glo=0
   iv%num_Radar_glo=0
   iv%num_bogus_glo=0

   iv%num_pseudo=num_pseudo
   iv%num_pseudo_glo=iv%num_pseudo

   IF (use_obsgts) then
     ! Conventional obs can be in BUFR or ascii format
     IF ( ob_format == 1 ) THEN
       CALL wrf_message('Using MM5 format observation input')
       ! [1.1] BUFR observation input file:
       IF (iv%num_pseudo > 0) THEN
         CALL wrf_message('Pseudo test only.')
         CALL DA_Allocate_Y( iv, ob )
         CALL da_setup_pseudo_obs( iv, ob )
         iv%pseudo(1)%loc%i=int(iv%pseudo(1)%loc%x)
         iv%pseudo(1)%loc%j=int(iv%pseudo(1)%loc%y)
         iv%pseudo(1)%loc%dx=iv%pseudo(1)%loc%x-real(iv%pseudo(1)%loc%i)
         iv%pseudo(1)%loc%dy=iv%pseudo(1)%loc%y-real(iv%pseudo(1)%loc%j)
         iv%pseudo(1)%loc%dxm=1.0-iv%pseudo(1)%loc%dx
         iv%pseudo(1)%loc%dym=1.0-iv%pseudo(1)%loc%dy
       ELSE
         CALL da_setup_obs_structures_bufr( xp, ob, iv )
       END IF 
     ELSE IF ( ob_format == 2 ) THEN
       ! [1.2] MM5 observation input file:
       CALL da_setup_obs_structures_ascii( xp, ob, iv )
     END IF
   END IF

   ! Radiance files can only be in BUFR

   IF (use_radiance) then
     CALL wrf_message('Using NCEP BUFR radiance 1b input')
     CALL da_setup_bufrtovs_structures( xp, ob, iv )
     ! These are per processor, so uninteresting
     ! write (6, *) ' iv%total_rad_channel = ', iv%total_rad_channel
     ! write (6, *) ' iv%total_rad_pixel   = ', iv%total_rad_pixel
   END IF

!     Get horizontal interpolation weights.
      call da_setup_obs_interp_wts( xp, iv )     

END SUBROUTINE da_setup_obs_structures

