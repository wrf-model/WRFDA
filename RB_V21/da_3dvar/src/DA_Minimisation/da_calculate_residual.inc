      subroutine da_calculate_residual( iv, y, re )

      implicit none
      
      type (ob_type), intent(in)        :: iv     ! Innovation vector (O-B).
      type (y_type), intent(in)         :: y      ! y = H (xa)
      type (y_type), intent(out)        :: re     ! Residual (O-A).

      integer    :: np_available, np_obs_used, np_missing  , np_bad_data 
      
      np_available = 0
      np_obs_used  = 0
      np_missing   = 0
      np_bad_data  = 0

!-------------------------------------------------------------------------
!     [1.0] (O-A) = (O-B) - H x~:
!-------------------------------------------------------------------------

!     for radiosonde:

      call da_calculate_residual_Sound( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

      call da_calculate_residual_Sonde_sfc( iv, y, re, np_missing, np_bad_data, &
                                            np_obs_used, np_available )

!     for surface

      call da_calculate_residual_Synop( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for Geo. Satellite AMVs :

      call da_calculate_residual_GeoAMV( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for Polar Satellite AMVs :

      call da_calculate_residual_PolarAMV( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for airep:

      call da_calculate_residual_Airep( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for metar

      call da_calculate_residual_Metar( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for ships

      call da_calculate_residual_Ships( iv, y, re, np_missing, np_bad_data, & 
                                        np_obs_used, np_available )

!  GPSPW:

      call da_calculate_residual_Gpspw( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available ) 

!  GPSRF:

      call da_calculate_residual_Gpsref( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available ) 

!     for SSMI

      call DA_Cal_Residual_SSMI( iv, y, re, np_missing, np_bad_data, &
                                 np_obs_used, np_available )
!     for SSMT1

      call DA_Cal_Residual_ssmt1( iv, y, re, np_missing, np_bad_data, &
                                  np_obs_used, np_available )

!     for SSMT2

      call DA_Cal_Residual_ssmt2( iv, y, re, np_missing, np_bad_data, &
                                  np_obs_used, np_available )
                                 
!     for pilot

      call da_calculate_residual_Pilot( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for TC bogus

      call da_calculate_residual_bogus( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for Satem

      call da_calculate_residual_Satem( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!    Pseudo-obs

      call da_calculate_residual_Pseudo( iv, y, re, &
                                         np_missing, np_bad_data, &
                                         np_obs_used, np_available )
!    Quikscat scatterometer:

      call da_calculate_residual_qscat( iv, y, re, &
                                        np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!    for Radar

      call da_calculate_residual_Radar( iv, y, re, &
                                        np_missing, np_bad_data, &
                                        np_obs_used, np_available )


!     for profiler

      call da_calculate_residual_Profiler( iv, y, re, np_missing, np_bad_data, &
                                        np_obs_used, np_available )

!     for buoy   

      call da_calculate_residual_Buoy( iv, y, re, np_missing, np_bad_data, & 
                                        np_obs_used, np_available )

      end subroutine da_calculate_residual
