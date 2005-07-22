      subroutine DA_Write_Diagnostics( ob, iv, re, y, xp, xa, j )
!---- -------------------------------------------------------------------------
!  PURPOSE: Output data assimilation diagnostics.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 04/07/2000 - Creation of F90 version.           Dale Barker
!           03/04/2005 - CMV's from Geostationary and       Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!
!
!  PARENT_MODULE: DA_Diagnostics
!------------------------------------------------------------------------------
   
      implicit none
      
      type (y_type), intent(in)    :: ob      ! Observation structure.
      type (ob_type), intent(inout):: iv      ! innovation vector.
      type (y_type), intent(inout) :: re      ! residual vector.
      type (y_type), intent(in)    :: y       ! y = H(x_inc) structure.
      type (xpose_type), intent(in):: xp     ! Dimensions and xpose buffers.
      type (x_type), intent(in)    :: xa      ! gridded analysis increments.
      type (j_type), intent(inout) :: j       ! Cost function.

!------------------------------------------------------------------------------
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor     ! For statistics output.
!------------------------------------------------------------------------------
 
!---- -------------------------------------------------------------------------
!  [1.0] Calculate innovation vector (O-B) statistics:
!------------------------------------------------------------------------------
   
!  [1.1] Sondes:

      call DA_OI_Stats_Sound ( stats_unit, iv )

      call DA_OI_Stats_Sonde_sfc ( stats_unit, iv )

!  [1.2] Surface:

      call DA_OI_Stats_Synop ( stats_unit, iv )
 
!  [1.3.1] Geostationary Satellite Cloud track winds:

      call DA_OI_Stats_GeoAMV ( stats_unit, iv )


!  [1.3.2] Polar Satellite Cloud track winds:

      call DA_OI_Stats_PolarAMV ( stats_unit, iv )


!  [1.4] Aireps:

      call DA_OI_Stats_Airep ( stats_unit, iv )

!  [1.5] Pilot:

      call DA_OI_Stats_Pilot ( stats_unit, iv )

!  [1.6] Metar:

      call DA_OI_Stats_Metar ( stats_unit, iv )

!  [1.7] Ships:

      call DA_OI_Stats_Ships ( stats_unit, iv )

!  [1.8] GPSPW:

      call DA_OI_Stats_Gpspw ( stats_unit, iv )

!  [1.9] GPSPW:

      call DA_OI_Stats_Gpsref ( stats_unit, iv )

!  [1.10] SSMI:

      call DA_OI_Stats_SSMI ( stats_unit, iv )

!  [1.11] SATEMs:

      call  DA_OI_Stats_Satem ( stats_unit, iv )

!  [1.12] Pseudo obs:

      call da_ob_stats_pseudo ( stats_unit, iv )

!  [1.13] SSM/T1:

      call da_ob_stats_ssmt1 ( stats_unit, iv )

!  [1.14] SSM/T2:

      call da_ob_stats_ssmt2 ( stats_unit, iv )
      
!  [1.15] Quikscat:

      call da_ob_stats_qscat ( stats_unit, iv )

!  [1.16] Profiler:

      call DA_OI_Stats_Profiler ( stats_unit, iv )

!  [1.17] Buoy:  

      call DA_OI_Stats_Buoy ( stats_unit, iv )

!  [1.18] Radar: 

      call DA_OI_Stats_Radar ( stats_unit, iv )

!  [1.19] TC bogus:

      call DA_OI_Stats_bogus ( stats_unit, iv )


!---- -------------------------------------------------------------------------
!  [2.0] Calculate residual vector (O-A) statistics:
!------------------------------------------------------------------------------

!  [2.1] Sondes:

      num_sound_tot = 0
      call DA_AO_Stats_Sound ( stats_unit, iv, re )
      call DA_AO_Stats_Sonde_sfc ( stats_unit, iv, re )

!  [2.2] Surface:

      call DA_AO_Stats_Synop ( stats_unit, iv, re )

!  [2.3.1] Geostationary Satellite Cloud track winds:

      call DA_AO_Stats_GeoAMV ( stats_unit, iv, re )


!  [2.3.2] Polar Satellite Cloud track winds:

      call DA_AO_Stats_PolarAMV ( stats_unit, iv, re )



!  [2.4] Aireps:

      call DA_AO_Stats_Airep ( stats_unit, iv, re )

!  [2.5] Pilot:

      call DA_AO_Stats_Pilot ( stats_unit, iv, re )

!  [2.6] Metar:

      call DA_AO_Stats_Metar ( stats_unit, iv, re )

!  [2.7] Ships:

      call DA_AO_Stats_Ships ( stats_unit, iv, re )

!  [2.8] GPSPW:

      call DA_AO_Stats_Gpspw ( stats_unit, iv, re )

!  [2.9] GPSRF:

      call DA_AO_Stats_Gpsref ( stats_unit, iv, re )

!  [2.10] SSMI:

      call DA_AO_Stats_SSMI ( stats_unit, iv, re )

!  [2.11] SATEMs:

      call DA_AO_Stats_Satem ( stats_unit, iv, re )
   
!  [2.12] Pseudo obs:

      call da_oa_stats_pseudo ( stats_unit, iv, re )

!  [2.13] SSM/T1:

      call da_oa_stats_ssmt1 ( stats_unit, iv, re )
      
!  [2.14] SSM/T2:

      call da_oa_stats_ssmt2 ( stats_unit, iv, re )
      
!  [2.15] Quikscat:

      call da_oa_stats_qscat ( stats_unit, iv, re )

!  [2.16] Profiler

      call DA_AO_Stats_Profiler ( stats_unit, iv, re )

!  [2.17] Buoy: 

      call DA_AO_Stats_Buoy ( stats_unit, iv, re )

!  [2.18] Radar:

      call DA_AO_Stats_Radar ( stats_unit, iv, re )

!  [2.19] TC bogus:

      call DA_AO_Stats_Bogus ( stats_unit, iv, re )

!---- -------------------------------------------------------------------------
!  [3.0] Calculate analysis increment (A-B) statistics:
!------------------------------------------------------------------------------

      call DA_Analysis_Stats ( stats_unit, xp, xa )
                    
!---- -------------------------------------------------------------------------
!  [4.0] Write VAR diagnostic :
!------------------------------------------------------------------------------
      call da_get_var_diagnostics ( iv, j )

!---- -------------------------------------------------------------------------
!  [5.0] Write observation data (O, O-B, O-A, y=hx'):
!------------------------------------------------------------------------------
!rizvi   if ( wrf_dm_on_monitor()  ) then
      call da_write_obs( ob, iv, re )
!rizvi   end if
   if ( wrf_dm_on_monitor()  ) then
      call da_write_y( iv, y )
   end if
 
   end subroutine DA_Write_Diagnostics
