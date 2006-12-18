SUBROUTINE da_transform_xtoy( xb, iv, xa, xp, y )
   
   implicit none
   
   type (xb_type), intent(in)    :: xb       ! first guess state.
   type (ob_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xpose_type), intent(in) :: xp       ! Domain decomposition vars.
   type (y_type), intent(inout)  :: y        ! y = h (xa)

   integer :: i, j
   
!-----------------------------------------------------------------------------
!  [1.0] Radiosonde observation operator y = H(x):
!-----------------------------------------------------------------------------
  
   call da_transform_xtoy_Sound ( xa, iv, xp, y ) 
   call da_transform_xtoy_Sonde_sfc ( xa, xb, iv, xp, y ) 

!------------------------------------------------------------------------------
!  [2.0] Surface observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Synop ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [3.0] Cloud track wind observation operator y = H(x):
!------------------------------------------------------------------------------
!  Geostationary Satellite
   call da_transform_xtoy_GeoAMV( xa, iv, xp, y )
!  Polar orbitting Satellite
   call da_transform_xtoy_PolarAMV( xa, iv, xp, y )


!------------------------------------------------------------------------------
!  [4.0] Aircraft observation operator y = H(x):
!------------------------------------------------------------------------------
   
   call da_transform_xtoy_Airep ( xa, iv, xp, y )

!------------------------------------------------------------------------------
!  [5.0] Metar observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Metar ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [6.0] Ship observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Ships ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [7.0] GPS TPW/ZTD observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Gpspw ( xb, iv, xa, xp, y )

!------------------------------------------------------------------------------
!  [8.0] SSMI observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_SSMI ( xb, iv, xa, xp, y )

!------------------------------------------------------------------------------
!  [9.0] Pilot observation operator y = H(x):
!------------------------------------------------------------------------------
  
   call da_transform_xtoy_Pilot ( xa, iv, xp, y )

!------------------------------------------------------------------------------
!  [10.0] Pseudo obs observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_pseudo ( xa, iv, xp, y )

!------------------------------------------------------------------------------
!  [11.0] Satem observation operator y = H(x):
!------------------------------------------------------------------------------
   
   call da_transform_xtoy_Satem ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [12.0] SSM/T1 observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1 ( xa, iv, xp, y )
   
!------------------------------------------------------------------------------
!  [13.0] SSM/T2 observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2 ( xa, iv, xp, y )
   
!------------------------------------------------------------------------------
!  [14.0] Scatterometer observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_qscat ( xa, iv, xp, y )

!------------------------------------------------------------------------------
!  [15.0] Profiler observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_profiler ( xa, iv, xp, y )

!------------------------------------------------------------------------------
!  [16.0] Buoy observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_buoy  ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [17.0] GPS Refractivity observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Gpsref ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [18.0] Radar observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Radar ( xa, xb, iv, xp, y )

!------------------------------------------------------------------------------
!  [19.0] TC bogus observation operator y = H(x):
!------------------------------------------------------------------------------

   call da_transform_xtoy_Bogus ( xa, xb, iv, xp, y )

END SUBROUTINE da_transform_xtoy
