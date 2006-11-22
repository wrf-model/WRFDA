subroutine da_transform_xtoy( xb, iv, xa, xp, y )

   implicit none
   
   type (xb_type), intent(in)    :: xb       ! first guess state.
   type (ob_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xpose_type), intent(in) :: xp       ! Domain decomposition vars.
   type (y_type), intent(inout)  :: y        ! y = h (xa)

   if (trace_use) call da_trace_entry("da_transform_xtoy")
   
   !--------------------------------------------------------------------------
   ! [1.0] Radiosonde observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   call da_transform_xtoy_sound ( xa, iv, xp, y ) 
   call da_transform_xtoy_sonde_sfc ( xa, xb, iv, xp, y ) 

   !---------------------------------------------------------------------------
   ! [2.0] Surface observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_synop ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [3.0] Cloud track wind observation operator y = H(x):
   !---------------------------------------------------------------------------

   ! Geostationary Satellite
   call da_transform_xtoy_geoamv( xa, iv, xp, y )
   ! Polar orbiting Satellite
   call da_transform_xtoy_polaramv( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [4.0] Aircraft observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_airep ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [5.0] Metar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_metar ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [6.0] Ship observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ships ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [7.0] GPS TPW/ZTD observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpspw ( xb, iv, xa, xp, y )


   !---------------------------------------------------------------------------
   ! [8.0] SSMI observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_ssmi ( xb, iv, xa, y )

   !---------------------------------------------------------------------------
   ! [9.0] Pilot observation operator y = H(x):
   !---------------------------------------------------------------------------
  
   call da_transform_xtoy_pilot ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [10.0] Pseudo obs observation operator y = H(x):
   !----------------------------------------------------------------------------

   call da_transform_xtoy_pseudo ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [11.0] Satem observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_satem ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [12.0] SSM/T1 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1 ( xa, iv, xp, y )
   
   !---------------------------------------------------------------------------
   ! [13.0] SSM/T2 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2 ( xa, iv, xp, y )
   
   !---------------------------------------------------------------------------
   ! [14.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_qscat ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [15.0] Profiler observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_profiler ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [16.0] Buoy observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_buoy  ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [17.0] GPS Refractivity observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpsref ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [18.0] Radar observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_radar ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [19.0] TC bogus observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_bogus ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------------
   ! [20.0] AIRS retrievals observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airsr ( xa, iv, xp, y ) 

   !---------------------------------------------------------------------------
   ! [20.0] Radiance observation operator y = H(x):
   !---------------------------------------------------------------------------

    if (rtm_option == 1) then
       call da_transform_xtoy_rad ( xa, iv, xp, y )
    elseif (rtm_option == 2) then
      if (use_crtm_kmatrix) then
       call da_transform_xtoy_rad_crtmk ( xa, iv, xp, y )
      else if (use_crtm_kmatrix_fast) then
       call da_transform_xtoy_rad_crtmk_fast ( xa, iv, xp, y )
      else
       call da_transform_xtoy_rad_crtm ( xa, iv, xp, y )
      end if
    else
       write(6,'(a)') ' WARNING:  UNKNOWN Radiative Transfer Model in da_tranform_xtoy'
    endif

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy
