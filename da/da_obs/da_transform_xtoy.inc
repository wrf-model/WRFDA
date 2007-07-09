subroutine da_transform_xtoy(grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout)  :: grid
   type (ob_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type), intent(inout)  :: y        ! y = h (grid%xa)

   if (trace_use) call da_trace_entry("da_transform_xtoy")
   
   !--------------------------------------------------------------------------
   ! [1.0] Radiosonde observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   call da_transform_xtoy_sound (grid%xa, iv, grid%xp, y) 
   call da_transform_xtoy_sonde_sfc (grid%xa, grid%xb, iv, grid%xp, y) 

   !---------------------------------------------------------------------------
   ! [2.0] Surface observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_synop (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [3.0] Cloud track wind observation operator y = H(x):
   !---------------------------------------------------------------------------

   ! Geostationary Satellite
   call da_transform_xtoy_geoamv(grid%xa, iv, grid%xp, y)
   ! Polar orbiting Satellite
   call da_transform_xtoy_polaramv(grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [4.0] Aircraft observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_airep (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [5.0] Metar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_metar (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [6.0] Ship observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ships (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [7.0] GPS TPW/ZTD observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpspw (grid%xb, iv, grid%xa, grid%xp, y)


   !---------------------------------------------------------------------------
   ! [8.0] SSMI observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_ssmi (grid%xb, iv, grid%xa, y)

   !---------------------------------------------------------------------------
   ! [9.0] Pilot observation operator y = H(x):
   !---------------------------------------------------------------------------
  
   call da_transform_xtoy_pilot (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [10.0] Pseudo obs observation operator y = H(x):
   !----------------------------------------------------------------------------

   call da_transform_xtoy_pseudo (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [11.0] Satem observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_satem (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [12.0] SSM/T1 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1 (grid%xa, iv, grid%xp, y)
   
   !---------------------------------------------------------------------------
   ! [13.0] SSM/T2 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2 (grid%xa, iv, grid%xp, y)
   
   !---------------------------------------------------------------------------
   ! [14.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_qscat (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [15.0] Profiler observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_profiler (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [16.0] Buoy observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_buoy  (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [17.0] GPS Refractivity observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpsref (grid%xa, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [18.0] Radar observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_radar (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [19.0] TC bogus observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_bogus (grid%xa, grid%xb, iv, grid%xp, y)

   !---------------------------------------------------------------------------
   ! [20.0] AIRS retrievals observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airsr (grid%xa, iv, grid%xp, y) 

   !---------------------------------------------------------------------------
   ! [20.0] Radiance observation operator y = H(x):
   !---------------------------------------------------------------------------

   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
         call da_transform_xtoy_rttov (grid%xa, iv, grid%xp, y)
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         !if (use_crtm_kmatrix) then
         !   call da_transform_xtoy_crtmk (grid%xa, iv, grid%xp, y)
         !else if (use_crtm_kmatrix_fast) then
         !   call da_transform_xtoy_crtmk_f (grid%xa, iv, grid%xp, y)
         !else
            call da_transform_xtoy_crtm (grid%xa, iv, grid%xp, y)
         !end if
#endif
       else
          call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
       endif
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy


