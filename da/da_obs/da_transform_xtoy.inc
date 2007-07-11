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
  
   call da_transform_xtoy_sound (grid, iv, y) 
   call da_transform_xtoy_sonde_sfc (grid, iv, y) 

   !---------------------------------------------------------------------------
   ! [2.0] Surface observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_synop (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [3.0] Cloud track wind observation operator y = H(x):
   !---------------------------------------------------------------------------

   ! Geostationary Satellite
   call da_transform_xtoy_geoamv(grid, iv, y)
   ! Polar orbiting Satellite
   call da_transform_xtoy_polaramv(grid, iv, y)

   !---------------------------------------------------------------------------
   ! [4.0] Aircraft observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_airep (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [5.0] Metar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_metar (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [6.0] Ship observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ships (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [7.0] GPS TPW/ZTD observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpspw (grid, iv, y)


   !---------------------------------------------------------------------------
   ! [8.0] SSMI observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_ssmi (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [9.0] Pilot observation operator y = H(x):
   !---------------------------------------------------------------------------
  
   call da_transform_xtoy_pilot (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [10.0] Pseudo obs observation operator y = H(x):
   !----------------------------------------------------------------------------

   call da_transform_xtoy_pseudo (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [11.0] satem observation operator y = H(x):
   !---------------------------------------------------------------------------
   
   call da_transform_xtoy_satem (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [12.0] SSM/T1 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1 (grid, iv, y)
   
   !---------------------------------------------------------------------------
   ! [13.0] SSM/T2 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2 (grid, iv, y)
   
   !---------------------------------------------------------------------------
   ! [14.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_qscat (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [15.0] Profiler observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_profiler (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [16.0] Buoy observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_buoy  (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [17.0] GPS Refractivity observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpsref (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [18.0] radar observation operator y = H(x):
   !------------------------------------------------------------------------------

   call da_transform_xtoy_radar (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [19.0] TC bogus observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_bogus (grid, iv, y)

   !---------------------------------------------------------------------------
   ! [20.0] AIRS retrievals observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airsr (grid, iv, y) 

   !---------------------------------------------------------------------------
   ! [20.0] Radiance observation operator y = H(x):
   !---------------------------------------------------------------------------

   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
         call da_transform_xtoy_rttov (grid, iv, y)
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         !if (use_crtm_kmatrix) then
         !   call da_transform_xtoy_crtmk (grid, iv, y)
         !else if (use_crtm_kmatrix_fast) then
         !   call da_transform_xtoy_crtmk_f (grid, iv, y)
         !else
            call da_transform_xtoy_crtm (grid, iv, y)
         !end if
#endif
       else
          call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
       endif
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy


