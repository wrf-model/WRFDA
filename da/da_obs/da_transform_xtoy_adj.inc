subroutine da_transform_xtoy_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
   
   implicit none
   
   type (domain), intent(inout)      :: grid
   type (ob_type), intent(in)        :: iv          ! obs. inc vector (o-b).
   type (y_type), intent(inout)      :: jo_grad_y   ! grad_y(jo)
   type (x_type), intent(inout)      :: jo_grad_x   ! grad_x(jo)

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] Radiosonde observation operator y = H(x):
   !--------------------------------------------------------------------------

   call da_transform_xtoy_sound_adj (iv, jo_grad_y, jo_grad_x)
   call da_transform_xtoy_sonde_sfc_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [2.0] Surface observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_synop_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [3.0] Cloud track wind observation operator y = H(x):
   !---------------------------------------------------------------------------

   ! Geostationary Satellite
   call da_transform_xtoy_geoamv_adj (iv, jo_grad_y, jo_grad_x)

   ! Polar orbitting Satellite
   call da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [4.0] Aircraft observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airep_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [5.0] Metar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_metar_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [6.0] Ship observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ships_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [7.0] GPS TPW/ZTD observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpspw_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [8.0] SSMI observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmi_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [9.0] Pilot observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_pilot_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [10.0] Pseudo obs observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_pseudo_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [11.0] satem observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_satem_adj (iv, jo_grad_y, jo_grad_x, grid)

   !---------------------------------------------------------------------------
   ! [12.0] SSM/T1 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [13.0] SSM/T2 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [14.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_qscat_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [15.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_profiler_adj (iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [16.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_buoy_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [17.0] GPS Refractivity observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpsref_adj(iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [18.0] radar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_radar_adj (grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [19.0] TC bogus observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_bogus_adj (grid, iv, jo_grad_y, jo_grad_x)
!whl
   call da_transform_xtoy_mtgirs_adj (iv, jo_grad_y, jo_grad_x)
   !---------------------------------------------------------------------------
   ! [20.0] Radiance observation operator y = H(x):
   !---------------------------------------------------------------------------

   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
         call da_transform_xtoy_rttov_adj (iv, jo_grad_y, jo_grad_x)
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         !if (use_crtm_kmatrix) then
         !   call da_transform_xtoy_crtmk_adj (iv, jo_grad_y, jo_grad_x)
         !else if (use_crtm_kmatrix_fast) then
         !   call da_transform_xtoy_crtmk_f_adj (iv, jo_grad_y, jo_grad_x)
         !else
            call da_transform_xtoy_crtm_adj (iv, jo_grad_y, jo_grad_x)
         !end if
#endif
      else
         call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
      endif
   end if

   !---------------------------------------------------------------------------
   ! [21.0]  AIRS retrievals adjoint observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airsr_adj (iv, jo_grad_y, jo_grad_x)

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj


