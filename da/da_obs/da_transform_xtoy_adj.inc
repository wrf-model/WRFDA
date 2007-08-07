subroutine da_transform_xtoy_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
   
   implicit none
   
   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type),  intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type),  intent(inout) :: jo_grad_x   ! grad_x(jo)

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv % num_sound > 0) then
      call da_transform_xtoy_sound_adj     (iv, jo_grad_y, jo_grad_x) 
      call da_transform_xtoy_sonde_sfc_adj (grid, iv, jo_grad_y, jo_grad_x) 
   end if

   if (iv % num_synop          > 0) call da_transform_xtoy_synop_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_geoamv         > 0) call da_transform_xtoy_geoamv_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv % num_polaramv       > 0) call da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)   
   if (iv % num_airep          > 0) call da_transform_xtoy_airep_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv % num_metar          > 0) call da_transform_xtoy_metar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_ships          > 0) call da_transform_xtoy_ships_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_gpspw          > 0) call da_transform_xtoy_gpspw_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_gpsref         > 0) call da_transform_xtoy_gpsref_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv % num_ssmi_tb        > 0) call da_transform_xtoy_ssmi_tb_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_ssmi_retrieval > 0) call da_transform_xtoy_ssmi_rv_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_pilot          > 0) call da_transform_xtoy_pilot_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv % num_satem          > 0) call da_transform_xtoy_satem_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_ssmt1          > 0) call da_transform_xtoy_ssmt1_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv % num_ssmt2          > 0) call da_transform_xtoy_ssmt2_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv % num_qscat          > 0) call da_transform_xtoy_qscat_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv % num_profiler       > 0) call da_transform_xtoy_profiler_adj (iv, jo_grad_y, jo_grad_x)
   if (iv % num_buoy           > 0) call da_transform_xtoy_buoy_adj     (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_gpsref         > 0) call da_transform_xtoy_gpsref_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv % num_radar          > 0) call da_transform_xtoy_radar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_bogus          > 0) call da_transform_xtoy_bogus_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv % num_airsr          > 0) call da_transform_xtoy_airsr_adj    (iv, jo_grad_y, jo_grad_x)

   if (num_pseudo              > 0) call da_transform_xtoy_pseudo_adj   (iv, jo_grad_y, jo_grad_x)

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
      end if
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj


