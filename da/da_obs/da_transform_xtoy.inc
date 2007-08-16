subroutine da_transform_xtoy(grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none
   
   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   if (trace_use) call da_trace_entry("da_transform_xtoy")
   
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv % nlocal(sound) > 0) then
      call da_transform_xtoy_sound (grid, iv, y) 
      call da_transform_xtoy_sonde_sfc (grid, iv, y) 
   end if

   if (iv % nlocal(synop)          > 0) call da_transform_xtoy_synop    (grid, iv, y)
   if (iv % nlocal(geoamv)         > 0) call da_transform_xtoy_geoamv   (grid, iv, y)
   if (iv % nlocal(polaramv)       > 0) call da_transform_xtoy_polaramv (grid, iv, y)
   if (iv % nlocal(airep)          > 0) call da_transform_xtoy_airep    (grid, iv, y)
   if (iv % nlocal(metar)          > 0) call da_transform_xtoy_metar    (grid, iv, y)
   if (iv % nlocal(ships)          > 0) call da_transform_xtoy_ships    (grid, iv, y)
   if (iv % nlocal(gpspw)          > 0) call da_transform_xtoy_gpspw    (grid, iv, y)
   if (iv % nlocal(gpsref)         > 0) call da_transform_xtoy_gpsref   (grid, iv, y)
   if (iv % nlocal(ssmi_tb)        > 0) call da_transform_xtoy_ssmi_tb  (grid, iv, y)
   if (iv % nlocal(ssmi_rv) > 0) call da_transform_xtoy_ssmi_rv  (grid, iv, y)
   if (iv % nlocal(pilot)          > 0) call da_transform_xtoy_pilot    (grid, iv, y)
   if (iv % nlocal(satem)          > 0) call da_transform_xtoy_satem    (grid, iv, y)
   if (iv % nlocal(ssmt1)          > 0) call da_transform_xtoy_ssmt1    (grid, iv, y)
   if (iv % nlocal(ssmt2)          > 0) call da_transform_xtoy_ssmt2    (grid, iv, y)
   if (iv % nlocal(qscat)          > 0) call da_transform_xtoy_qscat    (grid, iv, y)
   if (iv % nlocal(profiler)       > 0) call da_transform_xtoy_profiler (grid, iv, y)
   if (iv % nlocal(buoy)           > 0) call da_transform_xtoy_buoy     (grid, iv, y)
   if (iv % nlocal(gpsref)         > 0) call da_transform_xtoy_gpsref   (grid, iv, y)
   if (iv % nlocal(radar)          > 0) call da_transform_xtoy_radar    (grid, iv, y)
   if (iv % nlocal(bogus)          > 0) call da_transform_xtoy_bogus    (grid, iv, y)
   if (iv % nlocal(airsr)          > 0) call da_transform_xtoy_airsr    (grid, iv, y)

   if (num_pseudo              > 0) call da_transform_xtoy_pseudo   (grid, iv, y)

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
       end if
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy


