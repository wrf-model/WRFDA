subroutine da_transform_xtoy_adj( xb, iv, xp, jo_grad_y, jo_grad_x )
   
   implicit none
   
   type (xb_type), intent(in)        :: xb          ! first guess state.
   type (ob_type), intent(in)        :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in)     :: xp          ! Domain decomposition vars.
   type (y_type), intent(inout)      :: jo_grad_y   ! grad_y(jo)
   type (x_type), intent(inout)      :: jo_grad_x   ! grad_x(jo)

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] Radiosonde observation operator y = H(x):
   !--------------------------------------------------------------------------

   call da_transform_xtoy_sound_adj ( iv, xp, jo_grad_y, jo_grad_x )
   call da_transform_xtoy_sonde_sfc_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [2.0] Surface observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_synop_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [3.0] Cloud track wind observation operator y = H(x):
   !---------------------------------------------------------------------------

   ! Geostationary Satellite
   call da_transform_xtoy_geoamv_adj ( iv, xp, jo_grad_y, jo_grad_x )

   ! Polar orbitting Satellite
   call da_transform_xtoy_polaramv_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [4.0] Aircraft observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airep_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [5.0] Metar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_metar_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [6.0] Ship observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ships_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [7.0] GPS TPW/ZTD observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpspw_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [8.0] SSMI observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmi_adj ( xb, iv, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [9.0] Pilot observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_pilot_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [10.0] Pseudo obs observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_pseudo_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [11.0] Satem observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_satem_adj ( iv, xp, jo_grad_y, jo_grad_x, xb )

   !---------------------------------------------------------------------------
   ! [12.0] SSM/T1 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt1_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [13.0] SSM/T2 observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_ssmt2_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [14.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_qscat_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [15.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_profiler_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [16.0] Scatterometer observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_buoy_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [17.0] GPS Refractivity observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_gpsref_adj( iv, xp, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [18.0] Radar observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_radar_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [19.0] TC bogus observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_bogus_adj ( xb, iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! [20.0] Radiance observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_rad_adj (iv, xp, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------------
   ! [21.0]  AIRS retrievals adjoint observation operator y = H(x):
   !---------------------------------------------------------------------------

   call da_transform_xtoy_airsr_adj ( iv, xp, jo_grad_y, jo_grad_x )

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj
