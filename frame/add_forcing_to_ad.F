SUBROUTINE add_forcing_to_ad(grid)
   USE module_domain
   IMPLICIT NONE
   TYPE(domain), INTENT(INOUT)  :: grid
integer ims,ime,jms,jme,i,j
#ifdef WRFVAR
   grid%em_a_u_1  = grid%em_a_u_1  + grid%em_g_u_1

   grid%em_a_u_2  = grid%em_a_u_2  + grid%em_g_u_2
   grid%em_a_v_1  = grid%em_a_v_1  + grid%em_g_v_1
   grid%em_a_v_2  = grid%em_a_v_2  + grid%em_g_v_2
   grid%em_a_w_1  = grid%em_a_w_1  + grid%em_g_w_1
   grid%em_a_w_2  = grid%em_a_w_2  + grid%em_g_w_2
   grid%em_a_ph_1 = grid%em_a_ph_1 + grid%em_g_ph_1
   grid%em_a_ph_2 = grid%em_a_ph_2 + grid%em_g_ph_2
   grid%em_a_t_1  = grid%em_a_t_1  + grid%em_g_t_1
   grid%em_a_t_2  = grid%em_a_t_2  + grid%em_g_t_2
   grid%em_a_mu_1 = grid%em_a_mu_1 + grid%em_g_mu_1
   grid%em_a_mu_2 = grid%em_a_mu_2 + grid%em_g_mu_2
   grid%a_moist_1 = grid%a_moist_1 + grid%g_moist_1
   grid%a_moist_2 = grid%a_moist_2 + grid%g_moist_2
#endif
END SUBROUTINE add_forcing_to_ad
