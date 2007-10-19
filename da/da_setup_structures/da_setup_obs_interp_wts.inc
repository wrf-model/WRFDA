subroutine da_setup_obs_interp_wts (iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)  :: iv         ! Innovation vector (O-B).

   integer                        :: i, n          ! Loop counter.

   if (trace_use) call da_trace_entry("da_setup_obs_interp_wts")

   do i=1,num_ob_indexes
      call da_store_obs_grid_info_new (iv%info(i))
   end do

   do n=1, iv%info(satem)%nlocal
      call da_store_obs_grid_info (iv%satem(n)%loc)
   end do

   do i = 1, iv % num_inst
      if (iv % instid(i) % num_rad < 1) cycle
      call da_store_obs_grid_info_new (iv%instid(i)%info)
   end do

   if (trace_use) call da_trace_exit("da_setup_obs_interp_wts")

end subroutine da_setup_obs_interp_wts


