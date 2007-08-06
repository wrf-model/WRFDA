subroutine da_residual_ssmi(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv     ! Obs increment vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(out)   :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   if (iv%num_ssmi_tb > 0) then
      call da_residual_ssmi_tb (iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
   else if (iv%num_ssmi_retrieval > 0) then
      call da_residual_ssmi_rv (iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
   end if

end subroutine da_residual_ssmi


