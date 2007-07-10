subroutine da_transform_xtoy_ssmi (grid, oi, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: oi       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   if (oi%num_ssmi_tb > 0) then
      call da_transform_xtoy_ssmi_tb (grid, oi, y)
   else if (oi%num_ssmi_retrieval > 0) then
      call da_transform_xtoy_ssmi_rv (grid, oi, y)
   end if

end subroutine da_transform_xtoy_ssmi


