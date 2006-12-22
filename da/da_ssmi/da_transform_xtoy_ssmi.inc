subroutine da_transform_xtoy_ssmi ( xb, oi, xa, y )

   implicit none

   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (ob_type), intent(in)   :: oi       ! obs. increment vector (o-b).
   type (x_type),  intent(inout):: xa       ! gridded analysis increment.
   type (y_type), intent(inout) :: y        ! y = h (xa)

   if ( oi%num_ssmi_tb > 0 ) then
      call da_transform_xtoy_ssmi_tb ( xb, oi, xa, y )
   else if ( oi%num_ssmi_retrieval > 0) then
      call da_transform_xtoy_ssmi_rv ( xb, oi, xa, y )
   end if

end subroutine da_transform_xtoy_ssmi


