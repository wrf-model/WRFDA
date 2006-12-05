subroutine da_check_xtoy_adjoint_ssmi( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   if ( oi%num_ssmi_tb > 0 ) then

      call da_check_xtoy_adjoint_ssmi_tb ( oi, y, adjtest_lhs, pertile_lhs )

   else if ( oi%num_ssmi_retrieval > 0) then

      call da_check_xtoy_adjoint_ssmi_rv( oi, y, adjtest_lhs, pertile_lhs )

   end if

end subroutine da_check_xtoy_adjoint_ssmi

