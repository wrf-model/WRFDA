subroutine da_get_innov_vector_ssmi ( it, xb, ob, iv )

   implicit none

   integer, intent(in)           :: it         ! External iteration.
   type (xb_type), intent(in)    :: xb         ! first guess state.
   type (y_type),  intent(in)    :: ob         ! Observation structure.
   type (ob_type), intent(inout) :: iv         ! O-B structure.

   if ( ob%num_ssmi_tb > 0 ) then

      call da_get_innov_vector_ssmi_tb ( it, xb, ob, iv )

   else if ( ob%num_ssmi_retrieval > 0) then

      call da_get_innov_vector_ssmi_rv ( it, xb, ob, iv )

   end if

end subroutine da_get_innov_vector_ssmi

