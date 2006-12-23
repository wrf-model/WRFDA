subroutine da_check_xtoy_adjoint_ssmi_rv( oi, y, adjtest_lhs, pertile_lhs)

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! loop counter.

   real :: var

   if (oi%num_ssmi_retrieval > 0) then
      do n=1, oi%num_ssmi_retrieval
         y%ssmi_retrieval(n)%speed = y%ssmi_retrieval(n)%speed/typical_speed_rms
         y%ssmi_retrieval(n)%tpw   = y%ssmi_retrieval(n)%tpw/typical_tpw_rms

         var = (y%ssmi_retrieval(n)%speed) ** 2 &
             + (y%ssmi_retrieval(n)%tpw) ** 2

         pertile_lhs = pertile_lhs + var

         if (oi%ssmi_retrieval(n)%loc%proc_domain) then
            adjtest_lhs = adjtest_lhs + var
         end if

         y%ssmi_retrieval(n)%speed = y%ssmi_retrieval(n)%speed/typical_speed_rms
         y%ssmi_retrieval(n)%tpw   = y%ssmi_retrieval(n)%tpw/typical_tpw_rms
      end do
   end if

end subroutine da_check_xtoy_adjoint_ssmi_rv


