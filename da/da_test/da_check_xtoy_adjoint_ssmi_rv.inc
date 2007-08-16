subroutine da_check_xtoy_adjoint_ssmi_rv(oi, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! loop counter.
   real    :: var

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint_ssmi_rv")

   if (oi%nlocal(ssmi_rv) > 0) then
      do n=1, oi%nlocal(ssmi_rv)
         y%ssmi_rv(n)%speed = y%ssmi_rv(n)%speed/typical_speed_rms
         y%ssmi_rv(n)%tpw   = y%ssmi_rv(n)%tpw/typical_tpw_rms

         var = (y%ssmi_rv(n)%speed) ** 2 + (y%ssmi_rv(n)%tpw) ** 2

         pertile_lhs = pertile_lhs + var

         if (oi%ssmi_rv(n)%loc%proc_domain) then
            adjtest_lhs = adjtest_lhs + var
         end if

         y%ssmi_rv(n)%speed = y%ssmi_rv(n)%speed/typical_speed_rms
         y%ssmi_rv(n)%tpw   = y%ssmi_rv(n)%tpw/typical_tpw_rms
      end do
   end if

   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint_ssmi_rv")

end subroutine da_check_xtoy_adjoint_ssmi_rv


