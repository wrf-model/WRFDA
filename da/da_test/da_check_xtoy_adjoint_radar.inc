subroutine da_check_xtoy_adjoint_radar(oi, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.

   if (trace_use) call da_trace_entry("da_jo_and_grady_radar")

   do n=1, oi%nlocal(radar)
      if (oi%radar(n)%loc%proc_domain) then
         do k=1, oi%radar(n)%info%levels
            adjtest_lhs = adjtest_lhs + &
               (y%radar(n)%rv(k)/typical_rv_rms)**2 + (y%radar(n)%rf(k)/typical_rf_rms)**2
         end do
      end if

      do k=1, oi%radar(n)%info%levels
         pertile_lhs = pertile_lhs + &
            (y%radar(n)%rv(k)/typical_rv_rms)**2 + (y%radar(n)%rf(k)/typical_rf_rms)**2

         y%radar(n)%rv(k)= y%radar(n)%rv(k) / typical_rv_rms ** 2
         y%radar(n)%rf(k)= y%radar(n)%rf(k) / typical_rf_rms ** 2
      end do
   end do

end subroutine da_check_xtoy_adjoint_radar


