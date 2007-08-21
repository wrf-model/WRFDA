subroutine da_check_xtoy_adjoint_gpsref(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_gpsref")

   do n=1, iv%nlocal(gpsref)
      if (iv%gpsref(n)%loc%proc_domain) then
         do k=1, iv%gpsref(n)%info%levels
            adjtest_lhs = adjtest_lhs + (y%gpsref(n)%ref(k) / typical_ref_rms)**2 
         end do
      end if

      do k=1, iv%gpsref(n)%info%levels
         pertile_lhs = pertile_lhs + (y%gpsref(n)%ref(k) / typical_ref_rms)**2
         y%gpsref(n)%ref(k) = y%gpsref(n)%ref(k) / typical_ref_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_gpsref")

end subroutine da_check_xtoy_adjoint_gpsref


