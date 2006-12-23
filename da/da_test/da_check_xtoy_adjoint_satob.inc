subroutine da_check_xtoy_adjoint_satob ( oi, y, adjtest_lhs, pertile_lhs)

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n             ! Loop counter.

   if (oi%num_satob > 0) then
      do n=1, oi%num_satob
         if (oi%satob(n)%loc%proc_domain) then
            adjtest_lhs = adjtest_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                          (y%satob(n)%v/typical_v_rms)**2
         end if

         pertile_lhs = pertile_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                       (y%satob(n)%v/typical_v_rms)**2

         y%satob(n)%u = y%satob(n)%u/typical_u_rms ** 2
         y%satob(n)%v = y%satob(n)%v/typical_v_rms ** 2
      end do
   end if

end subroutine da_check_xtoy_adjoint_satob


