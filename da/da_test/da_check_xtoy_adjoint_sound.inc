subroutine da_check_xtoy_adjoint_sound( oi, y, adjtest_lhs, pertile_lhs)

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n, k          ! Loop counter.

   if (oi%num_Sound < 1) return

   do n=1, oi%num_Sound
      if (oi%sound(n)%loc%proc_domain) then
         do k=1, oi%sound(n)%info%levels
            adjtest_lhs = adjtest_lhs + &
                          ( y%Sound(n)%u(k)/typical_u_rms)**2 + &
                          ( y%Sound(n)%v(k)/typical_v_rms)**2 + &
                          ( y%Sound(n)%t(k)/typical_t_rms)**2 + &
                          ( y%Sound(n)%q(k)/typical_q_rms)**2
         end do
      end if

      do k=1, oi%sound(n)%info%levels
         pertile_lhs = pertile_lhs + &
                       ( y%Sound(n)%u(k)/typical_u_rms)**2 + &
                       ( y%Sound(n)%v(k)/typical_v_rms)**2 + &
                       ( y%Sound(n)%t(k)/typical_t_rms)**2 + &
                       ( y%Sound(n)%q(k)/typical_q_rms)**2

         y%sound(n)%u(k) = y%sound(n)%u(k) / typical_u_rms ** 2
         y%sound(n)%v(k) = y%sound(n)%v(k) / typical_v_rms ** 2
         y%sound(n)%t(k) = y%sound(n)%t(k) / typical_t_rms ** 2
         y%sound(n)%q(k) = y%sound(n)%q(k) / typical_q_rms ** 2
      end do
   end do

end subroutine da_check_xtoy_adjoint_sound


