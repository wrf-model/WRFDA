subroutine da_check_xtoy_adjoint_bogus( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n, k          ! Loop counter.

   if ( oi%num_Bogus > 0 ) then

      do n=1, oi%num_Bogus
         if( oi%bogus(n)%loc%proc_domain ) then
            do k=1, oi%bogus(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                             ( y%Bogus(n)%u(k)/typical_u_rms )**2 + &
                             ( y%Bogus(n)%v(k)/typical_v_rms )**2 + &
                             ( y%Bogus(n)%t(k)/typical_t_rms )**2 + &
                             ( y%Bogus(n)%q(k)/typical_q_rms )**2 
            end do
            adjtest_lhs = adjtest_lhs + &
                          ( y%Bogus(n)%slp/typical_p_rms )**2
         end if

         do k=1, oi%bogus(n)%info%levels
            pertile_lhs = pertile_lhs + &
                          ( y%bogus(n)%u(k)/typical_u_rms )**2 + &
                          ( y%bogus(n)%v(k)/typical_v_rms )**2 + &
                          ( y%bogus(n)%t(k)/typical_t_rms )**2 + &
                          ( y%bogus(n)%q(k)/typical_q_rms )**2

            y%bogus(n)%u(k) = y%bogus(n)%u(k) / typical_u_rms ** 2
            y%bogus(n)%v(k) = y%bogus(n)%v(k) / typical_v_rms ** 2
            y%bogus(n)%t(k) = y%bogus(n)%t(k) / typical_t_rms ** 2
            y%bogus(n)%q(k) = y%bogus(n)%q(k) / typical_q_rms ** 2
         end do
         pertile_lhs = pertile_lhs + &
                       ( y%bogus(n)%slp/typical_p_rms )**2
         y%bogus(n)%slp = y%bogus(n)%slp / typical_p_rms ** 2
      end do
   end if

end subroutine da_check_xtoy_adjoint_bogus


