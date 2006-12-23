subroutine da_check_xtoy_adjoint_airep( oi, y, adjtest_lhs, pertile_lhs)

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n, k          ! Loop counter.

   if (oi%num_Airep > 0) then
      do n=1, oi%num_Airep
         if (oi%airep(n)%loc%proc_domain) then
            do k=1, oi%Airep(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%Airep(n)%u(k) / typical_u_rms)**2 + &
                           ( y%Airep(n)%v(k) / typical_v_rms)**2 + &
                           ( y%Airep(n)%t(k) / typical_t_rms)**2 
            end do
         end if

         do k=1, oi%Airep(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%Airep(n)%u(k) / typical_u_rms)**2 + &
                        ( y%Airep(n)%v(k) / typical_v_rms)**2 + &
                        ( y%Airep(n)%t(k) / typical_t_rms)**2

           y%airep(n)%u(k) = y%airep(n)%u(k) / typical_u_rms ** 2
           y%airep(n)%v(k) = y%airep(n)%v(k) / typical_v_rms ** 2
           y%airep(n)%t(k) = y%airep(n)%t(k) / typical_t_rms ** 2
         end do
      end do
   end if

end subroutine da_check_xtoy_adjoint_airep


