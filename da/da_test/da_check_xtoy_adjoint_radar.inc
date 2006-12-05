subroutine da_check_xtoy_adjoint_radar( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n, k          ! Loop counter.

   if ( oi%num_Radar > 0 ) then
      do n=1, oi%num_Radar
         if( oi%Radar(n)%loc%proc_domain ) then
            do k=1, oi%Radar(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                             ( y%Radar(n)%rv(k)/typical_rv_rms )**2 + &
                             ( y%Radar(n)%rf(k)/typical_rf_rms )**2
            end do
         end if

         do k=1, oi%Radar(n)%info%levels
            pertile_lhs = pertile_lhs + &
                         ( y%Radar(n)%rv(k)/typical_rv_rms )**2 + &
                         ( y%Radar(n)%rf(k)/typical_rf_rms )**2

            y%Radar(n)%rv(k)= y%Radar(n)%rv(k) / typical_rv_rms ** 2
            y%Radar(n)%rf(k)= y%Radar(n)%rf(k) / typical_rf_rms ** 2
         end do
      end do
   end if

end subroutine da_check_xtoy_adjoint_radar
