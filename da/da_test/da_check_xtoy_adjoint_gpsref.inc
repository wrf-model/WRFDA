subroutine da_check_xtoy_adjoint_gpsref( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n, k          ! Loop counter.

   if ( oi%num_Gpsref > 0 ) then
      do n=1, oi%num_Gpsref
         if( oi%gpsref(n)%loc%proc_domain ) then
            do k=1, oi%Gpsref(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%Gpsref(n)%ref(k) / typical_ref_rms )**2 
            end do
         end if

         do k=1, oi%Gpsref(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%Gpsref(n)%ref(k) / typical_ref_rms )**2

           y%gpsref(n)%ref(k) = y%gpsref(n)%ref(k) / typical_ref_rms ** 2
         end do
      end do
   end if

end subroutine da_check_xtoy_adjoint_gpsref

