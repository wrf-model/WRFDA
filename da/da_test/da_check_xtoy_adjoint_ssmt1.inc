subroutine da_check_xtoy_adjoint_ssmt1( iv, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   if ( iv%num_ssmt1 > 0 ) then
      do n=1, iv%num_ssmt1
         if( iv%ssmt1(n)%loc%proc_domain ) then
            do k=1, iv%ssmt1(n)%info%levels
               adjtest_lhs = adjtest_lhs + ( y%ssmt1(n)%t(k)/typical_t_rms )**2
            end do
         end if

         do k=1, iv%ssmt1(n)%info%levels
            pertile_lhs = pertile_lhs + ( y%ssmt1(n)%t(k)/typical_t_rms )**2

            y%ssmt1(n)%t(k) = y%ssmt1(n)%t(k) / typical_t_rms ** 2
         end do
      end do
   end if

end subroutine da_check_xtoy_adjoint_ssmt1

