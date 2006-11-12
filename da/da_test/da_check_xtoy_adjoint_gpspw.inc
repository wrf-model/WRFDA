subroutine da_check_xtoy_adjoint_gpspw( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n             ! Loop counter.

   if ( oi%num_Gpspw > 0 ) then
      do n=1, oi%num_Gpspw
         if( oi%gpspw(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs &
                       + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2
         end if

         pertile_lhs = pertile_lhs &
                    + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2

         y%gpspw (n)%tpw = y%gpspw (n)%tpw/typical_tpw_rms ** 2
      end do
   end if

end subroutine da_check_xtoy_adjoint_gpspw

