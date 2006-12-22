subroutine da_check_xtoy_adjoint_qscat( iv, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   if ( iv%num_qscat > 0 ) then

   do n=1, iv%num_qscat
      if( iv%qscat(n)%loc%proc_domain ) then
         adjtest_lhs = adjtest_lhs + &
                     ( y%qscat(n)%u/typical_u_rms )**2 + &
                     ( y%qscat(n)%v/typical_v_rms )**2
      end if

      pertile_lhs = pertile_lhs + &
                  ( y%qscat(n)%u/typical_u_rms )**2 + &
                  ( y%qscat(n)%v/typical_v_rms )**2

      y%qscat (n)%u = y%qscat (n)%u/typical_u_rms ** 2
      y%qscat (n)%v = y%qscat (n)%v/typical_v_rms ** 2
   end do

   end if

end subroutine da_check_xtoy_adjoint_qscat


