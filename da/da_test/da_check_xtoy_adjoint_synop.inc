subroutine da_check_xtoy_adjoint_synop( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   if ( oi%num_synop < 1 ) return

   do n=1, oi%num_synop
      if(oi%synop(n)%zk < 1.0) then
         y%synop(n)%u = 0.0
         y%synop(n)%v = 0.0
         y%synop(n)%t = 0.0
         y%synop(n)%p = 0.0
         y%synop(n)%q = 0.0

         cycle
      end if

      y%synop(n)%u = y%synop(n)%u/typical_u_rms
      y%synop(n)%v = y%synop(n)%v/typical_v_rms
      y%synop(n)%t = y%synop(n)%t/typical_t_rms
      y%synop(n)%p = y%synop(n)%p/typical_p_rms
      y%synop(n)%q = y%synop(n)%q/typical_q_rms

      if( oi%synop(n)%loc%proc_domain ) then
         adjtest_lhs = adjtest_lhs  &
                     + ( y%synop(n)%u )**2 &
                     + ( y%synop(n)%v )**2 &
                     + ( y%synop(n)%t )**2 &
                     + ( y%synop(n)%p )**2 &
                     + ( y%synop(n)%q )**2
      end if

      pertile_lhs = pertile_lhs &
                  + ( y%synop(n)%u )**2 &
                  + ( y%synop(n)%v )**2 &
                  + ( y%synop(n)%t )**2 &
                  + ( y%synop(n)%p )**2 &
                  + ( y%synop(n)%q )**2

      y%synop(n)%u = y%synop(n)%u/typical_u_rms
      y%synop(n)%v = y%synop(n)%v/typical_v_rms
      y%synop(n)%t = y%synop(n)%t/typical_t_rms
      y%synop(n)%p = y%synop(n)%p/typical_p_rms
      y%synop(n)%q = y%synop(n)%q/typical_q_rms

   end do

end subroutine da_check_xtoy_adjoint_synop

