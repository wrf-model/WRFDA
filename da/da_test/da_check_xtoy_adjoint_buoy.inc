subroutine da_check_xtoy_adjoint_buoy( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! Loop counter.

   if (oi%num_buoy < 1) return

   do n=1, oi%num_buoy
      if (oi%buoy(n)%zk < 1.0) then
         y%buoy(n)%u = 0.0
         y%buoy(n)%v = 0.0
         y%buoy(n)%t = 0.0
         y%buoy(n)%p = 0.0
         y%buoy(n)%q = 0.0

         cycle
      end if

      y%buoy(n)%u = y%buoy(n)%u/typical_u_rms
      y%buoy(n)%v = y%buoy(n)%v/typical_v_rms
      y%buoy(n)%t = y%buoy(n)%t/typical_t_rms
      y%buoy(n)%p = y%buoy(n)%p/typical_p_rms
      y%buoy(n)%q = y%buoy(n)%q/typical_q_rms

      if (oi%buoy(n)%loc%proc_domain) then
         adjtest_lhs = adjtest_lhs  &
                     + ( y%buoy(n)%u )**2 &
                     + ( y%buoy(n)%v )**2 &
                     + ( y%buoy(n)%t )**2 &
                     + ( y%buoy(n)%p )**2 &
                     + ( y%buoy(n)%q )**2
      end if

      pertile_lhs = pertile_lhs &
                  + ( y%buoy(n)%u )**2 &
                  + ( y%buoy(n)%v )**2 &
                  + ( y%buoy(n)%t )**2 &
                  + ( y%buoy(n)%p )**2 &
                  + ( y%buoy(n)%q )**2

      y%buoy(n)%u = y%buoy(n)%u/typical_u_rms
      y%buoy(n)%v = y%buoy(n)%v/typical_v_rms
      y%buoy(n)%t = y%buoy(n)%t/typical_t_rms
      y%buoy(n)%p = y%buoy(n)%p/typical_p_rms
      y%buoy(n)%q = y%buoy(n)%q/typical_q_rms
   end do

end subroutine da_check_xtoy_adjoint_buoy


