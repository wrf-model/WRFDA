subroutine da_check_xtoy_adjoint_metar( oi, y, adjtest_lhs, pertile_lhs )

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! Loop counter.

   if ( oi%num_metar < 1 ) return

   do n=1, oi%num_metar
      if(oi%metar(n)%zk < 1.0) then
         y%metar(n)%u = 0.0
         y%metar(n)%v = 0.0
         y%metar(n)%t = 0.0
         y%metar(n)%p = 0.0
         y%metar(n)%q = 0.0

         cycle
      end if

      y%metar(n)%u = y%metar(n)%u/typical_u_rms
      y%metar(n)%v = y%metar(n)%v/typical_v_rms
      y%metar(n)%t = y%metar(n)%t/typical_t_rms
      y%metar(n)%p = y%metar(n)%p/typical_p_rms
      y%metar(n)%q = y%metar(n)%q/typical_q_rms

      if( oi%metar(n)%loc%proc_domain ) then
         adjtest_lhs = adjtest_lhs  &
                     + ( y%metar(n)%u )**2 &
                     + ( y%metar(n)%v )**2 &
                     + ( y%metar(n)%t )**2 &
                     + ( y%metar(n)%p )**2 &
                     + ( y%metar(n)%q )**2
      end if

      pertile_lhs = pertile_lhs &
                  + ( y%metar(n)%u )**2 &
                  + ( y%metar(n)%v )**2 &
                  + ( y%metar(n)%t )**2 &
                  + ( y%metar(n)%p )**2 &
                  + ( y%metar(n)%q )**2

      y%metar(n)%u = y%metar(n)%u/typical_u_rms
      y%metar(n)%v = y%metar(n)%v/typical_v_rms
      y%metar(n)%t = y%metar(n)%t/typical_t_rms
      y%metar(n)%p = y%metar(n)%p/typical_p_rms
      y%metar(n)%q = y%metar(n)%q/typical_q_rms
   end do

end subroutine da_check_xtoy_adjoint_metar


