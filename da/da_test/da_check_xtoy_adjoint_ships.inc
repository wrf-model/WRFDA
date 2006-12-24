subroutine da_check_xtoy_adjoint_ships(oi, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)     :: oi            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: n             ! Loop counter.

   if (oi%num_ships < 1) return

   do n=1, oi%num_ships
      if (oi%ships(n)%zk < 1.0) then
         y%ships(n)%u = 0.0
         y%ships(n)%v = 0.0
         y%ships(n)%t = 0.0
         y%ships(n)%p = 0.0
         y%ships(n)%q = 0.0

         cycle
      end if

      y%ships(n)%u = y%ships(n)%u/typical_u_rms
      y%ships(n)%v = y%ships(n)%v/typical_v_rms
      y%ships(n)%t = y%ships(n)%t/typical_t_rms
      y%ships(n)%p = y%ships(n)%p/typical_p_rms
      y%ships(n)%q = y%ships(n)%q/typical_q_rms

      if (oi%ships(n)%loc%proc_domain) then
         adjtest_lhs = adjtest_lhs  &
                     + (y%ships(n)%u)**2 &
                     + (y%ships(n)%v)**2 &
                     + (y%ships(n)%t)**2 &
                     + (y%ships(n)%p)**2 &
                     + (y%ships(n)%q)**2
      end if

      pertile_lhs = pertile_lhs &
                  + (y%ships(n)%u)**2 &
                  + (y%ships(n)%v)**2 &
                  + (y%ships(n)%t)**2 &
                  + (y%ships(n)%p)**2 &
                  + (y%ships(n)%q)**2

      y%ships(n)%u = y%ships(n)%u/typical_u_rms
      y%ships(n)%v = y%ships(n)%v/typical_v_rms
      y%ships(n)%t = y%ships(n)%t/typical_t_rms
      y%ships(n)%p = y%ships(n)%p/typical_p_rms
      y%ships(n)%q = y%ships(n)%q/typical_q_rms
   end do

end subroutine da_check_xtoy_adjoint_ships


