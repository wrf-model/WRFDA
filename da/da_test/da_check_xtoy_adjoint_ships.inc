SUBROUTINE DA_Check_XToY_Adjoint_Ships( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_ships < 1 ) return

   DO n=1, oi%num_ships
      if( oi%ships(n)%loc%proc_domain_with_halo ) then
         if(oi%ships(n)%zk < 1.0) then
            y%ships(n)%u = 0.0
            y%ships(n)%v = 0.0
            y%ships(n)%t = 0.0
            y%ships(n)%p = 0.0
            y%ships(n)%q = 0.0

            cycle
         endif

         y%ships(n)%u = y%ships(n)%u/typical_u_rms
         y%ships(n)%v = y%ships(n)%v/typical_v_rms
         y%ships(n)%t = y%ships(n)%t/typical_t_rms
         y%ships(n)%p = y%ships(n)%p/typical_p_rms
         y%ships(n)%q = y%ships(n)%q/typical_q_rms

         if( oi%ships(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs  &
                        + ( y%ships(n)%u )**2 &
                        + ( y%ships(n)%v )**2 &
                        + ( y%ships(n)%t )**2 &
                        + ( y%ships(n)%p )**2 &
                        + ( y%ships(n)%q )**2
         endif

         pertile_lhs = pertile_lhs &
                     + ( y%ships(n)%u )**2 &
                     + ( y%ships(n)%v )**2 &
                     + ( y%ships(n)%t )**2 &
                     + ( y%ships(n)%p )**2 &
                     + ( y%ships(n)%q )**2

         y%ships(n)%u = y%ships(n)%u/typical_u_rms
         y%ships(n)%v = y%ships(n)%v/typical_v_rms
         y%ships(n)%t = y%ships(n)%t/typical_t_rms
         y%ships(n)%p = y%ships(n)%p/typical_p_rms
         y%ships(n)%q = y%ships(n)%q/typical_q_rms
      else
         y%ships(n)%u = 0.0
         y%ships(n)%v = 0.0
         y%ships(n)%t = 0.0
         y%ships(n)%p = 0.0
         y%ships(n)%q = 0.0
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Ships

