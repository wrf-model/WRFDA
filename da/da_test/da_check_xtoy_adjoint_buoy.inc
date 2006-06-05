SUBROUTINE DA_Check_XToY_Adjoint_Buoy( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_buoy < 1 ) return

   DO n=1, oi%num_buoy
      if( oi%buoy(n)%loc%proc_domain_with_halo ) then
         if(oi%buoy(n)%zk < 1.0) then
            y%buoy(n)%u = 0.0
            y%buoy(n)%v = 0.0
            y%buoy(n)%t = 0.0
            y%buoy(n)%p = 0.0
            y%buoy(n)%q = 0.0

            cycle
         endif

         y%buoy(n)%u = y%buoy(n)%u/typical_u_rms
         y%buoy(n)%v = y%buoy(n)%v/typical_v_rms
         y%buoy(n)%t = y%buoy(n)%t/typical_t_rms
         y%buoy(n)%p = y%buoy(n)%p/typical_p_rms
         y%buoy(n)%q = y%buoy(n)%q/typical_q_rms

         if( oi%buoy(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs  &
                        + ( y%buoy(n)%u )**2 &
                        + ( y%buoy(n)%v )**2 &
                        + ( y%buoy(n)%t )**2 &
                        + ( y%buoy(n)%p )**2 &
                        + ( y%buoy(n)%q )**2
         endif

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
      else
         y%buoy(n)%u = 0.0
         y%buoy(n)%v = 0.0
         y%buoy(n)%t = 0.0
         y%buoy(n)%p = 0.0
         y%buoy(n)%q = 0.0
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Buoy

