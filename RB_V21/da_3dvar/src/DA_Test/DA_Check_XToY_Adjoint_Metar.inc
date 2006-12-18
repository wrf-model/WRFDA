SUBROUTINE DA_Check_XToY_Adjoint_Metar( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_metar < 1 ) return

   DO n=1, oi%num_metar
      if( oi%metar(n)%loc%proc_domain_with_halo ) then
         if(oi%metar(n)%zk < 1.0) then
            y%metar(n)%u = 0.0
            y%metar(n)%v = 0.0
            y%metar(n)%t = 0.0
            y%metar(n)%p = 0.0
            y%metar(n)%q = 0.0

            cycle
         endif

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
         endif

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
      else
         y%metar(n)%u = 0.0
         y%metar(n)%v = 0.0
         y%metar(n)%t = 0.0
         y%metar(n)%p = 0.0
         y%metar(n)%q = 0.0
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Metar

