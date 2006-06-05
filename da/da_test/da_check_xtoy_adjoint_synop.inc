SUBROUTINE DA_Check_XToY_Adjoint_Synop( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_synop < 1 ) return

   DO n=1, oi%num_synop
      if( oi%synop(n)%loc%proc_domain_with_halo ) then
         if(oi%synop(n)%zk < 1.0) then
            y%synop(n)%u = 0.0
            y%synop(n)%v = 0.0
            y%synop(n)%t = 0.0
            y%synop(n)%p = 0.0
            y%synop(n)%q = 0.0

            cycle
         endif

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
         endif

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
      else
         y%synop(n)%u = 0.0
         y%synop(n)%v = 0.0
         y%synop(n)%t = 0.0
         y%synop(n)%p = 0.0
         y%synop(n)%q = 0.0
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Synop

