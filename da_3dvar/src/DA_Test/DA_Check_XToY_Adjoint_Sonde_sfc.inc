SUBROUTINE DA_Check_XToY_Adjoint_Sonde_Sfc( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_sound < 1 ) return

   DO n=1, oi%num_sound
      if( oi%sonde_sfc(n)%loc%proc_domain_with_halo ) then
         if(oi%sonde_sfc(n)%zk < 1.0) then
            y%sonde_sfc(n)%u = 0.0
            y%sonde_sfc(n)%v = 0.0
            y%sonde_sfc(n)%t = 0.0
            y%sonde_sfc(n)%p = 0.0
            y%sonde_sfc(n)%q = 0.0

            cycle
         endif

         y%sonde_sfc(n)%u = y%sonde_sfc(n)%u/typical_u_rms
         y%sonde_sfc(n)%v = y%sonde_sfc(n)%v/typical_v_rms
         y%sonde_sfc(n)%t = y%sonde_sfc(n)%t/typical_t_rms
         y%sonde_sfc(n)%p = y%sonde_sfc(n)%p/typical_p_rms
         y%sonde_sfc(n)%q = y%sonde_sfc(n)%q/typical_q_rms

         if( oi%sonde_sfc(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs  &
                        + ( y%sonde_sfc(n)%u )**2 &
                        + ( y%sonde_sfc(n)%v )**2 &
                        + ( y%sonde_sfc(n)%t )**2 &
                        + ( y%sonde_sfc(n)%p )**2 &
                        + ( y%sonde_sfc(n)%q )**2
         endif

         pertile_lhs = pertile_lhs &
                     + ( y%sonde_sfc(n)%u )**2 &
                     + ( y%sonde_sfc(n)%v )**2 &
                     + ( y%sonde_sfc(n)%t )**2 &
                     + ( y%sonde_sfc(n)%p )**2 &
                     + ( y%sonde_sfc(n)%q )**2

         y%sonde_sfc(n)%u = y%sonde_sfc(n)%u/typical_u_rms
         y%sonde_sfc(n)%v = y%sonde_sfc(n)%v/typical_v_rms
         y%sonde_sfc(n)%t = y%sonde_sfc(n)%t/typical_t_rms
         y%sonde_sfc(n)%p = y%sonde_sfc(n)%p/typical_p_rms
         y%sonde_sfc(n)%q = y%sonde_sfc(n)%q/typical_q_rms
      else
         y%sonde_sfc(n)%u = 0.0
         y%sonde_sfc(n)%v = 0.0
         y%sonde_sfc(n)%t = 0.0
         y%sonde_sfc(n)%p = 0.0
         y%sonde_sfc(n)%q = 0.0
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Sonde_Sfc

