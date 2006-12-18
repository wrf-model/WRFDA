SUBROUTINE da_check_xtoy_adjoint_pseudo( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_pseudo > 0 ) THEN
   DO n=1, oi%num_pseudo
      if( oi%synop(n)%loc%proc_domain_with_halo ) then
         if( oi%synop(n)%loc%proc_domain_with_halo ) then
            adjtest_lhs = adjtest_lhs + (y%pseudo(n)%u/typical_u_rms)**2 + &
                          (y%pseudo(n)%v/typical_v_rms)**2 + &
                          (y%pseudo(n)%v/typical_t_rms)**2 + &
                          (y%pseudo(n)%v/typical_p_rms)**2 + &
                          (y%pseudo(n)%v/typical_q_rms)**2
         endif

         pertile_lhs = pertile_lhs + (y%pseudo(n)%u/typical_u_rms)**2 + &
                       (y%pseudo(n)%v/typical_v_rms)**2 + &
                       (y%pseudo(n)%v/typical_t_rms)**2 + &
                       (y%pseudo(n)%v/typical_p_rms)**2 + &
                       (y%pseudo(n)%v/typical_q_rms)**2

         y%pseudo(n)%u = y%pseudo(n)%u/typical_u_rms ** 2
         y%pseudo(n)%v = y%pseudo(n)%v/typical_v_rms ** 2
         y%pseudo(n)%t = y%pseudo(n)%p/typical_t_rms ** 2
         y%pseudo(n)%p = y%pseudo(n)%v/typical_p_rms ** 2
         y%pseudo(n)%q = y%pseudo(n)%p/typical_q_rms ** 2
      endif
   ENDDO
   ENDIF

END SUBROUTINE da_check_xtoy_adjoint_pseudo
