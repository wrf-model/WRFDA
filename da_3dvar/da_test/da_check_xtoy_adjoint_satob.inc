SUBROUTINE DA_Check_XToY_Adjoint_Satob ( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_satob > 0 ) THEN
   DO n=1, oi%num_satob
      if( oi%satob(n)%loc%proc_domain_with_halo ) then
         if( oi%satob(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                          (y%satob(n)%v/typical_v_rms)**2
         endif

         pertile_lhs = pertile_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                       (y%satob(n)%v/typical_v_rms)**2

         y%satob(n)%u = y%satob(n)%u/typical_u_rms ** 2
         y%satob(n)%v = y%satob(n)%v/typical_v_rms ** 2
      endif
   ENDDO
   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Satob

