SUBROUTINE DA_Check_XToY_Adjoint_Gpspw( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Gpspw > 0 ) THEN

      DO n=1, oi%num_Gpspw
      if( oi%gpspw(n)%loc%proc_domain_with_halo ) then
        if( oi%gpspw(n)%loc%proc_domain ) then
           adjtest_lhs = adjtest_lhs &
                       + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2
        endif

        pertile_lhs = pertile_lhs &
                    + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2

        y%gpspw (n)%tpw = y%gpspw (n)%tpw/typical_tpw_rms ** 2
      endif
      END DO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Gpspw

