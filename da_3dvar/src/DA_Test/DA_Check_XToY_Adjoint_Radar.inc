SUBROUTINE DA_Check_XToY_Adjoint_Radar( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Radar > 0 ) THEN

      DO n=1, oi%num_Radar

      if( oi%Radar(n)%loc%proc_domain_with_halo ) then

         if( oi%Radar(n)%loc%proc_domain ) then
            do k=1, oi%Radar(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                             ( y%Radar(n)%rv(k)/typical_rv_rms )**2 + &
                             ( y%Radar(n)%rf(k)/typical_rf_rms )**2
            enddo
         endif

        do k=1, oi%Radar(n)%info%levels
           pertile_lhs = pertile_lhs + &
                         ( y%Radar(n)%rv(k)/typical_rv_rms )**2 + &
                         ( y%Radar(n)%rf(k)/typical_rf_rms )**2

           y%Radar(n)%rv(k)= y%Radar(n)%rv(k) / typical_rv_rms ** 2
           y%Radar(n)%rf(k)= y%Radar(n)%rf(k) / typical_rf_rms ** 2
        enddo

      endif
      END DO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Radar
