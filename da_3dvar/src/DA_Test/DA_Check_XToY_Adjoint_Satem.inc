SUBROUTINE DA_Check_XToY_Adjoint_Satem( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Satem > 0 ) THEN

   DO n=1, oi%num_Satem
      if( oi%satem(n)%loc%proc_domain_with_halo ) then
         if( oi%satem(n)%loc%proc_domain ) then
            do k=1, oi%satem(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%Satem(n)%thickness(k)/typical_thickness_rms )**2
            enddo
         endif

         do k=1, oi%satem(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%Satem(n)%thickness(k)/typical_thickness_rms )**2

            y%satem(n)%thickness(k) = y%satem(n)%thickness(k) /      &
                                      typical_thickness_rms ** 2
   
         enddo
      endif
   ENDDO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Satem

