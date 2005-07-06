SUBROUTINE DA_Check_XToY_Adjoint_PolarAMV ( oi, y, adjtest_lhs, pertile_lhs )
!----------------------------------------------------------------------------
!  History:
!    Created:      03/04/2005                            Syed RH Rizvi           
!    Purpose:  Adjoint Test for Polar AMVs Obs
!----------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n, k          ! Loop counter.

!
!----------------------------------------------------------------------------

   IF ( oi%num_polaramv > 0 ) THEN

   DO n=1, oi%num_polaramv
      if( oi%polaramv(n)%loc%proc_domain_with_halo ) then
         if( oi%polaramv(n)%loc%proc_domain ) then
            do k=1, oi%polaramv(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%polaramv(n)%u(k)/typical_u_rms )**2 + &
                           ( y%polaramv(n)%v(k)/typical_v_rms )**2

            end do
         end if

         do k=1, oi%polaramv(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%polaramv(n)%u(k)/typical_u_rms )**2 + &
                        ( y%polaramv(n)%v(k)/typical_v_rms )**2

            y%polaramv(n)%u(k)= y%polaramv(n)%u(k) / typical_u_rms ** 2
            y%polaramv(n)%v(k)= y%polaramv(n)%v(k) / typical_v_rms ** 2
         end do
         end if
   END DO

   END IF

END SUBROUTINE DA_Check_XToY_Adjoint_PolarAMV
