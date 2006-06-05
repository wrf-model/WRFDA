SUBROUTINE DA_Check_XToY_Adjoint_pilot( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs  

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_pilot > 0 ) THEN

   DO n=1, oi%num_pilot
      if( oi%pilot(n)%loc%proc_domain_with_halo ) then
         if( oi%pilot(n)%loc%proc_domain ) then
            do k=1, oi%pilot(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%pilot(n)%u(k)/typical_u_rms )**2 + &
                           ( y%pilot(n)%v(k)/typical_v_rms )**2

            end do
         end if

         do k=1, oi%pilot(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%pilot(n)%u(k)/typical_u_rms )**2 + &
                        ( y%pilot(n)%v(k)/typical_v_rms )**2

            y%pilot(n)%u(k)= y%pilot(n)%u(k) / typical_u_rms ** 2
            y%pilot(n)%v(k)= y%pilot(n)%v(k) / typical_v_rms ** 2
         end do
         end if
   END DO

   END IF

END SUBROUTINE DA_Check_XToY_Adjoint_pilot

