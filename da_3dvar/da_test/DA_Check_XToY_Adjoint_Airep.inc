SUBROUTINE DA_Check_XToY_Adjoint_Airep( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Airep > 0 ) THEN

   DO n=1, oi%num_Airep
      if( oi%airep(n)%loc%proc_domain_with_halo ) then
         if( oi%airep(n)%loc%proc_domain ) then
            do k=1, oi%Airep(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%Airep(n)%u(k) / typical_u_rms )**2 + &
                           ( y%Airep(n)%v(k) / typical_v_rms )**2 + &
                           ( y%Airep(n)%t(k) / typical_t_rms )**2 
            end do
         end if

         do k=1, oi%Airep(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%Airep(n)%u(k) / typical_u_rms )**2 + &
                        ( y%Airep(n)%v(k) / typical_v_rms )**2 + &
                        ( y%Airep(n)%t(k) / typical_t_rms )**2

           y%airep(n)%u(k) = y%airep(n)%u(k) / typical_u_rms ** 2
           y%airep(n)%v(k) = y%airep(n)%v(k) / typical_v_rms ** 2
           y%airep(n)%t(k) = y%airep(n)%t(k) / typical_t_rms ** 2
         end do
      end if
   END DO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Airep

