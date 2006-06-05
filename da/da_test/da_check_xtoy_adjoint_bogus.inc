SUBROUTINE DA_Check_XToY_Adjoint_Bogus( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Bogus > 0 ) THEN

   DO n=1, oi%num_Bogus
      if( oi%bogus(n)%loc%proc_domain_with_halo ) then
         if( oi%bogus(n)%loc%proc_domain ) then
            do k=1, oi%bogus(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                             ( y%Bogus(n)%u(k)/typical_u_rms )**2 + &
                             ( y%Bogus(n)%v(k)/typical_v_rms )**2 + &
                             ( y%Bogus(n)%t(k)/typical_t_rms )**2 + &
                             ( y%Bogus(n)%q(k)/typical_q_rms )**2 
            enddo
            adjtest_lhs = adjtest_lhs + &
                          ( y%Bogus(n)%slp/typical_p_rms )**2
         end if

         do k=1, oi%bogus(n)%info%levels
            pertile_lhs = pertile_lhs + &
                          ( y%bogus(n)%u(k)/typical_u_rms )**2 + &
                          ( y%bogus(n)%v(k)/typical_v_rms )**2 + &
                          ( y%bogus(n)%t(k)/typical_t_rms )**2 + &
                          ( y%bogus(n)%q(k)/typical_q_rms )**2

            y%bogus(n)%u(k) = y%bogus(n)%u(k) / typical_u_rms ** 2
            y%bogus(n)%v(k) = y%bogus(n)%v(k) / typical_v_rms ** 2
            y%bogus(n)%t(k) = y%bogus(n)%t(k) / typical_t_rms ** 2
            y%bogus(n)%q(k) = y%bogus(n)%q(k) / typical_q_rms ** 2
         enddo
         pertile_lhs = pertile_lhs + &
                       ( y%bogus(n)%slp/typical_p_rms )**2
         y%bogus(n)%slp = y%bogus(n)%slp / typical_p_rms ** 2

      endif

   END DO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Bogus
