SUBROUTINE DA_Check_XToY_Adjoint_Sound( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Sound < 1 ) return

   DO n=1, oi%num_Sound
      if( oi%sound(n)%loc%proc_domain_with_halo ) then
         if( oi%sound(n)%loc%proc_domain ) then
            do k=1, oi%sound(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                             ( y%Sound(n)%u(k)/typical_u_rms )**2 + &
                             ( y%Sound(n)%v(k)/typical_v_rms )**2 + &
                             ( y%Sound(n)%t(k)/typical_t_rms )**2 + &
                             ( y%Sound(n)%q(k)/typical_q_rms )**2
            enddo
         endif

         do k=1, oi%sound(n)%info%levels
            pertile_lhs = pertile_lhs + &
                          ( y%Sound(n)%u(k)/typical_u_rms )**2 + &
                          ( y%Sound(n)%v(k)/typical_v_rms )**2 + &
                          ( y%Sound(n)%t(k)/typical_t_rms )**2 + &
                          ( y%Sound(n)%q(k)/typical_q_rms )**2

            y%sound(n)%u(k) = y%sound(n)%u(k) / typical_u_rms ** 2
            y%sound(n)%v(k) = y%sound(n)%v(k) / typical_v_rms ** 2
            y%sound(n)%t(k) = y%sound(n)%t(k) / typical_t_rms ** 2
            y%sound(n)%q(k) = y%sound(n)%q(k) / typical_q_rms ** 2
         enddo
      endif
   ENDDO

END SUBROUTINE DA_Check_XToY_Adjoint_Sound

