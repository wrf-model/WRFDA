subroutine da_check_xtoy_adjoint_qscat( iv, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: iv            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! Loop counter.

!----------------------------------------------------------------------------

   IF ( iv%num_qscat > 0 ) THEN

   DO n=1, iv%num_qscat
      if( iv%qscat(n)%loc%proc_domain_with_halo ) then
         if( iv%qscat(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs + &
                        ( y%qscat(n)%u/typical_u_rms )**2 + &
                        ( y%qscat(n)%v/typical_v_rms )**2
         endif

         pertile_lhs = pertile_lhs + &
                     ( y%qscat(n)%u/typical_u_rms )**2 + &
                     ( y%qscat(n)%v/typical_v_rms )**2

         y%qscat (n)%u = y%qscat (n)%u/typical_u_rms ** 2
         y%qscat (n)%v = y%qscat (n)%v/typical_v_rms ** 2
      endif
   ENDDO

   ENDIF

end subroutine da_check_xtoy_adjoint_qscat

