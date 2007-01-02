SUBROUTINE da_check_xtoy_adjoint_ssmt1( iv, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: iv            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( iv%num_ssmt1 > 0 ) THEN
   DO n=1, iv%num_ssmt1
      if( iv%ssmt1(n)%loc%proc_domain_with_halo ) then
         if( iv%ssmt1(n)%loc%proc_domain ) then
            do k=1, iv%ssmt1(n)%info%levels
               adjtest_lhs = adjtest_lhs + ( y%ssmt1(n)%t(k)/typical_t_rms )**2
            enddo
         endif

         do k=1, iv%ssmt1(n)%info%levels
            pertile_lhs = pertile_lhs + ( y%ssmt1(n)%t(k)/typical_t_rms )**2

            y%ssmt1(n)%t(k) = y%ssmt1(n)%t(k) / typical_t_rms ** 2
         enddo
      endif
   ENDDO
   ENDIF

END SUBROUTINE da_check_xtoy_adjoint_ssmt1

