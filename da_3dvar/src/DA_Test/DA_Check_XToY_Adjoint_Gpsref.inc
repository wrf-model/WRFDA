SUBROUTINE DA_Check_XToY_Adjoint_Gpsref( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n, k          ! Loop counter.

!----------------------------------------------------------------------------

   IF ( oi%num_Gpsref > 0 ) THEN

   DO n=1, oi%num_Gpsref
      if( oi%gpsref(n)%loc%proc_domain_with_halo ) then
         if( oi%gpsref(n)%loc%proc_domain ) then
            do k=1, oi%Gpsref(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%Gpsref(n)%ref(k) / typical_ref_rms )**2 
            end do
         end if

         do k=1, oi%Gpsref(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%Gpsref(n)%ref(k) / typical_ref_rms )**2

           y%gpsref(n)%ref(k) = y%gpsref(n)%ref(k) / typical_ref_rms ** 2
         end do
      end if
   END DO

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_Gpsref

