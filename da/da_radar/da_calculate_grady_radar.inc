subroutine da_calculate_grady_radar( iv, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n, k

   IF (iv%num_Radar > 0) THEN 
      DO n=1, iv%num_Radar
         do k=1, iv%Radar(n)%info%levels
            IF ( USE_Radar_rv ) THEN
               if (iv%Radar(n)%rv(k)%qc < obs_qc_pointer) THEN
                  re%Radar(n)%rv(k) = 0.0
               end if

               jo_grad_y%Radar(n)%rv(k) = -re%Radar(n)%rv(k) / &
                                      ( iv%Radar(n)%rv(k)%error * &
                                        iv%Radar(n)%rv(k)%error )
            ENDIF

            IF (USE_Radar_rf) THEN
               if (iv%Radar(n)%rf(k)%qc < obs_qc_pointer) then
                  re%Radar(n)%rf(k) = 0.0
               end if
               jo_grad_y%Radar(n)%rf(k) = -re%Radar(n)%rf(k) / &
                                      ( iv%Radar(n)%rf(k)%error * &
                                        iv%Radar(n)%rf(k)%error )
            ENDIF
         end do
      END DO
   END IF

end subroutine da_calculate_grady_radar
