subroutine da_calculate_jo_and_grady_rad( iv, re, jo, jo_grad_y ) 

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculate Gradient_y i and cost function Jo for radiance data.
   !
   ! METHOD:  grad_y = -R^-1 ( d - H delta_x )
   !              Jo = -( d - H delta_x ) grad_y
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! Innovation vector.
   TYPE (y_type) , INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type) , INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   TYPE (jo_type), INTENT(INOUT) :: jo          ! Obs cost function.

   INTEGER                       :: n, k, i

   IF ( iv%num_inst < 1 ) RETURN

   if (trace_use) call da_trace_entry("da_calculate_jo_and_grady_rad")

   ALLOCATE ( jo % rad(1:iv%num_inst) )

   DO i =1, iv%num_inst
      ALLOCATE ( jo % rad(i) % jo_ichan(iv%instid(i)%nchan) )
      ALLOCATE ( jo % rad(i) % num_ichan(iv%instid(i)%nchan) )

      jo % rad(i)%jo_ichan(:) = 0.0
      jo % rad(i)%num_ichan(:) = 0

      IF (iv%instid(i)%num_rad < 1) CYCLE

      DO n=1, iv%instid(i)%num_rad
         DO k=1, iv%instid(i)%nchan
            jo_grad_y%instid(i)%rad(n)%tb(k) = -re%instid(i)%rad(n)%tb(k) / &
                                    ( iv%instid(i)%rad(n)%tb(k)%error * &
                                      iv%instid(i)%rad(n)%tb(k)%error )
         END DO
         IF (iv%instid(i)%rad(n)%loc%proc_domain) THEN
            DO k=1, iv%instid(i)%nchan
               IF (iv%instid(i)%rad(n)%tb(k)%qc >= obs_qc_pointer) THEN
                  jo % rad(i) % jo_ichan(k) = jo % rad(i) % jo_ichan(k) - &
                     re%instid(i)%rad(n)%tb(k) * jo_grad_y%instid(i)%rad(n)%tb(k)
                  jo % rad(i) % num_ichan(k) = jo % rad(i) % num_ichan(k) + 1
               END IF
            END DO
         END IF
      END DO
      jo % rad(i)%jo_ichan(:) = 0.5 * jo % rad(i)%jo_ichan(:)

   END DO

   if (trace_use) call da_trace_exit("da_calculate_jo_and_grady_rad")

end subroutine da_calculate_jo_and_grady_rad

