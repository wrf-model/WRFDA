SUBROUTINE DA_Get_Jo_and_GradY_Sonde_sfc( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % sonde_sfc_u = 0.0
   jo % sonde_sfc_v = 0.0
   jo % sonde_sfc_t = 0.0
   jo % sonde_sfc_p = 0.0
   jo % sonde_sfc_q = 0.0

   IF ( iv%num_sound < 1 ) return

      DO n=1, iv%num_sound

       if( iv%sonde_sfc(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%sonde_sfc(n)%u = -re%sonde_sfc(n)%u / &
                                ( iv%sonde_sfc(n)%u%error * iv%sonde_sfc(n)%u%error)
         jo_grad_y%sonde_sfc(n)%v = -re%sonde_sfc(n)%v / &
                                ( iv%sonde_sfc(n)%v%error * iv%sonde_sfc(n)%v%error)
         jo_grad_y%sonde_sfc(n)%t = -re%sonde_sfc(n)%t / &
                                ( iv%sonde_sfc(n)%t%error * iv%sonde_sfc(n)%t%error)
         jo_grad_y%sonde_sfc(n)%p = -re%sonde_sfc(n)%p / &
                                ( iv%sonde_sfc(n)%p%error * iv%sonde_sfc(n)%p%error)
         jo_grad_y%sonde_sfc(n)%q = -re%sonde_sfc(n)%q / &
                                ( iv%sonde_sfc(n)%q%error * iv%sonde_sfc(n)%q%error)
       endif
  
       if( iv%sonde_sfc(n)%loc%proc_domain ) then

         jo % sonde_sfc_u = jo % sonde_sfc_u - re%sonde_sfc(n)%u * jo_grad_y%sonde_sfc(n)%u
         jo % sonde_sfc_v = jo % sonde_sfc_v - re%sonde_sfc(n)%v * jo_grad_y%sonde_sfc(n)%v
         jo % sonde_sfc_t = jo % sonde_sfc_t - re%sonde_sfc(n)%t * jo_grad_y%sonde_sfc(n)%t
         jo % sonde_sfc_p = jo % sonde_sfc_p - re%sonde_sfc(n)%p * jo_grad_y%sonde_sfc(n)%p
         jo % sonde_sfc_q = jo % sonde_sfc_q - re%sonde_sfc(n)%q * jo_grad_y%sonde_sfc(n)%q

       endif

      END DO
      
      jo % sonde_sfc_u = 0.5 * jo % sonde_sfc_u
      jo % sonde_sfc_v = 0.5 * jo % sonde_sfc_v
      jo % sonde_sfc_t = 0.5 * jo % sonde_sfc_t
      jo % sonde_sfc_p = 0.5 * jo % sonde_sfc_p
      jo % sonde_sfc_q = 0.5 * jo % sonde_sfc_q
     
END SUBROUTINE DA_Get_Jo_and_GradY_Sonde_sfc

