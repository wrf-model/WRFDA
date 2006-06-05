SUBROUTINE DA_Calculate_GradY_GPSRef( iv, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!
!      Called by DA_Minimisation/ DA_Calculate_GradY.inc.
!
!               06/02/2004                   Y.-R. Guo
!            
!-------------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)


   INTEGER :: n, k

   IF ( iv%num_GPSRef < 1 ) return
   
   DO n=1, iv%num_GPSRef

      if( iv%gpsref(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%gpsref(n)%info%levels

            if(iv%GPSRef(n)%ref(k)%qc < obs_qc_pointer) re%GPSRef(n)%ref(k) = 0.0

            jo_grad_y%GPSRef(n)%ref(k) = -re%GPSRef(n)%ref(k) / &
                                      ( iv%GPSRef(n)%ref(k)%error * &
                                        iv%GPSRef(n)%ref(k)%error )
         end do

      endif

   END DO

END SUBROUTINE DA_Calculate_GradY_GPSRef

