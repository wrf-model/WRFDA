subroutine da_calculate_jo_and_grady_airsr( iv, re, jo, jo_grad_y )
!-------------------------------------------------------------------------
!  History:
!
!     Creation:        03/07/2006                Syed RH Rizvi        
!
!     Purpose: 
!             Calculates cost function and its gradient
!             at all AIRS retrieval locations   
!-------------------------------------------------------------------------

  IMPLICIT NONE

  TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
  TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
  TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
  type (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.

  INTEGER                      :: n, k
  LOGICAL, EXTERNAL            :: wrf_dm_on_monitor
  ! the following "global" objects are used only when testing
  TYPE (ob_type) :: iv_glob         ! Global Innovation vector (O-B).
  TYPE (y_type)  :: re_glob         ! Global Residual vector (O-A).
  TYPE (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

  jo % airsr_t = 0.0
  jo % airsr_q = 0.0

   IF ( iv%num_airsr < 0 ) RETURN

  DO n=1, iv%num_airsr
    if( iv%airsr(n)%loc%proc_domain_with_halo ) then
      do k=1, iv%airsr(n)%info%levels
        jo_grad_y%airsr(n)%t(k) = -re%airsr(n)%t(k) / &
                                     ( iv%airsr(n)%t(k)%error * &
                                       iv%airsr(n)%t(k)%error )
        jo_grad_y%airsr(n)%q(k) = -re%airsr(n)%q(k) / &
                                     ( iv%airsr(n)%q(k)%error * &
                                       iv%airsr(n)%q(k)%error )
      end do
    endif

      if( iv%airsr(n)%loc%proc_domain ) then
        do k=1, iv%airsr(n)%info%levels
           jo % airsr_t = jo % airsr_t - &
                        re%airsr(n)%t(k) * jo_grad_y%airsr(n)%t(k)
           jo % airsr_q = jo % airsr_q - &
                        re%airsr(n)%q(k) * jo_grad_y%airsr(n)%q(k)
        end do
      endif
  END DO

  jo % airsr_t = 0.5 * jo % airsr_t
  jo % airsr_q = 0.5 * jo % airsr_q

  return

end subroutine da_calculate_jo_and_grady_airsr
