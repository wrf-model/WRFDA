subroutine da_allocate_cv( cv_size, cv )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate components of control variable.
!
!  METHOD:  Allocate component in turn.
!
!  HISTORY: 06/08/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Define_Structures
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: cv_size
   type (cv_type), intent(inout)        :: cv     ! Control variable structure.

   integer                              :: count, i
   real                                 :: z, mean_cv, rms_cv, std_dev_cv
   real                                 :: sum_cv, sum_cv2

!------------------------------------------------------------------------------
!  [1.0] Initialize parameters:
!------------------------------------------------------------------------------

   cv % j_grad_norm_target = 1.0

!------------------------------------------------------------------------------
!  [2.0] Allocate arrays:
!------------------------------------------------------------------------------

   allocate ( cv % array(1:cv_size) )

   if ( analysis_type == 'randomcv' ) then
   
!     [2.1] Initialize random number generator and scalars:

      call random_seed
      
      sum_cv = 0.0
      sum_cv2 = 0.0
      count = 0

!    [2.2] Calculate random numbers with Gaussian distribution:

     do i = 1, cv_size
   
        call da_gauss_noise(z)
        cv % array(i) = z
         
        sum_cv = sum_cv + cv % array(i)
        sum_cv2 = sum_cv2 + cv % array(i) * cv % array(i)
        count = count + 1
      
        mean_cv = sum_cv / real(count)
        rms_cv = sqrt(sum_cv2 / real(count) )
        std_dev_cv = sqrt( rms_cv * rms_cv - mean_cv * mean_cv )

     end do
   
     write(6,*)
     write(6,'(a)')' Gaussian (Normal) noise statistics:'
     write(6,'(a,f15.5)')' Mean = ',mean_cv
     write(6,'(a,f15.5)')' RMS = ', rms_cv
     write(6,'(a,f15.5)')' STD DEV = ', std_dev_cv

   else

      cv % array = 0.0

   end if

end subroutine da_allocate_cv
