subroutine da_set_randomcv( cv_size, cv )

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
   real, intent(inout)                  :: cv(1:cv_size)     ! Control variable structure.

   integer                              :: i
   real                                 :: mean_cv, rms_cv, std_dev_cv
   real                                 :: sum_cv, sum_cv2

!  [1] Initialize random number generator and scalars:

   call da_random_seed

   sum_cv = 0.0
   sum_cv2 = 0.0

!  [2] Calculate random numbers with Gaussian distribution:

   do i = 1, cv_size
      call da_gauss_noise(cv(i))

      sum_cv = sum_cv + cv(i)
      sum_cv2 = sum_cv2 + cv(i) * cv(i)
  end do

  mean_cv = sum_cv / real(cv_size)
  rms_cv = sqrt(sum_cv2 / real(cv_size) )
  std_dev_cv = sqrt( rms_cv * rms_cv - mean_cv * mean_cv )

  write(6,*)
  write(6,'(a)')' Gaussian (Normal) noise statistics:'
  write(6,'(a,f15.5)')' Mean = ',mean_cv
  write(6,'(a,f15.5)')' RMS = ', rms_cv
  write(6,'(a,f15.5)')' STD DEV = ', std_dev_cv

end subroutine da_set_randomcv

