SUBROUTINE DA_Check_Balance( phi, phi_u )

!------------------------------------------------------------------------------
!  PURPOSE: Compare balanced mass (phi_b - function of wind) and actual phi.
!
!  METHOD:  Calculate correlation between balanced and actual phi.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
      
   REAL, INTENT(IN)             :: phi(:,:,:)      ! Total phi.
   REAL, INTENT(IN)             :: phi_u(:,:,:)    ! Unbalanced phi.

   INTEGER                      :: iy              ! Size of 1st dimension.
   INTEGER                      :: jx              ! Size of 2nd dimension.
   INTEGER                      :: kz              ! Size of 3rd dimension.
   INTEGER                      :: i, j, k         ! Loop counters
   REAL                         :: corr_coef       ! Correlation coefficient.
   REAL                         :: accurac         ! Accuracy.
   REAL, ALLOCATABLE            :: phi_b1(:)       ! 1D balanced phi.
   REAL, ALLOCATABLE            :: phi_b2(:,:)     ! 2D balanced phi.
   REAL, ALLOCATABLE            :: corr_coeff(:,:) ! Correlation coefficient.
   REAL, ALLOCATABLE            :: accuracy(:,:)   ! Accuracy.
          
   IF ( balance_type == balance_geo ) THEN
      write(unit=*, fmt='(a)') ' DA_Check_Balance: Balance is geostrophic.'
   ELSE IF ( balance_type == balance_cyc ) THEN
      write(unit=*, fmt='(a)') ' DA_Check_Balance: Balance is cyclostrophic.'
   ELSE IF ( balance_type == balance_geocyc ) THEN
      write(unit=*, fmt='(a)') ' DA_Check_Balance: Balance is geo/cyclostrophic.'
   END IF
      
   write(unit=*, fmt='(a)') ' DA_Check_Balance: Correlation/accuracy: '
      
!-------------------------------------------------------------------------
!  [1.0]: Initialise:
!-------------------------------------------------------------------------  

   iy = SIZE(phi_u, DIM=1)
   jx = SIZE(phi_u, DIM=2)
   kz = SIZE(phi_u, DIM=3)
      
   ALLOCATE(phi_b1(1:jx))
   ALLOCATE(phi_b2(1:iy,1:jx))

   ALLOCATE(corr_coeff(1:kz,1:iy))
   corr_coeff(1:kz,1:iy) = 0.0

   ALLOCATE(accuracy(1:kz,1:iy))
   accuracy(1:kz,1:iy) = 0.0
      
!-------------------------------------------------------------------------
!  [2.0]: Calculate correlations/accuracy:
!-------------------------------------------------------------------------  

   do k = 1, kz
      do i = 1, iy

         phi_b1(2:jx-1) = phi(i,2:jx-1,k) - phi_u(i,2:jx-1,k)
            
         CALL DA_Correlation_Coeff1d( phi_b1(2:jx-1), phi(i,2:jx-1,k), &
                                      corr_coeff(k,i), accuracy(k,i) )
     
!        write(58,*) corr_coeff(k,i), accuracy(k,i)
      end do
         
      phi_b2(2:iy-1,2:jx-1) = phi(2:iy-1,2:jx-1,k) - phi_u(2:iy-1,2:jx-1,k)
      CALL DA_Correlation_Coeff2d( phi_b2(2:iy-1,2:jx-1), &
                                   phi(2:iy-1,2:jx-1,k), &
                                   corr_coef, accurac )

      write(unit=*, fmt='(i6,1pe9.2,1pe9.2)') &
            k, corr_coef, accurac

   end do

!-------------------------------------------------------------------------
!  [3.0]: Tidy up:
!-------------------------------------------------------------------------  

   DEALLOCATE(phi_b1)
   DEALLOCATE(phi_b2)
   DEALLOCATE(corr_coeff)
   DEALLOCATE(accuracy)

END SUBROUTINE DA_Check_Balance

