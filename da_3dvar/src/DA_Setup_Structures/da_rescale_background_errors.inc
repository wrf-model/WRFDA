SUBROUTINE DA_Rescale_Background_Errors ( ix, jy, var_scaling, len_scaling, &
                                          ds, s, be_sub )

!------------------------------------------------------------------------------
!  PURPOSE: Rescale 3dvar background errors.
!
!  METHOD:  Empirical scaling and inclusion of recursive filter rescaling.
!
!  HISTORY: 10/10/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)              :: ix                ! I dimension of grid.
   INTEGER, INTENT(IN)              :: jy                ! J dimension of grid.
   REAL, INTENT(IN)                 :: var_scaling       ! Variance factor.
   REAL, INTENT(IN)                 :: len_scaling       ! Lengthscale factor.
   REAL, INTENT(IN)                 :: ds                ! Resolution (m)
   REAL, INTENT(INOUT)              :: s(:)              ! RF lengthscale.
   TYPE (be_subtype), INTENT(INOUT) :: be_sub            ! Backgrd error struct.
    
   INTEGER                          :: mz                ! Vertical truncation.
   INTEGER                          :: m
   REAL, ALLOCATABLE                :: rf_scale_factor(:)! RF rescaling.
   
!-----------------------------------------------------------------------------
!  [1.0] Initialise:
!-----------------------------------------------------------------------------

   mz = be_sub % mz

!-----------------------------------------------------------------------------
!  [2.0] Perform various rescalings:
!-----------------------------------------------------------------------------

   IF ( mz > 0 ) THEN

!     [2.1] Empirical rescaling of lengthscales:
      s(1:mz) = len_scaling * s(1:mz)
   
      IF ( Test_Statistics ) THEN
         WRITE(0,'(a,a)')TRIM(be_sub % name), 'Error Lengthscales (m):'
         do m = 1, mz
            WRITE(0,'(a,i4,1pe13.5)')be_sub % name, m, s(m)
         end do
      END IF
      
!     [2.2] Make lengthscale nondimensional:
      s(1:mz) = s(1:mz) / ds

!     [2.3] Empirical rescaling of variances:
      be_sub % val(:,:) = var_scaling * be_sub % val(:,:)

!     Calculate recursive filter rescaling:

      ALLOCATE( rf_scale_factor(1:mz) )

      CALL DA_Calculate_RF_Factors( s(:), be_sub % rf_alpha(:), &
                                    rf_scale_factor(:) )

      DO m = 1, mz
         be_sub % val(:,m) = rf_scale_factor(m) * be_sub % val(:,m)
      END DO
                                       
      DEALLOCATE ( rf_scale_factor )   

   END IF

END SUBROUTINE DA_Rescale_Background_Errors

