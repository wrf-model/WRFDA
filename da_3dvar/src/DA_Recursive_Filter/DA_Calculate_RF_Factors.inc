SUBROUTINE DA_Calculate_RF_Factors( rf_lengthscale, rf_alpha, rf_scale_factor )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate:
!           1) Alpha value for recursive filter.
!           2) Turning conditions appropriate for B=UU^T RF.
!           3) Generic (depends only on rf_passes) rescaling factor.
!           4) Grid-dependent (hopefully temporary) scaling factor - removed.
!
!  METHOD:  See below and documentation.
!
!  HISTORY: 08/07/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   REAL, INTENT(IN)   :: rf_lengthscale(:)      ! Non-dim. R.F. lengthscale.
   REAL, INTENT(OUT)  :: rf_alpha(:)            ! RF alpha factor.
   REAL, INTENT(OUT)  :: rf_scale_factor(:)     ! Variance scaling factor.
      
   INTEGER, PARAMETER :: n = 500                ! 2n +1 = # pts in delta func.
   INTEGER            :: kz                     ! 3rd array dimension.
   INTEGER            :: pass                   ! Pass of recursive filter.
   INTEGER            :: k, nn                  ! Loop counters.
      
   REAL               :: rf_e                   ! RF E factor.      

   real, dimension(-n:n) :: field_in, field_out, field_out1  ! Working field.

!-------------------------------------------------------------------------
!  [1.0]: Initialise:
!-------------------------------------------------------------------------  

   kz = SIZE(rf_scale_factor)
   
   rf_scale_factor(:) = 0.0
   
   DO k = 1, kz

!-------------------------------------------------------------------------
!     [2.0]: Calculate RF alpha:
!-------------------------------------------------------------------------  

      rf_e = 0.25 * rf_passes / ( rf_lengthscale(k) * rf_lengthscale(k) )
      rf_alpha(k) = 1.0 + rf_e - SQRT( rf_e * ( rf_e + 2.0 ) )

!-------------------------------------------------------------------------
!     [3.0]: Calculate rescaling factor:
!-------------------------------------------------------------------------

!     [3.1]: Calculate generic rescaling (normalise zero distance to 1):
!     For rf_passes=2 (SOAR) = 4*rf_lengthscale.
!     For rf_passes=infinity (Gaussian) = SQRT(8*pi)*rf_lengthscale.

      field_in(-n:n) = 0.0
      field_in(0) = 1.0
      field_out(-n:n) = field_in(-n:n)

      DO pass = 1, rf_passes / 2
         CALL DA_Recursive_Filter_1D_Adj( pass, rf_alpha(k), field_out, 2*n+1 )
      END DO

      DO pass = 1, rf_passes / 2
         CALL DA_Recursive_Filter_1D( pass, rf_alpha(k), field_out, 2*n+1 )
      END DO

      rf_scale_factor(k) = 1.0 / field_out(0)

!     Uncomment the following to test equivalence of UU^T and RF:
!      WRITE(0,'(A,f15.5)')' RF Scaling Factor = ', 1.0 / field_out(0)
!      field_out1(-n:n) = field_in(-n:n)
!      DO pass = 1, rf_passes
!         CALL DA_Recursive_Filter_1D( pass, rf_alpha(k), field_out1, 2*n+1 )
!      END DO
         
!      do nn = -n, n
!         WRITE(0,'(2i5,4f12.5)')k, nn, field_in(nn), &
!                                field_out(nn) / field_out(0), &
!                                field_out1(nn) / field_out1(0), &
!                                exp( -0.125*(REAL(nn)/rf_lengthscale(k))**2 )
!      end do

   END DO ! End loop over k
   
END SUBROUTINE DA_Calculate_RF_Factors
      
