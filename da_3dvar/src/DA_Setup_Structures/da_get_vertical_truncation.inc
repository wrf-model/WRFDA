SUBROUTINE DA_Get_Vertical_Truncation( max_vert_var, eigenval, be_sub )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate vertical mode truncation from explained variance.
!
!  METHOD:  Calculate cumulative variance and compare with limit.
!
!  HISTORY: 06/01/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)                 :: max_vert_var    ! Vertical variance limit.
   REAL, INTENT(IN)                 :: eigenval(:)     ! Global eigenvaluess.
   TYPE (be_subtype), INTENT(INOUT) :: be_sub          ! Back. error sub type.
    
   INTEGER                          :: kz              ! # vertical levels.
   INTEGER                          :: k               ! Loop counter.
   REAL                             :: tot_variance    ! Total variance.
   REAL                             :: cum_variance    ! Cumulative variance.
   CHARACTER (LEN = 6)              :: name            ! Variable name.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   kz = SIZE( eigenval(:) )
   name = TRIM(be_sub % name)

!------------------------------------------------------------------------------
!  [2.0] Calculate vertical truncation:
!------------------------------------------------------------------------------

   IF ( max_vert_var >= 100.0 ) THEN
   
!     [2.1] No truncation: 
      be_sub % mz = kz

!     Disregard zero/-ve eigenvalues (which should be very small and only 
!     appear if statistics have been interpolated between domains):

      DO k = 1, kz
         if ( eigenval(k) <= 0.0 ) then
            be_sub % mz = k - 1
            exit
         end if
      END DO
      
   ELSE
   
!     [2.2] Calculate cumulative variance and truncate:

      tot_variance = SUM( eigenval(1:kz) )
      cum_variance = 0.0
      
      DO k = 1, kz
         cum_variance = cum_variance + eigenval(k)
         
         if ( eigenval(k) <= 0.0 ) then
            be_sub % mz = k - 1
            exit
         end if
         
         IF ( cum_variance/tot_variance >= 0.01 * max_vert_var ) then
            be_sub % mz = k
            exit
         END IF  
      END DO
      
      IF ( max_vert_var == 0.0 ) be_sub % mz = 0 

   END IF

   WRITE(6,'(3x,A,A6,A3,I3,A1,f7.2,A2)')' Vertical truncation for ', name, &
                                       ' = ', be_sub % mz, '(', &
                                       max_vert_var, '%)'
                                       
END SUBROUTINE DA_Get_Vertical_Truncation
