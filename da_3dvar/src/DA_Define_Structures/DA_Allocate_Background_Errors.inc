SUBROUTINE DA_Allocate_Background_Errors ( jy, kz, l, e, be_eval_loc, &
                                           be_evec_loc, be_sub )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate components of wrfvar background errors.
!
!  METHOD:  Allocate component in turn.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)              :: jy                 ! i/y dimension.
   INTEGER, INTENT(IN)              :: kz                 ! k/z dimension.
   REAL, INTENT(IN)                 :: l(:)               ! Global eigenvalue.
   REAL, INTENT(IN)                 :: e(:,:)             ! Global eigenvectors.
   REAL, INTENT(IN)                 :: be_eval_loc(:,:)   ! Std dev/local evalue.
   REAL, INTENT(IN)                 :: be_evec_loc(:,:,:) ! Local eigenvectors.
   TYPE (be_subtype), INTENT(inout) :: be_sub             ! Backgrd error struct.
    
   INTEGER                          :: mz                 ! Vertical truncation.
   INTEGER                          :: j, m, k            ! Loop counter.

   IF (trace_use) CALL da_trace_entry("da_allocate_background_errors")


!-----------------------------------------------------------------------------
!  [1.0] Initialise:
!-----------------------------------------------------------------------------

   mz = be_sub % mz
   
!-----------------------------------------------------------------------------
!  [2.0] Allocate components of be_sub structure:
!-----------------------------------------------------------------------------
   IF ( mz > 0 ) THEN

      ALLOCATE ( be_sub % val(1:jy,1:mz) )
      
      IF ( vert_corr == 2 ) THEN

!-----------------------------------------------------------------------------
!        [3.0] Allocate eigenvalues of vertical error covariance matrix:
!-----------------------------------------------------------------------------

         IF ( vert_evalue == 1 ) THEN

!           Use global eigenvalues:
            do m = 1, mz
               be_sub % val(1:jy,m) = SQRT( l(m) )
            end do

         ELSE

!           Use eigenvalues varying with j-direction.
         do j = 1, jy
         do k = 1, mz
         if(be_eval_loc(j,k) <=0) then
           WRITE(UNIT=errmsg(1),FMT='(A,I5,A,I5,A,F10.2)') &
             "At lat= ",j," For mode = ",k," local eigen value= ",be_eval_loc(j,k)
           CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
         end if
         end do
         end do
            be_sub % val(1:jy,1:mz) = SQRT( be_eval_loc(1:jy,1:mz) )            

         END IF
 
         IF ( Test_Statistics ) THEN
            WRITE(UNIT=stderr,FMT='(/A,A)')' j*k Eigenvalues for ', be_sub % name
            CALL DA_Array_Print(2, be_sub % val(1:jy,1:mz), 'j*k Eigenvalues' )
         END IF

!----------------------------------------------------------------------------- 
!        [4.0] Allocate global eigenvectors of vertical error cov.:
!-----------------------------------------------------------------------------

         ALLOCATE ( be_sub % evec(1:jy,1:kz,1:mz) )
         
         IF ( vert_evalue == 1 ) THEN

!           Use global eigenvectors:
            DO j = 1, jy
               be_sub % evec(j,1:kz,1:mz) = e(1:kz,1:mz)
            END DO

         ELSE

!           Use eigenvectors varying with i-direction.
            be_sub % evec(1:jy,1:kz,1:mz) =  be_evec_loc(1:jy,1:kz,1:mz)

         END IF
         
         IF ( Test_Statistics ) THEN      
            WRITE(UNIT=stderr,FMT='(/A,A)')' k*k Eigenvectors for j = 1', be_sub % name
            CALL DA_Array_Print( 2, be_sub % evec(1,1:kz,1:mz), &
                                 'Eigenvectors for j=1' )
         
            WRITE(UNIT=stderr,FMT='(/A,A)')' k*k Eigenvectors for j = jy', be_sub % name
            CALL DA_Array_Print( 2, be_sub % evec(jy,1:kz,1:mz), &
                                 'Eigenvectors for j=jy' )
         END IF

         allocate (be_sub%val_g(1:mz))
         allocate (be_sub%evec_g(1:kz,1:mz))
  
         be_sub % val_g(1:mz) = l(1:mz)
         be_sub % evec_g(1:kz,1:mz) = e(1:kz,1:mz)
      ELSE IF ( vert_corr == 1 ) THEN

         WRITE(UNIT=stdout) 'Change be std dev to variance in NMC code'
         be_sub % val(1:jy,1:mz) = be_eval_loc(1:jy,1:mz)

      END IF

!-----------------------------------------------------------------------------
!     [2.2] Allocate recursive filter lengthscales and variance scaling factors:
!-----------------------------------------------------------------------------

      ALLOCATE( be_sub % rf_alpha(1:mz) )

      be_sub % rf_alpha(1:mz) = 1.0
      
   END IF

   IF (trace_use) CALL da_trace_exit("da_allocate_background_errors")

END SUBROUTINE DA_Allocate_Background_Errors

