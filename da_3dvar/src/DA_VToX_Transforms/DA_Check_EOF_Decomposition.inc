SUBROUTINE DA_Check_EOF_Decomposition( be_eigenval, be_eigenvec, name )

!------------------------------------------------------------------------------
!  PURPOSE: Check eigenvectors E of vertical covariance matrix B_{x} which have 
!           been read in from NMC-statistics file.
!
!  METHOD:  E and L (eigenvalues) are computed using LAPACK/BLAS software in the 
!           NMC code using the definition E^{T} B_{x} E = L. This routine checks 
!           for eigenvector orthogonality and completeness as defined below.
!
!  HISTORY: 05/18/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
      
   REAL, INTENT(IN)              :: be_eigenval(:)   ! Back. error eigenvalues.
   REAL, INTENT(IN)              :: be_eigenvec(:,:) ! Back. error eigenvector
   CHARACTER(LEN=*), INTENT(IN)  :: name             ! Variable name.

   INTEGER                       :: kz               ! Size of 3rd dimension.
   INTEGER                       :: k, k1, k2, m     ! Loop counters
   REAL                          :: tot_variance     ! Total variance.
   REAL                          :: cum_variance     ! Cumulative variance.
   REAL                          :: max_off_diag     ! Maximum off-diagonal.
      
   REAL, ALLOCATABLE             :: matrix2(:,:)     ! 2D Work matrix.
   LOGICAL, ALLOCATABLE          :: array_mask(:)    ! Array mask for MAXVAL.

!-------------------------------------------------------------------------
!  [1.0]: Initialise:
!-------------------------------------------------------------------------  

   kz = SIZE(be_eigenval)
                         
!-------------------------------------------------------------------------
!  [2.0]: Print out global eigenvalues (used for truncation):
!-------------------------------------------------------------------------  

   cum_variance = 0.0
   tot_variance = SUM( be_eigenval(1:kz) )

   WRITE(0,'(A)')' Name Mode  Eigenvalue Normalised Variance'
   DO k = 1, kz
      cum_variance =  be_eigenval(k) + cum_variance
      WRITE(0,'(A,I4,e14.4,f10.4)') trim(name), k, be_eigenval(k), &
                                    cum_variance / tot_variance
   END DO
   WRITE(0,*)
   WRITE(0,'(A,e13.5)')' Total variance = Tr(Bv) = ', tot_variance

!-----------------------------------------------------------------------------
!  [2.0]: Test global eigenvectors:
!-----------------------------------------------------------------------------

!  [2.1] Print out global eigenvectors:

   WRITE(0,'(3A)') ' DA_Check_EOF_Decomposition:', &
                   ' Domain eigenvectors for ', trim(name)

   WRITE(0,'(50i13)')( m, m=1,kz)
   do k = 1, kz      
      WRITE(0,'(I3,50e13.5)')k, ( be_eigenvec(k,m), m=1,kz)
   end do

!  [2.2]: Test eigenvector orthogonality: sum_k (e_m(k) e_n(k)) = delta_mn:

   ALLOCATE( array_mask(1:kz) )
   ALLOCATE( matrix2(1:kz,1:kz) )
      
   WRITE(0,'(2A)')' Eigenvector orthogonality check for ', trim(name)
   WRITE(0,'(A)')' Mode     Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = SUM( be_eigenvec(:,k1) * be_eigenvec(:,k2) )
      end do
         
      array_mask(1:kz) =.TRUE.
      array_mask(k1) = .FALSE.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      WRITE(0,'(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), max_off_diag
   end do
   WRITE(0,*)

!  [2.3]: Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:

   WRITE(0,'(2A)')' Eigenvector completeness check for ', trim(name)
   WRITE(0,'(A)')' Level    Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = SUM(be_eigenvec(k1,:) * be_eigenvec(k2,:))
      end do
         
      array_mask(1:kz) =.TRUE.
      array_mask(k1) = .FALSE.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      WRITE(0,'(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), max_off_diag
   end do
   WRITE(0,*)

!-------------------------------------------------------------------------
!  [3.0]: Tidy up:
!-------------------------------------------------------------------------  

   DEALLOCATE( matrix2 )
   DEALLOCATE( array_mask )
       
END SUBROUTINE DA_Check_EOF_Decomposition

