SUBROUTINE DA_Setup_Background_Errors5(xb, be, xbx, xp)

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.
   TYPE (xbx_type),INTENT(IN)  :: xbx                   ! Header non-grid info 
   type (xpose_type), intent(in) :: xp   ! Dimensions and xpose buffers.

   INTEGER                     :: nrec, max_wavenumber  ! Scalar.
   INTEGER                     :: ij,ijk                ! Scalar.
   INTEGER                     :: i, j, k, m, ic, jc       ! Loop counters.
   INTEGER                     :: ier                   ! error index
   real, allocatable   :: height(:,:,:)      ! Height field.
   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point
   integer             :: bin_type           ! Type of bin to average over.
   integer             :: num_bins           ! Number of bins (3D fields).
   integer             :: num_bins2d         ! Number of bins (3D fields).
   logical  :: use_global_eofs               ! True if projected data uses global EOFs
   REAL, ALLOCATABLE           :: be1_eval_loc(:,:)     ! Temp arrays.
   REAL, ALLOCATABLE           :: be2_eval_loc(:,:)     ! Temp arrays.
   REAL, ALLOCATABLE           :: be3_eval_loc(:,:)     ! Temp arrays.
   REAL, ALLOCATABLE           :: be4_eval_loc(:,:)     ! Temp arrays.
   REAL, ALLOCATABLE           :: be5_eval_loc(:,:)     ! Temp arrays.

   REAL, ALLOCATABLE           :: be1_eval_glo(:)       ! Global Eigenvalues.
   REAL, ALLOCATABLE           :: be2_eval_glo(:)       ! Global Eigenvalues.
   REAL, ALLOCATABLE           :: be3_eval_glo(:)       ! Global Eigenvalues.
   REAL, ALLOCATABLE           :: be4_eval_glo(:)       ! Global Eigenvalues.
   REAL, ALLOCATABLE           :: be5_eval_glo(:)       ! Global Eigenvalues.

   REAL, ALLOCATABLE           :: be1_evec_loc(:,:,:)   ! Local Eigenvectors.
   REAL, ALLOCATABLE           :: be2_evec_loc(:,:,:)   ! Local Eigenvectors.
   REAL, ALLOCATABLE           :: be3_evec_loc(:,:,:)   ! Local Eigenvectors.
   REAL, ALLOCATABLE           :: be4_evec_loc(:,:,:)   ! Local Eigenvectors.
   REAL, ALLOCATABLE           :: be5_evec_loc(:,:,:)   ! Local Eigenvectors.

   REAL, ALLOCATABLE           :: be1_evec_glo(:,:)     ! Global Eigenvectors.
   REAL, ALLOCATABLE           :: be2_evec_glo(:,:)     ! Global Eigenvectors.
   REAL, ALLOCATABLE           :: be3_evec_glo(:,:)     ! Global Eigenvectors.
   REAL, ALLOCATABLE           :: be4_evec_glo(:,:)     ! Global Eigenvectors.
   REAL, ALLOCATABLE           :: be5_evec_glo(:,:)     ! Global Eigenvectors.

   REAL, ALLOCATABLE           :: be1_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be2_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be3_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be4_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be5_rf_lengthscale(:)

!------------------------------------------------------------------------------
!
   character*10                :: variable
   integer                     :: ni, nj, nk, b, nlev           
   integer                     :: ix, jy, kz
   real, allocatable           :: regcoeff1(:)
   real, allocatable           :: regcoeff2(:,:)
   real, allocatable           :: regcoeff3(:,:,:)
   real                        :: avg,avg2,avg3
!---------------------------------------------------------------------
   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,'(A,I3)') ' [3.0] Set up background errors (be) for cv_option:', cv_options
   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,*)

   IF(cv_options /= 5) THEN
      write(unit=*, fmt='(a, i2)') &
           'cv_options:', cv_options

      CALL wrf_shutdown

      STOP "Stopped for Wrong CV_OPTION."
   ENDIF

   be % cv % option = cv_options

   write(6,'(3x,A)')' DA_Setup_Background_Errors: 3DVAR dry control variables are:'
   write(6,'(4x,7A)')TRIM(be % v1 % name), ', ', TRIM(be % v2 % name), ', ', &
                  TRIM(be % v3 % name), ' and ', TRIM(be % v5 % name)

   write(6,'(3x,A,A)')' DA_Setup_Background_Errors: humidity control variable is ',&
                     TRIM(be % v4 % name)

   write(6,*)
!---------------------------------------------------------
   ix = xb % mix
   jy = xb % mjy
   kz = xb % mkz

!-------------------------
rewind (be_cv_5_unit)
!---------------------------------------------------------
   read(be_cv_5_unit) ni, nj, nk 
   read(be_cv_5_unit) num_bins, num_bins2d
!---------------------------------------------------------

   allocate (regcoeff1(1:num_bins))
   allocate (regcoeff2(1:nk,1:num_bins2d))
   allocate (regcoeff3(1:nk,1:nk,1:num_bins2d))

   read(be_cv_5_unit) regcoeff1  
   read(be_cv_5_unit) regcoeff2 
   read(be_cv_5_unit) regcoeff3  

   allocate (be%reg_chi(1:nj,1:nk))
   allocate (be%reg_ps (1:nj,1:nk))
   allocate (be%reg_t  (1:nj,1:nk,1:nk))

!-------------------------------------------------------------
   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   call da_create_bins( ni, nj, nk, bin_type, num_bins, &
                                num_bins2d, bin, bin2d )
!--------------------------------------------------------------
! Following "latitude, height" variables are optional arguments 
! The same may be added, if required
!--------------------------------------------------------------
! Fill regression coeff. array
   do k=1,nk
    do j =1, nj
      b = bin(1,j,k)
      be%reg_chi(j,k) = regcoeff1(b)
    end do
   end do

!
   do j=1,nj
      b = bin2d(1,j)
      do k=1,nk
       be%reg_ps(j,k) = regcoeff2(k,b)
      end do
   end do
!
   do j=1,nj
      b = bin2d(1,j)
      do i=1,nk
      do k=1,nk
       be%reg_t(j,i,k) = regcoeff3(i,k,b)
      end do
      end do
   end do
!
!   write(901,'(10e13.5)') ((be%reg_chi (j,k),j=1,num_bins2d),k=1,nk)
!   write(902,'(10e13.5)') ((be%reg_ps (j,k),j=1,num_bins2d),k=1,nk)
!   write(903,'(10e13.5)') ((be%reg_t (j,k,k),j=1,num_bins2d),k=1,nk)

    if (.not.lat_stats_option) then
     write(unit=*, fmt='(/5x, a/)') &
     '*** Using the averaged regression coefficients for unbalanced part ***'

    do k=1,nk
    avg= 0.0
    avg2=0.0
    do j=1,num_bins2d
    avg= avg + be%reg_ps (j,k)/float(num_bins2d) 
    avg2= avg2 + be%reg_chi (j,k)/float(num_bins2d) 
    enddo
!
    do j=1,num_bins2d
    be%reg_ps(j,k)=avg
    be%reg_chi (j,k)=avg2
    enddo
    enddo

    do m=1,nk
    do k=1,nk
    avg3= 0.0

    do j=1,num_bins2d
    avg3= avg3 + be%reg_t (j,k,m)/float(num_bins2d)
    enddo

    do j=1,num_bins2d
    be%reg_t(j,k,m)=avg3
    enddo

    enddo
    enddo

    else

    write(unit=*, fmt='(/5x, a/)') &
    '*** Using the latitude-dependent regression coefficients for unbalanced part ***'
    endif

!------------------------------------------------------------------------------
!  [4.0] Allocate temporary arrays:
!------------------------------------------------------------------------------

   ALLOCATE ( be1_eval_loc (1:jy,1:kz) )
   ALLOCATE ( be2_eval_loc (1:jy,1:kz) )
   ALLOCATE ( be3_eval_loc (1:jy,1:kz) )
   ALLOCATE ( be4_eval_loc (1:jy,1:kz) )
   ALLOCATE ( be5_eval_loc (1:jy,1:kz) )

   IF ( vert_corr == 2 ) THEN

      ALLOCATE ( be1_eval_glo(1:kz) )
      ALLOCATE ( be2_eval_glo(1:kz) )
      ALLOCATE ( be3_eval_glo(1:kz) )
      ALLOCATE ( be4_eval_glo(1:kz) )
      ALLOCATE ( be5_eval_glo(1:kz) )

      ALLOCATE ( be1_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be2_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be3_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be4_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be5_evec_loc(1:jy,1:kz,1:kz) )

      ALLOCATE ( be1_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be2_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be3_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be4_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be5_evec_glo(1:kz,1:kz) )
   END IF
!CV1
   read(be_cv_5_unit) variable   
   read(be_cv_5_unit) nk, num_bins2d 
   print*,'variable ',variable,nk,num_bins2d
!
   be % v1 % name = variable

   read(be_cv_5_unit)  be1_evec_glo   
   read(be_cv_5_unit)  be1_eval_glo    
   read(be_cv_5_unit)  be1_evec_loc     
   read(be_cv_5_unit)  be1_eval_loc    
!CV2
   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   print*,'variable ',variable,nk,num_bins2d
!
   be % v2 % name = variable

   read(be_cv_5_unit)  be2_evec_glo
   read(be_cv_5_unit)  be2_eval_glo
   read(be_cv_5_unit)  be2_evec_loc
   read(be_cv_5_unit)  be2_eval_loc

!CV3
   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   print*,'variable ',variable,nk,num_bins2d
!
   be % v3 % name = variable

   read(be_cv_5_unit)  be3_evec_glo
   read(be_cv_5_unit)  be3_eval_glo
   read(be_cv_5_unit)  be3_evec_loc
   read(be_cv_5_unit)  be3_eval_loc

!CV4
   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   print*,'variable ',variable,nk,num_bins2d
!
   be % v4 % name = variable

   read(be_cv_5_unit)  be4_evec_glo
   read(be_cv_5_unit)  be4_eval_glo
   read(be_cv_5_unit)  be4_evec_loc
   read(be_cv_5_unit)  be4_eval_loc
!
   IF ( vert_corr == 2 ) THEN

!     [11.1] Perform checks on eigenvectors:

      IF ( test_statistics ) THEN
         CALL DA_Check_EOF_Decomposition( be1_eval_glo(:), be1_evec_glo(:,:), be % v1 % name)
         CALL DA_Check_EOF_Decomposition( be2_eval_glo(:), be2_evec_glo(:,:), be % v2 % name)
         CALL DA_Check_EOF_Decomposition( be3_eval_glo(:), be3_evec_glo(:,:), be % v3 % name)
         CALL DA_Check_EOF_Decomposition( be4_eval_glo(:), be4_evec_glo(:,:), be % v4 % name)
      END IF

!     [11.2] Truncate in vertical:
      CALL DA_Get_Vertical_Truncation( max_vert_var1, be1_eval_glo(:), be % v1 )
      CALL DA_Get_Vertical_Truncation( max_vert_var2, be2_eval_glo(:), be % v2 )
      CALL DA_Get_Vertical_Truncation( max_vert_var3, be3_eval_glo(:), be % v3 )
      CALL DA_Get_Vertical_Truncation( max_vert_var4, be4_eval_glo(:), be % v4 )

      be % v5 % mz = 1

      write(6,*)

   ELSE
      be % v1 % mz = xb % mkz
      be % v2 % mz = xb % mkz
      be % v3 % mz = xb % mkz
      be % v4 % mz = xb % mkz
      be % v5 % mz = xb % mkz

   END IF
!------------------------------------------------------------------------------
!  [12.0] Initialise control variable space components of header:
!------------------------------------------------------------------------------
!This is for DA_Test
   kz_vv(1) = be % v1 % mz
   kz_vv(2) = be % v2 % mz
   kz_vv(3) = be % v3 % mz
   kz_vv(4) = be % v4 % mz
   kz_vv(5) = be % v5 % mz
   kz_vp    = xb % mkz
!------
   be % mix = ix
   be % mjy = jy

   ij = ix * jy

   be % cv % size1  = ij * be % v1 % mz    ! Fix size, read from file later.
   be % cv % size2  = ij * be % v2 % mz    ! Fix size, read from file later.
   be % cv % size3  = ij * be % v3 % mz    ! Fix size, read from file later.
   be % cv % size4  = ij * be % v4 % mz    ! Fix size, read from file later.
   be % cv % size5  = ij * be % v5 % mz    ! Fix size, read from file later.

   print*,'mz size', be % v1 % mz, be % v2 % mz, be % v3 % mz, be % v4 % mz, be % v5 % mz
   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size
!------------------------------------------------------------------------------
!  [13.0] Transfer errors to error structure:
!------------------------------------------------------------------------------

   CALL DA_Allocate_Background_Errors( jy, kz, be1_eval_glo, be1_evec_glo, be1_eval_loc, &
                                       be1_evec_loc, be % v1 )
   CALL DA_Allocate_Background_Errors( jy, kz, be2_eval_glo, be2_evec_glo, be2_eval_loc, &
                                       be2_evec_loc, be % v2 )
   CALL DA_Allocate_Background_Errors( jy, kz, be3_eval_glo, be3_evec_glo, be3_eval_loc, &
                                       be3_evec_loc, be % v3 )
   CALL DA_Allocate_Background_Errors( jy, kz, be4_eval_glo, be4_evec_glo, be4_eval_loc, &
                                       be4_evec_loc, be % v4 )

!--------------------------------------------------------------------------------------
     allocate ( be1_rf_lengthscale(1:nk) )
     allocate ( be2_rf_lengthscale(1:nk) )
     allocate ( be3_rf_lengthscale(1:nk) )
     allocate ( be4_rf_lengthscale(1:nk) )
     allocate ( be5_rf_lengthscale(1:nk) )

     read(be_cv_5_unit) variable
     read(be_cv_5_unit) be1_rf_lengthscale

     read(be_cv_5_unit) variable
     read(be_cv_5_unit) be2_rf_lengthscale

     read(be_cv_5_unit) variable
     read(be_cv_5_unit) be3_rf_lengthscale

     read(be_cv_5_unit) variable
     read(be_cv_5_unit) be4_rf_lengthscale

     read(be_cv_5_unit) variable
     read(be_cv_5_unit) be5_rf_lengthscale(1:1)
     be%v5%name = variable
     print*,'xb%ds ',xb%ds
     do k=1,nk
     write(*,'(I4,5F10.3 )' ) k, be1_rf_lengthscale(k), be2_rf_lengthscale(k), &
 be3_rf_lengthscale(k), be4_rf_lengthscale(k),be5_rf_lengthscale(1)
     enddo
     be1_rf_lengthscale(1:nk) = be1_rf_lengthscale(1:nk) * xb%ds
     be2_rf_lengthscale(1:nk) = be2_rf_lengthscale(1:nk) * xb%ds
     be3_rf_lengthscale(1:nk) = be3_rf_lengthscale(1:nk) * xb%ds
     be4_rf_lengthscale(1:nk) = be4_rf_lengthscale(1:nk) * xb%ds
     be5_rf_lengthscale(1:1)  = be5_rf_lengthscale(1:1)  * xb%ds

!--------------------------------------------------------------

! Perform checks on eigenvectors:

 IF ( test_statistics ) THEN

    CALL DA_Check_EOF_Decomposition( be%v1%val_g(:), be%v1%evec_g(:,:),&
                                     be%v1%name)
    CALL DA_Check_EOF_Decomposition( be%v2%val_g(:), be%v2%evec_g(:,:),&
                                     be%v2%name)
    CALL DA_Check_EOF_Decomposition( be%v3%val_g(:), be%v3%evec_g(:,:),&
                                     be%v3%name)
    CALL DA_Check_EOF_Decomposition( be%v4%val_g(:), be%v4%evec_g(:,:),&
                                     be%v4%name)
 END IF

   be % cv % size1  = ij * be % v1 % mz
   be % cv % size2  = ij * be % v2 % mz
   be % cv % size3  = ij * be % v3 % mz
   be % cv % size4  = ij * be % v4 % mz
   be % cv % size5  = ij * be % v5 % mz

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size

   close(be_cv_5_unit)
!------------------------------------------------------------------------------
!  [16.0] Apply empirical and recursive filter rescaling factor:
!------------------------------------------------------------------------------
   print*,'mz',be%v1%mz,be%v2%mz,be%v3%mz,be%v4%mz,be%v5%mz
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling1, len_scaling1, &
                                      xb % ds, be1_rf_lengthscale, be % v1 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling2, len_scaling2, &
                                      xb % ds, be2_rf_lengthscale, be % v2 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling3, len_scaling3, &
                                      xb % ds, be3_rf_lengthscale, be % v3 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling4, len_scaling4, &
                                      xb % ds, be4_rf_lengthscale, be % v4 )

      ALLOCATE( be%v5 % rf_alpha(1:be%v5%mz) )
      be%v5 % rf_alpha(1:be%v5%mz) = 1.0
      ALLOCATE( be%v5 % val(1:jy,1:be%v5%mz) )
      be%v5 % val(1:jy,1:be%v5%mz) = 0.0
    CALL DA_Rescale_Background_Errors( ix, jy, var_scaling5, len_scaling5, &
                                      xb % ds, be5_rf_lengthscale, be % v5 )
!------------------------------------------------------------------------------
!  [9.0] Deallocate input model state:
!------------------------------------------------------------------------------

   DEALLOCATE ( be1_eval_loc )
   DEALLOCATE ( be2_eval_loc )
   DEALLOCATE ( be3_eval_loc )
   DEALLOCATE ( be4_eval_loc )
   DEALLOCATE ( be5_eval_loc )

   IF ( vert_corr == 2 ) THEN
      DEALLOCATE ( be1_eval_glo )
      DEALLOCATE ( be2_eval_glo )
      DEALLOCATE ( be3_eval_glo )
      DEALLOCATE ( be4_eval_glo )
      DEALLOCATE ( be5_eval_glo )

      DEALLOCATE ( be1_evec_loc )
      DEALLOCATE ( be2_evec_loc )
      DEALLOCATE ( be3_evec_loc )
      DEALLOCATE ( be4_evec_loc )
      DEALLOCATE ( be5_evec_loc )

      DEALLOCATE ( be1_evec_glo )
      DEALLOCATE ( be2_evec_glo )
      DEALLOCATE ( be3_evec_glo )
      DEALLOCATE ( be4_evec_glo )
      DEALLOCATE ( be5_evec_glo )

   END IF

   deallocate( bin )
   deallocate( bin2d )

END SUBROUTINE DA_Setup_Background_Errors5
