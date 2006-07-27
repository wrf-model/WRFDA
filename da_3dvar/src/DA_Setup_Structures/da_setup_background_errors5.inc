
SUBROUTINE DA_Setup_Background_Errors5(xb, be, xbx, xp)

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!  Updates:
!
!      Bug fix for reading BE            Syed RH Rizvi 07/26/2006 
!      compatible with gen_be output
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
   real    :: lat_min, lat_max, binwidth_lat ! Used if bin_type = 2 (degrees)..
   real    :: hgt_min, hgt_max, binwidth_hgt ! Used if bin_type = 2 (m). .
   integer :: num_bins_lat, num_bins_hgt     ! Used if bin_type = 2.

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
   integer                     :: ni, nj, nk, nk_2d, b, nlev           
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
!!!!!!!!! cv_options=3
      be % v1 % name = 'psi  '           ! Streamfunction
      be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
      be % v3 % name = 't_u'             ! Unbalanced temperature.
      be % v4 % name = 'q/qsg'
      be % v5 % name = 'psfc'            ! surface pressure

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

     allocate( bin(1:ni,1:nj,1:nk) )
     allocate( bin2d(1:ni,1:nj) )

     read(be_cv_5_unit)bin_type
     read(be_cv_5_unit)lat_min, lat_max, binwidth_lat
     read(be_cv_5_unit)hgt_min, hgt_max, binwidth_hgt
     read(be_cv_5_unit)num_bins, num_bins2d
     read(be_cv_5_unit)bin(1:ni,1:nj,1:nk)
     read(be_cv_5_unit)bin2d(1:ni,1:nj)

!   read(be_cv_5_unit) num_bins, num_bins2d
!---------------------------------------------------------

! 1.0 Load the regression coefficients
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! 1.1 Read in regression coefficients

   allocate (regcoeff1(1:num_bins))
   allocate (regcoeff2(1:nk,1:num_bins2d))
   allocate (regcoeff3(1:nk,1:nk,1:num_bins2d))

   read(be_cv_5_unit) regcoeff1  
   read(be_cv_5_unit) regcoeff2 
   read(be_cv_5_unit) regcoeff3  

! 1.2 Re-assign the regression coefficients

! 1.2.1 Create the bins

 !  allocate( bin(1:ni,1:nj,1:nk) )
 !  allocate( bin2d(1:ni,1:nj) )

 !  bin_type = 1
 !  print '(/a,i4)', "bin_type=",bin_type
 !  call da_create_bins( ni, nj, nk, bin_type, num_bins, &
 !                               num_bins2d, bin, bin2d )

   print '("ni, nj, nk, num_bins, num_bins2d:",5i8)', ni, nj, nk, num_bins, num_bins2d
!--------------------------------------------------------------
! Following "latitude, height" variables are optional arguments 
! The same may be added, if required
!--------------------------------------------------------------

! 1.2.2 Fill regression coeff. array

   allocate (be%reg_chi(1:nj,1:nk))
   allocate (be%reg_ps (1:nj,1:nk))
   allocate (be%reg_t  (1:nj,1:nk,1:nk))

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

! 1.3 Domain_averaged regression coefficients

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
    
    print '(2x,a,i8,a,i4)',"a) Finished regression coefficients processing! jy=",jy," kz=",kz

! 2.0 Load the eigenvector and eigenvalue
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! 2.1 Allocate the arrays

   ALLOCATE ( be1_eval_loc (1:kz,1:num_bins2d) )
   ALLOCATE ( be2_eval_loc (1:kz,1:num_bins2d) )
   ALLOCATE ( be3_eval_loc (1:kz,1:num_bins2d) )
   ALLOCATE ( be4_eval_loc (1:kz,1:num_bins2d) )
   ALLOCATE ( be5_eval_loc (1:1,1:num_bins2d) )

   IF ( vert_corr == 2 ) THEN

      ALLOCATE ( be1_eval_glo(1:kz) )
      ALLOCATE ( be2_eval_glo(1:kz) )
      ALLOCATE ( be3_eval_glo(1:kz) )
      ALLOCATE ( be4_eval_glo(1:kz) )
      ALLOCATE ( be5_eval_glo(1:1) )

      ALLOCATE ( be1_evec_loc(1:kz,1:kz,1:num_bins2d) )
      ALLOCATE ( be2_evec_loc(1:kz,1:kz,1:num_bins2d) )
      ALLOCATE ( be3_evec_loc(1:kz,1:kz,1:num_bins2d) )
      ALLOCATE ( be4_evec_loc(1:kz,1:kz,1:num_bins2d) )
      ALLOCATE ( be5_evec_loc(1:1,1:1,1:num_bins2d) )

      ALLOCATE ( be1_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be2_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be3_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be4_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be5_evec_glo(1:1,1:1) )
   END IF

! 2.2 Read in the eigenvector and eigenvalue 

! 2.2.1 Control variable 1 (psi)

   read(be_cv_5_unit) variable   
   read(be_cv_5_unit) nk, num_bins2d 
   read(be_cv_5_unit)  be1_evec_glo   
   read(be_cv_5_unit)  be1_eval_glo    
   read(be_cv_5_unit)  be1_evec_loc     
   read(be_cv_5_unit)  be1_eval_loc    

   be % v1 % name = variable
!   print*,'variable ',be % v1 % name,nk,num_bins2d
!
! 2.2.2 Control variable 2 (chi_u)

   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   read(be_cv_5_unit)  be2_evec_glo
   read(be_cv_5_unit)  be2_eval_glo
   read(be_cv_5_unit)  be2_evec_loc
   read(be_cv_5_unit)  be2_eval_loc

   be % v2 % name = variable
!   print*,'variable ',be % v2 % name,nk,num_bins2d

! 2.2.3 Control variable 3 (t_u)

   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   read(be_cv_5_unit)  be3_evec_glo
   read(be_cv_5_unit)  be3_eval_glo
   read(be_cv_5_unit)  be3_evec_loc
   read(be_cv_5_unit)  be3_eval_loc

   be % v3 % name = variable
!   print*,'variable ',be % v3 % name,nk,num_bins2d

! 2.2.4 Control variable 4 (q/qsg)

   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk, num_bins2d
   read(be_cv_5_unit)  be4_evec_glo
   read(be_cv_5_unit)  be4_eval_glo
   read(be_cv_5_unit)  be4_evec_loc
   read(be_cv_5_unit)  be4_eval_loc
!
   be % v4 % name = variable
!   print*,'variable ',be % v4 % name,nk,num_bins2d

   read(be_cv_5_unit) variable
   read(be_cv_5_unit) nk_2d, num_bins2d
   read(be_cv_5_unit)  be5_evec_glo
   read(be_cv_5_unit)  be5_eval_glo
   read(be_cv_5_unit)  be5_evec_loc
   read(be_cv_5_unit)  be5_eval_loc

   be % v5 % name = variable
!   print*,'variable ',be % v5 % name,nk_2d,num_bins2d

! 3.0 Check and get the truncated number of the vertical modes
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   IF ( vert_corr == 2 ) THEN

! 3.1 Perform checks on eigenvectors:

      IF ( test_statistics ) THEN
         CALL DA_Check_EOF_Decomposition( be1_eval_glo(:), be1_evec_glo(:,:), be % v1 % name)
         CALL DA_Check_EOF_Decomposition( be2_eval_glo(:), be2_evec_glo(:,:), be % v2 % name)
         CALL DA_Check_EOF_Decomposition( be3_eval_glo(:), be3_evec_glo(:,:), be % v3 % name)
         CALL DA_Check_EOF_Decomposition( be4_eval_glo(:), be4_evec_glo(:,:), be % v4 % name)
      END IF

! 3.2 Truncate in vertical:

      CALL DA_Get_Vertical_Truncation( max_vert_var1, be1_eval_glo(:), be % v1 )
      CALL DA_Get_Vertical_Truncation( max_vert_var2, be2_eval_glo(:), be % v2 )
      CALL DA_Get_Vertical_Truncation( max_vert_var3, be3_eval_glo(:), be % v3 )
      CALL DA_Get_Vertical_Truncation( max_vert_var4, be4_eval_glo(:), be % v4 )

      be % v5 % mz = 1

      write(6,*)

   ELSE

! 3.3 no truncated

      be % v1 % mz = xb % mkz
      be % v2 % mz = xb % mkz
      be % v3 % mz = xb % mkz
      be % v4 % mz = xb % mkz
      be % v5 % mz = xb % mkz

   END IF

! 4.0 Initialise control variable space components of header:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!This is for DA_Test

   kz_vv(1) = be % v1 % mz
   kz_vv(2) = be % v2 % mz
   kz_vv(3) = be % v3 % mz
   kz_vv(4) = be % v4 % mz
   kz_vv(5) = be % v5 % mz
   kz_vp    = xb % mkz
!------

! 4.1 Compute the size of the control variables

   be % mix = ix
   be % mjy = jy

   ij = ix * jy

   be % cv % size1  = ij * be % v1 % mz    ! Fix size, read from file later.
   be % cv % size2  = ij * be % v2 % mz    ! Fix size, read from file later.
   be % cv % size3  = ij * be % v3 % mz    ! Fix size, read from file later.
   be % cv % size4  = ij * be % v4 % mz    ! Fix size, read from file later.
   be % cv % size5  = ij * be % v5 % mz    ! Fix size, read from file later.

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size

! 4.2 Transfer errors to error structure:

   CALL DA_Allocate_Background_Errors( jy, kz, be1_eval_glo, be1_evec_glo, be1_eval_loc, &
                                       be1_evec_loc, be % v1 )
   CALL DA_Allocate_Background_Errors( jy, kz, be2_eval_glo, be2_evec_glo, be2_eval_loc, &
                                       be2_evec_loc, be % v2 )
   CALL DA_Allocate_Background_Errors( jy, kz, be3_eval_glo, be3_evec_glo, be3_eval_loc, &
                                       be3_evec_loc, be % v3 )
   CALL DA_Allocate_Background_Errors( jy, kz, be4_eval_glo, be4_evec_glo, be4_eval_loc, &
                                       be4_evec_loc, be % v4 )

! 4.2.1 Transfer the ps_u variance to be % v5:

   CALL DA_Allocate_Background_Errors( jy,  1, be5_eval_glo, be5_evec_glo, be5_eval_loc, &
                                       be5_evec_loc, be % v5 )

   print '(2x,a,i10)',"b) Finished eigenvector processing! ij=", ij
   
! 5.0 Load the scale lengths
! ~~~~~~~~~~~~~~~~~~~~~~~~~~

! 5.1 Allocate the array for scale lengths

     allocate ( be1_rf_lengthscale(1:nk) )
     allocate ( be2_rf_lengthscale(1:nk) )
     allocate ( be3_rf_lengthscale(1:nk) )
     allocate ( be4_rf_lengthscale(1:nk) )
     allocate ( be5_rf_lengthscale(1:nk) )

! 5.2 read in the scale lengths

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

! 5.3 Convert the scale lengths in the real distance (meter)

     be1_rf_lengthscale(1:nk) = be1_rf_lengthscale(1:nk) * xb%ds
     be2_rf_lengthscale(1:nk) = be2_rf_lengthscale(1:nk) * xb%ds
     be3_rf_lengthscale(1:nk) = be3_rf_lengthscale(1:nk) * xb%ds
     be4_rf_lengthscale(1:nk) = be4_rf_lengthscale(1:nk) * xb%ds
     be5_rf_lengthscale(1:1)  = be5_rf_lengthscale(1:1)  * xb%ds

!--------------------------------------------------------------

! 6.0 Perform checks on eigenvectors with be data structure:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

! 6.1 Re-assign the control variable size

   be % cv % size1  = ij * be % v1 % mz
   be % cv % size2  = ij * be % v2 % mz
   be % cv % size3  = ij * be % v3 % mz
   be % cv % size4  = ij * be % v4 % mz
   be % cv % size5  = ij * be % v5 % mz

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size

! 6.2 Close the be unit

   close(be_cv_5_unit)

! 7.0 Apply empirical and recursive filter rescaling factor:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling1, len_scaling1, &
                                      xb % ds, be1_rf_lengthscale, be % v1 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling2, len_scaling2, &
                                      xb % ds, be2_rf_lengthscale, be % v2 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling3, len_scaling3, &
                                      xb % ds, be3_rf_lengthscale, be % v3 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling4, len_scaling4, &
                                      xb % ds, be4_rf_lengthscale, be % v4 )

    

    CALL DA_Rescale_Background_Errors( ix, jy, var_scaling5, len_scaling5, &
                                      xb % ds, be5_rf_lengthscale, be % v5 )

!     print '(/a,f10.2)','xb%ds=',xb%ds
!     do k=1,nk
!       write(*,'(I4,5F10.3 )' ) k, be1_rf_lengthscale(k), be2_rf_lengthscale(k), &
!             be3_rf_lengthscale(k), be4_rf_lengthscale(k),be5_rf_lengthscale(1)
!     enddo

    print '(2x,a)',"c) Finished the scale length processing!"

    print '(a,i10/5(a,i10))',   &
      "Size of control variable =",  cv_size, &
      " and the size for each of control vaiables: cv1=", be % cv % size1, &
                        " cv2=",be % cv % size2, " cv3=", be % cv % size3, &
                        " cv4=",be % cv % size4, " cv5=", be % cv % size5

! 8.0 Deallocate input model state:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
