SUBROUTINE DA_Setup_Background_Errors4(xb, be, xbx, xp)

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!
!  HISTORY: 20th November, 2004                   Syed RH Rizvi
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.
   TYPE (xbx_type),INTENT(IN)  :: xbx                   ! Header non-grid info 
   type (xpose_type), intent(in) :: xp   ! Dimensions and xpose buffers.

   INTEGER                     :: nrec, max_wavenumber  ! Scalar.
   INTEGER                     :: i, j, k, ic, jc       ! Loop counters.
   INTEGER                     :: ier                   ! error index
!  real, allocatable   :: height(:,:,:)      ! Height field.
   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point
   integer             :: bin_type           ! Type of bin to average over.
   integer             :: num_bins           ! Number of bins (3D fields).
   integer             :: num_bins2d         ! Number of bins (3D fields).
   logical  :: use_global_eofs               ! True if projected data uses global EOFs
!
! Addeded for new gen_be  Syed Rizvi
   integer             :: num_bins_hgt               ! Used if bin_type = 2. 
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). 
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). 
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
!------------------------------------------------------------------------------
!
   character*10                :: variable
   integer                     :: ni, nj, nk, b, nlev           
   real, allocatable           :: evec_g(:,:)
   real, allocatable           :: eval_g(:)  
   real, allocatable           :: evec_loc(:,:,:)
   real, allocatable           :: eval_loc(:,:)  
   real, allocatable           :: regcoeff1(:)
   real, allocatable           :: regcoeff2(:,:)
   real, allocatable           :: regcoeff3(:,:,:)

   write (6,'(A)') ' ----------------------------------------------------------'
   IF(cv_options /= 4) THEN
      write(unit=*, fmt='(a, i2)') &
           'cv_options:', cv_options

      STOP "Stopped in setting up errors due to wrong CV_OPTION."
   ENDIF
   be % cv % option = cv_options
   write (6,'(A,I3)') ' [3.0] Set up background errors (be) for cv_option:', cv_options
   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,*)
!---------------------------------------------------------------------
   rewind (be_cv_4_unit)
!---------------------------------------------------------
   read(be_cv_4_unit, end= 99, err = 100) ni, nj, nk 
   read(be_cv_4_unit, err = 100)bin_type
   read(be_cv_4_unit, err = 100)lat_min, lat_max, binwidth_lat
   read(be_cv_4_unit, err = 100)hgt_min, hgt_max, binwidth_hgt
!---------------------------------------------------------------

   read(be_cv_4_unit, err = 100) num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   read(be_cv_4_unit, err = 100)bin(1:ni,1:nj,1:nk)
   read(be_cv_4_unit, err = 100)bin2d(1:ni,1:nj)

!---------------------------------------------------------
   if( ni /= xbx%ni .or. nj /= xbx%nj .or. nk /= xbx%nk ) &
   stop ' Cannot generate BE at this resolution'

   allocate (regcoeff1(1:num_bins))
   allocate (regcoeff2(1:nk,1:num_bins2d))
   allocate (regcoeff3(1:nk,1:nk,1:num_bins2d))

   read(be_cv_4_unit,end= 99,  err = 100) regcoeff1  
   read(be_cv_4_unit,end= 99,  err = 100) regcoeff2 
   read(be_cv_4_unit,end= 99,  err = 100) regcoeff3  

   allocate (be%reg_chi(1:nj,1:nk))
   allocate (be%reg_ps (1:nj,1:nk))
   allocate (be%reg_t  (1:nj,1:nk,1:nk))
!--------------- not needed with new gen_be ------------------
!   allocate( bin(1:ni,1:nj,1:nk) )
!   allocate( bin2d(1:ni,1:nj) )
!   bin_type = 1
!   call da_create_bins( ni, nj, nk, bin_type, num_bins, &
!                        num_bins2d, bin, bin2d )
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
!-------------------------------------------------------------------
   do nrec = 1, 4
   read(be_cv_4_unit,end= 99,  err = 100) variable   
   read(be_cv_4_unit,end= 99,  err = 100) nk, num_bins2d 
!
   allocate ( evec_g(1:nk,1:nk) )     
   allocate ( eval_g(1:nk) )     
   allocate ( evec_loc(1:nk,1:nk,num_bins2d) )     
   allocate ( eval_loc(1:nk,num_bins2d) )     
!-------------------------------------------------------------
   read(be_cv_4_unit,end= 99,  err = 100)  evec_g     
   read(be_cv_4_unit,end= 99,  err = 100)  eval_g     
   read(be_cv_4_unit,end= 99,  err = 100)  evec_loc     
   read(be_cv_4_unit,end= 99,  err = 100)  eval_loc    
!
  if( nrec == 1) then
    be % v1 % name = variable               
    call da_get_bins_info(ni, nj, nk, num_bins2d, bin2d, evec_g, eval_g, &
    evec_loc, eval_loc, max_vert_var1, var_scaling1, be%v1)
!
  else if( nrec == 2) then
    be % v2 % name = variable               
    call da_get_bins_info(ni, nj, nk, num_bins2d, bin2d, evec_g, eval_g, &
    evec_loc, eval_loc, max_vert_var2, var_scaling2, be%v2)
!
  else if( nrec == 3) then
    be % v3 % name = variable               
    call da_get_bins_info(ni, nj, nk, num_bins2d, bin2d, evec_g, eval_g, &
    evec_loc, eval_loc, max_vert_var3, var_scaling3, be%v3)
!
  else if( nrec == 4) then
    be % v4 % name = variable               
    call da_get_bins_info(ni, nj, nk, num_bins2d, bin2d, evec_g, eval_g, &
    evec_loc, eval_loc, max_vert_var4, var_scaling4, be%v4)
!
  end if 
!
   deallocate ( evec_g )     
   deallocate ( eval_g )     
   deallocate ( evec_loc )     
   deallocate ( eval_loc )     

   enddo ! loop nrec

   deallocate( bin )
   deallocate( bin2d )

!  Read Power spectrum
  
  do k = 1, nk
   read(be_cv_4_unit,end= 99,  err = 100) variable
   read(be_cv_4_unit,end= 99,  err = 100) max_wavenumber, nrec
   read(be_cv_4_unit,end= 99,  err = 100) use_global_eofs    
   if ( k == 1 ) allocate(be%v1%power(0:max_wavenumber,1:nk)) 
   read(be_cv_4_unit,end= 99,  err = 100)be%v1%power(0:max_wavenumber,k) 
   be%v1%power(0,k) =  len_scaling1 * be%v1%power(0,k) 
  end do
  do k = 1, nk
   read(be_cv_4_unit,end= 99,  err = 100) variable
   read(be_cv_4_unit,end= 99,  err = 100) max_wavenumber, nrec
   read(be_cv_4_unit,end= 99,  err = 100) use_global_eofs    
   if ( k == 1 ) allocate(be%v2%power(0:max_wavenumber,1:nk)) 
   read(be_cv_4_unit,end= 99,  err = 100)be%v2%power(0:max_wavenumber,k) 
   be%v2%power(0,k) =  len_scaling2 * be%v2%power(0,k) 
  end do
  
  do k = 1, nk
   read(be_cv_4_unit,end= 99,  err = 100) variable
   read(be_cv_4_unit,end= 99,  err = 100) max_wavenumber, nrec
   read(be_cv_4_unit,end= 99,  err = 100) use_global_eofs    
   if ( k == 1 ) allocate(be%v3%power(0:max_wavenumber,1:nk)) 
   read(be_cv_4_unit,end= 99,  err = 100)be%v3%power(0:max_wavenumber,k) 
   be%v3%power(0,k) =  len_scaling3 * be%v3%power(0,k) 
  end do

  do k = 1, nk
   read(be_cv_4_unit,end= 99,  err = 100) variable
   read(be_cv_4_unit,end= 99,  err = 100) max_wavenumber, nrec
   read(be_cv_4_unit,end= 99,  err = 100) use_global_eofs    
   if ( k == 1 ) allocate(be%v4%power(0:max_wavenumber,1:nk)) 
   read(be_cv_4_unit,end= 99,  err = 100)be%v4%power(0:max_wavenumber,k) 
   be%v4%power(0,k) =  len_scaling4 * be%v4%power(0,k) 
  end do

   read(be_cv_4_unit,end= 99,  err = 100) variable
   be % v5 % name = variable          ! surface pressure
   be % v5 % mz = 1
   if(max_vert_var5 <=  0.0 ) be % v5 % mz = 0                         
   read(be_cv_4_unit,end= 99,  err = 100) max_wavenumber, nrec
   read(be_cv_4_unit,end= 99,  err = 100) use_global_eofs    
   allocate(be%v5%power(0:max_wavenumber,1:1)) 
   read(be_cv_4_unit,end= 99,  err = 100)be%v5%power(0:max_wavenumber,1) 
   be%v5%power(0,1) =  len_scaling5 * be%v5%power(0,1) 
!
   write(6,'(3x,A)')' DA_Setup_Background_Errors: 3DVAR dry control variables are:'
   write(6,'(4x,7A)')TRIM(be % v1 % name), ', ', TRIM(be % v2 % name), ', ', &
                  TRIM(be % v3 % name), ' and ', TRIM(be % v5 % name)

   write(6,'(3x,A,A)')' DA_Setup_Background_Errors: 3DVAR humidity control variable is ',&
                     TRIM(be % v4 % name)

   write(6,*)
!--------------------------------------------------------------
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
    write(6,*)
   kz_vv(1) = be % v1 % mz
   kz_vv(2) = be % v2 % mz
   kz_vv(3) = be % v3 % mz
   kz_vv(4) = be % v4 % mz
   kz_vv(5) = be % v5 % mz
   kz_vp    = xb % mkz

!--------------------------------------------------------------

   be % cv % size1  = 2* xbx%c_cv_size * be % v1 % mz   
   be % cv % size2  = 2* xbx%c_cv_size * be % v2 % mz   
   be % cv % size3  = 2* xbx%c_cv_size * be % v3 % mz   
   be % cv % size4  = 2* xbx%c_cv_size * be % v4 % mz   
   be % cv % size5  = 2* xbx%c_cv_size * be % v5 % mz   

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size
   close(be_cv_4_unit)
   return
99  write(6,'(a, i5)')' Unexpected end on BE-unit = ',be_cv_4_unit
100 write(6,'(a, i5)')' Read error on BE-unit = ',be_cv_4_unit
   stop     
END SUBROUTINE DA_Setup_Background_Errors4
