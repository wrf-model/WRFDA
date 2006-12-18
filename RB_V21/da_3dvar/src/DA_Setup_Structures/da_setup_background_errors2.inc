SUBROUTINE DA_Setup_Background_Errors2( xb, xbx, be )

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!
!  METHOD:  Allocate components in turn.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           09/18/2003 - Modified for WRF coordinator.      Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (xbx_type),INTENT(IN)  :: xbx                   ! Header & non-gridded vars.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.

   INTEGER                     :: ix, jy, kz            ! xb array dimen.
   INTEGER                     :: iys, jxs, kzs         ! be array dimen.
   INTEGER                     :: ij                    ! Scalar.
   INTEGER                     :: j, k, k1, k2          ! Loop counters.
   INTEGER                     :: ier                   ! error index
   real                        :: ptop_stats            ! Ptop of stats file.
   real                        :: psac_stats            ! Mean pstar of stats file.
   real                        :: res_ratio             ! Stats/xb resolution.
   CHARACTER*32                :: be_file

   real, allocatable           :: meanp_stats(:)
   real, allocatable           :: meanl_stats(:)
   real, allocatable           :: meanp_xb(:)

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
   
   real, allocatable           :: e1(:,:), e2(:,:), e3(:,:), e4(:,:), e5(:,:)
   real, allocatable           :: l1(:), l2(:), l3(:), l4(:), l5(:)
   real, allocatable           :: s1(:), s2(:), s3(:), s4(:), s5(:)
   
   REAL, ALLOCATABLE           :: be1_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be2_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be3_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be4_rf_lengthscale(:) ! RF lengthscale.
   REAL, ALLOCATABLE           :: be5_rf_lengthscale(:) ! RF lengthscale.

   REAL, ALLOCATABLE           :: pb_vert_reg(:,:,:)    ! Vertical regression.
   REAL                        :: y_mean

   type (mm5_model_type) :: x_mm5

   integer(kind=4)   :: flag
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   
   write (6,'(A)') ' -----------------------------------'
   write (6,'(A)') ' [3.0] Set up background errors (be)'
   write (6,'(A)') ' -----------------------------------'
   write (6,*)
   
   be % cv % option = cv_options

!------------------------------------------------------------------------------
!  [1.0] Set control variables:
!------------------------------------------------------------------------------

   IF ( cv_options == 2 ) THEN
      be % v1 % name = 'psi  '           ! Streamfunction
      be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
      be % v3 % name = 'phi_u'           ! Unbalanced mass variable (p/M).
      be % v5 % name = 'tgrn '           ! Ground temperature
   ELSE
      write(unit=*, fmt='(a, i2)') &
           'cv_options:', cv_options

      CALL wrf_shutdown

      STOP "Stopped for Wrong CV_OPTION."
   END IF
   
   IF ( cv_options_hum == 1 ) THEN
      be % v4 % name = 'q    '
   ELSE IF ( cv_options_hum == 2 ) THEN
      be % v4 % name = 'rh   '
   ELSE IF ( cv_options_hum == 3 ) THEN
      be % v4 % name = 'qt   '
   END IF

   write(6,'(3x,A)')' DA_Setup_Background_Errors: 3DVAR dry control variables are:'
   write(6,'(4x,7A)')TRIM(be % v1 % name), ', ', TRIM(be % v2 % name), ', ', &
                  TRIM(be % v3 % name), ' and ', TRIM(be % v5 % name)
                          
   write(6,'(3x,A,A)')' DA_Setup_Background_Errors: 3DVAR humidity control variable is ',&
                     TRIM(be % v4 % name)
                     
   write(6,*)
                     
!------------------------------------------------------------------------------
!  [2.0] Read input file:
!------------------------------------------------------------------------------

   IF ( wrf_dm_on_monitor() ) THEN
      rewind(be_cv_2_unit)

      READ(be_cv_2_unit, iostat=ier) flag

      if(ier /= 0) then
         write(0, fmt='(a, i4, a)') &
              'Error in read from unit:', be_cv_2_unit, &
              'Stopped here.'
         call abort()
      end if

      READ(be_cv_2_unit, iostat=ier) x_mm5 % big_header % bhi, &
                                     x_mm5 % big_header % bhr,&
                                     x_mm5 % big_header % bhic, &
                                     x_mm5 % big_header % bhrc
   
      REWIND(be_cv_2_unit)

      CALL DA_Allocate_MM5_Model( x_mm5 )

      CALL DA_Read_MM5 ( be_cv_2_unit, x_mm5, ier)
   ENDIF

   call mm5_struct_bcast( x_mm5 )

!------------------------------------------------------------------------------
!  [3.0] Calculate variables required if interpolating stats:
!------------------------------------------------------------------------------

   iys = x_mm5 % big_header % bhi(16,1)
   jxs = x_mm5 % big_header % bhi(17,1)
   kzs = x_mm5 % big_header % bhi(12,5)
   
   ix = xb % mix
   jy = xb % mjy
   kz = xb % mkz
   
   if ( interpolate_stats ) then
   
      allocate( meanp_stats(1:kzs) )
      allocate( meanp_xb(1:kz) )
      allocate( meanl_stats(1:iys) )

!     Calculate mean REFERENCE pressure on stats and xb levels:

      ptop_stats = x_mm5 % big_header % bhr(2,2)
      psac_stats = SUM( x_mm5 % psac(1:iys-1,1:jxs-1) ) / REAL((iys-1)*(jxs-1))

      do k = 1, kzs
         meanp_stats(k) = x_mm5 % sigmah(k) * psac_stats + ptop_stats
      end do

      do k = 1, kz
         meanp_xb(k) = xb % znu(k) * xbx % psac_mean + xb % ptop
      end do
      
!     Calculate mean latitude on stats and xb rows:
      do ij = 1, iys-1
         meanl_stats(ij) = SUM( x_mm5 % latc(ij,1:jxs-1) ) / REAL((jxs-1))
      end do

      meanl_stats(iys) = meanl_stats(iys-1)

   end if
   
!------------------------------------------------------------------------------
!  [4.0] Allocate temporary arrays:
!------------------------------------------------------------------------------

   ALLOCATE ( be1_eval_loc (1:iys,1:kzs) )
   ALLOCATE ( be2_eval_loc (1:iys,1:kzs) )
   ALLOCATE ( be3_eval_loc (1:iys,1:kzs) )
   ALLOCATE ( be4_eval_loc (1:iys,1:kzs) )
   ALLOCATE ( be5_eval_loc (1:iys,1:kzs) )
   
   ALLOCATE( be1_rf_lengthscale(1:kzs) )
   ALLOCATE( be2_rf_lengthscale(1:kzs) )
   ALLOCATE( be3_rf_lengthscale(1:kzs) )
   ALLOCATE( be4_rf_lengthscale(1:kzs) )
   ALLOCATE( be5_rf_lengthscale(1:kzs) )

   IF ( vert_corr == 2 ) THEN   

      ALLOCATE ( be1_eval_glo(1:kzs) )
      ALLOCATE ( be2_eval_glo(1:kzs) )
      ALLOCATE ( be3_eval_glo(1:kzs) )
      ALLOCATE ( be4_eval_glo(1:kzs) )
      ALLOCATE ( be5_eval_glo(1:kzs) )

      ALLOCATE ( be1_evec_loc(1:iys,1:kzs,1:kzs) )
      ALLOCATE ( be2_evec_loc(1:iys,1:kzs,1:kzs) )
      ALLOCATE ( be3_evec_loc(1:iys,1:kzs,1:kzs) )
      ALLOCATE ( be4_evec_loc(1:iys,1:kzs,1:kzs) )
      ALLOCATE ( be5_evec_loc(1:iys,1:kzs,1:kzs) )

      ALLOCATE ( be1_evec_glo(1:kzs,1:kzs) )
      ALLOCATE ( be2_evec_glo(1:kzs,1:kzs) )
      ALLOCATE ( be3_evec_glo(1:kzs,1:kzs) )
      ALLOCATE ( be4_evec_glo(1:kzs,1:kzs) )
      ALLOCATE ( be5_evec_glo(1:kzs,1:kzs) )
      ALLOCATE ( pb_vert_reg(1:iys,1:kzs,1:kzs) )

   END IF
   
!------------------------------------------------------------------------------
!  [5.0] Error-check namelist variables with error file header:
!------------------------------------------------------------------------------
   
   IF ( ( iys /= jy+1 .or. iys /= jy+1 .or. kzs /= kz ) .and. &
          .not. interpolate_stats ) then
         write(0,'(/,A,A,/)')' DA_Setup_Background_Errors: ', &
                             ' Inconsistent grid dimensions with interpolate_stats=.f.'
         STOP
   END IF

   IF ( interpolate_stats .and. vert_evalue /= 1 ) then
         write(0,'(/,A,A,/)')' DA_Setup_Background_Errors: ', &
                             ' Can only use interpolate_stats=.t. with vert_evalue = 1'
         STOP
   END IF

   IF ( x_mm5 % big_header % bhi(26,10) /= vert_corr ) THEN
      write(0,'(A)') ' DA_Setup_Background_Errors: Inconsistent vert_corr'
      write(0,'(A,I6)')' 3DVAR namelist value = ', vert_corr
      write(0,'(A,I6)')' BackError file value = ', x_mm5 % big_header % bhi(26,10)
      STOP
   END IF

   IF ( x_mm5 % big_header % bhi(27,10) /= cv_options ) THEN
      write(0,'(A,A)') ' DA_Setup_Background_Errors: Inconsistent cv_options'
      write(0,'(A,I6)')' 3DVAR namelist value = ', cv_options
      write(0,'(A,I6)')' BackError file value = ', x_mm5 % big_header % bhi(27,10)
      STOP
   END IF
   
   IF ( x_mm5 % big_header % bhi(28,10) /= balance_type .AND. cv_options == 2 ) THEN
      write(0,'(A,A)') ' DA_Setup_Background_Errors: Inconsistent balance_type'
      write(0,'(A,I6)')' 3DVAR namelist value = ', balance_type
      write(0,'(A,I6)')' BackError file value = ', x_mm5 % big_header % bhi(28,10)
      STOP
   END IF

   IF ( x_mm5 % big_header % bhi(30,10) /= 1 ) THEN
      write(0,'(A,A)') ' DA_Setup_Background_Errors:', &
                       ' Error file does not contain RF lengthscales.'
!     STOP

      write(unit=*, fmt='(a)') &
           ' WARNING  WARNING WARNING', &
           ' DA_Setup_Background_Errors:', &
           ' Error file does not contain RF lengthscales.'
   END IF
   
   IF ( x_mm5 % big_header % bhi(31,10) /= vertical_ip .AND. vert_corr == 2 ) THEN
      write(0,'(A,A)') ' DA_Setup_Background_Errors: Inconsistent vertical_ip'
      write(0,'(A,I6)')' 3DVAR namelist value = ', vertical_ip
      write(0,'(A,I6)')' BackError file value = ', x_mm5 % big_header % bhi(31,10)
      STOP
   END IF
   
!------------------------------------------------------------------------------
!  [6.0] Read in grid-point standard deviation fields:
!------------------------------------------------------------------------------

   be1_eval_loc(:,:) = x_mm5 % psi_eval_loc(:,:)
   be2_eval_loc(:,:) = x_mm5 % chi_eval_loc(:,:)
   be3_eval_loc(:,:) = x_mm5 % blnc_p_eval_loc(:,:)
! be5 will be changed to tgrn later.
   be5_eval_loc(:,:) = x_mm5 % t_eval_loc(:,:)

   IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
      be4_eval_loc(:,:) = x_mm5 % q_eval_loc(:,:)
   ELSE IF ( cv_options_hum == 2 ) THEN
      be4_eval_loc(:,:) = x_mm5 % rh_eval_loc(:,:)
   END IF

!------------------------------------------------------------------------------
!  [7.0] Read in recursive filter lengthscales:
!------------------------------------------------------------------------------

!  Convert non-dimensional lengthscale to m:

   IF ( x_mm5 % big_header % bhi(30,10) /= 1 ) THEN
      be1_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*2.0
      be2_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*2.0
      be3_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*2.0
      be4_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*2.0
      be5_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*2.0
   ELSE
      be1_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*x_mm5%psi_sl(1:kzs)
      be2_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*x_mm5%chi_sl(1:kzs)
      be3_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*x_mm5%blnc_p_sl(1:kzs)
      IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
         be4_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1) * x_mm5%q_sl(1:kzs)
      ELSE IF ( cv_options_hum == 2 ) THEN
         be4_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*x_mm5%rh_sl(1:kzs)
      END IF      
      be5_rf_lengthscale(1:kzs) = x_mm5%big_header%bhr(9,1)*x_mm5%t_sl(1:kzs)
   ENDIF

   do k=1,kzs
      if(be1_rf_lengthscale(k) < 0.1) be1_rf_lengthscale(k) = 0.1
      if(be2_rf_lengthscale(k) < 0.1) be2_rf_lengthscale(k) = 0.1
      if(be3_rf_lengthscale(k) < 0.1) be3_rf_lengthscale(k) = 0.1
      if(be4_rf_lengthscale(k) < 0.1) be4_rf_lengthscale(k) = 0.1
      if(be5_rf_lengthscale(k) < 0.1) be5_rf_lengthscale(k) = 0.1
   enddo

!------------------------------------------------------------------------------
!  [8.0] Read in vertical eigenvectors and eigenvalues if vert_corr=2:
!------------------------------------------------------------------------------

   IF ( vert_corr == 2 ) THEN

      be1_eval_glo(:) = x_mm5 % psi_eval_glo(:)
      be2_eval_glo(:) = x_mm5 % chi_eval_glo(:)
      be3_eval_glo(:) = x_mm5 % blnc_p_eval_glo(:)
      be5_eval_glo(:) = x_mm5 % t_eval_glo(:)

      be1_evec_glo(:,:) = x_mm5 % psi_evec_glo(:,:)
      be2_evec_glo(:,:) = x_mm5 % chi_evec_glo(:,:)
      be3_evec_glo(:,:) = x_mm5 % blnc_p_evec_glo(:,:)
      be5_evec_glo(:,:) = x_mm5 % t_evec_glo(:,:)

      be1_evec_loc(:,:,:) = x_mm5 % psi_evec_loc(:,:,:)
      be2_evec_loc(:,:,:) = x_mm5 % chi_evec_loc(:,:,:)
      be3_evec_loc(:,:,:) = x_mm5 % blnc_p_evec_loc(:,:,:)
      be5_evec_loc(:,:,:) = x_mm5 % t_evec_loc(:,:,:)
         
      IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
         be4_eval_glo(:) = x_mm5 % q_eval_glo(:)
         be4_evec_glo(:,:) = x_mm5 % q_evec_glo(:,:)
         be4_evec_loc(:,:,:) = x_mm5 % q_evec_loc(:,:,:)
      ELSE IF ( cv_options_hum == 2 ) THEN
         be4_eval_glo(:) = x_mm5 % rh_eval_glo(:)
         be4_evec_glo(:,:) = x_mm5 % rh_evec_glo(:,:)
         be4_evec_loc(:,:,:) = x_mm5 % rh_evec_loc(:,:,:)
      END IF

      pb_vert_reg(:,:,:) = x_mm5 % pb_vert_reg(:,:,:)
      pb_vert_reg(1,:,:) = x_mm5 % pb_vert_reg(2,:,:)
         
   END IF
   
!------------------------------------------------------------------------------
!  [10.0] Impose minimum standard deviation (only for be % cv % option == 1):
!------------------------------------------------------------------------------

   IF ( be % cv % option == 1 ) THEN
      be1_eval_loc(:,:) = MAX( pp_umin, ABS(be1_eval_loc(:,:)) )
      be2_eval_loc(:,:) = MAX( pp_vmin, ABS(be2_eval_loc(:,:)) )
      be3_eval_loc(:,:) = MAX( pp_tmin, ABS(be3_eval_loc(:,:)) )
      be4_eval_loc(:,:) = MAX( pp_qmin, ABS(be4_eval_loc(:,:)) )
      be5_eval_loc(:,:) = MAX( pp_pmin, ABS(be5_eval_loc(:,:)) )
   END IF
   
!------------------------------------------------------------------------------
!  [11.0] Interpolate statistics to current grid (if different)
!------------------------------------------------------------------------------

   kz = xb % mkz
   allocate( e1(1:kz,1:kz) )
   allocate( e2(1:kz,1:kz) )
   allocate( e3(1:kz,1:kz) )
   allocate( e4(1:kz,1:kz) )
   allocate( e5(1:kz,1:kz) )
   allocate( l1(1:kz) )
   allocate( l2(1:kz) )
   allocate( l3(1:kz) )
   allocate( l4(1:kz) )
   allocate( l5(1:kz) )   
   allocate( s1(1:kz) )
   allocate( s2(1:kz) )
   allocate( s3(1:kz) )
   allocate( s4(1:kz) )
   allocate( s5(1:kz) )
   
   if ( interpolate_stats ) then

!     Interpolate statistics to new domain:
      call da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be1_evec_glo, be1_eval_glo, &
                                 be1_rf_lengthscale, e1, l1, s1 )
      call da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be2_evec_glo, be2_eval_glo, &
                                 be2_rf_lengthscale, e2, l2, s2 )
      call da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be3_evec_glo, be3_eval_glo, &
                                 be3_rf_lengthscale, e3, l3, s3 )
      call da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be4_evec_glo, be4_eval_glo, &
                                 be4_rf_lengthscale, e4, l4, s4 )
      call da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be5_evec_glo, be5_eval_glo, &
                                 be5_rf_lengthscale, e5, l5, s5 )

   else
      do k2=1, kz
         do k1=1, kz
            e1(k2,k1) = be1_evec_glo(kz+1-k2,k1)
            e2(k2,k1) = be2_evec_glo(kz+1-k2,k1)
            e3(k2,k1) = be3_evec_glo(kz+1-k2,k1)
            e4(k2,k1) = be4_evec_glo(kz+1-k2,k1)
            e5(k2,k1) = be5_evec_glo(kz+1-k2,k1)
         enddo
      
         l1(k2) = be1_eval_glo(k2)
         l2(k2) = be2_eval_glo(k2)
         l3(k2) = be3_eval_glo(k2)
         l4(k2) = be4_eval_glo(k2)
         l5(k2) = be5_eval_glo(k2)
      
         s1(k2) = be1_rf_lengthscale(k2)
         s2(k2) = be2_rf_lengthscale(k2)
         s3(k2) = be3_rf_lengthscale(k2)
         s4(k2) = be4_rf_lengthscale(k2)
         s5(k2) = be5_rf_lengthscale(k2)
      enddo
   end if

   if(print_detail > 0) then
   write(unit=*, fmt='(//a)') 'Input Scale length'

   do k=1,kzs
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           be1_rf_lengthscale(k), &
           be2_rf_lengthscale(k), &
           be3_rf_lengthscale(k), &
           be4_rf_lengthscale(k), &
           be5_rf_lengthscale(k)
   enddo

   write(unit=*, fmt='(//a)') 'Input eval'

   do k=1,kzs
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           be1_eval_glo(k), &
           be2_eval_glo(k), &
           be3_eval_glo(k), &
           be4_eval_glo(k), &
           be5_eval_glo(k)
   enddo

   write(unit=*, fmt='(//a)') 'Input evec (diagnol)'
   do k=1,kzs
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           be1_evec_glo(k,k), &
           be2_evec_glo(k,k), &
           be3_evec_glo(k,k), &
           be4_evec_glo(k,k), &
           be5_evec_glo(k,k)
   enddo

   write(unit=*, fmt='(//a)') 'used Scale lenght'

   do k=1,kz
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           s1(k), &
           s2(k), &
           s3(k), &
           s4(k), &
           s5(k)
   enddo

   write(unit=*, fmt='(//a)') 'used eval'

   do k=1,kz
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           l1(k), &
           l2(k), &
           l3(k), &
           l4(k), &
           l5(k)
   enddo

   write(unit=*, fmt='(//a)') 'used evec (diagnol)'
   do k=1,kz
      write(unit=*, fmt='(a, i3, 5f24.4)') &
           'level:', k, &
           e1(k,k), &
           e2(k,k), &
           e3(k,k), &
           e4(k,k), &
           e5(k,k)
   enddo
   endif
   
   DEALLOCATE ( be1_rf_lengthscale )
   DEALLOCATE ( be2_rf_lengthscale )
   DEALLOCATE ( be3_rf_lengthscale )
   DEALLOCATE ( be4_rf_lengthscale )
   DEALLOCATE ( be5_rf_lengthscale )

!------------------------------------------------------------------------------
!  [12.0] Prescribe vertical truncation if using EOFS:
!------------------------------------------------------------------------------

   IF ( vert_corr == 2 ) THEN
   
      DEALLOCATE ( be1_eval_glo )
      DEALLOCATE ( be2_eval_glo )
      DEALLOCATE ( be3_eval_glo )
      DEALLOCATE ( be4_eval_glo )
      DEALLOCATE ( be5_eval_glo )

      DEALLOCATE ( be1_evec_glo )
      DEALLOCATE ( be2_evec_glo )
      DEALLOCATE ( be3_evec_glo )
      DEALLOCATE ( be4_evec_glo )
      DEALLOCATE ( be5_evec_glo )

!     [11.1] Perform checks on eigenvectors:

      IF ( test_statistics ) THEN
         CALL DA_Check_EOF_Decomposition( l1(:), e1(:,:), be % v1 % name)
         CALL DA_Check_EOF_Decomposition( l2(:), e2(:,:), be % v2 % name)
         CALL DA_Check_EOF_Decomposition( l3(:), e3(:,:), be % v3 % name)
         CALL DA_Check_EOF_Decomposition( l4(:), e4(:,:), be % v4 % name)
         CALL DA_Check_EOF_Decomposition( l5(:), e5(:,:), be % v5 % name)
      END IF
      
!     [11.2] Truncate in vertical:

      CALL DA_Get_Vertical_Truncation( max_vert_var1, l1(:), be % v1 )
      CALL DA_Get_Vertical_Truncation( max_vert_var2, l2(:), be % v2 )
      CALL DA_Get_Vertical_Truncation( max_vert_var3, l3(:), be % v3 )
      CALL DA_Get_Vertical_Truncation( max_vert_var4, l4(:), be % v4 )
      CALL DA_Get_Vertical_Truncation( max_vert_var5, l5(:), be % v5 )
      write(6,*)
      
   ELSE
   
      be % v1 % mz = xb % mkz
      be % v2 % mz = xb % mkz
      be % v3 % mz = xb % mkz
      be % v4 % mz = xb % mkz
      be % v5 % mz = xb % mkz
      
   END IF

   kz_vv(1) = be % v1 % mz
   kz_vv(2) = be % v2 % mz
   kz_vv(3) = be % v3 % mz
   kz_vv(4) = be % v4 % mz
   kz_vv(5) = be % v5 % mz
   kz_vp = xb % mkz

!------------------------------------------------------------------------------
!  [12.0] Initialise control variable space components of header:
!------------------------------------------------------------------------------

   ix = xb % mix
   jy = xb % mjy

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
   
!------------------------------------------------------------------------------
!  [13.0] Transfer errors to error structure:
!------------------------------------------------------------------------------

   CALL DA_Allocate_Background_Errors( jy, kz, l1, e1, be1_eval_loc, &
                                       be1_evec_loc, be % v1 )
   CALL DA_Allocate_Background_Errors( jy, kz, l2, e2, be2_eval_loc, &
                                       be2_evec_loc, be % v2 )
   CALL DA_Allocate_Background_Errors( jy, kz, l3, e3, be3_eval_loc, &
                                       be3_evec_loc, be % v3 )
   CALL DA_Allocate_Background_Errors( jy, kz, l4, e4, be4_eval_loc, &
                                       be4_evec_loc, be % v4 )
   CALL DA_Allocate_Background_Errors( jy, kz, l5, e5, be5_eval_loc, &
                                       be5_evec_loc, be % v5 )

!------------------------------------------------------------------------------
!  [14.0] Store p_b vertical regression in background error structure:
!------------------------------------------------------------------------------

   ALLOCATE( be % pb_vert_reg(1:jy,1:kz,1:kz) )

   IF ( interpolate_stats ) THEN

      call da_interpolate_regcoeff( jy, iys, kz, kzs, meanl_stats, &
                                    xbx % latc_mean, &
                                    meanp_stats, meanp_xb, &
                                    pb_vert_reg, be % pb_vert_reg )

      deallocate( meanp_stats )
      deallocate( meanp_xb )
      deallocate( meanl_stats )

!     call da_write_interpolated_be ( x_mm5%big_header, be%pb_vert_reg,  &
!                                     e1, e2, e3, e4, l1, l2, l3, l4,    &
!                                     s1, s2, s3, s4 )
   ELSE
      do j=1,jy
      do k2=1, kz
      do k1=1, kz
         be % pb_vert_reg(j,k2,k1) = pb_vert_reg(j,kz+1-k2,kz+1-k1)
      enddo
      enddo
      enddo
   END IF

! ....................................................................
! To average the regression coefficients (YRG 20040817):
   if (.not.lat_stats_option) then
     write(unit=*, fmt='(/5x, a/)') &
     '*** Using the averaged regression coefficients for balanced P ***'
     do k1 = 1, kz
     do k2 = 1, kz
! ... Constant in y-direction:  
        y_mean = sum(be % pb_vert_reg(:,k1,k2)) / float(jy)
        be % pb_vert_reg(:,k1,k2) = y_mean
     enddo
     enddo
   endif
! .....................................................................
   
   IF ( Test_Statistics ) THEN 
      write(6,'(/A)')' k*k Vertical Regression Coefficients for i=1'
      CALL DA_Array_Print(2, be % pb_vert_reg(1,:,:), 'j=1 Vert Reg Coeffs' )
      write(6,'(/A)')' k*k Vertical Regression Coefficients for j=jy'
      CALL DA_Array_Print(2, be % pb_vert_reg(jy,:,:), 'j=jy Vert Reg Coeffs' )
      write(6,'(/A)')' i*k Vertical Regression Coefficients for k=1'
      CALL DA_Array_Print(2, be % pb_vert_reg(:,1,:), 'k=1 vert Reg Coeffs' )
      write(6,'(/A)')' i*k Vertical Regression Coefficients for k=kz'
      CALL DA_Array_Print(2, be % pb_vert_reg(:,kz,:), 'k=kz vert Reg Coeffs' )
   END IF

!------------------------------------------------------------------------------
!  [15.0] Tidy up:
!------------------------------------------------------------------------------

   DEALLOCATE ( be1_eval_loc )
   DEALLOCATE ( be2_eval_loc )
   DEALLOCATE ( be3_eval_loc )
   DEALLOCATE ( be4_eval_loc )
   DEALLOCATE ( be5_eval_loc )

   IF ( vert_corr == 2 ) THEN
      DEALLOCATE ( e1 )
      DEALLOCATE ( e2 )
      DEALLOCATE ( e3 )
      DEALLOCATE ( e4 )
      DEALLOCATE ( e5 )
      
      DEALLOCATE ( l1 )
      DEALLOCATE ( l2 )
      DEALLOCATE ( l3 )
      DEALLOCATE ( l4 )
      DEALLOCATE ( l5 )

      DEALLOCATE ( be1_evec_loc )
      DEALLOCATE ( be2_evec_loc )
      DEALLOCATE ( be3_evec_loc )
      DEALLOCATE ( be4_evec_loc )
      DEALLOCATE ( be5_evec_loc )
      
      DEALLOCATE ( pb_vert_reg )

   END IF

!------------------------------------------------------------------------------
!  [16.0] Apply empirical and recursive filter rescaling factor:
!------------------------------------------------------------------------------

   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling1, len_scaling1, &
                                      xb % ds, s1, be % v1 ) 
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling2, len_scaling2, &
                                      xb % ds, s2, be % v2 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling3, len_scaling3, &
                                      xb % ds, s3, be % v3 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling4, len_scaling4, &
                                      xb % ds, s4, be % v4 )
   CALL DA_Rescale_Background_Errors( ix, jy, var_scaling5, len_scaling5, &
                                      xb % ds, s5, be % v5 )
                                      
   deallocate( s1 )
   deallocate( s2 )
   deallocate( s3 )
   deallocate( s4 )
   deallocate( s5 )

!------------------------------------------------------------------------------
!  [9.0] Deallocate input model state:
!------------------------------------------------------------------------------

   IF ( wrf_dm_on_monitor() ) THEN
      CALL DA_Deallocate_MM5_Model( x_mm5 )
   ENDIF

END SUBROUTINE DA_Setup_Background_Errors2

