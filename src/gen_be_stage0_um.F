program gen_be_stage0_um
!---------------------------------------------------------------------- 
! Purpose: To convert UM output to "standard perturbation fields"
!          required by the multi-model BE covariance generation 
!          utility "gen_be".
!
! Input  : UM forecasts for a specified date in NETCDF format. 
!
! Output : Binary files for use in gen_be_stage1.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Initial version [ adapted from Dale Barker's stage0 for WRF ]
! 25/05/11 J.-F. Caron (Met Office)
!          Modifications to da_hydro_p_pert_um to save memory space
! -------- End History
!
!----------------------------------------------------------------------
! UM grid definition
! ************************  2x2 UM Grid   *****************************
!
!   V/P(1,3)                  V/P(2,3)                  V/P(3,3)             
!
!               MASS(1,2)     U-WD(1,2)    MASS(2,2)    U-WD(2,2)                                  
!
!   V/P(1,2)    V-WD(1,1)     V/P(2,2)     V-WD(2,1)    V/P(3,2)  
!
!               MASS(1,1)     U-WD(1,1)    MASS(2,1)    U-WD(2,1)  
!
!   V/P(1,1)                  V/P(2,1)                  V/P(3,1)
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : num_fft_factors, pi, stdout, stderr, &
      filename_len, radian, earth_omega, vertical_ip, &
      vertical_ip_0
   use da_control, only : es_alpha, es_beta, rd_over_rv, rd_over_rv1, &
      es_gamma,kappa,t_kelvin, gravity,gas_constant,cp

   use da_gen_be, only : da_get_field, da_get_height, da_get_trh, &
      da_stage0_initialize
   use da_tools_serial, only : da_get_unit, da_free_unit,da_find_fft_factors, &
      da_find_fft_trig_funcs
   use module_ffts, only : fft551, fft661

   implicit none

   integer :: gen_be_iunit, gen_be_ounit

   integer, parameter    :: nrange = 50               ! Range to search for efficient FFT.

   character (len=filename_len)   :: filestub         ! General filename stub.
   character (len=filename_len)   :: input_file       ! Input file. 
   character (len=filename_len)   :: output_file      ! Output file. 
   character (len=filename_len)   :: fileplev         ! Filename for P levels.
   character (len=filename_len)   :: filetlev         ! Filename for T levels.
   character (len=10)    :: date                      ! Character date.
   character (len=12)    :: var                       ! Variable to search for.
   character (len=3)     :: be_method                 ! "NMC" or "ENS".
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.
   character (len=3)     :: ccut                      ! Boundary points to be removed.
   character (len=2)     :: cvip                      ! Type of vertical inner product.
   character (len=10)    :: carg                      ! Generic variable for arguments.

   character (len=4)     :: masscv                    ! Mass ctrl variable = press or temp.
   character (len=2)     :: humcv                     ! Humidity ctrl variable = rh or sh
   character (len=10)    :: balpres                   ! Balance pressure = purestats or linestats

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: dim1                      ! Dimensions of grid (T points).
   integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim2                      ! Dimensions of grid (T points).
   integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim3                      ! Dimensions of grid (T points).
   integer               :: dim1c                     ! Dimensions of grid (T points) - cut.
   integer               :: dim2c                     ! Dimensions of grid (T points) - cut.
   integer               :: n1, n2                    ! Padded dimensions (n=dim-1+pad).
   integer               :: n1s, n2s                  ! Padded dimensions (n=dim-1+pad).
   integer               :: poisson_method            ! 1=Spectral, 2=SOR.
   integer               :: fft_method                ! For poisson_method=1: 1=FCT, 2=FST.
   integer               :: ktest                     ! Test level.
   integer               :: cut                       ! rim grid points to be remove on output.

   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution.
   real                  :: dim12_inv_u               ! 1 / (dim1*dim2).
   real                  :: dim12_inv_v               ! 1 / (dim1*dim2).
   real                  :: dim12_inv                 ! 1 / (dim1*dim2).

   real                  :: um_npole_lat              ! Latitude of the North Pole
   real                  :: um_npole_lon              ! Longitude of the North Pole
   real                  :: um_lat1                   ! Lower left mass grid point latitude
   real                  :: um_lon1                   ! Lower left mass grid point longitude
   real                  :: um_dx_deg                 ! Grid point distance in degree
   real                  :: um_R                      ! Radius of the earth

   logical               :: test_inverse              ! Test conversion by performing inverse.

   integer               :: ifax1(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax2(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax1s(1:num_fft_factors) ! FFT factors.
   integer               :: ifax2s(1:num_fft_factors) ! FFT factors.

   real, allocatable     :: pheight(:)                ! Height of pressure level
   real, allocatable     :: theight(:)                ! Height of temperature level

   real, allocatable     :: xlat(:,:)                 ! Latitude of mass points.
   real, allocatable     :: xlon(:,:)                 ! Longitude of mass points.

   real, allocatable     :: dx_m(:,:)                 ! x distance - mass pts.
   real, allocatable     :: dy_m(:,:)                 ! y distance - mass pts.
   real, allocatable     :: dx_u(:,:)                 ! x distance - u points.
   real, allocatable     :: dy_u(:,:)                 ! y distance - u points.
   real, allocatable     :: dx_v(:,:)                 ! x distance - v points.
   real, allocatable     :: dy_v(:,:)                 ! y distance - v points.
   real, allocatable     :: dx_p(:,:)                 ! x distance - vort/psi points.
   real, allocatable     :: dy_p(:,:)                 ! y distance - vort/psi points.

   real, allocatable     :: mlat(:,:)                 ! Mass points latitude on rotated grid.
   real, allocatable     :: mlon(:,:)                 ! Mass points longitude on rotated grid
   real, allocatable     :: ulat(:,:)                 ! u points latitude on rotated grid.
   real, allocatable     :: ulon(:,:)                 ! u points longitude on rotated grid
   real, allocatable     :: vlat(:,:)                 ! v points latitude on rotated grid.
   real, allocatable     :: vlon(:,:)                 ! v points longitude on rotated grid
   real, allocatable     :: plat(:,:)                 ! vort/psi pts latitude on rotated grid.
   real, allocatable     :: plon(:,:)                 ! vort/psi pts longitude on rotated grid

   real, allocatable     :: u(:,:)                    ! u-wind.
   real, allocatable     :: v(:,:)                    ! v-wind.
   real, allocatable     :: div(:,:)                  ! Divergence.
   real, allocatable     :: vor(:,:)                  ! Vorticity.
   real, allocatable     :: psi2d(:,:)                ! Streamfunction copy. 
   real, allocatable     :: chi2d(:,:)                ! Velocity potential copy.
   real, allocatable     :: temp2d(:,:)               ! Temperature.
   real, allocatable     :: rh2d(:,:)                 ! Relative humidity.
   real, allocatable     :: sh2d(:,:)                 ! Specific humidity.
   real, allocatable     :: pres2d(:,:)               ! Pressure.
   real, allocatable     :: rho2d(:,:)                ! Density.

   real, allocatable     :: trigs1(:)                 ! FFT trig functions.
   real, allocatable     :: trigs2(:)                 ! FFT trig functions.
   real, allocatable     :: fft_coeffs(:,:)           ! FFT coefficients.
   real, allocatable     :: trigs1s(:)                ! FFT trig functions.
   real, allocatable     :: trigs2s(:)                ! FFT trig functions.
   real, allocatable     :: fft_coeffss(:,:)          ! FFT coefficients.

!  Standard fields:
   real, allocatable     :: psi(:,:,:)                ! Streamfunction.
   real, allocatable     :: chi(:,:,:)                ! Velocity Potential.
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: rh(:,:,:)                 ! Relative humidity.
   real, allocatable     :: sh(:,:,:)                 ! Specific humidity.
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: height(:,:,:)             ! Height.(here pressure)
   real, allocatable     :: pres(:,:,:)               ! Pressure.
   real, allocatable     :: hpres(:,:,:)              ! Hydrostatic pressure.
   real, allocatable     :: lbpres(:,:,:)             ! linear balance pressure.
   real, allocatable     :: rho(:,:,:)                ! Density.

   real, allocatable     :: psi_mean(:,:,:)           ! Streamfunction.
   real, allocatable     :: chi_mean(:,:,:)           ! Velocity Potential.
   real, allocatable     :: temp_mean(:,:,:)          ! Temperature.
   real, allocatable     :: rh_mean(:,:,:)            ! Relative humidity.
   real, allocatable     :: sh_mean(:,:,:)            ! Specific humidity.
   real, allocatable     :: psfc_mean(:,:)            ! Surface pressure.
   real, allocatable     :: pres_mean(:,:,:)          ! Pressure.
   real, allocatable     :: lbpres_mean(:,:,:)        ! Linear balance pressure.
   real, allocatable     :: rho_mean(:,:,:)           ! Density.
   real, allocatable     :: trimdata(:,:,:)           ! Trimmed data fields

   real, allocatable     :: presrho_2dmean(:)         ! Fullfield domain mean pressure.
   real, allocatable     :: prestheta_2dmean(:)       ! Fullfield domain mean pressure.

!----------------------------------------------------------------------

   stderr = 0
   stdout = 6

   um_R = 6.371229E6       ! UM value for radius of the Earth

   write(6,'(/a)')' [1] Initialize information.'

   call da_get_unit(gen_be_iunit)
   call da_get_unit(gen_be_ounit)

!  Get Arguments
   numarg = iargc()
   if ( numarg /= 16 )then
      write(UNIT=6,FMT='(a)') &
      "Usage: gen_be_stage0_um launch with wrong number of arguments"
      stop
   end if
   
   ! Initialise to stop false Cray compiler warnings
   be_method=""
   date=""
   cne=""
   filestub=""
   ccut=""
   masscv=""
   humcv=""
   balpres=""
   cvip=""
   fileplev=""
   filetlev=""
   carg=""

   call getarg( 1, be_method )
   call getarg( 2, date )
   call getarg( 3, cne )
   read(cne,'(i3)')ne
   call getarg( 4, filestub )
   call getarg( 5, ccut )
   read(ccut,'(i3)')cut
   call getarg( 6, masscv )
   call getarg( 7, humcv )
   call getarg( 8, balpres )
   call getarg( 9, cvip )
   read(cvip,'(i2)')vertical_ip
   call getarg(10, fileplev )
   call getarg(11, filetlev )
   call getarg(12, carg )
   read(carg,*)um_npole_lat
   call getarg(13, carg )
   read(carg,*)um_npole_lon
   call getarg(14, carg )
   read(carg,*)um_lat1
   call getarg(15, carg )
   read(carg,*)um_lon1
   call getarg(16, carg )
   read(carg,*)um_dx_deg

   ! Set some options
   test_inverse = .true. 
   ktest = 1
   poisson_method = 1    
   fft_method = 2

   if (masscv.eq."temp") then
      write(UNIT=6,FMT='(a)') " masscv = Temperature"
   else
      write(UNIT=6,FMT='(a)') " masscv = Pressure"
   endif

   if (humcv.eq."rh") then
      write(UNIT=6,FMT='(a)') " humcv  = Relative Humidity"
   else
      write(UNIT=6,FMT='(a)') " humcv  = Specific Humidity"
   endif

   if (balpres.eq."purestats") then
      write(UNIT=6,FMT='(a)') &
      " balpres = Purely statistical: Psi-(P or T) regression)"
   else
      write(UNIT=6,FMT='(a)') &
      " balpres = linear balance and Pb-P regression"
   endif  

   if (vertical_ip /= vertical_ip_0) then
      write(UNIT=6,FMT='(a)') " A inner product is used in the EOF"
   else
      write(UNIT=6,FMT='(a)') " No inner product used in the EOF"
   endif

   if ( be_method == "NMC" ) then
      write(6,'(a,a)') & 
      ' Computing gen_be NMC-method forecast difference files ', date
      ne = 2                      ! NMC-method uses differences between 2 forecasts.
   else if ( be_method == "ENS" ) then
      write(6,'(a,a)') &
      ' Computing gen_be ensemble perturbation files for date ', date
      write(6,'(a,i4)')' Ensemble Size = ', ne
   else
      write(6,'(a,a)') &
      trim(be_method), ' is an invalid value of be_method. Stop'
      stop
   end if
   write(6,'(a,a)')' Input filestub = ', trim(filestub)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)') &
      ' [2] Setup up ancillary fields using 1st member values.' 
!---------------------------------------------------------------------------------------------

!  UM rotated lat-lon grid parameters
   print*
   print*," um_npole_lat = ", um_npole_lat
   print*," um_npole_lon = ", um_npole_lon
   print*," um_lat1      = ", um_lat1
   print*," um_lon1      = ", um_lon1
   print*," um_dx_deg    = ", um_dx_deg
   print*

   var = "theta"
   input_file = trim(filestub)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )

   dim1s = dim1+1 ! Vorticity/streamfunction array 1 larger.
   dim2s = dim2+1 ! Vorticity/streamfunction array 1 larger.

   dim12_inv_u = 1.0 / real((dim1+1) * dim2)
   dim12_inv_v = 1.0 / real(dim1 * (dim2+1))
   dim12_inv = 1.0 / real(dim1 * dim2)

   print*, " dim1 : ", dim1
   print*, " dim2 : ", dim2
   print*, " dim3 : ", dim3
   print*

!  Replace dummy ds outputed by da_stage0_initialize
   call da_deg2m(ds,um_dx_deg,0.0)

   allocate( xlat(1:dim1,1:dim2) )
   allocate( xlon(1:dim1,1:dim2) )

   allocate( dx_m(1:dim1,1:dim2) )
   allocate( dy_m(1:dim1,1:dim2) )
   allocate( dx_u(1:dim1,1:dim2) )
   allocate( dy_u(1:dim1,1:dim2) )
   allocate( dx_v(1:dim1,1:dim2-1) )
   allocate( dy_v(1:dim1,1:dim2-1) )
   allocate( dx_p(1:dim1+1,1:dim2+1) )
   allocate( dy_p(1:dim1+1,1:dim2+1) )

   allocate( mlat(1:dim1,1:dim2) )
   allocate( mlon(1:dim1,1:dim2) )
   allocate( ulat(1:dim1,1:dim2) )
   allocate( ulon(1:dim1,1:dim2) )
   allocate( vlat(1:dim1,1:dim2-1) )
   allocate( vlon(1:dim1,1:dim2-1) )
   allocate( plat(1:dim1+1,1:dim2+1) )
   allocate( plon(1:dim1+1,1:dim2+1) )

   allocate( pheight(1:dim3+1) )
   allocate( theight(1:dim3) )

!  Get UM height for pressure and temperature levels
   call da_get_um_height( pheight, theight, fileplev, filetlev, dim3, &
                          gen_be_iunit)

!  Calculate lat-lon on rotated earth for mass, u, v, and vort/psi grid points
   call da_rotlatlon( mlat, mlon, ulat, ulon, vlat, vlon, plat, plon, &
                      dim1, dim2, um_lat1, um_lon1, um_dx_deg )

!  Calculate real latitude and longitude of MASS grid points
   call EQTOLL( mlat, mlon, xlat, xlon, um_npole_lat, um_npole_lon, &
                dim1*dim2) 

!  Calculate distance between grid points
   call da_distance ( dx_m, dy_m, mlat, mlon, dim1, dim2)
   call da_distance ( dx_u, dy_u, ulat, ulon, dim1, dim2)
   call da_distance ( dx_v, dy_v, vlat, vlon, dim1, dim2-1)
   call da_distance ( dx_p, dy_p, plat, plon, dim1+1, dim2+1)

!  Initialize FFT coefficients:
   if ( poisson_method == 1 ) then
      call da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )
      call da_fft_initialize1( dim1s, dim2s, n1s, n2s, ifax1s, ifax2s )

      allocate( trigs1(1:3*n1) )
      allocate( trigs2(1:3*n2) )
      allocate( fft_coeffs(1:n1+1,1:n2+1) )
      call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )
      allocate( trigs1s(1:3*n1s) )
      allocate( trigs2s(1:3*n2s) )
      allocate( fft_coeffss(1:n1s+1,1:n2s+1) )
      call da_fft_initialize2( n1s, n2s, ds, trigs1s, trigs2s, fft_coeffss )
   end if

!  Allocate arrays in output fields:
   allocate( psi(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( chi(1:dim1,1:dim2,1:dim3) )
   allocate( temp(1:dim1,1:dim2,1:dim3) )
   allocate( rh(1:dim1,1:dim2,1:dim3) )
   allocate( sh(1:dim1,1:dim2,1:dim3) )
   allocate( psfc(1:dim1,1:dim2) )
   allocate( height(1:dim1,1:dim2,1:dim3) )
   allocate( pres(1:dim1,1:dim2,1:dim3) )
   allocate( hpres(1:dim1,1:dim2,1:dim3) )
   allocate( lbpres(1:dim1,1:dim2,1:dim3) )
   allocate( rho(1:dim1,1:dim2,1:dim3) )
   allocate( trimdata(1:dim1-2*cut,1:dim2-2*cut,1:dim3) )

   allocate( psi_mean(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( chi_mean(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mean(1:dim1,1:dim2,1:dim3) )
   allocate( rh_mean(1:dim1,1:dim2,1:dim3) )
   allocate( sh_mean(1:dim1,1:dim2,1:dim3) )
   allocate( pres_mean(1:dim1,1:dim2,1:dim3) )
   allocate( lbpres_mean(1:dim1,1:dim2,1:dim3) )
   allocate( rho_mean(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mean(1:dim1,1:dim2) )

   psi_mean = 0.0
   chi_mean = 0.0
   temp_mean = 0.0
   rh_mean = 0.0
   sh_mean = 0.0
   psfc_mean = 0.0
   pres_mean = 0.0
   lbpres_mean = 0.0
   rho_mean = 0.0

!---------------------------------------------------------------------------------------------
   write(6,'(/a)') &
      ' [3] Convert UM forecast fields to standard fields and output' 
!---------------------------------------------------------------------------------------------

   ! Allocate temporary arrays:
   allocate( u(1:dim1,1:dim2) )
   allocate( v(1:dim1,1:dim2-1) )

   allocate( vor(1:dim1s,1:dim2s) ) ! dim1+1, dim2+1
   allocate( psi2d(1:dim1s,1:dim2s) )

   allocate( div(1:dim1,1:dim2) )
   allocate( chi2d(1:dim1,1:dim2) )

   allocate( temp2d(1:dim1,1:dim2) )
   allocate( rh2d(1:dim1,1:dim2) )
   allocate( sh2d(1:dim1,1:dim2) )
   allocate( pres2d(1:dim1,1:dim2) )
   allocate( rho2d(1:dim1,1:dim2) )

   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      print*
      print*,"member #",member, "filename =", input_file

      do k = 1, dim3

         ! Read u, v on rho levels
         var = "x-wind"
!         print*, var
         call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, u )
         var = "y-wind"
!         print*, var
         call da_get_field( input_file, var, 3, dim1, dim2-1, dim3, k, v )

         ! Calculate vorticity (in center of mass grid on UM's Arakawa C-grid):
         call da_uv_to_vor_umc( dim1, dim2, dy_u, dx_v, u, v, vor )

         ! Calculate divergence (at mass pts. on UM's Arakawa C-grid):
         call da_uv_to_div_umc( dim1, dim2, dx_u, dy_v, u, v, div )

         ! Calculate streamfunction and potential 
         ! Assumes vor/div converted to Del**2 psi/chi):
         if ( poisson_method == 1 ) then
            call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, &
                                trigs1s, trigs2s, fft_coeffss, vor, psi2d )
            call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, &
                                trigs1, trigs2, fft_coeffs, div, chi2d )
         else if ( poisson_method == 2 ) then
!jfc            call da_sor( dim1s, dim2s, ds, vor, psi2d )
!jfc            call da_sor( dim1, dim2, ds, div, chi2d )
            print*
            print*," poisson_method == 2 is not adapted for the UM"
            print*," .... ABORT !!!!"
            stop
        end if

        if ( test_inverse .and. k == ktest .and. member == 1 ) then
           call da_test_inverse( dim1, dim2, dx_u, dy_u, dx_v, dy_v, dx_m, dy_m, dx_p, dy_p, &
                                  n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                                  n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                                  u, v, psi2d, chi2d )
        end if

!       Interpolate psi to mass pts ready for output:
        do j = 1, dim2
           do i = 1, dim1
              psi(i,j,k) = 0.25 * ( psi2d(i,j) + psi2d(i+1,j) + &
                                    psi2d(i,j+1) + psi2d(i+1,j+1) )
           end do
        end do
        chi(:,:,k) = chi2d(:,:)

!       Read humidity fields and convert to appropriate variables
        call da_get_rh_um( input_file, dim1, dim2, dim3, k, & ! outputs RH on theta levels
                           pheight, theight, rh2d )

        var = "q" ! specific humidity on theta levels
        call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, sh2d )

        rh(:,:,k) = rh2d(:,:)
        sh(:,:,k) = sh2d(:,:)

!       Read mass fields
        if (masscv.eq."temp") then
           call da_get_trhol_um( input_file, dim1, dim2, dim3, k, &  ! outputs T on rho levels
                                  pheight, theight, temp2d)
        else
           var = "theta" ! potential temperature theta levels
           call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, temp2d )
        endif
        temp(:,:,k) = temp2d(:,:)

        var = "p"        ! pressure on rho levels
        call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, pres2d )
        pres(:,:,k) = pres2d(:,:)

        var = "unspecified" ! rho * R * R on rho levels
        call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, rho2d)
        rho(:,:,k) = rho2d(:,:)/(um_R**2)

      end do

!     Extract the surface pressure :
      psfc(:,:)=pres(:,:,1)
      print*,"Surface pressure =",psfc(50,50)/100.0 

!     Fill height vector with rho level heights (without top level)
      do k=1,dim3
         height(1:dim1,1:dim2,k)=pheight(k)
      enddo

!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)sh
      write(gen_be_ounit)psfc
      write(gen_be_ounit)height
      write(gen_be_ounit)xlat
      write(gen_be_ounit)pres
      write(gen_be_ounit)rho
      close(gen_be_ounit)

!     Calculate accumulating mean:
      member_inv = 1.0 / real(member)

      psi_mean = ( real( member-1 ) * psi_mean + psi ) * member_inv
      chi_mean = ( real( member-1 ) * chi_mean + chi ) * member_inv
      temp_mean = ( real( member-1 ) * temp_mean + temp ) * member_inv
      rh_mean = ( real( member-1 ) * rh_mean + rh ) * member_inv
      sh_mean = ( real( member-1 ) * sh_mean + sh ) * member_inv
      psfc_mean = ( real( member-1 ) * psfc_mean + psfc ) * member_inv
      pres_mean = ( real( member-1 ) * pres_mean + pres ) * member_inv
      rho_mean = ( real( member-1 ) * rho_mean + rho ) * member_inv
   end do

   if (vertical_ip /= vertical_ip_0) then
!     Computation of domain-mean pressure on both theta and rho levels to be used later for
!     the computation of the innner product in the EOF decomposition of the vertical cov.
      allocate( presrho_2dmean(1:dim3+1) )
      allocate( prestheta_2dmean(0:dim3) )
      do k=1,dim3
         presrho_2dmean(k) = sum(pres_mean(1+cut:dim1-cut,1+cut:dim2-cut,k)) / &
                             ((dim1-2*cut)*(dim2-2*cut))
      enddo
      ! Extrapolation to get a mean pressure at rho level dim3+1
      presrho_2dmean(dim3+1) = ( (presrho_2dmean(dim3)-presrho_2dmean(dim3-1)) * &
                               (pheight(dim3+1)-pheight(dim3)) /                 &
                               (pheight(dim3)-pheight(dim3-1)) ) + presrho_2dmean(dim3)

      call da_pres_rho2theta_1d(prestheta_2dmean, presrho_2dmean, pheight, theight, dim3)

!     Write out mean pressure profile from the NMC reference state or the ensemble mean:
      output_file = 'p_for_innerprod.'//date(1:10)  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)presrho_2dmean
      write(gen_be_ounit)prestheta_2dmean
      close(gen_be_ounit)

      deallocate( presrho_2dmean )
      deallocate( prestheta_2dmean )
   end if

   deallocate( u )
   deallocate( v )
   deallocate( vor )
   deallocate( div )
   deallocate( psi2d )
   deallocate( chi2d )
   deallocate( temp2d )
   deallocate( rh2d )
   deallocate( sh2d )
   deallocate( pres2d )
   deallocate( rho2d )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( be_method == "NMC" ) then
      write(6,'(/a)')  &
      ' Compute perturbation as a difference between two forecasts' 

!     Re-read input forecast standard fields (ne=2 hard-wired above for NMC-method):
      input_file = 'tmp.e001'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi
      read(gen_be_iunit)chi
      read(gen_be_iunit)temp
      read(gen_be_iunit)rh
      read(gen_be_iunit)sh
      read(gen_be_iunit)psfc
      read(gen_be_iunit)height
      read(gen_be_iunit)xlat
      read(gen_be_iunit)pres
      read(gen_be_iunit)rho
      close(gen_be_iunit)
      call da_free_unit(gen_be_iunit)

      print*
      print*,input_file
      print*,date, dim1, dim2, dim3

!     Note overwriting mean diagnostic to save memory!
      input_file = 'tmp.e002'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi_mean
      read(gen_be_iunit)chi_mean
      read(gen_be_iunit)temp_mean
      read(gen_be_iunit)rh_mean
      read(gen_be_iunit)sh_mean
      read(gen_be_iunit)psfc_mean
      read(gen_be_iunit)height
      read(gen_be_iunit)xlat
      read(gen_be_iunit)pres_mean
      read(gen_be_iunit)rho_mean
      close(gen_be_iunit)

      print*
      print*,input_file
      print*,date, dim1, dim2, dim3

!     Take forecast difference:
      psi = psi - psi_mean
      chi = chi - chi_mean
      temp = temp - temp_mean
      rh  = rh - rh_mean
      sh  = sh - sh_mean
      psfc = psfc - psfc_mean
      pres = pres - pres_mean
      rho = rho - rho_mean ! rho perts are not used for now

      if (masscv.ne."temp") then
         print*,"Calling hydro_p_pert"
         call da_hydro_p_pert_um(hpres,pres,pres_mean,temp,temp_mean,sh,sh_mean, &
                                 pheight,dim1,dim2,dim3)
         print*,"Exiting hydro_p_pert"
          pres=hpres
          if (balpres.eq."linestats") then
             print*,"Calling linbal"
             call da_linbal_um(lbpres,psi,rho_mean,xlat,dim1,dim2,dim3)
             print*,"Exiting linbal"
          endif
      endif

      print*,"dim1, dim2, dim1cut, dim2cut =",dim1,dim2,dim1-2*cut,dim2-2*cut

!     Write out trimmed NMC-method standard perturbations:
      output_file = 'pert.'//date(1:10)//'.e001'
      open (gen_be_ounit, file = output_file, form='unformatted')
      
      dim1c = dim1-2*cut
      dim2c = dim2-2*cut
      write(gen_be_ounit)date, dim1c, dim2c, dim3

      trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = psi(1+cut:dim1-cut,1+cut:dim2-cut,:) 
      write(gen_be_ounit)trimdata

      trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = chi(1+cut:dim1-cut,1+cut:dim2-cut,:)
      write(gen_be_ounit)trimdata

      if (masscv.eq."temp") then
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = temp(1+cut:dim1-cut,1+cut:dim2-cut,:)
         write(gen_be_ounit)trimdata
      endif

      if (humcv.eq."rh") then
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = rh(1+cut:dim1-cut,1+cut:dim2-cut,:)
      else
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = sh(1+cut:dim1-cut,1+cut:dim2-cut,:)
      end if
      write(gen_be_ounit)trimdata

      if (masscv.eq."temp") then
         trimdata(1:dim1-2*cut,1:dim2-2*cut,1) = psfc(1+cut:dim1-cut,1+cut:dim2-cut)
         write(gen_be_ounit)trimdata(:,:,1)
      endif

      trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = height(1+cut:dim1-cut,1+cut:dim2-cut,:)
      write(gen_be_ounit)trimdata

      trimdata(1:dim1-2*cut,1:dim2-2*cut,1) = xlat(1+cut:dim1-cut,1+cut:dim2-cut)
      write(gen_be_ounit)trimdata(:,:,1)

      if (masscv.ne."temp") then
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = pres(1+cut:dim1-cut,1+cut:dim2-cut,:)
         write(gen_be_ounit)trimdata
         if (balpres.ne."purestats") then
            trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = lbpres(1+cut:dim1-cut,1+cut:dim2-cut,:)
            write(gen_be_ounit)trimdata
         endif
      endif

      close(gen_be_ounit)

!     Restore mean (1 member only for NMC-method):
      psi_mean = psi
      chi_mean = chi
      temp_mean = temp
      rh_mean  = rh
      sh_mean  = sh
      psfc_mean = psfc
      pres_mean = pres
      rho_mean = rho
      lbpres_mean = lbpres

   else ! be_method = "ENS"

      write(6,'(/a)') &
      "     [4.1] Convert ensemble of standard fields to perturbations"

      do member = 1, ne
         write(UNIT=ce,FMT='(i3.3)')member

!        Re-read ensemble member standard fields:
         input_file = 'tmp.e'//ce  
         open (gen_be_iunit, file = input_file, form='unformatted')
         read(gen_be_iunit)date, dim1, dim2, dim3
         read(gen_be_iunit)psi
         read(gen_be_iunit)chi
         read(gen_be_iunit)temp
         read(gen_be_iunit)rh
         read(gen_be_iunit)sh
         read(gen_be_iunit)psfc
         read(gen_be_iunit)height
         read(gen_be_iunit)xlat
         read(gen_be_iunit)pres
         read(gen_be_iunit)rho
         close(gen_be_iunit)

         psi = psi - psi_mean
         chi = chi - chi_mean
         temp = temp - temp_mean
         rh  = rh - rh_mean
         sh  = sh - sh_mean
         psfc = psfc - psfc_mean
         pres = pres - pres_mean
         rho = rho - rho_mean

         if (masscv.ne."temp") then
            call da_hydro_p_pert_um(hpres,pres,pres_mean,temp,temp_mean,sh,sh_mean, &
                                    pheight,dim1,dim2,dim3)
            pres=hpres
            if (balpres.eq."linestats") then
               call da_linbal_um(lbpres,psi,rho_mean,xlat,dim1,dim2,dim3)
            endif
         endif

!        Write out standard perturbations for this member:
         output_file = 'pert.'//date(1:10)//'.e'//ce  
         open (gen_be_ounit, file = output_file, form='unformatted')

         dim1c = dim1-2*cut
         dim2c = dim2-2*cut
         write(gen_be_ounit)date, dim1c, dim2c, dim3

         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = psi(1+cut:dim1-cut,1+cut:dim2-cut,:) 
         write(gen_be_ounit)trimdata
         
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = chi(1+cut:dim1-cut,1+cut:dim2-cut,:)
         write(gen_be_ounit)trimdata
         
         if (masscv.eq."temp") then
            trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = temp(1+cut:dim1-cut,1+cut:dim2-cut,:)
            write(gen_be_ounit)trimdata
         endif

         if (humcv.eq."rh") then
            trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = rh(1+cut:dim1-cut,1+cut:dim2-cut,:)
         else
            trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = sh(1+cut:dim1-cut,1+cut:dim2-cut,:)
         end if
         write(gen_be_ounit)trimdata

         if (masscv.eq."temp") then
            trimdata(1:dim1-2*cut,1:dim2-2*cut,1) = psfc(1+cut:dim1-cut,1+cut:dim2-cut)
            write(gen_be_ounit)trimdata(:,:,1)
         endif

         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = height(1+cut:dim1-cut,1+cut:dim2-cut,:) ! Full height
         write(gen_be_ounit)trimdata
         
         trimdata(1:dim1-2*cut,1:dim2-2*cut,1) = xlat(1+cut:dim1-cut,1+cut:dim2-cut) ! Full latitude
         write(gen_be_ounit)trimdata(:,:,1)

         if (masscv.ne."temp") then
            trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = pres(1+cut:dim1-cut,1+cut:dim2-cut,:)
            write(gen_be_ounit)trimdata
            if (balpres.ne."purestats") then
               trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = lbpres(1+cut:dim1-cut,1+cut:dim2-cut,:)
               write(gen_be_ounit)trimdata
            endif
         endif

         close(gen_be_ounit)

      end do

   end if

!  Write out mean/mean square fields:

   output_file = 'psi/'//date(1:10)//'.psi.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = psi_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   write(gen_be_ounit)trimdata
   close(gen_be_ounit)

   output_file = 'chi/'//date(1:10)//'.chi.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = chi_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   write(gen_be_ounit)trimdata
   close(gen_be_ounit)

   if (masscv.eq."temp") then
   output_file = 't/'//date(1:10)//'.t.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = temp_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   write(gen_be_ounit)trimdata
   close(gen_be_ounit)
   endif

   output_file = 'rh/'//date(1:10)//'.rh.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   if (humcv.eq."rh") then
      trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = rh_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   else
      trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = sh_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   end if
   write(gen_be_ounit)trimdata
   close(gen_be_ounit)

   if (masscv.eq."temp") then
   output_file = 'ps/'//date(1:10)//'.ps.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   trimdata(1:dim1-2*cut,1:dim2-2*cut,1) = psfc_mean(1+cut:dim1-cut,1+cut:dim2-cut)
   write(gen_be_ounit)trimdata(:,:,1)
   close(gen_be_ounit)
   endif

   if (masscv.ne."temp") then
   output_file = 'p/'//date(1:10)//'.p.mean'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
   trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = pres_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
   write(gen_be_ounit)trimdata
   close(gen_be_ounit)
      if (balpres.ne."purestats") then
         output_file = 'lbp/'//date(1:10)//'.lbp.mean'
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1-2*cut, dim2-2*cut, dim3
         trimdata(1:dim1-2*cut,1:dim2-2*cut,:) = lbpres_mean(1+cut:dim1-cut,1+cut:dim2-cut,:)
         write(gen_be_ounit)trimdata
         close(gen_be_ounit)
      endif
   endif

   deallocate( pheight )
   deallocate( theight )
   deallocate( psi )
   deallocate( chi )
   deallocate( temp )
   deallocate( rh )
   deallocate( sh )
   deallocate( psfc )
   deallocate( pres )
   deallocate( hpres )
   deallocate( lbpres )
   deallocate( rho )
   deallocate( height )
   deallocate( xlat )
   deallocate( trimdata )
   deallocate( mlat )
   deallocate( mlon )
   deallocate( ulat )
   deallocate( ulon )
   deallocate( vlat )
   deallocate( vlon )
   deallocate( plat )
   deallocate( plon )
   deallocate( dx_m )
   deallocate( dy_m )
   deallocate( dx_u )
   deallocate( dy_u )
   deallocate( dx_v )
   deallocate( dy_v )
   deallocate( dx_p )
   deallocate( dy_p )
   deallocate( psi_mean )
   deallocate( chi_mean )
   deallocate( temp_mean )
   deallocate( rh_mean )
   deallocate( sh_mean )
   deallocate( psfc_mean )
   deallocate( pres_mean )
   deallocate( lbpres_mean )
   deallocate( rho_mean )

CONTAINS
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!***********************          SUBROUTINES        **************************
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

subroutine da_rotlatlon( mlat, mlon, ulat, ulon, vlat, vlon, plat, plon, &
      dim1, dim2, um_lat1, um_lon1, um_dx_deg )

!---------------------------------------------------------------------- 
! Purpose: Compute lat-lon on rotated earth for mass, u, v, and 
!          vort/psi grid points.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   real, intent(out)  :: mlat(1:dim1,1:dim2)          ! mass points latitude
   real, intent(out)  :: mlon(1:dim1,1:dim2)          ! mass points longitude
   real, intent(out)  :: ulat(1:dim1,1:dim2)          ! u points latitude
   real, intent(out)  :: ulon(1:dim1,1:dim2)          ! u points longitude
   real, intent(out)  :: vlat(1:dim1,1:dim2-1)        ! v points latitude
   real, intent(out)  :: vlon(1:dim1,1:dim2-1)        ! v points longitude
   real, intent(out)  :: plat(1:dim1+1,1:dim2+1)      ! v points latitude
   real, intent(out)  :: plon(1:dim1+1,1:dim2+1)      ! v points longitude

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: um_lat1                      ! lower left latitude
   real, intent(in)   :: um_lon1                      ! lower left longitude
   real, intent(in)   :: um_dx_deg                    ! grid resolution in degree

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! UM dims (dummies for now).

!  mass points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         mlat(i,j) =  um_lat1 + (float(j-1)*um_dx_deg)
         mlon(i,j) =  um_lon1 + (float(i-1)*um_dx_deg)
      end do
   end do

!  u-wind points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         ulat(i,j) =  um_lat1 + (float(j-1)*um_dx_deg)
         ulon(i,j) =  (um_lon1+(um_dx_deg/2.0)) + (float(i-1)*um_dx_deg)
      end do
   end do

!  v-wind points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2-1

   do j = jts, jte
      do i = its, ite
         vlat(i,j) =  (um_lat1+(um_dx_deg/2.0)) + (float(j-1)*um_dx_deg)
         vlon(i,j) =  um_lon1 + (float(i-1)*um_dx_deg)
      end do
   end do

!  vort/psi points:
   its = 1
   ite = dim1+1
   jts = 1
   jte = dim2+1

   do j = jts, jte
      do i = its, ite
         plat(i,j) =  (um_lat1-(um_dx_deg/2.0)) + (float(j-1)*um_dx_deg)
         plon(i,j) =  (um_lon1-(um_dx_deg/2.0)) + (float(i-1)*um_dx_deg)
      end do
   end do

end subroutine da_rotlatlon

!------------------------------------------------------------------------------

SUBROUTINE EQTOLL(PHI_EQ,LAMBDA_EQ,PHI,LAMBDA,&
      PHI_POLE,LAMBDA_POLE,POINTS)

! ******************************COPYRIGHT******************************
! (c) CROWN COPYRIGHT 1995, METEOROLOGICAL OFFICE, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Meteorological Office
!                London Road
!                BRACKNELL
!                Berkshire UK
!                RG12 2SZ
!
! If no contract has been raised with this copy of the code, the use,
! duplication or disclosure of it is strictly prohibited.  Permission
! to do so must first be obtained in writing from the Head of Numerical
! Modelling at the above address.
! ******************************COPYRIGHT******************************
!
!LL  Subroutine EQTOLL-------------------------------------------------
!LL
!LL  Purpose:  Calculates latitude and longitude on standard grid
!LL            from input arrays of latitude and longitude on
!LL            equatorial latitude-longitude (eq) grid used
!LL            in regional models. Both input and output latitudes
!LL            and longitudes are in degrees.
!LL
!LL  Written by A. Dickinson
!LL
!LL  Model            Modification history from model version 3.0:
!LL version  Date
!LL
!LL  Documentation: The transformation formulae are described in
!LL                 unified model on-line documentation paper S1.
!LL
!LL Logical components covered : S131
!LL
!LL Project task :
!LL
!LL External documentation:
!LL
!LLEND-----------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, intent(in) :: POINTS            !IN  Number of points to be processed

      REAL, intent(out)   :: PHI(POINTS)       !OUT Latitude
      REAL, intent(out)   :: LAMBDA(POINTS)    !OUT Longitude (0 =< LON < 360)
      REAL, intent(in)    :: LAMBDA_EQ(POINTS) !IN  Longitude in equatorial lat-lon coords
      REAL, intent(in)    :: PHI_EQ(POINTS)    !IN  Latitude in equatorial lat-lon coords
      REAL, intent(in)    :: PHI_POLE          !IN  Latitude of equatorial lat-lon pole
      REAL, intent(in)    :: LAMBDA_POLE       !IN  Longitude of equatorial lat-lon pole

! Workspace usage:-----------------------------------------------------
! None
!----------------------------------------------------------------------
! External subroutines called:-----------------------------------------
! None
!*---------------------------------------------------------------------
! Local varables:------------------------------------------------------
      REAL E_LAMBDA,E_PHI,A_LAMBDA,ARG,A_PHI,SIN_PHI_POLE,COS_PHI_POLE
      REAL TERM1,TERM2,SMALL,LAMBDA_ZERO
      INTEGER I
      PARAMETER(SMALL=1.0E-6)
!----------------------------------------------------------------------
! Constants from comdecks:---------------------------------------------
!*L------------------COMDECK C_PI---------------------------------------
!LL
!LL 4.0 19/09/95  New value for PI. Old value incorrect
!LL               from 12th decimal place. D. Robinson
!LL
      REAL PI,PI_OVER_180,RECIP_PI_OVER_180

      PARAMETER( PI=3.14159265358979323846,  & ! Pi
                 PI_OVER_180=PI/180.0,       & ! Conversion factor degrees to radians
                 RECIP_PI_OVER_180=180.0/PI )  ! Conversion factor radians to
                                               ! degrees
!*----------------------------------------------------------------------
!----------------------------------------------------------------------

!L 1. Initialise local constants
!
! Latitude of zeroth meridian
      LAMBDA_ZERO=LAMBDA_POLE+180.
! Sine and cosine of latitude of eq pole
      SIN_PHI_POLE=SIN(PI_OVER_180*PHI_POLE)
      COS_PHI_POLE=COS(PI_OVER_180*PHI_POLE)

!L 2. Transform from equatorial to standard latitude-longitude

      DO 200 I= 1,POINTS

! Scale eq longitude to range -180 to +180 degs

      E_LAMBDA=LAMBDA_EQ(I)
      IF(E_LAMBDA.GT. 180.0) E_LAMBDA=E_LAMBDA-360.0
      IF(E_LAMBDA.LT.-180.0) E_LAMBDA=E_LAMBDA+360.0

! Convert eq latitude & longitude to radians

      E_LAMBDA=PI_OVER_180*E_LAMBDA
      E_PHI=PI_OVER_180*PHI_EQ(I)

! Compute latitude using equation (4.7)

      ARG=COS_PHI_POLE*COS(E_LAMBDA)*COS(E_PHI) &
                         +SIN(E_PHI)*SIN_PHI_POLE
      ARG=MIN(ARG, 1.0)
      ARG=MAX(ARG,-1.0)
      A_PHI=ASIN(ARG)
      PHI(I)=RECIP_PI_OVER_180*A_PHI

! Compute longitude using equation (4.8)

      TERM1 =(COS(E_PHI)*COS(E_LAMBDA)*SIN_PHI_POLE &
             -SIN(E_PHI)*COS_PHI_POLE)
      TERM2=COS(A_PHI)
      IF(TERM2.LT.SMALL) THEN
        A_LAMBDA=0.0
      ELSE
        ARG=TERM1/TERM2
        ARG=MIN(ARG, 1.0)
        ARG=MAX(ARG,-1.0)
        A_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
        A_LAMBDA=SIGN(A_LAMBDA,E_LAMBDA)
        A_LAMBDA=A_LAMBDA+LAMBDA_ZERO
      END IF

! Scale longitude to range 0 to 360 degs

      IF(A_LAMBDA.GE.360.0) A_LAMBDA=A_LAMBDA-360.0
      IF(A_LAMBDA.LT.0.0) A_LAMBDA=A_LAMBDA+360.0
      LAMBDA(I)=A_LAMBDA

200   CONTINUE

      RETURN
      END SUBROUTINE EQTOLL

!------------------------------------------------------------------------------

subroutine da_distance( dx, dy, lat, lon, dim1, dim2 )

!----------------------------------------------------------------------
! Purpose: Calculate grid point distance in x and y directions using the 
!          latitude and longitude at every grid points and the Haversine
!          formula assuming a spherical earth.  
!
! Note   : dx(i,j) = distance between (i,j) and (i+1,j)
!          dy(i,j) = distance between (i,j) and (i,j+1)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   real, intent(out)  :: dx(1:dim1,1:dim2)            ! x-distance (meter)
   real, intent(out)  :: dy(1:dim1,1:dim2)            ! y-distance (meter)
   real, intent(in)   :: lat(1:dim1,1:dim2)           ! latitude
   real, intent(in)   :: lon(1:dim1,1:dim2)           ! longitude

   integer, intent(in):: dim1, dim2                   ! Dimensions.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! UM dims (dummies for now).

   real               :: dlat, dlon, lat1, lat2, a, c, R, npi

   npi=3.14159265358979323846
   R = 6.371229E6                   ! UM radius of the earth

   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite

         ! x distance
         if (i.ne.ite) then
            dlon = abs(lon(i+1,j) - lon(i,j))
            dlat = abs(lat(i+1,j) - lat(i,j))
            lat1 = lat(i,j)
            lat2 = lat(i+1,j)
         else
            dlon = abs(lon(i,j) - lon(i-1,j))
            dlat = abs(lat(i,j) - lat(i-1,j))
            lat1 = lat(i,j)
            lat2 = lat(i-1,j)
         endif

         a = (sin(dlat/2.0)*npi/180.0)**2 + &
         cos(lat1*npi/180.0)* cos(lat2*npi/180.0)* &
         (sin(dlon/2.0*npi/180.0))**2
         c = 2.0 * atan2(sqrt(a),sqrt(1-a))
         dx(i,j)= R * c

         ! y distance
         if (j.ne.jte) then
            dlat = abs(lat(i,j+1) - lat(i,j))
            dlon = abs(lon(i,j+1) - lon(i,j))
            lat1 = lat(i,j)
            lat2 = lat(i,j+1)
         else
            dlat = abs(lat(i,j) - lat(i,j-1))
            dlon = abs(lon(i,j) - lon(i,j-1))
            lat1 = lat(i,j)
            lat2 = lat(i,j-1)
         endif
         
         a = (sin(dlat/2.0*npi/180.0))**2 + &
         cos(lat1*npi/180.0)*               &
         cos(lat2*npi/180.0)*               &
         (sin(dlon/2.0*npi/180.0))**2
         c = 2.0 * atan2(sqrt(a),sqrt(1-a))
         dy(i,j)= R * c

      end do
   end do

end subroutine da_distance

!------------------------------------------------------------------------------

subroutine da_deg2m( dx_m, dx_deg, lat)

!---------------------------------------------------------------------- 
! Purpose: Calculate grid point distance in m from a distance in degree 
!          using the the Haversine formula assuming a spherical earth.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/04/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   real, intent(out)  :: dx_m       ! x-distance (meter)
   real, intent(in)   :: dx_deg     ! y-distance (meter)
   real, intent(in)   :: lat        ! latitude

   real               :: dlat, dlon, lat1, lat2, a, c, R, npi

   npi=3.14159265358979323846
   R = 6.371229E6                   ! UM radius of the earth

   dlon = dx_deg
   dlat = 0.0
   lat1 = 0.0
   lat2 = 0.0

   a = (sin(dlat/2.0)*npi/180.0)**2 + &
       cos(lat1*npi/180.0)* cos(lat2*npi/180.0)* &
       (sin(dlon/2.0*npi/180.0))**2
   c = 2.0 * atan2(sqrt(a),sqrt(1-a))
   dx_m= R * c

   print*," um_dx_m      = ", dx_m
   print*

end subroutine da_deg2m

!------------------------------------------------------------------------------

subroutine da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )

!---------------------------------------------------------------------- 
! Purpose: Compute some factors for LAM FFT transforms
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Imported from WRF's stage 0
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(out):: n1, n2                      ! Padded dimensions (n=dim-1+pad).
   integer, intent(out):: ifax1(1:num_fft_factors)    ! FFT factors.
   integer, intent(out):: ifax2(1:num_fft_factors)    ! FFT factors.

   integer            :: n                            ! n+1 is the length of the data.
   integer            :: fft_pad1, fft_pad2           ! Range to search for efficient FFT.
   logical            :: found_magic                  ! True if 2**p 3**p 5**r dimension found..

   integer            :: fft_factors(1:num_fft_factors)! FFT factors. 

!  Ensure efficient FFT dimensions by padding if necessary:
   n1 = dim1 - 1 
   do n = n1, n1 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors ) 
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad1 = n - n1
         ifax1 = fft_factors
         exit
      end if 
   end do
   n1 = n1 + fft_pad1

   n2 = dim2 - 1
   do n = n2, n2 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors )
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad2 = n - n2
         ifax2 = fft_factors
         exit
      end if
   end do
   n2 = n2 + fft_pad2

end subroutine da_fft_initialize1

!------------------------------------------------------------------------------

subroutine da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )

!---------------------------------------------------------------------- 
! Purpose: Compute some factors for LAM FFT transforms
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Imported from WRF's stage 0
! -------- End History
!
!----------------------------------------------------------------------

!  Need to split fft_initialize as array dimensions need to be calculated first.

   implicit none

   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(out)  :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(out)  :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(out)  :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.

   integer            :: i, j                         ! Loop counters.
   real               :: const                        ! Multiplicative constant.
   real               :: coeff_nx                     ! Multiplicative constant.
   real               :: coeff_ny                     ! Multiplicative constant.
   real               :: cos_coeff_nx                 ! Multiplicative constant.
   real               :: cos_coeff_ny                 ! Multiplicative constant.

   const = -0.5 * ds * ds
   coeff_nx = pi / real(n1)
   coeff_ny = pi / real(n2)

!  Calculate spectral Del**2 coefficients for C-grid (all pts. except i=j=1):
   fft_coeffs(1,1) = 0.0 ! Not used?
   do j = 2, n2+1
      cos_coeff_ny = cos(coeff_ny * real(j - 1))
      do i = 1, n1+1
         cos_coeff_nx = cos(coeff_nx * real(i - 1))
         fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
      end do
   end do
   j = 1
   cos_coeff_ny = cos(coeff_ny * real(j - 1))
   do i = 2, n1+1
      cos_coeff_nx = cos(coeff_nx * real(i - 1))
      fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
   end do

   call da_find_fft_trig_funcs( n1, trigs1 )
   call da_find_fft_trig_funcs( n2, trigs2 )

end subroutine da_fft_initialize2

!------------------------------------------------------------------------------

subroutine da_uv_to_div_umc( dim1, dim2, dx_u, dy_v, u, v, div )
   
!----------------------------------------------------------------------
! Purpose: Calculate divergence on a co-ordinate surface, given an input
!          wind field on an Arakawa C-grid.
!  
! Note   : Zero gradient divergence boundary conditions for
!          Western, Southern and Northern boudaries
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Adaptation of Dale Barker's WRF version
! -------- End History
!
!                   du      dv
!           Vor = [----  + ---- ]
!                  dx_u    dy_v
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: dx_u(1:dim1,1:dim2)       ! Map factor - u points.
   real, intent(in)   :: dy_v(1:dim1,1:dim2-1)     ! Map factor - u points.
   real, intent(in)   :: u(1:dim1,1:dim2)          ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2-1)        ! v wind.

   real, intent(out)  :: div(1:dim1,1:dim2)           ! Divergence.

   integer            :: i, j                         ! Loop counters.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   div(:,:) = 0.0

!------------------------------------------------------------------------------
!  [2] Calculate divergence field:
!------------------------------------------------------------------------------
   do j = 2, dim2-1
      do i = 2, dim1
         div(i,j) = ( (u(i,j) - u(i-1,j)) / dx_u(i-1,j) ) +  &
                    ( (v(i,j) - v(i,j-1)) / dy_v(i,j-1) )

       end do
   end do

!  Boundary values (zero gradient):
   div(1,2:dim2-1)    = div(2,2:dim2-1)    ! West
   div(1:dim1,1)      = div(1:dim1,2)      ! South
   div(1:dim1,dim2)   = div(1:dim1,dim2-1) ! North

end subroutine da_uv_to_div_umc

!------------------------------------------------------------------------------

subroutine da_uv_to_vor_umc( dim1, dim2, dy_u, dx_v, u, v, vor )

!----------------------------------------------------------------------
! Purpose: Calculate vorticity on a co-ordinate surface, given an input
!          wind field on an Arakawa C-grid.
!
! Note   : Zero gradient vorticity boundary conditions.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Adaptation of Dale Barker's WRF version
! -------- End History
!
!                   dv      du
!           Vor = [----  - ---- ]
!                  dx_v    dy_u
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: dy_u(1:dim1,1:dim2)       ! Map factor - u points.
   real, intent(in)   :: dx_v(1:dim1,1:dim2-1)     ! Map factor - u points.
   real, intent(in)   :: u(1:dim1,1:dim2)          ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2-1)        ! v wind.

   real, intent(out)  :: vor(1:dim1+1,1:dim2+1)    ! Vorticity.

   integer            :: i, j                      ! Loop counters.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   vor(:,:) = 0.0

!------------------------------------------------------------------------------
!  [2] Calculate vorticity field:
!------------------------------------------------------------------------------
   do j = 2, dim2
      do i = 2, dim1
         vor(i,j) = ( (v(i,j-1) - v(i-1,j-1)) / dx_v(i-1,j-1) ) -  &
                    ( (u(i-1,j) - u(i-1,j-1)) / dy_u(i-1,j-1) )
          
      end do
   end do

!  Boundary values (zero gradient):
   vor(1,2:dim2)        = vor(2,2:dim2)      ! West
   vor(dim1+1,2:dim2)   = vor(dim1,2:dim2)   ! East
   vor(1:dim1+1,1)      = vor(1:dim1+1,2)    ! South
   vor(1:dim1+1,dim2+1) = vor(1:dim1+1,dim2) ! North

end subroutine da_uv_to_vor_umc

!------------------------------------------------------------------------------

subroutine da_psichi_to_uv_umc( dim1, dim2, dx_m, dy_m, dx_p, dy_p, &
                                psi, chi, u, v )

!----------------------------------------------------------------------
! Purpose: Calculate u and v wind components on an Arakawa C-grid.
!  
! History:
! Date     Author & Comment
! -------- ----------------
! 08/09/05 Dale Barker
!          Creation of F90 version.
! 15/11/10 J.-F. Caron (Met Office)
!          Adaptation to UM
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.

   real, intent(in)   :: dx_m(1:dim1,1:dim2)          ! x distance - mass points
   real, intent(in)   :: dy_m(1:dim1,1:dim2)          ! y distance - mass points
   real, intent(in)   :: dx_p(1:dim1+1,1:dim2+1)      ! x distance - vort/psi pts
   real, intent(in)   :: dy_p(1:dim1+1,1:dim2+1)      ! y distance - vort/psi pts
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velcoity potential.

   real, intent(out)  :: u(1:dim1,1:dim2)             ! v wind.
   real, intent(out)  :: v(1:dim1,1:dim2-1)           ! v wind.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! WRF dims (dummies for now).

!  u-wind component:
   its = 1
   ite = dim1-1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         u(i,j) = -1.0*( psi(i+1,j+1) - psi(i+1,j) ) / dy_p(i+1,j) + &
                  ( chi(i+1,j) - chi(i,j) ) / dx_m(i,j)
      end do
   end do

!  Remaining points on E boundaries (extrapolation):
   u(dim1,jts:jte) = 2.0 * u(dim1-1,jts:jte) - u(dim1-2,jts:jte)

!  v-wind component:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2-1

   do j = jts, jte
      do i = its, ite
         v(i,j) =  ( psi(i+1,j+1) - psi(i,j+1) ) / dx_p(i,j+1) &
                 + ( chi(i,j+1) - chi(i,j) ) / dy_m(i,j)
      end do
   end do

end subroutine da_psichi_to_uv_umc

!------------------------------------------------------------------------------

subroutine da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, & 
      trigs1, trigs2, fft_coeffs, del2a, a )

!---------------------------------------------------------------------- 
! Purpose: Invert a Laplacian using spectral technique
!
! History:
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.
   real, intent(in)   :: del2a(1:dim1,1:dim2)         ! Del**2 a.
   real, intent(out)  :: a(1:dim1,1:dim2)             ! Field a.

   integer            :: i, j                         ! Loop counters.
   integer            :: ij                           ! 1D array counter.
   integer            :: isign                        ! -1=Grid>spec, 1=Spec>Grid.
   integer            :: inc                          ! Stride between data points.
   integer            :: jump                         ! Increment between start of data vectors.
   integer            :: lot                          ! Number of data vectors.
   integer            :: n                            ! n+1 is the length of the data.
   integer            :: work_area                    ! Dimension of workspace.
   real               :: a2d(1:n1+1,1:n2+1)           ! 2D data array.
   real               :: a1d(1:(n1+1)*(n2+1))         ! 1D data array.

   work_area = ( n1 + 1 ) * ( n2 + 1 )

!  Fill 2D array structure
   do j = 1, dim2
      do i = 1, dim1
         a2d(i,j) = del2a(i,j)
      end do

!     Fill pad zone (and force b.c.s to satisfy solution type):
      if ( fft_method == 1 ) then ! Cosine transform.
         a2d(1,j) = a2d(2,j)
         do i = dim1, n1+1
            a2d(i,j) = a2d(dim1-1,j)
         end do
      else if ( fft_method == 2 ) then ! Sine transform:
         a2d(1,j) = 0.0
         do i = dim1, n1+1
            a2d(i,j) = 0.0
         end do
      end if
   end do

   if ( fft_method == 1 ) then ! Cosine transform.
      do i = 1, n1+1
         a2d(i,1) = a2d(i,2)
         do j = dim2, n2+1
            a2d(i,j) = a2d(i,dim2-1)
         end do
      end do
   else if ( fft_method == 2 ) then ! Sine transform:
      do i = 1, n1+1
         a2d(i,1) = 0.0
         do j = dim2, n2+1
            a2d(i,j) = 0.0
         end do
      end do
   end if

!  Transfer to data array:
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = a2d(i,j)
      end do
   end do

!------------------------------------------------------------------------------
!     Perform double fast sine/cosine transform to get spectral del2a:
!------------------------------------------------------------------------------

   isign = -1 ! Grid to spectral

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.
   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!------------------------------------------------------------------------------
!  Perform conversion from del2a to a in spectral space:
!------------------------------------------------------------------------------

!  Note fft_coeffs(1,1)=0 so a(k=0,l=0) is also 0.
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = fft_coeffs(i,j) * a1d(ij)
      end do
   end do

!------------------------------------------------------------------------------
!  Perform double fast sine/cosine transform to get gridpoint a:
!------------------------------------------------------------------------------

   isign = 1 ! Spectral to grid.

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!  Transfer grid-point chi to 2D-array (throwing away pad):
   do j = 1, dim2
      do i = 1, dim1
         ij = (j-1) * (n1+1) + i
         a(i,j) = a1d(ij)
      end do
   end do

end subroutine da_del2a_to_a

!---------------------------------------------------------------------------------------------

subroutine da_test_inverse( dim1, dim2, dx_u, dy_u, dx_v, dy_v, dx_m, &
      dy_m, dx_p, dy_p, &
      n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
      n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
      u1, v1, psi, chi )

!----------------------------------------------------------------------
! Purpose: Test u, v -> psi, chi calculation by performing inverse test.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 08/09/05 Dale Barker
!          Creation of F90 version.
! 23/11/10 J.-F. Caron (Met Office)
!          Adaptation to UM
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.

   real, intent(in)   :: dx_u(1:dim1,1:dim2)          ! x distance - u pts.
   real, intent(in)   :: dy_u(1:dim1,1:dim2)          ! y distance - u points.
   real, intent(in)   :: dx_v(1:dim1,1:dim2-1)        ! x distance - v points.
   real, intent(in)   :: dy_v(1:dim1,1:dim2-1)        ! y distance - v points.
   real, intent(in)   :: dx_m(1:dim1,1:dim2)          ! x distance - mass pts.
   real, intent(in)   :: dy_m(1:dim1,1:dim2)          ! y distance - mass points.
   real, intent(in)   :: dx_p(1:dim1+1,1:dim2+1)      ! x distance - psi/vort pts.
   real, intent(in)   :: dy_p(1:dim1+1,1:dim2+1)      ! y distance - psi/vort pts.

   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.

   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.

   integer, intent(in):: n1s, n2s                     ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: ifax1s(1:num_fft_factors)    ! FFT factors.
   integer, intent(in):: ifax2s(1:num_fft_factors)    ! FFT factors.

   real, intent(in)   :: trigs1s(1:3*n1)              ! FFT trig functions.
   real, intent(in)   :: trigs2s(1:3*n2)              ! FFT trig functions.
   real, intent(in)   :: fft_coeffss(1:n1+1,1:n2+1)   ! FFT coefficients.

   real, intent(in)   :: u1(1:dim1,1:dim2)            ! u
   real, intent(in)   :: v1(1:dim1,1:dim2-1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.
   real               :: u2(1:dim1,1:dim2)            ! u
   real               :: v2(1:dim1,1:dim2-1)          ! v
   real               :: u3(1:dim1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2-1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence

   write(6,'(a,i4)')' Using FFT method (1=Cosine, 2=Sine): ', fft_method

   call da_psichi_to_uv_umc( dim1, dim2, dx_m, dy_m, dx_p, dy_p, &
                             psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_umc( dim1, dim2, dx_u, dy_v, u2, v2, div )
   call da_uv_to_vor_umc( dim1, dim2, dy_u, dx_v, u2, v2, vor )

   call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                       fft_coeffs, div, chi1 )
   call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, trigs1s, trigs2s, &
                       fft_coeffss, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))


   call da_psichi_to_uv_umc( dim1, dim2, dx_m, dy_m, dx_p, dy_p, &
                             psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse

!---------------------------------------------------------------------------------------------

subroutine da_get_rh_um( input_file, dim1, dim2, dim3, k, pheight, theight, rh )

!---------------------------------------------------------------------
! Purpose: Calculates RH on THETA levels.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file         ! 1 file name.

   integer,            intent(in)  :: dim1, dim2, dim3   ! Dimensions.
   integer,            intent(in)  :: k                  ! Model level.

   real,               intent(in) :: pheight(1:dim3+1)   ! pressure height
   real,               intent(in) :: theight(1:dim3)     ! temperature height

   real,               intent(out) :: rh(1:dim1,1:dim2)  ! Relative humidity.

   character(len=12) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: theta(1:dim1,1:dim2)      ! Potential temperature.
   real              :: p_below(1:dim1,1:dim2)    ! pressure below theta level
   real              :: p_above(1:dim1,1:dim2)    ! pressure above theta level
   real              :: q(1:dim1,1:dim2)          ! Specific humidity.
   real              :: temp(1:dim1,1:dim2)       ! Temperature.

   real              :: p                         ! Pressure.
   real              :: t_c                       ! Temp(Celsius).
   real              :: es                        ! Saturation vapor pressure.
   real              :: qs                        ! Saturation specific humidity.

   var = "theta" ! Potential temperature
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, theta)

   var = "p" ! pressure on rho levels.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k+1, p_above)
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, p_below)

   var = "q"  ! specific humidity
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, q)

   do j = 1, dim2
      do i = 1, dim1

         ! Interpolate pressure at rho level to theta level
         p = p_below(i,j) + ( (p_above(i,j) - p_below(i,j)) * &
                              (theight(k)-pheight(k)) /      &
                              (pheight(k+1)-pheight(k)) )

         temp(i,j) = theta(i,j) *( p/1.0E5)**kappa ! Theta to T.

         ! Calculate relative humidity:
         t_c = temp(i,j) - t_kelvin
         es = es_alpha * exp( es_beta * t_c /( t_c + es_gamma))
         qs = rd_over_rv * es /( p - rd_over_rv1 * es)
         rh(i,j) = q(i,j) / qs

      end do
   end do

end subroutine da_get_rh_um

!---------------------------------------------------------------------------------------------

subroutine da_get_trhol_um( input_file, dim1, dim2, dim3, k, pheight, theight, temp)

!---------------------------------------------------------------------
! Purpose: Calculates T on rho level
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file           ! 1 file name.

   integer,            intent(in)  :: dim1, dim2, dim3     ! Dimensions.
   integer,            intent(in)  :: k                    ! Model level.

   real,               intent(in) :: pheight(1:dim3+1)     ! rho height
   real,               intent(in) :: theight(1:dim3)       ! theta height

   real,               intent(out) :: temp(1:dim1,1:dim2)  ! Temperature.

   character(len=12) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: rho(1:dim1,1:dim2)        ! density.
   real              :: q_below(1:dim1,1:dim2)    ! q below theta level
   real              :: q_above(1:dim1,1:dim2)    ! q above theta level
   real              :: p(1:dim1,1:dim2)          ! pressure

   real              :: q                         ! specific humidity at rho levels
   real              :: R                         ! radius of the earth

   R = 6.371229E6       ! UM value

   var = "p" ! pressure on rho levels
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, p)

   var = "unspecified" ! rho * R * R
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, rho)

   var = "q" ! ! specific humidity
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, q_above)
   if (k .ne. 1) then
      call da_get_field( input_file, var, 3, dim1, dim2, dim3, k-1, q_below)
   endif

   do j = 1, dim2
      do i = 1, dim1

         rho(i,j)=rho(i,j)/(R**2)   ! Convert rho*R*R to rho

         ! Interpolate specific humidity at theta level to rho level
         if (k .ne. 1) then
            q = q_below(i,j) + ( (q_above(i,j) - q_below(i,j)) * &
                               (pheight(k)-theight(k-1)) /      &
                               (theight(k)-theight(k-1)) )
         else
            q = q_above(i,j)
         endif

         temp(i,j) = p(i,j) / ( rho(i,j) * gas_constant * (1.0+0.608*q) )

      end do
   end do

end subroutine da_get_trhol_um

!---------------------------------------------------------------------------------------------

subroutine da_hydro_p_pert_um( hp_p, pres_p, pres, theta_p, theta, q_p, q, &
                               pheight, dim1, dim2, dim3)

!---------------------------------------------------------------------
! Purpose: Calculates hydrostatic pressure perturbations
!
! Note   : theta is assumed to be on theta levels and pressure on rho levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! 25/05/11 J.-F. Caron (Met Office)
!          Use 1D working arrays instead of 3D arrays to save memory space
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in) :: dim1, dim2, dim3              ! Dimensions.

   real,               intent(in) :: pheight(1:dim3+1)             ! rho height

   real,               intent(in) :: pres_p(1:dim1,1:dim2,1:dim3)  ! Pressure perts.
   real,               intent(in) :: pres(1:dim1,1:dim2,1:dim3)    ! Bkgd presure.
   real,               intent(in) :: theta_p(1:dim1,1:dim2,1:dim3) ! Theta perts.
   real,               intent(in) :: theta(1:dim1,1:dim2,1:dim3)   ! Bkgd theta.
   real,               intent(in) :: q_p(1:dim1,1:dim2,1:dim3)     ! q perts.
   real,               intent(in) :: q(1:dim1,1:dim2,1:dim3)       ! Bkgd q.

   real,               intent(out) :: hp_p(1:dim1,1:dim2,1:dim3)   ! Hydrostatic pres perts.

   integer           :: i, j, k                           ! Loop counters.

   real              :: thetav_p(1:dim3)    ! Virtual pot temp perts.
   real              :: thetav(1:dim3)      ! Bkgd virtual pot temp.
   real              :: exnh_p(1:dim3)      ! Hydro Exner function perts.
   real              :: exnh(1:dim3)        ! Bkgd hydro exner function.
   real              :: hp(1:dim3)          ! Bkgd hydrostatic pressure.
   
   real              :: pref,d, cv

   pref=1000.0*100.0  ! reference pressure in Pa
   cv=0.608

   do j = 1, dim2
      do i = 1, dim1

         ! Compute background virtual temperature
         do k = 1, dim3
            thetav(k) = theta(i,j,k) * ( 1.0 + cv * q(i,j,k) )
         end do
         
         ! Compute background hydrostatic pressure
         do k = 1, dim3
            if (k.eq.1) then
               exnh(k) =  ( pres(i,j,k) / pref)**kappa
            else
               d = pheight(k) - pheight(k-1)
               exnh(k) =  exnh(k-1) - ( gravity * d ) / ( cp * thetav(k-1) )
            endif
            hp(k) = pref * ( exnh(k)**(1.0/kappa) )
         end do

         ! Compute linearized virtual temperature perturbations
         do k = 1, dim3
            thetav_p(k) = theta_p(i,j,k) * ( 1.0 + cv * q(i,j,k) ) + &
                              theta(i,j,k) * cv * q_p(i,j,k)
         end do

         ! Compute linearized hydrostatic pressure perturbations
         do k = 1, dim3
            if (k.eq.1) then
               exnh_p(k) =  kappa * exnh(k) * ( pres_p(i,j,k) / pres(i,j,k) )
            else
               d = pheight(k) - pheight(k-1)
               exnh_p(k) =  exnh_p(k-1) + ( gravity * d * thetav_p(k-1)) &
                                                / ( cp * thetav(k-1)**2 )
            endif
            hp_p(i,j,k) = ( exnh_p(k) * hp(k) ) /  &
                          ( kappa * exnh(k) )
         end do

      end do
   end do

end subroutine da_hydro_p_pert_um

!------------------------------------------------------------------------------

subroutine da_linbal_um( lbpres_p, psi_p, rho, &
                               lat, dim1, dim2, dim3)

!---------------------------------------------------------------------- 
! Purpose: Calculates linearized balance pressure perturbations
!
! Note   : Linear balance : lbpres_p = rho * f * psi_p
!          All data assumed to be on the same levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/12/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in) :: dim1, dim2, dim3             ! Dimensions.

   real,               intent(in) :: psi_p(1:dim1,1:dim2,1:dim3)  ! Psi perts.
   real,               intent(in) :: rho(1:dim1,1:dim2,1:dim3)    ! Bkgd density..
   real,               intent(in) :: lat(1:dim1,1:dim2)           ! Latitude.

   real,               intent(out) :: lbpres_p(1:dim1,1:dim2,1:dim3) ! balance pres perts.

   integer           :: i, j, k                           ! Loop counters.

   real              :: coriol(1:dim1,1:dim2)    ! Coriolis.

   ! Compute coriolis parameter
   do j = 1, dim2
      do i = 1, dim1
         coriol(i,j) = 2.0 * earth_omega * sin( lat(i,j)*radian )
      end do
   end do

   ! Compute simplified linear balance
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            lbpres_p(i,j,k) = rho(i,j,k) * coriol(i,j) * psi_p(i,j,k) 
         end do
      end do
   end do

end subroutine da_linbal_um

!----------------------------------------------------------------------------------------

subroutine da_get_um_height( pheight, theight, fileplev, filetlev, dim3, &
                             iunit)

!---------------------------------------------------------------------- 
! Purpose: Read height of UM levels and check ordering
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------
   
   implicit none
   
   character(len=filename_len), intent(in)  :: fileplev   ! file name.
   character(len=filename_len), intent(in)  :: filetlev   ! file name.
   integer,            intent(in)  :: iunit               ! Input unit.
   integer,            intent(in)  :: dim3                ! Dimensions.
   real,               intent(out) :: pheight(1:dim3+1)   ! Height.
   real,               intent(out) :: theight(1:dim3)     ! Height.
 

   character (len=filename_len)   :: input_file           ! Input file.
   integer           :: k                                 ! Loop counter.

   input_file = trim(fileplev)
   open (gen_be_iunit, file = input_file)
   do k = 1, dim3+1
      read(gen_be_iunit,*)pheight(k)
      print*,k,pheight(k)
   end do
   close(gen_be_iunit)

   input_file = trim(filetlev)
   open (gen_be_iunit, file = input_file)
   do k = 1, dim3
      read(gen_be_iunit,*)theight(k)
      print*,k,theight(k)
   end do
   close(gen_be_iunit)

   ! test #1
   do k = 1, dim3
      if (pheight(k).ge.pheight(k+1)) then
         print*
         print*,"Error in pressure heights, h(k) >= h(k+1)", k, pheight(k), pheight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

   ! test #2
   do k = 1, dim3-1
      if (theight(k).ge.theight(k+1)) then
         print*
         print*,"Error in temperature heights, h(k) >= h(k+1)", k, theight(k), theight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

   ! test #3   
   do k = 1, dim3
      if ( (theight(k).le.pheight(k)) .or. (theight(k).ge.pheight(k+1)) ) then
         print*
         print*,"T height is not between P heights", k, pheight(k), theight(k), pheight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

end subroutine da_get_um_height

!---------------------------------------------------------------------------------------------

subroutine da_pres_rho2theta_1d( pres_theta, pres_rho, pheight, theight, dim3 )

!---------------------------------------------------------------------- 
! Purpose: Interpolate pressure from RHO levels to THETA levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/12/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in)  :: dim3                ! Dimensions.

   real,               intent(in) :: pheight(1:dim3+1)    ! pressure height
   real,               intent(in) :: theight(1:dim3)      ! temperature height

   real,               intent(in) :: pres_rho(1:dim3+1)   ! field on rho levels
   real,               intent(out) :: pres_theta(0:dim3)  ! field on theta level.

   integer           :: k                      ! Loop counters.

   do k = 1, dim3
      pres_theta(k) = pres_rho(k) + ( (pres_rho(k+1) - pres_rho(k) ) * &
                              (theight(k)-pheight(k)) /                  &
                              (pheight(k+1)-pheight(k)) )
   end do

   ! extrapolation  below theta surface assuming :
   !                         theight(0) = pheight(1) - (theight(1)-pheight(1)) 
   pres_theta(0) = ( (pres_theta(1)-pres_rho(1)) *           &
                    2.0 * (pheight(1)-theight(1)) /                   &
                    (theight(1)-pheight(1)) ) + pres_theta(1)

end subroutine da_pres_rho2theta_1d

!----------------------------------------------------------------------------------------

#ifdef crayx1

   subroutine getarg(i, harg)
     implicit none
     character(len=*) :: harg
     integer :: ierr, ilen, i

     call pxfgetarg(i, harg, ilen, ierr)
     return
   end subroutine getarg
#endif

end program gen_be_stage0_um

