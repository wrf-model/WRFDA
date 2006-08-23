program gen_be_stage0_wrf
!
!---------------------------------------------------------------------- 
!  Purpose : To convert WRF output to "standard perturbation fields"
!  required by the WRF-Var BE covariance generation utility "gen_be".
!
!  Input  : WRF forecasts for a specified date (NETCDF format). 
!
!  Output : Binary files for use in gen_be_stage1.
!
!  Author: Dale Barker (NCAR/MMM)
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_constants
   use da_gen_be
   use da_tracing

   implicit none

   integer, parameter    :: nrange = 50               ! Range to search for efficient FFT.

   character (len=200)   :: filestub                  ! General filename stub.
   character (len=200)   :: input_file                ! Input file. 
   character (len=200)   :: output_file               ! Output file. 
   character (len=10)    :: date                      ! Character date.
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: be_method                 ! "NMC" or "ENS".
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: dim1                      ! Dimensions of grid (T points).
   integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim2                      ! Dimensions of grid (T points).
   integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim3                      ! Dimensions of grid (T points).
   integer               :: n1, n2                    ! Padded dimensions (n=dim-1+pad).
   integer               :: n1s, n2s                  ! Padded dimensions (n=dim-1+pad).
   integer               :: poisson_method            ! 1=Spectral, 2=SOR.
   integer               :: fft_method                ! For poisson_method=1: 1=FCT, 2=FST.
   integer               :: ktest                     ! Test level.
   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution.
   real                  :: dim12_inv_u               ! 1 / (dim1*dim2).
   real                  :: dim12_inv_v               ! 1 / (dim1*dim2).
   real                  :: dim12_inv                 ! 1 / (dim1*dim2).
   logical               :: test_inverse              ! Test conversion by performing inverse.
   logical               :: remove_mean               ! Remove mean from standard fields.

   integer               :: ifax1(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax2(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax1s(1:num_fft_factors) ! FFT factors.
   integer               :: ifax2s(1:num_fft_factors) ! FFT factors.
   real, allocatable     :: xlat(:,:)                 ! Latitude of mass points.
   real, allocatable     :: mapfac_m(:,:)             ! Map factor - mass pts.
   real, allocatable     :: mapfac_u(:,:)             ! Map factor - u points.
   real, allocatable     :: mapfac_v(:,:)             ! Map factor - v points.

   real, allocatable     :: u(:,:)                    ! u-wind.
   real, allocatable     :: v(:,:)                    ! v-wind.
   real, allocatable     :: div(:,:)                  ! Divergence.
   real, allocatable     :: vor(:,:)                  ! Vorticity.
   real, allocatable     :: psi2d(:,:)                ! Streamfunction copy. 
   real, allocatable     :: chi2d(:,:)                ! Velocity potential copy.
   real, allocatable     :: temp2d(:,:)               ! Temperature.
   real, allocatable     :: rh2d(:,:)                 ! Relative humidity.

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
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: height(:,:,:)             ! Height.
   real, allocatable     :: psi_store(:,:,:)          ! Streamfunction.
   real, allocatable     :: chi_store(:,:,:)          ! Velocity Potential.
   real, allocatable     :: temp_store(:,:,:)         ! Temperature.
   real, allocatable     :: rh_store(:,:,:)           ! Relative humidity.
   real, allocatable     :: psfc_store(:,:)           ! Surface pressure.
   real, allocatable     :: height_store(:,:,:)       ! Height.

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [1] Initialize information.'
!---------------------------------------------------------------------------------------------

   if (trace_use) call da_trace_init
   if (trace_use) call da_trace_entry("gen_be_stage0_wrf")

   test_inverse = .true. 
   ktest = 1
   poisson_method = 1    
   fft_method = 2
   remove_mean = .true.

   numarg = iargc()
   if ( numarg /= 4 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_stage0_wrf be_method date ne <wrf_file_stub> Stop"
      stop
   end if

   call getarg( 1, be_method )
   call getarg( 2, date )
   call getarg( 3, cne )
   read(cne,'(i3)')ne
   call getarg( 4, filestub )

   if ( be_method == "NMC" ) then
      write(6,'(a,a)')' Computing gen_be NMC-method forecast difference files for date ', date
      ne = 2                      ! NMC-method uses differences between 2 forecasts.
      remove_mean = .false.
   else if ( be_method == "ENS" ) then
      if ( remove_mean ) then
         write(6,'(a,a)')' Computing gen_be ensemble perturbation files for date ', date
      else
         write(6,'(a,a)')' Computing gen_be ensemble forecast files for date ', date
      end if
      write(6,'(a,i4)')' Ensemble Size = ', ne
   else
      write(6,'(a,a)')trim(be_method), ' is an invalid value of be_method. Stop'
      stop
   end if
   write(6,'(a,a)')' Input filestub = ', trim(filestub)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Setup up ancillary fields using 1st member values.' 
!---------------------------------------------------------------------------------------------

   var = "T"
   input_file = trim(filestub)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )
   dim1s = dim1+1 ! Vorticity/streamfunction array 1 larger.
   dim2s = dim2+1 ! Vorticity/streamfunction array 1 larger.
   dim12_inv_u = 1.0 / real((dim1+1) * dim2)
   dim12_inv_v = 1.0 / real(dim1 * (dim2+1))
   dim12_inv = 1.0 / real(dim1 * dim2)

   allocate( xlat(1:dim1,1:dim2) )
   allocate( mapfac_m(1:dim1,1:dim2) )
   allocate( mapfac_u(1:dim1+1,1:dim2) )
   allocate( mapfac_v(1:dim1,1:dim2+1) )

   var = "XLAT"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, xlat )
   var = "MAPFAC_M"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, mapfac_m )
   var = "MAPFAC_U"
   call da_get_field( input_file, var, 2, dim1+1, dim2, 1, 1, mapfac_u )
   var = "MAPFAC_V"
   call da_get_field( input_file, var, 2, dim1, dim2+1, 1, 1, mapfac_v )

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
   allocate( psfc(1:dim1,1:dim2) )
   allocate( height(1:dim1,1:dim2,1:dim3) )

   allocate( psi_store(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( chi_store(1:dim1,1:dim2,1:dim3) )
   allocate( temp_store(1:dim1,1:dim2,1:dim3) )
   allocate( rh_store(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_store(1:dim1,1:dim2) )
   allocate( height_store(1:dim1,1:dim2,1:dim3) )
   psi_store = 0.0
   chi_store = 0.0
   temp_store = 0.0
   rh_store = 0.0
   psfc_store = 0.0
   height_store = 0.0

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Convert WRF forecast fields to standard fields and output' 
!---------------------------------------------------------------------------------------------

   ! Allocate temporary arrays:
   allocate( u(1:dim1s,1:dim2) )
   allocate( v(1:dim1,1:dim2s) )
   allocate( vor(1:dim1s,1:dim2s) )
   allocate( div(1:dim1,1:dim2) )
   allocate( psi2d(1:dim1s,1:dim2s) )
   allocate( chi2d(1:dim1,1:dim2) )
   allocate( temp2d(1:dim1,1:dim2) )
   allocate( rh2d(1:dim1,1:dim2) )

   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      do k = 1, dim3

         ! Read u, v:
         var = "U"
         call da_get_field( input_file, var, 3, dim1s, dim2, dim3, k, u )
         var = "V"
         call da_get_field( input_file, var, 3, dim1, dim2s, dim3, k, v )

         ! Calculate vorticity (in center of mass grid on WRF's Arakawa C-grid):
         call da_uv_to_vor_c( dim1, dim2, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, vor )

         ! Calculate divergence (at mass pts. on WRF's Arakawa C-grid):

         call da_uv_to_div_c( dim1, dim2, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, div )

         ! Calculate streamfunction and potential 
         ! Assumes vor/div converted to Del**2 psi/chi):

         if ( poisson_method == 1 ) then
            call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, &
                                trigs1s, trigs2s, fft_coeffss, vor, psi2d )

            call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, &
                                trigs1, trigs2, fft_coeffs, div, chi2d )
         else if ( poisson_method == 2 ) then
            call da_sor( dim1s, dim2s, ds, vor, psi2d )
            call da_sor( dim1, dim2, ds, div, chi2d )
         end if

         if ( test_inverse .and. k == ktest .and. member == 1 ) then
            call da_test_inverse( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                                  n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                                  n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                                  u, v, psi2d, chi2d )
         end if

!        Interpolate psi to mass pts ready for output:
         do j = 1, dim2
            do i = 1, dim1
               psi(i,j,k) = 0.25 * ( psi2d(i,j) + psi2d(i+1,j) + &
                                     psi2d(i,j+1) + psi2d(i+1,j+1) )
            end do
         end do
         chi(:,:,k) = chi2d(:,:)

!        Read mass fields, and convert to T and rh:

         call da_get_trh( input_file, dim1, dim2, dim3, k, temp2d, rh2d )
         temp(:,:,k) = temp2d(:,:)
         rh(:,:,k) = 0.01 * rh2d(:,:) ! *0.01 to conform with WRF-Var units.

      end do

      call da_get_height( input_file, dim1, dim2, dim3, height )

!     Finally, extract surface pressure:
      var = "PSFC"
      call da_get_field( input_file, var, 2, dim1, dim2, dim3, 1, psfc )

!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)psfc
      write(gen_be_ounit)height
      write(gen_be_ounit)xlat
      close(gen_be_ounit)

!     Optionally calculate accumulating mean:
      if ( remove_mean ) then
         member_inv = 1.0 / real(member)
         psi_store = ( real( member-1 ) * psi_store + psi ) * member_inv
         chi_store = ( real( member-1 ) * chi_store + chi ) * member_inv
         temp_store = ( real( member-1 ) * temp_store + temp ) * member_inv
         rh_store = ( real( member-1 ) * rh_store + rh ) * member_inv
         psfc_store = ( real( member-1 ) * psfc_store + psfc ) * member_inv
         height_store = ( real( member-1 ) * height_store + height ) * member_inv
      end if

   end do

   deallocate( u )
   deallocate( v )
   deallocate( vor )
   deallocate( div )
   deallocate( psi2d )
   deallocate( chi2d )
   deallocate( temp2d )
   deallocate( rh2d )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( be_method == "NMC" ) then
      write(6,'(/a)')'    Compute perturbation as a difference between two forecasts' 

!     Re-read input forecast standard fields (ne=2 hard-wired above for NMC-method):
      input_file = 'tmp.e001'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi
      read(gen_be_iunit)chi
      read(gen_be_iunit)temp
      read(gen_be_iunit)rh
      read(gen_be_iunit)psfc
      read(gen_be_iunit)height
      read(gen_be_iunit)xlat
      close(gen_be_iunit)

      input_file = 'tmp.e002'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi_store
      read(gen_be_iunit)chi_store
      read(gen_be_iunit)temp_store
      read(gen_be_iunit)rh_store
      read(gen_be_iunit)psfc_store
      read(gen_be_iunit)height_store
      read(gen_be_iunit)xlat
      close(gen_be_iunit)

!     Take forecast difference:
      psi = psi - psi_store
      chi = chi - chi_store
      temp = temp - temp_store
      rh  = rh - rh_store
      psfc = psfc - psfc_store
      height = height - height_store

!     Write out NMC-method standard perturbations:
      output_file = 'pert.'//date(1:10)//'.e001'
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)psfc
      write(gen_be_ounit)height
      write(gen_be_ounit)xlat
      close(gen_be_ounit)

   else ! be_method = "ENS"

      if ( remove_mean ) then
         write(6,'(/a)') "[4] Convert ensemble of standard fields to perturbations"
      else
         write(6,'(/a)') "[4] WARNING: Outputting ensemble member standard fields in pert files!"
      end if

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
         read(gen_be_iunit)psfc
         read(gen_be_iunit)height
         read(gen_be_iunit)xlat
         close(gen_be_iunit)

         if ( remove_mean ) then
            psi = psi - psi_store
            chi = chi - chi_store
            temp = temp - temp_store
            rh  = rh - rh_store
            psfc = psfc - psfc_store
            height = height - height_store
         end if

!        Write out standard perturbations for this member:
         output_file = 'pert.'//date(1:10)//'.e'//ce  
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)date, dim1, dim2, dim3
         write(gen_be_ounit)psi
         write(gen_be_ounit)chi
         write(gen_be_ounit)temp
         write(gen_be_ounit)rh
         write(gen_be_ounit)psfc
         write(gen_be_ounit)height
         write(gen_be_ounit)xlat
         close(gen_be_ounit)
      end do
   end if

   deallocate( psi )
   deallocate( chi )
   deallocate( temp )
   deallocate( rh )
   deallocate( psfc )
   deallocate( height )
   deallocate( xlat )
   deallocate( psi_store )
   deallocate( chi_store )
   deallocate( temp_store )
   deallocate( rh_store )
   deallocate( psfc_store )
   deallocate( height_store )

   if (trace_use) call da_trace_exit("gen_be_stage0_wrf")
   if (trace_use) call da_trace_report

CONTAINS

!------------------------------------------------------------------------------

subroutine da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(out):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(out):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(out):: ifax2(1:num_fft_factors)     ! FFT factors.

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

!  Need to split fft_initialize as array dimensions need to be calculated first.

   implicit none

   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(out)  :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(out)  :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(out)  :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.

   integer            :: n                            ! n+1 is the length of the data.
   integer            :: fft_pad1, fft_pad2           ! Range to search for efficient FFT.
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

subroutine da_uv_to_div_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, & 
                           u, v, div )
   
!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: No boundary conditions required on the WRF Arakawa C-grid as
!        divergence (mass) points are all within the outer u/v pts.
!
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!                        d   U      d   V
!           Div = m^2 *[---(---) + ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)           ! v wind.
   real, intent(out)  :: div(1:dim1,1:dim2)           ! Divergence.

   integer            :: i, j                         ! Loop counters.
   real               :: ds_inv                       ! 1/ds.
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)          ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)          ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         coeff(i,j) = ( mapfac_m(i,j) * mapfac_m(i,j) ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate divergence field:
!------------------------------------------------------------------------------

   do j = 1, dim2
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1
         vm(i,j) = v(i,j) / mapfac_v(i,j)
      end do
   end do

   do j = 1, dim2
      do i = 1, dim1
         div(i,j) = coeff(i,j) * ( um(i+1,j) - um(i,j) + vm(i,j+1) - vm(i,j) )
      end do
   end do

end subroutine da_uv_to_div_c

subroutine da_uv_to_vor_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, &
                           u, v, vor )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate vorticity on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: Zero vorticity boundary conditions.
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!                        d   V      d   U
!           Vor = m^2 *[---(---) - ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)           ! v wind.
   real, intent(out)  :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   integer            :: i, j                         ! Loop counters.
   real               :: ds_inv                       ! 1/ds.
   real               :: mapfac_vor                   ! Map factor (vorticity pts).
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)          ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)          ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   vor(:,:) = 0.0

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         mapfac_vor = 0.25 * ( mapfac_u(i,j-1) + mapfac_u(i,j) + &
!                               mapfac_v(i-1,j) + mapfac_v(i,j) ) ! Average.
!         coeff(i,j) = ( mapfac_vor * mapfac_vor ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate vorticity field:
!------------------------------------------------------------------------------

   do j = 1, dim2  
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1  
         vm(i,j) = v(i,j) / mapfac_v(i,j)
      end do
   end do

   do j = 2, dim2
      do i = 2, dim1
         vor(i,j) = coeff(i,j) * ( vm(i,j) - vm(i-1,j) - um(i,j) + um(i,j-1) )
      end do
   end do

!  Boundary values (extrapolation):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
!   vor(1,1:dim2+1)      = 2.0 * vor(2,1:dim2+1) - vor(3,1:dim2+1)         ! West
!   vor(dim1+1,1:dim2+1) = 2.0 * vor(dim1,1:dim2+1) - vor(dim1-1,1:dim2+1) ! East
!   vor(1:dim1+1,1)      = 2.0 * vor(1:dim1+1,2) - vor(1:dim1+1,3)         ! South
!   vor(1:dim1+1,dim2+1) = 2.0 * vor(1:dim1+1,dim2) - vor(1:dim1+1,dim2-1) ! South

!  Boundary values (zero gradient):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
   vor(1,2:dim2)        = vor(2,2:dim2)      ! West
   vor(dim1+1,2:dim2)   = vor(dim1,2:dim2)   ! East
   vor(1:dim1+1,1)      = vor(1:dim1+1,2)    ! South
   vor(1:dim1+1,dim2+1) = vor(1:dim1+1,dim2) ! South

end subroutine da_uv_to_vor_c

subroutine da_psichi_to_uv_c( dim1, dim2, ds, &
                              mapfac_u, mapfac_v, &
                              psi, chi, u, v )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate u and v wind components on an Arakawa C-grid.
!  
!  NOTE: 
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velcoity potential.
   real, intent(out)  :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(out)  :: v(1:dim1,1:dim2+1)           ! v wind.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! WRF dims (dummies for now).
   real               :: ds_inv                       ! 1/ds.
   real               :: one_third                    ! 1/3.

   ds_inv = 1.0 / ds
   one_third = 1.0 / 3.0

!  u-wind component:
   its = 2
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         u(i,j) = mapfac_u(i,j) * ds_inv * &
                  ( -psi(i,j+1) + psi(i,j) + chi(i,j) - chi(i-1,j) )
      end do
   end do

!  Remaining points on E/W boundaries (extrapolation):
   u(1,jts:jte) = 2.0 * u(2,jts:jte) - u(3,jts:jte)
   u(dim1+1,jts:jte) = 2.0 * u(dim1,jts:jte) - u(dim1-1,jts:jte)

!  v-wind component:
   its = 1
   ite = dim1
   jts = 2
   jte = dim2

   do j = jts, jte
      do i = its, ite
         v(i,j) = mapfac_v(i,j) * ds_inv * &
                  ( psi(i+1,j) - psi(i,j) + chi(i,j) - chi(i,j-1) )
      end do
   end do

!  Remaining points on S/N boundaries (extrapolation):
   v(its:ite,1) = 2.0 * v(its:ite,2) - v(its:ite,3)
   v(its:ite,dim2+1) = 2.0 * v(its:ite,dim2) - v(its:ite,dim2-1)

end subroutine da_psichi_to_uv_c

!------------------------------------------------------------------------------

subroutine da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                          fft_coeffs, del2a, a )

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
      call da_fast_cosine_transform( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call da_fast_sine_transform( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call da_fast_cosine_transform( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call da_fast_sine_transform( isign, inc, jump, lot, n, &
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
      call da_fast_cosine_transform( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call da_fast_sine_transform( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call da_fast_cosine_transform( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call da_fast_sine_transform( isign, inc, jump, lot, n, &
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
subroutine da_test_inverse( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                            n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                            n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                            u1, v1, psi, chi )

!------------------------------------------------------------------------------
!  PURPOSE: Test u, v -> psi, chi calculation by performing inverse test.
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
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

   real, intent(in)   :: u1(1:dim1+1,1:dim2)          ! u
   real, intent(in)   :: v1(1:dim1,1:dim2+1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   real               :: u2(1:dim1+1,1:dim2)          ! u
   real               :: v2(1:dim1,1:dim2+1)          ! v
   real               :: u3(1:dim1+1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2+1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence

   write(6,'(a,i4)')' Using FFT method (1=Cosine, 2=Sine): ', fft_method

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, div )
   call da_uv_to_vor_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, vor )

   call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                       fft_coeffs, div, chi1 )
   call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, trigs1s, trigs2s, &
                       fft_coeffss, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse

!---------------------------------------------------------------------------------------------

subroutine da_test_inverse2( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                            u1, v1, psi, chi )

!------------------------------------------------------------------------------
!  PURPOSE: Test u, v -> psi, chi calculation by performing inverse test.
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.

   real, intent(in)   :: u1(1:dim1+1,1:dim2)          ! u
   real, intent(in)   :: v1(1:dim1,1:dim2+1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   real               :: u2(1:dim1+1,1:dim2)          ! u
   real               :: v2(1:dim1,1:dim2+1)          ! v
   real               :: u3(1:dim1+1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2+1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence

   write(6,'(a,i4)')' Using SOR method'

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, div )
   call da_uv_to_vor_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, vor )

   call da_sor( dim1, dim2, ds, div, chi1 )
   call da_sor( dim1s, dim2s, ds, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse2

!------------------------------------------------------------------------------

!DECK FFT551
!     SUBROUTINE 'FFT551' - MULTIPLE FAST COSINE TRANSFORM
!
!     AUTHOR: CLIVE TEMPERTON, MAY 1988
!     [ALL-FORTRAN VERSION: C.T., OCTOBER 1995]
!
!     COSINE TRANSFORM OF LENGTH N IS CONVERTED TO
!     REAL PERIODIC TRANSFORM OF LENGTH N BY PRE- AND POST-
!     PROCESSING. REAL PERIODIC TRANSFORM IS PERFORMED BY
!     PRUNING REDUNDANT OPERATIONS FROM COMPLEX TRANSFORM.
!
!     SEE FOR EXAMPLE PAUL SWARZTRAUBER, "SYMMETRIC FFT'S",
!     MATH. COMP. 47 (1986), 323-346.
!
!     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*MIN(LOT,64)
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N+1 IS THE LENGTH OF THE DATA VECTORS
!        (WHICH INCLUDE NONZERO VALUES AT BOTH ENDPOINTS)
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:   Z(0) , Z(1) , Z(2) , ... , Z(N)
!
!     ORDERING OF DATA:           X(0) , X(1) , X(2) , ... , X(N)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS
!     IN PARALLEL
!
!     N MUST BE COMPOSED OF FACTORS 2,3 & 5 AND MUST BE EVEN
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(I)=SUM(J=0,...,N)(E(J)*Z(J)*COS(I*J*PI/N))
!                    WHERE E(J)=0.5 FOR J=0,N --- ELSE E(J)=1
!
!     ISIGN=-1: Z(J)=(2/N)*SUM(I=0,...,N)(E(I)*X(I)*COS(I*J*PI/N))
!
! N.B.  FFT551 has an unusual definition of the FFTs,
!       such that the the coeff of wave0 is NOT the mean.
!
!---------------------------------------------------------------------
Subroutine DA_Fast_Cosine_Transform & ! in
 ( ISIGN,                & ! in
   INC,                  & ! in
   JUMP,                 & ! in
   LOT,                  & ! in
   N,                    & ! in
   IFAX,                 & ! in
   TRIGS,                & ! in
   A,                    & ! inout
   IDIM )                   ! in

! Code Description:  ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for 
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data 

 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array                                       !  vectors  (which include zeros 
                                           ! at the endpoints)
 Integer , intent (in)    :: IDIM           ! dimension workspace 

 Real :: WORK(IDIM)                      ! size (n+1)*min(lot,VectorLength)
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K, IC, J, LA, IGO, JA,JB,IA,IB
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: CO,S, t1,t2,si,scale, vectorlength

CHARACTER (LEN=*), PARAMETER :: RoutineName = "Var_FFT551"

      VectorLength = LOT
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      DO 200 NB=1,NBLOX
!
!     PREPROCESSING
!     -------------
      IA=ISTART
      IB=IA+NH*INC
      IC=IA+N*INC
      JA=1
      JB=NH+1
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 105 J=1,NVEX
         T1=0.5*(A(IA)+A(IC))
         T2=0.5*(A(IA)-A(IC))
         A(IA)=T1
         A(IC)=T2
         IA=IA+JUMP
         IC=IC+JUMP
  105    CONTINUE
      ELSE  
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA)=0.5*(A(IA)+A(IC))
         WORK(JB)=A(IB)
         A(IC)=0.5*(A(IA)-A(IC))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ENDIF
!
      DO 130 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      IC=ISTART+N*INC
      SI=TRIGS(2*N+K)
      CO=TRIGS(2*N+NH-K)
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         T2 = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
  115    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 120 J=1,NVEX
         WORK(JA) = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         WORK(JB) = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  120    CONTINUE
      ENDIF
  130 CONTINUE
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
      IGO=1-2*MOD(NFAX,2)
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL DA_QPASSM(WORK,WORK(IFAC*LA+1),A(IA),A(IA+LA*INC), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL DA_QPASSM(A(IA),A(IA+IFAC*LA*INC),WORK,WORK(LA+1), &
                    TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      ENDIF
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
  140 CONTINUE
!
!     POSTPROCESSING
!     --------------
      SCALE=2.0
      IF (ISIGN.EQ.+1) SCALE = FLOAT(N)
      S=1.0
      IF (ISIGN.EQ.-1) S = 2.0/FLOAT(N)
      JA=ISTART
      JB=JA+N*INC
      IA=1
      IB=N
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=S*A(JB)
      A(JB)=SCALE*WORK(IB)
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE
      GO TO 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(6,520) NVEX
  520 FORMAT('VECTOR LENGTH =',I4,', GREATER THAN VectorLength')
      GO TO 570
  530 CONTINUE
      WRITE(6,540) IFAC
  540 FORMAT('FACTOR =',I3,', NOT CATERED FOR')
      GO TO 570
  550 CONTINUE
      WRITE(6,560) IFAC
  560 FORMAT('FACTOR =',I3,', ONLY CATERED FOR IF LA*IFAC=N')
  570 CONTINUE

      RETURN
      END SUBROUTINE DA_Fast_Cosine_Transform
!----------------------------------------------------------------------------------------------------------

Subroutine DA_Fast_Sine_Transform  & ! in 
 ( ISIGN,              & ! in
   INC,                & ! in
   JUMP,               & ! in
   LOT,                & ! in
   N,                  & ! in
   IFAX,               & ! in
   TRIGS,              & ! in
   A,                  & ! inout
   DIM )                 ! in
!     
!
! Description:
!     MULTIPLE FAST SINE TRANSFORM
!     (Originally called FFT661, then Var_SinTrans)
!      author: clive temperton, may 1988 
!       (slightly modified for all-fortran version)
!    
!     Sine transform of length n is converted to
!     Real periodic transform of length n by pre- and post-
!     processing. Real periodic transform is performed by
!     pruning redundant operations from complex transform.
!     
!     see for example paul swarztrauber, "symmetric fft's",
!     math. comp. 47 (1986), 323-346.
!
! Method:
! 
!     ordering of coefficients:   z(0) , z(1) , z(2) , ... , z(n)
!     ordering of data:           x(0) , x(1) , x(2) , ... , x(n)
! 
!    vectorization is achieved on cray by doing the transforms
!    in parallel
!
!    N must be composed of factors 2,3 & 5 and must be even
!
!    definition of transforms:
!     -------------------------
!
!     isign=+1: x(i)=sum(j=1,...,n-1)(z(j)*sin(i*j*pi/n))
!
!     isign=-1: z(j)=(2/n)*sum(i=1,...,n-1)(x(i)*sin(i*j*pi/n))
!
! Current Code Owner: Andrew Lorenc
!
!   History:
! Version   Date     Comment
! -------   ----     -------
! 0.1       14/12/93 Original code. Phil Andrews
! 0.2       16/09/94 Small Modifications for the
!                    incorporation in the VAR project. HB
! 1.1       21/04/95 placed under control. JB
! 1.2       01/06/95 Tracing added. JB
!
! Code Description:
!    NB   BECAUSE OF THE TRICKY NESTED LOOPS
!         ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

Implicit none

! Subroutine arguments
 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data
                                           !  vectors  (which include zeros
                                           ! at the endpoints)
 Integer , intent (in)    :: DIM           ! dimension workspace
 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of
                                           ! factors of N

 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array

                                                    ! No descriptions given
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K,JA,JB,IA,IB,IGO,LA,J
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: SI,T1,T2,SCALE, vectorlength
 Real                     :: WORK(DIM)     ! size (n+1)*min(lot,VectorLength)

      VectorLength = LOT
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      DO 200 NB=1,NBLOX
!
!     PREPROCESSING
!     -------------
      DO 120 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      SI=TRIGS(2*N+K)
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA) = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         WORK(JB) = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         T2 = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
  115    CONTINUE
      ENDIF
  120 CONTINUE

      JA=1
      JB=NH+1
      IA=ISTART
      IB=ISTART+NH*INC
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 130 J=1,NVEX
         WORK(JA)=0.0
         WORK(JB)=2.0*A(IB)
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  130    CONTINUE
         IGO = +1
      ELSE
!DIR$ IVDEP
         DO 135 J=1,NVEX
         A(IA)=0.0
         A(IB)=2.0*A(IB)
         IA=IA+JUMP
         IB=IB+JUMP
  135    CONTINUE
         IGO = -1
      ENDIF
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL DA_QPASSM(WORK,WORK(IFAC*LA+1),A(IA),A(LA*INC+IA), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL DA_QPASSM(A(IA),A(IFAC*LA*INC+IA),WORK,WORK(LA+1), &
                    TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      ENDIF
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
  140 CONTINUE
!
!     POSTPROCESSING
!     --------------
      SCALE=2.0
      IF (ISIGN.EQ.+1) SCALE = FLOAT(N)
      JA=ISTART
      JB=JA+N*INC
      IA=1
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=0.0
      A(JA+INC)=0.5*SCALE*WORK(IA)
      A(JB)=0.0
      IA=IA+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=-SCALE*WORK(IA+1)
      A(JA+INC)=SCALE*WORK(IA)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE

      Go To 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(6,*) 'NVEX=', NVEX ,' GREATER THAN VectorLength'
      GO TO 570
  530 CONTINUE
      WRITE(6,*) 'IFAC=', IFAC, 'NOT CATERED FOR'
      GO TO 570
  550 CONTINUE
      WRITE(6,*) 'IFAC=', IFAC, ' ONLY CATERED FOR IF LA*IFAC=N'
  570 CONTINUE


      RETURN


End subroutine DA_Fast_Sine_Transform

!----------------------------------------------------------------------------------------------------------

!C     SUBROUTINE 'Var_QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART!C     OF MULTIPLE REAL FFT (FOURIER ANALYSIS) ROUTINE
!C
!C     A IS FIRST REAL INPUT VECTOR
!C         EQUIVALENCE B(1) WITH A(IFAC*LA*INC1+1)
!C     C IS FIRST REAL OUTPUT VECTOR
!C         EQUIVALENCE D(1) WITH C(LA*INC2+1)
!C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
!C     INC1 IS THE ADDRESSING INCREMENT FOR A
!C     INC2 IS THE ADDRESSING INCREMENT FOR C
!C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
!C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
!C     LOT IS THE NUMBER OF VECTORS
!C     N IS THE LENGTH OF THE VECTORS
!C     IFAC IS THE CURRENT FACTOR OF N
!C     LA = N/(PRODUCT OF FACTORS USED SO FAR)
!C     IERR IS AN ERROR INDICATOR:
!C              0 - PASS COMPLETED WITHOUT ERROR
!C              1 - LOT GREATER THAN VectorLength
!C              2 - IFAC NOT CATERED FOR
!C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
!C
!C-----------------------------------------------------------------------
!C
     SUBROUTINE DA_QPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA,IERR)

      INTEGER :: inc1, inc2, inc3, inc4, lot, n, ifac, la, ierr

      REAL  :: a, b, c, d, trigs
      DIMENSION A(*),B(*),C(*),D(*),TRIGS(N)
! Local named constants
 CHARACTER (LEN=*), PARAMETER :: RoutineName = "Var_QPASSM"
!
      REAL  :: SIN36, SIN72, QRT5, SIN60
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/, &
          QRT5/0.559016994374947/,SIN60/0.866025403784437/

      REAL    :: s1, s2, s3, s4, s5
      REAL    :: sin45, zsin36, zsin72, zqrt5, zsin60, zsin45, z
      REAL    :: a0, a1, a2, a3, a4, a5, a6, a10, a11, a20, a21
      REAL    :: b0, b1, b2, b3, b4, b5, b6, b10, b11, b20, b21
      REAL    :: c0, c1, c2, c3, c4, c5
      INTEGER :: i, ijk, l, k, kb, m, iink, jink, ijump, kstop
      INTEGER :: ibad, igo, ia, ie, je, ibase, jbase, ja, jb, j, ic
      INTEGER :: if, jf, kf, ib, jc, kc, id, jd, kd, ke, ig, ih
      INTEGER :: vectorlength
!
!- End of header ---------------------------------------------------------------


      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      IJUMP=(IFAC-1)*IINK
      KSTOP=(N-IFAC)/(2*IFAC)
!
      IBAD=1
      VectorLength = lot
      IF (LOT.GT.VectorLength) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
!
!     CODING FOR FACTOR 2
!     -------------------
  200 CONTINUE
      IA=1
      IB=IA+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
!
      IF (LA.EQ.M) GO TO 290
!
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      JBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB-JINK
  250 CONTINUE
      IF (JA.GT.JB) GO TO 900
  260 CONTINUE
      JBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      D(JA+J)=-A(IB+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
!
  290 CONTINUE
      Z=1.0/FLOAT(N)
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+A(IB+I))
      C(JB+J)=Z*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 3
!     -------------------
  300 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB
!
      IF (LA.EQ.M) GO TO 390
!
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))
      D(JB+J)=SIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      JBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 330 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))
      A2=A(IA+I)-0.5*A1
      B2=B(IA+I)-0.5*B1
      A3=SIN60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))
      B3=SIN60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))
      C(JA+J)=A(IA+I)+A1
      D(JA+J)=B(IA+I)+B1
      C(JB+J)=A2+B3
      D(JB+J)=B2-A3
      C(JC+J)=A2-B3
      D(JC+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
  350 CONTINUE
      IF (JA.GT.JC) GO TO 900
  360 CONTINUE
      JBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))
      D(JA+J)=-SIN60*(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
!
  390 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))
      D(JB+J)=ZSIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 4
!     -------------------
  400 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JB
!
      IF (LA.EQ.M) GO TO 490
!
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      C(JB+J)=A(IA+I)-A(IC+I)
      D(JB+J)=A(ID+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      JBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 430 IJK=1,LOT
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))
      C(JA+J)=A0+A1
      C(JC+J)=A0-A1
      D(JA+J)=B0+B1
      D(JC+J)=B1-B0
      C(JB+J)=A2+B3
      C(JD+J)=A2-B3
      D(JB+J)=B2-A3
      D(JD+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
  450 CONTINUE
      IF (JB.GT.JC) GO TO 900
  460 CONTINUE
      SIN45=SQRT(0.5)
      JBASE=0
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+SIN45*(A(IB+I)-A(ID+I))
      C(JB+J)=A(IA+I)-SIN45*(A(IB+I)-A(ID+I))
      D(JA+J)=-A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      D(JB+J)=A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
!
  490 CONTINUE
      Z=1.0/FLOAT(N)
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JB+J)=Z*(A(IA+I)-A(IC+I))
      D(JB+J)=Z*(A(ID+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 5
!     -------------------
  500 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC
      JE=JB
!
      IF (LA.EQ.M) GO TO 590
!
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 510 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-SIN72*A3-SIN36*A4
      D(JC+J)=-SIN36*A3+SIN72*A4
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JD) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      JBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 530 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      B5=B(IA+I)-0.25*(B1+B2)
      B6=QRT5*(B1-B2)
      A10=A5+A6
      A20=A5-A6
      B10=B5+B6
      B20=B5-B6
      A11=SIN72*B3+SIN36*B4
      A21=SIN36*B3-SIN72*B4
      B11=SIN72*A3+SIN36*A4
      B21=SIN36*A3-SIN72*A4
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A10+A11
      C(JE+J)=A10-A11
      C(JC+J)=A20+A21
      C(JD+J)=A20-A21
      D(JA+J)=B(IA+I)+(B1+B2)
      D(JB+J)=B10-B11
      D(JE+J)=-(B10+B11)
      D(JC+J)=B20-B21
      D(JD+J)=-(B20+B21)
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
  550 CONTINUE
      IF (JB.GT.JD) GO TO 900
  560 CONTINUE
      JBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 570 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)+0.25*(A3-A4)
      A6=QRT5*(A3+A4)
      C(JA+J)=A5+A6
      C(JB+J)=A5-A6
      C(JC+J)=A(IA+I)-(A3-A4)
      D(JA+J)=-SIN36*A1-SIN72*A2
      D(JB+J)=-SIN72*A1+SIN36*A2
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
!
  590 CONTINUE
      Z=1.0/FLOAT(N)
      ZQRT5=Z*QRT5
      ZSIN36=Z*SIN36
      ZSIN72=Z*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 592 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=Z*(A(IA+I)-0.25*(A1+A2))
      A6=ZQRT5*(A1-A2)
      C(JA+J)=Z*(A(IA+I)+(A1+A2))
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-ZSIN72*A3-ZSIN36*A4
      D(JC+J)=-ZSIN36*A3+ZSIN72*A4
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 6
!     -------------------
  600 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JC
      JF=JB
!
      IF (LA.EQ.M) GO TO 690
!
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 610 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=(A(IA+I)+A(ID+I))+A11
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)
      D(JC+J)=SIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11
      D(JB+J)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=(A(IA+I)-A(ID+I))+A11
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JC.EQ.JD) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      JBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 630 IJK=1,LOT
      A1=C1*A(IB+I)+S1*B(IB+I)
      B1=C1*B(IB+I)-S1*A(IB+I)
      A2=C2*A(IC+I)+S2*B(IC+I)
      B2=C2*B(IC+I)-S2*A(IC+I)
      A3=C3*A(ID+I)+S3*B(ID+I)
      B3=C3*B(ID+I)-S3*A(ID+I)
      A4=C4*A(IE+I)+S4*B(IE+I)
      B4=C4*B(IE+I)-S4*A(IE+I)
      A5=C5*A(IF+I)+S5*B(IF+I)
      B5=C5*B(IF+I)-S5*A(IF+I)
      A11=(A2+A5)+(A1+A4)
      A20=(A(IA+I)+A3)-0.5*A11
      A21=SIN60*((A2+A5)-(A1+A4))
      B11=(B2+B5)+(B1+B4)
      B20=(B(IA+I)+B3)-0.5*B11
      B21=SIN60*((B2+B5)-(B1+B4))
      C(JA+J)=(A(IA+I)+A3)+A11
      D(JA+J)=(B(IA+I)+B3)+B11
      C(JC+J)=A20-B21
      D(JC+J)=A21+B20
      C(JE+J)=A20+B21
      D(JE+J)=A21-B20
      A11=(A2-A5)+(A4-A1)
      A20=(A(IA+I)-A3)-0.5*A11
      A21=SIN60*((A4-A1)-(A2-A5))
      B11=(B5-B2)-(B4-B1)
      B20=(B3-B(IA+I))-0.5*B11
      B21=SIN60*((B5-B2)+(B4-B1))
      C(JB+J)=A20-B21
      D(JB+J)=A21-B20
      C(JD+J)=A11+(A(IA+I)-A3)
      D(JD+J)=B11+(B3-B(IA+I))
      C(JF+J)=A20+B21
      D(JF+J)=A21+B20
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
  650 CONTINUE
      IF (JC.GT.JD) GO TO 900
  660 CONTINUE
      JBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ SIN60*(A(IB+I)-A(IF+I))
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))-SIN60*(A(IC+I)+A(IE+I))
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))
      D(JB+J)=A(ID+I)-(A(IB+I)+A(IF+I))
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-SIN60*(A(IB+I)-A(IF+I))
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))+SIN60*(A(IC+I)+A(IE+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
!
  690 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 692 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)
      D(JC+J)=ZSIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)
      D(JB+J)=ZSIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 8
!     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      IG=IF+IINK
      IH=IG+IINK
      JA=1
      JB=JA+LA*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JD+2*M*INC2
      Z=1.0/FLOAT(N)
      ZSIN45=Z*SQRT(0.5)
!
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+ &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))- &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(IF+I)))
      C(JB+J)=Z*(A(IA+I)-A(IE+I))+ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      C(JD+J)=Z*(A(IA+I)-A(IE+I))-ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      D(JB+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))+Z*(A(IG+I)-A(IC+I))
      D(JD+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))-Z*(A(IG+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
!

  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD

      RETURN
      END SUBROUTINE DA_QPASSM

!----------------------------------------------------------------------------------------

!DALE: Use fortran 90 free-form, and lower case throughout?
!DALE: Provide short description of code here, including assumptions, ref., etc.

   SUBROUTINE da_sor ( dim1, dim2, ds, del2a, a )

      IMPLICIT NONE

      REAL,          PARAMETER    :: SMALLRES = 1.0E-4 ! DALE: Provide comments here.

      INTEGER,       PARAMETER    :: MM = 20000        ! DALE: And here, etc.
      REAL,          PARAMETER    :: ALPHA = 1.8
      REAL,          PARAMETER    :: ALPHAOV4 = alpha / 4.0

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(in)   :: del2a(1:dim1,1:dim2)         ! Del**2 a.
   real, intent(out)  :: a(1:dim1,1:dim2)             ! Field a.

   integer            :: i, j                         ! Loop counters.
   real               :: rd(1:dim1,1:dim2)            ! Solution residual.

   integer            :: ids, ide, jds, jde
      INTEGER                     :: ITER
      INTEGER                     :: MI
      REAL                        :: CHI         ( 1:dim1,1:dim2 )
      REAL                        :: CHIMX       ( 1:dim1 )
      REAL                        :: EPX
      REAL                        :: FAC
      REAL                        :: FF          ( 1:dim1 , 1:dim2 )
      REAL                        :: RDMAX       ( 1:dim1 )

      LOGICAL                     :: converged = .FALSE.

      ids = 1
      ide = dim1
      jds = 1
      jde = dim2
  
      rd  = 0.0
      chi = 0.0

      fac = 2.0 * ds * ds
      DO i = ids, ide
         DO j = jds, jde
               ff(i,j) = del2a(i,j) * fac
         END DO
      END DO

      iter_loop : DO iter = 1, mm
        mi = iter
         chimx = 0.0


!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               chimx(i) = MAX(ABS(chi(i,j)),chimx(i))
            END DO
         END DO
!csd$ end parallel do

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = jds+1, jde-1
            DO i = ids+1, ide-1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = jde-1, jds+1, -1
            DO i = ide-1, ids+1, -1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!csd$ end parallel do

         rdmax = 0.0

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               rdmax(i) = MAX(ABS(rd(i,j)),rdmax(i))
            END DO
         END DO
!csd$ end parallel do

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         ELSE
!            print '("iter No.",i5,"  Max. Residule=",e12.5)', mi, maxval(rdmax)
         END IF

      END DO iter_loop

      IF (converged ) THEN
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation did not converge in',mm,' iterations.'
!         STOP 'no_converge'
      END IF

      a(:,:) = chi(:,:)

   END SUBROUTINE da_sor

   SUBROUTINE da_relax (psi, vor, residual, ids, ide, jds, jde, ds)

      IMPLICIT NONE

      REAL,          PARAMETER    :: SMALLRES = 5.0E-7
!      REAL,          PARAMETER    :: SMALLRES = 1.0E-6
!      REAL,          PARAMETER    :: SMALLRES = 1.0E-4 ! DALE: Provide comments here.

      INTEGER,       PARAMETER    :: MM = 20000        ! DALE: And here, etc.
!      REAL,          PARAMETER    :: ALPHA = 1.8
      REAL,          PARAMETER    :: ALPHA = 1.2
      REAL,          PARAMETER    :: ALPHAOV4 = alpha / 4.0

      INTEGER                     :: I
      INTEGER                     :: IDS, IDE
      INTEGER                     :: ITER
      INTEGER                     :: J
      INTEGER                     :: JDS, JDE
      INTEGER                     :: MI

!DALE: Define intent of subroutine args:
      REAL                        :: PSI         ( ids:ide , jds:jde )
      REAL                        :: VOR         ( ids:ide , jds:jde )
      REAL                        :: residual        ( ids:ide , jds:jde )

      REAL (kind=8)                        :: CHI         ( ids:ide , jds:jde )
      REAL (kind=8)                        :: CHIMX       ( ids:ide )
      REAL (kind=8)                       :: EPX
      REAL                                :: DS
      REAL (kind=8)                       :: FAC
      REAL (kind=8)                        :: FF          ( ids:ide , jds:jde )
      REAL (kind=8)                        :: RD          ( ids:ide , jds:jde )
      REAL (kind=8)                       :: RDMAX       ( ids:ide )

      LOGICAL                     :: converged = .FALSE.

      rd  = 0.0
      chi = 0.0

      fac = 2.0 * ds * ds
      DO i = ids, ide
         DO j = jds, jde
               ff(i,j) = vor(i,j) * fac
         END DO
      END DO

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               chimx(i) = MAX(ABS(chi(i,j)),chimx(i))
            END DO
         END DO
!csd$ end parallel do

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = jds+1, jde-1
            DO i = ids+1, ide-1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = jde-1, jds+1, -1
            DO i = ide-1, ids+1, -1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!csd$ end parallel do

         rdmax = 0.0

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               rdmax(i) = MAX(ABS(rd(i,j)),rdmax(i))
            END DO
         END DO
!csd$ end parallel do

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         ELSE
!            print '("iter No.",i5,"  Max. Residule=",e12.5)', mi, maxval(rdmax)
         END IF

      END DO iter_loop

      IF (converged ) THEN
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation did not converge in',mm,' iterations.'
!         STOP 'no_converge'
      END IF

      psi(:,:) = chi(:,:)
      residual(:,:) = rd(:,:)

   END SUBROUTINE da_relax

#ifdef crayx1

   subroutine getarg(i, harg)
     implicit none
     character(len=*) :: harg
     integer :: ierr, ilen, i

     call pxfgetarg(i, harg, ilen, ierr)
     return
   end subroutine getarg
#endif

end program gen_be_stage0_wrf

