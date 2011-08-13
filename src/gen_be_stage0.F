program gen_be_stage0
!----------------------------------------------------------------------
! Authors: Dale Barker,    NCAR/MMM/DAG 
!          Yann Michel,    NCAR/MMM/DAG & CNRM-GAME/GMAP
!          Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
!----------------------------------------------------------------------
! Purpose: Converts WRF-ARW standard output files (wrfout) to  the
!          desired format required by Stage1 of BE statistics for WRF/GSI
!
!  Input  : WRF forecasts for a specified date (NETCDF format).
!
!  Output : Binary files for use in Stage1 for WRF/GSI
!
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : num_fft_factors, pi, stdout, stderr, &
      filename_len, base_pres, base_temp, base_lapse
   use da_gen_be, only : da_get_field, da_get_trh, &
      da_stage0_initialize
   use da_tools_serial, only : da_get_unit, da_free_unit,da_find_fft_factors, &
      da_find_fft_trig_funcs
   use module_ffts, only : fft551, fft661
   use da_change_wind_variables
   use da_fft_initialize
   use da_cloud_fraction, only : cal_cldfra2
   
   implicit none

   integer :: gen_be_iunit, gen_be_ounit

   character (len=filename_len)   :: filestub                  ! General filename stub.
   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=filename_len)   :: output_file               ! Output file. 

   character (len=10)    :: date                      ! Character date.
   character (len=12)    :: var                       ! Variable to search for.
   character (len=3)     :: be_method                 ! "NMC" or "ENS".
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.
   character (len=3)   :: model                      ! Model name 'WRF', 'GSI' or 'UM' etc. 

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: ni                      ! Dimensions of grid (T points).
   integer               :: nis                     ! Dimensions of grid (vor2d/psi pts).
   integer               :: nj                      ! Dimensions of grid (T points).
   integer               :: njs                     ! Dimensions of grid (vor2d/psi pts).
   integer               :: nk                      ! Dimensions of grid (T points).
   integer               :: n1, n2                    ! Padded dimensions (n=dim-1+pad).
   integer               :: n1s, n2s                  ! Padded dimensions (n=dim-1+pad).
   integer               :: poisson_method            ! 1=Spectral, 2=SOR.
   integer               :: fft_method                ! For poisson_method=1: 1=FCT, 2=FST.
   integer               :: ktest                     ! Test level.
   integer               :: bin_type                  ! Type of bin to average over.
   integer               :: nstart_member             ! 1 or 2, starting member when ENS be

   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution in meters.
   real                  :: ni2_inv_u               ! 1 / (ni*nj).
   real                  :: ni2_inv_v               ! 1 / (ni*nj).
   real                  :: ni2_inv                 ! 1 / (ni*nj).
   logical               :: test_inverse              ! Test conversion by performing inverse.
   logical               :: use_mean_ens              ! Use mean of perturbations rather than pairs
   
   integer               :: ifax1(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax2(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax1s(1:num_fft_factors) ! FFT factors.
   integer               :: ifax2s(1:num_fft_factors) ! FFT factors.

   real, allocatable     :: xlat(:,:)                 ! Latitude  of mass points.
   real, allocatable     :: xlon(:,:)                 ! Longitude of mass points.
   real, allocatable     :: mapfac_m(:,:)             ! Map factor - mass pts.
   real, allocatable     :: mapfac_u(:,:)             ! Map factor - u points.
   real, allocatable     :: mapfac_v(:,:)             ! Map factor - v points.
   real, allocatable     :: znu(:)                    ! Half sigma levels      

   real, allocatable     :: u(:,:)                    ! u-wind.
   real, allocatable     :: v(:,:)                    ! v-wind.
   real, allocatable     :: div2d(:,:)                  ! div2dergence.
   real, allocatable     :: vor2d(:,:)                  ! vor2dticity.
   real, allocatable     :: psi2d(:,:)                ! Streamfunction copy. 
   real, allocatable     :: chi2d(:,:)                ! Velocity potential copy.
   real, allocatable     :: temp2d(:,:)               ! Temperature.
   real, allocatable     :: rh2d(:,:)                 ! Relative humidity.
   real, allocatable     :: qhydro2d(:,:)             ! Hydrometeors
   real, allocatable     :: tmp2d(:,:)                ! Others
  
   real, allocatable     :: trigs1(:)                 ! FFT trig functions.
   real, allocatable     :: trigs2(:)                 ! FFT trig functions.
   real, allocatable     :: fft_coeffs(:,:)           ! FFT coefficients.
   real, allocatable     :: trigs1s(:)                ! FFT trig functions.
   real, allocatable     :: trigs2s(:)                ! FFT trig functions.
   real, allocatable     :: fft_coeffss(:,:)          ! FFT coefficients.

!  Standard fields:
   real, allocatable     :: psi(:,:,:)                ! Streamfunction.
   real, allocatable     :: chi(:,:,:)                ! Velocity Potential.
   real, allocatable     :: vor(:,:,:)                ! Vorticity
   real, allocatable     :: div(:,:,:)                ! Divergence
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: press(:,:,:)              ! Pressure
   real, allocatable     :: qvapor(:,:,:)             ! Humidity.
   real, allocatable     :: rh(:,:,:)                 ! Relative humidity.
   real, allocatable     :: qcloud(:,:,:)             ! Cloud Water 
   real, allocatable     :: qrain(:,:,:)              ! Cloud Rain  
   real, allocatable     :: qice(:,:,:)               ! Cloud Ice   
   real, allocatable     :: qsnow(:,:,:)              ! Cloud Snow  
   real, allocatable     :: cld_fra(:,:,:)            ! Cloud fraction
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   
   real, allocatable     :: psi_mean(:,:,:)           ! Streamfunction.
   real, allocatable     :: chi_mean(:,:,:)           ! Velocity Potential.
   real, allocatable     :: vor_mean(:,:,:)           ! Vorticity
   real, allocatable     :: div_mean(:,:,:)           ! Divergence  
   real, allocatable     :: temp_mean(:,:,:)          ! Temperature.
   real, allocatable     :: rh_mean(:,:,:)            ! Relative humidity.
   real, allocatable     :: psfc_mean(:,:)            ! Surface pressure.
   real, allocatable     :: qcloud_mean(:,:,:)        ! Cloud Water 
   real, allocatable     :: qrain_mean(:,:,:)         ! Cloud Rain  
   real, allocatable     :: qice_mean(:,:,:)          ! Cloud Ice   
   real, allocatable     :: qsnow_mean(:,:,:)         ! Cloud Snow 
   real, allocatable     :: cld_fra_mean(:,:,:)       ! Cloud fraction
   
   integer, allocatable  :: rain_class(:,:)           ! 2D rain class of perturbation
   real, allocatable     :: vert_qrain(:,:)           ! Vertical sum of Rain water
   real, allocatable     :: vert_qrain_mean(:,:)      ! Vertical sum of Rain water
   real qrain_th_low, qrain_th_high                   ! two thresholds in the rain class when bin_type==7

   namelist / gen_be_stage0_nl / bin_type, qrain_th_low, qrain_th_high, model, use_mean_ens

   integer namelist_unit

   qrain_th_low = 0.0001
   qrain_th_high = 0.01
   use_mean_ens = .false.
 
   model = 'WRF'
   bin_type = 5         ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
   
   stderr = 0
   stdout = 6

   write(6,'(/a)')' [1] Initialize information.'

   call da_get_unit(gen_be_iunit)
   call da_get_unit(gen_be_ounit)

   call da_get_unit(namelist_unit)
   open(unit=namelist_unit, file='gen_be_stage0_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage0_nl)
   close(namelist_unit)

   test_inverse = .true. 
   ktest = 1
   poisson_method = 1    
   fft_method = 2
   numarg = iargc()
   if ( numarg /= 4 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_stage0 be_method date ne <wrf_file_stub> Stop"
      stop
   end if
   
   ! Initialise to stop false Cray compiler warnings
   be_method=""
   date=""
   cne=""
   filestub=""

   call getarg( 1, be_method )
   call getarg( 2, date )
   call getarg( 3, cne )
   read(cne,'(i3)')ne
   call getarg( 4, filestub )

   if ( be_method == "NMC" ) then
      write(6,'(a,a)')' Computing gen_be NMC-method forecast difference files for date ', date
      ne = 2                      ! NMC-method uses differences between 2 forecasts.
      use_mean_ens = .false.
   else if ( be_method == "ENS" ) then
      write(6,'(a,a)')' Computing gen_be ensemble perturbation files for date ', date
      write(6,'(a,i4)')' Ensemble Size = ', ne
      if (use_mean_ens) write(6,'(a)') 'Use mean of ensemble for defining the perturbations'
   else
      write(6,'(a,a)')trim(be_method), ' is an invalid value of be_method. Stop'
      stop
   end if
   write(6,'(a,a)')' Input filestub = ', trim(filestub)
   write(6,'(2a)') ' Computing statistics for model ',trim(model)
   
!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Setup up ancillary fields using 1st member values.' 
!---------------------------------------------------------------------------------------------

   var = "T"
   input_file = trim(filestub)//'.e001'
   call da_stage0_initialize( input_file, var, ni, nj, nk,ds )
   nis = ni+1 ! vor2dticity/streamfunction array 1 larger.
   njs = nj+1 ! vor2dticity/streamfunction array 1 larger.
   ni2_inv_u = 1.0 / real((ni+1) * nj)
   ni2_inv_v = 1.0 / real(ni * (nj+1))
   ni2_inv = 1.0 / real(ni * nj)

!  Allocate arrays in output fields:
   allocate( psi(1:ni,1:nj,1:nk) ) 
   allocate( chi(1:ni,1:nj,1:nk) )
   allocate( vor(1:ni,1:nj,1:nk) ) 
   allocate( div(1:ni,1:nj,1:nk) )  
   allocate( temp(1:ni,1:nj,1:nk) )
   allocate( press(1:ni,1:nj,1:nk) )
   allocate( rh(1:ni,1:nj,1:nk) )
   allocate( psfc(1:ni,1:nj) )
   allocate( qcloud(1:ni,1:nj,1:nk) )
   allocate( qrain(1:ni,1:nj,1:nk) )
   allocate( qice(1:ni,1:nj,1:nk) )
   allocate( qsnow(1:ni,1:nj,1:nk) )
   allocate( cld_fra(1:ni,1:nj,1:nk) )
   allocate( qvapor(1:ni,1:nj,1:nk) )

   if (bin_type==7) then
      allocate( rain_class(1:ni,1:nj) )
      allocate( vert_qrain(1:ni,1:nj) )
      allocate( vert_qrain_mean(1:ni,1:nj) )
      vert_qrain(:,:)=0.0
      vert_qrain_mean(:,:)=0.0    
   end if

   allocate( psi_mean(1:ni,1:nj,1:nk) ) 
   allocate( chi_mean(1:ni,1:nj,1:nk) )
   allocate( vor_mean(1:ni,1:nj,1:nk) ) 
   allocate( div_mean(1:ni,1:nj,1:nk) )  
   allocate( temp_mean(1:ni,1:nj,1:nk) )
   allocate( rh_mean(1:ni,1:nj,1:nk) )
   allocate( psfc_mean(1:ni,1:nj) )
   allocate( qcloud_mean(1:ni,1:nj,1:nk) )
   allocate( qrain_mean(1:ni,1:nj,1:nk) )
   allocate( qice_mean(1:ni,1:nj,1:nk) )
   allocate( qsnow_mean(1:ni,1:nj,1:nk) )
   allocate( cld_fra_mean(1:ni,1:nj,1:nk) )

   psi_mean = 0.0
   chi_mean = 0.0
   vor_mean = 0.0
   div_mean = 0.0
   temp_mean = 0.0
   rh_mean = 0.0
   psfc_mean = 0.0
   qcloud_mean = 0.0
   qrain_mean = 0.0
   qice_mean = 0.0
   qsnow_mean = 0.0

   allocate( xlat(1:ni,1:nj) )
   allocate( xlon(1:ni,1:nj) )
   allocate( mapfac_m(1:ni,1:nj) )
   allocate( mapfac_u(1:nis,1:nj) )
   allocate( mapfac_v(1:ni,1:njs) )
   allocate( znu(1:nk) )

   var = "XLAT"
   call da_get_field( input_file, var, 2, ni, nj, 1, 1, xlat )
   var = "XLONG"
   call da_get_field( input_file, var, 2, ni, nj, 1, 1, xlon )
   var = "MAPFAC_M"
   call da_get_field( input_file, var, 2, ni, nj, 1, 1, mapfac_m )
   var = "MAPFAC_U"
   call da_get_field( input_file, var, 2, ni+1, nj, 1, 1, mapfac_u )
   var = "MAPFAC_V"
   call da_get_field( input_file, var, 2, ni, nj+1, 1, 1, mapfac_v )
   var = "ZNU"
   allocate( tmp2d(1:nk,1:1) )
   call da_get_field( input_file, var, 1, nk, 1, 1, 1, tmp2d )
   znu(:)=tmp2d(:,1)
   deallocate( tmp2d )
   
!  Initialize FFT coefficients:
   if ( poisson_method == 1 ) then
      call da_fft_initialize1( ni, nj, n1, n2, ifax1, ifax2 )
      call da_fft_initialize1( nis, njs, n1s, n2s, ifax1s, ifax2s )
      allocate( trigs1(1:3*n1) )
      allocate( trigs2(1:3*n2) )
      allocate( fft_coeffs(1:n1+1,1:n2+1) )
      call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )
      allocate( trigs1s(1:3*n1s) )
      allocate( trigs2s(1:3*n2s) )
      allocate( fft_coeffss(1:n1s+1,1:n2s+1) )
      call da_fft_initialize2( n1s, n2s, ds, trigs1s, trigs2s, fft_coeffss )
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Convert WRF forecast fields to standard fields and output' 
!---------------------------------------------------------------------------------------------

   ! Allocate temporary arrays:
   allocate( u(1:nis,1:nj) )
   allocate( v(1:ni,1:njs) )
   allocate( vor2d(1:nis,1:njs) )
   allocate( div2d(1:ni,1:nj) )
   allocate( psi2d(1:nis,1:njs) )
   allocate( chi2d(1:ni,1:nj) )
   allocate( temp2d(1:ni,1:nj) )
   allocate( rh2d(1:ni,1:nj) )
   allocate( tmp2d(1:ni,1:nj) )
   allocate( qhydro2d(1:ni,1:nj) )
   
   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      do k = 1, nk

         ! Read u, v:
         var = "U"
         call da_get_field( input_file, var, 3, nis, nj, nk, k, u )
         var = "V"
         call da_get_field( input_file, var, 3, ni, njs, nk, k, v )

         ! Calculate vor2dticity (in center of mass grid on WRF's Arakawa C-grid):
         call da_uv_to_vor_c( ni, nj, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, vor2d )

         ! Calculate div2dergence (at mass pts. on WRF's Arakawa C-grid):
         call da_uv_to_div_c( ni, nj, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, div2d )

         ! Calculate streamfunction and potential 
         ! Assumes vor2d/div2d converted to Del**2 psi/chi):

         if ( poisson_method == 1 ) then
            call da_del2a_to_a( nis, njs, n1s, n2s, fft_method, ifax1s, ifax2s, &
                                trigs1s, trigs2s, fft_coeffss, vor2d, psi2d )
            call da_del2a_to_a( ni, nj, n1, n2, fft_method, ifax1, ifax2, &
                                trigs1, trigs2, fft_coeffs, div2d, chi2d )
         else
            stop 'Unsupported poisson_method'
         end if
	 
	 vor(:,:,k) = vor2d(:,:)
 	 div(:,:,k) = div2d(:,:)
	 
         if ( test_inverse .and. k == ktest .and. member == 1 ) then
            call da_test_inverse( ni, nj, ds, mapfac_m, mapfac_u, mapfac_v, &
                                  n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                                  n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                                  u, v, psi2d, chi2d )
         end if

!        Interpolate psi to mass pts ready for output:
         do j = 1, nj
            do i = 2, ni
               psi(i,j,k) = 0.25 * ( psi2d(i,j) + psi2d(i+1,j) + &
                                     psi2d(i,j+1) + psi2d(i+1,j+1) )
            end do
         end do

         chi(:,:,k) = chi2d(:,:)
	 
!        Read mass fields, and convert to T and rh:
         call da_get_trh( input_file, ni, nj, nk, k, temp2d, rh2d )
         temp(:,:,k) = temp2d(:,:)
         rh(:,:,k) = 0.01 * rh2d(:,:) ! *0.01 to conform with WRF-Var/GSI units
	 
!        Needed for cld fraction: pressure, qvapor
	 var = "PB"
	 call da_get_field( input_file, var, 3, ni, nj, nk, k, tmp2d )
	 press(:,:,k) = tmp2d(:,:)
	 var = "P"
	 call da_get_field( input_file, var, 3, ni, nj, nk, k, tmp2d )
	 press(:,:,k) = press(:,:,k) + tmp2d(:,:)
	 var = "QVAPOR"
	 call da_get_field( input_file, var, 3, ni, nj, nk, k, tmp2d )
	 qvapor(:,:,k) = tmp2d(:,:)
	 
!        Hydrometeors
         var = "QCLOUD"
         call da_get_field( input_file, var, 3, ni, nj, nk, k, qhydro2d )
         qcloud(:,:,k) = qhydro2d(:,:)

         var = "QRAIN"
         call da_get_field( input_file, var, 3, ni, nj, nk, k, qhydro2d )
         qrain(:,:,k) = qhydro2d(:,:)
	 ! vertical sum of rain in g/kg
	 if (bin_type==7) vert_qrain=vert_qrain+1000.0*qhydro2d

         var = "QICE"
         call da_get_field( input_file, var, 3, ni, nj, nk, k, qhydro2d )
         qice(:,:,k) = qhydro2d(:,:)

         var = "QSNOW"
         call da_get_field( input_file, var, 3, ni, nj, nk, k, qhydro2d )
         qsnow(:,:,k) = qhydro2d(:,:)
	 
      end do

!     Finally, extract surface pressure:
      var = "PSFC"
      call da_get_field( input_file, var, 2, ni, nj, nk, 1, psfc )
      if( model(1:3) == "GSI" .or. model(1:3) == "gsi") then
      ! convert psfc in centibar     
          psfc = psfc*0.001
      end if

!     Compute background cloud fraction
      call cal_cldfra2(CLDFRA=cld_fra,QV=qvapor,QC=qcloud,QI=qice,QS=qsnow,&
      			F_QC=.true.,F_QI=.true.,F_QV=.true.,F_QS=.true.,&
			t_phy=temp,p_phy=press,&
      			ids=1,ide=ni,jds=1,jde=nj,kds=1,kde=nk,&
			ims=1,ime=ni,jms=1,jme=nj,kms=1,kme=nk,&
			its=1,ite=ni,jts=1,jte=nj,kts=1,kte=nk)
!     debug
      write(6,'(a,f12.4)') ' Mean cloud fraction for this field is ',sum(cld_fra)/real(ni*nj*nk)
      
!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, ni, nj, nk
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)vor
      write(gen_be_ounit)div
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)psfc
      write(gen_be_ounit)qcloud
      write(gen_be_ounit)qrain
      write(gen_be_ounit)qice
      write(gen_be_ounit)qsnow
      write(gen_be_ounit)cld_fra
      if (bin_type==7)  write(gen_be_ounit)vert_qrain
      close(gen_be_ounit)

      if ( be_method == "ENS" .and. use_mean_ens) then
   !     Calculate accumulating mean:
	 member_inv = 1.0 / real(member)
	 psi_mean = ( real( member-1 ) * psi_mean + psi ) * member_inv
	 chi_mean = ( real( member-1 ) * chi_mean + chi ) * member_inv
	 vor_mean = ( real( member-1 ) * vor_mean + vor ) * member_inv
	 div_mean = ( real( member-1 ) * div_mean + div ) * member_inv
	 temp_mean = ( real( member-1 ) * temp_mean + temp ) * member_inv
	 rh_mean = ( real( member-1 ) * rh_mean + rh ) * member_inv
	 qcloud_mean = ( real( member-1 ) * qcloud_mean + qcloud ) * member_inv
	 qrain_mean = ( real( member-1 ) * qrain_mean + qrain ) * member_inv
	 qice_mean = ( real( member-1 ) * qice_mean + qice ) * member_inv
	 qsnow_mean = ( real( member-1 ) * qsnow_mean + qsnow  ) * member_inv
	 psfc_mean = ( real( member-1 ) * psfc_mean + psfc ) * member_inv
      end if
   end do

   deallocate( u )
   deallocate( v )
   deallocate( vor2d )
   deallocate( div2d )
   deallocate( psi2d )
   deallocate( chi2d )
   deallocate( temp2d )
   deallocate( rh2d )
   deallocate( tmp2d )
   deallocate( qhydro2d )
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( be_method == "NMC" ) then
      write(6,'(/a)')'    Compute perturbation as a difference between two forecasts' 

!     Re-read input forecast standard fields (ne=2 hard-wired above for NMC-method):
      input_file = 'tmp.e001'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, ni, nj, nk
      read(gen_be_iunit)psi
      read(gen_be_iunit)chi
      read(gen_be_iunit)vor
      read(gen_be_iunit)div
      read(gen_be_iunit)temp
      read(gen_be_iunit)rh
      read(gen_be_iunit)psfc
      read(gen_be_iunit)qcloud
      read(gen_be_iunit)qrain
      read(gen_be_iunit)qice
      read(gen_be_iunit)qsnow
      read(gen_be_iunit)cld_fra
      if (bin_type==7)    read(gen_be_iunit)vert_qrain
      close(gen_be_iunit)
      call da_free_unit(gen_be_iunit)

      input_file = 'tmp.e002'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, ni, nj, nk
      read(gen_be_iunit)psi_mean
      read(gen_be_iunit)chi_mean
      read(gen_be_iunit)vor_mean
      read(gen_be_iunit)div_mean
      read(gen_be_iunit)temp_mean
      read(gen_be_iunit)rh_mean
      read(gen_be_iunit)psfc_mean
      read(gen_be_iunit)qcloud_mean
      read(gen_be_iunit)qrain_mean
      read(gen_be_iunit)qice_mean
      read(gen_be_iunit)qsnow_mean
      read(gen_be_iunit)cld_fra_mean
      if (bin_type==7)    read(gen_be_iunit)vert_qrain_mean
      close(gen_be_iunit)

      if (bin_type==7) then 
         ! find out class of rain    
         rain_class=1 ! mixed case
         where ((vert_qrain.ge.qrain_th_high).and.(vert_qrain_mean.ge.qrain_th_high))
            rain_class=3 ! Heavy Rain
         end where
         where ((vert_qrain.lt.qrain_th_high).and.(vert_qrain_mean.lt.qrain_th_high)&
              .and.(vert_qrain.ge.qrain_th_low).and.(vert_qrain_mean.ge.qrain_th_low))
            rain_class=2 ! Light Rain
         end where
         where ((vert_qrain.lt.qrain_th_low).and.(vert_qrain_mean.lt.qrain_th_low))
            rain_class=0 ! Clear
         end where        
      end if

!     Write out NMC-method standard perturbations:
      output_file = 'pert.'//date(1:10)//'.e001'
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, ni, nj, nk
      write(gen_be_ounit)(psi - psi_mean)
      write(gen_be_ounit)(chi - chi_mean)
      write(gen_be_ounit)(vor - vor_mean)
      write(gen_be_ounit)(div - div_mean)
      write(gen_be_ounit)(temp - temp_mean)
      write(gen_be_ounit)(rh - rh_mean)
      write(gen_be_ounit)(psfc - psfc_mean)
      write(gen_be_ounit)(qcloud - qcloud_mean)
      write(gen_be_ounit)(qrain - qrain_mean)
      write(gen_be_ounit)(qice - qice_mean)
      write(gen_be_ounit)(qsnow - qsnow_mean)
      close(gen_be_ounit)
      
      !   Write out NMC-method Holm/Full reference fields:
      output_file = 'fullflds.'//date(1:10)//'.e001'
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, ni, nj, nk
      write(gen_be_ounit)rh
      write(gen_be_ounit)(rh+rh_mean)*0.5
      write(gen_be_ounit)temp
      write(gen_be_ounit)(temp+temp_mean)*0.5
      write(gen_be_ounit)cld_fra
      write(gen_be_ounit)(cld_fra+cld_fra_mean)*0.5
      close(gen_be_ounit)

     !  Write rain class
     if (bin_type==7) then
         var = 'raincl' ! 2D field
         output_file = trim(var)//'/'//date(1:10)
         output_file = trim(output_file)//'.'//trim(var)//'.e'//ce//'.01'
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)ni, nj, 1
         write(gen_be_ounit)rain_class
     end if
	
   else ! be_method = "ENS"

      do member = 1, ne
	 if (.not. use_mean_ens) then
	    if (member.eq.1) then
	       write(UNIT=ce,FMT='(i3.3)')ne
	       input_file = 'tmp.e'//ce
               open (gen_be_iunit, file = input_file, form='unformatted')
               read(gen_be_iunit)date, ni, nj, nk
               read(gen_be_iunit)psi_mean
               read(gen_be_iunit)chi_mean
	       read(gen_be_iunit)vor_mean
               read(gen_be_iunit)div_mean
               read(gen_be_iunit)temp_mean
               read(gen_be_iunit)rh_mean
               read(gen_be_iunit)psfc_mean
               read(gen_be_iunit)qcloud_mean
               read(gen_be_iunit)qrain_mean
               read(gen_be_iunit)qice_mean
               read(gen_be_iunit)qsnow_mean
	       read(gen_be_iunit)cld_fra_mean
	       if (bin_type==7)    read(gen_be_iunit)vert_qrain_mean
               close(gen_be_iunit)
	    else
	    	psi_mean = psi
	    	chi_mean = chi
		vor_mean = vor
	    	div_mean = div
		temp_mean=temp
		rh_mean  = rh
		psfc_mean= psfc
		qcloud_mean = qcloud
		qrain_mean = qrain
		qice_mean  = qice
		qsnow_mean = qsnow
		cld_fra_mean = cld_fra
            end if
	 end if
	 
         write(UNIT=ce,FMT='(i3.3)')member

!        Re-read ensemble member standard fields:
         input_file = 'tmp.e'//ce  
         open (gen_be_iunit, file = input_file, form='unformatted')
         read(gen_be_iunit)date, ni, nj, nk
         read(gen_be_iunit)psi
         read(gen_be_iunit)chi
	 read(gen_be_iunit)vor
         read(gen_be_iunit)div
         read(gen_be_iunit)temp
         read(gen_be_iunit)rh
         read(gen_be_iunit)psfc
         read(gen_be_iunit)qcloud
         read(gen_be_iunit)qrain
         read(gen_be_iunit)qice
         read(gen_be_iunit)qsnow
	 read(gen_be_iunit)cld_fra
	 if (bin_type==7)    read(gen_be_iunit)vert_qrain
         close(gen_be_iunit)

 	 if (bin_type==7) then 
         ! find out class of rain    
           rain_class=1 ! mixed case
           where ((vert_qrain.ge.qrain_th_high).and.(vert_qrain_mean.ge.qrain_th_high))
              rain_class=3 ! Heavy Rain
           end where
           where ((vert_qrain.lt.qrain_th_high).and.(vert_qrain_mean.lt.qrain_th_high)&
                .and.(vert_qrain.ge.qrain_th_low).and.(vert_qrain_mean.ge.qrain_th_low))
              rain_class=2 ! Light Rain
           end where
           where ((vert_qrain.lt.qrain_th_low).and.(vert_qrain_mean.lt.qrain_th_low))
              rain_class=0 ! Clear
           end where        
         end if
      
!        Write out standard perturbations for this member:
         output_file = 'pert.'//date(1:10)//'.e'//ce
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)date, ni, nj, nk
         write(gen_be_ounit)(psi - psi_mean)
         write(gen_be_ounit)(chi - chi_mean)
	 write(gen_be_ounit)(vor - vor_mean)
         write(gen_be_ounit)(div - div_mean)
         write(gen_be_ounit)(temp - temp_mean)
         write(gen_be_ounit)(rh - rh_mean)
         write(gen_be_ounit)(psfc - psfc_mean)
         write(gen_be_ounit)(qcloud - qcloud_mean)
         write(gen_be_ounit)(qrain - qrain_mean)
         write(gen_be_ounit)(qice - qice_mean)
         write(gen_be_ounit)(qsnow - qsnow_mean)
         close(gen_be_ounit)

         !  Write out ensemble Holm/Full reference fields:
         output_file = 'fullflds.'//date(1:10)//'.e'//ce
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)date, ni, nj, nk
	 write(gen_be_ounit)rh
         write(gen_be_ounit)(rh+rh_mean)*0.5
         write(gen_be_ounit)temp
         write(gen_be_ounit)(temp+temp_mean)*0.5
         write(gen_be_ounit)cld_fra
         write(gen_be_ounit)(cld_fra+cld_fra_mean)*0.5 
         close(gen_be_ounit)
	 
	 !  Write rain class
   	 if (bin_type==7) then
    	    var = 'raincl' ! 2D field
       	    output_file = trim(var)//'/'//date(1:10)
       	    output_file = trim(output_file)//'.'//trim(var)//'.e'//ce//'.01'
       	    open (gen_be_ounit, file = output_file, form='unformatted')
       	    write(gen_be_ounit)ni, nj, 1
       	    write(gen_be_ounit)rain_class
	 end if
	
      end do
   end if

  ! Write fixed fields (domain, geometry)
   output_file = 'fixed_flds'
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)ni, nj, nk
   write(gen_be_ounit)znu 
   write(gen_be_ounit)xlat
   write(gen_be_ounit)xlon
   write(gen_be_ounit)mapfac_m
   write(gen_be_ounit)mapfac_u
   write(gen_be_ounit)mapfac_v
   close(gen_be_ounit)

   ! Deallocate
   deallocate( psi )
   deallocate( chi )
   deallocate( vor )
   deallocate( div )
   deallocate( temp )
   deallocate( rh )
   deallocate( psfc )
   deallocate( xlat )
   deallocate( xlon )
   deallocate( psi_mean )
   deallocate( chi_mean )
   deallocate( vor_mean )
   deallocate( div_mean )
   deallocate( temp_mean )
   deallocate( rh_mean )
   deallocate( psfc_mean )
   deallocate( qcloud )
   deallocate( qrain )
   deallocate( qice )
   deallocate( qsnow )
   deallocate( cld_fra )
   deallocate( qcloud_mean )
   deallocate( qrain_mean )
   deallocate( qice_mean )
   deallocate( qsnow_mean )
   deallocate( cld_fra_mean )
   deallocate( mapfac_m )
   deallocate( znu )       
   write(6,'(a)') ' End of program gen_be_stage0'

end program gen_be_stage0
