program gen_be_stage4

   use da_constants
   use da_spectral

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*80        :: dat_dir                    ! Input data directory.
   character*80        :: expt                       ! Experiment ID.
   character*80        :: filename                   ! Input filename.
   character*2         :: ck                         ! Loop index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: num_states                 ! Number of data times.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: i, j, k, member, n         ! Loop counters.
   integer             :: ne                         ! Number of ensemble members.
   integer             :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer             :: inc                        ! Jump between elements of vector in array.
   integer             :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   integer             :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer             :: lenwrk                     ! Dimension of work array.
   integer             :: ier                        ! FFT error flag.
   integer             :: count                      ! Counter
   integer             :: alp_size                   ! Ass. Leg. Pol. array size.
   integer             :: v_size                     ! Complex control variable array size.

   logical             :: first_time                 ! True if first time through loop.
   logical             :: testing_spectral           ! Test spectral transform.
   logical             :: data_on_levels             ! False if data is projected onto EOFs.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.
   real                :: pi_over_180                ! pi / 180
   real                :: diff_rms                   ! RMS error measure.
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   real                :: variance                   ! Variance (sum of power spectra) .

   real, allocatable   :: field(:,:)                 ! Gridpoint field to be decomposed.
   real, allocatable   :: field_out(:,:)             ! Output test field.

   real, allocatable   :: lat(:)                     ! Latitude (radians).
   real, allocatable   :: sinlat(:)                  ! sine(latitude).
   real, allocatable   :: coslat(:)                  ! cosine(latitude).
   real, allocatable   :: int_wgts(:)                ! Legendre integration weights.
   real, allocatable   :: lon(:)                     ! Longitude (radians).
   real, allocatable   :: sinlon(:)                  ! sine(longitude).
   real, allocatable   :: coslon(:)                  ! cosine(longitude).
   real, allocatable   :: wsave(:)                   ! Prime values for fast Fourier transform.
   real, allocatable   :: alp(:)                     ! Associated Legendre Polynomial.
   real, allocatable   :: power(:)                   ! Power spectrum (n).
   real, allocatable   :: total_power(:)             ! Total Power spectrum (averaged over time/members).

   complex, allocatable:: cv(:)                      ! Control variable vector.
   real,    allocatable:: rcv(:)                     ! Real Control variable vector.


   namelist / gen_be_stage4_nl / start_date, end_date, interval, variable, gaussian_lats, &
                                 be_method, ne, k, testing_spectral, expt, dat_dir

   pi_over_180 = pi / 180.0

!---------------------------------------------------------------------------------------------
!   write(6,(a/)) [1] Read in 2D perturbation fields for variable , variable
!---------------------------------------------------------------------------------------------

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   gaussian_lats = .false.
   be_method = 'NMC'
   ne = 1
   k = 1
   testing_spectral = .true.
   expt = 'gen_be_stage4'
   dat_dir = '/mmmtmp1/dmbarker'

   open(unit=namelist_unit, file='gen_be_stage4_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_nl)
   close(namelist_unit)

   write(ck,'(i2)')k
   if ( k < 10 ) ck = '0'//ck(2:2)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
!   write(6,(4a)) Computing horizontal power spectra for period , start_date,  to , end_date
!   write(6,(a,i8,a)) Interval between dates = , interval, hours.
!   write(6,(a,i8)) Number of ensemble members at each time = , ne
!   write(6,(3a,i4)) Variable , variable,  at level , k

   date = start_date
   cdate = sdate
   first_time = .true.
   num_states = 1

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!         write(6,(5a,i4))    Calculate spectra for date , date, , variable , trim(variable), &
!                            and member , member

!---------------------------------------------------------------------------------------------
!        Read in data for given variable/level/time/member:
!---------------------------------------------------------------------------------------------

         filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
         filename = trim(filename)//'.'//trim(be_method)//'.e'//ce//'.'//ck
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk ! nk not used.
         read(iunit)data_on_levels, use_global_eofs

         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( field(1:ni,1:nj) )
         end if
         read(iunit)field
         close(iunit)

         if ( first_time ) then
            if ( data_on_levels ) then
               write(6,'(a)')' Input Data is on model levels.'
            else
               write(6,'(a)')' Input Data is projected on vertical modes.'
               if ( use_global_eofs ) then
                  write(6,'(a)')' Input 2D field is projected using global EOFs.'
               else
                  write(6,'(a)')' Input 2D field is projected using local EOFs.'
               end if
            end if

!---------------------------------------------------------------------------------------------
!           write(6,(a)) Initialize spectral transforms.
!---------------------------------------------------------------------------------------------

            inc = 1
            lenr = inc * (ni - 1 ) + 1
            lensav = ni + int(log(real(ni))) + 4
            lenwrk = ni

            allocate( lat(1:nj) )
            allocate( sinlat(1:nj) )
            allocate( coslat(1:nj) )
            allocate( int_wgts(1:nj) )
            allocate( lon(1:ni) )
            allocate( sinlon(1:ni) )
            allocate( coslon(1:ni) )
            allocate( wsave(1:lensav) )

            max_wavenumber =  ni / 2 - 1
            allocate ( power(0:max_wavenumber) )
            allocate ( total_power(0:max_wavenumber) )
            total_power(:) = 0.0

            alp_size = ( nj + 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 4
            allocate( alp( 1:alp_size) )

            call da_initialize_h( ni, nj, max_wavenumber, lensav, alp_size, &
                                  wsave, lon, sinlon, coslon, lat, sinlat, coslat, &
                                  int_wgts, alp )

            v_size = ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2
            allocate( cv( 1:v_size) )
            allocate( rcv( 1:2*v_size) )


!           Test horizontal transforms:
            if ( testing_spectral ) then
               call da_test_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                      alp_size, 2*v_size, alp, wsave, int_wgts, field )
            end if
            first_time = .false.
         end if

!---------------------------------------------------------------------------------------------
!         write(6,(a)) Perform gridpoint to spectral decomposition.
!---------------------------------------------------------------------------------------------
         call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                   alp_size, 2*v_size, alp, wsave, int_wgts, rcv, field)

!---------------------------------------------------------------------------------------------
!         write(6,(a)) Calculate power spectra.
!---------------------------------------------------------------------------------------------

         do i = 1, v_size
         cv(i) = cmplx ( rcv(2*i-1), rcv(2*i) )
         end do

         call da_calc_power( max_wavenumber, v_size, cv, power )

         coeffa = 1.0 / real(num_states)
         coeffb = real(num_states-1) * coeffa

         do n = 0, max_wavenumber
            total_power(n) = total_power(n) * coeffb + power(n) * coeffa
!            write(6,(2i4,2f18.6))num_states, n, power(n), total_power(n)
         end do

         num_states = num_states + 1
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do     ! End loop over times.

   variance = sum( total_power(0:max_wavenumber) )   
   write(6,'(3a,i2,a,1pe15.5)')' Variable = ', trim(variable), ', Vertical Index = ', &
                                k, ', Variance = ', variance

   filename = trim(variable)//'/'//trim(variable)
   filename = trim(filename)//'.'//trim(be_method)//'.'//ck//'.spectrum'
   open (ounit, file = filename, form='unformatted')
   write(ounit)variable
   write(ounit)max_wavenumber, k
   write(ounit)data_on_levels, use_global_eofs
   write(ounit)total_power

end program gen_be_stage4
