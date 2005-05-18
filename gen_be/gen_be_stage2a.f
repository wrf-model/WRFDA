program gen_be_stage2a

   use da_constants
   use da_gen_be

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*80        :: dat_dir                    ! Input data directory.
   character*80        :: expt                       ! Experiment ID.
   character*80        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
   integer             :: i, j, k, member, k2, k3, m ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: mmax                       ! Maximum mode (after variance truncation)..
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: num_passes                 ! Recursive filter passes.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   real                :: total_variance             ! Total variance of <psi psi> matrix.
   real                :: cumul_variance             ! Cumulative variance of <psi psi> matrix.
   real                :: summ                       ! Summation dummy.
   real                :: rf_scale                   ! Recursive filter scale.
   logical             :: first_time                 ! True if first file.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: ldum1, ldum2               ! Dummy logicals.

   real, allocatable   :: latitude(:,:)              ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)              ! Height field.
   real, allocatable   :: psi(:,:,:)                 ! psi.
   real, allocatable   :: chi(:,:,:)                 ! chi.
   real, allocatable   :: temp(:,:,:)                ! Temperature.
   real, allocatable   :: ps(:,:)                    ! Surface pressure.
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: covar1(:)                  ! Covariance between input fields.
   real, allocatable   :: covar2(:,:)                ! Covariance between input fields.
   real, allocatable   :: covar3(:,:,:)              ! Covariance between input fields.
   real, allocatable   :: var1(:)                    ! Autocovariance of field.
   real, allocatable   :: var2(:,:,:)                ! Autocovariance of field.
   real, allocatable   :: var2_inv(:,:,:)            ! Inverse Autocovariance of field.

   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: evec(:,:)                  ! Gridpoint eigenvectors.
   real, allocatable   :: eval(:)                    ! Gridpoint sqrt(eigenvalues).
   real, allocatable   :: LamInvET(:,:)              ! ET/sqrt(Eigenvalue).
   real, allocatable   :: regcoeff1(:)               ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)             ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)           ! psi/T regression cooefficient.

   namelist / gen_be_stage2a_nl / start_date, end_date, interval, &
                                  be_method, ne, &
                                  num_passes, rf_scale, & 
                                  testing_eofs, expt, dat_dir

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   be_method = 'NMC'
   ne = 1
   num_passes = 2
   rf_scale = 1.0
   testing_eofs = .true.
   expt = 'gen_be_stage2a'
   dat_dir = '/mmmtmp1/dmbarker'

   open(unit=namelist_unit, file='gen_be_stage2a_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2a_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing control variable fields'
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read regression coefficients and bin information:'
!--------------------------------------------------------------------------------------------- 

   filename = 'gen_be_stage2.'//trim(be_method)//'.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   allocate( psi(1:ni,1:nj,1:nk) )
   allocate( chi(1:ni,1:nj,1:nk) )
   allocate( temp(1:ni,1:nj,1:nk) )
   allocate( ps(1:ni,1:nj) )
   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

   read(iunit)regcoeff1
   read(iunit)regcoeff2
   read(iunit)regcoeff3
   close(iunit)

!  Read bin info:
   filename = 'bin.data'
   open (iunit, file = filename, form='unformatted')
   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d
   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)
   close(iunit)

   if ( num_passes > 0 ) then

!---------------------------------------------------------------------------------------------
      write(6,'(a,i4,a)')' [3] Apply ', num_passes, ' pass recursive filter to regression coefficients:'
!---------------------------------------------------------------------------------------------
      call da_filter_regcoeffs( ni, nj, nk, num_bins, num_bins2d, num_passes, rf_scale, bin, &
                                regcoeff1, regcoeff2, regcoeff3 )
   else
      write(6,'(a)')' [3] num_passes = 0. Bypassing recursive filtering.'
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [4] Read standard fields, and compute control variable fields:'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Calculating unbalanced fields for date ', date

      do member = 1, ne

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!        Read psi:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

!--------------------------------------------------------------------------------------
!        Re-read chi. Calculate, and output unbalanced chi:
!--------------------------------------------------------------------------------------

         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi
         close(iunit)

         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  chi(i,j,k) = chi(i,j,k) - regcoeff1(b) * psi(i,j,k)
               end do
            end do
         end do

         variable = 'chi_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)chi
         close(ounit)

!--------------------------------------------------------------------------------------
!        Re-read surface pressure (ps). Calculate, and output unbalanced ps:
!--------------------------------------------------------------------------------------

!        Read ps:
         variable = 'ps'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ldum1, ldum2 ! Dummy logicals.
         read(iunit)ps
         close(iunit)

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               ps(i,j) = ps(i,j) - SUM(regcoeff2(1:nk,b) * psi(i,j,1:nk))
            end do
         end do

         variable = 'ps_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit)ldum1, ldum2 ! Dummy logicals.
         write(ounit)ps
         close(ounit)

!--------------------------------------------------------------------------------------
!        Re-read temperature. Calculate, and output unbalanced temperature:
!--------------------------------------------------------------------------------------

!        Read T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp
         close(iunit)

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               do k = 1, nk
                  temp(i,j,k) = temp(i,j,k) - SUM(regcoeff3(k,1:nk,b) * psi(i,j,1:nk))
               end do
            end do
         end do

         variable = 't_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)temp
         close(ounit)

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

end program gen_be_stage2a
