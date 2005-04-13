program gen_be_stage1
!
!---------------------------------------------------------------------- 
! Purpose : to remove the binned mean from the difference fields.
!
! Input   : binary files: diff.ccyy-mm-dd_hh:00:00.ce for ENS or
!                         ...............................
!                         diff.ccyy-mm-dd_hh:00:00    for NMC.
!                         ...............................
!
! Output : binary files for use of the gen_be_stage2:
!
!----------------------------------------------------------------------
!
   use da_constants
   use da_gen_be

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates (ccyymmddhh).
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name.
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*3         :: ce                         ! Ensemble member index.
   character*80        :: dat_dir                    ! Input data directory.
   character*80        :: expt                       ! Experiment ID.
   character*80        :: filename                   ! Input filename.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: member, b, i, j, k         ! Loop counters.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Interval between file times (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: dphi                       ! Latitude interval (not used).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: first_time                 ! True if first file.
   logical             :: remove_mean                ! Remove time/ensemble/area mean.

   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t_prime(:,:,:)             ! Temperature perturbation.
   real, allocatable   :: psi_prime(:,:,:)           ! Streamfunction perturbation.
   real, allocatable   :: chi_prime(:,:,:)           ! Velocity Potential perturbation.
   real, allocatable   :: rh_prime(:,:,:)            ! Relative Humidity Perturbation.
   real, allocatable   :: height(:,:,:)              ! Geopotential height.
   real, allocatable   :: latitude(:,:)              ! Latitude (radians)
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: psi_mean(:)                ! Mean field in bin.
   real, allocatable   :: chi_mean(:)                ! Mean field in bin.
   real, allocatable   :: t_mean(:)                  ! Mean field in bin.
   real, allocatable   :: rh_mean(:)                 ! Mean field in bin.
   real, allocatable   :: ps_mean(:)                 ! Mean field in bin.

   namelist / gen_be_stage1_nl / start_date, end_date, interval, &
                                 be_method, ne, bin_type, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 remove_mean, gaussian_lats, expt, dat_dir

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   be_method = 'NMC'
   ne = 1
   bin_type = 1         ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   remove_mean = .true.
   gaussian_lats = .false.
   expt = 'gen_be_stage1'
   dat_dir = '/data2/hcshin/youn/DIFF63'

   open(unit=namelist_unit, file='gen_be_stage1_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   first_time = .true.

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from standard files, and calculate mean fields'
!---------------------------------------------------------------------------------------------

   do while ( cdate <= edate )
      do member = 1, ne

         write(6,'(a,a)')'    Processing data for date ', date

         filename = 'diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if (be_method.eq.'ENS') then
           if (member.lt.10) write(ce,'(I1)') member
           if (member.ge.10.and.member.lt.100) write(ce,'(I2)') member
           filename = trim(filename)//'.'//trim(ce)
           print*,filename
         endif

         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
!         read(iunit)date, ni, nj, nk, dphi
         read(iunit)date, ni, nj, nk

         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( ps_prime(1:ni,1:nj) )
            allocate( t_prime(1:ni,1:nj,1:nk) )
            allocate( psi_prime(1:ni,1:nj,1:nk) )
            allocate( chi_prime(1:ni,1:nj,1:nk) )
            allocate( rh_prime(1:ni,1:nj,1:nk) )
            allocate( height(1:ni,1:nj,1:nk) )
            allocate( latitude(1:ni,1:nj) )
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
         end if

         read(iunit)psi_prime
         read(iunit)chi_prime
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)ps_prime

         read(iunit)height

         read(iunit)latitude

         if ( first_time ) then
            call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, latitude, height )

            allocate( psi_mean(1:num_bins) )
            allocate( chi_mean(1:num_bins) )
            allocate( t_mean(1:num_bins) )
            allocate( rh_mean(1:num_bins) )
            allocate( ps_mean(1:num_bins2d) )
            allocate( bin_pts(1:num_bins) )
            allocate( bin_pts2d(1:num_bins2d) )
            psi_mean(:) = 0.0
            chi_mean(:) = 0.0
            t_mean(:) = 0.0
            rh_mean(:) = 0.0
            ps_mean(:) = 0.0
            bin_pts(:) = 0
            bin_pts2d(:) = 0
            first_time = .false.
         end if

         close(iunit)

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Calculate time/bin mean.
!---------------------------------------------------------------------------------------------

!        Calculate means of 3D fields:
         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  bin_pts(b) = bin_pts(b) + 1
                  coeffa = 1.0 / real(bin_pts(b))
                  coeffb = real(bin_pts(b)-1) * coeffa
                  psi_mean(b) = coeffb * psi_mean(b) + coeffa * psi_prime(i,j,k)
                  chi_mean(b) = coeffb * chi_mean(b) + coeffa * chi_prime(i,j,k)
                  t_mean(b)   = coeffb * t_mean(b)   + coeffa * t_prime(i,j,k)
                  rh_mean(b)  = coeffb * rh_mean(b)  + coeffa * rh_prime(i,j,k)
               end do
            end do
         end do

!        Calculate means of 2D fields:
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               ps_mean(b) = coeffb * ps_mean(b) + coeffa * ps_prime(i,j)
            end do
         end do

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!   do b = 1, num_bins
!      write(6,'(a,2i8,2f20.5)')' Mean_field for psi', b, bin_pts(b), psi_mean(b)
!   end do
!   do b = 1, num_bins
!      write(6,'(a,2i8,2f20.5)') 'Mean_field for chi', b, bin_pts(b), chi_mean(b)
!   end do
!   do b = 1, num_bins
!      write(6,'(a,2i8,2f20.5)') 'Mean_field for t', b, bin_pts(b), t_mean(b)
!   end do
!   do b = 1, num_bins
!      write(6,'(a,2i8,2f20.5)') 'Mean_field for rh', b, bin_pts(b), rh_mean(b)
!   end do
!   do b = 1, num_bins2d
!      write(6,'(a,2i8,2f20.5)') 'Mean_field for ps', b, bin_pts2d(b), ps_mean(b)
!   end do

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields again, and remove time/ensemble/area mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      do member = 1, ne

         write(6,'(a,a)')'    Removing mean for date ', date

         filename = 'diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'

         if (be_method.eq.'ENS') then
         if (member.lt.10) write(ce,'(I1)') member
         if (member.ge.10.and.member.lt.100) write(ce,'(I2)') member
         filename = trim(filename)//'.'//trim(ce)
         print*,filename
         endif

         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
!         read(iunit)date, ni, nj, nk, dphi
         read(iunit)date, ni, nj, nk

         read(iunit)psi_prime
         read(iunit)chi_prime
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)ps_prime
         read(iunit)height
         read(iunit)latitude
         close(iunit)

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Remove mean.
!---------------------------------------------------------------------------------------------

         if ( remove_mean ) then
!           3D fields:
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     b = bin(i,j,k)
                     psi_prime(i,j,k) = psi_prime(i,j,k) - psi_mean(b)
                     chi_prime(i,j,k) = chi_prime(i,j,k) - chi_mean(b)
                     t_prime(i,j,k) = t_prime(i,j,k) - t_mean(b)
                     rh_prime(i,j,k) = rh_prime(i,j,k) - rh_mean(b)
                  end do
               end do
            end do

!           2D fields:
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
                  ps_prime(i,j) = ps_prime(i,j) - ps_mean(b)
               end do
            end do
         end if

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Write fields to file for further processing.
!---------------------------------------------------------------------------------------------

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!        Write necessary full-fields:
         variable = 'fullflds'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)latitude
         write(ounit)height
         close(ounit)

!        Write psi:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)psi_prime
         close(ounit)

!        Write chi:
         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)chi_prime
         close(ounit)

!        Write T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)t_prime
         close(ounit)

!        Write RH:
         variable = 'rh'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)rh_prime
         close(ounit)

!        Write ps:
         variable = 'ps' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)ps_prime
         close(ounit)

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!  Finally, write bin info:

   filename = 'bin.data'
   open (ounit, file = filename, form='unformatted')
   write(ounit)bin_type
   write(ounit)lat_min, lat_max, binwidth_lat
   write(ounit)hgt_min, hgt_max, binwidth_hgt
   write(ounit)num_bins, num_bins2d
   write(ounit)bin(1:ni,1:nj,1:nk)
   write(ounit)bin2d(1:ni,1:nj)
   close(ounit)

end program gen_be_stage1
