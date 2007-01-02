program gen_be_stage3

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
   character*2         :: ck                         ! Level index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: i, j, k, k1, k2, b, m, member ! Loop counters.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: first_time                 ! True if first file.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.
   logical             :: data_on_levels             ! True if output level data (diagnostic).
   logical             :: ldum1, ldum2

   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: latitude(:,:)              ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)              ! Height field.
   real, allocatable   :: field(:,:,:)               ! Input field.
   real, allocatable   :: field_out(:,:,:)          ! Field projected into EOF space.
   real, allocatable   :: vertical_wgt(:,:,:)        ! Inner product for vertical transform.
   real, allocatable   :: bv(:,:,:)                  ! Vertical BE for this time.
   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable   :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.
   real, allocatable   :: e_vec(:,:)                 ! Domain-averaged eigenvectors.
   real, allocatable   :: e_val(:)                   ! Domain-averaged eigenvalues.
   real, allocatable   :: evec(:,:,:)                ! Gridpoint eigenvectors.
   real, allocatable   :: eval(:,:)                  ! Gridpoint sqrt(eigenvalues).

   namelist / gen_be_stage3_nl / start_date, end_date, interval, variable, &
                                 be_method, ne, bin_type, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 testing_eofs, use_global_eofs, data_on_levels, &
                                 expt, dat_dir

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   vertical_ip = 0

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   be_method = 'NMC'
   ne = 1
   bin_type = 1
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   testing_eofs = .true.
   use_global_eofs = .true.
   data_on_levels = .false.
   expt = 'gen_be_stage3'
   dat_dir = '/mmmtmp1/dmbarker'

   open(unit=namelist_unit, file='gen_be_stage3_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage3_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing vertical error statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   first_time = .true.

   if ( .not. data_on_levels ) then ! Can bypass [2] and [3] if outputing on levels.

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Process fields for variable ', variable
!---------------------------------------------------------------------------------------------

   do while ( cdate <= edate )
      do member = 1, ne
         write(6,'(5a,i4)')'    Processing data for date ', date, ', variable ', trim(variable), &
                           ' and member ', member

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!        Read Full-fields:
         filename = 'fullflds'//'/'//date(1:10)//'.'//'fullflds'//'.'//trim(be_method)//'.e'//ce
         open (iunit, file = filename, form='unformatted')

         read(iunit)ni, nj, nk
         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( latitude(1:ni,1:nj) )
            allocate( height(1:ni,1:nj,1:nk) )
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
         end if
         read(iunit)latitude
         read(iunit)height

         close(iunit)

         if ( first_time ) then
!           Create and sort into bins:
            call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, latitude, height )

            allocate( bin_pts2d(1:num_bins2d) )
            bin_pts2d(:) = 0
         end if

         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
! For 2d fields (YRG 06/17/2005):
         if (trim(variable) == 'ps_u' .or. trim(variable) == 'ps') then
              filename = trim(filename)//'.01'
!              print '(4a)',"variable=",trim(variable),"  filename =",filename
         endif
! ...............................
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
!         print '(a,3i5)', "ni, nj, nk:", ni, nj, nk
         if ( first_time ) then
! Allocate the arrays:
            allocate( field(1:ni,1:nj,1:nk) )
            allocate( bv(1:nk,1:nk,1:num_bins2d) )
            bv(:,:,:) = 0.0
            first_time = .false.
         endif
! For 2d field:
         if (nk == 1) read(iunit) ldum1, ldum2

         read(iunit)field

         call remove_horizontal_mean(field, ni, nj, nk)
!   rizvi
         close(iunit)

         call remove_horizontal_mean(field, ni, nj, nk)

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               do k1 = 1, nk
                  do k2 = 1, k1
                     bv(k1,k2,b) = coeffb * bv(k1,k2,b) + coeffa * field(i,j,k1) * field(i,j,k2)
                  end do
               end do
            end do
         end do
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!  Fill in upper-right part of BE matrix by symmetry:

   do b = 1, num_bins2d
      do k1 = 1, nk
         do k2 = k1+1, nk ! Symmetry.
            bv(k1,k2,b) = bv(k2,k1,b)
         end do
      end do
   end do

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [3] Calculate eigenvectors and eigenvalues for variable ', variable
!---------------------------------------------------------------------------------------------

   allocate( work(1:nk,1:nk) )
   allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
   allocate( e_val_loc(1:nk,1:num_bins2d) )
   allocate( e_vec(1:nk,1:nk) )
   allocate( e_val(1:nk) )

!  Latitudinally varying BE decomposition:
   do b = 1, num_bins2d
      write(6,'(2(a,i6))')' Calculate eigenvectors and eigenvalues for bin ', b, &
	                 ' of ', num_bins2d
	  
      work(1:nk,1:nk) = bv(1:nk,1:nk,b)
!      print '(a,i6,2x,e15.8)', "bin=",b, sqrt(bv(1,1,b))
      call da_eof_decomposition( nk, work, e_vec, e_val )
!      print '(i6,a,e15.8,a,e15.8)', b,"  e_val=",e_val(1), "  e_vec=",e_vec(1,1)
      e_vec_loc(1:nk,1:nk,b) = e_vec(1:nk,1:nk)
      e_val_loc(1:nk,b) = e_val(1:nk)
   end do

!  Globally-averaged BE decomposition:
   work(1:nk,1:nk) = 0.0
   do b = 1, num_bins2d
      work(1:nk,1:nk) = work(1:nk,1:nk) + bv(1:nk,1:nk,b)
   end do
   work(1:nk,1:nk) = work(1:nk,1:nk) / real( num_bins2d )
!   print '(a,e15.8)', "Global bv=", sqrt(work(1,1))
   call da_eof_decomposition( nk, work, e_vec, e_val )
!   print '(a,e15.8,a,e15.8)', "e_val=",e_val(1), "  e_vec=",e_vec(1,1)
   if ( testing_eofs ) then
      call da_eof_decomposition_test( nk, work, e_vec, e_val )
   end if

!  Output eigenvectors, eigenvalues for use in 3/4Var:
   filename = 'gen_be_stage3.'//trim(variable)//'.'//trim(be_method)//'.dat'
   open (ounit, file = filename, form='unformatted')
   write(ounit)variable
   write(ounit)nk, num_bins2d
   write(ounit)e_vec
   write(ounit)e_val
   write(ounit)e_vec_loc
   write(ounit)e_val_loc
   close(ounit)

!  Decide on local or global EOFs for spectral decomposition:
   if ( use_global_eofs ) then
      do b = 1, num_bins2d
         e_vec_loc(1:nk,1:nk,b) = e_vec(1:nk,1:nk)
         e_val_loc(1:nk,b) = e_val(1:nk)
      end do
   end if

!  Map binned eigenvectors to x, y grid, and take sqrt(this is used in 3/4D-Var):
   allocate( evec(1:nj,1:nk,1:nk) )
   allocate( eval(1:nj,1:nk) )

   do j = 1, nj
      do i = 1, ni
         b = bin2d(i,j)
         evec(j,1:nk,1:nk) = e_vec_loc(1:nk,1:nk,b)
         eval(j,1:nk) = sqrt(e_val_loc(1:nk,b))
      end do
   end do

   end if ! End bypass [2] and [3] if outputing on levels.

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Transform perturbations (or not), and output.'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate
   first_time = .true.

   do while ( cdate <= edate )
      do member = 1, ne
         write(6,'(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
! For 2d fields (YRG 06/17/2005):
         if (trim(variable) == 'ps_u' .or. trim(variable) == 'ps') then
              filename = trim(filename)//'.01'
!              print '(4a)',"variable=",trim(variable),"  filename =",filename
         endif
! ...............................
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
!         print '(a,3i5)', "ni, nj, nk:", ni, nj, nk
         if ( first_time ) then
! Allocate the arrays:
            if ( data_on_levels) allocate( latitude(1:ni,1:nj) )   ! Not allocated earlier.
            if ( data_on_levels) allocate( field(1:ni,1:nj,1:nk) ) ! Not allocated earlier.
            allocate( field_out(1:ni,1:nj,1:nk) )
            allocate( vertical_wgt(1:ni,1:nj,1:nk) )
            vertical_wgt(1:ni,1:nj,1:nk) = 1.0 ! vertical_ip = 0 hardwired.
            first_time = .false.
         endif
! For 2d field:
         if (nk == 1) read(iunit) ldum1, ldum2

         read(iunit)field
         close(iunit)

         if ( data_on_levels ) then
!           Keep data on vertical levels:
            field_out(:,:,:) = field(:,:,:)
         else
!           Project fields onto vertical modes:
            call da_transform_vptovv( evec, eval, vertical_wgt, &
                                      field, field_out, nk, &
                                      1, ni, 1, nj, 1, nk, & ! WRF ids, ide etc.
                                      1, ni, 1, nj, 1, nk, & ! WRF ims, ime etc.
                                      1, ni, 1, nj, 1, nk )  ! WRF its, ite etc.
         end if

!         write(6,'(i12,2f18.4)')cdate, &
!                                sqrt( sum(field(1:ni,1:nj,1:nk)**2)     / real(ni*nj*nk) ), &
!                                sqrt( sum(field_out(1:ni,1:nj,1:nk)**2) / real(ni*nj*nk) )

! For 2D physical fields, no 2D fields in eigenvector space are needed: 
         if (nk == 1) cycle

!        Output fields (split into 2D files to allow parallel horizontal treatment):

         do k = 1, nk
            write(ck,'(i2)')k
            if ( k < 10 ) ck = '0'//ck(2:2)
!           Assumes variable directory has been created by script:
            filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
            filename = trim(filename)//'.'//trim(be_method)//'.e'//ce//'.'//ck
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, k
            write(ounit)data_on_levels, use_global_eofs
            write(ounit)field_out(1:ni,1:nj,k)
            close(ounit)
         end do
      end do ! End loop over members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do

end program gen_be_stage3

  subroutine remove_horizontal_mean(field,n1,n2,n3)
  implicit none
  integer, intent(in)   :: n1, n2, n3
  real, intent(inout)   :: field(1:n1,1:n2,1:n3)

  integer    ::  i, j, k        ! loop counters
  real       :: avg, inv_n1n2

  inv_n1n2 = 1.0/real(n1*n2)
  do k = 1, n3
  avg = 0
    do j=1,n2
    do i=1,n1
    avg = avg + field(i,j,k)
    end do
    end do
  field(1:n1,1:n2,k) = field(1:n1,1:n2,k) - avg*inv_n1n2
  end do
 end subroutine remove_horizontal_mean

