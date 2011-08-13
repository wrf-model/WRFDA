program gen_be_stage2a
!---------------------------------------------------------------------- 
! Purpose: Compute the unbalanced componenet of the control variables 
!          using the regression coefficients from stage 2.
!
! History:
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! 01/04/11 J.-F. Caron (Met Office)
!          Adaptation for the UK MetO model and variational system
! -------- End History
!
!----------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len, cv_options
   use da_gen_be, only : da_filter_regcoeffs, da_perform_2drf
   use da_tools_serial, only : da_get_unit, da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date ! Starting and ending dates.
   character*10        :: date, new_date       ! Current date (ccyymmddhh).
   character*10        :: variable             ! Variable name
   character(len=filename_len)        :: filename             ! Input filename.
   character*3         :: ce                   ! Member index -> character.
   character (len=4)   :: masscv               ! Mass ctrl variable = pres or temp
   character (len=10)  :: balpres              ! Balance pressure = purestats or linestats

   integer             :: ni, nj, nk, nkdum    ! Grid dimensions.
   integer             :: i, j, k, member      ! Loop counters.
   integer             :: b                    ! Bin marker.
   integer             :: sdate, cdate, edate  ! Starting, current ending dates.
   integer             :: interval             ! Period between dates (hours).
   integer             :: ne                   ! Number of ensemble members.
   integer             :: bin_type             ! Type of bin to average over.
   integer             :: num_bins             ! Number of bins (3D fields).
   integer             :: num_bins2d           ! Number of bins (2D fields).
   integer             :: num_passes           ! Recursive filter passes.
   real                :: lat_min, lat_max     ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat         ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max     ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt         ! Used if bin_type = 2 (m).
   real                :: rf_scale             ! Recursive filter scale.

   real, allocatable   :: psi(:,:,:)           ! psi.
   real, allocatable   :: chi(:,:,:)           ! chi.
   real, allocatable   :: temp(:,:,:)          ! Temperature.
   real, allocatable   :: pres(:,:,:)          ! Pressure.
   real, allocatable   :: lbpres(:,:,:)        ! Linear Balance Pressure.
   real, allocatable   :: ps(:,:)              ! Surface pressure.
   integer, allocatable:: bin(:,:,:)           ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)           ! Bin assigned to each 2D point.

   real, allocatable   :: regcoeff1(:)         ! psi/chi regression cooefficient
   real, allocatable   :: regcoeff2(:,:)       ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)     ! psi/T regression cooefficient.
   real, allocatable   :: regcoeff3p(:,:,:)    ! psi/p or (pg/p) regression cooefficient

   namelist / gen_be_stage2a_nl / start_date, end_date, interval, &
                                  ne, num_passes, rf_scale, cv_options, &
                                  masscv, balpres

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   ne = 1
   num_passes = 0
   rf_scale = 1.0
   cv_options = 5
   masscv = ""
   balpres = ""

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

   print*,"masscv  = ", masscv
   print*,"balpres = ", balpres

   if (masscv.eq."temp") then
      write(UNIT=6,FMT='(a)') " masscv = Temperature"
   else
      write(UNIT=6,FMT='(a)') " masscv = Pressure"
   endif
   if (balpres.eq."purestats") then
      write(UNIT=6,FMT='(a)') &
      " balpres = Purely statistical: Psi-(P or T) regression)"
   else
      write(UNIT=6,FMT='(a)') &
      " balpres = linear balance and Pb-P regression"
   endif

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')'      [2] Read regression coefficients and bin information:'
!--------------------------------------------------------------------------------------------- 

   filename = 'gen_be_stage2.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   allocate( psi(1:ni,1:nj,1:nk) )
   allocate( chi(1:ni,1:nj,1:nk) )
   allocate( temp(1:ni,1:nj,1:nk) )
   allocate( pres(1:ni,1:nj,1:nk) )
   allocate( lbpres(1:ni,1:nj,1:nk) )
   allocate( ps(1:ni,1:nj) )
   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff3p(1:nk,1:nk,1:num_bins2d) )

   read(iunit)regcoeff1
   if (masscv.eq."temp") then
      read(iunit)regcoeff2
      read(iunit)regcoeff3
      regcoeff3p=0.0
   else
      read(iunit)regcoeff3p
      regcoeff2=0.0
      regcoeff3=0.0
   endif
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
   write(6,'(a,i4,a)') '[3] Apply ', &
      num_passes, ' pass recursive filter to regression coefficients:'
!---------------------------------------------------------------------------------------------
      call da_filter_regcoeffs( ni, nj, nk, num_bins, num_bins2d, num_passes, rf_scale, bin, &
!rizvi                                regcoeff1, regcoeff2, regcoeff3, regcoeff3p )
                                regcoeff1, regcoeff2, regcoeff3)
   else
      write(6,'(a)') &
      ' [3] num_passes = 0. Bypassing recursive filtering.'
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(a)')'   [4] Read standard fields, and compute control variable fields:'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Calculating unbalanced fields for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read psi predictor:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

         if (balpres.ne."purestats") then
!           Read lbp predictor:
            variable = 'lbp'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)ni, nj, nk
            read(iunit)lbpres
            close(iunit)
         endif

!        Calculate unbalanced chi:
         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
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
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)chi
         close(ounit)

!        Calculate unbalanced T:
         if (masscv.eq."temp") then
            variable = 't'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
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
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)temp
            close(ounit)
         else
            variable = 'p'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)ni, nj, nk
            read(iunit)pres
            close(iunit)
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
                  do k = 1, nk
                     if (balpres.eq."purestats") then
                     pres(i,j,k) = pres(i,j,k) - SUM(regcoeff3p(k,1:nk,b) * psi(i,j,1:nk))
                     else
                     pres(i,j,k) = pres(i,j,k) - SUM(regcoeff3p(k,1:nk,b) * lbpres(i,j,1:nk))
                     endif
                  end do
               end do
            end do
            variable = 'p_u'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)pres
            close(ounit)
         endif

         if (masscv.eq."temp") then
!        Calculate unbalanced ps:
         variable = 'ps'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
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
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit)ps
         close(ounit)

         endif

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

end program gen_be_stage2a
