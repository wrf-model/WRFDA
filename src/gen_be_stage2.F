program gen_be_stage2
!---------------------------------------------------------------------- 
! Purpose: Compute regression coefficients.
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! 01/04/11 J.-F. Caron (Met Office)
!          Adaptation for the UK MetO model and variational system
! 01/07/11 Y. Michel (Meteo France)
!          Phasing geographical masks, combining with former stage 2a
!----------------------------------------------------------------------

   use da_control, only : stdout, stderr, filename_len
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_gen_be, only : da_eof_decomposition,da_eof_decomposition_test, da_invert_var, &
   	read_2d_field, read_3d_field, write_2d_field, write_3d_field, update_rain_bin

   implicit none

   real, parameter     :: variance_threshold = 1e-6  ! Percentage of variance discarded.

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   character (len=3)   :: model                      ! Model name 'WRF', 'GSI' or 'UM' etc. 
   character (len=4)   :: masscv                     ! Mass ctrl variable = pres or temp
   character (len=10)  :: balpres                    ! Balance pressure = purestats or linestats

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
   
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   real                :: total_variance             ! Total variance of <psi psi> matrix.
   real                :: cumul_variance             ! Cumulative variance of <psi psi> matrix.
   real                :: summ                       ! Summation dummy.
   
   logical             :: first_time                 ! True if first file.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: nobaldiv                   ! True means chi_u = chi.

   real, allocatable   :: latitude(:,:)              ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)              ! Height field.
   real, allocatable   :: psi(:,:,:)                 ! psi.
   real, allocatable   :: chi(:,:,:)                 ! chi.
   real, allocatable   :: temp(:,:,:)                ! Temperature.
   real, allocatable   :: pres(:,:,:)                ! Pressure.
   real, allocatable   :: lbpres(:,:,:)              ! linear balance pressure.
   real, allocatable   :: ps(:,:)                    ! Surface pressure.
   
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   
   real, allocatable   :: covar1(:)                  ! Covariance between input fields.
   real, allocatable   :: covar2(:,:)                ! Covariance between input fields.
   real, allocatable   :: covar3(:,:,:)              ! Covariance between input fields.
   real, allocatable   :: covar3p(:,:,:)             ! Covariance between input fields.
   
   real, allocatable   :: var1(:)                    ! Autocovariance of field.
   real, allocatable   :: var2(:,:,:)                ! Autocovariance of field.
   real, allocatable   :: var2_inv(:,:,:)            ! Inverse Autocovariance of field.
   real, allocatable   :: var2p(:,:,:)               ! Autocovariance of field.
   real, allocatable   :: var2p_inv(:,:,:)           ! Inverse Autocovariance of field.

   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: evec(:,:)                  ! Gridpoint eigenvectors.
   real, allocatable   :: eval(:)                    ! Gridpoint sqrt(eigenvalues).
   real, allocatable   :: LamInvET(:,:)              ! ET/sqrt(Eigenvalue).
   real, allocatable   :: regcoeff_psi_chi(:)        ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff_psi_temp(:,:,:)   ! psi/T regression cooefficient.
   real, allocatable   :: regcoeff_lbpb_temp(:,:,:)  ! psi/p or lbp/p regression cooefficient.

   namelist / gen_be_stage2_nl / start_date, end_date, interval, &
                                 ne, testing_eofs, masscv, balpres, &
                                 nobaldiv, model, bin_type

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
   testing_eofs = .true.
   masscv = ""
   balpres = ""
   nobaldiv = .false.

   open(unit=namelist_unit, file='gen_be_stage2_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing regression coefficients'
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   
   ! Note: these flags should be set up with model choice
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

   if ( .not. nobaldiv ) then
      write(UNIT=6,FMT='(a)') &
      " We compute the Psi-Chi regression"
   else
      write(UNIT=6,FMT='(a)') &
      " No Psi-Chi regression : chi_u = chi)"
   endif   

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read fields, and calculate correlations'
!--------------------------------------------------------------------------------------------- 

   first_time = .true.

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Processing data for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read dimensions:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit) ni, nj, nk
	 close(iunit)
	 
         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( psi(1:ni,1:nj,1:nk) )
            allocate( chi(1:ni,1:nj,1:nk) )
            allocate( temp(1:ni,1:nj,1:nk) )
            allocate( ps(1:ni,1:nj) )
            allocate( pres(1:ni,1:nj,1:nk) )
            allocate( lbpres(1:ni,1:nj,1:nk) )
         end if

         if ( first_time ) then
!           Read bin info:
            filename = 'bin.data'
            open (iunit, file = filename, form='unformatted')
            read(iunit)bin_type
            read(iunit)lat_min, lat_max, binwidth_lat
            read(iunit)hgt_min, hgt_max, binwidth_hgt
            read(iunit)num_bins, num_bins2d
            read(iunit)bin(1:ni,1:nj,1:nk)
            read(iunit)bin2d(1:ni,1:nj)
            close(iunit)

            allocate( bin_pts(1:num_bins) )
            allocate( bin_pts2d(1:num_bins2d) )
            allocate( covar1(1:num_bins) )
            allocate( covar2(1:nk,1:num_bins2d) )
            allocate( covar3(1:nk,1:nk,1:num_bins2d) )
            allocate( covar3p(1:nk,1:nk,1:num_bins2d) )
            allocate( var1(1:num_bins) )
            allocate( var2(1:nk,1:nk,1:num_bins2d) )
            allocate( var2p(1:nk,1:nk,1:num_bins2d) )
            bin_pts(:) = 0
            bin_pts2d(:) = 0
            covar1(:) = 0.0
            covar2(:,:) = 0.0
            covar3(:,:,:) = 0.0
            covar3p(:,:,:) = 0.0
            var1(:) = 0.0
            var2(:,:,:) = 0.0
            var2p(:,:,:) = 0.0
            first_time = .false.
         end if

!        Read psi:
         variable = 'psi'
	 call read_3d_field(variable, date, ce, iunit, ni, nj, nk, psi)

         if ( .not. nobaldiv ) then
!           Read chi:
            variable = 'chi'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, chi)
         endif

         if (masscv.eq."temp") then
            variable = 't'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, temp)
            variable = 'ps'
	    call read_2d_field(variable, date, ce, iunit, ni, nj, nkdum, ps)
         else
 !       Read pres:
	    variable = 'p'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, pres)
            if (balpres.ne."purestats") then
!           Read linear balance pres:
               variable = 'lbp'
	       call read_3d_field(variable, date, ce, iunit, ni, nj, nk, lbpres)
            endif
         endif
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if   
	 
         if ( .not. nobaldiv ) then
!        Calculate psi/chi covariances:
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     b = bin(i,j,k)
                     bin_pts(b) = bin_pts(b) + 1
                     coeffa = 1.0 / real(bin_pts(b))
                     coeffb = real(bin_pts(b)-1) * coeffa
                     covar1(b) = coeffb * covar1(b) + coeffa * psi(i,j,k) * chi(i,j,k)
                     var1(b) = coeffb * var1(b) + coeffa * psi(i,j,k) * psi(i,j,k)
                  end do
               end do
            end do
         end if

!        Calculate psi/ps, and psi/T, and psi/psi covariances:
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               do k = 1, nk
                  if (masscv.eq."temp") then
!                    psi/ps:
                     covar2(k,b) = coeffb * covar2(k,b) + coeffa * ps(i,j) * psi(i,j,k)
!                    psi/T:
                     do k2 = 1, nk
                        covar3(k,k2,b) = coeffb * covar3(k,k2,b) + &
                                         coeffa * temp(i,j,k) * psi(i,j,k2)
                     end do
                  else
                     if (balpres.eq."purestats") then
!                       psi/p:
                        do k2 = 1, nk
                           covar3p(k,k2,b) = coeffb * covar3p(k,k2,b) + &
                                             coeffa * pres(i,j,k) * psi(i,j,k2)
                        end do
                     else
!                       lbp/p:
                        do k2 = 1, nk
                           covar3p(k,k2,b) = coeffb * covar3p(k,k2,b) + &
                                             coeffa * pres(i,j,k) * lbpres(i,j,k2)
                        end do                      
                     endif
                  endif
!                 psi/psi (symmetric):
                  do k2 = 1, k
                     var2(k,k2,b) = coeffb * var2(k,k2,b) + &
                                    coeffa * psi(i,j,k) * psi(i,j,k2)
                  end do
                  if (balpres.ne."purestats") then
!                    lbp/lbp (symmetric):
                     do k2 = 1, k
                        var2p(k,k2,b) = coeffb * var2p(k,k2,b) + &
                                        coeffa * lbpres(i,j,k) * lbpres(i,j,k2)
                     end do
                  endif
               end do
            end do
         end do
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!  Fill in psi/psi covariance by symmetry:
   do b = 1, num_bins2d
      do k = 1, nk
         do k2 = k+1, nk ! Symmetry.
            var2(k,k2,b) = var2(k2,k,b)
         end do
      end do
   end do

   if (balpres.ne."purestats") then
      do b = 1, num_bins2d
         do k = 1, nk
            do k2 = k+1, nk     ! Symmetry.
               var2p(k,k2,b) = var2p(k2,k,b)
            end do
         end do
      end do
   endif

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [3] Calculate eigenvectors, eigenvalues and inverse for psi/psi covariance '
!---------------------------------------------------------------------------------------------
   allocate( var2_inv(1:nk,1:nk,1:num_bins2d) )
   call da_invert_var(var2_inv, var2, num_bins2d, nk, testing_eofs)
   if (balpres.ne."purestats") then
      allocate( var2p_inv(1:nk,1:nk,1:num_bins2d) )
      call da_invert_var(var2p_inv, var2p, num_bins2d, nk, testing_eofs)
   endif
!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Calculate regression coefficients '
!---------------------------------------------------------------------------------------------

   allocate( regcoeff_psi_chi(1:num_bins) )
   allocate( regcoeff_psi_ps(1:nk,1:num_bins2d) )
   allocate( regcoeff_psi_temp(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff_lbpb_temp(1:nk,1:nk,1:num_bins2d) )

!  psi/chi:
   if ( .not. nobaldiv ) then
      do b = 1, num_bins
         regcoeff_psi_chi(b) = covar1(b) / var1(b)
      end do
   else
      regcoeff_psi_chi(1:num_bins) = 0.0
   end if

   if (masscv.eq."temp") then
!      psi/ps:
      do b = 1, num_bins2d
         do k = 1, nk
            summ = 0.0
            do k2 = 1, nk
               summ = summ + covar2(k2,b) * var2_inv(k2,k,b)
            end do
            regcoeff_psi_ps(k,b) = summ
         end do
      end do
!     psi/T:
      do b = 1, num_bins2d
         do k = 1, nk
            do k2 = 1, nk
               summ = 0.0
               do k3 = 1, nk
                  summ = summ + covar3(k,k3,b) * var2_inv(k3,k2,b)
               end do
               regcoeff_psi_temp(k,k2,b) = summ
            end do
         end do
      end do
   else
!     psi/p or lbp/p:
      do b = 1, num_bins2d
         do k = 1, nk
            do k2 = 1, nk
               summ = 0.0
               do k3 = 1, nk
                  if (balpres.eq."purestats") then
                     summ = summ + covar3p(k,k3,b) * var2_inv(k3,k2,b)
                  else
                     summ = summ + covar3p(k,k3,b) * var2p_inv(k3,k2,b)
                  endif
               end do
               regcoeff_lbpb_temp(k,k2,b) = summ
            end do
         end do
      end do
   endif

!  Output regression coefficients for use in 3/4D-Var:
   filename = 'gen_be_stage2.dat'
   open (ounit, file = filename, form='unformatted')
   write(ounit)ni, nj, nk
   write(ounit)num_bins, num_bins2d
   write(ounit)regcoeff_psi_chi
   if (masscv.eq."temp") then
      write(ounit)regcoeff_psi_ps
      write(ounit)regcoeff_psi_temp
   else
      write(ounit)regcoeff_lbpb_temp
   endif
   close(ounit)

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [5] Compute unbalanced fields'
!---------------------------------------------------------------------------------------------

  date = start_date
  cdate = sdate

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Calculating unbalanced fields for date ', date

      do member = 1, ne
      
         write(ce,'(i3.3)')member
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if 
	 
!        Read psi predictor:
         variable = 'psi'
	 call read_3d_field(variable, date, ce, iunit, ni, nj, nk, psi)
         if (balpres.ne."purestats") then
!           Read lbp predictor:
            variable = 'lbp'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, lbpres)
         endif

!        Calculate unbalanced chi:
         variable = 'chi'
	 call read_3d_field(variable, date, ce, iunit, ni, nj, nk, chi)
	 
         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  chi(i,j,k) = chi(i,j,k) - regcoeff_psi_chi(b) * psi(i,j,k)
               end do
            end do
         end do
         variable = 'chi_u'
	 call write_3d_field(variable, date, ce, iunit, ni, nj, nk, chi)
	 
!        Calculate unbalanced T:
         if (masscv.eq."temp") then
            variable = 't'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, temp)
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
                  do k = 1, nk
                     temp(i,j,k) = temp(i,j,k) - SUM(regcoeff_psi_temp(k,1:nk,b) * psi(i,j,1:nk))
                  end do
               end do
            end do
            variable = 't_u'
	    call write_3d_field(variable, date, ce, iunit, ni, nj, nk, temp)
	    
         else
            variable = 'p'
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, pres)    
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
                  do k = 1, nk
                     if (balpres.eq."purestats") then
	                pres(i,j,k) = pres(i,j,k) - SUM(regcoeff_lbpb_temp(k,1:nk,b) * psi(i,j,1:nk))
                     else
        	        pres(i,j,k) = pres(i,j,k) - SUM(regcoeff_lbpb_temp(k,1:nk,b) * lbpres(i,j,1:nk))
                     endif
                  end do
               end do
            end do
            variable = 'p_u'
	    call write_3d_field(variable, date, ce, iunit, ni, nj, nk, pres)
         endif

         if (masscv.eq."temp") then
!        Calculate unbalanced ps:
         variable = 'ps'
	 call read_2d_field(variable, date, ce, iunit, ni, nj, nkdum, ps)
	 
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               ps(i,j) = ps(i,j) - SUM(regcoeff_psi_ps(1:nk,b) * psi(i,j,1:nk))
            end do
         end do
         variable = 'ps_u'
         call write_2d_field(variable, date, ce, iunit, ni, nj, nkdum, ps)
         endif
      end do  ! End loop over ensemble members.
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
   
end program gen_be_stage2
