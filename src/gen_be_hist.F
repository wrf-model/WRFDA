program gen_be_hist

  ! -------------------------------------------------------------------------------------
  ! Purpose : Sample histogram of pdf of background error for a Control Variable        !
  ! -------------------------------------------------------------------------------------
  ! Author Yann MICHEL - NCAR/MMM and CNRM/GMAP/RECYF                                   !
  ! -------------------------------------------------------------------------------------
  ! 01/2009 - Creation                                                                  !
  ! 02/2009 - Geographical masks based on rain/norain                                   !
  ! -------------------------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh
   use da_gen_be, only : read_3d_field, update_rain_bin
   
   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: dat_dir     ! Input data directory.
   character(len=filename_len)        :: filename    ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   character           :: ch                         ! holm reference -> character.
   
   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
   integer             :: i, j, k, member            ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: n1, n2                     ! For Holm binning
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (3D fields).
   integer             :: N_holm_bins                ! Number of Holm bins for rh/rhm
   integer             :: Nstdev, N_dim_hist         ! Histogram parameters
   integer             :: intcl                      ! Histogram loop counter
   integer             :: holm_reference             ! May be 1,2,3,4 for rh/rhm/cldfra/cldfram
   
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: cldfrc1,cldfrc2            ! Cloud Fractions
   real                :: pas, counter               ! Some real
   real                :: th_cldfrac

   logical             :: first_time                 ! True if first file.

   real, allocatable   :: field(:,:,:)               ! Field 
   
   integer, allocatable :: bin(:,:,:)                ! Bin assigned to each 3D point.
   integer, allocatable :: bin2d(:,:)                ! Bin assigned to each 2D point.
   integer, allocatable :: bin_pts(:)                ! Number of points in bin (3D fields).
   integer, allocatable :: bin_pts2d(:)              ! Number of points in bin (2D fields).
   integer, allocatable :: hist(:,:,:)               ! Binned error values
   
   integer, allocatable :: counter_bin(:,:)
   
   real, allocatable   :: class_hist(:,:,:)          ! Binned error values

   real, allocatable   :: rh(:,:,:)                  ! Relative Humidity
   real, allocatable   :: rhm(:,:,:)      	     ! Sym. Relative Humidity
   real, allocatable   :: temp(:,:,:)		     ! Temperature
   real, allocatable   :: tempm(:,:,:)     	     ! Sym. Temperature
   real, allocatable   :: cld_fra(:,:,:)	     ! Cloud fraction
   real, allocatable   :: cld_fram(:,:,:)            ! Sym. Cloud fraction
   
   real, allocatable   :: vertvar(:,:)               ! Variance of input field
   real, allocatable   :: skewness(:,:)              ! Skewness
   real, allocatable   :: kurtosis(:,:)              ! Kurtosis

   namelist / gen_be_hist_nl / start_date, end_date, interval, &
                                ne, variable, Nstdev, N_dim_hist, N_holm_bins, holm_reference, th_cldfrac

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
   variable = 'psi'
   dat_dir = '/mmmtmp1/dmbarker'
   Nstdev = 5
   N_dim_hist = 20
   N_holm_bins = 1
   holm_reference = 1
   th_cldfrac = 0.1
   
   open(unit=namelist_unit, file='gen_be_hist_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_hist_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(2a)')' Computing error histogram for field ', variable
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne
   write(6,'(2(a,i8))')' Parameters of the histogram Nstdev = ', Nstdev,&
        ' and N_dim_hist = ',N_dim_hist

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Compute Variance per vertical level and bin2d for hist dimensions'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate
   first_time = .true.

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3.3)')member
         write(6,'(3a,i4)')'    Processing data for date ', date,' and member ',member

!        Read or size info
         filename     = 'fullflds/fullflds.'//trim(date)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit) date,ni,nj,nk
         close(iunit)
  
        if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( field(1:ni,1:nj,1:nk) )
            field(:,:,:)=0.0

!           Read bin info:
            filename = 'bin.data'
            open (iunit, file = filename, form='unformatted')
            read (iunit) bin_type
            read (iunit) lat_min, lat_max, binwidth_lat
            read (iunit) hgt_min, hgt_max, binwidth_hgt
            read (iunit) num_bins, num_bins2d
            read (iunit) bin(1:ni,1:nj,1:nk)
            read (iunit) bin2d(1:ni,1:nj)
            close(iunit)
	    
	    if (N_holm_bins>1 .and. bin_type/=5) then
	   	stop ' Holm binning goes with bin_type = 5'
	    end if	    
	    ! Holm: use special binning
	    if (N_holm_bins>1) then
	    	num_bins2d = N_holm_bins
		! allocations
		allocate(rh(1:ni,1:nj,1:nk))
		allocate(rhm(1:ni,1:nj,1:nk))
		allocate(temp(1:ni,1:nj,1:nk))
		allocate(tempm(1:ni,1:nj,1:nk))
		allocate(cld_fra(1:ni,1:nj,1:nk))
		allocate(cld_fram(1:ni,1:nj,1:nk))
	    end if	       
	    
            allocate( vertvar(1:nk,1:num_bins2d) )
            vertvar(:,:)=0.0            
            allocate( bin_pts2d(1:num_bins2d) )
            bin_pts2d(:) = 0           	    
            allocate( counter_bin(1:nk,1:num_bins2d) )
            counter_bin(:,:) = 0           
	    
            first_time = .false.
         end if
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if  

!        Read variable(s)
         call read_3d_field(variable, date, ce, iunit, ni, nj, nk, field)
	 
!        Holm type binning
	 if (N_holm_bins>1) then
 	    filename = 'fullflds/fullflds.'//date(1:10)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)date, ni,nj,nk
	    read(iunit) rh
            read(iunit) rhm
	    read(iunit) temp
            read(iunit) tempm
	    read(iunit) cld_fra
            read(iunit) cld_fram
            close(iunit)
	    ! RH/CLD_FRA between 0 and 1
	    rh(:,:,:) = min(1.0,max(0.0,rh))
	    rhm(:,:,:) = min(1.0,max(0.0,rhm))
	    cld_fra(:,:,:) = min(1.0,max(0.0,cld_fra))
	    cld_fram(:,:,:) = min(1.0,max(0.0,cld_fram))
	end if
		 
!        Sample variance (assume no mean, only for setup of dimensions
	do k= 1, nk
           do j = 1, nj
               do i = 1, ni               
                  b = bin2d(i,j)
	       
	       	  ! If Hom, decide on rh/cldfra for binning
 	       	  if (N_holm_bins>1) then
		     select case (holm_reference)
		     case default
		     	b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		     case (1)
		        b = 1+nint(rh(i,j,k)*(N_holm_bins-1))
		     case (2)
		        b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		     case (3)
		        b = 1+nint(cld_fra(i,j,k)*(N_holm_bins-1))
		     case (4)
		        b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-1))
		     case (5)
		        cldfrc1 = cld_fra(i,j,k)
			cldfrc2 = 2*cld_fram(i,j,k)-cld_fra(i,j,k)
			if (cldfrc1.ge.th_cldfrac .and. cldfrc2.ge.th_cldfrac) then
			   b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-2))
			else
			   b = N_holm_bins
			end if
		     case (6)
		        cldfrc1 = cld_fra(i,j,k)
			cldfrc2 = 2*cld_fram(i,j,k)-cld_fra(i,j,k)
			if (cldfrc1.ge.th_cldfrac .and. cldfrc2.ge.th_cldfrac) then
			   b = 1+nint(rhm(i,j,k)*(N_holm_bins-2))
			else
			   b = N_holm_bins
			end if			
	       	     end select
	           end if
                   counter_bin(k,b) = counter_bin(k,b)+1
                   vertvar(k,b) = vertvar(k,b)+field(i,j,k)**2.0
            	end do
	    end do
         end do
	 
      end do  ! End loop over ensemble members.  
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!  Write out results
   do k=1,nk
      do b=1,num_bins2d
         if (counter_bin(k,b).gt.0) then
      	     vertvar(k,b) = vertvar(k,b)/counter_bin(k,b)
             write(unit=6,fmt='(2(a,i3),a,i8,a,e15.5)')"    Lev ",k," Bin ",b," Count ",counter_bin(k,b),&
              " stdev ",sqrt(vertvar(k,b))
         end if
      end do
   end do
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [3] Accumulate histogram of values per bin and per vertical level'
!--------------------------------------------------------------------------------------------- 

   write(6,'(a)')' [3.1] Setup of histogram parameters'

   ! Allocate histogram, adapted for Holm binning
   allocate( class_hist(1:nk,1:num_bins2d,1:N_dim_hist) )
   class_hist(:,:,:)  = 0.0
   allocate( hist(1:nk,1:num_bins2d,1:N_dim_hist) )
   hist(:,:,:) = 0
   
   ! Find out center of classes
   do k=1, nk
      do b=1,num_bins2d
         pas = 2*Nstdev*sqrt(vertvar(k,b))/real(N_dim_hist-1)
         do i=1,N_dim_hist
            class_hist(k,b,i)=-Nstdev*sqrt(vertvar(k,b))+pas*real(i-1)
         end do
      end do
   end do

   write(6,'(a)')' [3.2] Accumulate values in histogram'   
  
  date = start_date
  cdate = sdate

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3.3)')member
         write(6,'(3a,i4)')'    Processing data for date ', date,' and member ',member

	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if  
	  
!        Read variable(s)
         call read_3d_field(variable, date, ce, iunit, ni, nj, nk, field)
	 
  !        Holm type binning
	 if (N_holm_bins>1) then
 	    filename = 'fullflds/fullflds.'//date(1:10)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)date, ni,nj,nk
	    read(iunit) rh
            read(iunit) rhm
	    read(iunit) temp
            read(iunit) tempm
	    read(iunit) cld_fra
            read(iunit) cld_fram
            close(iunit)
	    ! RH/CLD_FRA between 0 and 1
	    rh(:,:,:) = min(1.0,max(0.0,rh))
	    rhm(:,:,:) = min(1.0,max(0.0,rhm))
	    cld_fra(:,:,:) = min(1.0,max(0.0,cld_fra))
	    cld_fram(:,:,:) = min(1.0,max(0.0,cld_fram))
	end if
	
         !  Accumulate values in histogram
         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
		  ! Holm: update binning according to rhm/cld_frac value
		  if (N_holm_bins>1) then
		     select case (holm_reference)
		     case default
		     	b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		     case (1)
		        b = 1+nint(rh(i,j,k)*(N_holm_bins-1))
		     case (2)
		        b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		     case (3)
		        b = 1+nint(cld_fra(i,j,k)*(N_holm_bins-1))
		     case (4)
		        b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-1))
		     case (5)
		        cldfrc1 = cld_fra(i,j,k)
			cldfrc2 = 2*cld_fram(i,j,k)-cld_fra(i,j,k)
			if (cldfrc1.ge.th_cldfrac .and. cldfrc2.ge.th_cldfrac) then
			   b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-2))
			else
			   b = N_holm_bins
			end if
		     case (6)
		        cldfrc1 = cld_fra(i,j,k)
			cldfrc2 = 2*cld_fram(i,j,k)-cld_fra(i,j,k)
			if (cldfrc1.ge.th_cldfrac .and. cldfrc2.ge.th_cldfrac) then
			   b = 1+nint(rhm(i,j,k)*(N_holm_bins-2))
			else
			   b = N_holm_bins
			end if			
	       	     end select
		  end if
                  ! finds out in which hist bin the value falls
                  intcl=nint(0.5*(N_dim_hist+1.0)*(1+field(i,j,k)/(Nstdev*sqrt(vertvar(k,b)))))
                  if (intcl .ge. 1 .and. intcl .le. N_dim_hist) then
                     hist(k,b,intcl)=hist(k,b,intcl)+1
                  end if
               end do
            end do
         end do
      end do  ! End loop over ensemble members.      
      !     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [3] Write out computed histogram'
!--------------------------------------------------------------------------------------------- 

   write(ch,'(i1)')holm_reference   
   filename = 'hist.'//trim(variable)//'.'//ch//'.dat'
   open (ounit, file = filename, form='unformatted')
   write(ounit) nk, num_bins2d, N_dim_hist, holm_reference
   ! Split to allow easy NCL reading
   do k=1,nk
      do b=1,num_bins2d
         write(ounit)vertvar(k,b)
         write(ounit)class_hist(k,b,:)
         write(ounit)hist(k,b,:)
      end do
   end do
   close(ounit)
   write(6,'(a)')' End of program gen_be_hist'

end program gen_be_hist
