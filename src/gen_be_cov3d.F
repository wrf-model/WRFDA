program gen_be_cov3d
  
  ! -------------------------------------------------------------------------------------
  ! Purpose : Diagnose cross-covariances and explained variance ratios                  !
  ! -------------------------------------------------------------------------------------
  ! Author  : Yann Michel (Meteo France)                                                !
  ! -------------------------------------------------------------------------------------
  
   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh
   use da_gen_be, only : read_3d_field, write_3d_field, update_rain_bin

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable1                  ! Variable name
   character*10        :: variable2                  ! Variable name
   character*10        :: variable2u
 
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
  
   integer             :: ni, nj, nk, mmax           ! Grid dimensions.
   integer             :: i, j, k, k2,k3,member      ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (3D fields).
   
   integer             :: N_holm_bins                ! Number of Holm bins for rh/rhm
   integer             :: holm_reference             ! for rh/rhm/cldfra/cldfram/others
   
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: cldfrc1,cldfrc2            ! Cloud Fractions
   real                :: summ, coeffa, coeffb
   real                :: th_cldfrac
   
   logical             :: first_time                 ! True if first file.

   real, allocatable   :: field1(:,:,:)              ! Field 1.
   real, allocatable   :: field2(:,:,:)              ! Field 2.
   real, allocatable   :: vertcovar(:,:)             ! Covariance between input fields. 
   real, allocatable   :: vertvar1(:,:)              ! Variance of input field 1 
   real, allocatable   :: vertvar2(:,:)              ! Variance of input field 2
   real, allocatable   :: vertvar2u(:,:)             ! Variance of input field 2 (unbalanced)
   real, allocatable   :: regcoeff(:,:)              ! Regression coefficient
   
   real, allocatable   :: rh(:,:,:)                  ! Relative Humidity
   real, allocatable   :: rhm(:,:,:)      	     ! Sym. Relative Humidity
   real, allocatable   :: temp(:,:,:)		     ! Temperature
   real, allocatable   :: tempm(:,:,:)     	     ! Sym. Temperature
   real, allocatable   :: cld_fra(:,:,:)	     ! Cloud fraction
   real, allocatable   :: cld_fram(:,:,:)            ! Sym. Cloud fraction
   
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable :: counter_bin(:,:)
   
   namelist / gen_be_cov3d_nl / start_date, end_date, interval, &
                                ne, variable1, variable2, N_holm_bins, holm_reference, th_cldfrac

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
   variable1 = 'psi'
   variable2 = 'chi'
   variable2u='chi_psu'
   N_holm_bins = 1
   holm_reference = 1
   th_cldfrac = 0.1
   
   open(unit=namelist_unit, file='gen_be_cov3d_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_cov3d_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing covariance for fields ', variable1 , ' and ', variable2
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read fields, and calculate cross-covariances'
!--------------------------------------------------------------------------------------------- 

   date = start_date
   cdate = sdate
   
   first_time = .true.

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3.3)')member
	 write(6,'(3a,i4)')'    Processing data for date ', date,' and member ',member
	 
	 if ( first_time ) then
!           Read or size info
            filename     = 'fullflds/fullflds.'//trim(date)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit) date,ni,nj,nk
            close(iunit)
	    
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( field1(1:ni,1:nj,1:nk) )
            allocate( field2(1:ni,1:nj,1:nk) )

!           Read bin info:
            filename = 'bin.data'
            open (iunit+1, file = filename, form='unformatted')
            read(iunit+1)bin_type
            read(iunit+1)lat_min, lat_max, binwidth_lat
            read(iunit+1)hgt_min, hgt_max, binwidth_hgt
            read(iunit+1)num_bins, num_bins2d
            read(iunit+1)bin(1:ni,1:nj,1:nk)
            read(iunit+1)bin2d(1:ni,1:nj)
            close(iunit+1)
	    
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
	    
	    allocate( vertcovar(1:nk,1:num_bins2d) )  
            allocate( vertvar1(1:nk,1:num_bins2d) )  
            allocate( vertvar2(1:nk,1:num_bins2d) )  
	    allocate( counter_bin(1:nk,1:num_bins2d) )
            counter_bin(:,:) = 0           
            vertcovar(:,:) = 0.0
            vertvar1(:,:)  = 0.0
            vertvar2(:,:)  = 0.0
            first_time = .false.
         end if
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if  

!        Read variable(s)
         call read_3d_field(variable1, date, ce, iunit, ni, nj, nk, field1)
	 call read_3d_field(variable2, date, ce, iunit, ni, nj, nk, field2)
	 
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
	
!       Sample covariance (assume no mean)
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
		   coeffa = 1.0 / real(counter_bin(k,b))
              	   coeffb = real(counter_bin(k,b)-1) * coeffa
                   ! vertical/binned covariances
		   vertcovar(k,b) = coeffb*vertcovar(k,b)+coeffa*field1(i,j,k)*field2(i,j,k)		     
		   vertvar1(k,b) = coeffb*vertvar1(k,b)+coeffa*field1(i,j,k)*field1(i,j,k)
		   vertvar2(k,b) = coeffb*vertvar2(k,b)+coeffa*field2(i,j,k)*field2(i,j,k)                
              end do
            end do
         end do
      end do  ! End loop over ensemble members.
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
   
!----------------------------------------------------------------------------------------------------!
   write(6,'(2a)')' [3] Compute balance and write pseudo-unbalanced fields'
!----------------------------------------------------------------------------------------------------!

   allocate( regcoeff(1:nk,1:num_bins2d) ) 
   regcoeff(:,:) = 0.
     
   do b = 1, num_bins2d
      do k = 1, nk
         if (vertvar1(k,b).gt.0) then
            regcoeff(k,b) = vertcovar(k,b)/vertvar1(k,b)
         end if
      end do
   end do
   
!---------------------------------------------------------------------------------------------------!    
   
   variable2u=trim(variable2)//'_psu'
    
   date = start_date
   cdate = sdate
   
   first_time = .true.

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3.3)')member
	 write(6,'(3a,i4)')'    Processing data for date ', date,' and member ',member
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if  

!        Read variable(s)
         call read_3d_field(variable1, date, ce, iunit, ni, nj, nk, field1)
	 call read_3d_field(variable2, date, ce, iunit, ni, nj, nk, field2)
	 
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
		   field2(i,j,k) = field2(i,j,k) - regcoeff(k,b)*field1(i,j,k)	   
  	       end do
            end do
         end do
	
	! write pseudo-unbal field
        call write_3d_field(variable2u, date, ce, iunit, ni, nj, nk, field2)
	
      end do  ! End loop over ensemble members.
      
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times. 

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Diagnose explained variance ratio'
!--------------------------------------------------------------------------------------------- 

   allocate( vertvar2u(1:nk,1:num_bins2d) )  
   vertvar2u(:,:)  = 0.0
   counter_bin(:,:) = 0
   
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
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if  

!        Read variable(s)
	 call read_3d_field(variable2u, date, ce, iunit, ni, nj, nk, field2)
	 
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
		   coeffa = 1.0 / real(counter_bin(k,b))
              	   coeffb = real(counter_bin(k,b)-1) * coeffa		   		   
		   vertvar2u(k,b) = coeffa*vertvar2u(k,b)+coeffb*field2(i,j,k)*field2(i,j,k)  		  
  	       end do
            end do
         end do	
      end do  ! End loop over ensemble members.
      
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times. 
        
   !--------------------------------------------------------------------------------------------!
   ! write vert covariance in binary format
   filename = 'vert.'//trim(variable1)//'.'//trim(variable2)//'.dat'
   write(unit=6,fmt='(2a)') " Write results in ",trim(filename)
   open (ounit, file = filename, form='unformatted')
   write(ounit)ni,nj,nk,num_bins2d
   write(ounit)vertvar1
   write(ounit)vertvar2
   write(ounit)vertcovar
   write(ounit)regcoeff
   write(ounit)vertvar2u
   close(ounit)
   !--------------------------------------------------------------------------------------------!
   write(unit=6,fmt='(a)') "End of gen_be_cov3d"

end program gen_be_cov3d
