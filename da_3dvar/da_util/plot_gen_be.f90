
program plot_reg_coeff
!--------------------------------------------------------------------------!
! Purpose: 1) Compute ratio of chi_b, temp_b and ps_b explained by psi to  !
!             its full fields, and plot the global and local figures of    !
!             the ratio.                                                   !
!          2) Plot the global eigenvalues, the first 5 global eigenvectors !
!             and the first 5 local eigenvalues for psi, chi_u, t_u, and   !
!             rh.                                                          !
!          3) Plot the scale-lengths for psi, chi_u, t_u, rh, and ps_u.    !
!                                                                          !  
! Note: this program is working for gen_be.NMC.dat and gen_be.ENS.dat for  !
!       for cv_options=5.                                                  !
!                                                                          !
! History:                                                                 !
!   06/09/2005   Original developer.                     Yong-Run Guo      ! 
!   06/20/2005   Modified for plotting the BES file                        !
!                including bin information and ps_u                        !
!                covariance.                             Yong-Run Guo      !
!                                                                          !
!--------------------------------------------------------------------------!

   implicit none

   type eigen_type
      real(kind=8), pointer :: g_val(:)
      real(kind=8), pointer :: g_vec(:,:)
      real(kind=8), pointer :: l_val(:,:)
      real(kind=8), pointer :: l_vec(:,:,:)
      real(kind=8), pointer :: sl(:)
      real(kind=8), pointer :: power(:,:)
   end type eigen_type

   type (eigen_type) :: cv

   integer, parameter :: iunit = 10, input_unit = 11

!
   integer :: ni, nj, nk, nkdum, num_bins, num_bins2d
   integer :: bin_type           ! Type of bin to average over. !!!DALE ADD.
   real(kind=8)    :: lat_min, lat_max   ! Used if bin_type = 2 (degrees).
   real(kind=8)    :: binwidth_lat       ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real(kind=8)    :: hgt_min, hgt_max   ! Used if bin_type = 2 (m).
   real(kind=8)    :: binwidth_hgt       ! Used if bin_type = 2 (m). !!!DALE ADD..
   integer :: num_bins_lat               ! Used if bin_type = 2.
   integer :: num_bins_hgt               ! Used if bin_type = 2.

   logical                   :: ldum1, ldum2               ! Dummy logicals.

   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point.
   real(kind=8), allocatable   :: regcoeff_chi(:)    ! psi/chi regression cooefficient.
   real(kind=8), allocatable   :: regcoeff_ps (:,:)  ! psi/ps regression cooefficient.
   real(kind=8), allocatable   :: regcoeff_t  (:,:,:)! psi/T regression cooefficient.
   real(kind=8), allocatable :: reg_chi(:,:)
   real(kind=8), allocatable :: reg_ps (:,:) 
   real(kind=8), allocatable :: reg_t(:,:,:) 

! Namelist variables:

   character(len= 10)        :: start_date, end_date ! Starting and ending dates.
   character(len=120)        :: main_title1
   character(len=120)        :: main_title2
   character(len=  3)        :: be_method        ! Be method ('NMC', or 'ENS')
   character(len=  8)        :: uh_method        ! Uh_method (power, scale)
   character(len= 80)        :: gen_be_dir
   character(len=  8)        :: code_version
   integer                   :: stride           ! in s-N direction
   integer                   :: interval         ! file interval
   integer                   :: ne               ! number of ENS members
   real                      :: Resolution_km
   logical                   :: domain_averaged

   namelist /plot_title/start_date, end_date, interval, &
                        main_title1, main_title2,       &
                        Resolution_km, stride, ne,      &
                        be_method, Uh_method, gen_be_dir, &
                        code_version, domain_averaged

! working variables
   integer :: i, j, k, n, m, b, member, n_times, ier
   integer :: sdate, cdate, edate        ! Starting, current ending dates.
   character(len= 10)        :: variable             ! Variable name
   character(len= 80)        :: filename  
   character(len= 10)        :: date, new_date       ! Current date (ccyymmddhh).
   character(len=  3)        :: ce                   ! Member index -> characte
   character(len=  5)        :: cvv                  ! control variable name

! Variable fields
   real(kind=8), allocatable   :: psi (:,:,:)                 ! psi.
   real(kind=8), allocatable   :: chi (:,:,:)                 ! chi
   real(kind=8), allocatable   :: chi_u (:,:,:)               ! chi_u

   real(kind=8), allocatable   :: temp(:,:,:)                 ! Temperature.
   real(kind=8), allocatable   :: t_u (:,:,:)                 ! Temperature.

   real(kind=8), allocatable   :: ps  (:,:)                   ! Surface pressure.
   real(kind=8), allocatable   :: ps_u(:,:)                   ! Surface pressure.

! Arrays for Ratio computation:
   real(kind=4), allocatable   :: chi_loc (:,:), chi_global(:) ! chi_b  covariance 
   real(kind=4), allocatable   :: temp_loc(:,:), temp_global(:)! Temp_b covariance
   real(kind=4), allocatable   :: ps_loc  (:)                  ! ps_b  .covariance
   real(kind=4), allocatable   :: chi_var (:,:), chi_gvar(:)   ! chi    variance 
   real(kind=4), allocatable   :: temp_var(:,:), temp_gvar(:)  ! Temp   variance
   real(kind=4), allocatable   :: ps_var  (:)                  ! ps     variance
   real(kind=8)  :: balance, unbalan, avg, avg2, avg3

   integer                   :: max_wavenumber
   real(kind=8), allocatable :: total_power(:,:)           ! Total Power spectrum.
!
! For contour plot:

      integer  :: IOFFP, ndot, jj, interval_j, len_title
      real     :: flo, hi, cint, spval0

      character (len=120) :: Title
      common /conre1/ IOFFP, SPVAL0
      IOFFP = 1
      SPVAL0 = -99999.9
!
! 1.0 Initialize GKS
! ------------------

   call opngks
   call setup_color_table

! 2.0 Read namelist
! -----------------

   start_date    = '2005061000'
   end_date      = '2005061000'
   interval      = 12
   main_title1   = ' '
   main_title2   = ' '
   stride        =   1
   Resolution_km = 100.0
   be_method     = 'NMC'
   NE            = 1
   Uh_method     = 'scale'
   gen_be_dir    = '.'
   code_version  = 'wrfvar'

   open(unit=5, file='namelist.title', status='old')
   read(unit=5, nml=plot_title,iostat=ier)
   if (ier/=0) then
      print '(a,i2,a)','iostat=',ier,'  Error in Namelist file...'
      stop
   endif
   print  plot_title
   close (5)
   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date  (1:10), fmt='(i10)')edate

! 3.0 Open the gen_be file and read in the information
! ----------------------------------------------------

   filename = trim(gen_be_dir)//'/gen_be.'//trim(be_method)//'.dat'
   print '("*** Unit=",i3,3X,"filename=",a60)',input_unit, filename
   open (input_unit, file = filename, form='unformatted')

! 3.1 Read the domain size
        
   read(input_unit) ni, nj, nk

! 3.2. Setup the bin information

   if (code_version == 'wrf3dvar') then

! 3.2.1 the bin information from wrf3dvar/gen_be 

     read(input_unit)bin_type, num_bins_hgt, binwidth_hgt, binwidth_lat
     read(input_unit)num_bins, num_bins2d

     call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d )   

   else if (code_version == 'wrfvar  ') then

! 3.2.2 Read the bin information from wrfvar/gen_be

     allocate( bin(1:ni,1:nj,1:nk) )
     allocate( bin2d(1:ni,1:nj) )

     read(input_unit)bin_type
     read(input_unit)lat_min, lat_max, binwidth_lat
     read(input_unit)hgt_min, hgt_max, binwidth_hgt
     read(input_unit)num_bins, num_bins2d
     read(input_unit)bin(1:ni,1:nj,1:nk)
     read(input_unit)bin2d(1:ni,1:nj)

    else
   
   print '(a,a,a)', "Not support this version of ",code_version," code."
   stop 22222

    endif

! ----------------------------------------------------------------------------------
! PART I. Regression coefficient
! ---------------------------------------------------------------------------------
   
! 4.0 Read in the regression coefficients
! ---------------------------------------

   allocate( regcoeff_chi(1:num_bins) )
   allocate( regcoeff_ps (1:nk,1:num_bins2d) )
   allocate( regcoeff_t  (1:nk,1:nk,1:num_bins2d) )

   read(input_unit) regcoeff_chi
   read(input_unit) regcoeff_ps
   read(input_unit) regcoeff_t

   print '(a,5I8)', &
      "1) Finished Ceofficients reading:: ni, nj, nk, num_bins, num_bins2d:", &
                           ni, nj, nk, num_bins, num_bins2d

! .. Skip over processing the regression coefficient ......................
!                  goto 1000
! ..........................................................................

! 4.1 Re-assign the regression coefficients

      allocate(reg_chi(nj, nk))
      allocate(reg_ps (nj, nk))
      allocate(reg_t  (nj, ni, nk))

    do k=1,nk
    do j =1, nj
      b = bin(1,j,k)
      reg_chi(j,k) = regcoeff_chi(b)
    end do
    end do

!
    do j=1,nj
      b = bin2d(1,j)
      do k=1,nk
        reg_ps(j,k) = regcoeff_ps(k,b)
      end do
    end do
!
    do j=1,nj
      b = bin2d(1,j)
      do i=1,nk
      do k=1,nk
         reg_t(j,i,k) = regcoeff_t(i,k,b)
      end do
      end do
    end do

! 4.1.1 domain averaged regression coefficients

    if (domain_averaged) then

      print '(/5x, a/)',   &
     '*** Using the averaged regression coefficients for balanced part ***'

! .. ps and chi:
      do k=1,nk
        avg= 0.0
        avg2=0.0
        do j=1,nj
          avg = avg  + reg_ps  (j,k)/float(nj) 
          avg2= avg2 + reg_chi (j,k)/float(nj) 
        enddo
!
        do j=1,nj
          reg_ps (j,k)=avg
          reg_chi(j,k)=avg2
        enddo
      enddo

! .. temperature:
      do i=1,nk
      do k=1,nk
        avg3= 0.0

        do j=1,nj
          avg3= avg3 + reg_t (j,k,i)/float(nj)
        enddo

        do j=1,nj
          reg_t(j,k,i)=avg3
        enddo

      enddo
      enddo

    endif
    
    print '(a)', "2) Re-assign the regression coefficients."

! 4.2 Compute the balanced part of chi, ps and t

   print '("ni,nj,nk:",3I8)', ni,nj,nk

      allocate(psi (ni, nj, nk))
      allocate(chi (ni, nj, nk))
      allocate(chi_u (ni, nj, nk))
      allocate(temp(ni, nj, nk))
      allocate(t_u (ni, nj, nk))
      allocate(ps  (ni, nj))
      allocate(ps_u(ni, nj))

      allocate( chi_loc(nj, nk)); allocate( chi_global(nk))
      allocate(temp_loc(nj, nk)); allocate(temp_global(nk))
      allocate(  ps_loc(nj))
      allocate( chi_var(nj, nk)); allocate( chi_gvar(nk))
      allocate(temp_var(nj, nk)); allocate(temp_gvar(nk))
      allocate(  ps_var(nj))

      chi_loc     = 0.0
      temp_loc    = 0.0
      ps_loc      = 0.0

   date  = start_date
   cdate = sdate
   n_times = 0

   do while ( cdate <= edate )
      n_times = n_times + 1

      write(6,'(i4.4,a,a)') n_times,'    Calculating unbalanced fields for date ', date

      do member = 1, ne

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

! 4.2.1 Read psi (mean-removed):
         variable = 'psi'
         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
!         print '("Read file:",a60)', filename
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

! 4.2.2 Read chi (mean-removed):
         variable = 'chi'
         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
!         print '("Read file:",a60)', filename
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi
         close(iunit)

! 4.2.2.1 Read chi_u (mean-removed):
!         variable = 'chi_u'
!         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
!         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
!         print '("Read file:",a60)', filename
!         open (iunit, file = filename, form='unformatted')
!         read(iunit)ni, nj, nk
!         read(iunit)chi_u
!         close(iunit)

      do k=1,nk
        do j =1, nj
          do i = 1,ni
            balance = reg_chi(j,k)*psi(i,j,k)
            chi_loc(j,k) = chi_loc(j,k) + balance * chi(i,j,k)
            chi_var(j,k) = chi_var(j,k) + chi(i,j,k) * chi(i,j,k)

!            unbalan = chi(i,j,k) - balance
!            if (abs(chi_u(i,j,k)-unbalan) > 1.E-25) &
!               print '("n,i,j,k,chi_u,unbalan:",4i5,2e20.12)', n_times,i,j,k, chi_u(i,j,k), unbalan
          enddo
        end do
      end do
    
! 4.2.3 Read ps (mean-removed):
         variable = 'ps'
         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce//'.01'
!         print '("Read file:",a60)', filename
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ldum1, ldum2 ! Dummy logicals.
         read(iunit)ps
         close(iunit)

! 4.2.3.1 Read ps_u (mean-removed):
!         variable = 'ps_u'
!         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
!         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce//'.01'
!         print '("Read file:",a60)', filename
!         open (iunit, file = filename, form='unformatted')
!         read(iunit)ni, nj, nkdum
!         read(iunit)ldum1, ldum2 ! Dummy logicals.
!         read(iunit)ps_u
!         close(iunit)

        do j =1, nj
          do i = 1,ni
            balance = SUM(reg_ps(j,1:nk) * psi(i,j,1:nk))
            ps_loc(j) = ps_loc(j) + balance * ps(i,j)
            ps_var(j) = ps_var(j) + ps(i,j) * ps(i,j)

!            unbalan = ps(i,j) - balance
!            if (abs(ps_u(i,j)-unbalan) > 1.E-12) &
!               print '("n,i,j,ps_u,unbalan:",3i5,2e20.12)', n_times,i,j,k, ps_u(i,j), unbalan
          enddo
        end do
    
! 4.2.4 Read t (mean-removed):
         variable = 't'
         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
!         print '("Read file:",a60)', filename
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp
         close(iunit)

! 4.2.4.1 Read t_u (mean-removed):
!         variable = 't_u'
!         filename = trim(gen_be_dir)//'/'//trim(variable)//'/'//date(1:10)
!         filename = trim(filename)//'.'//trim(variable)//'.'//trim(be_method)//'.e'//ce
!         print '("Read file:",a60)', filename
!         open (iunit, file = filename, form='unformatted')
!         read(iunit)ni, nj, nk
!         read(iunit)t_u
!         close(iunit)

      do k=1,nk
        do j =1, nj
          do i = 1,ni
            balance =  SUM(reg_t(j,k,1:nk) * psi(i,j,1:nk))
            temp_loc(j,k) = temp_loc(j,k) + balance * temp(i,j,k)
            temp_var(j,k) = temp_var(j,k) + temp(i,j,k) * temp(i,j,k)

!            unbalan = temp(i,j,k) - balance
!            if (abs(t_u(i,j,k)-unbalan) > 1.E-12) &
!               print '("n,i,j,k,temp_u,unbalan:",4i5,2e20.12)', n_times,i,j,k, t_u(i,j,k), unbalan
          enddo
        end do
      end do
      
      if (member == ne) print '(10x,i5,a)', member, ' members has been processed.'

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do     ! End loop over times

! 5.0 print the ratio of balance / full_field::

   print '(/10x,a,3I8/)', 'Balanced part:', ni, nj, n_times

! 5.1 Chi and temperature

      chi_global  = 0.0
      temp_global = 0.0
      do k=1,nk
        do j =1, nj
          chi_global(k) = chi_global(k) + chi_loc(j,k)
          chi_gvar  (k) = chi_gvar  (k) + chi_var(j,k)
          chi_loc(j,k)  = chi_loc(j,k) / chi_var(j,k)
     
          temp_global(k) = temp_global(k) + temp_loc(j,k)
          temp_gvar  (k) = temp_gvar  (k) + temp_var(j,k)
          temp_loc(j,k) = temp_loc(j,k) / temp_var(j,k)

        end do
        chi_global (k) = chi_global (k) / chi_gvar  (k)
        temp_global(k) = temp_global(k) / temp_gvar (k)
        print '(i3,a,f12.5,a,f12.5)',  &
          k, "  Ratio of chi, temp explained by psi: <chi_b*chi>/<chi*chi>=", &
             chi_global(k), "   and <t_b*t>/<t*t>=", temp_global(k)
      end do

! 5.2 Surface pressure:
      
      print '(/a)', "Surface pressure explained by psi: <ps_b*ps>/<ps*ps>"
      do j = 1,nj
        ps_loc(j) = ps_loc(j) / ps_var(j)
        print '(10x,i6,f12.5)', j,ps_loc(j)
      enddo
  
! 6.0 Plot the <Xb*X> / <X*X>

! 6.1 Surface pressure and chi, Temp globally explained by psi

      call Balance_ratio_plot (ps_loc, chi_global, temp_global, nj, nk, main_title1)

! 6.2 chi and Temp expalined by psi locally

      NDOT = -682        ! dashed-line pattern 
      flo = 0.0           
      hi  = 0.0
      call setusv('LW',2000)

! 6.2.1 Balanced chi

      Print '(a)', 'PLOT the local <chi_b*chi>/<chi*chi>...'

        call set(0.20,1.0,0.2,0.9, 1.0,float(nj),1.0,float(nk),1)  

! Contour: 
      cint= 0.025 
        CALL CONREC(chi_loc,nj,nk,nk,flo,hi, cint,-1, 0, NDOT) 
        CALL PERIML( nj/10, 5, nk/5, 5) 
      
        write(Title,'("CHI_B LOCALLY EXPLAINED BY PSI")')  
        print '(a)', Title 
        call set(0.1,0.97,0.1,0.97, 0.0,100.,0.0,100.,1)  
        CALL PWRITX(50.0, 95.0,Title,23,20,0,0) 
        CALL PWRITX(40.0, 2.0,"GRID IN Y-DIRECTION",19,20,0,0) 
        CALL PWRITX(1.5,30.0,"<CHI_B*CHI> / <CHI*CHI>",23,20,90,-1) 
        CALL FRAME 

! 6.2.1 Balanced temperature

      Print '(a)', 'PLOT the local <temp_b*temp>/<temp*temp>...'

        call set(0.20,1.0,0.2,0.9, 1.0,float(nj),1.0,float(nk),1)  

! Contour: 
      cint= 0.050 
        CALL CONREC(temp_loc,nj,nk,nk,flo,hi, cint,-1, 0, NDOT) 
        CALL PERIML( nj/10, 5, nk/5, 5) 
      
        write(Title,'("TEMP_B LOCALLY EXPLAINED BY PSI")')  
        print '(a)', Title 
        call set(0.1,0.97,0.1,0.97, 0.0,100.,0.0,100.,1)  
        CALL PWRITX(50.0, 95.0,Title,24,20,0,0) 
        CALL PWRITX(40.0, 2.0,"GRID IN Y-DIRECTION",19,20,0,0) 
        CALL PWRITX(1.5,30.0,"<TEMP_B*TEMP> / <TEMP*TEMP>",26,20,90,-1) 
        CALL FRAME 

! --------------------------------------------------------------------------
! PART II. Eigenvector and eigenvalue
! --------------------------------------------------------------------------
1000 continue

   call allocate_gen_be(cv, ni, nj, nk)

   do n = 1,5
 
     read(input_unit) variable   
     read(input_unit) nk, num_bins2d 
     print*,'n=',n,' Eigenvector/values for variable ',variable,nk,num_bins2d
     cvv = trim(variable)
!
     if ( n <=4) then

       read(input_unit)  cv%g_vec   
       read(input_unit)  cv%g_val    
       read(input_unit)  cv%l_vec
       read(input_unit)  cv%l_val
 
! .. Skip over plotting eigenvectors ...................................
!           goto 2000
! ......................................................................          

       if (cvv == 'psi')    CVV = 'PSI'
       if (cvv == 'chi_u')  CVV = 'CHI_U'
       if (cvv == 't_u')    CVV = 'T_U'
       if (cvv == 'rh')     CVV = 'RH'

! .. Global eigenvectors:
       call eigen_plot(cvv, cv, main_title1)

! .. local eigenvalues:
       call eigen_plot2(cvv, cv, main_title1, 99) 

     else

       read(input_unit)  cv%g_vec(1,1)   
       read(input_unit)  cv%g_val(1)    
       read(input_unit)  cv%l_vec(1,1,1:nj)
       read(input_unit)  cv%l_val(1,1:nj)

       if (cvv == 'ps_u')    CVV = 'PS_U'
       if (cvv == 'ps')      CVV = 'PS'
 
       do j = 1,nj
         ps_var(j) = cv%l_val(1,j)
       enddo

       call Covariance_plot (cvv, ps_var, nj, main_title1)

      endif

 2000 continue

   enddo


! -------------------------------------------------------------------------

   do n = 1,5
     read(input_unit) variable
     print*,'n=',n,' scale length for variable ',variable,nk,num_bins2d
     if (n==5) then
       cv%sl = 0.0
       read(input_unit) cv%sl(1:1)
     else
       read(input_unit) cv%sl
     endif

     cvv = trim(variable)

     if (cvv == 'psi')    CVV = 'PSI'
     if (cvv == 'chi_u')  CVV = 'CHI_U'
     if (cvv == 't_u')    CVV = 'T_U'
     if (cvv == 'rh')     CVV = 'RH'

     call scale_plot(cvv, cv, main_title1, resolution_km)


   enddo

! -------------------------------------------------------------------------

   call clsgks

   stop

contains

  subroutine setup_color_table

   implicit none

   call gscr(1,  0, 1.00, 1.00, 1.00) ! White
   call gscr(1,  1, 0.00, 0.00, 0.00) ! BLACK
   call gscr(1,  2, 1.00, 0.00, 0.00) ! Red
   call gscr(1,  3, 0.00, 1.00, 0.00) ! Green
   call gscr(1,  4, 0.00, 0.00, 1.00) ! Blue
   call gscr(1,  5, 0.60, 0.00, 0.80) ! Dark violet
   call gscr(1,  6, 0.00, 1.00, 1.00) ! Cyran
   call gscr(1,  7, 1.00, 0.00, 1.00) ! Magenta
   call gscr(1,  8, 0.90, 0.25, 0.00) ! FreshRed
   call gscr(1,  9, 0.40, 0.30, 0.20) ! Tan
   call gscr(1, 10, 1.00, 1.00, 0.00) ! Yellow
   call gscr(1, 11, 0.60, 0.60, 0.60) ! Gray

  end subroutine setup_color_table

  subroutine allocate_gen_be(be, ix, jy, kz)

      implicit none

      type (eigen_type), intent(inout) :: be
      integer,           intent(in)    :: ix, jy, kz
      integer                          :: max_waveno

      max_waveno = ix/2 - 1

      allocate(be%g_val(kz))
      allocate(be%g_vec(kz, kz))
      allocate(be%l_vec(kz, kz,jy))
      allocate(be%l_val(kz,jy))
      allocate(be%sl(kz))
      allocate(be%power(max_waveno,kz))

      be%g_val = 0.0
      be%g_vec = 0.0
      be%l_val = 0.0
      be%l_vec = 0.0
      be%sl    = 0.0

   end subroutine allocate_gen_be

   subroutine da_advance_cymdh( start_date, dh, end_date )

     implicit none

     character (len=10), intent(in)  :: start_date ! In date (ccyymmddhh).
     integer, intent(in)             :: dh         ! Period to advance (-ve for past).
     character (len=10), intent(out) :: end_date   ! Out date (ccyymmddhh).

     integer :: ccyy, mm, dd, hh

     read(start_date(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh

     hh = hh + dh

     do while (hh < 0) 
        hh = hh + 24
        call da_change_date ( ccyy, mm, dd, -1 )
     end do

     do while (hh > 23) 
        hh = hh - 24
        call da_change_date ( ccyy, mm, dd, 1 )
     end do

     write(end_date(1:10), fmt='(i4, 3i2.2)')  ccyy, mm, dd, hh

   end subroutine da_advance_cymdh

   subroutine da_change_date( ccyy, mm, dd, delta )

     implicit none

     integer, intent(inout) :: ccyy, mm, dd
     integer, intent(in)    :: delta

     integer, dimension(12) :: mmday

     mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

     mmday(2) = 28

     if (mod(ccyy,4) == 0) then
        mmday(2) = 29

        if ( mod(ccyy,100) == 0) then
           mmday(2) = 28
        endif

        if(mod(ccyy,400) == 0) then
           mmday(2) = 29
        end if
     endif

     dd = dd + delta

     if(dd == 0) then
        mm = mm - 1

        if(mm == 0) then
           mm = 12
           ccyy = ccyy - 1
        endif

        dd = mmday(mm)
     elseif ( dd .gt. mmday(mm) ) then
        dd = 1
        mm = mm + 1
        if(mm > 12 ) then
           mm = 1
           ccyy = ccyy + 1
        end if
     end if
   end subroutine da_change_date

   subroutine da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, &
                           bin, bin2d, &
                           lat_min, lat_max, binwidth_lat, &
                           hgt_min, hgt_max, binwidth_hgt, latitude, height )
!----------------------------------------------------------------------------------------
!
! Purpose: To create the bins for calculation of statistics.
!
! Input  : ni, nj, nk   ----- Dimensions
!          bin_type     ----- 0: No binning; 
!                             1: bins for X-direction mean; 
!                             2: bins for each of binwidth_lat/binwidth_hgt.  
!                             3: bins for each of binwidth_lat/nk.  
!                             4: num_hor_bins horizontal bins /nk.  
!                             5: Average over all horizontal points (nk bins for 3D fields).
!                             6: Average over all points (only 1 bin).
!          Optional for bin_type = 2:
!          lat_min, lat_max-- Minimum/maximum latitude ranges for bin_type = 2
!          binwidth_lat ----- interval between bins for latitude in degree for bin_type = 2
!          binwidth_hgt ----- interval between bins for height in meter for bin_type = 2
!          num_bins_hgt ----- the number of height bins for bin_type = 2
!          latitude     ----- 3d field latitude in degree for bin_type = 2
!          height       ----- 3d field height in meter for bin_type = 2
!
! Output:  num_bins,num_bins2d ---- the number of bins for 3d and 2d fields
!          bin     ,     bin2d ---- Assigned bin to a gridpoints for 3d and 2d fields
!
!-----------------------------------------------------------------------------------------       
   implicit none

   integer, intent(in)      :: ni, nj, nk                 ! Dimensions read in.
   integer, intent(in)      :: bin_type                   ! Type of bin to average over.
   integer, intent(out)     :: num_bins                   ! Number of bins.
   integer, intent(out)     :: num_bins2d                 ! Number of bins for 2D fields.
   integer, intent(out)     :: bin(1:ni,1:nj,1:nk)        ! Bin assigned to each 3D point.
   integer, intent(out)     :: bin2d(1:ni,1:nj)           ! Bin assigned to each 2D point.

   real(kind=8), intent(in),optional:: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real(kind=8), intent(in),optional:: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real(kind=8), intent(in),optional:: hgt_min, hgt_max           ! Used if bin_type = 2 (degrees).
   real(kind=8), intent(in),optional:: binwidth_hgt               ! Used if bin_type = 2 (m).
   real(kind=8), intent(in),optional:: latitude(1:ni,1:nj)        ! Latitude (degrees).
   real(kind=8), intent(in),optional:: height(1:ni,1:nj,1:nk)     ! Height (m).

   integer                  :: b, i, j, k                 ! Loop counters.
   integer                  :: count                      ! Counter
   integer                  :: num_bins_lat               ! Used if bin_type = 2.
   integer                  :: num_bins_hgt               ! Used if bin_type = 2.
   integer                  :: bin_lat                    ! Latitude bin.
   integer                  :: bin_hgt                    ! Height bin.
   integer                  :: num_bins_i, num_bins_j     ! Used if bin_type = 4.
   integer                  :: nii, njj                   ! Used if bin_type = 4.
   integer                  :: bin_i(1:ni), bin_j(1:nj)   ! Used if bin_type = 4.
   real(kind=8), allocatable        :: binstart_lat(:)            ! Used if bin_type = 2 (degrees).
   real(kind=8), allocatable        :: binstart_hgt(:)            ! Used if bin_type = 2 (degrees).

   if ( bin_type == 0 ) then         ! No averaging in space

      num_bins = nk * nj * ni
      num_bins2d = nj * ni    ! Equals number of horizontal points.

      count = 1
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               bin(i,j,k) = count
               count = count + 1
            end do
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if ( bin_type == 1 ) then    ! Average over x-direction.

      num_bins = nj * nk
      num_bins2d = nj

      count = 1
      do k = 1, nk
         do j = 1, nj
            bin(1:ni,j,k) = count
            count = count + 1
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if ( bin_type == 2 ) then    ! Global latitude/height bins:

!     Setup latitude bins:
      write(6,'(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(6,'(a,f12.5)')'    Maximum latitude = ', lat_max
      write(6,'(a,f12.5)')'    Latitude bin width = ', binwidth_lat
      num_bins_lat = nint( ( lat_max - lat_min ) / binwidth_lat)
      write(6,'(a,i8)')   '    Number of latitude bins = ', num_bins_lat
   
      allocate( binstart_lat(1:num_bins_lat) )
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = lat_min + real(b-1) * binwidth_lat
!         write(6,'(i4,f15.5)')b, binstart_lat(b)
      end do

!     Setup height bins:
      write(6,'(/a,f12.5)')'    Minimum height = ', hgt_min
      write(6,'(a,f12.5)')'    Maximum height = ', hgt_max
      write(6,'(a,f12.5)')'    Height bin width = ', binwidth_hgt
      num_bins_hgt = nint( ( hgt_max - hgt_min ) / binwidth_hgt)
      write(6,'(a,i8)')'    Number of height bins = ', num_bins_hgt

      allocate( binstart_hgt(1:num_bins_hgt) )
      do b = 1, num_bins_hgt
         binstart_hgt(b) = hgt_min + real(b-1) * binwidth_hgt
!         write(6,'(i4,f15.5)')b, binstart_hgt(b)
      end do

      num_bins = num_bins_lat * num_bins_hgt
      num_bins2d = num_bins_lat

!     Select height bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk
               if ( height(i,j,k) < binstart_hgt(1) ) then 
                  bin_hgt = 1 ! In first bin.
               else if ( height(i,j,k) >= binstart_hgt(num_bins_hgt) ) then
                  bin_hgt = num_bins_hgt ! In final bin.
               else 
                  do b = 1, num_bins_hgt-1
                     if ( height(i,j,k) >= binstart_hgt(b) .and. &
                          height(i,j,k) <  binstart_hgt(b+1) ) then
                        bin_hgt = b
                        exit
                     end if
                  end do
               end if

!              Select latitude bin that point falls in:
               if ( k == 1 ) then
                  do b = 1, num_bins_lat-1
                     if ( latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1) ) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if ( latitude(i,j) >= binstart_lat(num_bins_lat) ) then ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * ( bin_hgt - 1 )
            end do
         end do
      end do

      deallocate( binstart_lat )
      deallocate( binstart_hgt )

   else if ( bin_type == 3 ) then    ! Latitude/nk bins:

!     Setup latitude bins:
      write(6,'(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(6,'(a,f12.5)')'    Maximum latitude = ', lat_max
      write(6,'(a,f12.5)')'    Latitude bin width = ', binwidth_lat
      num_bins_lat = nint( ( lat_max - lat_min ) / binwidth_lat)
      write(6,'(a,i8)')   '    Number of latitude bins = ', num_bins_lat
   
      allocate( binstart_lat(1:num_bins_lat) )
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = lat_min + real(b-1) * binwidth_lat
!         write(6,'(i4,f15.5)')b, binstart_lat(b)
      end do

      num_bins = num_bins_lat * nk
      num_bins2d = num_bins_lat

!     Select bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk

!              Select latitude bin that point falls in:
               if ( k == 1 ) then
                  do b = 1, num_bins_lat-1
                     if ( latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1) ) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if ( latitude(i,j) >= binstart_lat(num_bins_lat) ) then ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * ( k - 1 )
            end do
         end do
      end do

      deallocate( binstart_lat )

   else if ( bin_type == 4 ) then    ! binwidth_lat/nk bins:

!     Setup horizontal bins:
      write(6,'(/a,f12.5)')'   Number of grid-cells to average over = ', binwidth_lat
!     Use binwidth_lat, but actually an integer number of points.
 
      num_bins_j = int( real(nj) / real(binwidth_lat) )
      njj = int(binwidth_lat) * num_bins_j
      do j = 1, njj
         bin_j(j) = 1 + int( real(j-1) / binwidth_lat)
      end do
      if ( nj > njj ) bin_j(njj+1:nj) = bin_j(njj)

      num_bins_i = int( real(ni) / real(binwidth_lat) )
      nii = int(binwidth_lat) * num_bins_i
      do i = 1, nii
         bin_i(i) = 1 + int( real(i-1) / binwidth_lat )
      end do
      if ( ni > nii ) bin_i(nii+1:ni) = bin_i(nii)

      num_bins2d = num_bins_i * num_bins_j
      num_bins = num_bins2d * nk

      do j = 1, nj
         do i = 1, ni
            bin2d(i,j) = bin_i(i) + ( bin_j(j) - 1 ) * num_bins_i
            do k = 1, nk
               bin(i,j,k) = bin2d(i,j) + (k - 1) * num_bins2d
            end do
         end do
      end do

   else if ( bin_type == 5 ) then    ! Average over all horizontal points.

      num_bins = nk
      num_bins2d = 1

      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1

   else if ( bin_type == 6 ) then    ! Average over all points.

      num_bins = 1
      num_bins2d = 1
      bin(:,:,:) = 1
      bin2d(:,:) = 1

   end if

   end subroutine da_create_bins

   subroutine eigen_plot(vn, ev, main_title)

      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                            thick_dash = 21845, & ! PATTERN = 0101010101010101
                            thin_dash =  3855, &  ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   integer, parameter :: black =  0, &
                         white =  1, &
                           red =  2, &
                         green =  3, &
                          blue =  4, &
                        violet =  5, &
                         cyran =  6, &
                       magenta =  7, &
                      freshred =  8, &
                           tan =  9, &
                        yellow = 10, &
                          gray = 11

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y
   
      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, xmin, xmax, fct

      print '("PLOT:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      ix = size(ev%l_vec, dim=3)   ! dimension of bin
      jy = size(ev%l_vec, dim=1)   ! k-level
      kz = size(ev%l_vec, dim=2)   ! mode index

      mmy       = 6

      mny       = 2

      yb        = 1.0
      ye        = real(kz)
   
      y_title   = 'VERTICAL LEVEL'

      do k=1, 501
         y(k) = real(k)
      end do

!      if (ev%ok_ev) then

!------------- plot eigen value ---------------------
   
      mmx       = 1
      mnx       = 1
   
      do k=1,kz
         x(k)   = ev%g_val(k)
      end do
   
      x_title = vn // ' E VAL'
   
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
   
      call norm_min_max(xmin, xmax, magnitude)
   
      fct = 10.0**magnitude
    
      x(1:kz)  = fct * x(1:kz)
   
!mslee      xb = 1.0e-15
      xb = 1.0e-5
      xe = 100.0
   
      mn = 0
   
      call line_plot(x_title, y_title, main_title,  &
                     x,y,kz,mn,xb,xe,yb,ye,3, &
                     mmx, mnx, mmy, mny, &
                     red, 3500, thick_dash, magnitude,0)
   
      call frame

!-------------------plot eigenvectors ----------------   
   
      x_title = vn // ' E VEC'
    
      mmx       = 8
      mnx       = 2
   
      do k=1,kz
         x(k) = ev%g_vec(k,1)
      end do
   
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
   
      call norm_min_max(xmin, xmax, magnitude)
   
      fct = 10.0**magnitude
   
      xb =-80.0
      xe = 80.0
   
      line_color = red
   
      do mn=1,5
         fct = 10.0**magnitude *ev%g_val(mn) / ev%g_val(1)
   
         do k=1,kz
!test            x(k) = ev%g_vec(k,mn)*fct
           x(k) = ev%g_vec(k,mn)*10.0**magnitude
         end do

         call line_plot(x_title, y_title, main_title,  &
                        x,y,kz,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        line_color, 6000-1000*mn, thick_dash, magnitude,0)

         line_color = line_color + 1
   
         if(line_color == cyran ) line_color = line_color + 1
         if(line_color > 7) line_color = red
      end do
   
      call frame

   end subroutine  eigen_plot
 
   subroutine eigen_plot2 (vn, ev, main_title, iopt)
      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   integer, parameter :: black = 0, &
                         white = 1, &
                           red = 2, &
                         green = 3, &
                          blue = 4, &
                        violet = 5, &
                         cyran = 6, &
                       magenta = 7, &
                      freshred = 8, &
                           tan = 9, &
                        yellow = 10, &
                          gray = 11

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y

      integer                         :: iopt

      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, ymin, ymax, fct

      print '("PLOT LOCAL E VAL:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      if (iopt.eq.99) then
        ix = size(ev%l_vec, dim=3) ! dimension of bin
        jy = size(ev%l_vec, dim=1) ! k-level 
        kz = size(ev%l_vec, dim=2) ! mode index

      elseif (iopt.eq.98) then
        ix = size(ev%power, dim=1)
        jy = size(ev%power, dim=2)
      endif

      mmy       = 6
      mny       = 2

      xb        = 1.0
      xe        = real(ix)

      if (iopt .eq. 99) y_title   = 'EIGEN VALUE'
      if (iopt .eq. 98) y_title   = 'POWER SPECTRUM'

      do k=1, ix
         x(k) = real(k)
      end do

!-----------------------------------------------------

      if (iopt .eq. 99) x_title = vn // ' LATITUDE (GRID)'
      if (iopt .eq. 98) x_title = vn // ' WAVE NUMBER'

      mmx       = 8
      mnx       = 2

      do j=1,ix
        if (iopt.eq.99)   y(j) = ev%l_val(1,j)
        if (iopt.eq.98)   y(j) = ev%power(j,1)
      enddo

      ymin = minval(y(1:ix))
      ymax = maxval(y(1:ix))

      call norm_min_max(ymin, ymax, magnitude)

      fct = 10.0**magnitude

      if (iopt .eq. 99) then

        yb =0.0
        ye =100.0

        if (vn.eq.'CHI_U'.or.vn.eq.'CHI'.or.vn.eq.'CHI_B') then 
          ye=50.0
        elseif (vn.eq.'RH') then
          ye =100.0
        endif

      else 

        yb = 0.0
        ye = 150.0
        if (vn.eq.'CHI_U'.or.vn.eq.'CHI'.or.vn.eq.'CHI_B'.or.  &
            vn.eq.'RH')  ye=20.0
        if (vn.eq.'T_U'.or.vn.eq.'T'.or.vn.eq.'T_B') &
          ye=50.0

      endif

!      if (iopt.eq.98.and.vn.eq.'rh') ye=150.
      if (iopt.eq.98.and.vn.eq.'chi_b') ye=100.
      line_color = red
   
      do mn=1,5

         do j=1,ix
!test            x(k) = ev%g_vec(k,mn)*fct
        if (iopt.eq.99) y(j) = ev%l_val(mn,j)*10.0**magnitude
        if (iopt.eq.98) y(j) = ev%power(j,mn)*10.0**magnitude
       
         end do
         call line_plot(x_title, y_title, main_title,  &
                        x,y,ix,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        line_color, 6000-1000*mn, thick_dash, magnitude,2)

         line_color = line_color + 1

         if(line_color == cyran ) line_color = line_color + 1
         if(line_color > 7) line_color = red
      end do

      call frame

   end subroutine eigen_plot2

   subroutine scale_plot(vn, ev, main_title, dist)

      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                            thick_dash = 21845, & ! PATTERN = 0101010101010101
                            thin_dash =  3855, &  ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   integer, parameter :: black =  0, &
                         white =  1, &
                           red =  2, &
                         green =  3, &
                          blue =  4, &
                        violet =  5, &
                         cyran =  6, &
                       magenta =  7, &
                      freshred =  8, &
                           tan =  9, &
                        yellow = 10, &
                          gray = 11

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      real,                intent(in) :: dist 
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y
   
      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, xmin, xmax, fct

      print '("PLOT SCALE LENGTH:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      ix = size(ev%l_vec, dim=3)   ! dimension of bin
      jy = size(ev%l_vec, dim=1)   ! k-level
      kz = size(ev%l_vec, dim=2)   ! mode index

      mmy       = 6

      mny       = 2

      yb        = 1.0
      ye        = real(kz)
   
      do k=1, 501
         y(k) = real(k)
      end do

      mmx       = 5
      mnx       = 5

      do k=1,kz
         x(k)   = ev%sl(k)
         write(unit=15, fmt='(i4,f10.1,3x,f20.1)') k,x(k), dist*x(k)
      end do
      x_title = vn // ' SCALE LENGTH(KM)'
   
      x(1:kz)  = x(1:kz) * dist
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
  
      call norm_min_max(xmin, xmax, magnitude)

      fct = 10.0**magnitude
      x(1:kz)  = fct * x(1:kz) 
   
      xb = 0.0
      xe = maxval (x(1:kz))
   
      mn = 0
  
      y_title   = 'VERTICAL MODE'
 
      call line_plot(x_title, y_title, main_title,  &
                     x,y,kz,mn,xb,xe,yb,ye, 1, &
                     mmx, mnx, mmy, mny, &
                     red, 3500, thick_dash, magnitude,0)
      call frame

!-----------------------------------------------------   
   
   end subroutine scale_plot

   subroutine line_plot(x_title, y_title, main_title,  &
                        z,y,n,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mix, mmy, iy, &
                        color, width, line_pattern, magnitude,iopt)
   
      implicit none

      real, parameter    :: xfb = 0.10, &
                         xfe = 0.90, &
                         yfb = 0.10, &
                         yfe = 0.90

      integer, parameter :: black = 0, &
                         white = 1, &
                           red = 2, &
                         green = 3, &
                          blue = 4, &
                        violet = 5, &
                         cyran = 6, &
                       magenta = 7, &
                      freshred = 8, &
                           tan = 9, &
                        yellow = 10, &
                          gray = 11
  
      integer                        :: iopt 
      character(len=80),  intent(in) :: x_title, y_title
      character(len=120), intent(in) :: main_title
      integer,            intent(in) :: n, &      !  vertical points
                                        mn, &     !  mode number
                                        plot_type, &
                                        mmx, mix, mmy, iy, &
                                        color, width, line_pattern, magnitude
      real, dimension(501), intent(in) :: z, y
      real,               intent(in) :: xb,xe,yb,ye
   
      integer                        :: lx, ly
      real                           :: xc, yc, xw, yw
      character(len=120)             :: title,title2
      character(len=  2)             :: chr_2
      character(len=  3)             :: chr_3
   
      real, dimension(501)           :: x

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

      write(chr_3(1:3), fmt='(i3)') magnitude
  
      if (iopt.eq.0)then
        title = trim(x_title) // ' MAGNIFIED BY 10**(' // chr_3 // ')'
        title2 = trim(y_title)
      elseif (iopt.eq.2) then
        title = trim(x_title) 
        title2 = trim(y_title)//' MAGNIFIED BY 10**(' // chr_3 // ')' 
      endif
      print '(I2," magnitude=",i6,2x,a)', mn, magnitude, chr_3
      print '("title=",a)', title

      lx = len_trim(  title)
      ly = len_trim(title2)

      call setusv('LW',1500)

      call gsplci(5)
      call gspmci(5)
      call gstxci(5)

      call pwrity(0.1-0.075,0.5,trim(title2),ly,16,90,0)
      call pwrity(0.5,0.1-0.075,       title, lx,16, 0,0)
      lx = len_trim( main_title)
      call pwrity(0.5,1.0-0.05,   main_title, lx,16, 0,0)
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!      call gsplci(cyran)
!      call gspmci(cyran)
!      call gstxci(cyran)
      call gsplci(blue)
      call gspmci(blue)
      call gstxci(blue)

      call set(xfb,xfe,yfb,yfe,xb,xe,yb,ye,plot_type)
   
      call setusv('LW',2000)
   
      if(plot_type > 2) then
         call labmod('(E7.1)','(I4)',7,4,15,15,0,0,0)
      else
         call labmod('(I3)','(I3)',3,3,15,15,0,0,0)
      end if
   
      call halfax(mmx, mix, mmy, iy, xb, yb, 1, 1)
   
      if(plot_type < 3) call line(0.0, y(1), 0.0, y(n))
   
      call gsplci(color)
      call gspmci(color)
      call gstxci(color)
   
      call dashdb (line_pattern)
   
      call setusv('LW',width)
   
      x = z
     do lx=1,n
!        write(unit=*, fmt='(a, i5, 2e12.4)') &
!             'n,x,y=', lx, x(lx), y(lx)
        if(x(lx) < xb) x(lx) = xb
     end do
   
      call curve(x,y,n)
   
      if(mn > 0) then

         write(chr_2(1:2), fmt='(i2.2)') mn
   
         call pwrity(x(1), y(1)-0.005*(y(n)-y(1)), chr_2, 2, 16, 0, 0)
      end if

      if (x_title(7:9) == 'VEC') then
         call line(xe-25.0, ye-mn*1.5, xe-3.0,ye-mn*1.5)
         write(title,'("VECT ",I2)') mn
         call setusv('LW',2000)
         call pwrity(xe-15.0, ye-mn*1.5+0.5, title(1:7),7,10,0,0)
      endif
   
   end subroutine line_plot
  
   subroutine norm_min_max(xmin, xmax, magnitude)

      implicit none

      real,    intent(inout) :: xmin, xmax
      integer, intent(out)   :: magnitude


      real                   :: ymin, ymax

      magnitude = 0

      if(abs(xmin) < 1.0e-30 .and. abs(xmax) < 1.0e-30) return


      do
         if(abs(xmin) > 100.0 .or. abs(xmax) > 100.0) then
            magnitude = magnitude - 1
            xmin = 0.1*xmin
            xmax = 0.1*xmax
         else
            exit
         end if
      end do

      do
         if(abs(xmin) < 10.0 .and. abs(xmax) < 10.0) then
            magnitude = magnitude + 1
            xmin = 10.0*xmin
            xmax = 10.0*xmax
         else
            exit
         end if
      end do

      xmin = -100.0
      xmax =  100.0

   end subroutine norm_min_max

   subroutine Balance_ratio_plot (ps, chi, temp, nj, nk,main_title)

      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   integer, parameter :: black = 0, &
                         white = 1, &
                           red = 2, &
                         green = 3, &
                          blue = 4, &
                        violet = 5, &
                         cyran = 6, &
                       magenta = 7, &
                      freshred = 8, &
                           tan = 9, &
                        yellow = 10, &
                          gray = 11

      character(len= 120), intent(in) :: main_title
      integer            , intent(in) :: nj, nk
      real (kind=4),       intent(in) :: ps(nj)
      real (kind=4),       intent(in) :: chi(nk), temp(nk)

      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y

      integer                         :: iopt

      integer :: plot_type, mmx, mnx, mmy, mny, k, j, mn, magnitude

      real    :: xb, xe, yb, ye, ymin, ymax, fct

      print '("PLOT RATIO OF BALANCE PART: nj,nk", 2I5,a)', nj,nk, main_title(1:40)

      plot_type =  1
      magnitude =  1
      mn = 0

! 1.0 Surface pressure

      mmy       = 10
      mny       =  1

      yb =  0.0
      ye = 10.0

      y_title   = '<XB X> / <X X>'

      mmx       = nj/10 + 1
      mnx       =  5

      xb        = 0.0
      xe        = real(mmx*10)

      x_title =  ' Y-LATITUDE (GRID)'

         do k=1, nj
           x(k) = real(k)
         end do

         do j=1,nj
           y(j) = ps(j) * 10.0**magnitude
         end do

         call line_plot(x_title, y_title, main_title,  &
                        x,y,nj, mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        red, 3500, thick_dash, magnitude, 2)

         call frame

! 2.0 Chi and temperature

      mmy       = 6
      mny       = 5

      yb =  0.0
      ye = float(nk)

      y_title   = 'MODEL LEVEL'

      mmx       = 10
      mnx       =  5

      xb        = 0.0
      xe        = real(10)

      x_title =  '<XB X> / <X X>'

         do k=1, nk
           y(k) = real(k)
         end do

         do k=1,nk
           x(k) = chi(k) * 10.0**magnitude
         end do

         call line_plot(x_title, y_title, main_title,  &
                        x,y,nk, mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        blue, 3500, thick_dash, magnitude, 0)

         do k=1,nk
           x(k) = temp(k) * 10.0**magnitude
         end do

         call line_plot(x_title, y_title, main_title,  &
                        x,y,nk, mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        red, 3500, thick_dash, magnitude, 0)

         call frame

   end subroutine Balance_ratio_plot

   subroutine Covariance_plot (vn, ps, nj, main_title)

      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   integer, parameter :: black = 0, &
                         white = 1, &
                           red = 2, &
                         green = 3, &
                          blue = 4, &
                        violet = 5, &
                         cyran = 6, &
                       magenta = 7, &
                      freshred = 8, &
                           tan = 9, &
                        yellow = 10, &
                          gray = 11

      character(len= 5), intent(in)   :: vn
      character(len= 120), intent(in) :: main_title
      integer            , intent(in) :: nj
      real (kind=4),       intent(in) :: ps(nj)

      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y

      integer :: mmx, mnx, mmy, mny, k, j, magnitude

      real    :: xb, xe, yb, ye, ymin, ymax, fct

      print '("PLOT COVARIANCE: nj",I5,a,2x,a)', nj, vn, main_title(1:40)
       magnitude =  -3

       y_title   = trim(vn)//' COVARIANCE'

       mmx       = nj/10 + 1
       xe = real(mmx*10)
       x_title =  ' Y-LATITUDE (GRID)'

         do k=1, nj
           x(k) = real(k)
         end do

         do j=1,nj
           y(j) = ps(j) * 10.0**magnitude
         end do

         call line_plot(x_title, y_title, main_title,  &
                        x,y,nj, 0, 0.0, xe, 0.0, 4.0, 1, &
                        mmx, 5, 4, 5, &
                        red, 3500, thick_dash, magnitude, 2)

         call frame

  end subroutine Covariance_plot

end program plot_reg_coeff
