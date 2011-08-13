program gen_be_stage1

!---------------------------------------------------------------------------------------------
! Authors: Dale Barker,    NCAR/MMM/DAG 
!          Yann Michel,    NCAR/MMM/DAG & CNRM-GAME/GMAP
!          Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
!---------------------------------------------------------------------------------------------
! Purpose : To remove the binned mean from the perturbation fields.
!
! Input   : binary files: "pert.ccyymmddhh.e"ce for ENS or
!                         "pert.ccyymmddhh.e001" for NMC.
!
! Output : binary files for use of the gen_be_stage2
!
!---------------------------------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len, vertical_ip
   use da_tools_serial, only : da_get_unit,da_advance_cymdh
   use da_gen_be, only : da_create_bins, write_2d_field, &
   	read_3d_field, write_3d_field, update_rain_bin
   
   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates (ccyymmddhh).
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name.
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*3         :: ce                         ! Ensemble member index.
   character(len=filename_len)        :: dat_dir                    ! Input data directory.
   character(len=filename_len)        :: filename                   ! Input filename.
   character (len=3)   :: model                      ! Model name 'WRF', 'GSI' or 'UM' etc. 
   character (len=4)   :: masscv                     ! Mass ctrl variable = press or temp
   character (len=10)  :: balpres                    ! Balance pressure = purestats or linestats

   integer             :: counter                    ! counter
   integer             :: counter_bin                ! counter for bins
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: member                     ! Loop counterer
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Interval between file times (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).  
   integer             :: N_holm_bins                ! Number of bins for Holm RH
   
   real                :: counter_inv                  ! 1 / counter.
   real                :: ds                         ! Grid resolution in meters.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lon               ! Used if bin_type = 9/10
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   
   real                :: min_psi, max_psi           ! Min and Max value of field
   real                :: min_chi, max_chi           ! Min and Max value of field
   real                :: min_vor, max_vor           ! Min and Max value of field
   real                :: min_div, max_div           ! Min and Max value of field
   real                :: min_t, max_t               ! Min and Max value of field
   real                :: min_q, max_q               ! Min and Max value of field
   real                :: min_qc, max_qc             ! Min and Max value of field
   real                :: min_qr, max_qr             ! Min and Max value of field
   real                :: min_qi, max_qi             ! Min and Max value of field
   real                :: min_ps, max_ps             ! Min and Max value of field
   
   real, allocatable   :: psi_prime(:,:,:)           ! Streamfunction perturbation.
   real, allocatable   :: chi_prime(:,:,:)           ! Velocity Potential perturbation.
   real, allocatable   :: vor_prime(:,:,:)           ! Vorticity perturbation.
   real, allocatable   :: div_prime(:,:,:)           ! Divergence perturbation.
   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t_prime(:,:,:)             ! Temperature perturbation.
   real, allocatable   :: rh_prime(:,:,:)            ! Relative Humidity Perturbation.
   real, allocatable   :: qcloud_prime(:,:,:)        ! Cloud Water mixing ratio
   real, allocatable   :: qrain_prime(:,:,:)         ! Cloud Rain  mixing ratio
   real, allocatable   :: qice_prime(:,:,:)          ! Cloud Ice   mixing ratio
   real, allocatable   :: qsnow_prime(:,:,:)         ! Cloud Snow  mixing ratio
   
   real, allocatable   :: qcond_prime(:,:,:)        ! Cloud Condensate
   
   real, allocatable   :: psi_bin_mean(:,:)        ! Streamfunction Mean field.
   real, allocatable   :: chi_bin_mean(:,:)        ! Velocity Potential Mean field.
   real, allocatable   :: vor_bin_mean(:,:)        ! Vorticity Mean field.
   real, allocatable   :: div_bin_mean(:,:)        ! Divergence Mean field.
   real, allocatable   :: t_bin_mean(:,:)          ! Temperature Mean field.
   real, allocatable   :: rh_bin_mean(:,:)         ! Relative Humidity Mean field.
   real, allocatable   :: ps_bin_mean(:)           ! Surface pressure Mean field.
   real, allocatable   :: qcloud_bin_mean(:,:)     ! Mean Cloud Water mixing ratio
   real, allocatable   :: qrain_bin_mean(:,:)      ! Mean Cloud Rain  mixing ratio
   real, allocatable   :: qice_bin_mean(:,:)       ! Mean Cloud Ice   mixing ratio
   real, allocatable   :: qsnow_bin_mean(:,:)      ! Mean Cloud Snow  mixing ratio

   real, allocatable   :: latitude(:,:)            ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)            ! Height field.
   
   real, allocatable   :: rh(:,:,:)                ! RH of background
   real, allocatable   :: rhm(:,:,:)               ! Mean RH of 2 perturbations   
   real, allocatable   :: mean_drh(:,:)            ! Mean of dRH as a function of RH
   real, allocatable   :: mean_drhm(:,:)           ! Mean of dRH as a function of RHM
   real, allocatable   :: var_drh(:,:)             ! Variance of dRH as a function of RH
   real, allocatable   :: var_drhm(:,:)            ! Variance of dRH as a function of RHM

   real, allocatable   :: temp(:,:,:)               ! T of background
   real, allocatable   :: tempm(:,:,:)              ! Mean T of 2 perturbations   
   real, allocatable   :: cld_fra(:,:,:)            ! Cloud Fraction of background
   real, allocatable   :: cld_fram(:,:,:)           ! Mean Cloud Fraction of 2 perturbations   
   
   real, allocatable   :: mean_qcond(:,:)           ! Mean of Cloud Condensate as a function of CLD_FRA
   real, allocatable   :: var_qcond(:,:)            ! Variance of Cloud Condensate as a function of CLD_FRA
   real, allocatable   :: mean_qcondm(:,:)          ! Mean of Cloud Condensate as a function of CLD_FRAM
   real, allocatable   :: var_qcondm(:,:)           ! Variance of Cloud Condensate as a function of CLD_FRAM
     
   integer, allocatable :: bin(:,:,:)                  ! Bin assigned to each 3D point.
   integer, allocatable :: bin2d(:,:)                  ! Bin assigned to each 2D point.
   integer, allocatable :: ij_counter_rc(:,:,:)        ! counter for each class
   
   integer, allocatable :: counter_holm_rh(:,:)        ! counter for Holm class
   integer, allocatable :: counter_holm_rhm(:,:)       ! counter for Holm class
   integer, allocatable :: counter_holm_cldfra(:,:)    ! counter for Holm class
   integer, allocatable :: counter_holm_cldfram(:,:)   ! counter for Holm class
   
   integer             :: i, j, k, b

   namelist / gen_be_stage1_nl / start_date, end_date, interval, &
                                 be_method, ne, bin_type, model, &
                                 lat_min, lat_max, binwidth_lat, binwidth_lon, &
                                 hgt_min, hgt_max, binwidth_hgt, dat_dir, &
				 masscv, balpres, vertical_ip, model, N_holm_bins

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   be_method = 'NMC'
   ne = 1
   bin_type = 5         ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   N_holm_bins = 1
   dat_dir = '/data2/hcshin/youn/DIFF63'
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Read Namelist (gen_be_stage1_nl.nl)'
!---------------------------------------------------------------------------------------------

   open(unit=namelist_unit, file='gen_be_stage1_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_nl)
   close(namelist_unit)

   if ( be_method /= "ENS" ) ne = 1

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne
   write(6,'(2a)') ' Data dir is ',trim(dat_dir)
   date = start_date
   cdate = sdate
   counter = 0

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from standard files, and calculate mean fields'
!---------------------------------------------------------------------------------------------

   do while ( cdate <= edate )
      do member = 1, ne
         counter = counter + 1
         counter_inv = 1.0 / real(counter)

         if ( be_method == 'NMC' ) then
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif

         open (iunit, file = trim(filename), form='unformatted')
         read(iunit)date, ni, nj, nk
	 
         if ( counter == 1 ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( t_prime(1:ni,1:nj,1:nk) )
            allocate( psi_prime(1:ni,1:nj,1:nk) )
            allocate( chi_prime(1:ni,1:nj,1:nk) )
	    allocate( vor_prime(1:ni,1:nj,1:nk) )
            allocate( div_prime(1:ni,1:nj,1:nk) )
            allocate( rh_prime(1:ni,1:nj,1:nk) )
            allocate( ps_prime(1:ni,1:nj) )
            allocate( qcloud_prime(1:ni,1:nj,1:nk) )
            allocate( qrain_prime(1:ni,1:nj,1:nk) )
            allocate( qice_prime(1:ni,1:nj,1:nk) )
            allocate( qsnow_prime(1:ni,1:nj,1:nk) )

	    ! Binning GSI uses latitude-dependent binning (2), but set to domain average (5)
	    if( model(1:3) == "GSI" .or. model(1:3) == "gsi") then
	    	bin_type = 5
	    end if
	    
	    allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
	    
	    call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
                 lat_min, lat_max, binwidth_lat, &
                 hgt_min, hgt_max, binwidth_hgt, latitude, height )

            ! write bin info:
            filename = 'bin.data'
	    write(6,'(3a,i4,a,i4)') '    Write bin data in ',trim(filename),' bin_type=',bin_type,&
	    	' num_bins2d=',num_bins2d
            open (ounit, file = filename, form='unformatted')
            write(ounit)bin_type
            write(ounit)lat_min, lat_max, binwidth_lat
            write(ounit)hgt_min, hgt_max, binwidth_hgt
            write(ounit)num_bins, num_bins2d
            write(ounit)bin(1:ni,1:nj,1:nk)
            write(ounit)bin2d(1:ni,1:nj)
            close(ounit)

            allocate(ij_counter_rc(1:ni,1:nj,1:num_bins2d) )
            ij_counter_rc(:,:,:) = 0
	    
	    allocate( psi_bin_mean(1:num_bins2d,1:nk) )
            allocate( chi_bin_mean(1:num_bins2d,1:nk) )
            allocate( vor_bin_mean(1:num_bins2d,1:nk) )
            allocate( div_bin_mean(1:num_bins2d,1:nk) )
            allocate( t_bin_mean(1:num_bins2d,1:nk) )
            allocate( rh_bin_mean(1:num_bins2d,1:nk) )
            allocate( qcloud_bin_mean(1:num_bins2d,1:nk) )
            allocate( qrain_bin_mean(1:num_bins2d,1:nk) )
            allocate( qice_bin_mean(1:num_bins2d,1:nk) )
	    allocate( qsnow_bin_mean(1:num_bins2d,1:nk) )
            allocate( ps_bin_mean(1:num_bins2d) )

            vor_bin_mean(:,:) = 0.0
            div_bin_mean(:,:) = 0.0
            psi_bin_mean(:,:) = 0.0
            chi_bin_mean(:,:) = 0.0
            t_bin_mean(:,:) = 0.0
            rh_bin_mean(:,:) = 0.0
            qcloud_bin_mean(:,:) = 0.0
            qrain_bin_mean(:,:) = 0.0
            qice_bin_mean(:,:) = 0.0
	    qsnow_bin_mean(:,:) = 0.0
            ps_bin_mean(:) = 0.0
         
	 end if
	 
         read(iunit)psi_prime
         read(iunit)chi_prime
	 read(iunit)vor_prime
         read(iunit)div_prime
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)ps_prime
         read(iunit)qcloud_prime 
         read(iunit)qrain_prime
         read(iunit)qice_prime
         read(iunit)qsnow_prime
	 close(iunit)
	 
	 write(6,'(3a,i3)')'    Processing data for date ', date, ' member ',member
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if   

  	 do j = 1, nj
             do i = 1, ni
                b = bin2d(i,j)
                ! update counter
                ij_counter_rc(i,j,b) = ij_counter_rc(i,j,b)+1
                ! number of points in bin b
                counter_bin=sum(ij_counter_rc(:,:,b))
                ! update mean
                psi_bin_mean(b,:) = (real(counter_bin-1)*psi_bin_mean(b,:)+psi_prime(i,j,:))/real(counter_bin)
                chi_bin_mean(b,:) = (real(counter_bin-1)*chi_bin_mean(b,:)+chi_prime(i,j,:))/real(counter_bin)
                vor_bin_mean(b,:) = (real(counter_bin-1)*vor_bin_mean(b,:)+vor_prime(i,j,:))/real(counter_bin)
                div_bin_mean(b,:) = (real(counter_bin-1)*div_bin_mean(b,:)+div_prime(i,j,:))/real(counter_bin)
                t_bin_mean(b,:) = (real(counter_bin-1)*t_bin_mean(b,:)+t_prime(i,j,:))/real(counter_bin)
                rh_bin_mean(b,:) = (real(counter_bin-1)*rh_bin_mean(b,:)+rh_prime(i,j,:))/real(counter_bin)
                qcloud_bin_mean(b,:) = (real(counter_bin-1)*qcloud_bin_mean(b,:)+qcloud_prime(i,j,:))/real(counter_bin)
                qrain_bin_mean(b,:) = (real(counter_bin-1)*qrain_bin_mean(b,:)+qrain_prime(i,j,:))/real(counter_bin)
                qice_bin_mean(b,:) = (real(counter_bin-1)*qice_bin_mean(b,:)+qice_prime(i,j,:))/real(counter_bin)
		qsnow_bin_mean(b,:) = (real(counter_bin-1)*qsnow_bin_mean(b,:)+qsnow_prime(i,j,:))/real(counter_bin)
                ps_bin_mean(b) = (real(counter_bin-1)*ps_bin_mean(b)+ps_prime(i,j))/real(counter_bin)
             end do
         end do
	    
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
  
   filename = "binned_mean_stage1.dat"
   open (ounit, file = filename, form='unformatted')
   write(ounit)ni, nj, nk, num_bins2d
   ! split for easy NCL reading (differences between C/Fortran)
   do b=1,num_bins2d
      write(ounit)ij_counter_rc(:,:,b)
      write(ounit)psi_bin_mean(b,:)
      write(ounit)chi_bin_mean(b,:)
      write(ounit)vor_bin_mean(b,:)
      write(ounit)div_bin_mean(b,:)
      write(ounit)t_bin_mean(b,:)
      write(ounit)rh_bin_mean(b,:)
      write(ounit)qcloud_bin_mean(b,:)
      write(ounit)qrain_bin_mean(b,:)
      write(ounit)qice_bin_mean(b,:)
      write(ounit)qsnow_bin_mean(b,:)
      write(ounit)ps_bin_mean(b) 
    end do
    close(ounit)
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [3] Read fields again, and remove binned mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate
   counter = 0

   do while ( cdate <= edate )
      do member = 1, ne
         write(6,'(3a,i3)')'    Removing Time mean for date ', date,' member ',member
	 write(ce,'(i3.3)')member
         
	 counter = counter + 1
	 
	 if (counter==1) then
	    if( model(1:3) == "GSI" .or. model(1:3) == "gsi") then
	        filename = trim(dat_dir)//'pert_files'
		write(6,'(2a)')' For GSI writing perturbation filenames in ',trim(filename)
                open (unit=99, file = trim(filename), form='formatted')
	    end if
	 end if
	 	 
         if ( be_method == 'NMC' ) then
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif
	         
	 open (iunit, file = trim(filename), form='unformatted')
         read(iunit)date, ni, nj, nk
	 read(iunit)psi_prime
         read(iunit)chi_prime
         read(iunit)vor_prime
         read(iunit)div_prime
         read(iunit)t_prime
         read(iunit)rh_prime
	 read(iunit)ps_prime
         read(iunit)qcloud_prime
         read(iunit)qrain_prime
         read(iunit)qice_prime
	 read(iunit)qsnow_prime
         close(iunit)
	 
	 if (bin_type==7) then
            call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
	 end if   
	 
	 do j = 1, nj
             do i = 1, ni
                b=bin2d(i,j)
                psi_prime(i,j,:) = psi_prime(i,j,:)-psi_bin_mean(b,:)
                chi_prime(i,j,:) = chi_prime(i,j,:)-chi_bin_mean(b,:)
                vor_prime(i,j,:) = vor_prime(i,j,:)-vor_bin_mean(b,:)
                div_prime(i,j,:) = div_prime(i,j,:)-div_bin_mean(b,:)
                t_prime(i,j,:)  = t_prime(i,j,:)-t_bin_mean(b,:)
                rh_prime(i,j,:) = rh_prime(i,j,:)-rh_bin_mean(b,:)
                qcloud_prime(i,j,:) = qcloud_prime(i,j,:)-qcloud_bin_mean(b,:)
                qrain_prime(i,j,:) = qrain_prime(i,j,:)-qrain_bin_mean(b,:)
                qice_prime(i,j,:) = qice_prime(i,j,:)-qice_bin_mean(b,:)
		qsnow_prime(i,j,:) = qsnow_prime(i,j,:)-qsnow_bin_mean(b,:)
                ps_prime(i,j) = ps_prime(i,j)-ps_bin_mean(b)
             end do
          end do
	    
         close(ounit)

      !  Write psi:
	 variable = 'psi'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, psi_prime)
      !  Write chi:
	 variable = 'chi'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, chi_prime)
      !  Write vor:
	 variable = 'vor'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, vor_prime)
      !  Write div:
	 variable = 'div'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, div_prime)
      !  Write T:
	 variable = 't'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, t_prime)
      !  Write RH:
	 variable = 'rh'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, rh_prime)
      !  Write qcloud:
	 variable = 'qcloud'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, qcloud_prime)
      !  Write qrain:
	 variable = 'qrain'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, qrain_prime)
      !  Write qice:
	 variable = 'qice'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, qice_prime)
      !  Write qsnow:
	 variable = 'qsnow'
	 call write_3d_field(variable, date, ce, ounit, ni, nj, nk, qsnow_prime)
      !  Write ps:
	 variable = 'ps' ! 2D field
	 call write_2d_field(variable, date, ce, ounit, ni, nj, 1, ps_prime)
	 
	 if( model(1:3) == "GSI" .or. model(1:3) == "gsi") then
	    write(99,'(a)')trim(filename)
	 end if
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   deallocate( psi_bin_mean)
   deallocate( chi_bin_mean)
   deallocate( vor_bin_mean)
   deallocate( div_bin_mean)
   deallocate( rh_bin_mean)
   deallocate( t_bin_mean)
   deallocate( qcloud_bin_mean)
   deallocate( qrain_bin_mean)
   deallocate( qice_bin_mean)
   deallocate( qsnow_bin_mean)
   deallocate( ps_bin_mean)
   
   deallocate( psi_prime)
   deallocate( chi_prime)
   deallocate( vor_prime)
   deallocate( div_prime)
!   deallocate( rh_prime)
   deallocate( t_prime)
!   deallocate( qcloud_prime)
   deallocate( qrain_prime)
!   deallocate( qice_prime)
!   deallocate( qsnow_prime)
   deallocate( ps_prime)
   
   if( model(1:3) == "GSI" .or. model(1:3) == "gsi") then
       close(99)
   end if

if (N_holm_bins>1 .and. bin_type/=7) then

  !---------------------------------------------------------------------------------------------
   write(6,'(a,i8)')' [4] Compute Holm transform with number of bins = ',N_holm_bins
  !---------------------------------------------------------------------------------------------
  
  allocate( rh(1:ni,1:nj,1:nk) )
  allocate( rhm(1:ni,1:nj,1:nk) )
  allocate( temp(1:ni,1:nj,1:nk) )
  allocate( tempm(1:ni,1:nj,1:nk) )
  allocate( cld_fra(1:ni,1:nj,1:nk) )
  allocate( cld_fram(1:ni,1:nj,1:nk) )
  allocate( qcond_prime(1:ni,1:nj,1:nk) )
  
  allocate( mean_drh(1:N_holm_bins,1:nk) )
  allocate( mean_drhm(1:N_holm_bins,1:nk) )
  allocate( var_drh(1:N_holm_bins,1:nk) )
  allocate( var_drhm(1:N_holm_bins,1:nk) )
  mean_drh(:,:) = 0.
  mean_drhm(:,:) = 0.
  var_drh(:,:) = 0.
  var_drhm(:,:) = 0.

  allocate( mean_qcond(1:N_holm_bins,1:nk) )
  allocate( var_qcond(1:N_holm_bins,1:nk) ) 
  allocate( mean_qcondm(1:N_holm_bins,1:nk) )
  allocate( var_qcondm(1:N_holm_bins,1:nk) )
  mean_qcond(:,:)=0.0
  var_qcond(:,:)=0.0  
  mean_qcondm(:,:)=0.0
  var_qcondm(:,:)=0.0
    
  allocate(counter_holm_rh(1:N_holm_bins,1:nk))
  allocate(counter_holm_rhm(1:N_holm_bins,1:nk))
  allocate(counter_holm_cldfra(1:N_holm_bins,1:nk))
  allocate(counter_holm_cldfram(1:N_holm_bins,1:nk))
  counter_holm_rh(:,:) =0
  counter_holm_rhm(:,:)=0
  counter_holm_cldfra(:,:) =0
  counter_holm_cldfram(:,:)=0
!----------------------------------------------------------------------------------------------------!   
  date = start_date
  cdate = sdate
  counter = 0

  do while ( cdate <= edate )
     do member = 1, ne        
        counter = counter + 1
	write(ce,'(i3.3)')member        
        if ( be_method == 'NMC' ) then
           filename = 'fullflds/fullflds.'//date(1:10)//'.e001'
        else
           write(UNIT=ce,FMT='(i3.3)')member
           filename = 'fullflds/fullflds.'//date(1:10)//'.e'//trim(ce)
        endif
        open (iunit, file = filename, form='unformatted')
        read(iunit)date, ni, nj, nk
        read(iunit)rh
        read(iunit)rhm
        read(iunit)temp
        read(iunit)tempm
        read(iunit)cld_fra
        read(iunit)cld_fram
        close(iunit)

	! RH/CLD_FRA between 0 and 1
	rh(:,:,:) = min(1.0,max(0.0,rh))
	rhm(:,:,:) = min(1.0,max(0.0,rhm))
	cld_fra(:,:,:) = min(1.0,max(0.0,cld_fra))
	cld_fram(:,:,:) = min(1.0,max(0.0,cld_fram))
	
	!  Read dRH:
	variable = 'rh'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, rh_prime)
	!  Read qcloud:
	variable = 'qcloud'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qcloud_prime)
	!  Read qice:
	variable = 'qice'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qice_prime)
	!  Read qice:
	variable = 'qsnow'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qsnow_prime)
	! Build Cloud condensate and write it
	qcond_prime = qcloud_prime+qice_prime
	variable = 'qcond'
	call write_3d_field(variable, date, ce, ounit, ni, nj, nk, qcond_prime)
	
!       Compute Bias
	do k=1,nk
	   do i=1,ni
	      do j=1,nj
	         ! RH as a function of RH
	         b = 1+nint(rh(i,j,k)*(N_holm_bins-1))
		 counter_holm_rh(b,k) = counter_holm_rh(b,k)+1
	         mean_drh(b,k) = mean_drh(b,k)+rh_prime(i,j,k)
		 b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		 counter_holm_rhm(b,k) = counter_holm_rhm(b,k)+1
	         mean_drhm(b,k) = mean_drhm(b,k)+rh_prime(i,j,k)
		 ! QCOND as a function of CLDFRA
		 b = 1+nint(cld_fra(i,j,k)*(N_holm_bins-1))
		 counter_holm_cldfra(b,k) = counter_holm_cldfra(b,k)+1
	         mean_qcond(b,k) = mean_qcond(b,k)+qcond_prime(i,j,k)
		 b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-1))
		 counter_holm_cldfram(b,k) = counter_holm_cldfram(b,k)+1
	         mean_qcondm(b,k) = mean_qcondm(b,k)+qcond_prime(i,j,k)
		end do
	    end do	         
	end do
     end do  ! End loop over ensemble members.
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
   
   ! Normalize + Display results for rapid check
   do k=1,nk
	do b=1,N_holm_bins
	   if (counter_holm_rh(b,k).gt.0) then
		mean_drh(b,k) = mean_drh(b,k)/real(counter_holm_rh(b,k))
	   end if
	   if (counter_holm_rhm(b,k).gt.0) then
		mean_drhm(b,k) = mean_drhm(b,k)/real(counter_holm_rhm(b,k))
	   end if
	   if (counter_holm_cldfra(b,k).gt.0) then
		mean_qcond(b,k) = mean_qcond(b,k)/real(counter_holm_cldfra(b,k))
	   end if
	   if (counter_holm_cldfram(b,k).gt.0) then
		mean_qcondm(b,k) = mean_qcondm(b,k)/real(counter_holm_cldfram(b,k))
	   end if
	   if (k==1 .or. k==int(nk/2) .or. k==nk) then
	   	write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_rh(b,k),' bias drh    ',mean_drh(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_rhm(b,k),' bias drhm   ',mean_drhm(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_cldfra(b,k),' bias qcond  ',mean_qcond(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
		counter_holm_cldfram(b,k),' bias qcondm ',mean_qcondm(b,k)
	   end if
	end do
   end do

!----------------------------------------------------------------------------------------------------!   
! start again for stdev   
  date = start_date
  cdate = sdate
  counter = 0

  do while ( cdate <= edate )
     do member = 1, ne        
        counter = counter + 1
	write(ce,'(i3.3)')member        
        if ( be_method == 'NMC' ) then
           filename = 'fullflds/fullflds.'//date(1:10)//'.e001'
        else
           write(UNIT=ce,FMT='(i3.3)')member
           filename = 'fullflds/fullflds.'//date(1:10)//'.e'//trim(ce)
        endif
       
        open (iunit, file = filename, form='unformatted')
        read(iunit)date, ni, nj, nk
	read(iunit)rh
        read(iunit)rhm
        read(iunit)temp
        read(iunit)tempm
        read(iunit)cld_fra
        read(iunit)cld_fram
        close(iunit)

	! RH/CLD_FRA between 0 and 1
	rh(:,:,:) = min(1.0,max(0.0,rh))
	rhm(:,:,:) = min(1.0,max(0.0,rhm))
	cld_fra(:,:,:) = min(1.0,max(0.0,cld_fra))
	cld_fram(:,:,:) = min(1.0,max(0.0,cld_fram))
	
	!  Read dRH:
	variable = 'rh'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, rh_prime)
	!  Read qcloud:
	variable = 'qcloud'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qcloud_prime)
	!  Read qice:
	variable = 'qice'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qice_prime)
	!  Read qice:
	variable = 'qsnow'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qsnow_prime)
	! Build Cloud condensate
	qcond_prime = qcloud_prime+qice_prime

!       Compute Variance
	do k=1,nk
	   do i=1,ni
	      do j=1,nj
	         ! RH as a function of RH
	         b = 1+nint(rh(i,j,k)*(N_holm_bins-1))
		 var_drh(b,k) = var_drh(b,k)+((rh_prime(i,j,k)-mean_drh(b,k))**2.0)
		 b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
 		 var_drhm(b,k) = var_drhm(b,k)+((rh_prime(i,j,k)-mean_drhm(b,k))**2.0)
		 ! QCOND as a function of CLDFRA
		 b = 1+nint(cld_fra(i,j,k)*(N_holm_bins-1))
	         var_qcond(b,k) = var_qcond(b,k)+((qcond_prime(i,j,k)-mean_qcond(b,k))**2.0)
		 b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-1))
	         var_qcondm(b,k) = var_qcondm(b,k)+((qcond_prime(i,j,k)-mean_qcondm(b,k))**2.0)
		end do
	    end do	         
	end do
	
     end do  ! End loop over ensemble members.
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
   
   ! Normalize + Display results for rapid check
   do k=1,nk
	do b=1,N_holm_bins
	   if (counter_holm_rh(b,k).gt.0) then
		var_drh(b,k) = var_drh(b,k)/real(counter_holm_rh(b,k))
	   end if
	   if (counter_holm_rhm(b,k).gt.0) then
		var_drhm(b,k) = var_drhm(b,k)/real(counter_holm_rhm(b,k))
	   end if
	   if (counter_holm_cldfra(b,k).gt.0) then
		var_qcond(b,k) = var_qcond(b,k)/real(counter_holm_cldfra(b,k))
	   end if
	   if (counter_holm_cldfram(b,k).gt.0) then
		var_qcondm(b,k) = var_qcondm(b,k)/real(counter_holm_cldfram(b,k))
	   end if
	   if (k==1 .or. k==int(nk/2) .or. k==nk) then
	   	write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_rh(b,k),' var drh    ',var_drh(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_rhm(b,k),' var drhm   ',var_drhm(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
			counter_holm_cldfra(b,k),' var qcond  ',var_qcond(b,k)
		write(6,'(3(a,i8),2(a,f12.8))')' Vert. Lev ',k,'  Holm bin ',b,' Count ',&
		counter_holm_cldfram(b,k),' var qcondm ',var_qcondm(b,k)
	   end if
	end do
   end do
   
   filename = 'holm_stage1.dat'
   write(6,'(2a)')'       Store Holm bias and variance in ', trim(filename)
   open (ounit, file = filename, form='unformatted')
   write(ounit) N_holm_bins,nk
   write(ounit) mean_drh
   write(ounit) var_drh
   write(ounit) mean_drhm
   write(ounit) var_drhm
   write(ounit) mean_qcond
   write(ounit) var_qcond
   write(ounit) mean_qcondm
   write(ounit) var_qcondm
   close(ounit)
!----------------------------------------------------------------------------------------------------!   
   date = start_date
   cdate = sdate
   counter = 0

  do while ( cdate <= edate )
     do member = 1, ne        
        counter = counter + 1
	write(ce,'(i3.3)')member        
        if ( be_method == 'NMC' ) then
           filename = 'fullflds/fullflds.'//date(1:10)//'.e001'
        else
           write(UNIT=ce,FMT='(i3.3)')member
           filename = 'fullflds/fullflds.'//date(1:10)//'.e'//trim(ce)
        endif
	
        write(6,'(3a,i3)')' Non-linear change of rh/qcond variable for date ', date,' member ',member
	
        open (iunit, file = filename, form='unformatted')
        read(iunit)date, ni, nj, nk
        read(iunit)rh
        read(iunit)rhm
        read(iunit)temp
        read(iunit)tempm
        read(iunit)cld_fra
        read(iunit)cld_fram
        close(iunit)

	! RH/CLD_FRA between 0 and 1
	rh(:,:,:) = min(1.0,max(0.0,rh))
	rhm(:,:,:) = min(1.0,max(0.0,rhm))
	cld_fra(:,:,:) = min(1.0,max(0.0,cld_fra))
	cld_fram(:,:,:) = min(1.0,max(0.0,cld_fram))
	
	!  Read dRH:
	variable = 'rh'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, rh_prime)
	!  Read qcloud:
	variable = 'qcloud'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qcloud_prime)
	!  Read qice:
	variable = 'qice'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qice_prime)
	!  Read qice:
	variable = 'qsnow'
	call read_3d_field(variable, date, ce, ounit, ni, nj, nk, qsnow_prime)
	! Build Cloud condensate
	qcond_prime = qcloud_prime+qice_prime
	
	! Perform full non-linear change of variable
	do k=1,nk
  	   do i=1,ni
	      do j=1,nj
	         b = 1+nint(rhm(i,j,k)*(N_holm_bins-1))
		 if (var_drhm(b,k).gt.0) then
	         	rh_prime(i,j,k) = (rh_prime(i,j,k)-mean_drhm(b,k))/sqrt(var_drhm(b,k))
		 else
			rh_prime(i,j,k) = rh_prime(i,j,k)-mean_drhm(b,k)
		 end if
		 b = 1+nint(cld_fram(i,j,k)*(N_holm_bins-1))
		 if (var_qcondm(b,k)*cld_fram(i,j,k).gt.0) then
	         	qcond_prime(i,j,k) = (qcond_prime(i,j,k)-mean_qcondm(b,k))/&
			(sqrt(var_qcondm(b,k))*cld_fram(i,j,k))
		 else
		 	qcond_prime(i,j,k) = qcond_prime(i,j,k)-mean_qcondm(b,k)
		 end if
	      end do
	   end do
	end do   
	!  Write RHM
        variable = 'rhm'
        call write_3d_field(variable, date, ce, iunit, ni, nj, nk, rh_prime)
	!  Write QCM
        variable = 'qcondm'
        call write_3d_field(variable, date, ce, iunit, ni, nj, nk, qcond_prime)
	  
      end do  ! End loop over ensemble members.
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
end if
!----------------------------------------------------------------------------------------------------!   
write(6,'(a)')'    End of program gen_be_stage1'
!----------------------------------------------------------------------------------------------------!   
end program gen_be_stage1
