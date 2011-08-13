program gen_be_stage1_gsi
!
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! Author : Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
!
! Purpose : To remove the time mean from the perturbation fields.
!           created in STAGE0_GSI
! Input   : binary files: "pert.ccyymmddhh.e"ce for ENS or
!                         "pert.ccyymmddhh.e001" for NMC.
!
! Output : binary files for use of the gen_be_stage2_gsi:
!
!----------------------------------------------------------------------
!
   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh
   use da_gen_be, only : da_create_bins

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates (ccyymmddhh).
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*3         :: ce                         ! Ensemble member index.
   character(len=filename_len)        :: stage0_gsi_dir                 ! Input data directory.
   character(len=filename_len)        :: stage1_gsi_dir                 ! Input data directory.
   character(len=filename_len)        :: filename                   ! Input filename.
   integer             :: count                      ! Counter.
   integer             :: ncases                     ! Total number of perturbations
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: member                     ! Loop counter
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Interval between file times (hours).
   integer             :: ne                         ! Number of ensemble members.
   real                :: count_inv                  ! 1 / count.
   real                :: ds                         ! Grid resolution in meters.


   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t_prime(:,:,:)             ! Temperature perturbation.
   real, allocatable   :: psi_prime(:,:,:)           ! Streamfunction perturbation.
   real, allocatable   :: chi_prime(:,:,:)           ! Velocity Potential perturbation.
   real, allocatable   :: rh_prime(:,:,:)            ! Relative Humidity Perturbation.
   real, allocatable   :: qcloud_prime(:,:,:)        ! Cloud Water mixing ratio
   real, allocatable   :: qrain_prime(:,:,:)         ! Cloud Rain  mixing ratio
   real, allocatable   :: qice_prime(:,:,:)          ! Cloud Ice   mixing ratio
   real, allocatable   :: qsnow_prime(:,:,:)         ! Cloud Snow  mixing ratio


   real, allocatable   :: psi_mean(:,:,:)            ! Mean field.
   real, allocatable   :: chi_mean(:,:,:)            ! Mean field.
   real, allocatable   :: t_mean(:,:,:)              ! Mean field.
   real, allocatable   :: rh_mean(:,:,:)             ! Mean field.
   real, allocatable   :: ps_mean(:,:)               ! Mean field.
   real, allocatable   :: qcloudm(:,:,:)             ! Mean Cloud Water mixing ratio
   real, allocatable   :: qrainm(:,:,:)              ! Mean Cloud Rain  mixing ratio
   real, allocatable   :: qicem(:,:,:)               ! Mean Cloud Ice   mixing ratio
   real, allocatable   :: qsnowm(:,:,:)              ! Mean Cloud Snow  mixing ratio
   real, allocatable   :: qcloud_mean(:,:,:)         ! Mean Cloud Water mixing ratio
   real, allocatable   :: qrain_mean(:,:,:)          ! Mean Cloud Rain  mixing ratio
   real, allocatable   :: qice_mean(:,:,:)           ! Mean Cloud Ice   mixing ratio
   real, allocatable   :: qsnow_mean(:,:,:)          ! Mean Cloud Snow  mixing ratio
   real, allocatable   :: rhm(:,:,:)                 ! Mean RH                        
   real, allocatable   :: sum_hydro(:,:,:)           ! Total Hydrometeor Mixing raaio  


   real, allocatable   :: xlat(:,:)                  ! Latitude  of mass points.
   real, allocatable   :: xlon(:,:)                  ! Longitude of mass points.
   real, allocatable   :: mapfac_m(:,:)              ! Map factor - mass pts.
   real, allocatable   :: mapfac_mx(:,:)             ! Map factor - mass pts. in X-Dir
   real, allocatable   :: mapfac_my(:,:)             ! Map factor - mass pts. in Y-Dir
   real, allocatable   :: mapfac_u(:,:)              ! Map factor - u points.
   real, allocatable   :: mapfac_v(:,:)              ! Map factor - v points.
   real, allocatable   :: znu(:)                     ! Half sigma levels          

   integer             :: i, j, k 

   namelist / gen_be_stage1_gsi_nl / start_date, end_date, interval, &
                                 be_method, ne, stage0_gsi_dir, stage1_gsi_dir

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
   stage0_gsi_dir = '/ptmp/rizvi/data/con200/run_gen_be_gsi/stage0_gsi'                 
   stage1_gsi_dir = '/ptmp/rizvi/data/con200/run_gen_be_gsi/stage1_gsi'                 

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Read Namelist (gen_be_stage1_gsi_nl.nl)'
!---------------------------------------------------------------------------------------------

   open(unit=namelist_unit, file='gen_be_stage1_gsi_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_gsi_nl)
   close(namelist_unit)

   if ( be_method /= "ENS" ) ne = 1

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   count = 0

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from standard files, and calculate mean fields'
!---------------------------------------------------------------------------------------------

   do while ( cdate <= edate )
      do member = 1, ne
         count = count + 1
         count_inv = 1.0 / real(count)

         if ( be_method == 'NMC' ) then
            filename = trim(stage0_gsi_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(stage0_gsi_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif

         open (iunit, file = trim(filename), form='unformatted')
         read(iunit)date, ni, nj, nk, ds

         if ( count == 1 ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( t_prime(1:ni,1:nj,1:nk) )
            allocate( psi_prime(1:ni,1:nj,1:nk) )
            allocate( chi_prime(1:ni,1:nj,1:nk) )
            allocate( rh_prime(1:ni,1:nj,1:nk) )
            allocate( ps_prime(1:ni,1:nj) )
            allocate( qcloud_prime(1:ni,1:nj,1:nk) )
            allocate( qrain_prime(1:ni,1:nj,1:nk) )
            allocate( qice_prime(1:ni,1:nj,1:nk) )
            allocate( qsnow_prime(1:ni,1:nj,1:nk) )

            allocate( rhm(1:ni,1:nj,1:nk) )
            allocate( qcloudm(1:ni,1:nj,1:nk) )
            allocate( qrainm(1:ni,1:nj,1:nk) )
            allocate( qicem(1:ni,1:nj,1:nk) )
            allocate( qsnowm(1:ni,1:nj,1:nk) )
            allocate( sum_hydro(1:ni,1:nj,1:nk) )

            allocate( psi_mean(1:ni,1:nj,1:nk) )
            allocate( chi_mean(1:ni,1:nj,1:nk) )
            allocate( t_mean(1:ni,1:nj,1:nk) )
            allocate( rh_mean(1:ni,1:nj,1:nk) )
            allocate( ps_mean(1:ni,1:nj) )
            allocate( qcloud_mean(1:ni,1:nj,1:nk) )
            allocate( qrain_mean(1:ni,1:nj,1:nk) )
            allocate( qice_mean(1:ni,1:nj,1:nk) )
            allocate( qsnow_mean(1:ni,1:nj,1:nk) )

            allocate( xlat(1:ni,1:nj) )     
            allocate( xlon(1:ni,1:nj) )     
            allocate( mapfac_m(1:ni,1:nj) )     
            allocate( mapfac_mx(1:ni,1:nj) )     
            allocate( mapfac_my(1:ni,1:nj) )     
            allocate( mapfac_u(1:ni+1,1:nj) )
            allocate( mapfac_v(1:ni,1:nj+1) )
            allocate( znu(1:nk) )

            psi_mean(:,:,:) = 0.0
            chi_mean(:,:,:) = 0.0
            t_mean(:,:,:) = 0.0
            rh_mean(:,:,:) = 0.0
            ps_mean(:,:) = 0.0
            qcloud_mean(:,:,:) = 0.0
            qrain_mean(:,:,:) = 0.0
            qice_mean(:,:,:) = 0.0
            qsnow_mean(:,:,:) = 0.0

         end if

         write(6,'(3a,i3)')'    Processing data for date ', date, ' memebr ',member
         read(iunit)znu 
         read(iunit)xlat
         read(iunit)xlon
         read(iunit)mapfac_mx
         read(iunit)mapfac_my

         read(iunit)psi_prime
         read(iunit)chi_prime
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)rhm      
         read(iunit)ps_prime
         read(iunit)qcloud_prime 
         read(iunit)qcloudm      
         read(iunit)qrain_prime
         read(iunit)qrainm     
         read(iunit)qice_prime
         read(iunit)qicem      
         read(iunit)qsnow_prime
         read(iunit)qsnowm     

         close(iunit)

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Calculate time/ensemble mean.
!---------------------------------------------------------------------------------------------

         psi_mean = ( real( count-1 ) * psi_mean + psi_prime ) * count_inv
         chi_mean = ( real( count-1 ) * chi_mean + chi_prime ) * count_inv
         t_mean = ( real( count-1 ) * t_mean + t_prime ) * count_inv
         rh_mean = ( real( count-1 ) * rh_mean + rh_prime ) * count_inv
         ps_mean = ( real( count-1 ) * ps_mean + ps_prime ) * count_inv
         qcloud_mean = ( real( count-1 ) * qcloud_mean + qcloud_prime ) * count_inv
         qrain_mean = ( real( count-1 ) * qrain_mean + qrain_prime ) * count_inv
         qsnow_mean = ( real( count-1 ) * qsnow_mean + qsnow_prime ) * count_inv
         qice_mean = ( real( count-1 ) * qice_mean + qice_prime ) * count_inv

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   ncases = count
   write(6,'(a,1x,i4)')' Number of perturbations processed in Stage1 ',ncases

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields again, and remove time/ensemble/area mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate
   count = 0

   do while ( cdate <= edate )
      do member = 1, ne
         count = count + 1

         if ( be_method == 'NMC' ) then
            filename = trim(stage0_gsi_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(stage0_gsi_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif

         open (iunit, file = trim(filename), form='unformatted')
         read(iunit)date, ni, nj, nk, ds

         read(iunit)znu 
         read(iunit)xlat
         read(iunit)xlon
         read(iunit)mapfac_mx
         read(iunit)mapfac_my

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Remove mean.
!---------------------------------------------------------------------------------------------
!      
         write(6,'(3a,i3)')'    Removing Time mean for date ', date,' member ',member
         write(ce,'(i3.3)')member

         if( count == 1) then
         filename = trim(stage1_gsi_dir)//'/pert_files'
         open (unit=99, file = trim(filename), form='formatted')
         end if
!        Write necessary pert fields:
         filename = trim(stage1_gsi_dir)//'/pert.gsi.'//date(1:10)//'.e'//ce
         open (ounit, file = trim(filename), form='unformatted')
         write(99,'(a)')trim(filename)

         read(iunit)psi_prime
         psi_prime = psi_prime - psi_mean
         write(ounit)psi_prime

         read(iunit)chi_prime
         chi_prime = chi_prime - chi_mean
         write(ounit)chi_prime

         read(iunit)t_prime
         t_prime = t_prime - t_mean
         write(ounit)t_prime

         read(iunit)rh_prime
         rh_prime = rh_prime - rh_mean
         write(ounit)rh_prime
         read(iunit)rhm   
         write(ounit)rhm       

         read(iunit)ps_prime
         ps_prime = ps_prime - ps_mean
         write(ounit)ps_prime

         read(iunit)qcloud_prime
         qcloud_prime = qcloud_prime - qcloud_mean
         read(iunit)qcloudm 

         read(iunit)qrain_prime
         qrain_prime = qrain_prime - qrain_mean
         read(iunit)qrainm 

         read(iunit)qice_prime
         qice_prime = qice_prime - qice_mean
         read(iunit)qicem 

         read(iunit)qsnow_prime
         qsnow_prime = qsnow_prime - qsnow_mean
         read(iunit)qsnowm 
   
         sum_hydro = qcloudm + qrainm + qicem + qsnowm

         write(ounit)qcloud_prime
         write(ounit)qcloudm/sum_hydro

         write(ounit)qrain_prime
         write(ounit)qrainm/sum_hydro

         write(ounit)qice_prime
         write(ounit)qicem/sum_hydro

         write(ounit)qsnow_prime
         write(ounit)qsnowm/sum_hydro

         close(iunit)
         close(ounit)

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.
         close(99)

!        Write necessary fixed fields:
         filename = trim(stage1_gsi_dir)//'/fixed_field'
         open (ounit, file = filename, form='unformatted')
         write(ounit)date,ni, nj, nk, ncases, ds
         write(ounit)znu   
         write(ounit)xlat
         write(ounit)xlon
         write(ounit)mapfac_mx
         write(ounit)mapfac_my
         close(ounit)

         deallocate( ps_prime)
         deallocate( t_prime)
         deallocate( psi_prime)
         deallocate( chi_prime)
         deallocate( rh_prime)
         deallocate( qcloud_prime)
         deallocate( qrain_prime)
         deallocate( qice_prime)
         deallocate( qsnow_prime)

         deallocate( psi_mean )
         deallocate( chi_mean )
         deallocate( t_mean )
         deallocate( rh_mean )
         deallocate( ps_mean )
         deallocate( qcloud_mean)
         deallocate( qrain_mean)
         deallocate( qice_mean)
         deallocate( qsnow_mean)
         deallocate( qcloudm)
         deallocate( qrainm)
         deallocate( qicem)
         deallocate( qsnowm)
         deallocate( sum_hydro)

         deallocate( xlat)  
         deallocate( xlon)     
         deallocate( mapfac_m )     
         deallocate( mapfac_mx)     
         deallocate( mapfac_my)     
         deallocate( mapfac_u)
         deallocate( mapfac_v)
         deallocate( znu)


end program gen_be_stage1_gsi
