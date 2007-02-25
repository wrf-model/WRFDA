 program da_stats_rad 
!------------------------------------------------------------------------------
!  PURPOSE: perform innovation statistics for radiance data.
!
!  METHOD:  
!       1. collect obs from multi files
!       2. perform quality control 
!       3. perform innovation statistics(mean/rms/std)
!       4. perform bias corretion coefs fit
!
!  HISTORY: 2005/11/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   integer, parameter        :: iunit = 14     ! Input file unit
   integer, parameter        :: ounit = 15     ! Output file unit
   integer, parameter        :: eunit = 199    ! GSI error file unit
   integer, parameter        :: sunit = 77    ! ascii stat file unit
   integer, parameter        :: bunit = 22     ! bias correction coefs file unit
   integer, parameter        :: namelist_unit = 16
   integer, parameter        :: rtm_levs=43    ! rttov levels
   integer, parameter        :: mod_levs=50    ! model levels
   integer, parameter        :: num_procs=16   ! number of CPUs
   real, parameter           :: missing_r=-99.0 ! Missing data indicator.

   character (len=200)       :: cdate          ! Date to process
   character (len=200)       :: directory      ! Data directory.
   character (len=200)       :: filename       ! Data filename.
   character (len=200)       :: file_in        ! Data filename.
   character (len=200)       :: file_in1       ! Data filename.
   character (len=200)       :: file_out       ! Data filename.
   character (len=40)        :: platform       ! Satellite platform.
   character (len=40)        :: satid          ! Satellite id.
   character (len=40)        :: instrument     ! Instrument.
   character (len=3)         :: pc             ! Number of processors.
   character (len=200)       :: string1        ! Input string.
   character (len=200)       :: string2        ! Input string.
   character (len=200)       :: string3        ! Input string.
   character (len=200)       :: string4        ! Input string.
   character (len=200)       :: string5        ! Input string.
   character (len=200)       :: string6        ! Input string.
   character (len=200)       :: biasfile       ! bias coefs file name
   logical                   :: file_exists    ! True if file exists.
   logical                   :: lwrite_iv, lwrite_prof,lreadbias, lbiascorr

   integer                   :: p, i, n        ! Loop counters.
   integer                   :: ndomain        ! Number of obs. on processor.
   integer                   :: num_rad        ! Total observation count.
   integer                   :: nchan          ! Number of channels.
   integer                   :: nscan          ! Number of scan angles
   integer                   :: IOST

   TYPE rad_type

        character(len=20)    ::  date_char
        integer              ::  landsea_mask
        integer              ::  scanpos
        real                 ::  elv, lat, lon
        real                 ::  satzen, satazi         !  satellite angles
        real,   pointer      ::  tb(:),inv(:),invbc(:),emiss(:),error(:)
        integer,pointer      ::  qc(:)
        real,   pointer      ::  p(:), t(:), mr(:)
        real,   pointer      ::  pm(:), tm(:), qm(:), qrn(:), qcw(:),qci(:),qsn(:),qgr(:)
        real                 ::  ps,ts,t2m,mr2m,u10,v10, clwp
        real                 ::  smois, tslb, snowh, elevation,soiltyp,vegtyp,vegfra
        integer              ::  isflg

   END TYPE rad_type
   
   type(rad_type), pointer            :: rad(:)

   INTEGER                        :: j, k  ! Index dimension.
   INTEGER                        :: alloc_status(40)

   INTEGER    :: sensor_id, sensor_type, iuse, idum, iscan
   REAL       :: polar
   REAL,    pointer  :: error(:)
   INTEGER, pointer  :: ninv(:), nerr(:), ngood(:), ichan(:)
   INTEGER    :: nscat, nclwp, nsurf, nrej_chw_land, nrej_ch5_topo
   REAL,    pointer  :: vtm(:), clw(:), pf(:), dlnpf(:)
   INTEGER, pointer  :: npos(:,:), nc(:)
   REAL   , pointer  :: avescan(:,:), rmsscan(:,:), stdscan(:,:)
   REAL   , pointer  :: ave(:), rms(:), std(:)

   REAL              :: x,y,delta,si
   REAL   , pointer  :: sumx(:),sumy(:),sumxx(:),sumxy(:),a(:),b(:)
   REAL   , pointer  :: sumxscan(:,:),sumyscan(:,:), &
                        sumxxscan(:,:),sumxyscan(:,:), &
                        ascan(:,:),bscan(:,:), &
                        sumiv(:), sumiv2(:),  &
                        sumivscan(:,:), sumiv2scan(:,:)

   INTEGER, pointer  :: bias_npos(:,:), bias_nc(:)
   REAL   , pointer  :: bias_avescan(:,:), bias_rmsscan(:,:), bias_stdscan(:,:)
   REAL   , pointer  :: bias_ave(:), bias_rms(:), bias_std(:)
   REAL   , pointer  :: bias_a(:),bias_b(:),bias_ascan(:,:),bias_bscan(:,:)

   REAL  :: coszen, d0, d1, d2, ts, tb1, tb2
   REAL   , pointer  :: clwp(:)

   NAMELIST /nam_stat/ cdate, platform, satid, instrument, &
            nchan,nscan,directory,lwrite_iv,lwrite_prof, &
            lreadbias,lbiascorr,biasfile 

!
!  0.0 read namelist and get in/out file name
!---------------------------------------------------------------
   OPEN ( FILE   = 'namelist.stat', UNIT   = namelist_unit, &
          STATUS = 'OLD' , ACCESS = 'SEQUENTIAL', &
          FORM   = 'FORMATTED', ACTION = 'READ', &
          IOSTAT = IOST )

   READ  ( UNIT = namelist_unit, NML = nam_stat , IOSTAT = IOST)

   CLOSE ( UNIT = namelist_unit )

   write(unit=*, fmt='(a)') ' --------------------------------------------'
   write(unit=*, fmt='(a,a10)') ' [1.0] Doing statistics for  ', trim(cdate)
   write(unit=*, fmt='(a)') ' --------------------------------------------'

   write(6,'(a40,5a)')  ' instrument to process is ', trim(platform),'-',trim(satid),'-',trim(instrument)
   write(6,'(a40,2i8)') ' number of channels and scan angles is ', nchan, nscan
   write(6,'(a40,a)')   ' data source directory is ', trim(directory)//'/'//trim(cdate)
   write(6,'(a40,i8)')  ' Number of CPUs to collect data = ', num_procs

   filename = trim(directory) // '/' // trim(cdate) // '/' // trim(platform)// '-' // trim(satid) &
              // '-' // trim(instrument)
   file_in  = trim(filename) // '.inv_'
   file_out = trim(platform) // '-' //trim(satid)//'-'// trim(instrument)//'.inv_'//trim(cdate)

!
!  0.1 allocate space
!----------------------------------------------------------
   allocate ( ichan(nchan) )
   allocate ( error(nchan) )
   allocate ( ninv (nchan) )
   allocate ( nerr (nchan) )
   allocate ( ngood(nchan) )
   allocate ( ave  (nchan) )
   allocate ( rms  (nchan) )
   allocate ( std  (nchan) )
   allocate ( nc   (nchan) )
   allocate ( sumx (nchan) )
   allocate ( sumy (nchan) )
   allocate ( sumxx(nchan) )
   allocate ( sumxy(nchan) )
   allocate ( a    (nchan) )
   allocate ( b    (nchan) )
   allocate ( sumiv(nchan) )
   allocate ( sumiv2(nchan) )
   allocate ( sumivscan(nscan,nchan) )
   allocate ( sumiv2scan(nscan,nchan) )
   allocate ( npos (nscan,nchan) )
   allocate ( avescan(nscan,nchan) )
   allocate ( rmsscan(nscan,nchan) )
   allocate ( stdscan(nscan,nchan) )
   allocate ( sumxscan(nscan,nchan) )
   allocate ( sumyscan(nscan,nchan) )
   allocate ( sumxxscan(nscan,nchan) )
   allocate ( sumxyscan(nscan,nchan) )
   allocate ( ascan(nscan,nchan) )
   allocate ( bscan(nscan,nchan) )
   allocate ( vtm  (mod_levs) )
   allocate ( clw  (mod_levs) )
   allocate ( pf   (mod_levs+1) )
   allocate ( dlnpf(mod_levs) )
   allocate ( bias_ave  (nchan) )
   allocate ( bias_rms  (nchan) )
   allocate ( bias_std  (nchan) )
   allocate ( bias_nc   (nchan) )
   allocate ( bias_a(nchan) )
   allocate ( bias_b(nchan) )
   allocate ( bias_npos (nscan,nchan) )
   allocate ( bias_avescan(nscan,nchan) )
   allocate ( bias_rmsscan(nscan,nchan) )
   allocate ( bias_stdscan(nscan,nchan) )
   allocate ( bias_ascan(nscan,nchan) )
   allocate ( bias_bscan(nscan,nchan) )

!  
!  0.2 read GSI error file
!-----------------------------------------------------------
!   open(eunit,file=trim(platform)//'-'//trim(satid)//'-'//trim(instrument)//'.info')
!   read(eunit,'(a)') 
!   do i = 1,nchan
!     read(eunit,'(1x,5i5,2e18.10)') &
!          sensor_id, ichan(i), sensor_type, iuse, idum, error(i), polar
!   end do
!   close(eunit)

!
!  0.3 read bias correction coefs file
!--------------------------------------

 if ( lreadbias ) then
!   biasfile = trim(platform)//'-'//trim(satid)//'-'//trim(instrument)//'.bias'
   print *, 'READING bias correction file: ', trim(biasfile)
   open(bunit, file=trim(biasfile))

   read(bunit,*) ! trim(platform),'-',trim(satid),'-',trim(instrument)
   read(bunit,*) ! trim(cdate)
   read(bunit,*) ! nchan, nscan

   read(bunit,*) !  ' Number of    total obs                    = ', num_rad
   read(bunit,*) !  ' Number of rejected window chanl over land = ', nrej_chw_land
   read(bunit,*) !  ' Number of rejected ch5 over mountain      = ', nrej_ch5_topo
   read(bunit,*) !  ' Number of rejected obs by SI>3K           = ', nscat
   read(bunit,*) !  ' Number of rejected obs by CLWP>0.2mm      = ', nclwp
   read(bunit,*) !  ' Number of rejected obs by Mixture surface = ', nsurf
   read(bunit,*) !  ' Number of rejected obs by abs(IV)>20      = ', ninv(:)
   read(bunit,*) !  ' Number of rejected obs by abs(IV)>5*sigma = ', nerr(:)
   read(bunit,*) !  ' Number of     good obs                    = ', ngood(:)

   read(bunit,*) !  'scan  ', 'chan ', 'number ', 'average ', 'rms  std ', 'a ','b '

   do j = 1,nchan
     read(bunit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
                 iscan, ichan(j), bias_nc(j), bias_ave(j), &
                 bias_rms(j),bias_std(j),bias_a(j),bias_b(j)
   end do

   do k = 1,nscan
   do j = 1,nchan
      read(bunit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
         iscan, ichan(j), bias_npos(k,j),bias_avescan(k,j), &
         bias_rmsscan(k,j),bias_stdscan(k,j),bias_ascan(k,j),bias_bscan(k,j)
   end do
   end do

   close(bunit)

 end if

!  1.0 just scan file header for counting total obs number
!------------------------------------------------------------
   num_rad = 0
   do p = 1, num_procs

      if ( (p-1) < 10 ) write(pc,'(a1,i1)') '0',p - 1
      if ( (p-1) >=10 ) write(pc,'(i2)') p - 1

      file_in1 = trim(file_in) // trim(pc)
      inquire( file = file_in1, exist = file_exists )
      if ( .not. file_exists ) then
!         write(6,'(a,i3,a,i8)') ' Number of observations on processor ', p , ' = ', 0
         cycle
      end if

      open(iunit, file = file_in1, status = 'old' )

!  read file header
!
      read(iunit,'(a13,a20,i7,a32,i5,a21,20i3)')string1, string2, ndomain, &
                                        string3, nchan,string4, ichan(:) 
      read(iunit,'(a)') string5 ! Dummy text.
      read(iunit,'(a)') string6 ! Dummy text.

!      write(6,'(a,i3,a,i8)') ' Number of observations on processor ', p, ' = ', ndomain
      num_rad = num_rad + ndomain
      close(iunit)
   end do   

      if (num_rad == 0) stop

      allocate ( rad(num_rad) )
      allocate ( clwp(num_rad) )
      do i = 1,num_rad
         allocate( rad(i)%tb(nchan) )
         allocate( rad(i)%inv(nchan) )
         allocate( rad(i)%invbc(nchan) )
         allocate( rad(i)%emiss(nchan) )
         allocate( rad(i)%error(nchan) )
         allocate( rad(i)%qc(nchan) )
         allocate( rad(i)%p(rtm_levs) )
         allocate( rad(i)%t(rtm_levs) )
         allocate( rad(i)%mr(rtm_levs) )
         allocate( rad(i)%pm(mod_levs) )
         allocate( rad(i)%tm(mod_levs) )
         allocate( rad(i)%qm(mod_levs) )
         allocate( rad(i)%qrn(mod_levs) )
         allocate( rad(i)%qcw(mod_levs) )
         allocate( rad(i)%qci(mod_levs) )
         allocate( rad(i)%qsn(mod_levs) )
         allocate( rad(i)%qgr(mod_levs) )
      end do

!
!  2.0 read files
!----------------------------------------------------
      i = 0

!  2.1 open out file and write file header

      write(6,'(a,i7)') ' number of total obs = ', num_rad

    if (lwrite_iv) then
      open(ounit, file = file_out, status = 'unknown' )
      write(ounit,'(a13,a20,i7,a32,i5,a21,20i3)')string1, string2, num_rad, &
                                        string3, nchan,string4, ichan(:)
      write(ounit,'(a)')   trim(string5) ! Dummy text.
      write(ounit,'(a,a)') trim(string6), ' clwp ' ! Dummy text.
    end if
      
loop_cpu:   do p = 1, num_procs

      if ( (p-1) < 10 ) write(pc,'(a1,i1)') '0',p - 1
      if ( (p-1) >=10 ) write(pc,'(i2)') p - 1

      file_in1 = trim(file_in) // trim(pc)
      inquire( file = file_in1, exist = file_exists )
      if ( .not. file_exists ) then
         write(6,'(a,i3,a,i8)') ' Number of observations on processor ', p, ' = ', 0
         cycle
      end if

      open(iunit, file = file_in1, status = 'old' )

!  read file header
!
      read(iunit,'(a13,a20,i7,a32,i5,a21,20i3)')string1, string2, ndomain, &
                                           string3, nchan,string4,ichan(:)
      read(iunit,'(a)') string5 ! Dummy text.
      read(iunit,'(a)') string6 ! Dummy text.

      write(6,'(a,i3,a,i8)') ' Number of observations on processor ', p, ' = ', ndomain

loop_pixel:  do j =1,ndomain
    i = i + 1
    read(iunit,'(a7,i7,2x,a19,2i3,f6.0,4f8.2)') string1, n, &
                             rad(i)%date_char, &
                             rad(i)%scanpos,   &
                             rad(i)%landsea_mask, &
                             rad(i)%elv,  &
                             rad(i)%lat,  &
                             rad(i)%lon, &
                             rad(i)%satzen,    &
                             rad(i)%satazi

    read(iunit,'(a7,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') string2, n, &
                             rad(i)%t2m, &
                             rad(i)%mr2m,   &
                             rad(i)%u10, &
                             rad(i)%v10,  &
                             rad(i)%ps,  &
                             rad(i)%ts,  &
                             rad(i)%smois,  &
                             rad(i)%tslb,  &
                             rad(i)%snowh, &
                             rad(i)%isflg, &
                             rad(i)%soiltyp, &
                             rad(i)%vegtyp, &
                             rad(i)%vegfra, &
                             rad(i)%elevation, &
                             rad(i)%clwp

    read(iunit,'(a7,19f11.2)') string3, rad(i)%tb(:)
    read(iunit,'(a7,19f11.2)') string4, rad(i)%inv(:)
    read(iunit,'(a7,19f11.2)') string5, rad(i)%emiss(:)

!    rad(i)%error(:) = error(:)
!    rad(i)%qc(:)    = 1

    read(iunit,'(a)') ! dummy text
    do k=1,mod_levs
     if (k <= rtm_levs) then
        read(iunit,'(i3,f10.2,f8.2,e11.4,i3,f10.2,f8.2,3e11.4)') &
            n, &                             ! RTTOV levels
            rad(i)%p(k) , &
            rad(i)%t(k) , &
            rad(i)%mr(k), &
            n,  &                            ! WRF model levels
            rad(i)%pm(k) , &
            rad(i)%tm(k) , &
            rad(i)%qm(k) , &    
            rad(i)%qcw(k), &
            rad(i)%qrn(k)
     else
        read(iunit,'(32x,i3,f10.2,f8.2,3e11.4)') n, &
            rad(i)%pm(k) , &
            rad(i)%tm(k) , &
            rad(i)%qm(k) , &
            rad(i)%qcw(k), &
            rad(i)%qrn(k)
     end if
!        vtm (k) = (1.+0.608*0.001*rad(i)%qm(k))*rad(i)%tm(k)  ! virtual T
!        clw (k) = rad(i)%qcw(k)*rad(i)%pm(k)*100.*0.0289644/8.31451/rad(i)%tm(k) ! convert to g/m3
!        if ( rad(i)%pm(k)<100. ) clw (k) = 0.
    end do

!   compute cloud liquid water path (mm) from guess
!
!    pf(1) = 100.*rad(i)%ps
!    do k = 2,mod_levs
!      pf(k) = 100.*(rad(i)%pm(k-1)+rad(i)%pm(k))*0.5  ! full level pressure
!    end do
!    pf(mod_levs+1)= 100.*rad(i)%pm(mod_levs)*0.5 

!      rad(i)%clwp = 0.
!    do k = 1,mod_levs
!      dlnpf(k) = log(pf(k)) - log(pf(k+1))
!      clw  (k) = clw(k)*vtm(k)*287.05/9.80665*dlnpf(k)
!      rad(i)%clwp  = rad(i)%clwp + clw(k)
!    end do
!      rad(i)%clwp = 0.001*rad(i)%clwp   ! kg/m2 = mm

!  compute cloud liquid water path (mm) over sea from amsua Tb obs
!

! if (trim(instrument) == 'amsua') then  !.and. rad(i)%isflg == 0) then
!    coszen =  cos(rad(i)%satzen)
!    d0     =  8.24-(2.622-1.846*coszen)*coszen
!    d1     =  0.754
!    d2     =  -2.265
!    ts     =  rad(i)%ts
!    tb1    =  rad(i)%tb(1)
!    tb2    =  rad(i)%tb(2)
!    clwp(i)=  coszen*(d0+d1*log(ts-tb1)+d2*log(ts-tb2))
!    clwp(i)=  clwp(i) - 0.03
! end if

  end do  loop_pixel

  close(iunit)

  end do  loop_cpu

!
!  3.0 perform bias correction and QC for different instruments
!----------------------------------------------

!  3.1 Apply bias correction
!
 if (lbiascorr) then
   do i = 1,num_rad
     do j = 1,nchan
        x = rad(i)%tb(j) - rad(i)%inv(j)  ! guess Tb
        k = rad(i)%scanpos                ! scan position
        x = bias_ascan(k,j) + bias_bscan(k,j)*x ! apply scan dependent bias correction
        rad(i)%invbc(j) = rad(i)%tb(j) - x  ! bias corrected innovation
     enddo
   enddo
 else
   do i = 1,num_rad
     do j = 1,nchan
        rad(i)%invbc(j) = rad(i)%inv(j)  ! bias corrected innovation
     enddo
   enddo
 end if

!  3.2  perform QC
!
     nscat   = 0
     nsurf   = 0
     nclwp   = 0
     nrej_chw_land = 0
     nrej_ch5_topo = 0
     ninv(:) = 0
     nerr(:) = 0
     ngood(:)= 0

 if ( trim(instrument) == 'amsua' ) then
   do i = 1,num_rad
     rad(i)%qc(:)    = 1
!     rad(i)%error(:) = missing_r

!  a. reject all channels over mixture surface type
!------------------------------------------------------
     if ( rad(i)%isflg >=4 ) then
        nsurf = nsurf + 1
        rad(i)%qc(:) = -1
     end if

!  b. reject channels 1-4,15 over land/sea-ice/snow
!------------------------------------------------------
      if ( rad(i)%isflg > 0 ) then
          nrej_chw_land = nrej_chw_land + 1
          rad(i)%qc(1:4) = -1
          rad(i)%qc(15) = -1
      end if

!  c. reject channels 13,14(above top model 10mb),15
!------------------------------------------------------
!        rad%qc(13:15) = -1

!  d. check precipitation
!-----------------------------------------------------------
     if ( rad(i)%tb(1) > 0. .and. rad(i)%tb(15) > 0.) then
       si = rad(i)%tb(1) - rad(i)%tb(15)
       if ( si >= 3. ) then
         nscat = nscat + 1
         rad(i)%qc(:) = -1
       end if
     end if

     if ( rad(i)%clwp >= 0.2 ) then
       nclwp = nclwp + 1
       rad(i)%qc(:) = -1
     end if

!  e. check surface pressure over land/snow etc
!---------------------------------------------------
    if ( (rad(i)%isflg .ne. 0) .and. (rad(i)%ps < 850.) ) then
        nrej_ch5_topo = nrej_ch5_topo + 1
        rad(i)%qc(5) = -1
    end if

!  f. check innovation
!-----------------------------------------------------------
     do j = 1,nchan
       if ( abs(rad(i)%invbc(j)) > 15. ) then
            ninv(j) = ninv(j) + 1
            rad(i)%qc(j) = -1
       end if
       if ( lreadbias .and. (.not. lbiascorr) ) then
         if ( abs(rad(i)%invbc(j)) >3.*bias_rmsscan(rad(i)%scanpos,j) ) then
            nerr(j) = nerr(j) + 1
            rad(i)%qc(j) = -1
         end if
       end if
       if ( lreadbias .and. lbiascorr ) then
         if ( abs(rad(i)%invbc(j)) >3.*bias_stdscan(rad(i)%scanpos,j) ) then
            nerr(j) = nerr(j) + 1
            rad(i)%qc(j) = -1
         end if
       end if
       if ( rad(i)%qc(j) == 1 ) ngood(j) = ngood(j) + 1
     end do  
   end do   ! end loop over pixel
 end if

 if ( trim(instrument) == 'amsub' ) then
  do i=1,num_rad
      rad(i)%qc(:) = 1

!  a. reject all channels over mixture surface type
!------------------------------------------------------
     if ( rad(i)%isflg >=4 ) then
        nsurf = nsurf + 1
        rad(i)%qc(:) = -1
     end if

!  b. reject channels 1,2 over land/sea-ice/snow
!------------------------------------------------------
      if ( rad(i)%isflg > 0 ) then
          nrej_chw_land = nrej_chw_land + 1
          rad(i)%qc(1:2) = -1
      end if

!  d. check precipitation
!-----------------------------------------------------------
     if ( rad(i)%tb(1) > 0. .and. rad(i)%tb(2) > 0.) then
       si = rad(i)%tb(1)-rad(i)%tb(2)
       if ( si >= 3. ) then
         nscat = nscat + 1
         rad(i)%qc(:) = -1
       end if
     end if

     if ( rad(i)%clwp >= 0.2) then
        nclwp = nclwp + 1
        rad(i)%qc(:) = -1
     end if

!  e. check surface pressure over land/snow etc
!---------------------------------------------------
    if ( (rad(i)%isflg .ne. 0) .and. (rad(i)%ps < 800.) ) then
        nrej_ch5_topo = nrej_ch5_topo + 1
        rad(i)%qc(5) = -1
    end if

!  f. check innovation
!----------------------------------------------
     do j = 1,nchan
       if ( abs(rad(i)%invbc(j)) > 15. ) then
            ninv(j) = ninv(j) + 1
            rad(i)%qc(j) = -1
       end if
       if ( lreadbias .and. (.not. lbiascorr) ) then
         if ( abs(rad(i)%invbc(j)) >3.*bias_rmsscan(rad(i)%scanpos,j) ) then
            nerr(j) = nerr(j) + 1
            rad(i)%qc(j) = -1
         end if
       end if
       if ( lreadbias .and. lbiascorr ) then
         if ( abs(rad(i)%invbc(j)) >3.*bias_stdscan(rad(i)%scanpos,j) ) then
            nerr(j) = nerr(j) + 1
            rad(i)%qc(j) = -1
         end if
       end if
       if ( rad(i)%qc(j) == 1 ) ngood(j) = ngood(j) + 1
     end do
   end do  ! end loop over pixel
 end if

!
!  4.0 write innovation to single output file
!---------------------------------------------------
 if (lwrite_iv) then
   do i=1,num_rad
    write(ounit,'(a7,i7,2x,a19,2i3,f6.0,4f8.2)') string1, i, &
                             rad(i)%date_char, &
                             rad(i)%scanpos,   &
                             rad(i)%landsea_mask, &
                             rad(i)%elv,  &
                             rad(i)%lat,  &
                             rad(i)%lon, &
                             rad(i)%satzen,    &
                             rad(i)%satazi
    write(ounit,'(a7,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') string2, i, &
                             rad(i)%t2m, &
                             rad(i)%mr2m,   &
                             rad(i)%u10, &
                             rad(i)%v10,  &
                             rad(i)%ps,  &
                             rad(i)%ts,  &
                             rad(i)%smois,  &
                             rad(i)%tslb,  &
                             rad(i)%snowh, &
                             rad(i)%isflg, &
                             rad(i)%soiltyp, &
                             rad(i)%vegtyp, &
                             rad(i)%vegfra, &
                             rad(i)%elevation, &
                             rad(i)%clwp
    write(ounit,'(a7,19f11.2)') string3, rad(i)%tb(:)
    write(ounit,'(a7,19f11.2)') string4, rad(i)%inv(:)
    write(ounit,'(a7,19f11.2)') 'BIAS : ', rad(i)%invbc(:)
    write(ounit,'(a7,19f11.2)') string5, rad(i)%emiss(:)
!    write(ounit,'(a7,19f11.2)') ' ERR : ', rad(i)%error(:)
    write(ounit,'(a7,19i11)')   ' QC  : ', rad(i)%qc(:)

  if (lwrite_prof) then
    write(ounit,*)'RTM_level pres(mb) T(k) Q(ppmv) WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
    do k=1,mod_levs
     if (k <= rtm_levs) then
      write(ounit,'(i3,f10.2,f8.2,e11.4,i3,f10.2,f8.2,3e11.4)') &
            k, &                             ! RTTOV levels
            rad(i)%p(k) , &
            rad(i)%t(k) , &
            rad(i)%mr(k), &
            k,  &                            ! WRF model levels
            rad(i)%pm(k) , &
            rad(i)%tm(k) , &
            rad(i)%qm(k) , &
            rad(i)%qcw(k), &
            rad(i)%qrn(k)
     else
      write(ounit,'(32x,i3,f10.2,f8.2,3e11.4)') k, &
            rad(i)%pm(k) , &
            rad(i)%tm(k) , &
            rad(i)%qm(k), &
            rad(i)%qcw(k), &
            rad(i)%qrn(k)
     end if
    end do
   end if

   end do  ! end loop over pixel
    close(ounit)
 end if    ! end enif lwrite_prof

!
!  5.0 perform statistics for different scan angles
!---------------------------------------------------
   npos      (:,:) = 0
   avescan   (:,:) = 0.
   rmsscan   (:,:) = 0.
   sumivscan (:,:) = 0.
   sumiv2scan(:,:) = 0.

   do i = 1,num_rad
      do j = 1,nchan
         if (rad(i)%qc(j) == 1) then
            npos(rad(i)%scanpos,j) = npos(rad(i)%scanpos,j) + 1
            sumivscan(rad(i)%scanpos,j) =  sumivscan(rad(i)%scanpos,j) + rad(i)%invbc(j)
           sumiv2scan(rad(i)%scanpos,j) = sumiv2scan(rad(i)%scanpos,j) + rad(i)%invbc(j)*rad(i)%invbc(j)
         end if
      end do
   end do

   do j = 1,nchan
      nc(j)  = sum(npos(:,j))
      sumiv(j) = sum(sumivscan(:,j))
      sumiv2(j)= sum(sumiv2scan(:,j))
     if (nc(j) >= 2) then
      ave(j) = sumiv(j)/nc(j)
      rms(j) = sumiv2(j)/nc(j)
      std(j) = rms(j)-ave(j)*ave(j)
      rms(j) = sqrt(rms(j))
      std(j) = sqrt(std(j))
     else
      ave(j) = missing_r
      rms(j) = missing_r
      std(j) = missing_r
     end if
   end do

   do k = 1,nscan
   do j = 1,nchan
     if (npos(k,j) >= 10) then
      avescan(k,j) = sumivscan(k,j)/npos(k,j)
      rmsscan(k,j) = sumiv2scan(k,j)/npos(k,j)
      stdscan(k,j) = rmsscan(k,j)-avescan(k,j)*avescan(k,j)
      rmsscan(k,j) = sqrt(rmsscan(k,j))
      stdscan(k,j) = sqrt(stdscan(k,j))
     else
      avescan(k,j) = missing_r
      rmsscan(k,j) = missing_r
      stdscan(k,j) = missing_r
     end if
   end do
   end do
!
!  6.0 bias correction coef fitting y = a + b*x
!----------------------------------------------------
   npos(:,:)      = 0
   sumxscan(:,:)  = 0.
   sumyscan(:,:)  = 0.
   sumxxscan(:,:) = 0.
   sumxyscan(:,:) = 0.

   do i = 1,num_rad
      do j = 1,nchan
         if (rad(i)%qc(j) == 1) then
            npos(rad(i)%scanpos,j) = npos(rad(i)%scanpos,j) + 1
            x = rad(i)%tb(j) - rad(i)%invbc(j)
            y = rad(i)%tb(j)
            sumxscan(rad(i)%scanpos,j) = sumxscan(rad(i)%scanpos,j) + x
            sumyscan(rad(i)%scanpos,j) = sumyscan(rad(i)%scanpos,j) + y
            sumxxscan(rad(i)%scanpos,j)= sumxxscan(rad(i)%scanpos,j)+ x*x
            sumxyscan(rad(i)%scanpos,j)= sumxyscan(rad(i)%scanpos,j)+ x*y 
         end if
      end do
   end do
   
   do j = 1,nchan
      nc(j)   = sum(npos(:,j))
      sumx(j) = sum(sumxscan(:,j))
      sumy(j) = sum(sumyscan(:,j))
      sumxx(j)= sum(sumxxscan(:,j))
      sumxy(j)= sum(sumxyscan(:,j))
    if (nc(j) >= 10) then
      delta   = nc(j)*sumxx(j) - sumx(j)*sumx(j)
       a(j)   = (sumxx(j)*sumy (j)-sumx(j)*sumxy(j))/delta
       b(j)   = (   nc(j)*sumxy(j)-sumx(j)*sumy (j))/delta
    else
       a(j)   = missing_r
       b(j)   = missing_r
    end if
   end do

   do k = 1,nscan
   do j = 1,nchan
     if (npos(k,j) >= 10) then
          delta   = npos(k,j)*sumxxscan(k,j) - sumxscan(k,j)*sumxscan(k,j)
       ascan(k,j) = (sumxxscan(k,j)*sumyscan (k,j)-sumxscan(k,j)*sumxyscan(k,j))/delta
       bscan(k,j) = (     npos(k,j)*sumxyscan(k,j)-sumxscan(k,j)*sumyscan (k,j))/delta
     else
       ascan(k,j) = missing_r
       bscan(k,j) = missing_r
     end if
   end do
   end do 

!
!  7.0 write bias correction coef statistics to file
!----------------------------------------------------
   write(sunit,'(2x,5a)')  trim(platform),'-',trim(satid),'-',trim(instrument)
   write(sunit,'(2x,a10)') trim(cdate)
   write(sunit,'(2x,2i5)') nchan, nscan

   write(sunit,'(a,i7)')   ' Number of    total obs                    = ', num_rad
   write(sunit,'(a,i7)')   ' Number of rejected window chanl over land = ', nrej_chw_land
   write(sunit,'(a,i7)')   ' Number of rejected ch5 over mountain      = ', nrej_ch5_topo
   write(sunit,'(a,i7)')   ' Number of rejected obs by SI>3K           = ', nscat
   write(sunit,'(a,i7)')   ' Number of rejected obs by CLWP>0.2mm      = ', nclwp
   write(sunit,'(a,i7)')   ' Number of rejected obs by Mixture surface = ', nsurf
   write(sunit,'(a,19i7)') ' Number of rejected obs by abs(IV)>15K     = ', ninv(:)
   write(sunit,'(a,19i7)') ' Number of rejected obs by abs(IV)>3*sigma = ', nerr(:)
   write(sunit,'(a,19i7)') ' Number of     good obs                    = ', ngood(:)

   write(sunit,'(7a8)') 'scan  ', 'chan ', 'number ', 'average ', 'rms  std ', 'a ','b '

   do j = 1,nchan
     write(sunit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
                 0, j, nc(j), ave(j), rms(j), std(j),a(j),b(j)
   end do

   do k = 1,nscan
   do j = 1,nchan
      write(sunit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
         k, j, npos(k,j),avescan(k,j),rmsscan(k,j),stdscan(k,j),ascan(k,j),bscan(k,j)
   end do
   end do

!  8.0 write sum variable to file 
!      for accumulating total statistics later
!----------------------------------------------------
     write(88) num_rad,nscat,nclwp, &
               nsurf,ninv,nerr,ngood, &
               nrej_chw_land,nrej_ch5_topo

     write(88) nc,sumx,sumy, &
               sumxx, sumxy, &
               sumiv, sumiv2
     
     write(88) npos,sumxscan, sumyscan, &
               sumxxscan, sumxyscan, &
               sumivscan, sumiv2scan


  deallocate(rad)

  stop
 
END program da_stats_rad
