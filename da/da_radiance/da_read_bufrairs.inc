subroutine da_read_bufrairs(obstype,iv,xp,lnbufr,infile)
!--------------------------------------------------------
!  PURPOSE: read in NCEP bufr eos AIRS/AMSUA/HSB 1b data 
!            to innovation structure
!
!   METHOD: use F90 sequantial data structure to avoid read file twice
!            so that da_scan_bufrairs is not necessary any more.
!            1. read file radiance data in sequential data structure
!            2. do gross QC check
!            2. assign sequential data structure to innovation structure
!                  and deallocate sequential data structure
!
!  HISTORY: 2006/01/03 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

  implicit none

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AIRSCHAN = 281	 !- 281 subset ch out of 2378 ch for AIRS
  integer(i_kind),parameter :: N_AMSUCHAN =  15  
  integer(i_kind),parameter :: N_HSBCHAN  =   4
  integer(i_kind),parameter :: N_MAXCHAN  = 350
  integer(i_kind),parameter :: maxinfo    =  12


! BUFR format for AQUASPOT 
  integer(i_kind),parameter :: N_AQUASPOT_LIST = 25
  type aquaspot_list
     sequence
     real(r_double) :: said   ! Satellite identifier
     real(r_double) :: orbn   ! Orbit number
     real(r_double) :: slnm   ! Scan line number 
     real(r_double) :: mjfc   ! Major frame count
     real(r_double) :: selv   ! Height of station
     real(r_double) :: soza   ! Solar zenith angle
     real(r_double) :: solazi ! Solar azimuth angle
     real(r_double) :: intms(2,9) ! SATELLITE INSTRUMENT TEMPERATURES
  end type aquaspot_list


! BUFR format for AIRSSPOT 
  integer(i_kind),parameter :: N_AIRSSPOT_LIST = 12
  type airsspot_list
     sequence
     real(r_double) :: siid  ! Satellite instruments
     real(r_double) :: year
     real(r_double) :: mnth
     real(r_double) :: days
     real(r_double) :: hour
     real(r_double) :: minu
     real(r_double) :: seco
     real(r_double) :: clath ! Latitude (high accuracy)
     real(r_double) :: clonh ! Longitude (high accuracy)
     real(r_double) :: saza  ! Satellite zenith angle 
     real(r_double) :: bearaz ! Bearing or azimuth
     real(r_double) :: fovn  ! Field of view number
  end type airsspot_list


! BUFR format for AIRSCHAN 
  integer(i_kind),parameter :: N_AIRSCHAN_LIST = 4
  type airschan_list
     sequence
     real(r_double) :: chnm    ! Channel number
     real(r_double) :: logrcw  ! Log-10 of (Temperature-radiance central wavenumber
     real(r_double) :: acqf    ! Channel quality flags for ATOVS
     real(r_double) :: tmbrst  ! Brightness temperature
  end type airschan_list
  

! Input/Output variables
  character(9)     ,  intent (in)  :: obstype
  character(100)    ,  intent (in)  :: infile
  integer           ,  intent (in)  :: lnbufr
  type (xpose_type) ,  intent (in)  :: xp
  type (ob_type)    ,intent (inout) :: iv


! BUFR talble file sequencial number
  character(len=512)  :: table_file


! Variables for BUFR IO    
  type(aquaspot_list) :: aquaspot
  type(airsspot_list) :: airsspot
  type(airschan_list) :: airschan(N_MAXCHAN)
  
  real(r_kind)      :: step, start
  real(r_kind)      :: airdata(N_AIRSCHAN+maxinfo)
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=8)  :: spotname
  character(len=8)  :: channame
  integer(i_kind)   :: nchanl,nchanlr
  integer(i_kind)   :: iret


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(6)
  character(len=10) :: date
  integer(i_kind)   :: nmind
  integer(i_kind)   :: iy, im, idd, ihh


! Other work variables
  integer(i_kind)  :: nreal, nobs, ityp,ityp2
  integer(i_kind)  :: itx, k, itt, iobsout
  integer(i_kind),dimension(19)::icount
  real(r_kind)     :: rlat, rlon, dx, dy, dx1, dy1, sstx, dlon, dlat
  real(r_kind)     :: sza, timedif, pred, crit1
  integer(i_kind)  :: klat1, klon1, klatp1, klonp1
  integer(i_kind)  :: ifov,size
  integer(i_kind)  :: inst,platform_id,satellite_id,sensor_id
  logical          :: iflag,outside,inside_halo
  integer(i_kind)           :: i, l, n, error
  INTEGER          :: iost
  real(r_kind),allocatable,dimension(:,:):: airdata_all


! Set standard parameters
  real(r_kind)     :: POINT001 =   0.001_r_kind
  real(r_kind)     :: POINT01  =   0.01_r_kind
  real(r_kind)     :: TEN      =  10._r_kind
  real(r_kind)     :: R45      =  45._r_kind
  real(r_kind)     :: R60      =  60._r_kind
  real(r_kind)     :: R90      =  90._r_kind
  real(r_kind)     :: R180     = 180._r_kind
  real(r_kind)     :: R360     = 360._r_kind

  logical           :: airs, eos_amsua, hsb, airstab
  type(info_type)                ::  info
  type(model_loc_type)           ::  loc
  type (datalink), pointer    :: head, p, current

!  0.0  Initialize variables
!-----------------------------------
  platform_id  = 9   ! eos series
  satellite_id = 2   ! eos-2
  nreal  = maxinfo
  nobs   = 0
  airs=      obstype == 'airs     '
  eos_amsua= obstype == 'eos_amsua'
  hsb=       obstype == 'hsb      '

  icount=0
  if(airs)then
     sensor_id = 11
     step   = 1.1_r_kind
     start = -48.9_r_kind
     senname = 'AIRS'
     nchanl  = N_AIRSCHAN
     nchanlr = N_AIRSCHAN
  else if(eos_amsua)then
     sensor_id = 3
     step   = three + one/three
     start  = -48.33_r_kind
     senname = 'AMSU'
     nchanl  = N_AMSUCHAN
     nchanlr = N_AMSUCHAN
  else if(hsb)then
     sensor_id = 12
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     senname = 'HSB'
     nchanl  = N_HSBCHAN
     nchanlr = N_HSBCHAN+1
  endif
  spotname = trim(senname)//'SPOT'
  channame = trim(senname)//'CHAN'
 
      do inst = 1, rtminit_nsensor
        if (    platform_id  == rtminit_platform(inst) &
          .and. satellite_id == rtminit_satid(inst)    &
          .and. sensor_id    == rtminit_sensor(inst)    ) then
            exit
        end if
      end do

      if ( inst == rtminit_nsensor .and.           &
           platform_id  /= rtminit_platform(inst)  &
          .or. satellite_id /= rtminit_satid(inst) &
          .or. sensor_id /= rtminit_sensor(inst)  ) return

!    1.0  Open BUFR table and BUFR file
!--------------------------------------------------------------
  table_file = 'gmao_airs_bufr.tbl'      ! make table file name
  inquire(file=table_file,exist=airstab)
  if (airstab) then
      write(UNIT=stdout,FMT=*) &
        'DA_READ_BUFRAIRS:  Reading BUFR Table A file: ',trim(table_file)
      open(UNIT=airs_table_unit,file=table_file,iostat = iost)
    IF (iost /= 0) THEN
      CALL wrf_error(__FILE__,__LINE__, &
        "Cannot open file "//TRIM(table_file))
    END IF
  endif

! Open BUFR file
  open(UNIT=lnbufr,file=trim(infile),form='unformatted',iostat = iost)
  IF (iost /= 0) THEN
    CALL wrf_error(__FILE__,__LINE__, &
      "Cannot open file "//TRIM(infile))
  END IF
  if ( airstab ) then
     call openbf(lnbufr,'IN',airs_table_unit)
  else
     call openbf(lnbufr,'IN',lnbufr)
  endif
  call datelen(10)


!   2.0  Read headder
!---------------------------
  call readmg(lnbufr,subset,idate,iret)

  iy = 0
  im = 0
  idd = 0
  ihh = 0
  if( iret /= 0 ) goto 1000     ! no data?

  write(UNIT=date,FMT='( i10)') idate
  read(UNIT=date,FMT='(i4,3i2)') iy,im,idd,ihh
  write(UNIT=stdout,FMT=*) 'DA_READ_BUFRAIRS:     bufr file date is ',iy,im,idd,ihh
  
!   3.0 Loop over observations
!----------------------------

  allocate ( head )
!  allocate ( head % rad % tb (1:nchanl) )
  nullify  ( head % next )
  p => head
  size = 0

  LOOP_OBSPOINTS: do

!   3.1 Read headder
!-------------------------------
     call readsb(lnbufr,iret)

     if( iret /=0 )then
        call readmg(lnbufr,subset,idate,iret)
        if( iret /= 0 ) exit LOOP_OBSPOINTS     ! end of file
        cycle LOOP_OBSPOINTS
     end if
  
!   3.2 Read AQUASPOT
!------------------------
     call ufbseq(lnbufr,aquaspot,N_AQUASPOT_LIST,1,iret,'AQUASPOT')

!   3.3 Read AIRSSPOT or AMSUSPOT or HSBSPOT
!-------------------------------------------------
     call ufbseq(lnbufr,airsspot,N_AIRSSPOT_LIST,1,iret,spotname)

!   3.4 Read AIRSCHAN or AMSUCHAN or HSBCHAN
!-------------------------------------------
     call ufbseq(lnbufr,airschan,N_AIRSCHAN_LIST,N_MAXCHAN,iret,channame)

     if( iret /= nchanlr)then
        write(UNIT=stderr,FMT=*) &
          'DA_READ_BUFRAIRS:  ### ERROR IN READING ', channame, ' BUFR DATA:', &
             iret, ' CH DATA IS READ INSTEAD OF', nchanlr
        cycle LOOP_OBSPOINTS
     endif

!   4.0  Check observing position (lat/lon)
!      QC1:  juge if data is in the domain, 
!            read next record if not
!------------------------------------------
     if( abs(airsspot%clath) > R90  .or. &
          abs(airsspot%clonh) > R360 .or. &
          (abs(airsspot%clath) == R90 .and. airsspot%clonh /= ZERO) )then
        cycle LOOP_OBSPOINTS
     endif

!    Retrieve observing position
     if(airsspot%clonh >= R360) then
        airsspot%clonh = airsspot%clonh - R360
!     else if(airsspot%clonh < ZERO) then
!        airsspot%clonh = airsspot%clonh + R360
     endif

        info%lat  = airsspot%clath
        info%lon  = airsspot%clonh 
        call da_ll_to_xy (info, loc, xp, outside, inside_halo )
        if (outside) cycle LOOP_OBSPOINTS

!  4.1  Check obs time
!-------------------------------------
     idate5(1) = airsspot%year ! year
     idate5(2) = airsspot%mnth ! month
     idate5(3) = airsspot%days ! day
     idate5(4) = airsspot%hour ! hour
     idate5(5) = airsspot%minu ! minute
     idate5(6) = airsspot%seco ! second

     if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
          idate5(2) <    1 .or. idate5(2) >   12 .or. &
          idate5(3) <    1 .or. idate5(3) >   31 .or. &
          idate5(4) <    0 .or. idate5(4) >   24 .or. &
          idate5(5) <    0 .or. idate5(5) >   60 .or. &
          idate5(6) <    0 .or. idate5(6) >   60 ) then
        cycle LOOP_OBSPOINTS
     endif

   write(UNIT=info%date_char,FMT='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
            ':', idate5(5), ':', idate5(6)

   info%elv = 0.  !aquaspot%selv

!  4.2  Check observational info
!-------------------------------------------------------
     if( airsspot%fovn <    0._r_kind .or. airsspot%fovn > 100._r_kind .or. &
          airsspot%saza < -360._r_kind .or. airsspot%saza > 360._r_kind .or. &
          aquaspot%soza < -180._r_kind .or. aquaspot%soza > 180._r_kind )then
        write(UNIT=stderr,FMT=*) &
          'READ_AIRS:  ### ERROR IN READING ', channame, ' BUFR DATA:', &
             ' STRANGE OBS INFO(FOV,SAZA,SOZA):', airsspot%fovn, airsspot%saza, aquaspot%soza
        cycle LOOP_OBSPOINTS
     endif

!    Retrieve observing info
     ifov = int( airsspot%fovn + POINT001 )
     sza  = abs(airsspot%saza)
!     if( ((airs .or. hsb) .and. ifov <= 45) .or. &
!          ( eos_amsua     .and. ifov <= 15) )then
!        sza = - sza
!     endif

!     airdata(6) = (start + float(ifov-1)*step)  ! look angle (deg)
!     airdata(9) = ZERO                          ! surface height
!     airdata(10)= POINT001                      ! land sea mask


!   4.3 Retrieve Tb
!-----------------------
     iflag = .false.
  
     do l=1,nchanl
        airdata(l+nreal) = airschan(l)%tmbrst	         ! brightness temerature
        if( airdata(l+nreal) > 0._r_kind .and. airdata(l+nreal) < 500._r_kind )then
           iflag = .true.
        else
           airdata(l+nreal) = missing_r
        endif
     end do

     if( .not. iflag )then
       write(UNIT=stderr,FMT=*) &
         'READ_AIRS:  ### ERROR IN READING ', channame, ' BUFR DATA:', &
            ' ALL TB DATA IS MISSING'
        cycle LOOP_OBSPOINTS
     endif

     nobs=nobs + 1

!  4.0   assign information to sequential radiance structure
!--------------------------------------------------------------------------
   allocate ( p % rad % tb (1:nchanl) )
   p%rad%info             = info
   p%rad%loc              = loc
   p%rad%landsea_mask     = POINT001
   p%rad%scanline         = int(aquaspot%slnm + POINT001)
   p%rad%scanpos          = ifov
   p%rad%satzen           = sza
   p%rad%satazi           = (start + float(ifov-1)*step)  ! look angle (deg) ! airsspot%bearaz
   p%rad%solzen           = aquaspot%soza
   p%rad%solazi           = aquaspot%solazi
   p%rad%tb(1:nchanl)%inv = airdata(nreal+1:nreal+nchanl)
   p%sensor_index         = inst

   size = size + 1
   allocate ( p%next, stat=error)   ! add next data
   if (error /= 0 ) then
     CALL wrf_error(__FILE__,__LINE__, &
       "Cannot allocate radiance structure")
   end if

   p => p%next
   nullify (p%next)

  enddo LOOP_OBSPOINTS

   iv%total_obs = iv%total_obs + size
   iv%total_rad_pixel   = iv%total_rad_pixel + size
   iv%total_rad_channel = iv%total_rad_channel + size*nchanl

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------
!  do i = 1, iv%num_inst
   if ( nobs > 0 ) then
      iv%instid(inst)%num_rad = nobs
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        ' allocating space for radiance innov structure', &
                             inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      allocate( iv%instid(inst)%rad(1:iv%instid(inst)%num_rad), stat = error )
      If( error /= 0 ) Then
        CALL wrf_error(__FILE__,__LINE__, &
        "mem allocation error to iv%instid(inst)%rad")
      End if
   End If
!  end do

!   6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
  n = 0
  p => head
  do i = 1, size
!   inst = p%sensor_index
   n = n + 1

   iv%instid(inst)%rad(n)%info   =  p%rad%info
   iv%instid(inst)%rad(n)%loc    =  p%rad%loc

   iv%instid(inst)%rad(n)%landsea_mask = p%rad%landsea_mask
   iv%instid(inst)%rad(n)%scanline     = p%rad%scanline
   iv%instid(inst)%rad(n)%scanpos      = p%rad%scanpos
   iv%instid(inst)%rad(n)%satzen       = p%rad%satzen
   iv%instid(inst)%rad(n)%satazi       = p%rad%satazi
   iv%instid(inst)%rad(n)%solzen       = p%rad%solzen
   iv%instid(inst)%rad(n)%solazi       = p%rad%solazi
   allocate ( iv%instid(inst)%rad(n)%tb(1:nchanl) )
   iv%instid(inst)%rad(n)%tb(1:nchanl)%inv = p%rad%tb(1:nchanl)%inv
!   iv%instid(inst)%rad(n)%inv(1:nchanl) = 0.0
!   iv%instid(inst)%rad(n)%err(1:nchanl) = 0.0

!    write(UNIT=stdout,FMT='(4i10)') inst,n, iv%instid(inst)%rad(n)%scanline,iv%instid(inst)%rad(n)%scanpos
!     write(UNIT=stdout,FMT='(4i5)') nint(aquaspot%said), nint(airsspot%siid), nint(aquaspot%slnm),ifov
!     write(UNIT=stdout,FMT='(7f10.2)') iv%instid(inst)%rad(n)%info%lat,iv%instid(inst)%rad(n)%info%lon, &
!                         iv%instid(inst)%rad(n)%info%elv, &
!                         iv%instid(inst)%rad(n)%satzen, iv%instid(inst)%rad(n)%satazi, &
!                         iv%instid(inst)%rad(n)%solzen, iv%instid(inst)%rad(n)%solazi 
!     write(UNIT=stdout,FMT='(10f8.2)') iv%instid(inst)%rad(n)%tb(1:nchanl)%inv

   current => p
   p => p%next

! free current data
   deallocate ( current % rad % tb )
   deallocate ( current )

 end do

1000 continue
  call closbf(lnbufr)

  return

end subroutine da_read_bufrairs
