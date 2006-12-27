subroutine da_read_bufrtovs (obstype,iv,xp,infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr tovs 1b data to innovation structure
   !
   !   Method: use F90 sequential data structure to avoid reading file twice  
   !            so that da_scan_bufrtovs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   implicit none

   character(5)      ,  intent (in)  :: obstype
   character(20)     ,  intent (in)  :: infile
   type (xpose_type) ,  intent (in)  :: xp
   type (ob_type)    ,  intent (inout) :: iv

   integer          :: iost
   integer(i_kind), allocatable :: nread(:)

   integer(i_kind),parameter:: n1bhdr=14
   integer(i_kind),parameter:: maxinfo=12
   integer(i_kind),parameter:: maxchanl=100

   logical hirs2,hirs3,msu,amsua,amsub
   logical outside,inside_halo, iuse

   character(10) date
   character(8) subset,subfgn
   character(80) hdr1b

   integer(i_kind) ihh,i,j,k,ifov,idd,jdate,ireadmg,ireadsb
   integer(i_kind) iret,idate,im,iy,iyr,nchan,klatp1,klonp1
   integer(i_kind) ich1,ich2,ich8,ich10,ich15,klon1,kidsat,klat1
   integer(i_kind) nmind,iobs,jlat,jlon,itx,nk,itt,iobsout
   integer(i_kind) chan1,iskip,ichan10,ichan2,ichan1,ichan15
   integer(i_kind) ksatid,ichan8,ityp,ityp2

   real(r_kind) w00,w10,w01,w11,dx1,dy1,cosza
   real(r_kind) ch1,ch2,ch8,ch10,ch8ch10,d0,d1,d2,sval,ch15,qval
   real(r_kind) ch1flg

   real(r_kind) pred,tbmin,tbmax, tbbad
   real(r_kind) rsat,dlat,panglr,dlon,rato,sstime,rmask
   real(r_kind) dlon_earth,dlat_earth
   real(r_kind) crit1,step,start,ch8flg,sstx,each
   real(r_kind) terrain,timedif
   real(r_kind),dimension(4):: a0,a1,a2,a3
   real(r_kind),dimension(19):: rlndsea

   real(r_double),dimension(maxinfo+maxchanl):: data1b8
   real(r_double),dimension(n1bhdr):: bfr1bhdr

   !  type bright_temperature
   ! Instrument triplet, follow the convension of RTTOV 
   integer   :: platform_id, satellite_id, sensor_id

   ! pixel information
   integer   ::  year,month,day,hour,minute,second  ! observation time
   real      ::  obs_time
   integer   ::  scanline, scanpos
   real      ::  rlat, rlon                         !  lat/lon in degrees   for Anfovs
   real      ::  satzen, satazi, solzen, solazi     !  scan angles for Anfovs
   integer   ::  landsea_mask
   real      ::  srf_height
   ! channels' bright temperature
   real , allocatable ::   tb(:)                    !  bright temperatures
   !  end type bright_temperature

   type (datalink), pointer    :: head, p, current

   integer                        ::  inst, error, ifgat
   type(info_type)                ::  info
   type(model_loc_type)           ::  loc

   data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL'/
   !  data hdr1b /'FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL SLNM BEARAZ'/

   data tbmin,tbmax,tbbad / 50.0_r_kind, 550.0_r_kind, -9.99e11_r_kind /
   integer :: num_tovs_local
   integer :: lnbufr
   integer :: n

   call da_trace_entry("da_read_bufrtovs")

   ! Initialize variables

   nchan = 20
   allocate(nread(1:rtminit_nsensor))
   nread(1:rtminit_nsensor) = 0

   ! Set various variables depending on type of data to be read

   call init_constants_derived

   platform_id  = 1                 !! for NOAA series

   hirs2=     obstype == 'hirs2'
   hirs3=     obstype == 'hirs3'
   msu=       obstype == 'msu  '
   amsua=     obstype == 'amsua'
   amsub=     obstype == 'amsub'

   if (hirs2) then
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=19
      subfgn='NC021021'
      rato=1.1363987_r_kind
   else if (hirs3) then 
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=19
      subfgn='NC021025'
   else if (msu) then
      sensor_id    =  1
      step   = 9.474_r_kind
      start  = -47.37_r_kind
      nchan=4
      subfgn='NC021022'
      rato=1.1363987_r_kind
   else if (amsua) then
      sensor_id    =  3
      step   = three + one/three
      start  = -48.33_r_kind
      nchan=15
      subfgn='NC021023'
   else if (amsub)  then
      sensor_id    =  4
      step   = 1.1_r_kind
      start  = -48.95_r_kind
      nchan=5
      subfgn='NC021024'
   end if

   allocate (tb(nchan))

   ! 0.0  Open unit to satellite bufr file and read file header
   !--------------------------------------------------------------

   call da_get_unit(lnbufr)
   open(unit=lnbufr,file=trim(infile),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//TRIM(infile)/))
   end if

   call openbf(lnbufr,'IN',lnbufr)
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)
   if (subset /= subfgn) then
      write(unit=stderr,fmt=*) 'da_read_bufrtovs:  *** WARNING: ',&
         'THE FILE TITLE NOT MATCH DATA SUBSET'
      write(unit=stderr,fmt=*) '  infile=', lnbufr, infile,' subset=',&
         subset, ' subfgn=',subfgn
      write(unit=stderr,fmt=*) 'SKIP PROCESSING OF THIS 1B FILE'
      call closbf(lnbufr)
      call da_trace_exit("da_read_bufrtovs")
      return
   end if

   iy=0
   im=0
   idd=0
   ihh=0
   write(unit=date,fmt='(i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt=*) &
      'da_read_bufrtovs: bufr file date is ',iy,im,idd,ihh,infile

   ! Loop to read bufr file and assign information to a sequential structure
   !-------------------------------------------------------------------------

   allocate (head)
   !  allocate (head % rad % tb (1:nchan))
   nullify  (head % next)
   p => head

   num_tovs_local=0

   obs: do while (IREADMG(lnbufr,subset,idate)==0 .and. subset==subfgn)
      do while (IREADSB(lnbufr)==0)

         ! open(15, file='bufr.out')
         ! call ufbdmp(lnbufr, 6)
         ! close(15)


         ! 1.0     Read header record and data record

         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbrep(lnbufr,data1b8,1,nchan,iret,'TMBR')
         ! call ufbrep(lnbufr,data1b8,1,1,iret,'BEARAZ')

         ! 2.0     Extract observation location and other required information
         !     QC1:  judge if data is in the domain, read next record if not
         !------------------------------------------------------------------------
         ! rlat = bfr1bhdr(9)
         ! rlon = bfr1bhdr(10)
         ! if (rlon < 0.0) rlon = rlon+360.0

         info%lat  =  bfr1bhdr(9)  ! rlat
         info%lon  =  bfr1bhdr(10) ! rlon
         call da_ll_to_xy (info, loc, xp, outside, inside_halo)
         if (outside) cycle

         !  3.0     Extract other information
         !------------------------------------------------------
         !  3.1     Extract satellite id and scan position. 
   
         satellite_id = nint(bfr1bhdr(1))-191
         ifov = nint(bfr1bhdr(2))    

         !  QC2:  limb pixel rejected (not implemented)

         !  3.2     Extract date information.
    
         year   = bfr1bhdr(3) !year
         month  = bfr1bhdr(4) !month
         day    = bfr1bhdr(5) !day
         hour   = bfr1bhdr(6) !hour
         minute = bfr1bhdr(7) !minute
         second = bfr1bhdr(8) !second

         write(unit=info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            year, '-', month, '-', day, '_', hour, ':', minute, ':', second

         !  QC3: time consistency check with the background date

         IF (year .LE. 99) THEN
            IF (year .LT. 78) THEN
               year = year + 2000
            ELSE
               year = year + 1900
            end if
         end if

         call da_get_julian_time(year,month,day,hour,minute,obs_time)

         if (obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time)) cycle


         ! 3.3 Find wrfvar instrument index from RTTOV instrument triplet
         !     goto next data if id is not in the lists
   
         do inst = 1, rtminit_nsensor
            if (platform_id  == rtminit_platform(inst) &
               .and. satellite_id == rtminit_satid(inst)    &
               .and. sensor_id    == rtminit_sensor(inst)) then
               nread (inst) = nread (inst) + 1
               exit
            end if
         end do

         if (inst == rtminit_nsensor .and.           &
            platform_id  /= rtminit_platform(inst)  &
            .or. satellite_id /= rtminit_satid(inst) &
            .or. sensor_id /= rtminit_sensor(inst)) cycle

         ! 3.2.1 determine FGAT index ifgat
   
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                 obs_time  < time_slots(ifgat)) exit
         end do

         ! 3.4 extract satellite and solor angle
   
         panglr=(start+float(ifov-1)*step)*deg2rad
         if (hirs2 .or. msu) then
            satzen = asin(rato*sin(panglr))*rad2deg
            satzen = abs(satzen)
         else
            satzen = bfr1bhdr(11) !*deg2rad   ! local zenith angle
            satzen = abs(satzen)
            ! if (amsua .and. ifov .le. 15) satzen=-satzen
            ! if (amsub .and. ifov .le. 45) satzen=-satzen
            ! if (hirs3 .and. ifov .le. 28) satzen=-satzen
         end if
         satazi = panglr*rad2deg            ! look angle
         ! if (satazi<0.0) satazi = satazi+360.0
         solzen = bfr1bhdr(12)              ! solar zenith angle

         ! 3.5 extract surface information
   
         srf_height = bfr1bhdr(13)          ! station height
         landsea_mask = nint(bfr1bhdr(14))  ! 0:land ; 1:sea (same as RTTOV)
         ! rmask=one                          ! land
         ! if (nint(bfr1bhdr(14))==1) rmask=0.0_r_kind   ! reverse the land/sea mask in bufr
         ! landsea_mask = rmask+.001_r_kind             ! land sea mask

         info%elv = srf_height

         ! 3.6 extract channel bright temperature
   
         tb(1:nchan) = data1b8(1:nchan)
         do k = 1, nchan
            if (tb(k) < tbmin .or. tb(k) > tbmax) &
               tb(k) = missing_r
         end do

         !  4.0   assign information to sequential radiance structure
         !--------------------------------------------------------------------------
         allocate (p % rad % tb (1:nchan))
         p%rad%info             = info
         p%rad%loc              = loc
         p%rad%landsea_mask     = landsea_mask
         p%rad%scanpos          = ifov
         p%rad%satzen           = satzen
         p%rad%satazi           = satazi
         p%rad%solzen           = solzen
         p%rad%tb(1:nchan)%inv = tb(1:nchan)
         p%sensor_index         = inst
         p%ifgat                = ifgat

         num_tovs_local = num_tovs_local + 1

         !   print *, num_tovs_local,year,month,day,hour,minute,second,obs_time,ifgat

         allocate (p%next, stat=error)   ! add next data
         if (error /= 0) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot allocate radiance structure"/))
         end if

         p => p%next
         nullify (p%next)
         ! WHY
         ! if (num_tovs_local == max_num_rad) then
         !    write(unit=errmsg(1),fmt='(A,I10)') &
         !       "Reached maximum number of obs", max_num_rad
         !    call da_warning(__FILE__,__LINE__,errmsg(1:1))
         !    exit obs
         ! end if
      end do
   end do obs

   iv%total_obs = iv%total_obs + num_tovs_local
   iv%total_rad_pixel   = iv%total_rad_pixel + num_tovs_local
   iv%total_rad_channel = iv%total_rad_channel + num_tovs_local*nchan

   deallocate(tb)  
   call closbf(lnbufr)
   call da_free_unit(lnbufr)

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------
   do i = 1, iv%num_inst
      if (nread(i) < 1) cycle
      iv%instid(i)%num_rad = nread (i)
      write(unit=stdout,fmt='(a,i3,2x,a,3x,i10)') &
         ' allocating space for radiance innov structure', &
         i, iv%instid(i)%rttovid_string, iv%instid(i)%num_rad
      allocate(iv%instid(i)%rad(1:iv%instid(i)%num_rad), stat = error)
      if (error /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"mem allocation error to iv%instid(i)%rad"/))
      end if
   end do

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   nread(1:rtminit_nsensor) = 0 
   p => head
   ! do while (associated(p))

   do i = 1, num_tovs_local
      inst = p%sensor_index
      nread(inst) = nread(inst) + 1

      iv%instid(inst)%rad(nread(inst))%info = p%rad%info
      iv%instid(inst)%rad(nread(inst))%loc  = p%rad%loc
      iv%instid(inst)%rad(nread(inst))%ifgat  =  p%ifgat
      iv%instid(inst)%rad(nread(inst))%landsea_mask = p%rad%landsea_mask
      !   iv%instid(inst)%rad(nread(inst))%scanline
      iv%instid(inst)%rad(nread(inst))%scanpos      = p%rad%scanpos
      iv%instid(inst)%rad(nread(inst))%satzen       = p%rad%satzen
      iv%instid(inst)%rad(nread(inst))%satazi       = p%rad%satazi
      iv%instid(inst)%rad(nread(inst))%solzen       = p%rad%solzen
      !   iv%instid(inst)%rad(nread(inst))%solazi
      iv%instid(inst)%rad(nread(inst))%solazi = 0.0
      allocate (iv%instid(inst)%rad(nread(inst))%cloud_flag(1:nchan))
      iv%instid(inst)%rad(nread(inst))%cloud_flag(:) = 1
      allocate (iv%instid(inst)%rad(nread(inst))%tb(1:nchan), stat = error)
      if (error /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"mem allocation error to iv%instid(inst)%rad(nread(inst))%tb"/))
      end if
      iv%instid(inst)%rad(nread(inst))%tb(1:nchan)%inv = p%rad%tb(1:nchan)%inv

      current => p
      p => p%next

      ! free current data
      deallocate (current % rad % tb)
      deallocate (current)
   end do

   deallocate (nread)

   ! check if sequential structure has been freed
   !
   !  p => head
   !  do i = 1, num_tovs_local
   !   print *, i, p%rad%tb(1:nchan)%inv
   !   p => p%next
   !  end do

   close(lnbufr)
   call da_free_unit(lnbufr)

   call da_trace_exit("da_read_bufrtovs")

end subroutine da_read_bufrtovs


