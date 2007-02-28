subroutine da_read_bufrtovs (obstype,iv,xp,infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr tovs 1b data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            so that da_scan_bufrtovs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   character(5)      ,  intent (in)  :: obstype
   character(20)     ,  intent (in)  :: infile
   type (xpose_type) ,  intent (in)  :: xp
   type (ob_type)    ,  intent (inout) :: iv

#ifdef BUFR

   integer          :: iost
   integer(i_kind), allocatable :: nread(:)

   integer(i_kind),parameter:: n1bhdr=14
   integer(i_kind),parameter:: maxinfo=12
   integer(i_kind),parameter:: maxchanl=100

   logical hirs2,hirs3,msu,amsua,amsub
   logical outside, outside_all
   integer :: inst

   character(10) date
   character(8) subset,subfgn
   character(80) hdr1b

   integer(i_kind) ihh,i,k,ifov,idd,ireadmg,ireadsb
   integer(i_kind) iret,idate,im,iy,nchan

   real(r_kind) tbmin,tbmax, tbbad
   real(r_kind) panglr,rato
   ! real(r_kind) rmask
   real(r_kind) step,start

   real(r_double),dimension(maxinfo+maxchanl):: data1b8
   real(r_double),dimension(n1bhdr):: bfr1bhdr

   !  type bright_temperature
   ! Instrument triplet, follow the convension of RTTOV 
   integer   :: platform_id, satellite_id, sensor_id

   ! pixel information
   integer   ::  year,month,day,hour,minute,second  ! observation time
   real      ::  obs_time
   ! real      ::  rlat, rlon                         !  lat/lon in degrees   for Anfovs
   real      ::  satzen, satazi, solzen     !  scan angles for Anfovs
   integer   ::  landsea_mask
   real      ::  srf_height
   ! channels' bright temperature
   real , allocatable ::   tb_inv(:)                    !  bright temperatures
   !  end type bright_temperature

   type (datalink_type), pointer    :: head, p, current

   integer                        ::  ifgat
   type(info_type)                ::  info
   type(model_loc_type)           ::  loc

   data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL'/
   !  data hdr1b /'FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL SLNM BEARAZ'/

   data tbmin,tbmax,tbbad / 50.0_r_kind, 550.0_r_kind, -9.99e11_r_kind /
   integer :: num_tovs_local, num_tovs_file, num_tovs_global, num_tovs_used
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
      nchan=nchan_hirs2
      subfgn='NC021021'
      rato=1.1363987_r_kind
   else if (hirs3) then 
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=nchan_hirs3
      subfgn='NC021025'
   else if (msu) then
      sensor_id    =  1
      step   = 9.474_r_kind
      start  = -47.37_r_kind
      nchan=nchan_amsua
      subfgn='NC021022'
      rato=1.1363987_r_kind
   else if (amsua) then
      sensor_id    =  3
      step   = three + one/three
      start  = -48.33_r_kind
      nchan=nchan_amsua
      subfgn='NC021023'
   else if (amsub)  then
      sensor_id    =  4
      step   = 1.1_r_kind
      start  = -48.95_r_kind
      nchan=5
      subfgn='NC021024'
   end if

   allocate (tb_inv(nchan))

   ! 0.0  Open unit to satellite bufr file and read file header
   !--------------------------------------------------------------

   call da_get_unit(lnbufr)
   open(unit=lnbufr,file=trim(infile),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//trim(infile)/))
   end if

   call openbf(lnbufr,'IN',lnbufr)
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)
   if (subset /= subfgn) then
      message(1)='The file title does not match the data subset'
      write(unit=message(2),fmt=*) &
         'infile=', lnbufr, infile,' subset=', subset, ' subfgn=',subfgn
      call da_error(__FILE__,__LINE__,message(1:2))
   end if

   iy=0
   im=0
   idd=0
   ihh=0
   write(unit=date,fmt='( i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt=*) &
      'Bufr file date is ',iy,im,idd,ihh,infile

   ! Loop to read bufr file and assign information to a sequential structure
   !-------------------------------------------------------------------------

   allocate (head)
   !  allocate ( head % tb_inv (1:nchan) )
   nullify  ( head % next )
   p => head

   num_tovs_local  = 0
   num_tovs_file   = 0
   num_tovs_global = 0
   num_tovs_used = 0

   if (tovs_start > 1) then
      write (unit=stdout,fmt='(A,I6)') "   Skipping tovs obs before", tovs_start
   end if

   obs: do while (ireadmg(lnbufr,subset,idate)==0 .and. subset==subfgn)
      do while (ireadsb(lnbufr)==0)

         ! 1.0     Read header record and data record

         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbrep(lnbufr,data1b8,1,nchan,iret,'TMBR')
         ! call ufbrep(lnbufr,data1b8,1,1,iret,'BEARAZ')

         ! check if observation outside range

         num_tovs_file = num_tovs_file + 1

         ! 2.0     Extract observation location and other required information
         !     QC1:  judge if data is in the domain, read next record if not
         !------------------------------------------------------------------------
         ! rlat = bfr1bhdr(bufr_lat)
         ! rlon = bfr1bhdr(bufr_lat)
         ! if (rlon < 0.0) rlon = rlon+360.0

         info%lat  =  bfr1bhdr(bufr_lat)
         info%lon  =  bfr1bhdr(bufr_lon)
         call da_ll_to_xy (info, loc, xp, outside, outside_all)

         if (outside_all) cycle

         !  3.0     Extract other information
         !------------------------------------------------------
         !  3.1     Extract satellite id and scan position. 
   
         satellite_id = nint(bfr1bhdr(bufr_satellite_id))-191
         ifov = nint(bfr1bhdr(bufr_ifov))    

         !  QC2:  limb pixel rejected (not implemented)

         !  3.2     Extract date information.
    
         year   = bfr1bhdr(bufr_year)   
         month  = bfr1bhdr(bufr_month)  
         day    = bfr1bhdr(bufr_day)    
         hour   = bfr1bhdr(bufr_hour)   
         minute = bfr1bhdr(bufr_minute) 
         second = bfr1bhdr(bufr_second) 

         write(unit=info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            year, '-', month, '-', day, '_', hour, ':', minute, ':', second

         !  QC3: time consistency check with the background date

         if (year <= 99) then
            if (year < 78) then
               year = year + 2000
            else
               year = year + 1900
            end if
         end if

         call da_get_julian_time(year,month,day,hour,minute,obs_time)

         if (obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time)) cycle

         ! 3.2.1 determine FGAT index ifgat
   
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat)) exit
         end do

         ! 3.3 Find wrfvar instrument index from RTTOV instrument triplet
         !     go to next data if id is not in the lists

         inst = 0
         do i = 1, rtminit_nsensor
            if (platform_id  == rtminit_platform(i) &
               .and. satellite_id == rtminit_satid(i)    &
               .and. sensor_id    == rtminit_sensor(i)) then
               inst = i
               exit
            end if
         end do
         if (inst == 0) cycle

         num_tovs_global = num_tovs_global + 1

         if (num_tovs_global < tovs_start) then
            cycle
         end if

         if (num_tovs_global > tovs_end) then
            write (unit=stdout,fmt='(A,I6)') "   Skipping radiance obs after", tovs_end
            exit obs
         end if

         num_tovs_used = num_tovs_used + 1

         if (num_tovs_used > max_tovs_input) then
            write(unit=message(1),fmt='(A,I10,A)') &
               "Max number of tovs",max_tovs_input," reached"
            call da_warning(__FILE__,__LINE__,message(1:1))
            num_tovs_used = num_tovs_used-1
            exit obs
         end if
         if (outside) cycle ! No good for this PE

         num_tovs_local = num_tovs_local + 1
         nread(inst) = nread(inst) + 1

         ! 3.4 extract satellite and solar angle
   
         panglr=(start+float(ifov-1)*step)*deg2rad
         if (hirs2 .or. msu) then
            satzen = asin(rato*sin(panglr))*rad2deg
            satzen = abs(satzen)
         else
            satzen = bfr1bhdr(bufr_satzen) !*deg2rad   ! local zenith angle
            satzen = abs(satzen)
            ! if (amsua .and. ifov .le. 15) satzen=-satzen
            ! if (amsub .and. ifov .le. 45) satzen=-satzen
            ! if (hirs3 .and. ifov .le. 28) satzen=-satzen
         end if
         satazi = panglr*rad2deg            ! look angle
         ! if (satazi<0.0) satazi = satazi+360.0
         solzen = bfr1bhdr(bufr_solzen)              ! solar zenith angle

         ! 3.5 extract surface information
   
         srf_height = bfr1bhdr(bufr_station_height)          ! station height
         landsea_mask = nint(bfr1bhdr(bufr_landsea_mask))  ! 0:land ; 1:sea (same as RTTOV)
         ! rmask=one                          ! land
         ! if (nint(bfr1bhdr(bufr_landsea_mask))==1) rmask=0.0_r_kind   ! reverse the land/sea mask in bufr
         ! landsea_mask = rmask+.001_r_kind             ! land sea mask

         info%elv = srf_height

         ! 3.6 extract channel bright temperature
   
         tb_inv(1:nchan) = data1b8(1:nchan)
         do k = 1, nchan
            if ( tb_inv(k) < tbmin .or. tb_inv(k) > tbmax) &
               tb_inv(k) = missing_r
         end do

         !  4.0   assign information to sequential radiance structure
         !--------------------------------------------------------------------------
         allocate (p % tb_inv (1:nchan))
         p%info             = info
         p%loc              = loc
         p%landsea_mask     = landsea_mask
         p%scanpos          = ifov
         p%satzen           = satzen
         p%satazi           = satazi
         p%solzen           = solzen
         p%tb_inv(1:nchan)  = tb_inv(1:nchan)
         p%sensor_index     = inst
         p%ifgat            = ifgat

         allocate (p%next)   ! add next data

         p => p%next
         nullify (p%next)
      end do
   end do obs

   iv%total_obs         = iv%total_obs + num_tovs_used
   iv%total_rad_pixel   = iv%total_rad_pixel + num_tovs_used
   iv%total_rad_channel = iv%total_rad_channel + num_tovs_used*nchan

   deallocate(tb_inv)  
   call closbf(lnbufr)
   call da_free_unit(lnbufr)

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
   
   do i = 1, iv%num_inst
      if (nread(i) < 1) cycle
      iv%instid(i)%num_rad = nread(i)
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         i, iv%instid(i)%rttovid_string, iv%instid(i)%num_rad
      if (rtm_option == rtm_option_crtm) then
         iv%instid(i)%nlevels = xp%kme-xp%kms+1
         iv%instid(i)%nchannels=nchan
      end if

#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         call rttov_setupchan(1, nchan, coefs(i), &   ! in
            iv%instid(i)%nfrequencies,iv%instid(i)%nchannels, &
            iv%instid(i)%nbtout)      ! out
      end if
#endif

      allocate (iv%instid(i)%info(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_i(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_j(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_k(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dz(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dzm(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%zk(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%t(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%mr(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dx(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dy(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dxm(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%loc_dym(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tm(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qm(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qrn(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qcw(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qci(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qsn(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qgr(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%pm(xp%kms:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%pf(0:xp%kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%u10(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%v10(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%t2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%q2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%mr2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%psfc(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%ts(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%smois(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tslb(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%snowh(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%isflg(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%landsea_mask(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%elevation(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%vegfra(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%vegtyp(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%soiltyp(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%clwp(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%ps(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_xb(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_qc(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_inv(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_error(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%emiss(iv%instid(i)%nchannels,iv%instid(i)%num_rad))
      allocate(iv%instid(i)%scanpos(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%scanline(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%ifgat(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%cloud_flag(nchan,iv%instid(i)%num_rad))
      allocate(iv%instid(i)%satzen(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%satazi(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%solzen(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%solazi(iv%instid(i)%num_rad))
      allocate(iv%instid(i)%proc_domain(iv%instid(i)%num_rad))
      if (rtm_option == rtm_option_crtm) then
        allocate(iv%instid(i)%water_coverage(iv%instid(i)%num_rad))
        allocate(iv%instid(i)%land_coverage(iv%instid(i)%num_rad))
        allocate(iv%instid(i)%ice_coverage(iv%instid(i)%num_rad))
        allocate(iv%instid(i)%snow_coverage(iv%instid(i)%num_rad))
        allocate(iv%instid(i)%ps_jacobian(nchan,iv%instid(i)%num_rad))
        allocate(iv%instid(i)%t_jacobian(nchan,xp%kte,iv%instid(i)%num_rad))
        allocate(iv%instid(i)%q_jacobian(nchan,xp%kte,iv%instid(i)%num_rad))
      end if
   end do
   
   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   nread(1:rtminit_nsensor) = 0 
   p => head
   ! do while ( associated(p) )

   do n = 1, num_tovs_local
      i = p%sensor_index
      nread(i) = nread(i) + 1
      iv%instid(i)%info(nread(i))         = p%info
      iv%instid(i)%loc(nread(i))          = p%loc
      iv%instid(i)%loc_i(nread(i))        = p%loc%i
      iv%instid(i)%loc_j(nread(i))        = p%loc%j
      iv%instid(i)%loc_k(:,nread(i))      = 0
      iv%instid(i)%loc_dx(nread(i))       = p%loc%dx
      iv%instid(i)%loc_dy(nread(i))       = p%loc%dy
      iv%instid(i)%loc_dz(:,nread(i))     = 0.0
      iv%instid(i)%loc_dxm(nread(i))      = p%loc%dxm
      iv%instid(i)%loc_dym(nread(i))      = p%loc%dym
      iv%instid(i)%loc_dzm(:,nread(i))    = 0.0
      ! z done in da_get_innov_vector_rad
      iv%instid(i)%t(:,nread(i))          = 0.0
      iv%instid(i)%mr(:,nread(i))         = 0.0
      iv%instid(i)%tm(:,nread(i))         = 0.0
      iv%instid(i)%qm(:,nread(i))         = 0.0
      iv%instid(i)%qrn(:,nread(i))        = 0.0
      iv%instid(i)%qcw(:,nread(i))        = 0.0
      iv%instid(i)%qci(:,nread(i))        = 0.0
      iv%instid(i)%qsn(:,nread(i))        = 0.0
      iv%instid(i)%qgr(:,nread(i))        = 0.0
      iv%instid(i)%pm(:,nread(i))         = 0.0
      iv%instid(i)%u10(nread(i))          = 0.0
      iv%instid(i)%v10(nread(i))          = 0.0
      iv%instid(i)%t2m(nread(i))          = 0.0
      iv%instid(i)%q2m(nread(i))          = 0.0
      iv%instid(i)%mr2m(nread(i))         = 0.0
      iv%instid(i)%psfc(nread(i))         = 0.0
      iv%instid(i)%ts(nread(i))           = 0.0
      iv%instid(i)%smois(nread(i))        = 0.0
      iv%instid(i)%tslb(nread(i))         = 0.0
      iv%instid(i)%snowh(nread(i))        = 0.0
      iv%instid(i)%isflg(nread(i))        = 0
      iv%instid(i)%soiltyp(nread(i))      = 0.0
      iv%instid(i)%landsea_mask(nread(i)) = p%landsea_mask
      iv%instid(i)%elevation(nread(i))    = 0.0
      iv%instid(i)%vegfra(nread(i))       = 0.0
      iv%instid(i)%vegtyp(nread(i))       = 0.0
      iv%instid(i)%clwp(nread(i))         = 0.0
      iv%instid(i)%ps(nread(i))           = 0.0
      iv%instid(i)%tb_xb(:,nread(i))      = 0
      iv%instid(i)%tb_inv(:,nread(i))     = p%tb_inv(:) 
      iv%instid(i)%tb_qc(:,nread(i))      = 0
      iv%instid(i)%tb_error(:,nread(i))   = 500.
      iv%instid(i)%emiss(:,nread(i))      = 0.0
      iv%instid(i)%scanpos(nread(i))      = p%scanpos 
      ! iv%instid(i)%scanline(nread(i)    = p%scanline
      iv%instid(i)%scanline(nread(i))     = 0
      iv%instid(i)%ifgat(nread(i))        = p%ifgat
      iv%instid(i)%cloud_flag(:,nread(i)) = 1        ! no cloud
      iv%instid(i)%satzen(nread(i))       = p%satzen  
      iv%instid(i)%satazi(nread(i))       = p%satazi  
      iv%instid(i)%solzen(nread(i))       = p%solzen  
      ! iv%instid(i)%solazi(nread(i))     = p%solazi  
      iv%instid(i)%solazi(nread(i))       = 0.0
      iv%instid(i)%proc_domain(nread(i))  = .false.

      current => p
      p => p%next

      ! free current data
      deallocate ( current % tb_inv )
      deallocate ( current )
   end do

   deallocate ( p )

   deallocate (nread)

   ! check if sequential structure has been freed
   !
   ! p => head
   ! do i = 1, num_rad_used
   !    write (unit=stdout,fmt=*)  i, p%tb_inv(1:nchan)
   !    p => p%next
   ! end do

   close(lnbufr)
   call da_free_unit(lnbufr)

   call da_trace_exit("da_read_bufrtovs")
#endif

end subroutine da_read_bufrtovs


