subroutine da_scan_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename

#ifdef BUFR

   type (multi_level_type)      :: platform
   logical                      :: outside
   logical                      :: outside_all

   character(len=40) :: hdstr
   character(len=10) :: date
   character(len=8)  :: subset
   real              :: hdr(7)

   integer           :: iost, ndup, n, report
   integer           :: year,month, day,hour
   integer           :: iret,idate, kx, nrecs,miscd, t29
   integer           :: iunit

   if (trace_use) call da_trace_entry("da_scan_obs_bufr")

   ! open FILE
   ! ---------

   call da_get_unit(iunit)
   open(unit   = iunit, FILE  = trim(filename), &
      iostat =  iost, form = 'unformatted', STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1), fmt='(A,I3,A,A)') &
         'error in obs input file unit ',iunit, &
         'obs filename:', trim(filename)
      message(2)="bufr gts observations cannot be found or cannot be opened"
      call da_warning(__FILE__,__LINE__,message(1:2))
      call da_free_unit(iunit)
      return
   end if

   !--------------------------------------------------------------------------
   ! Initialise the obs-counter
   !--------------------------------------------------------------------------

   hdstr='SID XOB YOB DHR TYP ELV T29     '
   nrecs = 0
   miscd = 0

   !-------------------------------- 
   ! open bufr file then check date        
   !--------------------------------

   call datelen(10)

   call openbf(iunit,'in',iunit)
   call readmg(iunit,subset,idate,iret)

   if (iret/=0) then
      message(1) = "No bufr observations"
      write(unit=message(2), fmt='(a, i4)') &
         'return code from readmg:', iret, &
         'Reach the end of obs unit: ', iunit
      call da_error(__FILE__,__LINE__,message(1:2))
   end if

   write(unit=date,fmt='(i10)') idate
   read (unit=date,fmt='(i4,3i2)') year,month, day,hour
   write(unit=message(1),fmt=*) 'Bufr file date is ',year,month, day,hour
   call da_message(message(1:1))

   report = 0 ! report number in file

   reports: do
      report = report+1

      call readsb(iunit,iret)

      if (iret/=0) then
         call readmg(iunit,subset,idate,iret)
         if (iret/=0) then
            write (unit=message(1), fmt='(a, i4)') &
               'return code from readmg:', iret, &
               'Reach the end of obs unit: ', iunit
            call da_warning(__FILE__,__LINE__,message(1:1))
            exit reports
         end if

         cycle reports
      end if

      nrecs=nrecs+1
      call ufbint(iunit,hdr,7,1,iret,hdstr)

      platform  % info % name(1:8) = subset
      platform  % info % id        = hdstr(1:5)

      if (hdr(2) > 180.0) hdr(2)=hdr(2)-360.0

      ! Put a check on Lat 
      hdr(3) = MAX(hdr(3), -89.95)     
      hdr(3) = Min(hdr(3),  89.95)     
      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      if (platform%info%lon == 180.0 ) platform%info%lon =-180.000
      ! Fix funny wind direction at Poles
      if (platform%info%lat <= -89.95 .or. platform%info%lat >= 89.95) then
         platform%info%lon = 0.0
      end if

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) cycle
      if (report > report_end) exit  

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (outside_all) cycle reports

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      if (kx .eq. 183) then          ! reset kx based on t29
         if (t29 .eq. 511) kx = 181
         if (t29 .eq. 512) kx = 187
         if (t29 .eq. 522) kx = 180
         if (t29 .eq. 523) kx = 180
         if (t29 .eq. 531) kx = 180
         if (t29 .eq. 561) kx = 180
         if (t29 .eq. 562) kx = 180
      end if

      ! if ((kx >= 160) .and. (kx <= 179)) then    ! bypass satellite data
      !    if (t29 .eq. 61 .or. t29 .eq. 63 .or. t29 .ge. 571) then
      !      cycle reports
      !    end if

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2

      if (test_wrfvar) ndup = 1

      do n = 1, ndup
         select case(t29)

         case (11, 12, 13, 22, 23, 31)
            select case(kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs .or. iv%info(sound)%ntotal == max_sound_input) cycle reports
               if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
               if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
               if (outside) cycle reports
               iv%info(sound)%nlocal = iv%info(sound)%nlocal + 1
               iv%info(sonde_sfc)%nlocal = iv%info(sonde_sfc)%nlocal + 1
               platform%loc%obs_global_index  = iv%info(sound)%ntotal

            case (221) ;                   ! Pilot 
               if (.not.use_pilotobs .or. iv%info(pilot)%ntotal == max_pilot_input) cycle reports
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) cycle reports
               iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               platform%loc%obs_global_index  = iv%info(pilot)%ntotal
            end select

         case (41)
         ! case (130:131, 133, 230:231, 233) ;  ! Airep
            if (.not.use_airepobs .or. iv%info(airep)%ntotal == max_airep_input) cycle reports
            if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
            if (outside) cycle reports
            iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(airep)%ntotal

         ! case (180, 182, 280, 282) ;          ! Ships
         case (522, 523) ;      
            if (.not.use_shipsobs .or. iv%info(ships)%ntotal == max_ships_input) cycle reports
            if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
            if (outside) cycle reports
            iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(ships)%ntotal

         case (531, 561, 562) ;                ! Buoy
            if (.not.use_synopobs .or. iv%info(synop)%ntotal == max_synop_input) cycle reports
            if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
            if (outside) cycle reports
            iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(synop)%ntotal

         case (511)
         ! case (181, 281) ;            ! Synop
            if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
            if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
            if (outside) cycle reports
            iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(metar)%ntotal

         case (512)
         ! case (187 , 287) ;                   ! Metar
            if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
            if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
            if (outside) cycle reports
            iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(metar)%ntotal

         case (63)
         ! case (242:246, 252:253, 255);  ! Geo. AMVs
            if (.not.use_geoamvobs .or. iv%info(geoamv)%ntotal == max_geoamv_input) cycle reports
            if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
            if (outside) cycle reports
            iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(geoamv)%ntotal

         case (71, 72)        ;         !  Profiler & VADWND - NEXRAD winds
            if (.not.use_polaramvobs .or. iv%info(polaramv)%ntotal == max_polaramv_input) cycle reports
            if (n==1) iv%info(polaramv)%ntotal = iv%info(polaramv)%ntotal + 1
            if (outside) cycle reports
            iv%info(polaramv)%nlocal = iv%info(polaramv)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(polaramv)%ntotal

         case (582)    ;                 !  Quikscat
            if (.not.use_qscatobs .or. iv%info(qscat)%ntotal == max_qscat_input) cycle reports
            if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
            if (outside) cycle reports
            iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(qscat)%ntotal

         case (583)    ;                 !  GPS IPW      
            if (.not.use_gpspwobs .or. iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
            if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
            if (outside) cycle reports
            iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(gpspw)%ntotal

         case (584)    ;                 !  GPS REF 
            if (.not.use_gpsrefobs .or. iv%info(gpsref)%ntotal == max_gpsref_input) cycle reports
            if (n==1) iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
            if (outside) cycle reports
            iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1
            platform%loc%obs_global_index  = iv%info(gpsref)%ntotal

         case default ;
            select case (kx)
            case (111 , 210)    ;           !  Tropical Cyclone Bogus
               !  Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs) cycle reports
            iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1

            case default 
               miscd = miscd + 1
               message(1)='unsaved obs found:'
               write(unit=message(2), fmt='(2a)') &
                 'platform%info%platform=', platform%info%platform
               call da_warning(__FILE__,__LINE__,message(1:2))
            end select
         end select
      end do        !  loop over duplicate
   end do reports

   write (unit=message(1),fmt=*) 'total bufr records =',nrecs
   write (unit=message(3),fmt=*) 'number unhandled   =',miscd 
   call da_message(message(1:3))

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_bufr")
#endif

end subroutine da_scan_obs_bufr


