subroutine da_scan_bufr_obs (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in) :: xp           ! Domain decomposition vars.
   type (ob_type), intent(inout) :: ob

   character(*),   intent (in)   :: filename

   type (multi_level_type)      :: platform
   logical                      :: outside, cycle_report
   real                         :: height_error
   integer                      :: total_obs, &
                                   num_sound, &
                                   num_synop, &
                                   num_pilot, &
                                   num_geoamv, &
                                   num_polaramv, &
                                   num_satem, &
                                   num_airep, &
                                   num_metar, &
                                   num_ships, &
                                   num_gpspw, &
                                   num_gpsref, &
                                   num_ssmi_retrieval, &
                                   num_ssmi_tb      , &
                                   num_ssmt1, &
                                   num_ssmt2, &
                                   num_pseudo, num_qscat, &
                                   num_profiler, num_buoy, num_bogus
   character(len=40) :: hdstr
   character(len=10) :: date
   character(len=8 ) :: subset
   real,dimension(7) :: hdr

   integer           :: iost, ndup, n, report
   integer           :: year,month, day,hour
   integer           :: iret,idate, kx, nrecs,miscd, t29
   logical           :: exist
   integer           :: iunit

   ! default value
   ob%ob_numb(ob%current_ob_time)%sound = ob%num_sound
   ob%ob_numb(ob%current_ob_time)%synop = ob%num_synop
   ob%ob_numb(ob%current_ob_time)%pilot = ob%num_pilot
   ob%ob_numb(ob%current_ob_time)%satem = ob%num_satem
   ob%ob_numb(ob%current_ob_time)%geoamv = ob%num_geoamv
   ob%ob_numb(ob%current_ob_time)%polaramv = ob%num_polaramv
   ob%ob_numb(ob%current_ob_time)%airep = ob%num_airep
   ob%ob_numb(ob%current_ob_time)%gpspw = ob%num_gpspw
   ob%ob_numb(ob%current_ob_time)%gpsref = ob%num_gpsref
   ob%ob_numb(ob%current_ob_time)%metar = ob%num_metar
   ob%ob_numb(ob%current_ob_time)%ships = ob%num_ships
   ob%ob_numb(ob%current_ob_time)%qscat = ob%num_qscat
   ob%ob_numb(ob%current_ob_time)%buoy  = ob%num_buoy
   ob%ob_numb(ob%current_ob_time)%profiler = ob%num_profiler

   ob%ob_numb(ob%current_ob_time)%ssmt1 = ob%num_ssmt1
   ob%ob_numb(ob%current_ob_time)%ssmt2 = ob%num_ssmt2
   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! open FILE
   ! ---------

   call da_get_unit(iunit)
   open(unit   = iunit, FILE  = trim(filename), &
      iostat =  iost, form = 'unformatted', STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1), fmt='(A,I3,A,A)') &
         'ERROR in OBS inPUT FILE unit ',iunit, &
         'OBS FILENAME:', trim(filename)
      message(2)="BUFR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE openED"
      call da_warning(__FILE__,__LINE__,message(1:2))
      call da_free_unit(iunit)
      return
   end if

   !--------------------------------------------------------------------------
   ! Initialise the obs-counter
   !--------------------------------------------------------------------------

   total_obs = 0
   num_sound = 0
   num_synop = 0
   num_pilot = 0
   num_geoamv = 0
   num_polaramv = 0
   num_satem = 0
   num_airep = 0
   num_metar = 0
   num_ships = 0
   num_gpspw = 0
   num_gpsref = 0
   num_ssmi_retrieval = 0
   num_ssmi_tb       = 0
   num_ssmt1 = 0
   num_ssmt2 = 0
   num_pseudo = 0
   num_qscat = 0
   num_profiler = 0
   num_buoy = 0
   num_bogus = 0

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
      message(1) = "No BUFR observations"
      write(unit=message(2), fmt='(a, i4)') &
         'return code from readmg:', iret, &
         'Reach the end of obs unit: ', iunit
      call da_error(__FILE__,__LINE__,message(1:2))
   end if

   write(unit=date,fmt='( i10)') idate
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

      if(hdr(2) > 180.) hdr(2)=hdr(2)-360.

      ! Put a check on Lat 
      hdr(3) = MAX(hdr(3), -89.95)     
      hdr(3) = Min(hdr(3),  89.95)     
      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      if (platform%info%lon == 180.0   ) platform%info%lon =-180.000
      ! Fix funny wind direction at Poles
      if (platform%info%lat <= -89.95 .or. platform%info%lat >= 89.95 ) then
         platform%info%lon = 0.0
      end if

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) then
         cycle
      end if

      if (report > report_end) then
         exit
      end if

      call da_ll_to_xy (platform%info, platform%loc, &
                       xp, outside)

      if (outside) cycle reports

      total_obs = total_obs + 1

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
      if (global .and. &
         (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      if (Testing_WRFVAR ) ndup = 1

      do n = 1, ndup

         select case(t29)

         case (11, 12, 13, 22, 23, 31)
            select case(kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_SoundObs) cycle reports
               num_sound = num_sound + 1

            case (221) ;                   ! Pilot 
               if (.not.Use_PilotObs) cycle reports
               num_pilot = num_pilot + 1
            end select

         case (41)
         ! case (130:131, 133, 230:231, 233) ;  ! Airep
            if (.not.use_AirepObs) cycle reports
            num_airep = num_airep + 1

         ! case (180, 182, 280, 282) ;          ! Ships and buoys
         case (522, 523) ;
            if (.not.use_ShipsObs) cycle reports
            num_ships = num_ships + 1

         case (531, 561, 562) ;                ! Buoy
            if (.not.use_BuoyObs) cycle reports
            num_buoy  = num_buoy  + 1

         case (511)
         ! case (181, 281) ;            ! Synop
            if (.not.use_SynopObs) cycle reports
            num_synop = num_synop + 1

         case (512)
         ! case (187 , 287) ;                   ! Metar
            if (.not.use_MetarObs) cycle reports
            num_metar = num_metar + 1

         case (63)
         ! case (242:246, 252:253, 255);  ! Geo. CMVs
            if (.not.use_GeoAMVObs) cycle reports
            num_geoamv = num_geoamv + 1

         case (71, 72 )        ;         !  Profiler & VADWND - NEXRAD winds
            if (.not.use_ProfilerObs) cycle reports
            num_profiler = num_profiler + 1

         case (582)    ;                 !  Quikscat
            if (.not.use_qscatobs) cycle reports
            num_qscat  = num_qscat  + 1

         case (583)    ;                 !  GPS IPW
            if (.not.use_GpspwObs) cycle reports
            num_gpspw  = num_gpspw + 1

         case (584)    ;                 !  GPS REF
            if (.not.use_GpsrefObs) cycle reports
            num_gpsref  = num_gpsref + 1

         case default ;
            select case (kx)
            case (111 , 210)    ;           !  Tropical Cyclone Bogus
               !  Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_BogusObs) cycle reports
            num_bogus = num_bogus + 1

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

   rewind iunit
   ! close(iunit)

   write (unit=message(1),fmt=*) 'total bufr records =',nrecs
   write (unit=message(2),fmt=*) 'number in domain   =',total_obs
   write (unit=message(3),fmt=*) 'number Unhandled   =',miscd 
   call da_message(message(1:3))

   !------------------------------------------------------------------------
   ! Check the numbers again, make sure we have the right number.
   !------------------------------------------------------------------------

   ob%num_sound = ob%num_sound + num_sound
   ob%num_synop = ob%num_synop + num_synop
   ob%num_pilot = ob%num_pilot + num_pilot
   ob%num_satem = ob%num_satem + num_satem
   ob%num_geoamv = ob%num_geoamv + num_geoamv
   ob%num_polaramv = ob%num_polaramv + num_polaramv
   ob%num_airep = ob%num_airep + num_airep
   ob%num_gpspw = ob%num_gpspw + num_gpspw
   ob%num_gpsref = ob%num_gpsref + num_gpsref
   ob%num_metar = ob%num_metar + num_metar
   ob%num_ships = ob%num_ships + num_ships
   ob%num_qscat = ob%num_qscat + num_qscat
   ob%num_buoy  = ob%num_buoy  + num_buoy
   ob%num_profiler = ob%num_profiler + num_profiler
   ob%num_bogus = ob%num_bogus + num_bogus

   ob%num_ssmt1 = ob%num_ssmt1 + num_ssmt1
   ob%num_ssmt2 = ob%num_ssmt2 + num_ssmt2

   ! ob%num_ssmi_tb        = ob%num_ssmi_tb + num_ssmi_tb
   ! ob%num_ssmi_retrieval = ob%num_ssmi_retrieval + num_ssmi_retrieval

   ob%ob_numb(ob%current_ob_time)%sound = ob%num_sound
   ob%ob_numb(ob%current_ob_time)%synop = ob%num_synop
   ob%ob_numb(ob%current_ob_time)%pilot = ob%num_pilot
   ob%ob_numb(ob%current_ob_time)%satem = ob%num_satem
   ob%ob_numb(ob%current_ob_time)%geoamv = ob%num_geoamv
   ob%ob_numb(ob%current_ob_time)%polaramv = ob%num_polaramv
   ob%ob_numb(ob%current_ob_time)%airep = ob%num_airep
   ob%ob_numb(ob%current_ob_time)%gpspw = ob%num_gpspw
   ob%ob_numb(ob%current_ob_time)%gpsref = ob%num_gpsref
   ob%ob_numb(ob%current_ob_time)%metar = ob%num_metar
   ob%ob_numb(ob%current_ob_time)%ships = ob%num_ships
   ob%ob_numb(ob%current_ob_time)%qscat = ob%num_qscat
   ob%ob_numb(ob%current_ob_time)%buoy  = ob%num_buoy
   ob%ob_numb(ob%current_ob_time)%profiler = ob%num_profiler
   ob%ob_numb(ob%current_ob_time)%bogus = ob%num_bogus

   ob%ob_numb(ob%current_ob_time)%ssmt1 = ob%num_ssmt1
   ob%ob_numb(ob%current_ob_time)%ssmt2 = ob%num_ssmt2

   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! PRinT out
   ! =============

   if (print_detail_obs) then
      write(unit=stdout, fmt='(a)')  ' '

      write(unit=stdout, fmt='(5x,a,i6,a)') &
         'Scan:  ', ob%current_ob_time, ' ob time:', &
         'Scan:  ', total_obs,    ' Total reports.', &
         'Scan:  ', num_sound,    ' SOUND      reports,', &
         'Scan:  ', num_synop,    ' SYNOP      reports,', &
         'Scan:  ', num_pilot,    ' PILOT      reports,', &
         'Scan:  ', num_satem,    ' SATEM      reports,', &
         'Scan:  ', num_geoamv,   ' Geo. AMVs  reports,', &
         'Scan:  ', num_polaramv, ' Polar AMVs reports,', &
         'Scan:  ', num_airep,    ' AIREP      reports,', &
         'Scan:  ', num_gpspw,    ' GPSPW      reports,', &
         'Scan:  ', num_gpsref,   ' GPSRF      reports,', &
         'Scan:  ', num_metar,    ' METAR      reports,', &
         'Scan:  ', num_ships,    ' SHIP       reports,', &
         'Scan:  ', num_qscat,    ' QSCAT      reports,', &
         'Scan:  ', num_profiler, ' Profiler   reports,', &
         'Scan:  ', num_buoy,     ' Buoy       reports,', &
         'Scan:  ', num_bogus,    ' TCBGS      reports,', & 
         'Scan:  ', num_ssmt1,    ' SSMT1      reports,', & 
         'Scan:  ', num_ssmt2,    ' SSMT2      reports.'    
      write(unit=stdout,fmt=*) ' '

      ! write(unit=stdout, fmt='(5x,a,i6,a)') &
      !    'Scan:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
      !    'Scan:  ', num_ssmi_tb        , ' SSMI_TB        reports.'

   end if

   close(iunit)
   call da_free_unit(iunit)

end subroutine da_scan_bufr_obs
