subroutine da_scan_bufr_obs (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in) :: xp           ! Domain decomposition vars.
   type (ob_type), intent(inout) :: ob

   character(*),   intent (in)   :: filename

#ifdef BUFR

   type (multi_level_type)      :: platform
   logical                      :: outside
   logical                      :: outside_all

   character(len=40) :: hdstr
   character(len=10) :: date
   character(len=8)  :: subset
   real,dimension(7) :: hdr

   integer           :: iost, ndup, n, report
   integer           :: year,month, day,hour
   integer           :: iret,idate, kx, nrecs,miscd, t29
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

      if (report < report_start) then
         cycle
      end if

      if (report > report_end) then
         exit
      end if

      call da_ll_to_xy (platform%info, platform%loc, xp, outside, outside_all)

      if (outside_all) cycle reports

      ob%total_obs = ob%total_obs + 1

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

      if (test_wrfvar) ndup = 1

      do n = 1, ndup

         select case(t29)

         case (11, 12, 13, 22, 23, 31)
            select case(kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs .or. ob%num_sound_glo == max_sound_input) cycle reports
               if (n==1) ob%num_sound_glo = ob%num_sound_glo + 1
               if (outside) cycle reports
               ob%num_sound = ob%num_sound + 1
               platform%loc%obs_global_index  = ob%num_sound_glo

            case (221) ;                   ! Pilot 
               if (.not.use_pilotobs .or. ob%num_pilot_glo == max_pilot_input) cycle reports
               if (n==1) ob%num_pilot_glo = ob%num_pilot_glo + 1
               if (outside) cycle reports
               ob%num_pilot = ob%num_pilot + 1
               platform%loc%obs_global_index = ob%num_pilot_glo
            end select

         case (41)
         ! case (130:131, 133, 230:231, 233) ;  ! Airep
            if (.not.use_airepobs .or. ob%num_airep_glo == max_airep_input) cycle reports
            if (n==1) ob%num_airep_glo = ob%num_airep_glo + 1
            if (outside) cycle reports
            ob%num_airep = ob%num_airep + 1
            platform%loc%obs_global_index = ob%num_airep_glo

         ! case (180, 182, 280, 282) ;          ! Ships
         case (522, 523) ;      
            if (.not.use_shipsobs .or. ob%num_ships_glo == max_ships_input) cycle reports
            if (n==1) ob%num_ships_glo = ob%num_ships_glo + 1
            if (outside) cycle reports
            ob%num_ships  = ob%num_ships  + 1
            platform%loc%obs_global_index = ob%num_ships_glo

         case (531, 561, 562) ;                ! Buoy
            if (.not.use_buoyobs .or. ob%num_buoy_glo == max_buoy_input) cycle reports
            if (n==1) ob%num_buoy_glo = ob%num_buoy_glo + 1
            if (outside) cycle reports
            ob%num_buoy  = ob%num_buoy  + 1
            platform%loc%obs_global_index = ob%num_buoy_glo

         case (511)
         ! case (181, 281) ;            ! Synop
            if (.not.use_synopobs .or. ob%num_synop_glo == max_synop_input) cycle reports
            if (n==1) ob%num_synop_glo = ob%num_synop_glo + 1
            if (outside) cycle reports
            ob%num_synop = ob%num_synop + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_synop_glo
            if (.not.use_synopobs) cycle reports
            ob%num_synop = ob%num_synop + 1

         case (512)
         ! case (187 , 287) ;                   ! Metar
            if (.not.use_metarobs .or. ob%num_metar_glo == max_metar_input) cycle reports
            if (n==1) ob%num_metar_glo = ob%num_metar_glo + 1
            if (outside) cycle reports
            ob%num_metar = ob%num_metar + 1
            platform%loc%obs_global_index = ob%num_metar_glo

         case (63)
         ! case (242:246, 252:253, 255);  ! Geo. AMVs
            if (.not.use_geoamvobs .or. ob%num_geoamv_glo == max_geoamv_input) cycle reports
            if (n==1) ob%num_geoamv_glo = ob%num_geoamv_glo + 1
            if (outside) cycle reports 
            ob%num_geoamv = ob%num_geoamv + 1

         case (71, 72)        ;         !  Profiler & VADWND - NEXRAD winds
            if (.not.use_profilerobs .or. ob%num_profiler_glo == max_profiler_input) cycle reports
            if (n==1) ob%num_profiler_glo = ob%num_profiler_glo + 1
            if (outside) cycle reports
            ob%num_profiler = ob%num_profiler + 1
            platform%loc%obs_global_index = ob%num_profiler_glo

         case (582)    ;                 !  Quikscat
            if (.not.use_qscatobs .or. ob%num_qscat_glo == max_qscat_input) cycle reports
            if (n==1) ob%num_qscat_glo = ob%num_qscat_glo + 1
            if (outside) cycle reports
            ob%num_qscat  = ob%num_qscat  + 1
            platform%loc%obs_global_index = ob%num_qscat_glo

         case (583)    ;                 !  GPS IPW      
            if (.not.use_gpspwobs .or. ob%num_gpspw_glo == max_gpspw_input) cycle reports
            if (n==1) ob%num_gpspw_glo = ob%num_gpspw_glo + 1
            if (outside) cycle reports
            ob%num_gpspw = ob%num_gpspw + 1
            platform%loc%obs_global_index = ob%num_gpspw_glo

         case (584)    ;                 !  GPS REF
            if (.not.use_gpsrefobs .or. ob%num_gpsref_glo == max_gpsref_input) cycle reports
            if (n==1) ob%num_gpsref_glo = ob%num_gpsref_glo + 1
            if (outside) cycle reports
            ob%num_gpsref = ob%num_gpsref + 1
            platform%loc%obs_global_index = ob%num_gpsref_glo

         case default ;
            select case (kx)
            case (111 , 210)    ;           !  Tropical Cyclone Bogus
               !  Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs) cycle reports
            ob%num_bogus = ob%num_bogus + 1

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
   write (unit=message(2),fmt=*) 'number in domain   =',ob%total_obs
   write (unit=message(3),fmt=*) 'number Unhandled   =',miscd 
   call da_message(message(1:3))

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

   ! print out
   ! =============

   write(unit=stdout, fmt='(a)') ' '
   write(unit=stdout, fmt='(5x,a,i6)') 'ob time ', ob%current_ob_time
   write(unit=stdout, fmt='(5x,a,i6,a,i6,a)') &
      'sound   ', ob%num_sound_glo,    ' global,',ob%num_sound,   ' local', &
      'synop   ', ob%num_synop_glo,    ' global,',ob%num_synop,   ' local', &
      'pilot   ', ob%num_pilot_glo,    ' global,',ob%num_pilot,   ' local', &
      'satem   ', ob%num_satem_glo,    ' global,',ob%num_satem,   ' local', &
      'geoamv  ', ob%num_geoamv_glo,   ' global,',ob%num_geoamv,  ' local', &
      'polaramv', ob%num_polaramv_glo, ' global,',ob%num_polaramv,' local', &
      'airep   ', ob%num_airep_glo,    ' global,',ob%num_airep,   ' local', &
      'gpspw   ', ob%num_gpspw_glo,    ' global,',ob%num_gpspw,   ' local', &
      'gpsref  ', ob%num_gpsref_glo,   ' global,',ob%num_gpsref,  ' local', &
      'metar   ', ob%num_metar_glo,    ' global,',ob%num_metar,   ' local', &
      'ships   ', ob%num_ships_glo,    ' global,',ob%num_ships,   ' local', &
      'qscat   ', ob%num_qscat_glo,    ' global,',ob%num_qscat,   ' local', &
      'profiler', ob%num_profiler_glo, ' global,',ob%num_profiler,' local', &
      'buoy    ', ob%num_buoy_glo,     ' global,',ob%num_buoy,    ' local', &
      'bogus   ', ob%num_bogus_glo,    ' global,',ob%num_bogus,   ' local', &
      'ssmt1   ', ob%num_ssmt1_glo,    ' global,',ob%num_ssmt1,   ' local', &
      'ssmt2   ', ob%num_ssmt2_glo,    ' global,',ob%num_ssmt2,   ' local', &
      'airsr   ', ob%num_airsr_glo,    ' global,',ob%num_airsr,   ' local'
   write(unit=stdout, fmt='(5x,a,i6)') 'total:', ob%total_obs

   close(iunit)
   call da_free_unit(iunit)
#endif

end subroutine da_scan_bufr_obs


