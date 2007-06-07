subroutine da_scan_obs (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan WRFVAR GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in) :: xp           ! Domain decomposition vars.
   type (ob_type), intent(inout) :: ob

   character(*),   intent (in)   :: filename

   character (LEN =  10)        :: fmt_name

   character (LEN = 160)        :: info_string

   character (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, & 
                                   fmt_each

   integer                      :: i, iost, fm, report,iunit

   type (multi_level_type)      :: platform

   logical                      :: outside, outside_all

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
                                   num_ssmt2, num_airsr, &
                                   num_qscat, &
                                   num_profiler, num_buoy, num_bogus

   integer                        :: ndup, n

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
   ob%ob_numb(ob%current_ob_time)%airsr = ob%num_airsr
   ! WHY?
   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! open file
   ! ---------
   call da_get_unit(iunit)
   open(unit   = iunit,     &
      FILE   = trim(filename), &
      FORM   = 'FORMATTED',  &
      ACCESS = 'SEQUENTIAL', &
      iostat =  iost,     &
      STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening gts obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit)
      return
   end if

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
   num_qscat = 0
   num_profiler = 0
   num_buoy = 0
   num_bogus = 0
   num_airsr = 0

   ! read header

   head_info: do
      read (unit = iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I3,A,I3)') &
            "Error",iost,"reading gts obs header on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
         return
      end if
      if (info_string(1:6) == 'EACH  ') exit
   end do head_info

   ! read formats

   read (iunit, fmt = '(A,1X,A)', iostat = iost) &
       fmt_name, fmt_info, &
       fmt_name, fmt_loc,  &
       fmt_name, fmt_each

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I3,A,I3)') &
         "Error",iost,"reading gts obs formats on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      return
   end if

   ! skip units
   read (iunit, fmt = '(A)') fmt_name

   ! loop over records

   report = 0 ! report number in file

   reports: &
   do

      report = report+1

      ! read station general info

      read (iunit, fmt = fmt_info, iostat = iost) &
                   platform%info%platform,    &
                   platform%info%date_char,   &
                   platform%info%name,        &
                   platform%info%levels,      &
                   platform%info%lat,         &
                   platform%info%lon,         &
                   platform%info%elv,         &
                   platform%info%id

      if (iost /= 0) then
         ! FIX? This is expected, but its unclear how we handle failure
         ! here without assuming the fortran2003 convention on
         ! error statuses
         exit reports
      end if

      if (print_detail_obs) then
         write(unit=stdout, fmt = fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id
      end if

      if (platform%info%lon == 180.0) platform%info%lon =-180.000
      ! WHY?
      ! Fix funny wind direction at South Poles
      ! if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
      !    platform%info%lon = 0.0
      ! end if

      read (platform%info%platform(4:6), '(I3)') fm

      ! read model location
      read (iunit, fmt = fmt_loc) platform%loc%slp, platform%loc%pw

      total_obs = total_obs + 1

      ! levels < 1 and not GPSPW, go back to reports

      if ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
         cycle reports
      end if

      ! read each level

      do i = 1, platform%info%levels
         platform%each (i) = each_level_type(missing_r, missing, -1.0, & ! height
            field_type(missing_r, missing, missing_r), & ! u
            field_type(missing_r, missing, missing_r), & ! v
            field_type(missing_r, missing, missing_r), & ! p
            field_type(missing_r, missing, missing_r), & ! t
            field_type(missing_r, missing, missing_r), & ! q
            field_type(missing_r, missing, missing_r), & ! rh
            field_type(missing_r, missing, missing_r), & ! td
            field_type(missing_r, missing, missing_r))  ! speed 

         read (unit = iunit, fmt = trim (fmt_each)) &
            platform%each (i)%p,            &
            platform%each (i)%speed,        &
            ! Here the 'direction' is stored in platform%each (i)%v:
            platform%each (i)%v,            &
            platform%each (i)%height,       &
            platform%each (i)%height_qc,    &
            height_error,                   &
            platform%each (i)%t,            &
            platform%each (i)%td,           &
            platform%each (i)%rh
      end do

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) then
         cycle
      end if

      if (report > report_end) then
         exit
      end if

      call da_ll_to_xy (platform%info, platform%loc,   &
                        xp, outside, outside_all)

      if (outside) then
         cycle reports
      end if

      if (platform%info%levels < 1) then
         if (fm /= 111) then
            cycle reports
         end if
      end if

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      if (test_wrfvar) ndup = 1

      do n = 1, ndup
         select case(fm)

         case (12) ;
            if (.not.use_SynopObs) cycle reports
            num_synop = num_synop + 1

         case (13, 17) ;                  ! ships          
            if (.not.use_ShipsObs) cycle reports
            num_ships  = num_ships  + 1

         case (15:16) ;
            if (.not.use_MetarObs) cycle reports
            num_metar = num_metar + 1

         case (32:34) ;
            if (.not.use_PilotObs) cycle reports
            num_pilot = num_pilot + 1

         case (35:38) ;
            if (.not.use_SoundObs) cycle reports
            num_sound = num_sound + 1

         case (86) ;
            if (.not.use_SatemObs) cycle reports

            ! Reject cloudy Satem obs.
            if (platform%loc%pw%inv > 10.) then
               cycle reports
            end if

            num_satem = num_satem + 1

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0)  then
               if (.not.use_PolarAMVObs) cycle reports
               num_polaramv = num_polaramv + 1
            else
               if (.not.use_GeoAMVObs) cycle reports 
               num_geoamv = num_geoamv + 1
            end if

         case (42,96:97) ;
            if (.not.use_AirepObs) cycle reports
            num_airep = num_airep + 1

         case (111) ;       
            if (.not.use_GpspwObs) cycle reports
            num_gpspw = num_gpspw + 1

         case (116) ;
         if (.not.use_GpsrefObs) cycle reports
         num_gpsref = num_gpsref + 1

          case (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs) cycle reports
            num_ssmt1 = num_ssmt1 + 1

         case (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs) cycle reports
            num_ssmt2 = num_ssmt2 + 1

         case (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs) cycle reports
            num_qscat  = num_qscat  + 1

         case (132) ;
            if (.not.use_ProfilerObs) cycle reports
            num_profiler = num_profiler + 1

         case (135) ;
            if (.not.use_BogusObs) cycle reports
            num_bogus = num_bogus + 1

         case (18,19) ;             ! buoy
            if (.not.use_BuoyObs) cycle reports
            num_buoy  = num_buoy  + 1

         case (133) ;               ! AIRS retrievals
            if (.not.use_AIRSRETObs) cycle reports
            num_airsr = num_airsr + 1

         case default;
            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
         end select
      end do        !  loop over duplicate
   end do reports

   close(iunit)
   call da_free_unit(iunit)

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
   ob%num_airsr = ob%num_airsr + num_airsr

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
   ob%ob_numb(ob%current_ob_time)%airsr = ob%num_airsr

   ! WHY?
   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! print out

   if (print_detail_obs) then
      write(unit=stdout, fmt='(a)')  ' '
      write(unit=stdout, fmt='(5x,a,i6,a)') &
           'Scan:  ', ob%current_ob_time, ' ob time:', &
           'Scan:  ', total_obs, ' Total reports.', &
           'Scan:  ', num_sound, ' SOUND reports,', &
           'Scan:  ', num_synop, ' SYNOP reports,', &
           'Scan:  ', num_pilot, ' PILOT reports,', &
           'Scan:  ', num_satem, ' SATEM reports,', &
           'Scan:  ', num_geoamv,' Geo. AMVs rports,', &
           'Scan:  ', num_polaramv,' Polar AMVs rports,', &
           'Scan:  ', num_airep, ' AIREP reports,', &
           'Scan:  ', num_gpspw, ' GPSPW reports,', &
           'Scan:  ', num_gpsref,' GPSRF reports,', &
           'Scan:  ', num_metar, ' METAR reports,', &
           'Scan:  ', num_ships, ' SHIP  reports,', &
           'Scan:  ', num_qscat, ' QSCAT reports,', &
           'Scan:  ', num_profiler, ' Profiler reports,', &
           'Scan:  ', num_buoy, ' Buoy reports,', &
           'Scan:  ', num_bogus, ' TCBGS reports,', &
           'Scan:  ', num_ssmt1, ' SSMT1 reports,', &
           'Scan:  ', num_ssmt2, ' SSMT2 reports.', &
           'Scan:  ', num_airsr, ' AIRS retrieval reports'

      ! WHY?
      ! write(unit=stdout, fmt='(5x,a,i6,a)') &
      !    'Scan:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
      !    'Scan:  ', num_ssmi_tb        , ' SSMI_TB        reports.'

   end if

end subroutine da_scan_obs


