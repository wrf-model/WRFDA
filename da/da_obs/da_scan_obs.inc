SUBROUTINE da_scan_obs (iunit, ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan WRFVAR GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   INTEGER,         INTENT (in)  :: iunit
   TYPE (xpose_type), INTENT(IN) :: xp           ! Domain decomposition vars.
   TYPE (ob_type), INTENT(inout) :: ob

   CHARACTER(*),   INTENT (in)   :: filename

   CHARACTER (LEN =  10)        :: fmt_name

   CHARACTER (LEN = 160)        :: info_string

   CHARACTER (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, & 
                                   fmt_each

   INTEGER                      :: i, j, iost, fm, report

   TYPE (multi_level_type)      :: platform

   LOGICAL                      :: inside_halo
   LOGICAL                      :: outside, outside_all

   REAL                         :: height_error


   INTEGER                      :: total_obs, &
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
                                   num_pseudo, num_qscat, &
                                   num_profiler, num_buoy, num_bogus

   INTEGER                        :: ndup, n

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
   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! OPEN FILE
   ! ---------

   OPEN(UNIT   = iunit,     &
      FILE   = trim(filename), &
      FORM   = 'FORMATTED',  &
      ACCESS = 'SEQUENTIAL', &
      IOSTAT =  iost,     &
      STATUS = 'OLD')

   IF (iost /= 0) THEN
      WRITE(UNIT=errmsg(1),FMT='(A,I5,A)') &
         "Error",iost," opening gts obs file "//TRIM(filename)
      CALL da_warning(__FILE__,__LINE__,errmsg(1:1))
      RETURN
   ENDIF

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
   num_airsr = 0

   ! READ HEADER
   ! ===========

   head_info: DO
      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) info_string
      IF (iost /= 0) THEN
         WRITE(UNIT=errmsg(1),FMT='(A,I3,A,I3)') &
            "Error",iost,"reading gts obs header on unit",iunit
         CALL da_warning(__FILE__,__LINE__,errmsg(1:1))
         return
      ENDIF
      if (info_string(1:6) == 'EACH  ') EXIT
   ENDDO head_info

   ! READ FORMATS
   ! ------------

   READ (iunit, FMT = '(A,1X,A)', IOSTAT = iost) &
       fmt_name, fmt_info, &
       fmt_name, fmt_loc,  &
       fmt_name, fmt_each

   IF (iost /= 0) THEN
      WRITE(UNIT=errmsg(1),FMT='(A,I3,A,I3)') &
         "Error",iost,"reading gts obs formats on unit",iunit
         CALL da_warning(__FILE__,__LINE__,errmsg(1:1))
      RETURN
   ENDIF

   ! SKIP UNITS
   ! ----------

   READ (iunit, FMT = '(A)') fmt_name

   ! LOOP OVER RECORDS
   ! -----------------

   report = 0 ! report number in file

   reports: &
   DO

      report = report+1

      ! READ STATION GENERAL INFO
      ! =============================

      READ (iunit, FMT = fmt_info, IOSTAT = iost) &
                   platform%info%platform,    &
                   platform%info%date_char,   &
                   platform%info%name,        &
                   platform%info%levels,      &
                   platform%info%lat,         &
                   platform%info%lon,         &
                   platform%info%elv,         &
                   platform%info%id

      IF (iost /= 0) THEN
         ! This is expected, but its unclear how we handle failure
         ! here without assuming the fortran2003 convention on
         ! error statuses
         EXIT reports
      ENDIF

      if (print_detail_obs) then
         write(UNIT=stdout, FMT = fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id
      endif

      if (platform%info%lon == 180.0) platform%info%lon =-180.000
      ! Fix funny wind direction at South Poles
      ! if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
      !    platform%info%lon = 0.0
      ! endif

      read(platform%info%platform(4:6), '(I3)') fm

      ! READ MODEL LOCATION
      ! =========================

      READ (iunit, FMT = fmt_loc) platform%loc%slp, platform%loc%pw

      total_obs = total_obs + 1

      ! levels < 1 and not GPSPW, go back to reports

      IF ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
         cycle reports
      ENDIF

      ! READ EACH LEVELS
      ! ----------------

      loop_level: DO i = 1, platform%info%levels

         platform%each (i) = each_level_type(missing_r, missing, -1.0, & ! height
            field_type(missing_r, missing, missing_r), & ! u
            field_type(missing_r, missing, missing_r), & ! v
            field_type(missing_r, missing, missing_r), & ! p
            field_type(missing_r, missing, missing_r), & ! t
            field_type(missing_r, missing, missing_r), & ! q
            field_type(missing_r, missing, missing_r), & ! rh
            field_type(missing_r, missing, missing_r), & ! td
            field_type(missing_r, missing, missing_r))  ! speed 

         READ (UNIT = iunit, FMT = TRIM (fmt_each)) &
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
      ENDDO loop_level

      ! Restrict to a range of reports, useful for debugging

      IF (report < report_start) THEN
         CYCLE
      END IF

      IF (report > report_end) THEN
         EXIT
      END IF

      call da_ll_to_xy (platform%info, platform%loc,   &
                        xp, outside, inside_halo,outside_all )

      IF(outside) THEN
         cycle reports
      ENDIF

      if (platform%info%levels < 1) then
         if ( fm /= 111 ) then
            cycle reports
         endif
      endif

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      if (Testing_WRFVAR) ndup = 1

      DO n = 1, ndup
         SELECT CASE(fm)

         CASE (12) ;
            if (.not.use_SynopObs) cycle reports
            num_synop = num_synop + 1

         CASE (13, 17) ;                  ! ships          
            if (.not.use_ShipsObs) cycle reports
            num_ships  = num_ships  + 1

         CASE (15:16) ;
            if(.not.use_MetarObs) cycle reports
            num_metar = num_metar + 1

         CASE (32:34) ;
            if (.not.use_PilotObs) cycle reports
            num_pilot = num_pilot + 1

         CASE (35:38) ;
            if (.not.use_SoundObs) cycle reports
            num_sound = num_sound + 1

         CASE (86) ;
            if (.not.use_SatemObs) cycle reports

            ! Reject cloudy Satem obs.
            if (platform%loc%pw%inv > 10.) then
               cycle reports
            endif

            num_satem = num_satem + 1

         CASE (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0 )  then
               if (.not.use_PolarAMVObs) cycle reports
               num_polaramv = num_polaramv + 1
            else
               if (.not.use_GeoAMVObs) cycle reports 
               num_geoamv = num_geoamv + 1
            end if

         CASE (42,96:97) ;
            if (.not.use_AirepObs) cycle reports
            num_airep = num_airep + 1

         CASE (111) ;       
            if (.not.use_GpspwObs) cycle reports
            num_gpspw = num_gpspw + 1

         CASE (116) ;
            if (.not.use_GpsrefObs) cycle reports
            num_gpsref = num_gpsref + 1

         CASE (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs) cycle reports
            num_ssmt1 = num_ssmt1 + 1

         CASE (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs) cycle reports
            num_ssmt2 = num_ssmt2 + 1

         CASE (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs) cycle reports
            num_qscat  = num_qscat  + 1

         CASE (132) ;
            if (.not.use_ProfilerObs) cycle reports
            num_profiler = num_profiler + 1

         CASE (135) ;
            if (.not.use_BogusObs) cycle reports
            num_bogus = num_bogus + 1

         CASE (18,19) ;             ! buoy
            if (.not.use_BuoyObs) cycle reports
            num_buoy  = num_buoy  + 1

         CASE (133) ;               ! AIRS retrievals
            if (.not.use_AIRSRETObs) cycle reports
            num_airsr = num_airsr + 1

         CASE DEFAULT;
            write(UNIT=errmsg(1), fmt='(a)') 'unsaved obs found:'
            write(UNIT=errmsg(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(UNIT=errmsg(3), fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,errmsg(1:3))
         END SELECT
      ENDDO        !  loop over duplicate
   ENDDO reports

   CLOSE(iunit)

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

   ! ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb
   ! ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval

   ! PRINT OUT
   ! =============

   if (print_detail_obs) then
      write(UNIT=stdout, fmt='(a)')  ' '
      write(UNIT=stdout, fmt='(5x,a,i6,a)') &
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

      ! write(UNIT=stdout, fmt='(5x,a,i6,a)') &
      !    'Scan:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
      !    'Scan:  ', num_ssmi_tb        , ' SSMI_TB        reports.'

   end if

END SUBROUTINE da_scan_obs

