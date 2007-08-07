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

   integer                        :: ndup, n

   if (trace_use) call da_trace_entry("da_scan_obs")

   ! default value

   ob%ob_numb(ob%current_ob_time)%sound    = ob%num_sound
   ob%ob_numb(ob%current_ob_time)%synop    = ob%num_synop
   ob%ob_numb(ob%current_ob_time)%pilot    = ob%num_pilot
   ob%ob_numb(ob%current_ob_time)%satem    = ob%num_satem
   ob%ob_numb(ob%current_ob_time)%geoamv   = ob%num_geoamv
   ob%ob_numb(ob%current_ob_time)%polaramv = ob%num_polaramv
   ob%ob_numb(ob%current_ob_time)%airep    = ob%num_airep
   ob%ob_numb(ob%current_ob_time)%gpspw    = ob%num_gpspw
   ob%ob_numb(ob%current_ob_time)%gpsref   = ob%num_gpsref
   ob%ob_numb(ob%current_ob_time)%metar    = ob%num_metar
   ob%ob_numb(ob%current_ob_time)%ships    = ob%num_ships
   ob%ob_numb(ob%current_ob_time)%qscat    = ob%num_qscat
   ob%ob_numb(ob%current_ob_time)%buoy     = ob%num_buoy
   ob%ob_numb(ob%current_ob_time)%profiler = ob%num_profiler

   ob%ob_numb(ob%current_ob_time)%ssmt1 = ob%num_ssmt1
   ob%ob_numb(ob%current_ob_time)%ssmt2 = ob%num_ssmt2
   ob%ob_numb(ob%current_ob_time)%airsr = ob%num_airsr

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

   reports: do
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

      ob%total_obs = ob%total_obs + 1

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

      if (report < report_start) cycle
      if (report > report_end) exit

      call da_ll_to_xy (platform%info, platform%loc, xp, outside, outside_all)

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
            if (.not.use_synopobs .or. ob%num_synop_glo == max_synop_input) cycle reports
            if (n==1) ob%num_synop_glo = ob%num_synop_glo + 1
            if (outside) cycle reports
            ob%num_synop = ob%num_synop + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_synop_glo

         case (13, 17) ;                  ! ships          
            if (.not.use_shipsobs .or. ob%num_ships_glo == max_ships_input) cycle reports
            if (n==1) ob%num_ships_glo = ob%num_ships_glo + 1
            if (outside) cycle reports
            ob%num_ships  = ob%num_ships  + 1
            platform%loc%obs_global_index = ob%num_ships_glo

         case (15:16) ;
            if (.not.use_metarobs .or. ob%num_metar_glo == max_metar_input) cycle reports
            if (n==1) ob%num_metar_glo = ob%num_metar_glo + 1
            if (outside) cycle reports
            ob%num_metar = ob%num_metar + 1
            platform%loc%obs_global_index = ob%num_metar_glo

         case (32:34) ;
            if (.not.use_pilotobs .or. ob%num_pilot_glo == max_pilot_input) cycle reports
            if (n==1) ob%num_pilot_glo = ob%num_pilot_glo + 1
            if (outside) cycle reports
            ob%num_pilot = ob%num_pilot + 1
            platform%loc%obs_global_index = ob%num_pilot_glo

         case (35:38) ;
            if (.not.use_soundobs .or. ob%num_sound_glo == max_sound_input) cycle reports
            if (n==1) ob%num_sound_glo = ob%num_sound_glo + 1
            if (outside) cycle reports
            ob%num_sound = ob%num_sound + 1
            platform%loc%obs_global_index  = ob%num_sound_glo

         case (86) ;
            if (.not.use_satemobs .or. ob%num_satem_glo == max_satem_input) cycle reports
            ! Reject cloudy satem obs.
            if (platform%loc%pw%inv > 10.0) then
               cycle reports
            end if
            if (n==1) ob%num_satem_glo = ob%num_satem_glo + 1
            if (outside) cycle reports
            ob%num_satem = ob%num_satem + 1
            platform%loc%obs_global_index = ob%num_satem_glo

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0)  then
               if (.not.use_polaramvobs .or. ob%num_polaramv_glo == max_polaramv_input) cycle reports
               if (n==1) ob%num_polaramv_glo = ob%num_polaramv_glo + 1
               if (outside) cycle reports
               ob%num_polaramv = ob%num_polaramv + 1
               platform%loc%obs_global_index = ob%num_polaramv_glo
            else
               if (.not.use_geoamvobs .or. ob%num_geoamv_glo == max_geoamv_input) cycle reports
               if (n==1) ob%num_geoamv_glo = ob%num_geoamv_glo + 1
               if (outside) cycle reports 
               ob%num_geoamv = ob%num_geoamv + 1
               platform%loc%obs_global_index = ob%num_geoamv_glo
            end if

         case (42,96:97) ;
            if (.not.use_airepobs .or. ob%num_airep_glo == max_airep_input) cycle reports
            if (n==1) ob%num_airep_glo = ob%num_airep_glo + 1
            if (outside) cycle reports
            ob%num_airep = ob%num_airep + 1
            platform%loc%obs_global_index = ob%num_airep_glo

         case (111) ;       
            if (.not.use_gpspwobs .or. ob%num_gpspw_glo == max_gpspw_input) cycle reports
            if (n==1) ob%num_gpspw_glo = ob%num_gpspw_glo + 1
            if (outside) cycle reports
            ob%num_gpspw = ob%num_gpspw + 1
            platform%loc%obs_global_index = ob%num_gpspw_glo

         case (116) ;
         if (.not.use_gpsrefobs .or. ob%num_gpsref_glo == max_gpsref_input) cycle reports
            if (n==1) ob%num_gpsref_glo = ob%num_gpsref_glo + 1
            if (outside) cycle reports
            ob%num_gpsref = ob%num_gpsref + 1
            platform%loc%obs_global_index = ob%num_gpsref_glo

          case (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs .or. ob%num_ssmt1_glo == max_ssmt1_input) cycle reports
            if (n==1) ob%num_ssmt1_glo = ob%num_ssmt1_glo + 1
            if (outside) cycle reports
            ob%num_ssmt1 =ob% num_ssmt1 + 1
            platform%loc%obs_global_index = ob%num_ssmt1_glo

         case (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs .or. ob%num_ssmt2_glo == max_ssmt2_input) cycle reports
            if (n==1) ob%num_ssmt2_glo = ob%num_ssmt2_glo + 1
            if (outside) cycle reports
            ob%num_ssmt2 = ob%num_ssmt2 + 1
            platform%loc%obs_global_index = ob%num_ssmt2_glo

         case (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs .or. ob%num_qscat_glo == max_qscat_input) cycle reports
            if (n==1) ob%num_qscat_glo = ob%num_qscat_glo + 1
            if (outside) cycle reports
            ob%num_qscat  = ob%num_qscat  + 1
            platform%loc%obs_global_index = ob%num_qscat_glo

         case (132) ;
            if (.not.use_profilerobs .or. ob%num_profiler_glo == max_profiler_input) cycle reports
            if (n==1) ob%num_profiler_glo = ob%num_profiler_glo + 1
            if (outside) cycle reports
            ob%num_profiler = ob%num_profiler + 1
            platform%loc%obs_global_index = ob%num_profiler_glo

         case (135) ;
            if (.not.use_bogusobs .or. ob%num_bogus_glo == max_bogus_input) cycle reports
            if (n==1) ob%num_bogus_glo = ob%num_bogus_glo + 1
            if (outside) cycle reports
            ob%num_bogus = ob%num_bogus + 1
            platform%loc%obs_global_index = ob%num_bogus_glo

         case (18,19) ;             ! buoy
            if (.not.use_buoyobs .or. ob%num_buoy_glo == max_buoy_input) cycle reports
            if (n==1) ob%num_buoy_glo = ob%num_buoy_glo + 1
            if (outside) cycle reports
            ob%num_buoy  = ob%num_buoy  + 1
            platform%loc%obs_global_index = ob%num_buoy_glo

         case (133) ;               ! AIRS retrievals
            if (.not.use_airsretobs .or. ob%num_airsr_glo == max_airsret_input) cycle reports
            if (n==1) ob%num_airsr_glo = ob%num_airsr_glo + 1
            if (outside) cycle reports
            ob%num_airsr = ob%num_airsr + 1
            platform%loc%obs_global_index = ob%num_airsr_glo

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

   ob%ob_numb(ob%current_ob_time)%sound    = ob%num_sound
   ob%ob_numb(ob%current_ob_time)%synop    = ob%num_synop
   ob%ob_numb(ob%current_ob_time)%pilot    = ob%num_pilot
   ob%ob_numb(ob%current_ob_time)%satem    = ob%num_satem
   ob%ob_numb(ob%current_ob_time)%geoamv   = ob%num_geoamv
   ob%ob_numb(ob%current_ob_time)%polaramv = ob%num_polaramv
   ob%ob_numb(ob%current_ob_time)%airep    = ob%num_airep
   ob%ob_numb(ob%current_ob_time)%gpspw    = ob%num_gpspw
   ob%ob_numb(ob%current_ob_time)%gpsref   = ob%num_gpsref
   ob%ob_numb(ob%current_ob_time)%metar    = ob%num_metar
   ob%ob_numb(ob%current_ob_time)%ships    = ob%num_ships
   ob%ob_numb(ob%current_ob_time)%qscat    = ob%num_qscat
   ob%ob_numb(ob%current_ob_time)%buoy     = ob%num_buoy
   ob%ob_numb(ob%current_ob_time)%profiler = ob%num_profiler
   ob%ob_numb(ob%current_ob_time)%bogus    = ob%num_bogus

   ob%ob_numb(ob%current_ob_time)%ssmt1    = ob%num_ssmt1
   ob%ob_numb(ob%current_ob_time)%ssmt2    = ob%num_ssmt2
   ob%ob_numb(ob%current_ob_time)%airsr    = ob%num_airsr

   write(unit=stdout, fmt='(a)')  'Obs from GTS file'
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
   write(unit=stdout, fmt='(5x,a,i6,a)') &
      'There are ', ob%total_obs - ob%num_sound_glo - ob%num_synop_glo &
         - ob%num_pilot_glo - ob%num_satem_glo &
         - ob%num_geoamv_glo - ob%num_polaramv_glo - ob%num_airep_glo &
         - ob%num_metar_glo - ob%num_ships_glo &
         - ob%num_ssmt1_glo - ob%num_ssmt2_glo &
         - ob%num_gpspw_glo- ob%num_gpsref_glo - ob%num_qscat_glo  &
         - ob%num_profiler_glo - ob%num_buoy_glo &
         - ob%num_bogus_glo - ob%num_airsr_glo , &
         '  reports unprocessed.'

   if (trace_use) call da_trace_exit("da_scan_obs")

end subroutine da_scan_obs


