subroutine da_scan_obs_ascii (iv, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan WRFVAR GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)    :: xp           ! Domain decomposition vars.
   type (iv_type),    intent(inout) :: iv
   character(*),      intent(in)    :: filename

   character (len =  10)   :: fmt_name
   character (len = 160)   :: info_string
   character (len = 160)   :: fmt_info
   character (len = 160)   :: fmt_loc
   character (len = 160)   :: fmt_each

   integer                 :: i, iost, fm, report, iunit
   type (multi_level_type) :: platform
   logical                 :: outside, outside_all
   real                    :: height_error
   integer                 :: ndup, n

   if (trace_use) call da_trace_entry("da_scan_obs_ascii")

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
      if (global .and. (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      if (test_wrfvar) ndup = 1

      do n = 1, ndup
         select case(fm)

         case (12) ; 
            if (.not.use_synopobs .or. iv%ntotal(synop) == max_synop_input) cycle reports
            if (n==1) iv%ntotal(synop) = iv%ntotal(synop) + 1
            if (outside) cycle reports
            iv%nlocal(synop) = iv%nlocal(synop) + 1

         case (13, 17) ;    
            if (.not.use_shipsobs .or. iv%ntotal(ships) == max_ships_input) cycle reports
            if (n==1) iv%ntotal(ships) = iv%ntotal(ships) + 1
            if (outside) cycle reports
            iv%nlocal(ships) = iv%nlocal(ships) + 1

         case (15:16) ;     
            if (.not.use_metarobs .or. iv%ntotal(metar) == max_metar_input) cycle reports
            if (n==1) iv%ntotal(metar) = iv%ntotal(metar) + 1
            if (outside) cycle reports
            iv%nlocal(metar) = iv%nlocal(metar) + 1

         case (32:34) ;
            if (.not.use_pilotobs .or. iv%ntotal(pilot) == max_pilot_input) cycle reports
            if (n==1) iv%ntotal(pilot) = iv%ntotal(pilot) + 1
            if (outside) cycle reports
            iv%nlocal(pilot) = iv%nlocal(pilot) + 1

         case (35:38) ;
            if (.not.use_soundobs .or. iv%ntotal(sound) == max_sound_input) cycle reports
            if (n==1) iv%ntotal(sound) = iv%ntotal(sound) + 1
            if (outside) cycle reports
            iv%nlocal(sound) = iv%nlocal(sound) + 1

         case (86) ;
            if (.not.use_satemobs .or. iv%ntotal(satem) == max_satem_input) cycle reports
            ! Reject cloudy satem obs.
            if (platform%loc%pw%inv > 10.0) then
               cycle reports
            end if
            if (n==1) iv%ntotal(satem) = iv%ntotal(satem) + 1
            if (outside) cycle reports
            iv%nlocal(satem) = iv%nlocal(satem) + 1

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0)  then
               if (.not.use_polaramvobs .or. iv%ntotal(polaramv) == max_polaramv_input) cycle reports
               if (n==1) iv%ntotal(polaramv) = iv%ntotal(polaramv) + 1
               if (outside) cycle reports
               iv%nlocal(polaramv) = iv%nlocal(polaramv) + 1
            else
               if (.not.use_geoamvobs .or. iv%ntotal(geoamv) == max_geoamv_input) cycle reports
               if (n==1) iv%ntotal(geoamv) = iv%ntotal(geoamv) + 1
               if (outside) cycle reports
               iv%nlocal(geoamv) = iv%nlocal(geoamv) + 1
            end if

         case (42,96:97) ;
            if (.not.use_airepobs .or. iv%ntotal(airep) == max_airep_input) cycle reports
            if (n==1) iv%ntotal(airep) = iv%ntotal(airep) + 1
            if (outside) cycle reports
            iv%nlocal(airep) = iv%nlocal(airep) + 1

         case (111) ;       
            if (.not.use_gpspwobs .or. iv%ntotal(gpspw) == max_gpspw_input) cycle reports
            if (n==1) iv%ntotal(gpspw) = iv%ntotal(gpspw) + 1
            if (outside) cycle reports
            iv%nlocal(gpspw) = iv%nlocal(gpspw) + 1

         case (116) ;
            if (.not.use_gpsrefobs .or. iv%ntotal(gpsref) == max_gpsref_input) cycle reports
            if (n==1) iv%ntotal(gpsref) = iv%ntotal(gpsref) + 1
            if (outside) cycle reports
            iv%nlocal(gpsref) = iv%nlocal(gpsref) + 1

          case (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs .or. iv%ntotal(ssmt1) == max_ssmt1_input) cycle reports
            if (n==1) iv%ntotal(ssmt1) = iv%ntotal(ssmt1) + 1
            if (outside) cycle reports
            iv%nlocal(ssmt1) = iv%nlocal(ssmt1) + 1

         case (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs .or. iv%ntotal(ssmt2) == max_ssmt2_input) cycle reports
            if (n==1) iv%ntotal(ssmt2) = iv%ntotal(ssmt2) + 1
            if (outside) cycle reports
            iv%nlocal(ssmt2) = iv%nlocal(ssmt2) + 1

         case (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs .or. iv%ntotal(qscat) == max_qscat_input) cycle reports
            if (n==1) iv%ntotal(qscat) = iv%ntotal(qscat) + 1
            if (outside) cycle reports
            iv%nlocal(qscat) = iv%nlocal(qscat) + 1

         case (132) ;
            if (.not.use_profilerobs .or. iv%ntotal(profiler) == max_profiler_input) cycle reports
            if (n==1) iv%ntotal(profiler) = iv%ntotal(profiler) + 1
            if (outside) cycle reports
            iv%nlocal(profiler) = iv%nlocal(profiler) + 1

         case (135) ;
            if (.not.use_bogusobs .or. iv%ntotal(bogus) == max_bogus_input) cycle reports
            if (n==1) iv%ntotal(bogus) = iv%ntotal(bogus) + 1
            if (outside) cycle reports
            iv%nlocal(bogus) = iv%nlocal(bogus) + 1

         case (18,19) ;             ! buoy
            if (.not.use_buoyobs .or. iv%ntotal(buoy) == max_buoy_input) cycle reports
            if (n==1) iv%ntotal(buoy) = iv%ntotal(buoy) + 1
            if (outside) cycle reports
            iv%nlocal(buoy) = iv%nlocal(buoy) + 1

         case (133) ;               ! AIRS retrievals
            if (.not.use_airsretobs .or. iv%ntotal(airsr) == max_airsr_input) cycle reports
            if (n==1) iv%ntotal(airsr) = iv%ntotal(airsr) + 1
            if (outside) cycle reports
            iv%nlocal(airsr) = iv%nlocal(airsr) + 1

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
 
   iv%plocal(iv%time,:) = iv%nlocal(:)
   iv%ptotal(iv%time,:) = iv%ntotal(:)

   if (trace_use) call da_trace_exit("da_scan_obs_ascii")

end subroutine da_scan_obs_ascii


