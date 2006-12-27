subroutine da_read_obs (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a GTS observation file
   !
   ! Modifications:      10/26/2004            Syed RH Rizvi
   !
   !              Fix Obs-Long as -180 if it is +180
   !
   !      Date:  03/04/2005 -       Syed  RH  Rizvi
   !
   !      (a)  AMVs from Geostationary and Polar orbiting satellite are
   !           seperated & used as profile. Currently there is a provision
   !           to use MODIS winds only.
   !      (b)  MODIS obs error are currently assigned through namelist
   !           parameter (modis_cmv_error)
   !
   !                     03/30/2005           Syed RH Rizvi
   !              For global option duplicate obs at East/West boundary
   !                        
   !                     08/30/2005           Syed RH Rizvi
   !           Writing original errors & thicknesses
   !           desired for outputting QC obs with NTMAX = 0
   !
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)    :: xp    ! Domain decomposition vars.
   type (ob_type),    intent(inout) :: ob

   character(*), intent(in)         :: filename

   character (LEN =  10)        :: fmt_name

   character (LEN = 160)        :: info_string

   character (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, & 
                                   fmt_each

   integer                      :: i, j, iost, nlevels, fm,iunit

   type (multi_level_type)      :: platform

   logical                      :: outside
   logical                      :: outside_all
   integer                      :: surface_level
   real                         :: height_error, u_comp, v_comp
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
                                   num_qscat, &
                                   num_profiler, &
                                   num_airsr,num_buoy, num_bogus

   integer                        :: ndup, n, report

   if (trace_use) call da_trace_entry("da_read_obs")

   total_obs = ob%total_obs
   num_sound = ob%num_sound
   num_synop = ob%num_synop
   num_pilot = ob%num_pilot
   num_geoamv = ob%num_geoamv
   num_polaramv = ob%num_polaramv
   num_satem = ob%num_satem
   num_airep = ob%num_airep
   num_metar = ob%num_metar
   num_ships = ob%num_ships
   num_gpspw = ob%num_gpspw
   num_gpsref = ob%num_gpsref
   num_ssmi_retrieval = ob%num_ssmi_retrieval
   num_ssmi_tb       = ob%num_ssmi_tb
   num_ssmt1 = ob%num_ssmt1
   num_ssmt2 = ob%num_ssmt2
   num_qscat = ob%num_qscat
   num_profiler = ob%num_profiler
   num_buoy = ob%num_buoy
   num_bogus= ob%num_bogus
   num_airsr= ob%num_airsr

   ! open file
   ! =========

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
      return
   end if

   ! read header
   ! ===========

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         call da_warning(__FILE__,__LINE__, &
           (/"Problem reading gts obs header, skipping file"/))
         return
      end if

      if (info_string(1:6) == 'EACH  ') exit
   end do

   !  read formats
   !  ------------

   read (iunit, fmt = '(A,1X,A)', iostat = iost) &
      fmt_name, fmt_info, &
      fmt_name, fmt_loc,  &
      fmt_name, fmt_each

   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Problem reading gts obs file, skipping"/))
      return
   end if

   if (print_detail_obs) then
      write(unit=stdout, fmt='(2a)') &
         'fmt_info=', fmt_info, &
         'fmt_loc =', fmt_loc,  &
         'fmt_each=', fmt_each
   end if

   ! skip line
   ! ----------

   read (iunit, fmt = '(a)') fmt_name

   ! loop over records
   ! -----------------

   report = 0 ! report number in file

   reports: &
   do

      report = report+1

      ! read station general info
      ! =============================

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
         ! JRB This is expected, but its unclear how we handle failure
         ! here without assuming the fortran2003 convention on
         ! error statuses
         exit reports
      end if

      if (print_detail_obs) then
         write(unit=stdout, fmt=fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id
      end if

      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000 
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if

      if (platform%info%platform(6:6) == ' ') then
         read(platform%info%platform(4:5), '(I2)') fm
      else
         read(platform%info%platform(4:6), '(I3)') fm
      end if

      ! read model location
      ! =========================

      read (iunit, fmt = fmt_loc) platform%loc%slp, platform%loc%pw
      total_obs = total_obs + 1

      ! levels < 1 and not GPSPW, go back to reports

      if ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
           cycle reports
      end if

      ! read each level
      ! ---------------

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

         if (print_detail_obs) then
            write(unit=stdout, fmt = '(a, i6)') 'i=', i
            write(unit=stdout, fmt = trim (fmt_each)) &
               platform%each (i)%p,            &
               platform%each (i)%speed,        &
               platform%each (i)%v,            &
               platform%each (i)%height,       &
               platform%each (i)%height_qc,    &
               height_error,                   &
               platform%each (i)%t,            &
               platform%each (i)%td,           &
               platform%each (i)%rh
         end if

         ! Uncomment the following if errors in reported heights (test later):             
         ! if (fm == 97 .or. fm == 96 .or. fm == 42) then
         !    platform%each (i)%height_qc = -5 ! Aircraft heights are not above surface.
         ! end if

         ! To convert the wind speed and direction to 
         !      the model wind components: u, v

         if (platform%each (i)%speed%qc /= missing_data .and. &
             platform%each (i)%v%qc /= missing_data) then
            call da_ffdduv (platform%each (i)%speed%inv, &
               platform%each (i)%v%inv,     &
               U_comp, V_comp, platform%info%lon, convert_fd2uv)
               platform%each (i)%u = platform%each (i)%speed
               platform%each (i)%v = platform%each (i)%speed
               platform%each (i)%u%inv = U_comp
               platform%each (i)%v%inv = V_comp
         else
            platform%each (i)%u%inv = missing_r
            platform%each (i)%v%inv = missing_r
            platform%each (i)%u%error = missing_r
            platform%each (i)%v%error = missing_r
            platform%each (i)%u%qc  = missing_data
            platform%each (i)%v%qc  = missing_data
         end if
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

      if (outside_all) then
         cycle reports
      end if

      if (print_detail_obs) then
         ! Simplistic approach, could be improved to get it all done on PE 0
         if (.NOT. outside) then
            write(unit=stdout,fmt='(A,I4,A,F8.2,A,F8.2,A,I3,A,2F8.2)') &
               "Report",report," at",platform%info%lon,"E",platform%info%lat, &
               "N on processor", myproc,", position", platform%loc%x,platform%loc%y
         end if
      end if

      nlevels = platform%info%levels
      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels
         write(unit=message(1), fmt='(/4(2a,2x),a,2f8.2,2x,2(a,f9.2,2x),2(a,i4,2x)/)') &
            'Station: ', trim(platform%info%name), &
            'ID: ', trim(platform%info%id), &
            'Platform: ', trim(platform%info%platform), &
            'Date: ', trim(platform%info%date_char), &
            'At Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
            'At elv: ', platform%info%elv, &
            'with pstar: ', platform%info%pstar, &
            'Has level: ', platform%info%levels, &
            'which is great than max_ob_levels: ', max_ob_levels

         write (unit=message(2), fmt = '(A,1X,A,1X,A,1X,I4,2f9.3,f9.1,1X,A)') &
            platform%info%name,        &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
         call da_warning(__FILE__,__LINE__,message(1:2))

         platform%info%levels = nlevels
      else if (nlevels < 1) then
         ! Not GPSPW and GPSZD data:
         if (fm /= 111 .and. fm /= 114) then
            cycle reports
         end if
      end if

      if (fm /= 86) call da_obs_proc_station(platform)

      nlevels = platform%info%levels
      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the 
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.  
      ! TBH:  20050913

      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               platform%loc%i,  platform%loc%j,   &
               platform%loc%dx, platform%loc%dxm, &
              platform%loc%dy, platform%loc%dym
         end if
      end if

      dup_loop: do n = 1, ndup
         select case(fm)

         case (12) ;

            if (n==1) ob%num_synop_glo = ob%num_synop_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_SynopObs) cycle reports

            num_synop = num_synop + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_synop_glo

            ob%synop(num_synop)%info = platform%info
            ob%synop(num_synop)%loc  = platform%loc

            ob%synop(num_synop)%h = platform%each(1)%height
            ob%synop(num_synop)%u = platform%each(1)%u
            ob%synop(num_synop)%v = platform%each(1)%v
            ob%synop(num_synop)%t = platform%each(1)%t
            ob%synop(num_synop)%q = platform%each(1)%q
            ob%synop(num_synop)%p = platform%each(1)%p

         case (13, 17) ;                  ! ships   

            if (n == 1) ob%num_ships_glo = ob%num_ships_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_ShipsObs) cycle reports

            num_ships  = num_ships  + 1

            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_ships_glo

            ob%ships(num_ships)%info = platform%info
            ob%ships(num_ships)%loc  = platform%loc

            ob%ships(num_ships)%h = platform%each(1)%height
            ob%ships(num_ships)%u = platform%each(1)%u
            ob%ships(num_ships)%v = platform%each(1)%v
            ob%ships(num_ships)%t = platform%each(1)%t
            ob%ships(num_ships)%p = platform%each(1)%p
            ob%ships(num_ships)%q = platform%each(1)%q

         case (15:16) ;

            if (n==1) ob%num_metar_glo = ob%num_metar_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_MetarObs) cycle reports

            num_metar = num_metar + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_metar_glo

            ob%metar(num_metar)%info = platform%info
            ob%metar(num_metar)%loc  = platform%loc

            ob%metar(num_metar)%h = platform%each(1)%height
            ob%metar(num_metar)%u = platform%each(1)%u
            ob%metar(num_metar)%v = platform%each(1)%v
            ob%metar(num_metar)%t = platform%each(1)%t
            ob%metar(num_metar)%p = platform%each(1)%p
            ob%metar(num_metar)%q = platform%each(1)%q

         case (32:34) ;

            if (n==1) ob%num_pilot_glo = ob%num_pilot_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_PilotObs) cycle reports

            num_pilot = num_pilot + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_pilot_glo

            ob%pilot(num_pilot)%info = platform%info
            ob%pilot(num_pilot)%loc  = platform%loc

            allocate (ob%pilot(num_pilot)%p (1:nlevels))
            allocate (ob%pilot(num_pilot)%zk(1:nlevels))
            allocate (ob%pilot(num_pilot)%u (1:nlevels))
            allocate (ob%pilot(num_pilot)%v (1:nlevels))

            do i = 1, nlevels
              ob%pilot(num_pilot)%p(i) = platform%each(i)%p%inv
              ob%pilot(num_pilot)%u(i) = platform%each(i)%u
              ob%pilot(num_pilot)%v(i) = platform%each(i)%v
            end do

         case (35:38) ;

            if (n==1) ob%num_sound_glo = ob%num_sound_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_SoundObs) cycle reports

            num_sound = num_sound + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index  = ob%num_sound_glo
            platform%loc%obs_global_report = report

            ob%sonde_sfc(num_sound)%info = platform%info
            ob%sonde_sfc(num_sound)%loc  = platform%loc

            ! Search to see if we have surface obs.

            surface_level = 0

            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                  surface_level = i

                  ! Save surface pressure.
                  ob%sonde_sfc(num_sound)%h = platform%each(i)%height
                  ob%sonde_sfc(num_sound)%u = platform%each(i)%u
                  ob%sonde_sfc(num_sound)%v = platform%each(i)%v
                  ob%sonde_sfc(num_sound)%t = platform%each(i)%t
                  ob%sonde_sfc(num_sound)%q = platform%each(i)%q
                  ob%sonde_sfc(num_sound)%p = platform%each(i)%p

                  exit
               end if
            end do

            ! processing the sound_sfc data:

            ob%sound(num_sound)%info = platform%info
            ob%sound(num_sound)%loc  = platform%loc

            if (surface_level > 0) then
               nlevels = nlevels - 1
            else
               ob%sonde_sfc(num_sound)%h = missing_r
               ob%sonde_sfc(num_sound)%u%inv   = missing_r
               ob%sonde_sfc(num_sound)%u%qc    = missing
               ob%sonde_sfc(num_sound)%u%error = abs(missing_r)
               ob%sonde_sfc(num_sound)%v = ob%sonde_sfc(num_sound)%u
               ob%sonde_sfc(num_sound)%t = ob%sonde_sfc(num_sound)%u
               ob%sonde_sfc(num_sound)%p = ob%sonde_sfc(num_sound)%u
               ob%sonde_sfc(num_sound)%q = ob%sonde_sfc(num_sound)%u
            end if

            if (nlevels > 0) then

               allocate (ob%sound(num_sound)%h (1:nlevels))
               allocate (ob%sound(num_sound)%p (1:nlevels))
               allocate (ob%sound(num_sound)%zk(1:nlevels))
               allocate (ob%sound(num_sound)%u (1:nlevels))
               allocate (ob%sound(num_sound)%v (1:nlevels))
               allocate (ob%sound(num_sound)%t (1:nlevels))
               allocate (ob%sound(num_sound)%q (1:nlevels))

               j = 0
               do i = 1, ob%sound(num_sound)%info%levels
                 if (i == surface_level) cycle

                 j=j+1

                 ob%sound(num_sound)%h(j) = platform%each(i)%height
                 ob%sound(num_sound)%p(j) = platform%each(i)%p%inv
                 ob%sound(num_sound)%u(j) = platform%each(i)%u
                 ob%sound(num_sound)%v(j) = platform%each(i)%v
                 ob%sound(num_sound)%t(j) = platform%each(i)%t
                 ob%sound(num_sound)%q(j) = platform%each(i)%q
               end do
            end if

            ob%sound(num_sound)%info%levels = nlevels

         case (86)    ;

            if (n==1) ob%num_satem_glo = ob%num_satem_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_SatemObs) cycle reports

            ! Reject cloudy Satem obs.

            if (platform%loc%pw%inv > 10.) then
               cycle reports
            end if

            num_satem = num_satem + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_satem_glo

            ob%satem(num_satem)%info = platform%info
            ob%satem(num_satem)%loc  = platform%loc

            ! The Satem ref_p is put in the SLP position in OBSPROC data stream.

            ob%satem(num_satem)%ref_p= platform%loc%slp%inv

            allocate (ob%satem(num_satem)%p        (1:nlevels))
            allocate (ob%satem(num_satem)%thickness(1:nlevels))
            allocate (ob%satem(num_satem)%org_thickness(1:nlevels))

            ob%satem(num_satem)%p(1) = platform%each(1)%p%inv
            ob%satem(num_satem)%thickness(1) = platform%each(1)%t
            !  write original thickness errors for filtered obs
            if (anal_type_qcobs) then
               do i = 1, nlevels
                  ob%satem(num_satem)%org_thickness(i) = platform%each(i)%t
               end do 
            end if

            ! Splitting the reported Satem data into smaller layers.

            do i = 2, nlevels
               ob%satem(num_satem)%p(i) = platform%each(i)%p%inv
               ob%satem(num_satem)%thickness(i) = platform%each(i)%t
               if (platform%each(i)%t%qc /= missing_data   .and. &
                  platform%each(i-1)%t%qc /= missing_data) then
                  ob%satem(num_satem)%thickness(i)%inv =            &
                  platform%each(i)%t%inv - platform%each(i-1)%t%inv
               else
                  ob%satem(num_satem)%thickness(i)%inv = missing_r
                  ob%satem(num_satem)%thickness(i)%qc  = missing_data
               end if
            end do

            ! Thickness error (m):

            do i = nlevels, 2, -1
               ob%satem(num_satem)%thickness(i)%error = &
               sqrt(ob%satem(num_satem)%thickness(i )%error ** 2 + &
                     ob%satem(num_satem)%thickness(i-1)%error ** 2) / gravity
            end do

            ob%satem(num_satem)%thickness(1)%error = &
                           sqrt(ob%satem(num_satem)%thickness(1)%error ** 2 + &
                           platform%loc%pw%error ** 2) / gravity

            ! Geostatinary ot Polar orbitting Satellite AMVs:

         case (88)    ;

            if (index(platform%info%name, 'MODIS') > 0 .or. &
               index(platform%info%name, 'modis') > 0)  then

               if (n==1) ob%num_polaramv_glo = ob%num_polaramv_glo + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_polaramvObs) cycle reports

               num_polaramv = num_polaramv + 1
               ! Track serial obs index for reassembly of obs during bit-for-bit 
               ! tests with different numbers of MPI tasks.  
               platform%loc%obs_global_index = ob%num_polaramv_glo

               ob%polaramv(num_polaramv)%info = platform%info
               ob%polaramv(num_polaramv)%loc  = platform%loc

               allocate (ob%polaramv(num_polaramv)%p (1:nlevels))
               allocate (ob%polaramv(num_polaramv)%zk(1:nlevels))
               allocate (ob%polaramv(num_polaramv)%u (1:nlevels))
               allocate (ob%polaramv(num_polaramv)%v (1:nlevels))

               do i = 1, nlevels
                  ob%polaramv(num_polaramv)%p(i) = platform%each(i)%p%inv
                  ob%polaramv(num_polaramv)%u(i) = platform%each(i)%u
                  ob%polaramv(num_polaramv)%v(i) = platform%each(i)%v
               end do
            else
               if (n==1) ob%num_geoamv_glo = ob%num_geoamv_glo + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_geoamvObs) cycle reports

               num_geoamv = num_geoamv + 1
               ! Track serial obs index for reassembly of obs during bit-for-bit 
               ! tests with different numbers of MPI tasks.  
               platform%loc%obs_global_index = ob%num_geoamv_glo

               ob%geoamv(num_geoamv)%info = platform%info
               ob%geoamv(num_geoamv)%loc  = platform%loc

               allocate (ob%geoamv(num_geoamv)%p (1:nlevels))
               allocate (ob%geoamv(num_geoamv)%zk(1:nlevels))
               allocate (ob%geoamv(num_geoamv)%u (1:nlevels))
               allocate (ob%geoamv(num_geoamv)%v (1:nlevels))

               do i = 1, nlevels
                  ob%geoamv(num_geoamv)%p(i) = platform%each(i)%p%inv
                  ob%geoamv(num_geoamv)%u(i) = platform%each(i)%u
                  ob%geoamv(num_geoamv)%v(i) = platform%each(i)%v
               end do
            end if

         case (42,96:97) ;

            if (n==1) ob%num_airep_glo = ob%num_airep_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_AirepObs) cycle reports

            num_airep = num_airep + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_airep_glo

            ob%airep(num_airep)%info = platform%info
            ob%airep(num_airep)%loc  = platform%loc

            allocate (ob%airep(num_airep)%h        (1:nlevels))
            allocate (ob%airep(num_airep)%p        (1:nlevels))
            allocate (ob%airep(num_airep)%zk       (1:nlevels))
            allocate (ob%airep(num_airep)%u        (1:nlevels))
            allocate (ob%airep(num_airep)%v        (1:nlevels))
            allocate (ob%airep(num_airep)%t        (1:nlevels))

            do i = 1, nlevels
               ob%airep(num_airep)%h(i) = platform%each(i)%height
               ob%airep(num_airep)%p(i) = platform%each(i)%p%inv
               ob%airep(num_airep)%u(i) = platform%each(i)%u
               ob%airep(num_airep)%v(i) = platform%each(i)%v
               ob%airep(num_airep)%t(i) = platform%each(i)%t
            end do

         case (111, 114) ;

            if (n==1) ob%num_gpspw_glo = ob%num_gpspw_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_GpspwObs) cycle reports

            num_gpspw = num_gpspw + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_gpspw_glo

            ob%gpspw(num_gpspw)%info = platform%info
            ob%gpspw(num_gpspw)%loc  = platform%loc
            ob%gpspw(num_gpspw)%tpw  = platform%loc%pw

         case (116) ;

            if (n==1) ob%num_gpsref_glo = ob%num_gpsref_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_GpsrefObs) cycle reports

            num_gpsref = num_gpsref + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_gpsref_glo

            ob%gpsref(num_gpsref)%info = platform%info
            ob%gpsref(num_gpsref)%loc  = platform%loc

            allocate (ob%gpsref(num_gpsref)%h (1:nlevels))
            allocate (ob%gpsref(num_gpsref)%zk(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%ref(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  p(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  t(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  q(1:nlevels))

            do i = 1, nlevels
               ob%gpsref(num_gpsref)%h(i)   = platform%each(i)%height

               ! In OBSPROC, use "td" field to store "gpsref"
               ob%gpsref(num_gpsref)%ref(i) = platform%each(i)%td
               ! Keep the retrieved p and t (and q) as "field_type":   
               ob%gpsref(num_gpsref)%p(i)   = platform%each(i)%p
               ob%gpsref(num_gpsref)%t(i)   = platform%each(i)%t
               ob%gpsref(num_gpsref)%q(i)   = platform%each(i)%q
            end do

         case (121) ;
            ! SSM/T1 temperatures:

            if (n==1) ob%num_ssmt1_glo = ob%num_ssmt1_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_ssmt1obs) cycle reports

            num_ssmt1 = num_ssmt1 + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_ssmt1_glo

            ob%ssmt1(num_ssmt1)%info = platform%info
            ob%ssmt1(num_ssmt1)%loc  = platform%loc

            allocate (ob%ssmt1(num_ssmt1)%h (1:nlevels))
            allocate (ob%ssmt1(num_ssmt1)%p (1:nlevels))
            allocate (ob%ssmt1(num_ssmt1)%t (1:nlevels))
            allocate (ob%ssmt1(num_ssmt1)%zk(1:nlevels))

            do i = 1, nlevels
              ob%ssmt1(num_ssmt1)%h(i) = platform%each(i)%height
              ob%ssmt1(num_ssmt1)%p(i) = platform%each(i)%p%inv
              ob%ssmt1(num_ssmt1)%t(i) = platform%each(i)%t
            end do
         
         case (122) ;
            ! SSM/T2 relative humidities:

            if (n==1) ob%num_ssmt2_glo = ob%num_ssmt2_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_ssmt2obs) cycle reports

            num_ssmt2 = num_ssmt2 + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_ssmt2_glo

            ob%ssmt2(num_ssmt2)%info = platform%info
            ob%ssmt2(num_ssmt2)%loc  = platform%loc

            allocate (ob%ssmt2(num_ssmt2)%h (1:nlevels))
            allocate (ob%ssmt2(num_ssmt2)%p (1:nlevels))
            allocate (ob%ssmt2(num_ssmt2)%zk(1:nlevels))
            allocate (ob%ssmt2(num_ssmt2)%rh(1:nlevels))

            do i = 1, nlevels
               ob%ssmt2(num_ssmt2)% h(i) = platform%each(i)%height
               ob%ssmt2(num_ssmt2)% p(i) = platform%each(i)%p%inv
               ob%ssmt2(num_ssmt2)%rh(i) = platform%each(i)%rh
            end do

         case (281)    ;
            ! Scatterometer:

            if (n==1) ob%num_qscat_glo = ob%num_qscat_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_qscatobs) cycle reports

            num_qscat  = num_qscat  + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_qscat_glo

            ob%qscat(num_qscat)%info = platform%info
            ob%qscat(num_qscat)%loc  = platform%loc

            ob%qscat(num_qscat)%h = platform%each(1)%height
            ob%qscat(num_qscat)%u = platform%each(1)%u
            ob%qscat(num_qscat)%v = platform%each(1)%v

            ! Impose minimum observation error = 1.0m/s for Quikscat data:
            ob%qscat(num_qscat)%u%error = max(platform%each(1)%u%error,1.0)
            ob%qscat(num_qscat)%v%error = max(platform%each(1)%v%error,1.0)

         case (132) ; ! profiler

         if (n==1) ob%num_profiler_glo = ob%num_profiler_glo + 1
         if (outside) then
            cycle reports
         end if

         if (.not.use_ProfilerObs) cycle reports

         num_profiler = num_profiler + 1
         ! Track serial obs index for reassembly of obs during bit-for-bit 
         ! tests with different numbers of MPI tasks.  
         platform%loc%obs_global_index = ob%num_profiler_glo

         ob%profiler(num_profiler)%info = platform%info
         ob%profiler(num_profiler)%loc  = platform%loc

         allocate (ob%profiler(num_profiler)%p (1:nlevels))
         allocate (ob%profiler(num_profiler)%zk(1:nlevels))
         allocate (ob%profiler(num_profiler)%u (1:nlevels))
         allocate (ob%profiler(num_profiler)%v (1:nlevels))

         do i = 1, nlevels
            ob%profiler(num_profiler)%p(i) = platform%each(i)%p%inv
            ob%profiler(num_profiler)%u(i) = platform%each(i)%u
            ob%profiler(num_profiler)%v(i) = platform%each(i)%v
         end do

         case (135) ; ! Bogus

            if (n==1) ob%num_bogus_glo = ob%num_bogus_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_BogusObs) cycle reports

            num_bogus = num_bogus + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_bogus_glo

            if (num_bogus > max_bogus_input) then
               write(unit=message(1),fmt='(A,I6,A,I6)') &
                  'Bogus #=', num_bogus, ' > max_bogus_input=', max_bogus_input
               call da_error(__FILE__,__LINE__,message(1:1))
            end if

            ob%bogus(num_bogus)%info = platform%info
            ob%bogus(num_bogus)%loc  = platform%loc

            allocate (ob%bogus(num_bogus)%h (1:nlevels))
            allocate (ob%bogus(num_bogus)%p (1:nlevels))
            allocate (ob%bogus(num_bogus)%zk(1:nlevels))
            allocate (ob%bogus(num_bogus)%u (1:nlevels))
            allocate (ob%bogus(num_bogus)%v (1:nlevels))
            allocate (ob%bogus(num_bogus)%t (1:nlevels))
            allocate (ob%bogus(num_bogus)%q (1:nlevels))

            do i = 1, nlevels
              ob%bogus(num_bogus)%h(i) = platform%each(i)%height
              ob%bogus(num_bogus)%p(i) = platform%each(i)%p%inv
              ob%bogus(num_bogus)%u(i) = platform%each(i)%u
              ob%bogus(num_bogus)%v(i) = platform%each(i)%v
              ob%bogus(num_bogus)%t(i) = platform%each(i)%t
              ob%bogus(num_bogus)%q(i) = platform%each(i)%q
            end do

            ob%bogus(num_bogus)%slp    = platform%loc%slp

            if (print_detail_obs) then
              write(unit=stdout,fmt=*) 'nlevels=', nlevels
              write(unit=stdout,fmt=*) 'ob % num_bogus,slp', num_bogus,  &
                       ob % bogus (num_bogus) % slp
               do i=1,nlevels
                  write(unit=stdout,fmt=*) 'ob % num_bogus, i ', num_bogus,i
                  write(unit=stdout,fmt=*) 'ob%bogus(ob%num_bogus)%u,v=',  &
                          ob%bogus(num_bogus)%u(i),ob%bogus(num_bogus)%v(i)
                  write(unit=stdout,fmt=*) 'ob%bogus(ob%num_bogus)%t,q=',  &
                          ob%bogus(num_bogus)%t(i),ob%bogus(num_bogus)%q(i)
               end do
               write(unit=stdout,fmt='(2(a,i4))') 'ob%num_bogus=',num_bogus, &
                                        'nlevels=',nlevels
            end if

         case (18,19) ;             ! buoy

            if (n==1) ob%num_buoy_glo = ob%num_buoy_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_BuoyObs) cycle reports

            num_buoy  = num_buoy  + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit 
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_buoy_glo

            ob%buoy(num_buoy)%info = platform%info
            ob%buoy(num_buoy)%loc  = platform%loc

            ob%buoy(num_buoy)%h = platform%each(1)%height
            ob%buoy(num_buoy)%u = platform%each(1)%u
            ob%buoy(num_buoy)%v = platform%each(1)%v
            ob%buoy(num_buoy)%t = platform%each(1)%t
            ob%buoy(num_buoy)%p = platform%each(1)%p
            ob%buoy(num_buoy)%q = platform%each(1)%q
   
         case (133)    ;         ! AIRS retrievals  

            if (n==1) ob%num_airsr_glo = ob%num_airsr_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_AIRSRETObs) cycle reports

            num_airsr = num_airsr + 1

            platform%loc%obs_global_index = ob%num_airsr_glo

            ob%airsr(num_airsr)%info = platform%info
            ob%airsr(num_airsr)%loc  = platform%loc


            allocate (ob%airsr(num_airsr)%zk(1:nlevels))
            allocate (ob%airsr(num_airsr)%h (1:nlevels))
            allocate (ob%airsr(num_airsr)%p (1:nlevels))
            allocate (ob%airsr(num_airsr)%t (1:nlevels))
            allocate (ob%airsr(num_airsr)%q (1:nlevels))
            do i = 1, nlevels
               ob%airsr(num_airsr)%h(i) = platform%each(i)%height
               ob%airsr(num_airsr)%p(i) = platform%each(i)%p%inv
               ob%airsr(num_airsr)%t(i) = platform%each(i)%t
               ob%airsr(num_airsr)%q(i) = platform%each(i)%q
            end do

         case default;

            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
               'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))

         end select

         if (global .and. n < 2) then
            if (Testing_WRFVAR) exit dup_loop
            if (platform%loc % i >= xp % ide) then
                platform%loc%i = platform%loc % i - xp % ide
            else if (platform%loc % i < xp % ids) then
               platform%loc%i = platform%loc % i + xp % ide
            end if

            platform%loc%proc_domain = .not. platform%loc%proc_domain
         end if
      end do dup_loop
   end do reports

   ob%num_sound = num_sound
   ob%num_synop = num_synop
   ob%num_pilot = num_pilot
   ob%num_satem = num_satem
   ob%num_geoamv = num_geoamv
   ob%num_polaramv = num_polaramv
   ob%num_airep = num_airep
   ob%num_gpspw = num_gpspw
   ob%num_gpsref = num_gpsref
   ob%num_metar = num_metar
   ob%num_ships = num_ships
   ob%num_qscat = num_qscat
   ob%num_buoy  = num_buoy
   ob%num_profiler = num_profiler
   ob%num_bogus = num_bogus
   ob%num_airsr = num_airsr

   ! WHY
   ! ob%num_ssmt1 = num_ssmt1
   ! ob%num_ssmt2 = num_ssmt2
   ! ob%num_ssmi_tb        = num_ssmi_tb
   ! ob%num_ssmi_retrieval = num_ssmi_retrieval

   if (print_detail_obs) then
      write(unit=stdout, fmt='(/a,i6/)') 'ob%current_ob_time=', ob%current_ob_time

      write(unit=stdout, fmt='(5x,a,i6,a)') &
           'Read:  ', num_sound,           ' SOUND          reports,', &
           'Read:  ', num_synop,           ' SYNOP          reports,', &
           'Read:  ', num_pilot,           ' PILOT          reports,', &
           'Read:  ', num_satem,           ' SATEM          reports,', &
           'Read:  ', num_geoamv,          ' Geo AMV        reports,', & 
           'Read:  ', num_polaramv,        ' Polar AMV      reports,', & 
           'Read:  ', num_airep,           ' AIREP          reports,', &
           'Read:  ', num_gpspw ,          ' GPSPW/GPSZD    reports,', &
           'Read:  ', num_gpsref,          ' GPSRF          reports,', &
           'Read:  ', num_metar,           ' METAR          reports,', &
           'Read:  ', num_ships ,          ' SHIP           reports,', &
           'Read:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
           'Read:  ', num_ssmi_tb,         ' SSMI_TB        reports,', &
           'Read:  ', num_ssmt1,           ' SSMT1          reports,', &
           'Read:  ', num_ssmt2,           ' SSMT2          reports,', &
           'Read:  ', num_qscat,           ' QSCAT          reports,', &
           'Read:  ', num_profiler,        ' Profiler       reports,', &
           'Read:  ', num_buoy,            ' Buoy           reports,', &
           'Read:  ', num_bogus,           ' Bogus          reports,', &
           'Read:  ', num_airsr,           ' AIRSRET        reports,', &
           'Read:  ', total_obs,           ' Total reports.', &
           'There are ', total_obs - num_sound - num_synop &
                                   - num_pilot - num_satem &
                                   - num_geoamv - num_polaramv - num_airep &
                                   - num_metar - num_ships &
                                   - num_ssmi_retrieval  &
                                   - num_ssmi_tb - num_ssmt1 - num_ssmt2 &
                                   - num_gpspw - num_gpsref - num_qscat  &
                                   - num_profiler - num_buoy &
                                   - num_bogus - num_airsr , &
                                   '  reports unsaved.'
      write(unit=stdout,fmt=*) ' '
   end if

   if ((ob%ob_numb(ob%current_ob_time)%sound /= ob%num_sound) .or. &
      (ob%ob_numb(ob%current_ob_time)%synop /= ob%num_synop) .or. &
      (ob%ob_numb(ob%current_ob_time)%pilot /= ob%num_pilot) .or. &
      (ob%ob_numb(ob%current_ob_time)%satem /= ob%num_satem) .or. &
      (ob%ob_numb(ob%current_ob_time)%geoamv /= ob%num_geoamv) .or. &
      (ob%ob_numb(ob%current_ob_time)%polaramv /= ob%num_polaramv) .or. &
      (ob%ob_numb(ob%current_ob_time)%airep /= ob%num_airep) .or. &
      (ob%ob_numb(ob%current_ob_time)%gpspw /= ob%num_gpspw) .or. &
      (ob%ob_numb(ob%current_ob_time)%gpsref /= ob%num_gpsref) .or. &
      (ob%ob_numb(ob%current_ob_time)%metar /= ob%num_metar) .or. &
      (ob%ob_numb(ob%current_ob_time)%ships /= ob%num_ships) .or. &
      (ob%ob_numb(ob%current_ob_time)%qscat /= ob%num_qscat) .or. &
      (ob%ob_numb(ob%current_ob_time)%buoy  /= ob%num_buoy) .or. &
      (ob%ob_numb(ob%current_ob_time)%bogus /= ob%num_bogus) .or. &
      (ob%ob_numb(ob%current_ob_time)%ssmt2 /= ob%num_ssmt1) .or. &
      (ob%ob_numb(ob%current_ob_time)%ssmt2 /= ob%num_ssmt2) .or. &
      (ob%ob_numb(ob%current_ob_time)%airsr /= ob%num_airsr) .or. &
      (ob%ob_numb(ob%current_ob_time)%profiler /= ob%num_profiler)) then

      write(unit=stderr, fmt='(a, i6, 2x, a, i6)') &
         'ob%ob_numb(ob%current_ob_time)%sound=', &
         ob%ob_numb(ob%current_ob_time)%sound, &
         'ob%num_sound=', ob%num_sound, &
         'ob%ob_numb(ob%current_ob_time)%synop=', &
         ob%ob_numb(ob%current_ob_time)%synop, &
         'ob%num_synop=', ob%num_synop, &
         'ob%ob_numb(ob%current_ob_time)%pilot=', &
         ob%ob_numb(ob%current_ob_time)%pilot, &
         'ob%num_pilot=', ob%num_pilot, &
         'ob%ob_numb(ob%current_ob_time)%satem=', &
         ob%ob_numb(ob%current_ob_time)%satem, &
         'ob%num_satem=', ob%num_satem, &
         'ob%ob_numb(ob%current_ob_time)%geoamv=', &
         ob%ob_numb(ob%current_ob_time)%geoamv, &
         'ob%num_geoamv=', ob%num_geoamv, &
         'ob%ob_numb(ob%current_ob_time)%polaramv=', &
         ob%ob_numb(ob%current_ob_time)%polaramv, &
         'ob%num_polaramv=', ob%num_polaramv, &
         'ob%ob_numb(ob%current_ob_time)%airep=', &
         ob%ob_numb(ob%current_ob_time)%airep, &
         'ob%num_airep=', ob%num_airep, &
         'ob%ob_numb(ob%current_ob_time)%gpspw=', &
         ob%ob_numb(ob%current_ob_time)%gpspw, &
         'ob%num_gpspw=', ob%num_gpspw, &
         'ob%ob_numb(ob%current_ob_time)%gpsref=', &
         ob%ob_numb(ob%current_ob_time)%gpsref,&
         'ob%num_gpsref=', ob%num_gpsref, &
         'ob%ob_numb(ob%current_ob_time)%metar=', &
         ob%ob_numb(ob%current_ob_time)%metar, &
         'ob%num_metar=', ob%num_metar, &
         'ob%ob_numb(ob%current_ob_time)%ships=', &
         ob%ob_numb(ob%current_ob_time)%ships, &
         'ob%num_ships=', ob%num_ships, &
         'ob%ob_numb(ob%current_ob_time)%qscat=', &
         ob%ob_numb(ob%current_ob_time)%qscat, &
         'ob%num_qscat=', ob%num_qscat, &
         'ob%ob_numb(ob%current_ob_time)%buoy =', &
         ob%ob_numb(ob%current_ob_time)%buoy , &
         'ob%num_buoy =', ob%num_buoy , &
         'ob%ob_numb(ob%current_ob_time)%bogus=', &
         ob%ob_numb(ob%current_ob_time)%bogus, &
         'ob%num_bogus=', ob%num_bogus, &
         'ob%ob_numb(ob%current_ob_time)%ssmt1=', &
         ob%ob_numb(ob%current_ob_time)%ssmt1, &
         'ob%num_ssmt1=', ob%num_ssmt1, &
         'ob%ob_numb(ob%current_ob_time)%ssmt2=', &
         ob%ob_numb(ob%current_ob_time)%ssmt2, &
         'ob%num_ssmt2=', ob%num_ssmt2, &
         'ob%ob_numb(ob%current_ob_time)%airsr=', &
         ob%ob_numb(ob%current_ob_time)%airsr, &
         'ob%num_airsr=', ob%num_airsr, &
         'ob%ob_numb(ob%current_ob_time)%profiler=', &
         ob%ob_numb(ob%current_ob_time)%profiler, &
         'ob%num_profiler=', ob%num_profiler

      call da_error(__FILE__,__LINE__,(/"Obs mismatch"/))
   end if

   ! WHY
   ! if ((ob%ob_numb(ob%current_ob_time)%ssmi_tb        /= ob%num_ssmi_tb) .or. &
   !     (ob%ob_numb(ob%current_ob_time)%ssmi_retrieval /= ob%num_ssmi_retrieval)) then
   !    write(unit=message(1), fmt='(a, i6, 2x, a, i6)') &
   !       'ob%ob_numb(ob%current_ob_time)%ssmi_tb       =', ob%ob_numb(ob%current_ob_time)%ssmi_tb, &
   !       'ob%num_ssmi_tb       =', ob%num_ssmi_tb, &
   !       'ob%ob_numb(ob%current_ob_time)%ssmi_retrieval=', ob%ob_numb(ob%current_ob_time)%ssmi_retrieval, &
   !       'ob%num_ssmi_retrieval=', ob%num_ssmi_retrieval
   !    call da_error(__FILE__,__LINE,message(1:1))
   ! end if

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs")

end subroutine da_read_obs


