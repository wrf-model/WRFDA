SUBROUTINE DA_Read_Obs (iunit, ob, xp, filename)
!-----------------------------------------------------------------------------!
!  Purpose: Read the header of a MM5 3D-VAR 2.0 GTS observation file
!
!  History:
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
!           Writing original errors & thicknesses on unit 193
!           desired for outputting QC obs with NTMAX = 0
!-----------------------------------------------------------------------------!

   implicit none

   INTEGER,           INTENT (in)   :: iunit
   TYPE (xpose_type), INTENT(IN)    :: xp    ! Domain decomposition vars.
   TYPE (ob_type),    INTENT(inout) :: ob

   CHARACTER(*), INTENT(in), OPTIONAL :: filename

   CHARACTER (LEN =  10)        :: fmt_name

   CHARACTER (LEN = 160)        :: info_string

   CHARACTER (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, & 
                                   fmt_each

   INTEGER                      :: i, j, iost, nlevels, fm

   TYPE (multi_level_type)      :: platform

   LOGICAL                      :: inside_halo
   LOGICAL                      :: outside
   INTEGER                      :: surface_level
   REAL                         :: height_error, u_comp, v_comp
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
                                   num_ssmt2, &
                                   num_pseudo, num_qscat, &
                                   num_profiler, num_buoy, num_bogus

   INTEGER                        :: ndup, n
   LOGICAL                        :: surface_sound

!--------------------------------------------------------------------

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
   num_pseudo = 0
!----------------------------------------------------------------------------!
!  OPEN FILE
!  ---------

   if(present(filename)) then
      OPEN(UNIT   = iunit,     &
           FILE   = trim(filename), &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/A,I3,3(2X,A)/)') &
              'ERROR IN OBS INPUT FILE UNIT ',iunit, &
              'OBS FILENAME:', trim(filename), &
              'FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         RETURN
      ENDIF
   else
#ifndef crayx1
      OPEN(UNIT   = iunit,     &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/A,I3,2X,a,i3/A/)') &
              'ERROR IN OBS INPUT FILE UNIT ',iunit, 'iost=', iost, &
              'FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         RETURN
      ENDIF
#endif
   endif

!  READ HEADER
!  ===========

   head_info: DO
 
      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) info_string

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3/a/)') &
              'ERROR READING FILE UNIT:', iunit, &
              'in read_obs_gts_header.F' 
         return
      ENDIF

      if (info_string(1:6) == 'EACH  ') EXIT

   ENDDO head_info

!----------------------------------------------------------------------------!

!  READ FORMATS
!  ------------

   READ (iunit, FMT = '(A,1X,A)', IOSTAT = iost) &
        fmt_name, fmt_info, &
        fmt_name, fmt_loc,  &
        fmt_name, fmt_each

   IF (iost /= 0) THEN
      write(unit=*, fmt='(/A,I3,A/)') &
           ' ERROR IN INPUT FILE UNIT ',iunit, &
           ' FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
      RETURN
   ENDIF

   if(print_detail > 500) then
      write(unit=*, fmt='(2a)') &
           'fmt_info=', fmt_info, &
           'fmt_loc =', fmt_loc,  &
           'fmt_each=', fmt_each
   endif

!----------------------------------------------------------------------------!

!  SKIP LINE
!  ----------

   READ (iunit, FMT = '(A)') fmt_name

!  LOOP OVER RECORDS
!  -----------------

   reports: &
   DO
!     READ STATION GENERAL INFO
!     =============================

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
         WRITE (0,'(/A,I9)') ' END OF OBS UNIT: ',iunit
         WRITE (0,'(/A,I9)') ' IOSTAT:          ',iost
         EXIT reports
      ENDIF

      if(print_detail > 500) then
         write(unit=*, fmt=fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id
      endif

      if(platform%info%lon == 180.0   ) platform%info%lon =-180.000 
!-----Fix funny wind direction at Poles
      if(platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999 ) then
         platform%info%lon = 0.0
      endif

      if(platform%info%platform(6:6) == ' ') then
         read(platform%info%platform(4:5), '(I2)') fm
      else
         read(platform%info%platform(4:6), '(I3)') fm
      endif

!     READ MODEL LOCATION
!     =========================

      READ (iunit, FMT = fmt_loc) platform%loc%slp, platform%loc%pw

      total_obs = total_obs + 1

! == levels < 1 and not GPSPW, go back to reports

      IF ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
           cycle reports
      ENDIF

!     READ EACH LEVELS
!     ----------------

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

      if(print_detail > 500) then
         write(unit=*, FMT = '( a, i6)') 'i=', i
         write(unit=*, FMT = TRIM (fmt_each)) &
              platform%each (i)%p,            &
              platform%each (i)%speed,        &
              platform%each (i)%v,            &
              platform%each (i)%height,       &
              platform%each (i)%height_qc,    &
              height_error,                   &
              platform%each (i)%t,            &
              platform%each (i)%td,           &
              platform%each (i)%rh
      endif

! Uncomment the following if errors in reported heights (test later):             
!      if(fm == 97 .or. fm == 96 .or. fm == 42) then
!         platform%each (i)%height_qc = -5 ! Aircraft heights are not above surface.
!      end if

      ! To convert the wind speed and direction to 
      !      the model wind components: u, v

       if (platform%each (i)%speed%qc /= missing_data .and. &
           platform%each (i)%v%qc /= missing_data) then

           call FFDDUV (platform%each (i)%speed%inv, &
                        platform%each (i)%v%inv,     &
                        U_comp, V_comp, platform%info%lon, 1)
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
       endif

      ENDDO loop_level

      call DA_ll_to_xy (platform%info, platform%loc,   &
                        xp, outside, inside_halo )

      IF(outside) THEN
         cycle reports
      ENDIF

      nlevels = platform%info%levels
      if( nlevels > max_ob_levels) then
         nlevels = max_ob_levels

         write(unit=*, fmt='(/a/)') &
              'WARNING   WARNING   WARNING   WARNING   WARNING :'

         write(unit=*, fmt='(/4(2a,2x),a,2f8.2,2x,2(a,f9.2,2x),2(a,i4,2x)/)') &
              'Station: ', trim(platform%info%name), &
              'ID: ', trim(platform%info%id), &
              'Platfrom: ', trim(platform%info%platform), &
              'Date: ', trim(platform%info%date_char), &
              'At Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
              'At elv: ', platform%info%elv, &
              'with pstar: ', platform%info%pstar, &
              'Has level: ', platform%info%levels, &
              'which is great than max_ob_levels: ', max_ob_levels

             write (unit=*, FMT = '(A,1X,A,1X,A,1X,I4,2f9.3,f9.1,1X,A)') &
             platform%info%name,        &
             platform%info%platform,    &
             platform%info%date_char,   &
             platform%info%levels,      &
             platform%info%lat,         &
             platform%info%lon,         &
             platform%info%elv,         &
             platform%info%id

         platform%info%levels = nlevels
      else if( nlevels < 1) then
      ! Not GPSPW and GPSZD data:
         if ( fm /= 111 .and. fm /= 114 ) then
            cycle reports
         endif
      endif

      if( fm /= 86) CALL DA_Obs_Proc_Station(platform)
      nlevels = platform%info%levels
!    Loop over duplicating obs for global
     ndup = 1
     if(global .and. &
     (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

     if( print_detail >= 10 .and. ndup > 1) then
         write(unit=*, FMT = fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id

         write(unit=*, FMT = '(a,2i5,4e20.10)') &
          ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
          platform%loc%i,  platform%loc%j,   &
          platform%loc%dx, platform%loc%dxm, &
          platform%loc%dy, platform%loc%dym

    end if

   dup_loop: DO n = 1, ndup

      SELECT CASE(fm)

!     if(index(platform%info%source, 'SYNOP') > 0) then

         CASE (12) ;

         if(.not.use_SynopObs) cycle reports

         num_synop = num_synop + 1

         ob%synop(num_synop)%info = platform%info
         ob%synop(num_synop)%loc  = platform%loc
         
         ob%synop(num_synop)%h = platform%each(1)%height
         ob%synop(num_synop)%u = platform%each(1)%u
         ob%synop(num_synop)%v = platform%each(1)%v
         ob%synop(num_synop)%t = platform%each(1)%t
         ob%synop(num_synop)%q = platform%each(1)%q
         ob%synop(num_synop)%p = platform%each(1)%p

!     else if(index(platform%info%source, 'SHIP') > 0 .or. &
!             index(platform%info%source, 'BOUY') > 0) then

         CASE (13, 17) ;                  ! ships          

         if(.not.use_ShipsObs) cycle reports

         num_ships  = num_ships  + 1

         ob%ships(num_ships)%info = platform%info
         ob%ships(num_ships)%loc  = platform%loc
         
         ob%ships(num_ships)%h = platform%each(1)%height
         ob%ships(num_ships)%u = platform%each(1)%u
         ob%ships(num_ships)%v = platform%each(1)%v
         ob%ships(num_ships)%t = platform%each(1)%t
         ob%ships(num_ships)%p = platform%each(1)%p
         ob%ships(num_ships)%q = platform%each(1)%q

!     else if(index(platform%info%source, 'METAR') > 0) then

         CASE (15:16) ;

         if(.not.use_MetarObs) cycle reports

         num_metar = num_metar + 1

         ob%metar(num_metar)%info = platform%info
         ob%metar(num_metar)%loc  = platform%loc

         ob%metar(num_metar)%h = platform%each(1)%height
         ob%metar(num_metar)%u = platform%each(1)%u
         ob%metar(num_metar)%v = platform%each(1)%v
         ob%metar(num_metar)%t = platform%each(1)%t
         ob%metar(num_metar)%p = platform%each(1)%p
         ob%metar(num_metar)%q = platform%each(1)%q

!     else if(index(platform%info%source, 'PILOT') > 0) then

         CASE (32:34) ;

         if(.not.use_PilotObs) cycle reports

         num_pilot = num_pilot + 1

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
         enddo

!     else if(index(platform%info%source, 'FM-35 TEMP') > 0) then

         CASE (35:38) ;

         if(.not.use_SoundObs) cycle reports

         num_sound = num_sound + 1

         ob%sonde_sfc(num_sound)%info = platform%info
         ob%sonde_sfc(num_sound)%loc  = platform%loc

!--------Search to see if we have surface obs.

         surface_level = 0

         do i = 1, nlevels
!-----------if(elevation and height are the same, it is surface.)
            if(abs(platform%info%elv - platform%each(i)%height) < 0.1) then
               surface_level = i

!--------------Save surface pressure.
               ob%sonde_sfc(num_sound)%h = platform%each(i)%height
               ob%sonde_sfc(num_sound)%u = platform%each(i)%u
               ob%sonde_sfc(num_sound)%v = platform%each(i)%v
               ob%sonde_sfc(num_sound)%t = platform%each(i)%t
               ob%sonde_sfc(num_sound)%q = platform%each(i)%q
               ob%sonde_sfc(num_sound)%p = platform%each(i)%p

               exit
            endif
         enddo

! ............................
!        processing the sound_sfc data:

         ob%sound(num_sound)%info = platform%info
         ob%sound(num_sound)%loc  = platform%loc

         if(surface_level > 0) then
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
         endif

         if(nlevels < 1) then
              ob%sound(num_sound)%info%levels = 0
              cycle reports
         endif

         allocate (ob%sound(num_sound)%h (1:nlevels))
         allocate (ob%sound(num_sound)%p (1:nlevels))
         allocate (ob%sound(num_sound)%zk(1:nlevels))
         allocate (ob%sound(num_sound)%u (1:nlevels))
         allocate (ob%sound(num_sound)%v (1:nlevels))
         allocate (ob%sound(num_sound)%t (1:nlevels))
         allocate (ob%sound(num_sound)%q (1:nlevels))

         j = 0
         do i = 1, ob%sound(num_sound)%info%levels
           if(i == surface_level) cycle

           j=j+1

           ob%sound(num_sound)%h(j) = platform%each(i)%height
           ob%sound(num_sound)%p(j) = platform%each(i)%p%inv
           ob%sound(num_sound)%u(j) = platform%each(i)%u
           ob%sound(num_sound)%v(j) = platform%each(i)%v
           ob%sound(num_sound)%t(j) = platform%each(i)%t
           ob%sound(num_sound)%q(j) = platform%each(i)%q
         enddo

         ob%sound(num_sound)%info%levels = nlevels

!     else if(index(platform%info%source, 'SATEM') > 0) then

         CASE (86)    ;

         if(.not.use_SatemObs) cycle reports

!--------Reject cloudy Satem obs.

         if(platform%loc%pw%inv > 10.) then
            cycle reports
         endif

         num_satem = num_satem + 1

         ob%satem(num_satem)%info = platform%info
         ob%satem(num_satem)%loc  = platform%loc

!        The Satem ref_p is put in the SLP position in OBSPROC data stream.

         ob%satem(num_satem)%ref_p= platform%loc%slp%inv

         allocate (ob%satem(num_satem)%p        (1:nlevels))
         allocate (ob%satem(num_satem)%thickness(1:nlevels))

         ob%satem(num_satem)%p(1) = platform%each(1)%p%inv
         ob%satem(num_satem)%thickness(1) = platform%each(1)%t
!  write original thickness errors for filtered obs on 193 unit 
         if(analysis_type(1:6)=='QC-OBS' .or. analysis_type(1:6) == 'qc-obs') then
         do i = 1, nlevels
         write(193,'(i8,2x,i8,2f12.3)') &
         num_satem,i,platform%each(i)%t%inv,platform%each(i)%t%error 
         end do 
         endif
!        Splitting the reported Satem data into smaller layers.

         do i = 2, nlevels
           ob%satem(num_satem)%p(i) = platform%each(i)%p%inv
           ob%satem(num_satem)%thickness(i) = platform%each(i)%t

           if(platform%each(i)%t%qc /= missing_data   .and. &
              platform%each(i-1)%t%qc /= missing_data) then
              ob%satem(num_satem)%thickness(i)%inv =            &
              platform%each(i)%t%inv - platform%each(i-1)%t%inv
           else
              ob%satem(num_satem)%thickness(i)%inv = missing_r
              ob%satem(num_satem)%thickness(i)%qc  = missing_data
           endif
         enddo
   
!--------Thickness error (m):

         do i = nlevels, 2, -1
            ob%satem(num_satem)%thickness(i)%error = &
            sqrt( ob%satem(num_satem)%thickness(i  )%error ** 2 + &
                  ob%satem(num_satem)%thickness(i-1)%error ** 2) / gravity
         end do

         ob%satem(num_satem)%thickness(1)%error = &
                        sqrt( ob%satem(num_satem)%thickness(1)%error ** 2 + &
                        platform%loc%pw%error ** 2) / gravity


!--------Geostatinary ot Polar orbitting Satellite AMVs:

         CASE (88)    ;

      if(index(platform%info%name, 'MODIS') > 0 .or. &
         index(platform%info%name, 'modis') > 0 )  then
         if(.not.use_polaramvObs) cycle reports
   
         num_polaramv = num_polaramv + 1
        
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
         if(.not.use_geoamvObs) cycle reports

         num_geoamv = num_geoamv + 1

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
         enddo
!
      end if
!
!     else if(index(platform%info%source, 'AIREP') > 0) then

         CASE (42,96:97) ;

         if(.not.use_AirepObs) cycle reports

         num_airep = num_airep + 1

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
         enddo

!     else if(index(platform%info%source, 'GPSPW') > 0) then
        ! GPSPW(111), GPSZD(114):
         CASE (111, 114) ;
         
         if(.not.use_GpspwObs) cycle reports

         num_gpspw = num_gpspw + 1

         ob%gpspw(num_gpspw)%info = platform%info
         ob%gpspw(num_gpspw)%loc  = platform%loc
         ob%gpspw(num_gpspw)%tpw  = platform%loc%pw

!     else if(index(platform%info%source, 'GPSRF') > 0) then
         CASE (116) ;
         if(.not.use_GpsrefObs) cycle reports

         num_gpsref = num_gpsref + 1

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

! ==> In 3DVAR_OBSPROC, use "td" field to store "gpsref" (Y.-R. Guo):
           ob%gpsref(num_gpsref)%ref(i) = platform%each(i)%td
! ==> Keep the retrieved p and t (and q) as "field_type":   
           ob%gpsref(num_gpsref)%p(i)   = platform%each(i)%p
           ob%gpsref(num_gpsref)%t(i)   = platform%each(i)%t
           ob%gpsref(num_gpsref)%q(i)   = platform%each(i)%q
         enddo

!        SSM/T1 temperatures:
         CASE (121) ;

         if(.not.use_ssmt1obs) cycle reports

         num_ssmt1 = num_ssmt1 + 1

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
         enddo
         
!        SSM/T2 relative humidities:
         CASE (122) ;

         if(.not.use_ssmt2obs) cycle reports

         num_ssmt2 = num_ssmt2 + 1

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
         enddo

!        Scatterometer:

         CASE (281)    ;

         if(.not.use_qscatobs) cycle reports

         num_qscat  = num_qscat  + 1

         ob%qscat(num_qscat)%info = platform%info
         ob%qscat(num_qscat)%loc  = platform%loc
         
         ob%qscat(num_qscat)%h = platform%each(1)%height
         ob%qscat(num_qscat)%u = platform%each(1)%u
         ob%qscat(num_qscat)%v = platform%each(1)%v

!!!DALE  Impose minimum observation error = 1.0m/s for Quikscat data:
         ob%qscat(num_qscat)%u%error = max(platform%each(1)%u%error,1.0)
         ob%qscat(num_qscat)%v%error = max(platform%each(1)%v%error,1.0)
!!!DALE 

         CASE (132) ;

         if(.not.use_ProfilerObs) cycle reports

         num_profiler = num_profiler + 1

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
         enddo

!     else if(index(platform%info%source, 'TCBGS') > 0) then

         CASE (135) ;

         if(.not.use_BogusObs) cycle reports

         num_bogus = num_bogus + 1

         if( num_bogus > max_bogus_input) then
                  write(0,'(A,I6,A,I6)') 'Bogus #=', num_bogus, &
                         ' > max_bogus_input=', max_bogus_input
                  STOP
         endif

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
         enddo

         ob%bogus(num_bogus)%slp    = platform%loc%slp

         if ( print_detail > 0) then
            print*,'nlevels=', nlevels
            print*,'ob % num_bogus,slp', num_bogus,  &
                    ob % bogus ( num_bogus ) % slp
  	    do i=1,nlevels
               print*,'ob % num_bogus, i ', num_bogus,i
               print*,'ob%bogus(ob%num_bogus)%u,v=',  &
                       ob%bogus(num_bogus)%u(i),ob%bogus(num_bogus)%v(i)
               print*,'ob%bogus(ob%num_bogus)%t,q=',  &
                       ob%bogus(num_bogus)%t(i),ob%bogus(num_bogus)%q(i)
            enddo
            write(*,fmt='(2(a,i4))') 'ob%num_bogus=',num_bogus, &
                                     'nlevels=',nlevels
         end if

         CASE (18,19) ;             ! bouy

         if(.not.use_BuoyObs) cycle reports

         num_buoy  = num_buoy  + 1

         ob%buoy(num_buoy)%info = platform%info
         ob%buoy(num_buoy)%loc  = platform%loc

         ob%buoy(num_buoy)%h = platform%each(1)%height
         ob%buoy(num_buoy)%u = platform%each(1)%u
         ob%buoy(num_buoy)%v = platform%each(1)%v
         ob%buoy(num_buoy)%t = platform%each(1)%t
         ob%buoy(num_buoy)%p = platform%each(1)%p
         ob%buoy(num_buoy)%q = platform%each(1)%q

         CASE DEFAULT;

            write(unit=*, fmt='(/a/)') &
                 'WARNING   WARNING   WARNING   WARNING   WARNING :'

            write(unit=*, fmt='(a)') 'unsaved obs found:'

            write(unit=*, fmt='(2a)') &
                 'platform%info%platform=', platform%info%platform
 
            write(unit=*, fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels

      END SELECT

     if( global .and. n < 2 ) then
      if( Testing_3DVAR ) exit dup_loop
      if( platform%loc % i >= xp % ide) then
         platform%loc%i = platform%loc % i - xp % ide
      else if( platform%loc % i < xp % ids) then
         platform%loc%i = platform%loc % i + xp % ide
      end if

      platform%loc%proc_domain = .not. platform%loc%proc_domain
     end if

    ENDDO dup_loop

   ENDDO reports

!------------------------------------------------------------------------
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

!  ob%num_ssmt1 = num_ssmt1
!  ob%num_ssmt2 = num_ssmt2
!  ob%num_ssmi_tb        = num_ssmi_tb
!  ob%num_ssmi_retrieval = num_ssmi_retrieval

!------------------------------------------------------------------------

!  PRINT OUT
!  =============

   if(print_detail > 0) then

      write(6, fmt='(/a,i6/)') 'ob%current_ob_time=', ob%current_ob_time

      write(6, fmt='(5x,a,i6,a)') &
           'Read:  ', num_sound, ' SOUND       reports,', &
           'Read:  ', num_synop, ' SYNOP       reports,', &
           'Read:  ', num_pilot, ' PILOT       reports,', &
           'Read:  ', num_satem, ' SATEM       reports,', &
           'Read:  ', num_geoamv, ' Geo AMV     reports,', & 
           'Read:  ', num_polaramv,' Polar AMV     reports,', & 
           'Read:  ', num_airep, ' AIREP       reports,', &
           'Read:  ', num_gpspw , ' GPSPW/GPSZD reports,', &
           'Read:  ', num_gpsref, ' GPSRF       reports,', &
           'Read:  ', num_metar, ' METAR       reports,', &
           'Read:  ', num_ships , ' SHIP       reports,', &
           'Read:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
           'Read:  ', num_ssmi_tb,         ' SSMI_TB        reports,', &
           'Read:  ', num_ssmt1, ' SSMT1       reports,', &
           'Read:  ', num_ssmt2, ' SSMT2       reports,', &
           'Read:  ', num_qscat, ' QSCAT       reports,', &
           'Read:  ', num_profiler, ' Profiler reports,', &
           'Read:  ', num_buoy, ' Buoy         reports,', &
           'Read:  ', num_bogus, ' Bogus       reports,', &
           'Read:  ', total_obs, ' Total Observations.', &
           'There are ', total_obs - num_sound - num_synop &
                                   - num_pilot - num_satem &
                                   - num_geoamv - num_polaramv - num_airep &
                                   - num_metar - num_ships &
                                   - num_ssmi_retrieval  &
                                   - num_ssmi_tb - num_ssmt1 - num_ssmt2 &
                                   - num_gpspw - num_gpsref - num_qscat  &
                                   - num_profiler - num_buoy, &
                                   - num_bogus - num_bogus, &
                                   '  Observations unsaved.'

   end if

   if((ob%ob_numb(ob%current_ob_time)%sound /= ob%num_sound) .or. &
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
      (ob%ob_numb(ob%current_ob_time)%profiler /= ob%num_profiler)) then

      write(unit=*, fmt='(a, i6, 2x, a, i6)') &
           'ob%ob_numb(ob%current_ob_time)%sound=', ob%ob_numb(ob%current_ob_time)%sound, &
           'ob%num_sound=', ob%num_sound, &
           'ob%ob_numb(ob%current_ob_time)%synop=', ob%ob_numb(ob%current_ob_time)%synop, &
           'ob%num_synop=', ob%num_synop, &
           'ob%ob_numb(ob%current_ob_time)%pilot=', ob%ob_numb(ob%current_ob_time)%pilot, &
           'ob%num_pilot=', ob%num_pilot, &
           'ob%ob_numb(ob%current_ob_time)%satem=', ob%ob_numb(ob%current_ob_time)%satem, &
           'ob%num_satem=', ob%num_satem, &
           'ob%ob_numb(ob%current_ob_time)%geoamv=', ob%ob_numb(ob%current_ob_time)%geoamv, &
           'ob%num_geoamv=', ob%num_geoamv, &
           'ob%ob_numb(ob%current_ob_time)%polaramv=', ob%ob_numb(ob%current_ob_time)%polaramv, &
           'ob%num_polaramv=', ob%num_polaramv, &
           'ob%ob_numb(ob%current_ob_time)%airep=', ob%ob_numb(ob%current_ob_time)%airep, &
           'ob%num_airep=', ob%num_airep, &
           'ob%ob_numb(ob%current_ob_time)%gpspw=', ob%ob_numb(ob%current_ob_time)%gpspw, &
           'ob%num_gpspw=', ob%num_gpspw, &
           'ob%ob_numb(ob%current_ob_time)%gpsref=', ob%ob_numb(ob%current_ob_time)%gpsref,&
           'ob%num_gpsref=', ob%num_gpsref, &
           'ob%ob_numb(ob%current_ob_time)%metar=', ob%ob_numb(ob%current_ob_time)%metar, &
           'ob%num_metar=', ob%num_metar, &
           'ob%ob_numb(ob%current_ob_time)%ships=', ob%ob_numb(ob%current_ob_time)%ships, &
           'ob%num_ships=', ob%num_ships, &
           'ob%ob_numb(ob%current_ob_time)%qscat=', ob%ob_numb(ob%current_ob_time)%qscat, &
           'ob%num_qscat=', ob%num_qscat, &
           'ob%ob_numb(ob%current_ob_time)%buoy =', ob%ob_numb(ob%current_ob_time)%buoy , &
           'ob%num_buoy =', ob%num_buoy , &
           'ob%ob_numb(ob%current_ob_time)%bogus=', ob%ob_numb(ob%current_ob_time)%bogus, &
           'ob%num_bogus=', ob%num_bogus, &
           'ob%ob_numb(ob%current_ob_time)%ssmt1=', ob%ob_numb(ob%current_ob_time)%ssmt1, &
           'ob%num_ssmt1=', ob%num_ssmt1, &
           'ob%ob_numb(ob%current_ob_time)%ssmt2=', ob%ob_numb(ob%current_ob_time)%ssmt2, &
           'ob%num_ssmt2=', ob%num_ssmt2, &
           'ob%ob_numb(ob%current_ob_time)%profiler=', ob%ob_numb(ob%current_ob_time)%profiler, &
           'ob%num_profiler=', ob%num_profiler

      stop 'Obs mismatch.'
   endif

!  if((ob%ob_numb(ob%current_ob_time)%ssmi_tb        /= ob%num_ssmi_tb) .or. &
!     (ob%ob_numb(ob%current_ob_time)%ssmi_retrieval /= ob%num_ssmi_retrieval)) then
!     write(unit=*, fmt='(a, i6, 2x, a, i6)') &
!          'ob%ob_numb(ob%current_ob_time)%ssmi_tb       =', ob%ob_numb(ob%current_ob_time)%ssmi_tb, &
!          'ob%num_ssmi_tb       =', ob%num_ssmi_tb, &
!          'ob%ob_numb(ob%current_ob_time)%ssmi_retrieval=', ob%ob_numb(ob%current_ob_time)%ssmi_retrieval, &
!          'ob%num_ssmi_retrieval=', ob%num_ssmi_retrieval
!     stop 'Obs mismatch.'
!  endif

END SUBROUTINE DA_Read_Obs
