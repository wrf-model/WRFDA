SUBROUTINE DA_Count_Filtered_Obs (iunit, xp)
!-----------------------------------------------------------------------------!
!  Purpose: Scans intermediate Filtered Obs file, 
!           counts various obs type and writes final filtered Obs 
!-----------------------------------------------------------------------------!
! Original code:      10/18/2005            Syed RH Rizvi
!
!-----------------------------------------------------------------------------!
   implicit none
   INTEGER,         INTENT (in)  :: iunit
   TYPE (xpose_type), INTENT(IN) :: xp           ! Domain decomposition vars.
   CHARACTER (LEN =  10)        :: fmt_name
   CHARACTER (LEN = 160)        :: info_string
   CHARACTER (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, & 
                                   fmt_each
   INTEGER                      :: i, j, iost, fm
   TYPE (multi_level_type)      :: platform
   LOGICAL                      :: inside_halo
   LOGICAL                      :: outside
   REAL                         :: height_error

   INTEGER                      :: total_obs, &
                                   num_synop, &
                                   num_metar, &
                                   num_ships, &
                                   num_buoy, &
                                   num_sound, &
                                   num_airep, &
                                   num_pilot, &
                                   num_geoamv, &
                                   num_polaramv, &
                                   num_gpspw, &
                                   num_gpsref, &
                                   num_profiler, &          
                                   num_qscat, &
                                   num_ssmt1, &
                                   num_ssmt2, &
                                   num_satem, &
                                   num_bogus, &
                                   num_pseudo

   INTEGER                        :: ounit     
   CHARACTER (LEN =  80)          :: blank
   logical                        :: first_time
!----------------------------------------------------------------------------!
  ounit = filtered_obs_iunit 
   first_time = .true.
!
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
   num_ssmt1 = 0
   num_ssmt2 = 0
   num_pseudo = 0
   num_qscat = 0
   num_profiler = 0
   num_buoy = 0
   num_bogus = 0
!----------------------------------------------------------------------------!

! READ HEADER
! ===========
   head_info: DO
 
      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) info_string
      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3)') &
        '1. Error in reading intermediate Filtered Obs Unit :', iunit
          stop   
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
      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3)') &
        '2. Error in reading intermediate Filtered Obs Unit :', iunit
          stop   
      ENDIF
   ENDIF

!----------------------------------------------------------------------------!
!  SKIP UNITS
!  ----------

   READ (iunit, FMT = '(A80)') blank    
1000      continue
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
         if( first_time) then
         EXIT reports
         else
         return
         endif
      ENDIF
      if( .not. first_time )&
       write(ounit, FMT = fmt_info)           &
                   platform%info%platform,    &
                   platform%info%date_char,   &
                   platform%info%name,        &
                   platform%info%levels,      &
                   platform%info%lat,         &
                   platform%info%lon,         &
                   platform%info%elv,         &
                   platform%info%id

!     READ MODEL LOCATION
!     =========================

      READ (iunit, FMT = fmt_loc) platform%loc%slp, platform%loc%pw
      if( .not. first_time )&
      write(ounit, FMT = fmt_loc) platform%loc%slp, platform%loc%pw

! == levels < 1 and not GPSPW, go back to reports

      IF ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
           cycle reports
      ENDIF

     if( first_time) then
      read(platform%info%platform(4:6), '(I3)') fm
      total_obs = total_obs + 1
     endif
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
              platform%each (i)%v,            &
              platform%each (i)%height,       &
              platform%each (i)%height_qc,    &
              height_error,                   &
              platform%each (i)%t,            &
              platform%each (i)%td,           &
              platform%each (i)%rh
      if( .not. first_time )&
       write (UNIT = ounit, FMT = TRIM (fmt_each)) &
              platform%each (i)%p,            &
              platform%each (i)%speed,        &
              platform%each (i)%v,            &
              platform%each (i)%height,       &
              platform%each (i)%height_qc,    &
              height_error,                   &
              platform%each (i)%t,            &
              platform%each (i)%td,           &
              platform%each (i)%rh

      ENDDO loop_level
      if( .not. first_time ) cycle reports
!
      call DA_ll_to_xy (platform%info, platform%loc,   &
                        xp, outside, inside_halo )

      IF(outside) THEN
         cycle reports
      ENDIF

      if( platform%info%levels < 1) then
         if ( fm /= 111 ) then
            cycle reports
         endif
      endif

      SELECT CASE(fm)

!        if(index(platform%info%source, 'SYNOP') > 0) then

         CASE (12) ;

            if(.not.use_SynopObs) cycle reports
            num_synop = num_synop + 1

!        else if(index(platform%info%source, 'SHIP') > 0 .or. &
!                index(platform%info%source, 'BOUY') > 0) then

         CASE (13, 17) ;                  ! ships          

            if(.not.use_ShipsObs) cycle reports
            num_ships  = num_ships  + 1

!        else if(index(platform%info%source, 'METAR') > 0) then

         CASE (15:16) ;

            if(.not.use_MetarObs) cycle reports
            num_metar = num_metar + 1

!        else if(index(platform%info%source, 'PILOT') > 0) then

         CASE (32:34) ;

            if(.not.use_PilotObs) cycle reports
            num_pilot = num_pilot + 1

!        else if(index(platform%info%source, 'FM-35 TEMP') > 0) then

         CASE (35:38) ;
            if(.not.use_SoundObs) cycle reports

            num_sound = num_sound + 1

!        else if(index(platform%info%source, 'SATEM') > 0) then

         CASE (86) ;

            if(.not.use_SatemObs) cycle reports

!-----------Reject cloudy Satem obs.
 
            if(platform%loc%pw%inv > 10.) then
               cycle reports
            endif

            num_satem = num_satem + 1


!--------Geostationary or Polar orbitting Satellite AMVs:

         CASE (88)    ;

      if(index(platform%info%name, 'MODIS') > 0 .or. &
         index(platform%info%name, 'modis') > 0 )  then
         if(.not.use_PolarAMVObs) cycle reports
         num_polaramv = num_polaramv + 1
      else
         if(.not.use_GeoAMVObs) cycle reports 
         num_geoamv = num_geoamv + 1
      end if

!        else if(index(platform%info%source, 'AIREP') > 0) then

         CASE (42,96:97) ;

            if(.not.use_AirepObs) cycle reports
            num_airep = num_airep + 1

!        else if(index(platform%info%source, 'GPSPW') > 0) then

         CASE (111) ;
         
            if(.not.use_GpspwObs) cycle reports
            num_gpspw = num_gpspw + 1

!        else if(index(platform%info%source, 'GPSRF') > 0) then

         CASE (116) ;
         
            if(.not.use_GpsrefObs) cycle reports
            num_gpsref = num_gpsref + 1

!        SSM/T1 temperatures:
         CASE (121) ;

            if(.not.use_ssmt1obs) cycle reports
            num_ssmt1 = num_ssmt1 + 1

!        SSM/T2 relative humidities:
         CASE (122) ;

            if(.not.use_ssmt2obs) cycle reports
            num_ssmt2 = num_ssmt2 + 1

!        Scatterometer:

         CASE (281)    ;

            if(.not.use_qscatobs) cycle reports
            num_qscat  = num_qscat  + 1

         CASE (132) ;

            if(.not.use_ProfilerObs) cycle reports
            num_profiler = num_profiler + 1

         CASE (135) ;

            if(.not.use_BogusObs) cycle reports
            num_bogus = num_bogus + 1

         CASE (18,19) ;             ! bouy

            if(.not.use_BuoyObs) cycle reports
            num_buoy  = num_buoy  + 1

         CASE DEFAULT;

            write(unit=*, fmt='(/a/)') &
                 'WARNING   WARNING   WARNING   WARNING   WARNING :'

            write(unit=*, fmt='(a)') 'unsaved obs found:'

            write(unit=*, fmt='(2a)') &
                 'platform%info%platform=', platform%info%platform
 
            write(unit=*, fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels

      END SELECT

   ENDDO reports
!-------------------------------------------------------------------------------
!  Write Counts in the Header for filtered obs
   ounit = filtered_obs_iunit 
!-------------------------------------------------------------------------------
   write(ounit,'(A,i7,1x,A)')'TOTAL =',total_obs,', MISS. = -888888.,'
   write(ounit,'(5(A,i7))')'SYNOP =',  num_synop,&
                           ', METAR =',  num_metar,&
                           ', SHIP  = ', num_ships,&
                           ', BUOY  =',  num_buoy,&
                           ', TEMP  =',  num_sound
   write(ounit,'(5(A,i7))')'AIREP =',  num_airep,&
                           ', PILOT =',  num_pilot,&
                           ', GeoAMV= ', num_geoamv,&
                           ', PolarAMV = ', num_polaramv,&
                           ', GPSPW =',  num_gpspw  
   write(ounit,'(5(A,i7))')'GPSRF =',  num_gpsref, &
                           ', PROFL = ', num_profiler,&
                           ', QSCAT =',  num_qscat, &  
                           ', SSMT1 =',  num_ssmt1,&
                           ', SSMT2 =',  num_ssmt2
   write(ounit,'(5(A,i7))')'SATEM =',  num_satem , &
                           ', BOGUS =',  num_bogus 

!-------------------------------------------------------------------------------
!  Read and Skip till "PHIC" 
   if( first_time) rewind (iunit)
!-------------------------------------------------------------------------------
   head1_info: DO

      READ (UNIT = iunit, FMT = '(A80)', IOSTAT = iost) blank

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3)') &
        '3. Error in reading intermediate Filtered Obs Unit :', iunit
         stop
      ENDIF

      if (blank(1:6) == 'PHIC  ') exit        

   ENDDO head1_info
   write(ounit, FMT = '(A80)') blank
   head2_info: DO

      READ (UNIT = iunit, FMT = '(A80)', IOSTAT = iost) blank
      write(ounit, FMT = '(A80)') blank
      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3)') &
        '4. Error in reading intermediate Filtered Obs Unit :', iunit
         stop
      ENDIF
      if (blank(1:10) == '#---------') exit        
   ENDDO head2_info
!-------------------------------------------------------------------------------
     first_time = .false.
     go to 1000
!-------------------------------------------------------------------------------
END SUBROUTINE DA_Count_Filtered_Obs 
