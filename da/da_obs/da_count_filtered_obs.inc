subroutine da_count_filtered_obs(&
                    ts0, tlp, ptop, ps0, map, ds, &   
                    phic, xlonc, truelat1, truelat2)

   !---------------------------------------------------------------------------
   ! Purpose: Scans intermediate Filtered Obs file, 
   !          counts various obs type and writes on filtered_obs_unit
   !---------------------------------------------------------------------------

   implicit none

   real,              intent(in) :: ts0, tlp, ptop, ps0, ds
   real,              intent(in) :: phic, xlonc, truelat1, truelat2
   integer,           intent(in) :: map

   integer                      :: i, iost, fm
   type (multi_level_type)      :: platform
   real                         :: height_error
   integer                      :: num_synop, &
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
                                   num_airsr, &
                                   num_others

   integer                        :: iunit, files, total_obs     
   integer                        :: maxnes, numc, idd, nesti, nestj
   character*20                   :: filename


   if (trace_use) call da_trace_entry("da_count_filtered_obs")

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
   num_qscat = 0
   num_profiler = 0
   num_buoy = 0
   num_bogus = 0
   num_airsr = 0
   num_others = 0

   call da_get_unit(iunit)

   ! Loop over all data files
   do files = 0, num_procs-1
      write(unit=filename, fmt='(a,i3.3)') 'filtered_obs.',files  
      open(unit=iunit, file= trim(filename), form='formatted',iostat=iost)
      if (iost /= 0) &
      call da_error(__FILE__,__LINE__, (/"Cannot open "//trim(filename)/))

      !  loop over records

      reports: do
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
             write (0,'(/A,I9)') ' end OF OBS unit: ',iunit
             write (0,'(/A,I9)') ' iostat:          ',iost
            exit reports
         end if

         ! Read surface Info

         read (iunit, fmt = fmt_srfc) platform%loc%slp, platform%loc%pw

         ! levels < 1 and not GPSPW, go back to reports

         if ((platform%info%levels < 1) .AND.            &
             (index(platform%info%platform, 'GPSPW') <= 0)) then
              cycle reports
         end if
         read(platform%info%platform(4:6), '(I3)') fm
         ! read each levels


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
                 platform%each (i)%v,            &
                 platform%each (i)%height,       &
                 platform%each (i)%height_qc,    &
                 height_error,                   &
                 platform%each (i)%t,            &
                 platform%each (i)%td,           &
                 platform%each (i)%rh


         end do

         if (platform%info%levels < 1) then
            if (fm /= 111) then
               cycle reports
            end if
         end if
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
            ! SSM/T1 temperatures:

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

         case (18,19) ;             ! bouy

            if (.not.use_BuoyObs) cycle reports
            num_buoy  = num_buoy  + 1

         case (133) ;      !  AIRS retrievals
            if (.not.use_AIRSRETObs) cycle reports
            num_airsr = num_airsr + 1

         case default;
            num_others = num_others + 1
            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
               'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
         end select
      end do reports                  !  Loop over reports              
      close (iunit)
   end do               !  Loop over all data files
   call da_free_unit (iunit)

   ! write counts in the header 

   total_obs = num_synop + num_metar + num_ships + &
      num_buoy + num_sound + num_airep + num_pilot + &
      num_geoamv + num_polaramv + num_gpspw + num_gpsref + &
      num_profiler + num_qscat + num_ssmt1 + num_ssmt2 +  &
      num_satem  + num_bogus +  num_airsr + num_others

   write  (unit = filtered_obs_unit, fmt = '(A,I7,A,F8.0,A)')    &
      "TOTAL =",total_obs, ", MISS. =",missing_r,","  

   ! Write other counts       

   write  (unit = filtered_obs_unit, fmt = '(6(A,I7,A))')    &
      "SYNOP =",num_synop,", ", &
      "METAR =",num_metar,", ", &
      "SHIP  =",num_ships,", ", &
      "BUOY  =",num_buoy,", ", &
      "TEMP  =",num_sound,", ", &
      "AIREP =",num_airep,", ", &
      "PILOT =",num_pilot,", ", &
      "GeoAMV =",num_geoamv,", ", &
      "PolarAMV =",num_polaramv,", ", &
      "GPSPW =",num_gpspw,", ", &
      "GPSPRF =",num_gpsref,", ", &
      "PROFL =",num_profiler,", ", &
      "QSCAT =",num_qscat,", ", &
      "SSMT1 =",num_ssmt1,", ", &
      "SSMT2 =",num_ssmt2,", ", &
      "SATEM =",num_satem, ", ", &
      "BOGUS =",num_bogus,", ", &
      "AIRSRET =",num_airsr,", ", &
      "OTHER =",num_others,", "

   ! write reference state info

   write (unit = filtered_obs_unit, fmt = '(A,F7.2,A,F7.2,4(A,F7.2),A)') &
      "PHIC  =", phic,", XLONC =", xlonc,", true1 =", truelat1,&
      ", true2 =",truelat2                                                      
   write (unit = filtered_obs_unit, fmt = '(2(A,F7.2),A,F7.0,A,F7.0,A)') &
      "TS0   =",  ts0, ", TLP   =", tlp, &
      ", PTOP  =",  ptop,", PS0   =",  ps0,"," 

   ! write domain info 

   !  hardwire following variables
   maxnes = 1 ; numc = 1 ;  idd = 1
   nesti = 1 ; nestj = 1
   write (unit = filtered_obs_unit, fmt = '(5(A,I7),A)') &
      "IXC   =", ide   ,", JXC   =", jde   ,", IPROJ =", map,&
      ", IDD   =", idd,", MAXNES=",maxnes,","
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      !       "NESTIX=",(nestix (i),", ", i = 1, maxnes)
      "NESTIX=",(ide    ,", ", i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTJX=",(jde       ,", ", i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NUMC  =",(numc ,", ", i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,F7.2,A))')&
      "DIS   =",(ds/1000.,", ",i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTI =",(nesti     ,", ", i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTJ =",(nestj     ,", ", i = 1, maxnes)

   ! write variable names and unit

   write (unit = filtered_obs_unit, fmt = '(A)') &
      "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
   write (unit = filtered_obs_unit, fmt = '(A)') &
      "SRFC  = SLP, PW (DATA,QC,ERROR)."
   write (unit = filtered_obs_unit, fmt = '(A)') &
      "EACH  = PRES, SPEED, DIR, HEIGHT, TEMP, DEW PT, HUMID (DATA,QC,ERROR)*LEVELS."

   ! write format info
 
   write (unit = filtered_obs_unit, fmt = '(2A)') 'INFO_fmt = ', trim (fmt_info)
   write (unit = filtered_obs_unit, fmt = '(2A)') 'SRFC_fmt = ', trim (fmt_srfc)
   write (unit = filtered_obs_unit, fmt = '(2A)') 'EACH_fmt = ', trim (fmt_each)

   ! write end of header record

   write (unit = filtered_obs_unit, fmt = '(A)') &
      "#------------------------------------------------------------------------------#"

   if (trace_use) call da_trace_exit("da_count_filtered_obs")

end subroutine da_count_filtered_obs 


