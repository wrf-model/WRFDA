SUBROUTINE da_write_filtered_obs(iunit, ounit, ob, iv )
!----------------------------------------------------------------------------   
!  History:
!  Purpose: Writes filtered observations by WRFVAR
!  Author:
!             02/28/2005         Syed RH Rizvi
!----------------------------------------------------------------------------   
!  Note:                                                           
!   (a) Information just needed for WRFVAR  is only written  
!   (b) Both U and V should be of good quality ( qc > obs_qc_pointer) 
!   (c) Pressure & Temperature quality should be > fails_error_max ( = -3) 
!   (d) Since there is no check for missing T,P and Q to get Qs in   
!       "DA_TP_To_Qs", RH data is recovered from Q if 
!       q_error = missing_r and both P and T is not missing
!   (e) Since currently Sfc_Obs_Correction changes quality of
!       observations to "surface_correction (= 2)" even if it is bad
!       quality obs (qc < fails_error_max), such obs (element) is 
!       made missing. Otherwise the bad obs (element) will also get 
!       assimilated with "max_iv_check = .false."
!   (f) AMV's from Geostationary and Polar orbiting satellite are
!       seperated & used as profile
!   (g) Currently only MODIS winds from Polar orbiting satellite are used
!----------------------------------------------------------------------------   
   IMPLICIT NONE

   INTEGER,        INTENT (in)   :: iunit, ounit
   type (y_type), intent(in)     :: ob      ! Observation structure.
   TYPE (ob_type), INTENT(IN)    :: iv      ! O-B structure.
!
   CHARACTER (LEN =  10)        :: fmt_name
   CHARACTER (LEN = 160)        :: info_string
   CHARACTER (LEN = 160)        :: fmt_info,    &
                                   fmt_loc, &
                                   fmt_each
   INTEGER                      :: i, j, iost, nlevels, fm
   TYPE (multi_level_type)      :: platform

   integer                       :: n, k, zero 
   integer                       :: thick_qc, dir_qc, td_qc, rh_qc, height_qc
   real                          :: es, qs, speed, dir, rh, rh_error, uu, vv
   real                          :: xmiss, height, height_error, pr_error, slp_error
   real                          :: thick1, thick, thick_error,dir_error,speed_error
   real                          :: td, td_error , elv_err     
   logical, external             :: wrf_dm_on_monitor

!----------------------------------------------------------------------------!
    if(print_detail > 1) print*,' Going to write filtered obs on unit: ',ounit   
   zero = 0
! Note: Currently in 3DVAR there is no active role of 
!       "height" and "TD"        
   xmiss  = -888.0
   height       = missing_r
   height_qc    = missing_data
   height_error = xmiss    
   td           = missing_r 
   td_qc        = missing_data
   td_error     = xmiss        
! Currently following errors are fixed 
   rh_error     = 10.0       
   pr_error = 100.
   slp_error = 200.
   elv_err = 6.0
   dir_error = 5.0
!----------------------------------------------------------------------------!
!
   if ( wrf_dm_on_monitor() ) then
   
!----------------------------------------------------------------------------!
!  READ HEADER
!  ===========
   rewind (iunit)
   head_info: DO

      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) info_string
      write(ounit, FMT = '(A)') info_string

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3/a/)') &
              'ERROR READING FILE UNIT:', iunit, &
              'in read_obs_gts_header.F'
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

   write(ounit, FMT = '(A,1X,A)', IOSTAT = iost) &
        fmt_name, fmt_info, &
        fmt_name, fmt_loc,  &
        fmt_name, fmt_each

   IF (iost /= 0) THEN
      write(unit=*, fmt='(/A,I3,A/)') &
           ' ERROR IN INPUT FILE UNIT ',iunit, &
           ' FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
      stop   
   ENDIF
!----------------------------------------------------------------------------!
    READ (iunit, FMT = '(A)') fmt_name
    write(ounit, FMT = '(A)') fmt_name
!----------------------------------------------------------------------------!
!  Write Synop:
   IF ( iv % num_synop > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for synop ',iv%num_synop
!
     
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_synop               
    if(iv%synop(i)%info%levels > 1) then
    print*,' for SYNOP station ',iv%synop(i)%info%name,' got levels ',&
    iv%synop(i)%info%levels ,' but wrote only one level'
    iv%synop(i)%info%levels = 1
    end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%synop(i)%info%platform,    &
                   iv%synop(i)%info%date_char,   &
                   iv%synop(i)%info%name,        &
                   iv%synop(i)%info%levels,      &
                   iv%synop(i)%info%lat,         &
                   iv%synop(i)%info%lon,         &
                   iv%synop(i)%info%elv,         &
                   iv%synop(i)%info%id
     write(ounit, FMT = fmt_loc) iv%synop(i)%loc%slp, iv%synop(i)%loc%pw
!
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%synop(i)%u%qc >= obs_qc_pointer .and. &
          iv%synop(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%synop(i)%u
         vv = ob%synop(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%synop(i)%info%lon, -1)                        
       speed_error = iv%synop(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%synop(i)%u%qc = missing_data
       iv%synop(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%synop(i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%synop(i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%synop(i)%t       - missing_r ) > 1. .and.  &
          abs(ob%synop(i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%synop(i)%t, ob%synop(i)%p, es, qs )
         rh = (ob%synop(i)%q/qs ) * 100.
       rh_qc = iv%synop(i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%synop(i)%p%qc <= fails_error_max) then
     iv%synop(i)%p%qc = missing_data  
     ob%synop(i)%p    = missing_r     
   end if
   if( iv%synop(i)%t%qc <= fails_error_max) then
    iv%synop(i)%t%qc = missing_data  
     ob%synop(i)%t   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         ob%synop(i)%p,iv%synop(i)%p%qc,pr_error,           &
         speed,iv%synop(i)%u%qc,speed_error,                &
         dir  ,iv%synop(i)%v%qc,dir_error,                  &
         iv%synop(i)%info%elv, zero, elv_err,               &
         ob%synop(i)%t,iv%synop(i)%t%qc,iv%synop(i)%t%error,&
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
!
      ENDDO 
   ENDIF

!----------------------------------------------------------------------------!
!  Write Metar:
   IF ( iv % num_metar > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for metar ',iv%num_metar
!
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_metar               
    if(iv%metar(i)%info%levels > 1) then
    print*,' for metar station ',iv%metar(i)%info%name,' got levels ',&
    iv%metar(i)%info%levels ,' but wrote only one level'
    iv%metar(i)%info%levels = 1
    end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%metar(i)%info%platform,    &
                   iv%metar(i)%info%date_char,   &
                   iv%metar(i)%info%name,        &
                   iv%metar(i)%info%levels,      &
                   iv%metar(i)%info%lat,         &
                   iv%metar(i)%info%lon,         &
                   iv%metar(i)%info%elv,         &
                   iv%metar(i)%info%id
     write(ounit, FMT = fmt_loc) iv%metar(i)%loc%slp, iv%metar(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%metar(i)%u%qc >= obs_qc_pointer .and. &
          iv%metar(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%metar(i)%u
         vv = ob%metar(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%metar(i)%info%lon, -1)                        
       speed_error = iv%metar(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%metar(i)%u%qc = missing_data
       iv%metar(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%metar(i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%metar(i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%metar(i)%t       - missing_r ) > 1. .and.  &
          abs(ob%metar(i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%metar(i)%t, ob%metar(i)%p, es, qs )
         rh = (ob%metar(i)%q/qs ) * 100.
       rh_qc = iv%metar(i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%metar(i)%p%qc <= fails_error_max) then
     iv%metar(i)%p%qc = missing_data  
     ob%metar(i)%p    = missing_r     
   end if
   if( iv%metar(i)%t%qc <= fails_error_max) then
    iv%metar(i)%t%qc = missing_data  
     ob%metar(i)%t   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         ob%metar(i)%p,iv%metar(i)%p%qc,pr_error,           &
         speed,iv%metar(i)%u%qc,speed_error,                &
         dir  ,iv%metar(i)%v%qc,dir_error,                  &
         iv%metar(i)%info%elv, zero, elv_err,               &
         ob%metar(i)%t,iv%metar(i)%t%qc,iv%metar(i)%t%error,&
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Ships:
   IF ( iv % num_ships > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for ships ',iv%num_ships
!
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_ships               
    if(iv%ships(i)%info%levels > 1) then
    print*,' for ships station ',iv%ships(i)%info%name,' got levels ',&
    iv%ships(i)%info%levels ,' but wrote only one level'
    iv%ships(i)%info%levels = 1
    end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%ships(i)%info%platform,    &
                   iv%ships(i)%info%date_char,   &
                   iv%ships(i)%info%name,        &
                   iv%ships(i)%info%levels,      &
                   iv%ships(i)%info%lat,         &
                   iv%ships(i)%info%lon,         &
                   iv%ships(i)%info%elv,         &
                   iv%ships(i)%info%id
     write(ounit, FMT = fmt_loc) iv%ships(i)%loc%slp, iv%ships(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%ships(i)%u%qc >= obs_qc_pointer .and. &
          iv%ships(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%ships(i)%u
         vv = ob%ships(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%ships(i)%info%lon, -1)                        
       speed_error = iv%ships(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%ships(i)%u%qc = missing_data
       iv%ships(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%ships(i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%ships(i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%ships(i)%t       - missing_r ) > 1. .and.  &
          abs(ob%ships(i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%ships(i)%t, ob%ships(i)%p, es, qs )
         rh = (ob%ships(i)%q/qs ) * 100.
       rh_qc = iv%ships(i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%ships(i)%p%qc <= fails_error_max) then
     iv%ships(i)%p%qc = missing_data  
     ob%ships(i)%p    = missing_r     
   end if
   if( iv%ships(i)%t%qc <= fails_error_max) then
    iv%ships(i)%t%qc = missing_data  
     ob%ships(i)%t   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         ob%ships(i)%p,iv%ships(i)%p%qc,pr_error,           &
         speed,iv%ships(i)%u%qc,speed_error,                &
         dir  ,iv%ships(i)%v%qc,dir_error,                  &
         iv%ships(i)%info%elv, zero, elv_err,               &
         ob%ships(i)%t,iv%ships(i)%t%qc,iv%ships(i)%t%error,&
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
!
      ENDDO 
   ENDIF
!
!----------------------------------------------------------------------------!
!  Write Buoy :
   IF ( iv % num_buoy  > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for buoy  ',iv%num_buoy 
!
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_buoy               
    if(iv%buoy(i)%info%levels > 1) then
    print*,' for buoy station ',iv%buoy(i)%info%name,' got levels ',&
    iv%buoy(i)%info%levels ,' but wrote only one level'
    iv%buoy(i)%info%levels = 1
    end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%buoy (i)%info%platform,    &
                   iv%buoy (i)%info%date_char,   &
                   iv%buoy (i)%info%name,        &
                   iv%buoy (i)%info%levels,      &
                   iv%buoy (i)%info%lat,         &
                   iv%buoy (i)%info%lon,         &
                   iv%buoy (i)%info%elv,         &
                   iv%buoy (i)%info%id
     write(ounit, FMT = fmt_loc) iv%buoy (i)%loc%slp, iv%buoy(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%buoy(i)%u%qc >= obs_qc_pointer .and. &
          iv%buoy(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%buoy(i)%u
         vv = ob%buoy(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%buoy(i)%info%lon, -1)                        
       speed_error = iv%buoy(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%buoy(i)%u%qc = missing_data
       iv%buoy(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%buoy(i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%buoy(i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%buoy(i)%t       - missing_r ) > 1. .and.  &
          abs(ob%buoy(i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%buoy(i)%t, ob%buoy(i)%p, es, qs )
         rh = (ob%buoy(i)%q/qs ) * 100.
       rh_qc = iv%buoy(i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%buoy(i)%p%qc <= fails_error_max) then
     iv%buoy(i)%p%qc = missing_data  
     ob%buoy(i)%p    = missing_r     
   end if
   if( iv%buoy(i)%t%qc <= fails_error_max) then
    iv%buoy(i)%t%qc = missing_data  
     ob%buoy(i)%t   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                  &
         ob%buoy(i)%p,iv%buoy(i)%p%qc,pr_error,            &
         speed,iv%buoy(i)%u%qc,speed_error,                &
         dir  ,iv%buoy(i)%v%qc,dir_error,                  &
         iv%buoy(i)%info%elv, zero, elv_err,               &
         ob%buoy(i)%t,iv%buoy(i)%t%qc,iv%buoy(i)%t%error,  &
         td, td_qc, td_error,                              &
         rh, rh_qc,  rh_error                
!
      ENDDO 
!
   ENDIF
!----------------------------------------------------------------------------!
!  Write Satob Obs:
   IF ( iv % num_satob    > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for SATOB    ',iv%num_satob    
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_satob                  

    if(iv%satob(i)%info%levels > 1) then
    print*,' in satob got levels ',iv%satob(i)%info%levels,' but writing only one ' 
    iv%satob(i)%info%levels = 1
    end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%satob(i)%info%platform,    &
                   iv%satob(i)%info%date_char,   &
                   iv%satob(i)%info%name,        &
                   iv%satob(i)%info%levels,      &
                   iv%satob(i)%info%lat,         &
                   iv%satob(i)%info%lon,         &
                   iv%satob(i)%info%elv,         &
                   iv%satob(i)%info%id
     write(ounit, FMT = fmt_loc) iv%satob(i)%loc%slp, iv%satob(i)%loc%pw
!
!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%satob(i)%u%qc >= obs_qc_pointer .and. &
          iv%satob(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%satob(i)%u
         vv = ob%satob(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%satob(i)%info%lon, -1)                        
       speed_error = iv%satob(i)%u%error
       if( speed_error == missing_r) speed_error = xmiss        
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%satob(i)%u%qc = missing_data
       iv%satob(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         iv%satob(i)%p,iv%satob(i)%u%qc,slp_error,  &
         speed,iv%satob(i)%u%qc,speed_error,            &
         dir  ,iv%satob(i)%u%qc,dir_error,              &
         missing_r, standard_atmosphere, xmiss,             &
         missing_r, zero_t_td, xmiss,                       & 
         missing_r, zero_t_td, xmiss,                       & 
         missing_r, zero_t_td, xmiss   
!
    ENDDO   
   ENDIF
!----------------------------------------------------------------------------!
!  Write Sounde 
   IF ( iv % num_sound > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for sound  ',iv%num_sound 
!
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_sound               
     if( iv%sonde_sfc(i)%info%platform /= iv%sound(i)%info%platform .or. &
         iv%sonde_sfc(i)%info%lat      /= iv%sound(i)%info%lat      .or. &
         iv%sonde_sfc(i)%info%lon      /= iv%sound(i)%info%lon      .or. &
         iv%sonde_sfc(i)%info%elv      /= iv%sound(i)%info%elv      .or. &
         iv%sonde_sfc(i)%info%id       /= iv%sound(i)%info%id       ) then
     print*,' Mismatch for Sound surface and upper air report '
     print*,' Surface Sound Deatils:            '
      write(unit=*, fmt=fmt_info) &
            iv%sonde_sfc(i)%info%platform,    &
            iv%sonde_sfc(i)%info%date_char,   &
            iv%sonde_sfc(i)%info%name,        &
            iv%sonde_sfc(i)%info%levels,      &
            iv%sonde_sfc(i)%info%lat,         &
            iv%sonde_sfc(i)%info%lon,         &
            iv%sonde_sfc(i)%info%elv,         &
            iv%sonde_sfc(i)%info%id

     print*,' Upper level  Deatils: '
      write(unit=*, fmt=fmt_info) &
            iv%sound(i)%info%platform,    &
            iv%sound(i)%info%date_char,   &
            iv%sound(i)%info%name,        &
            iv%sound(i)%info%levels,      &
            iv%sound(i)%info%lat,         &
            iv%sound(i)%info%lon,         &
            iv%sound(i)%info%elv,         &
            iv%sound(i)%info%id
     stop
     end if
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%sound(i)%info%platform,    &
                   iv%sound(i)%info%date_char,   &
                   iv%sound(i)%info%name,        &
                   iv%sound(i)%info%levels + 1,  &
                   iv%sound(i)%info%lat,         &
                   iv%sound(i)%info%lon,         &
                   iv%sound(i)%info%elv,         &
                   iv%sound(i)%info%id
     write(ounit, FMT = fmt_loc) iv%sound(i)%loc%slp, iv%sound(i)%loc%pw
!----------------------------------------------------------------------------!
! First write surface level information
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%sonde_sfc(i)%u%qc >= obs_qc_pointer .and. &
          iv%sonde_sfc(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%sonde_sfc(i)%u
         vv = ob%sonde_sfc(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%sonde_sfc(i)%info%lon, -1)                        
       speed_error = iv%sonde_sfc(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%sonde_sfc(i)%u%qc = missing_data
       iv%sonde_sfc(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%sonde_sfc(i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%sonde_sfc(i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%sonde_sfc(i)%t       - missing_r ) > 1. .and.  &
          abs(ob%sonde_sfc(i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%sonde_sfc(i)%t, ob%sonde_sfc(i)%p, es, qs )
         rh = (ob%sonde_sfc(i)%q/qs ) * 100.
       rh_qc = iv%sonde_sfc(i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%sonde_sfc(i)%p%qc <= fails_error_max) then
     iv%sonde_sfc(i)%p%qc = missing_data  
     ob%sonde_sfc(i)%p    = missing_r     
   end if
   if( iv%sonde_sfc(i)%t%qc <= fails_error_max) then
    iv%sonde_sfc(i)%t%qc = missing_data  
     ob%sonde_sfc(i)%t   = missing_r     
     iv%sonde_sfc(i)%t%error = xmiss     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                       &
         ob%sonde_sfc(i)%p,iv%sonde_sfc(i)%p%qc,pr_error,       &
         speed,iv%sonde_sfc(i)%u%qc,speed_error,                &
         dir  ,iv%sonde_sfc(i)%v%qc,dir_error,                  &
         iv%sonde_sfc(i)%info%elv, zero, elv_err,               &
         ob%sonde_sfc(i)%t,iv%sonde_sfc(i)%t%qc,iv%sonde_sfc(i)%t%error,&
         td, td_qc, td_error,                                   &
         rh, rh_qc,  rh_error                
!----------------------------------------------------------------------------!
      DO n = 1, iv%sound(i)%info%levels 
!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%sound(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%sound(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%sound(i)%u(n)
         vv = ob%sound(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%sound(i)%info%lon, -1)                        
       speed_error = iv%sound(i)%u(n)%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%sound(i)%u(n)%qc = missing_data
       iv%sound(i)%v(n)%qc = missing_data
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%sound(i)%q(n)%qc >= obs_qc_pointer .and.  &
          abs(iv%sound(i)%q(n)%error - missing_r ) > 1. .and.  &
          abs(ob%sound(i)%t(n)       - missing_r ) > 1. .and.  &
          abs(iv%sound(i)%p(n)       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%sound(i)%t(n), iv%sound(i)%p(n), es, qs )
         rh = (ob%sound(i)%q(n)/qs ) * 100.
       rh_qc = iv%sound(i)%q(n)%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) then
   rh_qc = missing_data
   end if
!----------------------------------------------------------------------------!
   if( iv%sound(i)%t(n)%qc <= fails_error_max) then
    iv%sound(i)%t(n)%qc = missing_data  
     ob%sound(i)%t(n)   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                    &
         iv%sound(i)%p(n),zero, pr_error,                    &
         speed,iv%sound(i)%u(n)%qc,speed_error,              &
         dir  ,iv%sound(i)%v(n)%qc,dir_error,                &
         iv%sound(i)%info%elv, zero, elv_err,                &
         ob%sound(i)%t(n),iv%sound(i)%t(n)%qc,iv%sound(i)%t(n)%error,&
         td, td_qc, td_error,                                &
         rh, rh_qc,  rh_error                

      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Airep: 
   IF ( iv % num_airep > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for airep  ',iv%num_airep 
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_airep               
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%airep(i)%info%platform,    &
                   iv%airep(i)%info%date_char,   &
                   iv%airep(i)%info%name,        &
                   iv%airep(i)%info%levels,      &
                   iv%airep(i)%info%lat,         &
                   iv%airep(i)%info%lon,         &
                   iv%airep(i)%info%elv,         &
                   iv%airep(i)%info%id
     write(ounit, FMT = fmt_loc) iv%airep(i)%loc%slp, iv%airep(i)%loc%pw
!----------------------------------------------------------------------------!
     DO n = 1, iv%airep(i)%info%levels
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%airep(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%airep(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%airep(i)%u(n)
         vv = ob%airep(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%airep(i)%info%lon, -1)                        
       speed_error = iv%airep(i)%u(n)%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%airep(i)%u(n)%qc = missing_data
       iv%airep(i)%v(n)%qc = missing_data
      end if
!----------------------------------------------------------------------------!
  if( abs(iv%airep(i)%t(n)%qc) > 99) iv%airep(i)%t(n)%qc = missing_data
      write(ounit, FMT = TRIM (fmt_each))              &
         iv%airep(i)%p(n),zero , pr_error,             &
         speed,iv%airep(i)%u(n)%qc,speed_error,        &
         dir  ,iv%airep(i)%v(n)%qc,dir_error,          &
         missing_r, standard_atmosphere, xmiss,        &
         ob%airep(i)%t(n),iv%airep(i)%t(n)%qc,iv%airep(i)%t(n)%error,&
         missing_r, missing_data, xmiss,               & 
         missing_r, missing_data, xmiss

      ENDDO 
!
      ENDDO     
   ENDIF
!----------------------------------------------------------------------------!
!  Write Pilot:
   IF ( iv % num_pilot > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for pilot  ',iv%num_pilot 
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_pilot               
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%pilot(i)%info%platform,    &
                   iv%pilot(i)%info%date_char,   &
                   iv%pilot(i)%info%name,        &
                   iv%pilot(i)%info%levels,      &
                   iv%pilot(i)%info%lat,         &
                   iv%pilot(i)%info%lon,         &
                   iv%pilot(i)%info%elv,         &
                   iv%pilot(i)%info%id
     write(ounit, FMT = fmt_loc) iv%pilot(i)%loc%slp, iv%pilot(i)%loc%pw
!----------------------------------------------------------------------------!
     DO n = 1, iv%pilot(i)%info%levels
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%pilot(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%pilot(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%pilot(i)%u(n)
         vv = ob%pilot(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%pilot(i)%info%lon, -1)                        
       speed_error = iv%pilot(i)%u(n)%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%pilot(i)%u(n)%qc = missing_data
       iv%pilot(i)%v(n)%qc = missing_data
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))       &
         iv%pilot(i)%p(n),zero, pr_error,       &
         speed,iv%pilot(i)%u(n)%qc,speed_error, &
         dir  ,iv%pilot(i)%v(n)%qc,dir_error,   &
         missing_r, standard_atmosphere, xmiss, &
         missing_r, missing_data, xmiss,        & 
         missing_r, missing_data, xmiss,        & 
         missing_r, missing_data, xmiss

      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Satem:          
! Note: The Satem ref_p is put in the SLP position in OBSPROC data stream.
!
   IF ( iv % num_satem > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for satem  ',iv%num_satem 
!----------------------------------------------------------------------------!
!  Write Satem:          
   DO i = 1, iv % num_satem           
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%satem(i)%info%platform,    &
                   iv%satem(i)%info%date_char,   &
                   iv%satem(i)%info%name,        &
                   iv%satem(i)%info%levels,      &
                   iv%satem(i)%info%lat,         &
                   iv%satem(i)%info%lon,         &
                   iv%satem(i)%info%elv,         &
                   iv%satem(i)%info%id
     write(ounit, FMT = fmt_loc) iv%satem(i)%loc%slp, iv%satem(i)%loc%pw
     DO n = 1, iv%satem(i)%info%levels 
!
!  Approximate thickness errors are recovered 
         thick_error = iv%satem(i)%thickness(n)%error*gravity / 1.414 
!
       if( n ==  1 ) then
         thick1 = ob%satem(i)%thickness(1)
       else
         thick1 = thick1 + ob%satem(i)%thickness(n) 
       end if
       thick = thick1
       thick_qc = iv%satem(i)%thickness(n)%qc 
       if( iv%satem(i)%thickness(n)%qc < obs_qc_pointer)then
        thick = missing_r
        thick_qc = missing_data
       end if
!
         write(ounit, FMT = TRIM (fmt_each))               &
         iv%satem(i)%p(n),zero, slp_error,                 &
         missing_r, missing_data, xmiss,                   &
         missing_r, missing_data, xmiss,                   &
         missing_r, standard_atmosphere, xmiss,            &
         thick, thick_qc, thick_error,                     &
         missing_r, missing_data, xmiss,                   &
         missing_r, missing_data, xmiss

      ENDDO 
!
      ENDDO 
   ENDIF

!----------------------------------------------------------------------------!
!  Write Qscat: 
   IF ( iv % num_qscat > 0 ) THEN
!
    if(print_detail > 1) print*,' writing filtered Obs for qscat  ',iv%num_qscat 
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_qscat               
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%qscat(i)%info%platform,    &
                   iv%qscat(i)%info%date_char,   &
                   iv%qscat(i)%info%name,        &
                   iv%qscat(i)%info%levels,      &
                   iv%qscat(i)%info%lat,         &
                   iv%qscat(i)%info%lon,         &
                   iv%qscat(i)%info%elv,         &
                   iv%qscat(i)%info%id
     write(ounit, FMT = fmt_loc) iv%qscat(i)%loc%slp, iv%qscat(i)%loc%pw
!
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%qscat(i)%u%qc >= obs_qc_pointer .and. &
          iv%qscat(i)%v%qc >= obs_qc_pointer) then  
         uu = ob%qscat(i)%u
         vv = ob%qscat(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%qscat(i)%info%lon, -1)                        
       speed_error = iv%qscat(i)%u%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = xmiss
       iv%qscat(i)%u%qc = missing_data
       iv%qscat(i)%v%qc = missing_data
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))          &
         iv%qscat(i)%h,iv%qscat(i)%u%qc,slp_error, &  
         speed,iv%qscat(i)%u%qc,speed_error,       &
         dir  ,iv%qscat(i)%u%qc,dir_error,         &
         iv%qscat(i)%info%elv, zero, elv_err,      &
         missing_r, missing_data, xmiss,           & 
         missing_r, missing_data, xmiss,           & 
         missing_r, missing_data, xmiss
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
! Write Profiler: 
   IF ( iv % num_profiler > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for profiler  ',iv%num_profiler 
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_profiler            
     write(ounit, FMT = fmt_info, IOSTAT = iost)    &
                   iv%profiler(i)%info%platform,    &
                   iv%profiler(i)%info%date_char,   &
                   iv%profiler(i)%info%name,        &
                   iv%profiler(i)%info%levels,      &
                   iv%profiler(i)%info%lat,         &
                   iv%profiler(i)%info%lon,         &
                   iv%profiler(i)%info%elv,         &
                   iv%profiler(i)%info%id
     write(ounit, FMT = fmt_loc) iv%profiler(i)%loc%slp, iv%profiler(i)%loc%pw
!----------------------------------------------------------------------------!
   DO n = 1, iv%profiler(i)%info%levels
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%profiler(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%profiler(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%profiler(i)%u(n)
         vv = ob%profiler(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%profiler(i)%info%lon, -1)                        
       speed_error = iv%profiler(i)%u(n)%error
      else
         speed = missing_r
         dir   = missing_r
         speed_error = 1.1
       iv%profiler(i)%u(n)%qc = missing_data
       iv%profiler(i)%v(n)%qc = missing_data
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))          &
         iv%profiler(i)%p(n),zero,pr_error,        &
         speed,iv%profiler(i)%u(n)%qc,speed_error, &
         dir  ,iv%profiler(i)%v(n)%qc,dir_error,   &
         iv%profiler(i)%info%elv, zero, elv_err,   &
         missing_r, missing_data, xmiss,           & 
         missing_r, missing_data, xmiss,           & 
         missing_r, missing_data, xmiss

      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write SSMI retrieval:
   IF ( iv % num_ssmi_retrieval > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for ssmi retrieval  ', iv % num_ssmi_retrieval 
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_ssmi_retrieval
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%ssmi_retrieval(i)%info%platform,    &
                   iv%ssmi_retrieval(i)%info%date_char,   &
                   iv%ssmi_retrieval(i)%info%name,        &
                   iv%ssmi_retrieval(i)%info%levels,      &
                   iv%ssmi_retrieval(i)%info%lat,         &
                   iv%ssmi_retrieval(i)%info%lon,         &
                   iv%ssmi_retrieval(i)%info%elv,         &
                   iv%ssmi_retrieval(i)%info%id
     write(ounit, FMT = fmt_loc) iv%ssmi_retrieval(i)%loc%slp, iv%ssmi_retrieval(i)%loc%pw
      ENDDO    
   ENDIF
!----------------------------------------------------------------------------!
!  [10] Write SSMT1:          
   IF ( iv % num_ssmt1 > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for ssmt1 ', iv % num_ssmt1 
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_ssmi_retrieval
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%ssmt1(i)%info%platform,    &
                   iv%ssmt1(i)%info%date_char,   &
                   iv%ssmt1(i)%info%name,        &
                   iv%ssmt1(i)%info%levels,      &
                   iv%ssmt1(i)%info%lat,         &
                   iv%ssmt1(i)%info%lon,         &
                   iv%ssmt1(i)%info%elv,         &
                   iv%ssmt1(i)%info%id
     write(ounit, FMT = fmt_loc) iv%ssmt1(i)%loc%slp, iv%ssmt1(i)%loc%pw
!----------------------------------------------------------------------------!
     DO n = 1, iv%ssmt1(i)%info%levels
      write(ounit, FMT = TRIM (fmt_each))                            &
         iv%ssmt1(i)%p(n),zero,  slp_error,                          &
         missing_r, missing, missing_r,                              &
         missing_r, missing, missing_r,                              &
         height,          height_qc,height_error,                    &
         ob%ssmt1(i)%t(n),iv%ssmt1(i)%t(n)%qc,iv%ssmt1(i)%t(n)%error,&
         td, td_qc, td_error,                                        &   
         missing_r, missing, missing_r                                 
      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write SSMT2:          
   IF ( iv % num_ssmt2 > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for ssmt2 ', iv % num_ssmt2 
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_ssmi_retrieval
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%ssmt2(i)%info%platform,    &
                   iv%ssmt2(i)%info%date_char,   &
                   iv%ssmt2(i)%info%name,        &
                   iv%ssmt2(i)%info%levels,      &
                   iv%ssmt2(i)%info%lat,         &
                   iv%ssmt2(i)%info%lon,         &
                   iv%ssmt2(i)%info%elv,         &
                   iv%ssmt2(i)%info%id
     write(ounit, FMT = fmt_loc) iv%ssmt2(i)%loc%slp, iv%ssmt2(i)%loc%pw
!----------------------------------------------------------------------------!
    DO n = 1, iv%ssmt2(i)%info%levels
      write(ounit, FMT = TRIM (fmt_each))                  &
         iv%ssmt2(i)%p(n),zero,  slp_error,                &
         missing_r, missing, missing_r,                    &
         missing_r, missing, missing_r,                    &
         height,          height_qc,height_error,          &
         missing_r, missing, missing_r,                    &
         td, td_qc, td_error,                              &   
         ob%ssmt2(i)%rh(n),iv%ssmt2(i)%rh(n)%qc,rh_error                 
    ENDDO 
!
    ENDDO 
   ENDIF

!----------------------------------------------------------------------------!
!  Write Gpspw:          
   IF ( iv % num_gpspw > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for gpspw ', iv % num_gpspw 
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_gpspw
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%gpspw(i)%info%platform,    &
                   iv%gpspw(i)%info%date_char,   &
                   iv%gpspw(i)%info%name,        &
                   iv%gpspw(i)%info%levels,      &
                   iv%gpspw(i)%info%lat,         &
                   iv%gpspw(i)%info%lon,         &
                   iv%gpspw(i)%info%elv,         &
                   iv%gpspw(i)%info%id
     write(ounit, FMT = fmt_loc) iv%gpspw(i)%loc%slp, iv%gpspw(i)%loc%pw
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Gps Refractivity:
   IF ( iv % num_gpsref > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for gpsref ', iv % num_gpsref
!----------------------------------------------------------------------------!
   DO i = 1, iv % num_gpsref
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%gpsref(i)%info%platform,    &
                   iv%gpsref(i)%info%date_char,   &
                   iv%gpsref(i)%info%name,        &
                   iv%gpsref(i)%info%levels,      &
                   iv%gpsref(i)%info%lat,         &
                   iv%gpsref(i)%info%lon,         &
                   iv%gpsref(i)%info%elv,         &
                   iv%gpsref(i)%info%id
     write(ounit, FMT = fmt_loc) iv%gpsref(i)%loc%slp, iv%gpsref(i)%loc%pw
!----------------------------------------------------------------------------!
    DO n = 1, iv%gpsref(i)%info%levels
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%gpsref(i)%q(n)%qc >= obs_qc_pointer .and.  &
          abs(iv%gpsref(i)%q(n)%error - missing_r ) > 1. .and.  &
          abs(ob%gpsref(i)%t(n)       - missing_r ) > 1. .and.  &
          abs(ob%gpsref(i)%p(n)       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%gpsref(i)%t(n), ob%gpsref(i)%p(n), es, qs )
         rh = (ob%gpsref(i)%q(n)/qs ) * 100.
       rh_qc = iv%gpsref(i)%q(n)%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
!----------------------------------------------------------------------------!
   if( iv%gpsref(i)%p(n)%qc <= fails_error_max) then
     iv%gpsref(i)%p(n)%qc = missing_data  
     ob%gpsref(i)%p(n)    = missing_r     
   end if
   if( iv%gpsref(i)%t(n)%qc <= fails_error_max) then
    iv%gpsref(i)%t(n)%qc = missing_data  
     ob%gpsref(i)%t(n)   = missing_r     
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))             &
         ob%gpsref(i)%p(n),zero, slp_error,           &
         missing_r, missing, missing_r,               &
         missing_r, missing, missing_r,               &
         iv%gpsref(i)%info%elv, zero, elv_err,        &
         height,          height_qc,height_error,     &
         ob%gpsref(i)%t(n),iv%gpsref(i)%t(n)%qc,iv%gpsref(i)%t(n)%error,  &   
         ob%gpsref(i)%ref(n),iv%gpsref(i)%ref(n)%qc,iv%gpsref(i)%ref(n)%error,  &   
         rh, iv%gpsref(i)%q(n)%qc,iv%gpsref(i)%q(n)%error
      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
   IF ( iv % num_ssmi_tb > 0 ) THEN
   print*,'Warning ------>  Currently SSMI brightness temperature info is not written'
   stop
   ENDIF
  endif 
END SUBROUTINE da_write_filtered_obs
