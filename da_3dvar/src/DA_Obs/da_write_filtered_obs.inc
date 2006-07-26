SUBROUTINE da_write_filtered_obs( ob, iv, xp )
!---------------------------------------------------------------------------   
!  History:
!  Purpose: Writes filtered observations by WRFVAR
!  Author:
!             02/28/2005         Syed RH Rizvi
!---------------------------------------------------------------------------   
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
!  Modifications:
!                 10/18/2005         Syed RH Rizvi
!   (a) Original iv & ob arrays are not modified
!   (b) "DA_Count_Filtered_Obs" is called to get exact count of various
!        observations type included in the filtered obs
!
!..........................................................................
! The Quality Controlled observations (QCed obs) means
!
!   i) The check against the Background;
!
!  ii) The model bottom/top check, i.e. the observations below the
!      lowest model level (note, not bottom) and higher than the highest
!      model level (note, not model top) are filled with missing values
!      except the Sound data;
!
! iii) For Sound data, the above-top/below-bottom data are not written out.
!
! The can be use for 3D-Var with check_max_iv = .FALSE., or for
! verification with  ANALYSIS_TYPE = 'VERIFY.
!
!                                      Yong-Run Guo   02/01/2006        
!---------------------------------------------------------------------------   
   IMPLICIT NONE

   type (y_type),  intent(in)    :: ob      ! Observation structure.
   TYPE (ob_type), intent(inout) :: iv      ! O-B structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Domain decomposition vars.

!
   CHARACTER (LEN =  10)        :: info_fmt_name,srfc_fmt_name,each_fmt_name
   CHARACTER (LEN =  80)        :: blank                                         
   CHARACTER (LEN = 160)        :: fmt_info, fmt_loc, fmt_each
   INTEGER                      :: i, j, iost, nlevels, fm
   TYPE (multi_level_type)      :: platform

   integer                       :: n1, n2, n, k, zero , ships_elv_qual
   real                          :: es, qs, speed, dir, rh, rh_error, uu, vv
   real                          :: xmiss, height, height_error, pr_error, slp_error
   real                          :: thick1, thick, thick_error,dir_error,speed_error
   real                          :: td, td_error , elv_error,ships_elv, ships_pr_error
!----------------------------------------------------------------------------!
   real                          :: slp_inv,speed_err,dir_err, p,p_err
   real                          :: t, t_err,ref, ref_err
   integer                       :: thick_qc, t_qc, td_qc, rh_qc, height_qc
   integer                       :: slp_qc, speed_qc, dir_qc,p_qc, ref_qc
!----------------------------------------------------------------------------!
   INTEGER                       :: iunit, ounit, nz, mnz
!----------------------------------------------------------------------------!
   iunit = gts_iunit
   ounit = filtered_obs_iunit - 10
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
   ships_pr_error = 160.
   slp_error = 200.
   elv_error = 6.0
   dir_error = 5.0
   dir_err   = 5.0
   ships_elv = 0.0
   ships_elv_qual = 0 

!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!
!  READ HEADER
!  ===========
   rewind (iunit)
   head_info: DO

      READ (UNIT = iunit, FMT = '(A80)', IOSTAT = iost) blank        
      write(ounit, FMT = '(A80)') blank

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/a,i3/a/)') &
              'ERROR READING FILE UNIT:', iunit, &
              'in read_obs_gts_header.F'
         stop   
      ENDIF

      if (blank(1:6) == 'EACH  ') EXIT

   ENDDO head_info

!----------------------------------------------------------------------------!
!  READ FORMATS
!  ------------

   READ (iunit, FMT = '(A10,1X,A)', IOSTAT = iost) &
        info_fmt_name, fmt_info, &
        srfc_fmt_name, fmt_loc,  &
        each_fmt_name, fmt_each
   WRITE (ounit, FMT = '(A10,1X,A)') &
        info_fmt_name, fmt_info, &
        srfc_fmt_name, fmt_loc,  &
        each_fmt_name, fmt_each

   IF (iost /= 0) THEN
      write(unit=*, fmt='(/A,I3,A/)') &
           ' ERROR IN INPUT FILE UNIT ',iunit, &
           ' FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
      stop   
   ENDIF
!----------------------------------------------------------------------------!
    READ (iunit, FMT = '(A80)') blank
    write(ounit, FMT = '(A80)') blank     
!----------------------------------------------------------------------------!
!  Write Synop:
   IF ( iv % num_synop > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for synop ',iv%num_synop
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_synop
! Guo .. not write out the data below the lowest model level:
    if (iv%synop(i)%zk == missing_r) cycle

    nlevels = iv%synop(i)%info%levels
    if(iv%synop(i)%info%levels > 1) then
     print*,' for SYNOP station ',iv%synop(i)%info%name,' got levels ',&
     iv%synop(i)%info%levels ,' but wrote only one level'
     nlevels = 1
    end if
     write(ounit, FMT = fmt_info)                &
                   iv%synop(i)%info%platform,    &
                   iv%synop(i)%info%date_char,   &
                  iv%synop(i)%info%name,        &
                   nlevels,                      &
                   iv%synop(i)%info%lat,         &
                   iv%synop(i)%info%lon,         &
                   iv%synop(i)%info%elv,         &
                   iv%synop(i)%info%id
    slp_inv= iv%synop(i)%loc%slp%inv
    slp_qc = iv%synop(i)%loc%slp%qc
    if( iv%synop(i)%p%qc <= fails_error_max) then
    slp_inv = missing_r
    slp_qc  = missing_data
    end if
  write(ounit, FMT = fmt_loc) slp_inv,slp_qc,iv%synop(i)%loc%slp%error, iv%synop(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%synop(i)%u%qc >= obs_qc_pointer .and. &
          iv%synop(i)%v%qc >= obs_qc_pointer) then
         uu = ob%synop(i)%u
         vv = ob%synop(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%synop(i)%info%lon, -1)             
       speed_qc  = iv%synop(i)%u%qc
       dir_qc    = speed_qc
       speed_err = iv%synop(i)%u%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
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
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   p    = ob%synop(i)%p
   p_qc = iv%synop(i)%p%qc
   p_err = iv%synop(i)%p%error
   if( iv%synop(i)%p%qc <= fails_error_max) then
     p_qc = missing_data
     p    = missing_r
   p_err  = pr_error           
   end if
   t    =  ob%synop(i)%t
   t_qc =  iv%synop(i)%t%qc
   if( iv%synop(i)%t%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         p,  p_qc,  p_err,                                  &
         speed,speed_qc,        speed_err  ,                &
         dir  , dir_qc,         dir_err  ,                  &
         iv%synop(i)%info%elv, zero, elv_error,             &
         t, t_qc, iv%synop(i)%t%error,                      &
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Metar:
!----------------------------------------------------------------------------!
   IF ( iv % num_metar > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for metar ',iv%num_metar
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_metar
! Guo .. not write out the data below the lowest model level:
    if (iv%metar(i)%zk == missing_r) cycle
               
    nlevels = iv%metar(i)%info%levels
    if(iv%metar(i)%info%levels > 1) then
     print*,' for SYNOP station ',iv%metar(i)%info%name,' got levels ',&
     iv%metar(i)%info%levels ,' but wrote only one level'
     nlevels = 1
    end if
     write(ounit, FMT = fmt_info)                &
                   iv%metar(i)%info%platform,    &
                   iv%metar(i)%info%date_char,   &
                   iv%metar(i)%info%name,        &
                   nlevels,                      &
                   iv%metar(i)%info%lat,         &
                   iv%metar(i)%info%lon,         &
                   iv%metar(i)%info%elv,         &
                   iv%metar(i)%info%id
    slp_inv= iv%metar(i)%loc%slp%inv
    slp_qc = iv%metar(i)%loc%slp%qc
    if( iv%metar(i)%p%qc <= fails_error_max) then
    slp_inv = missing_r
    slp_qc  = missing_data
    end if
  write(ounit, FMT = fmt_loc) slp_inv,slp_qc,iv%metar(i)%loc%slp%error, iv%metar(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%metar(i)%u%qc >= obs_qc_pointer .and. &
          iv%metar(i)%v%qc >= obs_qc_pointer) then
         uu = ob%metar(i)%u
         vv = ob%metar(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%metar(i)%info%lon, -1)             
       speed_qc  = iv%metar(i)%u%qc
       dir_qc    = speed_qc
       speed_err = iv%metar(i)%u%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
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
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   p    = ob%metar(i)%p
   p_qc = iv%metar(i)%p%qc
   p_err = iv%metar(i)%p%error
   if( iv%metar(i)%p%qc <= fails_error_max) then
     p_qc = missing_data
     p    = missing_r
   p_err  = pr_error           
   end if
   t    =  ob%metar(i)%t
   t_qc =  iv%metar(i)%t%qc
   if( iv%metar(i)%t%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         p,  p_qc,  p_err,                                  &
         speed,speed_qc,        speed_err  ,                &
         dir  , dir_qc,         dir_err  ,                  &
         iv%metar(i)%info%elv, zero, elv_error,             &
         t, t_qc, iv%metar(i)%t%error,                      &
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Ships:
!----------------------------------------------------------------------------!
   IF ( iv % num_ships > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for ships ',iv%num_ships
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_ships
! Guo .. not write out the data below the lowest model level:
    if (iv%ships(i)%zk == missing_r) cycle
               
    nlevels = iv%ships(i)%info%levels
    if(iv%ships(i)%info%levels > 1) then
     print*,' for SYNOP station ',iv%ships(i)%info%name,' got levels ',&
     iv%ships(i)%info%levels ,' but wrote only one level'
     nlevels = 1
    end if
     write(ounit, FMT = fmt_info)                &
                   iv%ships(i)%info%platform,    &
                   iv%ships(i)%info%date_char,   &
                   iv%ships(i)%info%name,        &
                   nlevels,                      &
                   iv%ships(i)%info%lat,         &
                   iv%ships(i)%info%lon,         &
                   iv%ships(i)%info%elv,         &
                   iv%ships(i)%info%id
    slp_inv= iv%ships(i)%loc%slp%inv
    slp_qc = iv%ships(i)%loc%slp%qc
    if( iv%ships(i)%p%qc <= fails_error_max) then
    slp_inv = missing_r
    slp_qc  = missing_data
    end if
  write(ounit, FMT = fmt_loc) slp_inv,slp_qc,iv%ships(i)%loc%slp%error, iv%ships(i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%ships(i)%u%qc >= obs_qc_pointer .and. &
          iv%ships(i)%v%qc >= obs_qc_pointer) then
         uu = ob%ships(i)%u
         vv = ob%ships(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%ships(i)%info%lon, -1)             
       speed_qc  = iv%ships(i)%u%qc
       dir_qc    = speed_qc
       speed_err = iv%ships(i)%u%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
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
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   p     = ob%ships(i)%p
   p_qc  = iv%ships(i)%p%qc
   p_err =  ships_pr_error
   if( iv%ships(i)%p%qc <= fails_error_max) then
     p_qc = missing_data
     p    = missing_r
   end if
   t    =  ob%ships(i)%t
   t_qc =  iv%ships(i)%t%qc
   if( iv%ships(i)%t%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         p,  p_qc,  p_err,                                  &
         speed,speed_qc,        speed_err  ,                &
         dir  , dir_qc,         dir_err  ,                  &
         iv%ships(i)%info%elv, zero, elv_error,             &
         t, t_qc, iv%ships(i)%t%error,                      &
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Buoy :
!----------------------------------------------------------------------------!
   IF ( iv % num_buoy  > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for buoy  ',iv%num_buoy 
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_buoy
! Guo .. not write out the data below the lowest model level:
    if (iv%buoy (i)%zk == missing_r) cycle
                
    nlevels = iv%buoy (i)%info%levels
    if(iv%buoy (i)%info%levels > 1) then
     print*,' for BUOY  station ',iv%buoy (i)%info%name,' got levels ',&
     iv%buoy (i)%info%levels ,' but wrote only one level'
     nlevels = 1
    end if
     write(ounit, FMT = fmt_info)                &
                   iv%buoy (i)%info%platform,    &
                   iv%buoy (i)%info%date_char,   &
                   iv%buoy (i)%info%name,        &
                   nlevels,                      &
                   iv%buoy (i)%info%lat,         &
                   iv%buoy (i)%info%lon,         &
                   iv%buoy (i)%info%elv,         &
                   iv%buoy (i)%info%id
    slp_inv= iv%buoy (i)%loc%slp%inv
    slp_qc = iv%buoy (i)%loc%slp%qc
    if( iv%buoy (i)%p%qc <= fails_error_max) then
    slp_inv = missing_r
    slp_qc  = missing_data
    end if
  write(ounit, FMT = fmt_loc) slp_inv,slp_qc,iv%buoy (i)%loc%slp%error, iv%buoy (i)%loc%pw
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%buoy (i)%u%qc >= obs_qc_pointer .and. &
          iv%buoy (i)%v%qc >= obs_qc_pointer) then
         uu = ob%buoy (i)%u
         vv = ob%buoy (i)%v
         call FFDDUV( speed, dir, uu, vv, iv%buoy (i)%info%lon, -1)             
       speed_qc  = iv%buoy (i)%u%qc
       dir_qc    = speed_qc
       speed_err = iv%buoy (i)%u%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
      end if
!----------------------------------------------------------------------------!
! get RH from Q & T    
      if( iv%buoy (i)%q%qc >= obs_qc_pointer .and.  &
          abs(iv%buoy (i)%q%error - missing_r ) > 1. .and.  &
          abs(ob%buoy (i)%t       - missing_r ) > 1. .and.  &
          abs(ob%buoy (i)%p       - missing_r ) > 1.    ) then 
         call DA_TP_To_Qs(ob%buoy (i)%t, ob%buoy (i)%p, es, qs )
         rh = (ob%buoy (i)%q/qs ) * 100.
       rh_qc = iv%buoy (i)%q%qc
      else
        rh    = missing_r
        rh_qc = missing_data
      endif
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   p     = ob%buoy (i)%p
   p_qc  = iv%buoy (i)%p%qc
   p_err = iv%buoy (i)%p%error
   if( iv%buoy (i)%p%qc <= fails_error_max) then
     p_qc = missing_data
     p    = missing_r
   p_err  = pr_error           
   end if
   t    =  ob%buoy (i)%t
   t_qc =  iv%buoy (i)%t%qc
   if( iv%buoy (i)%t%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))                   &
         p,  p_qc,   p_err,                                 &
         speed,speed_qc,        speed_err  ,                &
         dir  , dir_qc,         dir_err  ,                  &
         iv%buoy (i)%info%elv, zero, elv_error,             &
         t, t_qc, iv%buoy (i)%t%error,                      &
         td, td_qc, td_error,                               &
         rh, rh_qc,  rh_error                
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
!  Write Geo. AMVs Obs:
   IF ( iv % num_geoamv > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for geoamv ',iv%num_geoamv
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_geoamv               
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%geoamv(i)%info%platform,    &
                   iv%geoamv(i)%info%date_char,   &
                   iv%geoamv(i)%info%name,        &
                   iv%geoamv(i)%info%levels,      &
                   iv%geoamv(i)%info%lat,         &
                   iv%geoamv(i)%info%lon,         &
                   iv%geoamv(i)%info%elv,         &
                   iv%geoamv(i)%info%id
     write(ounit, FMT = fmt_loc) iv%geoamv(i)%loc%slp, iv%geoamv(i)%loc%pw
!
!----------------------------------------------------------------------------!
     DO n = 1, iv%geoamv(i)%info%levels
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%geoamv(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%geoamv(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%geoamv(i)%u(n)
         vv = ob%geoamv(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%geoamv(i)%info%lon, -1)                        
       speed_qc  = iv%geoamv(i)%u(n)%qc
       dir_qc    = speed_qc
       speed_err = iv%geoamv(i)%u(n)%error
      else
       speed     = missing_r
       speed_qc  = missing_data
       dir       = missing_r
       dir_qc    = missing_data
       speed_err = xmiss
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))         &
         iv%geoamv(i)%p(n), speed_qc, slp_error,  &
         speed, speed_qc, speed_err,              &
         dir  , dir_qc,             dir_error,    &
         missing_r, standard_atmosphere, xmiss,   &
         missing_r, zero_t_td, xmiss,             & 
         missing_r, zero_t_td, xmiss,             & 
         missing_r, zero_t_td, xmiss   
     ENDDO   
!
    ENDDO   
   ENDIF
!----------------------------------------------------------------------------!
!  Write Polar AMVs Obs:
   IF ( iv % num_polaramv > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for polaramv ',iv%num_polaramv
!
!----------------------------------------------------------------------------!
    DO i = 1, iv % num_polaramv               
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%polaramv(i)%info%platform,    &
                   iv%polaramv(i)%info%date_char,   &
                   iv%polaramv(i)%info%name,        &
                   iv%polaramv(i)%info%levels,      &
                   iv%polaramv(i)%info%lat,         &
                   iv%polaramv(i)%info%lon,         &
                   iv%polaramv(i)%info%elv,         &
                   iv%polaramv(i)%info%id
     write(ounit, FMT = fmt_loc) iv%polaramv(i)%loc%slp, iv%polaramv(i)%loc%pw
!
!----------------------------------------------------------------------------!
     DO n = 1, iv%polaramv(i)%info%levels
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%polaramv(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%polaramv(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%polaramv(i)%u(n)
         vv = ob%polaramv(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%polaramv(i)%info%lon, -1)                        
       speed_qc  = iv%polaramv(i)%u(n)%qc
       dir_qc    = speed_qc
       speed_err = iv%polaramv(i)%u(n)%error
      else
       speed     = missing_r
       speed_qc  = missing_data
       dir       = missing_r
       dir_qc    = missing_data
       speed_err = xmiss
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))           &
         iv%polaramv(i)%p(n), speed_qc, slp_error,  &
         speed, speed_qc, speed_err,                &
         dir  , dir_qc,             dir_error,      &
         missing_r, standard_atmosphere, xmiss,     &
         missing_r, zero_t_td, xmiss,               & 
         missing_r, zero_t_td, xmiss,               & 
         missing_r, zero_t_td, xmiss   
!
      ENDDO   
    ENDDO   
   ENDIF
!----------------------------------------------------------------------------!
!  Write Sounde 
   IF ( iv % num_sound > 0 ) THEN
    if(print_detail > 1) print*,' writing filtered Obs for sound  ',iv%num_sound 
!
    mnz = 0
!    write(999 ,'(5X,a,i4/)') "Sound out unit =", ounit
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
! Guo....
     nz = 0
     if (iv%sonde_sfc(i)%zk == missing_r) then
        nz = nz + 1
!        write(999,'(i3,"  Sonde_sfc==> height,zk:",2f12.2)') &
!                       nz, iv%sonde_sfc(i)%h, iv%sonde_sfc(i)%zk
     endif

     DO n = 1, iv%sound(i)%info%levels
        if (iv%sound(i)%zk(n) == missing_r) then
           nz = nz + 1
!           write(999,'(i3,"  Level=",i3," height,zk:",2f12.2)') &
!                          nz, n,iv%sound(i)%h(n), iv%sound(i)%zk(n)
        endif
     ENDDO
     if (nz > 0) then
       mnz = mnz + 1
!       write(999, FMT = fmt_info, IOSTAT = iost) &
!                   iv%sound(i)%info%platform,    &
!                   iv%sound(i)%info%date_char,   &
!                   iv%sound(i)%info%name,        &
!                   iv%sound(i)%info%levels + 1,  &
!                   iv%sound(i)%info%lat,         &
!                   iv%sound(i)%info%lon,         &
!                   iv%sound(i)%info%elv,         &
!                   iv%sound(i)%info%id
!       write(999,'(i5,a,i3)') mnz, "  Num. of outside model column =", nz
     endif
     nz = iv%sound(i)%info%levels + 1 - nz
! Guo....
     write(ounit, FMT = fmt_info, IOSTAT = iost) &
                   iv%sound(i)%info%platform,    &
                   iv%sound(i)%info%date_char,   &
                   iv%sound(i)%info%name,        &
                   nz,                           &
                   iv%sound(i)%info%lat,         &
                   iv%sound(i)%info%lon,         &
                   iv%sound(i)%info%elv,         &
                   iv%sound(i)%info%id
!                   iv%sound(i)%info%levels + 1,  &
    slp_inv= iv%sound(i)%loc%slp%inv
    slp_qc = iv%sound(i)%loc%slp%qc
    if( iv%sonde_sfc(i)%p%qc <= fails_error_max) then
    slp_inv = missing_r
    slp_qc  = missing_data
    end if
  write(ounit, FMT = fmt_loc) slp_inv,slp_qc,iv%sound(i)%loc%slp%error, iv%sound(i)%loc%pw
!----------------------------------------------------------------------------!
!
  nz = 0
  j  = 0
! ==> Not below the first model level..... (YRG)
  if (iv%sonde_sfc(i)%zk == missing_r) then
     nz = nz + 1
  else
     j = j + 1
! First write surface level information
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%sonde_sfc(i)%u%qc >= obs_qc_pointer .and. &
          iv%sonde_sfc(i)%v%qc >= obs_qc_pointer) then
         uu = ob%sonde_sfc(i)%u
         vv = ob%sonde_sfc(i)%v
         call FFDDUV( speed, dir, uu, vv, iv%sonde_sfc(i)%info%lon, -1)             
       speed_qc  = iv%sonde_sfc(i)%u%qc
       dir_qc    = speed_qc
       speed_err = iv%sonde_sfc(i)%u%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
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
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   p     = ob%sonde_sfc(i)%p
   p_qc  = iv%sonde_sfc(i)%p%qc
   p_err = iv%sonde_sfc(i)%p%error
   if( iv%sonde_sfc(i)%p%qc <= fails_error_max) then
     p_qc  = missing_data
     p     = missing_r
   p_err   = pr_error           
   end if
   t     =  ob%sonde_sfc(i)%t
   t_qc  =  iv%sonde_sfc(i)%t%qc
   t_err =  iv%sonde_sfc(i)%t%error
   if( iv%sonde_sfc(i)%t%qc <= fails_error_max) then
     t_qc  = missing_data
     t     = missing_r
     t_err = xmiss       
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))           &
         p,  p_qc,   p_err,                         &
         speed,speed_qc, speed_err ,                &
         dir  , dir_qc,  dir_err ,                  &
         iv%sonde_sfc(i)%h, zero, elv_error,        &
         t, t_qc, t_err,                            &
         td, td_qc, td_error,                       &
         rh, rh_qc,  rh_error
  endif
                
!----------------------------------------------------------------------------!
      DO n = 1, iv%sound(i)%info%levels

     if (iv%sound(i)%zk(n) == missing_r) then
       nz = nz + 1
     else
       j  = j + 1
!----------------------------------------------------------------------------!
! get speed and direction
      if( iv%sound(i)%u(n)%qc >= obs_qc_pointer .and. &
          iv%sound(i)%v(n)%qc >= obs_qc_pointer) then  
         uu = ob%sound(i)%u(n)
         vv = ob%sound(i)%v(n)
         call FFDDUV( speed, dir, uu, vv, iv%sound(i)%info%lon, -1)                        
       speed_qc  = iv%sound(i)%u(n)%qc
       dir_qc    = speed_qc
       speed_err = iv%sound(i)%u(n)%error
      else
       speed     = missing_r
       speed_qc  = missing_data
       dir       = missing_r
       dir_qc    = missing_data
       speed_err = xmiss
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
   if( rh_qc < 0) rh_qc = missing_data
!----------------------------------------------------------------------------!
   t    =  ob%sound(i)%t(n)
   t_qc =  iv%sound(i)%t(n)%qc
   t_err= iv%sound(i)%t(n)%error
   if( iv%sound(i)%t(n)%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))           &
         iv%sound(i)%p(n), zero,    pr_error,       &
         speed, speed_qc, speed_err ,               &
         dir  , dir_qc,  dir_err ,                  &
         iv%sound(i)%h(n), zero, elv_error,         &
         t,  t_qc, t_err,                           &
         td, td_qc, td_error,                       &
         rh, rh_qc,  rh_error                
 
     endif

      ENDDO 
!       write(999,'("Sonde ",i5,a,i3,a,i3,a,i3," levels"/)')               &
!                       mnz, " Written out", j, " levels, discarded", nz, &
!                       " levels total", iv%sound(i)%info%levels+1
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
       speed_qc  = iv%airep(i)%u(n)%qc
       dir_qc    = speed_qc
       speed_err = iv%airep(i)%u(n)%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
      end if
!----------------------------------------------------------------------------!
   t    =  ob%airep(i)%t(n)
   t_qc =  iv%airep(i)%t(n)%qc
   t_err= iv%airep(i)%t(n)%error
   if( iv%airep(i)%t(n)%qc <= fails_error_max) then
     t_qc = missing_data
     t   = missing_r
   end if

      write(ounit, FMT = TRIM (fmt_each))       &
         iv%airep(i)%p(n),zero , pr_error,      &
         speed, speed_qc, speed_err,            &
         dir  , dir_qc, dir_error,              &
         missing_r, standard_atmosphere, xmiss, &
         t,  t_qc,   t_err,                     &
         missing_r, missing_data, xmiss,        & 
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
       speed_qc  = iv%pilot(i)%u(n)%qc
       dir_qc    = speed_qc
       speed_err = iv%pilot(i)%u(n)%error
      else
         speed     = missing_r
       speed_qc    = missing_data
         dir       = missing_r
         dir_qc    = missing_data
         speed_err = xmiss
      end if
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))       &
         iv%pilot(i)%p(n),zero, pr_error,       &
         speed, speed_qc, speed_err,            &
         dir  , dir_qc, dir_error,              &
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
   rewind 193
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
! read original thickness error from unit 193
     read(193,'(i8,2x,i8,2f12.3)')n1,n2,thick,thick_error
     if( n1 /= i  .or. n2 /= n) then
     print*,' mismatch in reading thickness errors from unit 193 ',i,n1,n,n2
     stop
     end if
       thick_qc = iv%satem(i)%thickness(n)%qc
       if( iv%satem(i)%thickness(n)%qc < obs_qc_pointer) thick_qc = missing_data
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
         iv%qscat(i)%info%elv, zero, elv_error,    &
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
         iv%profiler(i)%info%elv, zero, elv_error, &
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

! hardwired the # of levels = 0 for GPSPW (YRG 03/02/2006)
     iv%gpspw(i)%info%levels = 0

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
    p    = ob%gpsref(i)%p(n)
    p_qc = iv%gpsref(i)%p(n)%qc 
   if( iv%gpsref(i)%p(n)%qc <= fails_error_max) then
    p    = missing_r     
    p_qc = missing_data  
   end if
    t    = ob%gpsref(i)%t(n)
   t_qc  = iv%gpsref(i)%t(n)%qc
   t_err = iv%gpsref(i)%t(n)%error   
   if( iv%gpsref(i)%t(n)%qc <= fails_error_max) then
    t    = missing_r     
    t_qc = missing_data  
   end if
   ref = ob%gpsref(i)%ref(n)
   ref_qc = iv%gpsref(i)%ref(n)%qc
   ref_err = iv%gpsref(i)%ref(n)%error   
   if( iv%gpsref(i)%ref(n)%qc <= fails_error_max) then
    ref    = missing_r     
    ref_qc = missing_data  
   endif
!----------------------------------------------------------------------------!
      write(ounit, FMT = TRIM (fmt_each))             &
         p,  p_qc, slp_error,                         &
         missing_r, missing_data, xmiss,              &
         missing_r, missing_data, xmiss,              &
         iv%gpsref(i)%h(n), zero, height_error,       &
         t, t_qc,  t_err,                             &   
         ref, ref_qc,  ref_err,                       &   
         rh, rh_qc, iv%gpsref(i)%q(n)%error
      ENDDO 
!
      ENDDO 
   ENDIF
!----------------------------------------------------------------------------!
   IF ( iv % num_ssmi_tb > 0 ) &     
   print*,'Warning ------>  Currently SSMI brightness temperature info is not written'
   rewind ounit
   call DA_Count_Filtered_Obs(ounit, xp)
   close (ounit)
!--------------------------------------------------------------------------
END SUBROUTINE da_write_filtered_obs
