   Program Read_Diag  
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!  Program to read diagnostics written in fort.50 by WRFVAR
!  and write in proper format to get ploted using PC-XL utility
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/25/2006
!----------------------------------------------------------------------------   
  USE Define_Constants
  USE Initialize       
!----------------------------------------------------------------------------   
  IMPLICIT NONE
!                                          ! Typically 12 hours
   integer      :: num_obs 
   character*20 :: obs_type, dummy_c
   character*80 :: head               
   
   character*5  :: stn_id               
   integer      :: i,n, k, kk, l, levels, dummy_i
   real         :: lat, lon, press, miss , dummy           
   real         :: u_obs, u_inv, u_error, u_inc, & 
                   v_obs, v_inv, v_error, v_inc, &
                   t_obs, t_inv, t_error, t_inc, &
                   p_obs, p_inv, p_error, p_inc, &
                   q_obs, q_inv, q_error, q_inc, &
                   spd_obs, spd_inv, spd_err, spd_inc
   real         :: tpw_obs, tpw_inv, tpw_err, tpw_inc
   real         :: ref_obs, ref_inv, ref_err, ref_inc
    integer     :: u_qc, v_qc, t_qc, p_qc, q_qc, tpw_qc, spd_qc, ref_qc
    integer     :: npr, ier, iexp
    character*10  :: date, new_date             ! Current date (ccyymmddhh).
    integer       :: sdate, cdate, edate        ! Starting, current ending dates.
    LOGICAL       :: if_write, is_file
    CHARACTER(LEN=512)                           :: out_dir,filename

!----------------------------------------------------------------------------
  TYPE (surface_type)    :: surface
  TYPE (upr_type)        :: upr  
  TYPE (gpspw_type)      :: gpspw
  TYPE (gpsref_type)     :: gpsref
!----------------------------------------------------------------------------

   nml_unit      = 10
   diag_unit_in  = 50
   diag_unit_out = 20
   info_unit     = 30
!
  exp_num   = 0
  exp_dirs = ''
  out_dirs = ''
!
  if_plot_rmse  = .FALSE.
  if_plot_bias  = .FALSE.
  if_plot_abias = .FALSE.

  if_plot_synop     = .FALSE.
  if_plot_sonde_sfc = .FALSE.
  if_plot_metar     = .FALSE.
  if_plot_ships     = .FALSE.
  if_plot_qscat     = .FALSE.
  if_plot_buoy      = .FALSE.

  if_plot_sound     = .FALSE.
  if_plot_geoamv    = .FALSE.
  if_plot_polaramv  = .FALSE.
  if_plot_profiler  = .FALSE.
  if_plot_airep     = .FALSE.
  if_plot_pilot     = .FALSE.

  if_plot_gpspw     = .FALSE.
  if_plot_gpsref    = .FALSE.
  if_plot_airsret   = .FALSE.
!
  file_path_string = 'wrfvar/working/gts_omb_oma'

! Read in namelist information defined in module define_cons_types
!
  OPEN ( UNIT=nml_unit, FILE='namelist.plot_diag', STATUS='OLD',  &
         FORM='FORMATTED' )
  READ ( UNIT=nml_unit, NML=record1, IOSTAT=ier )
  WRITE ( UNIT=*, NML = record1 )
  IF ( ier /= 0 ) THEN
     WRITE (*,*) 'Error in reading namelist record1'
     STOP
  ENDIF

!
  READ ( UNIT=nml_unit, NML=record2, IOSTAT=ier )
  WRITE ( UNIT=*, NML = record2 )
  IF ( ier /= 0 ) THEN
     WRITE (*,*) 'Error in reading namelist record2'
     STOP
  ENDIF
  READ ( UNIT=nml_unit, NML=record3, IOSTAT=ier )
  WRITE ( UNIT=*, NML = record3 )
  IF ( ier /= 0 ) THEN
     WRITE (*,*) 'Error in reading namelist record3'
     STOP
  ENDIF
  READ ( UNIT=nml_unit, NML=record4, IOSTAT=ier )
  WRITE ( UNIT=*, NML = record4 )
  IF ( ier /= 0 ) THEN
     WRITE (*,*) 'Error in reading namelist record4'
     STOP
  ENDIF
  READ ( UNIT=nml_unit, NML=record5, IOSTAT=ier )
  WRITE ( UNIT=*, NML = record5 )
  IF ( ier /= 0 ) THEN
     WRITE (*,*) 'Error in reading namelist record5'
     STOP
  ENDIF
  CLOSE(nml_unit)
!----------------------------------------------------------------------------   
!  Loop over experiments 
  Do iexp  =1,exp_num
!----------------------------------------------------------------------------   
!
    if_plot_surface = .false.
    if( if_plot_synop .or. if_plot_metar .or. if_plot_ships .or. if_plot_buoy .or. &
         if_plot_sonde_sfc .or. if_plot_qscat   ) if_plot_surface = .true.

    if_plot_upr = .false.
    if( if_plot_sound .or. if_plot_pilot .or. if_plot_profiler   .or.    &
        if_plot_geoamv .or. if_plot_polaramv  .or. if_plot_airep .or.    &
        if_plot_airsret                                                  &
       ) if_plot_upr= .true.

!----------------------------------------------------------------------------   
!                                          ! Typically 12 hours
   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Diag Starting date ', start_date, ' Ending date ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, ' hours.'

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )         

!    Initialize various types
     call initialize_surface_type(surface)
     call initialize_upr_type(upr)
     call initialize_gpspw_type(gpspw)
     call initialize_gpsref_type(gpsref)

!----------------------------------------------------------------------------   
! construct file name
!
     filename = TRIM(exp_dirs(iexp))//'/'//date//'/'//trim(file_path_string)

! check if the file exists, then open the file

  INQUIRE ( FILE=trim(filename), EXIST=is_file) 
  IF ( .not. is_file ) THEN
     WRITE(0,*) 'STOP!! can not find the file: ', TRIM(filename)
     STOP
  ENDIF
  OPEN (UNIT=diag_unit_in, FILE=TRIM(filename), FORM='formatted',   &
        STATUS='old', IOSTAT=ier)
!----------------------------------------------------------------------------   
1  continue
!----------------------------------------------------------------------------   
   if_write = .false.
   read(diag_unit_in,'(a20,i8)', end=2000, err = 1000)obs_type,num_obs                    
   if( index( obs_type,'synop') > 0 ) then
    if( if_plot_synop ) if_write = .true.
    go to 10
   elseif( index( obs_type,'metar') > 0 ) then 
    if( if_plot_metar ) if_write = .true.
    go to 10
   elseif( index( obs_type,'ships') > 0 )  then
    if( if_plot_ships ) if_write = .true.
   go to 10
   elseif( index( obs_type,'buoy' ) > 0 )  then
    if( if_plot_buoy ) if_write = .true.
   go to 10
   elseif( index( obs_type,'sonde_sfc') > 0 )  then
    if( if_plot_sonde_sfc ) if_write = .true.
   go to 10

   elseif( index( obs_type,'polaramv') > 0)  then
    if( if_plot_polaramv ) if_write = .true.
   go to 20
   elseif( index( obs_type,'geoamv'  ) > 0)  then
    if( if_plot_geoamv ) if_write = .true.
   go to 20

   elseif( index( obs_type,'gpspw') > 0)  then
    if( if_plot_gpspw ) if_write = .true.
   go to 30

   elseif( index( obs_type,'sound') > 0)  then
    if( if_plot_sound ) if_write = .true.
    go to 40

   elseif( index( obs_type,'airep') > 0)  then
    if( if_plot_airep ) if_write = .true.
   go to 50

   elseif( index( obs_type,'pilot')    > 0)  then
    if( if_plot_pilot ) if_write = .true.
   go to 60
   elseif( index( obs_type,'profiler') > 0)  then
    if( if_plot_profiler ) if_write = .true.
   go to 60

   elseif( index( obs_type,'ssmir') > 0)  then
   go to 70

   elseif( index( obs_type,'ssmiT') > 0)  then
   go to 80

   elseif( index( obs_type,'satem') > 0)  then
   go to 90

   elseif( index( obs_type,'ssmt1') > 0)  then
   go to 100

   elseif( index( obs_type,'ssmt2') > 0)  then
   go to 100

   elseif( index( obs_type,'qscat') > 0)  then
    if( if_plot_qscat ) if_write = .true.
   go to 110
   elseif( index( obs_type,'gpsref' ) > 0) then
   if( if_plot_gpsref ) if_write = .true.
    go to 120
   elseif( index( obs_type,'airsr') > 0)  then
    if( if_plot_airsret ) if_write = .true.
    go to 130
   else
   print*,' Got unknown OBS_TYPE ',obs_type(1:20),' on unit ',diag_unit_in
   Stop    
   endif
!---------------------------------------------------------------------   
     10 continue      !   Synop, Metar, Ships, Buoy , Sonde_sfc
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         p_obs, p_inv, p_qc, p_error, p_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
          if (if_write) then
            if( u_qc >=  0) call update_stats(surface%uomb, surface%uoma, u_inv, u_inc)
            if( v_qc >=  0) call update_stats(surface%vomb, surface%voma, v_inv, v_inc)
            if( t_qc >=  0) call update_stats(surface%tomb, surface%toma, t_inv, t_inc)
            if( p_qc >=  0) call update_stats(surface%pomb, surface%poma, p_inv, p_inc)
            if( q_qc >=  0) call update_stats(surface%qomb, surface%qoma, q_inv, q_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   go to 1

!---------------------------------------------------------------------   
         20 continue      !    Polar or Geo AMV's
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &        ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc

          if (if_write .and. press > 0 ) then
            call get_std_pr_level(press, npr, stdp, nstd) 
            if( u_qc >=  0) call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
            if( v_qc >=  0) call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF

   go to 1

!----------------------------------------------------------------------------   
                30 continue      !    Gpspw  
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
          if (if_write) then
            if( tpw_qc >=  0) call update_stats(gpspw%tpwomb,gpspw%tpwoma,tpw_inv,tpw_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF

   go to 1
!----------------------------------------------------------------------------   
                40 continue      !    Sound 
!----------------------------------------------------------------------------   
!  [6] Transfer sound obs:

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
          if (if_write .and. press > 0 ) then
            call get_std_pr_level(press, npr, stdp, nstd) 
            if( u_qc >=  0) call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
            if( v_qc >=  0) call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
            if( t_qc >=  0) call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
            if( q_qc >=  0) call update_stats(upr%qomb(npr),upr%qoma(npr),q_inv,q_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   go to 1
!---------------------------------------------------------------------   
         50 continue      !    Airep  
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)') levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc    
          if (if_write .and. press > 0 ) then
            call get_std_pr_level(press, npr, stdp, nstd) 
            if( u_qc >=  0) call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
            if( v_qc >=  0) call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
            if( t_qc >=  0) call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF

   go to 1
!---------------------------------------------------------------------   
         60 continue      !    Pilot & Profiler  
!---------------------------------------------------------------------   
!  [8] Transfer pilot obs:
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
          if (if_write .and. press > 0 ) then
            call get_std_pr_level(press, npr, stdp, nstd) 
            if( u_qc >=  0) call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
            if( v_qc >=  0) call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   go to 1

!---------------------------------------------------------------------   
         70 continue      !  SSMI Radiance
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      read(diag_unit_in,'(i8)')levels
      DO n = 1, num_obs    
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         spd_obs, spd_inv, spd_qc, spd_err, spd_inc
      END DO
   ENDIF

   go to 1
!---------------------------------------------------------------------   
         80 continue      !  SSMI radiance   
!---------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,*)dummy_c                                 
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy
      END DO
   ENDIF
   go to 1
!---------------------------------------------------------------------   
         90 continue      !  SATEM           
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)') levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   
   go to 1
!---------------------------------------------------------------------   
         100  continue      !  SSMT1 & 2           
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)') levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF

   go to 1
!---------------------------------------------------------------------   
         110  continue      !  Scatrometer winds   
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)') levels
         DO k = 1, levels
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
          if (if_write) then
            if( u_qc >=  0) call update_stats(surface%uomb,surface%uoma,u_inv,u_inc)
            if( v_qc >=  0) call update_stats(surface%vomb,surface%voma,v_inv,v_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   go to 1

!---------------------------------------------------------------------   
         120  continue      !  Gpsref              
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         ref_obs, ref_inv, ref_qc, ref_err, ref_inc
          if (if_write) then
            if( ref_qc >=  0) call update_stats(gpsref%refomb,gpsref%refoma,ref_inv,ref_inc)
          endif
      END DO
   ENDIF
   go to 1
!---------------------------------------------------------------------   
         130  continue      !  AIRSRET             
!---------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(diag_unit_in,'(i8)')levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
          if (if_write .and. press > 0 ) then
            call get_std_pr_level(press, npr, stdp, nstd) 
            if( t_qc >=  0) call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
            if( q_qc >=  0) call update_stats(upr%qomb(npr),upr%qoma(npr),q_inv,q_inc)
          endif
      END DO      !  loop over levels
      END DO      !  loop over Obs    
   ENDIF
   go to 1
!-----------------------------------------------------------------------
! Now process the diagnostics
2000   continue
   close (diag_unit_in)
!  Write output on outunit
   out_dir=trim(out_dirs(iexp))
   if( if_plot_surface  )  then
       call write_diag_single_level(out_dir,diag_unit_out,date,'surface_u',surface%uomb,surface%uoma)     
       call write_diag_single_level(out_dir,diag_unit_out,date,'surface_v',surface%vomb,surface%voma)     
       call write_diag_single_level(out_dir,diag_unit_out,date,'surface_t',surface%tomb,surface%toma)     
       call write_diag_single_level(out_dir,diag_unit_out,date,'surface_p',surface%pomb,surface%poma)     
       call write_diag_single_level(out_dir,diag_unit_out,date,'surface_q',surface%qomb,surface%qoma)     
   endif      
!
!
   if( if_plot_gpspw )  then
       call write_diag_single_level(out_dir,diag_unit_out,date,'gpspw_tpw',gpspw%tpwomb,gpspw%tpwoma)     
   endif
!
   if( if_plot_gpsref  )  then
       call write_diag_single_level(out_dir,diag_unit_out,date,'gpsref_ref',gpsref%refomb,gpsref%refoma)     
   endif

   if( if_plot_upr ) then
   call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_u',upr%uomb,upr%uoma)
   call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_v',upr%vomb,upr%voma)
   call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_t',upr%tomb,upr%toma)
   call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_q',upr%qomb,upr%qoma)
   endif
!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over date   

!---------------------------------------------------------------------   
  ENDDO   ! Loop over experiments
  stop
!---------------------------------------------------------------------   
1000  print*,' Error while reading unit ',diag_unit_in,' for experiment ',exp_dirs(iexp)   
      stop
CONTAINS
      subroutine get_std_pr_level(prs, npr, stdp, nstd) 
      implicit none
!
      integer, intent(in )      :: nstd
      real,    intent(in)       :: stdp(nstd)       
      integer, intent(out)      :: npr
      real,    intent(in)       :: prs              

      real                      :: pr
      integer                   :: k   
!
      pr = prs/100.0
      if      ( pr >= stdp(1)    ) then
       npr = 1
       return
      else if ( pr < stdp(nstd-1) ) then
       npr = nstd
       return
      else
       do k = 2,nstd - 1
       if( pr   >= stdp(k) ) then
       npr = k 
       return
       endif
       enddo
      endif
     
    end subroutine get_std_pr_level

     subroutine update_stats(stats_omb, stats_oma, omb, oma) 
     implicit none
     type(stats_value),   intent(inout)   :: stats_omb, stats_oma  
     real, intent (in)                    :: omb, oma
!
     real      :: x1, x2
!
     stats_omb%num  = stats_omb%num + 1
     stats_oma%num  = stats_omb%num 

     x1 = 1.0/ stats_omb%num  
     x2 =  (stats_omb%num-1)*x1

     stats_omb%bias  = x2*stats_omb%bias  + omb  * x1   
     stats_oma%bias  = x2*stats_oma%bias  + oma  * x1   

     stats_omb%abias = x2*stats_omb%abias + abs(omb) * x1   
     stats_oma%abias = x2*stats_oma%abias + abs(oma) * x1   

     stats_omb%rmse  = x2*stats_omb%rmse  + omb*omb  * x1   
     stats_oma%rmse  = x2*stats_oma%rmse  + oma*oma  * x1   
!     
     end subroutine update_stats 

     subroutine write_diag_single_level(out_dir,ounit,date,obs_type,omb,oma)     
     implicit none
     integer, intent(in)            :: ounit
     character*512,intent(in)       :: out_dir          
     character*10,intent(in)        :: date       
     character*(*),intent(in)       :: obs_type
     type (stats_value),intent(in)  :: omb
     type (stats_value),intent(in)  :: oma
! 
     character*512                  :: filename         
     integer                        :: ounit1, ounit2
     logical                        :: is_file

     ounit1 = ounit
     ounit2 = ounit + 1
     
     filename = trim(out_dir)//'/'//trim(obs_type)//'_omb.diag'
     open (ounit1, file = trim(filename), form='formatted',status='unknown',position='append')                         
     filename = trim(out_dir)//'/'//trim(obs_type)//'_oma.diag'
     open (ounit2, file = trim(filename), form='formatted',status='unknown',position='append')                         

     if ( omb%num == 0 ) then    
     write(ounit1,'(1x,a10,1x,i5,3(1x,f6.2))') date,num_miss, rmiss, rmiss, rmiss
     write(ounit2,'(1x,a10,1x,i5,3(1x,f6.2))') date,num_miss, rmiss, rmiss, rmiss
     else
!     write(ounit1,'(5x,a10,4(2x,a9))') trim(obs_type),' Number','BIAS','ABIAS','RMSE'
     if( index(obs_type,'_q') > 0 ) then
     write(ounit1,'(1x,a10,1x,i5,3(1x,f6.3))') date,omb%num, 1000.0*omb%bias, 1000.0*omb%abias, 1000.0*sqrt(omb%rmse)
     else if( index(obs_type,'_p') > 0 ) then
     write(ounit1,'(1x,a10,1x,i5,3(1x,f6.2))') date,omb%num, omb%bias/100.0, omb%abias/100.0, sqrt(omb%rmse)/100.0
     else
     write(ounit1,'(1x,a10,1x,i5,3(1x,f6.2))') date,omb%num, omb%bias, omb%abias, sqrt(omb%rmse)
     endif
!          
!     write(ounit2,'(5x,a10,4(2x,a9))') trim(obs_type),' Number','BIAS','ABIAS','RMSE'
     if( index(obs_type, '_q') > 0 ) then
     write(ounit2,'(1x,a10,1x,i5,3(1x,f6.3))') date,oma%num, 1000.*oma%bias, 1000.*oma%abias, 1000.*sqrt(oma%rmse)
     else if( index(obs_type,'_p') > 0 ) then
     write(ounit2,'(1x,a10,1x,i5,3(1x,f6.2))') date,oma%num, oma%bias/100.0, oma%abias/100.0, sqrt(oma%rmse)/100.0
     else
     write(ounit2,'(1x,a10,1x,i5,3(1x,f6.2))') date,oma%num, oma%bias, oma%abias, sqrt(oma%rmse)
     endif
     endif
!
     close(ounit1)
     close(ounit2)
!     
     end subroutine write_diag_single_level     
!
     subroutine write_diag_multi_level(out_dir,ounit,date,obs_type,omb,oma)     
     implicit none
     integer, intent(in)            :: ounit
     character*512,intent(in)       :: out_dir         
     character*10,intent(in)        :: date       
     character*(*),intent(in)       :: obs_type
     type (stats_value),intent(in)  :: omb(nstd)
     type (stats_value),intent(in)  :: oma(nstd)
! 
     character*512                  :: filename         
     integer                        :: k
     integer                        :: num(nstd)
     real, dimension(nstd)          :: rmse, bias, abias
     
     integer                        :: ounit1, ounit2
!
     ounit1 = ounit
     ounit2 = ounit + 1
!

     filename = trim(out_dir)//'/'//trim(obs_type)//'_omb.diag'
     open (ounit1, file = trim(filename), form='formatted',status='unknown',position='append')                         
     filename = trim(out_dir)//'/'//trim(obs_type)//'_oma.diag'
     open (ounit2, file = trim(filename), form='formatted',status='unknown',position='append')                         

     do k = 1, nstd
     num(k) = omb(k)%num
     if( num(k) == 0 ) then
     num(k) = num_miss     
     rmse(k)  = rmiss       
     bias(k)  = rmiss       
     abias(k) = rmiss                 
     else
        if( index(obs_type,'_q') > 0 ) then
    
         rmse(k) = sqrt(omb(k)%rmse) * 1000
         bias(k) = omb(k)%bias * 1000
         abias(k) = omb(k)%abias * 1000
       else
        rmse(k) = sqrt(omb(k)%rmse)
        bias(k) = omb(k)%bias
        abias(k) = omb(k)%abias
       endif
     endif
     enddo
     
     write(ounit1,'(1x,a10,1x,16(1x,i5,3(1x,f6.2)))')date, (num(k), bias(k), abias(k), rmse(k),k=1,nstd)

     do k = 1, nstd
     num(k) = oma(k)%num
     if( num(k) == 0 ) then
     num(k) = num_miss     
     rmse(k)  = rmiss       
     bias(k)  = rmiss       
     abias(k) = rmiss                 
     else
        if( index(obs_type,'_q') > 0 ) then
    
         rmse(k) = sqrt(oma(k)%rmse) * 1000
         bias(k) = oma(k)%bias * 1000
         abias(k) = oma(k)%abias * 1000
       else
        rmse(k) = sqrt(oma(k)%rmse)
        bias(k) = oma(k)%bias
        abias(k) = oma(k)%abias
       endif
     endif
     enddo
     write(ounit2,'(1x,a10,1x,16(1x,i5,3(1x,f6.2)))')date, (num(k), bias(k), abias(k), rmse(k),k=1,nstd)

!          
     close(ounit1)
     close(ounit2)
!     
     end subroutine write_diag_multi_level     

!
END Program Read_Diag  
