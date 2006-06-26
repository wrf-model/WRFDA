subroutine da_read_fort50(unit_in,obs_type_in, nc)
!----------------------------------------------------------------------------   
!  Program to read diagnostics written in fort.50 by WRF-Var
!  Author:   Syed RH Rizvi
!            05/16/2005
!----------------------------------------------------------------------------   
   IMPLICIT NONE

   INTEGER      ,intent (IN) :: unit_in
   character*(*),intent (IN) :: obs_type_in                 
   INTEGER      ,intent (IN) :: nc      
!
   integer      :: num_obs 
   character*9  :: dummy_c
   character*20 :: ob_type               
   logical   :: if_write
   
   character*5  :: stn_id               
   integer      :: i,n, k, kk, l, levels, dummy_i
   real         :: lat, lon, press, dummy           
   real         :: tpw_obs, tpw_inv, tpw_err, tpw_inc
   real         :: u_obs, u_inv, u_error, u_inc, & 
                   v_obs, v_inv, v_error, v_inc, &
                   t_obs, t_inv, t_error, t_inc, &
                   p_obs, p_inv, p_error, p_inc, &
                   q_obs, q_inv, q_error, q_inc, &
                   spd_obs, spd_inv, spd_err, spd_inc
    integer     :: u_qc, v_qc, t_qc, p_qc, q_qc, tpw_qc, spd_qc, npr
!----------------------------------------------------------------------------   
1  continue
!----------------------------------------------------------------------------   
   read(unit_in,'(a20,i8)', end = 999, err = 1000)ob_type,num_obs                    
   if( ob_type(1:5) =='*****') return
   
   if_write = .FALSE.
   if( index(ob_type,OBS_TYPE_IN(1:nc)) > 0 ) if_write = .TRUE.
   if( index( ob_type,'synop') > 0) go to 10
   if( index( ob_type,'metar') > 0) go to 10
   if( index( ob_type,'ships') > 0) go to 10
   if( index( ob_type,'buoy' ) > 0) go to 10
   if( index( ob_type,'sonde_sfc') > 0 ) go to 10

   if( index( ob_type,'polaramv') > 0) go to 20
   if( index( ob_type,'geoamv'  ) > 0) go to 20

   if( index( ob_type,'gpspw') > 0) go to 30

   if( index( ob_type,'sound') > 0) go to 40

   if( index( ob_type,'airep') > 0) go to 50

   if( index( ob_type,'pilot')    > 0) go to 60
   if( index( ob_type,'profiler') > 0) go to 60

   if( index( ob_type,'ssmir') > 0) go to 70

   if( index( ob_type,'ssmiT') > 0) go to 80

   if( index( ob_type,'satem') > 0) go to 90

   if( index( ob_type,'ssmt1') > 0) go to 100
   if( index( ob_type,'ssmt2') > 0) go to 100

   if( index( ob_type,'qscat') > 0) go to 110

   if( index( ob_type,'*****') > 0) return   

   write(unit=stdout, fmt='(/a,a20,a,i3/a/)') &
              'Got unknown obs_type string:', trim(ob_type),' on unit ',unit_in, & 
              'in da_read_fort50'

   stop
    
!----------------------------------------------------------------------------   
                10 continue      !   Synop, Metar, Ships, Buoy , Sonde_sfc
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         p_obs, p_inv, p_qc, p_error, p_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
        if(if_write) &
         write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         p_obs, p_inv, p_qc, p_error, p_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc

      END DO
   ENDIF
   if (if_write) return  
   go to 1

!----------------------------------------------------------------------------   
                20 continue      !    Polar or Geo AMV's
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
         read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &        ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
        if(if_write) &
         write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &        ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc

         END DO

      END DO
   ENDIF

   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                30 continue      !    Gpspw  
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
         if(if_write) &
         write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
      END DO
   ENDIF

   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                40 continue      !    Sounde 
!----------------------------------------------------------------------------   
!  [6] Transfer sonde obs:

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
           if(if_write) &
           write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
         END DO
      END DO
   ENDIF
   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                50 continue      !    Airep  
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc    
           if(if_write) &
           write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc    
         END DO
      END DO
   ENDIF

   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                60 continue      !    Pilot & Profiler  
!----------------------------------------------------------------------------   
!  [8] Transfer pilot obs:

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
           if(if_write) &
           write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         END DO
      END DO
   ENDIF
   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                70 continue      !  SSMI Radiance
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         spd_obs, spd_inv, spd_qc, spd_err, spd_inc
           if(if_write) &
           write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         spd_obs, spd_inv, spd_qc, spd_err, spd_inc
      END DO
   ENDIF

   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                80 continue      !  SSMI radiance   
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,*)dummy_c                                 
         if(if_write) write(omb_unit,*)dummy_c          
         read(unit_in,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy
         if(if_write) &
         write(omb_unit,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
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
   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                90 continue      !  SATEM           
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
            if(if_write) &
            write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
         END DO
      END DO
   ENDIF
   
   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                100  continue      !  SSMT1 & 2           
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) write(omb_unit,'(i8)')levels
         DO k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
            if(if_write) &
            write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
         END DO
      END DO
   ENDIF

   if (if_write) return  
   go to 1
!----------------------------------------------------------------------------   
                110  continue      !  Scatrometer winds   
!----------------------------------------------------------------------------   
   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         if(if_write) &
         write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         k, l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
      END DO
   ENDIF

   if (if_write) return  
   go to 1
!--------------------------------------------------------------
999  continue
   write(unit=stdout, fmt='(/a,i3/a/)') &
              'unexpected end of file on unit: ',unit_in, & 
              'in da_read_fort50'
      stop
1000  continue
   write(unit=stdout, fmt='(/a,i3/a/)') &
              'read error on unit: ',unit_in, & 
              'in da_read_fort50'
      stop
end subroutine da_read_fort50
