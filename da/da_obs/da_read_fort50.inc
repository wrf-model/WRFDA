subroutine da_read_fort50(filename,unit_in,num,obs_type_in, nc)

   !-------------------------------------------------------------------------
   ! read diagnostics written in fort.50 by WRFVAR
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER      ,intent (IN)    :: unit_in
   INTEGER      ,intent (INOUT) :: num      
   character*(*),intent (IN)    :: obs_type_in, filename                 
   INTEGER      ,intent (IN)    :: nc      

   integer      :: num_obs, ios 
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

   open(UNIT=unit_in,file=trim(filename),form='formatted',status='old',iostat=ios)
   if (ios /= 0 ) Then
      call da_error(__FILE__,__LINE__, &
         (/"Canot open file"//trim(filename)/))
   Endif

1  continue

   read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type,num_obs
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

   if( index( ob_type,'bogus') > 0) go to 120
   if( index( ob_type,'airsr') > 0) go to 130

   write(unit=errmsg(1), fmt='(a,a20,a,i3)') &
      'Got unknown obs_type string:', trim(ob_type),' on unit ',unit_in
   call da_error(__FILE__,__LINE__,errmsg(1:1))
    
!----------------------------------------------------------------------------
10 continue      !   Synop, Metar, Ships, Buoy , Sonde_sfc
!----------------------------------------------------------------------------

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if (if_write) then
            write(omb_unit,'(i8)')levels
         num = num + 1
         end if
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         p_obs, p_inv, p_qc, p_error, p_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
            if (if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         p_obs, p_inv, p_qc, p_error, p_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
         enddo
      END DO
   ENDIF
   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
20 continue      !    Polar or Geo AMV's
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if (if_write) then
            write(omb_unit,'(i8)')levels
         num = num + 1
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, press, &        ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
            if (if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id, &          ! Station
                         lat, lon, press, &        ! Lat/lon, pressure
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc

         enddo 
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
30 continue      !    Gpspw  
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
         num = num + 1
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
            if (if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id,  &       ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
         enddo
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
40 continue      !    Sound 
!----------------------------------------------------------------------------   

   !  [6] Transfer sonde obs:

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) then
             write(omb_unit,'(i8)')levels
             num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id,  &       ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc, &
                         q_obs, q_inv, q_qc, q_error, q_inc
         enddo 
      END DO
   ENDIF
   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
50 continue      !    Airep  
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc    
           if(if_write) &
              write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id,  &       ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc, &
                         t_obs, t_inv, t_qc, t_error, t_inc    
         enddo  
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
60 continue      !    Pilot & Profiler  
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
           if(if_write) &
              write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id,  &       ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         enddo  
      END DO
   ENDIF
   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
70 continue      !  SSMI Radiance
!----------------------------------------------------------------------------   

   IF (num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         spd_obs, spd_inv, spd_qc, spd_err, spd_inc
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num, k, stn_id,  &       ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         spd_obs, spd_inv, spd_qc, spd_err, spd_inc
         enddo
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
80 continue      !  SSMI radiance   
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)')levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy
            if (if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy, &    
                         dummy, dummy, dummy_i, dummy, dummy
         enddo
      END DO
   ENDIF
   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
90 continue      !  SATEM           
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
         enddo  
      END DO
   ENDIF
   
   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
100  continue      !  SSMT1 & 2           
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, dummy, &       ! Lat/lon, dummy    
                         dummy,dummy, dummy_i, dummy, dummy
         enddo 
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
110  continue      !  Scatrometer winds   
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         enddo
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!----------------------------------------------------------------------------   
120  continue      !  TC bogus data           
! Note:      Syed  RH Rizvi   06/26/2006   
! TC Bogus data is written in two records
! 1st record holds info about surface level
! 2nd is for upper air
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if(if_write) then
            write(omb_unit,'(i8)')levels
            num = num + 1 
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,l,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         enddo
         read(unit_in,'(i8)') levels
         if (if_write) then
            write(omb_unit,'(i8)')levels
         endif
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
            if (if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,l,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
         enddo
      END DO
   ENDIF

!----------------------------------------------------------------------------   
130  continue      !  AIRS  retrievals        
!----------------------------------------------------------------------------   

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs    
         read(unit_in,'(i8)') levels
         if (if_write) write(omb_unit,'(i8)')levels
         num = num + 1
         do k = 1, levels
            read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk,l, stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         t_obs, t_inv, t_qc, t_error, t_inc, & 
                         q_obs, q_inv, q_qc, q_error, q_inc
            if(if_write) &
               write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         t_obs, t_inv, t_qc, t_error, t_inc, & 
                         q_obs, q_inv, q_qc, q_error, q_inc
         enddo
      END DO
   ENDIF

   if (if_write) go to 999
   go to 1

!--------------------------------------------------------------

999 continue
    close ( unit_in )
    return
1000 continue
   write(unit=errmsg(1), fmt='(/a,i3/a/)') &
      'read error on unit: ',unit_in, & 
      'in da_read_fort50'
   call da_error(__FILE__,__LINE__,errmsg(1:1))

end subroutine da_read_fort50