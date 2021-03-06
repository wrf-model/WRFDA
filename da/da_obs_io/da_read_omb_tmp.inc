subroutine da_read_omb_tmp(filename,unit_in,num,obs_type_in, nc)

   !-------------------------------------------------------------------------
   ! read diagnostics written to temporary file by WRFVAR
   !-------------------------------------------------------------------------

   implicit none

   integer      ,intent (in)    :: unit_in
   integer      ,intent (inout) :: num      
   character*(*),intent (in)    :: obs_type_in, filename                 
   integer      ,intent (in)    :: nc      

   integer      :: num_obs, ios 
   character*20 :: iv_type               
   logical   :: if_write
   
   character*5  :: stn_id               
   integer      :: n, k, kk, l, levels, dummy_i
   real         :: lat, lon, press, height, dummy           
   real         :: tpw_obs, tpw_inv, tpw_err, tpw_inc
   real         :: u_obs, u_inv, u_error, u_inc, & 
                   v_obs, v_inv, v_error, v_inc, &
                   t_obs, t_inv, t_error, t_inc, &
                   p_obs, p_inv, p_error, p_inc, &
                   q_obs, q_inv, q_error, q_inc, &
                   ref_obs, ref_inv, ref_error, ref_inc, &
                   spd_obs, spd_inv, spd_err, spd_inc
   integer     :: u_qc, v_qc, t_qc, p_qc, q_qc, tpw_qc, spd_qc, ref_qc

   if (trace_use_dull) call da_trace_entry("da_read_omb_tmp")

   open(unit=unit_in,file=trim(filename),form='formatted',status='old',iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, (/"Cannot open file"//trim(filename)/))
   end if

   reports: do

      read(unit_in,'(a20,i8)', end = 999, err = 1000) iv_type,num_obs
      if_write = .false.
      if (index(iv_type,OBS_type_in(1:nc)) > 0) if_write = .true.

      select case (trim(adjustl(iv_type)))

      case ('synop' , 'metar' , 'ships' , 'buoy' , 'sonde_sfc' )
         if (num_obs > 0) then
            do n = 1, num_obs    
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
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('geoamv' , 'polaramv' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
               num = num + 1
               end if
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

               end do 
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('gpspw' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
               num = num + 1
               end if
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
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('sound' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                   write(omb_unit,'(i8)')levels
                   num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc, &
                     t_obs, t_inv, t_qc, t_error, t_inc, &
                     q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num, k, stn_id,  &       ! Station
                        lat, lon, press, &       ! Lat/lon, dummy    
                        u_obs, u_inv, u_qc, u_error, u_inc, & 
                        v_obs, v_inv, v_qc, v_error, v_inc, &
                        t_obs, t_inv, t_qc, t_error, t_inc, &
                        q_obs, q_inv, q_qc, q_error, q_inc
               end do 
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('airep' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc, &
                     t_obs, t_inv, t_qc, t_error, t_inc    
                 if (if_write) &
                    write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                       num, k, stn_id,  &       ! Station
                       lat, lon, press, &       ! Lat/lon, dummy    
                       u_obs, u_inv, u_qc, u_error, u_inc, & 
                       v_obs, v_inv, v_qc, v_error, v_inc, &
                       t_obs, t_inv, t_qc, t_error, t_inc    
               end do  
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('pilot' , 'profiler' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc
                 if (if_write) &
                    write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                       num, k, stn_id,  &       ! Station
                       lat, lon, press, &       ! Lat/lon, dummy    
                       u_obs, u_inv, u_qc, u_error, u_inc, & 
                       v_obs, v_inv, v_qc, v_error, v_inc
               end do  
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('ssmir' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     spd_obs, spd_inv, spd_qc, spd_err, spd_inc, &
                     tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num, k, stn_id,  &       ! Station
                        lat, lon, dummy, &       ! Lat/lon, dummy    
                        spd_obs, spd_inv, spd_qc, spd_err, spd_inc, &
                        tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports
   
      case ('ssmit' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)')levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
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
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('satem' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     dummy,dummy, dummy_i, dummy, dummy
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num,k,stn_id, &          ! Station
                        lat, lon, dummy, &       ! Lat/lon, dummy    
                        dummy,dummy, dummy_i, dummy, dummy
               end do  
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('ssmt1' , 'ssmt2' )
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     dummy,dummy, dummy_i, dummy, dummy
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num,k,stn_id, &          ! Station
                        lat, lon, dummy, &       ! Lat/lon, dummy    
                        dummy,dummy, dummy_i, dummy, dummy
               end do 
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('qscat' )          
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                      kk,l, stn_id, &          ! Station
                      lat, lon, press, &       ! Lat/lon, dummy    
                      u_obs, u_inv, u_qc, u_error, u_inc, & 
                      v_obs, v_inv, v_qc, v_error, v_inc
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         u_obs, u_inv, u_qc, u_error, u_inc, & 
                         v_obs, v_inv, v_qc, v_error, v_inc
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('bogus' )          
         ! TC Bogus data is written in two records
         ! 1st record holds info about surface level
         ! 2nd is for upper air

         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
                  num = num + 1 
               end if
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
               end do
               read(unit_in,'(i8)') levels
               if (if_write) then
                  write(omb_unit,'(i8)')levels
               end if
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
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('airsr' )          
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) write(omb_unit,'(i8)')levels
               num = num + 1
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     t_obs, t_inv, t_qc, t_error, t_inc, & 
                     q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         num,k,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                         t_obs, t_inv, t_qc, t_error, t_inc, & 
                         q_obs, q_inv, q_qc, q_error, q_inc
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case ('gpsref' )          
         if (num_obs > 0) then
            do n = 1, num_obs    
               read(unit_in,'(i8)') levels
               if (if_write) write(omb_unit,'(i8)')levels
               num = num + 1
               do k = 1, levels
                  read(unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, height, &       ! Lat/lon, height   
                     ref_obs, ref_inv, ref_qc, ref_error, ref_inc 
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num,k,stn_id, &          ! Station
                        lat, lon, height, &       ! Lat/lon, height   
                        ref_obs, ref_inv, ref_qc, ref_error, ref_inc 
               end do
            end do
         end if
         if (if_write) exit reports
         cycle reports

      case default;

         write(unit=message(1), fmt='(a,a20,a,i3)') &
            'Got unknown obs_type string:', trim(iv_type),' on unit ',unit_in
         call da_error(__FILE__,__LINE__,message(1:1))
      end select
   end do reports 

999 continue
   close (unit_in)

   if (trace_use_dull) call da_trace_exit("da_read_omb_tmp")
   return

1000 continue
   write(unit=message(1), fmt='(a,i3)') &
      'read error on unit: ',unit_in
   call da_warning(__FILE__,__LINE__,message(1:1))

end subroutine da_read_omb_tmp

