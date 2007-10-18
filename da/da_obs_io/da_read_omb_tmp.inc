subroutine da_calculate_grady_ssmi(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Ob Inc. structure.
   type (y_type), intent(inout) :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use) call da_trace_entry("da_calculate_grady_ssmi")

   if (iv%nlocal(ssmi_tb) > 0) then
      do n=1, iv%nlocal(ssmi_tb)

         if (iv%ssmi_tb(n)%tb19v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19v = 0.0
         jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / &
             (iv%ssmi_tb(n)%tb19v%error * iv%ssmi_tb(n)%tb19v%error)

         if (iv%ssmi_tb(n)%tb19h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19h = 0.0
         jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / &
             (iv%ssmi_tb(n)%tb19h%error * iv%ssmi_tb(n)%tb19h%error)

         if (iv%ssmi_tb(n)%tb22v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb22v = 0.0
         jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / &
             (iv%ssmi_tb(n)%tb22v%error * iv%ssmi_tb(n)%tb22v%error)

         if (iv%ssmi_tb(n)%tb37v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37v = 0.0
         jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / &
            (iv%ssmi_tb(n)%tb37v%error * iv%ssmi_tb(n)%tb37v%error)

         if (iv%ssmi_tb(n)%tb37h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37h = 0.0
         jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / &
            (iv%ssmi_tb(n)%tb37h%error * iv%ssmi_tb(n)%tb37h%error)

         if (iv%ssmi_tb(n)%tb85v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85v = 0.0
         jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / &
            (iv%ssmi_tb(n)%tb85v%error * iv%ssmi_tb(n)%tb85v%error)

         if (iv%ssmi_tb(n)%tb85h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85h = 0.0
         jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / &
            (iv%ssmi_tb(n)%tb85h%error * iv%ssmi_tb(n)%tb85h%error)
      end do
   else if (iv%nlocal(ssmi_rv) > 0) then
      do n=1, iv%nlocal(ssmi_rv)
        if (iv%ssmi_rv(n)%speed%qc < obs_qc_pointer) then
            re%ssmi_rv(n)%speed = 0.0
        end if
        jo_grad_y%ssmi_rv(n)%Speed = - re%ssmi_rv(n)%Speed / &
           (iv%ssmi_rv(n)%Speed%error * iv%ssmi_rv(n)%Speed%error)

        if (iv%ssmi_rv(n)%tpw%qc < obs_qc_pointer) then
           re%ssmi_rv(n)%tpw = 0.0
        end if
        jo_grad_y%ssmi_rv(n)%tpw = -re%ssmi_rv(n)%tpw / &
           (iv%ssmi_rv(n)%tpw%error * iv%ssmi_rv(n)%tpw%error)
      end do  
   end if

   if (trace_use) call da_trace_exit("da_calculate_grady_ssmi")

end subroutine da_calculate_grady_ssmi


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
                     spd_obs, spd_inv, spd_qc, spd_err, spd_inc
                  if (if_write) &
                     write(omb_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                        num, k, stn_id,  &       ! Station
                        lat, lon, dummy, &       ! Lat/lon, dummy    
                        spd_obs, spd_inv, spd_qc, spd_err, spd_inc
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

