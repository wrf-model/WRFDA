subroutine da_write_y (iv, y)

   !-------------------------------------------------------------------------   
   ! Purpose: Writes out components of y=H(x_inc) structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (ob_type), intent(in)    :: iv   ! O-B structure.
   type (y_type), intent(in)     :: y    ! y = H(x_inc) structure.

   integer                       :: ounit ! Output file unit.
   integer                       :: n, k, num_obs, i, ios
   real                          :: f1, f2, f3, f4, f5, f6, f7, dum
   character(len=filename_len)   :: ob_name, filename, file_prefix

   if (trace_use) call da_trace_entry("da_write_y")

   !-------------------------------------------------------------------------   
   ! Fix output unit
   !-------------------------------------------------------------------------   

   if (omb_add_noise) then
      file_prefix='pert_obs.'
   else
      file_prefix='unpert_obs.'
   end if

   dum = -999999.9

#ifdef DM_PARALLEL
    write (unit=filename, fmt='(a,i3.3)') trim(file_prefix), myproc
#else
    write (unit=filename, fmt='(a)') trim(file_prefix)//'000'
#endif

   call da_get_unit(ounit)
   open (unit=ounit,file=trim(filename),form='formatted', &
         status='replace', iostat=ios )
   if (ios /= 0) Then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open (un)perturbed observation file"//filename/))
   end if

   ! [1] Transfer surface obs:

   if (iv % num_synop > 0) then
      num_obs = 0
      do n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'synop', num_obs
         num_obs = 0
         do n = 1, iv % num_synop
            if (iv%synop(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%synop(n)%u%qc, y%synop(n)%u, f1)
               call da_check_missing(iv%synop(n)%v%qc, y%synop(n)%v, f2)
               call da_check_missing(iv%synop(n)%t%qc, y%synop(n)%t, f3)
               call da_check_missing(iv%synop(n)%p%qc, y%synop(n)%p, f4)
               call da_check_missing(iv%synop(n)%q%qc, y%synop(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [2] Transfer metar obs:

   if (iv % num_metar > 0) then
      num_obs = 0
      do n = 1, iv % num_metar
         if(iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'metar', num_obs
         num_obs = 0
         DO n = 1, iv % num_metar
            if (iv%metar(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%metar(n)%u%qc, y%metar(n)%u, f1)
               call da_check_missing(iv%metar(n)%v%qc, y%metar(n)%v, f2)
               call da_check_missing(iv%metar(n)%t%qc, y%metar(n)%t, f3)
               call da_check_missing(iv%metar(n)%p%qc, y%metar(n)%p, f4)
               call da_check_missing(iv%metar(n)%q%qc, y%metar(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
               dum,dum
            end if
         end do
      end if
   end if

   ! [3] Transfer ships obs:

   if (iv % num_ships > 0) then
      num_obs = 0
      do n = 1, iv % num_ships
        if (iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ships', num_obs
         num_obs = 0
         do n = 1, iv % num_ships
            if (iv%ships(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ships(n)%u%qc, y%ships(n)%u, f1)
               call da_check_missing(iv%ships(n)%v%qc, y%ships(n)%v, f2)
               call da_check_missing(iv%ships(n)%t%qc, y%ships(n)%t, f3)
               call da_check_missing(iv%ships(n)%p%qc, y%ships(n)%p, f4)
               call da_check_missing(iv%ships(n)%q%qc, y%ships(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [4.1] Transfer Geo. AMVs Obs:

   if (iv % num_geoamv > 0) then 
      num_obs = 0
      do n = 1, iv % num_geoamv
         if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'geoamv', num_obs
         num_obs = 0
         do n = 1, iv % num_geoamv
            if (iv%geoamv(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % geoamv(n) % info % levels
               do k = 1, iv % geoamv(n) % info % levels
                  call da_check_missing(iv%geoamv(n)%u(k)%qc, &
                     y%geoamv(n)%u(k), f1)
                  call da_check_missing(iv%geoamv(n)%v(k)%qc, &
                     y%geoamv(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2 , dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [4.2] Transfer Polar AMVs Obs:

   if (iv % num_polaramv > 0) then
      num_obs = 0
      do n = 1, iv % num_polaramv
         if (iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'polaramv', num_obs
         num_obs = 0
         do n = 1, iv % num_polaramv
            if (iv%polaramv(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % polaramv(n) % info % levels
               do k = 1, iv % polaramv(n) % info % levels
                  call da_check_missing(iv%polaramv(n)%u(k)%qc, &
                     y%polaramv(n)%u(k), f1)
                  call da_check_missing(iv%polaramv(n)%v(k)%qc, &
                     y%polaramv(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2 , dum,dum,dum,&
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [5] Transfer gpspw obs:

   if (iv % num_gpspw > 0) then
      num_obs = 0
      do n = 1, iv % num_gpspw
         if (iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'gpspw', num_obs
         num_obs = 0
         do n = 1, iv % num_gpspw
            if (iv%gpspw(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%gpspw(n)%tpw%qc, y%gpspw(n)%tpw, f1)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, dum,dum,dum,dum, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [6] Transfer sonde obs:

   if (iv % num_sound > 0) then
      num_obs = 0
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sound', num_obs
         num_obs = 0
         do n = 1, iv % num_sound
            if (iv%sound(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % sound(n) % info % levels
               do k = 1, iv % sound(n) % info % levels
                  call da_check_missing(iv%sound(n)%u(k)%qc, y%sound(n)%u(k), f1)
                  call da_check_missing(iv%sound(n)%v(k)%qc, y%sound(n)%v(k), f2)
                  call da_check_missing(iv%sound(n)%t(k)%qc, y%sound(n)%t(k), f3)
                  call da_check_missing(iv%sound(n)%q(k)%qc, y%sound(n)%q(k), f4)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, f3, f4, dum, &
                     dum,dum
               end do
            end if
         end do
      end if

      num_obs = 0
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sonde_sfc', num_obs
         num_obs = 0
         do n = 1, iv % num_sound
            if (iv%sound(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%sonde_sfc(n)%u%qc, y%sonde_sfc(n)%u, f1)
               call da_check_missing(iv%sonde_sfc(n)%v%qc, y%sonde_sfc(n)%v, f2)
               call da_check_missing(iv%sonde_sfc(n)%t%qc, y%sonde_sfc(n)%t, f3)
               call da_check_missing(iv%sonde_sfc(n)%p%qc, y%sonde_sfc(n)%p, f4)
               call da_check_missing(iv%sonde_sfc(n)%q%qc, y%sonde_sfc(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if 
         end do
      end if
   end if

   ! [7] Transfer airep obs:

   if (iv % num_airep > 0) then
      num_obs = 0
      do n = 1, iv % num_airep
         if (iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'airep', num_obs
         num_obs = 0
         do n = 1, iv % num_airep
            if (iv%airep(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % airep(n) % info % levels
               do k = 1, iv % airep(n) % info % levels
                  call da_check_missing(iv%airep(n)%u(k)%qc, y%airep(n)%u(k), f1)
                  call da_check_missing(iv%airep(n)%v(k)%qc, y%airep(n)%v(k), f2)
                  call da_check_missing(iv%airep(n)%t(k)%qc, y%airep(n)%t(k), f3)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, f3, dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [8] Transfer pilot obs:

   if (iv % num_pilot > 0) then
      num_obs = 0
      do n = 1, iv % num_pilot
         if (iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'pilot', num_obs
         num_obs = 0
         do n = 1, iv % num_pilot
            if (iv%pilot(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % pilot(n) % info % levels
               do k = 1, iv % pilot(n) % info % levels
                  call da_check_missing(iv%pilot(n)%u(k)%qc, y%pilot(n)%u(k), f1)
                  call da_check_missing(iv%pilot(n)%v(k)%qc, y%pilot(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
     end if
   end if

   ! [9] Transfer SSM/I obs:SSMI:

   if (iv % num_ssmi_retrieval > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmi_retrieval
         if (iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmir', num_obs
         num_obs = 0
         do n = 1, iv % num_ssmi_retrieval
            if (iv%ssmi_retrieval(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ssmi_retrieval(n)%speed%qc, &
                                y % ssmi_retrieval(n) % speed, f1)
               call da_check_missing(iv%ssmi_retrieval(n)% tpw % qc, &
                                y % ssmi_retrieval(n) % tpw, f2)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, dum,dum,dum, &
                  dum,dum
            end if 
         end do
      end if
   end if

   if (iv % num_ssmi_tb > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmi_tb            
         if (iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmit', num_obs
         num_obs = 0
         do n = 1, iv % num_ssmi_tb
            if (iv%ssmi_tb(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ssmi_tb(n)%tb19h%qc, &
                                      y %ssmi_tb(n)%tb19h, f1)
               call da_check_missing(iv%ssmi_tb(n)%tb19v%qc, &
                                      y %ssmi_tb(n)%tb19v, f2)
               call da_check_missing(iv%ssmi_tb(n)%tb22v%qc, &
                                      y %ssmi_tb(n)%tb22v, f3)
               call da_check_missing(iv%ssmi_tb(n)%tb37h%qc, &
                                      y %ssmi_tb(n)%tb37h, f4)
               call da_check_missing(iv%ssmi_tb(n)%tb37v%qc, &
                                      y %ssmi_tb(n)%tb37v, f5)
               call da_check_missing(iv%ssmi_tb(n)%tb85h%qc, &
                                      y %ssmi_tb(n)%tb85h, f6)
               call da_check_missing(iv%ssmi_tb(n)%tb85v%qc, &
                                      y %ssmi_tb(n)%tb85v, f7)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, f6, f7
            end if
         end do
      end if
   end if

   ! [10] Transfer satem obs:

   if (iv % num_satem > 0) then
      num_obs = 0
      do n = 1, iv % num_satem            
         if (iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'satem', num_obs
         num_obs = 0
         do n = 1, iv % num_satem
            if (iv%satem(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % satem(n) % info % levels
               do k = 1, iv % satem(n) % info % levels
                  call da_check_missing(iv%satem(n)%thickness(k)%qc, &
                     y % satem(n) % thickness(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if
   
   ! [11] Transfer ssmt1 obs:

   if (iv % num_ssmt1 > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmt1            
         if (iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmt1', num_obs
         num_obs = 0
         do n = 1, iv % num_ssmt1
            if (iv%ssmt1(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % ssmt1(n) % info % levels
               do k = 1, iv % ssmt1(n) % info % levels
                  call da_check_missing(iv%ssmt1(n)%t(k)%qc, &
                     y % ssmt1(n) % t(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [12] Transfer ssmt2 obs:

   if (iv % num_ssmt2 > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmt2            
         if (iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmt2', num_obs
         num_obs = 0
         do n = 1, iv % num_ssmt2
            if (iv%ssmt2(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % ssmt2(n) % info % levels
               do k = 1, iv % ssmt2(n) % info % levels
                  call da_check_missing(iv%ssmt2(n)%rh(k)%qc, &
                  y % ssmt2(n) % rh(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [13] Transfer scatterometer obs:

   if (iv % num_qscat > 0) then
      num_obs = 0
      do n = 1, iv % num_qscat            
         if (iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'qscat', num_obs
         num_obs = 0
         do n = 1, iv % num_qscat
            if (iv%qscat(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%qscat(n)%u%qc, y%qscat(n)%u, f1)
               call da_check_missing(iv%qscat(n)%v%qc, y%qscat(n)%v, f2)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, dum,dum,dum, &
                  dum,dum
            end if
         end do
      end if
   end if
   
   ! [14] Transfer profiler obs:

   if (iv % num_profiler > 0) then
      num_obs = 0
      do n = 1, iv % num_profiler            
         if (iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'profiler', num_obs
         num_obs = 0
         do n = 1, iv % num_profiler
            if (iv%profiler(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % profiler(n) % info % levels
               do k = 1, iv % profiler(n) % info % levels
                  call da_check_missing(iv%profiler(n)%u(k)%qc, &
                     y%profiler(n)%u(k), f1)
                  call da_check_missing(iv%profiler(n)%v(k)%qc, &
                     y%profiler(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [15] Transfer buoy  obs:

   if (iv % num_buoy > 0) then
      num_obs = 0
      do n = 1, iv % num_buoy            
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'buoy', num_obs
         num_obs = 0
         do n = 1, iv % num_buoy
            if (iv%buoy(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%buoy(n)%u%qc, y%buoy(n)%u, f1)
               call da_check_missing(iv%buoy(n)%v%qc, y%buoy(n)%v, f2)
               call da_check_missing(iv%buoy(n)%t%qc, y%buoy(n)%t, f3)
               call da_check_missing(iv%buoy(n)%p%qc, y%buoy(n)%p, f4)
               call da_check_missing(iv%buoy(n)%q%qc, y%buoy(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [16] Transfer TC bogus obs:

   if (iv % num_bogus > 0) then
      num_obs = 0
      do n = 1, iv % num_bogus            
         if (iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'bogus', num_obs
         num_obs = 0
         do n = 1, iv % num_bogus
            if (iv%bogus(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, dum,dum,dum,dum,dum,dum
               write(ounit,'(i8)')iv % bogus(n) % info % levels
               do k = 1, iv % bogus(n) % info % levels
                  call da_check_missing(iv%bogus(n)%u(k)%qc, y%bogus(n)%u(k), f2)
                  call da_check_missing(iv%bogus(n)%v(k)%qc, y%bogus(n)%v(k), f3)
                  call da_check_missing(iv%bogus(n)%t(k)%qc, y%bogus(n)%t(k), f4)
                  call da_check_missing(iv%bogus(n)%q(k)%qc, y%bogus(n)%q(k), f5)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f2, f3, f4, f5, dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [17] Transfer AIRS retrievals:

   if (iv % num_airsr > 0) then
      num_obs = 0
      do n = 1, iv % num_airsr
         if (iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'airsr', num_obs
         num_obs = 0
         do n = 1, iv % num_airsr
            if (iv%airsr(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % airsr(n) % info % levels
               do k = 1, iv % airsr(n) % info % levels
                  call da_check_missing(iv%airsr(n)%t(k)%qc, y%airsr(n)%t(k), f1)
                  call da_check_missing(iv%airsr(n)%q(k)%qc, y%airsr(n)%q(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum, dum, &
                     dum, dum,dum
               end do
            end if
         end do
      end if
   end if
  
   ! [18] Transfer Radiance obs:

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         if (iv%instid(i)%num_rad < 1) cycle
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            ! Counting number of obs for channel k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%loc(n)%proc_domain .and. &
                  (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            if (num_obs < 1) cycle

            write(ob_name,'(a,a,i2.2)') trim(iv%instid(i)%rttovid_string),'-',k
            write(ounit,'(a20,i8)')  ob_name,num_obs

            num_obs = 0
            do n= 1, iv%instid(i)%num_rad      ! loop for pixel
              if(iv%instid(i)%loc(n)%proc_domain .and. &
                 (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                    num_obs = num_obs + 1
                    write(ounit,'(2i8,e15.7)')num_obs, 1, y%instid(i)%tb(k,n)
              end if
            end do                                ! end loop for pixel
         end do                                ! end loop for channel
      end do                                   ! end loop for sensor
   end if

   ! Transfer gpsref obs:

   if (iv % num_gpsref > 0) then
      num_obs = 0
      do n = 1, iv % num_gpsref
         if (iv%gpsref(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'gpsref', num_obs
         num_obs = 0
         do n = 1, iv % num_gpsref
            if (iv%gpsref(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % gpsref(n) % info % levels
               do k = 1, iv % gpsref(n) % info % levels
                  call da_check_missing(iv%gpsref(n)%ref(k)%qc, y%gpsref(n)%ref(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum, dum, dum, dum, dum,dum
               end do
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_y")

end subroutine da_write_y


