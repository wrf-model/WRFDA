subroutine da_final_write_y(iv, y)

   !-------------------------------------------------------------------------   
   ! Purpose: Writes full diagnostics for y=H(x_inc)    
   !-------------------------------------------------------------------------   

   implicit none

   type (ob_type), intent(in)    :: iv   ! O-B structure.
   type (y_type), intent(in)     :: y    ! y = H(x_inc) structure.
      
   integer                       :: n, k,kk,i, ounit, iunit
   integer                       :: sound_num_obs, num_obs, ios
   character*20, allocatable     :: filename(:)
   character*20                  :: ob_name, file_prefix  

   if (trace_use) call da_trace_entry("da_final_write_y")

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

   if (omb_add_noise) then
      ! perturbed ob run.
      file_prefix='pert_obs'
   else
      ! unperturbed ob run.
      file_prefix='unpert_obs'
   end if

   if (rootproc) then
      allocate (filename(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename(k),fmt ='(a,a,i3.3)')trim(file_prefix),'.',k
      end do
      call da_get_unit(ounit)
      open(unit=ounit,file=trim(file_prefix),form='formatted',iostat=ios, &
         status='new')
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//trim(file_prefix)/))
   end if

   !------------------------------------------------------------------
   ! [1] writing Surface
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % num_synop > 0) then
      do n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'synop',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [2] writing Metar
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_metar > 0) then
      do n = 1, iv % num_metar
        if (iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'metar',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % num_ships > 0) then
      do n = 1, iv % num_ships
        if (iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ships',5)
      end do
   end if

   !---------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_geoamv > 0) then
      do n = 1, iv % num_geoamv
         if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'geoamv',6)
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing PolarAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_polaramv > 0) then
      do n = 1, iv % num_polaramv
        if (iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'polaramv',8) 
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing GPSPW  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_gpspw > 0) then
      do n = 1, iv % num_gpspw
         if (iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'gpspw',5)     
      end do
   end if

   !------------------------------------------------------------------
   ! [6] writing Sonde  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_sound > 0) then
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   sound_num_obs = num_obs
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'sound', sound_num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'sound',5)     
      end do  
      !  writing Sonde_sfc  
      write(ounit,'(a20,i8)')'sonde_sfc', sound_num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'sonde_sfc',9)
      end do
   end if

   !------------------------------------------------------------------
   ! [7] writing Airep  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_airep > 0) then
      do n = 1, iv % num_airep
         if (iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'airep',5)   
      end do
   end if

   !------------------------------------------------------------------
   ! [8] writing Pilot  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_pilot > 0) then
      do n = 1, iv % num_pilot
        if (iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'pilot',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [9] writing SSMI_RETRIEVAL
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_ssmi_retrieval > 0) then
      do n = 1, iv % num_ssmi_retrieval
         if (iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmir',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [10] writing SSMITB
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_ssmi_tb > 0) then
      do n = 1, iv % num_ssmi_tb
         if (iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmiT',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [11] writing SATEM  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_satem > 0) then
      do n = 1, iv % num_satem
         if (iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'satem',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [12] writing SSMT1  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_ssmt1 > 0) then
      do n = 1, iv % num_ssmt1
        if (iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmt1',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [13] writing SSMT2  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_ssmt2 > 0) then
      do n = 1, iv % num_ssmt2
         if (iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmt2',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [14] writing QSCAT  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_qscat > 0) then
      do n = 1, iv % num_qscat
         if (iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'qscat',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [15] writing Profiler
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_profiler > 0) then
      do n = 1, iv % num_profiler
         if (iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'profiler',8)    
      end do
   end if

   !---------------------------------------------------------------
   ! [16] writing Buoy 
   !---------------------------------------------------------------

   num_obs = 0
   if (iv % num_buoy > 0) then
      do n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'buoy',4)    
      end do
   end if

   !---------------------------------------------------------------
   ! [17] writing  Bogus  
   !---------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_bogus > 0) then
      do n = 1, iv % num_bogus
         if (iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'bogus',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! writing AIRS retrievals:
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_airsr > 0) then
      do n = 1, iv % num_airsr
         if (iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'airsr', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'airsr',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! writing Radiance data:
   !------------------------------------------------------------------

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            !  Counting number of obs for channel k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%loc(n)%proc_domain .and. &
                 (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            call da_proc_sum_int(num_obs)
            if (rootproc .and. num_obs > 0) then
               write(ob_name,'(a,a,i2.2)') &
                  trim(iv%instid(i)%rttovid_string),'-',k
               write(ounit,'(a20,i8)')  ob_name,num_obs
               num_obs = 0
               do kk = 0,num_procs-1
                  call da_read_y_unit(trim(filename(kk)),ounit,num_obs, &
                     trim(ob_name),len(trim(ob_name)))    
               end do
            end if
         end do                                  ! end loop for Channel 
      end do                                   ! end loop for sensor
   end if

   if (rootproc) then
      close(ounit)
      call da_free_unit(ounit)
      deallocate (filename)
   end if

   if (trace_use) call da_trace_exit("da_final_write_y")
   
end subroutine da_final_write_y


