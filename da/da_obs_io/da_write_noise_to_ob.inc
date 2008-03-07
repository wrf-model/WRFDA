subroutine da_write_noise_to_ob (iv) 

   !-------------------------------------------------------------------------
   ! Purpose: Write consolidated obs noise created in da_add_noise_to_ob
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv   ! Obs and header structure.
   integer                       :: n, k,kk,i, iunit
   integer                       :: num_obs
   character(len=filename_len), allocatable     :: filename(:)   
   character*20                  :: ob_name   

   call da_trace_entry("da_write_noise_to_ob")

#ifdef DM_PARALLEL
   ! Ensure other processors have written their temporary files
   call mpi_barrier(comm, ierr)
#endif
   call da_get_unit (iunit)
   allocate (filename(0:num_procs-1))
   do k = 0,num_procs-1
      write(unit=filename(k), fmt='(a,i3.3)')'rand_obs_error.',k
   end do
   if (rootproc) then
      call da_get_unit (rand_unit)
      open(unit=rand_unit,file='rand_obs_error',form='formatted', &
         iostat=ierr,status='new')
      if (ierr /= 0) &
         call da_error(__FILE__,__LINE__, (/"Cannot open file rand_obs_error"/))
   end if

   num_obs = 0
   if (iv % num_synop > 0) then
      do n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'synop',5)
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'metar',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ships',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_geoamv > 0) then
      do n = 1, iv % num_geoamv
         if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'geoamv',6)
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
   if (num_obs > 0 .and. rootproc) then
      if (rootproc) write(rand_unit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'polaramv',8) 
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'gpspw',5)     
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'sound', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'sound',5)     
      end do
      !  writing Sonde_sfc  
      if (rootproc) write(rand_unit,'(a20,i8)')'sonde_sfc', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'sonde_sfc',9)
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'airep',5)   
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'pilot',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmir',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmiT',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'satem',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt1',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt2',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'qscat',5)    
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
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'profiler',8)    
      end do
   end if

   !------------------------------------------------------------------
   ! [16] writing Buoy 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_buoy > 0) then
      do n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'buoy',4)    
      end do
   end if

   !------------------------------------------------------------------
   ! [17] writing  Bogus 
   !------------------------------------------------------------------
  
   num_obs = 0
   if (iv % num_bogus > 0) then
      do n = 1, iv % num_bogus
         if (iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'bogus',5)    
      end do
   end if

   !------------------------------------------------------------------
   !  writing AIRS retrievals
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % num_airsr > 0) then
      do n = 1, iv % num_airsr
         if (iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
       write(rand_unit,'(a20,i8)')'airsr', num_obs  
       num_obs = 0
       do k = 0,num_procs-1
          call da_read_rand_unit(filename(k),iunit,num_obs,'airsr',5)    
       end do
    end if

   !------------------------------------------------------------------
   ! writing gpsref
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % num_gpsref > 0) then
      do n = 1, iv % num_gpsref
         if (iv%gpsref(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,20i8)')'gpsref', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'gpsref',6)    
      end do
   end if


   !------------------------------------------------------------------
   ! writing Radiance data:  
   !------------------------------------------------------------------

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         !if (iv%instid(i)%num_rad < 1) cycle ! may broken da_proc_sum_int(num_obs) if some PE num_rad=0
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            ! Counting number of obs for channle k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%proc_domain(n) .and. &
                  (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            call da_proc_sum_int(num_obs)
            if (rootproc .and. num_obs > 0) then
               write (ob_name,'(a,a,i4.4)') &
                  trim(iv%instid(i)%rttovid_string),'-',k
               write (rand_unit,'(a20,i8)')  ob_name,num_obs
               num_obs = 0
               do kk = 0,num_procs-1
                  call da_read_rand_unit(filename(kk),iunit,num_obs, &
                     trim(ob_name),len(trim(ob_name)))
               end do
            end if
         end do                           ! end loop for channel
      end do                            ! end loop for sensor
   end if
!whl
!write mtgirs
   num_obs = 0
   if (iv % num_mtgirs > 0) then
      do n = 1, iv % num_mtgirs
         if (iv%mtgirs(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'mtgirs', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'mtgirs',5)
      end do
   end if


   call da_free_unit (iunit)
!rizvi   call da_free_unit (rand_unit)
   if (rootproc ) call da_free_unit (rand_unit)
   deallocate (filename)
   call da_trace_exit("da_write_noise_to_ob")
   
end subroutine da_write_noise_to_ob  


