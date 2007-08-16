subroutine da_final_write_obs(iv)

   !-------------------------------------------------------------------------
   ! Purpose: Writes full diagnostics for O, (O-B) & OMA togather
   !-------------------------------------------------------------------------   

   implicit none

   type (ob_type), intent(in)    :: iv      ! O-B structure.
 
   integer                       :: n, k, iunit
   integer                       :: ios  ! Error code from MPI routines.
   integer                       :: num_obs, num_sound_obs
   character(len=filename_len), allocatable     :: filename(:)

   if (trace_use) call da_trace_entry("da_final_write_obs")

#ifdef DM_PARALLEL
   ! Wait to ensure all temporary files have been written
   call mpi_barrier(comm, ierr)
#endif

   if (rootproc) then
      call da_get_unit(iunit)
      allocate (filename(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename(k),fmt ='(a,i3.3)')'gts_omb_oma.',k
      end do 
      call da_get_unit(omb_unit)
      open(unit=omb_unit,file='gts_omb_oma',form='formatted', &
         status='replace', iostat=ios) 
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file gts_omb_oma"/))
   end if

   num_obs = 0
   if (iv % nlocal(synop) > 0) then
      do n = 1, iv % nlocal(synop)
         if(iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'synop',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [2] writing Metar
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(metar) > 0) then
      do n = 1, iv % nlocal(metar)
         if (iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'metar',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(ships) > 0) then
      do n = 1, iv % nlocal(ships)
         if(iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ships',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(geoamv) > 0) then
       do n = 1, iv % nlocal(geoamv)
          if(iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'geoamv',6)
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing PolarAMV
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(polaramv) > 0) then
      do n = 1, iv % nlocal(polaramv)
         if (iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'polaramv',8) 
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing GPSPW  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(gpspw) > 0) then
      do n = 1, iv % nlocal(gpspw)
         if(iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'gpspw',5)     
      end do
   end if

   !------------------------------------------------------------------
   ! [6] writing Sonde  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(sound) > 0) then
      do n = 1, iv % nlocal(sound)
         if(iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'sound', num_obs  
      num_sound_obs = num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'sound',5)     
      end do

      ! writing Sonde_sfc  
      write(omb_unit,'(a20,i8)')'sonde_sfc', num_sound_obs  
      num_obs = 0 
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'sonde_sfc',9)
      end do
   end if

   !------------------------------------------------------------------
   ! [7] writing Airep  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(airep) > 0) then
      do n = 1, iv % nlocal(airep)
         if(iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'airep',5)   
      end do
   end if

   !------------------------------------------------------------------
   ! [8] writing Pilot  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % nlocal(pilot) > 0) then
      do n = 1, iv % nlocal(pilot)
         if(iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'pilot',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [9] writing ssmi_rv
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(ssmi_rv) > 0) then
      do n = 1, iv % nlocal(ssmi_rv)
         if(iv%ssmi_rv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmir',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [10] writing SSMITB
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % nlocal(ssmi_tb) > 0) then
      do n = 1, iv % nlocal(ssmi_tb)
         if (iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmiT',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [11] writing SATEM  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(satem) > 0) then
      do n = 1, iv % nlocal(satem)
         if(iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'satem',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [12] writing SSMT1  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(ssmt1) > 0) then
      do n = 1, iv % nlocal(ssmt1)
         if(iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmt1',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [13] writing SSMT2  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(ssmt2) > 0) then
      do n = 1, iv % nlocal(ssmt2)
         if(iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmt2',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [14] writing QSCAT  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % nlocal(qscat) > 0) then
      do n = 1, iv % nlocal(qscat)
         if(iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'qscat',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [15] writing Profiler
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(profiler) > 0) then
      do n = 1, iv % nlocal(profiler)
         if(iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'profiler',8)    
      end do
   end if

   !------------------------------------------------------------------
   ! [16] writing Buoy 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % nlocal(buoy) > 0) then
      do n = 1, iv % nlocal(buoy)
         if(iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'buoy',4)    
      end do
   end if

   !------------------------------------------------------------------
   ! [17] writing Bogus 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv % nlocal(bogus) > 0) then
      do n = 1, iv % nlocal(bogus)
         if(iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'bogus',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! writing AIRS retrievals:
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(airsr) > 0) then
      do n = 1, iv % nlocal(airsr)
         if(iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'airsr', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'airsr',5)
      end do
   end if

   !------------------------------------------------------------------
   ! writing GPS refractivity
   !------------------------------------------------------------------

   num_obs = 0
   if (iv % nlocal(gpsref) > 0) then
      do n = 1, iv % nlocal(gpsref)
         if(iv%gpsref(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'gpsref', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'gpsref',6)    
      end do
   end if

   if (rootproc) then
      close(iunit)
      close(omb_unit)
      call da_free_unit(iunit)
      call da_free_unit(omb_unit)
      deallocate (filename)
   end if

   if (trace_use) call da_trace_exit("da_final_write_obs")
   
end subroutine da_final_write_obs


