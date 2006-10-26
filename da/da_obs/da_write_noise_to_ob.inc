subroutine da_write_noise_to_ob (iv) 

   !-------------------------------------------------------------------------
   ! Purpose: Write consolidated obs noise created in da_add_noise_to_ob
   !-------------------------------------------------------------------------   

   IMPLICIT NONE

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   TYPE (ob_type), INTENT(IN)    :: iv   ! Obs and header structure.
   integer                       :: n, k,kk,inst, iunit
   integer                       :: ierr,ios  ! Error code from MPI routines.
   integer                       :: num_obs
   character*20, allocatable     :: filename(:)   
   character*20                  :: ob_name   
   !-------------------------------------------------------------------------   

   call da_trace_entry("da_write_noise_to_ob")

#ifdef DM_PARALLEL
     call MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif
   call da_get_unit (iunit)
   allocate (filename(0:num_procs-1))
    DO k = 0,num_procs-1
     write(unit=filename(k), fmt='(a,i3.3)')'rand_obs_error.',k
    ENDDO
   if( rootproc) then
   open(UNIT=rand_unit,file='rand_obs_error',form='formatted',iostat=ios,status='new')
   if (ios /= 0 )call da_error(__FILE__,__LINE__, (/"Canot open file rand_obs_error"/))
   endif
!
   num_obs = 0
   IF (iv % num_synop > 0) THEN
      DO n = 1, iv % num_synop
        if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'synop',5)
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [2] writing Metar
   !------------------------------------------------------------------

   num_obs = 0
   IF ( iv % num_metar > 0 ) THEN
      DO n = 1, iv % num_metar
         if (iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'metar',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   IF (iv % num_ships > 0) THEN
      DO n = 1, iv % num_ships
         if (iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'ships',5)
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   IF (iv % num_geoamv > 0 ) THEN
      DO n = 1, iv % num_geoamv
        if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'geoamv',6)
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [5] writing PolarAMV
   !------------------------------------------------------------------

   num_obs = 0
   IF (iv % num_polaramv > 0 ) THEN
      DO n = 1, iv % num_polaramv
        if (iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      if (rootproc) write(rand_unit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'polaramv',8) 
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [5] writing GPSPW  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_gpspw > 0 ) THEN
      DO n = 1, iv % num_gpspw
        if (iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
           call da_read_rand_unit(filename(k),iunit,num_obs,'gpspw',5)     
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [6] writing Sonde  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_sound > 0 ) THEN
      DO n = 1, iv % num_sound
        if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'sound', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'sound',5)     
      ENDDO
      !  writing Sonde_sfc  
      if (rootproc) write(rand_unit,'(a20,i8)')'sonde_sfc', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'sonde_sfc',9)
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [7] writing Airep  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_airep > 0 ) THEN
      DO n = 1, iv % num_airep
        if (iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'airep',5)   
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [8] writing Pilot  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF (iv % num_pilot > 0 ) THEN
      DO n = 1, iv % num_pilot
        if (iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'pilot',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [9] writing SSMI_RETRIEVAL
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_ssmi_retrieval > 0 ) THEN
      DO n = 1, iv % num_ssmi_retrieval
        if (iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'ssmir',5)    
     ENDDO
   endif

   !------------------------------------------------------------------
   ! [10] writing SSMITB
   !------------------------------------------------------------------

   num_obs = 0
   IF ( iv % num_ssmi_tb > 0 ) THEN
      DO n = 1, iv % num_ssmi_tb
        if (iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'ssmiT',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [11] writing SATEM  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_satem > 0 ) THEN
      DO n = 1, iv % num_satem
        if (iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'satem',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [12] writing SSMT1  
   !------------------------------------------------------------------

   num_obs = 0
   IF (iv % num_ssmt1 > 0 ) THEN
      DO n = 1, iv % num_ssmt1
        if (iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt1',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [13] writing SSMT2  
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_ssmt2 > 0 ) THEN
      DO n = 1, iv % num_ssmt2
         if (iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt2',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [14] writing QSCAT  
   !------------------------------------------------------------------
    
   num_obs = 0
   IF (iv % num_qscat > 0 ) THEN
      DO n = 1, iv % num_qscat
         if (iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'qscat',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [15] writing Profiler
   !------------------------------------------------------------------
   
   num_obs = 0
   IF (iv % num_profiler > 0) THEN
      DO n = 1, iv % num_profiler
        if (iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'profiler',8)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [16] writing Buoy 
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_buoy > 0 ) THEN
      DO n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'buoy',4)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [17] writing  Bogus 
   !------------------------------------------------------------------
  
   num_obs = 0
   IF ( iv % num_bogus > 0 ) THEN
      DO n = 1, iv % num_bogus
         if (iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
      write(rand_unit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      DO k = 0,num_procs-1
       call da_read_rand_unit(filename(k),iunit,num_obs,'bogus',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   !  writing AIRS retrievals
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_airsr > 0 ) THEN
      DO n = 1, iv % num_airsr
        if (iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc ) then
       write(rand_unit,'(a20,i8)')'airsr', num_obs  
       num_obs = 0
       DO k = 0,num_procs-1
        call da_read_rand_unit(filename(k),iunit,num_obs,'airsr',5)    
       ENDDO
    endif
!------------------------------------------------------------------
!  writing  Radiance data:
!------------------------------------------------------------------

   IF ( iv%num_inst > 0 ) THEN
     do inst = 1, iv%num_inst                 ! loop for sensor
       do k = 1,iv%instid(inst)%nchan        ! loop for channel
!  Counting number of obs for channle k
         num_obs = 0
         do n = 1,iv%instid(inst)%num_rad      ! loop for pixel
           if(iv%instid(inst)%rad(n)%loc%proc_domain .and. &
              (iv%instid(inst)%rad(n)%tb(k)%qc >= obs_qc_pointer)) then
                num_obs = num_obs + 1
           end if
         end do                                ! end loop for pixel
       call da_proc_sum_int(num_obs)
       if(rootproc .and. num_obs > 0) then
         write(ob_name,'(a,a,i2.2)') trim(iv%instid(inst)%rttovid_string),'-',k
         write(rand_unit,'(a20,i8)')  ob_name,num_obs
         num_obs = 0
         DO kk = 0,num_procs-1
         call da_read_rand_unit(filename(kk),&
              iunit,num_obs,trim(ob_name),len(trim(ob_name)))
         ENDDO
       endif
       enddo                           ! end loop for channel
     end do                            ! end loop for sensor
   END IF


   call da_free_unit (iunit)
   deallocate (filename )
   call da_trace_exit("da_write_noise_to_ob")
   
end subroutine da_write_noise_to_ob  
