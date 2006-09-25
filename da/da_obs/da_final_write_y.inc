subroutine da_final_write_y( iv, y )

   !-------------------------------------------------------------------------   
   ! Purpose: Writes full diagnostics for y=H(x_inc)      
   ! History:
   ! Author:    Syed RH Rizvi     MMM/NCAR    05/16/2006     
   !-------------------------------------------------------------------------   
   IMPLICIT NONE

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   TYPE (ob_type), INTENT(IN)    :: iv   ! O-B structure.
   TYPE (y_type), INTENT(IN)     :: y    ! y = H(x_inc) structure.
      
   integer                       :: n, k, ounit, iunit
   integer                       :: root      ! Monitor processor.
   integer                       :: ierr      ! Error code from MPI routines.
   integer                       :: sound_num_obs, num_obs


   if (trace_use) call da_trace_entry("da_final_write_y")

#ifdef DM_PARALLEL
   call MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

   if ( omb_add_noise ) then
      ounit = yp_unit            ! 3DVAR is a perturbed ob run.
   else
      ounit = y_unit             ! 3DVAR is an unperturbed ob run.
   end if

   !------------------------------------------------------------------
   ! [1] writing Surface
   !------------------------------------------------------------------

   num_obs = 0
   IF (iv % num_synop > 0) THEN
      DO n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      DO k = 0,numb_procs-1
         iunit = ounit*10 +  k
         rewind iunit
         call da_read_y_unit(iunit,ounit,num_obs,'synop',5)
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      DO k = 0,numb_procs-1
         iunit = ounit*10 +  k
         rewind iunit
         call da_read_y_unit(iunit,ounit,num_obs,'metar',5)    
      ENDDO
   endif

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   IF (iv % num_ships > 0 ) THEN
      DO n = 1, iv % num_ships
        if (iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      DO k = 0,numb_procs-1
         iunit = ounit*10 +  k
         rewind iunit
         call da_read_y_unit(iunit,ounit,num_obs,'ships',5)
      ENDDO
   endif

   !---------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   IF ( iv % num_geoamv > 0 ) THEN
      DO n = 1, iv % num_geoamv
         if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'geoamv', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'geoamv',6)
    ENDDO
    endif
!------------------------------------------------------------------
! [5] writing PolarAMV
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_polaramv > 0 ) THEN
      DO n = 1, iv % num_polaramv
       if(iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'polaramv', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'polaramv',8) 
    ENDDO
    endif

!------------------------------------------------------------------
! [5] writing GPSPW  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_gpspw > 0 ) THEN
      DO n = 1, iv % num_gpspw
       if(iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'gpspw', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'gpspw',5)     
    ENDDO
    endif
!------------------------------------------------------------------
! [6] writing Sonde  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_sound > 0 ) THEN
      DO n = 1, iv % num_sound
       if(iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    sound_num_obs = num_obs
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'sound', sound_num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'sound',5)     
    ENDDO
!  writing Sonde_sfc  
    write(ounit,'(a20,i8)')'sonde_sfc', sound_num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'sonde_sfc',9)
    ENDDO
    endif

!------------------------------------------------------------------
! [7] writing Airep  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_airep > 0 ) THEN
      DO n = 1, iv % num_airep
       if(iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'airep', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'airep',5)   
    ENDDO
    endif
!------------------------------------------------------------------
! [8] writing Pilot  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_pilot > 0 ) THEN
      DO n = 1, iv % num_pilot
       if(iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'pilot', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'pilot',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [9] writing SSMI_RETRIEVAL
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_ssmi_retrieval > 0 ) THEN
      DO n = 1, iv % num_ssmi_retrieval
       if(iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'ssmir', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'ssmir',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [10] writing SSMITB
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_ssmi_tb > 0 ) THEN
      DO n = 1, iv % num_ssmi_tb
       if(iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'ssmiT', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'ssmiT',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [11] writing SATEM  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_satem > 0 ) THEN
      DO n = 1, iv % num_satem
       if(iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'satem', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'satem',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [12] writing SSMT1  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_ssmt1 > 0 ) THEN
      DO n = 1, iv % num_ssmt1
       if(iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'ssmt1', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'ssmt1',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [13] writing SSMT2  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_ssmt2 > 0 ) THEN
      DO n = 1, iv % num_ssmt2
       if(iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'ssmt2', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'ssmt2',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [14] writing QSCAT  
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_qscat > 0 ) THEN
      DO n = 1, iv % num_qscat
       if(iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'qscat', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'qscat',5)    
    ENDDO
    endif
!------------------------------------------------------------------
! [15] writing Profiler
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_profiler > 0 ) THEN
      DO n = 1, iv % num_profiler
       if(iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'profiler', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'profiler',8)    
    ENDDO
    endif

   !---------------------------------------------------------------
   ! [16] writing Buoy 
   !---------------------------------------------------------------

   num_obs = 0
   IF ( iv % num_buoy > 0 ) THEN
      DO n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
   ENDIF
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      DO k = 0,numb_procs-1
         iunit = ounit*10 +  k
         rewind iunit
         call da_read_y_unit(iunit,ounit,num_obs,'buoy',4)    
      ENDDO
   endif

   !---------------------------------------------------------------
   ! [17] writing  Bogus  
   !---------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_bogus > 0 ) THEN
      DO n = 1, iv % num_bogus
       if(iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'bogus', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'bogus',5)    
    ENDDO
    endif
!------------------------------------------------------------------
!  writing  AIRS retrievals:
!------------------------------------------------------------------
      num_obs = 0
    IF ( iv % num_airsr > 0 ) THEN
      DO n = 1, iv % num_airsr
       if(iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
    ENDIF
    call da_proc_sum_int(num_obs)
    if(rootproc .and. num_obs > 0) then
    write(ounit,'(a20,i8)')'airsr', num_obs  
    num_obs = 0
    DO k = 0,numb_procs-1
    iunit = ounit*10 +  k
    rewind iunit
    call da_read_y_unit(iunit,ounit,num_obs,'airsr',5)    
    ENDDO
    endif

   if ( rootproc ) write(ounit,'(a20,i8)')'********************', 0

   if (trace_use) call da_trace_exit("da_final_write_y")
   
end subroutine da_final_write_y
