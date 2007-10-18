subroutine da_print_be_stats_h_regional(outunit, variable, nk, scale_length)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none 

   integer,      intent(inout) :: outunit            ! Output file unit.
   character*10, intent(in)    :: variable           ! Variable name.
   integer,      intent(in)    :: nk                 ! Dimension of vertical index
   real,         intent(in)    :: scale_length(1:nk) ! Correlation scale lengths

   integer :: k                  ! Loop counter.

   if (trace_use) call da_trace_entry("da_print_be_stats_h_regional")

   write(unit=stdout,fmt='(3a,i5)') &
      ' Scale length for variable ', trim(variable), ' in unit ', outunit

   do k = 1, nk
     write(unit=outunit,fmt='(i4,1pe15.5)')k, scale_length(k)
   end do

   outunit = outunit + 1
   write(unit=stdout,fmt=*) ' '

   if (trace_use) call da_trace_exit("da_print_be_stats_h_regional")

end subroutine da_print_be_stats_h_regional


tem)))
   if (iv%info(metar)%nlocal     > 0) allocate(iv%metar    (1:iv%info(metar)%nlocal))
   if (iv%nlocal(ships)     > 0) allocate(iv%ships    (1:iv%nlocal(ships)))
   if (iv%nlocal(pilot)     > 0) allocate(iv%pilot    (1:iv%nlocal(pilot)))
   if (iv%info(gpspw)%nlocal     > 0) allocate(iv%gpspw    (1:iv%info(gpspw)%nlocal))
   if (iv%info(gpsref)%nlocal    > 0) allocate(iv%gpsref   (1:iv%info(gpsref)%nlocal))
   if (iv%nlocal(ssmi_tb)   > 0) allocate(iv%ssmi_tb  (1:iv%nlocal(ssmi_tb)))
   if (iv%nlocal(ssmi_rv)   > 0) allocate(iv%ssmi_rv  (1:iv%nlocal(ssmi_rv)))
   if (iv%info(ssmt1)%nlocal     > 0) allocate(iv%ssmt1    (1:iv%info(ssmt1)%nlocal))
   if (iv%nlocal(ssmt2)     > 0) allocate(iv%ssmt2    (1:iv%nlocal(ssmt2)))
   if (iv%nlocal(qscat)     > 0) allocate(iv%qscat    (1:iv%nlocal(qscat)))
   if (iv%info(profiler)%nlocal  > 0) allocate(iv%profiler (1:iv%info(profiler)%nlocal))
   if (iv%nlocal(buoy)      > 0) allocate(iv%buoy     (1:iv%nlocal(buoy)))
   if (iv%info(radar)%nlocal     > 0) allocate(iv%radar    (1:iv%info(radar)%nlocal))
   if (iv%nlocal(bogus)     > 0) allocate(iv%bogus    (1:iv%nlocal(bogus)))
   if (iv%nlocal(airsr)     > 0) allocate(iv%airsr    (1:iv%nlocal(airsr)))

   do i=1,num_ob_indexes
      allocate (iv%info(i)%name(iv%nlocal(i)))     
      allocate (iv%info(i)%platform(iv%nlocal(i))) 
      allocate (iv%info(i)%id(iv%nlocal(i)))       
      allocate (iv%info(i)%date_char(iv%nlocal(i)))
      allocate (iv%info(i)%levels(iv%nlocal(i)))   
      allocate (iv%info(i)%lat(iv%info(i)%max_lev,iv%nlocal(i)))    
      allocate (iv%info(i)%lon(iv%info(i)%max_lev,iv%nlocal(i)))    
      allocate (iv%info(i)%elv(iv%nlocal(i)))      
      allocate (iv%info(i)%pstar(iv%nlocal(i)))    

      allocate (iv%info(i)%slp(iv%nlocal(i)))   
      allocate (iv%info(i)%pw(iv%nlocal(i)))    

      allocate (iv%info(i)%x  (kms:kme,iv%nlocal(i)))   
      allocate (iv%info(i)%y  (kms:kme,iv%nlocal(i)))   
      allocate (iv%info(i)%i  (kms:kme,iv%nlocal(i)))   
      allocate (iv%info(i)%j  (kms:kme,iv%nlocal(i)))      
      allocate (iv%info(i)%dx (kms:kme,iv%nlocal(i)))  
      allocate (iv%info(i)%dxm(kms:kme,iv%nlocal(i))) 
      allocate (iv%info(i)%dy (kms:kme,iv%nlocal(i)))  
      allocate (iv%info(i)%dym(kms:kme,iv%nlocal(i))) 
      allocate (iv%info(i)%k  (iv%info(i)%max_lev,iv%nlocal(i)))
      allocate (iv%info(i)%dz (iv%info(i)%max_lev,iv%nlocal(i)))  
      allocate (iv%info(i)%dzm(iv%info(i)%max_lev,iv%nlocal(i))) 
      allocate (iv%info(i)%zk (iv%info(i)%max_lev,iv%nlocal(i))) 
      iv%info(i)%zk(:,:) = missing_r
      allocate (iv%info(i)%proc_domain(iv%info(i)%max_lev,iv%nlocal(i))) 
      allocate (iv%info(i)%obs_global_index(iv%nlocal(i))) 
   end do

   if (trace_use) call da_trace_exit("da_allocate_observations")

end subroutine da_allocate_observations


