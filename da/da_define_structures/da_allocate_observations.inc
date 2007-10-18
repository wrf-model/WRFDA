subroutine da_allocate_observations (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv     ! Observation structure.

   integer :: i

   if (trace_use) call da_trace_entry("da_allocate_observations")

   if (iv%nlocal(sound)     > 0) allocate(iv%sound    (1:iv%nlocal(sound)))
   if (iv%nlocal(sonde_sfc) > 0) allocate(iv%sonde_sfc(1:iv%nlocal(sonde_sfc)))
   if (iv%nlocal(synop)     > 0) allocate(iv%synop    (1:iv%nlocal(synop)))
   if (iv%nlocal(airep)     > 0) allocate(iv%airep    (1:iv%nlocal(airep)))
   if (iv%nlocal(geoamv)    > 0) allocate(iv%geoamv   (1:iv%nlocal(geoamv)))
   if (iv%nlocal(polaramv)  > 0) allocate(iv%polaramv (1:iv%nlocal(polaramv)))
   if (iv%nlocal(satem)     > 0) allocate(iv%satem    (1:iv%nlocal(satem)))
   if (iv%nlocal(metar)     > 0) allocate(iv%metar    (1:iv%nlocal(metar)))
   if (iv%nlocal(ships)     > 0) allocate(iv%ships    (1:iv%nlocal(ships)))
   if (iv%nlocal(pilot)     > 0) allocate(iv%pilot    (1:iv%nlocal(pilot)))
   if (iv%nlocal(gpspw)     > 0) allocate(iv%gpspw    (1:iv%nlocal(gpspw)))
   if (iv%nlocal(gpsref)    > 0) allocate(iv%gpsref   (1:iv%nlocal(gpsref)))
   if (iv%nlocal(ssmi_tb)   > 0) allocate(iv%ssmi_tb  (1:iv%nlocal(ssmi_tb)))
   if (iv%nlocal(ssmi_rv)   > 0) allocate(iv%ssmi_rv  (1:iv%nlocal(ssmi_rv)))
   if (iv%nlocal(ssmt1)     > 0) allocate(iv%ssmt1    (1:iv%nlocal(ssmt1)))
   if (iv%nlocal(ssmt2)     > 0) allocate(iv%ssmt2    (1:iv%nlocal(ssmt2)))
   if (iv%nlocal(qscat)     > 0) allocate(iv%qscat    (1:iv%nlocal(qscat)))
   if (iv%nlocal(profiler)  > 0) allocate(iv%profiler (1:iv%nlocal(profiler)))
   if (iv%nlocal(buoy)      > 0) allocate(iv%buoy     (1:iv%nlocal(buoy)))
   if (iv%nlocal(radar)     > 0) allocate(iv%radar    (1:iv%nlocal(radar)))
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


