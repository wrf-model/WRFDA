subroutine da_write_obs(ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (y_type), intent(in)     :: ob      ! Observation structure.
   type (ob_type), intent(in)    :: iv      ! O-B structure.
   type (y_type), intent(inout)  :: re      ! residual vector.
      
   integer                       :: n, k, num_obs, ios
   integer                       :: ounit     ! Output unit           
   character(len=filename_len)   :: filename

   if (trace_use) call da_trace_entry("da_write_obs")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i3.3)') 'gts_omb_oma.', myproc
#else
    write(unit=filename, fmt='(a)') 'gts_omb_oma.000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open conventional observation omb and oma file"//filename/))
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
               write(ounit,'(i8)') 1                                 
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , 1, iv % synop(n) % info % id, &  ! Station
                  iv % synop(n) % info % lat, &       ! Latitude
                  iv % synop(n) % info % lon, &       ! Longitude
                  ob % synop(n) % p, &                ! Obs Pressure
                  ob%synop(n)%u, iv%synop(n)%u, re%synop(n)%u, &! O, O-B, O-A u
                  ob%synop(n)%v, iv%synop(n)%v, re%synop(n)%v, &! O, O-B, O-A v
                  ob%synop(n)%t, iv%synop(n)%t, re%synop(n)%t, &! O, O-B, O-A t
                  ob%synop(n)%p, iv%synop(n)%p, re%synop(n)%p, &! O, O-B, O-A p
                  ob%synop(n)%q, iv%synop(n)%q, re%synop(n)%q   ! O, O-B, O-A q
            end if
         end do
      end if
   end if

   ! [2] Transfer metar obs:

   if (iv % num_metar > 0) then
      num_obs = 0
      do n = 1, iv % num_metar
         if (iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
      write(ounit,'(a20,i8)')'metar', num_obs  
      num_obs = 0
      do n = 1, iv % num_metar  
       if (iv%metar(n)%loc%proc_domain) then
       num_obs = num_obs + 1
       write(ounit,'(i8)') 1                                 
         write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                 num_obs  , 1, iv % metar(n) % info % id, &  ! Station
                         iv % metar(n) % info % lat, &       ! Latitude
                         iv % metar(n) % info % lon, &       ! Longitude
                         ob % metar(n) % p, &                ! Obs Pressure
                         ob%metar(n)%u, iv%metar(n)%u, re%metar(n)%u, &! O, O-B, O-A u
                         ob%metar(n)%v, iv%metar(n)%v, re%metar(n)%v, &! O, O-B, O-A v
                         ob%metar(n)%t, iv%metar(n)%t, re%metar(n)%t, &! O, O-B, O-A t
                         ob%metar(n)%p, iv%metar(n)%p, re%metar(n)%p, &! O, O-B, O-A p
                         ob%metar(n)%q, iv%metar(n)%q, re%metar(n)%q   ! O, O-B, O-A q
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
       write(ounit,'(i8)') 1                                 
       num_obs = num_obs + 1
         write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                    num_obs,1, iv % ships(n) % info % id, &  ! Station
                         iv % ships(n) % info % lat, &       ! Latitude
                         iv % ships(n) % info % lon, &       ! Longitude
                         ob % ships(n) % p, &                ! Obs Pressure
                         ob%ships(n)%u, iv%ships(n)%u, re%ships(n)%u, &! O, O-B, O-A u
                         ob%ships(n)%v, iv%ships(n)%v, re%ships(n)%v, &! O, O-B, O-A v
                         ob%ships(n)%t, iv%ships(n)%t, re%ships(n)%t, &! O, O-B, O-A t
                         ob%ships(n)%p, iv%ships(n)%p, re%ships(n)%p, &! O, O-B, O-A p
                         ob%ships(n)%q, iv%ships(n)%q, re%ships(n)%q   ! O, O-B, O-A q
      end if
      end do
     end if
   end if

   ! [4.1] Transfer Geo AMVs Obs:

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
                   write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                      num_obs, 1, iv % geoamv(n) % info % id, &  ! Station
                      iv % geoamv(n) % info % lat, &       ! Latitude
                      iv % geoamv(n) % info % lon, &       ! Longitude
                      iv % geoamv(n) % p(k), &                ! Obs Pressure
                      ob%geoamv(n)%u(k), iv%geoamv(n)%u(k), re%geoamv(n)%u(k), &! O, O-B, O-A u
                      ob%geoamv(n)%v(k), iv%geoamv(n)%v(k), re%geoamv(n)%v(k)
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
                   write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                      num_obs, 1, iv % polaramv(n) % info % id, &  ! Station
                      iv % polaramv(n) % info % lat, &       ! Latitude
                      iv % polaramv(n) % info % lon, &       ! Longitude
                      iv % polaramv(n) % p(k), &                ! Obs Pressure
                      ob%polaramv(n)%u(k), iv%polaramv(n)%u(k), re%polaramv(n)%u(k), &! O, O-B, O-A u
                      ob%polaramv(n)%v(k), iv%polaramv(n)%v(k), re%polaramv(n)%v(k)
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
               write(ounit,'(i8)') 1                                 
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, 1, iv % gpspw(n) % info % id, &  ! Station
                  iv % gpspw(n) % info % lat, &       ! Latitude
                  iv % gpspw(n) % info % lon, &       ! Longitude
                  missing_r, &
                  ob%gpspw(n)%tpw, iv%gpspw(n)%tpw, re%gpspw(n)%tpw ! O, O-B, O-A u
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs,k, iv % sound(n) % info % id, &  ! Station
                     iv % sound(n) % info % lat, &       ! Latitude
                     iv % sound(n) % info % lon, &       ! Longitude
                     iv % sound(n) % p(k), &             ! Obs Pressure
                     ob%sound(n)%u(k), iv%sound(n)%u(k), re%sound(n)%u(k), &! O,O-B,O-A u
                     ob%sound(n)%v(k), iv%sound(n)%v(k), re%sound(n)%v(k), &! O,O-B,O-A v
                     ob%sound(n)%t(k), iv%sound(n)%t(k), re%sound(n)%t(k), &! O,O-B,O-A t
                     ob%sound(n)%q(k), iv%sound(n)%q(k), re%sound(n)%q(k)   ! O,O-B,O-A q
               end do
            end if
         end do
      end if
   end if

   if (iv % num_sound > 0) then
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sonde_sfc', num_obs    
         num_obs = 0
         do n = 1, iv % num_sound
            if (iv%sound(n)%loc%proc_domain) then 
               num_obs = num_obs + 1
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , 1, iv % sonde_sfc(n) % info % id, &  ! Station
                  iv % sonde_sfc(n) % info % lat, &       ! Latitude
                  iv % sonde_sfc(n) % info % lon, &       ! Longitude
                  ob % sonde_sfc(n) % p, &                ! Obs Pressure
                  ob%sonde_sfc(n)%u, iv%sonde_sfc(n)%u, re%sonde_sfc(n)%u, &! O, O-B, O-A u
                  ob%sonde_sfc(n)%v, iv%sonde_sfc(n)%v, re%sonde_sfc(n)%v, &! O, O-B, O-A v
                  ob%sonde_sfc(n)%t, iv%sonde_sfc(n)%t, re%sonde_sfc(n)%t, &! O, O-B, O-A t
                  ob%sonde_sfc(n)%p, iv%sonde_sfc(n)%p, re%sonde_sfc(n)%p, &! O, O-B, O-A p
                  ob%sonde_sfc(n)%q, iv%sonde_sfc(n)%q, re%sonde_sfc(n)%q   ! O, O-B, O-A q
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs, k, iv % airep(n) % info % id, &  ! Station
                     iv % airep(n) % info % lat, &       ! Latitude
                     iv % airep(n) % info % lon, &       ! Longitude
                     iv % airep(n) % p(k), &             ! Obs pressure
                     ob%airep(n)%u(k), iv%airep(n)%u(k), re%airep(n)%u(k), &! O,O-B,O-A u
                     ob%airep(n)%v(k), iv%airep(n)%v(k), re%airep(n)%v(k), &! O,O-B,O-A v
                     ob%airep(n)%t(k), iv%airep(n)%t(k), re%airep(n)%t(k)
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs, k, iv % pilot(n) % info % id, &  ! Station
                     iv % pilot(n) % info % lat, &       ! Latitude
                     iv % pilot(n) % info % lon, &       ! Longitude
                     iv % pilot(n) % p(k), &             ! Obs Pressure
                     ob%pilot(n)%u(k), iv%pilot(n)%u(k), re%pilot(n)%u(k), &! O,O-B,O-A u
                     ob%pilot(n)%v(k), iv%pilot(n)%v(k), re%pilot(n)%v(k) ! O,O-B,O-A v
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
         write(ounit,'(a20,i8)')'ssmir',  num_obs
         num_obs = 0
         do n = 1, iv % num_ssmi_retrieval
            if (iv%ssmi_retrieval(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, 1, 'SSMIR',              &       ! Station
                  iv % ssmi_retrieval(n) % info % lat, &! Latitude
                  iv % ssmi_retrieval(n) % info % lon, &! Longitude
                  missing_r,                  &       ! Obs height
                  ob%ssmi_retrieval(n)%speed, iv%ssmi_retrieval(n)%speed, re%ssmi_retrieval(n)%speed, & ! O, O-B, O-A speed
                  ob%ssmi_retrieval(n)%tpw, iv%ssmi_retrieval(n)%tpw, re%ssmi_retrieval(n)%tpw ! O, O-B, O-A tpw
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
         write(ounit,'(a20,i8)')'ssmiT', num_obs     
         num_obs = 0
         do n = 1, iv % num_ssmi_tb
            write(ounit,*)' SSMI radiance output not yet coded.'
            if (iv%ssmi_tb(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))')&
                  num_obs, 1, 'SSMIT',              &        ! Station
                  iv % ssmi_tb(n) % info % lat, &! Latitude
                  iv % ssmi_tb(n) % info % lon, &! Longitude
                  missing_r,                  &       ! Obs height
                  ob%ssmi_tb(n)%tb19h, iv%ssmi_tb(n)%tb19h, re%ssmi_tb(n)%tb19h, & ! O, O-B, O-A Tb19h
                  ob%ssmi_tb(n)%tb19v, iv%ssmi_tb(n)%tb19v, re%ssmi_tb(n)%tb19v, & ! O, O-B, O-A Tb19v
                  ob%ssmi_tb(n)%tb22v, iv%ssmi_tb(n)%tb22v, re%ssmi_tb(n)%tb22v, & ! O, O-B, O-A Tb22v
                  ob%ssmi_tb(n)%tb37h, iv%ssmi_tb(n)%tb37h, re%ssmi_tb(n)%tb37h, & ! O, O-B, O-A Tb37h
                  ob%ssmi_tb(n)%tb37v, iv%ssmi_tb(n)%tb37v, re%ssmi_tb(n)%tb37v, & ! O, O-B, O-A Tb37v
                  ob%ssmi_tb(n)%tb85h, iv%ssmi_tb(n)%tb85h, re%ssmi_tb(n)%tb85h, & ! O, O-B, O-A Tb85h
                  ob%ssmi_tb(n)%tb85v, iv%ssmi_tb(n)%tb85v, re%ssmi_tb(n)%tb85v    ! O, O-B, O-A Tb85v
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs   , k, iv % satem(n) % info % id, &  ! Station
                     iv % satem(n) % info % lat, &       ! Latitude
                     iv % satem(n) % info % lon, &       ! Longitude
                     iv % satem(n) % p(k), &             ! Obs Pressure
                     ob%satem(n)%thickness(k), &
                     iv%satem(n)%thickness(k), &
                     re%satem(n)%thickness(k)            ! O,O-B,O-A
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs , k, iv % ssmt1(n) % info % id, &  ! Station
                     iv % ssmt1(n) % info % lat, &       ! Latitude
                     iv % ssmt1(n) % info % lon, &       ! Longitude
                     iv % ssmt1(n) % h(k), &             ! Obs height
                     ob%ssmt1(n)%t(k), &
                     iv%ssmt1(n)%t(k), &
                     re%ssmt1(n)%t(k)                    ! O,O-B,O-A u
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs , k, iv % ssmt2(n) % info % id, &  ! Station
                     iv % ssmt2(n) % info % lat, &       ! Latitude
                     iv % ssmt2(n) % info % lon, &       ! Longitude
                     iv % ssmt2(n) % h(k), &             ! Obs height
                     ob%ssmt2(n)%rh(k), &
                     iv%ssmt2(n)%rh(k), &
                     re%ssmt2(n)%rh(k)                   ! O,O-B,O-A u
               end do
            end if
         end do
      end if
   end if
   
   ! [3] Transfer scatterometer obs:

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
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                   num_obs, 1, iv % qscat(n) % info % id, &  ! Station
                   iv % qscat(n) % info % lat, &       ! Latitude
                   iv % qscat(n) % info % lon, &       ! Longitude
                   iv % qscat(n) % h, &                ! Obs height
                   ob%qscat(n)%u, iv%qscat(n)%u, re%qscat(n)%u, &! O, O-B, O-A u
                   ob%qscat(n)%v, iv%qscat(n)%v, re%qscat(n)%v   ! O, O-B, O-A v
            end if
         end do
      end if
   end if

   ! Transfer profiler obs:

   if (iv % num_profiler > 0) then
      num_obs = 0
      do n = 1, iv % num_profiler
         if (iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'profiler',  num_obs
         num_obs = 0
         do n = 1, iv % num_profiler
            if (iv%profiler(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv % profiler(n) % info % levels
               do k = 1, iv % profiler(n) % info % levels
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs, k, iv % profiler(n) % info % id, &  ! Station
                     iv % profiler(n) % info % lat, &       ! Latitude
                     iv % profiler(n) % info % lon, &       ! Longitude
                     iv % profiler(n) % p(k), &             ! Obs Pressure
                     ob%profiler(n)%u(k), iv%profiler(n)%u(k), re%profiler(n)%u(k), &! O,O-B,O-A u
                     ob%profiler(n)%v(k), iv%profiler(n)%v(k), re%profiler(n)%v(k) ! O,O-B,O-A v
               end do
            end if 
         end do
      end if
   end if

   ! Transfer Buoy obs:

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
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,1, iv % buoy(n) % info % id, &  ! Station
                  iv % buoy(n) % info % lat, &       ! Latitude
                  iv % buoy(n) % info % lon, &       ! Longitude
                  ob % buoy(n) % p, &                ! Obs Pressure
                  ob%buoy(n)%u, iv%buoy(n)%u, re%buoy(n)%u, &! O, O-B, O-A u
                  ob%buoy(n)%v, iv%buoy(n)%v, re%buoy(n)%v, &! O, O-B, O-A v
                  ob%buoy(n)%t, iv%buoy(n)%t, re%buoy(n)%t, &! O, O-B, O-A t
                  ob%buoy(n)%p, iv%buoy(n)%p, re%buoy(n)%p, &! O, O-B, O-A p
                  ob%buoy(n)%q, iv%buoy(n)%q, re%buoy(n)%q   ! O, O-B, O-A q
            end if
         end do
      end if
   end if

   ! Transfer TC bogus obs:

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
               write(ounit,'(i8)') 1
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                   num_obs, 1, iv % bogus(n) % info % id, &  ! Station
                   iv % bogus(n) % info % lat, &       ! Latitude
                   iv % bogus(n) % info % lon, &       ! Longitude
                   missing_r,                  &
                   ob%bogus(n)%slp, iv%bogus(n)%slp, re%bogus(n)%slp    ! O, O-B, O-A p
               write(ounit,'(i8)')iv % bogus(n) % info % levels
               do k = 1, iv % bogus(n) % info % levels
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs , k, iv % bogus(n) % info % id, &  ! Station
                     iv % bogus(n) % info % lat, &       ! Latitude
                     iv % bogus(n) % info % lon, &       ! Longitude
                     iv % bogus(n) % p(k), &             ! Obs Pressure
                     ob%bogus(n)%u(k), iv%bogus(n)%u(k), re%bogus(n)%u(k), &! O,O-B,O-A u
                     ob%bogus(n)%v(k), iv%bogus(n)%v(k), re%bogus(n)%v(k), &! O,O-B,O-A v
                     ob%bogus(n)%t(k), iv%bogus(n)%t(k), re%bogus(n)%t(k), &! O,O-B,O-A t
                     ob%bogus(n)%q(k), iv%bogus(n)%q(k), re%bogus(n)%q(k)   ! O,O-B,O-A q 
               end do
            end if
         end do
      end if
   end if

   ! Transfer AIRS retrievals:

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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs,k, iv % airsr(n) % info % id, &  ! Station
                     iv % airsr(n) % info % lat, &       ! Latitude
                     iv % airsr(n) % info % lon, &       ! Longitude
                     iv % airsr(n) % p(k), &             ! Obs Pressure
                     ob%airsr(n)%t(k), iv%airsr(n)%t(k), re%airsr(n)%t(k), &! O,O-B,O-A t
                     ob%airsr(n)%q(k), iv%airsr(n)%q(k), re%airsr(n)%q(k)   ! O,O-B,O-A q
               end do
            end if
         end do
      end if
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
                  write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                     num_obs,k, iv % gpsref(n) % info % id, &  ! Station
                     iv % gpsref(n) % info % lat, &       ! Latitude
                     iv % gpsref(n) % info % lon, &       ! Longitude
                     iv % gpsref(n) % h(k), &             ! Obs Height    
                     ob%gpsref(n)%ref(k), iv%gpsref(n)%ref(k), re%gpsref(n)%ref(k) ! O, O-B, O-A ref
               end do
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs")

end subroutine da_write_obs


