subroutine da_write_iv_for_multi_inc(file_index, iv)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(in)    :: iv      ! O-B structure.
   integer, intent (in)          :: file_index
      
   integer                       :: n, k, num_obs, ios
   integer                       :: ounit     ! Output unit           
   character(len=filename_len)   :: filename

   if (trace_use) call da_trace_entry("da_write_iv_for_multi_inc")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

   write(unit=filename, fmt='(a,i3.3)') 'gts_omb.', file_index

   ! [1] Transfer surface obs:

   if (iv%info(synop)%plocal(iv%time) - iv%info(synop)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.synop',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'synop',iv%info(synop)%plocal(iv%time) - &
                                     iv%info(synop)%plocal(iv%time-1) 
      do n = iv%info(synop)%plocal(iv%time-1) + 1, &
             iv%info(synop)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
             n , iv % synop(n) % info % id, &  ! Station
             iv % synop(n) % info % lat, &       ! Latitude
             iv % synop(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,E22.13))')&
             iv % synop(n) % h, &
             iv%synop(n)%u, &!  O-B u
             iv%synop(n)%v, &!  O-B v
             iv%synop(n)%t, &!  O-B t
             iv%synop(n)%p, &!  O-B p
             iv%synop(n)%q  !  O-B q
      end do
      close (ounit)
   end if

   ! [2] Transfer metar obs:

   if (iv%info(metar)%plocal(iv%time) - iv%info(metar)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.metar',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'metar', iv%info(metar)%plocal(iv%time) - &
                                       iv%info(metar)%plocal(iv%time-1)
      do n = iv%info(metar)%plocal(iv%time-1) + 1, &
             iv%info(metar)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % metar(n) % info % id, &  ! Station
                 iv % metar(n) % info % lat, &       ! Latitude
                 iv % metar(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,E22.13))')&
                 iv % metar(n) % h, & 
                 iv%metar(n)%u, &! O-B u
                 iv%metar(n)%v, &! O-B v
                 iv%metar(n)%t, &! O-B t
                 iv%metar(n)%p, &! O-B p
                 iv%metar(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [3] Transfer ships obs:

   if (iv%info(ships)%plocal(iv%time) - iv%info(ships)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ships',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ships', iv%info(ships)%plocal(iv%time) - &
                                       iv%info(ships)%plocal(iv%time-1)
      do n = iv%info(ships)%plocal(iv%time-1) + 1, &
             iv%info(ships)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % ships(n) % info % id, &  ! Station
                 iv % ships(n) % info % lat, &       ! Latitude
                 iv % ships(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,E22.13))')&
                 iv % ships(n) % h, &
                 iv%ships(n)%u, &! O-B u
                 iv%ships(n)%v, &! O-B v
                 iv%ships(n)%t, &! O-B t
                 iv%ships(n)%p, &! O-B p
                 iv%ships(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [4] Transfer sonde_sfc obs:

   if (iv%info(sound)%plocal(iv%time) - iv%info(sound)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.sonde_sfc',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sonde_sfc', iv%info(sound)%plocal(iv%time) - &
                                       iv%info(sound)%plocal(iv%time-1)
      do n = iv%info(sound)%plocal(iv%time-1) + 1, &
             iv%info(sound)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % sonde_sfc(n) % info % id, &  ! Station
                 iv % sonde_sfc(n) % info % lat, &       ! Latitude
                 iv % sonde_sfc(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,E22.13))')&
                 iv % sonde_sfc(n) % h, &
                 iv%sonde_sfc(n)%u, &! O-B u
                 iv%sonde_sfc(n)%v, &! O-B v
                 iv%sonde_sfc(n)%t, &! O-B t
                 iv%sonde_sfc(n)%p, &! O-B p
                 iv%sonde_sfc(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [5] Transfer sound obs:

   if (iv%info(sound)%plocal(iv%time) - iv%info(sound)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.sound',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sound', iv%info(sound)%plocal(iv%time) - &
                                       iv%info(sound)%plocal(iv%time-1)
      do n = iv%info(sound)%plocal(iv%time-1) + 1, &
             iv%info(sound)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % sound(n) % info % levels, iv % sound(n) % info % id, &  ! Station
                 iv % sound(n) % info % lat, &       ! Latitude
                 iv % sound(n) % info % lon          ! Longitude
         do k = 1 , iv % sound(n) % info % levels
            write(ounit,'(3E22.13,4(E22.13,i8,E22.13))')&
                     iv % sound(n) % h(k), &
                     iv % sound(n) % zk(k), &
                     iv % sound(n) % p(k), &             ! Obs Pressure
                     iv%sound(n)%u(k), &! O-B u
                     iv%sound(n)%v(k), &! O-B v
                     iv%sound(n)%t(k), &! O-B t
                     iv%sound(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [6] Transfer buoy obs:

   if (iv%info(buoy)%plocal(iv%time) - iv%info(buoy)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.buoy',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'buoy', iv%info(buoy)%plocal(iv%time) - &
                                       iv%info(buoy)%plocal(iv%time-1)
      do n = iv%info(buoy)%plocal(iv%time-1) + 1, &
             iv%info(buoy)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % buoy(n) % info % id, &  ! Station
                 iv % buoy(n) % info % lat, &       ! Latitude
                 iv % buoy(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,E22.13))')&
                 iv % buoy(n) % h, &
                 iv%buoy(n)%u, &! O-B u
                 iv%buoy(n)%v, &! O-B v
                 iv%buoy(n)%t, &! O-B t
                 iv%buoy(n)%p, &! O-B p
                 iv%buoy(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [7] Transfer Geo AMVs obs:

   if (iv%info(geoamv)%plocal(iv%time) - iv%info(geoamv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.geoamv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'geoamv', iv%info(geoamv)%plocal(iv%time) - &
                                       iv%info(geoamv)%plocal(iv%time-1)
      do n = iv%info(geoamv)%plocal(iv%time-1) + 1, &
             iv%info(geoamv)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % geoamv(n) % info % levels, iv % geoamv(n) % info % id, &  ! Station
                 iv % geoamv(n) % info % lat, &       ! Latitude
                 iv % geoamv(n) % info % lon          ! Longitude
         do k = 1 , iv % geoamv(n) % info % levels
            write(ounit,'(2E22.13,2(E22.13,i8,E22.13))')&
                      iv % geoamv(n) % p(k), &                ! Obs Pressure
                      iv % geoamv(n) % zk(k), &                ! Obs Pressure
                      iv%geoamv(n)%u(k), &! O-B u
                      iv%geoamv(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [8] Transfer gpspw obs:

   if (iv%info(gpspw)%plocal(iv%time) - iv%info(gpspw)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.gpspw',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpspw', iv%info(gpspw)%plocal(iv%time) - &
                                       iv%info(gpspw)%plocal(iv%time-1)
      do n = iv%info(gpspw)%plocal(iv%time-1) + 1, &
             iv%info(gpspw)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % gpspw(n) % info % id, &  ! Station
                 iv % gpspw(n) % info % lat, &       ! Latitude
                 iv % gpspw(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,i8,E22.13)')&
                 iv%gpspw(n)%tpw
      end do
      close (ounit)
   end if

   ! [9] Transfer SSM/I obs:

   if (iv%info(ssmi_rv)%plocal(iv%time) - iv%info(ssmi_rv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmir',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmir', iv%info(ssmi_rv)%plocal(iv%time) - &
                                       iv%info(ssmi_rv)%plocal(iv%time-1)
      do n = iv%info(ssmi_rv)%plocal(iv%time-1) + 1, &
             iv%info(ssmi_rv)%plocal(iv%time)
         write(ounit,'(i8,2E22.13)')&
                 n, &  ! Station
                 iv % ssmi_rv(n) % info % lat, &       ! Latitude
                 iv % ssmi_rv(n) % info % lon          ! Longitude
         write(ounit,'2(E22.13,i8,E22.13)')&
                 iv%ssmi_rv(n)%speed, & ! O-B speed
                 iv%ssmi_rv(n)%tpw ! O-BA tpw
      end do
      close (ounit)
   end if

   ! [10] Transfer airep obs:

   if (iv%info(airep)%plocal(iv%time) - iv%info(airep)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.airep',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airep', iv%info(airep)%plocal(iv%time) - &
                                       iv%info(airep)%plocal(iv%time-1)
      do n = iv%info(airep)%plocal(iv%time-1) + 1, &
             iv%info(airep)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % airep(n) % info % levels, iv % airep(n) % info % id, &  ! Station
                 iv % airep(n) % info % lat, &       ! Latitude
                 iv % airep(n) % info % lon          ! Longitude
         do k = 1 , iv % airep(n) % info % levels
            write(ounit,'(3E22.13,3(E22.13,i8,E22.13))')&
                     iv % airep(n) % h(k), &
                     iv % airep(n) % zk(k), &
                     iv % airep(n) % p(k), &             ! Obs pressure
                     iv%airep(n)%u(k), &! O-B u
                     iv%airep(n)%v(k), &! O-B v
                     iv%airep(n)%t(k)
         enddo
      end do
      close (ounit)
   end if

   ! [11] Transfer Polar AMVs obs:

   if (iv%info(polaramv)%plocal(iv%time) - iv%info(polaramv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.polaramv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'polaramv', iv%info(polaramv)%plocal(iv%time) - &
                                       iv%info(polaramv)%plocal(iv%time-1)
      do n = iv%info(polaramv)%plocal(iv%time-1) + 1, &
             iv%info(polaramv)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % polaramv(n) % info % levels, iv % polaramv(n) % info % id, &  ! Station
                 iv % polaramv(n) % info % lat, &       ! Latitude
                 iv % polaramv(n) % info % lon          ! Longitude
         do k = 1 , iv % polaramv(n) % info % levels
            write(ounit,'(2E22.13,2(E22.13,i8,E22.13))')&
                      iv % polaramv(n) % p(k), &                ! Obs Pressure
                      iv % polaramv(n) % zk(k), &                ! Obs Pressure
                      iv%polaramv(n)%u(k), &! O-B u
                      iv%polaramv(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [12] Transfer pilot obs:

   if (iv%info(pilot)%plocal(iv%time) - iv%info(pilot)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.pilot',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'pilot', iv%info(pilot)%plocal(iv%time) - &
                                       iv%info(pilot)%plocal(iv%time-1)
      do n = iv%info(pilot)%plocal(iv%time-1) + 1, &
             iv%info(pilot)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % pilot(n) % info % levels, iv % pilot(n) % info % id, &  ! Station
                 iv % pilot(n) % info % lat, &       ! Latitude
                 iv % pilot(n) % info % lon          ! Longitude
         do k = 1 , iv % pilot(n) % info % levels
            write(ounit,'(2E22.13,2(E22.13,i8,E22.13))')&
                      iv % pilot(n) % p(k), &                ! Obs Pressure
                      iv % pilot(n) % zk(k), &                ! Obs Pressure
                      iv%pilot(n)%u(k), &! O-B u
                      iv%pilot(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [13] Transfer ssmi_tb obs:

   if (iv%info(ssmi_tb)%plocal(iv%time) - iv%info(ssmi_tb)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmi_tb',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmi_tb', iv%info(ssmi_tb)%plocal(iv%time) - &
                                       iv%info(ssmi_tb)%plocal(iv%time-1)
      do n = iv%info(ssmi_tb)%plocal(iv%time-1) + 1, &
             iv%info(ssmi_tb)%plocal(iv%time)
         write(ounit,'(i8,2E22.13)')&
                 n, &  ! Station
                 iv % ssmi_tb(n) % info % lat, &       ! Latitude
                 iv % ssmi_tb(n) % info % lon          ! Longitude
         write(ounit,'(7(E22.13,i8,E22.13))')&
                 iv%ssmi_tb(n)%tb19h, & ! O-B Tb19h
                 iv%ssmi_tb(n)%tb19v, & ! O-B Tb19v
                 iv%ssmi_tb(n)%tb22v, & ! O-B Tb22v
                 iv%ssmi_tb(n)%tb37h, & ! O-B Tb37h
                 iv%ssmi_tb(n)%tb37v, & ! O-B Tb37v
                 iv%ssmi_tb(n)%tb85h, & ! O-B Tb85h
                 iv%ssmi_tb(n)%tb85v    ! O-B Tb85v
      end do
      close (ounit)
   end if

   ! [14] Transfer satem obs:

   if (iv%info(satem)%plocal(iv%time) - iv%info(satem)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.satem',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'satem', iv%info(satem)%plocal(iv%time) - &
                                       iv%info(satem)%plocal(iv%time-1)
      do n = iv%info(satem)%plocal(iv%time-1) + 1, &
             iv%info(satem)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % satem(n) % info % levels, iv % satem(n) % info % id, &  ! Station
                 iv % satem(n) % info % lat, &       ! Latitude
                 iv % satem(n) % info % lon          ! Longitude
         do k = 1 , iv % satem(n) % info % levels
            write(ounit,'(E22.13,(E22.13,i8,E22.13))')&
                 iv % satem(n) % p(k), &             ! Obs Pressure
                 iv%satem(n)%thickness(k)
         enddo
      end do
      close (ounit)
   end if

   ! [15] Transfer ssmt1 obs:

   if (iv%info(ssmt1)%plocal(iv%time) - iv%info(ssmt1)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt1',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt1', iv%info(ssmt1)%plocal(iv%time) - &
                                       iv%info(ssmt1)%plocal(iv%time-1)
      do n = iv%info(ssmt1)%plocal(iv%time-1) + 1, &
             iv%info(ssmt1)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % ssmt1(n) % info % levels, iv % ssmt1(n) % info % id, &  ! Station
                 iv % ssmt1(n) % info % lat, &       ! Latitude
                 iv % ssmt1(n) % info % lon          ! Longitude
         do k = 1 , iv % ssmt1(n) % info % levels
            write(ounit,'(E22.13,(E22.13,i8,E22.13))')&
                  iv % ssmt1(n) % h(k), &             ! Obs height
                  iv%ssmt1(n)%t(k)
         enddo
      end do
      close (ounit)
   end if

   ! [16] Transfer ssmt2 obs:

   if (iv%info(ssmt2)%plocal(iv%time) - iv%info(ssmt2)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt2',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt2', iv%info(ssmt2)%plocal(iv%time) - &
                                       iv%info(ssmt2)%plocal(iv%time-1)
      do n = iv%info(ssmt2)%plocal(iv%time-1) + 1, &
             iv%info(ssmt2)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % ssmt2(n) % info % levels, iv % ssmt2(n) % info % id, &  ! Station
                 iv % ssmt2(n) % info % lat, &       ! Latitude
                 iv % ssmt2(n) % info % lon          ! Longitude
         do k = 1 , iv % ssmt2(n) % info % levels
            write(ounit,'(E22.13,(E22.13,i8,E22.13))')&
                  iv % ssmt2(n) % h(k), &             ! Obs height
                  iv%ssmt2(n)%rh(k)
         enddo
      end do
      close (ounit)
   end if

   ! [17] Transfer scatterometer obs:

   if (iv%info(qscat)%plocal(iv%time) - iv%info(qscat)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.qscat',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'qscat', iv%info(qscat)%plocal(iv%time) - &
                                       iv%info(qscat)%plocal(iv%time-1)
      do n = iv%info(qscat)%plocal(iv%time-1) + 1, &
             iv%info(qscat)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv % qscat(n) % info % id, &  ! Station
                 iv % qscat(n) % info % lat, &       ! Latitude
                 iv % qscat(n) % info % lon          ! Longitude
         write(ounit,'(2E22.13,2(E22.13,i8,E22.13))')&
                   iv % qscat(n) % h, &                ! Obs height
                   iv % qscat(n) % zk, &                ! Obs height
                   iv%qscat(n)%u, &! O-B u
                   iv%qscat(n)%v   ! O-B v
      end do
      close (ounit)
   end if

   ! [18] Transfer profiler obs:

   if (iv%info(profiler)%plocal(iv%time) - iv%info(profiler)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.profiler',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'profiler', iv%info(profiler)%plocal(iv%time) - &
                                       iv%info(profiler)%plocal(iv%time-1)
      do n = iv%info(profiler)%plocal(iv%time-1) + 1, &
             iv%info(profiler)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % profiler(n) % info % levels, iv % profiler(n) % info % id, &  ! Station
                 iv % profiler(n) % info % lat, &       ! Latitude
                 iv % profiler(n) % info % lon          ! Longitude
         do k = 1 , iv % profiler(n) % info % levels
            write(ounit,'(2E22.13,2(E22.13,i8,E22.13))')&
                     iv % profiler(n) % p(k), &             ! Obs Pressure
                     iv % profiler(n) % zk(k), &             ! Obs Pressure
                     iv%profiler(n)%u(k), &! O-B u
                     iv%profiler(n)%v(k) ! O-B v
         enddo
      end do
      close (ounit)
   end if

   ! [19] Transfer TC bogus obs:

   if (iv%info(bogus)%plocal(iv%time) - iv%info(bogus)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.bogus',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'bogus', iv%info(bogus)%plocal(iv%time) - &
                                       iv%info(bogus)%plocal(iv%time-1)
      do n = iv%info(bogus)%plocal(iv%time-1) + 1, &
             iv%info(bogus)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % bogus(n) % info % levels, iv % bogus(n) % info % id, &  ! Station
                 iv % bogus(n) % info % lat, &       ! Latitude
                 iv % bogus(n) % info % lon          ! Longitude
         write(ounit,'(E22.13,i8,E22.13)')&
                 iv%bogus(n)%slp    ! O-B p
         do k = 1 , iv % bogus(n) % info % levels
            write(ounit,'(3E22.13,4(E22.13,i8,E22.13))')&
                     iv % bogus(n) % h(k), &
                     iv % bogus(n) % zk(k), &
                     iv % bogus(n) % p(k), &             ! Obs Pressure
                     iv%bogus(n)%u(k), &! O-B u
                     iv%bogus(n)%v(k), &! O-B v
                     iv%bogus(n)%t(k), &! O-B t
                     iv%bogus(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [20] Transfer AIRS retrievals:

   if (iv%info(airsr)%plocal(iv%time) - iv%info(airsr)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.airsr',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airsr', iv%info(airsr)%plocal(iv%time) - &
                                       iv%info(airsr)%plocal(iv%time-1)
      do n = iv%info(airsr)%plocal(iv%time-1) + 1, &
             iv%info(airsr)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % airsr(n) % info % levels, iv % airsr(n) % info % id, &  ! Station
                 iv % airsr(n) % info % lat, &       ! Latitude
                 iv % airsr(n) % info % lon          ! Longitude
         do k = 1 , iv % airsr(n) % info % levels
            write(ounit,'(E22.13,2(E22.13,i8,E22.13))')&
                     iv % airsr(n) % p(k), &             ! Obs Pressure
                     iv%airsr(n)%t(k), &! O-B t
                     iv%airsr(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [21] Transfer gpsref obs:

   if (iv%info(gpsref)%plocal(iv%time) - iv%info(gpsref)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.gpsref',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpsref', iv%info(gpsref)%plocal(iv%time) - &
                                       iv%info(gpsref)%plocal(iv%time-1)
      do n = iv%info(gpsref)%plocal(iv%time-1) + 1, &
             iv%info(gpsref)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv % gpsref(n) % info % levels, iv % gpsref(n) % info % id, &  ! Station
                 iv % gpsref(n) % info % lat, &       ! Latitude
                 iv % gpsref(n) % info % lon          ! Longitude
         do k = 1 , iv % gpsref(n) % info % levels
            write(ounit,'(2E22.13,(E22.13,i8,E22.13))')&
                     iv % gpsref(n) % h(k), &             ! Obs Height
                     iv % gpsref(n) % zk(k), &             ! Obs Height
                     iv%gpsref(n)%ref(k) ! O-B ref
         enddo
      end do
      close (ounit)
   end if

   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_iv_for_multi_inc")

end subroutine da_write_iv_for_multi_inc


