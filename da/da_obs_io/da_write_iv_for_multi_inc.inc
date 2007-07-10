subroutine da_write_iv_for_multi_inc(file_index, iv)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (ob_type), intent(in)    :: iv      ! O-B structure.
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

   if (iv%ob_numb(iv%current_ob_time)%synop - iv%ob_numb(iv%current_ob_time-1)%synop > 0) then

      open (unit=ounit,file=trim(filename)//'.synop',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'synop',iv%ob_numb(iv%current_ob_time)%synop - &
                                     iv%ob_numb(iv%current_ob_time-1)%synop 
      do n = iv%ob_numb(iv%current_ob_time-1)%synop + 1, &
             iv%ob_numb(iv%current_ob_time)%synop
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

   if (iv%ob_numb(iv%current_ob_time)%metar - iv%ob_numb(iv%current_ob_time-1)%metar > 0) then

      open (unit=ounit,file=trim(filename)//'.metar',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'metar', iv%ob_numb(iv%current_ob_time)%metar - &
                                       iv%ob_numb(iv%current_ob_time-1)%metar
      do n = iv%ob_numb(iv%current_ob_time-1)%metar + 1, &
             iv%ob_numb(iv%current_ob_time)%metar
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

   if (iv%ob_numb(iv%current_ob_time)%ships - iv%ob_numb(iv%current_ob_time-1)%ships > 0) then

      open (unit=ounit,file=trim(filename)//'.ships',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ships', iv%ob_numb(iv%current_ob_time)%metar - &
                                       iv%ob_numb(iv%current_ob_time-1)%metar
      do n = iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
             iv%ob_numb(iv%current_ob_time)%ships
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

   if (iv%ob_numb(iv%current_ob_time)%sound - iv%ob_numb(iv%current_ob_time-1)%sound > 0) then

      open (unit=ounit,file=trim(filename)//'.sonde_sfc',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sonde_sfc', iv%ob_numb(iv%current_ob_time)%sound - &
                                       iv%ob_numb(iv%current_ob_time-1)%sound
      do n = iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
             iv%ob_numb(iv%current_ob_time)%sound
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

   if (iv%ob_numb(iv%current_ob_time)%sound - iv%ob_numb(iv%current_ob_time-1)%sound > 0) then

      open (unit=ounit,file=trim(filename)//'.sound',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sound', iv%ob_numb(iv%current_ob_time)%sound - &
                                       iv%ob_numb(iv%current_ob_time-1)%sound
      do n = iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
             iv%ob_numb(iv%current_ob_time)%sound
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

   if (iv%ob_numb(iv%current_ob_time)%buoy - iv%ob_numb(iv%current_ob_time-1)%buoy > 0) then

      open (unit=ounit,file=trim(filename)//'.buoy',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'buoy', iv%ob_numb(iv%current_ob_time)%buoy - &
                                       iv%ob_numb(iv%current_ob_time-1)%buoy
      do n = iv%ob_numb(iv%current_ob_time-1)%buoy + 1, &
             iv%ob_numb(iv%current_ob_time)%buoy
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

   if (iv%ob_numb(iv%current_ob_time)%geoamv - iv%ob_numb(iv%current_ob_time-1)%geoamv > 0) then

      open (unit=ounit,file=trim(filename)//'.geoamv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'geoamv', iv%ob_numb(iv%current_ob_time)%geoamv - &
                                       iv%ob_numb(iv%current_ob_time-1)%geoamv
      do n = iv%ob_numb(iv%current_ob_time-1)%geoamv + 1, &
             iv%ob_numb(iv%current_ob_time)%geoamv
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

   if (iv%ob_numb(iv%current_ob_time)%gpspw - iv%ob_numb(iv%current_ob_time-1)%gpspw > 0) then

      open (unit=ounit,file=trim(filename)//'.gpspw',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpspw', iv%ob_numb(iv%current_ob_time)%gpspw - &
                                       iv%ob_numb(iv%current_ob_time-1)%gpspw
      do n = iv%ob_numb(iv%current_ob_time-1)%gpspw + 1, &
             iv%ob_numb(iv%current_ob_time)%gpspw
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

   if (iv%ob_numb(iv%current_ob_time)%ssmi_retrieval - iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmir',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmir', iv%ob_numb(iv%current_ob_time)%ssmi_retrieval - &
                                       iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval
      do n = iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval + 1, &
             iv%ob_numb(iv%current_ob_time)%ssmi_retrieval
         write(ounit,'(i8,2E22.13)')&
                 n, &  ! Station
                 iv % ssmi_retrieval(n) % info % lat, &       ! Latitude
                 iv % ssmi_retrieval(n) % info % lon          ! Longitude
         write(ounit,'2(E22.13,i8,E22.13)')&
                 iv%ssmi_retrieval(n)%speed, & ! O-B speed
                 iv%ssmi_retrieval(n)%tpw ! O-BA tpw
      end do
      close (ounit)
   end if

   ! [10] Transfer airep obs:

   if (iv%ob_numb(iv%current_ob_time)%airep - iv%ob_numb(iv%current_ob_time-1)%airep > 0) then

      open (unit=ounit,file=trim(filename)//'.airep',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airep', iv%ob_numb(iv%current_ob_time)%airep - &
                                       iv%ob_numb(iv%current_ob_time-1)%airep
      do n = iv%ob_numb(iv%current_ob_time-1)%airep + 1, &
             iv%ob_numb(iv%current_ob_time)%airep
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

   if (iv%ob_numb(iv%current_ob_time)%polaramv - iv%ob_numb(iv%current_ob_time-1)%polaramv > 0) then

      open (unit=ounit,file=trim(filename)//'.polaramv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'polaramv', iv%ob_numb(iv%current_ob_time)%polaramv - &
                                       iv%ob_numb(iv%current_ob_time-1)%polaramv
      do n = iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, &
             iv%ob_numb(iv%current_ob_time)%polaramv
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

   if (iv%ob_numb(iv%current_ob_time)%pilot - iv%ob_numb(iv%current_ob_time-1)%pilot > 0) then

      open (unit=ounit,file=trim(filename)//'.pilot',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'pilot', iv%ob_numb(iv%current_ob_time)%pilot - &
                                       iv%ob_numb(iv%current_ob_time-1)%pilot
      do n = iv%ob_numb(iv%current_ob_time-1)%pilot + 1, &
             iv%ob_numb(iv%current_ob_time)%pilot
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

   if (iv%ob_numb(iv%current_ob_time)%ssmi_tb - iv%ob_numb(iv%current_ob_time-1)%ssmi_tb > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmi_tb',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmi_tb', iv%ob_numb(iv%current_ob_time)%ssmi_tb - &
                                       iv%ob_numb(iv%current_ob_time-1)%ssmi_tb
      do n = iv%ob_numb(iv%current_ob_time-1)%ssmi_tb + 1, &
             iv%ob_numb(iv%current_ob_time)%ssmi_tb
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

   if (iv%ob_numb(iv%current_ob_time)%satem - iv%ob_numb(iv%current_ob_time-1)%satem > 0) then

      open (unit=ounit,file=trim(filename)//'.satem',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'satem', iv%ob_numb(iv%current_ob_time)%satem - &
                                       iv%ob_numb(iv%current_ob_time-1)%satem
      do n = iv%ob_numb(iv%current_ob_time-1)%satem + 1, &
             iv%ob_numb(iv%current_ob_time)%satem
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

   if (iv%ob_numb(iv%current_ob_time)%ssmt1 - iv%ob_numb(iv%current_ob_time-1)%ssmt1 > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt1',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt1', iv%ob_numb(iv%current_ob_time)%ssmt1 - &
                                       iv%ob_numb(iv%current_ob_time-1)%ssmt1
      do n = iv%ob_numb(iv%current_ob_time-1)%ssmt1 + 1, &
             iv%ob_numb(iv%current_ob_time)%ssmt1
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

   if (iv%ob_numb(iv%current_ob_time)%ssmt2 - iv%ob_numb(iv%current_ob_time-1)%ssmt2 > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt2',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt2', iv%ob_numb(iv%current_ob_time)%ssmt2 - &
                                       iv%ob_numb(iv%current_ob_time-1)%ssmt2
      do n = iv%ob_numb(iv%current_ob_time-1)%ssmt2 + 1, &
             iv%ob_numb(iv%current_ob_time)%ssmt2
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

   if (iv%ob_numb(iv%current_ob_time)%qscat - iv%ob_numb(iv%current_ob_time-1)%qscat > 0) then

      open (unit=ounit,file=trim(filename)//'.qscat',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'qscat', iv%ob_numb(iv%current_ob_time)%qscat - &
                                       iv%ob_numb(iv%current_ob_time-1)%qscat
      do n = iv%ob_numb(iv%current_ob_time-1)%qscat + 1, &
             iv%ob_numb(iv%current_ob_time)%qscat
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

   if (iv%ob_numb(iv%current_ob_time)%profiler - iv%ob_numb(iv%current_ob_time-1)%profiler > 0) then

      open (unit=ounit,file=trim(filename)//'.profiler',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'profiler', iv%ob_numb(iv%current_ob_time)%profiler - &
                                       iv%ob_numb(iv%current_ob_time-1)%profiler
      do n = iv%ob_numb(iv%current_ob_time-1)%profiler + 1, &
             iv%ob_numb(iv%current_ob_time)%profiler
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

   if (iv%ob_numb(iv%current_ob_time)%bogus - iv%ob_numb(iv%current_ob_time-1)%bogus > 0) then

      open (unit=ounit,file=trim(filename)//'.bogus',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'bogus', iv%ob_numb(iv%current_ob_time)%bogus - &
                                       iv%ob_numb(iv%current_ob_time-1)%bogus
      do n = iv%ob_numb(iv%current_ob_time-1)%bogus + 1, &
             iv%ob_numb(iv%current_ob_time)%bogus
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

   if (iv%ob_numb(iv%current_ob_time)%airsr - iv%ob_numb(iv%current_ob_time-1)%airsr > 0) then

      open (unit=ounit,file=trim(filename)//'.airsr',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airsr', iv%ob_numb(iv%current_ob_time)%airsr - &
                                       iv%ob_numb(iv%current_ob_time-1)%airsr
      do n = iv%ob_numb(iv%current_ob_time-1)%airsr + 1, &
             iv%ob_numb(iv%current_ob_time)%airsr
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

   if (iv%ob_numb(iv%current_ob_time)%gpsref - iv%ob_numb(iv%current_ob_time-1)%gpsref > 0) then

      open (unit=ounit,file=trim(filename)//'.gpsref',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpsref', iv%ob_numb(iv%current_ob_time)%gpsref - &
                                       iv%ob_numb(iv%current_ob_time-1)%gpsref
      do n = iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
             iv%ob_numb(iv%current_ob_time)%gpsref
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


