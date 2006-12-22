subroutine da_add_noise_to_ob (iv, ob)

   !--------------------------------------------------------------------------
   !  Purpose: Allocates observation structure and fills it fro iv.
   !------------------------------------------------------------------------- 
  
   implicit none


   type (ob_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(inout)  :: ob   ! (Smaller) observation structure.

   real                          :: z1, z2, z3, z4, z5, z6, z7, dum ! Random numbers.
   integer                       :: n, k, inst     ! Loop counters.
   integer                       :: ierr      ! Error code from MPI routines.
   integer                       :: ounit     ! Output unit
   integer                       :: num_obs, ios                 
   character(len=20)             :: ob_name, filename

   ! Fix output unit
   call da_get_unit(ounit)

   dum = -999999.9
   
   ! Initiate random number sequence:
   call da_random_seed
   
   ! Create noise and output:

#ifdef DM_PARALLEL
   write(unit=filename, fmt='(a,i3.3)') 'rand_obs_error.', myproc
#else
   write(unit=filename, fmt='(a)') 'rand_obs_error.000'
#endif

   open(unit=ounit,file=trim(filename),form='formatted',iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//filename/))
   end if

   !  [2.1] Transfer surface obs:

   if (iv % num_synop > 0) then
      num_obs = 0
      do n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'synop', num_obs   
      num_obs = 0 

      do n = 1, iv % num_synop
         if (iv%synop(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write (ounit,'(i8)')  1
            call da_add_noise( iv % synop(n) % u, ob % synop(n) % u, z1 )
            call da_add_noise( iv % synop(n) % v, ob % synop(n) % v, z2 )
            call da_add_noise( iv % synop(n) % t, ob % synop(n) % t, z3 )
            call da_add_noise( iv % synop(n) % p, ob % synop(n) % p, z4 )
            call da_add_noise( iv % synop(n) % q, ob % synop(n) % q, z5 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % synop(n) % u % error, &
                                     z1, &
                                     iv % synop(n) % v % error, z2, &
                                     iv % synop(n) % t % error, z3, &
                                     iv % synop(n) % p % error, z4, &
                                     iv % synop(n) % q % error, z5
         end if
      end do
   end if

   !  [2.2] Transfer metar obs:

   if (iv % num_metar > 0) then
      num_obs = 0
      do n = 1, iv % num_metar
         if (iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'metar', num_obs   
      num_obs = 0 
      do n = 1, iv % num_metar
         if (iv%metar(n)%loc%proc_domain) then
            num_obs = num_obs + 1 
            write(ounit,'(i8)')  1
            call da_add_noise( iv % metar(n) % u, ob % metar(n) % u, z1 )
            call da_add_noise( iv % metar(n) % v, ob % metar(n) % v, z2 )
            call da_add_noise( iv % metar(n) % t, ob % metar(n) % t, z3 )
            call da_add_noise( iv % metar(n) % p, ob % metar(n) % p, z4 )
            call da_add_noise( iv % metar(n) % q, ob % metar(n) % q, z5 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                     iv % metar(n) % u % error, z1, &
                                     iv % metar(n) % v % error, z2, &
                                     iv % metar(n) % t % error, z3, &
                                     iv % metar(n) % p % error, z4, &
                                     iv % metar(n) % q % error, z5
         end if
      end do
   end if

   !  [2.3] Transfer ships obs:

   if (iv % num_ships > 0) then
      num_obs = 0
      do n = 1, iv % num_ships
         if (iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ships', num_obs   
      num_obs = 0 
      do n = 1, iv % num_ships 
         if (iv%ships(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')  1
            ! Add random perturbation:
            call da_add_noise( iv % ships(n) % u, ob % ships(n) % u, z1 )
            call da_add_noise( iv % ships(n) % v, ob % ships(n) % v, z2 )
            call da_add_noise( iv % ships(n) % t, ob % ships(n) % t, z3 )
            call da_add_noise( iv % ships(n) % p, ob % ships(n) % p, z4 )
            call da_add_noise( iv % ships(n) % q, ob % ships(n) % q, z5 )
            ! Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                     iv % ships(n) % u % error, z1, &
                                     iv % ships(n) % v % error, z2, &
                                     iv % ships(n) % t % error, z3, &
                                     iv % ships(n) % p % error, z4, &
                                     iv % ships(n) % q % error, z5
         end if
      end do
   end if

   !  [2.4.1] Transfer Geostationary AMVs obs:

   if (iv % num_geoamv > 0) then
      num_obs = 0
      do n = 1, iv % num_geoamv
        if (iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'geoamv', num_obs   
      num_obs = 0 
      do n = 1, iv % num_geoamv
         if (iv%geoamv(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % geoamv(n) % info % levels
            do k = 1, iv % geoamv(n) % info % levels
               ! Add random perturbation:
               call da_add_noise( iv % geoamv(n) % u(k), &
                  ob % geoamv(n) % u(k), z1)
               call da_add_noise( iv % geoamv(n) % v(k), &
                  ob % geoamv(n) % v(k), z2)

               ! Write out data:
               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                                  iv % geoamv(n) % u(k) % error, z1, &
                                  iv % geoamv(n) % v(k) % error, z2, &
                                  dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   ! [2.4.2] Transfer Polar AMVs obs:

   if (iv % num_polaramv > 0) then
      num_obs = 0
      do n = 1, iv % num_polaramv
         if (iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'polaramv', num_obs   
      num_obs = 0 
      do n = 1, iv % num_polaramv
         if (iv%polaramv(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % polaramv(n) % info % levels
            do k = 1, iv % polaramv(n) % info % levels
               ! Add random perturbation:
               call da_add_noise( iv % polaramv(n) % u(k), &
                  ob % polaramv(n) % u(k), z1)
               call da_add_noise( iv % polaramv(n) % v(k), &
                  ob % polaramv(n) % v(k), z2)

               ! Write out data:
               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                                  iv % polaramv(n) % u(k) % error, z1, &
                                  iv % polaramv(n) % v(k) % error, z2, &
                                  dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   !  [2.5] Transfer gpspw obs:

   if (iv % num_gpspw > 0) then
      num_obs = 0
      do n = 1, iv % num_gpspw
         if (iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'gpspw', num_obs   
      num_obs = 0 
      do n = 1, iv % num_gpspw
         if (iv%gpspw(n)%loc%proc_domain) then
            num_obs = num_obs + 1
           write(ounit,'(i8)')  1
           ! Add random perturbation:
           call da_add_noise( iv % gpspw(n) % tpw, ob % gpspw(n) % tpw, z1 )
         
           ! Write out data:
           write(ounit,'(2i8,10e15.7)')num_obs, 1, &
              iv % gpspw(n) % tpw % error, z1, &
              dum, dum, dum, dum, dum, dum, dum, dum
        end if
     end do
   end if

   ! [2.6] Transfer sonde obs:

   if (iv % num_sound > 0) then
      num_obs = 0
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'sound', num_obs   
      num_obs = 0 
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % sound(n) % info % levels
            do k = 1, iv % sound(n) % info % levels
               call da_add_noise (iv % sound(n) % u(k), ob % sound(n) % u(k), &
                  z1)
               call da_add_noise (iv % sound(n) % v(k), ob % sound(n) % v(k), &
                  z2)
               call da_add_noise (iv % sound(n) % t(k), ob % sound(n) % t(k), &
                  z3)
               call da_add_noise (iv % sound(n) % q(k), ob % sound(n) % q(k), &
                  z4)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                                  iv % sound(n) % u(k) % error, z1, &
                                  iv % sound(n) % v(k) % error, z2, &
                                  iv % sound(n) % t(k) % error, z3, &
                                  iv % sound(n) % q(k) % error, z4, &
                                  dum, dum
            end do
         end if
      end do

      ! Now do surface level
      num_obs = 0
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'sonde_sfc', num_obs   
      num_obs = 0 
      do n = 1, iv % num_sound
         if (iv%sound(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            call da_add_noise( iv % sonde_sfc(n) % u, ob % sonde_sfc(n) % u, z1 )
            call da_add_noise( iv % sonde_sfc(n) % v, ob % sonde_sfc(n) % v, z2 )
            call da_add_noise( iv % sonde_sfc(n) % t, ob % sonde_sfc(n) % t, z3 )
            call da_add_noise( iv % sonde_sfc(n) % p, ob % sonde_sfc(n) % p, z4 )
            call da_add_noise( iv % sonde_sfc(n) % q, ob % sonde_sfc(n) % q, z5 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
               iv % sonde_sfc(n) % u % error, z1, &
               iv % sonde_sfc(n) % v % error, z2, &
               iv % sonde_sfc(n) % t % error, z3, &
               iv % sonde_sfc(n) % p % error, z4, &
               iv % sonde_sfc(n) % q % error, z5
         end if
      end do
   end if

   ! [2.7] Transfer airep obs:

   if (iv % num_airep > 0) then
      num_obs = 0
      do n = 1, iv % num_airep
         if (iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'airep', num_obs   
      num_obs = 0 
      do n = 1, iv % num_airep
         if (iv%airep(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % airep(n) % info % levels
            do k = 1, iv % airep(n) % info % levels
               call da_add_noise (iv % airep(n) % u(k), ob % airep(n) % u(k), z1)
               call da_add_noise (iv % airep(n) % v(k), ob % airep(n) % v(k), z2)
               call da_add_noise (iv % airep(n) % t(k), ob % airep(n) % t(k), z3)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airep(n) % u(k) % error, z1, &
                               iv % airep(n) % v(k) % error, z2, &
                               iv % airep(n) % t(k) % error, z3, &
                               dum, dum, dum, dum
            end do
         end if
      end do
   end if

   ! [2.8] Transfer pilot obs:

   if (iv % num_pilot > 0) then
      num_obs = 0
      do n = 1, iv % num_pilot
         if (iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'pilot', num_obs   
      num_obs = 0 
      do n = 1, iv % num_pilot
         if (iv%pilot(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % pilot(n) % info % levels
            do k = 1, iv % pilot(n) % info % levels
               call da_add_noise( iv % pilot(n) % u(k), ob % pilot(n) % u(k), z1)
               call da_add_noise( iv % pilot(n) % v(k), ob % pilot(n) % v(k), z2)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % pilot(n) % u(k) % error, z1, &
                               iv % pilot(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   ! [2.9] Transfer SSM/I obs:SSMI:

   if (iv % num_ssmi_retrieval > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmi_retrieval
         if (iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmir', num_obs   
      num_obs = 0 
      do n = 1, iv % num_ssmi_retrieval
         if (iv%ssmi_retrieval(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1 
 
            call da_add_noise( iv % ssmi_retrieval(n) % speed, &
                            ob % ssmi_retrieval(n) % speed, z1 )
            call da_add_noise( iv % ssmi_retrieval(n) % tpw, &
                            ob % ssmi_retrieval(n) % tpw, z2 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % ssmi_retrieval(n) % speed % error, z1, &
                                  iv % ssmi_retrieval(n) % tpw % error, z2,   & 
                                  dum, dum, dum, dum, dum, dum
         end if
      end do
   end if

   if (iv % num_ssmi_tb > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmi_tb
         if (iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmiT', num_obs   
      num_obs = 0 
      do n = 1, iv % num_ssmi_tb
         if (iv%ssmi_tb(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            call da_add_noise( iv % ssmi_tb(n) % tb19h, &
                               ob % ssmi_tb(n) % tb19h, z1)
            call da_add_noise( iv % ssmi_tb(n) % tb19v, &
                               ob % ssmi_tb(n) % tb19v, z2)
            call da_add_noise( iv % ssmi_tb(n) % tb22v, &
                               ob % ssmi_tb(n) % tb22v, z3)
            call da_add_noise( iv % ssmi_tb(n) % tb37h, &
                               ob % ssmi_tb(n) % tb37h, z4)
            call da_add_noise( iv % ssmi_tb(n) % tb37v, &
                               ob % ssmi_tb(n) % tb37v, z5)
            call da_add_noise( iv % ssmi_tb(n) % tb85h, &
                               ob % ssmi_tb(n) % tb85h, z6)
            call da_add_noise( iv % ssmi_tb(n) % tb85v, &
                               ob % ssmi_tb(n) % tb85v, z7)

            write(ounit,'(i8)') 1 
            write(ounit,'(2i8,14e15.7)')num_obs, 1, &
                                     iv % ssmi_tb(n) % tb19h % error, z1, &
                                     iv % ssmi_tb(n) % tb19v % error, z2, &
                                     iv % ssmi_tb(n) % tb22v % error, z3, &
                                     iv % ssmi_tb(n) % tb37h % error, z4, &
                                     iv % ssmi_tb(n) % tb37v % error, z5, &
                                     iv % ssmi_tb(n) % tb85h % error, z6, &
                                     iv % ssmi_tb(n) % tb85v % error, z7
          end if
      end do
   end if

   ! [2.10] Transfer satem obs:

   if (iv % num_satem > 0) then
      num_obs = 0
      do n = 1, iv % num_satem
         if (iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'satem', num_obs   
      num_obs = 0 
      do n = 1, iv % num_satem
         if (iv%satem(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % satem(n) % info % levels
            do k = 1, iv % satem(n) % info % levels
               call da_add_noise( iv % satem(n) % thickness(k), &
                  ob % satem(n) % thickness(k), z1 )
               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                  iv % satem(n) % thickness(k) % error, z1, &
                  dum, dum, dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if
   
   ! [2.11] Transfer ssmt1 obs:

   if (iv % num_ssmt1 > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmt1
         if (iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmt1', num_obs   
      num_obs = 0 

      do n = 1, iv % num_ssmt1
         if (iv%ssmt1(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % ssmt1(n) % info % levels      
            do k = 1, iv % ssmt1(n) % info % levels
               call da_add_noise( iv % ssmt1(n) % t(k), &
                               ob % ssmt1(n) % t(k), z1 )

               write (ounit,'(2i8,10e15.7)') num_obs, k, &
                  iv % ssmt1(n) % t(k) % error, z1, &
                  dum, dum, dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   ! [2.12] Transfer ssmt2 obs:

   if (iv % num_ssmt2 > 0) then
      num_obs = 0
      do n = 1, iv % num_ssmt2
         if (iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmt2', num_obs   
      num_obs = 0 

      do n = 1, iv % num_ssmt2
         if (iv%ssmt2(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % ssmt2(n) % info % levels
         
            do k = 1, iv % ssmt2(n) % info % levels

               call da_add_noise( iv % ssmt2(n) % rh(k), &
                               ob % ssmt2(n) % rh(k), z1 )

               write (ounit,'(2i8,10e15.7)')num_obs, k, &
                  iv % ssmt2(n) % rh(k) % error, z1, &
                  dum, dum, dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if
   
   ! [2.13] Transfer scatterometer obs:

   if (iv % num_qscat > 0) then
      num_obs = 0
      do n = 1, iv % num_qscat
         if (iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'qscat', num_obs   
      num_obs = 0 
      do n = 1, iv % num_qscat
         if (iv%qscat(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            call da_add_noise( iv % qscat(n) % u, ob % qscat(n) % u, z1 )
            call da_add_noise( iv % qscat(n) % v, ob % qscat(n) % v, z2 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % qscat(n) % u % error, z1, &
                                  iv % qscat(n) % v % error, z2, &
                                  dum, dum, dum, dum, dum, dum
         end if
      end do
   end if

   ! [2.14] Transfer buoy obs:

   if (iv % num_buoy > 0) then
      num_obs = 0
      do n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'buoy', num_obs   
      num_obs = 0 
      do n = 1, iv % num_buoy
         if (iv%buoy(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            call da_add_noise( iv % buoy(n) % u, ob % buoy(n) % u, z1 )
            call da_add_noise( iv % buoy(n) % v, ob % buoy(n) % v, z2 )
            call da_add_noise( iv % buoy(n) % t, ob % buoy(n) % t, z3 )
            call da_add_noise( iv % buoy(n) % p, ob % buoy(n) % p, z4 )
            call da_add_noise( iv % buoy(n) % q, ob % buoy(n) % q, z5 )

            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % buoy(n) % u % error, z1, &
                                  iv % buoy(n) % v % error, z2, &
                                  iv % buoy(n) % t % error, z3, &
                                  iv % buoy(n) % p % error, z4, &
                                  iv % buoy(n) % q % error, z5
         end if
      end do
   end if

   ! [2.15] Transfer profiler obs:

   if (iv % num_profiler > 0) then
      num_obs = 0
      do n = 1, iv % num_profiler
         if (iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'profiler', num_obs   
      num_obs = 0 
      do n = 1, iv % num_profiler
         if (iv%profiler(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % profiler(n) % info % levels
            do k = 1, iv % profiler(n) % info % levels
               call da_add_noise( iv % profiler(n) % u(k), &
                  ob % profiler(n) % u(k), z1)
               call da_add_noise( iv % profiler(n) % v(k), &
                  ob % profiler(n) % v(k), z2)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % profiler(n) % u(k) % error, z1, &
                               iv % profiler(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   ! [2.16] Transfer TC bogus obs:

   if (iv % num_bogus > 0) then
      num_obs = 0
      do n = 1, iv % num_bogus
         if (iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'bogus', num_obs   
      num_obs = 0 

      do n = 1, iv % num_bogus
         if (iv%bogus(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            call da_add_noise( iv % bogus(n) % slp, ob % bogus(n) % slp, z1 )
            write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % bogus(n) % slp % error, z1, &
                                  dum, dum, dum, dum, dum, dum, dum, dum

            write(ounit,'(i8)')iv % bogus(n) % info % levels
            do k = 1, iv % bogus(n) % info % levels
               call da_add_noise( iv % bogus(n) % u(k), ob % bogus(n) % u(k), z1)
               call da_add_noise( iv % bogus(n) % v(k), ob % bogus(n) % v(k), z2)
               call da_add_noise( iv % bogus(n) % t(k), ob % bogus(n) % t(k), z3)
               call da_add_noise( iv % bogus(n) % q(k), ob % bogus(n) % q(k), z4)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % bogus(n) % u(k) % error, z1, &
                               iv % bogus(n) % v(k) % error, z2, &
                               iv % bogus(n) % t(k) % error, z3, &
                               iv % bogus(n) % q(k) % error, z4, &
                               dum, dum
            end do
         end if
      end do
   end if

   ! Transfer AIRS retrievals:

   if (iv % num_airsr > 0) then
      num_obs = 0
      do n = 1, iv % num_airsr
         if (iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'airsr', num_obs   
      num_obs = 0 
      do n = 1, iv % num_airsr
         if (iv%airsr(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % airsr(n) % info % levels
            do k = 1, iv % airsr(n) % info % levels
               call da_add_noise( iv % airsr(n) % t(k), ob % airsr(n) % t(k), z1)
               call da_add_noise( iv % airsr(n) % q(k), ob % airsr(n) % q(k), z2)

               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airsr(n) % t(k) % error, z1, &
                               iv % airsr(n) % q(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   !  Transfer Radiance obs:

   if (iv%num_inst > 0) then
      do inst = 1, iv%num_inst                 ! loop for sensor
         if (iv%instid(inst)%num_rad < 1) cycle
         do k = 1,iv%instid(inst)%nchan        ! loop for channel
            !  Counting number of obs for channel k
            num_obs = 0
            do n = 1,iv%instid(inst)%num_rad      ! loop for pixel
               if (iv%instid(inst)%rad(n)%loc%proc_domain .and. &
                  (iv%instid(inst)%rad(n)%tb(k)%qc >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            if (num_obs < 1) cycle

            write (ob_name,'(a,a,i2.2)') &
               trim(iv%instid(inst)%rttovid_string),'-',k
            write(ounit,'(a20,i8)')  ob_name,num_obs

            num_obs = 0
            do n= 1, iv%instid(inst)%num_rad      ! loop for pixel
               if (iv%instid(inst)%rad(n)%loc%proc_domain .and. &
                  (iv%instid(inst)%rad(n)%tb(k)%qc >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
                  call da_add_noise( iv%instid(inst)%rad(n)%tb(k),  &
                                    ob%instid(inst)%rad(n)%tb(k), z1)
                 
                  write(ounit,'(2i8,f10.3,e15.7)') num_obs, 1,  &
                          iv%instid(inst)%rad(n)%tb(k)%error, z1
               end if
            end do                                ! end loop for pixel
         end do                                ! end loop for channel
      end do                                   ! end loop for sensor
   end if

   ! Transfer gpsref obs:

   if (iv % num_gpsref > 0) then
      num_obs = 0
      do n = 1, iv % num_gpsref
         if (iv%gpsref(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'gpsref', num_obs
      num_obs = 0 
      do n = 1, iv % num_gpsref
         if (iv%gpsref(n)%loc%proc_domain) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv % gpsref(n) % info % levels
            do k = 1, iv % gpsref(n) % info % levels
               call da_add_noise( iv % gpsref(n) % ref(k), &
                  ob % gpsref(n) % ref(k), z1 )
               write (ounit,'(2i8,10e15.7)')num_obs, k, &
                  iv % gpsref(n) % ref(k) % error, z1, &
                  dum, dum, dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

   close (ounit)
   call da_free_unit(ounit)

end subroutine da_add_noise_to_ob


