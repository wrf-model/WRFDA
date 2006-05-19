SUBROUTINE da_add_noise_to_ob( iv, ob )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003      Profiler and Buoy Obs            S. R. H. Rizvi
!
!  Purpose: Allocates observation structure and fills it fro iv.
!----------------------------------------------------------------------------   
   IMPLICIT NONE

   TYPE (ob_type), INTENT(INOUT) :: iv   ! Obs and header structure.
   TYPE (y_type), INTENT(INOUT)  :: ob   ! (Smaller) observation structure.

   real                          :: z1, z2, z3, z4, z5, z6, z7 ! Random numbers.
   integer                       :: n, k     ! Loop counters.

!----------------------------------------------------------------------
!  [1.0] Initiate random number sequence:
!----------------------------------------------------------------------

   call random_seed
   
!----------------------------------------------------------------------
!  [2.0] Create noise and output:
!----------------------------------------------------------------------

!  [2.1] Transfer surface obs:

   IF ( iv % num_synop > 0 ) THEN
   
      write(rand_unit,'(a,i8)')'synop', iv % num_synop

      DO n = 1, iv % num_synop

!        Add random perturbation:
         call da_add_noise( iv % synop(n) % u, ob % synop(n) % u, z1 )
         call da_add_noise( iv % synop(n) % v, ob % synop(n) % v, z2 )
         call da_add_noise( iv % synop(n) % t, ob % synop(n) % t, z3 )
         call da_add_noise( iv % synop(n) % p, ob % synop(n) % p, z4 )
         call da_add_noise( iv % synop(n) % q, ob % synop(n) % q, z5 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, iv % synop(n) % u % error, z1, &
                                  iv % synop(n) % v % error, z2, &
                                  iv % synop(n) % t % error, z3, &
                                  iv % synop(n) % p % error, z4, &
                                  iv % synop(n) % q % error, z5
      END DO
   ENDIF

!  [2.2] Transfer metar obs:

   IF ( iv % num_metar > 0 ) THEN

      write(rand_unit,'(a,i8)')'metar', iv % num_metar
   
      DO n = 1, iv % num_metar

!        Add random perturbation:
         call da_add_noise( iv % metar(n) % u, ob % metar(n) % u, z1 )
         call da_add_noise( iv % metar(n) % v, ob % metar(n) % v, z2 )
         call da_add_noise( iv % metar(n) % t, ob % metar(n) % t, z3 )
         call da_add_noise( iv % metar(n) % p, ob % metar(n) % p, z4 )
         call da_add_noise( iv % metar(n) % q, ob % metar(n) % q, z5 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % metar(n) % u % error, z1, &
                                  iv % metar(n) % v % error, z2, &
                                  iv % metar(n) % t % error, z3, &
                                  iv % metar(n) % p % error, z4, &
                                  iv % metar(n) % q % error, z5

      END DO
   ENDIF

!  [2.3] Transfer ships obs:

   IF ( iv % num_ships > 0 ) THEN

      write(rand_unit,'(a,i8)')'ships', iv % num_ships
  
      DO n = 1, iv % num_ships

!        Add random perturbation:
         call da_add_noise( iv % ships(n) % u, ob % ships(n) % u, z1 )
         call da_add_noise( iv % ships(n) % v, ob % ships(n) % v, z2 )
         call da_add_noise( iv % ships(n) % t, ob % ships(n) % t, z3 )
         call da_add_noise( iv % ships(n) % p, ob % ships(n) % p, z4 )
         call da_add_noise( iv % ships(n) % q, ob % ships(n) % q, z5 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % ships(n) % u % error, z1, &
                                  iv % ships(n) % v % error, z2, &
                                  iv % ships(n) % t % error, z3, &
                                  iv % ships(n) % p % error, z4, &
                                  iv % ships(n) % q % error, z5
      END DO
   ENDIF


!  [2.4.1] Transfer Geostationary AMVs obs:

   IF ( iv % num_geoamv > 0 ) THEN

      write(rand_unit,'(a,i8)')'geoamv', iv % num_geoamv

      DO n = 1, iv % num_geoamv
         write(rand_unit,'(i8)')iv % geoamv(n) % info % levels

         DO k = 1, iv % geoamv(n) % info % levels

!        Add random perturbation:
            call da_add_noise( iv % geoamv(n) % u(k), ob % geoamv(n) % u(k), z1)
            call da_add_noise( iv % geoamv(n) % v(k), ob % geoamv(n) % v(k), z2)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % geoamv(n) % u(k) % error, z1, &
                               iv % geoamv(n) % v(k) % error, z2
         END DO
      END DO
   ENDIF

!  [2.4.2] Transfer Polar AMVs obs:

   IF ( iv % num_polaramv > 0 ) THEN

      write(rand_unit,'(a,i8)')'polaramv', iv % num_polaramv

      DO n = 1, iv % num_geoamv
         write(rand_unit,'(i8)')iv % polaramv(n) % info % levels

         DO k = 1, iv % polaramv(n) % info % levels

!        Add random perturbation:
            call da_add_noise( iv % polaramv(n) % u(k), ob % polaramv(n) % u(k), z1)
            call da_add_noise( iv % polaramv(n) % v(k), ob % polaramv(n) % v(k), z2)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % polaramv(n) % u(k) % error, z1, &
                               iv % polaramv(n) % v(k) % error, z2
         END DO
      END DO
   ENDIF


!  [2.5] Transfer gpspw obs:

   IF ( iv % num_gpspw > 0 ) THEN

      write(rand_unit,'(a,i8)')'gpspw', iv % num_gpspw

      DO n = 1, iv % num_gpspw

!        Add random perturbation:
         call da_add_noise( iv % gpspw(n) % tpw, ob % gpspw(n) % tpw, z1 )
         
!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, iv % gpspw(n) % tpw % error, z1

      END DO
   ENDIF

!  [2.6] Transfer sonde obs:

   IF ( iv % num_sound > 0 ) THEN

      write(rand_unit,'(a,i8)')'sound', iv % num_sound

      DO n = 1, iv % num_sound
         write(rand_unit,'(i8)')iv % sound(n) % info % levels

         DO k = 1, iv % sound(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % sound(n) % u(k), ob % sound(n) % u(k), z1)
            call da_add_noise( iv % sound(n) % v(k), ob % sound(n) % v(k), z2)
            call da_add_noise( iv % sound(n) % t(k), ob % sound(n) % t(k), z3)
            call da_add_noise( iv % sound(n) % q(k), ob % sound(n) % q(k), z4)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % sound(n) % u(k) % error, z1, &
                               iv % sound(n) % v(k) % error, z2, &
                               iv % sound(n) % t(k) % error, z3, &
                               iv % sound(n) % q(k) % error, z4
         END DO

         call da_add_noise( iv % sonde_sfc(n) % u, ob % sonde_sfc(n) % u, z1 )
         call da_add_noise( iv % sonde_sfc(n) % v, ob % sonde_sfc(n) % v, z2 )
         call da_add_noise( iv % sonde_sfc(n) % t, ob % sonde_sfc(n) % t, z3 )
         call da_add_noise( iv % sonde_sfc(n) % p, ob % sonde_sfc(n) % p, z4 )
         call da_add_noise( iv % sonde_sfc(n) % q, ob % sonde_sfc(n) % q, z5 )
      END DO
   ENDIF

!  [2.7] Transfer airep obs:

   IF ( iv % num_airep > 0 ) THEN

      write(rand_unit,'(a,i8)')'airep', iv % num_airep

      DO n = 1, iv % num_airep
         write(rand_unit,'(i8)')iv % airep(n) % info % levels

         DO k = 1, iv % airep(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % airep(n) % u(k), ob % airep(n) % u(k), z1)
            call da_add_noise( iv % airep(n) % v(k), ob % airep(n) % v(k), z2)
            call da_add_noise( iv % airep(n) % t(k), ob % airep(n) % t(k), z3)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % airep(n) % u(k) % error, z1, &
                               iv % airep(n) % v(k) % error, z2, &
                               iv % airep(n) % t(k) % error, z3
         END DO
      END DO
   ENDIF

!  [2.8] Transfer pilot obs:

   IF ( iv % num_pilot > 0 ) THEN
   
      write(rand_unit,'(a,i8)')'pilot', iv % num_pilot
   
      DO n = 1, iv % num_pilot
         write(rand_unit,'(i8)')iv % pilot(n) % info % levels

         DO k = 1, iv % pilot(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % pilot(n) % u(k), ob % pilot(n) % u(k), z1)
            call da_add_noise( iv % pilot(n) % v(k), ob % pilot(n) % v(k), z2)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % pilot(n) % u(k) % error, z1, &
                               iv % pilot(n) % v(k) % error, z2
         END DO
      END DO
   ENDIF

!  [2.9] Transfer SSM/I obs:SSMI:

   IF ( iv % num_ssmi_retrieval > 0 ) THEN
      write(rand_unit,'(a,i8)')'ssmir', iv % num_ssmi_retrieval

      DO n = 1, iv % num_ssmi_retrieval
 
!        Add random perturbation:
         call da_add_noise( iv % ssmi_retrieval(n) % speed, &
                            ob % ssmi_retrieval(n) % speed, z1 )
         call da_add_noise( iv % ssmi_retrieval(n) % tpw, &
                            ob % ssmi_retrieval(n) % tpw, z2 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % ssmi_retrieval(n) % speed % error, z1, &
                                  iv % ssmi_retrieval(n) % tpw % error, z2
      END DO
   ENDIF

   IF ( iv % num_ssmi_tb > 0 ) THEN
      DO n = 1, iv % num_ssmi_tb

!        Add random perturbation:
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

!        Write out data:
         write(rand_unit,'(2i8,14e15.7)')n, 1, &
                                  iv % ssmi_tb(n) % tb19h % error, z1, &
                                  iv % ssmi_tb(n) % tb19v % error, z2, &
                                  iv % ssmi_tb(n) % tb22v % error, z3, &
                                  iv % ssmi_tb(n) % tb37h % error, z4, &
                                  iv % ssmi_tb(n) % tb37v % error, z5, &
                                  iv % ssmi_tb(n) % tb85h % error, z6, &
                                  iv % ssmi_tb(n) % tb85v % error, z7

      END DO
   ENDIF

!  [2.10] Transfer satem obs:

   IF ( iv % num_satem > 0 ) THEN
      write(rand_unit,'(a,i8)')'satem', iv % num_satem

      DO n = 1, iv % num_satem
         write(rand_unit,'(i8)')iv % satem(n) % info % levels

         DO k = 1, iv % satem(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % satem(n) % thickness(k), &
                               ob % satem(n) % thickness(k), z1 )

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                                     iv % satem(n) % thickness(k) % error, z1
         END DO
      END DO
   ENDIF
   
!  [2.11] Transfer ssmt1 obs:

   IF ( iv % num_ssmt1 > 0 ) THEN
      write(rand_unit,'(a,i8)')'ssmt1', iv % num_ssmt1

      DO n = 1, iv % num_ssmt1
         write(rand_unit,'(i8)')iv % ssmt1(n) % info % levels
         
         DO k = 1, iv % ssmt1(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % ssmt1(n) % t(k), &
                               ob % ssmt1(n) % t(k), z1 )

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, iv % ssmt1(n) % t(k) % error, z1
         END DO
      END DO
   ENDIF

!  [2.12] Transfer ssmt2 obs:

   IF ( iv % num_ssmt2 > 0 ) THEN
      write(rand_unit,'(a,i8)')'ssmt2', iv % num_ssmt2

      DO n = 1, iv % num_ssmt2
         write(rand_unit,'(i8)')iv % ssmt2(n) % info % levels
         
         DO k = 1, iv % ssmt2(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % ssmt2(n) % rh(k), &
                               ob % ssmt2(n) % rh(k), z1 )

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, iv % ssmt2(n) % rh(k) % error, z1
         END DO
      END DO
   ENDIF
   
!  [2.13] Transfer scatterometer obs:

   IF ( iv % num_qscat > 0 ) THEN

      write(rand_unit,'(a,i8)')'qscat', iv % num_qscat
  
      DO n = 1, iv % num_qscat

!        Add random perturbation:
         call da_add_noise( iv % qscat(n) % u, ob % qscat(n) % u, z1 )
         call da_add_noise( iv % qscat(n) % v, ob % qscat(n) % v, z2 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % qscat(n) % u % error, z1, &
                                  iv % qscat(n) % v % error, z2
      END DO
   ENDIF

!  [2.14] Transfer buoy obs:

   IF ( iv % num_buoy > 0 ) THEN

      write(rand_unit,'(a,i8)')'buoy', iv % num_buoy
  
      DO n = 1, iv % num_buoy

!        Add random perturbation:
         call da_add_noise( iv % buoy(n) % u, ob % buoy(n) % u, z1 )
         call da_add_noise( iv % buoy(n) % v, ob % buoy(n) % v, z2 )
         call da_add_noise( iv % buoy(n) % t, ob % buoy(n) % t, z3 )
         call da_add_noise( iv % buoy(n) % p, ob % buoy(n) % p, z4 )
         call da_add_noise( iv % buoy(n) % q, ob % buoy(n) % q, z5 )

!        Write out data:
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % buoy(n) % u % error, z1, &
                                  iv % buoy(n) % v % error, z2, &
                                  iv % buoy(n) % t % error, z3, &
                                  iv % buoy(n) % p % error, z4, &
                                  iv % buoy(n) % q % error, z5
      END DO
   ENDIF

!  [2.15] Transfer profiler obs:

   IF ( iv % num_profiler > 0 ) THEN
   
      write(rand_unit,'(a,i8)')'profiler', iv % num_profiler
   
      DO n = 1, iv % num_profiler
         write(rand_unit,'(i8)')iv % profiler(n) % info % levels

         DO k = 1, iv % profiler(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % profiler(n) % u(k), ob % profiler(n) % u(k), z1)
            call da_add_noise( iv % profiler(n) % v(k), ob % profiler(n) % v(k), z2)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % profiler(n) % u(k) % error, z1, &
                               iv % profiler(n) % v(k) % error, z2
         END DO
      END DO
   ENDIF

!  [2.16] Transfer TC bogus obs:

   IF ( iv % num_bogus > 0 ) THEN

      write(rand_unit,'(a,i8)')'bogus', iv % num_bogus

      DO n = 1, iv % num_bogus
         write(rand_unit,'(i8)')iv % bogus(n) % info % levels

         DO k = 1, iv % bogus(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % bogus(n) % u(k), ob % bogus(n) % u(k), z1)
            call da_add_noise( iv % bogus(n) % v(k), ob % bogus(n) % v(k), z2)
            call da_add_noise( iv % bogus(n) % t(k), ob % bogus(n) % t(k), z3)
            call da_add_noise( iv % bogus(n) % q(k), ob % bogus(n) % q(k), z4)

!           Write out data:
            write(rand_unit,'(2i8,10e15.7)')n, k, &
                               iv % bogus(n) % u(k) % error, z1, &
                               iv % bogus(n) % v(k) % error, z2, &
                               iv % bogus(n) % t(k) % error, z3, &
                               iv % bogus(n) % q(k) % error, z4
         END DO

         call da_add_noise( iv % bogus(n) % slp, ob % bogus(n) % slp, z5 )
         write(rand_unit,'(2i8,10e15.7)')n, 1, &
                                  iv % bogus(n) % slp % error, z5

      END DO
   ENDIF

END SUBROUTINE da_add_noise_to_ob
