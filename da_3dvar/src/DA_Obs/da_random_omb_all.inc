SUBROUTINE da_random_omb_all( iv, ob )
!----------------------------------------------------------------------------
!  History:
!
!  Additions:
!           07/08/2003 - Profiler and Buoy Obs            Syed  RH  Rizvi
!           03/04/2005 - AMV's from Geostationary and     Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!
!
!  Purpose: Allocates observation structure and fills it fro iv.
!----------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (ob_type), INTENT(INOUT) :: iv   ! Obs and header structure.
   TYPE (y_type), INTENT(INOUT)  :: ob   ! (Smaller) observation structure.

   integer                       :: n, k ! Loop counters.

!----------------------------------------------------------------------
!  [1.0] Initialise random number sequence:
!----------------------------------------------------------------------

   call da_random_seed
   
!----------------------------------------------------------------------
!  [2.0] Randomize each ob in turn:
!----------------------------------------------------------------------

!  [2.1] Transfer surface obs:

   IF ( iv % num_synop > 0 ) THEN
      DO n = 1, iv % num_synop
         call da_random_omb( iv % synop(n) % u % error, ob % synop(n) % u, &
                           iv % synop(n) % u % qc, iv % synop(n) % u % inv )
         call da_random_omb( iv % synop(n) % v % error, ob % synop(n) % v, &
                           iv % synop(n) % v % qc, iv % synop(n) % v % inv )
         call da_random_omb( iv % synop(n) % t % error, ob % synop(n) % t, &
                           iv % synop(n) % t % qc, iv % synop(n) % t % inv  )
         call da_random_omb( iv % synop(n) % p % error, ob % synop(n) % p, &
                           iv % synop(n) % p % qc, iv % synop(n) % p % inv )
         call da_random_omb( iv % synop(n) % q % error, ob % synop(n) % q, &
                           iv % synop(n) % q % qc, iv % synop(n) % q % inv )
      END DO
   ENDIF

!  [2.2] Transfer metar obs:

   IF ( iv % num_metar > 0 ) THEN
      DO n = 1, iv % num_metar
         call da_random_omb( iv % metar(n) % u % error, ob % metar(n) % u, &
                           iv % metar(n) % u % qc, iv % metar(n) % u % inv )
         call da_random_omb( iv % metar(n) % v % error, ob % metar(n) % v, &
                           iv % metar(n) % v % qc, iv % metar(n) % v % inv )
         call da_random_omb( iv % metar(n) % t % error, ob % metar(n) % t, &
                           iv % metar(n) % t % qc, iv % metar(n) % t % inv )
         call da_random_omb( iv % metar(n) % p % error, ob % metar(n) % p, &
                           iv % metar(n) % p % qc, iv % metar(n) % p % inv )
         call da_random_omb( iv % metar(n) % q % error, ob % metar(n) % q, &
                           iv % metar(n) % q % qc, iv % metar(n) % q % inv )
      END DO
   ENDIF

!  [2.3] Transfer ships obs:

   IF ( iv % num_ships > 0 ) THEN
      DO n = 1, iv % num_ships
         call da_random_omb( iv % ships(n) % u % error, ob % ships(n) % u, &
                           iv % ships(n) % u % qc, iv % ships(n) % u % inv )
         call da_random_omb( iv % ships(n) % v % error, ob % ships(n) % v, &
                           iv % ships(n) % v % qc, iv % ships(n) % v % inv )
         call da_random_omb( iv % ships(n) % t % error, ob % ships(n) % t, &
                           iv % ships(n) % t % qc, iv % ships(n) % t % inv )
         call da_random_omb( iv % ships(n) % p % error, ob % ships(n) % p, &
                           iv % ships(n) % p % qc, iv % ships(n) % p % inv )
         call da_random_omb( iv % ships(n) % q % error, ob % ships(n) % q, &
                           iv % ships(n) % q % qc, iv % ships(n) % q % inv )
      END DO
   ENDIF


!  [2.4.1] Transfer Geo. AMVs Obs:

   IF ( iv % num_geoamv > 0 ) THEN
      DO n = 1, iv % num_geoamv 
        DO k = 1, iv % geoamv(n) % info % levels
         call da_random_omb( iv % geoamv(n) % u(k) % error, ob % geoamv(n) % u(k), &
                           iv % geoamv(n) % u(k) % qc, iv % geoamv(n) % u(k) % inv )
         call da_random_omb( iv % geoamv(n) % v(k) % error, ob % geoamv(n) % v(k), &
                           iv % geoamv(n) % v(k) % qc, iv % geoamv(n) % v(k) % inv )
        END DO
      END DO 
   ENDIF 

!  [2.4.2] Transfer Polar  AMVs Obs:

   IF ( iv % num_polaramv > 0 ) THEN
      DO n = 1, iv % num_polaramv
        DO k = 1, iv % polaramv(n) % info % levels
         call da_random_omb( iv % polaramv(n) % u(k) % error, ob % polaramv(n) % u(k), &
                           iv % polaramv(n) % u(k) % qc, iv % polaramv(n) % u(k) % inv )
         call da_random_omb( iv % polaramv(n) % v(k) % error, ob % polaramv(n) % v(k), &
                           iv % polaramv(n) % v(k) % qc, iv % polaramv(n) % v(k) % inv )
        END DO
      END DO
   ENDIF

!  [2.5] Transfer gpspw obs:

   IF ( iv % num_gpspw > 0 ) THEN
      DO n = 1, iv % num_gpspw
         call da_random_omb( iv % gpspw(n) % tpw % error, ob % gpspw(n) % tpw, &
                           iv % gpspw(n) % tpw % qc, iv % gpspw(n) % tpw % inv )
      END DO
   ENDIF

!  [2.6] Transfer sonde obs:

   IF ( iv % num_sound > 0 ) THEN
      DO n = 1, iv % num_sound
         DO k = 1, iv % sound(n) % info % levels
            call da_random_omb( iv % sound(n) % u(k) % error, ob % sound(n) % u(k), &
                              iv % sound(n) % u(k) % qc, iv % sound(n) % u(k) % inv )
            call da_random_omb( iv % sound(n) % v(k) % error, ob % sound(n) % v(k), &
                              iv % sound(n) % v(k) % qc, iv % sound(n) % v(k) % inv )
            call da_random_omb( iv % sound(n) % t(k) % error, ob % sound(n) % t(k), &
                              iv % sound(n) % t(k) % qc, iv % sound(n) % t(k) % inv )
            call da_random_omb( iv % sound(n) % q(k) % error, ob % sound(n) % q(k), &
                              iv % sound(n) % q(k) % qc, iv % sound(n) % q(k) % inv )
         END DO

         call da_random_omb( iv % sonde_sfc(n) % u % error, ob % sonde_sfc(n) % u, &
                             iv % sonde_sfc(n) % u % qc, iv % sonde_sfc(n) % u % inv )
         call da_random_omb( iv % sonde_sfc(n) % v % error, ob % sonde_sfc(n) % v, &
                             iv % sonde_sfc(n) % v % qc, iv % sonde_sfc(n) % v % inv )
         call da_random_omb( iv % sonde_sfc(n) % t % error, ob % sonde_sfc(n) % t, &
                             iv % sonde_sfc(n) % t % qc, iv % sonde_sfc(n) % t % inv  )
         call da_random_omb( iv % sonde_sfc(n) % p % error, ob % sonde_sfc(n) % p, &
                             iv % sonde_sfc(n) % p % qc, iv % sonde_sfc(n) % p % inv )
         call da_random_omb( iv % sonde_sfc(n) % q % error, ob % sonde_sfc(n) % q, &
                             iv % sonde_sfc(n) % q % qc, iv % sonde_sfc(n) % q % inv )
      END DO
   ENDIF

!  [2.7] Transfer airep obs:

   IF ( iv % num_airep > 0 ) THEN
      DO n = 1, iv % num_airep
         DO k = 1, iv % airep(n) % info % levels
            call da_random_omb( iv % airep(n) % u(k) % error, ob % airep(n) % u(k), &
                              iv % airep(n) % u(k) % qc, iv % airep(n) % u(k) % inv )
            call da_random_omb( iv % airep(n) % v(k) % error, ob % airep(n) % v(k), &
                              iv % airep(n) % v(k) % qc, iv % airep(n) % v(k) % inv )
            call da_random_omb( iv % airep(n) % t(k) % error, ob % airep(n) % t(k), &
                              iv % airep(n) % t(k) % qc, iv % airep(n) % t(k) % inv )
         END DO
      END DO
   ENDIF

!  [2.8] Transfer pilot obs:

   IF ( iv % num_pilot > 0 ) THEN
      DO n = 1, iv % num_pilot
         DO k = 1, iv % pilot(n) % info % levels
            call da_random_omb( iv % pilot(n) % u(k) % error, ob % pilot(n) % u(k), &
                              iv % pilot(n) % u(k) % qc, iv % pilot(n) % u(k) % inv )
            call da_random_omb( iv % pilot(n) % v(k) % error, ob % pilot(n) % v(k), &
                              iv % pilot(n) % v(k) % qc, iv % pilot(n) % v(k) % inv )
         END DO
      END DO
   ENDIF

!  [2.9] Transfer SSM/I obs:SSMI:

   IF ( iv % num_ssmi_retrieval > 0 ) THEN
      DO n = 1, iv % num_ssmi_retrieval
         call da_random_omb( iv % ssmi_retrieval(n) % speed % error, &
                           ob % ssmi_retrieval(n) % speed, &
                           iv % ssmi_retrieval(n) % speed % qc, &
                           iv % ssmi_retrieval(n) % speed % inv )
         call da_random_omb( iv % ssmi_retrieval(n) % tpw % error, &
                           ob % ssmi_retrieval(n) % tpw, &
                           iv % ssmi_retrieval(n) % tpw % qc, &
                           iv % ssmi_retrieval(n) % tpw % inv )
      END DO
   ENDIF

   IF ( iv % num_ssmi_tb > 0 ) THEN
      DO n = 1, iv % num_ssmi_tb
         call da_random_omb( iv % ssmi_tb(n) % tb19h % error, &
                           ob % ssmi_tb(n) % tb19h, &
                           iv % ssmi_tb(n) % tb19h % qc, &
                           iv % ssmi_tb(n) % tb19h % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb19v % error, &
                           ob % ssmi_tb(n) % tb19v, &
                           iv % ssmi_tb(n) % tb19v % qc, &
                           iv % ssmi_tb(n) % tb19v % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb22v % error, &
                           ob % ssmi_tb(n) % tb22v, &
                           iv % ssmi_tb(n) % tb22v % qc, &
                           iv % ssmi_tb(n) % tb22v % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb37h % error, &
                           ob % ssmi_tb(n) % tb37h, &
                           iv % ssmi_tb(n) % tb37h % qc, &
                           iv % ssmi_tb(n) % tb37h % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb37v % error, &
                           ob % ssmi_tb(n) % tb37v, &
                           iv % ssmi_tb(n) % tb37v % qc, &
                           iv % ssmi_tb(n) % tb37v % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb85h % error, &
                           ob % ssmi_tb(n) % tb85h, &
                           iv % ssmi_tb(n) % tb85h % qc, &
                           iv % ssmi_tb(n) % tb85h % inv )
         call da_random_omb( iv % ssmi_tb(n) % tb85v % error, &
                           ob % ssmi_tb(n) % tb85v, &
                           iv % ssmi_tb(n) % tb85v % qc, &
                           iv % ssmi_tb(n) % tb85v % inv )
      END DO
   ENDIF

!  [2.10] Transfer satem obs:

   IF ( iv % num_satem > 0 ) THEN
      DO n = 1, iv % num_satem
         DO k = 1, iv % satem(n) % info % levels
            call da_random_omb( iv % satem(n) % thickness(k) % error, &
                              ob % satem(n) % thickness(k), &
                              iv % satem(n) % thickness(k) % qc, &
                              iv % satem(n) % thickness(k) % inv )
         END DO
      END DO
   ENDIF
   
!  [2.11] Transfer ssmt1 obs:

   IF ( iv % num_ssmt1 > 0 ) THEN
      DO n = 1, iv % num_ssmt1
         DO k = 1, iv % ssmt1(n) % info % levels
            call da_random_omb( iv % ssmt1(n) % t(k) % error, &
                              ob % ssmt1(n) % t(k), &
                              iv % ssmt1(n) % t(k) % qc, &
                              iv % ssmt1(n) % t(k) % inv )
         END DO
      END DO
   ENDIF

!  [2.12] Transfer ssmt2 obs:

   IF ( iv % num_ssmt2 > 0 ) THEN
      DO n = 1, iv % num_ssmt2
         DO k = 1, iv % ssmt2(n) % info % levels
            call da_random_omb( iv % ssmt2(n) % rh(k) % error, &
                              ob % ssmt2(n) % rh(k), &
                              iv % ssmt2(n) % rh(k) % qc, &
                              iv % ssmt2(n) % rh(k) % inv )
         END DO
      END DO
   ENDIF
   
!  [2.13] Transfer scatterometer obs:

   IF ( iv % num_qscat > 0 ) THEN
      DO n = 1, iv % num_qscat
         call da_random_omb( iv % qscat(n) % u % error, ob % qscat(n) % u, &
                           iv % qscat(n) % u % qc, iv % qscat(n) % u % inv )
         call da_random_omb( iv % qscat(n) % v % error, ob % qscat(n) % v, &
                           iv % qscat(n) % v % qc, iv % qscat(n) % v % inv )
      END DO
   ENDIF

!  [2.14] Transfer buoy obs:

   IF ( iv % num_buoy > 0 ) THEN
      DO n = 1, iv % num_buoy
         call da_random_omb( iv % buoy(n) % u % error, ob % buoy(n) % u, &
                           iv % buoy(n) % u % qc, iv % buoy(n) % u % inv )
         call da_random_omb( iv % buoy(n) % v % error, ob % buoy(n) % v, &
                           iv % buoy(n) % v % qc, iv % buoy(n) % v % inv )
         call da_random_omb( iv % buoy(n) % t % error, ob % buoy(n) % t, &
                           iv % buoy(n) % t % qc, iv % buoy(n) % t % inv )
         call da_random_omb( iv % buoy(n) % p % error, ob % buoy(n) % p, &
                           iv % buoy(n) % p % qc, iv % buoy(n) % p % inv )
         call da_random_omb( iv % buoy(n) % q % error, ob % buoy(n) % q, &
                           iv % buoy(n) % q % qc, iv % buoy(n) % q % inv )
      END DO
   ENDIF

!  [2.15] Transfer profiler obs:

   IF ( iv % num_profiler > 0 ) THEN
      DO n = 1, iv % num_profiler
         DO k = 1, iv % profiler(n) % info % levels
            call da_random_omb( iv % profiler(n) % u(k) % error, ob % profiler(n) % u(k), &
                              iv % profiler(n) % u(k) % qc, iv % profiler(n) % u(k) % inv )
            call da_random_omb( iv % profiler(n) % v(k) % error, ob % profiler(n) % v(k), &
                              iv % profiler(n) % v(k) % qc, iv % profiler(n) % v(k) % inv )
         END DO
      END DO
   ENDIF

!  [2.16] Transfer TC bogus obs:

   IF ( iv % num_bogus > 0 ) THEN
      DO n = 1, iv % num_bogus
         DO k = 1, iv % bogus(n) % info % levels
            call da_random_omb( iv % bogus(n) % u(k) % error, ob % bogus(n) % u(k), &
                              iv % bogus(n) % u(k) % qc, iv % bogus(n) % u(k) % inv )
            call da_random_omb( iv % bogus(n) % v(k) % error, ob % bogus(n) % v(k), &
                              iv % bogus(n) % v(k) % qc, iv % bogus(n) % v(k) % inv )
            call da_random_omb( iv % bogus(n) % t(k) % error, ob % bogus(n) % t(k), &
                              iv % bogus(n) % t(k) % qc, iv % bogus(n) % t(k) % inv )
            call da_random_omb( iv % bogus(n) % q(k) % error, ob % bogus(n) % q(k), &
                              iv % bogus(n) % q(k) % qc, iv % bogus(n) % q(k) % inv )
         END DO

         call da_random_omb( iv % bogus(n) % slp % error, ob % bogus(n) % slp, &
                             iv % bogus(n) % slp % qc, iv % bogus(n) % slp % inv )
      END DO
   ENDIF

END SUBROUTINE da_random_omb_all
