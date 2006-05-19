SUBROUTINE da_use_obs_errfac( iv )
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

   TYPE (ob_type), INTENT(INOUT) :: iv              ! Obs and header structure.

   integer                       :: n, k            ! Loop counters.
   real                          :: d1, d2, d3, d4  ! Dummy values.

!----------------------------------------------------------------------
!  [2.0] Scale observation errors:
!----------------------------------------------------------------------

!  [2.1] Transfer surface obs:

   IF ( iv % num_synop > 0 ) THEN
   
      call da_read_errfac( 'synop', iv % synop_ef_u, &
                           iv % synop_ef_v, iv % synop_ef_t, &
                           iv % synop_ef_p, iv % synop_ef_q )                           

      DO n = 1, iv % num_synop
         iv % synop(n) % u % error = iv % synop(n) % u % error * iv % synop_ef_u
         iv % synop(n) % v % error = iv % synop(n) % v % error * iv % synop_ef_v
         iv % synop(n) % t % error = iv % synop(n) % t % error * iv % synop_ef_t
         iv % synop(n) % p % error = iv % synop(n) % p % error * iv % synop_ef_p
         iv % synop(n) % q % error = iv % synop(n) % q % error * iv % synop_ef_q
      END DO
   ENDIF

!  [2.2] Transfer metar obs:

   IF ( iv % num_metar > 0 ) THEN

      call da_read_errfac( 'metar', iv % metar_ef_u, &
                           iv % metar_ef_v, iv % metar_ef_t, &
                           iv % metar_ef_p, iv % metar_ef_q )
                           
      DO n = 1, iv % num_metar
         iv % metar(n) % u % error = iv % metar(n) % u % error * iv % metar_ef_u
         iv % metar(n) % v % error = iv % metar(n) % v % error * iv % metar_ef_v
         iv % metar(n) % t % error = iv % metar(n) % t % error * iv % metar_ef_t
         iv % metar(n) % p % error = iv % metar(n) % p % error * iv % metar_ef_p
         iv % metar(n) % q % error = iv % metar(n) % q % error * iv % metar_ef_q
      END DO
   ENDIF

!  [2.2] Transfer ships obs:

   IF ( iv % num_ships > 0 ) THEN
      
      call da_read_errfac( 'ships', iv % ships_ef_u, &
                           iv % ships_ef_v, iv % ships_ef_t, &
                           iv % ships_ef_p, iv % ships_ef_q )
                           
      DO n = 1, iv % num_ships
         iv % ships(n) % u % error = iv % ships(n) % u % error * iv % ships_ef_u
         iv % ships(n) % v % error = iv % ships(n) % v % error * iv % ships_ef_v
         iv % ships(n) % t % error = iv % ships(n) % t % error * iv % ships_ef_t
         iv % ships(n) % p % error = iv % ships(n) % p % error * iv % ships_ef_p
         iv % ships(n) % q % error = iv % ships(n) % q % error * iv % ships_ef_q
      END DO
   ENDIF

!  [2.4.1] Transfer Geo. AMVs Obs:
   
   IF ( iv % num_geoamv > 0 ) THEN
   
      call da_read_errfac( 'geoamv', iv % geoamv_ef_u, iv % geoamv_ef_v, d1, d2, d3 )

      DO n = 1, iv % num_geoamv
        DO k = 1, iv % geoamv(n) % info % levels
         iv % geoamv(n) % u(k) % error = iv % geoamv(n) % u(k) % error * iv % geoamv_ef_u
         iv % geoamv(n) % v(k) % error = iv % geoamv(n) % v(k) % error * iv % geoamv_ef_v
        END DO
      END DO 
   ENDIF

!  [2.4.2] Transfer Polar AMVs Obs:

   IF ( iv % num_polaramv > 0 ) THEN 

      call da_read_errfac( 'polaramv', iv % polaramv_ef_u, iv % polaramv_ef_v, d1, d2, d3 )

      DO n = 1, iv % num_polaramv
        DO k = 1, iv % polaramv(n) % info % levels
         iv % polaramv(n) % u(k) % error = iv % polaramv(n) % u(k) % error * iv % polaramv_ef_u
         iv % polaramv(n) % v(k) % error = iv % polaramv(n) % v(k) % error * iv % polaramv_ef_v
        END DO
      END DO
   ENDIF


!  [2.5] Transfer gpspw obs:

   IF ( iv % num_gpspw > 0 ) THEN

      call da_read_errfac( 'gpspw', iv % gpspw_ef_tpw, d1, d2, d3, d4 )

      DO n = 1, iv % num_gpspw
         iv % gpspw(n) % tpw % error = iv % gpspw(n) % tpw % error * &
                                       iv % gpspw_ef_tpw

      END DO
   ENDIF

!  [2.6] Transfer sonde obs:

   IF ( iv % num_sound > 0 ) THEN

      call da_read_errfac( 'sound', iv % sound_ef_u, iv % sound_ef_v, &
                           iv % sound_ef_t, iv % sound_ef_p, iv % sound_ef_q )

      DO n = 1, iv % num_sound
         DO k = 1, iv % sound(n) % info % levels
            iv % sound(n) % u(k) % error = iv % sound(n) % u(k) % error * &
                                           iv % sound_ef_u
            iv % sound(n) % v(k) % error = iv % sound(n) % v(k) % error * &
                                           iv % sound_ef_v
            iv % sound(n) % t(k) % error = iv % sound(n) % t(k) % error * &
                                           iv % sound_ef_t
            iv % sound(n) % q(k) % error = iv % sound(n) % q(k) % error * &
                                           iv % sound_ef_q
         END DO

         iv % sonde_sfc(n) % u % error = iv % sonde_sfc(n) % u % error * iv % synop_ef_u
         iv % sonde_sfc(n) % v % error = iv % sonde_sfc(n) % v % error * iv % synop_ef_v
         iv % sonde_sfc(n) % t % error = iv % sonde_sfc(n) % t % error * iv % synop_ef_t
         iv % sonde_sfc(n) % p % error = iv % sonde_sfc(n) % p % error * iv % synop_ef_p
         iv % sonde_sfc(n) % q % error = iv % sonde_sfc(n) % q % error * iv % synop_ef_q
      END DO
   ENDIF

!  [2.7] Transfer airep obs:

   IF ( iv % num_airep > 0 ) THEN
   
      call da_read_errfac( 'airep', iv % airep_ef_u, iv % airep_ef_v, &
                           iv % airep_ef_t, d1, d2 )

      DO n = 1, iv % num_airep
         DO k = 1, iv % airep(n) % info % levels
            iv % airep(n) % u(k) % error = iv % airep(n) % u(k) % error * &
                                           iv % airep_ef_u
            iv % airep(n) % v(k) % error = iv % airep(n) % v(k) % error * &
                                           iv % airep_ef_v
            iv % airep(n) % t(k) % error = iv % airep(n) % t(k) % error * &
                                           iv % airep_ef_t
         END DO
      END DO
   ENDIF

!  [2.8] Transfer pilot obs:

   IF ( iv % num_pilot > 0 ) THEN

      call da_read_errfac( 'pilot', iv % pilot_ef_u, iv % pilot_ef_v, d1, d2, d3 )

      DO n = 1, iv % num_pilot
         DO k = 1, iv % pilot(n) % info % levels
            iv % pilot(n) % u(k) % error = iv % pilot(n) % u(k) % error * &
                                           iv % pilot_ef_u
            iv % pilot(n) % v(k) % error = iv % pilot(n) % v(k) % error * &
                                           iv % pilot_ef_v

         END DO
      END DO
   ENDIF

!  [2.9] Transfer SSM/I obs:SSMI:

   IF ( iv % num_ssmi_retrieval > 0 ) THEN

      call da_read_errfac( 'ssmir', iv % ssmir_ef_speed, iv % ssmir_ef_tpw, d1, d2, d3 )

      DO n = 1, iv % num_ssmi_retrieval
         iv%ssmi_retrieval(n)%tpw%error = iv%ssmi_retrieval(n)%tpw%error * &
                                          iv % ssmir_ef_tpw
         iv%ssmi_retrieval(n)%speed%error = iv%ssmi_retrieval(n)%speed%error * &
                                            iv % ssmir_ef_speed
      END DO
   ENDIF

   IF ( iv % num_ssmi_tb > 0 ) THEN

!     iv % ssmit_ef_tb19h = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb19v = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb22v = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb37h = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb37v = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb85h = 1.0 ! Tuning not yet coded.
!     iv % ssmit_ef_tb85v = 1.0 ! Tuning not yet coded.

      DO n = 1, iv % num_ssmi_tb
!        iv%ssmi_tb(n)%tb19h%error = iv%ssmi_tb(n)%tb19h%error
!        iv%ssmi_tb(n)%tb19v%error = iv%ssmi_tb(n)%tb19v%error
!        iv%ssmi_tb(n)%tb22v%error = iv%ssmi_tb(n)%tb22v%error
!        iv%ssmi_tb(n)%tb37h%error = iv%ssmi_tb(n)%tb37h%error * &
!                                    fac_ssmit_tb37h
!        iv%ssmi_tb(n)%tb37v%error = iv%ssmi_tb(n)%tb37v%error * &
!                                    fac_ssmit_tb37v
!        iv%ssmi_tb(n)%tb85h%error = iv%ssmi_tb(n)%tb85h%error * &
!                                    fac_ssmit_tb85h
!        iv%ssmi_tb(n)%tb85v%error = iv%ssmi_tb(n)%tb85v%error * &
!                                    fac_ssmit_tb85v
      END DO
   ENDIF

!  [2.10] Transfer satem obs:

   IF ( iv % num_satem > 0 ) THEN
   
      call da_read_errfac( 'satem', iv % satem_ef_thickness, d1, d2, d3, d4 )

      DO n = 1, iv % num_satem
         DO k = 1, iv % satem(n) % info % levels
            iv % satem(n) % thickness(k) % error = iv % satem(n) % thickness(k) % error*&
                                                   iv % satem_ef_thickness
         END DO
      END DO
   ENDIF
   
!  [2.11] Transfer ssmt1 obs:

   IF ( iv % num_ssmt1 > 0 ) THEN

      call da_read_errfac( 'ssmt1', iv % ssmt1_ef_t, d1, d2, d3, d4 )
      
      DO n = 1, iv % num_ssmt1
         DO k = 1, iv % ssmt1(n) % info % levels   
            iv % ssmt1(n) % t(k) % error = iv % ssmt1(n) % t(k) % error * &
                                        iv % ssmt1_ef_t
         END DO
      END DO
   ENDIF

!  [2.12] Transfer ssmt2 obs:

   IF ( iv % num_ssmt2 > 0 ) THEN

      call da_read_errfac( 'ssmt2', iv % ssmt2_ef_rh, d1, d2, d3, d4 )

      DO n = 1, iv % num_ssmt2
         DO k = 1, iv % ssmt2(n) % info % levels      
            iv % ssmt2(n) % rh(k) % error = iv % ssmt2(n) % rh(k) % error * &
                                         iv % ssmt2_ef_rh
         END DO
      END DO
   ENDIF
   
!  [2.13] Transfer scatterometer obs:

   IF ( iv % num_qscat > 0 ) THEN
      
      call da_read_errfac( 'qscat', iv % qscat_ef_u, &
                           iv % qscat_ef_v, d1, d2, d3 )
                           
      DO n = 1, iv % num_qscat
         iv % qscat(n) % u % error = iv % qscat(n) % u % error * iv % qscat_ef_u
         iv % qscat(n) % v % error = iv % qscat(n) % v % error * iv % qscat_ef_v
      END DO
   ENDIF

!  [2.14] Transfer profiler obs:

   IF ( iv % num_profiler > 0 ) THEN

      call da_read_errfac( 'profi', iv % profiler_ef_u, iv % profiler_ef_v, d1, d2, d3 )

      DO n = 1, iv % num_profiler
         DO k = 1, iv % profiler(n) % info % levels
            iv % profiler(n) % u(k) % error = iv % profiler(n) % u(k) % error * &
                                           iv % profiler_ef_u
            iv % profiler(n) % v(k) % error = iv % profiler(n) % v(k) % error * &
                                           iv % profiler_ef_v

         END DO
      END DO
   ENDIF
!
!  [2.15] Transfer buoy obs:

   IF ( iv % num_buoy > 0 ) THEN
      
      call da_read_errfac( 'buoy ', iv % buoy_ef_u, &
                           iv % buoy_ef_v, iv % buoy_ef_t, &
                           iv % buoy_ef_p, iv % buoy_ef_q )
                           
      DO n = 1, iv % num_buoy
         iv % buoy(n) % u % error = iv % buoy(n) % u % error * iv % buoy_ef_u
         iv % buoy(n) % v % error = iv % buoy(n) % v % error * iv % buoy_ef_v
         iv % buoy(n) % t % error = iv % buoy(n) % t % error * iv % buoy_ef_t
         iv % buoy(n) % p % error = iv % buoy(n) % p % error * iv % buoy_ef_p
         iv % buoy(n) % q % error = iv % buoy(n) % q % error * iv % buoy_ef_q
      END DO
   ENDIF

!  [2.16] Transfer TC bogus obs:

   IF ( iv % num_bogus > 0 ) THEN

      call da_read_errfac( 'bogus', iv % bogus_ef_u, iv % bogus_ef_v, &
                           iv % bogus_ef_t, iv % bogus_ef_q, iv % bogus_ef_slp )

      DO n = 1, iv % num_bogus
         DO k = 1, iv % bogus(n) % info % levels
            iv % bogus(n) % u(k) % error = iv % bogus(n) % u(k) % error * &
                                           iv % bogus_ef_u
            iv % bogus(n) % v(k) % error = iv % bogus(n) % v(k) % error * &
                                           iv % bogus_ef_v
            iv % bogus(n) % t(k) % error = iv % bogus(n) % t(k) % error * &
                                           iv % bogus_ef_t
            iv % bogus(n) % q(k) % error = iv % bogus(n) % q(k) % error * &
                                           iv % bogus_ef_q

         END DO

         iv % bogus(n) % slp % error = iv % bogus(n) % slp % error * iv % bogus_ef_slp
      END DO
   END IF

END SUBROUTINE da_use_obs_errfac

