SUBROUTINE DA_Zero_Y( iv, y )

!------------------------------------------------------------------------------
! PURPOSE: Initialises the Y-array
!
!  HISTORY:
!
!           07/28/2003 - Creation                           S. R. H. Rizvi
!           05/27/2004 - GPS Refractivity added.            Y.-R. Guo
!           03/04/2005 - AMV's from Geostationary and       Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------
   IMPLICIT NONE
   
   TYPE (ob_type), INTENT(IN)            :: iv      ! Ob type input.
   TYPE (y_type), INTENT(INOUT)          :: y       ! Residual type structure.

   INTEGER                               :: n       ! Loop counter.
   INTEGER                               :: nlevels ! Number of levels.

!------------------------------------------------------------------------------
!  [1.0] Copy number of observations:
!------------------------------------------------------------------------------

   y % total_obs = iv % total_obs
   y % num_synop = iv % num_synop
   y % num_ships = iv % num_ships
   y % num_metar = iv % num_metar
   y % num_sound = iv % num_sound
   y % num_pilot = iv % num_pilot
   y % num_airep = iv % num_airep
   y % num_satem = iv % num_satem
   y % num_geoamv = iv % num_geoamv
   y % num_polaramv = iv % num_polaramv
   y % num_gpspw  = iv % num_gpspw
   y % num_gpsref = iv % num_gpsref
   y % num_ssmi_tb = iv % num_ssmi_tb
   y % num_ssmi_retrieval = iv % num_ssmi_retrieval
   y % num_ssmt1 = iv % num_ssmt1
   y % num_ssmt2 = iv % num_ssmt2
   y % num_pseudo = num_pseudo
   y % num_qscat = iv % num_qscat
   y % num_profiler = iv % num_profiler
   y % num_buoy     = iv % num_buoy    
   y % num_radar    = iv % num_radar   
   y % num_bogus    = iv % num_bogus   

!------------------------------------------------------------------------------
!  [2.0] Allocate:
!------------------------------------------------------------------------------

!  Initialize synops:

   IF ( y % num_synop > 0 ) THEN
      y % synop(1:y % num_synop) % u = 0.0
      y % synop(1:y % num_synop) % v = 0.0
      y % synop(1:y % num_synop) % t = 0.0
      y % synop(1:y % num_synop) % p = 0.0
      y % synop(1:y % num_synop) % q = 0.0
   END IF

!  Initialize ships:

   IF ( y % num_ships > 0 ) THEN
      y % ships(1:y % num_ships) % u = 0.0
      y % ships(1:y % num_ships) % v = 0.0
      y % ships(1:y % num_ships) % t = 0.0
      y % ships(1:y % num_ships) % p = 0.0
      y % ships(1:y % num_ships) % q = 0.0
   END IF

!  Initialize metars:

   IF ( y % num_metar > 0 ) THEN
      y % metar(1:y % num_metar) % u = 0.0
      y % metar(1:y % num_metar) % v = 0.0
      y % metar(1:y % num_metar) % t = 0.0
      y % metar(1:y % num_metar) % p = 0.0
      y % metar(1:y % num_metar) % q = 0.0
   END IF
!  Initialize Geo. AMV's:

   IF ( y % num_geoamv > 0 ) THEN
      do n = 1, y % num_geoamv
       nlevels = iv % geoamv(n) % info % levels
       y % geoamv(n) % u(1:nlevels) = 0.0
       y % geoamv(n) % v(1:nlevels) = 0.0
      end do
   END IF

!  Initialize Polat AMVs:

   IF ( y % num_polaramv > 0 ) THEN
      do n = 1, y % num_polaramv
       nlevels = iv % polaramv(n) % info % levels
       y % polaramv(n) % u(1:nlevels) = 0.0
       y % polaramv(n) % v(1:nlevels) = 0.0
      end do
   END IF

!  Initialize GPS TPW:

   IF ( y % num_gpspw > 0 ) THEN
      y % gpspw(1:y % num_gpspw) % tpw = 0.0
   END IF

!  Initialize GPS REFRACTIVITY:

   IF ( y % num_gpsref > 0 ) THEN
      do n = 1, y % num_gpsref
         nlevels = iv % gpsref(n) % info % levels
         y % gpsref(n) % ref(1:nlevels) = 0.0
         y % gpsref(n) %   p(1:nlevels) = 0.0
         y % gpsref(n) %   t(1:nlevels) = 0.0
         y % gpsref(n) %   q(1:nlevels) = 0.0
      end do
   END IF

!  Initialize sondes:

   IF ( y % num_sound > 0 ) THEN
      do n = 1, y % num_sound

         nlevels = iv % sound(n) % info % levels

         y % sound(n) % u(1:nlevels) = 0.0
         y % sound(n) % v(1:nlevels) = 0.0
         y % sound(n) % t(1:nlevels) = 0.0
         y % sound(n) % q(1:nlevels) = 0.0

!  Initialize sonde_sfc

         y % sonde_sfc(n) % u = 0.0
         y % sonde_sfc(n) % v = 0.0
         y % sonde_sfc(n) % t = 0.0
         y % sonde_sfc(n) % p = 0.0
         y % sonde_sfc(n) % q = 0.0
      end do
   END IF

   IF ( y % num_bogus > 0 ) THEN
      do n = 1, y % num_bogus

         nlevels = iv % bogus(n) % info % levels

         y % bogus(n) % u(1:nlevels) = 0.0
         y % bogus(n) % v(1:nlevels) = 0.0
         y % bogus(n) % t(1:nlevels) = 0.0
         y % bogus(n) % q(1:nlevels) = 0.0
         y % bogus(n) % slp          = 0.0
      end do
   END IF

!  Initialize pilots:

   IF ( y % num_pilot > 0 ) THEN
      do n = 1, y % num_pilot

         nlevels = iv % pilot(n) % info % levels

         y % pilot(n) % u(1:nlevels) = 0.0
         y % pilot(n) % v(1:nlevels) = 0.0

      end do
   END IF

!  Initialize AIREPs:

   IF ( y % num_airep > 0 ) THEN
      do n = 1, y % num_airep

         nlevels = iv % airep(n) % info % levels

         y % airep(n) % u(1:nlevels) = 0.0
         y % airep(n) % v(1:nlevels) = 0.0
         y % airep(n) % t(1:nlevels) = 0.0

      end do
   END IF

!  Initialize satem:

   IF ( y % num_satem > 0 ) THEN
      do n = 1, y % num_satem

         nlevels = iv % satem(n) % info % levels

         y % satem(n) % thickness(1:nlevels) = 0.0

      end do
   END IF

   IF ( y % num_ssmi_tb > 0 ) THEN
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb22v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85h = 0.0
   ENDIF

   IF ( y % num_ssmi_retrieval > 0 ) THEN
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % tpw = 0.0
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % Speed = 0.0
   ENDIF
   
   IF ( y % num_ssmt1 > 0 ) THEN
      do n = 1, y % num_ssmt1
         nlevels = iv % ssmt1(n) % info % levels
         y % ssmt1(n) % t(1:nlevels) = 0.0
      end do
   ENDIF
   
   IF ( y % num_ssmt2 > 0 ) THEN
      do n = 1, y % num_ssmt2
         nlevels = iv % ssmt2(n) % info % levels
         y % ssmt2(n) % rh(1:nlevels) = 0.0
      end do
   ENDIF
   
   IF ( y % num_pseudo > 0 ) THEN
        y % pseudo(1:y % num_pseudo) % u = 0.0
        y % pseudo(1:y % num_pseudo) % v = 0.0
        y % pseudo(1:y % num_pseudo) % t = 0.0
        y % pseudo(1:y % num_pseudo) % p = 0.0
        y % pseudo(1:y % num_pseudo) % q = 0.0
   ENDIF

!  Initialize Quikscat:

   IF ( y % num_qscat > 0 ) THEN
      y % qscat(1:y % num_qscat) % u = 0.0
      y % qscat(1:y % num_qscat) % v = 0.0
   END IF
      
!  Initialize profilers:

   IF ( y % num_profiler > 0 ) THEN
      do n = 1, y % num_profiler

         nlevels = iv % profiler(n) % info % levels

         y % profiler(n) % u(1:nlevels) = 0.0
         y % profiler(n) % v(1:nlevels) = 0.0

      end do
   END IF

!  Initialize buoy:

   IF ( y % num_buoy > 0 ) THEN
      y % buoy(1:y % num_buoy) % u = 0.0
      y % buoy(1:y % num_buoy) % v = 0.0
      y % buoy(1:y % num_buoy) % t = 0.0
      y % buoy(1:y % num_buoy) % p = 0.0
      y % buoy(1:y % num_buoy) % q = 0.0
   END IF

!  Initialize buoy:
   IF ( y % num_radar > 0 ) THEN

      do n = 1, y % num_radar

         nlevels = iv % radar(n) % info % levels

         y % radar(n) % rv(1:nlevels) = 0.0
         y % radar(n) % rf(1:nlevels) = 0.0

      end do
   END IF
END SUBROUTINE DA_Zero_Y        
