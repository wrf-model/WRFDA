subroutine da_zero_y( iv, y )

   !---------------------------------------------------------------------------
   ! PURPOSE: Initialises the Y-array
   !---------------------------------------------------------------------------

   implicit none
   
   type (ob_type), intent(in)            :: iv      ! Ob type input.
   type (y_type), intent(inout)          :: y       ! Residual type structure.

   integer                               :: n       ! Loop counter.
   integer                               :: nlevels ! Number of levels.

   if (trace_use) call da_trace_entry("da_zero_y")

   !---------------------------------------------------------------------------
   ! [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

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
   y % num_airsr    = iv % num_airsr   

   !---------------------------------------------------------------------------
   ! [2.0] Allocate:
   !---------------------------------------------------------------------------

   ! Initialize synops:

   if ( y % num_synop > 0 ) then
      y % synop(1:y % num_synop) % u = 0.0
      y % synop(1:y % num_synop) % v = 0.0
      y % synop(1:y % num_synop) % t = 0.0
      y % synop(1:y % num_synop) % p = 0.0
      y % synop(1:y % num_synop) % q = 0.0
   end if

   ! Initialize ships:

   if ( y % num_ships > 0 ) then
      y % ships(1:y % num_ships) % u = 0.0
      y % ships(1:y % num_ships) % v = 0.0
      y % ships(1:y % num_ships) % t = 0.0
      y % ships(1:y % num_ships) % p = 0.0
      y % ships(1:y % num_ships) % q = 0.0
   end if

   ! Initialize metars:

   if ( y % num_metar > 0 ) then
      y % metar(1:y % num_metar) % u = 0.0
      y % metar(1:y % num_metar) % v = 0.0
      y % metar(1:y % num_metar) % t = 0.0
      y % metar(1:y % num_metar) % p = 0.0
      y % metar(1:y % num_metar) % q = 0.0
   end if

   ! Initialize Geo. AMV's:

   if ( y % num_geoamv > 0 ) then
      do n = 1, y % num_geoamv
       nlevels = iv % geoamv(n) % info % levels
       y % geoamv(n) % u(1:nlevels) = 0.0
       y % geoamv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize Polat AMVs:

   if ( y % num_polaramv > 0 ) then
      do n = 1, y % num_polaramv
       nlevels = iv % polaramv(n) % info % levels
       y % polaramv(n) % u(1:nlevels) = 0.0
       y % polaramv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize GPS TPW:

   if ( y % num_gpspw > 0 ) then
      y % gpspw(1:y % num_gpspw) % tpw = 0.0
   end if

   ! Initialize GPS REFRACTIVITY:

   if ( y % num_gpsref > 0 ) then
      do n = 1, y % num_gpsref
         nlevels = iv % gpsref(n) % info % levels
         y % gpsref(n) % ref(1:nlevels) = 0.0
         y % gpsref(n) %   p(1:nlevels) = 0.0
         y % gpsref(n) %   t(1:nlevels) = 0.0
         y % gpsref(n) %   q(1:nlevels) = 0.0
      end do
   end if

   ! Initialize sondes:

   if ( y % num_sound > 0 ) then
      do n = 1, y % num_sound
         nlevels = iv % sound(n) % info % levels

         y % sound(n) % u(1:nlevels) = 0.0
         y % sound(n) % v(1:nlevels) = 0.0
         y % sound(n) % t(1:nlevels) = 0.0
         y % sound(n) % q(1:nlevels) = 0.0

         ! Initialize sonde_sfc

         y % sonde_sfc(n) % u = 0.0
         y % sonde_sfc(n) % v = 0.0
         y % sonde_sfc(n) % t = 0.0
         y % sonde_sfc(n) % p = 0.0
         y % sonde_sfc(n) % q = 0.0
      end do
   end if

   if ( y % num_bogus > 0 ) then
      do n = 1, y % num_bogus
         nlevels = iv % bogus(n) % info % levels

         y % bogus(n) % u(1:nlevels) = 0.0
         y % bogus(n) % v(1:nlevels) = 0.0
         y % bogus(n) % t(1:nlevels) = 0.0
         y % bogus(n) % q(1:nlevels) = 0.0
         y % bogus(n) % slp          = 0.0
      end do
   end if

   ! Initialize pilots:

   if ( y % num_pilot > 0 ) then
      do n = 1, y % num_pilot
         nlevels = iv % pilot(n) % info % levels

         y % pilot(n) % u(1:nlevels) = 0.0
         y % pilot(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize AIREPs:

   if ( y % num_airep > 0 ) then
      do n = 1, y % num_airep
         nlevels = iv % airep(n) % info % levels

         y % airep(n) % u(1:nlevels) = 0.0
         y % airep(n) % v(1:nlevels) = 0.0
         y % airep(n) % t(1:nlevels) = 0.0
      end do
   end if

   ! Initialize satem:

   if ( y % num_satem > 0 ) then
      do n = 1, y % num_satem
         nlevels = iv % satem(n) % info % levels

         y % satem(n) % thickness(1:nlevels) = 0.0
      end do
   end if

   if ( y % num_ssmi_tb > 0 ) then
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb22v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85h = 0.0
   end if

   if ( y % num_ssmi_retrieval > 0 ) then
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % tpw = 0.0
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % Speed = 0.0
   end if
   
   if ( y % num_ssmt1 > 0 ) then
      do n = 1, y % num_ssmt1
         nlevels = iv % ssmt1(n) % info % levels
         y % ssmt1(n) % t(1:nlevels) = 0.0
      end do
   end if
   
   if ( y % num_ssmt2 > 0 ) then
      do n = 1, y % num_ssmt2
         nlevels = iv % ssmt2(n) % info % levels
         y % ssmt2(n) % rh(1:nlevels) = 0.0
      end do
   end if
   
   if ( y % num_pseudo > 0 ) then
        y % pseudo(1:y % num_pseudo) % u = 0.0
        y % pseudo(1:y % num_pseudo) % v = 0.0
        y % pseudo(1:y % num_pseudo) % t = 0.0
        y % pseudo(1:y % num_pseudo) % p = 0.0
        y % pseudo(1:y % num_pseudo) % q = 0.0
   end if

   !  Initialize Quikscat:

   if ( y % num_qscat > 0 ) then
      y % qscat(1:y % num_qscat) % u = 0.0
      y % qscat(1:y % num_qscat) % v = 0.0
   end if
      
   ! Initialize profilers:

   if ( y % num_profiler > 0 ) then
      do n = 1, y % num_profiler
         nlevels = iv % profiler(n) % info % levels

         y % profiler(n) % u(1:nlevels) = 0.0
         y % profiler(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize buoy:

   if ( y % num_buoy > 0 ) then
      y % buoy(1:y % num_buoy) % u = 0.0
      y % buoy(1:y % num_buoy) % v = 0.0
      y % buoy(1:y % num_buoy) % t = 0.0
      y % buoy(1:y % num_buoy) % p = 0.0
      y % buoy(1:y % num_buoy) % q = 0.0
   end if

   ! Initialize buoy:
   if ( y % num_radar > 0 ) then
      do n = 1, y % num_radar
         nlevels = iv % radar(n) % info % levels

         y % radar(n) % rv(1:nlevels) = 0.0
         y % radar(n) % rf(1:nlevels) = 0.0
      end do
   end if
   ! Initialize AIRS retrievals:

   if ( y % num_airsr > 0 ) then
      do n = 1, y % num_airsr
         nlevels = iv % airsr(n) % info % levels

         y % airsr(n) % t(1:nlevels) = 0.0
         y % airsr(n) % q(1:nlevels) = 0.0
      end do
   end if


   if (trace_use) call da_trace_exit("da_zero_y")

end subroutine da_zero_y        


