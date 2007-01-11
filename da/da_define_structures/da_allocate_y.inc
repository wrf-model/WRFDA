subroutine da_allocate_y (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (ob_type), intent(in)            :: iv      ! Ob type input.
   type (y_type), intent(inout)          :: y       ! Residual type structure.

   integer                               :: n, i    ! Loop counter.
   integer                               :: nlevels ! Number of levels.

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y")

   y % total_obs = iv % total_obs
   y % num_synop = iv % num_synop
   y % num_ships = iv % num_ships
   y % num_metar = iv % num_metar
   y % num_sound = iv % num_sound
   y % num_pilot = iv % num_pilot
   y % num_bogus = iv % num_bogus
   y % num_Radar = iv % num_Radar
   y % num_airep = iv % num_airep
   y % num_satem = iv % num_satem
   y % num_geoamv = iv % num_geoamv
   y % num_polaramv = iv % num_polaramv
   y % num_gpspw  = iv % num_gpspw
   y % num_gpsref = iv % num_gpsref
   y % num_ssmi_tb        = iv % num_ssmi_tb
   y % num_ssmi_retrieval = iv % num_ssmi_retrieval
   y % num_ssmt1 = iv % num_ssmt1
   y % num_ssmt2 = iv % num_ssmt2
   y % num_pseudo = num_pseudo
   y % num_qscat = iv % num_qscat
   y % num_profiler = iv % num_profiler
   y % num_buoy     = iv % num_buoy    
   y % num_airsr    = iv % num_airsr   
   y % num_inst     = iv % num_inst

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   ! Initialize synops:

   if (y % num_synop > 0) then
      allocate (y % synop(1:y % num_synop))
      y % synop(1:y % num_synop) % u = 0.0
      y % synop(1:y % num_synop) % v = 0.0
      y % synop(1:y % num_synop) % t = 0.0
      y % synop(1:y % num_synop) % p = 0.0
      y % synop(1:y % num_synop) % q = 0.0
   end if

   ! Initialize ships:

   if (y % num_ships > 0) then
      allocate (y % ships(1:y % num_ships))
      y % ships(1:y % num_ships) % u = 0.0
      y % ships(1:y % num_ships) % v = 0.0
      y % ships(1:y % num_ships) % t = 0.0
      y % ships(1:y % num_ships) % p = 0.0
      y % ships(1:y % num_ships) % q = 0.0
   end if

   ! Initialize metars:

   if (y % num_metar > 0) then
      allocate (y % metar(1:y % num_metar))
      y % metar(1:y % num_metar) % u = 0.0
      y % metar(1:y % num_metar) % v = 0.0
      y % metar(1:y % num_metar) % t = 0.0
      y % metar(1:y % num_metar) % p = 0.0
      y % metar(1:y % num_metar) % q = 0.0
   end if

   ! Initialize Geo. CMVs Obs:

   if (y % num_geoamv > 0) then
      allocate (y % geoamv(1:y % num_geoamv))
      do n = 1, y % num_geoamv
         nlevels = iv % geoamv(n) % info % levels
         allocate (y % geoamv(n)%u(1:nlevels))
         allocate (y % geoamv(n)%v(1:nlevels))
         y % geoamv(n) % u(1:nlevels) = 0.0
         y % geoamv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize Polar CMVS;

   if (y % num_polaramv > 0) then
      allocate (y % polaramv(1:y % num_polaramv))

      do n = 1, y % num_polaramv
         nlevels = iv % polaramv(n) % info % levels
         allocate (y % polaramv(n)%u(1:nlevels))
         allocate (y % polaramv(n)%v(1:nlevels))
         y % polaramv(n) % u(1:nlevels) = 0.0
         y % polaramv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize GPS TPW:
 
   if (y % num_gpspw > 0) then
      allocate (y % gpspw(1:y % num_gpspw))
      y % gpspw(1:y % num_gpspw) % tpw = 0.0
   end if

   ! Initialize GPS Refractivity:

   if (y % num_gpsref > 0) then
      allocate (y % gpsref(1:y % num_gpsref))
      do n = 1, y % num_gpsref
         nlevels = iv % gpsref(n) % info % levels
         allocate (y % gpsref(n)%ref(1:nlevels))
         allocate (y % gpsref(n)%  p(1:nlevels))
         allocate (y % gpsref(n)%  t(1:nlevels))
         allocate (y % gpsref(n)%  q(1:nlevels))

         y % gpsref(n) % ref(1:nlevels) = 0.0
         y % gpsref(n) %   p(1:nlevels) = 0.0
         y % gpsref(n) %   t(1:nlevels) = 0.0
         y % gpsref(n) %   q(1:nlevels) = 0.0
      end do
   end if

   !  Initialize sondes:

   if (y % num_sound > 0) then
      allocate (y % sound(1:y % num_sound))
      do n = 1, y % num_sound
         nlevels = max(1,iv % sound(n) % info % levels)
         allocate (y % sound(n)%u(1:nlevels))
         allocate (y % sound(n)%v(1:nlevels))
         allocate (y % sound(n)%t(1:nlevels))
         allocate (y % sound(n)%q(1:nlevels))

         y % sound(n) % u(1:nlevels) = 0.0
         y % sound(n) % v(1:nlevels) = 0.0
         y % sound(n) % t(1:nlevels) = 0.0
         y % sound(n) % q(1:nlevels) = 0.0
      end do

      ! Initialize sonde_sfc

      allocate (y % sonde_sfc(1:y % num_sound))

      y % sonde_sfc(1:y % num_sound) % u = 0.0
      y % sonde_sfc(1:y % num_sound) % v = 0.0
      y % sonde_sfc(1:y % num_sound) % t = 0.0
      y % sonde_sfc(1:y % num_sound) % p = 0.0
      y % sonde_sfc(1:y % num_sound) % q = 0.0
   end if

   ! Initialize pilots:

   if (y % num_pilot > 0) then
      allocate (y % pilot(1:y % num_pilot))
      do n = 1, y % num_pilot
         nlevels = iv % pilot(n) % info % levels
         allocate (y % pilot(n)%u(1:nlevels))
         allocate (y % pilot(n)%v(1:nlevels))

         y % pilot(n) % u(1:nlevels) = 0.0
         y % pilot(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize Radar:

   if (y % num_Radar > 0) then
      allocate (y % Radar(1:y % num_Radar))
      do n = 1, y % num_Radar
         nlevels = iv % Radar(n) % info % levels
         allocate (y % Radar(n)%rv(1:nlevels))
         allocate (y % Radar(n)%rf(1:nlevels))

         y % Radar(n) % rv(1:nlevels) = 0.0
         y % Radar(n) % rf(1:nlevels) = 0.0
      end do
   end if

   ! Initialize AIREPs:

   if (y % num_airep > 0) then
      allocate (y % airep(1:y % num_airep))
      do n = 1, y % num_airep
         nlevels = iv % airep(n) % info % levels
         allocate (y % airep(n)%u(1:nlevels))
         allocate (y % airep(n)%v(1:nlevels))
         allocate (y % airep(n)%t(1:nlevels))

         y % airep(n) % u(1:nlevels) = 0.0
         y % airep(n) % v(1:nlevels) = 0.0
         y % airep(n) % t(1:nlevels) = 0.0
      end do
   end if

   ! Initialize Bogus:

   if (y % num_bogus > 0) then
      allocate (y % bogus(1:y % num_bogus))
      do n = 1, y % num_bogus
         nlevels = iv % bogus(n) % info % levels
         allocate (y % bogus(n)%u(1:nlevels))
         allocate (y % bogus(n)%v(1:nlevels))
         allocate (y % bogus(n)%t(1:nlevels))
         allocate (y % bogus(n)%q(1:nlevels))

         y % bogus(n) % u(1:nlevels) = 0.0
         y % bogus(n) % v(1:nlevels) = 0.0
         y % bogus(n) % t(1:nlevels) = 0.0
         y % bogus(n) % q(1:nlevels) = 0.0
      end do

      y % bogus(1:y % num_bogus) % slp = 0.0
   end if

   ! Initialize satem:

   if (y % num_satem > 0) then
      allocate (y % satem(1:y % num_satem))
      do n = 1, y % num_satem
         nlevels = iv % satem(n) % info % levels
         allocate (y % satem(n) % thickness(1:nlevels))

         y % satem(n) % thickness(1:nlevels) = 0.0
      end do
   end if

   if (y % num_ssmi_tb > 0) then
      allocate (y % ssmi_tb(1:y % num_ssmi_tb))
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb19h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb22v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb37h = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85v = 0.0
      y % ssmi_tb(1:y % num_ssmi_tb) % tb85h = 0.0
   end if

   if (y % num_ssmi_retrieval > 0) then
        allocate (y % ssmi_retrieval(1:y % num_ssmi_retrieval))
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % tpw = 0.0
        y % ssmi_retrieval(1:y % num_ssmi_retrieval) % Speed = 0.0
   end if
   
   if (y % num_ssmt1 > 0) then
      allocate (y % ssmt1(1:y % num_ssmt1))
      do n = 1, y % num_ssmt1
         nlevels = iv % ssmt1(n) % info % levels
         allocate (y % ssmt1(n) % t(1:nlevels))
         y % ssmt1(n) % t(1:nlevels) = 0.0
      end do
   end if
   
   if (y % num_ssmt2 > 0) then
      allocate (y % ssmt2(1:y % num_ssmt2))
      do n = 1, y % num_ssmt2
         nlevels = iv % ssmt2(n) % info % levels
         allocate (y % ssmt2(n) % rh(1:nlevels))
         y % ssmt2(n) % rh(1:nlevels) = 0.0
      end do
   end if
   
   if (y % num_pseudo > 0) then
        allocate (y % pseudo(1:y % num_pseudo))
        y % pseudo(1:y % num_pseudo) % u = 0.0
        y % pseudo(1:y % num_pseudo) % v = 0.0
        y % pseudo(1:y % num_pseudo) % t = 0.0
        y % pseudo(1:y % num_pseudo) % p = 0.0
        y % pseudo(1:y % num_pseudo) % q = 0.0
   end if

   ! Initialize Quikscat:

   if (y % num_qscat > 0) then
      allocate (y % qscat(1:y % num_qscat))
      y % qscat(1:y % num_qscat) % u = 0.0
      y % qscat(1:y % num_qscat) % v = 0.0
   end if
      
   ! Initialize profilers:

   if (y % num_profiler > 0) then
      allocate (y % profiler(1:y % num_profiler))
      do n = 1, y % num_profiler

         nlevels = iv % profiler(n) % info % levels
         allocate (y % profiler(n)%u(1:nlevels))
         allocate (y % profiler(n)%v(1:nlevels))

         y % profiler(n) % u(1:nlevels) = 0.0
         y % profiler(n) % v(1:nlevels) = 0.0

      end do
   end if

   ! Initialize buoy:

   if (y % num_buoy > 0) then
      allocate (y % buoy(1:y % num_buoy))
      y % buoy(1:y % num_buoy) % u = 0.0
      y % buoy(1:y % num_buoy) % v = 0.0
      y % buoy(1:y % num_buoy) % t = 0.0
      y % buoy(1:y % num_buoy) % p = 0.0
      y % buoy(1:y % num_buoy) % q = 0.0
   end if

   ! Initialize radiance:

   if (y % num_inst > 0) then
      allocate (y % instid(1:y % num_inst))
      do i = 1,  y % num_inst
         y % instid(i) % num_rad = iv % instid(i) % num_rad
         y % instid(i) % nchan   = iv % instid(i) % nchan
         ! allocate (y % instid(i) % ichan(1:y % instid(i) % nchan))
         ! do n = 1, y % instid(i) % nchan
         !     y % instid(i) % ichan(n) = n
         ! end do
         if (y % instid(i) % num_rad < 1)  then
            nullify (y % instid(i) % tb)
            cycle
         end if
         allocate (y % instid(i) % tb(1:y % instid(i) % nchan, y % instid(i) % num_rad))
      end do
   end if
   ! Initialize AIRS retrievals:

   if (y % num_airsr > 0) then
      allocate (y % airsr(1:y % num_airsr))
      do n = 1, y % num_airsr
         nlevels = iv % airsr(n) % info % levels
         allocate (y % airsr(n)%t(1:nlevels))
         allocate (y % airsr(n)%q(1:nlevels))

         y % airsr(n) % t(1:nlevels) = 0.0
         y % airsr(n) % q(1:nlevels) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_allocate_y")

end subroutine da_allocate_y


