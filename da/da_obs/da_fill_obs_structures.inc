subroutine da_fill_obs_structures( iv, ob )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003     Profiler and Buoy Obs           Syed RH Rizvi    
!             07/03/2006     Updated for AIRS retrievals     Syed RH Rizvi 
!             10/19/2006     Filled default error factor     Syed RH Rizvi 
!
!  Purpose: Allocates observation structure and fills it from iv.
!----------------------------------------------------------------------------   
   implicit none

   type (ob_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(out)    :: ob   ! (Smaller) observation structure.

   integer                       :: n, k     ! Loop counters.
   real                          :: rh_error ! RH obs. error.
   real                          :: q_error  ! q obs. error.

   if (trace_use) call da_trace_entry("da_fill_obs_structures")

!---------------------------------------------------------------------------
!  Initialise obs error factors (which will be overwritten in use_obs_errfac)
!---------------------------------------------------------------------------
      iv % synop_ef_u = 1.0
      iv % synop_ef_v = 1.0
      iv % synop_ef_t = 1.0
      iv % synop_ef_p = 1.0
      iv % synop_ef_q = 1.0

      iv % metar_ef_u = 1.0
      iv % metar_ef_v = 1.0
      iv % metar_ef_t = 1.0
      iv % metar_ef_p = 1.0
      iv % metar_ef_q = 1.0

      iv % ships_ef_u = 1.0
      iv % ships_ef_v = 1.0
      iv % ships_ef_t = 1.0
      iv % ships_ef_p = 1.0
      iv % ships_ef_q = 1.0

      iv % geoamv_ef_u = 1.0
      iv % geoamv_ef_v = 1.0

      iv % polaramv_ef_u = 1.0
      iv % polaramv_ef_v = 1.0

      iv % gpspw_ef_tpw = 1.0

      iv % gpsref_ef_ref = 1.0
      iv % gpsref_ef_p = 1.0
      iv % gpsref_ef_t = 1.0
      iv % gpsref_ef_q = 1.0

      iv % sound_ef_u = 1.0
      iv % sound_ef_v = 1.0
      iv % sound_ef_t = 1.0
      iv % sound_ef_q = 1.0

      iv % airep_ef_u = 1.0
      iv % airep_ef_v = 1.0
      iv % airep_ef_t = 1.0

      iv % pilot_ef_u = 1.0
      iv % pilot_ef_v = 1.0

      iv % ssmir_ef_speed = 1.0
      iv % ssmir_ef_tpw = 1.0

      iv % satem_ef_thickness = 1.0

      iv % ssmt1_ef_t = 1.0

      iv % ssmt2_ef_rh = 1.0

      iv % qscat_ef_u = 1.0
      iv % qscat_ef_v = 1.0

      iv % profiler_ef_u = 1.0
      iv % profiler_ef_v = 1.0
      
      iv % buoy_ef_u = 1.0
      iv % buoy_ef_v = 1.0
      iv % buoy_ef_t = 1.0
      iv % buoy_ef_p = 1.0
      iv % buoy_ef_q = 1.0

      iv % Radar_ef_rv = 1.0
      iv % Radar_ef_rf = 1.0

      iv % bogus_ef_u = 1.0
      iv % bogus_ef_v = 1.0
      iv % bogus_ef_t = 1.0
      iv % bogus_ef_p = 1.0
      iv % bogus_ef_q = 1.0
      iv % bogus_ef_slp = 1.0

      iv % airsr_ef_t = 1.0
      iv % airsr_ef_q = 1.0

!---------------------------------------------------------------------------
!  [1.0] Allocate innovation vector and observation structures:
!---------------------------------------------------------------------------
   call da_allocate_y( iv, ob )

!----------------------------------------------------------------------
!  [2.0] Transfer observations:
!----------------------------------------------------------------------

!  [2.1] Transfer surface obs:

   if ( iv % num_synop > 0 ) then
   
      do n = 1, iv % num_synop

!        Copy observations variables:

         ob % synop(n) % u = iv % synop(n) % u % inv
         ob % synop(n) % v = iv % synop(n) % v % inv
         ob % synop(n) % t = iv % synop(n) % t % inv
         ob % synop(n) % q = iv % synop(n) % q % inv
         ob % synop(n) % p = iv % synop(n) % p % inv

!        Calculate q error from rh error:

         rh_error = iv%synop(n)%q%error ! q error is rh at this stage!

!        if((ob % synop(n) % p > iv%ptop) .AND. &
!           (ob % synop(n) % t > 100.0) .AND. &
!           (ob % synop(n) % q > 0.0) .AND. &
!           (iv % synop(n) % p % qc >= obs_qc_pointer) .and. &
!           (iv % synop(n) % t % qc >= obs_qc_pointer) .and. &
!           (iv % synop(n) % q % qc >= obs_qc_pointer)) then
         call da_get_q_error( ob % synop(n) % p, &
                              ob % synop(n) % t, &
                              ob % synop(n) % q, &
                              iv % synop(n) % t % error, &
                              rh_error, iv % synop(n) % q % error )
         if(iv%synop(n)% q % error == missing_r) iv%synop(n)% q % qc = missing_data

!        end if

      end do
      
      
   end if

!  [2.2] Transfer metar obs:

   if ( iv % num_metar > 0 ) then

      do n = 1, iv % num_metar

!        Copy observations variables:

         ob % metar(n) % u = iv % metar(n) % u % inv
         ob % metar(n) % v = iv % metar(n) % v % inv
         ob % metar(n) % t = iv % metar(n) % t % inv
         ob % metar(n) % q = iv % metar(n) % q % inv
         ob % metar(n) % p = iv % metar(n) % p % inv

!        Calculate q error from rh error:

         rh_error = iv%metar(n)%q%error ! q error is rh at this stage!
         call da_get_q_error( iv % metar(n) % p % inv, &
                              ob % metar(n) % t, &
                              ob % metar(n) % q, &
                              iv % metar(n) % t % error, &
                              rh_error, q_error )
         iv % metar(n) % q % error = q_error
         if(iv%metar(n)% q % error == missing_r) iv%metar(n)% q % qc = missing_data

      end do
      
   end if

!  [2.2] Transfer ships obs:

   if ( iv % num_ships > 0 ) then
      
      do n = 1, iv % num_ships

!        Copy observations variables:

         ob % ships(n) % u = iv % ships(n) % u % inv
         ob % ships(n) % v = iv % ships(n) % v % inv
         ob % ships(n) % t = iv % ships(n) % t % inv
         ob % ships(n) % q = iv % ships(n) % q % inv
         ob % ships(n) % p = iv % ships(n) % p % inv

!        Calculate q error from rh error:

         rh_error = iv%ships(n)%q%error ! q error is rh at this stage!
         call da_get_q_error( iv % ships(n) % p % inv, &
                              ob % ships(n) % t, &
                              ob % ships(n) % q, &
                              iv % ships(n) % t % error, &
                              rh_error, q_error )
         iv % ships(n) % q % error = q_error

         if(iv%ships(n)% q % error == missing_r) iv%ships(n)% q % qc = missing_data
      end do
      
   end if

!  [2.4.1] Transfer Geo. AMVs Obs:

   if ( iv % num_geoamv > 0 ) then

      do n = 1, iv % num_geoamv
         do k = 1, iv % geoamv(n) % info % levels
!        Copy observations variables:

         ob % geoamv(n) % u(k) = iv % geoamv(n) % u(k) % inv
         ob % geoamv(n) % v(k) = iv % geoamv(n) % v(k) % inv

         end do
      end do

   end if

!  [2.4.2] Transfer  Polar AMVs Obs:

   if ( iv % num_polaramv > 0 ) then

      do n = 1, iv % num_polaramv
         do k = 1, iv % polaramv(n) % info % levels
!        Copy observations variables:

         ob % polaramv(n) % u(k) = iv % polaramv(n) % u(k) % inv
         ob % polaramv(n) % v(k) = iv % polaramv(n) % v(k) % inv

         end do
      end do

   end if

!  [2.5] Transfer gpspw obs:

   if ( iv % num_gpspw > 0 ) then

      do n = 1, iv % num_gpspw
!        Copy observations variables:
         ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv
      end do

   end if

!  [2.6] Transfer GPS REF obs:

   if ( iv % num_gpsref > 0 ) then
      do n = 1, iv % num_gpsref
         do k = 1, iv % gpsref(n) % info % levels

!           Copy observations variables:

            ob % gpsref(n) % ref(k) = iv % gpsref(n) % ref(k) % inv
            ob % gpsref(n) %   p(k) = iv % gpsref(n) %   p(k) % inv
            ob % gpsref(n) %   t(k) = iv % gpsref(n) %   t(k) % inv
            ob % gpsref(n) %   q(k) = iv % gpsref(n) %   q(k) % inv

         end do

      end do

   end if

!  [2.7] Transfer sonde obs:
   if ( iv % num_sound > 0 ) then
      do n = 1, iv % num_sound
         do k = 1, iv % sound(n) % info % levels

!           Copy observations variables:

            ob % sound(n) % u(k) = iv % sound(n) % u(k) % inv
            ob % sound(n) % v(k) = iv % sound(n) % v(k) % inv
            ob % sound(n) % t(k) = iv % sound(n) % t(k) % inv
            ob % sound(n) % q(k) = iv % sound(n) % q(k) % inv

!           Calculate q error from rh error:

            rh_error = iv%sound(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error( iv % sound(n) % p(k), &
                                 ob % sound(n) % t(k), &
                                 ob % sound(n) % q(k), &
                                 iv % sound(n) % t(k) % error, &
                                 rh_error, q_error )

            iv % sound(n) % q(k) % error = q_error
         if(iv%sound(n)% q(k) % error == missing_r) iv%sound(n)% q(k) % qc = missing_data
         end do

!        Copy observations variables:

         ob % sonde_sfc(n) % u = iv % sonde_sfc(n) % u % inv
         ob % sonde_sfc(n) % v = iv % sonde_sfc(n) % v % inv
         ob % sonde_sfc(n) % t = iv % sonde_sfc(n) % t % inv
         ob % sonde_sfc(n) % q = iv % sonde_sfc(n) % q % inv
         ob % sonde_sfc(n) % p = iv % sonde_sfc(n) % p % inv

!        Calculate q error from rh error:

         rh_error = iv%sonde_sfc(n)%q%error ! q error is rh at this stage!
         call da_get_q_error( iv % sonde_sfc(n) % p % inv, &
                              ob % sonde_sfc(n) % t, &
                              ob % sonde_sfc(n) % q, &
                              iv % sonde_sfc(n) % t % error, &
                              rh_error, iv % sonde_sfc(n) % q % error )
         if(iv%sonde_sfc(n)% q % error == missing_r) iv%sonde_sfc(n)% q % qc = missing_data

      end do

   end if

!  [2.8] Transfer airep obs:

   if ( iv % num_airep > 0 ) then
   
      do n = 1, iv % num_airep
         do k = 1, iv % airep(n) % info % levels

!           Copy observations variables:

            ob % airep(n) % u(k) = iv % airep(n) % u(k) % inv
            ob % airep(n) % v(k) = iv % airep(n) % v(k) % inv
            ob % airep(n) % t(k) = iv % airep(n) % t(k) % inv
         end do
      end do

   end if

!  [2.9] Transfer pilot obs:

   if ( iv % num_pilot > 0 ) then

      do n = 1, iv % num_pilot
         do k = 1, iv % pilot(n) % info % levels

!           Copy observations variables:

            ob % pilot(n) % u(k) = iv % pilot(n) % u(k) % inv
            ob % pilot(n) % v(k) = iv % pilot(n) % v(k) % inv
         end do
      end do

   end if

!  [2.10] Transfer SSM/I obs:SSMI:

   if ( iv % num_ssmi_retrieval > 0 ) then
      do n = 1, iv % num_ssmi_retrieval
 
!        Copy observations variables:
         ob % ssmi_retrieval(n) % speed = iv % ssmi_retrieval(n) % speed % inv
         ob % ssmi_retrieval(n) % tpw   = iv % ssmi_retrieval(n) % tpw % inv
      end do

   end if

   if ( iv % num_ssmi_tb > 0 ) then
      do n = 1, iv % num_ssmi_tb

!        Copy observations variables:

         ob % ssmi_tb(n) % tb19v = iv % ssmi_tb(n) % tb19v % inv
         ob % ssmi_tb(n) % tb19h = iv % ssmi_tb(n) % tb19h % inv
         ob % ssmi_tb(n) % tb22v = iv % ssmi_tb(n) % tb22v % inv
         ob % ssmi_tb(n) % tb37v = iv % ssmi_tb(n) % tb37v % inv
         ob % ssmi_tb(n) % tb37h = iv % ssmi_tb(n) % tb37h % inv
         ob % ssmi_tb(n) % tb85v = iv % ssmi_tb(n) % tb85v % inv
         ob % ssmi_tb(n) % tb85h = iv % ssmi_tb(n) % tb85h % inv
      end do

!

   end if

!  [2.11] Transfer satem obs:

   if ( iv % num_satem > 0 ) then
   
      do n = 1, iv % num_satem
         do k = 1, iv % satem(n) % info % levels

!           Copy observations variables:

            ob % satem(n) % thickness(k) = iv % satem(n) % thickness(k) % inv

         end do
      end do

   end if
   
!  [2.12] Transfer ssmt1 obs:

   if ( iv % num_ssmt1 > 0 ) then

      do n = 1, iv % num_ssmt1
         do k = 1, iv % ssmt1(n) % info % levels

!           Copy observations variables:
            ob % ssmt1(n) % t(k) = iv % ssmt1(n) % t(k) % inv
         end do
      end do

   end if

!  [2.13] Transfer ssmt2 obs:

   if ( iv % num_ssmt2 > 0 ) then

      do n = 1, iv % num_ssmt2
         do k = 1, iv % ssmt2(n) % info % levels

!           Copy observations variables:
            ob % ssmt2(n) % rh(k) = iv % ssmt2(n) % rh(k) % inv
         end do
      end do

   end if
   
!  [2.14] Setup pseudo observations:

   call da_setup_pseudo_obs( iv, ob )

!  [2.15] Transfer scatterometer obs:

   if ( iv % num_qscat > 0 ) then
      
      do n = 1, iv % num_qscat
!        Copy observations variables:
         ob % qscat(n) % u = iv % qscat(n) % u % inv
         ob % qscat(n) % v = iv % qscat(n) % v % inv
      end do
      
   end if
!
!  [2.16] Transfer profiler obs:

   if ( iv % num_profiler > 0 ) then

      do n = 1, iv % num_profiler
         do k = 1, iv % profiler(n) % info % levels

!           Copy observations variables:

            ob % profiler(n) % u(k) = iv % profiler(n) % u(k) % inv
            ob % profiler(n) % v(k) = iv % profiler(n) % v(k) % inv
         end do
      end do

   end if

!  [2.17] Transfer buoy obs:

   if ( iv % num_buoy > 0 ) then
      do n = 1, iv % num_buoy
         ob % buoy(n) % p = iv % buoy(n) % p % inv
      end do
      
      do n = 1, iv % num_buoy

!        Copy observations variables:

         ob % buoy(n) % u = iv % buoy(n) % u % inv
         ob % buoy(n) % v = iv % buoy(n) % v % inv
         ob % buoy(n) % t = iv % buoy(n) % t % inv
         ob % buoy(n) % q = iv % buoy(n) % q % inv

!        Calculate q error from rh error:

         rh_error = iv%buoy(n)%q%error ! q error is rh at this stage!
         call da_get_q_error( iv % buoy(n) % p % inv, &
                              ob % buoy(n) % t, &
                              ob % buoy(n) % q, &
                              iv % buoy(n) % t % error, &
                              rh_error, q_error )
         iv % buoy(n) % q % error = q_error

         if(iv%buoy (n)% q % error == missing_r) iv%buoy (n)% q % qc = missing_data
      end do
   end if

!  [2.18] Transfer Radar obs:

   if ( iv % num_Radar > 0 ) then

      do n = 1, iv % num_Radar
         do k = 1, iv % Radar(n) % info % levels

!           Copy observations variables:

            ob % Radar(n) % rv(k) = iv % Radar(n) % rv(k) % inv
            ob % Radar(n) % rf(k) = iv % Radar(n) % rf(k) % inv

         end do
      end do

   end if

!  [2.19] Transfer TC bogus:

   if ( iv % num_bogus > 0 ) then

      do n = 1, iv % num_bogus
         do k = 1, iv % bogus(n) % info % levels

!           Copy observations variables:

            ob % bogus(n) % u(k) = iv % bogus(n) % u(k) % inv
            ob % bogus(n) % v(k) = iv % bogus(n) % v(k) % inv
            ob % bogus(n) % t(k) = iv % bogus(n) % t(k) % inv
            ob % bogus(n) % q(k) = iv % bogus(n) % q(k) % inv

!           Calculate q error from rh error:

            rh_error = iv%bogus(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error( iv % bogus(n) % p(k), &
                                 ob % bogus(n) % t(k), &
                                 ob % bogus(n) % q(k), &
                                 iv % bogus(n) % t(k) % error, &
                                 rh_error, q_error )

            iv % bogus(n) % q(k) % error = q_error
         if(iv%bogus(n)% q(k) % error == missing_r) iv%bogus(n)% q(k) % qc = missing_data
         end do
         ob % bogus(n) % slp = iv % bogus(n) % slp % inv
      end do

   end if

!  Transfer AIRS  retrievals:

   if ( iv % num_airsr > 0 ) then
      do n = 1, iv % num_airsr
         do k = 1, iv % airsr(n) % info % levels

!           Copy observations variables:

            ob % airsr(n) % t(k) = iv % airsr(n) % t(k) % inv
            ob % airsr(n) % q(k) = iv % airsr(n) % q(k) % inv

!           Calculate q error from rh error:

            rh_error = iv%airsr(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error( iv % airsr(n) % p(k), &
                                 ob % airsr(n) % t(k), &
                                 ob % airsr(n) % q(k), &
                                 iv % airsr(n) % t(k) % error, &
                                 rh_error, q_error )

            iv % airsr(n) % q(k) % error = q_error
          if(iv%airsr(n)% q(k) % error == missing_r) iv%airsr(n)% q(k) % qc = missing_data
          end do
      end do

   end if
   if (trace_use) call da_trace_exit("da_fill_obs_structures")


end subroutine da_fill_obs_structures
