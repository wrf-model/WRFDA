      subroutine da_calculate_jo_and_grady( iv, re, jot, jo, jo_grad_y)

      implicit none
!----------------------------------------------------------------------------
!       03/04/2005 - AMV's from Geostationary and       Syed  RH  Rizvi
!                    Polar orbiting satellite are
!                    seperated & used as profile
!                    Initialised contribution of all obs in Jo
!          08/2005 - Add radiance part                  Zhiquan Liu
!       07/03/2006 - Update for AIRS retrievals     Syed RH Rizvi
!
!-------------------------------------------------------------------------
      
      type (ob_type), intent(IN)   :: iv          ! Innovation vector (O-B).
      type (y_type), intent(IN)    :: re          ! Residual vector (O-A).
      real, intent(OUT)            :: jot         ! Obs cost function.
      type (jo_type), intent(OUT)  :: jo          ! Obs cost function.
      type (y_type), intent(OUT)   :: jo_grad_y   ! Grad_y(Jo)
      real                         :: jo_sound, jo_synop, jo_geoamv, jo_polaramv, &
                                      jo_airep, jo_pilot, jo_satem, &
                                      jo_metar, jo_ships, jo_gpspw, &
                                      jo_ssmi , jo_ssmt1, jo_ssmt2, &
                                      jo_pseudo, jo_qscat, jo_buoy, &
                                      jo_profiler, jo_Radar, jo_gpsref, jo_bogus, &
                                      jo_radiance, jo_airsr
      integer                      :: i
!-------------------------------------------------------------------------
!     [1.0] Compute components of Grad_y(Jo):
!-------------------------------------------------------------------------

!     Radiosonde

      call da_calculate_jo_and_grady_sound( iv, re, jo, jo_grad_y)
      jo_sound = jo % sound_u + jo % sound_v + &
                 jo % sound_t + jo % sound_q

      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_sound =', jo_sound, &
              'jo%sound_u =', jo%sound_u, &
              'jo%sound_v =', jo%sound_v, &
              'jo%sound_t =', jo%sound_t, &
              'jo%sound_q =', jo%sound_q
      endif

      call da_get_jo_and_grady_sonde_sfc( iv, re, jo, jo_grad_y)
      jo_sound = jo_sound         + jo % sonde_sfc_u + jo % sonde_sfc_v + &
                 jo % sonde_sfc_t + jo % sonde_sfc_q + jo % sonde_sfc_p

      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_sound =', jo_sound, &
              'jo%sonde_sfc_u =', jo%sonde_sfc_u, &
              'jo%sonde_sfc_v =', jo%sonde_sfc_v, &
              'jo%sonde_sfc_t =', jo%sonde_sfc_t, &
              'jo%sonde_sfc_p =', jo%sonde_sfc_p, &
              'jo%sonde_sfc_q =', jo%sonde_sfc_q
      endif

!     Surface

      call da_calculate_jo_and_grady_synop( iv, re, jo, jo_grad_y)
      jo_synop = jo % synop_u + jo % synop_v + jo % synop_t + &
                 jo % synop_p + jo % synop_q

      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_synop =', jo_synop, &
              'jo%synop_u =', jo%synop_u, &
              'jo%synop_v =', jo%synop_v, &
              'jo%synop_t =', jo%synop_t, &
              'jo%synop_p =', jo%synop_p, &
              'jo%synop_q =', jo%synop_q
      endif
                 

!     Geostationary Satellite AMVs:

      call da_get_jo_and_grady_geoamv( iv, re, jo, jo_grad_y)
      jo_geoamv = jo % geoamv_u + jo % geoamv_v
      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_geoamv =', jo_geoamv, &
              'jo%geoamv_u =', jo%geoamv_u, &
              'jo%geoamv_v =', jo%geoamv_v
      end if

!     Polar orbitting Satellite AMVs:

      call da_get_jo_and_grady_polaramv( iv, re, jo, jo_grad_y)
      jo_polaramv = jo % polaramv_u + jo % polaramv_v
      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_polaramv =', jo_polaramv, &
              'jo%polaramv_u =', jo%polaramv_u, &
              'jo%polaramv_v =', jo%polaramv_v
      end if

!     Airep

      call da_calculate_jo_and_grady_airep( iv, re, jo, jo_grad_y)
      jo_airep = jo % airep_u + jo % airep_v + jo % airep_t

!     Pilot

      call da_calculate_jo_and_grady_pilot( iv, re, jo, jo_grad_y)
      jo_pilot = jo % pilot_u + jo % pilot_v
      
!     Satem

      call da_calculate_jo_and_grady_satem( iv, re, jo, jo_grad_y)
      jo_satem = jo % satem_thickness

!     Metar:

      call da_calculate_jo_and_grady_metar( iv, re, jo, jo_grad_y)
      jo_metar = jo % metar_u + jo % metar_v + jo % metar_t + &
                 jo % metar_p + jo % metar_q

!     Ships:

      call da_calculate_jo_and_grady_ships( iv, re, jo, jo_grad_y)
      jo_ships = jo % ships_u + jo % ships_v + jo % ships_t + &
                 jo % ships_p + jo % ships_q

!     GPSPW:

      call da_calculate_jo_and_grady_gpspw( iv, re, jo, jo_grad_y)
      jo_gpspw = jo % gpspw_tpw

!     GPSRF:

      call da_get_jo_and_grady_gpsref( iv, re, jo, jo_grad_y)
      jo_gpsref = jo % gpsref_ref

!     SSMI:

      call da_cal_jo_and_grady_ssmi( iv, re, jo, jo_ssmi, jo_grad_y) 

!     SSMT1:

      call da_cal_jo_and_grady_ssmt1( iv, re, jo, jo_grad_y)
      jo_ssmt1 = jo % ssmt1_t

!     SSMT2:

      call da_cal_jo_and_grady_ssmt2( iv, re, jo, jo_grad_y)  
      jo_ssmt2 = jo % ssmt2_rh

!     Radar obs:

      call da_calculate_jo_and_grady_radar( iv, re, jo, jo_grad_y)
      jo_Radar = jo % Radar_rv + jo % Radar_rf

!     Pseudo obs:

      call da_cal_jo_and_grady_pseudo( iv, re, jo, jo_grad_y )    
      jo_pseudo = jo % pseudo_u + jo % pseudo_v + jo % pseudo_t + &
                  jo % pseudo_p + jo % pseudo_q
                  
!     Quikscat scatterometer:

      call da_calculate_jo_and_grady_qscat( iv, re, jo, jo_grad_y)
      jo_qscat = jo % qscat_u + jo % qscat_v

!     Profiler:

      call da_get_jo_and_grady_profiler (iv, re, jo, jo_grad_y)
      jo_profiler = jo % profiler_u + jo % profiler_v

!     TC bogus:

      call da_calculate_jo_and_grady_bogus ( iv, re, jo, jo_grad_y)
      jo_bogus = jo % bogus_u + jo % bogus_v + jo % bogus_slp + &
                 jo % bogus_t + jo % bogus_q
      
!     Buoy: 

      call da_calculate_jo_and_grady_buoy ( iv, re, jo, jo_grad_y)
      jo_buoy = jo % buoy_u + jo % buoy_v + jo % buoy_t + &
                jo % buoy_p + jo % buoy_q

!     Radiance:

      call da_calculate_jo_and_grady_rad ( iv, re, jo, jo_grad_y)
      if ( use_radiance ) then
        jo_radiance = sum ( jo % rad(:) )
      else
        jo_radiance = 0.
      end if
      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_radiance =', jo_radiance
       do i = 1, iv%num_inst
         write(UNIT=stdout, fmt='(a, e24.12)') &
              trim('jo_'//iv%instid(i)%rttovid_string), jo % rad(i)
       end do
      end if
!-------------------------------------------------------------------------
!     AIRS retrievals 
!
      call da_calculate_jo_and_grady_airsr( iv, re, jo, jo_grad_y)
      jo_airsr = jo % airsr_t + jo % airsr_q

      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_airsr =', jo_airsr, &
              'jo%airsr_t =', jo%airsr_t, &
              'jo%airsr_q =', jo%airsr_q
      endif
!-------------------------------------------------------------------------
!     [2.0] Jo = 1/2 * (yo-y)**2/ob_err_variance:
!-------------------------------------------------------------------------

      jo % total = jo_sound + jo_geoamv + jo_polaramv + jo_synop + jo_satem + &
                   jo_pilot + jo_airep + jo_metar + jo_ships + &
                   jo_gpspw + jo_ssmi + jo_ssmt1 + jo_ssmt2 + &
                   jo_pseudo + jo_qscat + jo_profiler + jo_buoy + &
                   jo_Radar + jo_gpsref + jo_bogus + jo_radiance + jo_airsr
                   
      jot = jo % total

      if(print_detail_grad) then
         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo % total = ', jot

         write(UNIT=stdout, fmt='(a, e24.12)') &
              'jo_sound =', jo_sound, &
              'jo_geoamv =', jo_geoamv, &
              'jo_polaramv =', jo_polaramv, &
              'jo_synop =', jo_synop, &
              'jo_satem =', jo_satem, &
              'jo_pilot =', jo_pilot, &
              'jo_airep =', jo_airep, &
              'jo_metar =', jo_metar, &
              'jo_ships =', jo_ships, &
              'jo_gpspw =', jo_gpspw, &
              'jo_ssmi  =', jo_ssmi, &
              'jo_ssmt1 =', jo_ssmt1, &
              'jo_ssmt2 =', jo_ssmt2, &
              'jo_pseudo=', jo_pseudo, &
              'jo_qscat =', jo_qscat, &
              'jo_profil=', jo_profiler, &
              'jo_buoy  =', jo_buoy, &
              'jo_Radar =', jo_Radar, &
              'jo_gpsref =', jo_gpsref, &
              'jo_bogus =', jo_bogus,  &
              'jo_radiance =', jo_radiance, &
              'jo_airsr =', jo_airsr          

      endif
                   
      end subroutine da_calculate_jo_and_grady

