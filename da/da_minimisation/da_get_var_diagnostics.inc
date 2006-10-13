subroutine da_get_var_diagnostics ( iv, j )

   type (ob_type), intent(inout):: iv      ! innovation vector.
   type (j_type), intent(inout) :: j       ! Cost function.

   integer                      :: num_obs_tot, ierror
   real                         :: dum1, dum2, dum3, dum4, dum5, dum6
   integer                      :: i,k
   real                         :: jo_radiance

   IF (trace_use) CALL da_trace_entry("da_get_var_diagnostics")

   !--------------------------------------------------------------------------
   ! [1.0] Sum up Jo across processors:
   !--------------------------------------------------------------------------

   num_obs_tot = num_gpspw_tot + num_synop_tot + num_metar_tot + &
                 num_pilot_tot + num_ssmi_tot  + num_ssmt1_tot + &
                 num_ssmt2_tot + num_satem_tot + num_airep_tot + &
                 num_geoamv_tot+ num_polaramv_tot+ num_ships_tot + &
                 num_sound_tot + num_qscat_tot + num_profiler_tot+ &
                 num_buoy_tot + num_gpsref_tot + num_bogus_tot   + &
                 num_radiance_tot + num_airsr_tot

   call da_sum_reals( 5, j % jo % synop_u, j % jo % synop_v, &
                      j % jo % synop_t, j % jo % synop_p, j % jo % synop_q,&
                                                                dum1, dum2 )
   call da_sum_reals( 5, j % jo % metar_u, j % jo % metar_v, &
                      j % jo % metar_t, j % jo % metar_p, j % jo % metar_q,&
                                                                dum1, dum2 )
   call da_sum_reals( 5, j % jo % ships_u, j % jo % ships_v, &
                      j % jo % ships_t, j % jo % ships_p, j % jo % ships_q,&
                                                                dum1, dum2 )
   call da_sum_reals( 2, j % jo % geoamv_u, j % jo % geoamv_v, dum1, dum2,   &
                                                          dum3, dum4, dum5 )
   call da_sum_reals( 2, j % jo % polaramv_u, j % jo % polaramv_v, dum1, dum2,   &
                                                          dum3, dum4, dum5 )
   call da_sum_reals( 1, j % jo % gpspw_tpw, dum1, dum2, dum3, dum4, dum5, &
                                                                      dum6 )
   call da_sum_reals( 1, j % jo % gpsref_ref, dum1, dum2, dum3, dum4, dum5, &
                                                                      dum6 )
   call da_sum_reals( 4, j % jo % sound_u, j % jo % sound_v, &
                         j % jo % sound_t, j % jo % sound_q, dum1, dum2,   &
                                                                      dum3 )
   call da_sum_reals( 5, j % jo % sonde_sfc_u, j % jo % sonde_sfc_v, &
                         j % jo % sonde_sfc_t, j % jo % sonde_sfc_p, &
                         j % jo % sonde_sfc_q, dum1, dum2)
   call da_sum_reals( 3, j % jo % airep_u, j % jo % airep_v, &
                      j % jo % airep_t, dum1, dum2, dum3, dum4 )
   call da_sum_reals( 2, j % jo % pilot_u, j % jo % pilot_v, dum1, dum2,   &
                                                          dum3, dum4, dum5 )
   call da_sum_reals( 5, j % jo % bogus_u, j % jo % bogus_v, &
                         j % jo % bogus_t, j % jo % bogus_q, &
                         j % jo % bogus_slp, dum1, dum2 )
   call da_sum_reals( 2, j % jo % ssmir_speed, j % jo % ssmir_tpw, &
                      dum1, dum2, dum3, dum4, dum5 )
   call da_sum_reals( 7, j % jo % ssmi_tb19v, j % jo % ssmi_tb19h, &
                         j % jo % ssmi_tb22v, j % jo % ssmi_tb37v, &
                         j % jo % ssmi_tb37h, j % jo % ssmi_tb85v, &
                         j % jo % ssmi_tb85h )
   call da_sum_reals( 1, j % jo % satem_thickness, dum1, dum2, dum3, dum4, &
                                                                dum5, dum6 )
   call da_sum_reals( 1, j % jo % ssmt1_t, dum1, dum2, dum3, dum4, dum5,   &
                                                                      dum6 )
   call da_sum_reals( 1, j % jo % ssmt2_rh, dum1, dum2, dum3, dum4, dum5,  &
                                                                      dum6 )
   call da_sum_reals( 2, j % jo % qscat_u, j % jo % qscat_v, dum1, dum2,   &
                                                          dum3, dum4, dum5 )
   call da_sum_reals( 2, j % jo % profiler_u, j % jo % profiler_v, dum1,   &
                                                    dum2, dum3, dum4, dum5 )
   call da_sum_reals( 5, j % jo % buoy_u, j % jo % buoy_v, &
                      j % jo % buoy_t, j % jo % buoy_p, j % jo % buoy_q,   &
                                                                dum1, dum2 )
   call da_sum_reals( 2, j % jo % airsr_t, j % jo % airsr_q, &
                         dum1, dum2,dum3, dum4, dum5 )

   if ( use_radiance ) then
      jo_radiance = 0.
      do i = 1, iv%num_inst                 ! loop for sensor
         do k = 1, iv%instid(i)%nchan          ! loop for channels
            call da_proc_sum_int ( j % jo % rad(i)% num_ichan(k) )
            call da_sum_reals( 1, j % jo % rad(i) % jo_ichan(k),  &
               dum1, dum2, dum3, dum4, dum5, dum6 )
            jo_radiance = jo_radiance + j % jo % rad(i) % jo_ichan(k)
         end do
      end do
   end if

   !-----------------------------------------------------------------------------
   ! [2.0] Print out VAR diagnostics:
   !-----------------------------------------------------------------------------

   if ( rootproc ) THEN

      write(UNIT=stdout,FMT=*) ' '
      WRITE(UNIT=stdout,FMT='(A,F12.2)')  '    Final cost function J    = ', j % total
      write(UNIT=stdout,FMT=*) ' '

      write(UNIT=stdout,FMT='(a39,i8)')   '    Total number of radiance obs.    = ', num_radiance_tot
      write(UNIT=stdout,FMT='(a39,f15.5)')'    Cost Function of radiance obs.   = ', jo_radiance

      write(UNIT=stdout,FMT='(a30,i8)')   '    Total number of obs.     = ', num_obs_tot
      write(UNIT=stdout,FMT='(a30,f15.5)')'    Final WRFVAR value of J  = ', j % total
      write(UNIT=stdout,FMT='(a30,f15.5)')'    Final WRFVAR value of Jo = ', j % jo % total
      write(UNIT=stdout,FMT='(a30,f15.5)')'    Final WRFVAR value of Jb = ', j % jb
      write(UNIT=stdout,FMT='(a30,f15.5)')'    Final WRFVAR value of Je = ', j % je
      if (num_obs_tot > 0) &
         write(UNIT=stdout,FMT='(a30,f15.5)')'    Final J / total num_obs = ', j % total / &
                                                          real(num_obs_tot)
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used (1)= ', var_scaling1
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used (2)= ', var_scaling2
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used (3)= ', var_scaling3
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used (4)= ', var_scaling4
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used (5)= ', var_scaling5
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Jb factor used    = ', jb_factor
      write(UNIT=stdout,FMT='(a30,f15.5)')'    WRFVAR Je factor used    = ', je_factor
      write(UNIT=stdout,FMT=*) ' '

      ! [4.2] Output components of Jo:

      if (iv % num_synop_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    synop obs, Jo (actual)  = ', &
                                  iv % num_synop_glo, num_synop_tot, &
                                  j % jo % synop_u, iv % synop_ef_u, &
                                  j % jo % synop_v, iv % synop_ef_v, &
                                  j % jo % synop_t, iv % synop_ef_t, &
                                  j % jo % synop_p, iv % synop_ef_p, &
                                  j % jo % synop_q, iv % synop_ef_q

      end if

      IF (trace_use) CALL da_trace("da_get_var_diagnostics", &
         message="Memory increase from internal write")

      if (iv % num_metar_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    metar obs, Jo (actual)  = ', &
                               iv % num_metar_glo, num_metar_tot, &
                               j % jo % metar_u, iv % metar_ef_u, &
                               j % jo % metar_v, iv % metar_ef_v, &
                               j % jo % metar_t, iv % metar_ef_t, &
                               j % jo % metar_p, iv % metar_ef_p, &
                               j % jo % metar_q, iv % metar_ef_q    
      end if

      if (iv % num_ships_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    ships obs, Jo (actual)  = ', &
                               iv % num_ships_glo, num_ships_tot, &
                               j % jo % ships_u, iv % ships_ef_u, &
                               j % jo % ships_v, iv % ships_ef_v, &
                               j % jo % ships_t, iv % ships_ef_t, &
                               j % jo % ships_p, iv % ships_ef_p, &
                               j % jo % ships_q, iv % ships_ef_q                                
      end if


      if (iv % num_geoamv_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    geoamv ob, Jo (actual)  = ', &
                               iv % num_geoamv_glo, num_geoamv_tot, &
                               j % jo % geoamv_u, iv % geoamv_ef_u, &
                               j % jo % geoamv_v, iv % geoamv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_polaramv_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    polaramv,  Jo (actual)  = ', &
                               iv % num_polaramv_glo, num_polaramv_tot, &
                               j % jo % polaramv_u, iv % polaramv_ef_u, &
                               j % jo % polaramv_v, iv % polaramv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if


      if (iv % num_gpspw_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    gpspw obs, Jo (actual)  = ', &
                               iv % num_gpspw_glo, num_gpspw_tot, &
                               j % jo % gpspw_tpw, iv % gpspw_ef_tpw, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_gpsref_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    gpsref obs, Jo (actual)  = ', &
                               iv % num_gpsref_glo, num_gpsref_tot, &
                               j % jo % gpsref_ref, iv % gpsref_ef_ref, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_sound_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    sound obs, Jo (actual)  = ', &
                               iv % num_sound_glo, num_sound_tot, &
                               j % jo % sound_u, iv % sound_ef_u, &
                               j % jo % sound_v, iv % sound_ef_v, &
                               j % jo % sound_t, iv % sound_ef_t, &
                               j % jo % sound_q, iv % sound_ef_q, 0.0, 1.0 
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    sonde obs, Jo (actual)  = ', &
                               iv % num_sound_glo, num_sound_tot, &
                               j % jo % sonde_sfc_u, iv % synop_ef_u, &
                               j % jo % sonde_sfc_v, iv % synop_ef_v, &
                               j % jo % sonde_sfc_t, iv % synop_ef_t, &
                               j % jo % sonde_sfc_p, iv % synop_ef_p, &
                               j % jo % sonde_sfc_q, iv % synop_ef_q
      end if

      if (iv % num_airep_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    airep obs, Jo (actual)  = ', &
                               iv % num_airep_glo, num_airep_tot, &
                               j % jo % airep_u, iv % airep_ef_u, &
                               j % jo % airep_v, iv % airep_ef_v, &
                               j % jo % airep_t, iv % airep_ef_t, &
                               0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_bogus_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    bogus obs, Jo (actual)  = ', &
                               iv % num_bogus_glo, num_bogus_tot, &
                               j % jo % bogus_u, iv % bogus_ef_u, &
                               j % jo % bogus_v, iv % bogus_ef_v, &
                               j % jo % bogus_t, iv % bogus_ef_t, &
                               j % jo % bogus_q, iv % bogus_ef_q, &
                               j % jo % bogus_slp, iv % bogus_ef_slp
      end if

      if (iv % num_pilot_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    pilot obs, Jo (actual)  = ', &
                               iv % num_pilot_glo, num_pilot_tot, &
                               j % jo % pilot_u, iv % pilot_ef_u, &
                               j % jo % pilot_v, iv % pilot_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmi_retrieval_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'     ssmi  obs, Jo (actual) = ', &
                                  iv % num_ssmi_retrieval_glo, num_ssmi_tot, &
                                  j % jo % ssmir_speed, iv % ssmir_ef_speed, &
                                  j % jo % ssmir_tpw, iv % ssmir_ef_tpw, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_satem_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    satem obs, Jo (actual)  = ', &
                               iv % num_satem_glo, num_satem_tot, &
                               j % jo % satem_thickness, iv % satem_ef_thickness, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmt1_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    ssmt1 obs, Jo (actual)  = ', &
                               iv % num_ssmt1_glo, num_ssmt1_tot, &
                               j % jo % ssmt1_t, iv % ssmt1_ef_t, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmt2_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    ssmt2 obs, Jo (actual)  = ', &
                               iv % num_ssmt2_glo, num_ssmt2_tot, &
                               j % jo % ssmt2_rh, iv % ssmt2_ef_rh, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_qscat_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    qscat obs, Jo (actual)  = ', &
                               iv % num_qscat_glo, num_qscat_tot, &
                               j % jo % qscat_u, iv % qscat_ef_u, &
                               j % jo % qscat_v, iv % qscat_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_buoy_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    buoy  obs, Jo (actual)  = ', &
                               iv % num_buoy_glo, num_buoy_tot, &
                               j % jo % buoy_u, iv % buoy_ef_u, &
                               j % jo % buoy_v, iv % buoy_ef_v, &
                               j % jo % buoy_t, iv % buoy_ef_t, &
                               j % jo % buoy_p, iv % buoy_ef_p, &
                               j % jo % buoy_q, iv % buoy_ef_q                                
      end if

      if (iv % num_profiler_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    profiler,  Jo (actual)  = ', &
                               iv % num_profiler_glo, num_profiler_tot, &
                               j % jo % profiler_u, iv % profiler_ef_u, &
                               j % jo % profiler_v, iv % profiler_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      if (iv % num_airsr_glo > 0 ) then
         write(UNIT=jo_unit,FMT='(a30,2i8,10f15.5)')'    airsr obs, Jo (actual)  = ', &
                               iv % num_airsr_glo, num_airsr_tot, &
                               j % jo % airsr_t, iv % airsr_ef_t, &
                               j % jo % airsr_q, iv % airsr_ef_q, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      do i = 1, iv%num_inst                 ! loop for sensor
         do k = 1, iv%instid(i)%nchan
            if ( j % jo % rad(i) % num_ichan(k) > 0 ) then
               write(UNIT=jo_unit,FMT='(a30,a19,i5,i8,f15.5)') 'radiance obs, Jo (actual)  = ', &
                                 iv%instid(i)%rttovid_string, k , &
                                 j % jo % rad(i) % num_ichan(k), &
                                 j % jo % rad(i) % jo_ichan(k)
            end if
         end do
      end do
   end if

   IF (trace_use) CALL da_trace_exit("da_get_var_diagnostics")
      
end subroutine da_get_var_diagnostics
