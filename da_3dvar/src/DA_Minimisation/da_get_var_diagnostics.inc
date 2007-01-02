subroutine da_get_var_diagnostics ( iv, j )


!------------------------------------------------------------------------------
!  History:
!           03/04/2005 - AMV's from Geostationary and       Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!------------------------------------------------------------------------------
      type (ob_type), intent(inout):: iv      ! innovation vector.
      type (j_type), intent(inout) :: j       ! Cost function.

      integer                      :: num_obs_tot, ierror
      real                         :: dum1, dum2, dum3, dum4, dum5, dum6

!------------------------------------------------------------------------------
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor     ! For statistics output.
!------------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!  [1.0] Sum up Jo across processors:
!-----------------------------------------------------------------------------

      num_obs_tot = num_gpspw_tot + num_synop_tot + num_metar_tot + &
                    num_pilot_tot + num_ssmi_tot  + num_ssmt1_tot + &
                    num_ssmt2_tot + num_satem_tot + num_airep_tot + &
                    num_geoamv_tot+ num_polaramv_tot+ num_ships_tot + &
                    num_sound_tot + num_qscat_tot + num_profiler_tot+ &
                    num_buoy_tot + num_gpsref_tot + num_bogus_tot

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
!-----------------------------------------------------------------------------
!  [2.0] Print out VAR diagnostics:
!-----------------------------------------------------------------------------

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( iv%num_synop, iv%num_synop_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_metar, iv%num_metar_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_ships, iv%num_ships_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_pilot, iv%num_pilot_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_ssmi_retrieval , iv%num_ssmi_retrieval_glo , 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_ssmt1, iv%num_ssmt1_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_ssmt2, iv%num_ssmt2_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_satem, iv%num_satem_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_gpspw, iv%num_gpspw_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_airep, iv%num_airep_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_geoamv, iv%num_geoamv_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_polaramv, iv%num_polaramv_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_sound, iv%num_sound_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_qscat, iv%num_qscat_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_buoy, iv%num_buoy_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_profiler, iv%num_profiler_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
      call MPI_ALLREDUCE( iv%num_bogus, iv%num_bogus_glo, 1, MPI_INTEGER, &
                          MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
      iv%num_profiler_glo = iv%num_profiler
      iv%num_buoy_glo = iv%num_buoy
      iv%num_qscat_glo = iv%num_qscat
      iv%num_sound_glo = iv%num_sound
      iv%num_airep_glo = iv%num_airep
      iv%num_geoamv_glo = iv%num_geoamv
      iv%num_polaramv_glo = iv%num_polaramv
      iv%num_gpspw_glo = iv%num_gpspw
      iv%num_satem_glo = iv%num_satem
      iv%num_ssmi_retrieval_glo = iv%num_ssmi_retrieval
      iv%num_ssmt1_glo = iv%num_ssmt1
      iv%num_ssmt2_glo = iv%num_ssmt2
      iv%num_pilot_glo = iv%num_pilot
      iv%num_bogus_glo = iv%num_bogus
      iv%num_ships_glo = iv%num_ships
      iv%num_metar_glo = iv%num_metar
      iv%num_synop_glo = iv%num_synop
#endif

      if ( wrf_dm_on_monitor() ) THEN

         write(6,*)
         print *, '   Final cost function J  = ', j % total
         write(6,*)
                    
         write(6,'(a30,i8)')   '    Total number of obs.    = ', num_obs_tot
         write(6,'(a30,f15.5)')'    Final 3DVAR value of J  = ', j % total
         write(6,'(a30,f15.5)')'    Final 3DVAR value of Jo = ', j % jo % total
         write(6,'(a30,f15.5)')'    Final 3DVAR value of Jb = ', j % jb
         if(num_obs_tot > 0) &
         write(6,'(a30,f15.5)')'    Final J / total num_obs = ', j % total / &
                                                             real(num_obs_tot)
         write(6,'(a30,f15.5)')'    3DVAR Jb factor used (1)= ', var_scaling1
         write(6,'(a30,f15.5)')'    3DVAR Jb factor used (2)= ', var_scaling2
         write(6,'(a30,f15.5)')'    3DVAR Jb factor used (3)= ', var_scaling3
         write(6,'(a30,f15.5)')'    3DVAR Jb factor used (4)= ', var_scaling4
         write(6,'(a30,f15.5)')'    3DVAR Jb factor used (5)= ', var_scaling5
         write(6,*)

!        [4.2] Output components of Jo:

         if ( iv % num_synop_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    synop obs, Jo (actual)  = ', &
                                     iv % num_synop_glo, num_synop_tot, &
                                     j % jo % synop_u, iv % synop_ef_u, &
                                     j % jo % synop_v, iv % synop_ef_v, &
                                     j % jo % synop_t, iv % synop_ef_t, &
                                     j % jo % synop_p, iv % synop_ef_p, &
                                     j % jo % synop_q, iv % synop_ef_q

         end if
      
         if ( iv % num_metar_glo > 0 ) then
               write(jo_unit,'(a30,2i8,10f15.5)')'    metar obs, Jo (actual)  = ', &
                                  iv % num_metar_glo, num_metar_tot, &
                                  j % jo % metar_u, iv % metar_ef_u, &
                                  j % jo % metar_v, iv % metar_ef_v, &
                                  j % jo % metar_t, iv % metar_ef_t, &
                                  j % jo % metar_p, iv % metar_ef_p, &
                                  j % jo % metar_q, iv % metar_ef_q    
         end if
      
         if ( iv % num_ships_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    ships obs, Jo (actual)  = ', &
                                  iv % num_ships_glo, num_ships_tot, &
                                  j % jo % ships_u, iv % ships_ef_u, &
                                  j % jo % ships_v, iv % ships_ef_v, &
                                  j % jo % ships_t, iv % ships_ef_t, &
                                  j % jo % ships_p, iv % ships_ef_p, &
                                  j % jo % ships_q, iv % ships_ef_q                                
         end if
      

         if ( iv % num_geoamv_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    geo. cmv obs, Jo (actual)  = ', &
                                  iv % num_geoamv_glo, num_geoamv_tot, &
                                  j % jo % geoamv_u, iv % geoamv_ef_u, &
                                  j % jo % geoamv_v, iv % geoamv_ef_v, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_polaramv_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'   Polar cmv obs, Jo (actual)  = ', &
                                  iv % num_polaramv_glo, num_polaramv_tot, &
                                  j % jo % polaramv_u, iv % polaramv_ef_u, &
                                  j % jo % polaramv_v, iv % polaramv_ef_v, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

      
         if ( iv % num_gpspw_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    gpspw obs, Jo (actual)  = ', &
                                  iv % num_gpspw_glo, num_gpspw_tot, &
                                  j % jo % gpspw_tpw, iv % gpspw_ef_tpw, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if
      
         if ( iv % num_gpsref_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    gpsref obs, Jo (actual)  = ', &
                                  iv % num_gpsref_glo, num_gpsref_tot, &
                                  j % jo % gpsref_ref, iv % gpsref_ef_ref, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if
      
         if ( iv % num_sound_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    sound obs, Jo (actual)  = ', &
                                  iv % num_sound_glo, num_sound_tot, &
                                  j % jo % sound_u, iv % sound_ef_u, &
                                  j % jo % sound_v, iv % sound_ef_v, &
                                  j % jo % sound_t, iv % sound_ef_t, &
                                  j % jo % sound_q, iv % sound_ef_q                                
            write(jo_unit,'(a30,2i8,10f15.5)')'sound_sfc obs, Jo (actual)  = ', &
                                  iv % num_sound_glo, num_sound_tot, &
                                  j % jo % sonde_sfc_u, iv % synop_ef_u, &
                                  j % jo % sonde_sfc_v, iv % synop_ef_v, &
                                  j % jo % sonde_sfc_t, iv % synop_ef_t, &
                                  j % jo % sonde_sfc_p, iv % synop_ef_p, &
                                  j % jo % sonde_sfc_q, iv % synop_ef_q
         end if
      
         if ( iv % num_airep_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    airep obs, Jo (actual)  = ', &
                                  iv % num_airep_glo, num_airep_tot, &
                                  j % jo % airep_u, iv % airep_ef_u, &
                                  j % jo % airep_v, iv % airep_ef_v, &
                                  j % jo % airep_t, iv % airep_ef_t, &
                                  0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_bogus_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    bogus obs, Jo (actual)  = ', &
                                  iv % num_bogus_glo, num_bogus_tot, &
                                  j % jo % bogus_u, iv % bogus_ef_u, &
                                  j % jo % bogus_v, iv % bogus_ef_v, &
                                  j % jo % bogus_t, iv % bogus_ef_t, &
                                  j % jo % bogus_q, iv % bogus_ef_q, &
                                  j % jo % bogus_slp, iv % bogus_ef_slp
         end if

         if ( iv % num_pilot_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    pilot obs, Jo (actual)  = ', &
                                  iv % num_pilot_glo, num_pilot_tot, &
                                  j % jo % pilot_u, iv % pilot_ef_u, &
                                  j % jo % pilot_v, iv % pilot_ef_v, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_ssmi_retrieval_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'     ssmi  obs, Jo (actual) = ', &
                                     iv % num_ssmi_retrieval_glo, num_ssmi_tot, &
                                     j % jo % ssmir_speed, iv % ssmir_ef_speed, &
                                     j % jo % ssmir_tpw, iv % ssmir_ef_tpw, &
                                     0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_satem_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    satem obs, Jo (actual)  = ', &
                                  iv % num_satem_glo, num_satem_tot, &
                                  j % jo % satem_thickness, iv % satem_ef_thickness, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_ssmt1_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    ssmt1 obs, Jo (actual)  = ', &
                                  iv % num_ssmt1_glo, num_ssmt1_tot, &
                                  j % jo % ssmt1_t, iv % ssmt1_ef_t, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if

         if ( iv % num_ssmt2_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    ssmt2 obs, Jo (actual)  = ', &
                                  iv % num_ssmt2_glo, num_ssmt2_tot, &
                                  j % jo % ssmt2_rh, iv % ssmt2_ef_rh, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if
         
         if ( iv % num_qscat_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    qscat obs, Jo (actual)  = ', &
                                  iv % num_qscat_glo, num_qscat_tot, &
                                  j % jo % qscat_u, iv % qscat_ef_u, &
                                  j % jo % qscat_v, iv % qscat_ef_v, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if
      
         if ( iv % num_buoy_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    buoy obs, Jo (actual)  = ', &
                                  iv % num_buoy_glo, num_buoy_tot, &
                                  j % jo % buoy_u, iv % buoy_ef_u, &
                                  j % jo % buoy_v, iv % buoy_ef_v, &
                                  j % jo % buoy_t, iv % buoy_ef_t, &
                                  j % jo % buoy_p, iv % buoy_ef_p, &
                                  j % jo % buoy_q, iv % buoy_ef_q                                
         end if

         if ( iv % num_profiler_glo > 0 ) then
            write(jo_unit,'(a30,2i8,10f15.5)')'    profiler obs, Jo (actual)  = ', &
                                  iv % num_profiler_glo, num_profiler_tot, &
                                  j % jo % profiler_u, iv % profiler_ef_u, &
                                  j % jo % profiler_v, iv % profiler_ef_v, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
         end if
      end if
      
end subroutine da_get_var_diagnostics
