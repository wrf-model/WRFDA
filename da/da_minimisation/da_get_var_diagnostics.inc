subroutine da_get_var_diagnostics( iv, j)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(ob_type), intent(inout):: iv      ! innovation vector.
   type(j_type), intent(inout) :: j       ! Cost function.

   integer                      :: num_obs_tot
   real                         :: dum1, dum2, dum3, dum4, dum5, dum6
   integer                      :: i,k
   real                         :: jo_radiance
   real                         :: temp(63)

   if (trace_use) call da_trace_entry("da_get_var_diagnostics")

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

   temp(1)  = j % jo % synop_u
   temp(2)  = j % jo % synop_v
   temp(3)  = j % jo % synop_t
   temp(4)  = j % jo % synop_p
   temp(5)  = j % jo % synop_q
   temp(6)  = j % jo % metar_u
   temp(7)  = j % jo % metar_v
   temp(8)  = j % jo % metar_t
   temp(9)  = j % jo % metar_p
   temp(10) = j % jo % metar_q
   temp(11) = j % jo % ships_u
   temp(12) = j % jo % ships_v
   temp(13) = j % jo % ships_t
   temp(14) = j % jo % ships_p
   temp(15) = j % jo % ships_q
   temp(16) = j % jo % geoamv_u
   temp(17) = j % jo % geoamv_v
   temp(18) = j % jo % polaramv_u
   temp(19) = j % jo % polaramv_v      
   temp(20) = j % jo % gpspw_tpw       
   temp(21) = j % jo % gpsref_ref      
   temp(22) = j % jo % sound_u         
   temp(23) = j % jo % sound_v         
   temp(24) = j % jo % sound_t         
   temp(25) = j % jo % sound_q         
   temp(26) = j % jo % sonde_sfc_u     
   temp(27) = j % jo % sonde_sfc_v     
   temp(28) = j % jo % sonde_sfc_t     
   temp(29) = j % jo % sonde_sfc_p     
   temp(30) = j % jo % sonde_sfc_q     
   temp(31) = j % jo % airep_u         
   temp(32) = j % jo % airep_v         
   temp(33) = j % jo % airep_t         
   temp(34) = j % jo % pilot_u         
   temp(35) = j % jo % pilot_v         
   temp(36) = j % jo % bogus_u         
   temp(37) = j % jo % bogus_v         
   temp(38) = j % jo % bogus_t         
   temp(39) = j % jo % bogus_q         
   temp(40) = j % jo % bogus_slp       
   temp(41) = j % jo % ssmir_speed     
   temp(42) = j % jo % ssmir_tpw       
   temp(43) = j % jo % ssmi_tb19v      
   temp(44) = j % jo % ssmi_tb19h      
   temp(45) = j % jo % ssmi_tb22v      
   temp(46) = j % jo % ssmi_tb37v      
   temp(47) = j % jo % ssmi_tb37h      
   temp(48) = j % jo % ssmi_tb85v      
   temp(49) = j % jo % ssmi_tb85h      
   temp(50) = j % jo % satem_thickness 
   temp(51) = j % jo % ssmt1_t         
   temp(52) = j % jo % ssmt2_rh        
   temp(53) = j % jo % qscat_u         
   temp(54) = j % jo % qscat_v         
   temp(55) = j % jo % profiler_u      
   temp(56) = j % jo % profiler_v      
   temp(57) = j % jo % buoy_u          
   temp(58) = j % jo % buoy_v          
   temp(59) = j % jo % buoy_t          
   temp(60) = j % jo % buoy_p          
   temp(61) = j % jo % buoy_q          
   temp(62) = j % jo % airsr_t         
   temp(63) = j % jo % airsr_q         

   call da_proc_sum_real(temp(:))

   j % jo % synop_u         = temp(1)  
   j % jo % synop_v         = temp(2)  
   j % jo % synop_t         = temp(3)  
   j % jo % synop_p         = temp(4)  
   j % jo % synop_q         = temp(5)  
   j % jo % metar_u         = temp(6)  
   j % jo % metar_v         = temp(7)  
   j % jo % metar_t         = temp(8)  
   j % jo % metar_p         = temp(9)  
   j % jo % metar_q         = temp(10) 
   j % jo % ships_u         = temp(11) 
   j % jo % ships_v         = temp(12) 
   j % jo % ships_t         = temp(13) 
   j % jo % ships_p         = temp(14) 
   j % jo % ships_q         = temp(15) 
   j % jo % geoamv_u        = temp(16) 
   j % jo % geoamv_v        = temp(17) 
   j % jo % polaramv_u      = temp(18) 
   j % jo % polaramv_v      = temp(19) 
   j % jo % gpspw_tpw       = temp(20) 
   j % jo % gpsref_ref      = temp(21) 
   j % jo % sound_u         = temp(22) 
   j % jo % sound_v         = temp(23) 
   j % jo % sound_t         = temp(24) 
   j % jo % sound_q         = temp(25) 
   j % jo % sonde_sfc_u     = temp(26) 
   j % jo % sonde_sfc_v     = temp(27) 
   j % jo % sonde_sfc_t     = temp(28) 
   j % jo % sonde_sfc_p     = temp(29) 
   j % jo % sonde_sfc_q     = temp(30) 
   j % jo % airep_u         = temp(31) 
   j % jo % airep_v         = temp(32) 
   j % jo % airep_t         = temp(33) 
   j % jo % pilot_u         = temp(34) 
   j % jo % pilot_v         = temp(35) 
   j % jo % bogus_u         = temp(36) 
   j % jo % bogus_v         = temp(37) 
   j % jo % bogus_t         = temp(38) 
   j % jo % bogus_q         = temp(39) 
   j % jo % bogus_slp       = temp(40) 
   j % jo % ssmir_speed     = temp(41) 
   j % jo % ssmir_tpw       = temp(42) 
   j % jo % ssmi_tb19v      = temp(43) 
   j % jo % ssmi_tb19h      = temp(44) 
   j % jo % ssmi_tb22v      = temp(45) 
   j % jo % ssmi_tb37v      = temp(46) 
   j % jo % ssmi_tb37h      = temp(47) 
   j % jo % ssmi_tb85v      = temp(48) 
   j % jo % ssmi_tb85h      = temp(49) 
   j % jo % satem_thickness = temp(50) 
   j % jo % ssmt1_t         = temp(51) 
   j % jo % ssmt2_rh        = temp(52) 
   j % jo % qscat_u         = temp(53) 
   j % jo % qscat_v         = temp(54) 
   j % jo % profiler_u      = temp(55) 
   j % jo % profiler_v      = temp(56) 
   j % jo % buoy_u          = temp(57) 
   j % jo % buoy_v          = temp(58) 
   j % jo % buoy_t          = temp(59) 
   j % jo % buoy_p          = temp(60) 
   j % jo % buoy_q          = temp(61) 
   j % jo % airsr_t         = temp(62) 
   j % jo % airsr_q         = temp(63) 

   if (use_radiance) then
      jo_radiance = 0.0
      do i = 1, iv%num_inst                 ! loop for sensor
         call da_proc_sum_ints(j % jo % rad(i)% num_ichan(:))
         call da_proc_sum_real(j % jo % rad(i) % jo_ichan(:))
         jo_radiance = jo_radiance + sum(j % jo % rad(i) % jo_ichan(:))
      end do
   end if

   !-----------------------------------------------------------------------------
   ! [2.0] Print out VAR diagnostics:
   !-----------------------------------------------------------------------------

   if (rootproc) then

      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt=*) 'Diagnostics'
      write(unit=stdout,fmt='(A,F12.2)')   '   Final cost function J       = ', j % total
      write(unit=stdout,fmt=*) ' '

      write(unit=stdout,fmt='(a,i8)')    '   Total number of obs.        = ', num_obs_tot
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of J            = ', j % total
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jo           = ', j % jo % total
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jb           = ', j % jb
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Je           = ', j % je
      if (num_obs_tot > 0) &
         write(unit=stdout,fmt='(a,f15.5)') '   Final J / total num_obs     = ', j % total / &
                                                          real(num_obs_tot)
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used(1)          = ', var_scaling1
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used(2)          = ', var_scaling2
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used(3)          = ', var_scaling3
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used(4)          = ', var_scaling4
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used(5)          = ', var_scaling5
      write(unit=stdout,fmt='(a,f15.5)') '   Jb factor used              = ', jb_factor
      write(unit=stdout,fmt='(a,f15.5)') '   Je factor used              = ', je_factor
      write(unit=stdout,fmt=*) ' '

      if (use_radiance) then
         write(unit=stdout,fmt='(a,i8)')    '   Total number of radiances    = ', num_radiance_tot
         write(unit=stdout,fmt='(a,f15.5)') '   Cost function for radiances  = ', jo_radiance
      end if

      ! [4.2] Output components of Jo:

      if (iv % num_synop_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    synop obs, Jo(actual)  = ', &
                                  iv % num_synop_glo, num_synop_tot, &
                                  j % jo % synop_u, iv % synop_ef_u, &
                                  j % jo % synop_v, iv % synop_ef_v, &
                                  j % jo % synop_t, iv % synop_ef_t, &
                                  j % jo % synop_p, iv % synop_ef_p, &
                                  j % jo % synop_q, iv % synop_ef_q

      end if

      if (trace_use) call da_trace("da_get_var_diagnostics", &
         message="Memory increase from internal write")

      if (iv % num_metar_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    metar obs, Jo(actual)  = ', &
                               iv % num_metar_glo, num_metar_tot, &
                               j % jo % metar_u, iv % metar_ef_u, &
                               j % jo % metar_v, iv % metar_ef_v, &
                               j % jo % metar_t, iv % metar_ef_t, &
                               j % jo % metar_p, iv % metar_ef_p, &
                               j % jo % metar_q, iv % metar_ef_q    
      end if

      if (iv % num_ships_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ships obs, Jo(actual)  = ', &
                               iv % num_ships_glo, num_ships_tot, &
                               j % jo % ships_u, iv % ships_ef_u, &
                               j % jo % ships_v, iv % ships_ef_v, &
                               j % jo % ships_t, iv % ships_ef_t, &
                               j % jo % ships_p, iv % ships_ef_p, &
                               j % jo % ships_q, iv % ships_ef_q                                
      end if


      if (iv % num_geoamv_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    geoamv ob, Jo(actual)  = ', &
                               iv % num_geoamv_glo, num_geoamv_tot, &
                               j % jo % geoamv_u, iv % geoamv_ef_u, &
                               j % jo % geoamv_v, iv % geoamv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_polaramv_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    polaramv,  Jo(actual)  = ', &
                               iv % num_polaramv_glo, num_polaramv_tot, &
                               j % jo % polaramv_u, iv % polaramv_ef_u, &
                               j % jo % polaramv_v, iv % polaramv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if


      if (iv % num_gpspw_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    gpspw obs, Jo(actual)  = ', &
                               iv % num_gpspw_glo, num_gpspw_tot, &
                               j % jo % gpspw_tpw, iv % gpspw_ef_tpw, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_gpsref_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    gpsref obs, Jo(actual)  = ', &
                               iv % num_gpsref_glo, num_gpsref_tot, &
                               j % jo % gpsref_ref, iv % gpsref_ef_ref, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_sound_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    sound obs, Jo(actual)  = ', &
                               iv % num_sound_glo, num_sound_tot, &
                               j % jo % sound_u, iv % sound_ef_u, &
                               j % jo % sound_v, iv % sound_ef_v, &
                               j % jo % sound_t, iv % sound_ef_t, &
                               j % jo % sound_q, iv % sound_ef_q, 0.0, 1.0 
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    sonde obs, Jo(actual)  = ', &
                               iv % num_sound_glo, num_sound_tot, &
                               j % jo % sonde_sfc_u, iv % synop_ef_u, &
                               j % jo % sonde_sfc_v, iv % synop_ef_v, &
                               j % jo % sonde_sfc_t, iv % synop_ef_t, &
                               j % jo % sonde_sfc_p, iv % synop_ef_p, &
                               j % jo % sonde_sfc_q, iv % synop_ef_q
      end if

      if (iv % num_airep_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    airep obs, Jo(actual)  = ', &
                               iv % num_airep_glo, num_airep_tot, &
                               j % jo % airep_u, iv % airep_ef_u, &
                               j % jo % airep_v, iv % airep_ef_v, &
                               j % jo % airep_t, iv % airep_ef_t, &
                               0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_bogus_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    bogus obs, Jo(actual)  = ', &
                               iv % num_bogus_glo, num_bogus_tot, &
                               j % jo % bogus_u, iv % bogus_ef_u, &
                               j % jo % bogus_v, iv % bogus_ef_v, &
                               j % jo % bogus_t, iv % bogus_ef_t, &
                               j % jo % bogus_q, iv % bogus_ef_q, &
                               j % jo % bogus_slp, iv % bogus_ef_slp
      end if

      if (iv % num_pilot_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    pilot obs, Jo(actual)  = ', &
                               iv % num_pilot_glo, num_pilot_tot, &
                               j % jo % pilot_u, iv % pilot_ef_u, &
                               j % jo % pilot_v, iv % pilot_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmi_retrieval_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'     ssmi  obs, Jo(actual) = ', &
                                  iv % num_ssmi_retrieval_glo, num_ssmi_tot, &
                                  j % jo % ssmir_speed, iv % ssmir_ef_speed, &
                                  j % jo % ssmir_tpw, iv % ssmir_ef_tpw, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_satem_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    satem obs, Jo(actual)  = ', &
                               iv % num_satem_glo, num_satem_tot, &
                               j % jo % satem_thickness, iv % satem_ef_thickness, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmt1_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ssmt1 obs, Jo(actual)  = ', &
                               iv % num_ssmt1_glo, num_ssmt1_tot, &
                               j % jo % ssmt1_t, iv % ssmt1_ef_t, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_ssmt2_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ssmt2 obs, Jo(actual)  = ', &
                               iv % num_ssmt2_glo, num_ssmt2_tot, &
                               j % jo % ssmt2_rh, iv % ssmt2_ef_rh, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_qscat_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    qscat obs, Jo(actual)  = ', &
                               iv % num_qscat_glo, num_qscat_tot, &
                               j % jo % qscat_u, iv % qscat_ef_u, &
                               j % jo % qscat_v, iv % qscat_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv % num_buoy_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    buoy  obs, Jo(actual)  = ', &
                               iv % num_buoy_glo, num_buoy_tot, &
                               j % jo % buoy_u, iv % buoy_ef_u, &
                               j % jo % buoy_v, iv % buoy_ef_v, &
                               j % jo % buoy_t, iv % buoy_ef_t, &
                               j % jo % buoy_p, iv % buoy_ef_p, &
                               j % jo % buoy_q, iv % buoy_ef_q                                
      end if

      if (iv % num_profiler_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    profiler,  Jo(actual)  = ', &
                               iv % num_profiler_glo, num_profiler_tot, &
                               j % jo % profiler_u, iv % profiler_ef_u, &
                               j % jo % profiler_v, iv % profiler_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      if (iv % num_airsr_glo > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    airsr obs, Jo(actual)  = ', &
                               iv % num_airsr_glo, num_airsr_tot, &
                               j % jo % airsr_t, iv % airsr_ef_t, &
                               j % jo % airsr_q, iv % airsr_ef_q, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      do i = 1, iv%num_inst                 ! loop for sensor
       do k = 1, iv%instid(i)%nchan
          if (j % jo % rad(i) % num_ichan(k) > 0) then
            write(unit=jo_unit,fmt='(a30,a16,i5,i10,8f15.5)')'    radiance,  Jo(actual)  = ', &
                               iv%instid(i)%rttovid_string, k , &
                               j % jo % rad(i) % num_ichan(k), &
                               j % jo % rad(i) % jo_ichan(k)
          end if
       end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_var_diagnostics")
      
end subroutine da_get_var_diagnostics


