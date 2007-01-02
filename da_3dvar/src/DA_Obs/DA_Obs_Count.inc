SUBROUTINE DA_Obs_Count(count_obs, fm, cycle_report, &
                        outside, discard, &
                        inside_halo )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003      Profiler and Buoy Obs            S. R. H. Rizvi
!             05/27/2004      GPS Refractivity                 Y.-R. Guo
!
!----------------------------------------------------------------------------   

   INTEGER,     intent(in) :: fm
   LOGICAL,     intent(in) :: outside, discard
   LOGICAL,     intent(in) :: inside_halo
 
   LOGICAL,                 intent(out)   :: cycle_report
   TYPE (count_obs_type),   intent(inout) :: count_obs

   CHARACTER (LEN = 40)         :: obs_platform
!----------------------------------------------------------------------------   
 
    cycle_report = .false.

    if (outside .or. discard .or. (.not. inside_halo) ) then
       CALL fm_decoder (fm, obs_platform, &
                        count_obs %num_synop % num_outside_iyjx, &
                        count_obs %num_ships % num_outside_iyjx, &
                        count_obs %num_metar % num_outside_iyjx, &
                        count_obs %num_pilot % num_outside_iyjx, &
                        count_obs %num_sound % num_outside_iyjx, &
                        count_obs %num_satem % num_outside_iyjx, &
                        count_obs %num_geoamv % num_outside_iyjx, &
                        count_obs %num_polaramv % num_outside_iyjx, &
                        count_obs %num_airep % num_outside_iyjx, &
                        count_obs %num_gpspw  % num_outside_iyjx, &
                        count_obs %num_gpsref % num_outside_iyjx, &
               count_obs %num_ssmi_retrieval % num_outside_iyjx, &
               count_obs %num_ssmi_tb        % num_outside_iyjx, &
               count_obs %num_ssmt1          % num_outside_iyjx, &
               count_obs %num_ssmt2          % num_outside_iyjx, &
               count_obs %num_qscat          % num_outside_iyjx, &
               count_obs %num_profiler       % num_outside_iyjx, &
               count_obs %num_buoy           % num_outside_iyjx, &
                        count_obs %num_other % num_outside_iyjx)

       cycle_report = .true.
    else

       CALL fm_decoder (fm, obs_platform, &
                        count_obs %num_synop % num_used, &
                        count_obs %num_ships % num_used, &
                        count_obs %num_metar % num_used, &
                        count_obs %num_pilot % num_used, &
                        count_obs %num_sound % num_used, &
                        count_obs %num_satem % num_used, &
                        count_obs %num_geoamv % num_used, &
                        count_obs %num_polaramv % num_used, &
                        count_obs %num_airep % num_used, &
                        count_obs %num_gpspw  % num_used, &
                        count_obs %num_gpsref % num_used, &
               count_obs %num_ssmi_retrieval % num_used, &
               count_obs %num_ssmi_tb        % num_used, &
               count_obs %num_ssmt1          % num_used, &
               count_obs %num_ssmt2          % num_used, &
               count_obs %num_qscat          % num_used, &
               count_obs %num_profiler       % num_used, &
               count_obs %num_buoy           % num_used, &
                        count_obs %num_other % num_used)
    endif

END SUBROUTINE DA_Obs_Count
