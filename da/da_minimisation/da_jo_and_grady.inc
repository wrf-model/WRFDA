subroutine da_jo_and_grady(iv, re, jot, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector (O-B).
   type (y_type),  intent(in)   :: re          ! Residual vector (O-A).
   real,           intent(out)  :: jot         ! Obs cost function.
   type (jo_type), intent(out)  :: jo          ! Obs cost function.
   type (y_type),  intent(out)  :: jo_grad_y   ! Grad_y(Jo)


   real    :: jo_sound, jo_synop, jo_geoamv, jo_polaramv, &
              jo_airep, jo_pilot, jo_satem, &
              jo_metar, jo_ships, jo_gpspw, &
              jo_ssmi , jo_ssmt1, jo_ssmt2, &
              jo_pseudo, jo_qscat, jo_buoy, &
              jo_profiler, jo_radar, jo_gpsref, jo_bogus, &
              jo_radiance, jo_airsr
   integer :: i,k

   if (trace_use) call da_trace_entry("da_jo_and_grady")

   !-------------------------------------------------------------------------
   ! [1.0] Compute components of Grad_y(Jo):
   !-------------------------------------------------------------------------

   if (use_soundobs .and. iv%num_sound > 0) then
      call da_jo_and_grady_sound(iv, re, jo, jo_grad_y)
      jo_sound = jo%sound_u + jo%sound_v + jo%sound_t + jo%sound_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_sound	     ', jo_sound, &
            '   jo%sound_u      ', jo%sound_u, &
            '   jo%sound_v      ', jo%sound_v, &
            '   jo%sound_t      ', jo%sound_t, &
            '   jo%sound_q      ', jo%sound_q
      end if

      call da_jo_and_grady_sonde_sfc(iv, re, jo, jo_grad_y)
      jo_sound = jo_sound + jo%sonde_sfc_u + jo%sonde_sfc_v + jo%sonde_sfc_t + &
         jo%sonde_sfc_q + jo%sonde_sfc_p

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_sound	     ', jo_sound, &
            '   jo%sonde_sfc_u  ', jo%sonde_sfc_u, &
            '   jo%sonde_sfc_v  ', jo%sonde_sfc_v, &
            '   jo%sonde_sfc_t  ', jo%sonde_sfc_t, &
            '   jo%sonde_sfc_p  ', jo%sonde_sfc_p, &
            '   jo%sonde_sfc_q  ', jo%sonde_sfc_q
      end if
   else
      jo_sound = 0.0
   end if

   if (use_synopobs .and. iv%num_synop > 0) then
      call da_jo_and_grady_synop(iv, re, jo, jo_grad_y)
      jo_synop = jo%synop_u + jo%synop_v + jo%synop_t + jo%synop_p + jo%synop_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_synop	     ', jo_synop, &
            '   jo%synop_u      ', jo%synop_u, &
            '   jo%synop_v      ', jo%synop_v, &
            '   jo%synop_t      ', jo%synop_t, &
            '   jo%synop_p      ', jo%synop_p, &
            '   jo%synop_q      ', jo%synop_q
      end if
   else
      jo_synop = 0.0
   end if

   if (use_geoamvobs .and. iv%num_geoamv > 0) then
      call da_jo_and_grady_geoamv(iv, re, jo, jo_grad_y)
      jo_geoamv = jo%geoamv_u + jo%geoamv_v
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_geoamv       ', jo_geoamv, &
            '   jo%geoamv_u     ', jo%geoamv_u, &
            '   jo%geoamv_v     ', jo%geoamv_v
      end if
   else
      jo_geoamv = 0.0
   end if

   if (use_polaramvobs .and. iv%num_polaramv > 0) then
      call da_jo_and_grady_polaramv(iv, re, jo, jo_grad_y)
      jo_polaramv = jo%polaramv_u + jo%polaramv_v
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_polaramv     ', jo_polaramv, &
            '   jo%polaramv_u   ', jo%polaramv_u, &
            '   jo%polaramv_v   ', jo%polaramv_v
      end if
   else
      jo_polaramv = 0.0
   end if

   if (use_airepobs .and. iv%num_airep > 0) then
      call da_jo_and_grady_airep(iv, re, jo, jo_grad_y)
      jo_airep = jo%airep_u + jo%airep_v + jo%airep_t
   else
      jo_airep = 0.0
   end if

   if (use_pilotobs .and. iv%num_pilot > 0) then
      call da_jo_and_grady_pilot(iv, re, jo, jo_grad_y)
      jo_pilot = jo%pilot_u + jo%pilot_v
   else
      jo_pilot = 0.0
   end if

   if (use_satemobs .and. iv%num_satem > 0) then
      call da_jo_and_grady_satem(iv, re, jo, jo_grad_y)
      jo_satem = jo%satem_thickness
   else
      jo_satem = 0.0
   end if

   if (use_metarobs .and. iv%num_metar > 0) then
      call da_jo_and_grady_metar(iv, re, jo, jo_grad_y)
      jo_metar = jo%metar_u + jo%metar_v + jo%metar_t + jo%metar_p + jo%metar_q
   else
      jo_metar = 0.0
   end if

   if (use_shipsobs .and. iv%num_ships > 0) then
      call da_jo_and_grady_ships(iv, re, jo, jo_grad_y)
      jo_ships = jo%ships_u + jo%ships_v + jo%ships_t + jo%ships_p + jo%ships_q
   else
      jo_ships = 0.0
   end if

   if (use_gpspwobs .and. iv%num_gpspw > 0) then
      call da_jo_and_grady_gpspw(iv, re, jo, jo_grad_y)
      jo_gpspw = jo%gpspw_tpw
   else
      jo_gpspw = 0.0
   end if

   if (use_gpsrefobs .and. iv%num_gpsref > 0) then
      call da_jo_and_grady_gpsref(iv, re, jo, jo_grad_y)
      jo_gpsref = jo%gpsref_ref
   else
      jo_gpsref = 0.0
   end if

   if (use_ssmiretrievalobs .or. use_ssmitbobs) then
      call da_jo_and_grady_ssmi(iv, re, jo, jo_ssmi, jo_grad_y)
   end if

   if (use_ssmt1obs .and. iv%num_ssmt1 > 0) then
      call da_jo_and_grady_ssmt1(iv, re, jo, jo_grad_y)
      jo_ssmt1 = jo%ssmt1_t
   else
      jo_ssmt1 = 0.0
   end if

   if (use_ssmt2obs .and. iv%num_ssmt2 > 0) then
      call da_jo_and_grady_ssmt2(iv, re, jo, jo_grad_y)  
      jo_ssmt2 = jo%ssmt2_rh
   else
      jo_ssmt2 = 0.0
   end if

   if (use_radarobs .and. iv%num_radar > 0) then
      call da_jo_and_grady_radar(iv, re, jo, jo_grad_y)
      jo_radar = jo%radar_rv + jo%radar_rf
   else
      jo_radar = 0.0
   end if

   if (iv%num_pseudo > 0) then
      call da_jo_and_grady_pseudo(iv, re, jo, jo_grad_y)    
      jo_pseudo = jo%pseudo_u + jo%pseudo_v + jo%pseudo_t + jo%pseudo_p + jo%pseudo_q
   else
      jo_pseudo = 0.0
   end if

   if (use_qscatobs .and. iv%num_qscat > 0) then
      call da_jo_and_grady_qscat(iv, re, jo, jo_grad_y)
      jo_qscat = jo%qscat_u + jo%qscat_v
   else
      jo_qscat = 0.0
   end if

   if (use_profilerobs .and. iv%num_profiler > 0) then
      call da_jo_and_grady_profiler (iv, re, jo, jo_grad_y)
      jo_profiler = jo%profiler_u + jo%profiler_v
   else
      jo_profiler = 0.0
   end if

   if (use_bogusobs .and. iv%num_bogus > 0) then
      call da_jo_and_grady_bogus (iv, re, jo, jo_grad_y)
      jo_bogus = jo%bogus_u + jo%bogus_v + jo%bogus_slp + jo%bogus_t + jo%bogus_q
   else
      jo_bogus = 0.0
   end if

   if (use_buoyobs .and. iv%num_buoy > 0) then
      call da_jo_and_grady_buoy (iv, re, jo, jo_grad_y)
      jo_buoy = jo%buoy_u + jo%buoy_v + jo%buoy_t + jo%buoy_p + jo%buoy_q
   else
      jo_buoy = 0.0
   end if

   if (use_rad .and. iv%num_inst > 0) then
      call da_jo_and_grady_rad (iv, re, jo, jo_grad_y)

      jo_radiance = 0.0 
      if (use_rad) then
         do i=1,iv%num_inst
            do k=1,iv%instid(i)%nchan
               jo_radiance = jo_radiance + jo%rad(i)%jo_ichan(k)
            end do
         end do
      end if
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_radiance     ', jo_radiance
         do i = 1, iv%num_inst
            write(unit=stdout, fmt='(a, e24.12)') &
               trim('   jo_'//iv%instid(i)%rttovid_string), sum(jo%rad(i)%jo_ichan(:))
         end do
      end if
   else
      jo_radiance = 0.0
   end if

   if (use_airsretobs .and. iv%num_airsr > 0) then
      call da_jo_and_grady_airsr(iv, re, jo, jo_grad_y)
      jo_airsr = jo%airsr_t + jo%airsr_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            '   jo_airsr        ', jo_airsr, &
            '   jo%airsr_t      ', jo%airsr_t, &
            '   jo%airsr_q      ', jo%airsr_q
      end if
   else
      jo_airsr = 0.0
   end if

   !-------------------------------------------------------------------------
   ! [2.0] Jo = 1/2 * (yo-y)**2/ob_err_variance:
   !-------------------------------------------------------------------------

   jo%total = jo_sound + jo_geoamv + jo_polaramv + jo_synop + jo_satem + &
      jo_pilot + jo_airep + jo_metar + jo_ships + &
      jo_gpspw + jo_ssmi + jo_ssmt1 + jo_ssmt2 + &
      jo_pseudo + jo_qscat + jo_profiler + jo_buoy + &
      jo_radar + jo_gpsref + jo_bogus + jo_radiance + jo_airsr

   jot = jo%total

   if (print_detail_grad) then
      write(unit=stdout, fmt='(a, e24.12)') &
         '   jo%total      ', jot

      write(unit=stdout, fmt='(a, e24.12)') &
         '   jo_sound	     ', jo_sound, &
         '   jo_geoamv       ', jo_geoamv, &
         '   jo_polaramv     ', jo_polaramv, &
         '   jo_synop	     ', jo_synop, &
         '   jo_satem	     ', jo_satem, &
         '   jo_pilot	     ', jo_pilot, &
         '   jo_airep	     ', jo_airep, &
         '   jo_metar	     ', jo_metar, &
         '   jo_ships	     ', jo_ships, &
         '   jo_gpspw	     ', jo_gpspw, &
         '   jo_ssmi	     ', jo_ssmi, &
         '   jo_ssmt1	     ', jo_ssmt1, &
         '   jo_ssmt2	     ', jo_ssmt2, &
         '   jo_pseudo       ', jo_pseudo, &
         '   jo_qscat	     ', jo_qscat, &
         '   jo_profil       ', jo_profiler, &
         '   jo_buoy	     ', jo_buoy, &
         '   jo_radar	     ', jo_radar, &
         '   jo_gpsref       ', jo_gpsref, &
         '   jo_bogus	     ', jo_bogus,  &
         '   jo_radiance     ', jo_radiance, &
         '   jo_airsr	     ', jo_airsr	  

   end if

   if (trace_use) call da_trace_exit("da_jo_and_grady")

end subroutine da_jo_and_grady


