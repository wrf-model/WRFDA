subroutine da_get_innov_vector (it, ob, iv, grid, config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: it
   type(y_type),               intent(inout) :: ob ! Observations.
   type(iv_type),              intent(inout) :: iv ! Innovation vector(O-B).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   type(xbx_type)     :: xbx          ! Header & non-gridded vars.

   character(len=120) :: filename

   integer            :: n

   if (trace_use) call da_trace_entry("da_get_innov_vector") 

   call da_message((/"Calculate innovation vector(iv)"/))

   iv%ptop = grid%xb%ptop

   filename = ' '
   
   do n=1, num_fgat_time
      iv%time = n
      iv%info(:)%n1 = iv%plocal(iv%time-1,:) + 1
      iv%info(:)%n2 = iv%plocal(iv%time,:)

      if (num_fgat_time > 1) then
         write(unit=filename(1:10), fmt='(a, i2.2)') 'fg', n
         call da_med_initialdata_input (grid , config_flags, filename)
         call da_setup_firstguess (xbx, grid)
      end if

      if (iv%info(sound)%nlocal > 0) then
         call da_get_innov_vector_sound     (it, grid, ob, iv)
         call da_get_innov_vector_sonde_sfc (it, grid, ob, iv)
      end if

      if (iv%nlocal(synop)          > 0) call da_get_innov_vector_synop    (it, grid, ob, iv)
      if (iv%info(geoamv)%nlocal         > 0) call da_get_innov_vector_geoamv   (it, grid, ob, iv)
      if (iv%info(polaramv)%nlocal       > 0) call da_get_innov_vector_polaramv (it, grid, ob, iv)
      if (iv%nlocal(airep)          > 0) call da_get_innov_vector_airep    (it, grid, ob, iv)
      if (iv%nlocal(pilot)          > 0) call da_get_innov_vector_pilot    (it, grid, ob, iv)
      if (iv%nlocal(bogus)          > 0) call da_get_innov_vector_bogus    (it, grid, ob, iv)
      if (iv%info(metar)%nlocal          > 0) call da_get_innov_vector_metar    (it, grid, ob, iv)
      if (iv%nlocal(ships)          > 0) call da_get_innov_vector_ships    (it, grid, ob, iv)
      if (iv%info(gpspw)%nlocal          > 0) call da_get_innov_vector_gpspw    (it, grid, ob, iv)
      if (iv%info(gpsref)%nlocal         > 0) call da_get_innov_vector_gpsref   (it, grid, ob, iv)
      if (iv%nlocal(ssmi_tb)        > 0) call da_get_innov_vector_ssmi_tb  (it, grid, ob, iv) 
      if (iv%nlocal(ssmi_rv) > 0) call da_get_innov_vector_ssmi_rv  (it, grid, ob, iv)
      if (iv%info(ssmt1)%nlocal          > 0) call da_get_innov_vector_ssmt1    (it, grid, ob, iv)
      if (iv%nlocal(ssmt2)          > 0) call da_get_innov_vector_ssmt2    (it, grid, ob, iv)
      if (iv%info(satem)%nlocal          > 0) call da_get_innov_vector_satem    (grid, it,  ob, iv)
      if (iv%info(radar)%nlocal          > 0) call da_get_innov_vector_radar    (it, grid, ob, iv)
      if (iv%nlocal(qscat)          > 0) call da_get_innov_vector_qscat    (it, grid, ob, iv)
      if (iv%info(profiler)%nlocal       > 0) call da_get_innov_vector_profiler (it, grid, ob, iv)
      if (iv%nlocal(buoy)           > 0) call da_get_innov_vector_buoy     (it, grid, ob, iv)

      if (iv%num_inst > 0) then
         if (rtm_option == rtm_option_rttov) then
            call da_get_innov_vector_rttov( it, grid, ob, iv )
         elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
           !if (use_crtm_kmatrix_fast) then
           !   call da_get_innov_vector_crtmk( it, grid, ob, iv )
           !else
            call da_get_innov_vector_crtm (it, grid, ob, iv )
           !end if
#endif
         else
            call da_warning(__FILE__,__LINE__,(/"Unknown Radiative Transfer Model"/))
         end if
      end if

      if (num_pseudo   > 0) call da_get_innov_vector_pseudo (grid, ob, iv)
      if (iv%nlocal(airsr) > 0) call da_get_innov_vector_airsr  (it, grid, ob, iv)
   end do

   iv%time = 1
   iv%info(:)%n1 = iv%plocal(iv%time-1,:) + 1
   iv%info(:)%n2 = iv%plocal(iv%time,:)

   !-----------------------------------------------------------------------
   ! [2] Having calculated the real O-Bs, optionally overwrite with scaled,
   !    random values:
   !----------------------------------------------------------------------- 
   
   if (omb_set_rand) call da_random_omb_all( iv, ob)
   
   !------------------------------------------------------------------------  
   ! [3] Optionally rescale observation errors:
   !------------------------------------------------------------------------ 
   
   if (use_obs_errfac) call da_use_obs_errfac( iv)

   !------------------------------------------------------------------------  
   ! [4] Optionally add Gaussian noise to O, O-B:
   !------------------------------------------------------------------------ 

   if (omb_add_noise) then
      call da_add_noise_to_ob( iv, ob)
   !#ifdef DM_PARALLEL
   !      if ((num_procs > 1) .and.(.not. use_rad)) call da_write_noise_to_ob(iv)
   !      if ((.not. use_rad)) call da_write_noise_to_ob(iv)
      call da_write_noise_to_ob(iv)
   !#endif
   end if

   !----------------------------------------------
   ! [5]  write out radiance iv in ascii format
   !-----------------------------------------------
   if (write_iv_rad_ascii) then
      write(unit=stdout,fmt='(A)')  'Writing radiance iv ascii'
      call da_write_iv_rad_ascii(ob,iv)
   end if

   !----------------------------------------------------------
   ! [6]  write out filtered radiance obs in binary format
   !----------------------------------------------------------

   if (write_filtered_rad) then
      write(unit=stdout,fmt='(A)') 'Writing filtered radiance'
      call da_write_filtered_rad(ob,iv)
   end if

   if (num_fgat_time > 1) then
      call da_med_initialdata_input( grid , config_flags, 'fg01')
      call da_setup_firstguess(xbx, grid)
   end if

   if (trace_use) call da_trace_exit("da_get_innov_vector")

end subroutine da_get_innov_vector


