subroutine da_get_innov_vector( it, ob, iv, &
                                grid, config_flags, &
#include "em_dummy_new_args.inc"
                )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: it
   type(y_type),  intent(inout) :: ob           ! Observations.
   type(ob_type), intent(inout) :: iv           ! Innovation vector(O-B).
   type(domain) ,  intent(inout) :: grid
   type(grid_config_rec_type), intent(inout)   :: config_flags

   ! Definitions of dummy arguments to solve
#include "em_dummy_new_decl.inc"

   type(xbx_type)     :: xbx          ! Header & non-gridded vars.

   character(len=120) :: filename

   integer            :: n

   if (trace_use) call da_trace_entry("da_get_innov_vector") 

   call da_message((/"Calculate innovation vector(iv)"/))

   iv%ptop    = grid%xb%ptop

   filename = ' '
   
   do n=1, num_fgat_time
      iv%current_ob_time = n

      if (num_fgat_time > 1) then
         write(unit=filename(1:10), fmt='(a, i2.2)') 'fg', n
         call da_med_initialdata_input( grid , config_flags, &
            filename)
         call da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                          )
      end if

      ! Radiosonde:

      call da_get_innov_vector_sound( it, grid%xb, grid%xp, ob, iv)
      call da_get_innov_vector_sonde_sfc( it, grid%xb, grid%xp, ob, iv)

      ! Surface

      call da_get_innov_vector_synop( it, grid%xb, grid%xp, ob, iv)

      ! Geostationary Satellite AMVs:

      call da_get_innov_vector_geoamv( it, grid%xb, grid%xp, ob, iv)

      ! Polar orbitting Satellite AMVs:

      call da_get_innov_vector_polaramv( it, grid%xb, grid%xp, ob, iv)

      ! Aireps:

      call da_get_innov_vector_airep( it, grid%xb, grid%xp, ob, iv)

      ! Pilot:

      call da_get_innov_vector_pilot( it, grid%xb, grid%xp, ob, iv)

      ! TC bogus

      call da_get_innov_vector_bogus( it, grid%xb, grid%xp, ob, iv)

      ! Metar:

      call da_get_innov_vector_metar( it, grid%xb, grid%xp, ob, iv)

      ! Ships:

      call da_get_innov_vector_ships( it, grid%xb, grid%xp, ob, iv)

      ! GPSPW:

      call da_get_innov_vector_gpspw( it, grid%xb, grid%xp, ob, iv)

      ! GPSRF: 
 
      call da_get_innov_vector_gpsref( it, grid%xb, grid%xp, ob, iv) 

      ! SSMI:

      call da_get_innov_vector_ssmi( it, grid%xb, ob, iv)

      ! SSMT1:

      call da_get_innov_vector_ssmt1( it, grid%xb, grid%xp, ob, iv)

      ! SSMT2:

      call da_get_innov_vector_ssmt2( it, grid%xb, grid%xp, ob, iv)

      ! SATEM:

      call da_get_innov_vector_satem( it, grid%xb, grid%xp, ob, iv)

      ! Radar obs:

      call da_get_innov_vector_radar( it, grid%xb, grid%xp, ob, iv)

      ! Scatterometer:

      call da_get_innov_vector_qscat( it, grid%xb, grid%xp, ob, iv)

      ! Profiler:

      call da_get_innov_vector_profiler( it, grid%xb, grid%xp, ob, iv)

      ! Buoy:

      call da_get_innov_vector_buoy( it, grid%xb, grid%xp, ob, iv)

      ! Radiance:

      call da_get_innov_vector_rad(it, grid%xb, grid%xp, ob, iv)

      ! Pseudo obs:

      call da_get_innov_vector_pseudo( grid%xb, grid%xp, ob, iv)

      ! AIRS retrievals

      call da_get_innov_vector_airsr( it, grid%xb, grid%xp, ob, iv)
   end do

   iv%current_ob_time = 1

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
!      if ((num_procs > 1) .and.(.not. use_radiance)) call da_write_noise_to_ob(iv)
!      if ((.not. use_radiance)) call da_write_noise_to_ob(iv)
      call da_write_noise_to_ob(iv)
!#endif
   end if

   !----------------------------------------------
   ! [5]  write out radiance iv in ascii format
   !-----------------------------------------------
   if (write_iv_rad_ascii) then
      write(unit=stdout,fmt=*)  ' writing radiance iv ascii'
      call da_write_iv_rad_ascii(grid%xp,ob,iv)
   end if

   !----------------------------------------------------------
   ! [6]  write out filtered radiance obs in binary format
   !----------------------------------------------------------

   if (write_filtered_rad) then
     write(unit=stdout,fmt=*) ' writing filtered radiance'
     call da_write_filtered_rad(ob,iv)
   end if

   if (num_fgat_time > 1) then
      call da_med_initialdata_input( grid , config_flags, 'fg01')
      call da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                            )
   end if

   if (trace_use) call da_trace_exit("da_get_innov_vector")

end subroutine da_get_innov_vector


