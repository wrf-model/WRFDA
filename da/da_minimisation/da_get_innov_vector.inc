subroutine da_get_innov_vector( it, ob, iv, &
                                grid, config_flags, &
#include "em_dummy_new_args.inc"
                 )
   IMPLICIT NONE

   INTEGER,        INTENT(IN)    :: it
   TYPE (y_type),  INTENT(INOUT) :: ob           ! Observations.
   TYPE (ob_type), INTENT(INOUT) :: iv           ! Innovation vector (O-B).
   TYPE(domain) ,  INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   ! Definitions of dummy arguments to solve
#include "em_dummy_new_decl.inc"

   TYPE(xbx_type)     :: xbx          ! Header & non-gridded vars.

   CHARACTER(len=120) :: filename

   integer            :: n

   IF (trace_use) CALL da_trace_entry("da_get_innov_vector") 

   CALL da_messages((/"[5.1] Calculate innovation vector (iv)"/))

   iv%ptop    = grid%xb%ptop

   filename = ' '
   
   do n=1, num_fgat_time
      iv%current_ob_time = n

      if (num_fgat_time > 1) then
         write(UNIT=filename(1:10), fmt='(a, i2.2)') 'fg', n
         call da_med_initialdata_input ( grid , config_flags, &
            filename )
         call da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                           )
      endif

      ! Radiosonde:

      call da_get_innov_vector_sound( it, grid%xb, grid%xp, ob, iv )
      call da_get_innov_vector_sonde_sfc( it, grid%xb, grid%xp, ob, iv )

      ! Surface

      call da_get_innov_vector_synop( it, grid%xb, grid%xp, ob, iv )

      ! Geostationary Satellite AMVs:

      call da_get_innov_vector_geoamv( it, grid%xb, grid%xp, ob, iv )

      ! Polar orbitting Satellite AMVs:

      call da_get_innov_vector_polaramv( it, grid%xb, grid%xp, ob, iv )

      ! Aireps:

      call da_get_innov_vector_airep( it, grid%xb, grid%xp, ob, iv )

      ! Pilot:

      call da_get_innov_vector_pilot( it, grid%xb, grid%xp, ob, iv )

      ! TC bogus

      call da_get_innov_vector_bogus( it, grid%xb, grid%xp, ob, iv )

      ! Metar:

      call da_get_innov_vector_metar( it, grid%xb, grid%xp, ob, iv )

      ! Ships:

      call da_get_innov_vector_ships( it, grid%xb, grid%xp, ob, iv )

      ! GPSPW:

      call da_get_innov_vector_gpspw( it, grid%xb, grid%xp, ob, iv )

      ! GPSRF: 
 
      CALL da_get_innov_vector_gpsref( it, grid%xb, grid%xp, ob, iv ) 

      ! SSMI:

      call da_get_innov_vector_ssmi( it, grid%xb, ob, iv )

      ! SSMT1:

      call da_get_innov_vector_ssmt1( it, grid%xb, grid%xp, ob, iv )

      ! SSMT2:

      call da_get_innov_vector_ssmt2( it, grid%xb, grid%xp, ob, iv )

      ! SATEM:

      call da_get_innov_vector_satem( it, grid%xb, grid%xp, ob, iv )

      ! Radar obs:

      call da_get_innov_vector_radar( it, grid%xb, grid%xp, ob, iv )

      ! Scatterometer:

      call da_get_innov_vector_qscat( it, grid%xb, grid%xp, ob, iv )

      ! Profiler:

      call da_get_innov_vector_profiler( it, grid%xb, grid%xp, ob, iv )

      ! Buoy:

      call da_get_innov_vector_buoy( it, grid%xb, grid%xp, ob, iv )

      ! Radiance:

      call da_get_innov_vector_rad( it, grid%xb, grid%xp, ob, iv )

      ! Pseudo obs:

      call da_get_innov_vector_pseudo( grid%xb, grid%xp, ob, iv )

      ! AIRS retrievals

      call da_get_innov_vector_airsr( it, grid%xb, grid%xp, ob, iv )
   enddo

   iv%current_ob_time = 1

   !-----------------------------------------------------------------------
   ! [2] Having calculated the real O-Bs, optionally overwrite with scaled,
   !    random values:
   !----------------------------------------------------------------------- 
   
   if ( omb_set_rand ) CALL da_random_omb_all( iv, ob )
   
   !------------------------------------------------------------------------  
   ! [3] Optionally rescale observation errors:
   !------------------------------------------------------------------------ 
   
   if ( use_obs_errfac ) CALL da_use_obs_errfac( iv )

   !------------------------------------------------------------------------  
   ! [4] Optionally add Gaussian noise to O, O-B:
   !------------------------------------------------------------------------ 

   if ( omb_add_noise ) then
      CALL da_add_noise_to_ob( iv, ob )
!#ifdef DM_PARALLEL
!      if( (numb_procs > 1) .and. (.not. use_radiance) ) call da_write_noise_to_ob(iv)
!#endif
   endif

   !----------------------------------------------
   ! [5]  write out radiance iv in ascii format
   !-----------------------------------------------
   if ( lwrite_iv_rad_ascii ) then
     write(UNIT=stdout,FMT=*)  ' writing radiance iv ascii'
     call da_write_iv_rad_ascii(grid%xp,ob,iv)
   end if

   !----------------------------------------------------------
   ! [6]  write out filtered radiance obs in binary format
   !----------------------------------------------------------
   if ( lwrite_filtered_rad ) then
     write(UNIT=stdout,FMT=*) ' writing filtered radiance'
     call da_write_filtered_rad(ob,iv)
   end if

   if (num_fgat_time > 1) then
      call da_med_initialdata_input ( grid , config_flags, 'fg01')
      call da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                             )
   endif

   IF (trace_use) CALL da_trace_exit("da_get_innov_vector")

end subroutine da_get_innov_vector

