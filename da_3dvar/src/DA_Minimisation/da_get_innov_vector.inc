SUBROUTINE da_get_innov_vector( it, ob, iv, &
                                grid, config_flags, &
#include <em_dummy_args.inc>
                 )


!----------------------------------------------------------------------------
!--Purpose: Compute O-B for different observation types:
!           03/04/2005 - AMV's from Geostationary and       Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!
!----------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,        INTENT(IN)    :: it
   TYPE (y_type),  INTENT(INOUT) :: ob           ! Observations.
   TYPE (ob_type), INTENT(INOUT) :: iv           ! Innovation vector (O-B).
   TYPE(domain) ,  INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type)   :: config_flags

!--Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   TYPE(xbx_type)     :: xbx          ! Header & non-gridded vars.

   CHARACTER(len=120) :: filename

   integer            :: n

!----------------------------------------------------------------------------   

   write (6,'(/,A)') ' --------------------------------------'
   write (6,'(A)')   ' [5.1] Calculate innovation vector (iv)'
   write (6,'(A)')   ' --------------------------------------'
   write(6,*)

   iv%ptop    = xb%ptop

   filename = ' '
   
   do n=1, num_fgat_time
      iv%current_ob_time = n

      if(num_fgat_time > 1) then
         write(filename(1:10), fmt='(a, i2.2)') 'fgat_fg.', n
         call med_initialdata_input_3dvar ( grid , config_flags, in_filename=filename )
         call DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                           )
      endif

!-----Radiosonde:

      call da_get_innov_vector_sound( it, xb, xp, ob, iv )
      call da_get_innov_vector_sonde_sfc( it, xb, xp, ob, iv )

!-----Surface

      call da_get_innov_vector_synop( it, xb, xp, ob, iv )

!-----Geostationary Satellite AMVs:

      call da_get_innov_vector_geoamv( it, xb, xp, ob, iv )


!-----Polar orbitting Satellite AMVs:

      call da_get_innov_vector_polaramv( it, xb, xp, ob, iv )

!-----Aireps:

      call da_get_innov_vector_airep( it, xb, xp, ob, iv )

!-----Pilot:

      call da_get_innov_vector_pilot( it, xb, xp, ob, iv )

!-----TC bogus

      call da_get_innov_vector_bogus( it, xb, xp, ob, iv )

!-----Metar:

      call da_get_innov_vector_metar( it, xb, xp, ob, iv )

!-----Ships:

      call da_get_innov_vector_ships( it, xb, xp, ob, iv )

!-----GPSPW:

      call da_get_innov_vector_gpspw( it, xb, xp, ob, iv )

!-----GPSRF: 
 
      CALL da_get_innov_vector_gpsref( it, xb, xp, ob, iv ) 

!-----SSMI:

      call da_get_innov_vector_ssmi( it, xb, xp, ob, iv )

!-----SSMT1:

      call da_get_innov_vector_ssmt1( it, xb, xp, ob, iv )

!-----SSMT2:

      call da_get_innov_vector_ssmt2( it, xb, xp, ob, iv )

!-----SATEM:

      call da_get_innov_vector_satem( it, xb, xp, ob, iv )

!-----Radar obs:

      call da_get_innov_vector_Radar( it, xb, xp, ob, iv )

!-----Scatterometer:

      call da_get_innov_vector_qscat( it, xb, xp, ob, iv )

!-----Profiler:

      call da_get_innov_vector_profiler( it, xb, xp, ob, iv )

!-----Buoy:

      call da_get_innov_vector_buoy( it, xb, xp, ob, iv )

!-----Pseudo obs:

      call da_get_innov_vector_pseudo( xb, xp, ob, iv )

   enddo

   iv%current_ob_time = 1

!---------------------------------------------------------------------------  
!--[2] Having calculated the real O-Bs, optionally overwrite with scaled,
!--    random values:
!--------------------------------------------------------------------------- 
   
   if ( omb_set_rand ) CALL da_random_omb_all( iv, ob )
   
!---------------------------------------------------------------------------  
!--[3] Optionally rescale observation errors:
!--------------------------------------------------------------------------- 
   
   if ( use_obs_errfac ) CALL da_use_obs_errfac( iv )

!---------------------------------------------------------------------------  
!--[4] Optionally add Gaussian noise to O, O-B:
!--------------------------------------------------------------------------- 

   if ( omb_add_noise ) CALL da_add_noise_to_ob( iv, ob )

!--------------------------------------------------------------------------- 

   if(num_fgat_time > 1) then
      call med_initialdata_input_3dvar ( grid , config_flags )
      call DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                              )
   endif

 if (ANALYSIS_TYPE == 'VERIFY' .or. ANALYSIS_TYPE == 'verify') then
 write(*,'(/a,i5,a,i5)') "OBS below lowest  level: p#=",p_below,"  h#=",h_below
 write(*,'(a,i5,a,i5/)') "OBS above highest level: p#=",p_above,"  h#=",h_above
 endif

END SUBROUTINE da_get_innov_vector

