!MEDIATION_LAYER:SOLVE_DA

SUBROUTINE da_solve ( grid , config_flags , &
#include "em_dummy_args.inc"
                 )

   ! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
   ! Mediation layer modules
   ! Model layer modules
   USE module_model_constants

   use da_constants
   use da_define_structures
   use da_setup_structures
   use da_test
   use da_tools
   use da_minimisation
   use da_par_util
   use da_tracing
   use da_reporting
   use module_get_file_names ! for system interface on cray

   ! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   TYPE(domain),                intent(inout) :: grid
   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   ! Definitions of dummy arguments to solve
#include "em_dummy_decl.inc"

   TYPE (xbx_type)              :: xbx         ! For header & non-grid arrays.
   TYPE (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   TYPE (y_type)                :: ob          ! Observation structure.
   TYPE (ob_type)               :: iv          ! Obs. increment structure.
   TYPE (y_type)                :: re          ! Residual (o-a) structure.
   TYPE (y_type)                :: y           ! y = H(x_inc) structure.
   INTEGER                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   integer                      :: cv_size
   real                         :: j_grad_norm_target ! TArget j norm.
   integer                      :: iost, ierr

   IF (trace_use) call da_trace_entry("da_solve")

   call wrf_message("***  VARIATIONAL ANALYSIS ***")
#ifdef DM_PARALLEL
   call MPI_COMM_SIZE(MPI_COMM_WORLD, numb_procs, ierr)
#else
   numb_procs = 1
#endif

   !---------------------------------------------------------------------------
   ! If it is verification run set check_max_iv as .false.
   !---------------------------------------------------------------------------

   if ((analysis_type(1:6)=="VERIFY" .or. analysis_type(1:6) == "verify")) then
      check_max_iv = .false.
   endif

   ! Things that used to be done in da_read_namelist

   IF (Use_SynopObs .OR. Use_ShipsObs .OR. Use_MetarObs .OR. Use_PilotObs .OR. &
      Use_ProfilerObs .OR. Use_BuoyObs .OR. Use_SoundObs .OR. Use_BogusObs .OR. &
      Use_RadarObs .OR. Use_Radar_rv .OR. Use_Radar_rf .OR. Use_SatemObs .OR. &
      Use_GeoAMVObs .OR. Use_PolarAMVObs .OR. Use_AirepObs .OR. &
      Use_GpspwObs .OR. Use_GpsrefObs .OR. Use_SsmiRetrievalObs .OR. &
      Use_SsmiTbObs .OR. Use_SSMT1Obs .OR. Use_SSMT2Obs .OR. use_qscatobs) THEN
 
      use_obsgts = .TRUE.
   ELSE
      use_obsgts = .FALSE.
   END IF

   IF (use_Hirs2Obs .OR. use_Hirs3Obs .OR. use_MsuObs .OR. use_AmsuaObs .OR. &
      use_AmsubObs .OR. use_AirsObs .OR. use_Eos_AmsuaObs .OR. &
      use_Eos_RadObs .OR. use_HsbObs .OR. use_kma1dvar .OR. use_filtered_rad) THEN
      use_radiance = .TRUE.
   ELSE
      use_radiance = .FALSE.
   END IF

   ! testing_dm_exact can be set to .true. to force DM_PARALLEL runs 
   ! to produce results that are bitwise-identical regardless of the number of 
   ! MPI tasks used.  This is useful for validating that multi-processor runs 
   ! are not a source of bugs.  Runtime will be much longer.  This option is 
   ! automatically overridden to .false. for serial or 1-MPI-task runs.  

   IF (testing_dm_exact) THEN
      call wrf_get_nproc(nproc)
      IF (nproc == 1) THEN
         testing_dm_exact = .FALSE.
         WRITE(UNIT=stdout,FMT='(A)') &
            ' testing_dm_exact overridden to .FALSE. for serial or 1-MPI-task run'
      ENDIF
      ! Only implmenented for Sound and Synop, so switch other types off
      Use_ShipsObs         = .FALSE.
      Use_MetarObs         = .FALSE.
      Use_BogusObs         = .FALSE.
      Use_PilotObs         = .FALSE.
      Use_AirepObs         = .FALSE.
      Use_GeoAMVObs        = .FALSE.
      Use_PolarAMVObs      = .FALSE.
      Use_BuoyObs          = .FALSE.
      Use_ProfilerObs      = .FALSE.
      Use_SatemObs         = .FALSE.
      Use_GpspwObs         = .FALSE.
      Use_GpsrefObs        = .FALSE.
      Use_SsmiRetrievalObs = .FALSE.
      Use_SsmiTbObs        = .FALSE.
      use_ssmt1obs         = .FALSE.
      use_ssmt2obs         = .FALSE.
      use_qscatobs         = .FALSE.
      use_Hirs2Obs         = .FALSE.
      use_Hirs3Obs         = .FALSE.
      use_MsuObs           = .FALSE.
      use_AmsuaObs         = .FALSE.
      use_AmsubObs         = .FALSE.
      use_AirsObs          = .FALSE.
      use_Eos_AmsuaObs     = .FALSE.
      use_Eos_radObs       = .FALSE.
      use_HsbObs           = .FALSE.
      Use_Obsgts           = .FALSE.
      Use_Radiance         = .FALSE.
   ENDIF
    
   if (num_pseudo > 0) then
      call wrf_message("Single OBS Test:: Turn off all the OBS switches ***")
      Use_SynopObs         = .FALSE.
      Use_ShipsObs         = .FALSE.
      Use_MetarObs         = .FALSE.
      Use_SoundObs         = .FALSE.
      Use_BogusObs         = .FALSE.
      Use_PilotObs         = .FALSE.
      Use_AirepObs         = .FALSE.
      Use_GeoAMVObs        = .FALSE.
      Use_PolarAMVObs      = .FALSE.
      Use_BuoyObs          = .FALSE.
      Use_ProfilerObs      = .FALSE.
      Use_SatemObs         = .FALSE.
      Use_GpspwObs         = .FALSE.
      Use_GpsrefObs        = .FALSE.
      Use_SsmiRetrievalObs = .FALSE.
      Use_SsmiTbObs        = .FALSE.
      use_ssmt1obs         = .FALSE.
      use_ssmt2obs         = .FALSE.
      use_qscatobs         = .FALSE.
      use_Hirs2Obs         = .FALSE.
      use_Hirs3Obs         = .FALSE.
      use_MsuObs           = .FALSE.
      use_AmsuaObs         = .FALSE.
      use_AmsubObs         = .FALSE.
      use_AirsObs          = .FALSE.
      use_Eos_AmsuaObs     = .FALSE.
      use_Eos_radObs       = .FALSE.
      use_HsbObs           = .FALSE.
      Use_Obsgts           = .FALSE.
      Use_Radiance         = .FALSE.
   endif

   IF ( cv_options_hum < 1 .OR. cv_options_hum > 3 ) THEN
      WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call wrf_error_fatal(errmsg(1))
   END IF

   IF ( sfc_assi_options < 1 .OR. sfc_assi_options > 2 ) THEN
      WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
         'Invalid sfc_assi_option = ', sfc_assi_options
      call wrf_error_fatal(errmsg(1))
   END IF

   IF (Use_SsmiRetrievalObs .OR. Use_SsmiTbObs) THEN
      OPEN ( UNIT   = ssmi_iunit,     &
             FORM   = 'FORMATTED',  &
             ACCESS = 'SEQUENTIAL', &
             IOSTAT =  iost,     &
             STATUS = 'OLD')
      CLOSE(UNIT=ssmi_iunit)

      IF (iost /= 0) THEN
         WRITE (UNIT=stderr,FMT='(/,A,I3,A,/)') &
            ' INPUT FILE UNIT ',ssmi_iunit, &
            ' FOR SSMI OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         Use_SsmiRetrievalObs=.false.
         Use_SsmiTbObs=.false.
      END IF 
   END IF 

   IF ( vert_corr == 2 ) THEN
      IF ( vertical_ip < 0 .OR. vertical_ip > 2 ) THEN
         WRITE (UNIT=errmsg(1),FMT='(A,I3)') &
           'Invalid vertical_ip = ', &
           vertical_ip
         call da_warning(__FILE__,__LINE__,errmsg(1:1))
      END IF
   END IF

   IF ( 0.5 * REAL(rf_passes) /= REAL(rf_passes / 2) ) THEN
      WRITE(UNIT=stdout,FMT='(A,I4,A)')'rf_passes = ', &
                         rf_passes, ' .Should be even.'
      rf_passes = INT( REAL( rf_passes / 2 ) )
      WRITE(UNIT=stdout,FMT='(A,I4)') 'Resetting rf_passes = ', rf_passes
   END IF

   if ( analysis_type == 'randomcv' ) then
      ntmax = 0
      write(UNIT=stdout)' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   call da_init_wrfvar( grid, xp, xb, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (xb):
   !---------------------------------------------------------------------------

   call da_setup_firstguess( xbx, grid, &
#include "em_dummy_args.inc"
                           )

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------

   call da_setup_obs_structures( xp, ob, iv )

   !---------------------------------------------------------------------------
   ! [5.0] Set up background errors (be):
   !---------------------------------------------------------------------------

   call da_setup_background_errors( xb, xbx, be, xp, &
                                    its, ite, jts, jte, kts, kte, &
                                    ids, ide, jds, jde, kds, kde )
   cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

   ep % ne = be % ne
   if (be % ne > 0) THEN
      call da_setup_flow_predictors( ide, jde, kde, be % ne, ep )
   end if

   !---------------------------------------------------------------------------
   ! [7.0] Setup control variable (cv):
   !---------------------------------------------------------------------------

   allocate( cvt(1:cv_size) )
   allocate( xhat(1:cv_size) )
   call da_initialize_cv( cv_size, cvt )
   call da_initialize_cv( cv_size, xhat )
      
   call da_zero_vp_type( vv )
   call da_zero_vp_type( vp )

   if ( test_transforms .or. Testing_WRFVAR ) then
      call da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include "em_dummy_args.inc"
                 )

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      allocate( cvt(1:cv_size) )
      allocate( xhat(1:cv_size) )
      call da_initialize_cv( cv_size, cvt )
      call da_initialize_cv( cv_size, xhat )

      call da_check( cv_size, xb, xbx, be, ep, iv, &
                     xa, vv, vp, xp, ob, y, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )
      call da_zero_vp_type( vv )
      call da_zero_vp_type( vp )
   endif

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0

   DO it = 1, max_ext_its

      ! [8.1] Calculate nonlinear model trajectory 

      if (lvar4d) then
         call da_trace("da_solve","Starting da_run_wrfplus_nl.ksh")
         call system("da_run_wrfplus_nl.ksh")
         call da_trace("da_solve","Finished da_run_wrfplus_nl.ksh")
      endif

      ! [8.2] Calculate innovation vector (O-B):

      call da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include "em_dummy_args.inc"
                 )

      if (test_transforms) then
         call da_check( cv_size, xb, xbx, be, ep, iv, &
                        xa, vv, vp, xp, ob, y, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
      end if

      if (testing_wrfvar) then
         call da_check( cv_size, xb, xbx, be, ep, iv, &
                        xa, vv, vp, xp, ob, y, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
      end if

      ! Write "clean" QCed observations if requested:
      if ((analysis_type(1:6) == "QC-OBS" .or. &
           analysis_type(1:6) == "qc-obs")) then
         if (it == 1) then
            CALL da_write_filtered_obs(ob, iv, xb, xp, &
                          grid%moad_cen_lat, grid%stand_lon,&
                          grid%truelat1, grid%truelat2 )
         end if     
      endif

      if (lmonitoring) call wrf_shutdown

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.4] Minimize cost function:

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      call da_minimise_cg( grid, config_flags,                  &
                           it, be % cv % size, & 
                           xb, xbx, be, ep, iv, &
                           j_grad_norm_target, xhat, cvt, &
                           xa, vv, vp, xp, re, y, j,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           its, ite, jts, jte, kts, kte         )

      !------------------------------------------------------------------------

      ! [8.5] Update latest analysis solution:

      call da_transform_vtox( cv_size, xb, xbx, be, ep, xhat, vv, vp, xp, xa,  &
                              ids, ide, jds, jde, kds, kde,             &
                              ims, ime, jms, jme, kms, kme,             &
                              its, ite, jts, jte, kts, kte )

      ! [8.6] Only when use_RadarObs = .false. and W_INCREMENTS =.true.,
      !       the W_increment need to be diagnosed:

      if (W_INCREMENTS .and. .not. use_RadarObs) then
         call da_uvprho_to_w_lin( xb, xa, xp,                 &
                                  ids,ide, jds,jde, kds,kde,  &
                                  ims,ime, jms,jme, kms,kme,  &
                                  its,ite, jts,jte, kts, kte )

         call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id13)
      endif

      ! [8.7] Write out diagnostics

      call da_write_diagnostics( ob, iv, re, y, xp, xa, j )

      ! [8.8] Write Ascii radiance OMB and OMA file

      if ( lwrite_oa_rad_ascii ) then
        write(UNIT=stdout,FMT=*)  ' writing radiance OMB and OMA ascii file'
        CALL da_write_oa_rad_ascii(xp,ob,iv,re)
      end if

      !------------------------------------------------------------------------
      ! [8.0] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis( it, xbx, grid, config_flags ,&
#include "em_dummy_args.inc"
         )
   END DO

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate ( cvt )
   deallocate ( xhat )
   call da_deallocate_obs(iv)
   call da_deallocate_y( re )
   call da_deallocate_y( y )

   call wrf_message("*** WRF-Var completed successfully ***")

   IF (trace_use) call da_trace_exit("da_solve")

CONTAINS

#include "da_init_wrfvar.inc"

end subroutine da_solve

