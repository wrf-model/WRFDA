!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em_tl ( grid , config_flags , &
!
#include "em_dummy_args.inc"
!
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
   USE module_small_step_em
   USE module_em
   USE module_big_step_utilities_em
   USE module_bc
   USE module_bc_em
   USE module_solvedebug_em
   USE module_physics_addtendc
   USE module_diffusion_em
! Registry generated module
   USE module_state_description
   USE module_radiation_driver
   USE module_surface_driver
   USE module_cumulus_driver
   USE module_microphysics_driver
   USE module_pbl_driver
#ifdef WRF_CHEM
   USE module_input_chem_data
#endif

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   !  WRF state bcs
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! WRF state data

!#include "../phys/physics_drive.int"

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: itmpstep
   INTEGER                         :: sz

! storage for tendencies and decoupled state (generated from Registry)
#include <em_i1_decl.inc>

integer , dimension(grid%sm31:grid%em31,grid%sm33:grid%em31) :: xxxxx , yyyyy , zzzzz , xx1 , xx2 , xx3

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk

#include <bench_solve_em_def.h>

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

!<DESCRIPTION>
!<pre>
! solve_em is the main driver for advancing a grid a single timestep.
! It is a mediation-layer routine -> DM and SM calls are made where 
! needed for parallel processing.  
!
! solve_em can integrate the equations using 3 time-integration methods
!      
!    - 3rd order Runge-Kutta time integration (recommended)
!      
!    - 2nd order Runge-Kutta time integration
!      
!    - Leapfrog time integration
!      (note: the leapfrog scheme is not correctly implemented
!      for most of the physics)
!
! The main sections of solve_em are
!     
! (1) Runge-Kutta (RK) loop
!     
! (2) Non-timesplit physics (i.e., tendencies computed for updating
!     model state variables during the first RK sub-step (loop)
!     
! (3) Small (acoustic, sound) timestep loop - within the RK sub-steps
!     
! (4) Scalar advance for moist and chem scalar variables (and TKE)
!     within the RK sub-steps.
!     
! (5) time-split physics (after the RK step), currently this includes
!     only microphyics
!
! A more detailed description of these sections follows.
!</pre>
!</DESCRIPTION>

#include <bench_solve_em_init.h>

!
!  set leapfrog or runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  De-reference dimension information stored in the grid data structure.

  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  k_start         = kps
  k_end           = kpe

  ijds = min(ids, jds)
  ijde = max(ide, jde)

  num_3d_m        = num_moist
  num_3d_c        = num_chem

   !  Compute these starting and stopping locations for each tile and number of tiles.

  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  itimestep = itimestep + 1

!**********************************************************************
!
!  LET US BEGIN.......
!
!<DESCRIPTION>
!<pre>
! (1) RK integration loop is named the "Runge_Kutta_loop:"
!
!   Predictor-corrector type time integration.
!   Advection terms are evaluated at time t for the predictor step,
!   and advection is re-evaluated with the latest predicted value for
!   each succeeding time corrector step
!
!   2nd order Runge Kutta (rk_order = 2):
!   Step 1 is taken to the midpoint predictor, step 2 is the full step.
!
!   3rd order Runge Kutta (rk_order = 3):
!   Step 1 is taken to from t to dt/3, step 2 is from t to dt/2,
!   and step 3 is from t to dt.
!
!   non-timesplit physics are evaluated during first RK step and
!   these physics tendencies are stored for use in each RK pass.
!</pre>
!</DESCRIPTION>
!**********************************************************************

   write(unit=*, fmt='(2a,2x,a,i8)') &
        'file:', __FILE__, 'line:', __LINE__

   write(unit=*, fmt='(a)') &
        'dummy subroutine solve_em_tl'

   RETURN

END SUBROUTINE solve_em_tl

