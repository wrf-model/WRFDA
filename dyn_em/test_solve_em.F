
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

SUBROUTINE test_solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
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

   USE g_module_small_step_em
   USE g_module_em
   USE g_module_big_step_utilities_em
   USE g_module_bc
   USE g_module_bc_em
   USE g_module_diffusion_em

!   USE a_module_small_step_em
!   USE a_module_em
!   USE a_module_big_step_utilities_em
!   USE a_module_bc
!   USE a_module_bc_em
!   USE a_module_diffusion_em

   IMPLICIT NONE

!  INTERFACE
! include  <../share/solve_em.int>
! include  <../share/solve_em_tl.int>
! include  <../share/solve_em_ad.int>
!  END INTERFACE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).
#include <em_dummy_decl.inc>

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

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

   INTEGER :: rc
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk

! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

!---------------------------------------------
!---------------------------------------------


   INTEGER :: kts  !zzma

!---------------------------------------------------------------------------------------
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: S_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: S_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: S_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: S_gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_xkmhd


real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: B_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: B_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: B_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: B_gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_xkmhd

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqu
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_phm
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqv
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_pm1
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqw
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_a
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_gamma
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_alpha
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rho
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_c2a


!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------

   REAL :: SAVE_L, COEF, ALPHA_M, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

! Define benchmarking timers if -DBENCH is compiled
#include <bench_solve_em_def.h>

!----------------------
! Executable statements
!----------------------

! Trick problematic compilers into not performing copy-in/copy-out by adding
! indices to array arguments in the CALL statements in this routine.
! It has the effect of passing only the first element of the array, rather
! than the entire array.  See:
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
#include "deref_kludge.h"
! Limit the number of arguments if compiled with -DLIMIT_ARGS by copying
! scalar (non-array) arguments out of the grid data structure into locally
! defined copies (defined in em_dummy_decl.inc, above, as they are if they
! are arguments). An equivalent include of em_scalar_derefs.inc appears
! at the end of the routine to copy back any chnaged non-array values.
! The definition of COPY_IN or COPY_OUT before the include defines the
! direction of the copy.  Em_scalar_derefs.inc is generated from Registry
#define COPY_IN
#include <em_scalar_derefs.inc>

! Needed by some comm layers, e.g. RSL. If needed, nmm_data_calls.inc is
! generated from the registry.  The definition of REGISTER_I1 allows
! I1 data to be communicated in this routine if necessary.
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

! Initialize timers if compiled with -DBENCH
#include <bench_solve_em_init.h>

!  zzma: new definition end

   its = grid%sp31-3 ; ite = grid%ep31+3
   kts = grid%sp32 ; kte = grid%ep32
   jts = grid%sp33-3 ; jte = grid%ep33+3
   print*,'its =',its
   print*,'ite =',ite

   print*,'jts =',jts
   print*,'jte =',jte

   print*,'kts =',kts
   print*,'kte =',kte
   print*,'num_moist =',num_moist
   print*,'grid%ed31 =',grid%ed31
   print*,'grid%ed32 =',grid%ed32
   print*,'grid%spec_bdy_width =',grid%spec_bdy_width

   S_u_1 = u_1 
   S_u_2 = u_2 
   S_v_1 = v_1 
   S_v_2 = v_2 
   S_w_1 = w_1 
   S_w_2 = w_2 
   S_ph_1 = ph_1 
   S_ph_2 = ph_2 
   S_t_1 = t_1 
   S_t_2 = t_2 
   S_mu_1 = mu_1 
   S_mu_2 = mu_2 
   S_p = p 
   S_al = al 
   S_moist_1 = moist_1 
   S_moist_2 = moist_2 
!   S_u_b = u_b 
!   S_u_bt = u_bt 
!   S_v_b = v_b 
!   S_v_bt = v_bt 
!   S_w_b = w_b 
!   S_w_bt = w_bt 
!   S_ph_b = ph_b 
!   S_ph_bt = ph_bt 
!   S_t_b = t_b 
!   S_t_bt = t_bt 
!   S_mu_b = mu_b 
!   S_mu_bt = mu_bt 
!   S_rqv_b = rqv_b 
!   S_rqv_bt = rqv_bt 

!-------------
!  goto 1234   !zzma
!-------------

!NLM

   CALL solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
    B_u_1(i,k,j) = u_1(i,k,j)
    B_u_2(i,k,j) = u_2(i,k,j) 
    B_v_1(i,k,j) = v_1(i,k,j)
    B_v_2(i,k,j) = v_2(i,k,j)
    B_w_1(i,k,j) = w_1(i,k,j)
    B_w_2(i,k,j) = w_2(i,k,j)
    B_ph_1(i,k,j) = ph_1(i,k,j)
    B_ph_2(i,k,j) = ph_2(i,k,j)
    B_t_1(i,k,j) = t_1(i,k,j)
    B_t_2(i,k,j) = t_2(i,k,j)
    B_p(i,k,j) = p(i,k,j)
    B_al(i,k,j) = al(i,k,j)
    B_z(i,k,j) = z(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
    B_mu_1(i,j) =mu_1(i,j)
    B_mu_2(i,j) =mu_2(i,j)
    B_mudf(i,j) =mudf(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
    B_moist_1(i,k,j,h) =moist_1(i,k,j,h)
    B_moist_2(i,k,j,h) =moist_2(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TCL

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!   w_bt = S_w_bt 
!   ph_b = S_ph_b 
!   ph_bt = S_ph_bt 
!   t_b = S_t_b 
!   t_bt = S_t_bt 
!   mu_b = S_mu_b 
!   mu_bt = S_mu_bt 
!   rqv_b = S_rqv_b 
!   rqv_bt = S_rqv_bt 


   g_u_1 = u_1 
   g_u_2 = u_2 
   g_v_1 = v_1 
   g_v_2 = v_2 
   g_w_1 = w_1 
   g_w_2 = w_2 
   g_ph_1 = ph_1 
   g_ph_2 = ph_2 
   g_t_1 = t_1 
   g_t_2 = t_2 
   g_mu_1 = mu_1 
   g_mu_2 = mu_2 
   g_p = p 
   g_al = al 
   g_moist_1 = moist_1 
   g_moist_2 = moist_2 
   g_u_b = 0.0 
   g_u_bt = 0.0 
   g_v_b = 0.0 
   g_v_bt = 0.0 
   g_w_b = 0.0 
   g_w_bt = 0.0 
   g_ph_b = 0.0 
   g_ph_bt = 0.0 
   g_t_b = 0.0 
   g_t_bt = 0.0 
   g_mu_b = 0.0 
   g_mu_bt = 0.0 
   g_rqv_b = 0.0 
   g_rqv_bt = 0.0 

   CALL solve_em_tl ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_actual_args.inc"
!
                    )

   SAVE_L =0.0
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L =SAVE_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                     + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                     + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                     + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                     + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                     + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                     + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                     + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                     + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                     + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                     + g_p(i,k,j)  * g_p(i,k,j)    &
                     + g_al(i,k,j)  * g_al(i,k,j)    &
                     + g_z(i,k,j)  * g_z(i,k,j)    

   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      SAVE_L =SAVE_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                     + g_mu_2(i,j)  * g_mu_2(i,j)    &
                     + g_mudf(i,j)  * g_mudf(i,j)    
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      SAVE_L =SAVE_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                     + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h) 
   enddo
   enddo
   enddo
   enddo

   ALPHA_M=1.
   DO NT=1,11
      ALPHA_M=0.1*ALPHA_M
      FACTOR=1.+ALPHA_M
   u_1 = FACTOR*S_u_1 
   u_2 = FACTOR*S_u_2 
   v_1 = FACTOR*S_v_1 
   v_2 = FACTOR*S_v_2 
   w_1 = FACTOR*S_w_1 
   w_2 = FACTOR*S_w_2 
   ph_1 = FACTOR*S_ph_1 
   ph_2 = FACTOR*S_ph_2 
   t_1 = FACTOR*S_t_1 
   t_2 = FACTOR*S_t_2 
   mu_1 = FACTOR*S_mu_1 
   mu_2 = FACTOR*S_mu_2 
   p = FACTOR*S_p 
   al = FACTOR*S_al 
   moist_1 = FACTOR*S_moist_1 
   moist_2 = FACTOR*S_moist_2 
!   u_b = FACTOR*S_u_b 
!   u_bt = FACTOR*S_u_bt 
!   v_b = FACTOR*S_v_b 
!   v_bt = FACTOR*S_v_bt 
!   w_b = FACTOR*S_w_b 
!   w_bt = FACTOR*S_w_bt 
!   ph_b = FACTOR*S_ph_b 
!   ph_bt = FACTOR*S_ph_bt 
!   t_b = FACTOR*S_t_b 
!   t_bt = FACTOR*S_t_bt 
!   mu_b = FACTOR*S_mu_b 
!   mu_bt = FACTOR*S_mu_bt 
!   rqv_b = FACTOR*S_rqv_b 
!   rqv_bt = FACTOR*S_rqv_bt 


   CALL solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_actual_args.inc"
!
                    )
    VAL_N = 0.0

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N  + (u_1(i,k,j)  - B_u_1(i,k,j) ) *(u_1(i,k,j)  - B_u_1(i,k,j))     &
                   + (u_2(i,k,j)  - B_u_2(i,k,j) ) *(u_2(i,k,j)  - B_u_2(i,k,j))     &
                   + (v_1(i,k,j)  - B_v_1(i,k,j) ) *(v_1(i,k,j)  - B_v_1(i,k,j))     &
                   + (v_2(i,k,j)  - B_v_2(i,k,j) ) *(v_2(i,k,j)  - B_v_2(i,k,j))     &
                   + (w_1(i,k,j)  - B_w_1(i,k,j) ) *(w_1(i,k,j)  - B_w_1(i,k,j))     &
                   + (w_2(i,k,j)  - B_w_2(i,k,j) ) *(w_2(i,k,j)  - B_w_2(i,k,j))     &
                   + (ph_1(i,k,j)  - B_ph_1(i,k,j) ) *(ph_1(i,k,j)  - B_ph_1(i,k,j)) &
                   + (ph_2(i,k,j)  - B_ph_2(i,k,j) ) *(ph_2(i,k,j)  - B_ph_2(i,k,j)) &
                   + (t_1(i,k,j)  - B_t_1(i,k,j) ) *(t_1(i,k,j)  - B_t_1(i,k,j))     &
                   + (t_2(i,k,j)  - B_t_2(i,k,j) ) *(t_2(i,k,j)  - B_t_2(i,k,j))     &
                   + (p(i,k,j)  - B_p(i,k,j) ) *(p(i,k,j)  - B_p(i,k,j))     &
                   + (al(i,k,j)  - B_al(i,k,j) ) *(al(i,k,j)  - B_al(i,k,j))     &
                   + (z(i,k,j)  - B_z(i,k,j) ) *(z(i,k,j)  - B_z(i,k,j))     
   enddo
   enddo
   enddo


   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N  + (mu_1(i,j) -B_mu_1(i,j))*(mu_1(i,j) -B_mu_1(i,j))    &
                   + (mu_2(i,j) -B_mu_2(i,j))*(mu_2(i,j) -B_mu_2(i,j))    &
                   + (mudf(i,j) -B_mudf(i,j))*(mudf(i,j) -B_mudf(i,j))  
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_N=VAL_N  + (moist_1(i,k,j,h) -B_moist_1(i,k,j,h))*(moist_1(i,k,j,h) -B_moist_1(i,k,j,h))  &
                   + (moist_2(i,k,j,h) -B_moist_2(i,k,j,h))*(moist_2(i,k,j,h) -B_moist_2(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA_M**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_em_sn: ALPHA_M=',ALPHA_M,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test
!-------------
!1234 continue  !zzma
!-------------

   FACTOR=0.1

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!   w_bt = S_w_bt 
!   ph_b = S_ph_b 
!   ph_bt = S_ph_bt 
!   t_b = S_t_b 
!   t_bt = S_t_bt 
!   mu_b = S_mu_b 
!   mu_bt = S_mu_bt 
!   rqv_b = S_rqv_b 
!   rqv_bt = S_rqv_bt 




   g_u_1 = FACTOR*S_u_1 
   g_u_2 = FACTOR*S_u_2 
   g_v_1 = FACTOR*S_v_1 
   g_v_2 = FACTOR*S_v_2 
   g_w_1 = FACTOR*S_w_1 
   g_w_2 = FACTOR*S_w_2 
   g_ph_1 = FACTOR*S_ph_1 
   g_ph_2 = FACTOR*S_ph_2 
   g_t_1 = FACTOR*S_t_1 
   g_t_2 = FACTOR*S_t_2 
   g_mu_1 = FACTOR*S_mu_1 
   g_mu_2 = FACTOR*S_mu_2 
   g_p = FACTOR*S_p 
   g_al = FACTOR*S_al 
   g_moist_1 = FACTOR*S_moist_1 
   g_moist_2 = FACTOR*S_moist_2 
!   g_u_b = FACTOR*S_u_b 
!   g_u_bt = FACTOR*S_u_bt 
!   g_v_b = FACTOR*S_v_b 
!   g_v_bt = FACTOR*S_v_bt 
!   g_w_b = FACTOR*S_w_b 
!   g_w_bt = FACTOR*S_w_bt 
!   g_ph_b = FACTOR*S_ph_b 
!   g_ph_bt = FACTOR*S_ph_bt 
!   g_t_b = FACTOR*S_t_b 
!   g_t_bt = FACTOR*S_t_bt 
!   g_mu_b = FACTOR*S_mu_b 
!   g_mu_bt = FACTOR*S_mu_bt 
!   g_rqv_b = FACTOR*S_rqv_b 
!   g_rqv_bt = FACTOR*S_rqv_bt 

   g_u_b = 0.
   g_u_bt =0.
   g_v_b = 0.
   g_v_bt =0.
   g_w_b = 0.
   g_w_bt =0.
   g_ph_b =0.
   g_ph_bt =0.
   g_t_b = 0.
   g_t_bt =0.
   g_mu_b =0.
   g_mu_bt =0.
   g_rqv_b =0.
   g_rqv_bt =0.


!   g_u_1 =0.0
!   g_v_1 =0.0
!   g_w_1 =0.0
!   g_ph_1 =0.0
!   g_t_1 = 0.0
!   g_moist_1 =0.0
!   g_w_2 = 0.0
!   g_mu_1 =0.0

!   g_u_2 =0.0
!   g_v_2 =0.0
!   g_ph_2 =0.0
!   g_t_2 =0.0

!   g_mu_2 = 0.0
!   g_p = 0.0
!   g_al = 0.0
!   g_moist_2 = 0.0

   B_u_1 = g_u_1 
   B_u_2 = g_u_2 
   B_v_1 = g_v_1 
   B_v_2 = g_v_2 
   B_w_1 = g_w_1 
   B_w_2 = g_w_2 
   B_ph_1 = g_ph_1 
   B_ph_2 = g_ph_2 
   B_t_1 = g_t_1 
   B_t_2 = g_t_2 
   B_mu_1 = g_mu_1 
   B_mu_2 = g_mu_2 
   B_p = g_p 
   B_al = g_al 
   B_moist_1 = g_moist_1 
   B_moist_2 = g_moist_2 
!   B_u_b = g_u_b 
!   B_u_bt = g_u_bt 
!   B_v_b = g_v_b 
!   B_v_bt = g_v_bt 
!   B_w_b = g_w_b 
!   B_w_bt = g_w_bt 
!   B_ph_b = g_ph_b 
!   B_ph_bt = g_ph_bt 
!   B_t_b = g_t_b 
!   B_t_bt = g_t_bt 
!   B_mu_b = g_mu_b 
!   B_mu_bt = g_mu_bt 
!   B_rqv_b = g_rqv_b 
!   B_rqv_bt = g_rqv_bt 


   CALL solve_em_tl ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )

   g_z=0.; g_mudf=0.

   VAL_L =0.0
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L =VAL_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                     + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                     + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                     + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                     + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                     + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                     + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                     + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                     + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                     + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                     + g_p(i,k,j)  * g_p(i,k,j)    &
                     + g_al(i,k,j)  * g_al(i,k,j)    &
                     + g_z(i,k,j)  * g_z(i,k,j)

   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      VAL_L =VAL_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                     + g_mu_2(i,j)  * g_mu_2(i,j)    &
                     + g_mudf(i,j)  * g_mudf(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_L =VAL_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                     + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h)
   enddo
   enddo
   enddo
   enddo


!  ADJ

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!  w_bt = S_w_bt 
!  ph_b = S_ph_b 
!  ph_bt = S_ph_bt 
!  t_b = S_t_b 
!  t_bt = S_t_bt 
!  mu_b = S_mu_b 
!  mu_bt = S_mu_bt 
!  rqv_b = S_rqv_b 
!  rqv_bt = S_rqv_bt 




   a_u_1 = 0.
   a_u_2 = 0.
   a_v_1 = 0.
   a_v_2 =0.
   a_w_1 =0.
   a_w_2 =0.
   a_ph_1 =0.
   a_ph_2 = 0.
   a_t_1 = 0.
   a_t_2 =0.
   a_mu_1 = 0.
   a_mu_2 =0.
   a_p = 0.
   a_al =0.
   a_moist_1 = 0.
   a_moist_2 =0.
   a_u_b = 0.
   a_u_bt = 0.
   a_v_b = 0.
   a_v_bt =0.
   a_w_b = 0.
   a_w_bt =0.
   a_ph_b =0.
   a_ph_bt =0.
   a_t_b = 0.
   a_t_bt =0.
   a_mu_b =0.
   a_mu_bt =0.
   a_rqv_b =0.
   a_rqv_bt =0.

   a_u_1 = g_u_1 
   a_u_2 = g_u_2 
   a_v_1 = g_v_1 
   a_v_2 = g_v_2 
   a_w_1 = g_w_1 
   a_w_2 = g_w_2 
   a_ph_1 = g_ph_1 
   a_ph_2 = g_ph_2 
   a_t_1 = g_t_1 
   a_t_2 = g_t_2 
   a_mu_1 = g_mu_1 
   a_mu_2 = g_mu_2 
   a_mudf = g_mudf 
   a_p = g_p 
   a_al = g_al 
   a_z = g_z 
   a_moist_1 = g_moist_1 
   a_moist_2 = g_moist_2 

   CALL solve_em_ad ( grid , config_flags , &
!
#include "em_dummy_args.inc"
!
                 )

    VAL_A=0.0

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A= VAL_A + a_u_1(i,k,j) * B_u_1(i,k,j)               &
            + a_u_2(i,k,j) * B_u_2(i,k,j)               &
            + a_v_1(i,k,j) * B_v_1(i,k,j)               &
            + a_v_2(i,k,j) * B_v_2(i,k,j)               &
            + a_w_1(i,k,j) * B_w_1(i,k,j)               &
            + a_w_2(i,k,j) * B_w_2(i,k,j)               &
            + a_ph_1(i,k,j) * B_ph_1(i,k,j)             &
            + a_ph_2(i,k,j) * B_ph_2(i,k,j)             &
            + a_t_1(i,k,j) * B_t_1(i,k,j)               &
            + a_t_2(i,k,j) * B_t_2(i,k,j)               &
            + a_p(i,k,j) * B_p(i,k,j)                   &
            + a_al(i,k,j) * B_al(i,k,j)  

   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A= VAL_A + a_mu_1(i,j) * B_mu_1(i,j)                 &
            + a_mu_2(i,j) * B_mu_2(i,j)        
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_A= VAL_A + a_moist_1(i,k,j,h) * B_moist_1(i,k,j,h)   &
            + a_moist_2(i,k,j,h) * B_moist_2(i,k,j,h)   
   enddo
   enddo
   enddo
   enddo
   do i=1,max(grid%ed31,grid%ed33)
   do k=kts,kte
   do j=1,grid%spec_bdy_width
   do h=1,4
!      VAL_A= VAL_A + a_u_b(i,k,j,h) * B_u_b(i,k,j,h)           &
!            + a_u_bt(i,k,j,h) * B_u_bt(i,k,j,h)         &
!            + a_v_b(i,k,j,h) * B_v_b(i,k,j,h)           &
!            + a_v_bt(i,k,j,h) * B_v_bt(i,k,j,h)         &
!            + a_w_b(i,k,j,h) * B_w_b(i,k,j,h)           &
!            + a_w_bt(i,k,j,h) * B_w_bt(i,k,j,h)         &
!            + a_ph_b(i,k,j,h) * B_ph_b(i,k,j,h)         &
!            + a_ph_bt(i,k,j,h) * B_ph_bt(i,k,j,h)       &
!            + a_t_b(i,k,j,h) * B_t_b(i,k,j,h)           &
!            + a_t_bt(i,k,j,h) * B_t_bt(i,k,j,h)         &
!            + a_mu_b(i,k,j,h) * B_mu_b(i,k,j,h)         &
!            + a_mu_bt(i,k,j,h) * B_mu_bt(i,k,j,h)       &
!            + a_rqv_b(i,k,j,h) * B_rqv_b(i,k,j,h)       &
!            + a_rqv_bt(i,k,j,h) * B_rqv_bt(i,k,j,h)  
   enddo
   enddo
   enddo
   enddo

   print*, '                '
   write(6,*) 'a_em: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

!  RECOVER

   u_1 = S_u_1 
   u_2 = S_u_2 
   ru = S_ru 
   ru_m = S_ru_m 
   ru_tend = S_ru_tend 
   u_save = S_u_save 
   v_1 = S_v_1 
   v_2 = S_v_2 
   rv = S_rv 
   rv_m = S_rv_m 
   rv_tend = S_rv_tend 
   v_save = S_v_save 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ww = S_ww 
   rw = S_rw 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   phb = S_phb 
   php = S_php 
   t_1 = S_t_1 
   t_2 = S_t_2 
   t_save = S_t_save 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   mub = S_mub 
   mudf = S_mudf 
   muu = S_muu 
   muv = S_muv 
   mut = S_mut 
   muts = S_muts 
   p = S_p 
   al = S_al 
   alt = S_alt 
   z = S_z 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
   u_b = S_u_b 
   u_bt = S_u_bt 
   v_b = S_v_b 
   v_bt = S_v_bt 
   w_b = S_w_b 
   w_bt = S_w_bt 
   ph_b = S_ph_b 
   ph_bt = S_ph_bt 
   t_b = S_t_b 
   t_bt = S_t_bt 
   mu_b = S_mu_b 
   mu_bt = S_mu_bt 
   rqv_b = S_rqv_b 
   rqv_bt = S_rqv_bt 
   rqc_b = S_rqc_b 
   rqc_bt = S_rqc_bt 
   rqr_b = S_rqr_b 
   rqr_bt = S_rqr_bt 
   rqi_b = S_rqi_b 
   rqi_bt = S_rqi_bt 
   rqs_b = S_rqs_b 
   rqs_bt = S_rqs_bt 
   rqg_b = S_rqg_b 
   rqg_bt = S_rqg_bt 
   fcx = S_fcx 
   gcx = S_gcx 
   xkmhd = S_xkmhd 
!***********************************************************
!***********************************************************
!g_em_sn: ALPHA_M=.1000E+00  COEF=   0.1002139921065E+01  VAL_N= 0.106907E+12  VAL_L= 0.106679E+12
!g_em_sn: ALPHA_M=.1000E-01  COEF=   0.1000033706173E+01  VAL_N= 0.106683E+10  VAL_L= 0.106679E+10
!g_em_sn: ALPHA_M=.1000E-02  COEF=   0.9998179439761E+00  VAL_N= 0.106660E+08  VAL_L= 0.106679E+08
!g_em_sn: ALPHA_M=.1000E-03  COEF=   0.9997966495443E+00  VAL_N= 0.106657E+06  VAL_L= 0.106679E+06
!g_em_sn: ALPHA_M=.1000E-04  COEF=   0.9997944894996E+00  VAL_N= 0.106657E+04  VAL_L= 0.106679E+04
!g_em_sn: ALPHA_M=.1000E-05  COEF=   0.9997942733074E+00  VAL_N= 0.106657E+02  VAL_L= 0.106679E+02
!g_em_sn: ALPHA_M=.1000E-06  COEF=   0.9997942529592E+00  VAL_N= 0.106657E+00  VAL_L= 0.106679E+00
!g_em_sn: ALPHA_M=.1000E-07  COEF=   0.9997942382646E+00  VAL_N= 0.106657E-02  VAL_L= 0.106679E-02
!g_em_sn: ALPHA_M=.1000E-08  COEF=   0.9997943929116E+00  VAL_N= 0.106657E-04  VAL_L= 0.106679E-04
!g_em_sn: ALPHA_M=.1000E-09  COEF=   0.9997942529716E+00  VAL_N= 0.106657E-06  VAL_L= 0.106679E-06
!g_em_sn: ALPHA_M=.1000E-10  COEF=   0.9997953239580E+00  VAL_N= 0.106657E-08  VAL_L= 0.106679E-08

! a_em:
!      VAL_TL:    0.1065110571042E+12
!      VAL_AD:    0.1065110571042E+12
!***********************************************************
!***********************************************************
!---------------------------------------------------------------------------------------

END SUBROUTINE test_solve_em



