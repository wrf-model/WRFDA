!DA_VAR:DRIVER_LAYER:ESMF
!

MODULE da_var_esmf_super
!<DESCRIPTION>
! This module defines da_var_init(), da_var_run(), and da_var_finalize() routines for 
! use by ESMF superstructure.  
! WRFVAR can be built with either ESMF_Mod (from an installed ESMF library) 
! or with built-in wrf_esmf_mod.  The choice is made at configure time 
! via cpp token WRF_ESMF_MOD.  
! Note that WRF_ESMF_MOD is USEd by module_domain.  
!</DESCRIPTION>

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_tiles
   USE module_timing
   USE module_wrf_error

#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE DA_Constants
   USE DA_Var_IO
!  USE DA_Define_Structures
!  USE DA_Setup_Structures
!  USE DA_Test
!  USE DA_Tools
!  USE DA_Minimisation
   USE par_util

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   LOGICAL :: input_from_file

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

CONTAINS

#include "da_init_3dvar.inc"
#include "da_var_init.inc"
#include "da_var_run.inc"
#include "da_var_finalize.inc"
#include "solve_v3d.inc"
#include "da_var_solve_interface.inc"

END MODULE da_var_esmf_super

