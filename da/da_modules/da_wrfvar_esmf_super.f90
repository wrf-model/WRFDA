!WRFVAR:DRIVER_LAYER:ESMF
!

MODULE da_wrfvar_esmf_super
   !<DESCRIPTION>
   ! This module defines wrfvar_init(), wrfvar_run(), and wrfvar_finalize() 
   ! routines for use by ESMF superstructure.  
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

   USE module_timing

#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE da_wrfvar_io

   USE da_constants

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop, levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: domain_id , fid , oid , idum1 , idum2 , ierr

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
#endif

   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message

   INTERFACE 
      SUBROUTINE setup_timekeeping( grid )
         USE module_domain
         TYPE(domain), POINTER :: grid
      END SUBROUTINE setup_timekeeping
   END INTERFACE

CONTAINS

#include "da_esmf_init.inc"
#include "da_esmf_run.inc"
#include "da_esmf_finalize.inc"
#include "da_wrfvar_interface.inc"

END MODULE da_wrfvar_esmf_super

