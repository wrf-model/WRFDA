!WRFVAR:DRIVER_LAYER:TOP
!

MODULE da_wrfvar_top
   !<DESCRIPTION>
   ! This module defines top-level wrfvar_init(), wrfvar_run(), and wrfvar_finalize() 
   ! routines.  
   !</DESCRIPTION>

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
   USE da_tracing

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
!JRB fix for WRF compilation system later
!   CHARACTER (LEN=40)      :: subversion_version=SVN_REV
   CHARACTER (LEN=40)      :: subversion_version="TBD"

   INTERFACE 
      SUBROUTINE setup_timekeeping( grid )
        USE module_domain
        TYPE(domain), POINTER :: grid
      END SUBROUTINE setup_timekeeping
   END INTERFACE


CONTAINS

#include "da_wrfvar_init.inc"
#include "da_wrfvar_run.inc"
#include "da_wrfvar_finalize.inc"
#include "da_wrfvar_interface.inc"

END MODULE da_wrfvar_top
