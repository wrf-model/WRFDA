module da_wrfvar_top

   !-----------------------------------------------------------------------
   ! Purpose: Defines top-level wrfvar_init(), wrfvar_run(), and 
   ! wrfvar_finalize() routines.  
   !-----------------------------------------------------------------------

   use module_machine
   use module_domain
   use module_integrate
   use module_configure

   use module_timing
   use da_tracing
   use da_tools
   use da_radiance
   use da_control
   use da_define_structures
   use da_setup_structures
   use da_test
   use da_minimisation
   use da_wrf_interfaces

#ifdef DM_PARALLEL
   use module_dm
#endif

   use da_wrfvar_io

   implicit none

   real    :: time

   integer :: loop, levels_to_process

   type (domain) , pointer :: keep_grid, grid_ptr, null_domain
   type (grid_config_rec_type), save :: config_flags
   integer                 :: number_at_same_level
   integer                 :: time_step_begin_restart

   integer :: domain_id , fid , oid , idum1 , idum2

#ifdef DM_PARALLEL
   integer                 :: nbytes
   integer, parameter      :: configbuflen = 4* CONFIG_BUF_LEN
   integer                 :: configbuf( configbuflen )
#endif

   character (LEN=80)      :: rstname

contains

#include "da_wrfvar_init1.inc"
#include "da_wrfvar_init2.inc"
#include "da_wrfvar_run.inc"
#include "da_wrfvar_interface.inc"
#include "da_wrfvar_finalize.inc"
#include "da_solve.inc"

end module da_wrfvar_top
