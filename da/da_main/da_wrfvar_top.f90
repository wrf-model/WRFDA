!WRFVAR:DRIVER_LAYER:TOP
!

module da_wrfvar_top
   !<DESCRIPTION>
   ! This module defines top-level wrfvar_init(), wrfvar_run(), and wrfvar_finalize() 
   ! routines.  
   !</DESCRIPTION>

   use module_machine
   use module_domain
   use module_integrate
   use module_configure

   use module_timing
   use da_tracing
   use da_tools
   use da_radiance

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

!JRB fix for WRF compilation system later
!   character (LEN=40)      :: subversion_version=SVN_REV
   character (LEN=40)      :: subversion_version="TBD"

   interface 
      subroutine setup_timekeeping( grid )
        use module_domain
        type(domain), pointer :: grid
      end subroutine setup_timekeeping
   end interface


contains

#include "da_wrfvar_init1.inc"
#include "da_wrfvar_init2.inc"
#include "da_wrfvar_run.inc"
#include "da_wrfvar_finalize.inc"
#include "da_wrfvar_interface.inc"

end module da_wrfvar_top
