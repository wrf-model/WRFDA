subroutine da_wrfvar_interface ( grid, config_flags ) 

   IMPLICIT NONE

#ifndef DEREF_KLUDGE
   INTERFACE
      subroutine da_solve_v3d ( grid , config_flags , &
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

      USE da_constants
      USE da_define_structures
      USE da_setup_structures
      USE da_test
      USE da_tools
      USE da_minimisation
      USE da_par_util

      !--Input data.

      TYPE(domain),                INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags

#include "em_dummy_decl.inc"

      end subroutine da_solve_v3d
   END INTERFACE
#endif

   TYPE(domain),                INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags
   INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   call da_trace_entry("da_wrfvar_interface")

   ! call mediation_setup_step ( grid , config_flags , 1 , 1 , 1 )

   call set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   call model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   grid%itimestep = 1

#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif

   call da_solve_v3d ( grid , config_flags , &
#include "em_actual_args.inc"
    )

   call da_trace_exit("da_wrfvar_interface")

end subroutine da_wrfvar_interface
