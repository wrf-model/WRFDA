MODULE module_wrf_3dvar_interface

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

! Registry generated module
   USE module_state_description

CONTAINS

SUBROUTINE da_3dvar_interface ( grid, config_flags ) 

   IMPLICIT NONE

#ifndef DEREF_KLUDGE
   INTERFACE
      SUBROUTINE solve_v3d ( grid , config_flags , &
#include <em_dummy_args.inc>
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

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Test
   USE DA_Tools
   USE DA_Minimisation
   USE par_util

! Registry generated module
!  USE module_state_description

!--Input data.

      TYPE(domain),                INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags

#include <em_dummy_decl.inc>

      END SUBROUTINE solve_v3d
   END INTERFACE
#endif

   TYPE(domain),                INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags
   INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

!   CALL mediation_setup_step ( grid , config_flags , 1 , 1 , 1 )

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   grid%itimestep = 1

#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif

     write(unit=0, fmt='(3a,i6)') 'before CALL solve_v3d:', __FILE__, ', line:', __LINE__

     CALL solve_v3d ( grid , config_flags , &

#include <em_actual_args.inc>

               )

END SUBROUTINE da_3dvar_interface

END MODULE module_wrf_3dvar_interface

