MODULE module_gen_be_stats_interface

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure

CONTAINS

SUBROUTINE da_gen_be_stats_interface ( grid, config_flags ) 

   IMPLICIT NONE

#ifndef DEREF_KLUDGE
   INTERFACE
      SUBROUTINE stats_be ( grid , config_flags , &
#include <em_dummy_args.inc>
                           )

      USE module_configure
      USE module_domain
      USE module_driver_constants

!--Input data.

      TYPE(domain),                INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags

#include <em_dummy_decl.inc>

      END SUBROUTINE stats_be
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

     CALL stats_be ( grid , config_flags , &

#include <em_actual_args.inc>

               )

END SUBROUTINE da_gen_be_stats_interface

END MODULE module_gen_be_stats_interface

