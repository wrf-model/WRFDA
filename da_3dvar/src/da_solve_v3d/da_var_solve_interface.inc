SUBROUTINE da_var_solve_interface ( grid, config_flags ) 

   IMPLICIT NONE

   TYPE(domain),                INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags
   INTEGER :: idum1, idum2

   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31 , em31y, sm32y, em32y, sm33y, em33y

#include "deref_kludge.h"

     CALL solve_v3d ( grid , config_flags , &
!
#include <em_actual_args.inc>
!
               )

END SUBROUTINE da_var_solve_interface

