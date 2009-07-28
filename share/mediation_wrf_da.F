!WRF:MEDIATION_LAYER:
!
SUBROUTINE med_shutdown_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: ierr

   WRITE ( message , FMT = '("main: time step #",I8)' ) grid%time_step_max
   IF ( grid%oid > 0 ) CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
#ifdef QUILT_NO
   IF ( grid%auxhist1_oid > 0 ) CALL close_dataset ( grid%auxhist1_oid , config_flags , "DATASET=AUXHIST1" )
#endif

   CALL wrf_ioexit( ierr )    ! shut down the quilt I/O

   RETURN

END SUBROUTINE med_shutdown_io

SUBROUTINE med_add_config_info_to_grid ( grid )

   USE module_domain
   USE module_configure
 
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

#define SOURCE_RECORD model_config_rec %
#define SOURCE_REC_DEX (grid%id)
#define DEST_RECORD   grid %
#include <config_assigns.inc>

   RETURN

END SUBROUTINE med_add_config_info_to_grid

