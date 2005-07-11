!WRF GEN_BE_STAGE0:DRIVER_LAYER:MAIN
!

PROGRAM gen_be_stage0

   USE module_gen_be_top

!<DESCRIPTION>
! Main program of gen_be.  Responsible for starting up, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=da_v3d_solve.html>da_v3d_solve</a> routine assimilation.
! After the assimilation is completed, the model is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

!--Initialize gen_be
   CALL gen_be_init

!--gen_be Calls integrate().  
   CALL gen_be_run

!--gen_be clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
   CALL gen_be_finalize

END PROGRAM gen_be_stage0

