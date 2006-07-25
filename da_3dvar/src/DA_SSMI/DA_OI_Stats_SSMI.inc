SUBROUTINE DA_OI_Stats_SSMI ( stats_unit, oi )
#ifdef DM_PARALLEL
   USE module_dm, ONLY : mpi_comm_local
#endif

   IMPLICIT NONE

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   INTEGER,        INTENT (IN)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (IN)      :: oi            ! OI

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.
   integer  :: ierror                                ! MPI return code.

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(oi%num_ssmi_tb, num_ssmi_tb, 1, &
                      MPI_INTEGER, MPI_SUM, mpi_comm_local, ierror)

   call MPI_ALLREDUCE(oi%num_ssmi_retrieval, num_ssmi_retrieval, 1, &
                      MPI_INTEGER, MPI_SUM, mpi_comm_local, ierror)
#else
   num_ssmi_tb = oi%num_ssmi_tb
   num_ssmi_retrieval = oi%num_ssmi_retrieval
#endif

   IF ( num_ssmi_tb .gt. 0 ) THEN

           CALL DA_OI_Stats_SSMI_Tb ( stats_unit, oi )

   ELSE IF ( num_ssmi_retrieval .gt. 0) THEN

           CALL DA_OI_Stats_SSMI_Rv ( stats_unit, oi )

   ENDIF

END SUBROUTINE DA_OI_Stats_SSMI
