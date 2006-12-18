SUBROUTINE DA_AO_Stats_SSMI ( stats_unit, oi, re )

   IMPLICIT NONE

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.
   integer  :: ierror                                ! MPI return code.

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(oi%num_ssmi_tb, num_ssmi_tb, 1, &
                      MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)

   call MPI_ALLREDUCE(oi%num_ssmi_retrieval, num_ssmi_retrieval, 1, &
                      MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
#else
   num_ssmi_tb = oi%num_ssmi_tb
   num_ssmi_retrieval = oi%num_ssmi_retrieval
#endif

   IF ( num_ssmi_tb .gt. 0 ) THEN

      CALL DA_AO_Stats_SSMI_Tb ( stats_unit, oi, re )
      num_ssmi_tot = num_ssmi_tb_tot

   ELSE IF ( num_ssmi_retrieval .gt. 0) THEN

      CALL DA_AO_Stats_SSMI_Rv( stats_unit, oi, re )
      num_ssmi_tot = num_ssmi_rv_tot

   ENDIF

END SUBROUTINE DA_AO_Stats_SSMI

