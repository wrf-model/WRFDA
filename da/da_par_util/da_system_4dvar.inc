SUBROUTINE da_system_4dvar( cmd_line_in )

   ! Not sure if this should go here, but here it goes for now.
   ! This is a wrapper around the "system" call.  Will compile
   ! to just system for single processor. On DM_PARALLEL compiles,
   ! it will call system only on the 0th processor.

   IMPLICIT NONE
   CHARACTER*(*) cmd_line_in
   CHARACTER*256 cmd_line

#ifdef DM_PARALLEL
   INTEGER comm, ierr
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   IF (wrf_dm_on_monitor()) THEN
      cmd_line = TRIM(cmd_line_in) // " monitor"
   ELSE
      cmd_line = TRIM(cmd_line_in) // " other"
   ENDIF
   call wrf_get_dm_communicator ( comm )
   call mpi_barrier ( comm, ierr )
#else
   cmd_line = cmd_line_in // " other"
#endif
   CALL system( cmd_line )
#ifdef DM_PARALLEL
   call mpi_barrier ( comm, ierr )
#endif

END subroutine da_system_4dvar
