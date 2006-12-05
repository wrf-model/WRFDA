subroutine da_system_4dvar( cmd_line_in )

   ! Not sure if this should go here, but here it goes for now.
   ! This is a wrapper around the "system" call.  Will compile
   ! to just system for single processor. On DM_PARALLEL compiles,
   ! it will call system only on the 0th processor.

   implicit none
   character*(*) cmd_line_in
   character*256 cmd_line

#ifdef DM_PARALLEL
   if (rootproc) then
      cmd_line = trim(cmd_line_in) // " monitor"
   else
      cmd_line = trim(cmd_line_in) // " other"
   end if
   call mpi_barrier ( comm, ierr )
#else
   cmd_line = cmd_line_in // " other"
#endif
   call da_system( cmd_line )
#ifdef DM_PARALLEL
   call mpi_barrier ( comm, ierr )
#endif

end subroutine da_system_4dvar


