subroutine da_wrfvar_init1( no_init1 )

   !<DESCRIPTION>
   !     WRF initialization routine, part 1
   !</DESCRIPTION>

   IMPLICIT NONE

   LOGICAL, OPTIONAL, INTENT(IN) :: no_init1
   integer :: ierr

   !<DESCRIPTION>
   ! Program_name, a global variable defined in frame/module_domain.F, is
   ! set, then a routine <a href=init_modules.html>init_modules</a> is
   ! called. This calls all the init programs that are provided by the
   ! modules that are linked into WRF.  These include initialization of
   ! external I/O packages.   Also, some key initializations for
   ! distributed-memory parallelism occur here if DM_PARALLEL is specified
   ! in the compile: setting up I/O quilt processes to act as I/O servers
   ! and dividing up MPI communicators among those as well as initializing
   ! external communication packages such as RSL or RSL_LITE.
   !
   !</DESCRIPTION>

   program_name = wrfvar_version//" (COMPATIBLE WITH "//wrf_version//") MODEL"

   ! Initialize WRF modules:  
   ! Phase 1 returns after MPI_INIT() (if it is called)
   IF (.NOT. PRESENT(no_init1)) THEN
      call init_modules(1)
      ! Initialize utilities (time manager, etc.)
      call wrfu_initialize(defaultCalendar=WRFU_CAL_GREGORIAN)
   ENDIF
   ! Phase 2 resumes after MPI_INIT() (if it is called)
   call init_modules(2)

   !<DESCRIPTION>
   ! The wrf namelist.input file is read and stored in the USE associated
   ! structure model_config_rec, defined in frame/module_configure.F, by the
   ! call to <a href=initial_config.html>initial_config</a>.  On distributed
   ! memory parallel runs this is done only on one processor, and then
   ! broadcast as a buffer.  For distributed-memory, the broadcast of the
   ! configuration information is accomplished by first putting the
   ! configuration information into a buffer (<a
   ! href=get_config_as_buffer.html>get_config_as_buffer</a>), broadcasting
   ! the buffer, then setting the configuration information (<a
   ! href=set_config_as_buffer.html>set_config_as_buffer</a>).
   !
   !</DESCRIPTION>

   call wrf_message("***  VARIATIONAL ANALYSIS ***")

#ifdef DM_PARALLEL
   call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
   call MPI_COMM_RANK(MPI_COMM_WORLD, myproc, ierr )
#else
   num_procs = 1
   myproc = 0
#endif

   if (myproc==0) then
      rootproc=.TRUE.
   else
      rootproc=.FALSE.
   end if

#ifdef DM_PARALLEL
   IF (rootproc) THEN
      call initial_config
   ENDIF
   call get_config_as_buffer( configbuf, configbuflen, nbytes )
   call wrf_dm_bcast_bytes( configbuf, nbytes )
   call set_config_as_buffer( configbuf, configbuflen )
   call wrf_dm_initialize
#else
   call initial_config
#endif

   ! Copy namelist variables to da_control

#define SOURCE_RECORD model_config_rec%
#define DEST_RECORD

#include "config_assigns.inc"

   if (.NOT. rootproc) check_max_iv_print=.false.

end subroutine da_wrfvar_init1

