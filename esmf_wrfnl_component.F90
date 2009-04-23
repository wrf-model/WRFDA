!===============================================================================
! !MODULE: esmf_wrfnl_component
!
! !DESCRIPTION:
! Component interfaces for the WRF NL (or forward WRF model) component
!===============================================================================
module esmf_wrfnl_component
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod
  use module_wrf_top, ONLY : wrf_init, wrf_run, wrf_finalize

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

    public wrfnl_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfnl_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The Register routine for
  !  Sets the subroutines to be called as the init, run, and
  !  finalize routines.
  !-----------------------------------------------------------------------------

    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "In user 2 register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, wrfnl_init,            & 
                                    ESMF_SINGLEPHASE, localrc)

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, wrfnl_run,              &
                                    ESMF_SINGLEPHASE, localrc)

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, wrfnl_final,          &
                                    ESMF_SINGLEPHASE, localrc)

    print *, "Registered Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine wrfnl_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfnl_init1(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  !<DESCRIPTION>
  !   - Initializes the WRF NL model component.  
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !</DESCRIPTION>
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                       :: vm
    type(ESMF_Array)                    :: sorted_data
    type(ESMF_DistGrid)                 :: distgrid
    type(ESMF_ArraySpec)                :: arrayspec

    ! Local variables
    integer                             :: petCount
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! get the local VM information
    !---------------------------------------------------------------------------
    print *, "wrfnl: initialization phase 1"
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=localrc)

    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)

    !---------------------------------------------------------------------------
    ! call to disable processor quilting (e.g. wrf real)
    !---------------------------------------------------------------------------
    call disable_quilting

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! call phase 2 of init_mnodules, phase 1 is skipped since ESMF manages the
    ! parallel environment. Thus these two calls
    ! 	- CALL init_modules(1)
    ! 	- CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN )
    ! are skipped.
    !---------------------------------------------------------------------------
    CALL init_modules(2)

    !---------------------------------------------------------------------------
    ! Call WRF "init" routine, which, for a DM_PARALLEL run, will recognize
    ! that ESMF has already called MPI_INIT and respond appropriately.
    !---------------------------------------------------------------------------
    call wrf_init( no_init1=.TRUE. )

    !---------------------------------------------------------------------------
    ! For now, use settings from WRF component intialization to set up
    ! top-level clock.  Per suggestion from ESMF Core team, these are passed
    ! back as attributes on exportState.
    !---------------------------------------------------------------------------
    call wrf_clockprint( 100, head_grid%domain_clock,                          &
            'DEBUG wrf_component_init1():  head_grid%domain_clock,' )

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfnl_init1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfnl_init2(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  !<DESCRIPTION>
  !   - Initializes the WRF NL model component's import and export state.  
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !</DESCRIPTION>
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                       :: vm
    type(ESMF_Array)                    :: sorted_data
    type(ESMF_DistGrid)                 :: distgrid
    type(ESMF_ArraySpec)                :: arrayspec

    ! Local variables
    integer                             :: petCount
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! get the local VM information
    !---------------------------------------------------------------------------
    print *, "wrfnl: initialization phase 2"
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=localrc)

    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)

    !---------------------------------------------------------------------------
    ! empty for now, will finish when the coupler is in place.
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfnl_init2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfnl_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(out)                :: rc
  !-----------------------------------------------------------------------------
  !<DESCRIPTION>
  !     WRF NL model component run routine.
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !</DESCRIPTION>
  !-----------------------------------------------------------------------------
    ! Local variables
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Call WRF "run" routine
    !---------------------------------------------------------------------------
    print *, "wrfnl: run phase"
    call wrf_debug ( 100 , 'DEBUG wrf_component_run():  calling wrf_run()' )
    call wrf_run( )
    call wrf_debug ( 100 , 'DEBUG wrf_component_run():  back from wrf_run()' )

    call wrf_debug ( 100 , 'DEBUG wrf_component_run():  end' )

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfnl_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfnl_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  !<DESCRIPTION>
  !     WRF component finalize routine.
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !</DESCRIPTION>
  !-----------------------------------------------------------------------------
    ! local ESMF types

    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Call WRF "finalize" routine, suppressing call to MPI_FINALIZE so
    ! ESMF can do it during ESMF_Finalize().
    !---------------------------------------------------------------------------
     CALL wrf_finalize( no_shutdown=.TRUE. )

        
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfnl_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfnl_component
!-------------------------------------------------------------------------------

