!===============================================================================
! !MODULE: esmf_wrfvar_component
!
! !DESCRIPTION:
! Component interfaces for the WRF Var component 
!===============================================================================
module esmf_wrfvar_component
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod

  ! wrfvar 
  use da_wrfvar_top

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

    public wrfvar_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfvar_register(comp, rc)
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

    print *, "In user 1 register routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! register initialize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, wrfvar_init,           &
                                    phase=ESMF_SINGLEPHASE, rc=localrc)

    !---------------------------------------------------------------------------
    ! register run phases
    ! 1. WRF VAR previous to running NL model
    ! 2. WRF VAR after running NL model, but before running Plus model
    ! 3. WRF VAR after running Plus model, but before finial VAR work
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, wrfvar_run1,            &
                                    phase=1, rc=localrc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, wrfvar_run2,            &
                                    phase=2, rc=localrc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, wrfvar_run3,            &
                                    phase=3, rc=localrc)

    !---------------------------------------------------------------------------
    ! register finalize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, wrfvar_final,         &
                                    phase=ESMF_SINGLEPHASE, rc=localrc)

    print *, "Registered Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine wrfvar_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfvar_init(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialization routine for the coupling between the main Var model and the
  ! NL model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_VM)                       :: vm

    ! Local variables
    logical                             :: no_init1
    integer                             :: petCount
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    print *, "In user 1 init routine"
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=localrc)

    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)

    !---------------------------------------------------------------------------
    ! call to disable processor quilting (e.g. wrf real)
    !---------------------------------------------------------------------------
    call disable_quilting

    !---------------------------------------------------------------------------
    ! DA first stage init, calls routine init_modules
    !---------------------------------------------------------------------------
    call da_wrfvar_init1( no_init1=.TRUE. )

    !---------------------------------------------------------------------------
    ! DA second stage init, initializes debug, trace and time keeping, plus
    ! calls var/da/da_main/da_med_initdata_input.
    !---------------------------------------------------------------------------
    call da_wrfvar_init2

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfvar_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfvar_run1(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The first run phase of the WRF VAR code occurs prior to the coupling with 
  ! the NL model code. 
  !-----------------------------------------------------------------------------

    ! Local ESMF types

    ! Local variables
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "WRF RUN Phase 1"

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    call da_wrfvar_run

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfvar_run1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfvar_run2(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The second run phase of the WRF VAR code follows the coupling with the NL
  ! model, but is before coupling with the WRF Plus code.
  !-----------------------------------------------------------------------------

    ! Local ESMF types

    ! Local variables
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "WRF RUN Phase 2"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfvar_run2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfvar_run3(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The third run phase of the WRF VAR code. Third phase follows the coupling
  ! with the WRF Plus.
  !-----------------------------------------------------------------------------

    ! Local ESMF types

    ! Local variables
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "WRF RUN Phase 3"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfvar_run3
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
 
  !-----------------------------------------------------------------------------
  subroutine wrfvar_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !-----------------------------------------------------------------------------

    ! local ESMF types
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "In wrf var finalize routine"

    !---------------------------------------------------------------------------
    ! wrfvar internal finalize call
    !---------------------------------------------------------------------------
    call da_wrfvar_finalize

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfvar_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfvar_component
!-------------------------------------------------------------------------------
