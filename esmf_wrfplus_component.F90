!===============================================================================
! !MODULE: esmf_wrfplus_component
!
! !DESCRIPTION:
! Component interfaces for the WRF plus component. The WRF plus component 
! consists of a tangent linear (TL) and an adjoint model of WRF.
!===============================================================================
module esmf_wrfplus_component
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

    public wrfplus_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfplus_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    integer, intent(  out)             :: rc
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

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, wrfplus_init,          & 
                                    ESMF_SINGLEPHASE, localrc)
!   if(ESMF_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rc)) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, wrfplus_run,            &
                                    ESMF_SINGLEPHASE, localrc)
!   if(ESMF_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rc)) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, wrfplus_final,        &
                                    ESMF_SINGLEPHASE, localrc)
!   if(ESMF_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rc)) return


    print *, "Registered Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine wrfplus_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfplus_init(comp, importState, exportState, externalclock, rc)
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
    !---------------------------------------------------------------------------

    print *, "In user 2 init routine"
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=localrc)
!   if(ESMF_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rc)) return

    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
!   if(ESMF_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rc)) return


    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfplus_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfplus_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                     :: importState, exportState
    type(ESMF_Clock)                     :: externalclock
    integer, intent(  out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !-----------------------------------------------------------------------------

    ! Local ESMF types

    ! Local variables
    integer                             :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    print *, "In user 2 run routine"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfplus_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine wrfplus_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(out)                :: rc
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
    print *, "In user 2 final routine"
  
        
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine wrfplus_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfplus_component
!-------------------------------------------------------------------------------

