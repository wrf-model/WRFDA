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
! use da_wrfvar_top

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

    public var_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_register(comp, rc)
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

    ! initialize return code
    rc = ESMF_FAILURE

    print*, "In var register routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! register initialize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, var_init,              &
                                    phase=ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! register run phases
    ! 1. WRF VAR previous to running NL model
    ! 2. WRF VAR after running NL model, but before running Plus model
    ! 3. WRF VAR after running Plus model, but before finial VAR work
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, var_run1,               &
                                    phase=1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, var_run2,               &
                                    phase=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, var_run3,               &
                                    phase=3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! register finalize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, var_final,            &
                                    phase=ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine var_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_init(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_ArraySpec)                :: arrayspec
    type(ESMF_DistGrid)                 :: distgrid
    type(ESMF_Array)                    :: var_array_in, var_array_out

    ! Local variables
    logical                             :: no_init1
    integer                             :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

  !-----------------------------------------------------------------------------
  ! Initialize ESMF objects
  !-----------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Var init routine"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount
!   call ESMF_VMPrint(vm, rc)

    !---------------------------------------------------------------------------
    ! Create a data Array and add it to the import and export State
    ! These arrays will be used to construct the routehandle for the direct
    ! coupling between the var and plus components.
    !---------------------------------------------------------------------------
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: array spec set"
    if (rc/=ESMF_SUCCESS) return ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,15/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: Dist Grid Create error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    var_array_out = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,   &
                      indexflag=ESMF_INDEX_GLOBAL, name="var.array.exp", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: arraycreate var_array.out"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, var_array_out, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: add export state var_array.out"
    if (rc/=ESMF_SUCCESS) return ! bail out

    var_array_in = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,    &
                      indexflag=ESMF_INDEX_GLOBAL, name="var.array.imp", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: arraycreate var_array.in"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(importState, var_array_in, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Var_init fail: add import state var_array.in"
    if (rc/=ESMF_SUCCESS) return ! bail out

  !-----------------------------------------------------------------------------
  ! WRFVAR initialization
  !-----------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! call to disable processor quilting (e.g. wrf real)
    !---------------------------------------------------------------------------
!   call disable_quilting

    !---------------------------------------------------------------------------
    ! DA first stage init, calls routine init_modules
    !---------------------------------------------------------------------------
!   call da_wrfvar_init1( no_init1=.TRUE. )

    !---------------------------------------------------------------------------
    ! DA second stage init, initializes debug, trace and time keeping, plus
    ! calls var/da/da_main/da_med_initdata_input.
    !---------------------------------------------------------------------------
!   call da_wrfvar_init2

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Var_init routine finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_run1(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_VM)                       :: vm

    ! Local variables
    logical                             :: no_init1
    integer                             :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Var run1 routine"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount
!   call ESMF_VMPrint(vm, rc)

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
!   call da_wrfvar_run

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Var run1 routine finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_run1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_run2(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The second run phase of the Var component calls the modified version of
  ! da_solve and cycles the inner loop linking with the WRF Plus component.
  ! This phase follows the coupling with the NL forward model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_VM)                       :: vm
    type(ESMF_Array)        :: array1, array2
    type(ESMF_RouteHandle)  :: var2plus_Redist, plus2var_Redist

    ! Local variables
    logical                             :: no_init1
    integer                             :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Var run2 start"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount
!   call ESMF_VMPrint(vm, rc)

    !---------------------------------------------------------------------------
    ! Gain access to RouteHandles for direct coupling between Var and Plus
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "plus2var_Redist",                         &
                       routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: state get plus2var_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out
    flush(6)

    call ESMF_StateGet(exportState, "var2plus_Redist",                         &
                       routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: state get var2plus_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out
    flush(6)
    
    !---------------------------------------------------------------------------
    ! Get the Array from the export State
    !---------------------------------------------------------------------------
    call ESMF_StateGet(exportState, "var.array.exp", array2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: state get var.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(importState, "var.array.imp", array1, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: state get var.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    !---------------------------------------------------------------------------
    ! Main Run Loop - direct coupling between Var and Plus
    !---------------------------------------------------------------------------
    ! -> compute input for tangent linear

    ! ArrayRedist() "send" to plus-TL
    call ESMF_ArrayRedist(srcArray=array2, routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: redist var2plus_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    ! -> wait until Plus returns from tangent linear model

    ! ArrayRedist() "receive" from Plus-tangent linear
    call ESMF_ArrayRedist(dstArray=array1, routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: redist plus2var_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    flush(6)
    ! -> compute input for adjont

    ! ArrayRedist() "send" to Plus-adjoint
!   call ESMF_ArrayRedist(srcArray=array2, routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: redist2 var2plus_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    ! -> wait until Plus returns from adjoint model

    ! ArrayRedist() "receive" from Plus-adjoint
!   call ESMF_ArrayRedist(dstArray=array2, routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var run failed: redist2 plus2var_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Var_run2 finished"
    flush(6)
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_run2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_run3(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array

    ! Local variables

    ! initialize return code
    rc = ESMF_FAILURE

    print *, "WRF RUN Phase 3"

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_run3
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
 
  !-----------------------------------------------------------------------------
  subroutine var_final(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: exp_array, imp_array


    ! initialize return code
    rc = ESMF_FAILURE

    print*, "var_final started"

    !---------------------------------------------------------------------------
    ! wrfvar internal finalize call
    !---------------------------------------------------------------------------
!   call da_wrfvar_finalize

    !---------------------------------------------------------------------------
    ! Garbage collection of objects explicitly created in this component
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "var.array.imp", imp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: getstate var.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(exportState, "var.array.exp", exp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: getstate var.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayGet(exp_array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: arrayget distgrid"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayDestroy(imp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: array destroy var.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayDestroy(exp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: array destroy var.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," var_final failed: destroy distgrid"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*, "var_final finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfvar_component
!-------------------------------------------------------------------------------
