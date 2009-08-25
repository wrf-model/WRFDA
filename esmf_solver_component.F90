!===============================================================================
! !MODULE: esmf_solver_component
!
! !DESCRIPTION:
!===============================================================================
module esmf_solver_component
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod

  ! Model Components
  use esmf_wrfvar_component,    only : var_register
  use esmf_wrfplus_component,   only : plus_register


  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

  public solver_register

  !-----------------------------------------------------------------------------
  ! internal module wide objects
  !-----------------------------------------------------------------------------
  type(ESMF_GridComp), save :: varComp, plusComp
  type(ESMF_State),    save :: varExp, plusExp, varImp, plusImp


!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_register(comp, rc)
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
    ! Local ESMF types

    ! Local variables

    ! initialize return code
    rc = ESMF_FAILURE

    print*,"In Solver register routine"
    !---------------------------------------------------------------------------
    ! register initialize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, solver_init1, 1, rc)  
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, solver_init2, 2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! register run phases
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, solver_run1, 1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, solver_run2, 2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! register finalize phase
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, solver_final,         &
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"Registered Solver Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine solver_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_init1(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialization routine for the solver component. Initializes ESMF objects 
  ! needed for the coupling between the variational model and the NL model.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)           :: vm
    type(ESMF_DistGrid)     :: distgrid
    type(ESMF_ArraySpec)    :: arrayspec
    type(ESMF_Array)        :: arrayInSolver, arrayOutSolver
    type(ESMF_RouteHandle)  :: routehandle_var2plus, routehandle_plus2var

    ! Local variables
    integer                 :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init1 started"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount

    !---------------------------------------------------------------------------
    ! !. Create the import and export Arrays for the solver component
    ! 2. Add the arrays to the solver's import and export States
    ! These objects function as the containers linking to the NL component
    ! through the outer loop coupler.
    !---------------------------------------------------------------------------
    ! right now this is just a place holder, eventually it will need to be
    ! a field or field bundle, and dimensioned correctly for the problem size.
    !---------------------------------------------------------------------------
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: arrayspec set"
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,15/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: distgrid create"
    if (rc/=ESMF_SUCCESS) return ! bail out

    arrayInSolver = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,   &
                      indexflag=ESMF_INDEX_GLOBAL, name="solverIMP.array", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: arraycreate arrayinsolver"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(importState, arrayInSolver, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: stateadd arrayinsolver"
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    arrayOutSolver = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,  &
                     indexflag=ESMF_INDEX_GLOBAL, name="solverEXP.array", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: arraycreate arrayoutsolver"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, arrayOutSolver, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init1 fail: stateadd arrayoutsolver"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Solver_init1 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver_init1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_init2(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Routine creates and initializes Solver child components Var and Plus. Creates
  ! route handles for direct coupling between child components.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)           :: vm
    type(ESMF_Array)        :: varArray_in, varArray_out
    type(ESMF_Array)        :: plusArray_in, plusArray_out
    type(ESMF_RouteHandle)  :: routehandle_var2plus, routehandle_plus2var

    ! Local variables
    integer                 :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(", pet_id,")", " Solver_init2 started"

  !-----------------------------------------------------------------------------
  ! Create two concurrent child components - for var and plus
  ! 1. Create two child components within solver; varComp and plusComp
  ! 2. Initialize varComp and plusComp so that they can create component local 
  !    arrays which are needed here to compute route handles for direct coupling
  ! 3. Save direct coupling route handles to the child component states for use 
  !    within the child components
  !-----------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    ! Create Variational Comp on PET 0
    !---------------------------------------------------------------------------
    varComp = ESMF_GridCompCreate(name="varComp", petList=(/0/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: grid comp create varComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Create Plus (TL and AD) Comp on PET 1
    !---------------------------------------------------------------------------
    plusComp = ESMF_GridCompCreate(name="plusComp", petList=(/1/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: grid comp create plusComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! SetServices for varComp and plusComp
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetServices(varComp, var_register, rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: set services varComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetServices(plusComp, plus_register, rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: set services plusComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Create States for child components to hold the route handles and arrays
    ! to be exchanged
    !---------------------------------------------------------------------------
    varExp = ESMF_StateCreate("varComp export", ESMF_STATE_EXPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: exp state create varComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    varImp = ESMF_StateCreate("varComp import", ESMF_STATE_IMPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: imp state create varComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    plusExp = ESMF_StateCreate("plusComp export", ESMF_STATE_EXPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: exp state create plusComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    plusImp = ESMF_StateCreate("plusComp import", ESMF_STATE_IMPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: imp state create plusComp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 states created"

    !---------------------------------------------------------------------------
    ! Initialize the ESMF objects in varComp and plusComp
    !---------------------------------------------------------------------------
    call ESMF_GridCompInitialize(varComp, importState=varImp,                  &
                                 exportState=varExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," Solver_init2 fail: varComp init"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompInitialize(plusComp, importState=plusImp,                &
                                 exportState=plusExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," Solver_init2 fail: plusComp init"
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 grid comps initalized"

    !---------------------------------------------------------------------------
    ! Reconcile module wide import and export States
    !---------------------------------------------------------------------------
    call ESMF_StateReconcile(varExp, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: state reconcile varExp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateReconcile(varImp, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: state reconcile varImp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateReconcile(plusExp, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: state reconcile plusExp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateReconcile(plusImp, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: state reconcile plusImp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 states reconciled"

    !---------------------------------------------------------------------------
    ! Get access to Arrays in States
    !---------------------------------------------------------------------------
    call ESMF_StateGet(varExp, "var.array.exp", varArray_out, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: stateget var.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(varImp, "var.array.imp", varArray_in, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: stateget var.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(plusImp, "plus.array.imp", plusArray_in, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: stateget plus.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(plusExp, "plus.array.exp", plusArray_out, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: stateget plus.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 state get arrays successful"

    !---------------------------------------------------------------------------
    ! Precompute arrayRedist routehandles for direct coupling exchange between 
    !   var  -->  plus
    !   plus -->  var
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedistStore(srcArray=varArray_out, dstArray=plusArray_in,   &
                               routehandle=routehandle_var2plus, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: Redist store var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayRedistStore(srcArray=plusArray_out, dstArray=varArray_in,   &
                               routehandle=routehandle_plus2var, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">Solver_init2 fail: Redist store var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 array redist store succeeded"

    !---------------------------------------------------------------------------
    ! Give names to RouteHandles
    !---------------------------------------------------------------------------
    call ESMF_RouteHandleSet(routehandle_var2plus, name="var2plus_Redist",rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_RouteHandleSet(routehandle_plus2var, name="plus2var_Redist",rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver_init2 give names to route handles"

    !---------------------------------------------------------------------------
    ! Add both RouteHandles to each component's States for direct coupling
    !---------------------------------------------------------------------------
    call ESMF_StateAdd(varExp, routehandle_var2plus, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(plusImp, routehandle_var2plus, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    call ESMF_StateAdd(plusExp, routehandle_plus2var, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(varImp, routehandle_plus2var, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(", pet_id,")", " Solver_init2 add route handles to state succeeded"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Solver_init2 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver_init2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_run1(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_Array)                    :: imp_array
    type(ESMF_VM)                       :: vm

    ! Local variables
    real(ESMF_KIND_R8), pointer         :: farrayPtr(:,:)
    integer                             :: i, j
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

    print*,"(",pet_id,")"," Solver_run1 started"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount

    !---------------------------------------------------------------------------
    ! Get the import Array
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "solverIMP.array", imp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_run1 failed: state get solverIMP.array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(imp_array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_run1 failed: array get exp_array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! extract Array Data
    print*,"(", pet_id,")", " solver_run1: values in transfered array"
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
       do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
          if( abs( farrayPtr(i,j) - 3.5d0 ) > 1.0e-10 )      &
             print*,"(",i,",",j,") ", farrayPtr(i,j)
       enddo
    enddo
    print*,"(", pet_id,")", " solver_run1: checked source array"

    !---------------------------------------------------------------------------
    ! Run varComp and plusComp concurrently -> direct coupling
    !---------------------------------------------------------------------------
    call ESMF_GridCompRun(varComp, importState=varImp,                         &
                          exportState=varExp, phase=1, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"(", pet_id,")", " solver_run1 error: varcomp1 run"
    if (rc/=ESMF_SUCCESS) return ! bail out
    flush(6)
    call ESMF_GridCompRun(varComp, importState=varImp,                         &
                          exportState=varExp, phase=2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"(", pet_id,")", " solver_run1 error: varcomp2 run"
    flush(6)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompRun(plusComp, importState=plusImp,                       &
                          exportState=plusExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"(", pet_id,")", " solver_run1 error: pluscomp run"
    flush(6)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Solver_run1 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver_run1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_run2(comp, importState, exportState, externalclock, rc)
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
    ! Local ESMF types
    type(ESMF_VM)                       :: vm

    ! Local variables
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

    print*,"(",pet_id,")"," Solver_run2 started"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount

    !---------------------------------------------------------------------------
    ! Run varComp and plusComp concurrently -> direct coupling
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Solver_run2 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine Solver_run2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver_final(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_VM)           :: vm
    type(ESMF_RouteHandle) :: routehandle_var2plus,routehandle_plus2var

    ! Local variables
    integer                 :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"(",pet_id,")"," Solver finialize started"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount

    !---------------------------------------------------------------------------
    ! Finalize varComp
    !---------------------------------------------------------------------------
    call ESMF_GridCompFinalize(varComp, importState=varImp,                    &
                               exportState=varExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: grid comp final varcomp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Finalize plusComp
    !---------------------------------------------------------------------------
    call ESMF_GridCompFinalize(plusComp, importState=plusImp,                  &
                                 exportState=plusExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: grid comp final pluscomp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Get access to the RouteHandle and release
    !---------------------------------------------------------------------------
    call ESMF_StateGet(varExp, "var2plus_Redist", routehandle_var2plus, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: stateget route var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayRedistRelease(routehandle=routehandle_var2plus, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: release route var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_StateGet(varImp, "plus2var_Redist", routehandle_plus2var, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: stateget route plus2var"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayRedistRelease(routehandle=routehandle_plus2var, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: release route plus2var"
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! Destroy both internal model Components
    !---------------------------------------------------------------------------
    call ESMF_GridCompDestroy(varComp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: comp destroy varcomp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompDestroy(plusComp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: comp destroy pluscomp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Destroy both internal model States
    !---------------------------------------------------------------------------
    call ESMF_StateDestroy(varExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: state destroy varExp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateDestroy(varImp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: state destroy varImp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateDestroy(plusExp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: state destroy plusExp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateDestroy(plusImp, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," solver_final fail: state destroy plusImp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Solver finialize finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_solver_component
!-------------------------------------------------------------------------------
