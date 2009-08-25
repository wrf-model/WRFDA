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

    public plus_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine plus_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The Register routine for the plus component.
  !  Sets the subroutines to be called as the init, run, and finalize routines.
  !-----------------------------------------------------------------------------
    ! local integer variables

    ! initialize return code
    rc = ESMF_FAILURE

    print*, "In plus register routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, plus_init,             & 
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
 
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, plus_run,               &
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, plus_final,           &
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    print *, "Registered Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !---------------------------------------------------------------------------
  end subroutine plus_register
  !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine plus_init(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_Array)                    :: plus_array_in, plus_array_out

    ! Local variables
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


    print*,"(",pet_id,")"," Plus init routine"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount
!   call ESMF_VMPrint(vm, rc)

    !---------------------------------------------------------------------------
    ! Create a data Array and add it to the import and export State
    ! This array will be used to construct the routehandle for the direct
    ! coupling between the var and plus components.
    !---------------------------------------------------------------------------
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init arrayspecset error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,15/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init distgrid create error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    plus_array_in = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,   &
                indexflag=ESMF_INDEX_GLOBAL, name="plus.array.imp", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init array create error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(importState, plus_array_in, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init state add import error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    plus_array_out = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,  &
                indexflag=ESMF_INDEX_GLOBAL, name="plus.array.exp", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init array create error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, plus_array_out, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,"Plus_init state add export error"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Plus init routine finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine plus_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine plus_run(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_VM)                       :: vm
    type(ESMF_RouteHandle)  :: var2plus_Redist, plus2var_Redist
    type(ESMF_Array)        :: array1, array2

    ! Local variables
    integer                             :: petCount, pet_id

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    ! Get VM and determine petCount
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPET=pet_id, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    print*,"(",pet_id,")"," Plus run routine start"
    print*,'Local Pet id:', pet_id, '  number of pets:', petCount
!   call ESMF_VMPrint(vm, rc)

    !---------------------------------------------------------------------------
    ! Gain access to RouteHandles for direct coupling between Var and Plus
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "var2plus_Redist",                         &
                       routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: state get var2plus_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(exportState, "plus2var_Redist",                         &
                       routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: state get plus2var_redist"
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    flush(6)
    !---------------------------------------------------------------------------
    ! Get the Array from the export State
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "plus.array.imp", array1, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: state get plus.array.imp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(exportState, "plus.array.exp", array2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: state get plus.array.exp"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    !---------------------------------------------------------------------------
    ! Main Run Loop - direct coupling between Plus and Var
    !---------------------------------------------------------------------------
    ! -> wait until contacted by Var component

    ! ArrayRedist() "receive" from Var
    call ESMF_ArrayRedist(dstArray=array1, routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: redist var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    ! -> run the tangent linear model

    ! ArrayRedist() "send" to Var from Plus-tangent linear
    call ESMF_ArrayRedist(srcArray=array2, routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: redist plus2var"
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    flush(6)
    ! -> wait while Var computes input for adjont

    ! ArrayRedist() "receive" from Var
!   call ESMF_ArrayRedist(dstArray=array1, routehandle=var2plus_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: redist2 var2plus"
    if (rc/=ESMF_SUCCESS) return ! bail out

    flush(6)
    ! -> run the adjoint model

    ! ArrayRedist() "send" to Var from Plus-adjoint
!   call ESMF_ArrayRedist(srcArray=array1, routehandle=plus2var_Redist, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," plus run failed: redist2 plus2var"
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! -> wait for further iterations


    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(",pet_id,")"," Plus run routine finish"
    flush(6)
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine plus_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine plus_final(comp, importState, exportState, externalclock, rc)
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
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: imp_array

    ! initialize return code
    rc = ESMF_FAILURE

    print*, "In plus final routine"

    !---------------------------------------------------------------------------
    ! Garbage collection of objects explicitly created in this component
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "plus.array.imp", imp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(imp_array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(imp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*, "exiting plus final routine"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine plus_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfplus_component
!-------------------------------------------------------------------------------

