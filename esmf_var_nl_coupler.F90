!===============================================================================
! !MODULE: esmf_var_nl_coupler
!
! !DESCRIPTION:
!
! This module contains the routine for registering, initalizing, running and 
! finializing the coupling between the top level Var dirver module and the NL 
! or forward WRF module.
!===============================================================================
module esmf_var_nl_coupler
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod
    
  implicit none
  private
 
!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

  public var_to_nl_cpl_register, nl_to_var_cpl_register
 
       
  ! global data
  type(ESMF_RouteHandle), save :: redistRH12

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_to_nl_cpl_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The Register routine for coupling from the main driver Var routine to the 
  !  forward NL model. Sets the subroutines to be called as the init, run, and
  !  finalize routines.  
  !-----------------------------------------------------------------------------
  
    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, var_to_nl_init,         &
                                   ESMF_SINGLEPHASE, localrc)

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, var_to_nl_run,           &
                                   ESMF_SINGLEPHASE, localrc)

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, var_to_nl_final,       &
                                   ESMF_SINGLEPHASE, localrc)


    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_to_nl_cpl_register
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_to_var_cpl_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The Register routine for coupling from the NL forward wrf model back to
  !  the main driver Var routine.
  !  Sets the subroutines to be called as the init, run, and finalize routines.
  !-----------------------------------------------------------------------------
  
    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, nl_to_var_init,         &
                                   ESMF_SINGLEPHASE, localrc)

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, nl_to_var_run,           &
                                   ESMF_SINGLEPHASE, localrc)

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, nl_to_var_final,       &
                                   ESMF_SINGLEPHASE, localrc)


    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_to_var_cpl_register
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_to_nl_init(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialization routine for the coupling between the main Var model and the
  ! NL model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    type(ESMF_Array)                   :: sorted_data1, sorted_data2

    ! Local variables
    integer                            :: localrc
    integer                            :: itemcount
    integer                            :: pet_id
  
    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    print *, "User Coupler Init starting"
  
    ! Get VM from coupler component
    call ESMF_CplCompGet(comp, vm=vm, rc=localrc)

    call ESMF_VMGet(vm, localPET=pet_id, rc=localrc)
  
    ! Since the components we are coupling between are running concurrently,
    ! they have each separately created ESMF objects.   We are planning to
    ! use a communications call (Redist) here, so first we must make a new
    ! call to reconcile the object lists in all the import and export states.
  
    ! New routine:
    ! Must be called on each state which is going to be accessed from
    ! this coupler.  When the call returns all objects which were not
    ! in existence on all PETs now have an object which represents them.
    call ESMF_StateReconcile(importState, vm, rc=localrc)   

    !call ESMF_StatePrint(importState, rc=localrc)   
    !if(ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc)) return
  
    call ESMF_StateReconcile(exportState, vm, rc=localrc)   

    !call ESMF_StatePrint(exportState, rc=localrc)   
    !if(ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc)) return
  
    call ESMF_StateGet(importState, itemcount=itemcount, rc=localrc)   

    !print *, "Import State contains ", itemcount, " items."

    call ESMF_StateGet(exportState, itemcount=itemcount, rc=localrc)   

    !print *, "Export State contains ", itemcount, " items."

    ! Get the src and dst arrays
    call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=localrc)      

    call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=localrc)      

    ! precompute redist handle
    call ESMF_ArrayRedistStore(sorted_data1, sorted_data2, redistRH12, rc=localrc)

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_to_nl_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_to_nl_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The run routine for the coupling between the main Var model and the NL model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_Array) :: sorted_data1, sorted_data2

    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    print *, "User Coupler Run starting"
        
    ! query data from States
    call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=localrc)   

    call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=localrc)   

    ! preform data redistribution
    ! deliver sorted result from component 1 to component 2
    ! component 2 will verify component 1 result
    call ESMF_ArrayRedist(srcArray=sorted_data1, dstArray=sorted_data2, &
          routehandle=redistRH12, checkflag=.true., rc=localrc)

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_to_nl_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine var_to_nl_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The Finalization routine where things are deleted and cleaned up from the 
  ! coupling between the main Var model and the NL model.
  !-----------------------------------------------------------------------------

    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------

    ! release route handle
    call ESMF_ArrayRedistRelease(redistRH12, rc=localrc)    

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine var_to_nl_final
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_to_var_init(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialization routine for the coupling between the NL model and the main 
  ! Var model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_VM) :: vm
    type(ESMF_Array) :: sorted_data1, sorted_data2

    ! Local variables
    integer :: itemcount
    integer :: pet_id
    integer :: localrc
  
    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE


    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    print *, "User Coupler Init starting"
    ! Get VM from coupler component
    call ESMF_CplCompGet(comp, vm=vm, rc=localrc)

    call ESMF_VMGet(vm, localPET=pet_id, rc=localrc)
  
    ! Since the components we are coupling between are running concurrently,
    ! they have each separately created ESMF objects.   We are planning to
    ! use a communications call (Redist) here, so first we must make a new
    ! call to reconcile the object lists in all the import and export states.
  
    ! New routine:
    ! Must be called on each state which is going to be accessed from
    ! this coupler.  When the call returns all objects which were not
    ! in existence on all PETs now have an object which represents them.
    call ESMF_StateReconcile(importState, vm, rc=localrc)   

    !call ESMF_StatePrint(importState, rc=localrc)   
    !if(ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc)) return
  
    call ESMF_StateReconcile(exportState, vm, rc=localrc)   

    !call ESMF_StatePrint(exportState, rc=localrc)   
    !if(ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc)) return
  
    call ESMF_StateGet(importState, itemcount=itemcount, rc=localrc)   

    !print *, "Import State contains ", itemcount, " items."

    call ESMF_StateGet(exportState, itemcount=itemcount, rc=localrc)   

    !print *, "Export State contains ", itemcount, " items."

    ! Get the src and dst arrays
    call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=localrc)      

    call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=localrc)      

    ! precompute redist handle
    call ESMF_ArrayRedistStore(sorted_data1, sorted_data2, redistRH12, rc=localrc)

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_to_var_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_to_var_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The run routine for the coupling between the NL model and the main Var model.
  !-----------------------------------------------------------------------------

    ! Local ESMF types
    type(ESMF_Array) :: sorted_data1, sorted_data2

    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE


    !---------------------------------------------------------------------------
    print *, "User Coupler Run starting"
        
    ! query data from States
    call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=localrc)   

    call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=localrc)   

    ! preform data redistribution
    ! deliver sorted result from component 1 to component 2
    ! component 2 will verify component 1 result
    call ESMF_ArrayRedist(srcArray=sorted_data1, dstArray=sorted_data2, &
          routehandle=redistRH12, checkflag=.true., rc=localrc)

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_to_var_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_to_var_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The Finalization routine where things are deleted and cleaned up from the 
  ! NL model to main Var model coupling.
  !-----------------------------------------------------------------------------

    ! local integer variables
    integer :: localrc

    ! initialize return code
    rc = ESMF_FAILURE
    localrc = ESMF_FAILURE

    !---------------------------------------------------------------------------

    ! release route handle
    call ESMF_ArrayRedistRelease(redistRH12, rc=localrc)    

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_to_var_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_var_nl_coupler
!-------------------------------------------------------------------------------
