!===============================================================================
! !MODULE: esmf_outerloop_coupler
!
! !DESCRIPTION:
!
! This module contains the routine for registering, initalizing, running and 
! finializing the coupler that links variational solver module and the NL 
! or forward WRF module.
!===============================================================================
module esmf_outerloop_coupler
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod
    
  implicit none
  private
 
!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

  public nl2solver_register, solver2nl_register
 
  ! global data
  type(ESMF_RouteHandle), save :: solver2nl_routehandle, nl2solver_routehandle


!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl2solver_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The ESMF Register routine that registers the coupler linking the NL forward
  !  component with the variational solver component. Sets the subroutines to be 
  !  called as the init, run, and finalize routines. 
  !-----------------------------------------------------------------------------
    ! local ESMF types

    ! Local variables

    ! initialize return code
    rc = ESMF_FAILURE
    
    print*,"In nl2solver Register Routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, nl2solver_init,         &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, nl2solver_run,           &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, nl2solver_final,       &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"Registered nl2solver Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl2solver_register
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver2nl_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The ESMF Register routine that registers the coupler linking the
  !  variational solver component with the NL forward model component. 
  !  Sets the subroutines to be called as the init, run, and finalize routines.
  !-----------------------------------------------------------------------------
    ! local ESMF types

    ! Local variables

    ! initialize return code
    rc = ESMF_FAILURE
    
    print*,"In solver2nl Register Routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, solver2nl_init,         &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, solver2nl_run,           &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, solver2nl_final,       &
                                   ESMF_SINGLEPHASE, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    print*,"Registered solver2nl Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver2nl_register
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver2nl_init(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  !<DESCRIPTION>
  ! !DESCRIPTION:
  ! Initialization routine for the coupling between the variational solver and
  ! the NL forward model.
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    type(ESMF_Array)                   :: src_array, dst_array
    
    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " Coupler solver 2 nl: Initialization Phase start"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets

    !---------------------------------------------------------------------------
    ! Reconcile import and export States
    !---------------------------------------------------------------------------
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! Get access to src and dst Arrays in States
    ! srcArray from solver/import state
    ! dstArray from NL/export state
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "solverEXP.array", src_array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "nlIMP.array", dst_array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Precompute and store ArrayRedist routehandle for srcArray --> dstArray
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedistStore(src_array, dst_array,         &
                               solver2nl_routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! Name the RouteHandle
    !---------------------------------------------------------------------------
    call ESMF_RouteHandleSet(solver2nl_routehandle, name="solver2nl_redist",   &
                             rc=rc )
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Coupler solver 2 nl: Initialization Phase start"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver2nl_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver2nl_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The SOLVER2NL coupler run routine, couples variational solver and the NL.
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    type(ESMF_Array)                   :: src_array, dst_array
    
    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " Coupler solver 2 nl: Run Phase started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets   
     
    !---------------------------------------------------------------------------
    ! Conduct the redistribution between the solver and the nl components
    !---------------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX add redist call when actually have
!something to pass back - multiple outerloops?

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Coupler solver 2 nl: Run Phase finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver2nl_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine solver2nl_final(comp, importState, exportState, externalclock, rc)
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
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    
    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " Coupler solver 2 nl: Finalize Phase start"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets

    !---------------------------------------------------------------------------
    ! Release routeHandle
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedistRelease(routehandle=solver2nl_routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")"," Coupler solver 2 nl: Finalize Phase finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine solver2nl_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl2solver_init(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialization routine for the coupling between the NL model and the
  ! variational solver.
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    type(ESMF_Array)                   :: src_array, dst_array

    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " Coupler nl 2 solver: Initialization Phase started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets    

    !---------------------------------------------------------------------------
    ! Reconcile import and export States
    !---------------------------------------------------------------------------
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! Get access to src and dst Arrays in States
    ! srcArray from NL/import state
    ! dstArray from solver/export state
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "nlEXP.array", src_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," state get nlEXP.array failed"
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "solverIMP.array", dst_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," state get solverIMP.array failed"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Precompute and store ArrayRedist routehandle for srcArray --> dstArray
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedistStore(src_array, dst_array,         &
                               nl2solver_routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) &
                  print*," Redist Store nl 2 solver failed"
    if (rc/=ESMF_SUCCESS) return ! bail out
    print*," Redist Store nl 2 solver"
    
    !---------------------------------------------------------------------------
    ! Name the RouteHandle
    !---------------------------------------------------------------------------
    call ESMF_RouteHandleSet(nl2solver_routehandle, name="nl2solver_redist",   &
                             rc=rc )
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Coupler nl 2 solver: Initialization Phase finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl2solver_init
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl2solver_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_CplComp), intent(inout)  :: comp
    type(ESMF_State)                   :: importState, exportState
    type(ESMF_Clock)                   :: externalclock
    integer, intent(out)               :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! The run routine for the coupling between the NL model and the variational
  ! solver. 
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    type(ESMF_Array)                   :: src_array, dst_array
    
    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " Coupler nl 2 solver: Run Phase started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets   
     
    !---------------------------------------------------------------------------
    ! Get access to src and dst Arrays in the States for the redistribution
    !   srcArray from NL/import state
    !   dstArray from solver/export state
    !---------------------------------------------------------------------------
    call ESMF_StateGet(importState, "nlEXP.array", src_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl2solver run: state get import nlEXP.array failed"
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "solverIMP.array", dst_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl2solver run: state get export solverIMP.array failed"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! Perform data redistribution - send export from NL comp to solver comp
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedist(srcArray=src_array, dstArray=dst_array,              &
             routehandle=nl2solver_routehandle, checkflag=ESMF_TRUE, rc=rc)
                          !  remove check flag when not debugging for best
                          !  performance
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " Coupler nl 2 solver: Run Phase finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl2solver_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl2solver_final(comp, importState, exportState, externalclock, rc)
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
  !
  !     The arguments are:
  !       comp            Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                      :: vm
    
    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")"," Coupler nl 2 solver: Finalize Phase started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets

    !---------------------------------------------------------------------------
    ! Release routeHandle
    !---------------------------------------------------------------------------
    call ESMF_ArrayRedistRelease(routehandle=nl2solver_routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")"," Coupler nl 2 solver: Finalize Phase finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl2solver_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_outerloop_coupler
!-------------------------------------------------------------------------------
