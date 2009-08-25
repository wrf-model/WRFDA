!===============================================================================
! !MODULE: esmf_wrfnl_component
!
! !DESCRIPTION:
! ESMF Component interfaces for the WRF NL (or forward WRF model) component
!===============================================================================
module esmf_wrfnl_component
!===============================================================================
! !USES:

  ! ESMF Framework module
  use ESMF_Mod
!  use module_wrf_top, ONLY : wrf_init, wrf_run, wrf_finalize

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------

    public nl_register

!===============================================================================

  contains

!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_register(comp, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !  The Register routine for the WRF nonlinear forward model.
  !  Sets the subroutines to be called as the init, run, and
  !  finalize routines.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    
    ! Local variables
    
    ! initialize return code
    rc = ESMF_FAILURE

    print*,"In NL component Register Routine"
    !---------------------------------------------------------------------------
    ! Register the callback routines.
    !---------------------------------------------------------------------------
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, nl_init1, 1, rc) 
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, nl_init2, 2, rc)     
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, nl_run,                 &
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, nl_final,             &
                                    ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print*,"Registered NL Comp Initialize, Run, and Finalize routines"

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine nl_register
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!===============================================================================
! Private Methods
!===============================================================================
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_init1(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !   - Initializes the WRF NL model component.  
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
    type(ESMF_VM)                       :: vm
    type(ESMF_Array)                    :: arrayIn, arrayOut
    type(ESMF_DistGrid)                 :: distgrid
    type(ESMF_ArraySpec)                :: arrayspec

    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE
 
  !-----------------------------------------------------------------------------
  ! Set up objects for ESMF communication
  !-----------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " wrfnl_init1 started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets
 
    !---------------------------------------------------------------------------
    ! !. Create the import and export Arrays for the NL component
    ! 2. Add the arrays to the NL components import and export States
    ! These objects function as the containers linking to the solver component
    ! through the outer loop coupler.
    !---------------------------------------------------------------------------
    ! right now this is just a place holder, eventually it will need to be
    ! a field or field bundle, and dimensioned correctly for the problem size.
    !---------------------------------------------------------------------------
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: arrayspec set"
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,15/), rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: distgrid create"
    if (rc/=ESMF_SUCCESS) return ! bail out

    arrayOut = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,        &
                   indexflag=ESMF_INDEX_GLOBAL, name="nlEXP.array", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: arraycreate arrayOut"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, arrayOut, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: stateadd arrayout"
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    arrayIn = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid,         &
                   indexflag=ESMF_INDEX_GLOBAL, name="nlIMP.array", rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: arraycreate arrayIn"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(importState, arrayIn, rc=rc)
    if (rc/=ESMF_SUCCESS) print*,">NL_init1 fail: stateadd arrayin"
    if (rc/=ESMF_SUCCESS) return ! bail out

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " nl_init1 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_init1
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_init2(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !   - Initializes the WRF NL model component's import and export state.  
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_VM)                       :: vm

    ! Local variables
    integer :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " nl_init2 started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets

  !-----------------------------------------------------------------------------
  ! Call first phase of WRF nonlinear model initialization
  !-----------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! call to disable processor quilting (e.g. wrf real)
    !---------------------------------------------------------------------------
!   call disable_quilting

    !---------------------------------------------------------------------------
    ! call phase 2 of init_mnodules, phase 1 is skipped since ESMF manages the
    ! parallel environment. Thus these two calls
    ! 	- CALL init_modules(1)
    ! 	- CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN )
    ! are skipped.
    !---------------------------------------------------------------------------
!   call init_modules(2)

    !---------------------------------------------------------------------------
    ! Call WRF "init" routine, which, for a DM_PARALLEL run, will recognize
    ! that ESMF has already called MPI_INIT and respond appropriately.
    !---------------------------------------------------------------------------
!   call wrf_init( no_init1=.TRUE. )

    !---------------------------------------------------------------------------
    ! For now, use settings from WRF component intialization to set up
    ! top-level clock.  Per suggestion from ESMF Core team, these are passed
    ! back as attributes on exportState.
    !---------------------------------------------------------------------------
!   call wrf_clockprint( 100, head_grid%domain_clock,                          &
!           'DEBUG wrf_component_init1():  head_grid%domain_clock,' )


    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " nl_init2 finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_init2
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_run(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(out)                :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !     WRF NL model component run routine.
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! Local ESMF types
    type(ESMF_Array)                    :: exp_array
    type(ESMF_VM)                       :: vm

    ! Local variables
    real(ESMF_KIND_R8), pointer         :: srcfarrayPtr(:,:)
    integer                             :: i, j
    integer                             :: pet_id, npets
    
    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) return ! bail out
    
    print*,"(", pet_id,")", " nl_run started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets
    
    !---------------------------------------------------------------------------
    ! Get the export Array
    !---------------------------------------------------------------------------
    call ESMF_StateGet(exportState, "nlEXP.array", exp_array, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_run failed: state get nlEXP.array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(exp_array, localDe=0, farrayPtr=srcfarrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_run failed: array get exp_array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data
    do j = lbound(srcfarrayPtr, 2), ubound(srcfarrayPtr, 2)
      do i = lbound(srcfarrayPtr, 1), ubound(srcfarrayPtr, 1)
        srcfarrayPtr(i,j) = 3.5d0
      enddo
    enddo

    print*,"(", pet_id,")", " nl_run: fill source array"

    !---------------------------------------------------------------------------
    ! Call WRF "run" routine
    !---------------------------------------------------------------------------
    print *, "wrfnl: run phase"
!   call wrf_debug ( 100 , 'DEBUG wrf_component_run():  calling wrf_run()' )
!   call wrf_run( )
!   call wrf_debug ( 100 , 'DEBUG wrf_component_run():  back from wrf_run()' )

!   call wrf_debug ( 100 , 'DEBUG wrf_component_run():  end' )

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " nl_run finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_run
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine nl_final(comp, importState, exportState, externalclock, rc)
  !-----------------------------------------------------------------------------
  ! !ARGUMENTS:
  !-----------------------------------------------------------------------------
    type(ESMF_GridComp), intent(inout)  :: comp
    type(ESMF_State)                    :: importState, exportState
    type(ESMF_Clock)                    :: externalclock
    integer, intent(  out)              :: rc
  !-----------------------------------------------------------------------------
  ! !DESCRIPTION:
  !     WRF component finalize routine.
  !
  !     The arguments are:
  !       gcomp           Component
  !       importState     Importstate
  !       exportState     Exportstate
  !       clock           External clock
  !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
  !                       otherwise ESMF_FAILURE.
  !-----------------------------------------------------------------------------
    ! local ESMF types
    type(ESMF_DistGrid)                 :: distgrid
    type(ESMF_Array)                    :: arrayIn, arrayOut
    type(ESMF_VM)                       :: vm

    ! Local variables
    integer :: pet_id, npets

    ! initialize return code
    rc = ESMF_FAILURE

    !---------------------------------------------------------------------------
    ! Get VM and determine local pet and pet count
    !---------------------------------------------------------------------------
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if(rc /= ESMF_SUCCESS) then                                            
       print*,'Failed to get VM'
       return ! bail out
    endif
    
    print*,"(", pet_id,")", " nl_final started"
    print*,'Local Pet id:', pet_id, '  number of pets:',npets
    
    !---------------------------------------------------------------------------
    ! Call WRF "finalize" routine, suppressing call to MPI_FINALIZE so
    ! ESMF can do it during ESMF_Finalize().
    !---------------------------------------------------------------------------
!    CALL wrf_finalize( no_shutdown=.TRUE. )


  !-----------------------------------------------------------------------------
  ! Shutdown and Garbage collection for ESMF objects.
  !-----------------------------------------------------------------------------
    
    !---------------------------------------------------------------------------
    ! Garbage collection of objects explicitly created in this component
    !---------------------------------------------------------------------------
    call ESMF_StateGet(exportState, "nlEXP.array", arrayOut, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_final fail: state get nlEXP.array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayDestroy(arrayOut, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_final fail: array destroy arrayOut"
    if (rc/=ESMF_SUCCESS) return ! bail out    

    call ESMF_StateGet(importState, "nlIMP.array", arrayIn, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_final fail: state get nlIMP.array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayGet(arrayIn, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_final fail: state get nlImp.array"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ArrayDestroy(arrayIn, rc=rc)
    if (rc/=ESMF_SUCCESS) print*," nl_final fail: array destroy arrayIn"
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    print*,"(", pet_id,")", " nl_final finished"
    rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine nl_final
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end module esmf_wrfnl_component
!-------------------------------------------------------------------------------

