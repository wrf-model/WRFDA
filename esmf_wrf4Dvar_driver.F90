!===============================================================================
  program ConcurrentComponent
!===============================================================================
  ! ESMF Framework module
  use ESMF_Mod

  use esmf_var_nl_coupler, only : var_to_nl_cpl_register, nl_to_var_cpl_register
  use esmf_wrfvar_component, only : wrfvar_register
  use esmf_wrfnl_component, only : wrfnl_register
  use esmf_wrfplus_component, only : wrfplus_register

  implicit none
    
  ! Local ESMF types
  type(ESMF_VM)        :: vm
  type(ESMF_Clock)     :: clock
  type(ESMF_State)     :: c1export, c2export, c3export
  type(ESMF_State)     :: c1import, c2import, c3import
  type(ESMF_GridComp)  :: comp1, comp2, comp3
  type(ESMF_CplComp)   :: cpl12, cpl21

  ! Local variables
  integer :: pet_id, ipet, npets, rc, localrc
  integer :: nVarPets, nNLPets, nPlusPets
  integer, allocatable :: var_to_nl_petlist(:), nl_to_var_petlist(:)
  integer, allocatable :: plus_petlist(:), nl_petlist(:), var_petlist(:)
  character(len=ESMF_MAXSTR) :: cname1, cname2, cname3
  character(len=ESMF_MAXSTR) :: cplname12, cplname21

  ! initialize error logging
  localrc = ESMF_FAILURE
  rc = ESMF_FAILURE

  ! set distribution sizes
  nVarPets  =  8
  nNLPets   = 16
  nPlusPets =  8

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  ! Initialize framework and get back default global VM
  !-----------------------------------------------------------------------------
  call ESMF_Initialize(vm=vm, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,'Failed ESMF Initialize'
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  !-----------------------------------------------------------------------------
  ! Get number of PETs 
  !-----------------------------------------------------------------------------
  call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,'Failed to get VM'
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  !-----------------------------------------------------------------------------
  ! check that we are running with at least 32 PETs
  !-----------------------------------------------------------------------------
  if (npets < 32) then
     print *, "This system test needs 32 PETS to run, current np = ", npets 
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  !-----------------------------------------------------------------------------
  ! set up the pet lists for each component, including couplers
  !-----------------------------------------------------------------------------
  allocate( var_petlist(nVarPets) )
  allocate( nl_petlist(nNLPets) )
  allocate( plus_petlist(nPlusPets) )
  allocate( var_to_nl_petlist(nVarPets+nNLPets) )
  allocate( nl_to_var_petlist(nVarPets+nNLPets) )

  ! petlist for Var component
  do ipet=1, nVarPets
     var_petlist(ipet) = ipet-1
  enddo
  ! petlist for NL component
  do ipet=1, nNLPets
     nl_petlist(ipet) = nVarPets + ipet - 1 
  enddo
  ! petlist for Plus component
  do ipet=1, nPlusPets
     plus_petlist(ipet) = nVarPets + nNLPets + ipet - 1 
  enddo

  ! petlist for Var to NL coupler component
  do ipet=1, nVarPets + nNLPets 
     var_to_nl_petlist(ipet) = ipet-1
     nl_to_var_petlist(ipet) = ipet-1
  enddo
   
  !-----------------------------------------------------------------------------
  ! print out pet list
  !-----------------------------------------------------------------------------
  print*,' Var petlist:', var_petlist(1:nVarPets)
  print*,' NL petlist:', nl_petlist(1:nNLPets)
  print*,' Plus petlist:', plus_petlist(1:nPlusPets)
  print*,' Var to NL petlist:', var_to_nl_petlist(1:nVarPets+nNLPets)
  print*,' NL to Var petlist:', nl_to_var_petlist(1:nVarPets+nNLPets)

  !-----------------------------------------------------------------------------
  ! Create the 3 model components and couplers
  !-----------------------------------------------------------------------------
  ! 4D Var driver
  !-----------------------------------------------------------------------------
  cname1 = "wrfvar"
  comp1 = ESMF_GridCompCreate(name=cname1, petList=var_petlist, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cname1), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Created component ", trim(cname1), "rc =", rc

  !-----------------------------------------------------------------------------
  ! Nonlinear Model (forward WRF)
  !-----------------------------------------------------------------------------
  cname2 = "wrfnl"
  comp2 = ESMF_GridCompCreate(name=cname2, petList=nl_petlist, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cname2), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Created component ", trim(cname2), "rc =", rc

  !-----------------------------------------------------------------------------
  ! Tangent linear and adjoint models
  !-----------------------------------------------------------------------------
  cname3 = "wrfplus"
  comp3 = ESMF_GridCompCreate(name=cname3, petList=plus_petlist, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cname3), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Created component ", trim(cname2), "rc =", rc

  !-----------------------------------------------------------------------------
  ! coupler from Variational model to Nonlinear model
  !-----------------------------------------------------------------------------
  cplname12 = "var to nl coupler"
  cpl12 = ESMF_CplCompCreate(name=cplname12,petList=var_to_nl_petlist,         &
             rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cplname12), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Created component ", trim(cplname12), ", rc =", rc

  !-----------------------------------------------------------------------------
  ! coupler from Nonlinear model to Variational model
  !-----------------------------------------------------------------------------
  cplname21 = "nl to var coupler"
  cpl21 = ESMF_CplCompCreate(name=cplname21,petList=nl_to_var_petlist,rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cplname21), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Created component ", trim(cplname21), ", rc =", rc

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Component 1
  !-----------------------------------------------------------------------------
  call ESMF_GridCompSetServices(comp1, wrfvar_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp1 SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Comp1 SetServices finished, rc= ", localrc

  !-----------------------------------------------------------------------------
  ! Component 2
  !-----------------------------------------------------------------------------
  call ESMF_GridCompSetServices(comp2, wrfnl_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"Comp2 Setservices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Comp2 SetServices finished, rc= ", localrc

  !-----------------------------------------------------------------------------
  ! Component 3
  !-----------------------------------------------------------------------------
  call ESMF_GridCompSetServices(comp3, wrfplus_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"Comp3 Setservices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Comp3 SetServices finished, rc= ", localrc

  !-----------------------------------------------------------------------------
  ! Coupler Component 12
  !-----------------------------------------------------------------------------
  call ESMF_CplCompSetServices(cpl12, var_to_nl_cpl_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Cpl12 SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Cpl12 SetServices finished, rc= ", localrc

  !-----------------------------------------------------------------------------
  ! Coupler Component 21
  !-----------------------------------------------------------------------------
  call ESMF_CplCompSetServices(cpl21, nl_to_var_cpl_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Cpl21 SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Cpl21 SetServices finished, rc= ", localrc


!===============================================================================
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  ! Var dirver component
  !-----------------------------------------------------------------------------
  c1import = ESMF_StateCreate("Var model import", ESMF_STATE_IMPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 1 import state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  c1export = ESMF_StateCreate("Var model export", ESMF_STATE_EXPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 1 export state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompInitialize(comp1, importState=c1import, &
        exportState=c1export, &
                               rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 1 Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
                               
  print *, "Comp 1 Initialize finished, rc =", rc
 
  !-----------------------------------------------------------------------------
  ! Nonlinear model component
  !-----------------------------------------------------------------------------
  c2import = ESMF_StateCreate("NL model import", ESMF_STATE_IMPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 2 import state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  
  c2export = ESMF_StateCreate("NL model export", ESMF_STATE_EXPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 2 export state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  

  call ESMF_GridCompInitialize(comp2, importState=c2import, & 
          exportState=c2export, &
          rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 2 Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Comp 2 Initialize finished, rc =", rc

  !-----------------------------------------------------------------------------
  ! Plus (tangent linear anf adjoint) model component
  !-----------------------------------------------------------------------------
  c3import = ESMF_StateCreate("Plus model import", ESMF_STATE_IMPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 3 import state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  c3export = ESMF_StateCreate("Plus model export", ESMF_STATE_EXPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 3 export state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompInitialize(comp3, importState=c3import, & 
          exportState=c3export, &
          rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Comp 3 Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Comp 3 Initialize finished, rc =", rc
 
  !-----------------------------------------------------------------------------
  ! Coupler components
  !-----------------------------------------------------------------------------
  ! note that the coupler's import is comp1's export
  call ESMF_CplCompInitialize(cpl12, c1export, c2import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Coupler 12 Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Coupler 12 Initialize finished, rc =", rc

    ! note that the coupler's import is comp2's export
  call ESMF_CplCompInitialize(cpl12, c2export, c1import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Coupler 12 Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print *, "Coupler 12 Initialize finished, rc =", rc

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp2 has returned. Consequently comp1 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp2, blockingflag=ESMF_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    
      ! Run the first component:
      ! After the first time thru the loop this will be running concurrently 
      ! with the second component since comp1 and comp2 are defined on 
      ! exclusive sets of PETs
      !print *, "I am calling into GridCompRun(comp1)"
  !-----------------------------------------------------------------------------
 !call ESMF_GridCompRun(comp1, exportState=c1export, clock=clock, rc=localrc)
  !-----------------------------------------------------------------------------
 !print *, "Comp 1 Run returned, rc =", localrc
  !-----------------------------------------------------------------------------
 !if(localrc /= ESMF_SUCCESS) then                                            
 !   call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
 !endif

      ! Uncomment the following calls to ESMF_GridCompWait() to sequentialize
      ! comp1, comp2 and the coupler. The following ESMF_GridCompWait() calls 
      ! will block all PETs until comp1 and comp2 have returned. Consequently 
      ! the coupler component will not be run until comp1 and comp2 have 
      ! returned.
      !call ESMF_GridCompWait(comp1, blockingflag=ESMF_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
      !call ESMF_GridCompWait(comp2, blockingflag=ESMF_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      ! Run the coupler:
      ! The coupler will run in "per-PET sequential" mode because it runs on 
      ! the union of all PETs. Depending on the per-PET runtime of comp1 and
      ! comp2 some PETs may start/finish executing the coupler at different
      ! times. There is no intrinsic inter PET synchronization in calling
      ! component methods via CompI/R/F(). However, collective communication
      ! calls contained in the user written coupler methods will indirectly
      ! lead to inter PET synchronization of the coupler component.
      !print *, "I am calling into CplCompRun(cpl)"
  !-----------------------------------------------------------------------------
  call ESMF_CplCompRun(cpl12, c1export, c2import, clock=clock, rc=localrc)
  !-----------------------------------------------------------------------------
  print *, "Coupler Run returned, rc =", localrc
  !-----------------------------------------------------------------------------
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp1 has returned. Consequently comp2 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp1, blockingflag=ESMF_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      ! Run the second component:
      ! Since comp1 and comp2 are defined on exclusive sets of PETs those PET
      ! that are part of comp1 will not block in the following call but proceed
      ! to the next loop increment, executing comp1 concurrently with comp2.
      !print *, "I am calling into GridCompRun(comp2)"
  !-----------------------------------------------------------------------------
 !call ESMF_GridCompRun(comp2, importState=c2import, clock=clock, rc=localrc)
  !-----------------------------------------------------------------------------
  !print *, "Comp 2 Run returned, rc =", localrc
  !-----------------------------------------------------------------------------
 !if(localrc /= ESMF_SUCCESS) then                                            
 !   call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
 !endif


      !print *, "... time step finished on PET ", pet_id, "."
 
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     Print result

  call ESMF_GridCompFinalize(comp1, exportState=c1export, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
    
  !print *, "Comp 1 Finalize finished, rc =", rc

  call ESMF_GridCompFinalize(comp2, importState=c2import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
    
    !print *, "Comp 2 Finalize finished, rc =", rc

  call ESMF_GridCompFinalize(comp3, importState=c3import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

    !print *, "Comp 3 Finalize finished, rc =", rc

  call ESMF_CplCompFinalize(cpl12, c1export, c2import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

    !print *, "Coupler Finalize finished, rc =", rc


    !print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     Clean up

  call ESMF_StateDestroy(c1export, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(c2export, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(c3export, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(c1import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(c2import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(c3import, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompDestroy(comp2, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompDestroy(comp3, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_CplCompDestroy(cpl12, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_CplCompDestroy(cpl21, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  !print *, "All Destroy routines done"

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  rc = localrc

  
  call ESMF_Finalize(rc=localrc) 
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

!-------------------------------------------------------------------------------
end program ConcurrentComponent
!-------------------------------------------------------------------------------
