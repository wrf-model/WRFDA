!===============================================================================
  program esmf_wrf4dvar
!===============================================================================
  ! ESMF Framework module
  use ESMF_Mod

  ! Application components
  use esmf_wrfnl_component, only   : nl_register
  use esmf_solver_component, only  : solver_register
  use esmf_outerloop_coupler, only : nl2solver_register, solver2nl_register

  implicit none
    
  ! Local ESMF types
  type(ESMF_VM)        :: vm
  type(ESMF_Clock)     :: clock
  type(ESMF_State)     :: nlExp, solverExp
  type(ESMF_State)     :: nlImp, solverImp
  type(ESMF_GridComp)  :: nlComp, solverComp
  type(ESMF_CplComp)   :: nl2solverCpl, solver2nlCpl

  ! Local variables
  integer :: iouter, nouter
  integer :: pet_id, ipet, npets, rc, localrc
  character(len=ESMF_MAXSTR) :: cname, cplname

  ! initialize error logging
  localrc = ESMF_FAILURE
  rc = ESMF_FAILURE

  ! initialize local variables
  nouter = 1  ! no outer loop for now!

  ! set distribution sizes

!-------------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Initialize framework and get back default global VM
  !-----------------------------------------------------------------------------
  call ESMF_Initialize(vm=vm, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,'Failed ESMF Initialize'
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  Print*,'ESMF Initialize'

  !-----------------------------------------------------------------------------
  ! Get number of PETs 
  !-----------------------------------------------------------------------------
  call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,'Failed to get VM'
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print*,'Local Pet id:', pet_id, '  number of pets:',npets

  !-----------------------------------------------------------------------------
  ! check that we are running with at least 2 PETs
  !-----------------------------------------------------------------------------
  if (npets < 2) then
     print *, "WRF 4D VAR needs 2 PETS to run, current np = ", npets 
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

!-------------------------------------------------------------------------------
! Create two  gridded components, one for the NL model and one for the 
! concurrent component driver called solver, and an NL to solver coupler
!-------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Gridded Component Solver - acts as upper level driver for concurrent
  ! component.
  !-----------------------------------------------------------------------------
  cname = "solver"
  solverComp = ESMF_GridCompCreate(name=cname, petList=(/0,1/), rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cname), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", " Created component ", trim(cname)
  endif

  !-----------------------------------------------------------------------------
  ! Nonlinear Model (forward WRF)
  !-----------------------------------------------------------------------------
  cname = "nl"
  nlComp = ESMF_GridCompCreate(name=cname, petList=(/0/), rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cname), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", " Created component ", trim(cname)
  endif

  !-----------------------------------------------------------------------------
  ! couplers between NL and solver components
  !-----------------------------------------------------------------------------
  cplname = "nl to solver coupler"
  nl2solverCpl = ESMF_CplCompCreate(name=cplname,petList=(/0,1/),rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cplname), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", " Created component ", trim(cplname)
  endif

  cplname = "solver to nl coupler"
  solver2nlCpl = ESMF_CplCompCreate(name=cplname,petList=(/0,1/),rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Failure to create component ", trim(cplname), ", rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", " Created component ", trim(cplname)
  endif

  print*, "(",pet_id,")"," Component creation completed"

!-------------------------------------------------------------------------------
!  Register section - set services
!-------------------------------------------------------------------------------
   print*,"(",pet_id,")","Register Components in wrf 4D var driver"
  !-----------------------------------------------------------------------------
  ! Component 1 - solverComp
  !-----------------------------------------------------------------------------
  call ESMF_GridCompSetServices(solverComp, solver_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "solverComp SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print *,"(",pet_id,")", " solverComp SetServices finished"
  endif

  !-----------------------------------------------------------------------------
  ! Component 2 - nlComp
  !-----------------------------------------------------------------------------
  call ESMF_GridCompSetServices(nlComp, nl_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"nlComp Setservices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print *,"(",pet_id,")", " nlComp SetServices finished"
  endif

  !-----------------------------------------------------------------------------
  ! Coupler Component 21 - nl2solverCpl
  !-----------------------------------------------------------------------------
  call ESMF_CplCompSetServices(nl2solverCpl, nl2solver_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "nl2solverCpl SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print *,"(",pet_id,")", " nl2solverCpl SetServices finished"
  endif

  !-----------------------------------------------------------------------------
  ! Coupler Component 12 - solver2nlCpl
  !-----------------------------------------------------------------------------
  call ESMF_CplCompSetServices(solver2nlCpl, solver2nl_register, localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "solver2nlCpl SetServices failed, rc= ", localrc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print *,"(",pet_id,")", " solver2nlCpl SetServices finished"
  endif

  print *,"(",pet_id,")", " Entering Init Phase"

!-------------------------------------------------------------------------------
!  Initialization section -
!   initialize separated into two phases,
!   (1) create states and initialize ESMF coupling objects (in Solver, NL, CPL)
!-------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! solver init - phase 1
  !-----------------------------------------------------------------------------
  solverImp = ESMF_StateCreate("solver import", ESMF_STATE_IMPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","solverComp import state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  solverExp = ESMF_StateCreate("solver export", ESMF_STATE_EXPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","solverComp export state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  ! initialize phase one - create ESMF objects for outer loop coupling
  call ESMF_GridCompInitialize(solverComp, importState=solverImp,              &
                               exportState=solverExp, phase=1, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","solverComp Initialize phase 1 failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
                               
  print*,"(",pet_id,")","solverComp phase 1 Initialize finished"
 
  !-----------------------------------------------------------------------------
  ! Nonlinear model component - phase 1
  !-----------------------------------------------------------------------------
  nlImp = ESMF_StateCreate("NL model import", ESMF_STATE_IMPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","nlComp import state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  
  nlExp = ESMF_StateCreate("NL model export", ESMF_STATE_EXPORT, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","nlComp export state create failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  
  call ESMF_GridCompInitialize(nlComp, importState=nlImp, & 
                               exportState=nlExp, phase=1, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","nlComp Initialize phase 1 failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  print*,"(",pet_id,")", "nlComp Initialize phase 1 finished"
  flush(6)
  !-----------------------------------------------------------------------------
  ! Coupler component - single phase
  !-----------------------------------------------------------------------------
  ! Note: coupler's import is NL export, and export is solver's import
  call ESMF_CplCompInitialize(nl2solverCpl, importState=nlExp,                 &
                              exportState=solverImp,  rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","NL2Solver Coupler Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print *, "NL2Solver Coupler Initialize finished"
  endif

  ! Note: coupler's import is solver's export, and export is NL import
  call ESMF_CplCompInitialize(solver2NLCpl, importState=solverExp,             &
                              exportState=nlImp,  rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","Solver2NL Coupler Initialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", "Solver2NL Coupler Initialize finished"
  endif
!-------------------------------------------------------------------------------
! Initialize - phase 2 (internal component initialization)
!-------------------------------------------------------------------------------
  ! NL component internal init
  call ESMF_GridCompInitialize(nlComp, importState=nlImp, & 
                               exportState=nlExp, phase=2, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","nlComp Initialize phase 2 failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")", "NL Comp phase 2 Initialize finished"
  endif

  ! Solver component internal init
  call ESMF_GridCompInitialize(solverComp, importState=solverImp,              &
                               exportState=solverExp, phase=2, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print*,"(",pet_id,")","solverComp Initialize phase 2 failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  else
     print*,"(",pet_id,")"," SolverComp phase 2 Initialize finished"
  endif
  flush(6)
!-------------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------------
! Run:
!     loop (outer loop)
!        NL -  evolve full NL model to the end of the analysis window.
!        CPL - NL 2 Solver
!        Solver Phase 1 - receive coupling state from NL
!        Solver Phase 2 - iterative CG solver to minimize cost function
!     end loop
!-------------------------------------------------------------------------------
  ! outer loop
! do iouter=1, nouter
  do iouter=1, 1
     ! run NL component
     call ESMF_GridCompRun(nlComp, importState=nlImp, exportState=nlExp,       &
                           rc=localrc)
     if(localrc /= ESMF_SUCCESS) then                                            
        print*,"(",pet_id,")", " nlComp run failed, rc =", rc
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
     endif

     ! couple NL component with the solver
     call ESMF_CplCompRun(nl2solverCpl, importState=nlExp,                     &
                          exportState=solverImp, rc=localrc)
     if(localrc /= ESMF_SUCCESS) then                                            
        print*,"(",pet_id,")", " nl2solver coupler run failed, rc =", rc
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
     endif

     ! run phase 1 of solver - receive state from NL
     call ESMF_GridCompRun(solverComp, importState=solverImp,                  &
                           exportState=solverExp, phase=1, rc=localrc)
     if(localrc /= ESMF_SUCCESS) then                                            
        print*,"(",pet_id,")", " Phase 1 of solverComp run failed, rc =", rc
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
     endif

     ! run phase 2 of solver - CG minimization of the cost function
!    call ESMF_GridCompRun(solverComp, importState=solverImp,                  &
!                          exportState=solverExp, phase=2, rc=localrc)
!    if(localrc /= ESMF_SUCCESS) then                                            
!       print*,"(",pet_id,")", " Phase 2 of solverComp run failed, rc =", rc
!       call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
!    endif

     ! if another outer loop, then couple the output of solver w/ NL component
!    if( iouter < nouter ) then
!       call ESMF_CplCompRun(solver2nlCpl, importState=solverExp,              &
!                            exportState=nlImp, rc=localrc)
!       if(localrc /= ESMF_SUCCESS) then                                            
!          print*,"(",pet_id,")", " Solver2nl coupler run failed, rc =", rc
!          call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
!       endif
!    endif

  enddo   ! outer loop

!-------------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------------
  ! finialize NL component
  call ESMF_GridCompFinalize(nlComp, importState=nlImp, &
                               exportState=nlExp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "NL component finialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
  flush(6)  
  ! finialize solver component
  call ESMF_GridCompFinalize(solverComp, importState=solverImp, &
                             exportState=solverExp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Solver component finialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif
   flush(6) 
  call ESMF_CplCompFinalize(nl2solverCpl, importState=nlExp, &
                               exportState=solverImp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "NL to Solver coupler finialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_CplCompFinalize(solver2nlCpl, importState=solverExp, &
                               exportState=nlImp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     print *, "Solver to NL coupler finialize failed, rc =", rc
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif


  flush(6)
!-------------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !     Clean up states
  !-----------------------------------------------------------------------------
  call ESMF_StateDestroy(nlExp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(nlImp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(solverExp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_StateDestroy(solverImp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  print*,"(",pet_id,")", "All states destroyed"

  !-----------------------------------------------------------------------------
  !     Clean up components
  !-----------------------------------------------------------------------------
  call ESMF_GridCompDestroy(nlComp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_GridCompDestroy(solverComp, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_CplCompDestroy(nl2solverCpl, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  call ESMF_CplCompDestroy(solver2nlCpl, rc=localrc)
  if(localrc /= ESMF_SUCCESS) then                                            
     call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
  endif

  print*,"(",pet_id,")", "All components destroyed"

  !-----------------------------------------------------------------------------
  ! ESMF shutdown
  !-----------------------------------------------------------------------------
  print*,"-----------run completed successfully--------------"
  rc = localrc
  
  call ESMF_Finalize(rc=localrc) 

!===============================================================================
end program esmf_wrf4dvar
!===============================================================================
