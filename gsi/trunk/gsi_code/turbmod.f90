module turbmod 

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none

  logical use_pbl
  real(r_kind),allocatable,dimension(:,:,:):: pim1
  real(r_kind),allocatable,dimension(:,:,:):: pim2
  real(r_kind),allocatable,dimension(:,:,:):: pim3
  real(r_kind),allocatable,dimension(:,:,:):: pih1
  real(r_kind),allocatable,dimension(:,:,:):: pih2
  real(r_kind),allocatable,dimension(:,:,:):: pih3

contains
  subroutine init_turb
    implicit none
    use_pbl=.false.                      ! set to true to turn on effect of pbl
    return
  end subroutine init_turb

  subroutine create_turbvars
    implicit none
    
    allocate( pim1(lat2,lon2,nsig) )
    allocate( pim2(lat2,lon2,nsig) )
    allocate( pim3(lat2,lon2,nsig) )
    allocate( pih1(lat2,lon2,nsig) )
    allocate( pih2(lat2,lon2,nsig) )
    allocate( pih3(lat2,lon2,nsig) )
    
    return
  end subroutine create_turbvars

  subroutine destroy_turbvars

    deallocate(pim1)
    deallocate(pim2)
    deallocate(pim3)
    deallocate(pih1)
    deallocate(pih2)
    deallocate(pih3)

    return
  end subroutine destroy_turbvars

end module turbmod 
