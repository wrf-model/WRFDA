subroutine q_diag(mype)
!$$$  subroutine documentation block
!                .      .    .                                       .
! subprogram:    q_diag        get moisture diagnostics
!
!   prgmmr: kleist           org: np20                date: 2005-11-21
!
! abstract: compute statistics for negative and supersatured moisture points
!
! program history log:
!   2005-11-21  kleist
!   2007-08-08  derber - optimize, remove 1 mpi call.
!
!   input argument list:
!     su       - mpi task id
!     guo/rt   - output count was local; changed to global
! 
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ges_q,ntguessig
  use jfunc, only: qsatg
  use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum
  use constants,only: zero,two,one
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  real(r_kind):: qrms_neg,qrms_sat
  real(r_kind),dimension(2,2):: qrms,qrms0
  integer(i_kind) it,i,j,k

  it=ntguessig

  qrms=zero

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        if (ges_q(i,j,k,it) < zero) then
          qrms(1,1)=qrms(1,1) + ges_q(i,j,k,it)**two
          qrms(1,2)=qrms(1,2) + one
        end if
        if (ges_q(i,j,k,it) > qsatg(i,j,k)) then
          qrms(2,1)=qrms(2,1) + (ges_q(i,j,k,it)-qsatg(i,j,k))**two
          qrms(2,2)=qrms(2,2) + one
        end if
      end do
    end do
  end do

  call mpi_reduce(qrms,qrms0,4,mpi_rtype,mpi_sum,0,mpi_comm_world,i)

  if(mype == 0) then
     qrms_neg = zero
     qrms_sat = zero
     if(qrms0(1,2)>zero) qrms_neg=sqrt(qrms0(1,1)/qrms0(1,2))
     if(qrms0(2,2)>zero) qrms_sat=sqrt(qrms0(2,1)/qrms0(2,2))
     write(6,100) nint(qrms0(1,2)),qrms_neg,nint(qrms0(2,2)),qrms_sat
100  format(' Q_DIAG:  NEG Q COUNT,RMS=',i9,1x,g12.6,',   SUPERSAT Q COUNT,RMS=',i9,1x,g12.6)
  end if

  return
end subroutine q_diag
