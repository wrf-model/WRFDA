subroutine strong_baldiag_inc(xhat,xhatt,xhatp,xhatuv,mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_baldiag_inc    get balance diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-12
!
! abstract: get balance diagnostic statistics of increment
!
! program history log:
!   2006-08-12  parrish
!   2007-04-16  kleist   - modified to be used for diagnostics only
!   2007-07-26 cucurull  - call getprs; add xhat3dp and remove ps in calctends_tl argument list
!   2007-08-08  derber - only calculate dynamics time derivatives
!
!   input argument list:
!     xhat     - current solution
!     xhatt    - current solution for temp
!     xhatp    - current solution for psfc
!     xhatuv   - current solution for u,v
!     mype     - pe number
!
!   output argument list:
!     xhat   - updated solution after nstrong_end iterations of inmi
!     xhatt  - updated solution for temperature
!     xhatp  - updated solution for surface pressure
!     xhatuv - updated solution for u,v
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use mpimod, only: levs_id
  use jfunc, only: noz,nq,nt,nsst,ncw,np,nst,nvp,&
       nclen,nuvlen,ntendlen,nu,nv,nut,nvt,ntt,nprst,&
       nqt,nozt,ncwt
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use mod_vtrans,only: nvmodes_keep
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nclen),intent(inout)::xhat
  real(r_kind),dimension(latlon1n),intent(inout)::xhatt
  real(r_kind),dimension(latlon11),intent(inout)::xhatp
  real(r_kind),dimension(nuvlen),intent(inout):: xhatuv

! Declare local variables
  integer(i_kind) k,nnn,mm1
  integer(i_kind) istrong
  logical fullfield,tracer
  real(r_kind),dimension(latlon1n):: xhat_q
  real(r_kind),dimension(latlon1n)::u_t_g,v_t_g,t_t_g
  real(r_kind),dimension(latlon1n+latlon11):: xhat3dp
  real(r_kind),dimension(latlon11)::ps_t_g
  real(r_kind),dimension(ntendlen)::xhat_t
  real(r_kind),dimension(nclen)::xhat_x,xhat_y

!************************************************************************************  
! Initialize variable
  mm1=mype+1

!   convert normalized rh variable to q
  call normal_rh_to_q(xhat(nq),xhatt,xhatp,xhat_q)

!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
  nnn=0
  do k=1,nsig1o
    if (levs_id(k)/=0) nnn=nnn+1
  end do
  call get_derivatives( &
     xhatuv(nu) ,xhatuv(nv) ,xhatt     ,xhatp   ,  &
     xhat_q     ,xhat  (noz),xhat  (nsst),xhat  (ncw), &
     xhat_x(nst),xhat_x(nvp),xhat_x(nt)  ,xhat_x(np),  &
     xhat_x(nq) ,xhat_x(noz),xhat_x(nsst),xhat_x(ncw), &
     xhat_y(nst),xhat_y(nvp),xhat_y(nt)  ,xhat_y(np),  &
     xhat_y(nq) ,xhat_y(noz),xhat_y(nsst),xhat_y(ncw), &
     nnn,mype,1)

  call getprs(xhatp,xhat3dp)
  tracer=.false.
  call calctends_tl( &
     xhatuv(nu) ,xhatuv(nv)  ,xhatt    ,                 &
     xhat_q     ,xhat(noz)   ,xhat(ncw)  ,               &
     xhat_x(nst),xhat_y(nst) ,xhat_x(nvp),xhat_y(nvp),   &
     xhat_x(nt) ,xhat_y(nt)  ,xhat_x(np) ,xhat_y(np),    &
     xhat_x(nq) ,xhat_y(nq)  ,xhat_x(noz),xhat_y(noz),   &
     xhat_x(ncw),xhat_y(ncw) ,mype,          &
     xhat_t(nut),xhat_t(nvt) ,xhat_t(ntt),xhat_t(nprst), &
     xhat_t(nqt),xhat_t(nozt),xhat_t(ncwt),xhat3dp,tracer)
  if(nvmodes_keep.gt.0) then
      fullfield=.false.
      call strong_bal_correction(xhat_t(nut),xhat_t(nvt),xhat_t(ntt),xhat_t(nprst),&
                  mype,xhatuv(nu),xhatuv(nv),xhatt,xhatp,.true.,fullfield,.false.)
  end if

  return
end subroutine strong_baldiag_inc
