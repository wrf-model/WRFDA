module inttmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inttmod    module for intt and its tangent linear intt_tl
!
! abstract: module for intt and its tangent linear intt_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intt and its tangent linear intt_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intt,intt_tl


contains

subroutine intt(rt,st,rtv,stv,rq,sq,ru,su,rv,sv,rp,sp,rsst,ssst, &
                drt,dst,drtsen,dstsen,drq,dsq,dru,dsu,drv,dsv,drp,dsp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intt        apply nonlin qc observation operator for temps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for temperatures with
!             nonlinear qc operator
!
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intt and intt_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su  - modify for variational qc
!   2005-12-20  parrish - add option for boundary layer tlm
!   2006-03-30  park - correct indexing error for surface temp adjoint interpolation
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-20  derber - add sensible temperature for conventional temperatures
!   2006-10-20  rancic - add foto
!
!   input argument list:
!     st       - sensible temperature increment in grid space
!     stv      - virtual temperature increment in grid space
!     sq       - moisture increment in grid space
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     sp       - surface pressure increment in grid space
!     ssst     - sst increment in grid space
!     dst      - time derivative of sensible temperature increment in grid space
!     dsq      - time derivative of moisture increment in grid space
!     dsu      - time derivative of u increment in grid space
!     dsv      - time derivative of v increment in grid space
!     dsp      - time derivative of surface pressure increment in grid space
!
!   output argument list:
!     rt       - sensible temperature results from observation operator
!     rtv      - virtual temperature results from observation operator
!     rq       - moisture results from observation operator
!     ru       - u results from observation operator
!     rv       - v results from observation operator
!     rp       - surface pressure results from observation operator
!     rsst     - sst results from observation operator
!     drt      - time derivative of sensible temperature results from observation operator
!     drq      - time derivative of moisture results from observation operator
!     dru      - time derivative of u results from observation operator
!     drv      - time derivative of v results from observation operator
!     drp      - time derivative of surface pressure results from observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: thead,tptr
  use qcmod, only: nlnqc_iter,c_varqc
  use gridmod, only: latlon1n,latlon11
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart
  implicit none
  

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,stv,sq,su,sv
  real(r_kind),dimension(latlon1n),intent(in):: dst,dsq,dsu,dsv
  real(r_kind),dimension(latlon11),intent(in):: ssst,sp,dsp
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rtv,rq,ru,rv
  real(r_kind),dimension(latlon1n),intent(inout):: drt,drq,dru,drv
  real(r_kind),dimension(latlon11),intent(inout):: rsst,rp,drp
  real(r_kind),dimension(latlon1n),intent(inout):: dstsen,drtsen

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_t
! real(r_kind) penalty
  real(r_kind) cg_t,val,p0,grad,wnotgross,wgross,t_pg,varqc_iter
  real(r_kind) psfc_grad,tg_grad,t2_grad,u10_grad,v10_grad,q2_grad
  real(r_kind) t2_prime0,q2_prime0,u10_prime0,v10_prime0
  real(r_kind) ts_grad,us_grad,vs_grad,qs_grad
  real(r_kind) qs_prime0,tg_prime0,ts_prime0,psfc_prime0
  real(r_kind) us_prime0,vs_prime0

  tptr => thead
  do while (associated(tptr))

     j1=tptr%ij(1)
     j2=tptr%ij(2)
     j3=tptr%ij(3)
     j4=tptr%ij(4)
     j5=tptr%ij(5)
     j6=tptr%ij(6)
     j7=tptr%ij(7)
     j8=tptr%ij(8)
     w1=tptr%wij(1)
     w2=tptr%wij(2)
     w3=tptr%wij(3)
     w4=tptr%wij(4)
     w5=tptr%wij(5)
     w6=tptr%wij(6)
     w7=tptr%wij(7)
     w8=tptr%wij(8)

     time_t=tptr%time
     if(tptr%use_sfc_model) then

!----------use surface model----------------------

       if(tptr%tv_ob)then
         ts_prime0=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)
       else
         ts_prime0=w1*st(j1)+w2*st(j2)+w3*st(j3)+w4*st(j4)
       end if 
       ts_prime0=ts_prime0+(w1*dstsen(j1)+w2*dstsen(j2)+w3*dstsen(j3)+w4*dstsen(j4))*time_t
       tg_prime0=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
       qs_prime0=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)+ &
                (w1*dsq(j1)+w2*dsq(j2)+w3*dsq(j3)+w4*dsq(j4))*time_t
       us_prime0=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+ &
                (w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4))*time_t
       vs_prime0=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+ &
                (w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4))*time_t
       psfc_prime0=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)+ &
                  (w1*dsp(j1)+w2*dsp(j2)+w3*dsp(j3)+w4*dsp(j4))*time_t
       val=psfc_prime0*tptr%tlm_tsfc(1) + tg_prime0*tptr%tlm_tsfc(2) + &
           ts_prime0  *tptr%tlm_tsfc(3) + qs_prime0*tptr%tlm_tsfc(4) + &
           us_prime0  *tptr%tlm_tsfc(5) + vs_prime0*tptr%tlm_tsfc(6)

     else

!    Forward model (for interpolation)
       if(tptr%tv_ob)then
         val=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)&
            +w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)&
           +(w1*dst(j1)+w2*dst(j2)+w3*dst(j3)+w4*dst(j4)&
            +w5*dst(j5)+w6*dst(j6)+w7*dst(j7)+w8*dst(j8))*time_t
       else
         val=w1*    st(j1)+w2*    st(j2)+w3*    st(j3)+w4*    st(j4)&
            +w5*    st(j5)+w6*    st(j6)+w7*    st(j7)+w8*    st(j8)&
           +(w1*dstsen(j1)+w2*dstsen(j2)+w3*dstsen(j3)+w4*dstsen(j4)&
            +w5*dstsen(j5)+w6*dstsen(j6)+w7*dstsen(j7)+w8*dstsen(j8))*time_t
       end if

     end if
     val=val-tptr%res

!    gradient of nonlinear operator
!    Gradually turn on variational qc to avoid possible convergence problems
     if(jiter == jiterstart .and. nlnqc_iter .and. tptr%pg > tiny_r_kind) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one ) varqc_iter= one
        t_pg=tptr%pg*varqc_iter
     else
        t_pg=tptr%pg
     endif

     if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and.  &
                          tptr%b  > tiny_r_kind) then
        cg_t=cg_term/tptr%b
        wnotgross= one-t_pg
        wgross =t_pg*cg_t/wnotgross
        p0=wgross/(wgross+exp(-half*tptr%err2*val**2))
        val=val*(one-p0)                  
     endif

     grad     = val*tptr%raterr2*tptr%err2

!    Adjoint of interpolation
     if(tptr%use_sfc_model) then

!      Surface model

       psfc_grad=tptr%tlm_tsfc(1)*grad
       rp(j1)=rp(j1)+w1*psfc_grad
       rp(j2)=rp(j2)+w2*psfc_grad
       rp(j3)=rp(j3)+w3*psfc_grad
       rp(j4)=rp(j4)+w4*psfc_grad
       drp(j1)=drp(j1)+w1*psfc_grad*time_t
       drp(j2)=drp(j2)+w2*psfc_grad*time_t
       drp(j3)=drp(j3)+w3*psfc_grad*time_t
       drp(j4)=drp(j4)+w4*psfc_grad*time_t

       vs_grad  =tptr%tlm_tsfc(6)*grad
       rv(j1)=rv(j1)+w1*vs_grad
       rv(j2)=rv(j2)+w2*vs_grad
       rv(j3)=rv(j3)+w3*vs_grad
       rv(j4)=rv(j4)+w4*vs_grad
       drv(j1)=drv(j1)+w1*vs_grad*time_t
       drv(j2)=drv(j2)+w2*vs_grad*time_t
       drv(j3)=drv(j3)+w3*vs_grad*time_t
       drv(j4)=drv(j4)+w4*vs_grad*time_t

       us_grad  =tptr%tlm_tsfc(5)*grad
       ru(j1)=ru(j1)+w1*us_grad
       ru(j2)=ru(j2)+w2*us_grad
       ru(j3)=ru(j3)+w3*us_grad
       ru(j4)=ru(j4)+w4*us_grad
       dru(j1)=dru(j1)+w1*us_grad*time_t
       dru(j2)=dru(j2)+w2*us_grad*time_t
       dru(j3)=dru(j3)+w3*us_grad*time_t
       dru(j4)=dru(j4)+w4*us_grad*time_t

       qs_grad  =tptr%tlm_tsfc(4)*grad
       rq(j1)=rq(j1)+w1*qs_grad
       rq(j2)=rq(j2)+w2*qs_grad
       rq(j3)=rq(j3)+w3*qs_grad
       rq(j4)=rq(j4)+w4*qs_grad
       drq(j1)=drq(j1)+w1*qs_grad*time_t
       drq(j2)=drq(j2)+w2*qs_grad*time_t
       drq(j3)=drq(j3)+w3*qs_grad*time_t
       drq(j4)=drq(j4)+w4*qs_grad*time_t

       tg_grad  =tptr%tlm_tsfc(2)*grad
       rsst(j1)=rsst(j1)+w1*tg_grad
       rsst(j2)=rsst(j2)+w2*tg_grad
       rsst(j3)=rsst(j3)+w3*tg_grad
       rsst(j4)=rsst(j4)+w4*tg_grad

       ts_grad  =tptr%tlm_tsfc(3)*grad
       if(tptr%tv_ob)then
         rtv(j1)=rtv(j1)+w1*ts_grad
         rtv(j2)=rtv(j2)+w2*ts_grad
         rtv(j3)=rtv(j3)+w3*ts_grad
         rtv(j4)=rtv(j4)+w4*ts_grad

         drt(j1)=drt(j1)+w1*ts_grad*time_t
         drt(j2)=drt(j2)+w2*ts_grad*time_t
         drt(j3)=drt(j3)+w3*ts_grad*time_t
         drt(j4)=drt(j4)+w4*ts_grad*time_t

       else
         rt(j1)=rt(j1)+w1*ts_grad
         rt(j2)=rt(j2)+w2*ts_grad
         rt(j3)=rt(j3)+w3*ts_grad
         rt(j4)=rt(j4)+w4*ts_grad

         drtsen(j1)=drtsen(j1)+w1*ts_grad*time_t
         drtsen(j2)=drtsen(j2)+w2*ts_grad*time_t
         drtsen(j3)=drtsen(j3)+w3*ts_grad*time_t
         drtsen(j4)=drtsen(j4)+w4*ts_grad*time_t

       end if

     else

!------bypass surface model--------------------------

       if(tptr%tv_ob)then
         rtv(j1)=rtv(j1)+w1*grad
         rtv(j2)=rtv(j2)+w2*grad
         rtv(j3)=rtv(j3)+w3*grad
         rtv(j4)=rtv(j4)+w4*grad
         rtv(j5)=rtv(j5)+w5*grad
         rtv(j6)=rtv(j6)+w6*grad
         rtv(j7)=rtv(j7)+w7*grad
         rtv(j8)=rtv(j8)+w8*grad

         drt(j1)=drt(j1)+w1*grad*time_t
         drt(j2)=drt(j2)+w2*grad*time_t
         drt(j3)=drt(j3)+w3*grad*time_t
         drt(j4)=drt(j4)+w4*grad*time_t
         drt(j5)=drt(j5)+w5*grad*time_t
         drt(j6)=drt(j6)+w6*grad*time_t
         drt(j7)=drt(j7)+w7*grad*time_t
         drt(j8)=drt(j8)+w8*grad*time_t

       else
         rt(j1)=rt(j1)+w1*grad
         rt(j2)=rt(j2)+w2*grad
         rt(j3)=rt(j3)+w3*grad
         rt(j4)=rt(j4)+w4*grad
         rt(j5)=rt(j5)+w5*grad
         rt(j6)=rt(j6)+w6*grad
         rt(j7)=rt(j7)+w7*grad
         rt(j8)=rt(j8)+w8*grad

         drtsen(j1)=drtsen(j1)+w1*grad*time_t
         drtsen(j2)=drtsen(j2)+w2*grad*time_t
         drtsen(j3)=drtsen(j3)+w3*grad*time_t
         drtsen(j4)=drtsen(j4)+w4*grad*time_t
         drtsen(j5)=drtsen(j5)+w5*grad*time_t
         drtsen(j6)=drtsen(j6)+w6*grad*time_t
         drtsen(j7)=drtsen(j7)+w7*grad*time_t
         drtsen(j8)=drtsen(j8)+w8*grad*time_t

       end if

     end if

     tptr => tptr%llpoint
  end do
  return
end subroutine intt

subroutine intt_tl(rt,st,rt_tl,st_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intt_tl        the tangent linear of the operator that applies 
!                              nonlin qc observation operator for temps
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-12
!
! abstract: the tangent linear of the operator that applies observation operator 
!           and adjoint for temperatures with nonlinear qc operator
!
!
! program history log:
!   2005-05-12  yanqiu zhu - tangent linear of intt
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su  - modify for variational qc
!
! usage: intt_tl(st,rt,rt_tl,st_tl)
!   input argument list:
!     st       - increment in grid space
!     st_tl    - tangent linear increment in grid space
!
!   output argument list:
!     rt       - results from observation operator (no change for no data)
!     rt_tl    - tangent linear results from observation operator (no change for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: thead,tptr
  use obsmod_tl, only: tdataerr_tl
  use qcmod, only: nlnqc_iter,c_varqc
  use gridmod, only: latlon1n
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart
  implicit none
  

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st
  real(r_kind),dimension(latlon1n),intent(in):: st_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rt
  real(r_kind),dimension(latlon1n),intent(inout):: rt_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
! real(r_kind) penalty
  real(r_kind) cg_t,val,p0,grad,wnotgross,wgross,term,t_pg,varqc_iter
  real(r_kind) val_tl,p0_tl,grad_tl,term_tl

  tptr => thead
  i=0
  do while (associated(tptr))

     i=i+1
     j1=tptr%ij(1)
     j2=tptr%ij(2)
     j3=tptr%ij(3)
     j4=tptr%ij(4)
     j5=tptr%ij(5)
     j6=tptr%ij(6)
     j7=tptr%ij(7)
     j8=tptr%ij(8)
     w1=tptr%wij(1)
     w2=tptr%wij(2)
     w3=tptr%wij(3)
     w4=tptr%wij(4)
     w5=tptr%wij(5)
     w6=tptr%wij(6)
     w7=tptr%wij(7)
     w8=tptr%wij(8)

!    Forward model (for interpolation)
     val=w1*st(j1)+w2*st(j2)&
        +w3*st(j3)+w4*st(j4)&
        +w5*st(j5)+w6*st(j6)&
        +w7*st(j7)+w8*st(j8)-tptr%res
     val_tl=w1*st_tl(j1)+w2*st_tl(j2)&
           +w3*st_tl(j3)+w4*st_tl(j4)&
           +w5*st_tl(j5)+w6*st_tl(j6)&
           +w7*st_tl(j7)+w8*st_tl(j8)-tdataerr_tl(i)

!    gradient of nonlinear operator
     if(jiter == jiterstart .and. nlnqc_iter .and. tptr%pg > tiny_r_kind) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one) varqc_iter= one
        t_pg=tptr%pg*varqc_iter
     else
        t_pg=tptr%pg
     endif

     if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and. tptr%b >tiny_r_kind) then
        cg_t=cg_term/tptr%b
        wnotgross= one-t_pg
        wgross =t_pg*cg_t/wnotgross       ! wgross is gama in the reference by Enderson
        p0=wgross/(wgross+exp(-half*tptr%err2*val**2)) ! p0 is P in the reference by Enderson
        term=one-p0                          !  term is Wqc in the reference
        p0_tl = (p0*val*exp(-half**tptr%err2*val**2)*val_tl)/(wgross+exp(-half*tptr%err2*val**2))
        term_tl = -p0_tl
     else
        term = one
        term_tl = zero
     endif
     grad     = val*term
     grad_tl   = val_tl*term + val*term_tl
     grad     = grad*tptr%raterr2*tptr%err2
     grad_tl   = grad_tl*tptr%raterr2*tptr%err2
     
!    Adjoint of interpolation
     rt(j1)=rt(j1)+w1*grad
     rt(j2)=rt(j2)+w2*grad
     rt(j3)=rt(j3)+w3*grad
     rt(j4)=rt(j4)+w4*grad
     rt(j5)=rt(j5)+w5*grad
     rt(j6)=rt(j6)+w6*grad
     rt(j7)=rt(j7)+w7*grad
     rt(j8)=rt(j8)+w8*grad
     rt_tl(j1)=rt_tl(j1)+w1*grad_tl
     rt_tl(j2)=rt_tl(j2)+w2*grad_tl
     rt_tl(j3)=rt_tl(j3)+w3*grad_tl
     rt_tl(j4)=rt_tl(j4)+w4*grad_tl
     rt_tl(j5)=rt_tl(j5)+w5*grad_tl
     rt_tl(j6)=rt_tl(j6)+w6*grad_tl
     rt_tl(j7)=rt_tl(j7)+w7*grad_tl
     rt_tl(j8)=rt_tl(j8)+w8*grad_tl
     
     tptr => tptr%llpoint
  end do
  return
end subroutine intt_tl

end module inttmod
