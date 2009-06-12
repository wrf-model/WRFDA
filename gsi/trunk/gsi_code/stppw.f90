module stppwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppwmod    module for stppw and its tangent linear stppw_tl
!
! abstract: module for stppw and its tangent linear stppw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stppw and its tangent linear stppw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stppw,stppw_tl

contains

subroutine stppw(rq,sq,out,sges,drq,dsq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppw       calculate penalty and contribution to stepsize
!                            for precip. water using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from
!           precip. water, using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-25  wu
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist, generalized to use interpolated delta(pressure)
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stppw and stppw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify to output for b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     drq      - search direction for time derivative of q
!     dsq      - analysis increment for time derivative of q
!     sges     - stepsize estimates(4)
!
!   output argument list:
!     out(1)   - contribution to penalty for precip. water sges(1)
!     out(2)   - contribution to penalty for precip. water sges(2)
!     out(3)   - contribution to penalty for precip. water sges(3)
!     out(4)   - contribution to penalty for precip. water sges(4)
!     out(5)   - contribution to numerator for precip. water
!     out(6)   - contribution to denomonator for precip. water
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: pwhead,pwptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,tpwcon,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig
  implicit none

! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq,drq,dsq
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables  
  integer(i_kind) i,i1,i2,i3,i4,k
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) val,val2,w1,w2,w3,w4,time_pw
  real(r_kind) cg_pw,pw0,pw1,pw2,pw3,pen1,pen2,pen3,pencur,wgross,wnotgross


  out=0._r_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  pwptr => pwhead
  do while (associated(pwptr))
    if(pwptr%luse)then
     val=zero
     val2=zero
     w1 = pwptr%wij(1)
     w2 = pwptr%wij(2)
     w3 = pwptr%wij(3)
     w4 = pwptr%wij(4)
     time_pw=pwptr%time

!    Calculate precipitable water increment and delta precip water increment
     do k=1,nsig

        i1 = pwptr%ij(1)+(k-1)*latlon11
        i2 = pwptr%ij(2)+(k-1)*latlon11
        i3 = pwptr%ij(3)+(k-1)*latlon11
        i4 = pwptr%ij(4)+(k-1)*latlon11
        val =val +(w1* rq(i1)+w2* rq(i2)&
                 + w3* rq(i3)+w4* rq(i4)&
                 +(w1*drq(i1)+w2*drq(i2)&
                 + w3*drq(i3)+w4*drq(i4))*time_pw)*tpwcon &
                 * pwptr%dp(k)
        val2=val2+(w1* sq(i1)+w2* sq(i2)&
                 + w3* sq(i3)+w4* sq(i4)&
                 +(w1*dsq(i1)+w2*dsq(i2)&
                 + w3*dsq(i3)+w4*dsq(i4))*time_pw)*tpwcon &
                 * pwptr%dp(k)
     end do
     
     val2=val2-pwptr%res
     pw0=val2+sges(1)*val
     pw1=val2+sges(2)*val
     pw2=val2+sges(3)*val
     pw3=val2+sges(4)*val

     pencur = pw0*pw0*pwptr%err2
     pen1   = pw1*pw1*pwptr%err2
     pen2   = pw2*pw2*pwptr%err2
     pen3   = pw3*pw3*pwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. pwptr%pg > tiny_r_kind .and. &
                          pwptr%b  > tiny_r_kind) then
        cg_pw=cg_term/pwptr%b
        wnotgross= one-pwptr%pg
        wgross = pwptr%pg*cg_pw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     out(1) = out(1)+pencur*pwptr%raterr2
     out(2) = out(2)+pen1  *pwptr%raterr2
     out(3) = out(3)+pen2  *pwptr%raterr2
     out(4) = out(4)+pen3  *pwptr%raterr2
     cc     = (pen1+pen3-two*pen2)*pwptr%raterr2
     out(5) = out(5)+(pen1-pen3)*pwptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    pwptr => pwptr%llpoint

  end do
  return
end subroutine stppw


subroutine stppw_tl(rq,sq,pen,b1,b3,sges1,sges2,sges3, &
                   rq_tl,sq_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppw_tl      the tangent linear of the operator that calculates penalty 
!                             and contribution to stepsize for precip. water using nonlinear qc
!   prgmmr: yanqiu zhu         org: GMAO                date: 2005-05-19
!
! abstract: the tangent linear of the operator that calculates penalty and contribution 
!           to stepsize from precip. water, using nonlinear qc.
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stppw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rq_tl       - tangent linear search direction for q
!     sq_tl       - tangent linear analysis increment for q
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:
!     pen      - contribution to penalty for precip. water
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of the contribution to penalty for precip. water
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: pwhead,pwptr
  use obsmod_tl, only: pwdataerr_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,tpwcon,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq
  real(r_kind),dimension(latlon1n),intent(in):: rq_tl,sq_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables  
  integer(i_kind) i,i1,i2,i3,i4,k
  real(r_kind) val,val2
  real(r_kind) val_tl,val2_tl,w1,w2,w3,w4
  real(r_kind) cg_pw,pw1,pw2,pw3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) pw1_tl,pw2_tl,pw3_tl,pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp


  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  pwptr => pwhead
  i=0
  do while (associated(pwptr))
    i=i+1
    if(pwptr%luse)then
     val=zero
     val2=zero
     val_tl=zero
     val2_tl=zero

!    Calculate precipitable water increment and delta precip water increment
     do k=1,nsig

        w1 = pwptr%wij(1)
        w2 = pwptr%wij(2)
        w3 = pwptr%wij(3)
        w4 = pwptr%wij(4)
        i1 = pwptr%ij(1)+(k-1)*latlon11
        i2 = pwptr%ij(2)+(k-1)*latlon11
        i3 = pwptr%ij(3)+(k-1)*latlon11
        i4 = pwptr%ij(4)+(k-1)*latlon11
        val =val +(w1*rq(i1)+w2*rq(i2)&
                 + w3*rq(i3)+w4*rq(i4))*tpwcon &
                 * pwptr%dp(k)
        val2=val2+(w1*sq(i1)+w2*sq(i2)&
                 + w3*sq(i3)+w4*sq(i4))*tpwcon &
                 * pwptr%dp(k)
        val_tl =val_tl +(w1*rq_tl(i1)+w2*rq_tl(i2)&
                       + w3*rq_tl(i3)+w4*rq_tl(i4))*tpwcon &
                       * pwptr%dp(k)
        val2_tl=val2_tl+(w1*sq_tl(i1)+w2*sq_tl(i2)&
                       + w3*sq_tl(i3)+w4*sq_tl(i4))*tpwcon &
                       * pwptr%dp(k)
     end do
     
     val2=val2-pwptr%res
     pw1=val2+sges1*val
     pw2=val2+sges2*val
     pw3=val2+sges3*val
     val2_tl=val2_tl-pwdataerr_tl(i)
     pw1_tl=val2_tl+sges1_tl*val+sges1*val_tl
     pw2_tl=val2_tl+sges2_tl*val+sges2*val_tl
     pw3_tl=val2_tl+sges3_tl*val+sges3*val_tl

     exp_arg  = -half*val2*val2*pwptr%err2
     exp_arg1 = -half*pw1*pw1*pwptr%err2
     exp_arg2 = -half*pw2*pw2*pwptr%err2
     exp_arg3 = -half*pw3*pw3*pwptr%err2
     exp_arg_tl  = -val2*val2_tl*pwptr%err2
     exp_arg1_tl = -pw1*pw1_tl*pwptr%err2
     exp_arg2_tl = -pw2*pw2_tl*pwptr%err2
     exp_arg3_tl = -pw3*pw3_tl*pwptr%err2

     if (nlnqc_iter .and. pwptr%pg > tiny_r_kind) then
        cg_pw=cg_term/pwptr%b
        wnotgross= one-pwptr%pg
        wgross = pwptr%pg*cg_pw
        temp    = wnotgross*exp(exp_arg)
        term_tl  = temp/(temp+wgross)*exp_arg_tl
        temp    = wnotgross*exp(exp_arg1)
        term1_tl = temp/(temp+wgross)*exp_arg1_tl
        temp    = wnotgross*exp(exp_arg2)
        term2_tl = temp/(temp+wgross)*exp_arg2_tl
        temp    = wnotgross*exp(exp_arg3)
        term3_tl = temp/(temp+wgross)*exp_arg3_tl
        term  = log(wnotgross*exp(exp_arg)  + wgross)
        term1 = log(wnotgross*exp(exp_arg1) + wgross)
        term2 = log(wnotgross*exp(exp_arg2) + wgross)
        term3 = log(wnotgross*exp(exp_arg3) + wgross)
     else
        term_tl  = exp_arg_tl
        term1_tl = exp_arg1_tl
        term2_tl = exp_arg2_tl
        term3_tl = exp_arg3_tl
        term  = exp_arg
        term1 = exp_arg1
        term2 = exp_arg2
        term3 = exp_arg3
     endif

     pencur_tl = term_tl
     pen1_tl   = term1_tl
     pen2_tl   = term2_tl
     pen3_tl   = term3_tl
     pencur = term
     pen1   = term1
     pen2   = term2
     pen3   = term3

     pen_tl = pen_tl -two*pencur_tl*pwptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*pwptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*pwptr%raterr2
     pen = pen -two*pencur*pwptr%raterr2
     b1  = b1-two*(pen1-pen2)*pwptr%raterr2
     b3  = b3-two*(pen3-pen2)*pwptr%raterr2
    end if

    pwptr => pwptr%llpoint

  end do
  return
end subroutine stppw_tl

end module stppwmod
