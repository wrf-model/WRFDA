module stplimqmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimqmod    module for stplimq and its tangent linear stplimq_tl
!
! abstract: module for stplimq and its tangent linear stplimq_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stplimq and its tangent linear stplimq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2005-11-22  Wu - return in factq's are zero
!

implicit none

PRIVATE
PUBLIC stplimq,stplimq_tl

contains
 
subroutine stplimq(rq,sq,rc,sc,sges,outmin,outmax)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq     calculate penalty and stepsize for limit of q 
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2006-09-18  derber - modify output b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     rq       - search direction                               
!     sq       - increment in grid space
!     sges     - step size estimates (4)
!
!   output argument list:
!     outmin(1)  - current penalty for negative q sges(1)
!     outmin(2)  - current penalty for negative q sges(2)
!     outmin(3)  - current penalty for negative q sges(3)
!     outmin(4)  - current penalty for negative q sges(4)
!     outmin(5)  - contribution to numerator from negative q
!     outmin(6)  - contribution to denomenator from negative q
!     outmax(1)  - current penalty for excess q sges(1)
!     outmax(2)  - current penalty for excess q sges(2)
!     outmax(3)  - current penalty for excess q sges(3)
!     outmax(4)  - current penalty for excess q sges(4)
!     outmax(5)  - contribution to numerator from excess q
!     outmax(6)  - contribution to denomenator from excess q
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,two,one,half
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax,rhgues
  implicit none

! Declare passed variables
  real(r_kind),dimension(4),intent(in):: sges
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_quad),dimension(6),intent(out):: outmin,outmax
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: rq,sq,rc,sc

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind) q,q0,q1,q2,q3
  real(r_kind):: x0max,x0min,x1max,x2max,x3max,x1min,x2min,x3min
  
  outmin=0._r_quad; outmax=0._r_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  if (factqmin==zero .and. factqmax==zero) return
  x0max=zero
  x1max=zero
  x2max=zero
  x3max=zero
  x0min=zero
  x1min=zero
  x2min=zero
  x3min=zero
  
! Loop over interior of subdomain          
  do k = 1,nsig
    do j = 2,lon1+1
      do i = 2,lat1+1

!       Values for q using 3 stepsizes
        q  = rhgues(i,j,k) + sq(i,j,k)
        q0 = q + sges(1)*rq(i,j,k)
        q1 = q + sges(2)*rq(i,j,k)
        q2 = q + sges(3)*rq(i,j,k)
        q3 = q + sges(4)*rq(i,j,k)

        x0min=zero
        x1min=zero
        x2min=zero
        x3min=zero

        x0max=zero
        x1max=zero
        x2max=zero
        x3max=zero

        if(q0 < zero)x0min=q0*q0 
        if(q1 < zero)x1min=q1*q1 
        if(q2 < zero)x2min=q2*q2 
        if(q3 < zero)x3min=q3*q3 

        if(q0 > one )x0max=(q0-one)*(q0-one) 
        if(q1 > one )x1max=(q1-one)*(q1-one) 
        if(q2 > one )x2max=(q2-one)*(q2-one) 
        if(q3 > one )x3max=(q3-one)*(q3-one) 

        cc=x1min+x3min-two*x2min
        outmin(1)=outmin(1)+factqmin*x0min
        outmin(2)=outmin(2)+factqmin*x1min
        outmin(3)=outmin(3)+factqmin*x2min
        outmin(4)=outmin(4)+factqmin*x3min
        outmin(5)=outmin(5)+factqmin*((x1min-x3min)*bcoef1+cc*bcoef2)
        outmin(6)=outmin(6)+factqmin*cc*ccoef


        cc=x1max+x3max-two*x2max
        outmax(1)=outmax(1)+factqmax*x0max
        outmax(2)=outmax(2)+factqmax*x1max
        outmax(3)=outmax(3)+factqmax*x2max
        outmax(4)=outmax(4)+factqmax*x3max
        outmax(5)=outmax(5)+factqmax*((x1max-x3max)*bcoef1+cc*bcoef2)
        outmax(6)=outmax(6)+factqmax*cc*ccoef

      end do
    end do
  end do

  return
end subroutine stplimq


subroutine stplimq_tl(rq,sq,sges1,sges2,sges3,penmin,b1min,b3min,&
                    penmax,b1max,b3max, &
                    rq_tl,sq_tl,sges1_tl,sges2_tl,sges3_tl,penmin_tl,b1min_tl,b3min_tl,&
                    penmax_tl,b1max_tl,b3max_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq_tl     the tangent linear of the operator that calculates 
!                              penalty and stepsize for limit of q 
!   prgmmr: yanqiu zhu           org: GMSO                date: 2005-04-06
!
! abstract: the tangent linear of the operator that calculates stepsize contribution 
!           and penalty for limiting q
!
! program history log:
!   2005-04-06  yanqiu zhu - tangent linear of stplimq
!
!   input argument list:
!     rq       - search direction                               
!     sq       - increment in grid space
!     sges1    - ges stepsize 1
!     sges2    - ges stepsize 2
!     sges3    - ges stepsize 3
!     rq_tl       - tangent linear search direction
!     sq_tl       - tangent linear increment in grid space
!     sges1_tl    - tangent linear ges stepsize 1
!     sges2_tl    - tangent linear ges stepsize 2
!     sges3_tl    - tangent linear ges stepsize 3
!
!   output argument list:
!     penmin   - current penalty for negative q
!     b1min    - penmin(sges1)-penmin(sges2)
!     b3min    - penmin(sges3)-penmin(sges2)
!     penmax   - current penalty for excess q
!     b1max    - penmax(sges1)-penmax(sges2)
!     b3max    - penmax(sges3)-penmax(sges2)
!     penmin_tl   - current tangent linear of penalty for negative q
!     b1min_tl    - penmin_tl(sges1)-penmin_tl(sges2)
!     b3min_tl    - penmin_tl(sges3)-penmin_tl(sges2)
!     penmax_tl   - current tangent linear of penalty for excess q
!     b1max_tl    - penmax_tl(sges1)-penmax_tl(sges2)
!     b3max_tl    - penmax_tl(sges3)-penmax_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax,qsatg,rhgues
  use jfunc_tl, only: qgues_tl
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl
  real(r_kind),intent(out):: penmin,b1min,b3min
  real(r_kind),intent(out):: penmin_tl,b1min_tl,b3min_tl
  real(r_kind),intent(out):: penmax,b1max,b3max
  real(r_kind),intent(out):: penmax_tl,b1max_tl,b3max_tl
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: rq,sq
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: rq_tl,sq_tl

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind) q,q1,q2,q3
  real(r_kind) q_tl,q1_tl,q2_tl,q3_tl
  real(r_kind),dimension(nsig):: p1max,p1min,x1max,x2max,x3max,x1min,x2min,x3min
  real(r_kind),dimension(nsig):: p1max_tl,p1min_tl,xmax1_tl,x2max_tl,x3max_tl, &
                                 x1min_tl,x2min_tl,x3min_tl
  
  penmin=zero; penmax=zero
  b1min=zero; b3min=zero
  b1max=zero; b3max=zero
  penmin_tl=zero; penmax_tl=zero
  b1min_tl=zero; b3min_tl=zero
  b1max_tl=zero; b3max_tl=zero

  if (factqmin==zero .and. factqmax==zero) return
  
! Loop over interior of subdomain          
!$omp parallel do  schedule(dynamic,1) private(k,i,j,q,q1,q2,q3)
  do k = 1,nsig
    p1max(k)=zero
    p1min(k)=zero
    x1max(k)=zero
    x1min(k)=zero
    x2max(k)=zero
    x2min(k)=zero
    x3max(k)=zero
    x3min(k)=zero
    p1max_tl(k)=zero
    p1min_tl(k)=zero
    xmax1_tl(k)=zero
    x1min_tl(k)=zero
    x2max_tl(k)=zero
    x2min_tl(k)=zero
    x3max_tl(k)=zero
    x3min_tl(k)=zero
    do j = 2,lon1+1
      do i = 2,lat1+1

!       Values for q using 3 stepsizes
        q  = rhgues(i,j,k) + sq(i,j,k)
        q1 = q + sges1*rq(i,j,k)
        q2 = q + sges2*rq(i,j,k)
        q3 = q + sges3*rq(i,j,k)
        q_tl  = qgues_tl(i,j,k) + sq_tl(i,j,k)
        q1_tl = q_tl + sges1_tl*rq(i,j,k) + sges1*rq_tl(i,j,k)
        q2_tl = q_tl + sges2_tl*rq(i,j,k) + sges2*rq_tl(i,j,k)
        q3_tl = q_tl + sges3_tl*rq(i,j,k) + sges3*rq_tl(i,j,k)

!       Compute penalty for neg q using 3 stepsize estimates
        if (q  <zero) p1min(k)  = p1min(k)  + q *q  
        if (q1 <zero) x1min(k)  = x1min(k)  + q1*q1
        if (q2 <zero) x2min(k)  = x2min(k)  + q2*q2
        if (q3 <zero) x3min(k)  = x3min(k)  + q3*q3
        if (q  <zero) p1min_tl(k)  = p1min_tl(k)  + 2.*q *q_tl 
        if (q1 <zero) x1min_tl(k)  = x1min_tl(k)  + 2.*q1*q1_tl
        if (q2 <zero) x2min_tl(k)  = x2min_tl(k)  + 2.*q2*q2_tl
        if (q3 <zero) x3min_tl(k)  = x3min_tl(k)  + 2.*q3*q3_tl

!       Compute penalty for excess q using 3 stepsize estimates
        if (q  >qsatg(i,j,k)) p1max(k)=p1max(k)+((q -qsatg(i,j,k))**2)
        if (q1 >qsatg(i,j,k)) x1max(k)=x1max(k)+((q1-qsatg(i,j,k))**2)
        if (q2 >qsatg(i,j,k)) x2max(k)=x2max(k)+((q2-qsatg(i,j,k))**2)
        if (q3 >qsatg(i,j,k)) x3max(k)=x3max(k)+((q3-qsatg(i,j,k))**2)
        if (q  >qsatg(i,j,k)) p1max_tl(k)=p1max_tl(k)+2.*(q -qsatg(i,j,k))*q_tl
        if (q1 >qsatg(i,j,k)) xmax1_tl(k)=xmax1_tl(k)+2.*(q1-qsatg(i,j,k))*q1_tl
        if (q2 >qsatg(i,j,k)) x2max_tl(k)=x2max_tl(k)+2.*(q2-qsatg(i,j,k))*q2_tl
        if (q3 >qsatg(i,j,k)) x3max_tl(k)=x3max_tl(k)+2.*(q3-qsatg(i,j,k))*q3_tl
        
      end do
    end do
  end do

  do k=1,nsig
    penmin=penmin+p1min(k)
    penmax=penmax+p1max(k)
    b1min=b1min+x1min(k)-x2min(k)
    b1max=b1max+x1max(k)-x2max(k)
    b3min=b3min+x3min(k)-x2min(k)
    b3max=b3max+x3max(k)-x2max(k)
    penmin_tl=penmin_tl+p1min_tl(k)
    penmax_tl=penmax_tl+p1max_tl(k)
    b1min_tl=b1min_tl+x1min_tl(k)-x2min_tl(k)
    b1max_tl=b1max_tl+xmax1_tl(k)-x2max_tl(k)
    b3min_tl=b3min_tl+x3min_tl(k)-x2min_tl(k)
    b3max_tl=b3max_tl+x3max_tl(k)-x2max_tl(k)
  end do

! Multiply by scale factor
  penmin=factqmin*penmin
  b1min =factqmin*b1min
  b3min =factqmin*b3min
  penmin_tl=factqmin*penmin_tl
  b1min_tl =factqmin*b1min_tl
  b3min_tl =factqmin*b3min_tl

  penmax=factqmax*penmax
  b1max =factqmax*b1max
  b3max =factqmax*b3max
  penmax_tl=factqmax*penmax_tl
  b1max_tl =factqmax*b1max_tl
  b3max_tl =factqmax*b3max_tl

  return
end subroutine stplimq_tl

end module stplimqmod
