      subroutine da_vd05bd(stx,fx,dx,sty,fy,dy,stp,fp,dp,brackt, &
                           stpmin,stpmax,info)
! 
!     subroutine vd05bd
! 
!     the purpose of vd05bd is to compute a safeguarded step for
!     a linesearch and to update an interval of uncertainty for
!     a minimizer of the function.
! 
!     the parameter stx contains the step with the least function
!     value. the parameter stp contains the current step. it is
!     assumed that the derivative at stx is negative in the
!     direction of the step. if brackt is set true then a
!     minimizer has been bracketed in an interval of uncertainty
!     with endpoints stx and sty.
! 
!     the subroutine statement is
! 
!     subroutine vd05bd(stx,fx,dx,sty,fy,dy,stp,fp,dp,brackt,
!                        stpmin,stpmax,info)
! 
!     where
! 
!       stx, fx, and dx are variables which specify the step,
!         the function, and the derivative at the best step obtained
!         so far. the derivative must be negative in the direction
!         of the step, that is, dx and stp-stx must have opposite
!         signs. on output these parameters are updated appropriately.
! 
!       sty, fy, and dy are variables which specify the step,
!         the function, and the derivative at the other endpoint of
!         the interval of uncertainty. on output these parameters are
!         updated appropriately.
! 
!       stp, fp, and dp are variables which specify the step,
!         the function, and the derivative at the current step.
!         if brackt is set true then on input stp must be
!         between stx and sty. on output stp is set to the new step.
! 
!       brackt is a logical variable which specifies if a minimizer
!         has been bracketed. if the minimizer has not been bracketed
!         then on input brackt must be set false. if the minimizer
!         is bracketed then on output brackt is set true.
! 
!       stpmin and stpmax are input variables which specify lower
!         and upper bounds for the step.
! 
!       info is an integer output variable set as follows:
!         if info = 1,2,3,4,5, then the step has been computed
!         according to one of the five cases below. otherwise
!         info = 0, and this indicates improper input parameters.
! 
!     subprograms called
! 
!       fortran-supplied ... abs,max,min,sqrt
! 
!     argonne national laboratory. minpack project. june 1983
!     jorge j. more`, david j. thuente
! 
!     **********
      real,intent(inout)                      :: stx,fx,dx
      real,intent(inout)                      :: sty,fy,dy
      real,intent(inout)                      :: stp,fp,dp
      logical,intent(inout)                   :: brackt
      real,intent(in)                         :: stpmin,stpmax
      integer,intent(out)                     :: info

      logical bound
      real gamma,p,q,r,s,sgnd,stpc,stpf,stpq,theta

      info = 0
! 
!     check the input parameters for errors.
! 
      if ((brackt .and. (stp <= min(stx,sty) .or. stp >= max(stx,sty))) .or. &
          dx*(stp-stx) >= 0.0 .or. stpmax < stpmin) return
! 
!     determine if the derivatives have opposite sign.
! 
      sgnd = dp*(dx/abs(dx))
! 
!     first case. a higher function value.
!     the minimum is bracketed. if the cubic step is closer
!     to stx than the quadratic step, the cubic step is taken,
!     else the average of the cubic and quadratic steps is taken.
! 
      if (fp > fx) then
         info = 1
         bound = .true.
         theta = 3*(fx - fp)/(stp - stx) + dx + dp
         s = max(abs(theta),abs(dx),abs(dp))
         gamma = s*sqrt((theta/s)**2 - (dx/s)*(dp/s))
         if (stp < stx) gamma = -gamma
         p = (gamma - dx) + theta
         q = ((gamma - dx) + gamma) + dp
         r = p/q
         stpc = stx + r*(stp - stx)
         stpq = stx + ((dx/((fx-fp)/(stp-stx)+dx))/2)*(stp - stx)
         if (abs(stpc-stx) < abs(stpq-stx)) then
            stpf = stpc
         else
           stpf = stpc + (stpq - stpc)/2
           end if
         brackt = .true.
! 
!     second case. a lower function value and derivatives of
!     opposite sign. the minimum is bracketed. if the cubic
!     step is closer to stx than the quadratic (secant) step,
!     the cubic step is taken, else the quadratic step is taken.
! 
      else if (sgnd < 0.0) then
         info = 2
         bound = .false.
         theta = 3*(fx - fp)/(stp - stx) + dx + dp
         s = max(abs(theta),abs(dx),abs(dp))
         gamma = s*sqrt((theta/s)**2 - (dx/s)*(dp/s))
         if (stp > stx) gamma = -gamma
         p = (gamma - dp) + theta
         q = ((gamma - dp) + gamma) + dx
         r = p/q
         stpc = stp + r*(stx - stp)
         stpq = stp + (dp/(dp-dx))*(stx - stp)
         if (abs(stpc-stp) > abs(stpq-stp)) then
            stpf = stpc
         else
            stpf = stpq
            end if
         brackt = .true.
! 
!     third case. a lower function value, derivatives of the
!     same sign, and the magnitude of the derivative decreases.
!     the cubic step is only used if the cubic tends to infinity
!     in the direction of the step or if the minimum of the cubic
!     is beyond stp. otherwise the cubic step is defined to be
!     either stpmin or stpmax. the quadratic (secant) step is also
!     computed and if the minimum is bracketed then the the step
!     closest to stx is taken, else the step farthest away is taken.
! 
      else if (abs(dp) < abs(dx)) then
         info = 3
         bound = .true.
         theta = 3*(fx - fp)/(stp - stx) + dx + dp
         s = max(abs(theta),abs(dx),abs(dp))
! 
!        the case gamma = 0 only arises if the cubic does not tend
!        to infinity in the direction of the step.
! 
         gamma = s*sqrt(max(0.0e0,(theta/s)**2 - (dx/s)*(dp/s)))
         if (stp > stx) gamma = -gamma
         p = (gamma - dp) + theta
         q = (gamma + (dx - dp)) + gamma
         r = p/q
         if (r < 0.0 .and. gamma /= 0.0) then
            stpc = stp + r*(stx - stp)
         else if (stp > stx) then
            stpc = stpmax
         else
            stpc = stpmin
         end if
         stpq = stp + (dp/(dp-dx))*(stx - stp)
         if (brackt) then
            if (abs(stp-stpc) < abs(stp-stpq)) then
               stpf = stpc
            else
               stpf = stpq
            end if
         else
            if (abs(stp-stpc) > abs(stp-stpq)) then
               stpf = stpc
            else
               stpf = stpq
            end if
         end if
! 
!     fourth case. a lower function value, derivatives of the
!     same sign, and the magnitude of the derivative does
!     not decrease. if the minimum is not bracketed, the step
!     is either stpmin or stpmax, else the cubic step is taken.
! 
      else
         info = 4
         bound = .false.
         if (brackt) then
            theta = 3*(fp - fy)/(sty - stp) + dy + dp
            s = max(abs(theta),abs(dy),abs(dp))
            gamma = s*sqrt((theta/s)**2 - (dy/s)*(dp/s))
            if (stp > sty) gamma = -gamma
            p = (gamma - dp) + theta
            q = ((gamma - dp) + gamma) + dy
            r = p/q
            stpc = stp + r*(sty - stp)
            stpf = stpc
         else if (stp > stx) then
              stpf = stpmax
            else
              stpf = stpmin
            end if
         end if
! 
!     update the interval of uncertainty. this update does not
!     depend on the new step or the case analysis above.
! 
      if (fp > fx) then
         sty = stp
         fy = fp
         dy = dp
      else
         if (sgnd < 0.0) then
            sty = stx
            fy = fx
            dy = dx
         end if
         stx = stp
         fx = fp
         dx = dp
      end if
! 
!     compute the new step and safeguard it.
! 
      stpf = min(stpmax,stpf)
      stpf = max(stpmin,stpf)
      stp = stpf
      if (brackt .and. bound) then
         if (sty > stx) then
            stp = min(stx+0.66*(sty-stx),stp)
         else
            stp = max(stx+0.66*(sty-stx),stp)
         end if
      end if
      return
! 
!     last card of subroutine vd05bd.
! 
      end subroutine da_vd05bd
