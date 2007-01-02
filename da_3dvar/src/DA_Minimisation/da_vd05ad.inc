      subroutine da_vd05ad(n,x,f,g,s,stp,ftol,gtol,xtol, &
                        stpmin,stpmax,maxfev,info,nfev,wa)
!
! 
!     subroutine vd05ad
! 
!     the purpose of vd05ad is to find a step which satisfies
!     a sufficient decrease condition and a curvature condition.
!     the user must provide a subroutine which calculates the
!     function and the gradient.
! 
!     at each stage the subroutine updates an interval of
!     uncertainty with endpoints stx and sty. the interval of
!     uncertainty is initially chosen so that it contains a
!     minimizer of the modified function
! 
!          f(x+stp*s) - f(x) - ftol*stp*(gradf(x)`s).
! 
!     if a step is obtained for which the modified function
!     has a nonpositive function value and nonnegative derivative,
!     then the interval of uncertainty is chosen so that it
!     contains a minimizer of f(x+stp*s).
! 
!     the algorithm is designed to find a step which satisfies
!     the sufficient decrease condition
! 
!           f(x+stp*s) .le. f(x) + ftol*stp*(gradf(x)`s),
! 
!     and the curvature condition
! 
!           abs(gradf(x+stp*s)`s)) .le. gtol*abs(gradf(x)`s).
! 
!     if ftol is less than gtol and if, for example, the function
!     is bounded below, then there is always a step which satisfies
!     both conditions. if no step can be found which satisfies both
!     conditions, then the algorithm usually stops when rounding
!     errors prevent further progress. in this case stp only
!     satisfies the sufficient decrease condition.
! 
!     the subroutine statement is
! 
!     subroutine vd05ad(n,x,f,g,s,stp,ftol,gtol,xtol,
!                          stpmin,stpmax,maxfev,info,nfev,wa)
!     where
! 
!       n is a positive integer input variable set to the number
!         of variables.
! 
!       x is an array of length n. on input it must contain the
!         base point for the line search. on output it contains
!         x + stp*s.
! 
!       f is a variable. on input it must contain the value of f
!         at x. on output it contains the value of f at x + stp*s.
! 
!       g is an array of length n. on input it must contain the
!         gradient of f at x. on output it contains the gradient
!         of f at x + stp*s.
! 
!       s is an input array of length n which specifies the
!         search direction.
! 
!       stp is a nonnegative variable. on input stp contains an
!         initial estimate of a satisfactory step. on output
!         stp contains the final estimate.
! 
!       ftol and gtol are nonnegative input variables. termination
!         occurs when the sufficient decrease condition and the
!         directional derivative condition are satisfied.
! 
!       xtol is a nonnegative input variable. termination occurs
!         when the relative width of the interval of uncertainty
!         is at most xtol.
! 
!       stpmin and stpmax are nonnegative input variables which
!         specify lower and upper bounds for the step.
! 
!       maxfev is a positive integer input variable. termination
!         occurs when the number of calls to fcn is at least
!         maxfev by the end of an iteration.
! 
!       info is an integer output variable set as follows:
! 
!         info = 0  improper input parameters.
! 
!         info =-1  a return is made to compute the function and gradien
! 
!         info = 1  the sufficient decrease condition and the
!                   directional derivative condition hold.
! 
!         info = 2  relative width of the interval of uncertainty
!                   is at most xtol.
! 
!         info = 3  number of calls to fcn has reached maxfev.
! 
!         info = 4  the step is at the lower bound stpmin.
! 
!         info = 5  the step is at the upper bound stpmax.
! 
!         info = 6  rounding errors prevent further progress.
!                   there may not be a step which satisfies the
!                   sufficient decrease and curvature conditions.
!                   tolerances may be too small.
! 
!       nfev is an integer output variable set to the number of
!         calls to fcn.
! 
!       wa is a work array of length n.
! 
!     subprograms called
! 
!       harwell-supplied...da_vd05bd
! 
!       fortran-supplied...abs,max,min
! 
!     argonne national laboratory. minpack project. june 1983
!     jorge j. more`, david j. thuente
! 
!     **********


      integer,intent(in)                 :: n
      real,dimension(n),intent(inout)    :: x
      real,intent(inout)                 :: f
      real,dimension(n),intent(inout)    :: g
      real,dimension(n),intent(in)       :: s
      real,intent(inout)                 :: stp
      real,intent(in)                    :: ftol
      real,intent(in)                    :: gtol
      real,intent(in)                    :: xtol
      real,intent(in)                    :: stpmin
      real,intent(in)                    :: stpmax
      integer,intent(in)                 :: maxfev
      integer,intent(inout)              :: info
      integer,intent(out)                :: nfev
      real,dimension(n),intent(inout)    :: wa
      integer :: j

!     integer infoc,j
!     logical brackt,stage1
!     save
!     real dg,dgm,dginit,dgtest,dgx,dgxm,dgy,dgym, &
!            finit,ftest1,fm,fx,fxm,fy,fym,p5,p66,stx,sty, &
!            stmin,stmax,width,width1,xtrapf,zero

      p5 = 0.5
      p66 = 0.66
      xtrapf = 4.0
      zero = 0.0
      nfev = 0
      
      if(info == -1) go to 45
      infoc = 1
! 
!     check the input parameters for errors.
! 
      if (n <= 0 .or. stp <= zero .or. ftol <= zero .or. &
          gtol <= zero .or. xtol < zero .or. stpmin < zero &
          .or. stpmax < stpmin .or. maxfev <= 0) return
! 
!     compute the initial gradient in the search direction
!     and check that s is a descent direction.
! 
      dginit=da_dot(g,s,n)
      if (dginit >= zero) return
!      print *, 'dginit = ', dginit
! 
!     initialize local variables.
! 
      brackt = .false.
      stage1 = .true.
      nfev = 0
      finit = f
      dgtest = ftol*dginit
      width = stpmax - stpmin
      width1 = width/p5
      do 20 j = 1, n
         wa(j) = x(j)
20    continue
! 
!     the variables stx, fx, dgx contain the values of the step,
!     function, and directional derivative at the best step.
!     the variables sty, fy, dgy contain the value of the step,
!     function, and derivative at the other endpoint of
!     the interval of uncertainty.
!     the variables stp, f, dg contain the values of the step,
!     function, and derivative at the current step.
! 
      stx = zero
      fx = finit
      dgx = dginit
      sty = zero
      fy = finit
      dgy = dginit
! 
!     start of iteration.
! 
   30 continue
! 
!        set the minimum and maximum steps to correspond
!        to the present interval of uncertainty.
! 
         if (brackt) then
            stmin = min(stx,sty)
            stmax = max(stx,sty)
         else
            stmin = stx
            stmax = stp + xtrapf*(stp - stx)
         end if
! 
!        force the step to be within the bounds stpmax and stpmin.
! 
         stp = max(stp,stpmin)
         stp = min(stp,stpmax)
! 
!        if an unusual termination is to occur then let
!        stp be the lowest point obtained so far.
! 
         if ((brackt .and. (stp <= stmin .or. stp >= stmax)) &
            .or. nfev >= maxfev-1 .or. infoc == 0 &
            .or. (brackt .and. stmax-stmin <= xtol*stmax)) stp = stx
! 
!        evaluate the function and gradient at stp
!        and compute the directional derivative.
! 
         do 40 j = 1, n
            x(j) = wa(j) + stp*s(j)
   40       continue
!       write (0,*) 'in vd05ad   step=',stp
!       write (0,*) 'in vd05ad    new x=',sum(x**2)
         info=-1
         return
! 
   45    info=0
         nfev = nfev + 1
         dg=da_dot(g,s,n)
         ftest1 = finit + stp*dgtest
! 
!        test for convergence.
! 
         if ((brackt .and. (stp <= stmin .or. stp >= stmax)) &
            .or. infoc == 0) info = 6
         if (stp == stpmax .and. f <= ftest1 .and. dg <= dgtest) info = 5
         if (stp == stpmin .and. (f >= ftest1 .or. dg >= dgtest)) info = 4
         if (nfev >= maxfev) info = 3
         if (brackt .and. stmax-stmin <= xtol*stmax) info = 2
         if (f <= ftest1 .and. abs(dg) <= gtol*(-dginit)) info = 1
! 
!        check for termination.
! 
         if (info /= 0) return
! 
!        in the first stage we seek a step for which the modified
!        function has a nonpositive value and nonnegative derivative.
! 
         if (stage1 .and. f <= ftest1 .and. &
             dg >= min(ftol,gtol)*dginit) stage1 = .false.
! 
!        a modified function is used to predict the step only if
!        we have not obtained a step for which the modified
!        function has a nonpositive function value and nonnegative
!        derivative, and if a lower function value has been
!        obtained but the decrease is not sufficient.
! 
         if (stage1 .and. f <= fx .and. f > ftest1) then
! 
!           define the modified function and derivative values.
! 
            fm = f - stp*dgtest
            fxm = fx - stx*dgtest
            fym = fy - sty*dgtest
            dgm = dg - dgtest
            dgxm = dgx - dgtest
            dgym = dgy - dgtest
! 
!           call cstep to update the interval of uncertainty
!           and to compute the new step.
! 
            call da_vd05bd(stx,fxm,dgxm,sty,fym,dgym,stp,fm,dgm, &
                       brackt,stmin,stmax,infoc)
! 
!           reset the function and gradient values for f.
! 
            fx = fxm + stx*dgtest
            fy = fym + sty*dgtest
            dgx = dgxm + dgtest
            dgy = dgym + dgtest
         else
! 
!           call da_vd05bd to update the interval of uncertainty
!           and to compute the new step.
! 
            call da_vd05bd(stx,fx,dgx,sty,fy,dgy,stp,f,dg, &
                       brackt,stmin,stmax,infoc)
            end if
! 
!        force a sufficient decrease in the size of the
!        interval of uncertainty.
! 
         if (brackt) then
            if (abs(sty-stx) >= p66*width1)stp = stx + p5*(sty - stx)
            width1 = width
            width = abs(sty-stx)
            end if
! 
!        end of iteration.
! 
         go to 30
! 
!     last card of subroutine vd05ad.
! 
      end subroutine da_vd05ad
