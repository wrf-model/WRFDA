      subroutine da_va15bd(iprint,iter,nfun,n,m,x,f,g,stp,eps,finish)
! 
!     ------------------------------------------------------------------
!     this routine prints monitoring information. the frequency and amou
!     of output are specified as follows:
! 
!     iprint(1) < 0 : no output is generated
!     iprint(1) = 0 : output only at first and last iteration
!     iprint(1) > 0 : output every iprint(1) iteration
!     iprint(2) = 0 : iteration count, function value, norm of the gradi
!                     ,number of function calls and step length
!     iprint(2) = 1 : + vector of variables and gradient vector at the
!                       initial point
!     iprint(2) = 2 : + vector of variables
!     iprint(2) = 3 : + gradient vector
!     ------------------------------------------------------------------
!
!
      integer,intent(in),dimension(2)         :: iprint
      integer,intent(in)                      :: iter
      integer,intent(in)                      :: nfun
      integer,intent(in)                      :: n
      integer,intent(in)                      :: m
      real,intent(in),dimension(n)            :: x
      real,intent(in)                         :: f
      real,intent(in),dimension(n)            :: g
      real,intent(in)                         :: stp
      real,intent(in)                         :: eps
      logical,intent(out)                     :: finish
      integer  :: i
      real gnorm

      gnorm= da_dot(g,g,n)
      gnorm= sqrt(gnorm)
!
!     convergence check
! 
      finish = .false.
      if (gnorm <= eps) finish = .true.
      if (iprint(1).lt.0)return
      if (iter == 0)then
           write(mp,10)
           write(mp,20) n,m
           write(mp,30)f,gnorm
           if (iprint(2) >= 1)then
               write(mp,40)
               write(mp,50) (x(i),i=1,n)
               write(mp,60)
               write(mp,50) (g(i),i=1,n)
           endif
           write(mp,10)
           write(mp,70)
           write(mp,80)iter,nfun,f,gnorm,stp
      else

          if ((iprint(1) == 0).and.(iter /= 1.and..not.finish))return
              if (iprint(1) /= 0)then
                   if(mod(iter-1,iprint(1)) == 0.or.finish)then
                         write(mp,80)iter,nfun,f,gnorm,stp
                   else
                         return
                   endif
              else
                   write(mp,80)iter,nfun,f,gnorm,stp
              endif
              if (iprint(2) == 2.or.iprint(2) == 3)then
                    if (finish)then
                        write(mp,90)
                    else
                        write(mp,40)
                    endif
                      write(mp,50)(x(i),i=1,n)
                  if (iprint(2) == 3)then
                      write(mp,60)
                      write(mp,50)(g(i),i=1,n)
                  endif
              endif
              if (finish) write(mp,100)
      endif
! 
 10   format(4x,'*************************************************')
 20   format(4x,'n=',i8,'   number of corrections=',i2)
 30   format(4x,'f= ',1pd10.3,'   gnorm= ',1pd10.3)
 40   format(' vector x= ')
 50   format(6(2x,1pd10.3))
 60   format(' gradient vector g= ')
 70   FORMAT(/'     I   NFN',10X,'FUNC',10X,'GNORM',10X,'STEPLENGTH')
 80   FORMAT(2X,2(I4,1X),3(2x,1PD15.8))
 90   format(' final point x= ')
 100  format(/'    The minimization terminated without detecting errors.')
! 
      return
      end subroutine da_va15bd
