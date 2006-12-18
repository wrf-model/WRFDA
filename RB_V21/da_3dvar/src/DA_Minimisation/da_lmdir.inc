      subroutine da_lmdir (stp,iter,ipoint,iflag, &
                           ndims, gr, gro, msave, s, y, w) 

      real, intent(in)       :: stp
      integer, intent(in)    :: iter
      integer, intent(inout) :: ipoint
      integer, intent(inout) :: iflag
      integer, intent(in)    :: ndims
      real, intent(in)       :: gr(:)
      real, intent(in)       :: gro(:)
      integer, intent(in)    :: msave
      real, intent(inout)    :: s(msave*ndims)
      real, intent(inout)    :: y(msave*ndims)
      real, intent(inout)    :: w(ndims+2*msave)

      integer i, n, m, j, k, ii, npt
      real stpmin,stpmax,f,ys,diag,beta,yy
! 
      integer bound,cp

      n = ndims
      m = msave
      bound = iter-1
!
      if (iter > m) bound=m
      if (iter == 1) return
!
!     ------------------------------------------------------------
!     compute the new s and y
!     ------------------------------------------------------------
! 
      npt = ipoint*n
!
      do i = 1, n
        s(npt+i) = stp*s(npt+i)
        y(npt+i) = gr(i)-gro(i)
      end do
!
      ys = da_dot(y(npt+1),s(npt+1),n )
      yy = da_dot(y(npt+1),y(npt+1),n )

      ipoint = ipoint+1
      if (ipoint == m) ipoint=0
! 
! 
!     ------------------------------------------------------------
!     compute -hg and the diagonal scaling matrix in diag
!     ------------------------------------------------------------
! 
      diag = ys/yy
      if (diag < 0.0) go to 95
      do i = 1, n
        w(i)= -gr(i)
      end do
! 
      cp = ipoint
      if (ipoint == 0) cp = m
!
      w(n+cp) = 1.0/ys
!
      cp= ipoint
!
      do 25 ii = 1, bound
         cp=cp-1
         if (cp == -1)cp=m-1
         w(n+m+cp+1)= w(n+cp+1) * da_dot(s(cp*n+1), w, n)
         do 20 k = 1, n
  20     w(k)= w(k)-w(n+m+cp+1)*y(cp*n+k)
  25  continue
! 
      do 30 i = 1, n
  30  w(i)=diag*w(i)
!
      do 45 ii=1,bound
         beta= w(n+cp+1) * da_dot(y(cp*n+1), w, n)
         do 40 k=1,n
  40     w(k)= w(k)+s(cp*n+k)*(w(n+m+cp+1)-beta)
         cp=cp+1
         if (cp == m)cp=0
  45  continue
! 
!     ------------------------------------------------------------
!     store the new direction in s
!     ------------------------------------------------------------
! 
      do 70 i=1,n
        s(ipoint*n+i)= w(i)
        w(i)=gr(i)
70    continue
! 
      return
95    iflag= -7
      if (lp > 0) write(lp,135) iflag
      return
! 
 135  format(/' iflag= ',i2,/' the diagonal element of the', &
             ' inverse hessian approximation is not positive')
  
      end subroutine da_lmdir
